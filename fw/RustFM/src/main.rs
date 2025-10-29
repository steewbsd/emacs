#![no_std]
#![no_main]

use panic_halt as _;
extern crate cortex_m;
// #[macro_use]
extern crate cortex_m_rt as rt;
extern crate cortex_m_semihosting as sh;

#[rtic::app(device = stm32l4xx_hal::pac, peripherals = true, dispatchers = [SAI1, ADC1_2, SPI1])]
mod app {

    use cortex_m_semihosting::hprintln;
    // use embedded_hal_compat::{ForwardCompat, ReverseCompat};
    // use critical_section::*;
    // use mpu6050::{device::CLKSEL, Mpu6050};
    use mpu6050_dmp::{address::Address, sensor::Mpu6050};
    use rtic_monotonics::{stm32::prelude::*, stm32_tim2_monotonic};
    stm32_tim2_monotonic!(Mono, 100);

    use rtic_sync::{
        channel::{Receiver, Sender},
        make_channel,
    };
    use stm32l4xx_hal::{
        delay::Delay,
        flash::FlashExt,
        gpio::{
            Alternate, Input, OpenDrain, Output, PullDown, PushPull, PA10, PA12, PA6, PA9, PC14,
            PC15,
        },
        i2c::{self, I2c},
        pac::{I2C1, NVIC, TIM7},
        prelude::*,
        pwr::PwrExt,
        rcc::{ClockSecuritySystem, CrystalBypass, RccExt},
        stm32,
        timer::{Event, Timer},
    };

    #[allow(non_camel_case_types)]
    #[repr(u8)]
    // MPU6050 registers
    pub enum MpuRegs {
        INT_PIN_CFG = 55,
        INT_ENABLE = 56,
        INT_STATUS = 58,
    }

    pub enum MpuFields {}

    impl From<MpuRegs> for u8 {
        fn from(r: MpuRegs) -> u8 {
            r as u8
        }
    }

    pub enum LEDS {
        RED,
        GREEN,
    }

    // Led blink speeds in Hz
    #[derive(Copy, Clone)]
    pub enum BlinkSpeed {
        SLOW = 1,
        MEDIUM = 3,
        FAST = 5,
        HOLD = 0,
    }

    #[derive(Clone, Copy, Debug)]
    pub enum SysStatus {
        IDLE,
        OK,
        FAIL,
        BOOT,
        TX,
    }

    #[shared]
    struct Shared {
        status: SysStatus,
    }

    #[local]
    struct Local {
        timer7: Timer<TIM7>,              // led blink timer
        led_ok: PC14<Output<PushPull>>,   // green led
        led_fail: PC15<Output<PushPull>>, // red led
        txpin: PA6<Output<PushPull>>,     // outgoing transmit data
        imu_int: PA12<Input<PullDown>>,
        mpu: Mpu6050<I2c<I2C1, (PA9<Alternate<OpenDrain, 4>>, PA10<Alternate<OpenDrain, 4>>)>>, // mpu: Mpu6050<I2c>,
    }

    const CAPACITY: usize = 1;
    #[init]
    fn init(c: init::Context) -> (Shared, Local) {
        let mut dp = c.device;
        let mut flash = dp.FLASH.constrain(); // .constrain();
        let mut rcc = dp.RCC.constrain();
        let mut pwr = dp.PWR.constrain(&mut rcc.apb1r1);

        let clocks = rcc
            .cfgr
            .hse(
                12.MHz(),
                CrystalBypass::Disable,
                ClockSecuritySystem::Disable,
            )
            .sysclk(48.MHz())
            .pclk1(24.MHz())
            .pclk2(48.MHz())
            .freeze(&mut flash.acr, &mut pwr);

        let mut timer7 = Timer::tim7(dp.TIM7, 1.Hz(), clocks, &mut rcc.apb1r1);
        timer7.listen(stm32l4xx_hal::timer::Event::TimeOut);

        let mut gpioc = dp.GPIOC.split(&mut rcc.ahb2);
        let mut gpioa = dp.GPIOA.split(&mut rcc.ahb2);
        let mut gpiob = dp.GPIOB.split(&mut rcc.ahb2);

        let led_ok = gpioc
            .pc14
            .into_push_pull_output(&mut gpioc.moder, &mut gpioc.otyper);
        let led_fail = gpioc
            .pc15
            .into_push_pull_output(&mut gpioc.moder, &mut gpioc.otyper);
        let mut txpin = gpioa
            .pa6
            .into_push_pull_output(&mut gpioa.moder, &mut gpioa.otyper);
        let mut rxen = gpiob
            .pb0
            .into_push_pull_output(&mut gpiob.moder, &mut gpiob.otyper);
        let mut txen = gpiob
            .pb1
            .into_push_pull_output(&mut gpiob.moder, &mut gpiob.otyper);

        // setup for transmission
        rxen.set_high();
        txen.set_low();

        // initialize i2c peripheral
        let scl = gpioa.pa9.into_alternate_open_drain::<4>(
            &mut gpioa.moder,
            &mut gpioa.otyper,
            &mut gpioa.afrh,
        );
        let sda = gpioa.pa10.into_alternate_open_drain::<4>(
            &mut gpioa.moder,
            &mut gpioa.otyper,
            &mut gpioa.afrh,
        );

        // Configure the external interrupt for MPU6050 data
        let mut imu_int = gpioa
            .pa12
            .into_pull_down_input(&mut gpioa.moder, &mut gpioa.pupdr);
        imu_int.make_interrupt_source(&mut dp.SYSCFG, &mut rcc.apb2);
        imu_int.trigger_on_edge(&mut dp.EXTI, stm32l4xx_hal::gpio::Edge::Rising);
        imu_int.enable_interrupt(&mut dp.EXTI);
        unsafe {
            NVIC::unmask(stm32::interrupt::EXTI15_10);
        }

        let iic = i2c::I2c::i2c1(
            dp.I2C1,
            (scl, sda),
            i2c::Config::new(400.kHz(), clocks),
            &mut rcc.apb1r1,
        );
        let mut mpu_slave_add_lsb = gpioa
            .pa11
            .into_push_pull_output(&mut gpioa.moder, &mut gpioa.otyper);
        // Force low the address of the I2C IMU peripheral
        mpu_slave_add_lsb.set_low();
        // let mut mpu = Mpu6050::new(iic);
        let mut delay = Delay::new(c.core.SYST, clocks);

        // init sequence for max7044
        txpin.set_low();
        delay.delay_ms(1_u32);
        txpin.set_high();
        delay.delay_ms(1_u32);
        txpin.set_low();

        // let init_result = mpu.init(&mut timer);
        // // Set the mpu clock source to the x axis gyroscope
        // mpu.set_clock_source(CLKSEL::GXAXIS).unwrap();
        // hprintln!("Initialized MPU peripheral: {:?}", init_result);

        // // Enable clear interrupt on read operation
        // mpu.write_bit(MpuRegs::INT_PIN_CFG.into(), 4, true).unwrap();
        // // Enable interrupt on data ready
        // mpu.write_bit(MpuRegs::INT_ENABLE.into(), 0, false).unwrap();

        let mut mpu = Mpu6050::new(iic, Address::default()).unwrap();
        mpu.initialize_dmp(&mut delay).unwrap();
        mpu.enable_dmp().unwrap();
        mpu.boot_firmware().unwrap();
        mpu.set_clock_source(mpu6050_dmp::clock_source::ClockSource::Xgyro)
            .unwrap();

        Mono::start(12_000_000);

        // mpu.disable_dmp

        #[allow(unused_variables)]
        let (send, receive) = make_channel!(usize, CAPACITY);
        transmitter::spawn(receive).unwrap();
        // sender1::spawn(send).unwrap();

        let status = SysStatus::FAIL;
        (
            Shared { status },
            Local {
                timer7,
                led_ok,
                led_fail,
                txpin,
                imu_int,
                mpu,
            },
        )
    }

    #[idle]
    fn idle(_: idle::Context) -> ! {
        loop {}
    }

    #[task(priority = 1)]
    async fn sender1(_: sender1::Context, mut sender: Sender<'static, usize, CAPACITY>) {
        loop {
            // hprintln!("Sender 1 sending: 1");
            sender.send(1).await.unwrap();
            Mono::delay(50_u64.millis()).await
        }
    }

    #[task(binds = EXTI15_10, priority = 7, local = [mpu, imu_int])]
    fn mpu_read(c: mpu_read::Context) {
        // let acc = c.local.mpu.get_acc();
        let acc = c.local.mpu.gyro();
        c.local.mpu.get_fifo_count().unwrap();
        hprintln!("Read MPU value: {:?}", acc.unwrap());
        // hprintln!("Read TEMP value: {:?}", c.local.mpu.get_temp().unwrap());

        // c.local.imu_int.clear_interrupt_pending_bit();
        // Clear the interrupt after reading the INT status register
        // c.local
        //     .mpu
        //     .read_bits(MpuRegs::INT_STATUS.into(), 0, 0)
        //     .unwrap();
    }

    #[task(priority = 5, local = [txpin], shared = [status])]
    async fn transmitter(
        mut ctx: transmitter::Context,
        mut receiver: Receiver<'static, usize, CAPACITY>,
    ) {
        hprintln!("Waiting for data to transmit");
        // Wait until we receive data to send
        while let Ok(_data) = receiver.recv().await {
            // hprintln!("Got data to transmit: {}", data);
            ctx.shared.status.lock(|s| {
                *s = SysStatus::TX;
            });
            ctx.local.txpin.toggle();
        }
        hprintln!("No more data received.");
        ctx.shared.status.lock(|s| {
            *s = SysStatus::IDLE;
        });
    }

    #[task(binds = TIM7, local = [led_ok, led_fail, timer7], shared = [status])]
    fn status_leds(mut ctx: status_leds::Context) {
        let mut led: LEDS = LEDS::RED;
        let mut speed: BlinkSpeed = BlinkSpeed::FAST;

        // Match the curren system status and assign a led and speed
        ctx.shared.status.lock(|status| match status {
            SysStatus::IDLE => {
                led = LEDS::GREEN;
                speed = BlinkSpeed::SLOW;
            }
            SysStatus::FAIL => {
                led = LEDS::RED;
                speed = BlinkSpeed::HOLD;
            }
            SysStatus::BOOT => {
                led = LEDS::GREEN;
                speed = BlinkSpeed::MEDIUM;
            }
            SysStatus::TX => {
                led = LEDS::GREEN;
                speed = BlinkSpeed::FAST;
            }

            _ => {
                led = LEDS::RED;
                speed = BlinkSpeed::HOLD;
            }
        });

        // Activate selected led and force off the other
        match led {
            LEDS::GREEN => {
                ctx.local.led_ok.toggle();
                ctx.local.led_fail.set_low();
            }
            LEDS::RED => {
                ctx.local.led_fail.toggle();
                ctx.local.led_ok.set_low();
            }
        }
        // ctx.local.timer7.clear_update_interrupt_flag();
        // ctx.local.timer7.clear_interrupt(Event::TimeOut);
        // hprintln!("{}", speed as u32);
        ctx.local.timer7.start((speed as u32).Hz());
        ctx.local.timer7.listen(Event::TimeOut);
    }
}
