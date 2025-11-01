#![no_std]
#![no_main]

use defmt::{trace};
use embassy_executor::Spawner;
use embassy_stm32::{
    exti::ExtiInput, gpio::{AnyPin, Level, Output, Speed}, i2c::{self, I2c, Master}, mode::Blocking, rcc::{self}, Config, Peri
};
use embassy_time::{Timer, Delay};
use {defmt_rtt as _, panic_probe as _};

use mpu6050_dmp::{sensor::Mpu6050};

#[embassy_executor::main]
async fn main(spawner: Spawner) {
    let mut config = Config::default();

    config.rcc.hse = Some(rcc::Hse {
        freq: embassy_stm32::time::Hertz(12_000_000),
        mode: rcc::HseMode::Oscillator,
    });
    let p = embassy_stm32::init(config);
    
    let iic = I2c::new_blocking(p.I2C1, p.PA9, p.PA10, i2c::Config::default());
    let _lsb_pin = Output::new(p.PA11, Level::Low, Speed::Low);

    let ok_pin = p.PC14;
    let fail_pin = p.PC15;
    let txpin = p.PA6;
    
    let _rxen = Output::new(p.PB0, Level::High, Speed::Low);
    let _txen = Output::new(p.PB1, Level::Low, Speed::Low);

    core::mem::forget(_rxen);
    core::mem::forget(_txen);
    core::mem::forget(_lsb_pin);

    let imu_int = ExtiInput::new(p.PA12, p.EXTI12, embassy_stm32::gpio::Pull::Down);
    
    spawner.spawn(transmit(txpin.into())).unwrap();
    spawner.spawn(read_mpu(iic, imu_int.into())).unwrap();

    spawner.spawn(status_leds(ok_pin.into())).unwrap();
    Timer::after_millis(500).await;
    spawner.spawn(status_leds(fail_pin.into())).unwrap();
    
}

#[embassy_executor::task(pool_size = 2)]
async fn status_leds(p: Peri<'static, AnyPin>) {
    let mut led = Output::new(p, Level::Low, Speed::Low);

    loop {
        led.set_high();
        Timer::after_millis(500).await;
        led.set_low();
        Timer::after_millis(500).await;
    }
}

#[embassy_executor::task]
async fn transmit(p: Peri<'static, AnyPin>) {
    let mut txpin = Output::new(p, Level::Low, Speed::VeryHigh);
    txpin.set_low();
    Timer::after_millis(1).await;
    txpin.set_high();
    Timer::after_millis(1).await;
    txpin.set_low();

    loop {
        txpin.toggle();
        Timer::after_millis(4).await;
    }
}


#[embassy_executor::task]
async fn read_mpu(iic: I2c<'static, Blocking, Master>, mut ext: ExtiInput<'static>) {
    let mpu = Mpu6050::new(iic, mpu6050_dmp::address::Address::default());
    let mut mpu = mpu.unwrap();
    mpu.initialize_dmp(&mut Delay).unwrap();
    mpu.enable_dmp().unwrap();
    mpu.set_clock_source(mpu6050_dmp::clock_source::ClockSource::Xgyro).unwrap();
    let mut fifo : [u8; 6] = [0; 6];
    mpu.interrupt_fifo_oflow_en().unwrap();
    mpu.enable_fifo().unwrap();
    mpu.interrupt_read_clear().unwrap();
    
    loop {
        ext.wait_for_rising_edge().await;
        trace!("Received new data");
        mpu.read_fifo(&mut fifo).unwrap();
        // let accel = mpu.gyro().unwrap();
        // trace!("X: {}, Y: {}, Z: {}" , accel.x(), accel.y(), accel.z());
        trace!("{}", fifo);
        mpu.interrupt_read_clear().unwrap();
    }
}
    
