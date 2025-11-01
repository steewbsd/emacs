#![no_std]
#![no_main]

use core::{any::Any, ops::Deref};

use defmt::*;
use embassy_executor::Spawner;
use embassy_stm32::{
    gpio::{AnyPin, Level, Output, Speed}, rcc::{self}, time::hz, timer::simple_pwm::{PwmPin, SimplePwm}, Config, Peri
};
use embassy_time::{Timer, WithTimeout};
use {defmt_rtt as _, panic_probe as _};

#[embassy_executor::main]
async fn main(spawner: Spawner) {
    let mut config = Config::default();

    config.rcc.hse = Some(rcc::Hse {
        freq: embassy_stm32::time::Hertz(12_000_000),
        mode: rcc::HseMode::Oscillator,
    });
    let p = embassy_stm32::init(config);
    
    let ok_pin = p.PC14;
    let fail_pin = p.PC15;
    let txpin = p.PA6;

    let rxen =  Output::new(p.PB0, Level::High, Speed::Low);
    let txen =  Output::new(p.PB1, Level::Low, Speed::Low);
    
    spawner.spawn(status_leds(ok_pin.into())).unwrap();
    Timer::after_millis(500).await;
    spawner.spawn(status_leds(fail_pin.into())).unwrap();
    spawner.spawn(transmit(txpin.into())).unwrap();

    // loop {
    // }
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
    let mut txpin = Output::new(p, Level::Low, Speed::Medium);
    txpin.set_low();
    Timer::after_millis(1).await;
    txpin.set_high();
    Timer::after_millis(1).await;
    txpin.set_low();
    
    loop {
        txpin.toggle();
        Timer::after_millis(1).await;
    }
}
