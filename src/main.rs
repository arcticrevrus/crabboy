use rand::Rng;
use rand::prelude::*;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use crate::cpu::*;
use crate::graphics::*;
use crate::memory::*;

mod cartridge;
mod cpu;
mod graphics;
mod memory;
mod window;

fn main() {
    let display = Arc::new(Mutex::new(graphics::Display::new()));
    {
        let mut fb = display.lock().unwrap();
        fb.test_pattern();
    }

    {
        let fbm = display.clone();
        std::thread::spawn(move || {
            let mut memory = MemoryMap::new();
            let mut cpu = Cpu::new(Mode::DMG);
            cpu.boot(&mut memory, fbm)
        });
    }
    // This blocks until the window closes
    window::draw_window(display.clone());
}
