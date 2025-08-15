use crate::cpu::*;
use crate::memory::*;

mod cartridge;
mod cpu;
mod memory;

fn main() {
    let mut memory = MemoryMap::new();
    let mut cpu = Cpu::new(cpu::Mode::DMG);
    cpu.boot(&mut memory);
}
