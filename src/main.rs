use crate::memory::Memory;

mod cartridge;
mod cpu;
mod memory;

fn main() {
    let mut memory = memory::MemoryMap::new();
    let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
}
