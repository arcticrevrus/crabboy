use crate::cpu::*;
use crate::memory::*;

mod cartridge;
mod cpu;
mod memory;

fn main() {
    let mut memory = MemoryMap::new();
    let mut cpu = Cpu::new(cpu::Mode::DMG);
    cpu.execute_next_instruction(&mut memory);
}
