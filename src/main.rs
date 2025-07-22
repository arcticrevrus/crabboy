use crate::memory::Memory;

mod cpu;
mod memory;

fn main() {
    let mut memory = memory::MemoryMap::new();
    let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
    memory.write(0xC000, 0xB);
    println!("{}", memory.read(0xC000));
}
