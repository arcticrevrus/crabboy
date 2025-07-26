use crate::memory::Memory;

mod cpu;
mod memory;

fn main() {
    let mut memory = memory::MemoryMap::new();
    let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
    memory.write(0xC000, 0xB);

    let opcode = cpu.parse_opcode(0xCB, Some(0x13));
    println!("{}", memory.read(0xC000));
    println!("{:?}", opcode);
}
