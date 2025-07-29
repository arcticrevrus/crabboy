use crate::memory::Memory;

mod cpu;
mod memory;
mod cartridge;
mod asm;

fn main() {
    let mut memory = memory::MemoryMap::new();
    let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
    memory.write(0xC000, 0xB);

    let bytestring = [0x06, 0xAB, 0x00];


    let opcode = asm::parse_opcode(0xCB, Some(0x13));


    println!("{}", memory.read(0xC000));
    println!("{:?}", opcode);
}
