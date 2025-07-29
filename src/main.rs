use crate::memory::*;
use crate::cpu::*;
use crate::asm::*;

mod cpu;
mod memory;
mod cartridge;
mod asm;

fn main() {
    let mut memory = memory::MemoryMap::new();
    let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
    
    println!("Inserting value 0xAC in B register");
    ld_operation(&mut cpu, OpTarget::Register(Register::B), OpTarget::Value(ValueType::u8), Some(0xAC), None, &mut memory);
    println!("Inserting value 0xC000 in HL register");
    ld_operation(&mut cpu, OpTarget::Register(Register::HL(HLMode::Normal)), OpTarget::Value(ValueType::u16), Some(0xC0), Some(0x00), &mut memory);
    println!("Inserting value 0xAC to memory address held in HL register");
    ld_operation(&mut cpu, OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))), OpTarget::Value(ValueType::u8), Some(0xAC), None, &mut memory);

    println!("Contents of HL Register: {:x}", cpu.registers.read_u16(Register::HL(HLMode::Normal)));
    println!("Contents of b register: {:x}", cpu.registers.bc.b);
    println!("Contents of memory location 0xC000: {:x}", memory.read(0xC000));
}
