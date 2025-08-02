use crate::cpu::asm::*;
pub use crate::cpu::enums::*;
use crate::cpu::opfunctions::*;
use crate::cpu::parse::*;
use crate::cpu::structs::*;
use crate::{memory::Memory, memory::MemoryMap};

mod asm;
mod enums;
mod structs;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Cpu {
    pub registers: Registers,
    pub is_running: bool,
}
#[allow(dead_code)]
impl Cpu {
    pub fn new(mode: Mode) -> Self {
        match mode {
            Mode::DMG => Cpu {
                is_running: true,
                registers: Registers {
                    af: AccumulatorAndFlags {
                        accumulator: 0x01,
                        flags: Flags { bits: 0xB0 },
                    },
                    bc: BAndC { b: 0x00, c: 0x13 },
                    de: DAndE { d: 0x00, e: 0xD8 },
                    hl: HAndL { h: 0x01, l: 0x4D },
                    sp: StackPointer {
                        stackpointer: 0xFFFE,
                    },
                    pc: ProgramCounter {
                        programcounter: 0x0100,
                    },
                },
            },
            Mode::GBC => Cpu {
                is_running: true,
                registers: Registers {
                    af: AccumulatorAndFlags {
                        accumulator: 0x11,
                        flags: Flags { bits: 0x80 },
                    },
                    bc: BAndC { b: 0x00, c: 0x00 },
                    de: DAndE { d: 0xFF, e: 0x56 },
                    hl: HAndL { h: 0x00, l: 0x0D },
                    sp: StackPointer {
                        stackpointer: 0xFFFE,
                    },
                    pc: ProgramCounter {
                        programcounter: 0x0100,
                    },
                },
            },
        }
    }
    fn get_next_byte(&mut self, memory: MemoryMap) -> u8 {
        let value = memory.read(self.registers.pc.programcounter);
        self.registers.pc.programcounter += 1;
        value
    }
    fn get_next_operation(&mut self, memory: MemoryMap) -> Operation {
        let opcode_byte = self.get_next_byte(memory);
        let mut cb_byte = None;
        if opcode_byte == 0xCB {
            cb_byte = Some(self.get_next_byte(memory));
        }
        let mut operation = parse_opcode(opcode_byte, cb_byte);
        operation.signed_value =
            matches!(opcode_byte, 0x18 | 0x20 | 0x28 | 0x30 | 0x38 | 0xE8 | 0xF8);
        match operation.opcode {
            Opcode::CB(_) => (),
            _ => match operation.length {
                OpLength::None => (),
                OpLength::One => (),
                OpLength::Two => operation.value_one = Some(self.get_next_byte(memory)),
                OpLength::Three => {
                    operation.value_one = Some(self.get_next_byte(memory));
                    operation.value_two = Some(self.get_next_byte(memory));
                }
            },
        }
        operation
    }
    pub fn execute_next_instruction(&mut self, memory: &mut MemoryMap) {
        let operation = self.get_next_operation(*memory);
        let mut i8_value: Option<i8> = None;
        if operation.signed_value {
            i8_value = Some(
                operation
                    .value_one
                    .expect("Signed value set on operation with no value") as i8,
            )
        }
        match operation.opcode {
            Opcode::PANIC => crash(self),
            Opcode::LD(target1, target2) => ld_operation(
                self,
                target1,
                target2,
                operation.value_one,
                operation.value_two,
                memory,
            ),
            Opcode::INC(target) => inc_operation(self, target, memory),
            Opcode::DEC(target) => dec_operation(self, target, memory),
            Opcode::NOP => nop_operation(),
            Opcode::CP(target) => cp_operation(self, target, operation.value_one, *memory),
            Opcode::JP(condition, target) => jp_operation(
                self,
                condition,
                target,
                operation.value_one,
                operation.value_two,
            ),
            Opcode::RRA => rra_operation(self),
            Opcode::RLCA => rlca_operation(self),
            Opcode::ADD(register, target2) => add_operation(
                self,
                register,
                target2,
                operation.value_one,
                i8_value,
                memory,
            ),
            _ => todo!(),
        }
    }
}
