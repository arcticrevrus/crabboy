use std::sync::Arc;
use std::sync::Mutex;

use crate::cpu::asm::*;
pub use crate::cpu::enums::*;
use crate::cpu::opfunctions::*;
use crate::cpu::parse::*;
use crate::cpu::structs::*;
use crate::graphics::Display;
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
                    ime: InterruptMasterEnable {
                        ime: false,
                        has_waited: false,
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
                    ime: InterruptMasterEnable {
                        ime: false,
                        has_waited: false,
                    },
                },
            },
        }
    }
    pub fn boot(&mut self, memory: &mut MemoryMap, display: Arc<Mutex<Display>>) {
        let bootrom = include_bytes!("../files/dmg.bin");
        self.mask_bootrom(*bootrom, memory);
        self.registers.pc.programcounter = 0x0000;
        while self.registers.pc.programcounter != 0x0100 {
            self.execute_next_instruction(memory);
            display
                .lock()
                .expect("failed to unlock display mutex")
                .update(memory);
        }
        println!("bootrom complete")
    }
    fn mask_bootrom(&mut self, bootrom: [u8; 256], memory: &mut MemoryMap) {
        for (address, byte) in (0_u16..).zip(bootrom.into_iter()) {
            memory.write(address, byte);
        }
    }
    #[inline(always)]
    fn get_next_byte(&mut self, memory: &MemoryMap) -> u8 {
        let value = memory.read(self.registers.pc.programcounter);
        self.registers.pc.programcounter = self.registers.pc.programcounter.wrapping_add(1);
        value
    }
    fn get_next_operation(&mut self, memory: &MemoryMap) -> Operation {
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
        if self.registers.ime.ime {
            if self.registers.ime.has_waited {
                self.registers.ime.has_waited = false;
                self.registers.ime.ime = false;
            }
            if self.registers.ime.ime {
                self.registers.ime.has_waited = true;
            }
        }
        let operation = self.get_next_operation(memory);

        let mut i8_value: Option<i8> = None;
        if operation.signed_value {
            i8_value = Some(
                operation
                    .value_one
                    .expect("Signed value set on operation with no value") as i8,
            )
        }
        //println!(
        //    "Operation: {:?}\npc: {}",
        //    operation, self.registers.pc.programcounter
        //);
        match operation.opcode {
            Opcode::DAA => daa_operation(self),
            Opcode::PANIC => crash(self),
            Opcode::STOP => todo!(),
            Opcode::HALT => todo!(),
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
            Opcode::CP(target) => cp_operation(self, target, operation.value_one, memory),
            Opcode::CCF => ccf_operation(self),
            Opcode::CPL => cpl_operation(self),
            Opcode::SCF => scf_operation(self),
            Opcode::JP(condition, target) => jp_operation(
                self,
                condition,
                target,
                operation.value_one,
                operation.value_two,
            ),
            Opcode::JR(condition) => jr_operation(
                self,
                condition,
                operation
                    .value_one
                    .expect("No value given for JR operation") as i8,
            ),
            Opcode::RRA => rra_operation(self),
            Opcode::RRCA => rrca_operation(self),
            Opcode::RLA => rla_operation(self),
            Opcode::RLCA => rlca_operation(self),
            Opcode::ADD(register, target2) => add_operation(
                self,
                register,
                target2,
                operation.value_one,
                i8_value,
                memory,
            ),
            Opcode::ADC(source) => adc_operation(self, source, operation.value_one, memory),
            Opcode::SUB(source) => sub_operation(self, source, operation.value_one, memory),
            Opcode::SBC(source) => sbc_operation(self, source, operation.value_one, memory),
            Opcode::AND(source) => and_operation(self, source, operation.value_one, memory),
            Opcode::XOR(source) => xor_operation(self, source, operation.value_one, memory),
            Opcode::OR(source) => or_operation(self, source, operation.value_one, memory),
            Opcode::POP(register) => pop_operation(self, register, memory),
            Opcode::PUSH(source) => push_operation(self, source, memory),
            Opcode::RET(condition) => ret_operation(self, condition, memory),
            Opcode::RETI => reti_operation(self, memory),
            Opcode::CALL(condition) => call_operation(
                self,
                condition,
                operation.value_one,
                operation.value_two,
                memory,
            ),
            Opcode::RST(rst_addr) => rst_operation(self, rst_addr, memory),
            Opcode::EI => ei_operation(self),
            Opcode::DI => di_operation(self),
            Opcode::CB(cbcode) => self.execute_cb_instruction(cbcode, memory),
        }
    }

    fn execute_cb_instruction(&mut self, cbcode: CBPrefix, memory: &mut MemoryMap) {
        match cbcode {
            CBPrefix::BIT(value, target, timing) => bit_operation(self, target, value, memory),
            CBPrefix::RRC(target, timing) => rrc_operation(self, target, memory),
            _ => todo!(),
        }
    }
}
