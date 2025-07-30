use crate::{asm::*, memory::Memory, memory::MemoryMap};

#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
#[allow(dead_code)]
pub enum Mode {
    DMG,
    GBC,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OpTarget {
    Register(Register),
    Value(ValueType),
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Register {
    AF,
    A,
    F,
    BC,
    B,
    C,
    DE,
    D,
    E,
    HL(HLMode),
    H,
    L,
    SP,
    PC,
    SPPlusi8,
}

#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DerefSource {
    u16,
    Register(Register),
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
#[allow(dead_code)]
pub enum ValueType {
    i8,
    u8,
    u16,
    deref(DerefSource),
    ff00_plus_u8_deref,
    ff00_plus_register_deref(Register),
}

#[derive(Debug)]
pub enum Condition {
    None,
    NZ,
    NC,
    C,
    Z,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum HLMode {
    Normal,
    Increment,
    Decrement,
}

#[derive(Debug)]
pub enum RSTAddr {
    H00,
    H08,
    H10,
    H18,
    H20,
    H28,
    H30,
    H38,
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
#[allow(dead_code)]
pub enum Opcode {
    NOP,
    LD(OpTarget, OpTarget),
    INC(OpTarget),
    DEC(Register),
    ADD(Register, OpTarget),
    ADC(Register, OpTarget),
    SUB(Register, OpTarget),
    SBC(Register, OpTarget),
    AND(Register, OpTarget),
    XOR(Register, OpTarget),
    OR(Register, OpTarget),
    CP(OpTarget),
    STOP,
    RLCA,
    RRCA,
    RRA,
    RLA,
    DAA,
    SCF,
    CCF,
    JR(Condition, ValueType),
    RET(Condition),
    CPL,
    RETI,
    JP(Condition, OpTarget),
    CALL(Condition, ValueType),
    RST(RSTAddr),
    POP(Register),
    PUSH(Register),
    DI,
    EI,
    HALT,
    CB(CBPrefix),
    PANIC,
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
#[allow(dead_code)]
pub enum CBPrefix {
    RLC(OpTarget),
    RRC(OpTarget),
    RL(OpTarget),
    RR(OpTarget),
    SLA(OpTarget),
    SRA(OpTarget),
    SWAP(OpTarget),
    SRL(OpTarget),
    BIT(u8, OpTarget),
    RES(u8, OpTarget),
    SET(u8, OpTarget),
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum OpLength {
    None,
    One,
    Two,
    Three,
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Registers {
    pub af: AccumulatorAndFlags,
    pub bc: BAndC,
    pub de: DAndE,
    pub hl: HAndL,
    pub sp: StackPointer,
    pub pc: ProgramCounter,
}
impl Registers {
    #[allow(dead_code)]
    pub fn read_u16(&mut self, register: Register) -> u16 {
        match register {
            Register::AF => ((self.af.accumulator as u16) << 8) | (self.af.flags.bits as u16),
            Register::BC => ((self.bc.b as u16) << 8) | (self.bc.c as u16),
            Register::DE => ((self.de.d as u16) << 8) | (self.de.e as u16),
            Register::HL(hlm) => match hlm {
                HLMode::Normal => ((self.hl.h as u16) << 8) | (self.hl.l as u16),
                HLMode::Increment => {
                    let return_value = ((self.hl.h as u16) << 8) | (self.hl.l as u16);
                    self.hl.l = self.hl.l.wrapping_add(1);
                    return_value
                }
                HLMode::Decrement => {
                    let return_value = ((self.hl.h as u16) << 8) | (self.hl.l as u16);
                    self.hl.l = self.hl.l.wrapping_sub(1);
                    return_value
                }
            },
            Register::SP => self.sp.stackpointer,
            Register::PC => self.pc.programcounter,
            _ => panic!("8 bit register given to 16 bit read operation"),
        }
    }

    #[allow(dead_code)]
    pub fn write_u16(&mut self, register: Register, value: u16) {
        let high_byte = (&value >> 8) as u8;
        let low_byte = (&value & 0xFF) as u8;
        match register {
            Register::AF => {
                self.af.accumulator = high_byte;
                self.af.flags.bits = low_byte;
            }
            Register::BC => {
                self.bc.b = high_byte;
                self.bc.c = low_byte;
            }
            Register::DE => {
                self.de.d = high_byte;
                self.de.e = low_byte;
            }
            Register::HL(_) => {
                self.hl.h = high_byte;
                self.hl.l = low_byte;
            }
            Register::SP => {
                self.sp.stackpointer = value;
            }
            Register::PC => {
                self.pc.programcounter = value;
            }
            _ => panic!("Attempted to write a 16 bit value to an 8 bit register"),
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Operation {
    pub opcode: Opcode,
    pub length: OpLength,
    pub value_one: Option<u8>,
    pub value_two: Option<u8>,
}
impl Operation {
    pub fn new(opcode: Opcode, length: OpLength) -> Operation {
        Operation {
            opcode,
            length,
            value_one: None,
            value_two: None,
        }
    }
}

#[derive(PartialEq)]
#[allow(non_camel_case_types)]
pub enum FieldLength {
    u8,
    u16,
}
#[derive(Debug, PartialEq)]
pub struct Cpu {
    pub registers: Registers,
}
#[allow(dead_code)]
impl Cpu {
    pub fn new(mode: Mode) -> Self {
        match mode {
            Mode::DMG => Cpu {
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
        match operation.opcode {
            Opcode::CB(_) => (),
            _ => match operation.length {
                OpLength::None => panic!("Invalid operation returned from opcode parser"),
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
        match operation.opcode {
            Opcode::LD(target1, target2) => ld_operation(
                self,
                target1,
                target2,
                operation.value_one,
                operation.value_two,
                memory,
            ),
            Opcode::INC(target) => inc_operation(self, target, memory),
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
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct AccumulatorAndFlags {
    pub accumulator: u8,
    pub flags: Flags,
}

pub enum Flag {
    Z,
    N,
    H,
    C,
}

#[derive(Debug, PartialEq)]
pub struct Flags {
    pub bits: u8,
}
#[allow(dead_code)]
impl Flags {
    pub fn new() -> Self {
        Flags { bits: 0 }
    }

    pub fn set(&mut self, flag: Flag, value: bool) {
        const Z_BIT: u8 = 0b1000_0000;
        const N_BIT: u8 = 0b0100_0000;
        const H_BIT: u8 = 0b0010_0000;
        const C_BIT: u8 = 0b0001_0000;
        let flag_bit = match flag {
            Flag::Z => Z_BIT,
            Flag::N => N_BIT,
            Flag::H => H_BIT,
            Flag::C => C_BIT,
        };
        if value {
            self.bits |= flag_bit;
        } else {
            self.bits &= !flag_bit;
        }
    }

    pub fn get(&self, flag: Flag) -> bool {
        const Z_BIT: u8 = 0b1000_0000;
        const N_BIT: u8 = 0b0100_0000;
        const H_BIT: u8 = 0b0010_0000;
        const C_BIT: u8 = 0b0001_0000;
        let flag_bit = match flag {
            Flag::Z => Z_BIT,
            Flag::N => N_BIT,
            Flag::H => H_BIT,
            Flag::C => C_BIT,
        };
        (self.bits & flag_bit) != 0
    }
}

#[derive(Debug, PartialEq)]
pub struct BAndC {
    pub b: u8,
    pub c: u8,
}

#[derive(Debug, PartialEq)]
pub struct DAndE {
    pub d: u8,
    pub e: u8,
}

#[derive(Debug, PartialEq)]
pub struct HAndL {
    pub h: u8,
    pub l: u8,
}

#[derive(Debug, PartialEq)]
pub struct StackPointer {
    pub stackpointer: u16,
}

#[derive(Debug, PartialEq)]
pub struct ProgramCounter {
    pub programcounter: u16,
}
