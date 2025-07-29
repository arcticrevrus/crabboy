use crate::{
    asm::*,
    memory::{Memory, MemoryMap},
};

#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum Mode {
    DMG,
    GBC,
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum OpTarget {
    Register(Register),
    Value(ValueType),
}

#[derive(Clone, Copy, Debug)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum ValueType {
    i8,
    u8,
    u16,
    deref(Register),
    u16_deref,
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

#[derive(Clone, Copy, Debug)]
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
pub enum Opcode {
    NOP,
    LD(OpTarget, OpTarget),
    INC(Register),
    DEC(Register),
    ADD(Register, OpTarget),
    ADC(Register, OpTarget),
    SUB(Register, OpTarget),
    SBC(Register, OpTarget),
    AND(Register, OpTarget),
    XOR(Register, OpTarget),
    OR(Register, OpTarget),
    CP(Register, OpTarget),
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
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
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
pub enum OpLength {
    One,
    Two,
    Three,
}

#[derive(Debug)]
pub struct Registers {
    pub af: AccumulatorAndFlags,
    pub bc: BAndC,
    pub de: DAndE,
    pub hl: HAndL,
    pub sp: StackPointer,
    pub pc: ProgramCounter,
}
impl Registers {
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

    pub fn write_u16(&mut self, register: Register, value: u16) {
        let low_byte = (&value >> 8) as u8;
        let high_byte = (&value & 0xFF) as u8;
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
pub struct Operation {
    opcode: Opcode,
    length: OpLength,
    value1: Option<u8>,
    value2: Option<u8>,
}

#[derive(PartialEq)]
pub enum FieldLength {
    u8,
    u16,
}
#[derive(Debug)]
pub struct Cpu {
    pub registers: Registers,
}
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
    pub fn execute(byte_one: u8, byte_two: Option<u8>, byte_three: Option<u8>) {
        let opcode = parse_opcode(byte_one, byte_two);
    }

    pub fn execute_operation(
        &mut self,
        operation: Opcode,
        value1: u8,
        value2: u8,
        memory_map: &mut MemoryMap,
    ) {
        match operation {
            Opcode::LD(target1, target2) => ld_operation(
                self,
                target1,
                target2,
                Some(value1),
                Some(value2),
                memory_map,
            ),

            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct AccumulatorAndFlags {
    pub accumulator: u8,
    pub flags: Flags,
}

#[derive(Debug)]
pub struct Flags {
    pub bits: u8,
}
impl Flags {
    const Z: u8 = 0b1000_0000;
    const N: u8 = 0b0100_0000;
    const H: u8 = 0b0010_0000;
    const C: u8 = 0b0001_0000;

    pub fn new() -> Self {
        Flags { bits: 0 }
    }

    pub fn set(&mut self, flag: u8, value: bool) {
        if value {
            self.bits |= flag;
        } else {
            self.bits &= !flag;
        }
    }

    pub fn get(&self, flag: u8) -> bool {
        (self.bits & flag) != 0
    }
}

#[derive(Debug)]
pub struct BAndC {
    pub b: u8,
    pub c: u8,
}

#[derive(Debug)]
pub struct DAndE {
    pub d: u8,
    pub e: u8,
}

#[derive(Debug)]
pub struct HAndL {
    pub h: u8,
    pub l: u8,
}

#[derive(Debug)]
pub struct StackPointer {
    pub stackpointer: u16,
}

#[derive(Debug)]
pub struct ProgramCounter {
    pub programcounter: u16,
}
