pub enum Mode {
    DMG,
    GBC,
}

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
}

pub enum OpTarget {
    Register(Register),
    Value(ValueType),
}

#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum ValueType {
    i8,
    u8,
    u16,
    deref(Register),
}

pub enum JumpCondition {
    None,
    NZ,
    NC,
    C,
    Z,
}

pub enum HLMode {
    Normal,
    Increment,
    Decrement,
}

#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum Opcode {
    NOP,
    LD(OpTarget, OpTarget),
    LDH,
    LD_HL,
    LD_SP,
    INC(Register),
    DEC(Register),
    ADD(OpTarget, OpTarget),
    ADC(OpTarget, OpTarget),
    ADD_SP,
    XOR,
    OR,
    CP,
    STOP,
    RLCA,
    RRCA,
    RL,
    RR,
    RRA,
    RLA,
    RLC,
    RRC,
    DAA,
    CPL,
    SCF,
    SLA,
    SRA,
    SRL,
    CCF,
    JR(JumpCondition, ValueType),
    RET,
    RETI,
    JP,
    CALL,
    RST,
    POP,
    PUSH,
    SWAP,
    DI,
    EI,
    RES,
    BIT,
    SET,
    HALT,
}

pub struct Registers {
    af: AccumulatorAndFlags,
    bc: BAndC,
    de: DAndE,
    hl: HAndL,
    sp: StackPointer,
    pc: ProgramCounter,
}

pub struct Cpu {
    registers: Registers,
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
    pub fn parse_opcode(operation: u8) -> Opcode {
        match operation {
            0x00 => Opcode::NOP,
            0x01 => Opcode::LD(
                OpTarget::Register(Register::BC),
                OpTarget::Value(ValueType::u16),
            ),
            0x02 => Opcode::LD(
                OpTarget::Value(ValueType::deref(Register::BC)),
                OpTarget::Register(Register::A),
            ),
            0x03 => Opcode::INC(Register::BC),
            0x04 => Opcode::INC(Register::B),
            0x05 => Opcode::DEC(Register::B),
            0x06 => Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Value(ValueType::u8),
            ),
            0x07 => Opcode::RLCA,
            0x08 => Opcode::LD(
                OpTarget::Value(ValueType::u16),
                OpTarget::Register(Register::SP),
            ),
            0x09 => Opcode::ADD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::BC),
            ),
            0x0A => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::deref(Register::BC)),
            ),
            0x0B => Opcode::DEC(Register::BC),
            0x0C => Opcode::INC(Register::C),
            0x0D => Opcode::DEC(Register::C),
            0x0E => Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Value(ValueType::u8),
            ),
            0x0F => Opcode::RRCA,
            0x10 => Opcode::STOP,
            0x11 => Opcode::LD(
                OpTarget::Register(Register::DE),
                OpTarget::Value(ValueType::u16),
            ),
            0x12 => Opcode::LD(
                OpTarget::Value(ValueType::deref(Register::DE)),
                OpTarget::Register(Register::A),
            ),
            0x13 => Opcode::INC(Register::DE),
            0x14 => Opcode::INC(Register::D),
            0x15 => Opcode::DEC(Register::D),
            0x16 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Value(ValueType::u8),
            ),
            0x17 => Opcode::RLA,
            0x18 => Opcode::JR(JumpCondition::None, ValueType::i8),
            0x19 => Opcode::ADD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::BC),
            ),
            0x1A => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::deref(Register::DE)),
            ),
            0x1B => Opcode::DEC(Register::DE),
            0x1C => Opcode::INC(Register::E),
            0x1D => Opcode::DEC(Register::E),
            0x1E => Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Value(ValueType::u8),
            ),
            0x1F => Opcode::RRA,
            0x20 => Opcode::JR(JumpCondition::NZ, ValueType::i8),
            0x21 => Opcode::LD(
                OpTarget::Register(Register::DE),
                OpTarget::Value(ValueType::u16),
            ),
            0x22 => Opcode::LD(
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Increment))),
                OpTarget::Value(ValueType::u16),
            ),
            0x23 => Opcode::INC(Register::HL(HLMode::Normal)),
            0x24 => Opcode::INC(Register::H),
            0x25 => Opcode::DEC(Register::H),
            0x26 => Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Value(ValueType::u8),
            ),
            0x27 => Opcode::DAA,
            0x28 => Opcode::JR(JumpCondition::Z, ValueType::i8),
            0x29 => Opcode::ADD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::SP),
            ),
            0x2A => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::HL(HLMode::Increment)),
            ),
            0x2B => Opcode::DEC(Register::HL(HLMode::Normal)),
            0x2C => Opcode::INC(Register::L),
            0x2D => Opcode::DEC(Register::L),
            0x2E => Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Value(ValueType::u8),
            ),
            0x2F => Opcode::CCF,
            0x30 => Opcode::JR(JumpCondition::NC, ValueType::i8),
            0x31 => Opcode::LD(
                OpTarget::Register(Register::SP),
                OpTarget::Value(ValueType::u16),
            ),
            0x32 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Decrement)),
                OpTarget::Register(Register::A),
            ),
            0x33 => Opcode::INC(Register::SP),
            0x34 => Opcode::INC(Register::HL(HLMode::Normal)),
            0x35 => Opcode::DEC(Register::HL(HLMode::Normal)),
            0x36 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Value(ValueType::u8),
            ),
            0x37 => Opcode::SCF,
            0x38 => Opcode::JR(JumpCondition::C, ValueType::i8),
            0x39 => Opcode::ADD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::SP),
            ),
            0x3A => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::HL(HLMode::Decrement)),
            ),
            0x3B => Opcode::DEC(Register::SP),
            0x3C => Opcode::INC(Register::A),
            0x3D => Opcode::DEC(Register::A),
            0x3E => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::u8),
            ),
            0x3F => Opcode::CCF,
            0x40 => Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::B),
            ),
            0x41 => Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::C),
            ),
            0x42 => Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::C),
            ),
            0x43 => Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::D),
            ),
            0x44 => Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::H),
            ),
            0x45 => Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::L),
            ),
            0x46 => Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x47 => Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::A),
            ),
            0x48 => Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::B),
            ),
            0x49 => Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::C),
            ),
            0x4A => Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::D),
            ),
            0x4B => Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::E),
            ),
            0x4C => Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::H),
            ),
            0x4D => Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::L),
            ),
            0x4E => Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x4F => Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::A),
            ),
            0x50 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::B),
            ),
            0x51 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::C),
            ),
            0x52 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::D),
            ),
            0x53 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::E),
            ),
            0x54 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::H),
            ),
            0x55 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::L),
            ),
            0x56 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x57 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::A),
            ),
            0x58 => Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::B),
            ),
            0x59 => Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::C),
            ),
            0x5A => Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::D),
            ),
            0x5B => Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::E),
            ),
            0x5C => Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::H),
            ),
            0x5D => Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::L),
            ),
            0x5E => Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x5F => Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::A),
            ),
            0x60 => Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::B),
            ),
            0x61 => Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::C),
            ),
            0x62 => Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::D),
            ),
            0x63 => Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::E),
            ),
            0x64 => Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::H),
            ),
            0x65 => Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::L),
            ),
            0x66 => Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x67 => Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::A),
            ),
            0x68 => Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::B),
            ),
            0x69 => Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::C),
            ),
            0x6A => Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::D),
            ),
            0x6B => Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::E),
            ),
            0x6C => Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::H),
            ),
            0x6D => Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::L),
            ),
            0x6E => Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x6F => Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::A),
            ),
            0x70 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::B),
            ),
            0x71 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::C),
            ),
            0x72 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::D),
            ),
            0x73 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::E),
            ),
            0x74 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::H),
            ),
            0x75 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::L),
            ),
            0x76 => Opcode::HALT,
            0x77 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::A),
            ),
            0x78 => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::B),
            ),
            0x79 => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::C),
            ),
            0x7A => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::D),
            ),
            0x7B => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::E),
            ),
            0x7C => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::H),
            ),
            0x7D => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::L),
            ),
            0x7E => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x7F => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::A),
            ),
            0x80 => Opcode::ADD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::B),
            ),
            0x81 => Opcode::ADD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::C),
            ),
            0x82 => Opcode::ADD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::D),
            ),
            0x83 => Opcode::ADD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::E),
            ),
            0x84 => Opcode::ADD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::H),
            ),
            0x85 => Opcode::ADD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::L),
            ),
            0x86 => Opcode::ADD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x87 => Opcode::ADD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::A),
            ),
            0x87 => Opcode::ADC(

            )

            _ => todo!(),
        }
    }
}

struct AccumulatorAndFlags {
    accumulator: u8,
    flags: Flags,
}

struct Flags {
    bits: u8,
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

struct BAndC {
    b: u8,
    c: u8,
}

struct DAndE {
    d: u8,
    e: u8,
}

struct HAndL {
    h: u8,
    l: u8,
}

struct StackPointer {
    stackpointer: u16,
}

struct ProgramCounter {
    programcounter: u16,
}
