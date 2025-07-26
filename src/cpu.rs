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
