#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum Mode {
    DMG,
    GBC,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum OpTarget {
    Register(Register),
    Value(ValueType),
}

#[derive(Debug)]
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

#[derive(Debug)]
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
    LDH,
    LD_HL,
    LD_SP,
    INC(Register),
    DEC(Register),
    ADD(Register, OpTarget),
    ADC(Register, OpTarget),
    ADD_SP,
    SUB(Register, OpTarget),
    SBC(Register, OpTarget),
    AND(Register, OpTarget),
    XOR(Register, OpTarget),
    OR(Register, OpTarget),
    CP(Register, OpTarget),
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
    JR(Condition, ValueType),
    RET(Condition),
    RETI,
    JP(Condition, OpTarget),
    CALL(Condition, ValueType),
    RST(RSTAddr),
    POP(Register),
    PUSH(Register),
    SWAP,
    DI,
    EI,
    RES,
    BIT,
    SET,
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
pub struct Registers {
    af: AccumulatorAndFlags,
    bc: BAndC,
    de: DAndE,
    hl: HAndL,
    sp: StackPointer,
    pc: ProgramCounter,
}

#[derive(Debug)]
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
    pub fn parse_opcode(&self, operation: u8, cb_opcode: Option<u8>) -> Opcode {
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
                Register::HL(HLMode::Normal),
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
            0x18 => Opcode::JR(Condition::None, ValueType::i8),
            0x19 => Opcode::ADD(
                Register::HL(HLMode::Normal),
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
            0x20 => Opcode::JR(Condition::NZ, ValueType::i8),
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
            0x28 => Opcode::JR(Condition::Z, ValueType::i8),
            0x29 => Opcode::ADD(
                Register::HL(HLMode::Normal),
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
            0x30 => Opcode::JR(Condition::NC, ValueType::i8),
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
            0x38 => Opcode::JR(Condition::C, ValueType::i8),
            0x39 => Opcode::ADD(
                Register::HL(HLMode::Normal),
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
            0x80 => Opcode::ADD(Register::A, OpTarget::Register(Register::B)),
            0x81 => Opcode::ADD(Register::A, OpTarget::Register(Register::C)),
            0x82 => Opcode::ADD(Register::A, OpTarget::Register(Register::D)),
            0x83 => Opcode::ADD(Register::A, OpTarget::Register(Register::E)),
            0x84 => Opcode::ADD(Register::A, OpTarget::Register(Register::H)),
            0x85 => Opcode::ADD(Register::A, OpTarget::Register(Register::L)),
            0x86 => Opcode::ADD(
                Register::A,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x87 => Opcode::ADD(Register::A, OpTarget::Register(Register::A)),
            0x88 => Opcode::ADC(Register::A, OpTarget::Register(Register::B)),
            0x89 => Opcode::ADC(Register::A, OpTarget::Register(Register::C)),
            0x8A => Opcode::ADC(Register::A, OpTarget::Register(Register::D)),
            0x8B => Opcode::ADC(Register::A, OpTarget::Register(Register::E)),
            0x8C => Opcode::ADC(Register::A, OpTarget::Register(Register::H)),
            0x8D => Opcode::ADC(Register::A, OpTarget::Register(Register::L)),
            0x8E => Opcode::ADC(
                Register::A,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x8F => Opcode::ADC(Register::A, OpTarget::Register(Register::A)),
            0x90 => Opcode::SUB(Register::A, OpTarget::Register(Register::B)),
            0x91 => Opcode::SUB(Register::A, OpTarget::Register(Register::C)),
            0x92 => Opcode::SUB(Register::A, OpTarget::Register(Register::D)),
            0x93 => Opcode::SUB(Register::A, OpTarget::Register(Register::E)),
            0x94 => Opcode::SUB(Register::A, OpTarget::Register(Register::H)),
            0x95 => Opcode::SUB(Register::A, OpTarget::Register(Register::L)),
            0x96 => Opcode::SUB(
                Register::A,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x97 => Opcode::SUB(Register::A, OpTarget::Register(Register::A)),
            0x98 => Opcode::SBC(Register::A, OpTarget::Register(Register::B)),
            0x99 => Opcode::SBC(Register::A, OpTarget::Register(Register::C)),
            0x9A => Opcode::SBC(Register::A, OpTarget::Register(Register::D)),
            0x9B => Opcode::SBC(Register::A, OpTarget::Register(Register::E)),
            0x9C => Opcode::SBC(Register::A, OpTarget::Register(Register::H)),
            0x9D => Opcode::SBC(Register::A, OpTarget::Register(Register::L)),
            0x9E => Opcode::SBC(
                Register::A,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0x9F => Opcode::SBC(Register::A, OpTarget::Register(Register::A)),
            0xA0 => Opcode::AND(Register::A, OpTarget::Register(Register::B)),
            0xA1 => Opcode::AND(Register::A, OpTarget::Register(Register::C)),
            0xA2 => Opcode::AND(Register::A, OpTarget::Register(Register::D)),
            0xA3 => Opcode::AND(Register::A, OpTarget::Register(Register::E)),
            0xA4 => Opcode::AND(Register::A, OpTarget::Register(Register::H)),
            0xA5 => Opcode::AND(Register::A, OpTarget::Register(Register::L)),
            0xA6 => Opcode::AND(
                Register::A,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0xA7 => Opcode::AND(Register::A, OpTarget::Register(Register::A)),
            0xA8 => Opcode::XOR(Register::A, OpTarget::Register(Register::B)),
            0xA9 => Opcode::XOR(Register::A, OpTarget::Register(Register::C)),
            0xAA => Opcode::XOR(Register::A, OpTarget::Register(Register::D)),
            0xAB => Opcode::XOR(Register::A, OpTarget::Register(Register::E)),
            0xAC => Opcode::XOR(Register::A, OpTarget::Register(Register::H)),
            0xAD => Opcode::XOR(Register::A, OpTarget::Register(Register::L)),
            0xAE => Opcode::XOR(
                Register::A,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0xAF => Opcode::XOR(Register::A, OpTarget::Register(Register::A)),
            0xB0 => Opcode::OR(Register::A, OpTarget::Register(Register::B)),
            0xB1 => Opcode::OR(Register::A, OpTarget::Register(Register::C)),
            0xB2 => Opcode::OR(Register::A, OpTarget::Register(Register::D)),
            0xB3 => Opcode::OR(Register::A, OpTarget::Register(Register::E)),
            0xB4 => Opcode::OR(Register::A, OpTarget::Register(Register::H)),
            0xB5 => Opcode::OR(Register::A, OpTarget::Register(Register::L)),
            0xB6 => Opcode::OR(
                Register::A,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0xB7 => Opcode::OR(Register::A, OpTarget::Register(Register::A)),
            0xB8 => Opcode::CP(Register::A, OpTarget::Register(Register::B)),
            0xB9 => Opcode::CP(Register::A, OpTarget::Register(Register::C)),
            0xBA => Opcode::CP(Register::A, OpTarget::Register(Register::D)),
            0xBB => Opcode::CP(Register::A, OpTarget::Register(Register::E)),
            0xBC => Opcode::CP(Register::A, OpTarget::Register(Register::H)),
            0xBD => Opcode::CP(Register::A, OpTarget::Register(Register::L)),
            0xBE => Opcode::CP(
                Register::A,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0xBF => Opcode::CP(Register::A, OpTarget::Register(Register::A)),
            0xC0 => Opcode::RET(Condition::NZ),
            0xC1 => Opcode::POP(Register::BC),
            0xC2 => Opcode::JP(Condition::NZ, OpTarget::Value(ValueType::u16)),
            0xC3 => Opcode::JP(Condition::None, OpTarget::Value(ValueType::u16)),
            0xC4 => Opcode::CALL(Condition::NZ, ValueType::u16),
            0xC5 => Opcode::PUSH(Register::BC),
            0xC6 => Opcode::ADD(Register::A, OpTarget::Value(ValueType::u8)),
            0xC7 => Opcode::RST(RSTAddr::H00),
            0xC8 => Opcode::RET(Condition::Z),
            0xC9 => Opcode::RET(Condition::None),
            0xCA => Opcode::JP(Condition::Z, OpTarget::Value(ValueType::u16)),
            0xCB => match cb_opcode {
                Some(oc) => Opcode::CB(self.parse_cb_prefix(oc)),
                None => panic!("CB opcode sent with no cb opcode"),
            },
            0xCC => Opcode::CALL(Condition::Z, ValueType::u16),
            0xCD => Opcode::CALL(Condition::None, ValueType::u16),
            0xCE => Opcode::ADC(Register::A, OpTarget::Value(ValueType::u8)),
            0xCF => Opcode::RST(RSTAddr::H08),
            0xD0 => Opcode::RET(Condition::NC),
            0xD1 => Opcode::POP(Register::DE),
            0xD2 => Opcode::JP(Condition::NC, OpTarget::Value(ValueType::u16)),
            0xD3 => Opcode::HALT,
            0xD4 => Opcode::CALL(Condition::NC, ValueType::u16),
            0xD5 => Opcode::PUSH(Register::DE),
            0xD6 => Opcode::SUB(Register::A, OpTarget::Value(ValueType::u8)),
            0xD7 => Opcode::RST(RSTAddr::H10),
            0xD8 => Opcode::RET(Condition::C),
            0xD9 => Opcode::RETI,
            0xDA => Opcode::JP(Condition::C, OpTarget::Value(ValueType::u16)),
            0xDB => Opcode::HALT,
            0xDC => Opcode::CALL(Condition::C, ValueType::u16),
            0xDD => Opcode::HALT,
            0xDE => Opcode::SBC(Register::A, OpTarget::Value(ValueType::u8)),
            0xDF => Opcode::RST(RSTAddr::H18),
            0xE0 => Opcode::LD(
                OpTarget::Value(ValueType::ff00_plus_u8_deref),
                OpTarget::Register(Register::A),
            ),
            0xE1 => Opcode::POP(Register::HL(HLMode::Normal)),
            0xE2 => Opcode::LD(
                OpTarget::Value(ValueType::ff00_plus_register_deref(Register::C)),
                OpTarget::Register(Register::A),
            ),
            0xE3 => Opcode::HALT,
            0xE4 => Opcode::HALT,
            0xE5 => Opcode::PUSH(Register::HL(HLMode::Normal)),
            0xE6 => Opcode::SUB(Register::A, OpTarget::Value(ValueType::u8)),
            0xE7 => Opcode::RST(RSTAddr::H20),
            0xE8 => Opcode::ADD(Register::SP, OpTarget::Value(ValueType::i8)),
            0xE9 => Opcode::JP(
                Condition::None,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0xEA => Opcode::LD(
                OpTarget::Value(ValueType::u16_deref),
                OpTarget::Register(Register::A),
            ),
            0xEB => Opcode::HALT,
            0xEC => Opcode::HALT,
            0xED => Opcode::HALT,
            0xEE => Opcode::XOR(Register::A, OpTarget::Value(ValueType::u8)),
            0xEF => Opcode::RST(RSTAddr::H28),
            0xF0 => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::ff00_plus_u8_deref),
            ),
            0xF1 => Opcode::POP(Register::AF),
            0xF2 => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::ff00_plus_register_deref(Register::C)),
            ),
            0xF3 => Opcode::DI,
            0xF4 => Opcode::HALT,
            0xF5 => Opcode::PUSH(Register::AF),
            0xF6 => Opcode::OR(Register::A, OpTarget::Value(ValueType::u8)),
            0xF7 => Opcode::RST(RSTAddr::H30),
            0xF8 => Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::SPPlusi8),
            ),
            0xF9 => Opcode::LD(
                OpTarget::Register(Register::SP),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            0xFA => Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::u16_deref),
            ),
            0xFB => Opcode::EI,
            0xFC => Opcode::HALT,
            0xFD => Opcode::HALT,
            0xFE => Opcode::CP(Register::A, OpTarget::Value(ValueType::u8)),
            0xFF => Opcode::RST(RSTAddr::H38),
        }
    }
    fn parse_cb_prefix(&self, cb_opcode: u8) -> CBPrefix {
        match cb_opcode {
            0x00 => CBPrefix::RLC(OpTarget::Register(Register::B)),
            0x01 => CBPrefix::RLC(OpTarget::Register(Register::C)),
            0x02 => CBPrefix::RLC(OpTarget::Register(Register::D)),
            0x03 => CBPrefix::RLC(OpTarget::Register(Register::E)),
            0x04 => CBPrefix::RLC(OpTarget::Register(Register::H)),
            0x05 => CBPrefix::RLC(OpTarget::Register(Register::L)),
            0x06 => CBPrefix::RLC(OpTarget::Value(ValueType::deref(Register::HL(
                HLMode::Normal,
            )))),
            0x07 => CBPrefix::RLC(OpTarget::Register(Register::A)),
            0x08 => CBPrefix::RRC(OpTarget::Register(Register::B)),
            0x09 => CBPrefix::RRC(OpTarget::Register(Register::C)),
            0x0A => CBPrefix::RRC(OpTarget::Register(Register::D)),
            0x0B => CBPrefix::RRC(OpTarget::Register(Register::E)),
            0x0C => CBPrefix::RRC(OpTarget::Register(Register::H)),
            0x0D => CBPrefix::RRC(OpTarget::Register(Register::L)),
            0x0E => CBPrefix::RRC(OpTarget::Value(ValueType::deref(Register::HL(
                HLMode::Normal,
            )))),
            0x0F => CBPrefix::RRC(OpTarget::Register(Register::A)),
            0x10 => CBPrefix::RL(OpTarget::Register(Register::B)),
            0x11 => CBPrefix::RL(OpTarget::Register(Register::C)),
            0x12 => CBPrefix::RL(OpTarget::Register(Register::D)),
            0x13 => CBPrefix::RL(OpTarget::Register(Register::E)),
            0x14 => CBPrefix::RL(OpTarget::Register(Register::H)),
            0x15 => CBPrefix::RL(OpTarget::Register(Register::L)),
            0x16 => CBPrefix::RL(OpTarget::Value(ValueType::deref(Register::HL(
                HLMode::Normal,
            )))),
            0x17 => CBPrefix::RL(OpTarget::Register(Register::A)),
            0x18 => CBPrefix::RR(OpTarget::Register(Register::B)),
            0x19 => CBPrefix::RR(OpTarget::Register(Register::C)),
            0x1A => CBPrefix::RR(OpTarget::Register(Register::D)),
            0x1B => CBPrefix::RR(OpTarget::Register(Register::E)),
            0x1C => CBPrefix::RR(OpTarget::Register(Register::H)),
            0x1D => CBPrefix::RR(OpTarget::Register(Register::L)),
            0x1E => CBPrefix::RR(OpTarget::Value(ValueType::deref(Register::HL(
                HLMode::Normal,
            )))),
            0x1F => CBPrefix::RR(OpTarget::Register(Register::A)),
            0x20 => CBPrefix::SLA(OpTarget::Register(Register::B)),
            0x21 => CBPrefix::SLA(OpTarget::Register(Register::C)),
            0x22 => CBPrefix::SLA(OpTarget::Register(Register::D)),
            0x23 => CBPrefix::SLA(OpTarget::Register(Register::E)),
            0x24 => CBPrefix::SLA(OpTarget::Register(Register::H)),
            0x25 => CBPrefix::SLA(OpTarget::Register(Register::L)),
            0x26 => CBPrefix::SLA(OpTarget::Value(ValueType::deref(Register::HL(
                HLMode::Normal,
            )))),
            0x27 => CBPrefix::SLA(OpTarget::Register(Register::A)),
            0x28 => CBPrefix::SRA(OpTarget::Register(Register::B)),
            0x29 => CBPrefix::SRA(OpTarget::Register(Register::C)),
            0x2A => CBPrefix::SRA(OpTarget::Register(Register::D)),
            0x2B => CBPrefix::SRA(OpTarget::Register(Register::E)),
            0x2C => CBPrefix::SRA(OpTarget::Register(Register::H)),
            0x2D => CBPrefix::SRA(OpTarget::Register(Register::L)),
            0x2E => CBPrefix::SRA(OpTarget::Value(ValueType::deref(Register::HL(
                HLMode::Normal,
            )))),
            0x2F => CBPrefix::SRA(OpTarget::Register(Register::A)),
            0x30 => CBPrefix::SWAP(OpTarget::Register(Register::B)),
            0x31 => CBPrefix::SWAP(OpTarget::Register(Register::C)),
            0x32 => CBPrefix::SWAP(OpTarget::Register(Register::D)),
            0x33 => CBPrefix::SWAP(OpTarget::Register(Register::E)),
            0x34 => CBPrefix::SWAP(OpTarget::Register(Register::H)),
            0x35 => CBPrefix::SWAP(OpTarget::Register(Register::L)),
            0x36 => CBPrefix::SWAP(OpTarget::Value(ValueType::deref(Register::HL(
                HLMode::Normal,
            )))),
            0x37 => CBPrefix::SWAP(OpTarget::Register(Register::A)),
            0x38 => CBPrefix::SRL(OpTarget::Register(Register::B)),
            0x39 => CBPrefix::SRL(OpTarget::Register(Register::C)),
            0x3A => CBPrefix::SRL(OpTarget::Register(Register::D)),
            0x3B => CBPrefix::SRL(OpTarget::Register(Register::E)),
            0x3C => CBPrefix::SRL(OpTarget::Register(Register::H)),
            0x3D => CBPrefix::SRL(OpTarget::Register(Register::L)),
            0x3E => CBPrefix::SRL(OpTarget::Value(ValueType::deref(Register::HL(
                HLMode::Normal,
            )))),
            0x3F => CBPrefix::SRL(OpTarget::Register(Register::A)),
            0x40 => CBPrefix::BIT(0, OpTarget::Register(Register::B)),
            0x41 => CBPrefix::BIT(0, OpTarget::Register(Register::C)),
            0x42 => CBPrefix::BIT(0, OpTarget::Register(Register::D)),
            0x43 => CBPrefix::BIT(0, OpTarget::Register(Register::E)),
            0x44 => CBPrefix::BIT(0, OpTarget::Register(Register::H)),
            0x45 => CBPrefix::BIT(0, OpTarget::Register(Register::L)),
            0x46 => CBPrefix::BIT(
                0,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x47 => CBPrefix::BIT(0, OpTarget::Register(Register::A)),
            0x48 => CBPrefix::BIT(1, OpTarget::Register(Register::B)),
            0x49 => CBPrefix::BIT(1, OpTarget::Register(Register::C)),
            0x4A => CBPrefix::BIT(1, OpTarget::Register(Register::D)),
            0x4B => CBPrefix::BIT(1, OpTarget::Register(Register::E)),
            0x4C => CBPrefix::BIT(1, OpTarget::Register(Register::H)),
            0x4D => CBPrefix::BIT(1, OpTarget::Register(Register::L)),
            0x4E => CBPrefix::BIT(
                1,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x4F => CBPrefix::BIT(1, OpTarget::Register(Register::A)),
            0x50 => CBPrefix::BIT(2, OpTarget::Register(Register::B)),
            0x51 => CBPrefix::BIT(2, OpTarget::Register(Register::C)),
            0x52 => CBPrefix::BIT(2, OpTarget::Register(Register::D)),
            0x53 => CBPrefix::BIT(2, OpTarget::Register(Register::E)),
            0x54 => CBPrefix::BIT(2, OpTarget::Register(Register::H)),
            0x55 => CBPrefix::BIT(2, OpTarget::Register(Register::L)),
            0x56 => CBPrefix::BIT(
                2,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x57 => CBPrefix::BIT(2, OpTarget::Register(Register::A)),
            0x58 => CBPrefix::BIT(3, OpTarget::Register(Register::B)),
            0x59 => CBPrefix::BIT(3, OpTarget::Register(Register::C)),
            0x5A => CBPrefix::BIT(3, OpTarget::Register(Register::D)),
            0x5B => CBPrefix::BIT(3, OpTarget::Register(Register::E)),
            0x5C => CBPrefix::BIT(3, OpTarget::Register(Register::H)),
            0x5D => CBPrefix::BIT(3, OpTarget::Register(Register::L)),
            0x5E => CBPrefix::BIT(
                3,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x5F => CBPrefix::BIT(3, OpTarget::Register(Register::A)),
            0x60 => CBPrefix::BIT(4, OpTarget::Register(Register::B)),
            0x61 => CBPrefix::BIT(4, OpTarget::Register(Register::C)),
            0x62 => CBPrefix::BIT(4, OpTarget::Register(Register::D)),
            0x63 => CBPrefix::BIT(4, OpTarget::Register(Register::E)),
            0x64 => CBPrefix::BIT(4, OpTarget::Register(Register::H)),
            0x65 => CBPrefix::BIT(4, OpTarget::Register(Register::L)),
            0x66 => CBPrefix::BIT(
                4,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x67 => CBPrefix::BIT(4, OpTarget::Register(Register::A)),
            0x68 => CBPrefix::BIT(5, OpTarget::Register(Register::B)),
            0x69 => CBPrefix::BIT(5, OpTarget::Register(Register::C)),
            0x6A => CBPrefix::BIT(5, OpTarget::Register(Register::D)),
            0x6B => CBPrefix::BIT(5, OpTarget::Register(Register::E)),
            0x6C => CBPrefix::BIT(5, OpTarget::Register(Register::H)),
            0x6D => CBPrefix::BIT(5, OpTarget::Register(Register::L)),
            0x6E => CBPrefix::BIT(
                5,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x6F => CBPrefix::BIT(5, OpTarget::Register(Register::A)),
            0x70 => CBPrefix::BIT(6, OpTarget::Register(Register::B)),
            0x71 => CBPrefix::BIT(6, OpTarget::Register(Register::C)),
            0x72 => CBPrefix::BIT(6, OpTarget::Register(Register::D)),
            0x73 => CBPrefix::BIT(6, OpTarget::Register(Register::E)),
            0x74 => CBPrefix::BIT(6, OpTarget::Register(Register::H)),
            0x75 => CBPrefix::BIT(6, OpTarget::Register(Register::L)),
            0x76 => CBPrefix::BIT(
                6,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x77 => CBPrefix::BIT(6, OpTarget::Register(Register::A)),
            0x78 => CBPrefix::BIT(7, OpTarget::Register(Register::B)),
            0x79 => CBPrefix::BIT(7, OpTarget::Register(Register::C)),
            0x7A => CBPrefix::BIT(7, OpTarget::Register(Register::D)),
            0x7B => CBPrefix::BIT(7, OpTarget::Register(Register::E)),
            0x7C => CBPrefix::BIT(7, OpTarget::Register(Register::H)),
            0x7D => CBPrefix::BIT(7, OpTarget::Register(Register::L)),
            0x7E => CBPrefix::BIT(
                7,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x7F => CBPrefix::BIT(7, OpTarget::Register(Register::A)),
            0x80 => CBPrefix::RES(0, OpTarget::Register(Register::B)),
            0x81 => CBPrefix::RES(0, OpTarget::Register(Register::C)),
            0x82 => CBPrefix::RES(0, OpTarget::Register(Register::D)),
            0x83 => CBPrefix::RES(0, OpTarget::Register(Register::E)),
            0x84 => CBPrefix::RES(0, OpTarget::Register(Register::H)),
            0x85 => CBPrefix::RES(0, OpTarget::Register(Register::L)),
            0x86 => CBPrefix::RES(
                0,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x87 => CBPrefix::RES(0, OpTarget::Register(Register::A)),
            0x88 => CBPrefix::RES(1, OpTarget::Register(Register::B)),
            0x89 => CBPrefix::RES(1, OpTarget::Register(Register::C)),
            0x8A => CBPrefix::RES(1, OpTarget::Register(Register::D)),
            0x8B => CBPrefix::RES(1, OpTarget::Register(Register::E)),
            0x8C => CBPrefix::RES(1, OpTarget::Register(Register::H)),
            0x8D => CBPrefix::RES(1, OpTarget::Register(Register::L)),
            0x8E => CBPrefix::RES(
                1,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x8F => CBPrefix::RES(1, OpTarget::Register(Register::A)),
            0x90 => CBPrefix::RES(2, OpTarget::Register(Register::B)),
            0x91 => CBPrefix::RES(2, OpTarget::Register(Register::C)),
            0x92 => CBPrefix::RES(2, OpTarget::Register(Register::D)),
            0x93 => CBPrefix::RES(2, OpTarget::Register(Register::E)),
            0x94 => CBPrefix::RES(2, OpTarget::Register(Register::H)),
            0x95 => CBPrefix::RES(2, OpTarget::Register(Register::L)),
            0x96 => CBPrefix::RES(
                2,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x97 => CBPrefix::RES(2, OpTarget::Register(Register::A)),
            0x98 => CBPrefix::RES(3, OpTarget::Register(Register::B)),
            0x99 => CBPrefix::RES(3, OpTarget::Register(Register::C)),
            0x9A => CBPrefix::RES(3, OpTarget::Register(Register::D)),
            0x9B => CBPrefix::RES(3, OpTarget::Register(Register::E)),
            0x9C => CBPrefix::RES(3, OpTarget::Register(Register::H)),
            0x9D => CBPrefix::RES(3, OpTarget::Register(Register::L)),
            0x9E => CBPrefix::RES(
                3,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0x9F => CBPrefix::RES(3, OpTarget::Register(Register::A)),
            0xA0 => CBPrefix::RES(4, OpTarget::Register(Register::B)),
            0xA1 => CBPrefix::RES(4, OpTarget::Register(Register::C)),
            0xA2 => CBPrefix::RES(4, OpTarget::Register(Register::D)),
            0xA3 => CBPrefix::RES(4, OpTarget::Register(Register::E)),
            0xA4 => CBPrefix::RES(4, OpTarget::Register(Register::H)),
            0xA5 => CBPrefix::RES(4, OpTarget::Register(Register::L)),
            0xA6 => CBPrefix::RES(
                4,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xA7 => CBPrefix::RES(4, OpTarget::Register(Register::A)),
            0xA8 => CBPrefix::RES(5, OpTarget::Register(Register::B)),
            0xA9 => CBPrefix::RES(5, OpTarget::Register(Register::C)),
            0xAA => CBPrefix::RES(5, OpTarget::Register(Register::D)),
            0xAB => CBPrefix::RES(5, OpTarget::Register(Register::E)),
            0xAC => CBPrefix::RES(5, OpTarget::Register(Register::H)),
            0xAD => CBPrefix::RES(5, OpTarget::Register(Register::L)),
            0xAE => CBPrefix::RES(
                5,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xAF => CBPrefix::RES(5, OpTarget::Register(Register::A)),
            0xB0 => CBPrefix::RES(6, OpTarget::Register(Register::B)),
            0xB1 => CBPrefix::RES(6, OpTarget::Register(Register::C)),
            0xB2 => CBPrefix::RES(6, OpTarget::Register(Register::D)),
            0xB3 => CBPrefix::RES(6, OpTarget::Register(Register::E)),
            0xB4 => CBPrefix::RES(6, OpTarget::Register(Register::H)),
            0xB5 => CBPrefix::RES(6, OpTarget::Register(Register::L)),
            0xB6 => CBPrefix::RES(
                6,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xB7 => CBPrefix::RES(6, OpTarget::Register(Register::A)),
            0xB8 => CBPrefix::RES(7, OpTarget::Register(Register::B)),
            0xB9 => CBPrefix::RES(7, OpTarget::Register(Register::C)),
            0xBA => CBPrefix::RES(7, OpTarget::Register(Register::D)),
            0xBB => CBPrefix::RES(7, OpTarget::Register(Register::E)),
            0xBC => CBPrefix::RES(7, OpTarget::Register(Register::H)),
            0xBD => CBPrefix::RES(7, OpTarget::Register(Register::L)),
            0xBE => CBPrefix::RES(
                7,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xBF => CBPrefix::RES(7, OpTarget::Register(Register::A)),
            0xC0 => CBPrefix::SET(0, OpTarget::Register(Register::B)),
            0xC1 => CBPrefix::SET(0, OpTarget::Register(Register::C)),
            0xC2 => CBPrefix::SET(0, OpTarget::Register(Register::D)),
            0xC3 => CBPrefix::SET(0, OpTarget::Register(Register::E)),
            0xC4 => CBPrefix::SET(0, OpTarget::Register(Register::H)),
            0xC5 => CBPrefix::SET(0, OpTarget::Register(Register::L)),
            0xC6 => CBPrefix::SET(
                0,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xC7 => CBPrefix::SET(0, OpTarget::Register(Register::A)),
            0xC8 => CBPrefix::SET(1, OpTarget::Register(Register::B)),
            0xC9 => CBPrefix::SET(1, OpTarget::Register(Register::C)),
            0xCA => CBPrefix::SET(1, OpTarget::Register(Register::D)),
            0xCB => CBPrefix::SET(1, OpTarget::Register(Register::E)),
            0xCC => CBPrefix::SET(1, OpTarget::Register(Register::H)),
            0xCD => CBPrefix::SET(1, OpTarget::Register(Register::L)),
            0xCE => CBPrefix::SET(
                1,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xCF => CBPrefix::SET(1, OpTarget::Register(Register::A)),
            0xD0 => CBPrefix::SET(2, OpTarget::Register(Register::B)),
            0xD1 => CBPrefix::SET(2, OpTarget::Register(Register::C)),
            0xD2 => CBPrefix::SET(2, OpTarget::Register(Register::D)),
            0xD3 => CBPrefix::SET(2, OpTarget::Register(Register::E)),
            0xD4 => CBPrefix::SET(2, OpTarget::Register(Register::H)),
            0xD5 => CBPrefix::SET(2, OpTarget::Register(Register::L)),
            0xD6 => CBPrefix::SET(
                2,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xD7 => CBPrefix::SET(2, OpTarget::Register(Register::A)),
            0xD8 => CBPrefix::SET(3, OpTarget::Register(Register::B)),
            0xD9 => CBPrefix::SET(3, OpTarget::Register(Register::C)),
            0xDA => CBPrefix::SET(3, OpTarget::Register(Register::D)),
            0xDB => CBPrefix::SET(3, OpTarget::Register(Register::E)),
            0xDC => CBPrefix::SET(3, OpTarget::Register(Register::H)),
            0xDD => CBPrefix::SET(3, OpTarget::Register(Register::L)),
            0xDE => CBPrefix::SET(
                3,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xDF => CBPrefix::SET(3, OpTarget::Register(Register::A)),
            0xE0 => CBPrefix::SET(4, OpTarget::Register(Register::B)),
            0xE1 => CBPrefix::SET(4, OpTarget::Register(Register::C)),
            0xE2 => CBPrefix::SET(4, OpTarget::Register(Register::D)),
            0xE3 => CBPrefix::SET(4, OpTarget::Register(Register::E)),
            0xE4 => CBPrefix::SET(4, OpTarget::Register(Register::H)),
            0xE5 => CBPrefix::SET(4, OpTarget::Register(Register::L)),
            0xE6 => CBPrefix::SET(
                4,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xE7 => CBPrefix::SET(4, OpTarget::Register(Register::A)),
            0xE8 => CBPrefix::SET(5, OpTarget::Register(Register::B)),
            0xE9 => CBPrefix::SET(5, OpTarget::Register(Register::C)),
            0xEA => CBPrefix::SET(5, OpTarget::Register(Register::D)),
            0xEB => CBPrefix::SET(5, OpTarget::Register(Register::E)),
            0xEC => CBPrefix::SET(5, OpTarget::Register(Register::H)),
            0xED => CBPrefix::SET(5, OpTarget::Register(Register::L)),
            0xEE => CBPrefix::SET(
                5,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xEF => CBPrefix::SET(5, OpTarget::Register(Register::A)),
            0xF0 => CBPrefix::SET(6, OpTarget::Register(Register::B)),
            0xF1 => CBPrefix::SET(6, OpTarget::Register(Register::C)),
            0xF2 => CBPrefix::SET(6, OpTarget::Register(Register::D)),
            0xF3 => CBPrefix::SET(6, OpTarget::Register(Register::E)),
            0xF4 => CBPrefix::SET(6, OpTarget::Register(Register::H)),
            0xF5 => CBPrefix::SET(6, OpTarget::Register(Register::L)),
            0xF6 => CBPrefix::SET(
                6,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xF7 => CBPrefix::SET(6, OpTarget::Register(Register::A)),
            0xF8 => CBPrefix::SET(7, OpTarget::Register(Register::B)),
            0xF9 => CBPrefix::SET(7, OpTarget::Register(Register::C)),
            0xFA => CBPrefix::SET(7, OpTarget::Register(Register::D)),
            0xFB => CBPrefix::SET(7, OpTarget::Register(Register::E)),
            0xFC => CBPrefix::SET(7, OpTarget::Register(Register::H)),
            0xFD => CBPrefix::SET(7, OpTarget::Register(Register::L)),
            0xFE => CBPrefix::SET(
                7,
                OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            ),
            0xFF => CBPrefix::SET(7, OpTarget::Register(Register::A)),
        }
    }
}

#[derive(Debug)]
struct AccumulatorAndFlags {
    accumulator: u8,
    flags: Flags,
}

#[derive(Debug)]
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

#[derive(Debug)]
struct BAndC {
    b: u8,
    c: u8,
}

#[derive(Debug)]
struct DAndE {
    d: u8,
    e: u8,
}

#[derive(Debug)]
struct HAndL {
    h: u8,
    l: u8,
}

#[derive(Debug)]
struct StackPointer {
    stackpointer: u16,
}

#[derive(Debug)]
struct ProgramCounter {
    programcounter: u16,
}
