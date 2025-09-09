use crate::cpu::*;

#[allow(dead_code)]
pub fn parse_opcode(operation: u8, cb_opcode: Option<u8>) -> Operation {
    match operation {
        0x00 => Operation::new(Opcode::NOP, OpLength::One, Timing::Four),
        0x01 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::BC),
                OpTarget::Value(ValueType::u16),
            ),
            OpLength::Three,
            Timing::Twelve,
        ),
        0x02 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::BC))),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x03 => Operation::new(
            Opcode::INC(OpTarget::Register(Register::BC)),
            OpLength::One,
            Timing::Eight,
        ),
        0x04 => Operation::new(
            Opcode::INC(OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0x05 => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0x06 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Value(ValueType::u8),
            ),
            OpLength::Two,
            Timing::Eight,
        ),
        0x07 => Operation::new(Opcode::RLCA, OpLength::One, Timing::Four),
        0x08 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::u16)),
                OpTarget::Register(Register::SP),
            ),
            OpLength::Three,
            Timing::Twenty,
        ),
        0x09 => Operation::new(
            Opcode::ADD(
                Register::HL(HLMode::Normal),
                OpTarget::Register(Register::BC),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x0A => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::BC))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x0B => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::BC)),
            OpLength::One,
            Timing::Eight,
        ),
        0x0C => Operation::new(
            Opcode::INC(OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0x0D => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0x0E => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Value(ValueType::u8),
            ),
            OpLength::Two,
            Timing::Eight,
        ),
        0x0F => Operation::new(Opcode::RRCA, OpLength::One, Timing::Four),
        0x10 => Operation::new(Opcode::STOP, OpLength::One, Timing::Four),
        0x11 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::DE),
                OpTarget::Value(ValueType::u16),
            ),
            OpLength::Three,
            Timing::Twelve,
        ),
        0x12 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::DE))),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x13 => Operation::new(
            Opcode::INC(OpTarget::Register(Register::DE)),
            OpLength::One,
            Timing::Eight,
        ),
        0x14 => Operation::new(
            Opcode::INC(OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0x15 => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0x16 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Value(ValueType::u8),
            ),
            OpLength::Two,
            Timing::Eight,
        ),
        0x17 => Operation::new(Opcode::RLA, OpLength::One, Timing::Four),
        0x18 => Operation::new(Opcode::JR(Condition::None), OpLength::Two, Timing::Twelve),
        0x19 => Operation::new(
            Opcode::ADD(
                Register::HL(HLMode::Normal),
                OpTarget::Register(Register::DE),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x1A => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::DE))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x1B => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::DE)),
            OpLength::One,
            Timing::Eight,
        ),
        0x1C => Operation::new(
            Opcode::INC(OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0x1D => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0x1E => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Value(ValueType::u8),
            ),
            OpLength::Two,
            Timing::Eight,
        ),
        0x1F => Operation::new(Opcode::RRA, OpLength::One, Timing::Four),
        0x20 => Operation::new(
            Opcode::JR(Condition::NZ),
            OpLength::Two,
            Timing::Variable(Box::new((Timing::Eight, Timing::Twelve))),
        ),
        0x21 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Value(ValueType::u16),
            ),
            OpLength::Three,
            Timing::Twelve,
        ),
        0x22 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Increment,
                )))),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x23 => Operation::new(
            Opcode::INC(OpTarget::Register(Register::HL(HLMode::Normal))),
            OpLength::One,
            Timing::Eight,
        ),
        0x24 => Operation::new(
            Opcode::INC(OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0x25 => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0x26 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Value(ValueType::u8),
            ),
            OpLength::Two,
            Timing::Eight,
        ),
        0x27 => Operation::new(Opcode::DAA, OpLength::One, Timing::Four),
        0x28 => Operation::new(
            Opcode::JR(Condition::Z),
            OpLength::Two,
            Timing::Variable(Box::new((Timing::Eight, Timing::Twelve))),
        ),
        0x29 => Operation::new(
            Opcode::ADD(
                Register::HL(HLMode::Normal),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x2A => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Increment,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x2B => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::HL(HLMode::Normal))),
            OpLength::One,
            Timing::Eight,
        ),
        0x2C => Operation::new(
            Opcode::INC(OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0x2D => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0x2E => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Value(ValueType::u8),
            ),
            OpLength::Two,
            Timing::Eight,
        ),
        0x2F => Operation::new(Opcode::CPL, OpLength::One, Timing::Four),
        0x30 => Operation::new(
            Opcode::JR(Condition::NC),
            OpLength::Two,
            Timing::Variable(Box::new((Timing::Eight, Timing::Twelve))),
        ),
        0x31 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::SP),
                OpTarget::Value(ValueType::u16),
            ),
            OpLength::Three,
            Timing::Twelve,
        ),
        0x32 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Decrement,
                )))),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x33 => Operation::new(
            Opcode::INC(OpTarget::Register(Register::SP)),
            OpLength::One,
            Timing::Eight,
        ),
        0x34 => Operation::new(
            Opcode::INC(OpTarget::Value(ValueType::deref(DerefSource::Register(
                Register::HL(HLMode::Normal),
            )))),
            OpLength::One,
            Timing::Twelve,
        ),
        0x35 => Operation::new(
            Opcode::DEC(OpTarget::Value(ValueType::deref(DerefSource::Register(
                Register::HL(HLMode::Normal),
            )))),
            OpLength::One,
            Timing::Twelve,
        ),
        0x36 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
                OpTarget::Value(ValueType::u8),
            ),
            OpLength::Two,
            Timing::Twelve,
        ),
        0x37 => Operation::new(Opcode::SCF, OpLength::One, Timing::Four),
        0x38 => Operation::new(
            Opcode::JR(Condition::C),
            OpLength::Two,
            Timing::Variable(Box::new((Timing::Eight, Timing::Twelve))),
        ),
        0x39 => Operation::new(
            Opcode::ADD(
                Register::HL(HLMode::Normal),
                OpTarget::Register(Register::SP),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x3A => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Decrement,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x3B => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::SP)),
            OpLength::One,
            Timing::Eight,
        ),
        0x3C => Operation::new(
            Opcode::INC(OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0x3D => Operation::new(
            Opcode::DEC(OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0x3E => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::u8),
            ),
            OpLength::Two,
            Timing::Eight,
        ),
        0x3F => Operation::new(Opcode::CCF, OpLength::One, Timing::Four),
        0x40 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::B),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x41 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::C),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x42 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::D),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x43 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::E),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x44 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::H),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x45 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::L),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x46 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x47 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::B),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x48 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::B),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x49 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::C),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x4A => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::D),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x4B => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::E),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x4C => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::H),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x4D => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::L),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x4E => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x4F => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::C),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x50 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::B),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x51 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::C),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x52 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::D),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x53 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::E),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x54 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::H),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x55 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::L),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x56 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x57 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::D),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x58 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::B),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x59 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::C),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x5A => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::D),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x5B => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::E),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x5C => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::H),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x5D => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::L),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x5E => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x5F => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::E),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x60 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::B),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x61 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::C),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x62 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::D),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x63 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::E),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x64 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::H),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x65 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::L),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x66 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x67 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::H),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x68 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::B),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x69 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::C),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x6A => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::D),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x6B => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::E),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x6C => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::H),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x6D => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::L),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x6E => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x6F => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::L),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x70 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
                OpTarget::Register(Register::B),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x71 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
                OpTarget::Register(Register::C),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x72 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
                OpTarget::Register(Register::D),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x73 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
                OpTarget::Register(Register::E),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x74 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
                OpTarget::Register(Register::H),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x75 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
                OpTarget::Register(Register::L),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x76 => Operation::new(Opcode::HALT, OpLength::One, Timing::Four),
        0x77 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x78 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::B),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x79 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::C),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x7A => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::D),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x7B => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::E),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x7C => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::H),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x7D => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::L),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x7E => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x7F => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0x80 => Operation::new(
            Opcode::ADD(Register::A, OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0x81 => Operation::new(
            Opcode::ADD(Register::A, OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0x82 => Operation::new(
            Opcode::ADD(Register::A, OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0x83 => Operation::new(
            Opcode::ADD(Register::A, OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0x84 => Operation::new(
            Opcode::ADD(Register::A, OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0x85 => Operation::new(
            Opcode::ADD(Register::A, OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0x86 => Operation::new(
            Opcode::ADD(
                Register::A,
                OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                    HLMode::Normal,
                )))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0x87 => Operation::new(
            Opcode::ADD(Register::A, OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0x88 => Operation::new(
            Opcode::ADC(OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0x89 => Operation::new(
            Opcode::ADC(OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0x8A => Operation::new(
            Opcode::ADC(OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0x8B => Operation::new(
            Opcode::ADC(OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0x8C => Operation::new(
            Opcode::ADC(OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0x8D => Operation::new(
            Opcode::ADC(OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0x8E => Operation::new(
            Opcode::ADC(OpTarget::Value(ValueType::deref(DerefSource::Register(
                Register::HL(HLMode::Normal),
            )))),
            OpLength::One,
            Timing::Eight,
        ),
        0x8F => Operation::new(
            Opcode::ADC(OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0x90 => Operation::new(
            Opcode::SUB(OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0x91 => Operation::new(
            Opcode::SUB(OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0x92 => Operation::new(
            Opcode::SUB(OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0x93 => Operation::new(
            Opcode::SUB(OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0x94 => Operation::new(
            Opcode::SUB(OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0x95 => Operation::new(
            Opcode::SUB(OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0x96 => Operation::new(
            Opcode::SUB(OpTarget::Value(ValueType::deref(DerefSource::Register(
                Register::HL(HLMode::Normal),
            )))),
            OpLength::One,
            Timing::Eight,
        ),
        0x97 => Operation::new(
            Opcode::SUB(OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0x98 => Operation::new(
            Opcode::SBC(OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0x99 => Operation::new(
            Opcode::SBC(OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0x9A => Operation::new(
            Opcode::SBC(OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0x9B => Operation::new(
            Opcode::SBC(OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0x9C => Operation::new(
            Opcode::SBC(OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0x9D => Operation::new(
            Opcode::SBC(OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0x9E => Operation::new(
            Opcode::SBC(OpTarget::Value(ValueType::deref(DerefSource::Register(
                Register::HL(HLMode::Normal),
            )))),
            OpLength::One,
            Timing::Eight,
        ),
        0x9F => Operation::new(
            Opcode::SBC(OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0xA0 => Operation::new(
            Opcode::AND(OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0xA1 => Operation::new(
            Opcode::AND(OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0xA2 => Operation::new(
            Opcode::AND(OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0xA3 => Operation::new(
            Opcode::AND(OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0xA4 => Operation::new(
            Opcode::AND(OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0xA5 => Operation::new(
            Opcode::AND(OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0xA6 => Operation::new(
            Opcode::AND(OpTarget::Value(ValueType::deref(DerefSource::Register(
                Register::HL(HLMode::Normal),
            )))),
            OpLength::One,
            Timing::Eight,
        ),
        0xA7 => Operation::new(
            Opcode::AND(OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0xA8 => Operation::new(
            Opcode::XOR(OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0xA9 => Operation::new(
            Opcode::XOR(OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0xAA => Operation::new(
            Opcode::XOR(OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0xAB => Operation::new(
            Opcode::XOR(OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0xAC => Operation::new(
            Opcode::XOR(OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0xAD => Operation::new(
            Opcode::XOR(OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0xAE => Operation::new(
            Opcode::XOR(OpTarget::Value(ValueType::deref(DerefSource::Register(
                Register::HL(HLMode::Normal),
            )))),
            OpLength::One,
            Timing::Eight,
        ),
        0xAF => Operation::new(
            Opcode::XOR(OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0xB0 => Operation::new(
            Opcode::OR(OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0xB1 => Operation::new(
            Opcode::OR(OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0xB2 => Operation::new(
            Opcode::OR(OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0xB3 => Operation::new(
            Opcode::OR(OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0xB4 => Operation::new(
            Opcode::OR(OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0xB5 => Operation::new(
            Opcode::OR(OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0xB6 => Operation::new(
            Opcode::OR(OpTarget::Value(ValueType::deref(DerefSource::Register(
                Register::HL(HLMode::Normal),
            )))),
            OpLength::One,
            Timing::Eight,
        ),
        0xB7 => Operation::new(
            Opcode::OR(OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0xB8 => Operation::new(
            Opcode::CP(OpTarget::Register(Register::B)),
            OpLength::One,
            Timing::Four,
        ),
        0xB9 => Operation::new(
            Opcode::CP(OpTarget::Register(Register::C)),
            OpLength::One,
            Timing::Four,
        ),
        0xBA => Operation::new(
            Opcode::CP(OpTarget::Register(Register::D)),
            OpLength::One,
            Timing::Four,
        ),
        0xBB => Operation::new(
            Opcode::CP(OpTarget::Register(Register::E)),
            OpLength::One,
            Timing::Four,
        ),
        0xBC => Operation::new(
            Opcode::CP(OpTarget::Register(Register::H)),
            OpLength::One,
            Timing::Four,
        ),
        0xBD => Operation::new(
            Opcode::CP(OpTarget::Register(Register::L)),
            OpLength::One,
            Timing::Four,
        ),
        0xBE => Operation::new(
            Opcode::CP(OpTarget::Value(ValueType::deref(DerefSource::Register(
                Register::HL(HLMode::Normal),
            )))),
            OpLength::One,
            Timing::Eight,
        ),
        0xBF => Operation::new(
            Opcode::CP(OpTarget::Register(Register::A)),
            OpLength::One,
            Timing::Four,
        ),
        0xC0 => Operation::new(
            Opcode::RET(Condition::NZ),
            OpLength::One,
            Timing::Variable(Box::new((Timing::Eight, Timing::Twenty))),
        ),
        0xC1 => Operation::new(Opcode::POP(Register::BC), OpLength::One, Timing::Twelve),
        0xC2 => Operation::new(
            Opcode::JP(Condition::NZ, OpTarget::Value(ValueType::u16)),
            OpLength::Three,
            Timing::Variable(Box::new((Timing::Twelve, Timing::Sixteen))),
        ),
        0xC3 => Operation::new(
            Opcode::JP(Condition::None, OpTarget::Value(ValueType::u16)),
            OpLength::Three,
            Timing::Sixteen,
        ),
        0xC4 => Operation::new(
            Opcode::CALL(Condition::NZ),
            OpLength::Three,
            Timing::Variable(Box::new((Timing::Twelve, Timing::TwentyFour))),
        ),
        0xC5 => Operation::new(Opcode::PUSH(Register::BC), OpLength::One, Timing::Sixteen),
        0xC6 => Operation::new(
            Opcode::ADD(Register::A, OpTarget::Value(ValueType::u8)),
            OpLength::Two,
            Timing::Eight,
        ),
        0xC7 => Operation::new(Opcode::RST(RSTAddr::H00), OpLength::One, Timing::Sixteen),
        0xC8 => Operation::new(
            Opcode::RET(Condition::Z),
            OpLength::One,
            Timing::Variable(Box::new((Timing::Eight, Timing::Twenty))),
        ),
        0xC9 => Operation::new(Opcode::RET(Condition::None), OpLength::One, Timing::Sixteen),
        0xCA => Operation::new(
            Opcode::JP(Condition::Z, OpTarget::Value(ValueType::u16)),
            OpLength::Three,
            Timing::Variable(Box::new((Timing::Twelve, Timing::Sixteen))),
        ),
        0xCB => match cb_opcode {
            Some(oc) => {
                Operation::new(Opcode::CB(parse_cb_prefix(oc)), OpLength::Two, Timing::Four)
            }
            None => panic!("CB opcode sent with no cb opcode"),
        },
        0xCC => Operation::new(
            Opcode::CALL(Condition::Z),
            OpLength::Three,
            Timing::Variable(Box::new((Timing::Twelve, Timing::TwentyFour))),
        ),
        0xCD => Operation::new(
            Opcode::CALL(Condition::None),
            OpLength::Three,
            Timing::TwentyFour,
        ),
        0xCE => Operation::new(
            Opcode::ADC(OpTarget::Value(ValueType::u8)),
            OpLength::Two,
            Timing::Eight,
        ),
        0xCF => Operation::new(Opcode::RST(RSTAddr::H08), OpLength::One, Timing::Sixteen),
        0xD0 => Operation::new(
            Opcode::RET(Condition::NC),
            OpLength::One,
            Timing::Variable(Box::new((Timing::Eight, Timing::Twenty))),
        ),
        0xD1 => Operation::new(Opcode::POP(Register::DE), OpLength::One, Timing::Twelve),
        0xD2 => Operation::new(
            Opcode::JP(Condition::NC, OpTarget::Value(ValueType::u16)),
            OpLength::Three,
            Timing::Variable(Box::new((Timing::Twelve, Timing::Sixteen))),
        ),
        0xD3 => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xD4 => Operation::new(
            Opcode::CALL(Condition::NC),
            OpLength::Three,
            Timing::Variable(Box::new((Timing::Twelve, Timing::TwentyFour))),
        ),
        0xD5 => Operation::new(Opcode::PUSH(Register::DE), OpLength::One, Timing::Sixteen),
        0xD6 => Operation::new(
            Opcode::SUB(OpTarget::Value(ValueType::u8)),
            OpLength::Two,
            Timing::Eight,
        ),
        0xD7 => Operation::new(Opcode::RST(RSTAddr::H10), OpLength::One, Timing::Sixteen),
        0xD8 => Operation::new(
            Opcode::RET(Condition::C),
            OpLength::One,
            Timing::Variable(Box::new((Timing::Eight, Timing::Twenty))),
        ),
        0xD9 => Operation::new(Opcode::RETI, OpLength::One, Timing::Sixteen),
        0xDA => Operation::new(
            Opcode::JP(Condition::C, OpTarget::Value(ValueType::u16)),
            OpLength::Three,
            Timing::Variable(Box::new((Timing::Twelve, Timing::Sixteen))),
        ),
        0xDB => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xDC => Operation::new(
            Opcode::CALL(Condition::C),
            OpLength::Three,
            Timing::Variable(Box::new((Timing::Twelve, Timing::TwentyFour))),
        ),
        0xDD => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xDE => Operation::new(
            Opcode::SBC(OpTarget::Value(ValueType::u8)),
            OpLength::Two,
            Timing::Eight,
        ),
        0xDF => Operation::new(Opcode::RST(RSTAddr::H18), OpLength::One, Timing::Sixteen),
        0xE0 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::ff00_plus_deref(DerefSource::u8)),
                OpTarget::Register(Register::A),
            ),
            OpLength::Two,
            Timing::Twelve,
        ),
        0xE1 => Operation::new(
            Opcode::POP(Register::HL(HLMode::Normal)),
            OpLength::One,
            Timing::Twelve,
        ),
        0xE2 => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::ff00_plus_deref(DerefSource::Register(
                    Register::C,
                ))),
                OpTarget::Register(Register::A),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0xE3 => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xE4 => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xE5 => Operation::new(
            Opcode::PUSH(Register::HL(HLMode::Normal)),
            OpLength::One,
            Timing::Sixteen,
        ),
        0xE6 => Operation::new(
            Opcode::AND(OpTarget::Value(ValueType::u8)),
            OpLength::Two,
            Timing::Eight,
        ),
        0xE7 => Operation::new(Opcode::RST(RSTAddr::H20), OpLength::One, Timing::Sixteen),
        0xE8 => Operation::new(
            Opcode::ADD(Register::SP, OpTarget::Value(ValueType::i8)),
            OpLength::Two,
            Timing::Sixteen,
        ),
        0xE9 => Operation::new(
            Opcode::JP(
                Condition::None,
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            OpLength::One,
            Timing::Four,
        ),
        0xEA => Operation::new(
            Opcode::LD(
                OpTarget::Value(ValueType::deref(DerefSource::u16)),
                OpTarget::Register(Register::A),
            ),
            OpLength::Three,
            Timing::Sixteen,
        ),
        0xEB => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xEC => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xED => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xEE => Operation::new(
            Opcode::XOR(OpTarget::Value(ValueType::u8)),
            OpLength::Two,
            Timing::Eight,
        ),
        0xEF => Operation::new(Opcode::RST(RSTAddr::H28), OpLength::One, Timing::Sixteen),
        0xF0 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::ff00_plus_deref(DerefSource::u8)),
            ),
            OpLength::Two,
            Timing::Twelve,
        ),
        0xF1 => Operation::new(Opcode::POP(Register::AF), OpLength::One, Timing::Twelve),
        0xF2 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::ff00_plus_deref(DerefSource::Register(
                    Register::C,
                ))),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0xF3 => Operation::new(Opcode::DI, OpLength::One, Timing::Four),
        0xF4 => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xF5 => Operation::new(Opcode::PUSH(Register::AF), OpLength::One, Timing::Sixteen),
        0xF6 => Operation::new(
            Opcode::OR(OpTarget::Value(ValueType::u8)),
            OpLength::Two,
            Timing::Eight,
        ),
        0xF7 => Operation::new(Opcode::RST(RSTAddr::H30), OpLength::One, Timing::Sixteen),
        0xF8 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::HL(HLMode::Normal)),
                OpTarget::Register(Register::SPPlusi8),
            ),
            OpLength::Two,
            Timing::Twelve,
        ),
        0xF9 => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::SP),
                OpTarget::Register(Register::HL(HLMode::Normal)),
            ),
            OpLength::One,
            Timing::Eight,
        ),
        0xFA => Operation::new(
            Opcode::LD(
                OpTarget::Register(Register::A),
                OpTarget::Value(ValueType::deref(DerefSource::u16)),
            ),
            OpLength::Three,
            Timing::Sixteen,
        ),
        0xFB => Operation::new(Opcode::EI, OpLength::One, Timing::Four),
        0xFC => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xFD => Operation::new(Opcode::PANIC, OpLength::None, Timing::Four),
        0xFE => Operation::new(
            Opcode::CP(OpTarget::Value(ValueType::u8)),
            OpLength::Two,
            Timing::Eight,
        ),
        0xFF => Operation::new(Opcode::RST(RSTAddr::H38), OpLength::One, Timing::Sixteen),
    }
}

fn parse_cb_prefix(cb_opcode: u8) -> CBPrefix {
    match cb_opcode {
        0x00 => CBPrefix::RLC(OpTarget::Register(Register::B), Timing::Eight),
        0x01 => CBPrefix::RLC(OpTarget::Register(Register::C), Timing::Eight),
        0x02 => CBPrefix::RLC(OpTarget::Register(Register::D), Timing::Eight),
        0x03 => CBPrefix::RLC(OpTarget::Register(Register::E), Timing::Eight),
        0x04 => CBPrefix::RLC(OpTarget::Register(Register::H), Timing::Eight),
        0x05 => CBPrefix::RLC(OpTarget::Register(Register::L), Timing::Eight),
        0x06 => CBPrefix::RLC(
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x07 => CBPrefix::RLC(OpTarget::Register(Register::A), Timing::Eight),
        0x08 => CBPrefix::RRC(OpTarget::Register(Register::B), Timing::Eight),
        0x09 => CBPrefix::RRC(OpTarget::Register(Register::C), Timing::Eight),
        0x0A => CBPrefix::RRC(OpTarget::Register(Register::D), Timing::Eight),
        0x0B => CBPrefix::RRC(OpTarget::Register(Register::E), Timing::Eight),
        0x0C => CBPrefix::RRC(OpTarget::Register(Register::H), Timing::Eight),
        0x0D => CBPrefix::RRC(OpTarget::Register(Register::L), Timing::Eight),
        0x0E => CBPrefix::RRC(
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x0F => CBPrefix::RRC(OpTarget::Register(Register::A), Timing::Eight),
        0x10 => CBPrefix::RL(OpTarget::Register(Register::B), Timing::Eight),
        0x11 => CBPrefix::RL(OpTarget::Register(Register::C), Timing::Eight),
        0x12 => CBPrefix::RL(OpTarget::Register(Register::D), Timing::Eight),
        0x13 => CBPrefix::RL(OpTarget::Register(Register::E), Timing::Eight),
        0x14 => CBPrefix::RL(OpTarget::Register(Register::H), Timing::Eight),
        0x15 => CBPrefix::RL(OpTarget::Register(Register::L), Timing::Eight),
        0x16 => CBPrefix::RL(
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x17 => CBPrefix::RL(OpTarget::Register(Register::A), Timing::Eight),
        0x18 => CBPrefix::RR(OpTarget::Register(Register::B), Timing::Eight),
        0x19 => CBPrefix::RR(OpTarget::Register(Register::C), Timing::Eight),
        0x1A => CBPrefix::RR(OpTarget::Register(Register::D), Timing::Eight),
        0x1B => CBPrefix::RR(OpTarget::Register(Register::E), Timing::Eight),
        0x1C => CBPrefix::RR(OpTarget::Register(Register::H), Timing::Eight),
        0x1D => CBPrefix::RR(OpTarget::Register(Register::L), Timing::Eight),
        0x1E => CBPrefix::RR(
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x1F => CBPrefix::RR(OpTarget::Register(Register::A), Timing::Eight),
        0x20 => CBPrefix::SLA(OpTarget::Register(Register::B), Timing::Eight),
        0x21 => CBPrefix::SLA(OpTarget::Register(Register::C), Timing::Eight),
        0x22 => CBPrefix::SLA(OpTarget::Register(Register::D), Timing::Eight),
        0x23 => CBPrefix::SLA(OpTarget::Register(Register::E), Timing::Eight),
        0x24 => CBPrefix::SLA(OpTarget::Register(Register::H), Timing::Eight),
        0x25 => CBPrefix::SLA(OpTarget::Register(Register::L), Timing::Eight),
        0x26 => CBPrefix::SLA(
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x27 => CBPrefix::SLA(OpTarget::Register(Register::A), Timing::Eight),
        0x28 => CBPrefix::SRA(OpTarget::Register(Register::B), Timing::Eight),
        0x29 => CBPrefix::SRA(OpTarget::Register(Register::C), Timing::Eight),
        0x2A => CBPrefix::SRA(OpTarget::Register(Register::D), Timing::Eight),
        0x2B => CBPrefix::SRA(OpTarget::Register(Register::E), Timing::Eight),
        0x2C => CBPrefix::SRA(OpTarget::Register(Register::H), Timing::Eight),
        0x2D => CBPrefix::SRA(OpTarget::Register(Register::L), Timing::Eight),
        0x2E => CBPrefix::SRA(
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x2F => CBPrefix::SRA(OpTarget::Register(Register::A), Timing::Eight),
        0x30 => CBPrefix::SWAP(OpTarget::Register(Register::B), Timing::Eight),
        0x31 => CBPrefix::SWAP(OpTarget::Register(Register::C), Timing::Eight),
        0x32 => CBPrefix::SWAP(OpTarget::Register(Register::D), Timing::Eight),
        0x33 => CBPrefix::SWAP(OpTarget::Register(Register::E), Timing::Eight),
        0x34 => CBPrefix::SWAP(OpTarget::Register(Register::H), Timing::Eight),
        0x35 => CBPrefix::SWAP(OpTarget::Register(Register::L), Timing::Eight),
        0x36 => CBPrefix::SWAP(
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x37 => CBPrefix::SWAP(OpTarget::Register(Register::A), Timing::Eight),
        0x38 => CBPrefix::SRL(OpTarget::Register(Register::B), Timing::Eight),
        0x39 => CBPrefix::SRL(OpTarget::Register(Register::C), Timing::Eight),
        0x3A => CBPrefix::SRL(OpTarget::Register(Register::D), Timing::Eight),
        0x3B => CBPrefix::SRL(OpTarget::Register(Register::E), Timing::Eight),
        0x3C => CBPrefix::SRL(OpTarget::Register(Register::H), Timing::Eight),
        0x3D => CBPrefix::SRL(OpTarget::Register(Register::L), Timing::Eight),
        0x3E => CBPrefix::SRL(
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x3F => CBPrefix::SRL(OpTarget::Register(Register::A), Timing::Eight),
        0x40 => CBPrefix::BIT(0, OpTarget::Register(Register::B), Timing::Eight),
        0x41 => CBPrefix::BIT(0, OpTarget::Register(Register::C), Timing::Eight),
        0x42 => CBPrefix::BIT(0, OpTarget::Register(Register::D), Timing::Eight),
        0x43 => CBPrefix::BIT(0, OpTarget::Register(Register::E), Timing::Eight),
        0x44 => CBPrefix::BIT(0, OpTarget::Register(Register::H), Timing::Eight),
        0x45 => CBPrefix::BIT(0, OpTarget::Register(Register::L), Timing::Eight),
        0x46 => CBPrefix::BIT(
            0,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Twelve,
        ),
        0x47 => CBPrefix::BIT(0, OpTarget::Register(Register::A), Timing::Eight),
        0x48 => CBPrefix::BIT(1, OpTarget::Register(Register::B), Timing::Eight),
        0x49 => CBPrefix::BIT(1, OpTarget::Register(Register::C), Timing::Eight),
        0x4A => CBPrefix::BIT(1, OpTarget::Register(Register::D), Timing::Eight),
        0x4B => CBPrefix::BIT(1, OpTarget::Register(Register::E), Timing::Eight),
        0x4C => CBPrefix::BIT(1, OpTarget::Register(Register::H), Timing::Eight),
        0x4D => CBPrefix::BIT(1, OpTarget::Register(Register::L), Timing::Eight),
        0x4E => CBPrefix::BIT(
            1,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Twelve,
        ),
        0x4F => CBPrefix::BIT(1, OpTarget::Register(Register::A), Timing::Eight),
        0x50 => CBPrefix::BIT(2, OpTarget::Register(Register::B), Timing::Eight),
        0x51 => CBPrefix::BIT(2, OpTarget::Register(Register::C), Timing::Eight),
        0x52 => CBPrefix::BIT(2, OpTarget::Register(Register::D), Timing::Eight),
        0x53 => CBPrefix::BIT(2, OpTarget::Register(Register::E), Timing::Eight),
        0x54 => CBPrefix::BIT(2, OpTarget::Register(Register::H), Timing::Eight),
        0x55 => CBPrefix::BIT(2, OpTarget::Register(Register::L), Timing::Eight),
        0x56 => CBPrefix::BIT(
            2,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Twelve,
        ),
        0x57 => CBPrefix::BIT(2, OpTarget::Register(Register::A), Timing::Eight),
        0x58 => CBPrefix::BIT(3, OpTarget::Register(Register::B), Timing::Eight),
        0x59 => CBPrefix::BIT(3, OpTarget::Register(Register::C), Timing::Eight),
        0x5A => CBPrefix::BIT(3, OpTarget::Register(Register::D), Timing::Eight),
        0x5B => CBPrefix::BIT(3, OpTarget::Register(Register::E), Timing::Eight),
        0x5C => CBPrefix::BIT(3, OpTarget::Register(Register::H), Timing::Eight),
        0x5D => CBPrefix::BIT(3, OpTarget::Register(Register::L), Timing::Eight),
        0x5E => CBPrefix::BIT(
            3,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Twelve,
        ),
        0x5F => CBPrefix::BIT(3, OpTarget::Register(Register::A), Timing::Eight),
        0x60 => CBPrefix::BIT(4, OpTarget::Register(Register::B), Timing::Eight),
        0x61 => CBPrefix::BIT(4, OpTarget::Register(Register::C), Timing::Eight),
        0x62 => CBPrefix::BIT(4, OpTarget::Register(Register::D), Timing::Eight),
        0x63 => CBPrefix::BIT(4, OpTarget::Register(Register::E), Timing::Eight),
        0x64 => CBPrefix::BIT(4, OpTarget::Register(Register::H), Timing::Eight),
        0x65 => CBPrefix::BIT(4, OpTarget::Register(Register::L), Timing::Eight),
        0x66 => CBPrefix::BIT(
            4,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Twelve,
        ),
        0x67 => CBPrefix::BIT(4, OpTarget::Register(Register::A), Timing::Eight),
        0x68 => CBPrefix::BIT(5, OpTarget::Register(Register::B), Timing::Eight),
        0x69 => CBPrefix::BIT(5, OpTarget::Register(Register::C), Timing::Eight),
        0x6A => CBPrefix::BIT(5, OpTarget::Register(Register::D), Timing::Eight),
        0x6B => CBPrefix::BIT(5, OpTarget::Register(Register::E), Timing::Eight),
        0x6C => CBPrefix::BIT(5, OpTarget::Register(Register::H), Timing::Eight),
        0x6D => CBPrefix::BIT(5, OpTarget::Register(Register::L), Timing::Eight),
        0x6E => CBPrefix::BIT(
            5,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Twelve,
        ),
        0x6F => CBPrefix::BIT(5, OpTarget::Register(Register::A), Timing::Eight),
        0x70 => CBPrefix::BIT(6, OpTarget::Register(Register::B), Timing::Eight),
        0x71 => CBPrefix::BIT(6, OpTarget::Register(Register::C), Timing::Eight),
        0x72 => CBPrefix::BIT(6, OpTarget::Register(Register::D), Timing::Eight),
        0x73 => CBPrefix::BIT(6, OpTarget::Register(Register::E), Timing::Eight),
        0x74 => CBPrefix::BIT(6, OpTarget::Register(Register::H), Timing::Eight),
        0x75 => CBPrefix::BIT(6, OpTarget::Register(Register::L), Timing::Eight),
        0x76 => CBPrefix::BIT(
            6,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Twelve,
        ),
        0x77 => CBPrefix::BIT(6, OpTarget::Register(Register::A), Timing::Eight),
        0x78 => CBPrefix::BIT(7, OpTarget::Register(Register::B), Timing::Eight),
        0x79 => CBPrefix::BIT(7, OpTarget::Register(Register::C), Timing::Eight),
        0x7A => CBPrefix::BIT(7, OpTarget::Register(Register::D), Timing::Eight),
        0x7B => CBPrefix::BIT(7, OpTarget::Register(Register::E), Timing::Eight),
        0x7C => CBPrefix::BIT(7, OpTarget::Register(Register::H), Timing::Eight),
        0x7D => CBPrefix::BIT(7, OpTarget::Register(Register::L), Timing::Eight),
        0x7E => CBPrefix::BIT(
            7,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Twelve,
        ),
        0x7F => CBPrefix::BIT(7, OpTarget::Register(Register::A), Timing::Eight),
        0x80 => CBPrefix::RES(0, OpTarget::Register(Register::B), Timing::Eight),
        0x81 => CBPrefix::RES(0, OpTarget::Register(Register::C), Timing::Eight),
        0x82 => CBPrefix::RES(0, OpTarget::Register(Register::D), Timing::Eight),
        0x83 => CBPrefix::RES(0, OpTarget::Register(Register::E), Timing::Eight),
        0x84 => CBPrefix::RES(0, OpTarget::Register(Register::H), Timing::Eight),
        0x85 => CBPrefix::RES(0, OpTarget::Register(Register::L), Timing::Eight),
        0x86 => CBPrefix::RES(
            0,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x87 => CBPrefix::RES(0, OpTarget::Register(Register::A), Timing::Eight),
        0x88 => CBPrefix::RES(1, OpTarget::Register(Register::B), Timing::Eight),
        0x89 => CBPrefix::RES(1, OpTarget::Register(Register::C), Timing::Eight),
        0x8A => CBPrefix::RES(1, OpTarget::Register(Register::D), Timing::Eight),
        0x8B => CBPrefix::RES(1, OpTarget::Register(Register::E), Timing::Eight),
        0x8C => CBPrefix::RES(1, OpTarget::Register(Register::H), Timing::Eight),
        0x8D => CBPrefix::RES(1, OpTarget::Register(Register::L), Timing::Eight),
        0x8E => CBPrefix::RES(
            1,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x8F => CBPrefix::RES(1, OpTarget::Register(Register::A), Timing::Eight),
        0x90 => CBPrefix::RES(2, OpTarget::Register(Register::B), Timing::Eight),
        0x91 => CBPrefix::RES(2, OpTarget::Register(Register::C), Timing::Eight),
        0x92 => CBPrefix::RES(2, OpTarget::Register(Register::D), Timing::Eight),
        0x93 => CBPrefix::RES(2, OpTarget::Register(Register::E), Timing::Eight),
        0x94 => CBPrefix::RES(2, OpTarget::Register(Register::H), Timing::Eight),
        0x95 => CBPrefix::RES(2, OpTarget::Register(Register::L), Timing::Eight),
        0x96 => CBPrefix::RES(
            2,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x97 => CBPrefix::RES(2, OpTarget::Register(Register::A), Timing::Eight),
        0x98 => CBPrefix::RES(3, OpTarget::Register(Register::B), Timing::Eight),
        0x99 => CBPrefix::RES(3, OpTarget::Register(Register::C), Timing::Eight),
        0x9A => CBPrefix::RES(3, OpTarget::Register(Register::D), Timing::Eight),
        0x9B => CBPrefix::RES(3, OpTarget::Register(Register::E), Timing::Eight),
        0x9C => CBPrefix::RES(3, OpTarget::Register(Register::H), Timing::Eight),
        0x9D => CBPrefix::RES(3, OpTarget::Register(Register::L), Timing::Eight),
        0x9E => CBPrefix::RES(
            3,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0x9F => CBPrefix::RES(3, OpTarget::Register(Register::A), Timing::Eight),
        0xA0 => CBPrefix::RES(4, OpTarget::Register(Register::B), Timing::Eight),
        0xA1 => CBPrefix::RES(4, OpTarget::Register(Register::C), Timing::Eight),
        0xA2 => CBPrefix::RES(4, OpTarget::Register(Register::D), Timing::Eight),
        0xA3 => CBPrefix::RES(4, OpTarget::Register(Register::E), Timing::Eight),
        0xA4 => CBPrefix::RES(4, OpTarget::Register(Register::H), Timing::Eight),
        0xA5 => CBPrefix::RES(4, OpTarget::Register(Register::L), Timing::Eight),
        0xA6 => CBPrefix::RES(
            4,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xA7 => CBPrefix::RES(4, OpTarget::Register(Register::A), Timing::Eight),
        0xA8 => CBPrefix::RES(5, OpTarget::Register(Register::B), Timing::Eight),
        0xA9 => CBPrefix::RES(5, OpTarget::Register(Register::C), Timing::Eight),
        0xAA => CBPrefix::RES(5, OpTarget::Register(Register::D), Timing::Eight),
        0xAB => CBPrefix::RES(5, OpTarget::Register(Register::E), Timing::Eight),
        0xAC => CBPrefix::RES(5, OpTarget::Register(Register::H), Timing::Eight),
        0xAD => CBPrefix::RES(5, OpTarget::Register(Register::L), Timing::Eight),
        0xAE => CBPrefix::RES(
            5,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xAF => CBPrefix::RES(5, OpTarget::Register(Register::A), Timing::Eight),
        0xB0 => CBPrefix::RES(6, OpTarget::Register(Register::B), Timing::Eight),
        0xB1 => CBPrefix::RES(6, OpTarget::Register(Register::C), Timing::Eight),
        0xB2 => CBPrefix::RES(6, OpTarget::Register(Register::D), Timing::Eight),
        0xB3 => CBPrefix::RES(6, OpTarget::Register(Register::E), Timing::Eight),
        0xB4 => CBPrefix::RES(6, OpTarget::Register(Register::H), Timing::Eight),
        0xB5 => CBPrefix::RES(6, OpTarget::Register(Register::L), Timing::Eight),
        0xB6 => CBPrefix::RES(
            6,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xB7 => CBPrefix::RES(6, OpTarget::Register(Register::A), Timing::Eight),
        0xB8 => CBPrefix::RES(7, OpTarget::Register(Register::B), Timing::Eight),
        0xB9 => CBPrefix::RES(7, OpTarget::Register(Register::C), Timing::Eight),
        0xBA => CBPrefix::RES(7, OpTarget::Register(Register::D), Timing::Eight),
        0xBB => CBPrefix::RES(7, OpTarget::Register(Register::E), Timing::Eight),
        0xBC => CBPrefix::RES(7, OpTarget::Register(Register::H), Timing::Eight),
        0xBD => CBPrefix::RES(7, OpTarget::Register(Register::L), Timing::Eight),
        0xBE => CBPrefix::RES(
            7,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xBF => CBPrefix::RES(7, OpTarget::Register(Register::A), Timing::Eight),
        0xC0 => CBPrefix::SET(0, OpTarget::Register(Register::B), Timing::Eight),
        0xC1 => CBPrefix::SET(0, OpTarget::Register(Register::C), Timing::Eight),
        0xC2 => CBPrefix::SET(0, OpTarget::Register(Register::D), Timing::Eight),
        0xC3 => CBPrefix::SET(0, OpTarget::Register(Register::E), Timing::Eight),
        0xC4 => CBPrefix::SET(0, OpTarget::Register(Register::H), Timing::Eight),
        0xC5 => CBPrefix::SET(0, OpTarget::Register(Register::L), Timing::Eight),
        0xC6 => CBPrefix::SET(
            0,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xC7 => CBPrefix::SET(0, OpTarget::Register(Register::A), Timing::Eight),
        0xC8 => CBPrefix::SET(1, OpTarget::Register(Register::B), Timing::Eight),
        0xC9 => CBPrefix::SET(1, OpTarget::Register(Register::C), Timing::Eight),
        0xCA => CBPrefix::SET(1, OpTarget::Register(Register::D), Timing::Eight),
        0xCB => CBPrefix::SET(1, OpTarget::Register(Register::E), Timing::Eight),
        0xCC => CBPrefix::SET(1, OpTarget::Register(Register::H), Timing::Eight),
        0xCD => CBPrefix::SET(1, OpTarget::Register(Register::L), Timing::Eight),
        0xCE => CBPrefix::SET(
            1,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xCF => CBPrefix::SET(1, OpTarget::Register(Register::A), Timing::Eight),
        0xD0 => CBPrefix::SET(2, OpTarget::Register(Register::B), Timing::Eight),
        0xD1 => CBPrefix::SET(2, OpTarget::Register(Register::C), Timing::Eight),
        0xD2 => CBPrefix::SET(2, OpTarget::Register(Register::D), Timing::Eight),
        0xD3 => CBPrefix::SET(2, OpTarget::Register(Register::E), Timing::Eight),
        0xD4 => CBPrefix::SET(2, OpTarget::Register(Register::H), Timing::Eight),
        0xD5 => CBPrefix::SET(2, OpTarget::Register(Register::L), Timing::Eight),
        0xD6 => CBPrefix::SET(
            2,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xD7 => CBPrefix::SET(2, OpTarget::Register(Register::A), Timing::Eight),
        0xD8 => CBPrefix::SET(3, OpTarget::Register(Register::B), Timing::Eight),
        0xD9 => CBPrefix::SET(3, OpTarget::Register(Register::C), Timing::Eight),
        0xDA => CBPrefix::SET(3, OpTarget::Register(Register::D), Timing::Eight),
        0xDB => CBPrefix::SET(3, OpTarget::Register(Register::E), Timing::Eight),
        0xDC => CBPrefix::SET(3, OpTarget::Register(Register::H), Timing::Eight),
        0xDD => CBPrefix::SET(3, OpTarget::Register(Register::L), Timing::Eight),
        0xDE => CBPrefix::SET(
            3,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xDF => CBPrefix::SET(3, OpTarget::Register(Register::A), Timing::Eight),
        0xE0 => CBPrefix::SET(4, OpTarget::Register(Register::B), Timing::Eight),
        0xE1 => CBPrefix::SET(4, OpTarget::Register(Register::C), Timing::Eight),
        0xE2 => CBPrefix::SET(4, OpTarget::Register(Register::D), Timing::Eight),
        0xE3 => CBPrefix::SET(4, OpTarget::Register(Register::E), Timing::Eight),
        0xE4 => CBPrefix::SET(4, OpTarget::Register(Register::H), Timing::Eight),
        0xE5 => CBPrefix::SET(4, OpTarget::Register(Register::L), Timing::Eight),
        0xE6 => CBPrefix::SET(
            4,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xE7 => CBPrefix::SET(4, OpTarget::Register(Register::A), Timing::Eight),
        0xE8 => CBPrefix::SET(5, OpTarget::Register(Register::B), Timing::Eight),
        0xE9 => CBPrefix::SET(5, OpTarget::Register(Register::C), Timing::Eight),
        0xEA => CBPrefix::SET(5, OpTarget::Register(Register::D), Timing::Eight),
        0xEB => CBPrefix::SET(5, OpTarget::Register(Register::E), Timing::Eight),
        0xEC => CBPrefix::SET(5, OpTarget::Register(Register::H), Timing::Eight),
        0xED => CBPrefix::SET(5, OpTarget::Register(Register::L), Timing::Eight),
        0xEE => CBPrefix::SET(
            5,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xEF => CBPrefix::SET(5, OpTarget::Register(Register::A), Timing::Eight),
        0xF0 => CBPrefix::SET(6, OpTarget::Register(Register::B), Timing::Eight),
        0xF1 => CBPrefix::SET(6, OpTarget::Register(Register::C), Timing::Eight),
        0xF2 => CBPrefix::SET(6, OpTarget::Register(Register::D), Timing::Eight),
        0xF3 => CBPrefix::SET(6, OpTarget::Register(Register::E), Timing::Eight),
        0xF4 => CBPrefix::SET(6, OpTarget::Register(Register::H), Timing::Eight),
        0xF5 => CBPrefix::SET(6, OpTarget::Register(Register::L), Timing::Eight),
        0xF6 => CBPrefix::SET(
            6,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xF7 => CBPrefix::SET(6, OpTarget::Register(Register::A), Timing::Eight),
        0xF8 => CBPrefix::SET(7, OpTarget::Register(Register::B), Timing::Eight),
        0xF9 => CBPrefix::SET(7, OpTarget::Register(Register::C), Timing::Eight),
        0xFA => CBPrefix::SET(7, OpTarget::Register(Register::D), Timing::Eight),
        0xFB => CBPrefix::SET(7, OpTarget::Register(Register::E), Timing::Eight),
        0xFC => CBPrefix::SET(7, OpTarget::Register(Register::H), Timing::Eight),
        0xFD => CBPrefix::SET(7, OpTarget::Register(Register::L), Timing::Eight),
        0xFE => CBPrefix::SET(
            7,
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            Timing::Sixteen,
        ),
        0xFF => CBPrefix::SET(7, OpTarget::Register(Register::A), Timing::Eight),
    }
}
