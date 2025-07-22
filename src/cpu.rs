pub enum Mode {
    DMG,
    GBC,
}

pub enum Opcode {
    NOP,
    LD,
    LDH,
    LD_HL,
    LD_SP,
    INC,
    DEC,
    ADD,
    ADD_SP,
    XOR,
    OR,
    CP,
    STOP,
    RLCA,
    RRCA,
    RL,
    RR,
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
    JR,
    JR_COND,
    RET,
    RET_COND,
    RETI,
    JP,
    JP_COND,
    CALL,
    CALL_COND,
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
            0x01 => Opcode::LD,
            0x02 => Opcode::LD,
            0x03 => Opcode::INC,
            0x04 => Opcode::INC,
            0x05 => Opcode::DEC,
            0x06 => Opcode::LD,
            0x07 => Opcode::RLCA,
            0x08 => Opcode::LD,
            0x09 => Opcode::ADD,
            0x0A => Opcode::LD,
            0x0B => Opcode::DEC,
            0x0C => Opcode::INC,
            0x0D => Opcode::DEC,
            0x0E => Opcode::LD,
            0x0F => Opcode::RRCA,
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
