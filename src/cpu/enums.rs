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

#[derive(Debug)]
pub enum Timing {
    Four,
    Eight,
    Twelve,
    Sixteen,
    Twenty,
    TwentyFour,
    Variable(Box<(Timing, Timing)>),
}

pub enum Flag {
    Z,
    N,
    H,
    C,
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
    u8,
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
    ff00_plus_deref(DerefSource),
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
pub enum Opcode {
    NOP,
    LD(OpTarget, OpTarget),
    INC(OpTarget),
    DEC(OpTarget),
    ADD(Register, OpTarget),
    ADC(OpTarget),
    SUB(OpTarget),
    SBC(OpTarget),
    AND(OpTarget),
    XOR(OpTarget),
    OR(OpTarget),
    CP(OpTarget),
    STOP,
    RLCA,
    RRCA,
    RRA,
    RLA,
    DAA,
    SCF,
    CCF,
    JR(Condition),
    RET(Condition),
    CPL,
    RETI,
    JP(Condition, OpTarget),
    CALL(Condition),
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
    RLC(OpTarget, Timing),
    RRC(OpTarget, Timing),
    RL(OpTarget, Timing),
    RR(OpTarget, Timing),
    SLA(OpTarget, Timing),
    SRA(OpTarget, Timing),
    SWAP(OpTarget, Timing),
    SRL(OpTarget, Timing),
    BIT(u8, OpTarget, Timing),
    RES(u8, OpTarget, Timing),
    SET(u8, OpTarget, Timing),
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum OpLength {
    None,
    One,
    Two,
    Three,
}

#[derive(PartialEq)]
#[allow(non_camel_case_types)]
pub enum FieldLength {
    u8,
    u16,
}
