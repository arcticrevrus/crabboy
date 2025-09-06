use crate::cpu::*;
use crate::memory::*;

pub mod opfunctions;
pub mod parse;

#[derive(Debug)]
pub struct Operation {
    pub opcode: Opcode,
    pub length: OpLength,
    pub cycles: Timing,
    pub value_one: Option<u8>,
    pub value_two: Option<u8>,
    pub signed_value: bool,
}
impl Operation {
    pub fn new(opcode: Opcode, length: OpLength, cycles: Timing) -> Operation {
        Operation {
            opcode,
            length,
            cycles,
            value_one: None,
            value_two: None,
            signed_value: false,
        }
    }
}

fn match_optarget(target: &OpTarget) -> (FieldLength, Option<Register>) {
    let dest_length: FieldLength;
    let mut dest_register: Option<Register> = None;
    match *target {
        OpTarget::Register(reg) => match reg {
            Register::AF => {
                dest_length = FieldLength::u16;
                dest_register = Some(Register::AF);
            }
            Register::A => {
                dest_length = FieldLength::u8;
                dest_register = Some(Register::A);
            }
            Register::F => {
                dest_length = FieldLength::u8;
                dest_register = Some(Register::F);
            }
            Register::BC => {
                dest_length = FieldLength::u16;
                dest_register = Some(Register::BC);
            }
            Register::B => {
                dest_length = FieldLength::u8;
                dest_register = Some(Register::B);
            }
            Register::C => {
                dest_length = FieldLength::u8;
                dest_register = Some(Register::C);
            }
            Register::DE => {
                dest_length = FieldLength::u16;
                dest_register = Some(Register::DE);
            }
            Register::D => {
                dest_length = FieldLength::u8;
                dest_register = Some(Register::D);
            }
            Register::E => {
                dest_length = FieldLength::u8;
                dest_register = Some(Register::E);
            }
            Register::HL(m) => {
                dest_length = FieldLength::u16;
                dest_register = Some(Register::HL(m))
            }
            Register::H => {
                dest_length = FieldLength::u8;
                dest_register = Some(Register::H);
            }
            Register::L => {
                dest_length = FieldLength::u8;
                dest_register = Some(Register::L);
            }
            Register::SP => {
                dest_length = FieldLength::u16;
                dest_register = Some(Register::SP);
            }
            Register::SPPlusi8 => {
                dest_length = FieldLength::u16;
                dest_register = Some(Register::SPPlusi8);
            }
            Register::PC => {
                dest_length = FieldLength::u16;
                dest_register = Some(Register::PC);
            }
        },
        OpTarget::Value(vt) => match vt {
            ValueType::i8 => dest_length = FieldLength::u8,
            ValueType::u8 => dest_length = FieldLength::u8,
            ValueType::u16 => dest_length = FieldLength::u16,
            ValueType::ff00_plus_deref(_) => dest_length = FieldLength::u16,
            ValueType::deref(_) => dest_length = FieldLength::u8,
        },
    };
    (dest_length, dest_register)
}
