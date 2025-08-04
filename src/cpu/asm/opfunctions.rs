use std::mem;

use crate::{cpu::asm::*, memory};

pub fn nop_operation() {}

pub fn crash(cpu: &mut Cpu) {
    cpu.is_running = false;
}

pub fn pop_operation(cpu: &mut Cpu, register: Register, memory_map: &mut MemoryMap) {
    let sp = OpTarget::Register(Register::SP);
    match register {
        Register::AF => {
            cpu.registers.af.flags.bits = memory_map.read(cpu.registers.sp.stackpointer);
            inc_operation(cpu, sp, memory_map);
            cpu.registers.af.accumulator = memory_map.read(cpu.registers.sp.stackpointer);
            inc_operation(cpu, sp, memory_map);
            todo!("Set the cpu flags for POP AF");
        }
        Register::BC => {
            cpu.registers.bc.c = memory_map.read(cpu.registers.sp.stackpointer);
            inc_operation(cpu, sp, memory_map);
            cpu.registers.bc.b = memory_map.read(cpu.registers.sp.stackpointer);
            inc_operation(cpu, sp, memory_map);
        }
        Register::DE => {
            cpu.registers.de.e = memory_map.read(cpu.registers.sp.stackpointer);
            inc_operation(cpu, sp, memory_map);
            cpu.registers.de.d = memory_map.read(cpu.registers.sp.stackpointer);
            inc_operation(cpu, sp, memory_map);
        }
        Register::HL(HLMode::Normal) => {
            cpu.registers.hl.l = memory_map.read(cpu.registers.sp.stackpointer);
            inc_operation(cpu, sp, memory_map);
            cpu.registers.hl.h = memory_map.read(cpu.registers.sp.stackpointer);
            inc_operation(cpu, sp, memory_map);
        }
        _ => panic!("Invalid register given for POP operation"),
    };
}

pub fn ret_operation(cpu: &mut Cpu, condition: Condition, memory_map: &mut MemoryMap) {
    match condition {
        Condition::C => {
            if !cpu.registers.af.flags.get(Flag::C) {
                return;
            }
        }
        Condition::NC => {
            if cpu.registers.af.flags.get(Flag::C) {
                return;
            }
        }
        Condition::Z => {
            if !cpu.registers.af.flags.get(Flag::Z) {
                return;
            }
        }
        Condition::NZ => {
            if cpu.registers.af.flags.get(Flag::Z) {
                return;
            }
        }
        Condition::None => (),
    }
    let sp = OpTarget::Register(Register::SP);
    let low_byte = memory_map.read(cpu.registers.sp.stackpointer);
    inc_operation(cpu, sp, memory_map);
    let high_byte = memory_map.read(cpu.registers.sp.stackpointer);
    inc_operation(cpu, sp, memory_map);
    cpu.registers.sp.stackpointer = (high_byte as u16) << 8 | low_byte as u16;
}

pub fn daa_operation(cpu: &mut Cpu) {
    let mut adjustment: u8 = 0;
    if cpu.registers.af.flags.get(Flag::N) {
        if cpu.registers.af.flags.get(Flag::H) {
            adjustment += 0x6;
        }
        if cpu.registers.af.flags.get(Flag::C) {
            adjustment += 0x60;
        }
    } else {
        if cpu.registers.af.flags.get(Flag::H) || cpu.registers.af.accumulator & 0x0F > 0x09 {
            adjustment += 0x6;
        }
        if cpu.registers.af.flags.get(Flag::C) || cpu.registers.af.accumulator > 0x99 {
            adjustment += 0x60;
        }
    }
    cpu.registers.af.accumulator = cpu.registers.af.accumulator.wrapping_sub(adjustment);
}

pub fn cp_operation(
    cpu: &mut Cpu,
    compare_target: OpTarget,
    value: Option<u8>,
    memory: &MemoryMap,
) {
    let a_register_value = cpu.registers.af.accumulator;
    let target_value = match compare_target {
        OpTarget::Register(register) => match register {
            Register::A => cpu.registers.af.accumulator,
            Register::B => cpu.registers.bc.b,
            Register::C => cpu.registers.bc.c,
            Register::D => cpu.registers.de.d,
            Register::E => cpu.registers.de.e,
            Register::H => cpu.registers.hl.h,
            Register::L => cpu.registers.hl.l,
            _ => panic!("Invalid register provided to cp operation register target"),
        },
        OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(HLMode::Normal)))) => {
            memory.read(cpu.registers.read_u16(Register::HL(HLMode::Normal)))
        }
        OpTarget::Value(ValueType::u8) => {
            value.expect("No value given for value type CP operation")
        }
        _ => panic!("Invalid OpTarget given to CP operation"),
    };
    cpu.registers
        .af
        .flags
        .set(Flag::Z, a_register_value == target_value);
    cpu.registers.af.flags.set(Flag::N, true);
    cpu.registers
        .af
        .flags
        .set(Flag::H, (a_register_value & 0x0F) < (target_value & 0x0F));
    cpu.registers
        .af
        .flags
        .set(Flag::C, a_register_value < target_value);
}

pub fn cpl_operation(cpu: &mut Cpu) {
    let accumulator = cpu.registers.af.accumulator;
    cpu.registers.af.accumulator = !accumulator;
    cpu.registers.af.flags.set(Flag::N, true);
    cpu.registers.af.flags.set(Flag::H, true);
}

pub fn scf_operation(cpu: &mut Cpu) {
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers.af.flags.set(Flag::H, false);
    cpu.registers.af.flags.set(Flag::C, true);
}

pub fn ccf_operation(cpu: &mut Cpu) {
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers.af.flags.set(Flag::H, false);
    cpu.registers
        .af
        .flags
        .set(Flag::C, !cpu.registers.af.flags.get(Flag::C));
}

#[inline]
fn carry_add_8(a: u8, b: u8) -> bool {
    (a as u16 + b as u16) > 0xFF
}

#[inline]
fn carry_sub_8(a: u8, b: u8) -> bool {
    a < b
}

#[inline]
fn carry_add_16(a: u16, b: u16) -> bool {
    (a as u32 + b as u32) > 0xFFFF
}

#[inline]
fn carry_sub_16(a: u16, b: u16) -> bool {
    a < b
}

#[inline]
fn half_add_carry_8(a: u8, b: u8) -> bool {
    ((a & 0x0F) + (b & 0x0F)) > 0x0F
}

fn half_carry_sub_8(a: u8, b: u8) -> bool {
    (a & 0x0F) < (b & 0x0F)
}

#[inline]
fn half_add_carry_16(a: u16, b: u16) -> bool {
    ((a & 0x0FFF) + (b & 0x0FFF)) > 0x0FFF
}

#[inline]
fn half_sub_carry_16(a: u16, b: u16) -> bool {
    (a & 0x00FF) < (b & 0x00FF)
}

pub fn add_operation(
    cpu: &mut Cpu,
    target1: Register,
    target2: OpTarget,
    u8_value: Option<u8>,
    i8_value: Option<i8>,
    memory_map: &mut MemoryMap,
) {
    let eight_bit_source_value = match &target2 {
        OpTarget::Register(register) => match register {
            Register::A => Some(cpu.registers.af.accumulator),
            Register::B => Some(cpu.registers.bc.b),
            Register::C => Some(cpu.registers.bc.c),
            Register::D => Some(cpu.registers.de.d),
            Register::E => Some(cpu.registers.de.e),
            Register::H => Some(cpu.registers.hl.h),
            Register::L => Some(cpu.registers.hl.l),
            _ => None,
        },
        OpTarget::Value(vt) => match vt {
            ValueType::deref(DerefSource::Register(Register::HL(HLMode::Normal))) => {
                Some(memory_map.read(cpu.registers.read_u16(Register::HL(HLMode::Normal))))
            }
            ValueType::u8 => u8_value,
            ValueType::i8 => {
                Some(i8_value.expect("No value given for i8 valuetype in add operation") as u8)
            }
            _ => None,
        },
    };
    let sixteen_bit_source_value = match &target2 {
        OpTarget::Register(register) => match register {
            Register::BC => Some(cpu.registers.read_u16(Register::BC)),
            Register::DE => Some(cpu.registers.read_u16(Register::DE)),
            Register::HL(_) => Some(cpu.registers.read_u16(Register::HL(HLMode::Normal))),
            Register::SP => Some(cpu.registers.read_u16(Register::SP)),
            _ => None,
        },
        _ => None,
    };

    if let Some(value) = eight_bit_source_value {
        match i8_value {
            Some(_) => match target1 {
                Register::SP => {
                    let stackpointer = cpu.registers.sp.stackpointer;
                    let signed_value = value as i16;
                    let stackpointer_low_byte = stackpointer as u8;
                    let value_low_byte = value;
                    cpu.registers.af.flags.set(
                        Flag::H,
                        ((stackpointer_low_byte & 0x0F) + (value_low_byte & 0x0F)) > 0x0F,
                    );
                    cpu.registers.af.flags.set(
                        Flag::C,
                        (stackpointer_low_byte as u16 + value_low_byte as u16) > 0xFF,
                    );
                    cpu.registers.af.flags.set(Flag::Z, false);
                    cpu.registers.af.flags.set(Flag::N, false);
                    cpu.registers.sp.stackpointer = stackpointer.wrapping_add_signed(signed_value);
                    return;
                }
                _ => panic!("i8 given for add operation on register other than stackpointer"),
            },
            None => match target1 {
                Register::A => {
                    let old_accumulator = cpu.registers.af.accumulator;
                    let new_value = cpu.registers.af.accumulator.wrapping_add(value);
                    cpu.registers.af.accumulator = new_value;
                    cpu.registers.af.flags.set(Flag::N, false);
                    cpu.registers
                        .af
                        .flags
                        .set(Flag::C, carry_add_8(old_accumulator, value));
                    cpu.registers
                        .af
                        .flags
                        .set(Flag::H, half_add_carry_8(old_accumulator, value));
                    cpu.registers
                        .af
                        .flags
                        .set(Flag::Z, cpu.registers.af.accumulator == 0);
                    return;
                }
                _ => panic!("8bit value given to add operation to register other than accumulator"),
            },
        }
    }
    match target1 {
        Register::HL(HLMode::Normal) => {
            let hl_value = cpu.registers.read_u16(Register::HL(HLMode::Normal));
            let src_value =
                sixteen_bit_source_value.expect("No value for 16 bit add to HL Register");
            cpu.registers
                .write_u16(Register::HL(HLMode::Normal), src_value);

            cpu.registers
                .af
                .flags
                .set(Flag::H, half_add_carry_16(hl_value, src_value));
            cpu.registers
                .af
                .flags
                .set(Flag::C, carry_add_16(hl_value, src_value));
            cpu.registers.af.flags.set(Flag::N, false);
        }
        _ => panic!("Invalid register given for 16 bit add"),
    }
}

pub fn adc_operation(
    cpu: &mut Cpu,
    value_source: OpTarget,
    u8_value: Option<u8>,
    memory_map: &mut MemoryMap,
) {
    let value = match value_source {
        OpTarget::Register(register) => match register {
            Register::A => cpu.registers.af.accumulator,
            Register::B => cpu.registers.bc.b,
            Register::C => cpu.registers.bc.c,
            Register::D => cpu.registers.de.d,
            Register::E => cpu.registers.de.e,
            Register::H => cpu.registers.hl.h,
            Register::L => cpu.registers.hl.l,
            _ => panic!("16 source bit register given to ADC operation"),
        },
        OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(HLMode::Normal)))) => {
            memory_map.read(cpu.registers.read_u16(Register::HL(HLMode::Normal)))
        }
        OpTarget::Value(ValueType::u8) => u8_value.expect("No value given for ADC A,u8 operation"),
        _ => panic!("Invalid optarget given for adc operation"),
    };
    let original_accumulator = cpu.registers.af.accumulator;
    let total = cpu.registers.af.accumulator.wrapping_add(value);
    if total == 0 {
        cpu.registers.af.flags.set(Flag::Z, false);
    }
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers
        .af
        .flags
        .set(Flag::H, half_add_carry_8(original_accumulator, value));
    cpu.registers
        .af
        .flags
        .set(Flag::C, carry_add_8(original_accumulator, value));
    cpu.registers.af.accumulator = total;
}

pub fn dec_operation(cpu: &mut Cpu, target: OpTarget, memory_map: &mut MemoryMap) {
    match target {
        OpTarget::Register(register) => match register {
            Register::A => {
                cpu.registers.af.accumulator = cpu.registers.af.accumulator.wrapping_sub(1)
            }
            Register::B => cpu.registers.bc.b = cpu.registers.bc.b.wrapping_sub(1),
            Register::C => cpu.registers.bc.c = cpu.registers.bc.c.wrapping_sub(1),
            Register::D => cpu.registers.de.d = cpu.registers.de.d.wrapping_sub(1),
            Register::E => cpu.registers.de.e = cpu.registers.de.e.wrapping_sub(1),
            Register::H => cpu.registers.hl.h = cpu.registers.hl.h.wrapping_sub(1),
            Register::L => cpu.registers.hl.l = cpu.registers.hl.l.wrapping_sub(1),
            Register::BC => {
                let bc_value = cpu.registers.read_u16(Register::BC);
                cpu.registers
                    .write_u16(Register::BC, bc_value.wrapping_sub(1));
            }
            Register::DE => {
                let de_value = cpu.registers.read_u16(Register::DE);
                cpu.registers
                    .write_u16(Register::DE, de_value.wrapping_sub(1));
            }
            Register::HL(_) => {
                let hl_value = cpu.registers.read_u16(Register::HL(HLMode::Normal));
                cpu.registers
                    .write_u16(Register::HL(HLMode::Normal), hl_value.wrapping_sub(1));
            }
            Register::SP => {
                let sp_value = cpu.registers.read_u16(Register::SP);
                cpu.registers
                    .write_u16(Register::SP, sp_value.wrapping_sub(1));
            }
            _ => panic!("Attempting to decrement invalid register"),
        },
        OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(_)))) => {
            let hl_value = cpu.registers.read_u16(Register::HL(HLMode::Normal));
            let deref_value = memory_map.read(hl_value);
            memory_map.write(hl_value, deref_value.wrapping_sub(1));
        }
        _ => panic!("Attempting to decrement invalid target: {target:?}"),
    }
}

pub fn sub_operation(
    cpu: &mut Cpu,
    target: OpTarget,
    value: Option<u8>,
    memory_map: &mut MemoryMap,
) {
    let accumulator = cpu.registers.af.accumulator;
    let value = match target {
        OpTarget::Register(register) => match register {
            Register::A => cpu.registers.af.accumulator,
            Register::B => cpu.registers.bc.b,
            Register::C => cpu.registers.bc.c,
            Register::D => cpu.registers.de.d,
            Register::E => cpu.registers.de.e,
            Register::H => cpu.registers.hl.h,
            Register::L => cpu.registers.hl.l,
            _ => panic!("16 bit register given for SUB operation target"),
        },
        OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(HLMode::Normal)))) => {
            memory_map.read(cpu.registers.read_u16(Register::HL(HLMode::Normal)))
        }
        OpTarget::Value(ValueType::u8) => value.expect("No value given for SUB A,u8"),
        _ => panic!("Invalid OpTarget for SUB operation"),
    };
    cpu.registers.af.flags.set(Flag::Z, accumulator == value);
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers
        .af
        .flags
        .set(Flag::H, half_carry_sub_8(accumulator, value));
    cpu.registers
        .af
        .flags
        .set(Flag::C, carry_sub_8(accumulator, value));
    cpu.registers.af.accumulator = value;
}

pub fn and_operation(
    cpu: &mut Cpu,
    target: OpTarget,
    value: Option<u8>,
    memory_map: &mut MemoryMap,
) {
    let value = match target {
        OpTarget::Register(register) => match register {
            Register::A => cpu.registers.af.accumulator,
            Register::B => cpu.registers.bc.b,
            Register::C => cpu.registers.bc.c,
            Register::D => cpu.registers.de.d,
            Register::E => cpu.registers.de.e,
            Register::H => cpu.registers.hl.h,
            Register::L => cpu.registers.hl.l,
            _ => panic!("16 bit register given as source for AND operation"),
        },
        OpTarget::Value(ValueType::u8) => value.expect("No value given for AND A,u8 operation"),
        OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(HLMode::Normal)))) => {
            memory_map.read(cpu.registers.read_u16(Register::HL(HLMode::Normal)))
        }
        _ => panic!("Invalid source given for AND operation"),
    };
    cpu.registers
        .af
        .flags
        .set(Flag::Z, cpu.registers.af.accumulator == value);
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers.af.flags.set(Flag::H, true);
    cpu.registers.af.flags.set(Flag::C, false);
    cpu.registers.af.accumulator &= value;
}

pub fn xor_operation(
    cpu: &mut Cpu,
    target: OpTarget,
    value: Option<u8>,
    memory_map: &mut MemoryMap,
) {
    let value = match target {
        OpTarget::Register(register) => match register {
            Register::A => cpu.registers.af.accumulator,
            Register::B => cpu.registers.bc.b,
            Register::C => cpu.registers.bc.c,
            Register::D => cpu.registers.de.d,
            Register::E => cpu.registers.de.e,
            Register::H => cpu.registers.hl.h,
            Register::L => cpu.registers.hl.l,
            _ => panic!("16 bit register given as source for XOR operation"),
        },
        OpTarget::Value(ValueType::u8) => value.expect("No value given for XOR A,u8 operation"),
        OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(HLMode::Normal)))) => {
            memory_map.read(cpu.registers.read_u16(Register::HL(HLMode::Normal)))
        }
        _ => panic!("Invalid source given for XOR operation"),
    };
    cpu.registers.af.accumulator ^= value;

    cpu.registers
        .af
        .flags
        .set(Flag::Z, cpu.registers.af.accumulator != 0);
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers.af.flags.set(Flag::H, false);
    cpu.registers.af.flags.set(Flag::C, false);
}

pub fn or_operation(
    cpu: &mut Cpu,
    target: OpTarget,
    value: Option<u8>,
    memory_map: &mut MemoryMap,
) {
    let value = match target {
        OpTarget::Register(register) => match register {
            Register::A => cpu.registers.af.accumulator,
            Register::B => cpu.registers.bc.b,
            Register::C => cpu.registers.bc.c,
            Register::D => cpu.registers.de.d,
            Register::E => cpu.registers.de.e,
            Register::H => cpu.registers.hl.h,
            Register::L => cpu.registers.hl.l,
            _ => panic!("16 bit register given as source for OR operation"),
        },
        OpTarget::Value(ValueType::u8) => value.expect("No value given for OR A,u8 operation"),
        OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(HLMode::Normal)))) => {
            memory_map.read(cpu.registers.read_u16(Register::HL(HLMode::Normal)))
        }
        _ => panic!("Invalid source given for OR operation"),
    };
    cpu.registers.af.accumulator |= value;

    cpu.registers
        .af
        .flags
        .set(Flag::Z, cpu.registers.af.accumulator != 0);
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers.af.flags.set(Flag::H, false);
    cpu.registers.af.flags.set(Flag::C, false);
}

pub fn inc_operation(cpu: &mut Cpu, target: OpTarget, memory_map: &mut MemoryMap) {
    match target {
        OpTarget::Register(register) => match register {
            Register::A => {
                cpu.registers.af.accumulator = cpu.registers.af.accumulator.wrapping_add(1)
            }
            Register::B => cpu.registers.bc.b = cpu.registers.bc.b.wrapping_add(1),
            Register::C => cpu.registers.bc.c = cpu.registers.bc.c.wrapping_add(1),
            Register::D => cpu.registers.de.d = cpu.registers.de.d.wrapping_add(1),
            Register::E => cpu.registers.de.e = cpu.registers.de.e.wrapping_add(1),
            Register::H => cpu.registers.hl.h = cpu.registers.hl.h.wrapping_add(1),
            Register::L => cpu.registers.hl.l = cpu.registers.hl.l.wrapping_add(1),
            Register::BC => {
                let bc_value = cpu.registers.read_u16(Register::BC);
                cpu.registers
                    .write_u16(Register::BC, bc_value.wrapping_add(1));
            }
            Register::DE => {
                let de_value = cpu.registers.read_u16(Register::DE);
                cpu.registers
                    .write_u16(Register::DE, de_value.wrapping_add(1));
            }
            Register::HL(_) => {
                let hl_value = cpu.registers.read_u16(Register::HL(HLMode::Normal));
                cpu.registers
                    .write_u16(Register::HL(HLMode::Normal), hl_value.wrapping_add(1));
            }
            Register::SP => {
                let sp_value = cpu.registers.read_u16(Register::SP);
                cpu.registers
                    .write_u16(Register::SP, sp_value.wrapping_add(1));
            }
            _ => panic!("Attempting to increment invalid register"),
        },
        OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(_)))) => {
            let hl_value = cpu.registers.read_u16(Register::HL(HLMode::Normal));
            let deref_value = memory_map.read(hl_value);
            memory_map.write(hl_value, deref_value.wrapping_add(1));
        }
        _ => panic!("Attempting to increment invalid target: {target:?}"),
    }
}

pub fn rra_operation(cpu: &mut Cpu) {
    let old_carry = cpu.registers.af.flags.get(Flag::C) as u8;
    let old_bit0 = cpu.registers.af.accumulator & 0x1;

    cpu.registers.af.accumulator >>= 1;
    cpu.registers.af.accumulator |= old_carry << 7;

    cpu.registers.af.flags.set(Flag::C, old_bit0 != 0);
    cpu.registers.af.flags.set(Flag::Z, false);
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers.af.flags.set(Flag::H, false);
}

pub fn rla_operation(cpu: &mut Cpu) {
    let old_carry = cpu.registers.af.flags.get(Flag::C) as u8;
    let old_bit8 = cpu.registers.af.accumulator >> 0x07;

    cpu.registers.af.accumulator >>= 1;
    cpu.registers.af.accumulator |= old_carry << 7;

    cpu.registers.af.flags.set(Flag::C, old_bit8 != 0);
    cpu.registers.af.flags.set(Flag::Z, false);
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers.af.flags.set(Flag::H, false);
}

pub fn rlca_operation(cpu: &mut Cpu) {
    let old_bit8 = cpu.registers.af.accumulator >> 0x07;
    cpu.registers.af.accumulator <<= 0x01;
    cpu.registers.af.accumulator |= old_bit8;
    cpu.registers.af.flags.set(Flag::C, old_bit8 != 0);
    cpu.registers.af.flags.set(Flag::Z, false);
    cpu.registers.af.flags.set(Flag::N, false);
    cpu.registers.af.flags.set(Flag::H, false);
}

pub fn jr_operation(cpu: &mut Cpu, condition: Condition, value: i8) {
    match condition {
        Condition::Z => {
            if !cpu.registers.af.flags.get(Flag::Z) {
                return;
            }
        }
        Condition::NZ => {
            if cpu.registers.af.flags.get(Flag::Z) {
                return;
            }
        }
        Condition::C => {
            if !cpu.registers.af.flags.get(Flag::C) {
                return;
            }
        }
        Condition::NC => {
            if cpu.registers.af.flags.get(Flag::C) {
                return;
            }
        }
        Condition::None => (),
    }
    cpu.registers.pc.programcounter = cpu
        .registers
        .pc
        .programcounter
        .wrapping_add_signed(value as i16);
}

pub fn jp_operation(
    cpu: &mut Cpu,
    condition: Condition,
    target: OpTarget,
    value1: Option<u8>,
    value2: Option<u8>,
) {
    match condition {
        Condition::Z => {
            if !cpu.registers.af.flags.get(Flag::Z) {
                return;
            }
        }
        Condition::NZ => {
            if cpu.registers.af.flags.get(Flag::Z) {
                return;
            }
        }
        Condition::C => {
            if !cpu.registers.af.flags.get(Flag::C) {
                return;
            }
        }
        Condition::NC => {
            if cpu.registers.af.flags.get(Flag::C) {
                return;
            }
        }
        Condition::None => (),
    }
    let value: u16 = match target {
        OpTarget::Value(ValueType::u16) => {
            let high = value1.expect("No value given for jp operation") as u16;
            let low = value2.expect("Second byte not given for JP operation") as u16;
            (high << 8) | low
        }
        OpTarget::Register(Register::HL(HLMode::Normal)) => {
            cpu.registers.read_u16(Register::HL(HLMode::Normal))
        }
        _ => panic!("invalid target given to jp operation"),
    };
    cpu.registers.write_u16(Register::PC, value)
}

pub fn ld_operation(
    cpu: &mut Cpu,
    target1: OpTarget,
    target2: OpTarget,
    byte_two: Option<u8>,
    byte_three: Option<u8>,
    memory_map: &mut MemoryMap,
) {
    let (dest_length, dest_register) = match_optarget(&target1);
    let (source_length, _source_register) = match_optarget(&target2);

    assert!(source_length == dest_length);
    let length = &source_length;
    let eight_bit_source_value = match source_length {
        FieldLength::u16 => None,
        FieldLength::u8 => match &target2 {
            OpTarget::Register(reg) => match reg {
                Register::A => Some(cpu.registers.af.accumulator),
                Register::B => Some(cpu.registers.bc.b),
                Register::C => Some(cpu.registers.bc.c),
                Register::D => Some(cpu.registers.de.d),
                Register::E => Some(cpu.registers.de.e),
                Register::H => Some(cpu.registers.hl.h),
                Register::L => Some(cpu.registers.hl.l),
                _ => panic!("16 bit register given for 8 bit LD operation"),
            },
            OpTarget::Value(vt) => match vt {
                ValueType::i8 => byte_two,
                ValueType::u8 => byte_two,
                ValueType::deref(source) => match source {
                    DerefSource::Register(reg) => match reg {
                        Register::BC => {
                            let high = cpu.registers.bc.b as u16;
                            let low = cpu.registers.bc.b as u16;
                            Some(memory_map.read((high << 8) | low))
                        }
                        Register::DE => {
                            let high = cpu.registers.de.d as u16;
                            let low = cpu.registers.de.e as u16;
                            Some(memory_map.read((high << 8) | low))
                        }
                        Register::HL(hlm) => {
                            let high = cpu.registers.hl.h as u16;
                            let low = cpu.registers.hl.l as u16;
                            let output = Some(memory_map.read((high << 8) | low));
                            match hlm {
                                HLMode::Normal => (),
                                HLMode::Increment => {
                                    cpu.registers.hl.l = cpu.registers.hl.l.wrapping_add(1)
                                }
                                HLMode::Decrement => {
                                    cpu.registers.hl.l = cpu.registers.hl.l.wrapping_sub(1)
                                }
                            }
                            output
                        }
                        _ => panic!("Invalid register given to dereference in 8 bit LD operation"),
                    },
                    DerefSource::u16 => Some(memory_map.read({
                        let high = byte_two.expect("no upper byte given to 16bit deref") as u16;
                        let low = byte_three.expect("no lower byte given to 16bit deref") as u16;
                        (high << 8) | low
                    })),
                },
                _ => panic!("16 bit value given to 8 bit LD operation"),
            },
        },
    };
    let sixteen_bit_source_value = match source_length {
        FieldLength::u8 => None,
        FieldLength::u16 => match &target2 {
            OpTarget::Register(reg) => match reg {
                Register::AF => Some(cpu.registers.read_u16(Register::AF)),
                Register::BC => Some(cpu.registers.read_u16(Register::BC)),
                Register::DE => Some(cpu.registers.read_u16(Register::DE)),
                Register::HL(hlmode) => match hlmode {
                    HLMode::Normal => Some(cpu.registers.read_u16(Register::HL(HLMode::Normal))),
                    HLMode::Increment => {
                        let output = Some(cpu.registers.read_u16(Register::HL(HLMode::Increment)));
                        cpu.registers.hl.l = cpu.registers.hl.l.wrapping_add(1);
                        output
                    }
                    HLMode::Decrement => {
                        let output = Some(cpu.registers.read_u16(Register::HL(HLMode::Decrement)));
                        cpu.registers.hl.l = cpu.registers.hl.l.wrapping_sub(1);
                        output
                    }
                },
                Register::SP => Some(cpu.registers.read_u16(Register::SP)),
                Register::PC => Some(cpu.registers.read_u16(Register::PC)),
                _ => panic!("8 bit register given to 16 bit LD operation"),
            },
            OpTarget::Value(vt) => match vt {
                ValueType::u16 => {
                    Some(((byte_two.unwrap() as u16) << 8) | (byte_three.unwrap() as u16))
                }
                _ => panic!("Invalid source value type sent to 16 bit LD operation)"),
            },
        },
    };
    match length {
        FieldLength::u8 => match target1 {
            OpTarget::Register(_) => match dest_register
                .expect("dest_register not set for register based 8 bit LD operation")
            {
                Register::A => {
                    cpu.registers.af.accumulator = eight_bit_source_value
                        .expect("8bit source did not contain a value for 8bit LD operation")
                }
                Register::B => {
                    cpu.registers.bc.b = eight_bit_source_value
                        .expect("8bit source did not contain a value for 8bit LD operation")
                }
                Register::C => {
                    cpu.registers.bc.c = eight_bit_source_value
                        .expect("8bit source did not contain a value for 8bit LD operation")
                }
                Register::D => {
                    cpu.registers.de.d = eight_bit_source_value
                        .expect("8bit source did not contain a value for 8bit LD operation")
                }
                Register::E => {
                    cpu.registers.de.e = eight_bit_source_value
                        .expect("8bit source did not contain a value for 8bit LD operation")
                }
                Register::H => {
                    cpu.registers.hl.h = eight_bit_source_value
                        .expect("8bit source did not contain a value for 8bit LD operation")
                }
                Register::L => {
                    cpu.registers.hl.l = eight_bit_source_value
                        .expect("8bit source did not contain a value for 8bit LD operation")
                }
                _ => panic!("16 bit register given to 8 bit LD operation"),
            },
            OpTarget::Value(ValueType::deref(deref_source)) => match deref_source {
                DerefSource::Register(Register::HL(m)) => memory_map.write(
                    cpu.registers.read_u16(Register::HL(m)),
                    eight_bit_source_value.expect("Butts2"),
                ),
                DerefSource::u16 => memory_map.write(
                    (byte_two.expect("High byte not given for u16 deref") as u16)
                        << byte_three.expect("Low byte not given for u16 deref"),
                    eight_bit_source_value
                        //.expect("8 bit source did not contain a value for 8bit LD operation"),
                        .expect("Butts"),
                ),
                _ => panic!("What"),
            },
            _ => panic!("Invalid source given for 8bit LD operation"),
        },
        FieldLength::u16 => match target1 {
            OpTarget::Register(_) => match dest_register
                .expect("dest_register not set for register based 16 bit ld operation")
            {
                Register::BC => cpu.registers.write_u16(
                    Register::BC,
                    sixteen_bit_source_value
                        .expect("16bit source did not contain a value for 16bit LD operation"),
                ),
                Register::HL(_) => cpu.registers.write_u16(
                    Register::HL(HLMode::Normal),
                    sixteen_bit_source_value
                        .expect("16bit source did not contain a value for 16bit LD operation"),
                ),
                _ => panic!("invalid optarget for 16 bit ld operation: {target1:?}"),
            },
            _ => todo!(),
        },
    }
}
