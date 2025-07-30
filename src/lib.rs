mod asm;
mod cpu;
mod memory;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asm::*;
    use crate::cpu::*;
    use crate::memory::*;

    #[test]
    fn test_nop() {
        let memory1 = memory::MemoryMap::new();
        let cpu1 = cpu::Cpu::new(cpu::Mode::DMG);
        let memory2 = memory::MemoryMap::new();
        let cpu2 = cpu::Cpu::new(cpu::Mode::DMG);
        nop_operation();
        assert!(cpu1.registers == cpu2.registers);
        assert!(memory1 == memory2);
    }

    #[test]
    fn test_inc() {
        let mut memory = memory::MemoryMap::new();
        let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
        cpu.registers.af.accumulator = 0x01;
        inc_operation(&mut cpu, OpTarget::Register(Register::A), &mut memory);
        assert!(cpu.registers.af.accumulator == 0x02);
        memory.write(0xFF00, 0x01);
        cpu.registers
            .write_u16(Register::HL(HLMode::Normal), 0xFF00);
        inc_operation(
            &mut cpu,
            OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            &mut memory,
        );
        assert!(memory.read(0xFF00) == 0x02);
    }

    #[test]
    fn test_dec() {
        let mut memory = memory::MemoryMap::new();
        let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
        cpu.registers.af.accumulator = 0x02;
        dec_operation(&mut cpu, OpTarget::Register(Register::A), &mut memory);
        assert!(cpu.registers.af.accumulator == 0x01);
        memory.write(0xFF00, 0x02);
        cpu.registers
            .write_u16(Register::HL(HLMode::Normal), 0xFF00);
        dec_operation(
            &mut cpu,
            OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            &mut memory,
        );
        assert!(memory.read(0xFF00) == 0x01);
    }

    #[test]
    fn test_ld() {
        let mut memory = memory::MemoryMap::new();
        let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);

        ld_operation(
            &mut cpu,
            OpTarget::Register(Register::B),
            OpTarget::Value(ValueType::u8),
            Some(0xAC),
            None,
            &mut memory,
        );
        ld_operation(
            &mut cpu,
            OpTarget::Register(Register::HL(HLMode::Normal)),
            OpTarget::Value(ValueType::u16),
            Some(0xC0),
            Some(0x00),
            &mut memory,
        );
        ld_operation(
            &mut cpu,
            OpTarget::Value(ValueType::deref(Register::HL(HLMode::Normal))),
            OpTarget::Value(ValueType::u8),
            Some(0xAC),
            None,
            &mut memory,
        );

        assert!(cpu.registers.read_u16(Register::HL(HLMode::Normal)) == 0xC000);
        assert!(cpu.registers.bc.b == 0xAC);
        assert!(memory.read(0xC000) == 0xAC);
    }
}
