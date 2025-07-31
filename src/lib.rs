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
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
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
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
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
            OpTarget::Value(ValueType::deref(DerefSource::Register(Register::HL(
                HLMode::Normal,
            )))),
            OpTarget::Value(ValueType::u8),
            Some(0xAC),
            None,
            &mut memory,
        );

        assert!(cpu.registers.read_u16(Register::HL(HLMode::Normal)) == 0xC000);
        assert!(cpu.registers.bc.b == 0xAC);
        assert!(memory.read(0xC000) == 0xAC);
    }

    #[test]
    fn test_hello() {
        const ASCII_H_UPPER: u8 = 0x48;
        const ASCII_E_LOWER: u8 = 0x65;
        const ASCII_L_LOWER: u8 = 0x6C;
        const ASCII_O_LOWER: u8 = 0x6F;
        const ASCII_COMMA: u8 = 0x2C;
        const ASCII_SPACE: u8 = 0x20;
        const ASCII_W_LOWER: u8 = 0x77;
        const ASCII_R_LOWER: u8 = 0x72;
        const ASCII_D_LOWER: u8 = 0x64;
        const ASCII_BANG: u8 = 0x21;

        let mut memory = memory::MemoryMap::new();
        let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);

        let string_instruction_count = 26;
        let hello = [
            // Write 0xC00 to HL register
            0x21,
            0xC0,
            0x00,
            // Write ascii H to HL dereference
            0x36,
            ASCII_H_UPPER,
            // Increment L register
            0x2C,
            // Write ascii e to HL dereference
            0x36,
            ASCII_E_LOWER,
            // Increment L register
            0x2C,
            0x36,
            ASCII_L_LOWER,
            0x2C,
            0x36,
            ASCII_L_LOWER,
            0x2C,
            0x36,
            ASCII_O_LOWER,
            0x2c,
            0x36,
            ASCII_COMMA,
            0x2C,
            0x36,
            ASCII_SPACE,
            0x2c,
            0x36,
            ASCII_W_LOWER,
            0x2c,
            0x36,
            ASCII_O_LOWER,
            0x2c,
            0x36,
            ASCII_R_LOWER,
            0x2c,
            0x36,
            ASCII_L_LOWER,
            0x2c,
            0x36,
            ASCII_D_LOWER,
            0x2c,
            0x36,
            ASCII_BANG,
            // Write 0xC00 to HL register again
            0x21,
            0xC0,
            0x00, //0x0129
            // LABEL: COPY_CHARACTER_TO_B_REGISTER_IF_NOT_EMPTY (x012C)
            // Rotate A register right and clear flags
            0x1F, //0x012C
            // Load HL deref to Accumulator and increment HL
            0x2A, //0x012D
            // LD Accumulator to B register
            0x47, //0x012E
            // Using the C register to trigger reading from B register in rust
            // Set C register to 1
            0x0E,
            0x01, //0x012F
            // Set C register to 0
            0x0E,
            0x00, //0x0131
            // Compare Accumulator to 0x00
            0xFE,
            0x00, //0x0133
            // Jump to COPY_CHARACTER_TO_B_REGISTER if Z flag is not set:
            0xC2,
            0x01,
            0x2C, //0x0135
            // Load 0x00 to B Register
            0x06,
            0xFF, //0x0138
        ];
        let mut memory_index: u16 = 0x0100;
        for byte in hello {
            memory.write(memory_index, byte);
            memory_index += 0x0001;
        }
        for _ in 0..string_instruction_count {
            cpu.execute_next_instruction(&mut memory);
        }

        let mut world = String::new();
        while cpu.registers.bc.b != 0xFF {
            cpu.execute_next_instruction(&mut memory);
            if cpu.registers.bc.c == 0x01 {
                world.push(cpu.registers.bc.b as char);
            }
        }
        assert!(world == "Hello, world!\0");
    }
}
