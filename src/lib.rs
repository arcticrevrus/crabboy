mod cpu;
mod memory;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpu::*;
    use crate::memory::*;

    const ENTRY_POINT: u16 = 0x0100;
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

    #[test]
    fn test_nop() {
        let mut memory1 = memory::MemoryMap::new();
        let mut cpu1 = cpu::Cpu::new(cpu::Mode::DMG);
        let mut memory2 = memory::MemoryMap::new();
        let mut cpu2 = cpu::Cpu::new(cpu::Mode::DMG);
        // NOP
        let bytes = [0x00, 0xDB];
        let mut memory_index = ENTRY_POINT;
        for byte in bytes {
            memory1.write(memory_index, byte);
            memory2.write(memory_index, byte);
            memory_index += 1;
        }
        while cpu1.is_running {
            cpu1.execute_next_instruction(&mut memory1);
            cpu2.execute_next_instruction(&mut memory2);
            assert!(cpu1.registers == cpu2.registers);
            assert!(memory1 == memory2);
            memory1.write(memory_index, 0xDB);
            memory2.write(memory_index, 0xDB);
        }
    }

    #[test]
    fn test_add() {
        let mut memory = memory::MemoryMap::new();
        let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
        /*
        LD  A,0x00
        RLCA
        LD  A,0x0E
        LD  C,0x01
        ADD A,C
        LD  B,0x01 ; toggle B to trigger register reads
        LD  B,0x00
        LD  A,0x00
        RLCA
        LD  A,0x0F
        ADD A,C
        LD  B,0x01
        LD  B,0x00
        LD  A,0x00
        RLCA
        LD  A,0xFF
        LD  C,0x01
        ADD A,C
        LD  B,0x01
        LD  B,0x00
        PANIC
        */
        let bytes = [
            0x3E, 0x00, 0x07, 0x3E, 0x0E, 0x0E, 0x01, 0x81, 0x06, 0x01, 0x06, 0x00, 0x3E, 0x00,
            0x07, 0x3E, 0x0F, 0x0E, 0x01, 0x81, 0x06, 0x01, 0x06, 0x00, 0x3E, 0x00, 0x07, 0x3E,
            0xFF, 0x0E, 0x01, 0x81, 0x06, 0x01, 0x06, 0x00, 0xDB,
        ];
        let mut memory_index = ENTRY_POINT;
        for byte in bytes {
            memory.write(memory_index, byte);
            memory_index += 1;
        }
        let mut cpu_state: Vec<Cpu> = Vec::new();
        while cpu.is_running {
            if cpu.registers.bc.b == 0 {
                cpu.execute_next_instruction(&mut memory);
            } else {
                cpu_state.push(cpu);
                cpu.execute_next_instruction(&mut memory);
            }
        }
        for i in 1..=3 {
            let state = cpu_state.pop().unwrap();
            match i {
                1 => {
                    assert!(state.registers.af.accumulator == 0x00);
                    assert!(state.registers.af.flags.bits == 0b1011_0000);
                }
                2 => {
                    assert!(state.registers.af.accumulator == 0x10);
                    assert!(state.registers.af.flags.bits == 0b0010_0000);
                }
                3 => {
                    assert!(state.registers.af.accumulator == 0x0F);
                    assert!(state.registers.af.flags.bits == 0b0000_0000);
                }
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_inc() {
        let mut memory = memory::MemoryMap::new();
        let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
        /*
        LD  A,0x0001
        INC A
        INC A
        LD  HL,0xFF00
        LD  (HL),0x01
        INC (HL)
        PANIC
        */
        let bytes = [
            0xFA, 0x00, 0x01, 0x3C, 0x3C, 0x21, 0xFF, 0x00, 0x36, 0x01, 0x34, 0xDB,
        ];
        let mut memory_index = ENTRY_POINT;
        for byte in bytes {
            memory.write(memory_index, byte);
            memory_index += 1;
        }
        while cpu.is_running {
            cpu.execute_next_instruction(&mut memory);
        }
        assert!(cpu.registers.af.accumulator == 0x02);
        assert!(memory.read(0xFF00) == 0x02);
    }

    #[test]
    fn test_dec() {
        let mut memory = memory::MemoryMap::new();
        let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
        let mut memory_index = ENTRY_POINT;
        /*
        LD  A,0x0002
        DEC A
        LD  HL,0xFF00
        LD  (HL),0x02
        DEC (HL)
        HALT
        */
        let bytes = [0x3E, 0x02, 0x3D, 0x21, 0xFF, 0x00, 0x36, 0x02, 0x35, 0xDB];
        for byte in bytes {
            memory.write(memory_index, byte);
            memory_index += 1;
        }
        while cpu.is_running {
            cpu.execute_next_instruction(&mut memory);
        }
        assert!(cpu.registers.af.accumulator == 0x01);
        assert!(memory.read(0xFF00) == 0x01);
    }

    #[test]
    fn test_ld() {
        let mut memory = memory::MemoryMap::new();
        let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);
        let mut memory_index = ENTRY_POINT;
        /*
        LD  B,0xAC
        LD  HL,0xC000
        LD  (HL),0xAC
        PANIC
        */
        let bytes = [0x06, 0xAC, 0x21, 0xC0, 0x00, 0x36, 0xAC, 0xDB];
        for byte in bytes {
            memory.write(memory_index, byte);
            memory_index += 1;
        }
        while memory.read(cpu.registers.pc.programcounter) != 0x00 {
            cpu.execute_next_instruction(&mut memory);
        }
        assert!(cpu.registers.read_u16(Register::HL(HLMode::Normal)) == 0xC000);
        assert!(cpu.registers.bc.b == 0xAC);
        assert!(memory.read(0xC000) == 0xAC);
    }

    #[test]
    fn test_hello() {
        let mut memory = memory::MemoryMap::new();
        let mut cpu = cpu::Cpu::new(cpu::Mode::DMG);

        /*
        LD HL,0xC000
        LD (HL),ASCII_H_UPPER
        INC HL
        LD (HL),ASCII_E_LOWER
        INC HL
        LD (HL),ASCII_L_LOWER
        INC HL
        LD (HL),ASCII_L_LOWER
        INC HL
        LD (HL),ASCII_O_LOWER
        INC HL
        LD (HL),ASCII_SPACE
        INC HL
        LD (HL),ASCII_W_LOWER
        INC HL
        LD (HL),ASCII_O_LOWER
        INC HL
        LD (HL),ASCII_R_LOWER
        INC HL
        LD (HL),ASCII_L_LOWER
        INC HL
        LD (HL),ASCII_D_LOWER
        INC HL
        LD (HL),ASCII_BANG
        LD HL,0xC000
        LABEL: COPY_CHARACTER_TO_B_REGISTER_IF_NOT_EMPTY
        RRA
        LD A,(HL+)
        LD B,A
        LD C,0x01   ; Using the C register to trigger reading from B register in rust
        LD C,0x00
        CP A,0x00
        JP NZ,COPY_CHARACTER_TO_B_REGISTER_IF_NOT_EMPTY
        PANIC
        */
        let hello = [
            0x21,
            0xC0,
            0x00,
            0x36,
            ASCII_H_UPPER,
            0x23,
            0x36,
            ASCII_E_LOWER,
            0x23,
            0x36,
            ASCII_L_LOWER,
            0x23,
            0x36,
            ASCII_L_LOWER,
            0x23,
            0x36,
            ASCII_O_LOWER,
            0x23,
            0x36,
            ASCII_COMMA,
            0x23,
            0x36,
            ASCII_SPACE,
            0x23,
            0x36,
            ASCII_W_LOWER,
            0x23,
            0x36,
            ASCII_O_LOWER,
            0x23,
            0x36,
            ASCII_R_LOWER,
            0x23,
            0x36,
            ASCII_L_LOWER,
            0x23,
            0x36,
            ASCII_D_LOWER,
            0x23,
            0x36,
            ASCII_BANG,
            0x21,
            0xC0,
            0x00,
            0x1F,
            0x2A,
            0x47,
            0x0E,
            0x01,
            0x0E,
            0x00,
            0xFE,
            0x00,
            0xC2,
            0x01,
            0x2D,
            0xDB,
        ];
        let mut memory_index: u16 = ENTRY_POINT;
        for byte in hello {
            memory.write(memory_index, byte);
            memory_index += 1;
        }
        let mut world = String::new();
        while cpu.is_running {
            cpu.execute_next_instruction(&mut memory);
            if cpu.registers.bc.c == 0x01 {
                world.push(cpu.registers.bc.b as char);
            }
        }
        assert!(world == "Hello, world!\0");
    }
}
