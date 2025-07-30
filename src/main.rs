use crate::memory::*;

mod asm;
mod cartridge;
mod cpu;
mod memory;

fn main() {
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
    let hello  = [
        // Write 0xC00 to HL register
        0x21, 0xC0, 0x00, 
        // Write ascii H to HL dereference
        0x36, ASCII_H_UPPER,
        // Increment L register
        0x2C,
        // Write ascii e to HL dereference
        0x36, ASCII_E_LOWER,
        // Increment L register
        0x2C,
        0x36, ASCII_L_LOWER,
        0x2C,
        0x36, ASCII_L_LOWER,
        0x2C,
        0x36, ASCII_O_LOWER,
        0x2c,
        0x36, ASCII_COMMA,
        0x2C,
        0x36, ASCII_SPACE,
        0x2c,
        0x36, ASCII_W_LOWER,
        0x2c,
        0x36, ASCII_O_LOWER,
        0x2c,
        0x36, ASCII_R_LOWER,
        0x2c,
        0x36, ASCII_L_LOWER,
        0x2c,
        0x36, ASCII_D_LOWER,
        0x2c,
        0x36, ASCII_BANG, 
        // Write 0xC00 to HL register again
        0x21, 0xC0, 0x00, //0x0129
        // LABEL: COPY_CHARACTER_TO_B_REGISTER_IF_NOT_EMPTY (x012C)
        // Rotate A register right and clear flags
        0x1F, //0x012C
        // Load HL deref to Accumulator and increment HL
        0x2A, //0x012D
        // LD Accumulator to B register
        0x47, //0x012E
        // Using the C register to trigger reading from B register in rust
        // Set C register to 1
        0x0E, 0x01, //0x012F
        // Set C register to 0
        0x0E, 0x00, //0x0131
        // Compare Accumulator to 0x00
        0xFE, 0x00, //0x0133
        // Jump to COPY_CHARACTER_TO_B_REGISTER if Z flag is not set:
        0xC2, 0x01, 0x2C, //0x0135
        // Load 0x00 to B Register
        0x06, 0xFF, //0x0138
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
    println!("{world}");
}
