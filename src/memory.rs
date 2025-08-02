macro_rules! memory_region {
    (
        $name:ident,    // Struct name
        $size:expr,     // Size of the memory array
        $offset:expr    // Address offset
    ) => {
        #[derive(Copy, Clone, PartialEq)]
        struct $name {
            memory: [u8; $size],
        }

        impl $name {
            pub fn new() -> Self {
                Self { memory: [0; $size] }
            }
        }

        impl Memory for $name {
            fn read(&self, address: u16) -> u8 {
                self.memory[(address - $offset) as usize]
            }

            fn write(&mut self, address: u16, value: u8) {
                self.memory[(address - $offset) as usize] = value;
            }
        }
    };
}

pub trait Memory {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);
}

memory_region!(Rom0, 0x4000, 0x0000);
memory_region!(RomX, 0x4000, 0x4000);
memory_region!(VRam, 0x2000, 0x8000);
memory_region!(SRam, 0x2000, 0xA000);
memory_region!(WRam0, 0x1000, 0xC000);
memory_region!(WRamX, 0x1000, 0xD000);
memory_region!(Echo, 0x1D00, 0xE000);
memory_region!(Aom, 0x00A0, 0xFE00);
memory_region!(UnusedMemory, 0x0050, 0xFEA0);
memory_region!(IORegisters, 0x0080, 0xFF00);
memory_region!(HRam, 0x007F, 0xFF80);
memory_region!(IERegister, 0x0001, 0xFFFF);

#[derive(Copy, Clone, PartialEq)]
pub struct MemoryMap {
    rom0: Rom0,
    romx: RomX,
    vram: VRam,
    sram: SRam,
    wram0: WRam0,
    wramx: WRamX,
    echo: Echo,
    aom: Aom,
    unused: UnusedMemory,
    io_registers: IORegisters,
    hram: HRam,
    ie_register: IERegister,
}

impl Memory for MemoryMap {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x3FFF => self.rom0.read(address),
            0x4000..=0x7FFF => self.romx.read(address),
            0x8000..=0x9FFF => self.vram.read(address),
            0xA000..=0xBFFF => self.sram.read(address),
            0xC000..=0xCFFF => self.wram0.read(address),
            0xD000..=0xDFFF => self.wramx.read(address),
            0xE000..=0xFDFF => self.echo.read(address),
            0xFE00..=0xFE9F => self.aom.read(address),
            0xFEA0..=0xFEFF => self.unused.read(address),
            0xFF00..=0xFF7F => self.io_registers.read(address),
            0xFF80..=0xFFFE => self.hram.read(address),
            0xFFFF => self.ie_register.read(address),
        }
    }
    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x3FFF => self.rom0.write(address, value),
            0x4000..=0x7FFF => self.romx.write(address, value),
            0x8000..=0x9FFF => self.vram.write(address, value),
            0xA000..=0xBFFF => self.sram.write(address, value),
            0xC000..=0xCFFF => self.wram0.write(address, value),
            0xD000..=0xDFFF => self.wramx.write(address, value),
            0xE000..=0xFDFF => self.echo.write(address, value),
            0xFE00..=0xFE9F => self.aom.write(address, value),
            0xFEA0..=0xFEFF => self.unused.write(address, value),
            0xFF00..=0xFF7F => self.io_registers.write(address, value),
            0xFF80..=0xFFFE => self.hram.write(address, value),
            0xFFFF => self.ie_register.write(address, value),
        }
    }
}

#[allow(dead_code)]
impl MemoryMap {
    pub fn new() -> Self {
        MemoryMap {
            rom0: Rom0::new(),
            romx: RomX::new(),
            vram: VRam::new(),
            sram: SRam::new(),
            wram0: WRam0::new(),
            wramx: WRamX::new(),
            echo: Echo::new(),
            aom: Aom::new(),
            unused: UnusedMemory::new(),
            io_registers: IORegisters::new(),
            hram: HRam::new(),
            ie_register: IERegister::new(),
        }
    }
}
