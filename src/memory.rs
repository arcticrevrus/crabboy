pub trait Memory {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);
}

#[derive(Copy, Clone, PartialEq)]
struct Rom0 {
    memory: [u8; 0x4000],
}
#[allow(dead_code)]
impl Rom0 {
    pub fn new() -> Self {
        Self {
            memory: [0; 0x4000],
        }
    }
}
impl Memory for Rom0 {
    fn read(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[address as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct RomX {
    memory: [u8; 0x4000],
}
#[allow(dead_code)]
impl RomX {
    pub fn new() -> Self {
        Self {
            memory: [0; 0x4000],
        }
    }
}
impl Memory for RomX {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0x4000) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0x4000) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct VRam {
    memory: [u8; 0x2000],
}
#[allow(dead_code)]
impl VRam {
    pub fn new() -> Self {
        Self {
            memory: [0; 0x2000],
        }
    }
}
impl Memory for VRam {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0x8000) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0x8000) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct SRam {
    memory: [u8; 0x2000],
}
#[allow(dead_code)]
impl SRam {
    pub fn new() -> Self {
        Self {
            memory: [0; 0x2000],
        }
    }
}
impl Memory for SRam {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0xA000) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0xA000) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct WRam0 {
    memory: [u8; 0x1000],
}
#[allow(dead_code)]
impl WRam0 {
    pub fn new() -> Self {
        Self {
            memory: [0; 0x1000],
        }
    }
}
impl Memory for WRam0 {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0xC000) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0xC000) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct WRamX {
    memory: [u8; 0x1000],
}
#[allow(dead_code)]
impl WRamX {
    pub fn new() -> Self {
        Self {
            memory: [0; 0x1000],
        }
    }
}
impl Memory for WRamX {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0xD000) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0xD000) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct Echo {
    memory: [u8; 0x1D00],
}
#[allow(dead_code)]
impl Echo {
    pub fn new() -> Self {
        Self {
            memory: [0; 0x1D00],
        }
    }
}
impl Memory for Echo {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0xE000) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0xE000) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct Aom {
    memory: [u8; 0xA0],
}
#[allow(dead_code)]
impl Aom {
    pub fn new() -> Self {
        Self { memory: [0; 0xA0] }
    }
}
impl Memory for Aom {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0xFE00) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0xFE00) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]

struct UnusedMemory {
    memory: [u8; 0x50],
}
#[allow(dead_code)]
impl UnusedMemory {
    pub fn new() -> Self {
        Self { memory: [0; 0x50] }
    }
}
impl Memory for UnusedMemory {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0xFEA0) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0xFEA0) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct IORegisters {
    memory: [u8; 0x80],
}
#[allow(dead_code)]
impl IORegisters {
    pub fn new() -> Self {
        Self { memory: [0; 0x80] }
    }
}
impl Memory for IORegisters {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0xFF00) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0xFF00) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct HRam {
    memory: [u8; 0x7F],
}
#[allow(dead_code)]
impl HRam {
    pub fn new() -> Self {
        Self { memory: [0; 0x7F] }
    }
}
impl Memory for HRam {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0xFF80) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0xFF80) as usize] = value;
    }
}

#[derive(Copy, Clone, PartialEq)]
struct IERegister {
    memory: [u8; 0x1],
}
#[allow(dead_code)]
impl IERegister {
    pub fn new() -> Self {
        Self { memory: [0; 0x1] }
    }
}
impl Memory for IERegister {
    fn read(&self, address: u16) -> u8 {
        self.memory[(address - 0xFFFF) as usize]
    }
    fn write(&mut self, address: u16, value: u8) {
        self.memory[(address - 0xFFFF) as usize] = value;
    }
}

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
