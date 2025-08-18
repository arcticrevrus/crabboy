#[derive(Copy, Clone)]
enum Color {
    C0,
    C1,
    C2,
    C3,
}

#[derive(Clone, Copy)]
struct TileLine {
    line: [Color; 8],
}
impl TileLine {
    fn bits_bool(byte: u8) -> impl Iterator<Item = bool> {
        (0..8).rev().map(move |i| ((byte >> i) & 1) != 0)
    }
    pub fn new(byte_one: u8, byte_two: u8) -> Self {
        let mut line: [Color; 8] = [Color::C0; 8];
        for (i, (bit_a, bit_b)) in Self::bits_bool(byte_one)
            .zip(Self::bits_bool(byte_two))
            .enumerate()
        {
            line[i] = match (bit_b, bit_a) {
                (false, false) => Color::C0,
                (false, true) => Color::C1,
                (true, false) => Color::C2,
                (true, true) => Color::C3,
            };
        }
        Self { line }
    }
    pub fn as_bytes(&self) -> u16 {
        let mut hi: u8 = 0;
        let mut lo: u8 = 0;
        for (i, color) in self.line.iter().enumerate() {
            let (bit1, bit0) = match color {
                Color::C0 => (0, 0),
                Color::C1 => (0, 1),
                Color::C2 => (1, 0),
                Color::C3 => (1, 1),
            };
            let shift = 7 - i;
            lo |= bit0 << shift;
            hi |= bit1 << shift;
        }
        ((hi as u16) << 8) | lo as u16
    }
}

struct Tile {
    lines: [TileLine; 8],
}

struct ScanLine {
    pixels: [Color; 160],
}

struct Display {
    lines: [ScanLine; 144],
}
impl Display {
    #[inline(always)]
    pub fn set(&mut self, color: Color, x: u8, y: u8) {
        debug_assert!(x <= 160 && y <= 144);
        const SCREEN_WIDTH: usize = 144;
    }
}
