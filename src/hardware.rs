use std::sync::{Arc, Mutex};

pub enum Button {
    Up,
    Down,
    Left,
    Right,
    Start,
    Select,
    A,
    B,
}

#[derive(Debug)]
pub enum ButtonState {
    Up,
    Down,
}

pub struct Hardware {
    pub joypad: Arc<Mutex<Joypad>>,
}
impl Hardware {
    pub fn new() -> Self {
        Self {
            joypad: Arc::new(Mutex::new(Joypad::new())),
        }
    }
}

impl Default for Hardware {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Joypad {
    up: bool,
    down: bool,
    left: bool,
    right: bool,
    start: bool,
    select: bool,
    a: bool,
    b: bool,
}

impl Joypad {
    pub fn new() -> Self {
        Self {
            up: false,
            down: false,
            left: false,
            right: false,
            start: false,
            select: false,
            a: false,
            b: false,
        }
    }
    pub fn set(&mut self, button: Button, state: ButtonState) {
        let button = &mut match button {
            Button::Up => self.up,
            Button::Down => self.down,
            Button::Left => self.left,
            Button::Right => self.right,
            Button::Start => self.start,
            Button::Select => self.select,
            Button::A => self.a,
            Button::B => self.b,
        };
        *button = match state {
            ButtonState::Up => false,
            ButtonState::Down => true,
        };
    }
    pub fn get(&mut self, button: Button) -> ButtonState {
        let button_bool = match button {
            Button::Up => self.up,
            Button::Down => self.down,
            Button::Left => self.left,
            Button::Right => self.right,
            Button::Start => self.start,
            Button::Select => self.select,
            Button::A => self.a,
            Button::B => self.b,
        };
        match button_bool {
            true => ButtonState::Up,
            false => ButtonState::Down,
        }
    }
}

impl Default for Joypad {
    fn default() -> Self {
        Self::new()
    }
}
