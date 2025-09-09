use crabboy::GameBoy;

mod window;

fn main() {
    let mut gameboy = GameBoy::default();
    gameboy.display.lock().unwrap().test_pattern();
    {
        let fbm = gameboy.display.clone();
        std::thread::spawn(move || gameboy.cpu.boot(&mut gameboy.memory, fbm));
    }
    // This blocks until the window closes
    window::draw_window(gameboy.display.clone());
}
