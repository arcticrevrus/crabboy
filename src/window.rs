use gtk::{Application, ApplicationWindow, DrawingArea, cairo, glib, prelude::*};
use libcrabboy::graphics::{Color, Display};
use std::sync::{Arc, Mutex};

const SCREEN_WIDTH: i32 = 160;
const SCREEN_HEIGHT: i32 = 144;

fn mask_surface_from_display(screen: &Display) -> cairo::ImageSurface {
    let (w, h) = (SCREEN_WIDTH, SCREEN_HEIGHT);
    let mut surf = cairo::ImageSurface::create(cairo::Format::A8, w, h).unwrap();
    {
        let mut data = surf.data().unwrap();
        for (i, color) in screen.lines.iter().flat_map(|line| line.pixels).enumerate() {
            data[i] = match color {
                Color::C0 => 0,
                Color::C1 => 85,
                Color::C2 => 170,
                Color::C3 => 255,
            };
        }
    }
    surf
}

pub fn draw_window(display: Arc<Mutex<Display>>) -> glib::ExitCode {
    let app = Application::builder()
        .application_id("org.arcticrevrus.CrabBoy")
        .build();

    app.connect_activate(move |app| {
        let area = DrawingArea::new();
        area.set_content_width(SCREEN_WIDTH);
        area.set_content_height(SCREEN_HEIGHT);

        // Redraw ~60 FPS (tune as you like)
        {
            let area = area.clone();
            glib::timeout_add_local(std::time::Duration::from_millis(16), move || {
                area.queue_draw();
                glib::ControlFlow::Continue
            });
        }

        let display_for_draw = display.clone();
        area.set_draw_func(move |_area, cr, w, h| {
            cr.set_source_rgb(0.85, 0.87, 0.70);

            // Snapshot the framebuffer (keep the lock short)
            let mask_surface = {
                let fb = display_for_draw.lock().unwrap();
                mask_surface_from_display(&fb)
            };

            let mask_pat = cairo::SurfacePattern::create(&mask_surface);
            mask_pat.set_filter(cairo::Filter::Nearest);

            let base_w = SCREEN_WIDTH as f64;
            let base_h = SCREEN_HEIGHT as f64;
            let zx = w as f64 / base_w;
            let zy = h as f64 / base_h;
            let scale = zx.min(zy).max(1.0);
            let dx = (w as f64 - base_w * scale) / 2.0;
            let dy = (h as f64 - base_h * scale) / 2.0;

            cr.save().unwrap();
            cr.translate(dx, dy);
            cr.scale(scale, scale);
            cr.mask(&mask_pat).unwrap();
            cr.restore().unwrap();
        });

        let window = ApplicationWindow::builder()
            .application(app)
            .default_width(SCREEN_WIDTH)
            .default_height(SCREEN_HEIGHT)
            .title("CrabBoy")
            .child(&area)
            .build();
        window.present();
    });

    app.run()
}
