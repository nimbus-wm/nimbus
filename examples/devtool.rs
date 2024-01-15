//! This tool is used to exercise nimbus and system APIs during development.

use core_graphics::display::CGMainDisplayID;
use nimbus::space;

use icrate::{AppKit::NSScreen, Foundation::MainThreadMarker};

fn main() {
    println!("Current space: {:?}", space::cur_space());
    println!("Visible spaces: {:?}", space::visible_spaces());
    println!("All spaces: {:?}", space::all_spaces());
    println!("{:?}", space::managed_display_spaces());
    println!("CG screens: {:?}, main={}", space::screens(), unsafe {
        CGMainDisplayID()
    });
    println!("{:?}", space::managed_displays());
    let screens = NSScreen::screens(MainThreadMarker::new().unwrap());
    let frames: Vec<_> = screens.iter().map(|screen| screen.visibleFrame()).collect();
    println!("Screen sizes: {frames:?}");
    let descrs: Vec<_> = screens.iter().map(|screen| screen.deviceDescription()).collect();
    println!("Screen descrs: {descrs:?}");
}
