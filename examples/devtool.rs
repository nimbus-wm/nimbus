//! This tool is used to exercise nimbus and system APIs during development.

use nimbus::space;

use icrate::{AppKit::NSScreen, Foundation::MainThreadMarker};

fn main() {
    println!("Current space: {:?}", space::cur_space());
    println!("Visible spaces: {:?}", space::visible_spaces());
    println!("All spaces: {:?}", space::all_spaces());
    let screens = NSScreen::screens(MainThreadMarker::new().unwrap());
    let screens: Vec<_> = screens.iter().map(|screen| screen.visibleFrame()).collect();
    println!("Screen sizes: {screens:?}");
}
