mod app;
mod hotkey;
mod notification_center;
mod reactor;
mod run_loop;
mod space;

use hotkey::{HotkeyManager, KeyCode, Modifiers};
use reactor::{Command, Event, Sender};

fn main() {
    env_logger::init();
    let events = reactor::Reactor::spawn();
    app::spawn_initial_app_threads(events.clone());
    let _mgr = register_hotkeys(events.clone());
    notification_center::watch_for_notifications(events)
}

fn register_hotkeys(events: Sender<Event>) -> HotkeyManager {
    let mgr = HotkeyManager::new(events);
    mgr.register(Modifiers::ALT, KeyCode::KeyW, Command::Hello);
    mgr.register(Modifiers::ALT, KeyCode::KeyS, Command::Shuffle);
    mgr
}
