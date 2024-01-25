mod animation;
mod app;
mod hotkey;
mod notification_center;
mod reactor;
mod run_loop;
mod screen;
mod util;

use hotkey::{HotkeyManager, KeyCode, Modifiers};
use reactor::{Command, Event, Sender};

fn main() {
    env_logger::Builder::from_default_env().format_timestamp_millis().init();
    install_panic_hook();
    let events_tx = reactor::Reactor::spawn();
    app::spawn_initial_app_threads(events_tx.clone());
    let _mgr = register_hotkeys(events_tx.clone());
    notification_center::watch_for_notifications(events_tx)
}

fn register_hotkeys(events_tx: Sender<Event>) -> HotkeyManager {
    let mgr = HotkeyManager::new(events_tx);
    mgr.register(Modifiers::ALT, KeyCode::KeyW, Command::Hello);
    mgr.register(Modifiers::ALT, KeyCode::KeyS, Command::Shuffle);
    mgr.register(Modifiers::ALT, KeyCode::KeyJ, Command::NextWindow);
    mgr.register(Modifiers::ALT, KeyCode::KeyK, Command::PrevWindow);
    mgr
}

#[cfg(panic = "unwind")]
fn install_panic_hook() {
    // Abort on panic instead of propagating panics to the main thread.
    // See Cargo.toml for why we don't use panic=abort everywhere.
    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        original_hook(info);
        std::process::abort();
    }));

    // Since this version only runs in development, let's default
    // RUST_BACKTRACE=1 too.
    if std::env::var("RUST_BACKTRACE").is_err() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }
}

#[cfg(not(panic = "unwind"))]
fn install_panic_hook() {}
