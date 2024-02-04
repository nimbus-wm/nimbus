mod animation;
mod app;
mod hotkey;
mod layout;
mod metrics;
mod notification_center;
mod reactor;
mod run_loop;
mod screen;
mod util;

use hotkey::{HotkeyManager, KeyCode, Modifiers};
use metrics::MetricsCommand;
use reactor::{Command, Event, Sender};
use tracing::Span;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};
use tracing_tree::time::UtcDateTime;

fn main() {
    tracing_subscriber::registry()
        .with(EnvFilter::from_default_env())
        .with(metrics::timing_layer())
        .with(
            tracing_tree::HierarchicalLayer::default()
                .with_indent_amount(2)
                .with_indent_lines(true)
                .with_deferred_spans(true)
                .with_span_retrace(true)
                .with_targets(true)
                .with_timer(UtcDateTime::default()),
        )
        .init();
    install_panic_hook();
    let events_tx = reactor::Reactor::spawn();
    app::spawn_initial_app_threads(events_tx.clone());
    let _mgr = register_hotkeys(events_tx.clone());
    notification_center::watch_for_notifications(events_tx)
}

fn register_hotkeys(events_tx: Sender<(Span, Event)>) -> HotkeyManager {
    let mgr = HotkeyManager::new(events_tx);
    mgr.register(Modifiers::ALT, KeyCode::KeyW, Command::Hello);
    mgr.register(Modifiers::ALT, KeyCode::KeyS, Command::Shuffle);
    mgr.register(Modifiers::ALT, KeyCode::KeyJ, Command::NextWindow);
    mgr.register(Modifiers::ALT, KeyCode::KeyK, Command::PrevWindow);
    mgr.register(
        Modifiers::ALT,
        KeyCode::KeyM,
        Command::Metrics(MetricsCommand::ShowTiming),
    );
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
