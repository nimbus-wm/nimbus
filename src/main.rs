mod animation;
mod app;
mod hotkey;
mod layout;
mod metrics;
mod model;
mod notification_center;
mod reactor;
mod run_loop;
mod screen;
mod util;

use hotkey::{HotkeyManager, KeyCode, Modifiers};
use layout::LayoutCommand;
use metrics::MetricsCommand;
use model::Direction;
use reactor::{Command, Event, Sender};

use tracing::Span;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};
use tracing_tree::time::UtcDateTime;

use crate::model::Orientation;

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
    let _mgr = register_hotkeys(events_tx.clone());
    notification_center::watch_for_notifications(events_tx)
}

fn register_hotkeys(events_tx: Sender<(Span, Event)>) -> HotkeyManager {
    const ALT: Modifiers = Modifiers::ALT;
    const SHIFT: Modifiers = Modifiers::SHIFT;
    use KeyCode::*;

    use Direction::*;
    use LayoutCommand::*;
    use MetricsCommand::*;

    let mgr = HotkeyManager::new(events_tx);
    mgr.register(ALT, KeyW, Command::Hello);
    //mgr.register(ALT, KeyS, Command::Layout(Shuffle));
    mgr.register(ALT, KeyA, Command::Layout(Ascend));
    mgr.register(ALT, KeyD, Command::Layout(Descend));
    mgr.register(ALT, KeyH, Command::Layout(MoveFocus(Left)));
    mgr.register(ALT, KeyJ, Command::Layout(MoveFocus(Down)));
    mgr.register(ALT, KeyK, Command::Layout(MoveFocus(Up)));
    mgr.register(ALT, KeyL, Command::Layout(MoveFocus(Right)));
    mgr.register(ALT | SHIFT, KeyH, Command::Layout(MoveNode(Left)));
    mgr.register(ALT | SHIFT, KeyJ, Command::Layout(MoveNode(Down)));
    mgr.register(ALT | SHIFT, KeyK, Command::Layout(MoveNode(Up)));
    mgr.register(ALT | SHIFT, KeyL, Command::Layout(MoveNode(Right)));
    mgr.register(ALT, Equal, Command::Layout(Split(Orientation::Vertical)));
    mgr.register(
        ALT,
        Backslash,
        Command::Layout(Split(Orientation::Horizontal)),
    );
    mgr.register(ALT, KeyS, Command::Layout(Group(Orientation::Vertical)));
    mgr.register(ALT, KeyT, Command::Layout(Group(Orientation::Horizontal)));
    mgr.register(ALT, KeyE, Command::Layout(Ungroup));
    mgr.register(ALT, KeyM, Command::Metrics(ShowTiming));
    mgr.register(ALT | SHIFT, KeyD, Command::Layout(Debug));
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
