mod actor;
mod metrics;
mod model;
mod sys;

use actor::layout::LayoutCommand;
use actor::reactor::{Command, Event, Reactor, Sender};
use clap::Parser;
use metrics::MetricsCommand;
use model::Direction;
use sys::hotkey::{HotkeyManager, KeyCode, Modifiers};

use tracing::Span;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};
use tracing_tree::time::UtcDateTime;

use crate::model::Orientation;

#[derive(Parser)]
struct Cli {
    /// Only run the window manager on the current space.
    #[arg(long)]
    one: bool,
}

fn main() {
    let opt: Cli = Parser::parse();

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

    // TODO: This should probably go in another actor.
    let events_tx = Reactor::spawn();
    let app_spawn_cb = {
        let events_tx = events_tx.clone();
        move |pid, app_info| actor::app::spawn_app_thread(pid, app_info, events_tx.clone())
    };
    let event_cb = {
        let send_event = {
            let events_tx = events_tx.clone();
            move |event| {
                _ = events_tx.send((Span::current().clone(), event));
            }
        };
        let events_tx = events_tx.clone();
        let mut starting_space = None;
        let mut _manager = None;
        if !opt.one {
            _manager = Some(register_hotkeys(events_tx.clone()));
        }
        move |mut event| {
            match &mut event {
                Event::SpaceChanged(spaces) | Event::ScreenParametersChanged(_, spaces)
                    if opt.one =>
                {
                    if let Some(&Some(space)) = spaces.first() {
                        if starting_space.is_none() {
                            starting_space = Some(space);
                        }
                        if Some(space) == starting_space {
                            _manager = Some(register_hotkeys(events_tx.clone()));
                        } else {
                            _manager = None;
                        }
                    }
                    for space in spaces {
                        if *space != starting_space {
                            *space = None;
                        }
                    }
                }
                _ => (),
            }
            send_event(event);
        }
    };
    let handler = actor::notification_center::NotificationHandler::new(
        Box::new(app_spawn_cb),
        Box::new(event_cb),
    );
    actor::app::spawn_initial_app_threads(events_tx);
    handler.watch_for_notifications()
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
