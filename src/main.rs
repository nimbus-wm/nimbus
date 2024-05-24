mod actor;
mod metrics;
mod model;
mod sys;

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use actor::layout::LayoutManager;
use actor::reactor::Reactor;
use actor::wm_controller::{self, WmController, WmEvent};
use clap::Parser;

use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};
use tracing_tree::time::UtcDateTime;

#[derive(Parser)]
struct Cli {
    /// Only run the window manager on the current space.
    #[arg(long)]
    one: bool,

    #[arg(long)]
    validate: bool,

    #[arg(long)]
    restore: bool,
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

    if opt.validate {
        LayoutManager::load(restore_file()).unwrap();
        return;
    }

    let layout = if opt.restore {
        LayoutManager::load(restore_file()).unwrap()
    } else {
        LayoutManager::new()
    };
    let events_tx = Reactor::spawn(layout);

    let config = wm_controller::Config {
        one_space: opt.one,
        restore_file: restore_file(),
    };
    let wm_controller = WmController::new(config, events_tx);
    let wm_controller = Rc::new(RefCell::new(wm_controller));

    let app_spawn_cb = {
        let wm_controller = wm_controller.clone();
        move |pid, info| wm_controller.borrow_mut().handle_event(WmEvent::AppLaunch(pid, info))
    };
    let event_cb = {
        let wm_controller = wm_controller.clone();
        move |event| wm_controller.borrow_mut().handle_event(WmEvent::ReactorEvent(event))
    };
    let handler = actor::notification_center::NotificationHandler::new(
        Box::new(app_spawn_cb),
        Box::new(event_cb),
    );
    wm_controller.borrow_mut().handle_event(WmEvent::AppEventsRegistered);
    handler.watch_for_notifications()
}

fn config_dir() -> PathBuf {
    dirs::home_dir().unwrap().join(".nimbus")
}

fn restore_file() -> PathBuf {
    config_dir().join("layout.ron")
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
