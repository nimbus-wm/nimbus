mod actor;
mod metrics;
mod model;
mod sys;

use std::path::PathBuf;

use actor::layout::LayoutManager;
use actor::notification_center::NotificationCenter;
use actor::reactor::Reactor;
use actor::wm_controller::{self, WmController};
use clap::Parser;

use sys::executor::Executor;
use tokio::join;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};
use tracing_tree::time::UtcDateTime;

#[derive(Parser)]
struct Cli {
    /// Only run the window manager on the current space.
    #[arg(long)]
    one: bool,

    /// Disable new spaces by default.
    #[arg(long)]
    default_disable: bool,

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
        default_disable: opt.default_disable,
        restore_file: restore_file(),
    };
    let (wm_controller, wm_controller_sender) = WmController::new(config, events_tx);
    let notification_center = NotificationCenter::new(wm_controller_sender);

    Executor::run(async move {
        join!(
            wm_controller.run(),
            notification_center.watch_for_notifications()
        );
    });
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
