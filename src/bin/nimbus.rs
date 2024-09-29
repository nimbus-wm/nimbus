use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use nimbus_wm::{
    actor::{
        layout::LayoutManager,
        mouse::{self, Mouse},
        notification_center::NotificationCenter,
        reactor::{self, Reactor},
        wm_controller::{self, WmController},
    },
    config::{config_file, restore_file, Config},
    log,
    sys::executor::Executor,
};
use tokio::join;

#[derive(Parser)]
struct Cli {
    /// Only run the window manager on the current space.
    #[arg(long)]
    one: bool,

    /// Disable new spaces by default.
    ///
    /// Ignored if --one is used.
    #[arg(long)]
    default_disable: bool,

    /// Disable animations.
    #[arg(long)]
    no_animate: bool,

    /// Check whether the restore file can be loaded without actually starting
    /// the window manager.
    #[arg(long)]
    validate: bool,

    /// Restore the configuration saved with the save_and_exit command. This is
    /// only useful within the same session.
    #[arg(long)]
    restore: bool,

    /// Record reactor events to the specified file path. Overwrites the file if
    /// exists.
    #[arg(long)]
    record: Option<PathBuf>,
}

fn main() {
    let opt: Cli = Parser::parse();

    if std::env::var_os("RUST_BACKTRACE").is_none() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }
    log::init_logging();
    install_panic_hook();

    let mut config = if config_file().exists() {
        Config::read(&config_file()).unwrap()
    } else {
        Config::default()
    };
    config.settings.animate &= !opt.no_animate;
    config.settings.default_disable |= opt.default_disable;
    let config = Arc::new(config);

    if opt.validate {
        LayoutManager::load(restore_file()).unwrap();
        return;
    }

    let layout = if opt.restore {
        LayoutManager::load(restore_file()).unwrap()
    } else {
        LayoutManager::new()
    };
    let (mouse_tx, mouse_rx) = mouse::channel();
    let events_tx = Reactor::spawn(
        config.clone(),
        layout,
        reactor::Record::new(opt.record.as_deref()),
        mouse_tx.clone(),
    );

    let wm_config = wm_controller::Config {
        one_space: opt.one,
        restore_file: restore_file(),
        config: config.clone(),
    };
    let (wm_controller, wm_controller_sender) =
        WmController::new(wm_config, events_tx.clone(), mouse_tx.clone());
    let notification_center = NotificationCenter::new(wm_controller_sender);
    let mouse = Mouse::new(config.clone(), events_tx, mouse_rx);

    Executor::run(async move {
        join!(
            wm_controller.run(),
            notification_center.watch_for_notifications(),
            mouse.run(),
        );
    });
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
