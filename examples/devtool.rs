//! This tool is used to exercise nimbus and system APIs during development.

use std::{future::Future, time::Instant};

use accessibility::{AXUIElement, AXUIElementAttributes};
use accessibility_sys::pid_t;
use anyhow::Context;
use clap::{Parser, Subcommand};
use core_foundation::{array::CFArray, base::TCFType, dictionary::CFDictionaryRef};
use core_graphics::{
    display::{CGDisplayBounds, CGMainDisplayID},
    window::{
        kCGNullWindowID, kCGWindowListOptionExcludeDesktopElements,
        kCGWindowListOptionOnScreenOnly, CGWindowID, CGWindowListCopyWindowInfo,
    },
};
use icrate::{
    AppKit::{NSScreen, NSWindow, NSWindowNumberListAllApplications},
    Foundation::MainThreadMarker,
};
use nimbus_wm::{
    sys::app,
    sys::screen::{self, ScreenCache},
    sys::window_server::{self, WindowServerId},
};
use tokio::sync::mpsc;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

#[derive(Parser)]
struct Opt {
    #[arg(long)]
    bundle: Option<String>,
    #[command(subcommand)]
    command: Command,
    #[arg(long)]
    verbose: bool,
}

#[derive(Subcommand, Clone)]
enum Command {
    #[command(subcommand)]
    Show(Show),
    #[command(subcommand)]
    App(App),
}

#[derive(Subcommand, Clone)]
enum Show {
    All,
    Apps,
    Ax,
    Cg,
    Ns,
    Spaces,
}

#[derive(Subcommand, Clone)]
enum App {
    #[command()]
    SetMainWindow {
        pid: pid_t,
        window_server_id: CGWindowID,
        #[arg(long)]
        wait: bool,
    },
    #[command()]
    ReadMainWindow {
        pid: pid_t,
        #[arg(long)]
        wait: bool,
    },
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::registry()
        .with(EnvFilter::from_default_env())
        .with(tracing_tree::HierarchicalLayer::default())
        .init();
    let opt: Opt = Parser::parse();

    match opt.command {
        Command::Show(Show::Ax) => {
            time("accessibility", || get_windows_with_ax(&opt, false, true)).await
        }
        Command::Show(Show::Cg) => time("core-graphics", || get_windows_with_cg(&opt, true)).await,
        Command::Show(Show::Ns) => time("ns-window", || get_windows_with_ns(&opt, true)).await,
        Command::Show(Show::Apps) => get_apps(&opt),
        Command::Show(Show::All) => {
            //time("accessibility serial", || get_windows_with_ax(&opt, true)).await;
            time("core-graphics", || get_windows_with_cg(&opt, true)).await;
            time("ns-window", || get_windows_with_ns(&opt, true)).await;
            time("accessibility", || get_windows_with_ax(&opt, false, true)).await;
            time("core-graphics second time", || {
                get_windows_with_cg(&opt, false)
            })
            .await;
            time("ns-window second time", || get_windows_with_ns(&opt, false)).await;
            time("accessibility second time", || {
                get_windows_with_ax(&opt, false, false)
            })
            .await;
        }
        Command::Show(Show::Spaces) => {
            println!("Current space: {:?}", screen::diagnostic::cur_space());
            println!("Visible spaces: {:?}", screen::diagnostic::visible_spaces());
            println!("All spaces: {:?}", screen::diagnostic::all_spaces());
            println!("{:?}", screen::diagnostic::managed_display_spaces());

            dbg!(screen::diagnostic::managed_displays());
            let screens = NSScreen::screens(MainThreadMarker::new().unwrap());
            let frames: Vec<_> = screens.iter().map(|screen| screen.visibleFrame()).collect();
            println!("NSScreen sizes: {frames:?}");

            println!();
            let mut sc = ScreenCache::new(MainThreadMarker::new().unwrap());
            println!("Frames: {:?}", sc.update_screen_config());
            println!("Spaces: {:?}", sc.get_screen_spaces());
        }
        Command::App(App::SetMainWindow { pid, window_server_id, wait }) => {
            let app = AXUIElement::application(pid);
            let windows = app.windows()?;
            let window = windows
                .iter()
                .filter(|w| {
                    let id: Result<window_server::WindowServerId, _> = (&**w).try_into();
                    id.is_ok_and(|id| id.as_u32() == window_server_id)
                })
                .next()
                .context("Could not find matching window")?;
            if wait {
                println!("Press enter to complete action");
                std::io::stdin().read_line(&mut String::new())?;
                window.set_messaging_timeout(3600.0)?;
            }
            window.set_main(true).context("Failed to set window as main")?;
        }
        Command::App(App::ReadMainWindow { pid, wait }) => {
            let app = AXUIElement::application(pid);
            println!("frontmost = {:?}", app.frontmost()?);
            let main_window = if opt.verbose {
                let main_window = dbg!(app.main_window()?);
                let main_window_id: WindowServerId = (&app.main_window()?).try_into()?;
                dbg!(main_window_id);
                main_window
            } else {
                let main_window = app.main_window();
                println!("main_window = {:?}", main_window.as_ref().map(|_| ()));
                // let main_window_id: WindowServerId = (&app.main_window()?).try_into()?;
                // dbg!(main_window_id);
                main_window?
            };
            if wait {
                println!("Press enter to complete action");
                std::io::stdin().read_line(&mut String::new())?;
                app.set_messaging_timeout(3600.0)?;
            }
            dbg!(main_window.main()?);
        }
    }
    Ok(())
}

async fn get_windows_with_cg(opt: &Opt, print: bool) {
    let windows: CFArray<CFDictionaryRef> = unsafe {
        CFArray::wrap_under_get_rule(CGWindowListCopyWindowInfo(
            kCGWindowListOptionOnScreenOnly | kCGWindowListOptionExcludeDesktopElements,
            kCGNullWindowID,
        ))
    };
    if print && opt.verbose {
        println!("{windows:?}");
    }
    if print {
        println!("visible window ids:");
        for window in window_server::get_visible_windows() {
            if opt.verbose {
                println!("- {window:?}");
            } else {
                println!("- {id:?}, pid={pid:?}", id = window.id, pid = window.pid);
            }
        }
    }
    let display_id = unsafe { CGMainDisplayID() };
    let screen = unsafe { CGDisplayBounds(display_id) };
    if print {
        println!("main display = {screen:?}");
    }
}

async fn get_windows_with_ns(_opt: &Opt, print: bool) {
    let mtm = MainThreadMarker::new().unwrap();
    let windows =
        unsafe { NSWindow::windowNumbersWithOptions(NSWindowNumberListAllApplications, mtm) };
    if print {
        println!("{windows:?}");
    }
}

async fn get_windows_with_ax(opt: &Opt, serial: bool, print: bool) {
    let (sender, mut receiver) = mpsc::unbounded_channel();
    for (pid, bundle_id) in app::running_apps(opt.bundle.clone()) {
        let sender = sender.clone();
        let task = move || {
            let app = AXUIElement::application(pid);
            let windows = get_windows_for_app(app);
            sender.send((bundle_id, windows)).unwrap()
        };
        if serial {
            task();
        } else {
            tokio::task::spawn_blocking(task);
        }
    }
    drop(sender);
    while let Some((info, windows)) = receiver.recv().await {
        //println!("{info:?}");
        match windows {
            Ok(windows) => {
                if print {
                    for win in windows {
                        println!("{win:?} from {}", info.bundle_id.as_deref().unwrap_or("?"));
                    }
                }
            }
            Err(_) => (), //println!("  * Error reading windows: {err:?}"),
        }
    }
}

fn get_windows_for_app(app: AXUIElement) -> Result<Vec<app::WindowInfo>, accessibility::Error> {
    let Ok(windows) = &app.windows() else {
        return Err(accessibility::Error::NotFound);
    };
    windows.into_iter().map(|win| app::WindowInfo::try_from(&*win)).collect()
}

fn get_apps(opt: &Opt) {
    for (pid, _bundle_id) in app::running_apps(opt.bundle.clone()) {
        let app = AXUIElement::application(pid);
        println!("{app:#?}");
    }
}

async fn time<O, F: Future<Output = O>>(desc: &str, f: impl FnOnce() -> F) -> O {
    let start = Instant::now();
    let out = f().await;
    let end = Instant::now();
    println!("{desc} took {:?}", end - start);
    out
}
