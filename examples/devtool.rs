//! This tool is used to exercise nimbus and system APIs during development.

use std::{future::Future, path::PathBuf, time::Instant};

use accessibility::{AXUIElement, AXUIElementAttributes};
use accessibility_sys::pid_t;
use anyhow::Context;
use clap::{Parser, Subcommand};
use core_foundation::{array::CFArray, base::TCFType, dictionary::CFDictionaryRef};
use core_graphics::{
    display::{CGDisplayBounds, CGMainDisplayID},
    window::{
        kCGNullWindowID, kCGWindowListExcludeDesktopElements, kCGWindowListOptionOnScreenOnly,
        CGWindowID, CGWindowListCopyWindowInfo,
    },
};
use icrate::{
    AppKit::{NSScreen, NSWindow, NSWindowNumberListAllApplications},
    Foundation::MainThreadMarker,
};
use nimbus_wm::{
    actor::reactor,
    sys::{
        self,
        app::WindowInfo,
        screen::{self, ScreenCache},
        window_server::{self, WindowServerId},
    },
};
use tokio::sync::mpsc;
use tracing::info;
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
    List(List),
    #[command(subcommand)]
    App(App),
    #[command(subcommand)]
    WindowServer(WindowServer),
    #[command()]
    Replay(Replay),
    #[command()]
    Mouse,
}

#[derive(Subcommand, Clone)]
enum List {
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

#[derive(Subcommand, Clone)]
enum WindowServer {
    #[command()]
    List {
        #[arg(short, long)]
        all: bool,
        /// Whether to show the raw window dictionaries. Implies --all.
        #[arg(short, long)]
        raw: bool,
    },
    #[command()]
    Get { id: u32 },
}

#[derive(Parser, Clone)]
struct Replay {
    path: PathBuf,
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::registry()
        .with(EnvFilter::from_default_env())
        .with(
            tracing_tree::HierarchicalLayer::default()
                .with_indent_amount(2)
                .with_indent_lines(true)
                .with_deferred_spans(true)
                .with_span_retrace(true)
                .with_targets(true),
        )
        .init();
    let opt: Opt = Parser::parse();

    match opt.command {
        Command::List(List::Ax) => {
            time("accessibility", || get_windows_with_ax(&opt, false, true)).await
        }
        Command::List(List::Cg) => time("core-graphics", || get_windows_with_cg(&opt, true)).await,
        Command::List(List::Ns) => time("ns-window", || get_windows_with_ns(&opt, true)).await,
        Command::List(List::Apps) => get_apps(&opt),
        Command::List(List::All) => {
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
        Command::List(List::Spaces) => {
            println!("Current space: {:?}", screen::diagnostic::cur_space());
            println!("Visible spaces: {:?}", screen::diagnostic::visible_spaces());
            println!("All spaces: {:?}", screen::diagnostic::all_spaces());
            println!(
                "Managed display spaces: {:?}",
                screen::diagnostic::managed_display_spaces()
            );

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
        Command::WindowServer(WindowServer::List { all, raw }) => {
            if raw {
                for window in window_server::get_visible_windows_raw().iter() {
                    println!("{window:?}");
                }
            } else {
                let layer = if all { None } else { Some(0) };
                for window in window_server::get_visible_windows_with_layer(layer) {
                    println!("{window:?}");
                }
            }
        }
        Command::WindowServer(WindowServer::Get { id }) => match window_server::get_window(id) {
            Some(win) => println!("{win:?}"),
            None => println!("Could not find window {id}"),
        },
        Command::Replay(Replay { path }) => {
            reactor::replay(&path, |rx| {
                while let Ok((_span, request)) = rx.try_recv() {
                    info!(?request);
                }
            })?;
        }
        Command::Mouse => {
            use core_foundation::runloop::{kCFRunLoopCommonModes, CFRunLoop};
            use core_graphics::event::{
                CGEventTap, CGEventTapLocation, CGEventTapOptions, CGEventTapPlacement, CGEventType,
            };
            let current = CFRunLoop::get_current();
            match CGEventTap::new(
                CGEventTapLocation::HID,
                CGEventTapPlacement::HeadInsertEventTap,
                CGEventTapOptions::Default,
                vec![CGEventType::LeftMouseUp],
                |_a, _b, d| {
                    println!("{:?}", d.location());
                    None
                },
            ) {
                Ok(tap) => unsafe {
                    let loop_source = tap.mach_port().create_runloop_source(0).unwrap();
                    current.add_source(&loop_source, kCFRunLoopCommonModes);
                    tap.enable();
                    CFRunLoop::run_current();
                },
                Err(_) => assert!(false),
            }
        }
    }
    Ok(())
}

async fn get_windows_with_cg(opt: &Opt, print: bool) {
    let windows: CFArray<CFDictionaryRef> = unsafe {
        CFArray::wrap_under_get_rule(CGWindowListCopyWindowInfo(
            kCGWindowListOptionOnScreenOnly | kCGWindowListExcludeDesktopElements,
            kCGNullWindowID,
        ))
    };
    if print && opt.verbose {
        println!("{windows:?}");
    }
    if print {
        println!("visible window ids:");
        for window in window_server::get_visible_windows_with_layer(None) {
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
    for (pid, bundle_id) in sys::app::running_apps(opt.bundle.clone()) {
        let sender = sender.clone();
        let verbose = opt.verbose;
        let task = move || {
            let app = AXUIElement::application(pid);
            let windows = get_windows_for_app(app, verbose);
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
                    for (win, dbg) in windows {
                        println!("{win:?} from {}", info.bundle_id.as_deref().unwrap_or("?"));
                        if opt.verbose {
                            println!("=> {dbg}");
                        }
                    }
                }
            }
            Err(_) => (), //println!("  * Error reading windows: {err:?}"),
        }
    }
}

fn get_windows_for_app(
    app: AXUIElement,
    verbose: bool,
) -> Result<Vec<(WindowInfo, String)>, accessibility::Error> {
    let Ok(windows) = &app.windows() else {
        return Err(accessibility::Error::NotFound);
    };
    windows
        .iter()
        .map(|win| {
            Ok((
                WindowInfo::try_from(&*win)?,
                verbose.then(|| format!("{:#?}", &*win)).unwrap_or_default(),
            ))
        })
        .collect()
}

fn get_apps(opt: &Opt) {
    for (pid, _bundle_id) in sys::app::running_apps(opt.bundle.clone()) {
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
