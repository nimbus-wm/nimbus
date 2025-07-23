//! This tool is used to exercise glide and system APIs during development.

use std::future::Future;
use std::path::PathBuf;
use std::ptr;
use std::time::Instant;

use accessibility::{AXUIElement, AXUIElementAttributes};
use accessibility_sys::{AXUIElementCopyElementAtPosition, AXUIElementRef, pid_t};
use anyhow::Context;
use clap::{Parser, Subcommand};
use core_foundation::array::CFArray;
use core_foundation::base::{FromMutVoid, TCFType};
use core_foundation::dictionary::CFDictionaryRef;
use core_graphics::display::{CGDisplayBounds, CGMainDisplayID};
use core_graphics::window::{
    CGWindowID, CGWindowListCopyWindowInfo, kCGNullWindowID, kCGWindowListOptionOnScreenOnly,
};
use glide_wm::actor::reactor;
use glide_wm::sys::app::WindowInfo;
use glide_wm::sys::event::{self, get_mouse_pos};
use glide_wm::sys::executor::Executor;
use glide_wm::sys::screen::{self, ScreenCache};
use glide_wm::sys::space;
use glide_wm::sys::window_server::{self, WindowServerId, get_window};
use glide_wm::sys::{self};
use livesplit_hotkey::{ConsumePreference, Modifiers};
use objc2_app_kit::{NSScreen, NSWindow, NSWindowNumberListOptions};
use objc2_foundation::MainThreadMarker;
use tokio::sync::mpsc::{self, UnboundedReceiver, unbounded_channel};
use tracing::info;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

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
    #[command(subcommand)]
    Mouse(Mouse),
    #[command(subcommand)]
    Space(Space),
    #[command()]
    Inspect,
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

#[derive(Subcommand, Clone)]
enum Mouse {
    #[command()]
    Clicks,
    #[command()]
    Hide,
}

#[derive(Subcommand, Clone)]
enum Space {
    #[command()]
    List {
        #[arg(short, long)]
        verbose: bool,
    },
    #[command()]
    Current,
    #[command()]
    Switch { space_ref: String },
    #[command()]
    Create {
        #[arg(short, long)]
        name: Option<String>,
        #[arg(short, long)]
        space_type: Option<String>,
    },
    #[command()]
    Destroy { space_ref: String },
    #[command()]
    Rename { space_ref: String, name: String },
    #[command()]
    SetLevel { space_ref: String, level: i32 },
    #[command()]
    SetType {
        space_ref: String,
        space_type: String,
    },
    #[command()]
    Info { space_ref: String },
    #[command()]
    Owners { space_ref: String },
    #[command()]
    AddWindows {
        space_ref: String,
        #[arg(required = true)]
        window_ids: Vec<u32>,
    },
    #[command()]
    RemoveWindows {
        space_ref: String,
        #[arg(required = true)]
        window_ids: Vec<u32>,
    },
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::registry()
        .with(glide_wm::log::tree_layer())
        .with(EnvFilter::from_default_env())
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
            time("core-graphics second time", || get_windows_with_cg(&opt, false)).await;
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
        Command::WindowServer(WindowServer::Get { id }) => {
            match window_server::get_window(WindowServerId(id)) {
                Some(win) => println!("{win:?}"),
                None => println!("Could not find window {id}"),
            }
        }
        Command::Replay(Replay { path }) => {
            reactor::replay(&path, |_span, request| {
                info!(?request);
            })?;
        }
        Command::Mouse(Mouse::Clicks) => {
            use core_foundation::runloop::{CFRunLoop, kCFRunLoopCommonModes};
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
        Command::Mouse(Mouse::Hide) => {
            window_server::allow_hide_mouse().unwrap();
            event::hide_mouse().unwrap();

            println!("Press enter to show");
            std::io::stdin().read_line(&mut String::new())?;
            event::show_mouse().unwrap();

            println!("Press enter to exit");
            std::io::stdin().read_line(&mut String::new())?;
        }
        Command::Space(space_cmd) => handle_space_command(space_cmd),
        Command::Inspect => inspect(MainThreadMarker::new().unwrap()),
    }
    Ok(())
}

fn parse_space_ref(space_ref: &str, all_spaces: &[screen::SpaceId]) -> Option<screen::SpaceId> {
    if space_ref.starts_with('@') {
        // Parse as space ID
        let space_id_str = &space_ref[1..];
        if let Ok(space_id) = space_id_str.parse::<u64>() {
            screen::SpaceId::from_u64(space_id)
        } else {
            None
        }
    } else {
        // Parse as index (1-based)
        if let Ok(index) = space_ref.parse::<usize>() {
            if index > 0 && index <= all_spaces.len() {
                Some(all_spaces[index - 1])
            } else {
                None
            }
        } else {
            None
        }
    }
}

fn handle_space_command(space_cmd: Space) {
    match space_cmd {
        Space::List { verbose } => {
            let current_space = space::get_active_space();
            let management_mode = space::get_space_management_mode();
            let all_spaces = space::get_all_spaces();
            let visible_spaces = space::get_visible_spaces();

            println!("Space management mode: {:?}", management_mode);
            if let Some(current) = current_space {
                println!("Current space: {}", current.as_u64());
            }

            println!("\nAll spaces ({} total):", all_spaces.len());
            for (i, space_id) in all_spaces.iter().enumerate() {
                let space_type = space::get_space_type(*space_id);
                let name = space::copy_space_name(*space_id)
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("Space {}", space_id.as_u64()));

                let current_indicator = if Some(*space_id) == current_space {
                    " (current)"
                } else {
                    ""
                };

                let visible_indicator = if visible_spaces.contains(space_id) {
                    " [visible]"
                } else {
                    ""
                };

                if verbose {
                    let level = space::get_space_absolute_level(*space_id);
                    let compat_id = space::get_space_compat_id(*space_id);
                    let display = space::copy_managed_display_for_space(*space_id)
                        .map(|d| d.to_string())
                        .unwrap_or_else(|| "unknown".to_string());

                    println!(
                        "  {}: {} - {:?} (ID: {}, level: {}, compat: {}, display: {}){}{}",
                        i + 1,
                        name,
                        space_type,
                        space_id.as_u64(),
                        level,
                        compat_id,
                        display,
                        current_indicator,
                        visible_indicator
                    );
                } else {
                    println!(
                        "  {}: {} - {:?} (ID: {}){}{}",
                        i + 1,
                        name,
                        space_type,
                        space_id.as_u64(),
                        current_indicator,
                        visible_indicator
                    );
                }
            }
        }
        Space::Current => {
            if let Some(current_space) = space::get_active_space() {
                let all_spaces = space::get_all_spaces();
                let index =
                    all_spaces.iter().position(|&s| s == current_space).map(|i| i + 1).unwrap_or(0);

                println!("Current space: {} (ID: {})", index, current_space.as_u64());
                if let Some(name) = space::copy_space_name(current_space) {
                    println!("Name: {}", name.to_string());
                }
                let space_type = space::get_space_type(current_space);
                println!("Type: {:?}", space_type);
                let level = space::get_space_absolute_level(current_space);
                println!("Level: {}", level);
                let compat_id = space::get_space_compat_id(current_space);
                println!("Compatibility ID: {}", compat_id);
                if let Some(display) = space::copy_managed_display_for_space(current_space) {
                    println!("Display: {}", display.to_string());
                }
            } else {
                println!("Could not get current space");
            }
        }
        Space::Switch { space_ref } => {
            let all_spaces = space::get_all_spaces();
            if let Some(space) = parse_space_ref(&space_ref, &all_spaces) {
                space::set_current_space(space);
                let index = all_spaces.iter().position(|&s| s == space).map(|i| i + 1).unwrap_or(0);
                println!("Switched to space {} (ID: {})", index, space.as_u64());
            } else {
                println!("Invalid space reference: {} (use index or @ID)", space_ref);
            }
        }
        Space::Create { name, space_type } => {
            use core_foundation::dictionary::CFDictionary;
            use core_foundation::number::CFNumber;
            use core_foundation::string::CFString;

            let mut options: Vec<(CFString, CFNumber)> = Vec::new();

            if let Some(st) = space_type {
                let type_num = match st.as_str() {
                    "user" => 0,
                    "fullscreen" => 1,
                    "system" => 2,
                    _ => {
                        println!("Invalid space type. Use: user, fullscreen, system");
                        return;
                    }
                };
                options.push((CFString::new("type"), CFNumber::from(type_num)));
            }

            let options_dict = if options.is_empty() {
                None
            } else {
                Some(CFDictionary::from_CFType_pairs(&options))
            };

            if let Some(new_space) = space::create_space(options_dict) {
                println!("Created space: {}", new_space.as_u64());

                // Set name after creation if provided
                if let Some(n) = name {
                    if let Err(err) = space::set_space_name(new_space, &n) {
                        println!("Warning: Failed to set space name: error {}", err);
                    }
                }
            } else {
                println!("Failed to create space");
            }
        }
        Space::Destroy { space_ref } => {
            let all_spaces = space::get_all_spaces();
            if let Some(space) = parse_space_ref(&space_ref, &all_spaces) {
                space::destroy_space(space);
                let index = all_spaces.iter().position(|&s| s == space).map(|i| i + 1).unwrap_or(0);
                println!("Destroyed space {} (ID: {})", index, space.as_u64());
            } else {
                println!("Invalid space reference: {} (use index or @ID)", space_ref);
            }
        }
        Space::Rename { space_ref, name } => {
            let all_spaces = space::get_all_spaces();
            if let Some(space) = parse_space_ref(&space_ref, &all_spaces) {
                match space::set_space_name(space, &name) {
                    Ok(_) => {
                        let index =
                            all_spaces.iter().position(|&s| s == space).map(|i| i + 1).unwrap_or(0);
                        println!("Renamed space {} (ID: {}) to '{}'", index, space.as_u64(), name);
                    }
                    Err(err) => println!("Failed to rename space: error {}", err),
                }
            } else {
                println!("Invalid space reference: {} (use index or @ID)", space_ref);
            }
        }
        Space::SetLevel { space_ref, level } => {
            let all_spaces = space::get_all_spaces();
            if let Some(space) = parse_space_ref(&space_ref, &all_spaces) {
                space::set_space_absolute_level(space, level);
                let index = all_spaces.iter().position(|&s| s == space).map(|i| i + 1).unwrap_or(0);
                println!("Set space {} (ID: {}) level to {}", index, space.as_u64(), level);
            } else {
                println!("Invalid space reference: {} (use index or @ID)", space_ref);
            }
        }
        Space::SetType { space_ref, space_type } => {
            let all_spaces = space::get_all_spaces();
            if let Some(space) = parse_space_ref(&space_ref, &all_spaces) {
                let st = match space_type.as_str() {
                    "user" => space::SpaceType::User,
                    "fullscreen" => space::SpaceType::Fullscreen,
                    "system" => space::SpaceType::System,
                    _ => {
                        println!("Invalid space type. Use: user, fullscreen, system");
                        return;
                    }
                };
                space::set_space_type(space, st);
                let index = all_spaces.iter().position(|&s| s == space).map(|i| i + 1).unwrap_or(0);
                println!("Set space {} (ID: {}) type to {:?}", index, space.as_u64(), st);
            } else {
                println!("Invalid space reference: {} (use index or @ID)", space_ref);
            }
        }
        Space::Info { space_ref } => {
            let all_spaces = space::get_all_spaces();
            if let Some(space) = parse_space_ref(&space_ref, &all_spaces) {
                let index = all_spaces.iter().position(|&s| s == space).map(|i| i + 1).unwrap_or(0);
                println!("Space {} (ID: {}) info:", index, space.as_u64());

                if let Some(name) = space::copy_space_name(space) {
                    println!("  Name: {}", name.to_string());
                }

                let space_type = space::get_space_type(space);
                println!("  Type: {:?}", space_type);

                let level = space::get_space_absolute_level(space);
                println!("  Level: {}", level);

                let compat_id = space::get_space_compat_id(space);
                println!("  Compatibility ID: {}", compat_id);

                if let Some(display) = space::copy_managed_display_for_space(space) {
                    println!("  Display: {}", display.to_string());
                }

                let transform = space::get_space_transform(space);
                println!(
                    "  Transform: a={}, b={}, c={}, d={}, tx={}, ty={}",
                    transform.a, transform.b, transform.c, transform.d, transform.tx, transform.ty
                );

                // Temporarily disabled due to segfault
                // if let Some(values) = space::copy_space_values(space) {
                //     println!("  Values: {:?}", values);
                // } else {
                //     println!("  Values: none");
                // }
            } else {
                println!("Invalid space reference: {} (use index or @ID)", space_ref);
            }
        }
        Space::Owners { space_ref } => {
            let all_spaces = space::get_all_spaces();
            if let Some(space) = parse_space_ref(&space_ref, &all_spaces) {
                let owners = space::copy_space_owners(space);
                let index = all_spaces.iter().position(|&s| s == space).map(|i| i + 1).unwrap_or(0);
                println!("Space {} (ID: {}) owners:", index, space.as_u64());
                for (i, owner) in owners.iter().enumerate() {
                    println!("  {}: PID {}", i + 1, owner);
                }
            } else {
                println!("Invalid space reference: {} (use index or @ID)", space_ref);
            }
        }
        Space::AddWindows { space_ref, window_ids } => {
            let all_spaces = space::get_all_spaces();
            if let Some(space) = parse_space_ref(&space_ref, &all_spaces) {
                space::add_windows_to_spaces(&window_ids, &[space]);
                let index = all_spaces.iter().position(|&s| s == space).map(|i| i + 1).unwrap_or(0);
                println!(
                    "Added {} windows to space {} (ID: {})",
                    window_ids.len(),
                    index,
                    space.as_u64()
                );
            } else {
                println!("Invalid space reference: {} (use index or @ID)", space_ref);
            }
        }
        Space::RemoveWindows { space_ref, window_ids } => {
            let all_spaces = space::get_all_spaces();
            if let Some(space) = parse_space_ref(&space_ref, &all_spaces) {
                space::remove_windows_from_spaces(&window_ids, &[space]);
                let index = all_spaces.iter().position(|&s| s == space).map(|i| i + 1).unwrap_or(0);
                println!(
                    "Removed {} windows from space {} (ID: {})",
                    window_ids.len(),
                    index,
                    space.as_u64()
                );
            } else {
                println!("Invalid space reference: {} (use index or @ID)", space_ref);
            }
        }
    }
}

fn inspect(mtm: MainThreadMarker) {
    let (tx, rx) = unbounded_channel();
    let hook =
        livesplit_hotkey::Hook::with_consume_preference(ConsumePreference::MustConsume).unwrap();
    let key = event::Hotkey {
        key_code: event::KeyCode::KeyI,
        modifiers: Modifiers::ALT | Modifiers::SHIFT,
    };
    hook.register(key, move || _ = tx.send(())).unwrap();
    println!("Press {key:?} to inspect the window under the mouse");
    Executor::run(inspect_inner(rx, mtm));
}

async fn inspect_inner(mut rx: UnboundedReceiver<()>, mtm: MainThreadMarker) {
    let mut screen_cache = ScreenCache::new(mtm);
    let (_, _, converter) = screen_cache.update_screen_config();
    while let Some(()) = rx.recv().await {
        let Some(pos) = get_mouse_pos(converter) else { continue };
        // This API doesn't always work, but for some reason get_window_at_point
        // *never* works from devtool.
        let mut element: AXUIElementRef = ptr::null_mut();
        let err = unsafe {
            AXUIElementCopyElementAtPosition(
                AXUIElement::system_wide().as_CFTypeRef() as _,
                pos.x as f32,
                pos.y as f32,
                &raw mut element,
            )
        };
        if err != 0 {
            println!("Failed to get element under cursor: {err:?}");
            continue;
        }
        let Ok(ax_window) = unsafe { AXUIElement::from_mut_void(element as *mut _) }.window()
        else {
            println!("No window for element {element:#?}");
            continue;
        };
        println!("{ax_window:#?}");
        let Some(info) =
            WindowServerId::try_from(&ax_window).ok().and_then(|wsid| get_window(wsid))
        else {
            println!("Couldn't get window server info for {element:?}");
            continue;
        };
        println!("{info:#?}");
    }
}

async fn get_windows_with_cg(opt: &Opt, print: bool) {
    let windows: CFArray<CFDictionaryRef> = unsafe {
        CFArray::wrap_under_get_rule(CGWindowListCopyWindowInfo(
            kCGWindowListOptionOnScreenOnly,
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
    let windows = unsafe {
        NSWindow::windowNumbersWithOptions(NSWindowNumberListOptions::AllApplications, mtm)
    };
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
