mod app;
mod notification_center;
mod run_loop;

use std::{
    collections::HashMap,
    future::Future,
    sync::{self, mpsc::Sender},
    thread,
    time::Instant,
};

use accessibility::{AXUIElement, AXUIElementAttributes};
use app::{pid_t, AppInfo, AppThreadHandle};
use core_foundation::{array::CFArray, base::TCFType, dictionary::CFDictionaryRef};
use core_graphics::{
    display::{CGDisplayBounds, CGMainDisplayID},
    window::{kCGNullWindowID, kCGWindowListOptionOnScreenOnly, CGWindowListCopyWindowInfo},
};
use core_graphics_types::geometry::{CGPoint, CGRect, CGSize};

use log::info;
use structopt::StructOpt;
use tokio::sync::mpsc;

use crate::app::Request;

#[derive(StructOpt)]
pub struct Opt {
    pub bundle: Option<String>,
    pub resize: Option<String>,
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    env_logger::init();
    let opt = Opt::from_args();
    //time("accessibility serial", || get_windows_with_ax(&opt, true)).await;
    time("core-graphics", || get_windows_with_cg(&opt, true)).await;
    time("accessibility", || get_windows_with_ax(&opt, false, true)).await;
    time("core-graphics second time", || {
        get_windows_with_cg(&opt, false)
    })
    .await;
    time("accessibility second time", || {
        get_windows_with_ax(&opt, false, false)
    })
    .await;
    let events = EventHandler::spawn(&opt);
    app::spawn_initial_app_threads(&opt, events.clone());
    notification_center::watch_for_notifications(events)
}

#[allow(dead_code)]
#[derive(Debug)]
struct Window {
    title: String,
    role: String,
    frame: CGRect,
}

impl Window {
    fn try_from_ui_element(element: &AXUIElement) -> Result<Self, accessibility::Error> {
        Ok(Window {
            title: element.title()?.to_string(),
            role: element.role()?.to_string(),
            frame: element.frame()?,
        })
    }
}

async fn get_windows_with_cg(_opt: &Opt, print: bool) {
    let windows: CFArray<CFDictionaryRef> = unsafe {
        CFArray::wrap_under_get_rule(CGWindowListCopyWindowInfo(
            kCGWindowListOptionOnScreenOnly,
            kCGNullWindowID,
        ))
    };
    if print {
        println!("{windows:?}");
    }
    let display_id = unsafe { CGMainDisplayID() };
    let screen = unsafe { CGDisplayBounds(display_id) };
    println!("main display = {screen:?}");
}

async fn get_windows_with_ax(opt: &Opt, serial: bool, print: bool) {
    let (sender, mut receiver) = mpsc::unbounded_channel();
    for (pid, bundle_id) in app::running_apps(opt) {
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

fn get_windows_for_app(app: AXUIElement) -> Result<Vec<Window>, accessibility::Error> {
    let Ok(windows) = &app.windows() else {
        return Err(accessibility::Error::NotFound);
    };
    windows.into_iter().map(|win| Window::try_from_ui_element(&*win)).collect()
}

async fn time<O, F: Future<Output = O>>(desc: &str, f: impl FnOnce() -> F) -> O {
    let start = Instant::now();
    let out = f().await;
    let end = Instant::now();
    println!("{desc} took {:?}", end - start);
    out
}

type WindowIdx = u32;

#[derive(Debug)]
enum Event {
    ApplicationLaunched(pid_t, AppInfo, AppThreadHandle, Vec<Window>),
    ApplicationTerminated(pid_t),
    ApplicationActivated(pid_t),
    WindowCreated(pid_t, Window),
    WindowDestroyed(pid_t, WindowIdx),
    WindowMoved(pid_t, WindowIdx, CGPoint),
    WindowResized(pid_t, WindowIdx, CGSize),
    ScreenParametersChanged,
}

struct EventHandler {
    windows: Vec<(pid_t, WindowIdx)>,
    apps: HashMap<pid_t, AppState>,
}

struct AppState {
    info: AppInfo,
    handle: AppThreadHandle,
    windows: Vec<Window>,
}

impl EventHandler {
    fn spawn(_opt: &Opt) -> Sender<Event> {
        let (events_tx, events) = sync::mpsc::channel::<Event>();
        thread::spawn(move || {
            let mut handler = EventHandler {
                windows: Vec::new(),
                apps: HashMap::new(),
            };
            for event in events {
                handler.handle_event(event);
            }
        });
        events_tx
    }

    fn handle_event(&mut self, event: Event) {
        info!("Event {event:?}");
        match event {
            Event::ApplicationLaunched(pid, info, handle, windows) => {
                self.windows.extend((0..windows.len()).map(|w| (pid, w as WindowIdx)));
                self.apps.insert(pid, AppState { info, handle, windows });
            }
            Event::ApplicationTerminated(pid) => {
                self.windows.retain(|(w_pid, _)| *w_pid != pid);
                self.apps.remove(&pid).unwrap();
            }
            Event::WindowCreated(pid, window) => {
                let app = self.apps.get_mut(&pid).unwrap();
                self.windows.push((pid, app.windows.len() as WindowIdx));
                app.windows.push(window);
            }
            Event::WindowDestroyed(pid, idx) => {
                self.windows.retain(|wid| *wid != (pid, idx));
                self.apps.get_mut(&pid).unwrap().windows.remove(idx as usize);
            }
            Event::WindowMoved(pid, idx, pos) => {
                self.apps.get_mut(&pid).unwrap().windows[idx as usize].frame.origin = pos;
            }
            Event::WindowResized(pid, idx, size) => {
                self.apps.get_mut(&pid).unwrap().windows[idx as usize].frame.size = size;
            }
            _ => return,
        }
        let list: Vec<_> = self
            .windows
            .iter()
            .map(|(pid, widx)| {
                (
                    &self.apps[pid].info,
                    &self.apps[pid].windows[*widx as usize],
                )
            })
            .collect();
        info!("Window list: {list:#?}");
    }
}

// Next:
// - Define a synchronous, long-lived task for each application.
// - Spawn each of these onto a thread pool. Ideally one thread per app.
// - Register AX observers on that thread's run loop.
// - Turn events into messages sent from the app threads and the main threads to
//   a single "wm logic" thread.
// - Bidirectional communication between this thread and the others becomes the
//   thing that async ops are built on (if we do that).
