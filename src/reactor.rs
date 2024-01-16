use std::{collections::HashMap, sync, thread};

use icrate::Foundation::{CGPoint, CGRect, CGSize};
use log::{debug, info};

use crate::{
    app::{pid_t, AppInfo, AppThreadHandle, Request},
    screen::SpaceId,
};

pub type WindowIdx = u32;

pub use std::sync::mpsc::Sender;

#[derive(Debug)]
pub enum Event {
    ApplicationLaunched(pid_t, AppInfo, AppThreadHandle, Vec<Window>),
    ApplicationTerminated(pid_t),
    ApplicationActivated(pid_t),
    WindowCreated(pid_t, Window),
    WindowDestroyed(pid_t, WindowIdx),
    WindowMoved(pid_t, WindowIdx, CGPoint),
    WindowResized(pid_t, WindowIdx, CGSize),
    ScreenParametersChanged(Vec<CGRect>),
    SpaceChanged(Vec<SpaceId>),
    Command(Command),
}

#[derive(Debug, Clone)]
pub enum Command {
    Hello,
    Shuffle,
}

pub struct Reactor {
    pub windows: Vec<(pid_t, WindowIdx)>,
    pub apps: HashMap<pid_t, AppState>,
    pub main_screen: Option<CGRect>,
}

pub struct AppState {
    pub info: AppInfo,
    pub handle: AppThreadHandle,
    pub windows: Vec<Window>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Window {
    pub is_standard: bool,
    pub title: String,
    pub frame: CGRect,
}

impl Reactor {
    pub fn spawn() -> Sender<Event> {
        let (events_tx, events) = sync::mpsc::channel::<Event>();
        thread::spawn(move || {
            let mut handler = Reactor {
                windows: Vec::new(),
                apps: HashMap::new(),
                main_screen: None,
            };
            for event in events {
                handler.handle_event(event);
            }
        });
        events_tx
    }

    pub fn handle_event(&mut self, event: Event) {
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
            Event::ApplicationActivated(_) => (),
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
            Event::ScreenParametersChanged(frame) => {
                self.main_screen = frame.first().copied();
            }
            Event::Command(Command::Hello) => {
                println!("Hello, world!");
            }
            Event::Command(Command::Shuffle) => (),
            Event::SpaceChanged(_space) => (),
        }
        self.update_layout();
    }

    pub fn update_layout(&mut self) {
        let Some(main_screen) = self.main_screen else { return };
        let list: Vec<_> = self
            .windows
            .iter()
            .map(|(pid, widx)| {
                (
                    &self.apps[pid].info,
                    &self.apps[pid].windows[*widx as usize],
                )
            })
            .filter(|(_app, win)| win.is_standard)
            .collect();
        debug!("Window list: {list:#?}");
        info!("Screen: {main_screen:?}");
        let layout = calculate_layout(main_screen.clone(), &list);
        info!("Layout: {layout:?}");
        for ((pid, widx), target) in self.windows.iter().zip(layout.into_iter()) {
            // TODO: Check if existing frame matches
            self.apps
                .get_mut(pid)
                .unwrap()
                .handle
                .send(Request::SetWindowFrame(*widx, target))
                .unwrap();
        }
    }
}

pub fn calculate_layout(screen: CGRect, windows: &Vec<(&AppInfo, &Window)>) -> Vec<CGRect> {
    let num_windows: u32 = windows.len().try_into().unwrap();
    let width = screen.size.width / f64::from(num_windows);
    // TODO: Convert between coordinate systems.
    (0..num_windows)
        .map(|i| CGRect {
            origin: CGPoint {
                x: screen.origin.x + f64::from(i) * width,
                y: screen.origin.y,
            },
            size: CGSize {
                width,
                height: screen.size.height,
            },
        })
        .collect()
}
