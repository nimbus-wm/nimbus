use std::{collections::HashMap, sync, sync::mpsc::Sender, thread};

use core_graphics_types::geometry::{CGPoint, CGRect, CGSize};
use icrate::Foundation::CGRect as NSRect;
use log::info;

use super::Opt;
use crate::app::{pid_t, AppInfo, AppThreadHandle, Request};

pub(crate) type WindowIdx = u32;

#[derive(Debug)]
pub(crate) enum Event {
    ApplicationLaunched(pid_t, AppInfo, AppThreadHandle, Vec<Window>),
    ApplicationTerminated(pid_t),
    ApplicationActivated(pid_t),
    WindowCreated(pid_t, Window),
    WindowDestroyed(pid_t, WindowIdx),
    WindowMoved(pid_t, WindowIdx, CGPoint),
    WindowResized(pid_t, WindowIdx, CGSize),
    ScreenParametersChanged(Option<NSRect>),
}

pub(crate) struct Reactor {
    pub(crate) windows: Vec<(pid_t, WindowIdx)>,
    pub(crate) apps: HashMap<pid_t, AppState>,
    pub(crate) main_screen: Option<NSRect>,
}

pub(crate) struct AppState {
    pub(crate) info: AppInfo,
    pub(crate) handle: AppThreadHandle,
    pub(crate) windows: Vec<Window>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) struct Window {
    pub(crate) title: String,
    pub(crate) role: String,
    pub(crate) frame: CGRect,
}

impl Reactor {
    pub(crate) fn spawn(_opt: &Opt) -> Sender<Event> {
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

    pub(crate) fn handle_event(&mut self, event: Event) {
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
            Event::ScreenParametersChanged(frame) => {
                self.main_screen = frame;
            }
            _ => return,
        }
        self.update_layout();
    }

    pub(crate) fn update_layout(&mut self) {
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
            .collect();
        info!("Window list: {list:#?}");
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

pub(crate) fn calculate_layout(screen: NSRect, windows: &Vec<(&AppInfo, &Window)>) -> Vec<CGRect> {
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
