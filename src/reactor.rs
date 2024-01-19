use std::{collections::HashMap, iter, sync, thread};

use icrate::Foundation::{CGPoint, CGRect, CGSize};
use log::{debug, info};
use rand::seq::SliceRandom;

use crate::{
    animation::Animation,
    app::{pid_t, AppThreadHandle, WindowId},
    screen::SpaceId,
};

pub use std::sync::mpsc::Sender;

#[derive(Debug)]
pub enum Event {
    ApplicationLaunched(pid_t, AppInfo, AppThreadHandle, Vec<(WindowId, WindowInfo)>),
    ApplicationTerminated(pid_t),
    ApplicationActivated(pid_t),
    WindowCreated(WindowId, WindowInfo),
    WindowDestroyed(WindowId),
    WindowMoved(WindowId, CGPoint),
    WindowResized(WindowId, CGSize),
    ScreenParametersChanged(Vec<CGRect>, Vec<SpaceId>),
    SpaceChanged(Vec<SpaceId>),
    Command(Command),
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct AppInfo {
    pub bundle_id: Option<String>,
    pub localized_name: Option<String>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct WindowInfo {
    pub is_standard: bool,
    pub title: String,
    pub frame: CGRect,
}

#[derive(Debug, Clone)]
pub enum Command {
    Hello,
    Shuffle,
}

pub struct Reactor {
    apps: HashMap<pid_t, AppState>,
    window_order: Vec<WindowId>,
    windows: HashMap<WindowId, WindowInfo>,
    main_screen: Option<Screen>,
    space: Option<SpaceId>,
}

pub struct AppState {
    pub info: AppInfo,
    pub handle: AppThreadHandle,
}

#[derive(Copy, Clone, Debug)]
struct Screen {
    frame: CGRect,
    space: SpaceId,
}

impl Reactor {
    pub fn spawn() -> Sender<Event> {
        let (events_tx, events) = sync::mpsc::channel::<Event>();
        thread::spawn(move || {
            let mut handler = Reactor {
                apps: HashMap::new(),
                window_order: Vec::new(),
                windows: HashMap::new(),
                main_screen: None,
                space: None,
            };
            for event in events {
                handler.handle_event(event);
            }
        });
        events_tx
    }

    pub fn handle_event(&mut self, event: Event) {
        info!("Event {event:?}");
        let mut new_wid = None;
        match event {
            Event::ApplicationLaunched(pid, info, handle, windows) => {
                self.apps.insert(pid, AppState { info, handle });
                self.window_order.extend(
                    windows.iter().filter(|(_, info)| info.is_standard).map(|(wid, _)| wid),
                );
                self.windows.extend(windows.into_iter());
            }
            Event::ApplicationTerminated(pid) => {
                self.window_order.retain(|wid| wid.pid != pid);
                self.apps.remove(&pid).unwrap();
            }
            Event::ApplicationActivated(_) => {
                return;
            }
            Event::WindowCreated(wid, window) => {
                // Don't manage windows on other spaces.
                // TODO: It's possible for a window to be on multiple spaces
                // or move spaces.
                if self.main_screen.map(|s| s.space) == self.space && window.is_standard {
                    self.window_order.push(wid);
                }
                self.windows.insert(wid, window);
                new_wid = Some(wid);
            }
            Event::WindowDestroyed(wid) => {
                self.window_order.retain(|&id| wid != id);
                self.windows.remove(&wid).unwrap();
            }
            Event::WindowMoved(wid, pos) => {
                self.windows.get_mut(&wid).unwrap().frame.origin = pos;
                return;
            }
            Event::WindowResized(wid, size) => {
                self.windows.get_mut(&wid).unwrap().frame.size = size;
                return;
            }
            Event::ScreenParametersChanged(frame, spaces) => {
                if self.space.is_none() {
                    self.space = spaces.first().copied();
                }
                self.main_screen = frame
                    .into_iter()
                    .zip(spaces)
                    .map(|(frame, space)| Screen { frame, space })
                    .next();
            }
            Event::Command(Command::Hello) => {
                println!("Hello, world!");
            }
            Event::Command(Command::Shuffle) => {
                self.window_order.shuffle(&mut rand::thread_rng());
            }
            Event::SpaceChanged(spaces) => {
                if let Some(screen) = self.main_screen.as_mut() {
                    screen.space = *spaces
                        .first()
                        .expect("Spaces should be non-empty if there is a main screen");
                }
            }
        }
        self.update_layout(new_wid);
    }

    pub fn update_layout(&mut self, new_wid: Option<WindowId>) {
        let Some(main_screen) = self.main_screen else { return };
        if Some(main_screen.space) != self.space {
            return;
        };
        let list: Vec<_> = self
            .window_order
            .iter()
            .map(|wid| (&self.apps[&wid.pid].info, &self.windows[&wid]))
            .collect();
        info!("Window list: {list:?}");
        info!("Screen: {main_screen:?}");
        let layout = calculate_layout(
            main_screen.frame.clone(),
            &list,
            Layout::Bsp(Orientation::Horizontal),
        );
        info!("Layout: {layout:?}");

        let mut anim = Animation::new();
        for (&wid, target_frame) in self.window_order.iter().zip(layout.into_iter()) {
            let current_frame = self.windows[&wid].frame;
            if target_frame == current_frame {
                continue;
            }
            let handle = &self.apps.get(&wid.pid).unwrap().handle;
            let is_new = Some(wid) == new_wid;
            anim.add_window(handle, wid, current_frame, target_frame, is_new);
        }
        anim.run();
        //anim.skip_to_end();
    }
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone)]
enum Layout {
    Slice(Orientation),
    Bsp(Orientation),
}

#[derive(Debug, Copy, Clone)]
enum Orientation {
    Horizontal,
    Vertical,
}

fn calculate_layout(
    screen: CGRect,
    windows: &[(&AppInfo, &WindowInfo)],
    layout: Layout,
) -> Vec<CGRect> {
    use Layout::*;
    use Orientation::*;
    let num_windows: u32 = windows.len().try_into().unwrap();
    if num_windows == 0 {
        return vec![];
    }
    if num_windows == 1 {
        return vec![screen];
    }
    match layout {
        Slice(Horizontal) => {
            let width = screen.size.width / f64::from(num_windows);
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
        Slice(Vertical) => todo!(),
        Bsp(Horizontal) => {
            let size = CGSize {
                width: screen.size.width / 2.0,
                height: screen.size.height,
            };

            let window_frame = CGRect { origin: screen.origin, size };
            let remainder = CGRect {
                origin: CGPoint {
                    x: screen.origin.x + size.width,
                    y: screen.origin.y,
                },
                size,
            };

            iter::once(window_frame)
                .chain(calculate_layout(
                    remainder,
                    &windows[1..],
                    Layout::Bsp(Orientation::Vertical),
                ))
                .collect()
        }
        Layout::Bsp(Orientation::Vertical) => {
            let size = CGSize {
                width: screen.size.width,
                height: screen.size.height / 2.0,
            };

            let window_frame = CGRect { origin: screen.origin, size };
            let remainder = CGRect {
                origin: CGPoint {
                    x: screen.origin.x,
                    y: screen.origin.y + size.height,
                },
                size,
            };

            iter::once(window_frame)
                .chain(calculate_layout(
                    remainder,
                    &windows[1..],
                    Layout::Bsp(Orientation::Horizontal),
                ))
                .collect()
        }
    }
}
