use std::{collections::HashMap, iter, sync, thread};

use icrate::Foundation::{CGPoint, CGRect, CGSize};
use rand::seq::SliceRandom;
use tracing::Span;
use tracing::{debug, info};

use crate::{
    animation::Animation,
    app::{pid_t, AppThreadHandle, RaiseToken, Request, WindowId},
    screen::SpaceId,
    util::{Round, SameAs},
};

pub use std::sync::mpsc::Sender;

#[derive(Debug)]
pub enum Event {
    ApplicationLaunched(pid_t, AppState, Vec<(WindowId, WindowInfo)>),
    ApplicationTerminated(pid_t),
    ApplicationActivated(pid_t, Option<WindowId>),
    ApplicationGloballyActivated(pid_t),
    ApplicationGloballyDeactivated(pid_t),
    ApplicationDeactivated(pid_t),
    ApplicationMainWindowChanged(pid_t, Option<WindowId>),
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

#[derive(Debug)]
#[allow(dead_code)]
pub struct WindowInfo {
    pub is_standard: bool,
    pub title: String,
    pub frame: CGRect,
}

#[derive(Debug, Clone)]
pub enum Command {
    Hello,
    Shuffle,
    NextWindow,
    PrevWindow,
}

pub struct Reactor {
    apps: HashMap<pid_t, AppState>,
    window_order: Vec<WindowId>,
    windows: HashMap<WindowId, WindowInfo>,
    main_screen: Option<Screen>,
    space: Option<SpaceId>,
    frontmost_app: Option<pid_t>,
    global_frontmost_app_pid: Option<pid_t>,
    last_layout: Option<Vec<(WindowId, CGRect)>>,
    raise_token: RaiseToken,
}

#[derive(Debug)]
pub struct AppState {
    pub info: AppInfo,
    pub handle: AppThreadHandle,
    pub main_window: Option<WindowId>,
    // You should probably use `frontmost_app` in reactor instead.
    pub is_frontmost: bool,
}

#[derive(Copy, Clone, Debug)]
struct Screen {
    frame: CGRect,
    space: SpaceId,
}

impl Reactor {
    pub fn spawn() -> Sender<(Span, Event)> {
        let (events_tx, events) = sync::mpsc::channel::<(Span, Event)>();
        thread::spawn(move || {
            let mut this = Reactor::new();
            for (span, event) in events {
                let _guard = span.enter();
                this.handle_event(event);
            }
        });
        events_tx
    }

    fn new() -> Reactor {
        Reactor {
            apps: HashMap::new(),
            window_order: Vec::new(),
            windows: HashMap::new(),
            main_screen: None,
            space: None,
            frontmost_app: None,
            global_frontmost_app_pid: None,
            last_layout: None,
            raise_token: RaiseToken::default(),
        }
    }

    /// The main window of the active app, if any.
    fn main_window(&self) -> Option<WindowId> {
        let Some(pid) = self.frontmost_app else { return None };
        self.apps[&pid].main_window
    }

    fn handle_event(&mut self, event: Event) {
        info!("Event {event:?}");
        let mut animation_focus_wid = None;
        match event {
            Event::ApplicationLaunched(pid, state, windows) => {
                self.apps.insert(pid, state);
                self.window_order.extend(
                    windows.iter().filter(|(_, info)| info.is_standard).map(|(wid, _)| wid),
                );
                self.windows.extend(windows.into_iter());
            }
            Event::ApplicationTerminated(pid) => {
                // FIXME: This isn't ordered wrt other events from the app;
                // reroute the event through the app thread so it's the last
                // event for this app.
                self.apps.remove(&pid).unwrap();
                self.window_order.retain(|wid| wid.pid != pid);
                if Some(pid) == self.frontmost_app {
                    self.frontmost_app = None;
                }
            }
            Event::ApplicationActivated(pid, main_window) => {
                let state = self.apps.get_mut(&pid).unwrap();
                state.is_frontmost = true;
                state.main_window = main_window;
                // Because apps self-report this event from their respective
                // threads, they can appear out of order. To mitigate this, we
                // require that the "global" view from NSNotificationCenter
                // agrees with the app about which is frontmost. This guarantees
                // eventual consistency.
                //
                // Since the global events provide an authoritative ordering, why
                // care about this event at all? The reason is that we want to
                // know what the main window of the app is upon activation. This
                // is important when the user clicks on a window of the app
                // that was not previously the main window: The frontmost app
                // and its main window can switch at the same time. In that case
                // we don't want to record the old main window as having focus,
                // since it never did. So we wait until both events are received.
                if self.global_frontmost_app_pid == Some(pid) {
                    self.frontmost_app = Some(pid);
                }
            }
            Event::ApplicationGloballyActivated(pid) => {
                // See above comment.
                self.global_frontmost_app_pid = Some(pid);
                if self.apps.get(&pid).map(|a| a.is_frontmost).unwrap_or(false) {
                    self.frontmost_app = Some(pid);
                }
            }
            Event::ApplicationDeactivated(pid) => {
                self.apps.get_mut(&pid).unwrap().is_frontmost = false;
                if self.frontmost_app == Some(pid) {
                    self.frontmost_app = None;
                }
            }
            Event::ApplicationGloballyDeactivated(pid) => {
                if self.global_frontmost_app_pid == Some(pid) {
                    self.global_frontmost_app_pid = None;
                }
                if self.frontmost_app == Some(pid) {
                    self.frontmost_app = None;
                }
            }
            Event::ApplicationMainWindowChanged(pid, main_window) => {
                self.apps.get_mut(&pid).unwrap().main_window = main_window;
            }
            Event::WindowCreated(wid, window) => {
                // Don't manage windows on other spaces.
                // TODO: It's possible for a window to be on multiple spaces
                // or move spaces.
                if self.main_screen.map(|s| s.space) == self.space && window.is_standard {
                    self.window_order.push(wid);
                }
                self.windows.insert(wid, window);
                animation_focus_wid = Some(wid);
            }
            Event::WindowDestroyed(wid) => {
                self.window_order.retain(|&id| wid != id);
                self.windows.remove(&wid).unwrap();
                animation_focus_wid = self.window_order.last().cloned();
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
            Event::SpaceChanged(spaces) => {
                if let Some(screen) = self.main_screen.as_mut() {
                    screen.space = *spaces
                        .first()
                        .expect("Spaces should be non-empty if there is a main screen");
                }
            }
            Event::Command(Command::Hello) => {
                println!("Hello, world!");
            }
            Event::Command(Command::Shuffle) => {
                self.window_order.shuffle(&mut rand::thread_rng());
            }
            Event::Command(Command::NextWindow) => {
                let Some(cur) = self.main_window() else { return };
                let Some(idx) = self.window_order.iter().position(|&wid| wid == cur) else {
                    return;
                };
                let Some(&new) = self.window_order.get(idx + 1) else {
                    return;
                };
                self.raise_window(new);
            }
            Event::Command(Command::PrevWindow) => {
                let Some(cur) = self.main_window() else { return };
                let Some(idx) = self.window_order.iter().position(|&wid| wid == cur) else {
                    return;
                };
                if idx == 0 {
                    return;
                }
                let Some(&new) = self.window_order.get(idx - 1) else {
                    return;
                };
                self.raise_window(new);
            }
        }
        self.update_layout(animation_focus_wid);
    }

    fn raise_window(&mut self, wid: WindowId) {
        self.raise_token.set_pid(wid.pid);
        self.apps
            .get_mut(&wid.pid)
            .unwrap()
            .handle
            .send(Request::Raise(wid, self.raise_token.clone()))
            .unwrap();
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
        info!("Screen: {main_screen:?}");
        let main_window = self.main_window();
        info!("Main window: {main_window:?}");
        let layout = calculate_layout(
            main_screen.frame.clone(),
            &list,
            Layout::Bsp(Orientation::Horizontal),
        );
        info!("Window list: {list:?}");
        info!("Layout: {layout:?}");

        assert_eq!(layout.len(), self.window_order.len());
        let layout: Vec<_> = self.window_order.iter().copied().zip(layout).collect();
        if let Some(last) = &self.last_layout {
            if last.len() == layout.len()
                && last.iter().zip(&layout).all(|(a, b)| a.0 == b.0 && a.1.same_as(b.1))
            {
                info!("Layout unchanged");
                return;
            }
        }

        let mut anim = Animation::new();
        for &(wid, target_frame) in &layout {
            let current_frame = self.windows[&wid].frame;
            let target_frame = target_frame.round();
            if target_frame.same_as(current_frame) {
                continue;
            }
            debug!("Change: {current_frame:?} to {target_frame:?}");
            let handle = &self.apps.get(&wid.pid).unwrap().handle;
            let is_new = Some(wid) == new_wid;
            anim.add_window(handle, wid, current_frame, target_frame, is_new);
        }
        anim.run();
        //anim.skip_to_end();

        self.last_layout = Some(layout);
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

#[cfg(test)]
mod tests {
    use std::sync::mpsc::Receiver;

    use crate::app::Request;

    use super::*;

    #[derive(Default)]
    struct Apps(HashMap<pid_t, Receiver<(Span, Request)>>);
    impl Apps {
        fn make_app(&mut self, pid: pid_t, windows: Vec<WindowInfo>) -> Event {
            let (handle, rx) = AppThreadHandle::new_for_test();
            let existing = self.0.insert(pid, rx);
            assert!(existing.is_none());
            Event::ApplicationLaunched(
                pid,
                AppState {
                    info: AppInfo {
                        bundle_id: Some(format!("com.testapp{pid}")),
                        localized_name: Some(format!("TestApp{pid}")),
                    },
                    handle,
                    main_window: None,
                    is_frontmost: false,
                },
                (1..).map(|idx| WindowId::new(pid, idx)).zip(windows).collect(),
            )
        }
    }

    fn make_windows(count: usize) -> Vec<WindowInfo> {
        (1..=count)
            .map(|idx| WindowInfo {
                is_standard: true,
                title: format!("Window{idx}"),
                frame: CGRect::new(
                    CGPoint::new(100.0 * f64::from(idx as u32), 100.0),
                    CGSize::new(50.0, 50.0),
                ),
            })
            .collect()
    }

    #[test]
    fn it_tracks_frontmost_app_and_main_window_correctly() {
        use Event::*;
        let mut apps = Apps::default();
        let mut reactor = Reactor::new();
        reactor.handle_event(apps.make_app(1, make_windows(2)));
        reactor.handle_event(apps.make_app(2, make_windows(2)));
        assert_eq!(None, reactor.frontmost_app);
        reactor.handle_event(ApplicationGloballyActivated(1));
        reactor.handle_event(ApplicationActivated(1, Some(WindowId::new(1, 1))));
        assert_eq!(Some(1), reactor.frontmost_app);
        assert_eq!(Some(WindowId::new(1, 1)), reactor.main_window());
        reactor.handle_event(ApplicationGloballyDeactivated(1));
        assert_eq!(None, reactor.frontmost_app);
        assert_eq!(None, reactor.main_window());
        reactor.handle_event(ApplicationActivated(2, None));
        reactor.handle_event(ApplicationGloballyActivated(2));
        assert_eq!(Some(2), reactor.frontmost_app);
        assert_eq!(None, reactor.main_window());
        reactor.handle_event(ApplicationMainWindowChanged(2, Some(WindowId::new(2, 2))));
        assert_eq!(Some(WindowId::new(2, 2)), reactor.main_window());
        reactor.handle_event(ApplicationMainWindowChanged(1, Some(WindowId::new(1, 2))));
        assert_eq!(Some(WindowId::new(2, 2)), reactor.main_window());
        reactor.handle_event(ApplicationDeactivated(1));
        assert_eq!(Some(2), reactor.frontmost_app);
        assert_eq!(Some(WindowId::new(2, 2)), reactor.main_window());
        reactor.handle_event(ApplicationDeactivated(2));
        assert_eq!(None, reactor.frontmost_app);
        assert_eq!(None, reactor.main_window());
    }
}
