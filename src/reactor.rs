use std::{collections::HashMap, sync, thread};

use icrate::Foundation::{CGPoint, CGRect};
use tracing::Span;
use tracing::{debug, info};

use crate::app::{AppInfo, WindowInfo};
use crate::layout::{self, LayoutCommand, LayoutEvent, LayoutManager};
use crate::metrics::{self, MetricsCommand};
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
    WindowMoved(WindowId, CGPoint, TransactionId),
    WindowResized(WindowId, CGRect, TransactionId),
    ScreenParametersChanged(Vec<CGRect>, Vec<SpaceId>),
    SpaceChanged(Vec<SpaceId>),
    Command(Command),
}

#[derive(Debug, Clone)]
pub enum Command {
    Hello,
    Layout(LayoutCommand),
    Metrics(MetricsCommand),
}

pub struct Reactor {
    apps: HashMap<pid_t, AppState>,
    layout: LayoutManager,
    windows: HashMap<WindowId, WindowState>,
    main_screen: Option<Screen>,
    space: Option<SpaceId>,
    frontmost_app: Option<pid_t>,
    global_frontmost_app_pid: Option<pid_t>,
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

/// A per-window counter that tracks the last time the reactor sent a request to
/// change the window frame.
#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct TransactionId(u32);

#[derive(Debug)]
pub struct WindowState {
    #[allow(unused)]
    title: String,
    frame_last_read: CGRect,
    frame_last_written: CGRect,
    last_sent_txid: TransactionId,
}

impl WindowState {
    fn next_txid(&mut self) -> TransactionId {
        self.last_sent_txid.0 += 1;
        self.last_sent_txid
    }
}

impl From<WindowInfo> for WindowState {
    fn from(info: WindowInfo) -> Self {
        WindowState {
            title: info.title,
            frame_last_read: info.frame,
            frame_last_written: CGRect::ZERO,
            last_sent_txid: TransactionId::default(),
        }
    }
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
            layout: LayoutManager::new(),
            windows: HashMap::new(),
            main_screen: None,
            space: None,
            frontmost_app: None,
            global_frontmost_app_pid: None,
            raise_token: RaiseToken::default(),
        }
    }

    /// The main window of the active app, if any.
    fn main_window(&self) -> Option<WindowId> {
        let Some(pid) = self.frontmost_app else { return None };
        self.apps[&pid].main_window
    }

    fn handle_event(&mut self, event: Event) {
        info!(?event, "Event");
        let main_window_orig = self.main_window();
        let mut animation_focus_wid = None;
        let mut is_resize = false;
        match event {
            Event::ApplicationLaunched(pid, state, windows) => {
                let is_frontmost = state.is_frontmost;
                self.apps.insert(pid, state);
                self.layout.add_windows(
                    self.space.unwrap(),
                    windows.iter().filter(|(_, info)| info.is_standard).map(|(wid, _)| *wid),
                );
                self.windows.extend(windows.into_iter().map(|(wid, info)| (wid, info.into())));
                // See comment for ApplicationActivated below.
                if is_frontmost && self.global_frontmost_app_pid == Some(pid) {
                    self.frontmost_app = Some(pid);
                }
            }
            Event::ApplicationTerminated(pid) => {
                // FIXME: This isn't ordered wrt other events from the app;
                // reroute the event through the app thread so it's the last
                // event for this app.
                self.apps.remove(&pid).unwrap();
                self.layout.retain_windows(|wid| wid.pid != pid);
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
                    self.layout.add_window(self.space.unwrap(), wid);
                }
                self.windows.insert(wid, window.into());
                animation_focus_wid = Some(wid);
            }
            Event::WindowDestroyed(wid) => {
                self.layout.retain_windows(|&id| wid != id);
                self.windows.remove(&wid).unwrap();
                //animation_focus_wid = self.window_order.last().cloned();
            }
            Event::WindowMoved(wid, pos, last_seen) => {
                let window = self.windows.get_mut(&wid).unwrap();
                if last_seen != window.last_sent_txid {
                    // Ignore events that happened before the last time we
                    // changed the size or position of this window.
                    return;
                }
                window.frame_last_read.origin = pos;
                return;
            }
            Event::WindowResized(wid, new_frame, last_seen) => {
                let window = self.windows.get_mut(&wid).unwrap();
                if last_seen != window.last_sent_txid {
                    // Ignore events that happened before the last time we
                    // changed the size or position of this window. Otherwise
                    // we would update the layout model incorrectly.
                    debug!(?last_seen, ?window.last_sent_txid, "Ignoring resize");
                    return;
                }
                if window.frame_last_read == new_frame {
                    return;
                }
                window.frame_last_read = new_frame;
                let Some(space) = self.space else { return };
                let Some(screen) = self.main_screen else { return };
                let response = self.layout.handle_event(LayoutEvent::WindowResized {
                    space,
                    wid,
                    new_frame,
                    screen: screen.frame,
                });
                self.handle_response(response);
                is_resize = true;
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
            Event::Command(Command::Layout(cmd)) => {
                let response = self.layout.handle_command(self.space.unwrap(), cmd);
                self.handle_response(response);
            }
            Event::Command(Command::Metrics(cmd)) => metrics::handle_command(cmd),
        }
        if self.main_window() != main_window_orig {
            let response = self.layout.handle_event(LayoutEvent::WindowRaised(
                self.space.unwrap(),
                self.main_window(),
            ));
            self.handle_response(response);
        }
        self.update_layout(animation_focus_wid, is_resize);
    }

    fn handle_response(&mut self, response: layout::EventResponse) {
        if let Some(wid) = response.raise_window {
            info!(raise_window = ?wid);
            self.raise_window(wid);
        }
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

    pub fn update_layout(&mut self, new_wid: Option<WindowId>, is_resize: bool) {
        let Some(main_screen) = self.main_screen else { return };
        if Some(main_screen.space) != self.space {
            return;
        };

        debug!(?main_screen);
        let main_window = self.main_window();
        debug!(?main_window);
        let layout = self.layout.calculate(self.space.unwrap(), main_screen.frame.clone());
        debug!(?layout, "Layout");

        info!(?layout, "New layout");

        let mut anim = Animation::new();
        for &(wid, target_frame) in &layout {
            let window = self.windows.get_mut(&wid).unwrap();
            let target_frame = target_frame.round();
            let current_frame = window.frame_last_written;
            if target_frame.same_as(current_frame) {
                // TODO: If there's been a read since this write that differs
                // from the written value, we should write again.
                continue;
            }
            debug!(?current_frame, ?target_frame, "Change");
            let handle = &self.apps.get(&wid.pid).unwrap().handle;
            let is_new = Some(wid) == new_wid;
            let txid = window.next_txid();
            anim.add_window(handle, wid, current_frame, target_frame, is_new, txid);
        }
        if is_resize {
            // If the user is doing something with the mouse we don't want to
            // animate on top of that.
            anim.skip_to_end();
        } else {
            anim.run();
        }

        for &(wid, target_frame) in &layout {
            self.windows.get_mut(&wid).unwrap().frame_last_written = target_frame;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::BTreeMap,
        sync::mpsc::{channel, Receiver, Sender},
    };

    use icrate::Foundation::CGSize;

    use super::*;
    use crate::app::Request;

    struct Apps(Sender<(Span, Request)>, Receiver<(Span, Request)>);
    impl Apps {
        fn new() -> Apps {
            let (tx, rx) = channel();
            Apps(tx, rx)
        }

        fn make_app(&mut self, pid: pid_t, windows: Vec<WindowInfo>) -> Event {
            self.make_app_with_opts(pid, windows, None, false)
        }

        fn make_app_with_opts(
            &mut self,
            pid: pid_t,
            windows: Vec<WindowInfo>,
            main_window: Option<WindowId>,
            is_frontmost: bool,
        ) -> Event {
            let handle = AppThreadHandle::new_for_test(self.0.clone());
            Event::ApplicationLaunched(
                pid,
                AppState {
                    info: AppInfo {
                        bundle_id: Some(format!("com.testapp{pid}")),
                        localized_name: Some(format!("TestApp{pid}")),
                    },
                    handle,
                    main_window,
                    is_frontmost,
                },
                (1..).map(|idx| WindowId::new(pid, idx)).zip(windows).collect(),
            )
        }

        fn requests(&mut self) -> Vec<Request> {
            self.1.try_iter().map(|(_span, rq)| rq).collect()
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
        let mut apps = Apps::new();
        let mut reactor = Reactor::new();
        reactor.handle_event(ScreenParametersChanged(
            vec![CGRect::ZERO],
            vec![SpaceId::new(1)],
        ));

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

        reactor.handle_event(ApplicationGloballyActivated(3));
        assert_eq!(None, reactor.frontmost_app);
        assert_eq!(None, reactor.main_window());

        reactor.handle_event(apps.make_app_with_opts(
            3,
            make_windows(2),
            Some(WindowId::new(3, 1)),
            true,
        ));
        assert_eq!(Some(3), reactor.frontmost_app);
        assert_eq!(Some(WindowId::new(3, 1)), reactor.main_window());
    }

    #[derive(Default, PartialEq, Debug)]
    struct WindowState {
        last_seen_txid: TransactionId,
        animating: bool,
        frame: CGRect,
    }

    fn simulate_events_for_requests(
        requests: Vec<Request>,
    ) -> (Vec<Event>, BTreeMap<WindowId, WindowState>) {
        let mut events = vec![];
        let mut windows: BTreeMap<WindowId, WindowState> = BTreeMap::new();

        for request in requests {
            match request {
                Request::SetWindowFrame(wid, frame, txid) => {
                    let window = windows.entry(wid).or_default();
                    window.last_seen_txid = txid;
                    let old_frame = window.frame;
                    if !window.animating && !old_frame.origin.same_as(frame.origin) {
                        events.push(Event::WindowMoved(wid, frame.origin, txid));
                    }
                    if !window.animating && !old_frame.size.same_as(frame.size) {
                        events.push(Event::WindowResized(wid, frame, txid));
                    }
                    window.frame = frame;
                }
                Request::SetWindowPos(wid, pos, txid) => {
                    let window = windows.entry(wid).or_default();
                    window.last_seen_txid = txid;
                    let old_frame = window.frame;
                    if !window.animating && !old_frame.origin.same_as(pos) {
                        events.push(Event::WindowMoved(wid, pos, txid));
                    }
                    window.frame.origin = pos;
                }
                Request::BeginWindowAnimation(wid) => {
                    windows.entry(wid).or_default().animating = true;
                }
                Request::EndWindowAnimation(wid) => {
                    let window = windows.entry(wid).or_default();
                    window.animating = false;
                    events.push(Event::WindowMoved(
                        wid,
                        window.frame.origin,
                        window.last_seen_txid,
                    ));
                    events.push(Event::WindowResized(
                        wid,
                        window.frame,
                        window.last_seen_txid,
                    ));
                }
                Request::Raise(_, _) => todo!(),
            }
        }

        (events, windows)
    }

    #[test]
    fn it_ignores_stale_resize_events() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new();
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![SpaceId::new(1)],
        ));

        reactor.handle_event(apps.make_app(1, make_windows(2)));
        let requests = apps.requests();
        assert!(!requests.is_empty());
        let (events_1, _) = simulate_events_for_requests(requests);

        reactor.handle_event(apps.make_app(2, make_windows(2)));
        assert!(!apps.requests().is_empty());

        for event in dbg!(events_1) {
            reactor.handle_event(event);
        }
        let requests = apps.requests();
        assert!(
            requests.is_empty(),
            "got requests when there should have been none: {requests:?}"
        );
    }

    #[test]
    fn it_sends_writes_when_stale_read_state_looks_same_as_written_state() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new();
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![SpaceId::new(1)],
        ));

        reactor.handle_event(apps.make_app(1, make_windows(2)));
        let (events_1, state_1) = simulate_events_for_requests(apps.requests());
        assert!(!state_1.is_empty());

        for event in events_1 {
            reactor.handle_event(event);
        }
        assert!(apps.requests().is_empty());

        reactor.handle_event(apps.make_app(2, make_windows(1)));
        let (_events_2, _state_2) = simulate_events_for_requests(apps.requests());

        reactor.handle_event(Event::WindowDestroyed(WindowId::new(2, 1)));
        let (_events_3, state_3) = simulate_events_for_requests(apps.requests());

        // These should be the same, because we should have resized the first
        // two windows both at the beginning, and at the end when the third
        // window was destroyed.
        for (wid, state) in state_1 {
            assert!(state_3.contains_key(&wid), "{wid:?} not in {state_3:#?}");
            assert_eq!(state.frame, state_3[&wid].frame);
        }
    }

    #[test]
    fn it_responds_to_resizes() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new();
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![SpaceId::new(1)],
        ));

        reactor.handle_event(apps.make_app(1, make_windows(3)));

        let (events, windows) = simulate_events_for_requests(apps.requests());
        for event in events {
            reactor.handle_event(event);
        }
        assert!(
            apps.requests().is_empty(),
            "reactor shouldn't react to unsurprising events"
        );

        // Resize the right edge of the middle window.
        let resizing = WindowId::new(1, 2);
        let window = &windows[&resizing];
        let frame = CGRect::new(
            window.frame.origin,
            CGSize::new(window.frame.size.width + 10., window.frame.size.height),
        );
        reactor.handle_event(Event::WindowResized(resizing, frame, window.last_seen_txid));

        // Expect the next window to be resized.
        let next = WindowId::new(1, 3);
        let old_frame = windows[&next].frame;
        let requests = apps.requests();
        assert!(!requests.is_empty());
        let (_events, windows) = simulate_events_for_requests(requests);
        assert_ne!(old_frame, windows[&next].frame);
    }
}
