//! The Reactor's job is to maintain coherence between the system and model state.
//!
//! It takes events from the rest of the system and builds a coherent picture of
//! what is going on. It shares this with the layout actor, and reacts to layout
//! changes by sending requests out to the other actors in the system.

mod animation;

use std::{collections::HashMap, mem, sync, thread};

use icrate::Foundation::CGRect;
use tracing::{debug, info, instrument, trace, warn, Span};

use crate::{
    actor::app::{pid_t, AppInfo, AppThreadHandle, RaiseToken, Request, WindowId, WindowInfo},
    actor::layout::{self, LayoutCommand, LayoutEvent, LayoutManager},
    metrics::{self, MetricsCommand},
    sys::geometry::{Round, SameAs},
    sys::screen::SpaceId,
};
use animation::Animation;

pub type Sender = std::sync::mpsc::Sender<(Span, Event)>;

#[derive(Debug)]
pub enum Event {
    ApplicationLaunched(pid_t, AppState),
    ApplicationTerminated(pid_t),
    ApplicationActivated(pid_t, Option<WindowId>),
    ApplicationGloballyActivated(pid_t),
    ApplicationGloballyDeactivated(pid_t),
    ApplicationDeactivated(pid_t),
    ApplicationMainWindowChanged(pid_t, Option<WindowId>),

    WindowsDiscovered {
        pid: pid_t,
        new: Vec<(WindowId, WindowInfo)>,
        known_visible: Vec<WindowId>,
    },
    WindowCreated(WindowId, WindowInfo),
    WindowDestroyed(WindowId),
    WindowFrameChanged(WindowId, CGRect, TransactionId, Requested),

    // None in the SpaceId vec disables managing windows on that screen until the next space change.
    ScreenParametersChanged(Vec<CGRect>, Vec<Option<SpaceId>>),
    SpaceChanged(Vec<Option<SpaceId>>),

    Command(Command),
}

#[derive(Debug)]
pub struct Requested(pub bool);

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
    space: Option<SpaceId>,
}

/// A per-window counter that tracks the last time the reactor sent a request to
/// change the window frame.
#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct TransactionId(u32);

#[derive(Debug)]
pub struct WindowState {
    #[allow(unused)]
    title: String,
    /// The last known frame of the window. Always includes the last write.
    ///
    /// This value only updates monotonically with respect to writes; in other
    /// words, we only accept reads when we know they come after the last write.
    frame_monotonic: CGRect,
    is_standard: bool,
    last_sent_txid: TransactionId,
}

impl WindowState {
    #[must_use]
    fn next_txid(&mut self) -> TransactionId {
        self.last_sent_txid.0 += 1;
        self.last_sent_txid
    }
}

impl From<WindowInfo> for WindowState {
    fn from(info: WindowInfo) -> Self {
        WindowState {
            title: info.title,
            frame_monotonic: info.frame,
            is_standard: info.is_standard,
            last_sent_txid: TransactionId::default(),
        }
    }
}

impl Reactor {
    pub fn spawn(layout: LayoutManager) -> Sender {
        let (events_tx, events) = sync::mpsc::channel::<(Span, Event)>();
        thread::spawn(move || {
            let mut this = Reactor::new(layout);
            for (span, event) in events {
                let _guard = span.enter();
                this.handle_event(event);
            }
        });
        events_tx
    }

    fn new(layout: LayoutManager) -> Reactor {
        // FIXME: Remove apps that are no longer running from restored state.
        Reactor {
            apps: HashMap::new(),
            layout,
            windows: HashMap::new(),
            main_screen: None,
            global_frontmost_app_pid: None,
            raise_token: RaiseToken::default(),
        }
    }

    fn handle_event(&mut self, event: Event) {
        debug!(?event, "Event");
        let main_window_orig = self.main_window();
        let mut animation_focus_wid = None;
        let mut is_resize = false;
        match event {
            Event::ApplicationLaunched(pid, state) => {
                self.apps.insert(pid, state);
            }
            Event::ApplicationTerminated(pid) => {
                // FIXME: This isn't ordered wrt other events from the app;
                // reroute the event through the app thread so it's the last
                // event for this app.
                self.apps.remove(&pid);
                self.send_layout_event(LayoutEvent::AppClosed(pid));
            }
            Event::ApplicationActivated(pid, main_window) => {
                let state = self.apps.get_mut(&pid).unwrap();
                state.is_frontmost = true;
                state.main_window = main_window;
            }
            Event::ApplicationGloballyActivated(pid) => {
                // See the comment in main_window() for the difference between
                // this and the ApplicationActivated event.
                self.global_frontmost_app_pid = Some(pid);
            }
            Event::ApplicationDeactivated(pid) => {
                self.apps.get_mut(&pid).unwrap().is_frontmost = false;
            }
            Event::ApplicationGloballyDeactivated(pid) => {
                if self.global_frontmost_app_pid == Some(pid) {
                    self.global_frontmost_app_pid = None;
                }
            }
            Event::ApplicationMainWindowChanged(pid, main_window) => {
                self.apps.get_mut(&pid).unwrap().main_window = main_window;
            }
            Event::WindowsDiscovered { pid, new, known_visible } => {
                // FIXME: There is no synchronization ensuring that these windows
                // are for the current space. The only way I've found to do that
                // is to take a "snapshot" using CGWindowListCopyWindowInfo.
                let mut app_windows = known_visible;
                app_windows.retain(|wid| self.windows[wid].is_standard);
                app_windows
                    .extend(new.iter().filter_map(|(wid, info)| info.is_standard.then_some(wid)));
                self.windows.extend(new.into_iter().map(|(wid, info)| (wid, info.into())));
                // FIXME: We assume all windows are on the main screen.
                if let Some(space) = self.main_screen_space() {
                    self.send_layout_event(LayoutEvent::WindowsOnScreenUpdated(
                        space,
                        pid,
                        app_windows,
                    ));
                }
            }
            Event::WindowCreated(wid, window) => {
                // TODO: It's possible for a window to be on multiple spaces
                // or move spaces. (Add a test)
                // FIXME: We assume all windows are on the main screen.
                if let Some(space) = self.main_screen_space() {
                    if window.is_standard {
                        animation_focus_wid = Some(wid);
                        self.send_layout_event(LayoutEvent::WindowAdded(space, wid));
                    }
                }
                self.windows.insert(wid, window.into());
            }
            Event::WindowDestroyed(wid) => {
                self.windows.remove(&wid).unwrap();
                //animation_focus_wid = self.window_order.last().cloned();
                self.send_layout_event(LayoutEvent::WindowRemoved(wid));
            }
            Event::WindowFrameChanged(wid, new_frame, last_seen, requested) => {
                let window = self.windows.get_mut(&wid).unwrap();
                if last_seen != window.last_sent_txid {
                    // Ignore events that happened before the last time we
                    // changed the size or position of this window. Otherwise
                    // we would update the layout model incorrectly.
                    debug!(?last_seen, ?window.last_sent_txid, "Ignoring resize");
                    return;
                }
                if requested.0 {
                    // TODO: If the size is different from requested, applying a
                    // correction to the model can result in weird feedback
                    // loops, so we ignore these for now.
                    return;
                }
                let old_frame = mem::replace(&mut window.frame_monotonic, new_frame);
                if old_frame == new_frame {
                    return;
                }
                let Some(screen) = self.main_screen else { return };
                let Some(space) = screen.space else { return };
                // This event is ignored if the window is not in the layout.
                self.send_layout_event(LayoutEvent::WindowResized {
                    space,
                    screen: screen.frame,
                    wid,
                    old_frame,
                    new_frame,
                });
                is_resize = true;
            }
            Event::ScreenParametersChanged(frames, spaces) => {
                self.main_screen = frames
                    .into_iter()
                    .zip(spaces)
                    .map(|(frame, space)| Screen { frame, space })
                    .next();
                if let Some(space) = self.main_screen_space() {
                    self.send_layout_event(LayoutEvent::SpaceExposed(
                        space,
                        self.main_screen.unwrap().frame.size,
                    ));
                }
                // FIXME: Update visible windows if space changed
            }
            Event::SpaceChanged(spaces) => {
                let Some(screen) = self.main_screen.as_mut() else {
                    return;
                };
                screen.space =
                    *spaces.first().expect("Spaces should be non-empty if there is a main screen");
                if let Some(space) = self.main_screen_space() {
                    self.send_layout_event(LayoutEvent::SpaceExposed(
                        space,
                        self.main_screen.unwrap().frame.size,
                    ));
                }
                if self.main_screen_space().is_some() {
                    // TODO: Do this correctly/more optimally using CGWindowListCopyWindowInfo
                    // (see notes for WindowsDiscovered above).
                    for app in self.apps.values_mut() {
                        // Errors mean the app terminated (and a termination event
                        // is coming); ignore.
                        _ = app.handle.send(Request::GetVisibleWindows);
                    }
                }
            }
            Event::Command(Command::Hello) => {
                println!("Hello, world!");
            }
            Event::Command(Command::Layout(cmd)) => {
                info!(?cmd);
                let Some(space) = self.main_screen_space() else { return };
                let response = self.layout.handle_command(space, cmd);
                self.handle_layout_response(response);
            }
            Event::Command(Command::Metrics(cmd)) => metrics::handle_command(cmd),
        }
        if self.main_window() != main_window_orig {
            // TODO: There's an edge case where the space updates and the main
            // window does not (because it is on multiple spaces). Update the
            // layout in that case too.
            if let Some(space) = self.main_screen_space() {
                self.send_layout_event(LayoutEvent::WindowRaised(space, self.main_window()));
            }
        }
        self.update_layout(animation_focus_wid, is_resize);
    }

    fn send_layout_event(&mut self, event: LayoutEvent) {
        let response = self.layout.handle_event(event);
        self.handle_layout_response(response)
    }

    fn handle_layout_response(&mut self, response: layout::EventResponse) {
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

    /// The main window of the active app, if any.
    fn main_window(&self) -> Option<WindowId> {
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
        let Some(pid) = self.global_frontmost_app_pid else {
            return None;
        };
        match self.apps.get(&pid) {
            Some(AppState {
                is_frontmost: true,
                main_window: Some(window),
                ..
            }) if self.windows.contains_key(window) => Some(*window),
            _ => None,
        }
    }

    fn main_screen_space(&self) -> Option<SpaceId> {
        self.main_screen?.space
    }

    #[instrument(skip(self), fields(?self.main_screen))]
    pub fn update_layout(&mut self, new_wid: Option<WindowId>, is_resize: bool) {
        let Some(main_screen) = self.main_screen else { return };
        let Some(space) = main_screen.space else { return };

        trace!(?main_screen);
        let main_window = self.main_window();
        trace!(?main_window);
        let layout = self.layout.calculate_layout(space, main_screen.frame.clone());
        trace!(?layout, "Layout");

        let mut anim = Animation::new();
        for &(wid, target_frame) in &layout {
            let Some(window) = self.windows.get_mut(&wid) else {
                // If we restored a saved state the window may not be available yet.
                continue;
            };
            let target_frame = target_frame.round();
            let current_frame = window.frame_monotonic;
            if target_frame.same_as(current_frame) {
                continue;
            }
            info!(?wid, ?current_frame, ?target_frame);
            let handle = &self.apps.get(&wid.pid).unwrap().handle;
            let is_new = Some(wid) == new_wid;
            let txid = window.next_txid();
            anim.add_window(handle, wid, current_frame, target_frame, is_new, txid);
            window.frame_monotonic = target_frame;
        }
        if is_resize {
            // If the user is doing something with the mouse we don't want to
            // animate on top of that.
            anim.skip_to_end();
        } else {
            anim.run();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::BTreeMap,
        sync::mpsc::{channel, Receiver, Sender},
    };

    use icrate::Foundation::{CGPoint, CGSize};

    use super::*;
    use crate::{
        actor::{app::Request, layout::LayoutManager},
        sys::window_server::WindowServerId,
    };

    impl Reactor {
        fn handle_events(&mut self, events: Vec<Event>) {
            for event in events {
                self.handle_event(event);
            }
        }
    }

    struct Apps(Sender<(Span, Request)>, Receiver<(Span, Request)>);
    impl Apps {
        fn new() -> Apps {
            let (tx, rx) = channel();
            Apps(tx, rx)
        }

        fn make_app(&mut self, pid: pid_t, windows: Vec<WindowInfo>) -> Vec<Event> {
            self.make_app_with_opts(pid, windows, None, false)
        }

        fn make_app_with_opts(
            &mut self,
            pid: pid_t,
            windows: Vec<WindowInfo>,
            main_window: Option<WindowId>,
            is_frontmost: bool,
        ) -> Vec<Event> {
            let handle = AppThreadHandle::new_for_test(self.0.clone());
            vec![
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
                ),
                Event::WindowsDiscovered {
                    pid,
                    new: (1..).map(|idx| WindowId::new(pid, idx)).zip(windows).collect(),
                    known_visible: vec![],
                },
            ]
        }

        fn requests(&mut self) -> Vec<Request> {
            self.1.try_iter().map(|(_span, rq)| rq).collect()
        }
    }

    fn make_window(idx: usize) -> WindowInfo {
        WindowInfo {
            is_standard: true,
            title: format!("Window{idx}"),
            frame: CGRect::new(
                CGPoint::new(100.0 * f64::from(idx as u32), 100.0),
                CGSize::new(50.0, 50.0),
            ),
            sys_id: WindowServerId::new(0),
        }
    }

    fn make_windows(count: usize) -> Vec<WindowInfo> {
        (1..=count).map(make_window).collect()
    }

    #[test]
    fn it_tracks_frontmost_app_and_main_window_correctly() {
        use Event::*;
        let mut apps = Apps::new();
        let mut reactor = Reactor::new(LayoutManager::new());
        let space = SpaceId::new(1);
        reactor.handle_event(ScreenParametersChanged(
            vec![CGRect::ZERO],
            vec![Some(space)],
        ));
        assert_eq!(None, reactor.main_window());

        reactor.handle_event(ApplicationGloballyActivated(1));
        reactor.handle_events(apps.make_app_with_opts(
            1,
            make_windows(2),
            Some(WindowId::new(1, 1)),
            true,
        ));
        reactor.handle_events(apps.make_app(2, make_windows(2)));
        assert_eq!(Some(WindowId::new(1, 1)), reactor.main_window());
        assert_eq!(
            reactor.layout.selected_window(space),
            Some(WindowId::new(1, 1))
        );
        reactor.handle_event(ApplicationGloballyDeactivated(1));
        assert_eq!(None, reactor.main_window());
        reactor.handle_event(ApplicationActivated(2, None));
        reactor.handle_event(ApplicationGloballyActivated(2));
        assert_eq!(None, reactor.main_window());
        reactor.handle_event(ApplicationMainWindowChanged(2, Some(WindowId::new(2, 2))));
        assert_eq!(Some(WindowId::new(2, 2)), reactor.main_window());
        assert_eq!(
            reactor.layout.selected_window(space),
            Some(WindowId::new(2, 2))
        );
        reactor.handle_event(ApplicationMainWindowChanged(1, Some(WindowId::new(1, 2))));
        assert_eq!(Some(WindowId::new(2, 2)), reactor.main_window());
        reactor.handle_event(ApplicationDeactivated(1));
        assert_eq!(Some(WindowId::new(2, 2)), reactor.main_window());
        reactor.handle_event(ApplicationDeactivated(2));
        assert_eq!(None, reactor.main_window());

        reactor.handle_event(ApplicationGloballyActivated(3));
        assert_eq!(None, reactor.main_window());

        reactor.handle_events(apps.make_app_with_opts(
            3,
            make_windows(2),
            Some(WindowId::new(3, 1)),
            true,
        ));
        assert_eq!(Some(WindowId::new(3, 1)), reactor.main_window());
        assert_eq!(
            reactor.layout.selected_window(space),
            Some(WindowId::new(3, 1))
        );
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
                Request::GetVisibleWindows => {}
                Request::SetWindowFrame(wid, frame, txid) => {
                    let window = windows.entry(wid).or_default();
                    window.last_seen_txid = txid;
                    let old_frame = window.frame;
                    window.frame = frame;
                    if !window.animating && !old_frame.same_as(frame) {
                        events.push(Event::WindowFrameChanged(wid, frame, txid, Requested(true)));
                    }
                }
                Request::SetWindowPos(wid, pos, txid) => {
                    let window = windows.entry(wid).or_default();
                    window.last_seen_txid = txid;
                    let old_frame = window.frame;
                    window.frame.origin = pos;
                    if !window.animating && !old_frame.same_as(window.frame) {
                        events.push(Event::WindowFrameChanged(
                            wid,
                            window.frame,
                            txid,
                            Requested(true),
                        ));
                    }
                }
                Request::BeginWindowAnimation(wid) => {
                    windows.entry(wid).or_default().animating = true;
                }
                Request::EndWindowAnimation(wid) => {
                    let window = windows.entry(wid).or_default();
                    window.animating = false;
                    events.push(Event::WindowFrameChanged(
                        wid,
                        window.frame,
                        window.last_seen_txid,
                        Requested(true),
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
        let mut reactor = Reactor::new(LayoutManager::new());
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![Some(SpaceId::new(1))],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(2)));
        let requests = apps.requests();
        assert!(!requests.is_empty());
        let (events_1, _) = simulate_events_for_requests(requests);

        reactor.handle_events(apps.make_app(2, make_windows(2)));
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
        let mut reactor = Reactor::new(LayoutManager::new());
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![Some(SpaceId::new(1))],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(2)));
        let (events_1, state_1) = simulate_events_for_requests(apps.requests());
        assert!(!state_1.is_empty());

        for event in events_1 {
            reactor.handle_event(event);
        }
        assert!(apps.requests().is_empty());

        reactor.handle_events(apps.make_app(2, make_windows(1)));
        let (_events_2, _state_2) = simulate_events_for_requests(apps.requests());
        dbg!(_state_2);

        reactor.handle_event(Event::WindowDestroyed(WindowId::new(2, 1)));
        let (_events_3, state_3) = simulate_events_for_requests(apps.requests());

        // These should be the same, because we should have resized the first
        // two windows both at the beginning, and at the end when the third
        // window was destroyed.
        for (wid, state) in dbg!(state_1) {
            assert!(state_3.contains_key(&wid), "{wid:?} not in {state_3:#?}");
            assert_eq!(state.frame, state_3[&wid].frame);
        }
    }

    #[test]
    fn sends_writes_same_as_last_written_state_if_changed_externally() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new(LayoutManager::new());
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![Some(SpaceId::new(1))],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(2)));
        let (events_1, state_1) = simulate_events_for_requests(apps.requests());
        assert!(!state_1.is_empty());

        for event in events_1 {
            reactor.handle_event(event);
        }
        assert!(apps.requests().is_empty());

        // Move a window in an invalid way.
        let wid = WindowId::new(1, 1);
        let old_frame = state_1[&wid].frame;
        reactor.handle_event(Event::WindowFrameChanged(
            wid,
            CGRect::new(
                CGPoint::new(old_frame.origin.x, old_frame.origin.y + 10.),
                old_frame.size,
            ),
            state_1[&wid].last_seen_txid,
            Requested(false),
        ));

        let requests = apps.requests();
        assert!(!requests.is_empty());
        let (_events_2, state_2) = simulate_events_for_requests(requests);
        assert_eq!(state_2[&wid].frame, old_frame);
    }

    #[test]
    fn it_responds_to_resizes() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new(LayoutManager::new());
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![Some(SpaceId::new(1))],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(3)));

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
        reactor.handle_event(Event::WindowFrameChanged(
            resizing,
            frame,
            window.last_seen_txid,
            Requested(false),
        ));

        // Expect the next window to be resized.
        let next = WindowId::new(1, 3);
        let old_frame = windows[&next].frame;
        let requests = apps.requests();
        assert!(!requests.is_empty());
        let (_events, windows) = simulate_events_for_requests(requests);
        assert_ne!(old_frame, windows[&next].frame);
    }

    #[test]
    fn it_manages_windows_on_enabled_spaces() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new(LayoutManager::new());
        let full_screen = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![Some(SpaceId::new(1))],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(1)));

        let (_events, windows) = simulate_events_for_requests(apps.requests());
        assert_eq!(
            full_screen,
            windows.get(&WindowId::new(1, 1)).expect("Window was not resized").frame,
        );
    }

    #[test]
    fn it_ignores_windows_on_disabled_spaces() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new(LayoutManager::new());
        let full_screen = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![None],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(1)));

        let (_events, windows) = simulate_events_for_requests(apps.requests());
        assert!(
            windows.get(&WindowId::new(1, 1)).is_none(),
            "Window should not have been moved",
        );

        // Make sure it doesn't choke on destroyed events for ignored windows.
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 1)));
        reactor.handle_event(Event::WindowCreated(WindowId::new(1, 2), make_window(2)));
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 2)));
    }
}
