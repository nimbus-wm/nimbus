//! The Reactor's job is to maintain coherence between the system and model state.
//!
//! It takes events from the rest of the system and builds a coherent picture of
//! what is going on. It shares this with the layout actor, and reacts to layout
//! changes by sending requests out to the other actors in the system.

mod animation;
mod main_window;
mod replay;

use std::{
    collections::{HashMap, HashSet},
    mem,
    path::PathBuf,
    sync, thread,
};

use animation::Animation;
use icrate::Foundation::CGRect;
use main_window::MainWindowTracker;
pub use replay::{replay, Record};
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use tokio::sync::oneshot;
use tracing::{debug, error, info, instrument, trace, warn, Span};

use crate::{
    actor::{
        app::{pid_t, AppInfo, AppThreadHandle, Quiet, RaiseToken, Request, WindowId, WindowInfo},
        layout::{self, LayoutCommand, LayoutEvent, LayoutManager},
    },
    metrics::{self, MetricsCommand},
    sys::{
        geometry::{CGRectDef, Round, SameAs},
        screen::SpaceId,
        window_server::{WindowServerId, WindowServerInfo},
    },
};

pub type Sender = std::sync::mpsc::Sender<(Span, Event)>;

#[serde_as]
#[derive(Serialize, Deserialize, Debug)]
pub enum Event {
    /// The screen layout, including resolution, changed. This is always the
    /// first event sent on startup.
    ///
    /// The first vec is the frame for each screen. The main screen is always
    /// first in the list.
    ///
    /// See the `SpaceChanged` event for an explanation of the other parameters.
    ScreenParametersChanged(
        #[serde_as(as = "Vec<CGRectDef>")] Vec<CGRect>,
        Vec<Option<SpaceId>>,
        Vec<WindowServerInfo>,
    ),

    /// The current space changed.
    ///
    /// There is one SpaceId per screen in the last ScreenParametersChanged
    /// event. `None` in the SpaceId vec disables managing windows on that
    /// screen until the next space change.
    ///
    /// A snapshot of visible windows from the window server is also taken and
    /// sent with this message. This allows us to determine more precisely which
    /// windows are visible on a given space, since app actor events like
    /// WindowsDiscovered are not ordered with respect to space events.
    SpaceChanged(Vec<Option<SpaceId>>, Vec<WindowServerInfo>),

    /// An application was launched. This event is also sent for every running
    /// application on startup.
    ///
    /// Both WindowInfo (accessibility) and WindowServerInfo are collected for
    /// any already-open windows when the launch event is sent. Since this
    /// event isn't ordered with respect to the Space events, it is possible to
    /// receive this event for a space we just switched off of.. FIXME. The same
    /// is true of WindowCreated events.
    ApplicationLaunched {
        pid: pid_t,
        info: AppInfo,
        #[serde(skip, default = "replay::deserialize_app_thread_handle")]
        handle: AppThreadHandle,
        is_frontmost: bool,
        main_window: Option<WindowId>,
        visible_windows: Vec<(WindowId, WindowInfo)>,
        window_server_info: Vec<WindowServerInfo>,
    },
    ApplicationTerminated(pid_t),
    ApplicationThreadTerminated(pid_t),
    ApplicationActivated(pid_t, Quiet),
    ApplicationDeactivated(pid_t),
    ApplicationGloballyActivated(pid_t),
    ApplicationGloballyDeactivated(pid_t),
    ApplicationMainWindowChanged(pid_t, Option<WindowId>, Quiet),

    WindowsDiscovered {
        pid: pid_t,
        new: Vec<(WindowId, WindowInfo)>,
        known_visible: Vec<WindowId>,
    },
    WindowCreated(WindowId, WindowInfo, Option<WindowServerInfo>),
    WindowDestroyed(WindowId),
    WindowFrameChanged(
        WindowId,
        #[serde(with = "CGRectDef")] CGRect,
        TransactionId,
        Requested,
    ),

    Command(Command),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Requested(pub bool);

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Command {
    Layout(LayoutCommand),
    Metrics(MetricsCommand),
    Debug,
    Serialize,
    SaveAndExit(PathBuf),
}

pub struct Reactor {
    apps: HashMap<pid_t, AppState>,
    layout: LayoutManager,
    windows: HashMap<WindowId, WindowState>,
    window_server_info: HashMap<WindowServerId, WindowServerInfo>,
    window_ids: HashMap<WindowServerId, WindowId>,
    visible_windows: HashSet<WindowServerId>,
    main_screen: Option<Screen>,
    raise_token: RaiseToken,
    main_window_tracker: MainWindowTracker,
}

#[derive(Debug)]
struct AppState {
    #[allow(unused)]
    pub info: AppInfo,
    pub handle: AppThreadHandle,
}

#[derive(Copy, Clone, Debug)]
struct Screen {
    frame: CGRect,
    space: Option<SpaceId>,
}

/// A per-window counter that tracks the last time the reactor sent a request to
/// change the window frame.
#[derive(Default, Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub struct TransactionId(u32);

#[derive(Debug)]
struct WindowState {
    #[allow(unused)]
    title: String,
    /// The last known frame of the window. Always includes the last write.
    ///
    /// This value only updates monotonically with respect to writes; in other
    /// words, we only accept reads when we know they come after the last write.
    frame_monotonic: CGRect,
    is_ax_standard: bool,
    last_sent_txid: TransactionId,
    window_server_id: Option<WindowServerId>,
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
            is_ax_standard: info.is_standard,
            last_sent_txid: TransactionId::default(),
            window_server_id: info.sys_id,
        }
    }
}

impl Reactor {
    pub fn spawn(layout: LayoutManager, mut record: Record) -> Sender {
        let (events_tx, events) = sync::mpsc::channel::<(Span, Event)>();
        thread::Builder::new()
            .name("reactor".to_string())
            .spawn(move || {
                let record = &mut record;
                record.start(&layout);
                let mut this = Reactor::new(layout);
                for (span, event) in events {
                    let _guard = span.enter();
                    record.on_event(&event);
                    this.handle_event(event);
                }
            })
            .unwrap();
        events_tx
    }

    fn new(layout: LayoutManager) -> Reactor {
        // FIXME: Remove apps that are no longer running from restored state.
        Reactor {
            apps: HashMap::new(),
            layout,
            windows: HashMap::new(),
            window_ids: HashMap::new(),
            window_server_info: HashMap::new(),
            visible_windows: HashSet::new(),
            main_screen: None,
            raise_token: RaiseToken::default(),
            main_window_tracker: MainWindowTracker::default(),
        }
    }

    pub fn handle_event(&mut self, event: Event) {
        debug!(?event, "Event");
        let mut animation_focus_wid = None;
        let mut is_resize = false;
        let raised_window = self.main_window_tracker.handle_event(&event);
        match event {
            Event::ApplicationLaunched {
                pid,
                info,
                handle,
                visible_windows,
                window_server_info,
                is_frontmost: _,
                main_window: _,
            } => {
                // FIXME: We don't get window server info for windows on app launch.
                self.apps.insert(pid, AppState { info, handle });
                self.update_partial_window_server_info(window_server_info);
                self.on_windows_discovered(pid, visible_windows, vec![]);
            }
            Event::ApplicationTerminated(pid) => {
                if let Some(app) = self.apps.get_mut(&pid) {
                    _ = app.handle.send(Request::Terminate);
                }
            }
            Event::ApplicationThreadTerminated(pid) => {
                self.apps.remove(&pid);
                self.send_layout_event(LayoutEvent::AppClosed(pid));
            }
            Event::ApplicationActivated(_, _)
            | Event::ApplicationDeactivated(_)
            | Event::ApplicationGloballyActivated(_)
            | Event::ApplicationGloballyDeactivated(_)
            | Event::ApplicationMainWindowChanged(_, _, _) => {
                // Handled by MainWindowTracker.
            }
            Event::WindowsDiscovered { pid, new, known_visible } => {
                self.on_windows_discovered(pid, new, known_visible);
            }
            Event::WindowCreated(wid, window, ws_info) => {
                // TODO: It's possible for a window to be on multiple spaces
                // or move spaces. (Add a test)
                // FIXME: We assume all windows are on the main screen.
                if let Some(wsid) = window.sys_id {
                    self.window_ids.insert(wsid, wid);
                }
                self.windows.insert(wid, window.into());
                if let Some(info) = ws_info {
                    self.window_server_info.insert(info.id, info);
                }
                if let Some(space) = self.main_screen_space() {
                    if self.window_is_standard(wid) {
                        animation_focus_wid = Some(wid);
                        self.send_layout_event(LayoutEvent::WindowAdded(space, wid));
                    }
                }
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
            Event::ScreenParametersChanged(frames, spaces, ws_info) => {
                info!("screen parameters changed");
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
                self.update_complete_window_server_info(ws_info);
                // FIXME: Update visible windows if space changed
            }
            Event::SpaceChanged(spaces, ws_info) => {
                info!("space changed");
                let Some(screen) = self.main_screen.as_mut() else {
                    return;
                };
                screen.space =
                    *spaces.first().expect("Spaces should be non-empty if there is a main screen");
                let Some(space) = self.main_screen_space() else {
                    // Either the space is disabled or there is no main screen.
                    return;
                };
                self.send_layout_event(LayoutEvent::SpaceExposed(
                    space,
                    self.main_screen.unwrap().frame.size,
                ));
                if let Some(main_window) = self.main_window() {
                    self.send_layout_event(LayoutEvent::WindowFocused(Some(space), main_window));
                }
                self.update_complete_window_server_info(ws_info);
                // TODO: Do this correctly/more optimally using CGWindowListCopyWindowInfo
                // (see notes for WindowsDiscovered above).
                for app in self.apps.values_mut() {
                    // Errors mean the app terminated (and a termination event
                    // is coming); ignore.
                    _ = app.handle.send(Request::GetVisibleWindows);
                }
            }
            Event::Command(Command::Layout(cmd)) => {
                info!(?cmd);
                let response = self.layout.handle_command(self.main_screen_space(), cmd);
                self.handle_layout_response(response);
            }
            Event::Command(Command::Metrics(cmd)) => metrics::handle_command(cmd),
            Event::Command(Command::Debug) => {
                if let Some(space) = self.main_screen_space() {
                    self.layout.debug_tree_desc(space, "", true);
                }
            }
            Event::Command(Command::Serialize) => {
                println!("{}", self.layout.serialize_to_string());
            }
            Event::Command(Command::SaveAndExit(path)) => match self.layout.save(path) {
                Ok(()) => std::process::exit(0),
                Err(e) => {
                    error!("Could not save layout: {e}");
                    std::process::exit(3);
                }
            },
        }
        if let Some(raised_window) = raised_window {
            self.send_layout_event(LayoutEvent::WindowFocused(
                self.main_screen_space(),
                raised_window,
            ));
        }
        self.update_layout(animation_focus_wid, is_resize);
    }

    fn update_complete_window_server_info(&mut self, ws_info: Vec<WindowServerInfo>) {
        self.visible_windows.clear();
        self.update_partial_window_server_info(ws_info);
    }

    fn update_partial_window_server_info(&mut self, ws_info: Vec<WindowServerInfo>) {
        self.visible_windows.extend(ws_info.iter().map(|info| info.id));
        for info in ws_info.iter().filter(|i| i.layer == 0) {
            let Some(wid) = self.window_ids.get(&info.id) else {
                continue;
            };
            let Some(window) = self.windows.get_mut(wid) else {
                continue;
            };
            // Assume this update comes from after the last write. The window
            // is on a different space (unless it's on all spaces) and
            // there's no way to order it with respect to our writes anyway.
            window.frame_monotonic = info.frame;
        }
        self.window_server_info.extend(ws_info.into_iter().map(|info| (info.id, info)));
    }

    fn on_windows_discovered(
        &mut self,
        pid: pid_t,
        new: Vec<(WindowId, WindowInfo)>,
        _known_visible: Vec<WindowId>,
    ) {
        // Note that we rely on the window server info, not accessibility, to
        // tell us which windows are visible.
        //
        // The accessibility APIs report that there are no visible windows when
        // at a login screen, for instance, but there is not a corresponding
        // system notification to use as context. Even if there were, lining
        // them up with the responses we get from the app would be unreliable.
        //
        // We therefore do not let accessibility `.windows()` results remove
        // known windows from the visible list. Doing so incorrectly would cause
        // us to destroy the layout. We do wait for windows to become initially
        // known to accesibility before adding them to the layout, but that is
        // not generally problematic.
        //
        // TODO: Notice when returning from the login screen and ask again for
        // undiscovered windows.
        self.window_ids
            .extend(new.iter().flat_map(|(wid, info)| info.sys_id.map(|wsid| (wsid, *wid))));
        self.windows.extend(new.into_iter().map(|(wid, info)| (wid, info.into())));
        let app_windows = self
            .visible_windows
            .iter()
            .flat_map(|wsid| self.window_ids.get(wsid))
            .copied()
            .filter(|wid| wid.pid == pid)
            .filter(|wid| self.window_is_standard(*wid))
            .collect();
        // FIXME: We assume all windows are on the main screen.
        if let Some(space) = self.main_screen_space() {
            // Filter out some noise.
            if self.windows.iter().any(|(wid, _)| wid.pid == pid) {
                self.send_layout_event(LayoutEvent::WindowsOnScreenUpdated(
                    space,
                    pid,
                    app_windows,
                ));
            }
        }
    }

    fn window_is_standard(&self, id: WindowId) -> bool {
        let Some(window) = self.windows.get(&id) else {
            return false;
        };
        if let Some(id) = window.window_server_id {
            if let Some(info) = self.window_server_info.get(&id) {
                if info.layer != 0 {
                    return false;
                }
            }
        }
        window.is_ax_standard
    }

    fn send_layout_event(&mut self, event: LayoutEvent) {
        let response = self.layout.handle_event(event);
        self.handle_layout_response(response);
        if let Some(space) = self.main_screen_space() {
            self.layout.debug_tree_desc(space, "after event", false);
        }
    }

    fn handle_layout_response(&mut self, response: layout::EventResponse) {
        let layout::EventResponse { raise_windows, focus_window } = response;
        for wid in raise_windows {
            info!(raise_window = ?wid);
            self.raise_window(wid, Quiet::Yes);
        }
        if let Some(wid) = focus_window {
            self.raise_window(wid, Quiet::No);
        }
    }

    fn raise_window(&mut self, wid: WindowId, quiet: Quiet) {
        self.raise_token.set_pid(wid.pid);
        let (tx, rx) = oneshot::channel();
        let Some(app) = self.apps.get_mut(&wid.pid) else { return };
        app.handle
            .send(Request::Raise(
                wid,
                self.raise_token.clone(),
                Some(tx),
                quiet,
            ))
            .unwrap();
        let _ = rx.blocking_recv();
    }

    /// The main window of the active app, if any.
    fn main_window(&self) -> Option<WindowId> {
        self.main_window_tracker.main_window()
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
            trace!(?wid, ?current_frame, ?target_frame);
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
pub mod tests {
    use std::{
        collections::BTreeMap,
        sync::mpsc::{channel, Receiver, Sender},
    };

    use icrate::Foundation::{CGPoint, CGSize};
    use test_log::test;

    use super::*;
    use crate::{
        actor::{app::Request, layout::LayoutManager},
        model::Direction,
        sys::window_server::WindowServerId,
    };

    impl Reactor {
        pub fn handle_events(&mut self, events: Vec<Event>) {
            for event in events {
                self.handle_event(event);
            }
        }
    }

    pub struct Apps(Sender<(Span, Request)>, Receiver<(Span, Request)>);
    impl Apps {
        pub fn new() -> Apps {
            let (tx, rx) = channel();
            Apps(tx, rx)
        }

        pub fn make_app(&mut self, pid: pid_t, windows: Vec<WindowInfo>) -> Vec<Event> {
            self.make_app_with_opts(pid, windows, None, false, true)
        }

        pub fn make_app_with_opts(
            &mut self,
            pid: pid_t,
            windows: Vec<WindowInfo>,
            main_window: Option<WindowId>,
            is_frontmost: bool,
            with_ws_info: bool,
        ) -> Vec<Event> {
            let handle = AppThreadHandle::new_for_test(self.0.clone());
            vec![Event::ApplicationLaunched {
                pid,
                info: AppInfo {
                    bundle_id: Some(format!("com.testapp{pid}")),
                    localized_name: Some(format!("TestApp{pid}")),
                },
                handle,
                is_frontmost,
                main_window,
                window_server_info: if with_ws_info {
                    windows
                        .iter()
                        .map(|info| WindowServerInfo {
                            pid,
                            id: info.sys_id.unwrap(),
                            layer: 0,
                            frame: info.frame,
                        })
                        .collect()
                } else {
                    Default::default()
                },
                visible_windows: (1..).map(|idx| WindowId::new(pid, idx)).zip(windows).collect(),
            }]
        }

        pub fn requests(&mut self) -> Vec<Request> {
            self.1.try_iter().map(|(_span, rq)| rq).collect()
        }

        pub fn simulate_until_quiet(&mut self, reactor: &mut Reactor) {
            let mut requests = self.requests();
            while !requests.is_empty() {
                for event in simulate_events_for_requests(requests).0 {
                    reactor.handle_event(event);
                }
                requests = self.requests();
            }
        }
    }

    pub fn make_window(idx: usize) -> WindowInfo {
        WindowInfo {
            is_standard: true,
            title: format!("Window{idx}"),
            frame: CGRect::new(
                CGPoint::new(100.0 * f64::from(idx as u32), 100.0),
                CGSize::new(50.0, 50.0),
            ),
            // TODO: This is wrong and conflicts with windows from other apps.
            sys_id: Some(WindowServerId::new(idx as u32)),
        }
    }

    pub fn make_windows(count: usize) -> Vec<WindowInfo> {
        (1..=count).map(make_window).collect()
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
                Request::Terminate => break,
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
                Request::Raise(_, _, _, _) => todo!(),
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
            vec![],
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
            vec![],
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
            vec![],
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
            vec![],
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
            vec![],
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
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(1)));

        let (_events, windows) = simulate_events_for_requests(apps.requests());
        assert!(
            windows.get(&WindowId::new(1, 1)).is_none(),
            "Window should not have been moved",
        );

        // Make sure it doesn't choke on destroyed events for ignored windows.
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 1)));
        reactor.handle_event(Event::WindowCreated(
            WindowId::new(1, 2),
            make_window(2),
            None,
        ));
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 2)));
    }

    #[test]
    fn it_ignores_windows_on_nonzero_layers() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new(LayoutManager::new());
        let full_screen = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![Some(SpaceId::new(1))],
            vec![WindowServerInfo {
                id: WindowServerId::new(1),
                pid: 1,
                layer: 10,
                frame: CGRect::ZERO,
            }],
        ));

        reactor.handle_events(apps.make_app_with_opts(1, make_windows(1), None, true, false));

        let (_events, windows) = simulate_events_for_requests(apps.requests());
        assert!(
            windows.get(&WindowId::new(1, 1)).is_none(),
            "Window should not have been moved",
        );

        // Make sure it doesn't choke on destroyed events for ignored windows.
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 1)));
        reactor.handle_event(Event::WindowCreated(
            WindowId::new(1, 2),
            make_window(2),
            None,
        ));
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 2)));
    }

    #[test]
    fn it_preserves_layout_after_login_screen() {
        // TODO: This would be better tested with a more complete simulation.
        let mut apps = Apps::new();
        let mut reactor = Reactor::new(LayoutManager::new());
        let space = SpaceId::new(1);
        let full_screen = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![Some(space)],
            vec![],
        ));

        reactor.handle_events(apps.make_app_with_opts(
            1,
            make_windows(3),
            Some(WindowId::new(1, 1)),
            true,
            true,
        ));
        reactor.handle_event(Event::ApplicationGloballyActivated(1));
        apps.simulate_until_quiet(&mut reactor);
        let default = reactor.layout.calculate_layout(space, full_screen);

        assert!(reactor.layout.selected_window(space).is_some());
        reactor.handle_event(Event::Command(Command::Layout(LayoutCommand::MoveNode(
            Direction::Up,
        ))));
        apps.simulate_until_quiet(&mut reactor);
        let modified = reactor.layout.calculate_layout(space, full_screen);
        assert_ne!(default, modified);

        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::ZERO],
            vec![None],
            vec![],
        ));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![Some(space)],
            (1..=3)
                .map(|n| WindowServerInfo {
                    pid: 1,
                    id: WindowServerId::new(n),
                    layer: 0,
                    frame: CGRect::ZERO,
                })
                .collect(),
        ));
        let requests = apps.requests();
        for request in requests {
            match request {
                Request::GetVisibleWindows => {
                    // Simulate the login screen condition: No windows are
                    // considered visible by the accessibility API, but they are
                    // from the window server API in the event above.
                    reactor.handle_event(Event::WindowsDiscovered {
                        pid: 1,
                        new: vec![],
                        known_visible: vec![],
                    });
                }
                req => {
                    let (events, _) = simulate_events_for_requests(vec![req]);
                    for event in events {
                        reactor.handle_event(event);
                    }
                }
            }
        }
        apps.simulate_until_quiet(&mut reactor);

        assert_eq!(
            reactor.layout.calculate_layout(space, full_screen),
            modified
        );
    }

    #[test]
    fn it_fixes_window_sizes_after_screen_config_changes() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new(LayoutManager::new());
        let full_screen = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![Some(SpaceId::new(1))],
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(1)));

        let (_events, windows) = simulate_events_for_requests(apps.requests());
        assert_eq!(
            full_screen,
            windows.get(&WindowId::new(1, 1)).expect("Window was not resized").frame,
        );

        // Simulate the system resizing a window after it recognizes an old
        // configurations. Resize events are not sent in this case.
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![
                full_screen,
                CGRect::new(CGPoint::new(1000., 0.), CGSize::new(1000., 1000.)),
            ],
            vec![Some(SpaceId::new(1)), None],
            vec![WindowServerInfo {
                id: WindowServerId::new(1),
                pid: 1,
                layer: 0,
                frame: CGRect::new(CGPoint::new(500., 0.), CGSize::new(500., 500.)),
            }],
        ));

        let (_events, windows) = simulate_events_for_requests(apps.requests());
        assert_eq!(
            full_screen,
            windows.get(&WindowId::new(1, 1)).expect("Window was not resized").frame,
        );
    }
}
