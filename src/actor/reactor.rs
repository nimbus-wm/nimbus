//! The Reactor's job is to maintain coherence between the system and model state.
//!
//! It takes events from the rest of the system and builds a coherent picture of
//! what is going on. It shares this with the layout actor, and reacts to layout
//! changes by sending requests out to the other actors in the system.

mod animation;
mod main_window;
mod replay;

#[cfg(test)]
mod testing;

use std::collections::BTreeMap;
use std::sync::Arc;
use std::{mem, thread};

use animation::Animation;
use main_window::MainWindowTracker;
use objc2_core_foundation::{CGPoint, CGRect};
pub use replay::{Record, replay};
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use tokio::sync::mpsc::unbounded_channel;
use tracing::{Span, debug, error, info, instrument, trace, warn};

use super::mouse;
use crate::actor::app::{AppInfo, AppThreadHandle, Quiet, Request, WindowId, WindowInfo, pid_t};
use crate::actor::layout::{self, LayoutCommand, LayoutEvent, LayoutManager};

use tokio::sync::mpsc;

use crate::actor::raise::{self, RaiseRequest};
use crate::collections::{HashMap, HashSet};
use crate::config::Config;
use crate::log::{self, MetricsCommand};
use crate::sys::event::MouseState;
use crate::sys::executor::Executor;
use crate::sys::geometry::{CGRectDef, CGRectExt, Round, SameAs};
use crate::sys::screen::SpaceId;
use crate::sys::window_server::{WindowServerId, WindowServerInfo};

pub type Sender = tokio::sync::mpsc::UnboundedSender<(Span, Event)>;
type Receiver = tokio::sync::mpsc::UnboundedReceiver<(Span, Event)>;

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
    WindowCreated(WindowId, WindowInfo, Option<WindowServerInfo>, MouseState),
    WindowDestroyed(WindowId),
    WindowFrameChanged(
        WindowId,
        #[serde(with = "CGRectDef")] CGRect,
        TransactionId,
        Requested,
        Option<MouseState>,
    ),

    /// Left mouse button was released.
    ///
    /// Layout changes are suppressed while the button is down so that they
    /// don't interfere with drags. This event is used to update the layout in
    /// case updates were supressed while the button was down.
    ///
    /// FIXME: This can be interleaved incorrectly with the MouseState in app
    /// actor events.
    MouseUp,
    /// The mouse cursor moved over a new window. Only sent if focus-follows-
    /// mouse is enabled.
    MouseMovedOverWindow(WindowServerId),

    /// A raise request completed. Used by the raise manager to track when
    /// all raise requests in a sequence have finished.
    RaiseCompleted {
        window_id: WindowId,
        sequence_id: u64,
    },

    /// A raise sequence timed out. Used by the raise manager to clean up
    /// pending raises that took too long.
    RaiseTimeout {
        sequence_id: u64,
    },

    Command(Command),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Requested(pub bool);

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum Command {
    Layout(LayoutCommand),
    Metrics(MetricsCommand),
    Reactor(ReactorCommand),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
pub enum ReactorCommand {
    Debug,
    Serialize,
    SaveAndExit,
}

use crate::actor::raise::RaiseManager;

pub struct Reactor {
    config: Arc<Config>,
    apps: HashMap<pid_t, AppState>,
    layout: LayoutManager,
    windows: HashMap<WindowId, WindowState>,
    window_server_info: HashMap<WindowServerId, WindowServerInfo>,
    window_ids: HashMap<WindowServerId, WindowId>,
    visible_windows: HashSet<WindowServerId>,
    screens: Vec<Screen>,
    main_window_tracker: MainWindowTracker,
    in_drag: bool,
    record: Record,
    mouse_tx: Option<mouse::Sender>,
    raise_manager_tx: raise::Sender,
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
    pub fn spawn(
        config: Arc<Config>,
        layout: LayoutManager,
        record: Record,
        mouse_tx: mouse::Sender,
    ) -> Sender {
        let (events_tx, events) = unbounded_channel();
        let events_tx_clone = events_tx.clone();
        thread::Builder::new()
            .name("reactor".to_string())
            .spawn(move || {
                let mut reactor = Reactor::new(config, layout, record);
                reactor.mouse_tx.replace(mouse_tx);
                Executor::run(reactor.run(events, events_tx_clone));
            })
            .unwrap();
        events_tx
    }

    pub fn new(config: Arc<Config>, layout: LayoutManager, mut record: Record) -> Reactor {
        // FIXME: Remove apps that are no longer running from restored state.
        record.start(&config, &layout);
        let (raise_manager_tx, _rx) = mpsc::unbounded_channel();
        Reactor {
            config,
            apps: HashMap::default(),
            layout,
            windows: HashMap::default(),
            window_ids: HashMap::default(),
            window_server_info: HashMap::default(),
            visible_windows: HashSet::default(),
            screens: vec![],
            main_window_tracker: MainWindowTracker::default(),
            in_drag: false,
            record,
            mouse_tx: None,
            raise_manager_tx,
        }
    }

    pub async fn run(mut self, events: Receiver, events_tx: Sender) {
        let (raise_manager_tx, raise_manager_rx) = mpsc::unbounded_channel();
        self.raise_manager_tx = raise_manager_tx.clone();

        let mouse_tx = self.mouse_tx.clone();
        let reactor_task = self.run_reactor_loop(events);
        let raise_manager_task = RaiseManager::run(raise_manager_rx, events_tx, mouse_tx);

        let _ = tokio::join!(reactor_task, raise_manager_task);
    }

    async fn run_reactor_loop(mut self, mut events: Receiver) {
        while let Some((span, event)) = events.recv().await {
            let _guard = span.enter();
            self.handle_event(event);
        }
    }

    fn log_event(&self, event: &Event) {
        match event {
            // Record more noisy events as trace logs instead of debug.
            Event::WindowFrameChanged(..) | Event::MouseUp => trace!(?event, "Event"),
            _ => debug!(?event, "Event"),
        }
    }

    fn handle_event(&mut self, event: Event) {
        self.record.on_event(&event);
        self.log_event(&event);
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
            Event::ApplicationActivated(..)
            | Event::ApplicationDeactivated(..)
            | Event::ApplicationGloballyActivated(..)
            | Event::ApplicationGloballyDeactivated(..)
            | Event::ApplicationMainWindowChanged(..) => {
                // Handled by MainWindowTracker.
            }
            Event::WindowsDiscovered { pid, new, known_visible } => {
                self.on_windows_discovered(pid, new, known_visible);
            }
            Event::WindowCreated(wid, window, ws_info, mouse_state) => {
                // TODO: It's possible for a window to be on multiple spaces
                // or move spaces. (Add a test)
                // FIXME: We assume all windows are on the main screen.
                if let Some(wsid) = window.sys_id {
                    self.window_ids.insert(wsid, wid);
                }
                let frame = window.frame;
                self.windows.insert(wid, window.into());
                if let Some(info) = ws_info {
                    self.window_server_info.insert(info.id, info);
                }
                if let Some(space) = self.best_space_for_window(&frame) {
                    if self.window_is_standard(wid) {
                        animation_focus_wid = Some(wid);
                        self.send_layout_event(LayoutEvent::WindowAdded(space, wid));
                    }
                }
                if mouse_state == MouseState::Down {
                    self.in_drag = true;
                    // Suppress updates while left button is pressed in case
                    // a drag is in progress.
                }
            }
            Event::WindowDestroyed(wid) => {
                self.windows.remove(&wid).unwrap();
                //animation_focus_wid = self.window_order.last().cloned();
                self.send_layout_event(LayoutEvent::WindowRemoved(wid));
            }
            Event::WindowFrameChanged(wid, new_frame, last_seen, requested, mouse_state) => {
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
                let screens = self
                    .screens
                    .iter()
                    .flat_map(|screen| Some((screen.space?, screen.frame)))
                    .collect::<Vec<_>>();
                // This event is ignored if the window is not in the layout.
                if old_frame.size != new_frame.size {
                    self.send_layout_event(LayoutEvent::WindowResized {
                        wid,
                        old_frame,
                        new_frame,
                        screens,
                    });
                    is_resize = true;
                } else if mouse_state == Some(MouseState::Down) {
                    self.in_drag = true;
                }
            }
            Event::ScreenParametersChanged(frames, spaces, ws_info) => {
                info!("screen parameters changed");
                self.screens = frames
                    .into_iter()
                    .zip(spaces)
                    .map(|(frame, space)| Screen { frame, space })
                    .collect();
                let screens = self.screens.clone();
                for screen in screens {
                    let Some(space) = screen.space else { continue };
                    self.send_layout_event(LayoutEvent::SpaceExposed(space, screen.frame.size));
                }
                self.update_complete_window_server_info(ws_info);
                // FIXME: Update visible windows if space changed
            }
            Event::SpaceChanged(spaces, ws_info) => {
                if spaces.len() != self.screens.len() {
                    warn!(
                        "Ignoring space change event: we have {} spaces, but {} screens",
                        spaces.len(),
                        self.screens.len()
                    );
                    return;
                }
                info!("space changed");
                for (space, screen) in spaces.iter().zip(&mut self.screens) {
                    screen.space = *space;
                }
                let screens = self.screens.clone();
                for screen in screens {
                    let Some(space) = screen.space else {
                        continue;
                    };
                    self.send_layout_event(LayoutEvent::SpaceExposed(space, screen.frame.size));
                }
                if let Some(main_window) = self.main_window() {
                    let spaces = spaces.iter().copied().flatten().collect();
                    self.send_layout_event(LayoutEvent::WindowFocused(spaces, main_window));
                }
                self.update_complete_window_server_info(ws_info);
                self.check_for_new_windows();
            }
            Event::MouseUp => {
                self.in_drag = false;
                // Now re-check the layout.
            }
            Event::MouseMovedOverWindow(wsid) => {
                let Some(&wid) = self.window_ids.get(&wsid) else { return };
                let Some(window) = self.windows.get(&wid) else { return };
                if self.best_space_for_window(&window.frame_monotonic).is_none() {
                    // The space is disabled.
                    return;
                }
                self.raise_window(wid, Quiet::No, None);
            }
            Event::RaiseCompleted { window_id, sequence_id } => {
                let msg = raise::Event::RaiseCompleted { window_id, sequence_id };
                _ = self.raise_manager_tx.send((Span::current(), msg));
            }
            Event::RaiseTimeout { sequence_id } => {
                let msg = raise::Event::RaiseTimeout { sequence_id };
                _ = self.raise_manager_tx.send((Span::current(), msg));
            }
            Event::Command(Command::Layout(cmd)) => {
                info!(?cmd);
                let visible_spaces =
                    self.screens.iter().flat_map(|screen| screen.space).collect::<Vec<_>>();
                let response =
                    self.layout.handle_command(self.main_window_space(), &visible_spaces, cmd);
                self.handle_layout_response(response);
            }
            Event::Command(Command::Metrics(cmd)) => log::handle_command(cmd),
            Event::Command(Command::Reactor(ReactorCommand::Debug)) => {
                for screen in &self.screens {
                    if let Some(space) = screen.space {
                        self.layout.debug_tree_desc(space, "", true);
                    }
                }
            }
            Event::Command(Command::Reactor(ReactorCommand::Serialize)) => {
                println!("{}", self.layout.serialize_to_string());
            }
            Event::Command(Command::Reactor(ReactorCommand::SaveAndExit)) => {
                match self.layout.save(crate::config::restore_file()) {
                    Ok(()) => std::process::exit(0),
                    Err(e) => {
                        error!("Could not save layout: {e}");
                        std::process::exit(3);
                    }
                }
            }
        }
        if let Some(raised_window) = raised_window {
            let spaces = self.screens.iter().flat_map(|screen| screen.space).collect();
            self.send_layout_event(LayoutEvent::WindowFocused(spaces, raised_window));
        }
        if !self.in_drag {
            self.update_layout(animation_focus_wid, is_resize);
        }
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

    fn check_for_new_windows(&mut self) {
        // TODO: Do this correctly/more optimally using CGWindowListCopyWindowInfo
        // (see notes for on_windows_discovered below).
        for app in self.apps.values_mut() {
            // Errors mean the app terminated (and a termination event
            // is coming); ignore.
            _ = app.handle.send(Request::GetVisibleWindows);
        }
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
        if !self.windows.iter().any(|(wid, _)| wid.pid == pid) {
            // Filter out log noise from NPCs.
            return;
        }
        let mut app_windows: BTreeMap<SpaceId, Vec<WindowId>> = BTreeMap::new();
        for wid in self
            .visible_windows
            .iter()
            .flat_map(|wsid| self.window_ids.get(wsid))
            .copied()
            .filter(|wid| wid.pid == pid)
            .filter(|wid| self.window_is_standard(*wid))
        {
            let Some(space) = self.best_space_for_window(&self.windows[&wid].frame_monotonic)
            else {
                continue;
            };
            app_windows.entry(space).or_default().push(wid);
        }
        let screens = self.screens.clone();
        for screen in screens {
            let Some(space) = screen.space else { continue };
            self.send_layout_event(LayoutEvent::WindowsOnScreenUpdated(
                space,
                pid,
                app_windows.remove(&space).unwrap_or_default(),
            ));
        }
        // If it's possible we just added the main window to the layout, make
        // sure the layout knows it's focused.
        if let Some(main_window) = self.main_window() {
            if main_window.pid == pid {
                let spaces = self.screens.iter().flat_map(|screen| screen.space).collect();
                self.send_layout_event(LayoutEvent::WindowFocused(spaces, main_window));
            }
        }
    }

    fn best_screen_for_window(&self, frame: &CGRect) -> Option<&Screen> {
        self.screens.iter().max_by_key(|s| s.frame.intersection(frame).area() as i64)
    }

    fn best_space_for_window(&self, frame: &CGRect) -> Option<SpaceId> {
        self.best_screen_for_window(frame)?.space
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
        for space in self.screens.iter().flat_map(|screen| screen.space) {
            self.layout.debug_tree_desc(space, "after event", false);
        }
    }

    fn handle_layout_response(&mut self, response: layout::EventResponse) {
        let layout::EventResponse { raise_windows, focus_window } = response;
        if raise_windows.is_empty() && focus_window.is_none() {
            return;
        }

        let mut app_handles = HashMap::default();
        for &wid in raise_windows.iter().chain(&focus_window) {
            if let Some(app) = self.apps.get(&wid.pid) {
                app_handles.insert(wid.pid, app.handle.clone());
            }
        }

        let mut windows_by_app_and_screen = HashMap::default();
        for &wid in &raise_windows {
            let Some(window) = self.windows.get(&wid) else { continue };
            windows_by_app_and_screen
                .entry((wid.pid, self.best_space_for_window(&window.frame_monotonic)))
                .or_insert(vec![])
                .push(wid);
        }

        let focus_window_with_warp = focus_window.map(|wid| {
            let warp = match self.config.settings.mouse_follows_focus {
                true => self.windows.get(&wid).map(|w| w.frame_monotonic.mid()),
                false => None,
            };
            (wid, warp)
        });

        let msg = raise::Event::RaiseRequest(RaiseRequest {
            raise_windows: windows_by_app_and_screen.into_values().collect(),
            focus_window: focus_window_with_warp,
            app_handles,
        });

        _ = self.raise_manager_tx.send((Span::current(), msg));
    }

    #[instrument(skip(self))]
    fn raise_window(&mut self, wid: WindowId, quiet: Quiet, warp: Option<CGPoint>) {
        let mut app_handles = HashMap::default();
        if let Some(app) = self.apps.get(&wid.pid) {
            app_handles.insert(wid.pid, app.handle.clone());
        }
        _ = self.raise_manager_tx.send((
            Span::current(),
            raise::Event::RaiseRequest(RaiseRequest {
                raise_windows: vec![],
                focus_window: Some((wid, warp)),
                app_handles: app_handles,
            }),
        ));
    }

    /// The main window of the active app, if any.
    fn main_window(&self) -> Option<WindowId> {
        self.main_window_tracker.main_window()
    }

    fn main_window_space(&self) -> Option<SpaceId> {
        // TODO: Optimize this with a cache or something.
        self.best_space_for_window(&self.windows.get(&self.main_window()?)?.frame_monotonic)
    }

    #[instrument(skip(self), fields())]
    pub fn update_layout(&mut self, new_wid: Option<WindowId>, is_resize: bool) {
        let screens = self.screens.clone();
        let mut anim = Animation::new();
        let main_window = self.main_window();
        trace!(?main_window);
        for screen in screens {
            let Some(space) = screen.space else { continue };
            trace!(?screen);
            let layout = self.layout.calculate_layout(space, screen.frame.clone());
            trace!(?layout, "Layout");

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
        }
        if is_resize || !self.config.settings.animate {
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
    use objc2_core_foundation::{CGPoint, CGSize};
    use test_log::test;

    use super::testing::*;
    use super::*;
    use crate::actor::app::Request;
    use crate::actor::layout::LayoutManager;
    use crate::model::Direction;
    use crate::sys::window_server::WindowServerId;

    #[test]
    fn it_ignores_stale_resize_events() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![Some(SpaceId::new(1))],
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(2)));
        let requests = apps.requests();
        assert!(!requests.is_empty());
        let events_1 = apps.simulate_events_for_requests(requests);

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
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![Some(SpaceId::new(1))],
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(2)));
        let events_1 = apps.simulate_events();
        let state_1 = apps.windows.clone();
        assert!(!state_1.is_empty());

        for event in events_1 {
            reactor.handle_event(event);
        }
        assert!(apps.requests().is_empty());

        reactor.handle_events(apps.make_app(2, make_windows(1)));
        let _events_2 = apps.simulate_events();

        reactor.handle_event(Event::WindowDestroyed(WindowId::new(2, 1)));
        let _events_3 = apps.simulate_events();
        let state_3 = apps.windows;

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
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![Some(SpaceId::new(1))],
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(2)));
        let events_1 = apps.simulate_events();
        let state_1 = apps.windows.clone();
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
            None,
        ));

        let requests = apps.requests();
        assert!(!requests.is_empty());
        let _events_2 = apps.simulate_events_for_requests(requests);
        assert_eq!(apps.windows[&wid].frame, old_frame);
    }

    #[test]
    fn it_responds_to_resizes() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.))],
            vec![Some(SpaceId::new(1))],
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(3)));

        let events = apps.simulate_events();
        let windows = apps.windows.clone();
        for event in events {
            reactor.handle_event(event);
        }
        assert!(
            apps.requests().is_empty(),
            "reactor shouldn't react to unsurprising events"
        );

        // Resize the right edge of the middle window.
        let resizing = WindowId::new(1, 2);
        let window = &apps.windows[&resizing];
        let frame = CGRect::new(
            window.frame.origin,
            CGSize::new(window.frame.size.width + 10., window.frame.size.height),
        );
        reactor.handle_event(Event::WindowFrameChanged(
            resizing,
            frame,
            window.last_seen_txid,
            Requested(false),
            None,
        ));

        // Expect the next window to be resized.
        let next = WindowId::new(1, 3);
        let old_frame = windows[&next].frame;
        let requests = apps.requests();
        assert!(!requests.is_empty());
        let _events = apps.simulate_events_for_requests(requests);
        assert_ne!(old_frame, apps.windows[&next].frame);
    }

    #[test]
    fn it_manages_windows_on_enabled_spaces() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        let full_screen = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![Some(SpaceId::new(1))],
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(1)));

        let _events = apps.simulate_events();
        assert_eq!(
            full_screen,
            apps.windows.get(&WindowId::new(1, 1)).expect("Window was not resized").frame,
        );
    }

    #[test]
    fn it_selects_the_main_window_on_space_enable() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        let full_screen = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        let ws_info = (1..=2)
            .map(|id| WindowServerInfo {
                id: WindowServerId::new(id),
                pid: 1,
                layer: 0,
                frame: CGRect::ZERO,
            })
            .collect::<Vec<_>>();
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![None],
            ws_info.clone(),
        ));

        reactor.handle_events(apps.make_app_with_opts(
            1,
            make_windows(2),
            Some(WindowId::new(1, 1)),
            true,
            true,
        ));
        reactor.handle_event(Event::ApplicationGloballyActivated(1));
        reactor.handle_events(apps.simulate_events());

        reactor.handle_event(Event::SpaceChanged(vec![Some(SpaceId::new(1))], ws_info));
        reactor.handle_events(apps.simulate_events());
        assert_eq!(
            reactor.layout.selected_window(SpaceId::new(1)),
            Some(WindowId::new(1, 1))
        );
    }

    #[test]
    fn it_ignores_windows_on_disabled_spaces() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        let full_screen = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![None],
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(1)));

        let state_before = apps.windows.clone();
        let _events = apps.simulate_events();
        assert_eq!(state_before, apps.windows, "Window should not have been moved",);

        // Make sure it doesn't choke on destroyed events for ignored windows.
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 1)));
        reactor.handle_event(Event::WindowCreated(
            WindowId::new(1, 2),
            make_window(2),
            None,
            MouseState::Up,
        ));
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 2)));
    }

    #[test]
    fn it_keeps_discovered_windows_on_their_initial_screen() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        let screen1 = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        let screen2 = CGRect::new(CGPoint::new(1000., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![screen1, screen2],
            vec![Some(SpaceId::new(1)), Some(SpaceId::new(2))],
            vec![],
        ));

        let mut windows = make_windows(2);
        windows[1].frame.origin = CGPoint::new(1100., 100.);
        reactor.handle_events(apps.make_app(1, windows));

        let _events = apps.simulate_events();
        assert_eq!(
            screen1,
            apps.windows.get(&WindowId::new(1, 1)).expect("Window was not resized").frame,
        );
        assert_eq!(
            screen2,
            apps.windows.get(&WindowId::new(1, 2)).expect("Window was not resized").frame,
        );
    }

    #[test]
    fn it_ignores_windows_on_nonzero_layers() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
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

        let state_before = apps.windows.clone();
        let _events = apps.simulate_events();
        assert_eq!(state_before, apps.windows, "Window should not have been moved",);

        // Make sure it doesn't choke on destroyed events for ignored windows.
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 1)));
        reactor.handle_event(Event::WindowCreated(
            WindowId::new(1, 2),
            make_window(2),
            None,
            MouseState::Up,
        ));
        reactor.handle_event(Event::WindowDestroyed(WindowId::new(1, 2)));
    }

    #[test]
    fn handle_layout_response_groups_windows_by_app_and_screen() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        let (raise_manager_tx, mut raise_manager_rx) = mpsc::unbounded_channel();
        reactor.raise_manager_tx = raise_manager_tx;

        let screen1 = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        let screen2 = CGRect::new(CGPoint::new(1000., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![screen1, screen2],
            vec![Some(SpaceId::new(1)), Some(SpaceId::new(2))],
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(2)));

        let mut windows = make_windows(2);
        windows[1].frame.origin = CGPoint::new(1100., 100.);
        reactor.handle_events(apps.make_app(2, windows));

        let _events = apps.simulate_events();
        while raise_manager_rx.try_recv().is_ok() {}

        reactor.handle_layout_response(layout::EventResponse {
            raise_windows: vec![
                WindowId::new(1, 1),
                WindowId::new(1, 2),
                WindowId::new(2, 1),
                WindowId::new(2, 2),
            ],
            focus_window: None,
        });
        let msg = raise_manager_rx.try_recv().expect("Should have sent an event").1;
        match msg {
            raise::Event::RaiseRequest(RaiseRequest {
                raise_windows,
                focus_window,
                app_handles: _,
            }) => {
                let raise_windows: HashSet<Vec<WindowId>> = raise_windows.into_iter().collect();
                let expected = [
                    vec![WindowId::new(1, 1), WindowId::new(1, 2)],
                    vec![WindowId::new(2, 1)],
                    vec![WindowId::new(2, 2)],
                ]
                .into_iter()
                .collect();
                assert_eq!(raise_windows, expected);
                assert!(focus_window.is_none());
            }
            _ => panic!("Unexpected event: {msg:?}"),
        }
    }

    #[test]
    fn handle_layout_response_includes_handles_for_raise_and_focus_windows() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        let (raise_manager_tx, mut raise_manager_rx) = mpsc::unbounded_channel();
        reactor.raise_manager_tx = raise_manager_tx;

        reactor.handle_events(apps.make_app(1, make_windows(1)));
        reactor.handle_events(apps.make_app(2, make_windows(1)));

        let _events = apps.simulate_events();
        while raise_manager_rx.try_recv().is_ok() {}
        reactor.handle_layout_response(layout::EventResponse {
            raise_windows: vec![WindowId::new(1, 1)],
            focus_window: Some(WindowId::new(2, 1)),
        });
        let msg = raise_manager_rx.try_recv().expect("Should have sent an event").1;
        match msg {
            raise::Event::RaiseRequest(RaiseRequest { app_handles, .. }) => {
                assert!(app_handles.contains_key(&1));
                assert!(app_handles.contains_key(&2));
            }
            _ => panic!("Unexpected event: {msg:?}"),
        }
    }

    #[test]
    fn it_preserves_layout_after_login_screen() {
        // TODO: This would be better tested with a more complete simulation.
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
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
                    let events = apps.simulate_events_for_requests(vec![req]);
                    for event in events {
                        reactor.handle_event(event);
                    }
                }
            }
        }
        apps.simulate_until_quiet(&mut reactor);

        assert_eq!(reactor.layout.calculate_layout(space, full_screen), modified);
    }

    #[test]
    fn it_fixes_window_sizes_after_screen_config_changes() {
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        let full_screen = CGRect::new(CGPoint::new(0., 0.), CGSize::new(1000., 1000.));
        reactor.handle_event(Event::ScreenParametersChanged(
            vec![full_screen],
            vec![Some(SpaceId::new(1))],
            vec![],
        ));

        reactor.handle_events(apps.make_app(1, make_windows(1)));

        let _events = apps.simulate_events();
        assert_eq!(
            full_screen,
            apps.windows.get(&WindowId::new(1, 1)).expect("Window was not resized").frame,
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

        let _events = apps.simulate_events();
        assert_eq!(
            full_screen,
            apps.windows.get(&WindowId::new(1, 1)).expect("Window was not resized").frame,
        );
    }

    #[test]
    fn it_doesnt_crash_after_main_window_closes() {
        use Direction::*;
        use Event::*;
        use LayoutCommand::*;

        use super::Command::*;
        use super::Reactor;
        let mut apps = Apps::new();
        let mut reactor = Reactor::new_for_test(LayoutManager::new());
        let space = SpaceId::new(1);
        reactor.handle_event(ScreenParametersChanged(
            vec![CGRect::ZERO],
            vec![Some(space)],
            vec![],
        ));
        assert_eq!(None, reactor.main_window());

        reactor.handle_event(ApplicationGloballyActivated(1));
        reactor.handle_events(apps.make_app_with_opts(
            1,
            make_windows(2),
            Some(WindowId::new(1, 1)),
            true,
            true,
        ));

        reactor.handle_event(WindowDestroyed(WindowId::new(1, 1)));
        reactor.handle_event(Command(Layout(MoveFocus(Left))));
    }
}
