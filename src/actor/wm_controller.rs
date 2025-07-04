//! The WM Controller handles major events like enabling and disabling the
//! window manager on certain spaces and launching app threads. It also
//! controls hotkey registration.

use std::path::PathBuf;
use std::sync::Arc;

use accessibility_sys::pid_t;
use objc2_app_kit::NSScreen;
use objc2_core_foundation::CGRect;
use objc2_foundation::MainThreadMarker;
use serde::{Deserialize, Serialize};
use tracing::{Span, debug, info, instrument, trace, warn};

pub type Sender = tokio::sync::mpsc::UnboundedSender<(Span, WmEvent)>;
type WeakSender = tokio::sync::mpsc::WeakUnboundedSender<(Span, WmEvent)>;
type Receiver = tokio::sync::mpsc::UnboundedReceiver<(Span, WmEvent)>;

use crate::actor::app::AppInfo;
use crate::actor::{self, mouse, reactor};
use crate::collections::HashSet;
use crate::sys;
use crate::sys::event::HotkeyManager;
use crate::sys::screen::{CoordinateConverter, NSScreenExt, ScreenId, SpaceId};
use crate::sys::window_server::WindowServerInfo;

#[derive(Debug)]
pub enum WmEvent {
    AppEventsRegistered,
    AppLaunch(pid_t, AppInfo),
    AppGloballyActivated(pid_t),
    AppGloballyDeactivated(pid_t),
    AppTerminated(pid_t),
    SpaceChanged(Vec<Option<SpaceId>>),
    ScreenParametersChanged(
        Vec<CGRect>,
        Vec<ScreenId>,
        CoordinateConverter,
        Vec<Option<SpaceId>>,
    ),
    Command(WmCommand),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum WmCommand {
    Wm(WmCmd),
    ReactorCommand(reactor::Command),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WmCmd {
    ToggleSpaceActivated,
}

pub struct Config {
    /// Only enables the WM on the starting space. On all other spaces, hotkeys are disabled.
    ///
    /// This can be useful for development.
    pub one_space: bool,
    pub restore_file: PathBuf,
    pub config: Arc<crate::config::Config>,
}

pub struct WmController {
    config: Config,
    events_tx: reactor::Sender,
    mouse_tx: mouse::Sender,
    receiver: Receiver,
    sender: WeakSender,
    starting_space: Option<SpaceId>,
    cur_space: Vec<Option<SpaceId>>,
    cur_screen_id: Vec<ScreenId>,
    disabled_spaces: HashSet<SpaceId>,
    enabled_spaces: HashSet<SpaceId>,
    login_window_pid: Option<pid_t>,
    login_window_active: bool,
    hotkeys: Option<HotkeyManager>,
    mtm: MainThreadMarker,
}

impl WmController {
    pub fn new(
        config: Config,
        events_tx: reactor::Sender,
        mouse_tx: mouse::Sender,
    ) -> (Self, Sender) {
        let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
        let this = Self {
            config,
            events_tx,
            mouse_tx,
            receiver,
            sender: sender.downgrade(),
            starting_space: None,
            cur_space: Vec::new(),
            cur_screen_id: Vec::new(),
            disabled_spaces: HashSet::default(),
            enabled_spaces: HashSet::default(),
            login_window_pid: None,
            login_window_active: false,
            hotkeys: None,
            mtm: MainThreadMarker::new().unwrap(),
        };
        (this, sender)
    }

    pub async fn run(mut self) {
        while let Some((span, event)) = self.receiver.recv().await {
            let _guard = span.enter();
            self.handle_event(event);
        }
    }

    #[instrument(skip(self))]
    pub fn handle_event(&mut self, event: WmEvent) {
        debug!("handle_event");
        use reactor::Event;

        use self::WmCmd::*;
        use self::WmCommand::*;
        use self::WmEvent::*;
        match event {
            AppEventsRegistered => {
                for (pid, info) in sys::app::running_apps(None) {
                    self.new_app(pid, info);
                }
            }
            AppLaunch(pid, info) => {
                self.new_app(pid, info);
            }
            AppGloballyActivated(pid) => {
                // Make sure the mouse cursor stays hidden after app switch.
                _ = self.mouse_tx.send((Span::current(), mouse::Request::EnforceHidden));
                if self.login_window_pid == Some(pid) {
                    // While the login screen is active AX APIs do not work.
                    // Disable all spaces to prevent errors.
                    info!("Login window activated");
                    self.login_window_active = true;
                    self.send_event(Event::SpaceChanged(self.active_spaces(), self.get_windows()));
                }
                self.send_event(Event::ApplicationGloballyActivated(pid));
            }
            AppGloballyDeactivated(pid) => {
                if self.login_window_pid == Some(pid) {
                    // Re-enable spaces; this also causes the reactor to update
                    // the set of visible windows on screen and their positions.
                    info!("Login window deactivated");
                    self.login_window_active = false;
                    self.send_event(Event::SpaceChanged(self.active_spaces(), self.get_windows()));
                }
                self.send_event(Event::ApplicationGloballyDeactivated(pid));
            }
            AppTerminated(pid) => {
                self.send_event(Event::ApplicationTerminated(pid));
            }
            ScreenParametersChanged(frames, ids, converter, spaces) => {
                self.cur_screen_id = ids;
                self.handle_space_changed(spaces);
                self.send_event(Event::ScreenParametersChanged(
                    frames.clone(),
                    self.active_spaces(),
                    self.get_windows(),
                ));
                _ = self.mouse_tx.send((
                    Span::current(),
                    mouse::Request::ScreenParametersChanged(frames, converter),
                ));
            }
            SpaceChanged(spaces) => {
                self.handle_space_changed(spaces);
                self.send_event(Event::SpaceChanged(self.active_spaces(), self.get_windows()));
            }
            Command(Wm(ToggleSpaceActivated)) => {
                let Some(space) = self.get_focused_space() else { return };
                let toggle_set = if self.config.config.settings.default_disable {
                    &mut self.enabled_spaces
                } else {
                    &mut self.disabled_spaces
                };
                if !toggle_set.remove(&space) {
                    toggle_set.insert(space);
                }
                self.send_event(Event::SpaceChanged(self.active_spaces(), self.get_windows()));
            }
            Command(ReactorCommand(cmd)) => {
                self.send_event(Event::Command(cmd));
            }
        }
    }

    fn new_app(&mut self, pid: pid_t, info: AppInfo) {
        if info.bundle_id.as_deref() == Some("com.apple.loginwindow") {
            if let Some(prev) = self.login_window_pid {
                warn!("Multiple loginwindow instances found: {prev:?} and {pid:?}");
            }
            self.login_window_pid = Some(pid);
        }
        actor::app::spawn_app_thread(pid, info, self.events_tx.clone());
    }

    fn get_focused_space(&self) -> Option<SpaceId> {
        // The currently focused screen is what NSScreen calls the "main" screen.
        let screen = NSScreen::mainScreen(self.mtm)?;
        let number = screen.get_number().ok()?;
        *self.cur_screen_id.iter().zip(&self.cur_space).find(|(id, _)| **id == number)?.1
    }

    fn handle_space_changed(&mut self, spaces: Vec<Option<SpaceId>>) {
        self.cur_space = spaces;
        let Some(&Some(space)) = self.cur_space.first() else {
            return;
        };
        if self.starting_space.is_none() {
            self.starting_space = Some(space);
            self.register_hotkeys();
        } else if self.config.one_space {
            if Some(space) == self.starting_space {
                self.register_hotkeys();
            } else {
                self.unregister_hotkeys();
            }
        }
    }

    fn active_spaces(&self) -> Vec<Option<SpaceId>> {
        let mut spaces = self.cur_space.clone();
        for space in &mut spaces {
            let enabled = match space {
                _ if self.login_window_active => false,
                Some(_) if self.config.one_space && *space != self.starting_space => false,
                Some(sp) if self.disabled_spaces.contains(sp) => false,
                Some(sp) if self.enabled_spaces.contains(sp) => true,
                _ if self.config.config.settings.default_disable => false,
                _ => true,
            };
            if !enabled {
                *space = None;
            }
        }
        spaces
    }

    fn send_event(&mut self, event: reactor::Event) {
        trace!(?event, "Sending event");
        _ = self.events_tx.send((Span::current().clone(), event));
    }

    fn register_hotkeys(&mut self) {
        debug!("register_hotkeys");
        let mgr = HotkeyManager::new(self.sender.upgrade().unwrap());
        for (key, cmd) in &self.config.config.keys {
            mgr.register_wm(key.modifiers, key.key_code, cmd.clone());
        }
        self.hotkeys = Some(mgr);
    }

    fn unregister_hotkeys(&mut self) {
        debug!("unregister_hotkeys");
        self.hotkeys = None;
    }

    fn get_windows(&self) -> Vec<WindowServerInfo> {
        #[cfg(not(test))]
        return sys::window_server::get_visible_windows_with_layer(None);
        #[cfg(test)]
        vec![]
    }
}
