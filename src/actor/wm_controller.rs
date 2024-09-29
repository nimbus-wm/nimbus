//! The WM Controller handles major events like enabling and disabling the
//! window manager on certain spaces and launching app threads. It also
//! controls hotkey registration.

use std::{collections::HashSet, path::PathBuf};

use accessibility_sys::pid_t;
use tracing::{debug, instrument, Span};

pub type Sender = tokio::sync::mpsc::UnboundedSender<(Span, WmEvent)>;
type WeakSender = tokio::sync::mpsc::WeakUnboundedSender<(Span, WmEvent)>;
type Receiver = tokio::sync::mpsc::UnboundedReceiver<(Span, WmEvent)>;

use crate::{
    actor::{self, app::AppInfo, reactor},
    sys::{hotkey::HotkeyManager, screen::SpaceId, window_server::WindowServerInfo},
};

#[derive(Debug)]
pub enum WmEvent {
    AppEventsRegistered,
    AppLaunch(pid_t, AppInfo),
    ReactorEvent(reactor::Event),
    Command(WmCommand),
}

#[derive(Debug, Clone)]
pub enum WmCommand {
    ToggleSpaceActivated,
    ReactorCommand(reactor::Command),
}

pub struct Config {
    /// Only enables the WM on the starting space. On all other spaces, hotkeys are disabled.
    ///
    /// This can be useful for development.
    pub one_space: bool,
    /// Whether new spaces are disabled by default.
    pub default_disable: bool,
    pub restore_file: PathBuf,
}

pub struct WmController {
    config: Config,
    events_tx: reactor::Sender,
    receiver: Receiver,
    sender: WeakSender,
    starting_space: Option<SpaceId>,
    cur_space: Vec<Option<SpaceId>>,
    disabled_spaces: HashSet<SpaceId>,
    enabled_spaces: HashSet<SpaceId>,
    hotkeys: Option<HotkeyManager>,
}

impl WmController {
    pub fn new(config: Config, events_tx: reactor::Sender) -> (Self, Sender) {
        let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
        let this = Self {
            config,
            events_tx,
            receiver,
            sender: sender.downgrade(),
            starting_space: None,
            cur_space: Vec::new(),
            disabled_spaces: HashSet::new(),
            enabled_spaces: HashSet::new(),
            hotkeys: None,
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

        use self::{WmCommand::*, WmEvent::*};
        match event {
            AppEventsRegistered => {
                actor::app::spawn_initial_app_threads(self.events_tx.clone());
            }
            AppLaunch(pid, info) => {
                actor::app::spawn_app_thread(pid, info, self.events_tx.clone());
            }
            ReactorEvent(mut event) => {
                if let Event::SpaceChanged(spaces, _)
                | Event::ScreenParametersChanged(_, spaces, _) = &mut event
                {
                    self.handle_space_changed(spaces);
                    self.apply_space_activation(spaces);
                }
                self.send_event(event);
            }
            Command(ToggleSpaceActivated) => {
                let toggle_set = if self.config.default_disable {
                    &mut self.enabled_spaces
                } else {
                    &mut self.disabled_spaces
                };
                for space in &self.cur_space {
                    let Some(space) = space else { return };
                    if !toggle_set.remove(space) {
                        toggle_set.insert(*space);
                    }
                }
                let mut spaces = self.cur_space.clone();
                self.apply_space_activation(&mut spaces);
                self.send_event(Event::SpaceChanged(spaces, self.get_windows()));
            }
            Command(ReactorCommand(cmd)) => {
                self.send_event(Event::Command(cmd));
            }
        }
    }

    fn get_windows(&self) -> Vec<WindowServerInfo> {
        // TODO: This probably shouldn't happen here.
        // We only do it because we manufacture a SpaceChanged event, and the
        // reactor might need an accurate list of visible windows.
        #[cfg(not(test))]
        return crate::sys::window_server::get_visible_windows_with_layer(None);
        #[cfg(test)]
        vec![]
    }

    fn handle_space_changed(&mut self, spaces: &[Option<SpaceId>]) {
        self.cur_space = spaces.iter().copied().collect();
        let Some(&Some(space)) = spaces.first() else { return };
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

    fn apply_space_activation(&self, spaces: &mut [Option<SpaceId>]) {
        for space in spaces {
            let enabled = match space {
                Some(_) if self.config.one_space && *space != self.starting_space => false,
                Some(sp) if self.disabled_spaces.contains(sp) => false,
                Some(sp) if self.enabled_spaces.contains(sp) => true,
                _ if self.config.default_disable => false,
                _ => true,
            };
            if !enabled {
                *space = None;
            }
        }
    }

    fn send_event(&mut self, event: reactor::Event) {
        _ = self.events_tx.send((Span::current().clone(), event));
    }

    fn register_hotkeys(&mut self) {
        debug!("register_hotkeys");
        use actor::{layout::LayoutCommand::*, reactor::Command};
        use KeyCode::*;

        use crate::{
            metrics::MetricsCommand::*,
            model::{Direction::*, Orientation},
            sys::hotkey::{KeyCode, Modifiers},
        };
        const ALT: Modifiers = Modifiers::ALT;
        const SHIFT: Modifiers = Modifiers::SHIFT;

        let mgr = HotkeyManager::new(self.sender.upgrade().unwrap());
        //mgr.register(ALT, KeyS, Command::Layout(Shuffle));
        mgr.register(ALT, KeyA, Command::Layout(Ascend));
        mgr.register(ALT, KeyD, Command::Layout(Descend));
        mgr.register(ALT, KeyH, Command::Layout(MoveFocus(Left)));
        mgr.register(ALT, KeyJ, Command::Layout(MoveFocus(Down)));
        mgr.register(ALT, KeyK, Command::Layout(MoveFocus(Up)));
        mgr.register(ALT, KeyL, Command::Layout(MoveFocus(Right)));
        mgr.register(ALT | SHIFT, KeyH, Command::Layout(MoveNode(Left)));
        mgr.register(ALT | SHIFT, KeyJ, Command::Layout(MoveNode(Down)));
        mgr.register(ALT | SHIFT, KeyK, Command::Layout(MoveNode(Up)));
        mgr.register(ALT | SHIFT, KeyL, Command::Layout(MoveNode(Right)));
        mgr.register(ALT, Equal, Command::Layout(Split(Orientation::Vertical)));
        mgr.register(
            ALT,
            Backslash,
            Command::Layout(Split(Orientation::Horizontal)),
        );
        mgr.register(ALT, KeyS, Command::Layout(Group(Orientation::Vertical)));
        mgr.register(ALT, KeyT, Command::Layout(Group(Orientation::Horizontal)));
        mgr.register(ALT, KeyE, Command::Layout(Ungroup));
        mgr.register(ALT, Space, Command::Layout(ToggleFocusFloating));
        mgr.register(ALT | SHIFT, Space, Command::Layout(ToggleWindowFloating));
        mgr.register(ALT, KeyF, Command::Layout(ToggleFullscreen));

        mgr.register(ALT, KeyM, Command::Metrics(ShowTiming));
        mgr.register(ALT | SHIFT, KeyD, Command::Debug);
        mgr.register(ALT | SHIFT, KeyS, Command::Serialize);
        mgr.register(
            ALT | SHIFT,
            KeyE,
            Command::SaveAndExit(self.config.restore_file.clone()),
        );
        mgr.register_wm(ALT, KeyZ, WmCommand::ToggleSpaceActivated);

        self.hotkeys = Some(mgr);
    }

    fn unregister_hotkeys(&mut self) {
        debug!("unregister_hotkeys");
        self.hotkeys = None;
    }
}
