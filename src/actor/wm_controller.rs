//! The WM Controller handles major events like enabling and disabling the
//! window manager on certain spaces and launching app threads. It also
//! controls hotkey registration.

use std::{path::PathBuf, sync::Arc};

use accessibility_sys::pid_t;
use serde::{Deserialize, Serialize};
use tracing::{debug, instrument, Span};

pub type Sender = tokio::sync::mpsc::UnboundedSender<(Span, WmEvent)>;
type WeakSender = tokio::sync::mpsc::WeakUnboundedSender<(Span, WmEvent)>;
type Receiver = tokio::sync::mpsc::UnboundedReceiver<(Span, WmEvent)>;

use crate::{
    actor::{self, app::AppInfo, reactor},
    collections::HashSet,
    sys::{event::HotkeyManager, screen::SpaceId, window_server::WindowServerInfo},
};

#[derive(Debug)]
pub enum WmEvent {
    AppEventsRegistered,
    AppLaunch(pid_t, AppInfo),
    ReactorEvent(reactor::Event),
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
            disabled_spaces: HashSet::default(),
            enabled_spaces: HashSet::default(),
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

        use self::{WmCmd::*, WmCommand::*, WmEvent::*};
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
            Command(Wm(ToggleSpaceActivated)) => {
                let toggle_set = if self.config.config.settings.default_disable {
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
                _ if self.config.config.settings.default_disable => false,
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
}
