//! The WM Controller launches app threads and controls hotkey registration.

use std::{path::PathBuf, sync::Arc};

use accessibility_sys::pid_t;
use serde::{Deserialize, Serialize};
use tracing::{debug, instrument, Span};

pub type Sender = tokio::sync::mpsc::UnboundedSender<(Span, WmEvent)>;
type WeakSender = tokio::sync::mpsc::WeakUnboundedSender<(Span, WmEvent)>;
type Receiver = tokio::sync::mpsc::UnboundedReceiver<(Span, WmEvent)>;

use crate::{
    actor::{self, app::AppInfo, reactor},
    sys::{event::HotkeyManager, screen::SpaceId},
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
    ReactorCommand(reactor::Command),
}

pub struct Config {
    pub restore_file: PathBuf,
    pub config: Arc<crate::config::Config>,
}

pub struct WmController {
    config: Config,
    events_tx: reactor::Sender,
    receiver: Receiver,
    sender: WeakSender,
    starting_space: Option<SpaceId>,
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
            Command(ReactorCommand(cmd)) => {
                self.send_event(Event::Command(cmd));
            }
        }
    }

    fn handle_space_changed(&mut self, spaces: &[Option<SpaceId>]) {
        let Some(&Some(space)) = spaces.first() else { return };
        if self.starting_space.is_none() {
            self.starting_space = Some(space);
            self.register_hotkeys();
        } else if self.config.config.settings.starting_space_only {
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
                Some(_)
                    if self.config.config.settings.starting_space_only
                        && *space != self.starting_space =>
                {
                    false
                }
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
