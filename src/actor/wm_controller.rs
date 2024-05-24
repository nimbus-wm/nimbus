//! The WM Controller handles major events like enabling and disabling the
//! window manager on certain spaces and launching app threads. It also
//! controls hotkey registration.

use std::path::PathBuf;

use accessibility_sys::pid_t;
use tracing::Span;

use crate::{
    actor::{self, app::AppInfo, reactor},
    sys::{hotkey::HotkeyManager, screen::SpaceId},
};

pub enum WmEvent {
    AppEventsRegistered,
    AppLaunch(pid_t, AppInfo),
    ReactorEvent(reactor::Event),
}

pub struct Config {
    pub one_space: bool,
    pub restore_file: PathBuf,
}

pub struct WmController {
    config: Config,
    events_tx: reactor::Sender,
    starting_space: Option<SpaceId>,
    hotkeys: Option<HotkeyManager>,
}

impl WmController {
    pub fn new(config: Config, events_tx: reactor::Sender) -> Self {
        Self {
            config,
            events_tx,
            starting_space: None,
            hotkeys: None,
        }
    }

    pub fn handle_event(&mut self, event: WmEvent) {
        use reactor::Event::*;
        use WmEvent::*;
        match event {
            AppEventsRegistered => actor::app::spawn_initial_app_threads(self.events_tx.clone()),
            AppLaunch(pid, info) => actor::app::spawn_app_thread(pid, info, self.events_tx.clone()),
            ReactorEvent(mut event) => {
                if let SpaceChanged(spaces) | ScreenParametersChanged(_, spaces) = &mut event {
                    self.handle_space_changed(spaces);
                    self.apply_space_activation(spaces);
                }
                self.send_event(event);
            }
        }
    }

    fn handle_space_changed(&mut self, spaces: &[Option<SpaceId>]) {
        if self.config.one_space {
            let Some(&Some(space)) = spaces.first() else { return };
            if self.starting_space.is_none() {
                self.starting_space = Some(space);
            }
            if Some(space) == self.starting_space {
                self.register_hotkeys();
            } else {
                self.unregister_hotkeys();
            }
        }
    }

    fn apply_space_activation(&self, spaces: &mut [Option<SpaceId>]) {
        if self.config.one_space {
            for space in spaces {
                if *space != self.starting_space {
                    *space = None;
                }
            }
        }
    }

    fn send_event(&mut self, event: reactor::Event) {
        _ = self.events_tx.send((Span::current().clone(), event));
    }

    fn register_hotkeys(&mut self) {
        use crate::metrics::MetricsCommand::*;
        use crate::model::Direction::*;
        use crate::model::Orientation;
        use crate::sys::hotkey::{KeyCode, Modifiers};
        use actor::layout::LayoutCommand::*;
        use actor::reactor::Command;

        use KeyCode::*;
        const ALT: Modifiers = Modifiers::ALT;
        const SHIFT: Modifiers = Modifiers::SHIFT;

        let mgr = HotkeyManager::new(self.events_tx.clone());
        mgr.register(ALT, KeyW, Command::Hello);
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
        mgr.register(ALT, KeyM, Command::Metrics(ShowTiming));
        mgr.register(ALT | SHIFT, KeyD, Command::Layout(Debug));
        mgr.register(ALT | SHIFT, KeyS, Command::Layout(Serialize));
        mgr.register(
            ALT | SHIFT,
            KeyE,
            Command::Layout(SaveAndExit(self.config.restore_file.clone())),
        );

        self.hotkeys = Some(mgr);
    }

    fn unregister_hotkeys(&mut self) {
        self.hotkeys = None;
    }
}
