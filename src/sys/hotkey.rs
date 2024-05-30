use livesplit_hotkey::{ConsumePreference, Hook};
pub use livesplit_hotkey::{Hotkey, KeyCode, Modifiers};
use tracing::info_span;

use crate::actor::{
    reactor::Command,
    wm_controller::{Sender, WmCommand, WmEvent},
};

pub struct HotkeyManager {
    hook: Hook,
    events_tx: Sender,
}

impl HotkeyManager {
    pub fn new(events_tx: Sender) -> Self {
        let hook = Hook::with_consume_preference(ConsumePreference::MustConsume).unwrap();
        HotkeyManager { hook, events_tx }
    }

    pub fn register(&self, modifiers: Modifiers, key_code: KeyCode, cmd: Command) {
        self.register_wm(modifiers, key_code, WmCommand::ReactorCommand(cmd))
    }

    pub fn register_wm(&self, modifiers: Modifiers, key_code: KeyCode, cmd: WmCommand) {
        let events_tx = self.events_tx.clone();
        self.hook
            .register(Hotkey { modifiers, key_code }, move || {
                let span = info_span!("hotkey::press", ?key_code);
                events_tx.send((span, WmEvent::Command(cmd.clone()))).unwrap()
            })
            .unwrap();
    }
}
