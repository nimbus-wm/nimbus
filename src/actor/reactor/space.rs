use std::sync::Arc;

use crate::{collections::HashSet, config::Config, sys::screen::SpaceId};

pub struct SpaceManager {
    pub config: Arc<Config>,
    disabled_spaces: HashSet<SpaceId>,
    enabled_spaces: HashSet<SpaceId>,
    screen_spaces: Vec<Option<SpaceId>>,
}

impl SpaceManager {
    pub fn new(config: Arc<Config>) -> Self {
        SpaceManager {
            config,
            disabled_spaces: HashSet::default(),
            enabled_spaces: HashSet::default(),
            screen_spaces: Vec::new(),
        }
    }

    pub fn update_screen_spaces(
        &mut self,
        mut spaces: Vec<Option<SpaceId>>,
    ) -> Vec<Option<SpaceId>> {
        self.screen_spaces = spaces.clone();
        self.apply_space_activation(&mut spaces);
        spaces
    }

    pub fn toggle_space_activated(&mut self, screen_idx: usize) -> Option<SpaceId> {
        if self.config.settings.starting_space_only {
            return None;
        }
        let Some(space) = self.screen_spaces[screen_idx] else {
            return None;
        };
        let toggle_set = if self.config.settings.default_disable {
            &mut self.enabled_spaces
        } else {
            &mut self.disabled_spaces
        };
        if !toggle_set.remove(&space) {
            toggle_set.insert(space);
        }
        self.is_enabled(Some(space)).then_some(space)
    }

    fn apply_space_activation(&self, spaces: &mut [Option<SpaceId>]) {
        for space in spaces {
            if !self.is_enabled(*space) {
                *space = None;
            }
        }
    }

    fn is_enabled(&self, space: Option<SpaceId>) -> bool {
        match space {
            Some(sp) if self.disabled_spaces.contains(&sp) => false,
            Some(sp) if self.enabled_spaces.contains(&sp) => true,
            _ if self.config.settings.default_disable => false,
            _ => true,
        }
    }
}
