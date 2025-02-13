use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::bail;
use livesplit_hotkey::Hotkey;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

use crate::actor::wm_controller::WmCommand;

pub fn data_dir() -> PathBuf {
    dirs::home_dir().unwrap().join(".nimbus")
}

pub fn restore_file() -> PathBuf {
    data_dir().join("layout.ron")
}

pub fn config_file() -> PathBuf {
    dirs::home_dir().unwrap().join(".nimbus.toml")
}

#[derive(Serialize, Deserialize)]
struct ConfigFile {
    keys: FxHashMap<String, WmCommand>,
}

pub struct Config {
    pub keys: Vec<(Hotkey, WmCommand)>,
}

impl Config {
    pub fn read(path: &Path) -> anyhow::Result<Config> {
        let mut buf = String::new();
        File::open(path).unwrap().read_to_string(&mut buf)?;
        Self::parse(&buf)
    }

    pub fn default() -> Config {
        Self::parse(include_str!("../nimbus.default.toml")).unwrap()
    }

    fn parse(buf: &str) -> anyhow::Result<Config> {
        let c: ConfigFile = toml::from_str(&buf)?;
        let mut keys = Vec::new();
        for (key, cmd) in c.keys {
            let Ok(key) = Hotkey::from_str(&key) else {
                bail!("Could not parse hotkey: {key}");
            };
            keys.push((key, cmd));
        }
        Ok(Config { keys })
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn default_config_parses() {
        super::Config::default();
    }
}
