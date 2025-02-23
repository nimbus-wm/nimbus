[![GHA Status]][GitHub Actions]

# Nimbus

Nimbus is a tiling window manager for macOS. It takes inspiration from window
managers like i3, Sway, and Hyprland.

#### Status

Nimbus is in early development and is not recommended for use.

## Quick start

Optional: Copy [nimbus.default.toml](./nimbus.default.toml) to
`$HOME/.nimbus.toml` and customize it to your needs.

```
git clone https://github.com/nimbus-wm/nimbus
cd nimbus
cargo run --release
```

Press Alt+Z to start managing the current space. Note: This will resize all your
windows!

## Save and restore

If you need to update Nimbus or restart it for any reason, exit with the
`save_and_exit` key binding (default Alt+Shift+E). Then, when starting again,
run it with the `--restore` flag:

```
cargo run --release -- --restore
```

Note that this does not work across machine restarts.

#### License and usage notes

Licensed under either of [Apache License, Version 2.0](LICENSE-APACHE) or
[MIT license](LICENSE-MIT) at your option.

[GitHub Actions]: https://github.com/nimbus-wm/nimbus/actions
[GHA Status]: https://github.com/nimbus-wm/nimbus/actions/workflows/rust.yml/badge.svg
