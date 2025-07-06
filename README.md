<!-- GUIDE_EXCLUDE_START -->
[![GHA Status]][GitHub Actions]

# Glide

Glide is a tiling window manager for macOS. It takes inspiration from window
managers like i3, Sway, and Hyprland.

#### Status
<!-- GUIDE_EXCLUDE_END -->

Glide is in "beta" and is recommended for early adopters.

## Quick start with Cargo

First, [install Rust](https://rustup.rs) and make sure you have the latest Xcode command line tools installed.

Optional: Copy [glide.default.toml] to `$HOME/.glide.toml` and customize it to
your needs.

Then, run the following:

```
git clone https://github.com/glide-wm/glide
cd glide
cargo run --release
```

Press Alt+Z to start managing the current space. Note: This will resize all your
windows! See [glide.default.toml] for a list of key bindings.

[glide.default.toml]: ./glide.default.toml

## Save and restore

If you need to update Glide or restart it for any reason, exit with the
`save_and_exit` key binding (default Alt+Shift+E). Then, when starting again,
run it with the `--restore` flag:

```
cargo run --release -- --restore
```

Note that this does not work across machine restarts.

<!-- GUIDE_EXCLUDE_START -->
#### License and usage notes

Licensed under either of [Apache License, Version 2.0](LICENSE-APACHE) or
[MIT license](LICENSE-MIT) at your option.

[GitHub Actions]: https://github.com/glide-wm/glide/actions
[GHA Status]: https://github.com/glide-wm/glide/actions/workflows/rust.yml/badge.svg
<!-- GUIDE_EXCLUDE_END -->
