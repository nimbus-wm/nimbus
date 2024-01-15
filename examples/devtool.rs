//! This tool is used to exercise nimbus and system APIs during development.

use nimbus::space;

fn main() {
    let space = space::cur_space();
    println!("Current space: {space:?}");
}
