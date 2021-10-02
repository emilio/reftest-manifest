use std::path::Path;

mod parser;

fn main() {
    let manifest = parser::parse_manifest(Path::new("/home/emilio/src/moz/gecko-3/layout/reftests/reftest.list")).unwrap();
    // println!("{:#?}", manifest);
    let _ = manifest;

    println!("Hello, world!");
}
