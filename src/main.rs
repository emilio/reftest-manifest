use std::path::Path;

mod parser;

fn main() {
    println!("{:#?}", parser::parse_manifest(Path::new("/home/emilio/src/moz/gecko-3/layout/reftests/reftest.list")).unwrap());

    println!("Hello, world!");
}
