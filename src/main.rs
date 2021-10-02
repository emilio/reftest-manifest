use std::path::Path;

mod parser;

fn simplify_manifest(path: &Path) {
    use std::io::BufRead;

    let file = std::fs::File::open(path).expect("Failed to open file");
    let mut lines = vec![];

    let reader = std::io::BufReader::new(file);
    let mut line_number = 0;
    let mut simplified = false;
    for line in reader.lines() {
        let line = line.expect("Error reading line");
        line_number += 1;
        match parser::parse_manifest_line(&line) {
            Ok(mut parsed_line) => {
                // TODO(emilio): There's probably a better way to do this.
                let old_entry = parsed_line.entry.clone();
                parsed_line.simplify();
                simplified = simplified || old_entry != parsed_line.entry;
                if let Some(ref entry) = parsed_line.entry {
                    if let parser::Entry::Include(_, ref nested_path) = entry {
                        let p = path.parent().unwrap().join(nested_path);
                        simplify_manifest(&p);
                    }
                }
                if old_entry != parsed_line.entry {
                    lines.push(parsed_line.serialize());
                } else {
                    lines.push(line);
                }
            }
            Err(error) => {
                panic!(
                    "Failed to parse {}:{}: {}",
                    path.display(),
                    line_number,
                    error
                );
            }
        }
    }
}


fn main() {
    let path = "/home/emilio/src/moz/gecko-5/layout/reftests/reftest.list";
    // let path = std::env::args().nth(1).expect("Expected a path to a reftest directory");
    simplify_manifest(Path::new(&path));
}
