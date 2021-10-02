use std::path::Path;

mod parser;

fn simplify_manifest(path: &Path) {
    use std::io::{BufRead, Write};

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
                let simplified_this = old_entry != parsed_line.entry;
                simplified |= simplified_this;
                if let Some(ref entry) = parsed_line.entry {
                    if let parser::Entry::Include(_, ref nested_path) = entry {
                        let p = path.parent().unwrap().join(nested_path);
                        simplify_manifest(&p);
                    }
                }
                if simplified_this {
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

    if !simplified {
        return; // We did nothing, so don't touch the file.
    }

    // TODO: In an ideal world we'd use a temp file to write in case there's any
    // write error (to avoid truncating it). But let's not bother.
    let mut file = std::fs::File::create(path).expect("Failed to open file to write");
    for line in lines {
        file.write_all(line.as_bytes()).expect("failed to write()");
        file.write_all(&[b'\n']).expect("failed to write()");
    }
}


fn main() {
    let path = std::env::args().nth(1).expect("Expected a path to a reftest directory");
    simplify_manifest(Path::new(&path));
}
