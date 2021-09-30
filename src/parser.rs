//! Reftest manifest parser.
//!
//! Based on:
//!
//! https://searchfox.org/mozilla-central/rev/79f93e7a8b9aa1903f1349f2dd46fb71596f2ae9/layout/tools/reftest/manifest.jsm#86

use std::path::{Path, PathBuf};

#[derive(Debug, PartialEq)]
pub enum IsHttp {
    No,
    Yes { depth: usize },
}

#[derive(Debug, PartialEq)]
pub enum Condition {
    Simple(String),
    Neg(Box<Condition>),
    Paren(Box<Condition>),
    And(Vec<Condition>),
    Or(Vec<Condition>),
}

fn is_binop(b: u8) -> bool {
    matches!(b, b'&' | b'|')
}

fn find_pos_in_scope(s: &[u8], mut matches: impl FnMut(u8) -> bool) -> Option<usize> {
    let mut counter = 0;
    for (i, &b) in s.iter().enumerate() {
        if counter == 0 && matches(b) {
            return Some(i);
        }
        match b {
            b'(' | b'[' => counter += 1,
            b')' | b']' => counter -= 1,
            _ => {}
        }
    }
    None
}

fn find_comma_in_scope(s: &str) -> Option<usize> {
    find_pos_in_scope(s.as_bytes(), |b| b == b',')
}

impl Condition {
    fn parse(mut condition: &[u8]) -> Self {
        let mut ret = vec![];
        let mut operator = None;
        loop {
            {
                let (parsed, advanced) = Self::parse_one(condition);
                ret.push(parsed);
                condition = &condition[advanced..];
            }

            if condition.is_empty() {
                break;
            }

            if !is_binop(condition[0]) {
                eprintln!(
                    "Failed to parse condition: {:?}",
                    String::from_utf8_lossy(condition)
                );
                break;
            }

            if condition.len() < 2 || condition[0] != condition[1] {
                eprintln!("Stray operator {}", condition[0]);
                break;
            }

            if let Some(op) = operator {
                if op != condition[0] {
                    eprintln!("Operator conflict: {}", condition[0]);
                    break;
                }
            } else {
                operator = Some(condition[0]);
            }

            condition = &condition[2..];
        }

        assert!(!ret.is_empty());
        if ret.len() == 1 {
            return ret.into_iter().next().unwrap();
        }

        let op = match operator {
            Some(b'&') => Condition::And,
            Some(b'|') => Condition::Or,
            _ => panic!("How don't we have an operator here?"),
        };

        op(ret)
    }

    fn parse_one(condition: &[u8]) -> (Self, usize) {
        if condition.is_empty() {
            return (Self::Simple(Default::default()), 0);
        }
        match condition[0] {
            b'!' => {
                let (negated, advanced) = Self::parse_one(&condition[1..]);
                (Self::Neg(Box::new(negated)), advanced + 1)
            }
            b'(' => {
                let pos = 1 + find_pos_in_scope(&condition[1..], |b| b == b')')
                    .unwrap_or(condition.len() - 1);
                let inner = Self::parse(&condition[1..pos]);
                (Self::Paren(Box::new(inner)), pos + 1)
            }
            _ => {
                let pos = find_pos_in_scope(condition, |b| is_binop(b)).unwrap_or(condition.len());
                (
                    Self::Simple(String::from_utf8_lossy(&condition[0..pos]).into_owned()),
                    pos,
                )
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct URange(usize, usize);

impl URange {
    fn from_str(s: &str, require_boundary: bool) -> Option<URange> {
        let mut number_end = 0;
        let mut first_number_end = 0;
        let mut iter = s.chars();
        let mut first = 0;
        let mut second = None;
        let mut found_boundary = false;
        while let Some(n) = iter.next() {
            if n.is_ascii_digit() {
                number_end += 1;
                continue;
            }
            if n != '-' || found_boundary {
                return None;
            }
            found_boundary = true;
            first_number_end = number_end;
            first = s[..number_end].parse::<usize>().ok()?;
        }

        if !found_boundary {
            if require_boundary {
                return None;
            }
            first = s.parse::<usize>().ok()?;
        } else {
            second = Some(s[first_number_end + 1..].parse::<usize>().ok()?);
        }

        Some(URange(first, second.unwrap_or(first)))
    }
}

#[derive(Debug, PartialEq)]
pub enum Status {
    #[allow(unused)]
    Pass,
    Fail,
    Random,
    Skip,
    SilentFail,
}

impl Status {
    fn from_str(s: &str) -> Option<Status> {
        // Pass is default.
        Some(match s {
            "fail" => Status::Fail,
            "random" => Status::Random,
            "skip" => Status::Skip,
            "silentfail" => Status::SilentFail,
            _ => return None,
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum TestItem {
    NeedsFocus,
    ChaosMode,
    WrCapture,
    WrCaptureRef,
    NoAutoFuzz,
    Status(Status, Option<Condition>),
    Slow(Option<Condition>),
    Asserts(Option<Condition>, URange),
    Fuzzy(Option<Condition>, URange, URange),
    RequireOr(Condition, Status),
}

fn parse_function<'a>(name: &str, mut item: &'a str) -> Result<&'a str, ()> {
    if !item.starts_with(name) {
        return Err(());
    }
    item = &item[name.len()..];
    if !item.starts_with('(') || !item.ends_with(')') {
        return Err(());
    }
    Ok(&item[1..item.len() - 1])
}

fn parse_maybe_conditional(name: &str, mut item: &str) -> Result<Option<Condition>, ()> {
    if item == name {
        return Ok(None);
    }
    if !item.starts_with(name) {
        return Err(());
    }
    item = &item[name.len()..];
    let condition = parse_function("-if", item)?;
    Ok(Some(Condition::parse(condition.as_bytes())))
}

impl TestItem {
    fn from_str(item: &str) -> Option<Self> {
        if let Ok(condition) = parse_maybe_conditional("fails", item) {
            return Some(Self::Status(Status::Fail, condition));
        }
        if let Ok(condition) = parse_maybe_conditional("random", item) {
            return Some(Self::Status(Status::Random, condition));
        }
        if let Ok(condition) = parse_maybe_conditional("skip", item) {
            return Some(Self::Status(Status::Skip, condition));
        }
        if let Ok(condition) = parse_maybe_conditional("silentfail", item) {
            return Some(Self::Status(Status::SilentFail, condition));
        }
        if let Ok(condition) = parse_maybe_conditional("slow", item) {
            return Some(Self::Slow(condition));
        }
        if let Ok(arguments) = parse_function("asserts", item) {
            let range = URange::from_str(arguments, /* require_boundary = */ false)?;
            return Some(Self::Asserts(None, range));
        }
        if let Ok(arguments) = parse_function("asserts-if", item) {
            let condition_end = find_comma_in_scope(arguments)?;
            let condition = Condition::parse(&arguments.as_bytes()[..condition_end]);
            let range = URange::from_str(
                &arguments[condition_end + 1..],
                /* require_boundary = */ false,
            )?;
            return Some(Self::Asserts(Some(condition), range));
        }
        if let Ok(arguments) = parse_function("fuzzy", item) {
            let first_range_end = find_comma_in_scope(arguments)?;
            let first = URange::from_str(
                &arguments[..first_range_end],
                /* require_boundary = */ true,
            )?;
            let second = URange::from_str(
                &arguments[first_range_end + 1..],
                /* require_boundary = */ true,
            )?;
            return Some(Self::Fuzzy(None, first, second));
        }
        if let Ok(arguments) = parse_function("fuzzy-if", item) {
            let condition_end = find_comma_in_scope(arguments)?;
            let condition = Condition::parse(&arguments.as_bytes()[..condition_end]);
            let first_range_end =
                condition_end + 1 + find_comma_in_scope(&arguments[condition_end + 1..])?;
            let first = URange::from_str(
                &arguments[condition_end + 1..first_range_end],
                /* require_boundary = */ true,
            )?;
            let second = URange::from_str(
                &arguments[first_range_end + 1..],
                /* require_boundary = */ true,
            )?;
            return Some(Self::Fuzzy(Some(condition), first, second));
        }
        if let Ok(arguments) = parse_function("require-or", item) {
            let condition_end = find_comma_in_scope(arguments)?;
            let condition = Condition::parse(&arguments.as_bytes()[..condition_end]);
            let status = Status::from_str(&arguments[condition_end + 1..])?;
            return Some(Self::RequireOr(condition, status));
        }
        Some(match item {
            "needs-focus" => Self::NeedsFocus,
            "chaos-mode" => Self::ChaosMode,
            "wr-capture" => Self::WrCapture,
            "wr-capture-ref" => Self::WrCaptureRef,
            "noautofuzz" => Self::NoAutoFuzz,
            _ => return None,
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum TestType {
    Script(PathBuf),
    Load(PathBuf),
    Equals(PathBuf, PathBuf),
    Unequals(PathBuf, PathBuf),
    Print(PathBuf, PathBuf),
}

#[derive(Debug, PartialEq)]
struct Line<'a> {
    entry: Option<Entry>,
    #[allow(unused)]
    comment: &'a str,
}

fn parse_items(items: &[&str]) -> Vec<TestItem> {
    let mut ret = Vec::with_capacity(items.len());
    for item in items {
        let item = match TestItem::from_str(item) {
            Some(i) => i,
            None => break,
        };
        ret.push(item);
    }
    ret
}

fn maybe_parse_http(item: &str) -> Option<usize> {
    if item == "HTTP" {
        return Some(0);
    }
    let mut path = match parse_function("HTTP", item) {
        Ok(p) => p,
        Err(()) => return None,
    };
    let mut depth = 0;
    while path.starts_with("..") {
        depth += 1;
        path = &path[2..];
        if path.is_empty() {
            break;
        }
        if !path.starts_with('/') {
            return None;
        }
        path = &path[1..];
    }
    if !path.is_empty() {
        return None;
    }
    return Some(depth);
}

fn parse_manifest_line<'a>(mut line: &'a str) -> Result<Line<'a>, &'static str> {
    line = line.trim();
    let mut comment = "";
    if let Some(pos) = line.find('#') {
        comment = &line[pos..];
        line = &line[..pos];
    }
    let items: Vec<_> = line.split_ascii_whitespace().collect();
    let mut items: &[_] = &*items;
    if items.is_empty() {
        return Ok(Line {
            entry: None,
            comment,
        });
    }

    if items.len() == 1 {
        return Err("At least two items in the line is needed");
    }

    {
        let first = items[0];
        if first == "url-prefix" {
            if items.len() != 2 {
                return Err("url-prefix() needs one url in the manifest file");
            }
            return Ok(Line {
                entry: Some(Entry::UrlPrefix(items[1].to_owned())),
                comment,
            });
        }

        if first == "defaults" {
            let defaults = parse_items(&items[1..]);
            if defaults.len() != items.len() - 1 {
                return Err("defaults condition has unrecognized items");
            }
            return Ok(Line {
                entry: Some(Entry::Defaults(defaults)),
                comment,
            });
        }
    }

    let test_items = parse_items(&items);
    items = &items[test_items.len()..];
    if items.is_empty() {
        return Err("Expected at least a test type");
    }

    let http_depth = maybe_parse_http(&items[0]);
    let is_http = match http_depth {
        Some(depth) => IsHttp::Yes { depth },
        None => IsHttp::No,
    };
    if http_depth.is_some() {
        items = &items[1..];
        if items.is_empty() {
            return Err("Expected at least a test type");
        }
    }

    let first = items[0];
    if first == "include" {
        if items.len() != 2 {
            return Err("include needs a single argument");
        }
        let path = PathBuf::from(items[1]);
        return Ok(Line {
            entry: Some(Entry::Include(test_items, path)),
            comment,
        });
    }

    if first == "load" || first == "script" {
        if items.len() != 2 {
            return Err("load / script needs a single argument");
        }

        let test = PathBuf::from(items[1]);
        let test_type = if first == "load" {
            TestType::Load(test)
        } else {
            TestType::Script(test)
        };
        return Ok(Line {
            entry: Some(Entry::Test(test_items, is_http, test_type)),
            comment,
        });
    }

    let test_type = match first {
        "==" => TestType::Equals,
        "!=" => TestType::Unequals,
        "print" => TestType::Print,
        _ => return Err("Unknown test type"),
    };

    if items.len() != 3 {
        return Err("Need two arguments for reftest");
    }

    let test = PathBuf::from(items[1]);
    let ref_ = PathBuf::from(items[2]);

    Ok(Line {
        entry: Some(Entry::Test(test_items, is_http, test_type(test, ref_))),
        comment,
    })
}

pub fn parse_manifest(path: &Path) -> Result<Vec<Entry>, &'static str> {
    use std::io::BufRead;

    let file = match std::fs::File::open(path) {
        Ok(f) => f,
        Err(..) => return Err("Failed to open file"),
    };

    let mut entries = vec![];
    let reader = std::io::BufReader::new(file);
    let mut line_number = 0;
    for line in reader.lines() {
        let line = line.expect("Error reading line");
        line_number += 1;
        match parse_manifest_line(&line) {
            Ok(line) => {
                if let Some(entry) = line.entry {
                    entries.push(entry);
                }
            }
            Err(error) => {
                eprintln!(
                    "Failed to parse {}:{}: {}",
                    path.display(),
                    line_number,
                    error
                );
            }
        }
    }

    Ok(entries)
}

#[derive(Debug, PartialEq)]
pub enum Entry {
    /// An `include foo.list` statement, with out the entries from parsing that
    /// manifest.
    Include(Vec<TestItem>, PathBuf),
    UrlPrefix(String),
    Defaults(Vec<TestItem>),
    Test(Vec<TestItem>, IsHttp, TestType),
}

#[test]
fn test_parsing() {
    assert_eq!(
        parse_manifest_line("fuzzy-if(foo&&!(bar||baz||/^aarch64-msvc/.test(xulRuntime.XPCOMABI)),0-3,5-6) == foo.html bar.html"),
        Ok(Line {
            entry: Some(Entry::Test(
                vec![TestItem::Fuzzy(
                    Some(Condition::And(vec![
                        Condition::Simple("foo".into()),
                        Condition::Neg(Box::new(Condition::Paren(Box::new(Condition::Or(vec![
                            Condition::Simple("bar".into()),
                            Condition::Simple("baz".into()),
                            Condition::Simple("/^aarch64-msvc/.test(xulRuntime.XPCOMABI)".into()),
                        ])))))
                    ])),
                    URange(0, 3),
                    URange(5, 6),
                )],
                IsHttp::No,
                TestType::Equals(PathBuf::from("foo.html"), PathBuf::from("bar.html")),
            )),
            comment: "",
        })
    )
}
