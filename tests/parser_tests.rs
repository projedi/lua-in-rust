extern crate lua_in_rust;

use lua_in_rust::lua_lexemes;
use lua_in_rust::lua_syntax;
use std::fmt;
use std::fs;
use std::io::Read;
use std::path;

// From https://stackoverflow.com/a/57929832
fn prefix_lines(prefix: &str, lines: &str) -> String {
    lines
        .lines()
        .map(|i| [prefix, i].concat())
        .collect::<Vec<String>>()
        .join("\n")
}

pub struct Diff(difference::Changeset);

impl Diff {
    pub fn new(left: &str, right: &str) -> Self {
        Self(difference::Changeset::new(left, right, "\n"))
    }
}

impl fmt::Display for Diff {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for d in &self.0.diffs {
            match *d {
                difference::Difference::Same(ref x) => {
                    write!(f, "{}{}", prefix_lines(" ", x), self.0.split)?;
                }
                difference::Difference::Add(ref x) => {
                    write!(f, "\x1b[92m{}\x1b[0m{}", prefix_lines("+", x), self.0.split)?;
                }
                difference::Difference::Rem(ref x) => {
                    write!(f, "\x1b[91m{}\x1b[0m{}", prefix_lines("-", x), self.0.split)?;
                }
            }
        }
        Ok(())
    }
}

// From pretty_assertions.
macro_rules! assert_eq {
    ($left:expr , $right:expr) => {{
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    panic!(
                        "assertion failed: `(left == right)`\
                         \n\
                         \n{}\
                         \n",
                        Diff::new(left_val, right_val)
                    )
                }
            }
        }
    }};
}

const SOURCE_FILES: &str = "reference/lua5.1-tests";
const GOLDEN_FILES: &str = "tests/parser_tests/golden_files";

struct TestFile {
    source_file: path::PathBuf,
    golden_file: path::PathBuf,
}

impl TestFile {
    fn from_name(name: &str) -> TestFile {
        let root_path = path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let source_file = {
            let mut path = root_path.clone();
            path.push(SOURCE_FILES);
            path.push(name);
            path.set_extension("lua");
            path
        };
        let golden_file = {
            let mut path = root_path.clone();
            path.push(GOLDEN_FILES);
            path.push(name);
            path.set_extension("golden");
            path
        };
        TestFile {
            source_file,
            golden_file,
        }
    }

    fn read_source(&self) -> String {
        let file = fs::File::open(&self.source_file).unwrap();
        let mut decoder = encoding_rs_io::DecodeReaderBytesBuilder::new()
            .encoding(Some(encoding_rs::WINDOWS_1252))
            .build(file);
        let mut dest = String::new();
        decoder.read_to_string(&mut dest).unwrap();
        dest
    }

    fn read_golden(&self) -> String {
        fs::read_to_string(&self.golden_file).unwrap_or_default()
    }
}

fn reprint(original: String) -> Result<String, String> {
    let tokens = lua_lexemes::parser::run_parser(&original)?;
    let ast = lua_syntax::parser::run_parser(&tokens)?;
    lua_syntax::printer::run_printer(&ast)
}

fn test(name: &str) -> Result<(), String> {
    let replace = std::env::args().any(|x| x == "--replace");

    let test_file = TestFile::from_name(name);
    let reprinted_text = reprint(test_file.read_source())?;
    if replace {
        fs::write(test_file.golden_file, reprinted_text).map_err(|e| e.to_string())?;
    } else {
        assert_eq!(
            test_file.read_golden().trim_end(),
            reprinted_text.trim_end()
        );
    }
    Ok(())
}

macro_rules! parse_lua_test_suite {
    ($($n:ident => $s:literal),* $(,)?) => {
        $(
        #[test]
        fn $n() -> Result<(), String> {
            test($s)
        }
        )*
    }
}

parse_lua_test_suite!(
    // TODO: Remove this one and unblacklist proper tests.
    parse_lua_test_suite_dummy => "dummy",
    /*
    parse_lua_test_suite_all => "all",
    parse_lua_test_suite_api => "api",
    parse_lua_test_suite_attrib => "attrib",
    parse_lua_test_suite_big => "big",
    parse_lua_test_suite_calls => "calls",
    parse_lua_test_suite_checktable => "checktable",
    parse_lua_test_suite_closure => "closure",
    parse_lua_test_suite_code => "code",
    parse_lua_test_suite_constructs => "constructs",
    parse_lua_test_suite_db => "db",
    parse_lua_test_suite_errors => "errors",
    parse_lua_test_suite_events => "events",
    parse_lua_test_suite_files => "files",
    parse_lua_test_suite_gc => "gc",
    parse_lua_test_suite_literals => "literals",
    parse_lua_test_suite_locals => "locals",
    parse_lua_test_suite_main => "main",
    parse_lua_test_suite_math => "math",
    parse_lua_test_suite_nextvar => "nextvar",
    parse_lua_test_suite_pm => "pm",
    parse_lua_test_suite_sort => "sort",
    parse_lua_test_suite_strings => "strings",
    parse_lua_test_suite_vararg => "vararg",
    parse_lua_test_suite_verybig => "verybig",
    */
);
