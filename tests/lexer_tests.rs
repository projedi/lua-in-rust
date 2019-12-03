extern crate lua_in_rust;

use lua_in_rust::lua_lexemes;
use std::fs;
use std::path;

const SOURCE_FILES: &str = "reference/lua5.1-tests";
const GOLDEN_FILES: &str = "tests/lexer_tests/golden_files";

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
        fs::read_to_string(&self.source_file).unwrap()
    }

    fn read_golden(&self) -> String {
        fs::read_to_string(&self.golden_file).unwrap_or_default()
    }
}

fn reprint(original: String) -> Result<String, String> {
    let tokens = lua_lexemes::parser::run_parser(&original)?;
    lua_lexemes::printer::run_printer(&tokens)
}

fn test(name: &str) -> Result<(), String> {
    let test_file = TestFile::from_name(name);
    let reprinted_text = reprint(test_file.read_source())?;
    assert_eq!(
        test_file.read_golden(),
        reprinted_text,
        "{}",
        test_file.source_file.display()
    );
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
