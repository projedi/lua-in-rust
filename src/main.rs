extern crate clap;
extern crate lua_in_rust;

use lua_in_rust::lua_interpreter;
use lua_in_rust::lua_lexemes;
use lua_in_rust::lua_syntax;
use std::fs;
use std::io::Read;

fn read_source(name: &str) -> Result<String, String> {
    let file = fs::File::open(name).map_err(|e| e.to_string())?;
    let mut decoder = encoding_rs_io::DecodeReaderBytesBuilder::new()
        .encoding(Some(encoding_rs::WINDOWS_1252))
        .build(file);
    let mut contents = String::new();
    decoder
        .read_to_string(&mut contents)
        .map_err(|e| e.to_string())?;
    Ok(contents)
}

fn print_source(name: &str) -> Result<(), String> {
    let contents = read_source(name)?;

    let tokens = lua_lexemes::parser::run_parser(&contents)?;
    let ast = lua_syntax::parser::run_parser(&tokens)?;
    let result = lua_syntax::printer::run_printer(&ast)?;

    println!("{}", result);
    Ok(())
}

fn interpret(name: &str) -> Result<(), String> {
    let contents = read_source(name)?;

    let tokens = lua_lexemes::parser::run_parser(&contents)?;
    let ast = lua_syntax::parser::run_parser(&tokens)?;

    lua_interpreter::interpret(ast)
}

fn get_args<'a>() -> clap::ArgMatches<'a> {
    clap::App::new("lua")
        .arg(
            clap::Arg::with_name("file")
                .index(1)
                .required(true)
                .value_name("FILE"),
        )
        .arg(
            clap::Arg::with_name("fmt")
                .long("fmt")
                .help("Format the source instead of running code."),
        )
        .arg(
            clap::Arg::with_name("trace")
                .long("trace")
                .help("Enable tracing"),
        )
        .get_matches()
}

fn main() {
    let args = get_args();
    if args.is_present("trace") {
        lua_in_rust::start_tracing();
    }
    if args.is_present("fmt") {
        print_source(args.value_of("file").unwrap()).unwrap();
    } else {
        interpret(args.value_of("file").unwrap()).unwrap();
    }
    if args.is_present("trace") {
        lua_in_rust::stop_tracing();
    }
}
