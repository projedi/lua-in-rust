extern crate lua_in_rust;

use lua_in_rust::lua_lexemes;
use lua_in_rust::lua_syntax;
use std::fs;
use std::io::Read;

fn print_source(name: String) -> Result<(), String> {
    let file = fs::File::open(&name).map_err(|e| e.to_string())?;
    let mut decoder = encoding_rs_io::DecodeReaderBytesBuilder::new()
        .encoding(Some(encoding_rs::WINDOWS_1252))
        .build(file);
    let mut contents = String::new();
    decoder
        .read_to_string(&mut contents)
        .map_err(|e| e.to_string())?;

    let tokens = lua_lexemes::parser::run_parser(&contents)?;
    let ast = lua_syntax::parser::run_parser(&tokens)?;
    let result = lua_syntax::printer::run_printer(&ast)?;

    println!("{}", result);
    Ok(())
}

fn main() {
    lua_in_rust::start_tracing();
    print_source(std::env::args().nth(1).unwrap()).unwrap();
    lua_in_rust::stop_tracing();
}
