#[macro_use]
extern crate rs_tracing;

#[macro_use]
pub mod parser_lib;
pub mod lua_lexemes;
pub mod lua_syntax;
pub mod utils;

pub fn start_tracing() {
    open_trace_file!(".").unwrap();
    trace_activate!();
}

pub fn stop_tracing() {
    close_trace_file!();
}
