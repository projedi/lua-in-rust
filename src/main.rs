#[macro_use]
mod parser_lib;
mod lua_lexemes;
mod lua_lexer;

fn main() {
    println!("Hello, world!");
    println!("{}", lua_lexemes::Keyword::ITEMS_COUNT);
    for keyword in lua_lexemes::Keyword::ITEMS.into_iter() {
        println!("{:?} => {}", keyword, keyword.to_str());
    }
    println!("{:?}", lua_lexemes::Keyword::ITEMS);
}
