extern crate lua_in_rust;

use lua_in_rust::lua_lexemes;

fn main() {
    println!("Hello, world!");
    println!("{}", lua_lexemes::Keyword::ITEMS_COUNT);
    for keyword in lua_lexemes::Keyword::ITEMS.into_iter() {
        println!("{:?} => {}", keyword, keyword.to_str());
    }
    println!("{:?}", lua_lexemes::Keyword::ITEMS);
}
