#[macro_use]
mod parser_lib;
mod lua_syntax;

fn main() {
    println!("Hello, world!");
    println!("{}", lua_syntax::Keyword::items_count());
    for keyword in lua_syntax::Keyword::ITEMS.into_iter() {
        println!("{:?} => {}", keyword, keyword);
    }
    println!(
        "{:?} {:?}",
        lua_syntax::Keyword::create_sorted_items(),
        lua_syntax::Keyword::ITEMS
    );
}
