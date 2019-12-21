use crate::lua_syntax;

mod memory_manager;

pub fn interpret(ast: lua_syntax::Block) -> Result<(), String> {
    println!("TODO: {:?}", ast);
    Ok(())
}
