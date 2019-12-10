use crate::lua_syntax;

pub fn interpret(ast: lua_syntax::Block) -> Result<(), String> {
    println!("TODO: {:?}", ast);
    Ok(())
}
