use crate::lua_syntax;

pub fn interpret(ast: lua_syntax::Block) -> Result<(), String> {
    let s = Scope{};
    interpret_block(&s, ast)
}

struct Scope {}

impl Scope {
    fn inherit(s: &Scope) -> Scope {
        Scope{}
    }
}

fn interpret_block(s: &Scope, ast: lua_syntax::Block) -> Result<(), String> {
    let s = Scope::inherit(s);
    for stmt in ast.0 {
        interpret_statement(&s, stmt)?;
    }
    if let Some(stmt) = ast.1 {
        interpret_last_statement(&s, stmt)?;
    }
    Ok(())
}

fn interpret_statement(s: &Scope, stmt: lua_syntax::Stat) -> Result<(), String> {
    eprintln!("stmt {:?}", stmt);
    Ok(())
}

fn interpret_last_statement(s: &Scope, stmt: lua_syntax::LastStat) -> Result<(), String> {
    eprintln!("stmt {:?}", stmt);
    Ok(())
}
