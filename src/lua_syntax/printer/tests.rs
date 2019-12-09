use super::*;
use crate::lua_lexemes;

fn print_exp(expr: lua_syntax::Exp) -> String {
    let mut result = String::new();
    expr.fmt(&mut result, Params::default_params())
        .expect("Failed to print expression");
    result
}

fn print_fun<'a>(
    name: &'a str,
    args: Option<lua_syntax::ParList<'a>>,
    body: lua_syntax::Block<'a>,
) -> String {
    run_printer(&lua_syntax::Block(
        vec![lua_syntax::Stat::Function(
            lua_syntax::FuncName(name, vec![], None),
            lua_syntax::FuncBody(args, body),
        )],
        None,
    ))
    .expect("Failed to print function")
}

fn print_funcname(name: lua_syntax::FuncName) -> String {
    let mut result = String::new();
    name.fmt(&mut result, Params::default_params())
        .expect("Failed to print funcname");
    result
}

fn string_literal<'a>(s: String) -> lua_syntax::StringLiteral<'a> {
    lua_lexemes::StringLiteral::QuotedStringLiteral(lua_lexemes::QuotedStringLiteral {
        string: s,
        quote: '\'',
    })
}

fn print_funcall(call: lua_syntax::FunctionCall) -> String {
    run_printer(&lua_syntax::Block(
        vec![lua_syntax::Stat::FunctionCall(call)],
        None,
    ))
    .expect("Failed to print function call")
}

fn num(n: u32) -> lua_syntax::Exp<'static> {
    lua_syntax::Exp::Number(f64::from(n))
}

fn binop(
    lhs: lua_syntax::Exp<'static>,
    op: lua_syntax::BinOp,
    rhs: lua_syntax::Exp<'static>,
) -> lua_syntax::Exp<'static> {
    lua_syntax::Exp::BinOp(Box::new(lhs), op, Box::new(rhs))
}

fn unop(op: lua_syntax::UnOp, expr: lua_syntax::Exp<'static>) -> lua_syntax::Exp<'static> {
    lua_syntax::Exp::UnOp(op, Box::new(expr))
}

fn add(lhs: lua_syntax::Exp<'static>, rhs: lua_syntax::Exp<'static>) -> lua_syntax::Exp<'static> {
    binop(lhs, lua_syntax::BinOp::Add, rhs)
}

fn sub(lhs: lua_syntax::Exp<'static>, rhs: lua_syntax::Exp<'static>) -> lua_syntax::Exp<'static> {
    binop(lhs, lua_syntax::BinOp::Sub, rhs)
}

fn mul(lhs: lua_syntax::Exp<'static>, rhs: lua_syntax::Exp<'static>) -> lua_syntax::Exp<'static> {
    binop(lhs, lua_syntax::BinOp::Mul, rhs)
}

fn pow(lhs: lua_syntax::Exp<'static>, rhs: lua_syntax::Exp<'static>) -> lua_syntax::Exp<'static> {
    binop(lhs, lua_syntax::BinOp::Pow, rhs)
}

fn unm(expr: lua_syntax::Exp<'static>) -> lua_syntax::Exp<'static> {
    unop(lua_syntax::UnOp::Unm, expr)
}

fn not(expr: lua_syntax::Exp<'static>) -> lua_syntax::Exp<'static> {
    unop(lua_syntax::UnOp::Not, expr)
}

fn s_local<'a>(name: &'a str, val: lua_syntax::Exp<'a>) -> lua_syntax::Stat<'a> {
    lua_syntax::Stat::LocalVarDecl(vec![name], Some(vec![val]))
}

fn s_if<'a>(expr: lua_syntax::Exp<'a>, then_block: lua_syntax::Block<'a>) -> lua_syntax::Stat<'a> {
    lua_syntax::Stat::If((expr, then_block), vec![], None)
}

fn s_ifelse<'a>(
    expr: lua_syntax::Exp<'a>,
    then_block: lua_syntax::Block<'a>,
    else_block: lua_syntax::Block<'a>,
) -> lua_syntax::Stat<'a> {
    lua_syntax::Stat::If((expr, then_block), vec![], Some(else_block))
}

#[test]
fn test_operator_printing() {
    assert_eq!(print_exp(add(num(1), add(num(2), num(3)))), "1 + 2 + 3",);
    assert_eq!(print_exp(add(add(num(1), num(2)), num(3))), "1 + 2 + 3",);
    assert_eq!(print_exp(add(num(1), sub(num(2), num(3)))), "1 + 2 - 3",);
    assert_eq!(print_exp(sub(add(num(1), num(2)), num(3))), "1 + 2 - 3",);
    assert_eq!(print_exp(add(num(1), mul(num(2), num(3)))), "1 + 2 * 3",);
    assert_eq!(print_exp(mul(add(num(1), num(2)), num(3))), "(1 + 2) * 3",);
    assert_eq!(
        print_exp(mul(add(num(1), num(2)), unm(num(3)))),
        "(1 + 2) * -3",
    );
    assert_eq!(
        print_exp(mul(unm(add(num(1), num(2))), num(3))),
        "-(1 + 2) * 3",
    );
    assert_eq!(print_exp(unm(pow(num(1), num(2)))), "-1 ^ 2",);
    assert_eq!(print_exp(pow(unm(num(1)), num(2))), "(-1) ^ 2",);
    assert_eq!(print_exp(pow(num(1), unm(num(2)))), "1 ^ (-2)",);
    assert_eq!(print_exp(not(num(1))), "not 1");
}

#[test]
fn test_function_printing() {
    assert_eq!(
        print_fun("abc", None, lua_syntax::Block(vec![], None,)),
        r#"function abc()
end
"#,
    );
    assert_eq!(
        print_fun(
            "abc",
            None,
            lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Return(None)),)
        ),
        r#"function abc()
  return
end
"#,
    );
    assert_eq!(
        print_fun(
            "abc",
            None,
            lua_syntax::Block(vec![s_local("x", num(1)),], None,)
        ),
        r#"function abc()
  local x = 1
end
"#,
    );
    assert_eq!(
        print_fun(
            "abc",
            None,
            lua_syntax::Block(
                vec![s_if(
                    num(1),
                    lua_syntax::Block(vec![s_local("x", num(2))], None,)
                ),],
                None,
            )
        ),
        r#"function abc()
  if 1 then
    local x = 2
  end
end
"#,
    );
    assert_eq!(
        print_fun(
            "abc",
            None,
            lua_syntax::Block(
                vec![s_ifelse(
                    num(1),
                    lua_syntax::Block(vec![s_local("x", num(2))], None,),
                    lua_syntax::Block(vec![s_local("y", num(3))], None,)
                ),],
                None,
            )
        ),
        r#"function abc()
  if 1 then
    local x = 2
  else
    local y = 3
  end
end
"#,
    );
}

#[test]
fn test_funcname_printing() {
    assert_eq!(
        print_funcname(lua_syntax::FuncName("abc", vec![], None)),
        "abc"
    );
    assert_eq!(
        print_funcname(lua_syntax::FuncName("abc", vec!["def", "ghi"], None)),
        "abc.def.ghi"
    );
    assert_eq!(
        print_funcname(lua_syntax::FuncName("abc", vec![], Some("xyz"))),
        "abc:xyz"
    );
    assert_eq!(
        print_funcname(lua_syntax::FuncName("abc", vec!["def", "ghi"], Some("xyz"))),
        "abc.def.ghi:xyz"
    );
}

#[test]
fn test_funcall_printing() {
    assert_eq!(
        print_funcall(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            "def",
            lua_syntax::Args::Args(None),
        )),
        "abc:def()\n"
    );
    assert_eq!(
        print_funcall(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            "def",
            lua_syntax::Args::String(string_literal("xyz".to_string())),
        )),
        "abc:def'xyz'\n"
    );

    assert_eq!(
        print_funcall(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            lua_syntax::Args::Args(None),
        )),
        "abc()\n"
    );
    assert_eq!(
        print_funcall(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            lua_syntax::Args::String(string_literal("xyz".to_string())),
        )),
        "abc'xyz'\n"
    );

    assert_eq!(
        print_funcall(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None),
            )),),
            lua_syntax::Args::Args(None),
        )),
        "abc()()\n"
    );
    assert_eq!(
        print_funcall(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None),
            )),),
            "def",
            lua_syntax::Args::Args(None),
        )),
        "abc():def()\n"
    );
    assert_eq!(
        print_funcall(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::MethodCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                "def",
                lua_syntax::Args::Args(None),
            )),),
            lua_syntax::Args::Args(None),
        )),
        "abc:def()()\n"
    );

    assert_eq!(
        print_funcall(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::Exp(Box::new(num(1)),),
            lua_syntax::Args::Args(None),
        )),
        "(1)()\n"
    );
    assert_eq!(
        print_funcall(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::Exp(Box::new(num(1)),),
            "def",
            lua_syntax::Args::Args(None),
        )),
        "(1):def()\n"
    );
}
