use super::*;

fn print_exp(expr: lua_syntax::Exp) -> String {
    let mut result = String::new();
    expr.fmt(&mut result, Params::default_params())
        .expect("Failed to print expression");
    result
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
}
