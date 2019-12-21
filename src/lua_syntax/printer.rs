use crate::lua_lexemes::printer::fmt_string;
use crate::lua_syntax;
use std::fmt;

pub fn run_printer<'a>(ast: &'a lua_syntax::Block<'a>) -> Result<String, String> {
    let mut result = String::new();
    match ast.fmt(&mut result, Params::default_params()) {
        Ok(_) => Ok(result),
        Err(e) => Err(e.to_string()),
    }
}

#[derive(Copy, Clone)]
struct Params {
    lvl: u64,
    precedence: u64,
}

impl Params {
    fn default_params() -> Params {
        Params {
            lvl: 0,
            precedence: 0,
        }
    }

    fn next_level(&self) -> Params {
        Params {
            lvl: self.lvl + 1,
            precedence: self.precedence,
        }
    }

    fn with_precedence(&self, precedence: u64) -> Params {
        Params {
            lvl: self.lvl,
            precedence,
        }
    }

    fn default_precedence(&self) -> Params {
        self.with_precedence(0)
    }
}

trait HasPrecedence {
    fn precedence(&self) -> u64;
}

impl HasPrecedence for lua_syntax::UnOp {
    fn precedence(&self) -> u64 {
        match &self {
            lua_syntax::UnOp::Unm => 7,
            lua_syntax::UnOp::Not => 7,
            lua_syntax::UnOp::Len => 7,
        }
    }
}

impl HasPrecedence for lua_syntax::BinOp {
    fn precedence(&self) -> u64 {
        match &self {
            lua_syntax::BinOp::Or => 1,
            lua_syntax::BinOp::And => 2,
            lua_syntax::BinOp::Lt => 3,
            lua_syntax::BinOp::Le => 3,
            lua_syntax::BinOp::Gt => 3,
            lua_syntax::BinOp::Ge => 3,
            lua_syntax::BinOp::Eq => 3,
            lua_syntax::BinOp::Neq => 3,
            lua_syntax::BinOp::Concat => 4,
            lua_syntax::BinOp::Add => 5,
            lua_syntax::BinOp::Sub => 5,
            lua_syntax::BinOp::Mul => 6,
            lua_syntax::BinOp::Div => 6,
            lua_syntax::BinOp::Mod => 6,
            lua_syntax::BinOp::Pow => 8,
        }
    }
}

trait Pretty {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result;
}

fn put_level(out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
    for _ in 0..params.lvl {
        write!(out, "  ")?;
    }
    Ok(())
}

fn comma_separated(out: &mut dyn fmt::Write, params: Params, elems: &[impl Pretty]) -> fmt::Result {
    let mut iter = elems.iter();
    match iter.next() {
        None => return Ok(()),
        Some(elem) => {
            elem.fmt(out, params)?;
        }
    }
    loop {
        match iter.next() {
            Some(elem) => {
                write!(out, ", ")?;
                elem.fmt(out, params)?;
            }
            None => return Ok(()),
        }
    }
}

fn with_parens(
    out: &mut dyn fmt::Write,
    f: impl FnOnce(&mut dyn fmt::Write) -> fmt::Result,
) -> fmt::Result {
    write!(out, "(")?;
    f(out)?;
    write!(out, ")")
}

fn maybe_parens(
    out: &mut dyn fmt::Write,
    params: Params,
    inner_precedence: u64,
    f: impl FnOnce(&mut dyn fmt::Write, Params) -> fmt::Result,
) -> fmt::Result {
    let needs_parens = inner_precedence < params.precedence;
    if needs_parens {
        with_parens(out, move |out| {
            f(out, params.with_precedence(inner_precedence))
        })
    } else {
        f(out, params.with_precedence(inner_precedence))
    }
}

impl Pretty for lua_syntax::Name<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, _params: Params) -> fmt::Result {
        write!(out, "{}", self)
    }
}

impl Pretty for lua_syntax::NumberLiteral {
    fn fmt(&self, out: &mut dyn fmt::Write, _params: Params) -> fmt::Result {
        write!(out, "{}", self)
    }
}

impl Pretty for lua_syntax::Block<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        for stmt in &self.0 {
            put_level(out, params)?;
            stmt.fmt(out, params)?;
            writeln!(out)?;
        }
        match &self.1 {
            None => Ok(()),
            Some(stmt) => {
                put_level(out, params)?;
                stmt.fmt(out, params)?;
                writeln!(out)
            }
        }
    }
}

impl Pretty for lua_syntax::Stat<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        match &self {
            lua_syntax::Stat::Assign(vars, exps) => {
                comma_separated(out, params, vars)?;
                write!(out, " = ")?;
                comma_separated(out, params, exps)
            }
            lua_syntax::Stat::FunctionCall(fcall) => fcall.fmt(out, params),
            lua_syntax::Stat::Block(block) => {
                writeln!(out, "do")?;
                block.fmt(out, params.next_level())?;
                write!(out, "end")
            }
            lua_syntax::Stat::While(exp, block) => {
                write!(out, "while ")?;
                exp.fmt(out, params)?;
                writeln!(out, " do")?;
                block.fmt(out, params.next_level())?;
                put_level(out, params)?;
                write!(out, "end")
            }
            lua_syntax::Stat::Repeat(block, exp) => {
                writeln!(out, "repeat")?;
                block.fmt(out, params.next_level())?;
                put_level(out, params)?;
                write!(out, "until ")?;
                exp.fmt(out, params)
            }
            lua_syntax::Stat::If(then_branch, else_if_branches, else_branch) => {
                write!(out, "if ")?;
                then_branch.0.fmt(out, params)?;
                writeln!(out, " then")?;
                then_branch.1.fmt(out, params.next_level())?;
                for else_if_branch in else_if_branches {
                    put_level(out, params)?;
                    write!(out, "elseif ")?;
                    else_if_branch.0.fmt(out, params)?;
                    writeln!(out, " then")?;
                    else_if_branch.1.fmt(out, params.next_level())?;
                }
                match &else_branch {
                    None => (),
                    Some(block) => {
                        put_level(out, params)?;
                        writeln!(out, "else")?;
                        block.fmt(out, params.next_level())?;
                    }
                }
                put_level(out, params)?;
                write!(out, "end")
            }
            lua_syntax::Stat::For(name, exp_from, exp_to, exp_step, block) => {
                write!(out, "for {} = ", name)?;
                exp_from.fmt(out, params)?;
                write!(out, ", ")?;
                exp_to.fmt(out, params)?;
                match exp_step {
                    None => (),
                    Some(exp_step) => {
                        write!(out, ", ")?;
                        exp_step.fmt(out, params)?;
                    }
                }
                writeln!(out, " do")?;
                block.fmt(out, params.next_level())?;
                put_level(out, params)?;
                write!(out, "end")
            }
            lua_syntax::Stat::ForIn(names, exps, block) => {
                write!(out, "for ")?;
                comma_separated(out, params, names)?;
                write!(out, " in ")?;
                comma_separated(out, params, exps)?;
                writeln!(out, " do")?;
                block.fmt(out, params.next_level())?;
                put_level(out, params)?;
                write!(out, "end")
            }
            lua_syntax::Stat::Function(funcname, funcbody) => {
                write!(out, "function ")?;
                funcname.fmt(out, params)?;
                funcbody.fmt(out, params)
            }
            lua_syntax::Stat::LocalFunctionDecl(funcname, funcbody) => {
                write!(out, "local function {}", funcname)?;
                funcbody.fmt(out, params)
            }
            lua_syntax::Stat::LocalVarDecl(names, exps) => {
                write!(out, "local ")?;
                comma_separated(out, params, names)?;
                match exps {
                    None => Ok(()),
                    Some(exps) => {
                        write!(out, " = ")?;
                        comma_separated(out, params, exps)
                    }
                }
            }
        }
    }
}

impl Pretty for lua_syntax::LastStat<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        match &self {
            lua_syntax::LastStat::Return(exps) => {
                write!(out, "return")?;
                match exps {
                    None => Ok(()),
                    Some(exps) => {
                        write!(out, " ")?;
                        comma_separated(out, params, exps)
                    }
                }
            }
            lua_syntax::LastStat::Break => write!(out, "break"),
        }
    }
}

impl Pretty for lua_syntax::FuncName<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, _params: Params) -> fmt::Result {
        write!(out, "{}", self.0)?;
        for name in &self.1 {
            write!(out, ".{}", name)?;
        }
        match &self.2 {
            None => Ok(()),
            Some(name) => write!(out, ":{}", name),
        }
    }
}

impl Pretty for lua_syntax::Var<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        match &self {
            lua_syntax::Var::Name(name) => write!(out, "{}", name),
            lua_syntax::Var::Index(arr, index) => {
                arr.fmt(out, params)?;
                write!(out, "[")?;
                index.fmt(out, params)?;
                write!(out, "]")
            }
            lua_syntax::Var::Field(table, field) => {
                table.fmt(out, params)?;
                write!(out, ".{}", field)
            }
        }
    }
}

impl Pretty for lua_syntax::Exp<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        match &self {
            lua_syntax::Exp::Nil => write!(out, "nil"),
            lua_syntax::Exp::False => write!(out, "false"),
            lua_syntax::Exp::True => write!(out, "true"),
            lua_syntax::Exp::Number(n) => write!(out, "{}", n),
            lua_syntax::Exp::String(s) => fmt_string(out, s),
            lua_syntax::Exp::VarArgs => write!(out, "..."),
            lua_syntax::Exp::Function(funcbody) => {
                write!(out, "function ")?;
                funcbody.fmt(out, params.default_precedence())
            }
            lua_syntax::Exp::PrefixExp(exp) => exp.fmt(out, params.default_precedence()),
            lua_syntax::Exp::TableCtor(table_ctor) => {
                table_ctor.fmt(out, params.default_precedence())
            }
            lua_syntax::Exp::BinOp(lhs, op, rhs) => maybe_parens(
                out,
                params,
                op.precedence(),
                |out: &mut dyn fmt::Write, params: Params| -> fmt::Result {
                    lhs.fmt(out, params)?;
                    write!(out, " ")?;
                    op.fmt(out, params)?;
                    write!(out, " ")?;
                    rhs.fmt(out, params)
                },
            ),
            lua_syntax::Exp::UnOp(op, exp) => maybe_parens(
                out,
                params,
                op.precedence(),
                |out: &mut dyn fmt::Write, params: Params| -> fmt::Result {
                    op.fmt(out, params)?;
                    if let lua_syntax::UnOp::Not = op {
                        write!(out, " ")?;
                    }
                    exp.fmt(out, params)
                },
            ),
        }
    }
}

impl Pretty for lua_syntax::PrefixExp<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        match &self {
            lua_syntax::PrefixExp::Var(v) => v.fmt(out, params),
            lua_syntax::PrefixExp::FunctionCall(fcall) => fcall.fmt(out, params),
            lua_syntax::PrefixExp::Parens(exp) => with_parens(out, move |out| exp.fmt(out, params)),
        }
    }
}

impl Pretty for lua_syntax::FunctionCall<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        match &self {
            lua_syntax::FunctionCall::FunctionCall(exp, args) => {
                exp.fmt(out, params)?;
                args.fmt(out, params)
            }
            lua_syntax::FunctionCall::MethodCall(exp, name, args) => {
                exp.fmt(out, params)?;
                write!(out, ":{}", name)?;
                args.fmt(out, params)
            }
        }
    }
}

impl Pretty for lua_syntax::Args<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        match &self {
            lua_syntax::Args::Args(args) => {
                write!(out, "(")?;
                match args {
                    Some(args) => comma_separated(out, params, args)?,
                    None => (),
                }
                write!(out, ")")
            }
            lua_syntax::Args::TableCtor(table_ctor) => table_ctor.fmt(out, params),
            lua_syntax::Args::String(s) => fmt_string(out, s),
        }
    }
}

impl Pretty for lua_syntax::FuncBody<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        write!(out, "(")?;
        match &self.0 {
            Some(args) => args.fmt(out, params)?,
            None => (),
        }
        writeln!(out, ")")?;
        self.1.fmt(out, params.next_level())?;
        put_level(out, params)?;
        write!(out, "end")
    }
}

impl Pretty for lua_syntax::ParList<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        match &self {
            lua_syntax::ParList::ParList(names, has_var_args) => {
                comma_separated(out, params, names)?;
                if *has_var_args {
                    write!(out, ", ...")?;
                }
                Ok(())
            }
            lua_syntax::ParList::VarArgs => write!(out, "..."),
        }
    }
}

impl Pretty for lua_syntax::TableCtor<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        write!(out, "{{")?;
        match &self.0 {
            None => (),
            Some(fields) => comma_separated(out, params, fields)?,
        }
        write!(out, "}}")
    }
}

impl Pretty for lua_syntax::Field<'_> {
    fn fmt(&self, out: &mut dyn fmt::Write, params: Params) -> fmt::Result {
        match &self {
            lua_syntax::Field::Field(index, exp) => {
                write!(out, "[")?;
                index.fmt(out, params)?;
                write!(out, "] = ")?;
                exp.fmt(out, params)
            }
            lua_syntax::Field::NameField(name, exp) => {
                write!(out, "{} = ", name)?;
                exp.fmt(out, params)
            }
            lua_syntax::Field::NumberField(exp) => exp.fmt(out, params),
        }
    }
}

impl Pretty for lua_syntax::BinOp {
    fn fmt(&self, out: &mut dyn fmt::Write, _params: Params) -> fmt::Result {
        match &self {
            lua_syntax::BinOp::Add => write!(out, "+"),
            lua_syntax::BinOp::Sub => write!(out, "-"),
            lua_syntax::BinOp::Mul => write!(out, "*"),
            lua_syntax::BinOp::Div => write!(out, "/"),
            lua_syntax::BinOp::Pow => write!(out, "^"),
            lua_syntax::BinOp::Mod => write!(out, "%"),
            lua_syntax::BinOp::Concat => write!(out, ".."),
            lua_syntax::BinOp::Lt => write!(out, "<"),
            lua_syntax::BinOp::Le => write!(out, "<="),
            lua_syntax::BinOp::Gt => write!(out, ">"),
            lua_syntax::BinOp::Ge => write!(out, ">="),
            lua_syntax::BinOp::Eq => write!(out, "=="),
            lua_syntax::BinOp::Neq => write!(out, "~="),
            lua_syntax::BinOp::And => write!(out, "and"),
            lua_syntax::BinOp::Or => write!(out, "or"),
        }
    }
}

impl Pretty for lua_syntax::UnOp {
    fn fmt(&self, out: &mut dyn fmt::Write, _params: Params) -> fmt::Result {
        match &self {
            lua_syntax::UnOp::Unm => write!(out, "-"),
            lua_syntax::UnOp::Not => write!(out, "not"),
            lua_syntax::UnOp::Len => write!(out, "#"),
        }
    }
}

#[cfg(test)]
mod tests;
