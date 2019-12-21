use crate::lua_syntax;
use crate::lua_interpreter::value::{TableRef, Value, Number, adjust_values, adjust_values_1};

mod value;

pub fn interpret(ast: lua_syntax::Block) -> Result<(), String> {
    let s = Scope{};
    let s = Scope::inherit(&s);
    match interpret_block(&s, &ast) {
        Ok(()) => Ok(()),
        Err(StatementError::Return(_)) => Ok(()),
        Err(StatementError::Break) => Err("Break called outside any loop".to_string()),
        Err(StatementError::Error(err)) => Err(err),
    }
}

struct Scope {}

impl Scope {
    fn inherit(s: &Scope) -> Scope {
        Scope{}
    }

    fn declare(&self, name: lua_syntax::Name) {
        // TODO: Implement
        eprintln!("declare {:?}", name);
    }

    fn get_table(&self, name: lua_syntax::Name) -> TableRef {
        // TODO: Implement
        eprintln!("get_table {:?}", name);
        TableRef::make(&[])
    }
}

enum StatementError<'s, 'g> {
    Return(Vec<Value<'s, 'g>>),
    Break,
    Error(String)
}

// Scope creation is left to the callers.
fn interpret_block<'s, 'g>(s: &Scope, block: &lua_syntax::Block<'s>) -> Result<(), StatementError<'s, 'g>> {
    for stmt in &block.0 {
        interpret_statement(s, stmt)?;
    }
    if let Some(stmt) = &block.1 {
        interpret_last_statement(s, stmt)?;
    }
    Ok(())
}

fn assign_var<'s, 'g>(s: &Scope, var: &lua_syntax::Var<'s>, val: Value<'s, 'g>) -> Result<(), StatementError<'s, 'g>> {
    // TODO: Implement. Do not forget about metatables.
    eprintln!("assign_var {:?} {:?}", var, val);
    Ok(())
}

fn assign<'s, 'g>(s: &Scope, vars: &[lua_syntax::Var<'s>], exps: Vec<Value<'s, 'g>>) -> Result<(), StatementError<'s ,'g>> {
    let exps = adjust_values(exps, vars.len());
    for (var, val) in vars.iter().zip(exps) {
        assign_var(s, var, val)?;
    }
    Ok(())
}

fn interpret_statement<'s, 'g>(s: &Scope, stmt: &lua_syntax::Stat<'s>) -> Result<(), StatementError<'s, 'g>> {
    match stmt {
        lua_syntax::Stat::Assign(vars, exps) => {
            let vals = interpret_exps(s, exps)?;
            assign(s, vars, vals)
        }
        lua_syntax::Stat::FunctionCall(fcall) => {
            function_call(s, fcall).map_err(StatementError::Error)?;
            Ok(())
        }
        lua_syntax::Stat::Block(block) => {
            let s = Scope::inherit(s);
            interpret_block(&s, block)
        }
        lua_syntax::Stat::While(exp, block) => {
            loop {
                if !adjust_values_1(interpret_exp(s, exp)?).to_bool() {
                    return Ok(())
                }
                let s = Scope::inherit(s);
                interpret_block(&s, block)?;
            }
        }
        lua_syntax::Stat::Repeat(block, exp) => {
            loop {
                let s = Scope::inherit(s);
                interpret_block(&s, block)?;
                if adjust_values_1(interpret_exp(&s, exp)?).to_bool() {
                    return Ok(())
                }
            }
        }
        lua_syntax::Stat::If(ifBlock, ifElseBlocks, elseBlock) => {
            let (e, block) = ifBlock;
            if adjust_values_1(interpret_exp(&s, e)?).to_bool() {
                let s = Scope::inherit(s);
                return interpret_block(&s, block);
            }
            for (e, block) in ifElseBlocks {
                if adjust_values_1(interpret_exp(&s, e)?).to_bool() {
                    let s = Scope::inherit(s);
                    return interpret_block(&s, block);
                }
            }
            if let Some(block) = elseBlock {
                let s = Scope::inherit(s);
                return interpret_block(&s, block);
            }
            Ok(())
        }
        lua_syntax::Stat::For(var, init, limit, step, block) => {
            let init = adjust_values_1(interpret_exp(&s, init)?).to_number().ok_or(StatementError::Error("Expected a number".to_string()))?;
            let limit = adjust_values_1(interpret_exp(&s, limit)?).to_number().ok_or(StatementError::Error("Expected a number".to_string()))?;
            let step = match step {
                Some(step) => adjust_values_1(interpret_exp(&s, step)?).to_number().ok_or(StatementError::Error("Expected a number".to_string()))?,
                None => 1 as Number,
            };
            let s = Scope::inherit(s);
            s.declare(var);
            let mut current_value = init;
            while current_value < limit {
                // TODO: More efficiently.
                let s = Scope::inherit(&s);
                assign(&s, &[lua_syntax::Var::Name(var)], vec![Value::from_number(current_value)])?;
                interpret_block(&s, block)?;
                current_value += step;
            }
            Ok(())
        }
        lua_syntax::Stat::ForIn(vars, exps, block) => {
            let mut pack = adjust_values(interpret_exps(s, exps)?, 3);
            let s = Scope::inherit(s);
            for var in vars {
                s.declare(var);
            }
            loop {
                let result = value_call(&s, &pack[0], &pack[1..]).map_err(StatementError::Error)?;
                if result.is_empty() {
                    return Ok(())
                }
                if result[0].is_nil() {
                    return Ok(())
                }
                pack[2] = result[0].clone();
                // TODO: More efficiently.
                let vars: Vec<_> = vars.into_iter().map(|n| lua_syntax::Var::Name(n)).collect();
                assign(&s, &vars, result)?;
                let s = Scope::inherit(&s);
                interpret_block(&s, block)?;
            }
        }
        lua_syntax::Stat::Function(lua_syntax::FuncName(name, fields, None), lua_syntax::FuncBody(pars, body)) => {
            let val = define_function(s, pars, body)?;
            let mut var = lua_syntax::Var::Name(name);
            for f in fields {
                var = lua_syntax::Var::Field(lua_syntax::PrefixExp::Var(Box::new(var)), f)
            }
            // TODO: More efficiently.
            assign(s, &[var], vec![val])
        }
        lua_syntax::Stat::Function(lua_syntax::FuncName(name, fields, Some(method)), lua_syntax::FuncBody(pars, body)) => {
            let pars = match pars {
                Some(lua_syntax::ParList::VarArgs) => Some(lua_syntax::ParList::ParList(vec!["self"], true)),
                Some(lua_syntax::ParList::ParList(names, hasVarArgs)) => {
                    let mut all_names= vec!["self"];
                    all_names.extend(names);
                    Some(lua_syntax::ParList::ParList(all_names, *hasVarArgs))
                },
                None => Some(lua_syntax::ParList::ParList(vec!["self"], false)),
            };
            let val = define_function(s, &pars, body)?;
            let mut var = lua_syntax::Var::Name(name);
            for f in fields {
                var = lua_syntax::Var::Field(lua_syntax::PrefixExp::Var(Box::new(var)), f)
            }
            var = lua_syntax::Var::Field(lua_syntax::PrefixExp::Var(Box::new(var)), method);
            // TODO: More efficiently.
            assign(s, &[var], vec![val])
        }
        lua_syntax::Stat::LocalFunctionDecl(name, lua_syntax::FuncBody(pars, body)) => {
            s.declare(name);
            let val = define_function(s, pars, body)?;
            // TODO: More efficiently.
            assign(s, &[lua_syntax::Var::Name(name)], vec![val])
        }
        lua_syntax::Stat::LocalVarDecl(names, exps) => {
            for name in names {
                s.declare(name);
            }
            if let Some(exps) = exps {
                let vals = interpret_exps(s, exps)?;
                // TODO: More efficiently.
                let names: Vec<_> = names.into_iter().map(|n| lua_syntax::Var::Name(n)).collect();
                assign(s, &names, vals)?;
            }
            Ok(())
        }
    }
}

fn interpret_last_statement<'s, 'g>(s: &Scope, stmt: &lua_syntax::LastStat<'s>) -> Result<(), StatementError<'s, 'g>> {
    match stmt {
        lua_syntax::LastStat::Return(Some(exps)) => {
            Err(StatementError::Return(interpret_exps(s, exps)?))
        },
        lua_syntax::LastStat::Return(None) => Err(StatementError::Return(vec![])),
        lua_syntax::LastStat::Break => Err(StatementError::Break),
    }
}

fn define_function<'s, 'g>(s: &Scope, pars: &Option<lua_syntax::ParList<'s>>, body: &lua_syntax::Block<'s>) -> Result<Value<'s, 'g>, StatementError<'s, 'g>> {
    eval_function(s, pars, body).map_err(StatementError::Error)
}

fn interpret_exps<'s, 'g>(s: &Scope, exps: &[lua_syntax::Exp<'s>]) -> Result<Vec<Value<'s, 'g>>, StatementError<'s, 'g>> {
    eval_exps(s, exps).map_err(StatementError::Error)
}

fn interpret_exp<'s, 'g>(s: &Scope, exp: &lua_syntax::Exp<'s>) -> Result<Vec<Value<'s, 'g>>, StatementError<'s, 'g>> {
    eval_exp(s, exp).map_err(StatementError::Error)
}

fn value_call<'s, 'g>(s: &Scope, f: &Value<'s, 'g>, args: &[Value<'s, 'g>]) -> Result<Vec<Value<'s, 'g>>, String> {
    // TODO: Implement.
    eprintln!("value_call {:?} {:?}", f, args);
    Ok(vec![])
}

fn function_call<'s, 'g>(s: &Scope, fcall: &lua_syntax::FunctionCall<'s>) -> Result<Vec<Value<'s, 'g>>, String> {
    /*
    let prepare_args = |args: &lua_syntax::Args<'a>| -> Result<Vec<Value<'a>>, String> {
        match args {
            lua_syntax::Args::Args(None) => Ok(vec![]),
            lua_syntax::Args::Args(Some(args)) => eval_exps(s, args),
            lua_syntax::Args::TableCtor(table_ctor) => {
                let val = eval_table_constructor(s, table_ctor)?;
                Ok(vec![val])
            },
            lua_syntax::Args::String(string) => Ok(vec![Value::StaticString(string.clone())]),
        }
    };
    match fcall {
        lua_syntax::FunctionCall::FunctionCall(f, args) => {
            let f = adjust_values_1(eval_prefixexp(s, f)?);
            let args = prepare_args(args)?;
            value_call(s, &f, &args)
        }
        lua_syntax::FunctionCall::MethodCall(f, method, args) => {
            let f_self = adjust_values_1(eval_prefixexp(s, f)?);
            let f = f_self.get_field(method);
            let mut args = prepare_args(args)?;
            args.insert(0, f_self);
            value_call(s, &f, &args)
        }
    }
    */
    // TODO: Implement.
    eprintln!("function_call {:?}", fcall);
    Ok(vec![])
}

fn eval_var<'s, 'g>(s: &Scope, var: &lua_syntax::Var<'s>) -> Result<Value<'s, 'g>, String> {
    // TODO: Implement.
    eprintln!("var {:?}", var);
    Ok(Value::from_nil())
}

fn eval_prefixexp<'s, 'g>(s: &Scope, exp: &lua_syntax::PrefixExp<'s>) -> Result<Vec<Value<'s, 'g>>, String> {
    match exp {
        lua_syntax::PrefixExp::Var(var) => {
            let result = eval_var(s, var)?;
            Ok(vec![result])
        },
        lua_syntax::PrefixExp::FunctionCall(fcall) => function_call(s, fcall),
        lua_syntax::PrefixExp::Parens(exp) => {
            let res = eval_exp(s, exp)?;
            Ok(adjust_values(res, 1))
        }
    }
}

fn eval_table_constructor<'s, 'g>(s: &Scope, table_ctor: &lua_syntax::TableCtor<'s>) -> Result<Value<'s, 'g>, String> {
    // TODO: Implement.
    eprintln!("table_ctor {:?}", table_ctor);
    Ok(Value::from_nil())
}

fn eval_binop<'s, 'g>(s: &Scope, op: lua_syntax::BinOp, lhs: &lua_syntax::Exp<'s>, rhs: &lua_syntax::Exp<'s>) -> Result<Vec<Value<'s, 'g>>, String> {
    // TODO: Implement.
    eprintln!("eval_binop {:?} {:?} {:?}", op, lhs, rhs);
    Ok(vec![])
}

fn eval_unop<'s, 'g>(s: &Scope, op: lua_syntax::UnOp, exp: &lua_syntax::Exp<'s>) -> Result<Vec<Value<'s, 'g>>, String> {
    /*
    let val = adjust_values_1(eval_exp(s, exp)?);
    match op {
        lua_syntax::UnOp::Unm => {
            if let Some(num) = val.to_number() {
                return Ok(vec![Value::Number(-num)])
            }
            let meta_unm = val.metadata().get_field("__unm");
            let result = value_call(s, &meta_unm, &[val])?;
            Ok(adjust_values(result, 1))
        },
        lua_syntax::UnOp::Not => {
            Ok(vec![Value::Bool(!val.to_bool())])
        },
        lua_syntax::UnOp::Len => {
            if let Some(len) = val.len() {
                return Ok(vec![Value::Number(len)])
            }
            let meta_len = val.metadata().get_field("__len");
            let result = value_call(s, &meta_len, &[val])?;
            Ok(adjust_values(result, 1))
        },
    }
    */
    // TODO: Implement.
    eprintln!("eval_unop {:?} {:?}", op, exp);
    Ok(vec![])
}

fn eval_varargs<'s, 'g>(s: &Scope) -> Result<Vec<Value<'s, 'g>>, String> {
    // TODO: Implement.
    eprintln!("eval_varargs");
    Ok(vec![])
}

fn eval_function<'s, 'g>(s: &Scope, pars: &Option<lua_syntax::ParList<'s>>, body: &lua_syntax::Block<'s>) -> Result<Value<'s, 'g>, String> {
    // TODO: Implement.
    eprintln!("define_function {:?} {:?}", pars, body);
    Ok(Value::from_nil())
}

fn eval_exp<'s, 'g>(s: &Scope, exp: &lua_syntax::Exp<'s>) -> Result<Vec<Value<'s, 'g>>, String> {
    match exp {
        lua_syntax::Exp::Nil => Ok(vec![Value::from_nil()]),
        lua_syntax::Exp::False => Ok(vec![Value::from_bool(false)]),
        lua_syntax::Exp::True => Ok(vec![Value::from_bool(true)]),
        lua_syntax::Exp::Number(n) => Ok(vec![Value::from_number(*n)]),
        lua_syntax::Exp::String(s) => Ok(vec![Value::from_string(value::String::from_string_literal(s.clone()))]),
        lua_syntax::Exp::VarArgs => eval_varargs(s),
        lua_syntax::Exp::Function(body) => {
            let res = eval_function(s, &body.0, &body.1)?;
            Ok(vec![res])
        },
        lua_syntax::Exp::PrefixExp(pe) => eval_prefixexp(s, pe),
        lua_syntax::Exp::TableCtor(table_ctor) => {
            let res = eval_table_constructor(s, table_ctor)?;
            Ok(vec![res])
        }
        lua_syntax::Exp::BinOp(lhs, op, rhs) => eval_binop(s, *op, lhs, rhs),
        lua_syntax::Exp::UnOp(op, e) => eval_unop(s, *op, e),
    }
}

fn eval_exps<'s, 'g>(s: &Scope, exps: &[lua_syntax::Exp<'s>]) -> Result<Vec<Value<'s, 'g>>, String> {
    let mut result = Vec::with_capacity(exps.len());
    for e in exps {
        result.extend(eval_exp(s, e)?);
    }
    Ok(result)
}
