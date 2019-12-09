use crate::lua_lexemes;
use crate::lua_syntax;
use crate::parser_lib;

pub fn run_parser<'a>(
    input: &'a [lua_lexemes::LocatedToken<'a>],
) -> Result<lua_syntax::Block<'a>, String> {
    trace_scoped!("lua_syntax run_parser");
    match parser_lib::run_parser(
        input.iter(),
        parser_lib::seq1(block_parser(), parser_lib::eof()),
    ) {
        (Some(result), _) => Ok(result),
        (None, mut iter) => match iter.next() {
            None => Err("Parser failed at EOF".to_string()),
            Some(token) => Err(format!("Parser failed at {}", token.location)),
        },
    }
}

fn keyword_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
    k: lua_lexemes::Keyword,
) -> parser_lib::Parser<'a, I, &'a lua_lexemes::LocatedToken<'a>> {
    trace_scoped!("lua_syntax keyword_parser_skeleton");
    parser_lib::satisfies(move |in_k: &&'a lua_lexemes::LocatedToken<'a>| {
        in_k.token == lua_lexemes::Token::Keyword(k)
    })
}

fn other_token_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
    t: lua_lexemes::OtherToken,
) -> parser_lib::Parser<'a, I, &'a lua_lexemes::LocatedToken<'a>> {
    trace_scoped!("lua_syntax other_token_parser_skeleton");
    parser_lib::satisfies(move |in_t: &&'a lua_lexemes::LocatedToken<'a>| {
        in_t.token == lua_lexemes::Token::OtherToken(t)
    })
}

fn name_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Name<'a>> {
    trace_scoped!("lua_syntax name_parser_skeleton");
    parser_lib::map_satisfies(|in_t: &'a lua_lexemes::LocatedToken<'a>| match in_t.token {
        lua_lexemes::Token::Identifier(i) => Some(i),
        _ => None,
    })
}

fn string_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::StringLiteral<'a>> {
    trace_scoped!("lua_syntax string_parser_skeleton");
    parser_lib::map_satisfies(
        |in_t: &'a lua_lexemes::LocatedToken<'a>| match &in_t.token {
            lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(s)) => Some(s.clone()),
            _ => None,
        },
    )
}

fn number_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::NumberLiteral> {
    trace_scoped!("lua_syntax number_parser_skeleton");
    parser_lib::map_satisfies(|in_t: &'a lua_lexemes::LocatedToken<'a>| match in_t.token {
        lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(n)) => Some(n),
        _ => None,
    })
}

fn block_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Block<'a>> {
    trace_scoped!("lua_syntax block_parser_skeleton");
    parser_lib::fmap(
        |(stats, last_stat)| lua_syntax::Block(stats, last_stat),
        parser_lib::seq(
            parser_lib::many(parser_lib::seq1(
                parser_lib::allow_recursion(stat_parser),
                parser_lib::try_parser(other_token_parser(lua_lexemes::OtherToken::Semi)),
            )),
            parser_lib::try_parser(parser_lib::seq1(
                laststat_parser(),
                parser_lib::try_parser(other_token_parser(lua_lexemes::OtherToken::Semi)),
            )),
        ),
    )
}

fn stat_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Stat<'a>> {
    trace_scoped!("lua_syntax stat_parser_skeleton");
    parser_lib::choices(vec![
        parser_lib::fmap(
            lua_syntax::Stat::Block,
            parser_lib::seq2(
                keyword_parser(lua_lexemes::Keyword::Do),
                parser_lib::seq1(block_parser(), keyword_parser(lua_lexemes::Keyword::End)),
            ),
        ),
        parser_lib::fmap(
            |(exp, block)| lua_syntax::Stat::While(exp, block),
            parser_lib::seq2(
                keyword_parser(lua_lexemes::Keyword::While),
                parser_lib::seq(
                    exp_parser(),
                    parser_lib::seq2(
                        keyword_parser(lua_lexemes::Keyword::Do),
                        parser_lib::seq1(block_parser(), keyword_parser(lua_lexemes::Keyword::End)),
                    ),
                ),
            ),
        ),
        parser_lib::fmap(
            |(block, exp)| lua_syntax::Stat::Repeat(block, exp),
            parser_lib::seq2(
                keyword_parser(lua_lexemes::Keyword::Repeat),
                parser_lib::seq(
                    block_parser(),
                    parser_lib::seq2(keyword_parser(lua_lexemes::Keyword::Until), exp_parser()),
                ),
            ),
        ),
        parser_lib::fmap(
            |(if_block, (elseifs, else_block))| lua_syntax::Stat::If(if_block, elseifs, else_block),
            parser_lib::seq(
                parser_lib::seq(
                    parser_lib::seq2(keyword_parser(lua_lexemes::Keyword::If), exp_parser()),
                    parser_lib::seq2(keyword_parser(lua_lexemes::Keyword::Then), block_parser()),
                ),
                parser_lib::seq1(
                    parser_lib::seq(
                        parser_lib::many(parser_lib::seq(
                            parser_lib::seq2(
                                keyword_parser(lua_lexemes::Keyword::Elseif),
                                exp_parser(),
                            ),
                            parser_lib::seq2(
                                keyword_parser(lua_lexemes::Keyword::Then),
                                block_parser(),
                            ),
                        )),
                        parser_lib::try_parser(parser_lib::seq2(
                            keyword_parser(lua_lexemes::Keyword::Else),
                            block_parser(),
                        )),
                    ),
                    keyword_parser(lua_lexemes::Keyword::End),
                ),
            ),
        ),
        parser_lib::fmap(
            |((name, (e1, (e2, e3))), block)| lua_syntax::Stat::For(name, e1, e2, e3, block),
            parser_lib::seq2(
                keyword_parser(lua_lexemes::Keyword::For),
                parser_lib::seq(
                    parser_lib::seq(
                        name_parser(),
                        parser_lib::seq2(
                            other_token_parser(lua_lexemes::OtherToken::Assign),
                            parser_lib::seq(
                                exp_parser(),
                                parser_lib::seq2(
                                    other_token_parser(lua_lexemes::OtherToken::Comma),
                                    parser_lib::seq(
                                        exp_parser(),
                                        parser_lib::try_parser(parser_lib::seq2(
                                            other_token_parser(lua_lexemes::OtherToken::Comma),
                                            exp_parser(),
                                        )),
                                    ),
                                ),
                            ),
                        ),
                    ),
                    parser_lib::seq2(
                        keyword_parser(lua_lexemes::Keyword::Do),
                        parser_lib::seq1(block_parser(), keyword_parser(lua_lexemes::Keyword::End)),
                    ),
                ),
            ),
        ),
        parser_lib::fmap(
            |((namelist, explist), block)| lua_syntax::Stat::ForIn(namelist, explist, block),
            parser_lib::seq2(
                keyword_parser(lua_lexemes::Keyword::For),
                parser_lib::seq(
                    parser_lib::seq(
                        namelist_parser(),
                        parser_lib::seq2(
                            keyword_parser(lua_lexemes::Keyword::In),
                            explist_parser(),
                        ),
                    ),
                    parser_lib::seq2(
                        keyword_parser(lua_lexemes::Keyword::Do),
                        parser_lib::seq1(block_parser(), keyword_parser(lua_lexemes::Keyword::End)),
                    ),
                ),
            ),
        ),
        parser_lib::fmap(
            |(funcname, funcbody)| lua_syntax::Stat::Function(funcname, funcbody),
            parser_lib::seq2(
                keyword_parser(lua_lexemes::Keyword::Function),
                parser_lib::seq(funcname_parser(), funcbody_parser()),
            ),
        ),
        parser_lib::fmap(
            |(name, funcbody)| lua_syntax::Stat::LocalFunctionDecl(name, funcbody),
            parser_lib::seq2(
                parser_lib::seq(
                    keyword_parser(lua_lexemes::Keyword::Local),
                    keyword_parser(lua_lexemes::Keyword::Function),
                ),
                parser_lib::seq(name_parser(), funcbody_parser()),
            ),
        ),
        parser_lib::fmap(
            |(namelist, explist)| lua_syntax::Stat::LocalVarDecl(namelist, explist),
            parser_lib::seq2(
                keyword_parser(lua_lexemes::Keyword::Local),
                parser_lib::seq(
                    namelist_parser(),
                    parser_lib::try_parser(parser_lib::seq2(
                        other_token_parser(lua_lexemes::OtherToken::Assign),
                        explist_parser(),
                    )),
                ),
            ),
        ),
        parser_lib::fmap(
            |(vl, el)| lua_syntax::Stat::Assign(vl, el),
            parser_lib::seq(
                varlist_parser(),
                parser_lib::seq2(
                    other_token_parser(lua_lexemes::OtherToken::Assign),
                    explist_parser(),
                ),
            ),
        ),
        parser_lib::fmap(lua_syntax::Stat::FunctionCall, functioncall_parser()),
    ])
}

fn laststat_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::LastStat<'a>> {
    trace_scoped!("lua_syntax laststat_parser_skeleton");
    parser_lib::choices(vec![
        parser_lib::fmap(
            lua_syntax::LastStat::Return,
            parser_lib::seq2(
                keyword_parser(lua_lexemes::Keyword::Return),
                parser_lib::try_parser(explist_parser()),
            ),
        ),
        parser_lib::fmap(
            |_| lua_syntax::LastStat::Break,
            keyword_parser(lua_lexemes::Keyword::Break),
        ),
    ])
}

fn funcname_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::FuncName<'a>> {
    trace_scoped!("lua_syntax funcname_parser_skeleton");
    parser_lib::fmap(
        |(n1, (n2, n3))| lua_syntax::FuncName(n1, n2, n3),
        parser_lib::seq(
            name_parser(),
            parser_lib::seq(
                parser_lib::many(parser_lib::seq2(
                    other_token_parser(lua_lexemes::OtherToken::Period),
                    name_parser(),
                )),
                parser_lib::try_parser(parser_lib::seq2(
                    other_token_parser(lua_lexemes::OtherToken::Colon),
                    name_parser(),
                )),
            ),
        ),
    )
}

fn varlist_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::NonEmptyVec<lua_syntax::Var<'a>>> {
    trace_scoped!("lua_syntax varlist_parser_skeleton");
    parser_lib::separated(
        var_parser(),
        other_token_parser(lua_lexemes::OtherToken::Comma),
        false,
    )
}

fn var_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Var<'a>> {
    trace_scoped!("lua_syntax var_parser_skeleton");
    parser_lib::map_filter(
        |pe| match pe {
            lua_syntax::PrefixExp::Var(var) => Some(*var),
            _ => None,
        },
        prefixexp_parser(),
    )
}

fn namelist_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::NonEmptyVec<lua_syntax::Name<'a>>> {
    trace_scoped!("lua_syntax namelist_parser_skeleton");
    parser_lib::separated(
        name_parser(),
        other_token_parser(lua_lexemes::OtherToken::Comma),
        false,
    )
}

fn explist_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::NonEmptyVec<lua_syntax::Exp<'a>>> {
    trace_scoped!("lua_syntax explist_parser_skeleton");
    parser_lib::separated(
        exp_parser(),
        other_token_parser(lua_lexemes::OtherToken::Comma),
        false,
    )
}

fn exp0_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp0_parser_skeleton");
    parser_lib::trace(
        "exp0_parser",
        parser_lib::choices(vec![
            parser_lib::fmap(
                |_| lua_syntax::Exp::Nil,
                keyword_parser(lua_lexemes::Keyword::Nil),
            ),
            parser_lib::fmap(
                |_| lua_syntax::Exp::False,
                keyword_parser(lua_lexemes::Keyword::False),
            ),
            parser_lib::fmap(
                |_| lua_syntax::Exp::True,
                keyword_parser(lua_lexemes::Keyword::True),
            ),
            parser_lib::fmap(lua_syntax::Exp::Number, number_parser()),
            parser_lib::fmap(lua_syntax::Exp::String, string_parser()),
            parser_lib::fmap(
                |_| lua_syntax::Exp::VarArgs,
                other_token_parser(lua_lexemes::OtherToken::Vararg),
            ),
            parser_lib::fmap(lua_syntax::Exp::TableCtor, tableconstructor_parser()),
            parser_lib::fmap(
                |x| lua_syntax::Exp::Function(Box::new(x)),
                parser_lib::allow_recursion(function_parser),
            ),
            parser_lib::fmap(lua_syntax::Exp::PrefixExp, prefixexp_parser()),
        ]),
    )
}

fn exp1_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp1_parser_skeleton");
    let op_parser = parser_lib::choices(vec![parser_lib::fmap(
        |_| lua_syntax::BinOp::Pow,
        other_token_parser(lua_lexemes::OtherToken::Pow),
    )]);
    let create_exp = |(lhs, rhss): (
        lua_syntax::Exp<'a>,
        Vec<(lua_syntax::BinOp, lua_syntax::Exp<'a>)>,
    )| {
        if rhss.is_empty() {
            return lhs;
        }
        let mut it = rhss.into_iter().rev();
        let (mut last_op, mut last_rhs) = it.next().unwrap();
        for (op, rhs) in it {
            last_rhs = lua_syntax::Exp::BinOp(Box::new(rhs), last_op, Box::new(last_rhs));
            last_op = op;
        }
        lua_syntax::Exp::BinOp(Box::new(lhs), last_op, Box::new(last_rhs))
    };
    parser_lib::trace(
        "exp1_parser",
        parser_lib::fmap(
            create_exp,
            parser_lib::seq(
                exp0_parser(),
                // Special rule for allowing unary operators on rhs of ^.
                parser_lib::many(parser_lib::seq(
                    op_parser,
                    parser_lib::allow_recursion(exp2_parser),
                )),
            ),
        ),
    )
}

fn exp2_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp2_parser_skeleton");
    let op_parser = parser_lib::choices(vec![
        parser_lib::fmap(
            |_| lua_syntax::UnOp::Not,
            keyword_parser(lua_lexemes::Keyword::Not),
        ),
        parser_lib::fmap(
            |_| lua_syntax::UnOp::Len,
            other_token_parser(lua_lexemes::OtherToken::Len),
        ),
        parser_lib::fmap(
            |_| lua_syntax::UnOp::Unm,
            other_token_parser(lua_lexemes::OtherToken::SubOrUnm),
        ),
    ]);
    let create_exp = |(ops, arg): (Vec<lua_syntax::UnOp>, lua_syntax::Exp<'a>)| {
        let mut result = arg;
        for op in ops.into_iter().rev() {
            result = lua_syntax::Exp::UnOp(op, Box::new(result))
        }
        result
    };
    parser_lib::trace(
        "exp2_parser",
        parser_lib::fmap(
            create_exp,
            parser_lib::seq(parser_lib::many(op_parser), exp1_parser()),
        ),
    )
}

fn exp3_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp3_parser_skeleton");
    let op_parser = parser_lib::choices(vec![
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Mul,
            other_token_parser(lua_lexemes::OtherToken::Mul),
        ),
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Div,
            other_token_parser(lua_lexemes::OtherToken::Div),
        ),
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Mod,
            other_token_parser(lua_lexemes::OtherToken::Mod),
        ),
    ]);
    let create_exp = |(lhs, rhss): (
        lua_syntax::Exp<'a>,
        Vec<(lua_syntax::BinOp, lua_syntax::Exp<'a>)>,
    )| {
        let mut result = lhs;
        for (op, rhs) in rhss {
            result = lua_syntax::Exp::BinOp(Box::new(result), op, Box::new(rhs))
        }
        result
    };
    let prev_parser = exp2_parser();
    parser_lib::trace(
        "exp3_parser",
        parser_lib::fmap(
            create_exp,
            parser_lib::seq(
                prev_parser.clone(),
                parser_lib::many(parser_lib::seq(op_parser, prev_parser)),
            ),
        ),
    )
}

fn exp4_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp4_parser_skeleton");
    let op_parser = parser_lib::choices(vec![
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Add,
            other_token_parser(lua_lexemes::OtherToken::Add),
        ),
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Sub,
            other_token_parser(lua_lexemes::OtherToken::SubOrUnm),
        ),
    ]);
    let create_exp = |(lhs, rhss): (
        lua_syntax::Exp<'a>,
        Vec<(lua_syntax::BinOp, lua_syntax::Exp<'a>)>,
    )| {
        let mut result = lhs;
        for (op, rhs) in rhss {
            result = lua_syntax::Exp::BinOp(Box::new(result), op, Box::new(rhs))
        }
        result
    };
    let prev_parser = exp3_parser();
    parser_lib::trace(
        "exp4_parser",
        parser_lib::fmap(
            create_exp,
            parser_lib::seq(
                prev_parser.clone(),
                parser_lib::many(parser_lib::seq(op_parser, prev_parser)),
            ),
        ),
    )
}

fn exp5_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp5_parser_skeleton");
    let op_parser = parser_lib::choices(vec![parser_lib::fmap(
        |_| lua_syntax::BinOp::Concat,
        other_token_parser(lua_lexemes::OtherToken::Concat),
    )]);
    let create_exp = |(lhs, rhss): (
        lua_syntax::Exp<'a>,
        Vec<(lua_syntax::BinOp, lua_syntax::Exp<'a>)>,
    )| {
        if rhss.is_empty() {
            return lhs;
        }
        let mut it = rhss.into_iter().rev();
        let (mut last_op, mut last_rhs) = it.next().unwrap();
        for (op, rhs) in it {
            last_rhs = lua_syntax::Exp::BinOp(Box::new(rhs), last_op, Box::new(last_rhs));
            last_op = op;
        }
        lua_syntax::Exp::BinOp(Box::new(lhs), last_op, Box::new(last_rhs))
    };
    let prev_parser = exp4_parser();
    parser_lib::trace(
        "exp5_parser",
        parser_lib::fmap(
            create_exp,
            parser_lib::seq(
                prev_parser.clone(),
                parser_lib::many(parser_lib::seq(op_parser, prev_parser)),
            ),
        ),
    )
}

fn exp6_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp6_parser_skeleton");
    let op_parser = parser_lib::choices(vec![
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Lt,
            other_token_parser(lua_lexemes::OtherToken::Lt),
        ),
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Gt,
            other_token_parser(lua_lexemes::OtherToken::Gt),
        ),
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Le,
            other_token_parser(lua_lexemes::OtherToken::Le),
        ),
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Ge,
            other_token_parser(lua_lexemes::OtherToken::Ge),
        ),
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Eq,
            other_token_parser(lua_lexemes::OtherToken::Eq),
        ),
        parser_lib::fmap(
            |_| lua_syntax::BinOp::Neq,
            other_token_parser(lua_lexemes::OtherToken::Neq),
        ),
    ]);
    let create_exp = |(lhs, rhss): (
        lua_syntax::Exp<'a>,
        Vec<(lua_syntax::BinOp, lua_syntax::Exp<'a>)>,
    )| {
        let mut result = lhs;
        for (op, rhs) in rhss {
            result = lua_syntax::Exp::BinOp(Box::new(result), op, Box::new(rhs))
        }
        result
    };
    let prev_parser = exp5_parser();
    parser_lib::trace(
        "exp6_parser",
        parser_lib::fmap(
            create_exp,
            parser_lib::seq(
                prev_parser.clone(),
                parser_lib::many(parser_lib::seq(op_parser, prev_parser)),
            ),
        ),
    )
}

fn exp7_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp7_parser_skeleton");
    let op_parser = parser_lib::choices(vec![parser_lib::fmap(
        |_| lua_syntax::BinOp::And,
        keyword_parser(lua_lexemes::Keyword::And),
    )]);
    let create_exp = |(lhs, rhss): (
        lua_syntax::Exp<'a>,
        Vec<(lua_syntax::BinOp, lua_syntax::Exp<'a>)>,
    )| {
        let mut result = lhs;
        for (op, rhs) in rhss {
            result = lua_syntax::Exp::BinOp(Box::new(result), op, Box::new(rhs))
        }
        result
    };
    let prev_parser = exp6_parser();
    parser_lib::trace(
        "exp7_parser",
        parser_lib::fmap(
            create_exp,
            parser_lib::seq(
                prev_parser.clone(),
                parser_lib::many(parser_lib::seq(op_parser, prev_parser)),
            ),
        ),
    )
}

fn exp8_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp8_parser_skeleton");
    let op_parser = parser_lib::choices(vec![parser_lib::fmap(
        |_| lua_syntax::BinOp::Or,
        keyword_parser(lua_lexemes::Keyword::Or),
    )]);
    let create_exp = |(lhs, rhss): (
        lua_syntax::Exp<'a>,
        Vec<(lua_syntax::BinOp, lua_syntax::Exp<'a>)>,
    )| {
        let mut result = lhs;
        for (op, rhs) in rhss {
            result = lua_syntax::Exp::BinOp(Box::new(result), op, Box::new(rhs))
        }
        result
    };
    let prev_parser = exp7_parser();
    parser_lib::trace(
        "exp8_parser",
        parser_lib::fmap(
            create_exp,
            parser_lib::seq(
                prev_parser.clone(),
                parser_lib::many(parser_lib::seq(op_parser, prev_parser)),
            ),
        ),
    )
}

fn exp_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Exp<'a>> {
    trace_scoped!("lua_syntax exp_parser_skeleton");
    // `exp_parser` is called from many places. Instead of calling
    // `exp8_parser` (and constructing it's skeleton here), `allow_recursion` moves the
    // work to parsing stage (which improves performance overall).
    parser_lib::trace("exp_parser", parser_lib::allow_recursion(exp8_parser))
}

// We're working with this grammar:
// var ::=  Name | prefixexp `[´ exp `]´ | prefixexp `.´ Name
// functioncall ::=  prefixexp args | prefixexp `:´ Name args
// prefixexp ::= var | functioncall | `(´ exp `)´
// In order to eliminate left recursion, we order these 3 rules,
// disallow the lower one to reference the upper one in left recursive position and
// remove left recursion altogether.
// `var` and `functioncall` are fine for now.
// After inlining `var` and `functioncall`, `prefixexp` becomes:
// prefixexp ::= Name
//             | prefixexp `[´ exp `]´
//             | prefixexp `.´ Name
//             | prefixexp args
//             | prefixexp `:´ Name args
//             | `(´ exp `)´
// And after left recursion elimination:
// prefixexp_pre ::= Name
//                 | `(´ exp `)´
// prefixexp ::= prefixexp_pre prefixexp_post
// prefixexp_post ::= `[´ exp `]´ prefixexp_post
//                  | `.´ Name prefixexp_post
//                  | args prefixexp_post
//                  | `:´ Name args prefixexp_post
//                  | <> (empty parser)
// Now, `var` and `functioncall` can actually just reuse `prefixexp` and
// look at it's result. If it parsed `PrefixExp::Var` then `var` can succeed,
// if it parsed `PrefixExp::FunctionCall`, then `functioncall` can succeed.
fn prefixexp_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::PrefixExp<'a>> {
    trace_scoped!("lua_syntax prefixexp_parser_skeleton");
    let prefixexp_pre = parser_lib::choices(vec![
        parser_lib::fmap(
            |x| lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name(x))),
            name_parser(),
        ),
        parser_lib::fmap(
            |x| lua_syntax::PrefixExp::Exp(Box::new(x)),
            parser_lib::seq2(
                other_token_parser(lua_lexemes::OtherToken::LParen),
                parser_lib::seq1(
                    parser_lib::allow_recursion(exp_parser),
                    other_token_parser(lua_lexemes::OtherToken::RParen),
                ),
            ),
        ),
    ]);
    let prefixexp_post = parser_lib::choices(vec![
            parser_lib::fmap(
                |e| -> Box<
                    dyn FnOnce(lua_syntax::PrefixExp<'a>) -> lua_syntax::PrefixExp<'a> + 'a,
                > {
                    Box::new(move |pe| {
                        lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Index(pe, e)))
                    })
                },
                parser_lib::seq2(
                    other_token_parser(lua_lexemes::OtherToken::LBracket),
                    parser_lib::seq1(
                        parser_lib::allow_recursion(exp_parser),
                        other_token_parser(lua_lexemes::OtherToken::RBracket),
                    ),
                ),
            ),
            parser_lib::fmap(
                |n| -> Box<
                    dyn FnOnce(lua_syntax::PrefixExp<'a>) -> lua_syntax::PrefixExp<'a> + 'a,
                > {
                    Box::new(move |pe| {
                        lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Field(pe, n)))
                    })
                },
                parser_lib::seq2(
                    other_token_parser(lua_lexemes::OtherToken::Period),
                    name_parser(),
                ),
            ),
            parser_lib::fmap(
                |args| -> Box<
                    dyn FnOnce(lua_syntax::PrefixExp<'a>) -> lua_syntax::PrefixExp<'a> + 'a,
                > {
                    Box::new(move |pe| {
                        lua_syntax::PrefixExp::FunctionCall(Box::new(
                            lua_syntax::FunctionCall::FunctionCall(pe, args),
                        ))
                    })
                },
                parser_lib::allow_recursion(args_parser),
            ),
            parser_lib::fmap(
                |(m, args)| -> Box<
                    dyn FnOnce(lua_syntax::PrefixExp<'a>) -> lua_syntax::PrefixExp<'a> + 'a,
                > {
                    Box::new(move |pe| {
                        lua_syntax::PrefixExp::FunctionCall(Box::new(
                            lua_syntax::FunctionCall::MethodCall(pe, m, args),
                        ))
                    })
                },
                parser_lib::seq(
                    parser_lib::seq2(
                        other_token_parser(lua_lexemes::OtherToken::Colon),
                        name_parser(),
                    ),
                    parser_lib::allow_recursion(args_parser),
                ),
            ),
        ]);
    type PrefixExpTransformer<'a> =
        Box<dyn FnOnce(lua_syntax::PrefixExp<'a>) -> lua_syntax::PrefixExp<'a> + 'a>;
    let create_prefixexp =
        |(lhs, rhss): (lua_syntax::PrefixExp<'a>, Vec<PrefixExpTransformer<'a>>)| {
            let mut result = lhs;
            for rhs in rhss {
                result = rhs(result);
            }
            result
        };
    parser_lib::trace(
        "prefixexp_parser",
        parser_lib::fmap(
            create_prefixexp,
            parser_lib::seq(prefixexp_pre, parser_lib::many(prefixexp_post)),
        ),
    )
}

fn functioncall_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::FunctionCall<'a>> {
    trace_scoped!("lua_syntax functioncall_parser_skeleton");
    parser_lib::map_filter(
        |pe| match pe {
            lua_syntax::PrefixExp::FunctionCall(fcall) => Some(*fcall),
            _ => None,
        },
        prefixexp_parser(),
    )
}

fn args_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Args<'a>> {
    trace_scoped!("lua_syntax args_parser_skeleton");
    parser_lib::choices(vec![
        parser_lib::fmap(lua_syntax::Args::String, string_parser()),
        parser_lib::fmap(
            lua_syntax::Args::Args,
            parser_lib::seq2(
                other_token_parser(lua_lexemes::OtherToken::LParen),
                parser_lib::seq1(
                    parser_lib::try_parser(explist_parser()),
                    other_token_parser(lua_lexemes::OtherToken::RParen),
                ),
            ),
        ),
        parser_lib::fmap(lua_syntax::Args::TableCtor, tableconstructor_parser()),
    ])
}

fn function_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::FuncBody<'a>> {
    trace_scoped!("lua_syntax function_parser_skeleton");
    parser_lib::seq2(
        keyword_parser(lua_lexemes::Keyword::Function),
        funcbody_parser(),
    )
}

fn funcbody_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::FuncBody<'a>> {
    trace_scoped!("lua_syntax funcbody_parser_skeleton");
    parser_lib::fmap(
        |(parlist, block)| lua_syntax::FuncBody(parlist, block),
        parser_lib::seq(
            parser_lib::seq2(
                other_token_parser(lua_lexemes::OtherToken::LParen),
                parser_lib::seq1(
                    parser_lib::try_parser(parlist_parser()),
                    other_token_parser(lua_lexemes::OtherToken::RParen),
                ),
            ),
            parser_lib::seq1(block_parser(), keyword_parser(lua_lexemes::Keyword::End)),
        ),
    )
}

fn parlist_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::ParList<'a>> {
    trace_scoped!("lua_syntax parlist_parser_skeleton");
    parser_lib::choices(vec![
        parser_lib::fmap(
            |(namelist, varargs)| lua_syntax::ParList::ParList(namelist, varargs.is_some()),
            parser_lib::seq(
                namelist_parser(),
                parser_lib::try_parser(parser_lib::seq_(
                    other_token_parser(lua_lexemes::OtherToken::Comma),
                    other_token_parser(lua_lexemes::OtherToken::Vararg),
                )),
            ),
        ),
        parser_lib::fmap(
            |_| lua_syntax::ParList::VarArgs,
            other_token_parser(lua_lexemes::OtherToken::Vararg),
        ),
    ])
}

fn tableconstructor_parser<
    'a,
    I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a,
>() -> parser_lib::Parser<'a, I, lua_syntax::TableCtor<'a>> {
    trace_scoped!("lua_syntax tableconstructor_parser_skeleton");
    parser_lib::fmap(
        lua_syntax::TableCtor,
        parser_lib::seq2(
            other_token_parser(lua_lexemes::OtherToken::LBrace),
            parser_lib::seq1(
                parser_lib::try_parser(parser_lib::allow_recursion(fieldlist_parser)),
                other_token_parser(lua_lexemes::OtherToken::RBrace),
            ),
        ),
    )
}

fn fieldlist_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::NonEmptyVec<lua_syntax::Field<'a>>> {
    trace_scoped!("lua_syntax fieldlist_parser_skeleton");
    parser_lib::separated(field_parser(), fieldsep_parser(), true)
}

fn field_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, lua_syntax::Field<'a>> {
    trace_scoped!("lua_syntax field_parser_skeleton");
    parser_lib::choices(vec![
        parser_lib::fmap(
            |(e1, e2)| lua_syntax::Field::Field(e1, e2),
            parser_lib::seq(
                parser_lib::seq2(
                    other_token_parser(lua_lexemes::OtherToken::LBracket),
                    parser_lib::seq1(
                        exp_parser(),
                        other_token_parser(lua_lexemes::OtherToken::RBracket),
                    ),
                ),
                parser_lib::seq2(
                    other_token_parser(lua_lexemes::OtherToken::Assign),
                    exp_parser(),
                ),
            ),
        ),
        parser_lib::fmap(
            |(n, e)| lua_syntax::Field::NameField(n, e),
            parser_lib::seq(
                name_parser(),
                parser_lib::seq2(
                    other_token_parser(lua_lexemes::OtherToken::Assign),
                    exp_parser(),
                ),
            ),
        ),
        parser_lib::fmap(lua_syntax::Field::NumberField, exp_parser()),
    ])
}

fn fieldsep_parser<'a, I: Iterator<Item = &'a lua_lexemes::LocatedToken<'a>> + Clone + 'a>(
) -> parser_lib::Parser<'a, I, &'a lua_lexemes::LocatedToken<'a>> {
    trace_scoped!("lua_syntax fieldsep_parser_skeleton");
    parser_lib::choice(
        other_token_parser(lua_lexemes::OtherToken::Comma),
        other_token_parser(lua_lexemes::OtherToken::Semi),
    )
}

#[cfg(test)]
mod tests;
