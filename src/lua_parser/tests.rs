#![cfg_attr(feature = "cargo-clippy", allow(clippy::needless_lifetimes))]

use super::*;

fn name<'a>(n: &'a str) -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Identifier(n)
}

fn number<'a>(n: f64) -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(n))
}

fn string<'a>(s: String) -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(s))
}

fn period<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Period)
}

fn colon<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Colon)
}

fn comma<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Comma)
}

fn semi<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Semi)
}

fn varargs<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Vararg)
}

fn l_bracket<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::LBracket)
}

fn r_bracket<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::RBracket)
}

fn l_brace<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::LBrace)
}

fn r_brace<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::RBrace)
}

fn l_paren<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::LParen)
}

fn r_paren<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::RParen)
}

fn assign<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Assign)
}

fn plus<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Add)
}

fn times<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Mul)
}

fn pow<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Pow)
}

fn minus<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::SubOrUnm)
}

fn lt<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Lt)
}

fn le<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Le)
}

fn gt<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Gt)
}

fn ge<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Ge)
}

fn eq<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Eq)
}

fn neq<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Neq)
}

fn concat<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Concat)
}

fn divide<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Div)
}

fn modulo<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Mod)
}

fn len<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Len)
}

fn k_nil<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Nil)
}

fn k_true<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::True)
}

fn k_false<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::False)
}

fn k_break<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Break)
}

fn k_return<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Return)
}

fn k_do<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Do)
}

fn k_end<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::End)
}

fn k_while<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::While)
}

fn k_repeat<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Repeat)
}

fn k_until<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Until)
}

fn k_for<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::For)
}

fn k_in<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::In)
}

fn k_function<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Function)
}

fn k_local<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Local)
}

fn k_if<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::If)
}

fn k_then<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Then)
}

fn k_elseif<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Elseif)
}

fn k_else<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Else)
}

fn k_and<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::And)
}

fn k_or<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Or)
}

fn k_not<'a>() -> lua_lexemes::Token<'a> {
    lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Not)
}

fn run_parser<'a, I: Iterator, T>(
    iterator: I,
    p: Box<dyn parser_lib::Parser<I, T> + 'a>,
) -> Option<T> {
    parser_lib::run_parser(iterator, parser_lib::seq1(p, parser_lib::eof()))
}

#[test]
fn test_funcname_parser() {
    assert_eq!(
        run_parser(vec![name("abc")].iter(), funcname_parser()),
        Some(lua_syntax::FuncName("abc", vec![], None))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), period(), name("def"), period(), name("ghi")].iter(),
            funcname_parser()
        ),
        Some(lua_syntax::FuncName("abc", vec!["def", "ghi"], None))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                period(),
                name("def"),
                period(),
                name("ghi"),
                colon(),
                name("xyz")
            ]
            .iter(),
            funcname_parser()
        ),
        Some(lua_syntax::FuncName("abc", vec!["def", "ghi"], Some("xyz")))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), colon(), name("xyz")].iter(),
            funcname_parser()
        ),
        Some(lua_syntax::FuncName("abc", vec![], Some("xyz")))
    );

    assert_eq!(
        run_parser(vec![name("abc"), period()].iter(), funcname_parser()),
        None
    );
    assert_eq!(
        run_parser(vec![name("abc"), colon()].iter(), funcname_parser()),
        None
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), period(), colon(), name("xyz")].iter(),
            funcname_parser()
        ),
        None
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), colon(), period(), name("xyz")].iter(),
            funcname_parser()
        ),
        None
    );
    assert_eq!(run_parser(vec![period()].iter(), funcname_parser()), None);
    assert_eq!(run_parser(vec![colon()].iter(), funcname_parser()), None);
    assert_eq!(
        run_parser(
            vec![period(), colon(), name("xyz")].iter(),
            funcname_parser()
        ),
        None
    );
    assert_eq!(
        run_parser(
            vec![colon(), period(), name("xyz")].iter(),
            funcname_parser()
        ),
        None
    );
    assert_eq!(
        run_parser(vec![period(), name("xyz")].iter(), funcname_parser()),
        None
    );
    assert_eq!(
        run_parser(vec![colon(), name("xyz")].iter(), funcname_parser()),
        None
    );
}

#[test]
fn test_namelist_parser() {
    assert_eq!(run_parser(vec![].iter(), namelist_parser()), None);
    assert_eq!(
        run_parser(
            vec![name("abc"), comma(), name("def")].iter(),
            namelist_parser()
        ),
        Some(vec!["abc", "def"])
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), comma(), name("def"), comma(), name("ghi")].iter(),
            namelist_parser()
        ),
        Some(vec!["abc", "def", "ghi"])
    );
    assert_eq!(run_parser(vec![comma()].iter(), namelist_parser()), None);
    assert_eq!(
        run_parser(vec![name("abc"), comma()].iter(), namelist_parser()),
        None
    );
    assert_eq!(
        run_parser(vec![comma(), name("abc")].iter(), namelist_parser()),
        None
    );
}

#[test]
fn test_parlist_parser() {
    assert_eq!(run_parser(vec![].iter(), parlist_parser()), None);
    assert_eq!(
        run_parser(vec![varargs()].iter(), parlist_parser()),
        Some(lua_syntax::ParList::VarArgs)
    );
    assert_eq!(
        run_parser(vec![name("abc")].iter(), parlist_parser()),
        Some(lua_syntax::ParList::ParList(vec!["abc"], false))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), comma(), varargs()].iter(),
            parlist_parser()
        ),
        Some(lua_syntax::ParList::ParList(vec!["abc"], true))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), comma(), name("def")].iter(),
            parlist_parser()
        ),
        Some(lua_syntax::ParList::ParList(vec!["abc", "def"], false))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), comma(), name("def"), comma(), varargs()].iter(),
            parlist_parser()
        ),
        Some(lua_syntax::ParList::ParList(vec!["abc", "def"], true))
    );

    assert_eq!(
        run_parser(vec![comma(), varargs()].iter(), parlist_parser()),
        None
    );
    assert_eq!(
        run_parser(vec![name("abc"), varargs()].iter(), parlist_parser()),
        None
    );
    assert_eq!(
        run_parser(vec![name("abc"), name("def")].iter(), parlist_parser()),
        None
    );
}

#[test]
fn test_exp_parser_literals() {
    assert_eq!(
        run_parser(vec![k_nil()].iter(), exp_parser()),
        Some(lua_syntax::Exp::Nil)
    );
    assert_eq!(
        run_parser(vec![k_false()].iter(), exp_parser()),
        Some(lua_syntax::Exp::False)
    );
    assert_eq!(
        run_parser(vec![k_true()].iter(), exp_parser()),
        Some(lua_syntax::Exp::True)
    );
    assert_eq!(
        run_parser(vec![number(42.0)].iter(), exp_parser()),
        Some(lua_syntax::Exp::Number(42.0))
    );
    assert_eq!(
        run_parser(vec![string("abc".to_string())].iter(), exp_parser()),
        Some(lua_syntax::Exp::String("abc".to_string()))
    );
    assert_eq!(
        run_parser(vec![varargs()].iter(), exp_parser()),
        Some(lua_syntax::Exp::VarArgs)
    );
}

#[test]
fn test_field_parser() {
    assert_eq!(
        run_parser(
            vec![l_bracket(), k_true(), r_bracket(), assign(), k_nil()].iter(),
            field_parser()
        ),
        Some(lua_syntax::Field::Field(
            lua_syntax::Exp::True,
            lua_syntax::Exp::Nil
        ))
    );
    assert_eq!(
        run_parser(vec![name("abc"), assign(), k_nil()].iter(), field_parser()),
        Some(lua_syntax::Field::NameField("abc", lua_syntax::Exp::Nil))
    );
    assert_eq!(
        run_parser(vec![k_nil()].iter(), field_parser()),
        Some(lua_syntax::Field::NumberField(lua_syntax::Exp::Nil))
    );
}

#[test]
fn test_tableconstructor_parser() {
    assert_eq!(
        run_parser(vec![l_brace(), r_brace()].iter(), tableconstructor_parser()),
        Some(lua_syntax::TableCtor(None))
    );
    assert_eq!(
        run_parser(
            vec![
                l_brace(),
                l_bracket(),
                k_true(),
                r_bracket(),
                assign(),
                k_nil(),
                comma(),
                k_nil(),
                semi(),
                k_false(),
                comma(),
                r_brace()
            ]
            .iter(),
            tableconstructor_parser()
        ),
        Some(lua_syntax::TableCtor(Some(vec![
            lua_syntax::Field::Field(lua_syntax::Exp::True, lua_syntax::Exp::Nil),
            lua_syntax::Field::NumberField(lua_syntax::Exp::Nil),
            lua_syntax::Field::NumberField(lua_syntax::Exp::False)
        ])))
    );

    assert_eq!(
        run_parser(vec![l_brace(), r_brace()].iter(), exp_parser()),
        Some(lua_syntax::Exp::TableCtor(lua_syntax::TableCtor(None)))
    );
    assert_eq!(
        run_parser(
            vec![
                l_brace(),
                l_bracket(),
                k_true(),
                r_bracket(),
                assign(),
                k_nil(),
                comma(),
                k_nil(),
                semi(),
                k_false(),
                comma(),
                r_brace()
            ]
            .iter(),
            exp_parser()
        ),
        Some(lua_syntax::Exp::TableCtor(lua_syntax::TableCtor(Some(
            vec![
                lua_syntax::Field::Field(lua_syntax::Exp::True, lua_syntax::Exp::Nil),
                lua_syntax::Field::NumberField(lua_syntax::Exp::Nil),
                lua_syntax::Field::NumberField(lua_syntax::Exp::False)
            ]
        ))))
    );
}

#[test]
fn test_explist_parser() {
    assert_eq!(run_parser(vec![].iter(), explist_parser()), None);
    assert_eq!(
        run_parser(vec![k_true(), comma(), k_false()].iter(), explist_parser()),
        Some(vec![lua_syntax::Exp::True, lua_syntax::Exp::False])
    );
    assert_eq!(
        run_parser(
            vec![k_true(), comma(), k_false(), comma(), k_nil()].iter(),
            explist_parser()
        ),
        Some(vec![
            lua_syntax::Exp::True,
            lua_syntax::Exp::False,
            lua_syntax::Exp::Nil
        ])
    );
}

#[test]
fn test_laststat_parser() {
    assert_eq!(
        run_parser(vec![k_break()].iter(), laststat_parser()),
        Some(lua_syntax::LastStat::Break)
    );
    assert_eq!(
        run_parser(
            vec![k_return(), k_false(), comma(), k_true()].iter(),
            laststat_parser()
        ),
        Some(lua_syntax::LastStat::Return(Some(vec![
            lua_syntax::Exp::False,
            lua_syntax::Exp::True
        ])))
    );
    assert_eq!(
        run_parser(vec![k_return()].iter(), laststat_parser()),
        Some(lua_syntax::LastStat::Return(None))
    );
}

#[test]
fn test_stat_parser1() {
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                comma(),
                name("def"),
                assign(),
                k_true(),
                comma(),
                k_false(),
                comma(),
                k_nil()
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::Assign(
            vec![lua_syntax::Var::Name("abc"), lua_syntax::Var::Name("def")],
            vec![
                lua_syntax::Exp::True,
                lua_syntax::Exp::False,
                lua_syntax::Exp::Nil
            ]
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), l_paren(), k_true(), r_paren()].iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::FunctionCall(
            lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(Some(vec![lua_syntax::Exp::True]))
            )
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                k_do(),
                name("abc"),
                assign(),
                k_true(),
                semi(),
                k_return(),
                semi(),
                k_end()
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::Block(lua_syntax::Block(
            vec![lua_syntax::Stat::Assign(
                vec![lua_syntax::Var::Name("abc")],
                vec![lua_syntax::Exp::True]
            )],
            Some(lua_syntax::LastStat::Return(None))
        )))
    );
    assert_eq!(
        run_parser(
            vec![k_while(), k_true(), k_do(), k_break(), k_end()].iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::While(
            lua_syntax::Exp::True,
            lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break))
        ))
    );
    assert_eq!(
        run_parser(
            vec![k_repeat(), k_break(), k_until(), k_true()].iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::Repeat(
            lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break)),
            lua_syntax::Exp::True
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                k_if(),
                k_true(),
                k_then(),
                k_return(),
                number(1.0),
                k_elseif(),
                k_false(),
                k_then(),
                k_return(),
                number(2.0),
                k_elseif(),
                k_nil(),
                k_then(),
                k_return(),
                number(3.0),
                k_else(),
                k_return(),
                number(4.0),
                k_end(),
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::If(
            (
                lua_syntax::Exp::True,
                lua_syntax::Block(
                    vec![],
                    Some(lua_syntax::LastStat::Return(Some(vec![
                        lua_syntax::Exp::Number(1.0)
                    ])))
                )
            ),
            vec![
                (
                    lua_syntax::Exp::False,
                    lua_syntax::Block(
                        vec![],
                        Some(lua_syntax::LastStat::Return(Some(vec![
                            lua_syntax::Exp::Number(2.0)
                        ])))
                    )
                ),
                (
                    lua_syntax::Exp::Nil,
                    lua_syntax::Block(
                        vec![],
                        Some(lua_syntax::LastStat::Return(Some(vec![
                            lua_syntax::Exp::Number(3.0)
                        ])))
                    )
                ),
            ],
            Some(lua_syntax::Block(
                vec![],
                Some(lua_syntax::LastStat::Return(Some(vec![
                    lua_syntax::Exp::Number(4.0)
                ])))
            ))
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                k_if(),
                k_true(),
                k_then(),
                k_return(),
                number(1.0),
                k_elseif(),
                k_false(),
                k_then(),
                k_return(),
                number(2.0),
                k_elseif(),
                k_nil(),
                k_then(),
                k_return(),
                number(3.0),
                k_end(),
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::If(
            (
                lua_syntax::Exp::True,
                lua_syntax::Block(
                    vec![],
                    Some(lua_syntax::LastStat::Return(Some(vec![
                        lua_syntax::Exp::Number(1.0)
                    ])))
                )
            ),
            vec![
                (
                    lua_syntax::Exp::False,
                    lua_syntax::Block(
                        vec![],
                        Some(lua_syntax::LastStat::Return(Some(vec![
                            lua_syntax::Exp::Number(2.0)
                        ])))
                    )
                ),
                (
                    lua_syntax::Exp::Nil,
                    lua_syntax::Block(
                        vec![],
                        Some(lua_syntax::LastStat::Return(Some(vec![
                            lua_syntax::Exp::Number(3.0)
                        ])))
                    )
                ),
            ],
            None
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                k_if(),
                k_true(),
                k_then(),
                k_return(),
                number(1.0),
                k_elseif(),
                k_false(),
                k_then(),
                k_return(),
                number(2.0),
                k_else(),
                k_return(),
                number(4.0),
                k_end(),
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::If(
            (
                lua_syntax::Exp::True,
                lua_syntax::Block(
                    vec![],
                    Some(lua_syntax::LastStat::Return(Some(vec![
                        lua_syntax::Exp::Number(1.0)
                    ])))
                )
            ),
            vec![(
                lua_syntax::Exp::False,
                lua_syntax::Block(
                    vec![],
                    Some(lua_syntax::LastStat::Return(Some(vec![
                        lua_syntax::Exp::Number(2.0)
                    ])))
                )
            ),],
            Some(lua_syntax::Block(
                vec![],
                Some(lua_syntax::LastStat::Return(Some(vec![
                    lua_syntax::Exp::Number(4.0)
                ])))
            ))
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                k_if(),
                k_true(),
                k_then(),
                k_return(),
                number(1.0),
                k_elseif(),
                k_false(),
                k_then(),
                k_return(),
                number(2.0),
                k_end(),
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::If(
            (
                lua_syntax::Exp::True,
                lua_syntax::Block(
                    vec![],
                    Some(lua_syntax::LastStat::Return(Some(vec![
                        lua_syntax::Exp::Number(1.0)
                    ])))
                )
            ),
            vec![(
                lua_syntax::Exp::False,
                lua_syntax::Block(
                    vec![],
                    Some(lua_syntax::LastStat::Return(Some(vec![
                        lua_syntax::Exp::Number(2.0)
                    ])))
                )
            ),],
            None
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                k_if(),
                k_true(),
                k_then(),
                k_return(),
                number(1.0),
                k_else(),
                k_return(),
                number(4.0),
                k_end(),
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::If(
            (
                lua_syntax::Exp::True,
                lua_syntax::Block(
                    vec![],
                    Some(lua_syntax::LastStat::Return(Some(vec![
                        lua_syntax::Exp::Number(1.0)
                    ])))
                )
            ),
            vec![],
            Some(lua_syntax::Block(
                vec![],
                Some(lua_syntax::LastStat::Return(Some(vec![
                    lua_syntax::Exp::Number(4.0)
                ])))
            ))
        ))
    );
    assert_eq!(
        run_parser(
            vec![k_if(), k_true(), k_then(), k_return(), number(1.0), k_end(),].iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::If(
            (
                lua_syntax::Exp::True,
                lua_syntax::Block(
                    vec![],
                    Some(lua_syntax::LastStat::Return(Some(vec![
                        lua_syntax::Exp::Number(1.0)
                    ])))
                )
            ),
            vec![],
            None
        ))
    );
}

#[test]
fn test_stat_parser2() {
    assert_eq!(
        run_parser(
            vec![
                k_for(),
                name("abc"),
                assign(),
                k_true(),
                comma(),
                k_false(),
                comma(),
                k_nil(),
                k_do(),
                k_break(),
                k_end()
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::For(
            "abc",
            lua_syntax::Exp::True,
            lua_syntax::Exp::False,
            Some(lua_syntax::Exp::Nil),
            lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break))
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                k_for(),
                name("abc"),
                assign(),
                k_true(),
                comma(),
                k_false(),
                comma(),
                k_do(),
                k_break(),
                k_end()
            ]
            .iter(),
            stat_parser()
        ),
        None
    );
    assert_eq!(
        run_parser(
            vec![
                k_for(),
                name("abc"),
                assign(),
                k_true(),
                comma(),
                k_false(),
                comma(),
                k_nil(),
                comma(),
                k_do(),
                k_break(),
                k_end()
            ]
            .iter(),
            stat_parser()
        ),
        None
    );
    assert_eq!(
        run_parser(
            vec![
                k_for(),
                name("abc"),
                assign(),
                k_true(),
                comma(),
                k_false(),
                comma(),
                k_nil(),
                comma(),
                k_true(),
                k_do(),
                k_break(),
                k_end()
            ]
            .iter(),
            stat_parser()
        ),
        None
    );
    assert_eq!(
        run_parser(
            vec![
                k_for(),
                name("abc"),
                comma(),
                name("def"),
                k_in(),
                k_true(),
                comma(),
                k_false(),
                k_do(),
                k_break(),
                k_end()
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::ForIn(
            vec!["abc", "def"],
            vec![lua_syntax::Exp::True, lua_syntax::Exp::False],
            lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break))
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                k_function(),
                name("abc"),
                period(),
                name("def"),
                l_paren(),
                varargs(),
                r_paren(),
                k_break(),
                k_end()
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::Function(
            lua_syntax::FuncName("abc", vec!["def"], None),
            lua_syntax::FuncBody(
                Some(lua_syntax::ParList::VarArgs),
                lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break))
            )
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                k_local(),
                k_function(),
                name("abc"),
                l_paren(),
                varargs(),
                r_paren(),
                k_break(),
                k_end()
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::LocalFunctionDecl(
            "abc",
            lua_syntax::FuncBody(
                Some(lua_syntax::ParList::VarArgs),
                lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break))
            )
        ))
    );
    assert_eq!(
        run_parser(
            vec![k_local(), name("abc"), comma(), name("def")].iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::LocalVarDecl(vec!["abc", "def"], None))
    );
    assert_eq!(
        run_parser(
            vec![
                k_local(),
                name("abc"),
                comma(),
                name("def"),
                assign(),
                k_true(),
                comma(),
                k_false()
            ]
            .iter(),
            stat_parser()
        ),
        Some(lua_syntax::Stat::LocalVarDecl(
            vec!["abc", "def"],
            Some(vec![lua_syntax::Exp::True, lua_syntax::Exp::False])
        ))
    );
    assert_eq!(
        run_parser(
            vec![k_local(), name("abc"), comma(), name("def"), assign(),].iter(),
            stat_parser()
        ),
        None
    );
    assert_eq!(
        run_parser(
            vec![k_local(), name("abc"), comma(), assign(),].iter(),
            stat_parser()
        ),
        None
    );
    assert_eq!(
        run_parser(vec![k_local(), assign(),].iter(), stat_parser()),
        None
    );
    assert_eq!(
        run_parser(vec![k_local(), name("abc"), comma(),].iter(), stat_parser()),
        None
    );
    assert_eq!(run_parser(vec![k_local(),].iter(), stat_parser()), None);
}

#[test]
fn test_block_parser() {
    assert_eq!(
        run_parser(vec![].iter(), block_parser()),
        Some(lua_syntax::Block(vec![], None))
    );
    assert_eq!(
        run_parser(vec![name("abc"), assign(), k_true()].iter(), block_parser()),
        Some(lua_syntax::Block(
            vec![lua_syntax::Stat::Assign(
                vec![lua_syntax::Var::Name("abc")],
                vec![lua_syntax::Exp::True]
            )],
            None
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), assign(), k_true(), semi()].iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![lua_syntax::Stat::Assign(
                vec![lua_syntax::Var::Name("abc")],
                vec![lua_syntax::Exp::True]
            )],
            None
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                name("def"),
                assign(),
                k_false()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            None
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                semi(),
                name("def"),
                assign(),
                k_false()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            None
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                name("def"),
                assign(),
                k_false(),
                semi()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            None
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                semi(),
                name("def"),
                assign(),
                k_false(),
                semi()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            None
        ))
    );

    assert_eq!(
        run_parser(vec![k_break()].iter(), block_parser()),
        Some(lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break)))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), assign(), k_true(), k_break()].iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![lua_syntax::Stat::Assign(
                vec![lua_syntax::Var::Name("abc")],
                vec![lua_syntax::Exp::True]
            )],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), assign(), k_true(), semi(), k_break()].iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![lua_syntax::Stat::Assign(
                vec![lua_syntax::Var::Name("abc")],
                vec![lua_syntax::Exp::True]
            )],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                name("def"),
                assign(),
                k_false(),
                k_break()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                semi(),
                name("def"),
                assign(),
                k_false(),
                k_break()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                name("def"),
                assign(),
                k_false(),
                semi(),
                k_break()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                semi(),
                name("def"),
                assign(),
                k_false(),
                semi(),
                k_break()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            Some(lua_syntax::LastStat::Break)
        ))
    );

    assert_eq!(
        run_parser(vec![k_break(), semi()].iter(), block_parser()),
        Some(lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break)))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), assign(), k_true(), k_break(), semi()].iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![lua_syntax::Stat::Assign(
                vec![lua_syntax::Var::Name("abc")],
                vec![lua_syntax::Exp::True]
            )],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), assign(), k_true(), semi(), k_break(), semi()].iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![lua_syntax::Stat::Assign(
                vec![lua_syntax::Var::Name("abc")],
                vec![lua_syntax::Exp::True]
            )],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                name("def"),
                assign(),
                k_false(),
                k_break(),
                semi()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                semi(),
                name("def"),
                assign(),
                k_false(),
                k_break(),
                semi()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                name("def"),
                assign(),
                k_false(),
                semi(),
                k_break(),
                semi()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            Some(lua_syntax::LastStat::Break)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                assign(),
                k_true(),
                semi(),
                name("def"),
                assign(),
                k_false(),
                semi(),
                k_break(),
                semi()
            ]
            .iter(),
            block_parser()
        ),
        Some(lua_syntax::Block(
            vec![
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("abc")],
                    vec![lua_syntax::Exp::True]
                ),
                lua_syntax::Stat::Assign(
                    vec![lua_syntax::Var::Name("def")],
                    vec![lua_syntax::Exp::False]
                ),
            ],
            Some(lua_syntax::LastStat::Break)
        ))
    );

    assert_eq!(run_parser(vec![semi()].iter(), block_parser()), None);
    assert_eq!(
        run_parser(vec![semi(), k_break()].iter(), block_parser()),
        None
    );
}

#[test]
fn test_funcbody_parser() {
    assert_eq!(
        run_parser(
            vec![l_paren(), r_paren(), k_break(), k_end()].iter(),
            funcbody_parser()
        ),
        Some(lua_syntax::FuncBody(
            None,
            lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break))
        ))
    );
    assert_eq!(
        run_parser(
            vec![l_paren(), varargs(), r_paren(), k_break(), k_end()].iter(),
            funcbody_parser()
        ),
        Some(lua_syntax::FuncBody(
            Some(lua_syntax::ParList::VarArgs),
            lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break))
        ))
    );

    assert_eq!(
        run_parser(
            vec![k_function(), l_paren(), r_paren(), k_break(), k_end()].iter(),
            exp_parser()
        ),
        Some(lua_syntax::Exp::Function(Box::new(lua_syntax::FuncBody(
            None,
            lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break))
        ))))
    );
    assert_eq!(
        run_parser(
            vec![
                k_function(),
                l_paren(),
                varargs(),
                r_paren(),
                k_break(),
                k_end()
            ]
            .iter(),
            exp_parser()
        ),
        Some(lua_syntax::Exp::Function(Box::new(lua_syntax::FuncBody(
            Some(lua_syntax::ParList::VarArgs),
            lua_syntax::Block(vec![], Some(lua_syntax::LastStat::Break))
        ))))
    );
}

#[test]
fn test_var_parser() {
    assert_eq!(
        run_parser(vec![name("abc")].iter(), var_parser()),
        Some(lua_syntax::Var::Name("abc"))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                l_bracket(),
                k_true(),
                r_bracket(),
                period(),
                name("def"),
                l_bracket(),
                k_false(),
                r_bracket()
            ]
            .iter(),
            var_parser()
        ),
        Some(lua_syntax::Var::Index(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Field(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Index(
                    lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                    lua_syntax::Exp::True
                ))),
                "def"
            ))),
            lua_syntax::Exp::False
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                l_paren(),
                name("abc"),
                r_paren(),
                l_bracket(),
                k_true(),
                r_bracket(),
                period(),
                name("def"),
                l_bracket(),
                k_false(),
                r_bracket()
            ]
            .iter(),
            var_parser()
        ),
        Some(lua_syntax::Var::Index(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Field(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Index(
                    lua_syntax::PrefixExp::Exp(Box::new(lua_syntax::Exp::PrefixExp(
                        lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc")))
                    ))),
                    lua_syntax::Exp::True
                ))),
                "def"
            ))),
            lua_syntax::Exp::False
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                l_paren(),
                r_paren(),
                l_bracket(),
                k_true(),
                r_bracket(),
                period(),
                name("def"),
                l_bracket(),
                k_false(),
                r_bracket()
            ]
            .iter(),
            var_parser()
        ),
        Some(lua_syntax::Var::Index(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Field(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Index(
                    lua_syntax::PrefixExp::FunctionCall(Box::new(
                        lua_syntax::FunctionCall::FunctionCall(
                            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                            lua_syntax::Args::Args(None)
                        )
                    )),
                    lua_syntax::Exp::True
                ))),
                "def"
            ))),
            lua_syntax::Exp::False
        ))
    );
}

#[test]
fn test_varlist_parser() {
    assert_eq!(run_parser(vec![].iter(), varlist_parser()), None);
    assert_eq!(
        run_parser(vec![name("abc")].iter(), varlist_parser()),
        Some(vec![lua_syntax::Var::Name("abc")])
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), comma(), name("def")].iter(),
            varlist_parser()
        ),
        Some(vec![
            lua_syntax::Var::Name("abc"),
            lua_syntax::Var::Name("def")
        ])
    );
    assert_eq!(
        run_parser(vec![name("abc"), comma()].iter(), varlist_parser()),
        None
    );
}

#[test]
fn test_args_parser() {
    assert_eq!(
        run_parser(vec![l_paren(), r_paren()].iter(), args_parser()),
        Some(lua_syntax::Args::Args(None))
    );
    assert_eq!(
        run_parser(vec![l_paren(), k_true(), r_paren()].iter(), args_parser()),
        Some(lua_syntax::Args::Args(Some(vec![lua_syntax::Exp::True])))
    );
    assert_eq!(
        run_parser(
            vec![l_paren(), k_true(), comma(), k_false(), r_paren()].iter(),
            args_parser()
        ),
        Some(lua_syntax::Args::Args(Some(vec![
            lua_syntax::Exp::True,
            lua_syntax::Exp::False
        ])))
    );
    assert_eq!(
        run_parser(vec![l_brace(), r_brace()].iter(), args_parser()),
        Some(lua_syntax::Args::TableCtor(lua_syntax::TableCtor(None)))
    );
    assert_eq!(
        run_parser(vec![l_brace(), k_true(), r_brace()].iter(), args_parser()),
        Some(lua_syntax::Args::TableCtor(lua_syntax::TableCtor(Some(
            vec![lua_syntax::Field::NumberField(lua_syntax::Exp::True)]
        ))))
    );
    assert_eq!(
        run_parser(vec![string("abc".to_string())].iter(), args_parser()),
        Some(lua_syntax::Args::String("abc".to_string()))
    );
}

#[test]
fn test_functioncall_parser() {
    assert_eq!(
        run_parser(
            vec![name("abc"), l_paren(), r_paren()].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            lua_syntax::Args::Args(None)
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), l_brace(), r_brace()].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            lua_syntax::Args::TableCtor(lua_syntax::TableCtor(None))
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), string("def".to_string())].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            lua_syntax::Args::String("def".to_string())
        ))
    );
    assert_eq!(
        run_parser(
            vec![l_paren(), name("abc"), r_paren(), l_paren(), r_paren()].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::Exp(Box::new(lua_syntax::Exp::PrefixExp(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc")))
            ))),
            lua_syntax::Args::Args(None)
        ))
    );
    assert_eq!(
        run_parser(
            vec![l_paren(), name("abc"), r_paren(), l_brace(), r_brace()].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::Exp(Box::new(lua_syntax::Exp::PrefixExp(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc")))
            ))),
            lua_syntax::Args::TableCtor(lua_syntax::TableCtor(None))
        ))
    );
    assert_eq!(
        run_parser(
            vec![l_paren(), name("abc"), r_paren(), string("def".to_string())].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::Exp(Box::new(lua_syntax::Exp::PrefixExp(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc")))
            ))),
            lua_syntax::Args::String("def".to_string())
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), l_paren(), r_paren(), l_paren(), r_paren()].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None)
            ))),
            lua_syntax::Args::Args(None)
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), l_paren(), r_paren(), l_brace(), r_brace()].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None)
            ))),
            lua_syntax::Args::TableCtor(lua_syntax::TableCtor(None))
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), l_paren(), r_paren(), string("def".to_string())].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::FunctionCall(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None)
            ))),
            lua_syntax::Args::String("def".to_string())
        ))
    );

    assert_eq!(
        run_parser(
            vec![name("abc"), colon(), name("def"), l_paren(), r_paren()].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            "def",
            lua_syntax::Args::Args(None)
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), colon(), name("def"), l_brace(), r_brace()].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            "def",
            lua_syntax::Args::TableCtor(lua_syntax::TableCtor(None))
        ))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), colon(), name("def"), string("xyz".to_string())].iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
            "def",
            lua_syntax::Args::String("xyz".to_string())
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                l_paren(),
                name("abc"),
                r_paren(),
                colon(),
                name("def"),
                l_paren(),
                r_paren()
            ]
            .iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::Exp(Box::new(lua_syntax::Exp::PrefixExp(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc")))
            ))),
            "def",
            lua_syntax::Args::Args(None)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                l_paren(),
                name("abc"),
                r_paren(),
                colon(),
                name("def"),
                l_brace(),
                r_brace()
            ]
            .iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::Exp(Box::new(lua_syntax::Exp::PrefixExp(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc")))
            ))),
            "def",
            lua_syntax::Args::TableCtor(lua_syntax::TableCtor(None))
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                l_paren(),
                name("abc"),
                r_paren(),
                colon(),
                name("def"),
                string("xyz".to_string())
            ]
            .iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::Exp(Box::new(lua_syntax::Exp::PrefixExp(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc")))
            ))),
            "def",
            lua_syntax::Args::String("xyz".to_string())
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                l_paren(),
                r_paren(),
                colon(),
                name("def"),
                l_paren(),
                r_paren()
            ]
            .iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None)
            ))),
            "def",
            lua_syntax::Args::Args(None)
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                l_paren(),
                r_paren(),
                colon(),
                name("def"),
                l_brace(),
                r_brace()
            ]
            .iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None)
            ))),
            "def",
            lua_syntax::Args::TableCtor(lua_syntax::TableCtor(None))
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                name("abc"),
                l_paren(),
                r_paren(),
                colon(),
                name("def"),
                string("xyz".to_string())
            ]
            .iter(),
            functioncall_parser()
        ),
        Some(lua_syntax::FunctionCall::MethodCall(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None)
            ))),
            "def",
            lua_syntax::Args::String("xyz".to_string())
        ))
    );
}

#[test]
fn test_prefixexp_parser() {
    assert_eq!(
        run_parser(vec![name("abc")].iter(), prefixexp_parser()),
        Some(lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name(
            "abc"
        ))))
    );
    assert_eq!(
        run_parser(
            vec![name("abc"), l_paren(), r_paren()].iter(),
            prefixexp_parser()
        ),
        Some(lua_syntax::PrefixExp::FunctionCall(Box::new(
            lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None)
            )
        )))
    );
    assert_eq!(
        run_parser(
            vec![l_paren(), k_true(), r_paren()].iter(),
            prefixexp_parser()
        ),
        Some(lua_syntax::PrefixExp::Exp(Box::new(lua_syntax::Exp::True)))
    );

    assert_eq!(
        run_parser(vec![name("abc")].iter(), exp_parser()),
        Some(lua_syntax::Exp::PrefixExp(lua_syntax::PrefixExp::Var(
            Box::new(lua_syntax::Var::Name("abc"))
        )))
    );
    assert_eq!(
        run_parser(vec![name("abc"), l_paren(), r_paren()].iter(), exp_parser()),
        Some(lua_syntax::Exp::PrefixExp(
            lua_syntax::PrefixExp::FunctionCall(Box::new(lua_syntax::FunctionCall::FunctionCall(
                lua_syntax::PrefixExp::Var(Box::new(lua_syntax::Var::Name("abc"))),
                lua_syntax::Args::Args(None)
            )))
        ))
    );
    assert_eq!(
        run_parser(vec![l_paren(), k_true(), r_paren()].iter(), exp_parser()),
        Some(lua_syntax::Exp::PrefixExp(lua_syntax::PrefixExp::Exp(
            Box::new(lua_syntax::Exp::True)
        )))
    );
}

#[test]
fn test_exp_ops_parser() {
    assert_eq!(
        run_parser(
            vec![number(1.0), k_and(), number(2.0), k_or(), number(3.0)].iter(),
            exp_parser()
        ),
        Some(lua_syntax::Exp::BinOp(
            Box::new(lua_syntax::Exp::BinOp(
                Box::new(lua_syntax::Exp::Number(1.0)),
                lua_syntax::BinOp::And,
                Box::new(lua_syntax::Exp::Number(2.0))
            )),
            lua_syntax::BinOp::Or,
            Box::new(lua_syntax::Exp::Number(3.0))
        ))
    );
    assert_eq!(
        run_parser(
            vec![number(1.0), k_or(), number(2.0), k_and(), number(3.0)].iter(),
            exp_parser()
        ),
        Some(lua_syntax::Exp::BinOp(
            Box::new(lua_syntax::Exp::Number(1.0)),
            lua_syntax::BinOp::Or,
            Box::new(lua_syntax::Exp::BinOp(
                Box::new(lua_syntax::Exp::Number(2.0)),
                lua_syntax::BinOp::And,
                Box::new(lua_syntax::Exp::Number(3.0))
            ))
        ))
    );
    assert_eq!(
        run_parser(
            vec![
                number(1.0),
                plus(),
                number(2.0),
                times(),
                number(3.0),
                plus(),
                minus(),
                number(4.0),
                pow(),
                number(5.0),
                pow(),
                number(6.0)
            ]
            .iter(),
            exp_parser()
        ),
        Some(lua_syntax::Exp::BinOp(
            Box::new(lua_syntax::Exp::BinOp(
                Box::new(lua_syntax::Exp::Number(1.0)),
                lua_syntax::BinOp::Add,
                Box::new(lua_syntax::Exp::BinOp(
                    Box::new(lua_syntax::Exp::Number(2.0)),
                    lua_syntax::BinOp::Mul,
                    Box::new(lua_syntax::Exp::Number(3.0))
                )),
            )),
            lua_syntax::BinOp::Add,
            Box::new(lua_syntax::Exp::UnOp(
                lua_syntax::UnOp::Unm,
                Box::new(lua_syntax::Exp::BinOp(
                    Box::new(lua_syntax::Exp::Number(4.0)),
                    lua_syntax::BinOp::Pow,
                    Box::new(lua_syntax::Exp::BinOp(
                        Box::new(lua_syntax::Exp::Number(5.0)),
                        lua_syntax::BinOp::Pow,
                        Box::new(lua_syntax::Exp::Number(6.0))
                    ))
                ))
            ))
        ))
    );
}

mod ops_tests {
    use super::*;

    #[derive(PartialEq, Eq, Debug, Clone, Copy)]
    enum Assoc {
        Left,
        Right,
    }

    #[derive(Clone, Copy)]
    enum Op<'a> {
        BinOp(lua_syntax::BinOp, Assoc, fn() -> lua_lexemes::Token<'a>),
        UnOp(lua_syntax::UnOp, fn() -> lua_lexemes::Token<'a>),
    }

    fn create_ops<'a>() -> Vec<Vec<Op<'a>>> {
        vec![
            vec![Op::BinOp(lua_syntax::BinOp::Or, Assoc::Left, k_or)],
            vec![Op::BinOp(lua_syntax::BinOp::And, Assoc::Left, k_and)],
            vec![
                Op::BinOp(lua_syntax::BinOp::Lt, Assoc::Left, lt),
                Op::BinOp(lua_syntax::BinOp::Gt, Assoc::Left, gt),
                Op::BinOp(lua_syntax::BinOp::Le, Assoc::Left, le),
                Op::BinOp(lua_syntax::BinOp::Ge, Assoc::Left, ge),
                Op::BinOp(lua_syntax::BinOp::Neq, Assoc::Left, neq),
                Op::BinOp(lua_syntax::BinOp::Eq, Assoc::Left, eq),
            ],
            vec![Op::BinOp(lua_syntax::BinOp::Concat, Assoc::Right, concat)],
            vec![
                Op::BinOp(lua_syntax::BinOp::Add, Assoc::Left, plus),
                Op::BinOp(lua_syntax::BinOp::Sub, Assoc::Left, minus),
            ],
            vec![
                Op::BinOp(lua_syntax::BinOp::Mul, Assoc::Left, times),
                Op::BinOp(lua_syntax::BinOp::Div, Assoc::Left, divide),
                Op::BinOp(lua_syntax::BinOp::Mod, Assoc::Left, modulo),
            ],
            vec![
                Op::UnOp(lua_syntax::UnOp::Not, k_not),
                Op::UnOp(lua_syntax::UnOp::Len, len),
                Op::UnOp(lua_syntax::UnOp::Unm, minus),
            ],
            vec![Op::BinOp(lua_syntax::BinOp::Pow, Assoc::Right, pow)],
        ]
    }

    fn create_ops_with_priorities<'a>() -> Vec<(Op<'a>, usize)> {
        let mut result = vec![];
        for (p, ops) in create_ops().into_iter().enumerate() {
            for op in ops {
                result.push((op, p));
            }
        }
        result
    }

    struct TestCase<'a> {
        input: Vec<lua_lexemes::Token<'a>>,
        output: lua_syntax::Exp<'a>,
    }

    fn generate_case<'a>(op1: (Op<'a>, usize), op2: (Op<'a>, usize)) -> TestCase<'a> {
        let num = |n| lua_syntax::Exp::Number(n);
        let unop = |unop, e| lua_syntax::Exp::UnOp(unop, Box::new(e));
        let binop = |lhs, binop, rhs| lua_syntax::Exp::BinOp(Box::new(lhs), binop, Box::new(rhs));
        match (op1, op2) {
            ((Op::BinOp(op1, ass1, t1), p1), (Op::BinOp(op2, ass2, t2), p2)) => TestCase {
                input: vec![number(1.0), t1(), number(2.0), t2(), number(3.0)],
                output: if p1 < p2 {
                    binop(num(1.0), op1, binop(num(2.0), op2, num(3.0)))
                } else if p1 == p2 {
                    assert_eq!(ass1, ass2);
                    match ass1 {
                        Assoc::Left => binop(binop(num(1.0), op1, num(2.0)), op2, num(3.0)),
                        Assoc::Right => binop(num(1.0), op1, binop(num(2.0), op2, num(3.0))),
                    }
                } else {
                    binop(binop(num(1.0), op1, num(2.0)), op2, num(3.0))
                },
            },
            ((Op::BinOp(op1, _, t1), _), (Op::UnOp(op2, t2), _)) => TestCase {
                input: vec![number(1.0), t1(), t2(), number(2.0)],
                output: binop(num(1.0), op1, unop(op2, num(2.0))),
            },
            ((Op::UnOp(op1, t1), p1), (Op::BinOp(op2, _, t2), p2)) => TestCase {
                input: vec![t1(), number(1.0), t2(), number(2.0)],
                output: if p1 < p2 {
                    unop(op1, binop(num(1.0), op2, num(2.0)))
                } else {
                    binop(unop(op1, num(1.0)), op2, num(2.0))
                },
            },
            ((Op::UnOp(op1, t1), _), (Op::UnOp(op2, t2), _)) => TestCase {
                input: vec![t1(), t2(), number(1.0)],
                output: unop(op1, unop(op2, num(1.0))),
            },
        }
    }

    fn generate_cases<'a>() -> Vec<TestCase<'a>> {
        let mut cases: Vec<TestCase<'static>> = vec![];
        let ops = create_ops_with_priorities();
        for op1 in ops.iter() {
            for op2 in ops.iter() {
                cases.push(generate_case(*op1, *op2))
            }
        }
        cases
    }

    #[test]
    fn test_exp_ops_parser_comprehensive() {
        for test_case in generate_cases() {
            assert_eq!(
                run_parser(test_case.input.iter(), exp_parser()),
                Some(test_case.output)
            );
        }
    }
}
