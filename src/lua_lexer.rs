use crate::lua_lexemes;
use crate::parser_lib;

fn keyword_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, lua_lexemes::Keyword> + 'a> {
    let mut sorted_keywords = lua_lexemes::Keyword::ITEMS;
    sorted_keywords.sort_unstable_by(|lhs, rhs| rhs.to_str().len().cmp(&lhs.to_str().len()));
    parser_lib::choices(
        sorted_keywords
            .iter()
            .map(|item| {
                let i = *item;
                parser_lib::fmap(move |_| i, parser_lib::string_parser(item.to_str()))
            })
            .collect(),
    )
}

fn other_token_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, lua_lexemes::OtherToken> + 'a>
{
    let mut sorted_tokens = lua_lexemes::OtherToken::ITEMS;
    sorted_tokens.sort_unstable_by(|lhs, rhs| rhs.to_str().len().cmp(&lhs.to_str().len()));
    parser_lib::choices(
        sorted_tokens
            .iter()
            .map(|item| {
                let i = *item;
                parser_lib::fmap(move |_| i, parser_lib::string_parser(item.to_str()))
            })
            .collect(),
    )
}

fn string_literal_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, String> + 'a> {
    let string_literal_char_lexer = |quote: char| -> Box<dyn parser_lib::Parser<'b, char> + 'a> {
        parser_lib::choice(
            parser_lib::seq2(
                parser_lib::char_parser('\\'),
                parser_lib::choices(vec![
                    parser_lib::fmap(|_| '\x07', parser_lib::char_parser('a')),
                    parser_lib::fmap(|_| '\x08', parser_lib::char_parser('b')),
                    parser_lib::fmap(|_| '\x0C', parser_lib::char_parser('f')),
                    parser_lib::fmap(|_| '\n', parser_lib::char_parser('n')),
                    parser_lib::fmap(|_| '\r', parser_lib::char_parser('r')),
                    parser_lib::fmap(|_| '\t', parser_lib::char_parser('t')),
                    parser_lib::fmap(|_| '\x0B', parser_lib::char_parser('v')),
                    parser_lib::fmap(|_| '\\', parser_lib::char_parser('\\')),
                    if quote == '\'' {
                        parser_lib::fmap(|_| '\'', parser_lib::char_parser('\''))
                    } else {
                        parser_lib::fmap(|_| '"', parser_lib::char_parser('"'))
                    },
                    parser_lib::seq_bind(
                        parser_lib::parsed_string(parser_lib::seq_(
                            parser_lib::satisfies(|c| c.is_ascii_digit()),
                            parser_lib::try_parser(parser_lib::seq_(
                                parser_lib::satisfies(|c| c.is_ascii_digit()),
                                parser_lib::try_parser(parser_lib::satisfies(|c| {
                                    c.is_ascii_digit()
                                })),
                            )),
                        )),
                        |(_, code_str)| match code_str.parse::<u8>() {
                            Ok(code) => parser_lib::fmap(
                                move |_| char::from(code),
                                parser_lib::empty_parser(),
                            ),
                            Err(_) => parser_lib::fail_parser(),
                        },
                    ),
                ]),
            ),
            parser_lib::satisfies(move |c| c != '\\' && c != quote),
        )
    };
    parser_lib::seq_bind(
        parser_lib::satisfies(|in_c| in_c == '\'' || in_c == '"'),
        move |opening_quote| {
            parser_lib::seq_bind(
                parser_lib::seq1(
                    parser_lib::many(string_literal_char_lexer(opening_quote)),
                    parser_lib::char_parser(opening_quote),
                ),
                |chars| {
                    parser_lib::fmap(move |_| chars.iter().collect(), parser_lib::empty_parser())
                },
            )
        },
    )
}

fn long_brackets_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, &'b str> + 'a> {
    parser_lib::seq_bind(
        parser_lib::seq1(
            parser_lib::seq1(
                parser_lib::seq2(
                    parser_lib::char_parser('['),
                    parser_lib::many(parser_lib::char_parser('=')),
                ),
                parser_lib::char_parser('['),
            ),
            parser_lib::try_parser(parser_lib::char_parser('\n')),
        ),
        |bracket_level| {
            let bracket_level_len = bracket_level.len();
            let closing_bracket_parser = || {
                let mut p = parser_lib::char_parser(']');
                for _ in 0..bracket_level_len {
                    p = parser_lib::seq1(p, parser_lib::char_parser('='));
                }
                p = parser_lib::seq1(p, parser_lib::char_parser(']'));
                p
            };
            parser_lib::fmap(
                |(_, s)| s,
                parser_lib::seq1(
                    parser_lib::parsed_string(parser_lib::many(parser_lib::seq2(
                        parser_lib::not_parser(closing_bracket_parser()),
                        parser_lib::satisfies(|_| true),
                    ))),
                    closing_bracket_parser(),
                ),
            )
        },
    )
}

fn number_literal_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, f64> + 'a> {
    let hexadecimal_parser = || {
        parser_lib::seq_bind(
            parser_lib::parsed_string(parser_lib::many1(parser_lib::satisfies(|c| c.is_digit(16)))),
            |(_, num_str)| match i64::from_str_radix(num_str, 16) {
                Ok(num) => parser_lib::fmap(move |_| num as f64, parser_lib::empty_parser()),
                Err(_) => parser_lib::fail_parser(),
            },
        )
    };
    let integer_parser = || parser_lib::many1(parser_lib::satisfies(|c| c.is_ascii_digit()));
    let point_parser = || parser_lib::char_parser('.');
    let exponent_parser = || {
        seqs_![
            parser_lib::choice(parser_lib::char_parser('E'), parser_lib::char_parser('e')),
            parser_lib::try_parser(parser_lib::choice(
                parser_lib::char_parser('+'),
                parser_lib::char_parser('-')
            )),
            integer_parser(),
        ]
    };
    let decimal_parser = || {
        parser_lib::seq_bind(
            parser_lib::parsed_string(parser_lib::choices(vec![
                seqs_![
                    integer_parser(),
                    parser_lib::try_parser(seqs_![
                        point_parser(),
                        parser_lib::try_parser(integer_parser()),
                    ]),
                    parser_lib::try_parser(exponent_parser()),
                ],
                seqs_![
                    point_parser(),
                    integer_parser(),
                    parser_lib::try_parser(exponent_parser()),
                ],
            ])),
            |(_, num_str)| match num_str.parse::<f64>() {
                Ok(num) => parser_lib::fmap(move |_| num, parser_lib::empty_parser()),
                Err(_) => parser_lib::fail_parser(),
            },
        )
    };
    parser_lib::choice(
        parser_lib::seq2(parser_lib::string_parser("0x"), hexadecimal_parser()),
        decimal_parser(),
    )
}

fn identifier_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, &'b str> + 'a> {
    let first_sym = parser_lib::satisfies(|c| c.is_alphabetic() || c == '_');
    let other_sym = parser_lib::satisfies(|c| c.is_alphanumeric() || c == '_');
    parser_lib::fmap(
        |(_, s)| s,
        parser_lib::parsed_string(parser_lib::seq(first_sym, parser_lib::many(other_sym))),
    )
}

fn token_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, lua_lexemes::Token<'b>> + 'a> {
    parser_lib::choices(vec![
        parser_lib::fmap(lua_lexemes::Token::Keyword, keyword_lexer()),
        parser_lib::fmap(lua_lexemes::Token::Identifier, identifier_lexer()),
        parser_lib::fmap(lua_lexemes::Token::OtherToken, other_token_lexer()),
        parser_lib::fmap(
            |x| lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(x)),
            string_literal_lexer(),
        ),
        parser_lib::fmap(
            |x| lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(x)),
            number_literal_lexer(),
        ),
    ])
}

fn comment_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, ()> + 'a> {
    seqs_![
        parser_lib::string_parser("--"),
        parser_lib::choice(
            parser_lib::fmap(|_| (), long_brackets_lexer()),
            parser_lib::fmap(
                |_| (),
                parser_lib::many(parser_lib::satisfies(|c| c != '\n'))
            )
        )
    ]
}

fn whitespace_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, ()> + 'a> {
    parser_lib::fmap(
        |_| (),
        parser_lib::many1(parser_lib::satisfies(|c| c.is_ascii_whitespace())),
    )
}

fn tokens_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<'b, Vec<lua_lexemes::Token<'b>>> + 'a> {
    let skip_lexer = || {
        parser_lib::fmap(
            |_| (),
            parser_lib::many(parser_lib::choice(whitespace_lexer(), comment_lexer())),
        )
    };
    parser_lib::seq1(
        parser_lib::seq2(
            skip_lexer(),
            parser_lib::many(parser_lib::seq1(token_lexer(), skip_lexer())),
        ),
        parser_lib::eof(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_lexer() {
        let items = lua_lexemes::Keyword::ITEMS;
        for item in &items {
            assert_eq!(
                parser_lib::run_parser(item.to_str(), keyword_lexer()),
                Some(*item)
            );
        }
    }

    #[test]
    fn test_other_token_lexer() {
        let items = lua_lexemes::OtherToken::ITEMS;
        for item in &items {
            assert_eq!(
                parser_lib::run_parser(item.to_str(), other_token_lexer()),
                Some(*item)
            );
        }
    }

    #[test]
    fn test_string_literal_lexer() {
        assert_eq!(
            parser_lib::run_parser(r#""""#, string_literal_lexer()),
            Some("".to_string())
        );
        assert_eq!(
            parser_lib::run_parser(r#""\"'""#, string_literal_lexer()),
            Some("\"'".to_string())
        );
        assert_eq!(
            parser_lib::run_parser(r#"''"#, string_literal_lexer()),
            Some("".to_string())
        );
        assert_eq!(
            parser_lib::run_parser(r#"'\'"'"#, string_literal_lexer()),
            Some("'\"".to_string())
        );
        assert_eq!(
            parser_lib::run_parser(r#""'"#, string_literal_lexer()),
            None
        );
        assert_eq!(
            parser_lib::run_parser(r#"'""#, string_literal_lexer()),
            None
        );
        assert_eq!(
            parser_lib::run_parser(r#""\'""#, string_literal_lexer()),
            None
        );
        assert_eq!(
            parser_lib::run_parser(r#"'\"'"#, string_literal_lexer()),
            None
        );

        assert_eq!(
            parser_lib::run_parser(
                "\"a bc\\\" \n \\0 \\10 \\127 \\0010\"",
                string_literal_lexer()
            ),
            Some("a bc\" \n \x00 \x0a \x7f \x010".to_string())
        );
        assert_eq!(
            parser_lib::run_parser(
                "'a bc\\\' \n \\0 \\10 \\127 \\0010'",
                string_literal_lexer()
            ),
            Some("a bc' \n \x00 \x0a \x7f \x010".to_string())
        );
    }

    #[test]
    fn test_long_brackets_lexer() {
        assert_eq!(
            parser_lib::run_parser(r#"[[abc]]"#, long_brackets_lexer()),
            Some(r#"abc"#)
        );
        assert_eq!(
            parser_lib::run_parser(r#"[=[abc]]]=]"#, long_brackets_lexer()),
            Some(r#"abc]]"#)
        );
        assert_eq!(
            parser_lib::run_parser(
                r#"[=[abc
def
]=]"#,
                long_brackets_lexer()
            ),
            Some(
                r#"abc
def
"#
            )
        );
        assert_eq!(
            parser_lib::run_parser(
                r#"[=[
abc
def
]=]"#,
                long_brackets_lexer()
            ),
            Some(
                r#"abc
def
"#
            )
        );
        assert_eq!(
            parser_lib::run_parser(r#"[=[ [=[ ]=] ]=]"#, long_brackets_lexer()),
            Some(r#" [=[ "#)
        );
        assert_eq!(
            parser_lib::run_parser(r#"[[ \' \" \n]]"#, long_brackets_lexer()),
            Some(r#" \' \" \n"#)
        );
    }

    #[test]
    fn test_number_literal_lexer1() {
        assert_eq!(
            parser_lib::run_parser("0", number_literal_lexer()),
            Some(0.0)
        );
        assert_eq!(
            parser_lib::run_parser("012", number_literal_lexer()),
            Some(12.0)
        );
        assert_eq!(
            parser_lib::run_parser("123", number_literal_lexer()),
            Some(123.0)
        );
        assert_eq!(
            parser_lib::run_parser("0.", number_literal_lexer()),
            Some(0.0)
        );
        assert_eq!(
            parser_lib::run_parser("123.", number_literal_lexer()),
            Some(123.0)
        );
        assert_eq!(
            parser_lib::run_parser("0.0", number_literal_lexer()),
            Some(0.0)
        );
        assert_eq!(
            parser_lib::run_parser("123.0", number_literal_lexer()),
            Some(123.0)
        );
        assert_eq!(
            parser_lib::run_parser("123.10", number_literal_lexer()),
            Some(123.1)
        );
        assert_eq!(
            parser_lib::run_parser(".0", number_literal_lexer()),
            Some(0.0)
        );
        assert_eq!(
            parser_lib::run_parser(".10", number_literal_lexer()),
            Some(0.1)
        );

        assert_eq!(
            parser_lib::run_parser("0e+2", number_literal_lexer()),
            Some(0.0e+2)
        );
        assert_eq!(
            parser_lib::run_parser("012e+2", number_literal_lexer()),
            Some(12.0e+2)
        );
        assert_eq!(
            parser_lib::run_parser("123e+2", number_literal_lexer()),
            Some(123.0e+2)
        );
        assert_eq!(
            parser_lib::run_parser("0.e+2", number_literal_lexer()),
            Some(0.0e+2)
        );
        assert_eq!(
            parser_lib::run_parser("123.e+2", number_literal_lexer()),
            Some(123.0e+2)
        );
        assert_eq!(
            parser_lib::run_parser("0.0e+2", number_literal_lexer()),
            Some(0.0e+2)
        );
        assert_eq!(
            parser_lib::run_parser("123.0e+2", number_literal_lexer()),
            Some(123.0e+2)
        );
        assert_eq!(
            parser_lib::run_parser("123.10e+2", number_literal_lexer()),
            Some(123.1e+2)
        );
        assert_eq!(
            parser_lib::run_parser(".0e+2", number_literal_lexer()),
            Some(0.0e+2)
        );
        assert_eq!(
            parser_lib::run_parser(".10e+2", number_literal_lexer()),
            Some(0.1e+2)
        );
    }

    #[test]
    fn test_number_literal_lexer2() {
        assert_eq!(
            parser_lib::run_parser("0e-2", number_literal_lexer()),
            Some(0.0e-2)
        );
        assert_eq!(
            parser_lib::run_parser("012e-2", number_literal_lexer()),
            Some(12.0e-2)
        );
        assert_eq!(
            parser_lib::run_parser("123e-2", number_literal_lexer()),
            Some(123.0e-2)
        );
        assert_eq!(
            parser_lib::run_parser("0.e-2", number_literal_lexer()),
            Some(0.0e-2)
        );
        assert_eq!(
            parser_lib::run_parser("123.e-2", number_literal_lexer()),
            Some(123.0e-2)
        );
        assert_eq!(
            parser_lib::run_parser("0.0e-2", number_literal_lexer()),
            Some(0.0e-2)
        );
        assert_eq!(
            parser_lib::run_parser("123.0e-2", number_literal_lexer()),
            Some(123.0e-2)
        );
        assert_eq!(
            parser_lib::run_parser("123.10e-2", number_literal_lexer()),
            Some(123.1e-2)
        );
        assert_eq!(
            parser_lib::run_parser(".0e-2", number_literal_lexer()),
            Some(0.0e-2)
        );
        assert_eq!(
            parser_lib::run_parser(".10e-2", number_literal_lexer()),
            Some(0.1e-2)
        );

        assert_eq!(
            parser_lib::run_parser("0e2", number_literal_lexer()),
            Some(0.0e2)
        );
        assert_eq!(
            parser_lib::run_parser("012e2", number_literal_lexer()),
            Some(12.0e2)
        );
        assert_eq!(
            parser_lib::run_parser("123e2", number_literal_lexer()),
            Some(123.0e2)
        );
        assert_eq!(
            parser_lib::run_parser("0.e2", number_literal_lexer()),
            Some(0.0e2)
        );
        assert_eq!(
            parser_lib::run_parser("123.e2", number_literal_lexer()),
            Some(123.0e2)
        );
        assert_eq!(
            parser_lib::run_parser("0.0e2", number_literal_lexer()),
            Some(0.0e2)
        );
        assert_eq!(
            parser_lib::run_parser("123.0e2", number_literal_lexer()),
            Some(123.0e2)
        );
        assert_eq!(
            parser_lib::run_parser("123.10e2", number_literal_lexer()),
            Some(123.1e2)
        );
        assert_eq!(
            parser_lib::run_parser(".0e2", number_literal_lexer()),
            Some(0.0e2)
        );
        assert_eq!(
            parser_lib::run_parser(".10e2", number_literal_lexer()),
            Some(0.1e2)
        );
    }

    #[test]
    fn test_number_literal_lexer3() {
        assert_eq!(
            parser_lib::run_parser("0x00", number_literal_lexer()),
            Some(0_f64)
        );
        assert_eq!(
            parser_lib::run_parser("0x0a", number_literal_lexer()),
            Some(10_f64)
        );
        assert_eq!(
            parser_lib::run_parser("0xf0", number_literal_lexer()),
            Some(240_f64)
        );

        assert_eq!(parser_lib::run_parser(".", number_literal_lexer()), None);
        assert_eq!(parser_lib::run_parser(".e2", number_literal_lexer()), None);
        assert_eq!(parser_lib::run_parser("e2", number_literal_lexer()), None);
    }

    #[test]
    fn test_identifier_lexer() {
        assert_eq!(parser_lib::run_parser("", identifier_lexer()), None);
        assert_eq!(parser_lib::run_parser(" ", identifier_lexer()), None);
        assert_eq!(parser_lib::run_parser("a", identifier_lexer()), Some("a"));
        assert_eq!(parser_lib::run_parser("_", identifier_lexer()), Some("_"));
        assert_eq!(parser_lib::run_parser("1", identifier_lexer()), None);
        assert_eq!(parser_lib::run_parser("aa", identifier_lexer()), Some("aa"));
        assert_eq!(parser_lib::run_parser("_a", identifier_lexer()), Some("_a"));
        assert_eq!(parser_lib::run_parser("1a", identifier_lexer()), None);
        assert_eq!(parser_lib::run_parser("a_", identifier_lexer()), Some("a_"));
        assert_eq!(parser_lib::run_parser("__", identifier_lexer()), Some("__"));
        assert_eq!(parser_lib::run_parser("1_", identifier_lexer()), None);
        assert_eq!(parser_lib::run_parser("a1", identifier_lexer()), Some("a1"));
        assert_eq!(parser_lib::run_parser("_1", identifier_lexer()), Some("_1"));
        assert_eq!(parser_lib::run_parser("11", identifier_lexer()), None);
        assert_eq!(parser_lib::run_parser("a ", identifier_lexer()), Some("a"));
        assert_eq!(parser_lib::run_parser("_ ", identifier_lexer()), Some("_"));
        assert_eq!(parser_lib::run_parser("1 ", identifier_lexer()), None);
        assert_eq!(parser_lib::run_parser(" a", identifier_lexer()), None);
        assert_eq!(parser_lib::run_parser(" _", identifier_lexer()), None);
        assert_eq!(parser_lib::run_parser(" 1", identifier_lexer()), None);
    }

    #[test]
    fn test_token_lexer() {
        assert_eq!(
            parser_lib::run_parser("a", token_lexer()),
            Some(lua_lexemes::Token::Identifier("a"))
        );
        assert_eq!(
            parser_lib::run_parser("if", token_lexer()),
            Some(lua_lexemes::Token::Keyword(lua_lexemes::Keyword::If))
        );
    }

    #[test]
    fn test_comment_lexer() {
        assert_eq!(
            parser_lib::run_parser(
                "-- This is a comment. \n And this is not.",
                parser_lib::parsed_string(comment_lexer())
            ),
            Some(((), "-- This is a comment. "))
        );
        assert_eq!(
            parser_lib::run_parser(
                "--[=[ This is a \n long \n multiline \n comment.]=] And this is not.",
                parser_lib::parsed_string(comment_lexer())
            ),
            Some(((), "--[=[ This is a \n long \n multiline \n comment.]=]"))
        );
        assert_eq!(
            parser_lib::run_parser(
                "-- [=[ This is a wrong \n example of multiline comment ]=]",
                parser_lib::parsed_string(comment_lexer())
            ),
            Some(((), "-- [=[ This is a wrong "))
        );
    }

    #[test]
    fn test_whitespace_lexer() {
        assert_eq!(
            parser_lib::run_parser(" \t\na\n", parser_lib::parsed_string(whitespace_lexer())),
            Some(((), " \t\n"))
        );
    }

    #[test]
    fn test_tokens_lexer() {
        assert_eq!(
            parser_lib::run_parser(
                r#"

-- A comment at the new line.

local x;
if smth == 'a string' {
    -- For whatever x is.
    x = 3.e2;
}
print(x);

"#,
                tokens_lexer()
            ),
            Some(vec![
                lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Local),
                lua_lexemes::Token::Identifier("x"),
                lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Semi),
                lua_lexemes::Token::Keyword(lua_lexemes::Keyword::If),
                lua_lexemes::Token::Identifier("smth"),
                lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Eq),
                lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(
                    "a string".to_string()
                )),
                lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::LBrace),
                lua_lexemes::Token::Identifier("x"),
                lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Assign),
                lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(3.0e2)),
                lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Semi),
                lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::RBrace),
                lua_lexemes::Token::Identifier("print"),
                lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::LParen),
                lua_lexemes::Token::Identifier("x"),
                lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::RParen),
                lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Semi),
            ])
        );
        assert_eq!(
            parser_lib::run_parser(
                r#"
local x;
x = "';
"#,
                tokens_lexer()
            ),
            None
        );
    }
}
