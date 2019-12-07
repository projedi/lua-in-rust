use crate::lua_lexemes;
use crate::parser_lib;
use crate::utils::located_chars::{make_located, LocatedChars, Location};

pub fn run_parser<'a>(input: &'a str) -> Result<Vec<lua_lexemes::LocatedToken<'a>>, String> {
    trace_scoped!("lua_lexer run_parser");
    match parser_lib::run_parser(make_located(input), tokens_lexer()) {
        (Some(result), _) => Ok(result),
        (None, iter) => Err(format!("Lexer failed at {}", iter.current_location())),
    }
}

fn other_token_lexer<'a, 'b: 'a>(
) -> parser_lib::Parser<'a, LocatedChars<'b>, (Location, lua_lexemes::OtherToken)> {
    let mut sorted_tokens = lua_lexemes::OtherToken::ITEMS;
    sorted_tokens.sort_unstable_by(|lhs, rhs| rhs.to_str().len().cmp(&lhs.to_str().len()));
    parser_lib::choices(
        sorted_tokens
            .iter()
            .map(|item| {
                let i = *item;
                parser_lib::fmap(
                    move |(loc, _)| (loc, i),
                    parser_lib::string::string_parser(item.to_str()),
                )
            })
            .collect(),
    )
}

fn string_literal_lexer<'a, 'b: 'a>(
) -> parser_lib::Parser<'a, LocatedChars<'b>, (Location, lua_lexemes::StringLiteral)> {
    let string_literal_char_lexer = |quote: char| -> parser_lib::Parser<
        'a,
        LocatedChars<'b>,
        (Location, char),
    > {
        parser_lib::choice(
            parser_lib::seq2(
                parser_lib::string::char_parser('\\'),
                parser_lib::choices(vec![
                    parser_lib::fmap(
                        |(loc, _)| (loc, '\x07'),
                        parser_lib::string::char_parser('a'),
                    ),
                    parser_lib::fmap(
                        |(loc, _)| (loc, '\x08'),
                        parser_lib::string::char_parser('b'),
                    ),
                    parser_lib::fmap(
                        |(loc, _)| (loc, '\x0C'),
                        parser_lib::string::char_parser('f'),
                    ),
                    parser_lib::fmap(|(loc, _)| (loc, '\n'), parser_lib::string::char_parser('n')),
                    parser_lib::fmap(
                        |(loc, _)| (loc, '\n'),
                        parser_lib::string::char_parser('\n'),
                    ),
                    parser_lib::fmap(|(loc, _)| (loc, '\r'), parser_lib::string::char_parser('r')),
                    parser_lib::fmap(|(loc, _)| (loc, '\t'), parser_lib::string::char_parser('t')),
                    parser_lib::fmap(
                        |(loc, _)| (loc, '\x0B'),
                        parser_lib::string::char_parser('v'),
                    ),
                    parser_lib::fmap(
                        |(loc, _)| (loc, '\\'),
                        parser_lib::string::char_parser('\\'),
                    ),
                    parser_lib::fmap(
                        |(loc, _)| (loc, '\''),
                        parser_lib::string::char_parser('\''),
                    ),
                    parser_lib::fmap(|(loc, _)| (loc, '"'), parser_lib::string::char_parser('"')),
                    parser_lib::seq_bind(
                        parser_lib::string::parsed_string(parser_lib::seq_(
                            parser_lib::satisfies(|c: &(_, char)| c.1.is_ascii_digit()),
                            parser_lib::try_parser(parser_lib::seq_(
                                parser_lib::satisfies(|c: &(_, char)| c.1.is_ascii_digit()),
                                parser_lib::try_parser(parser_lib::satisfies(|c: &(_, char)| {
                                    c.1.is_ascii_digit()
                                })),
                            )),
                        )),
                        |(_, (loc, code_str))| match code_str.parse::<u8>() {
                            Ok(code) => parser_lib::fmap(
                                move |_| (loc, char::from(code)),
                                parser_lib::empty_parser(),
                            ),
                            Err(_) => parser_lib::fail_parser(),
                        },
                    ),
                ]),
            ),
            parser_lib::satisfies(move |c: &(_, char)| c.1 != '\\' && c.1 != quote),
        )
    };
    parser_lib::map_filter(
        |result| result,
        parser_lib::try_parser(parser_lib::seq_bind(
            parser_lib::satisfies(|in_c: &(_, char)| in_c.1 == '\'' || in_c.1 == '"'),
            move |(loc, opening_quote)| {
                parser_lib::seq_bind(
                    parser_lib::seq1(
                        parser_lib::many(string_literal_char_lexer(opening_quote)),
                        parser_lib::string::char_parser(opening_quote),
                    ),
                    move |chars| {
                        parser_lib::fmap(
                            move |_| {
                                (
                                    loc,
                                    lua_lexemes::StringLiteral {
                                        string: chars.iter().map(|(_, c)| c).collect(),
                                        quote: opening_quote,
                                    },
                                )
                            },
                            parser_lib::empty_parser(),
                        )
                    },
                )
            },
        )),
    )
}

fn long_brackets_lexer<'a, 'b: 'a>(
) -> parser_lib::Parser<'a, LocatedChars<'b>, (Location, lua_lexemes::LongBrackets<'b>)> {
    parser_lib::seq_bind(
        parser_lib::seq(
            parser_lib::seq1(
                parser_lib::seq(
                    parser_lib::fmap(|(loc, _)| loc, parser_lib::string::char_parser('[')),
                    parser_lib::many(parser_lib::string::char_parser('=')),
                ),
                parser_lib::string::char_parser('['),
            ),
            parser_lib::try_parser(parser_lib::string::char_parser('\n')),
        ),
        |((loc, bracket_level), ghost_newline)| {
            let bracket_level_len = bracket_level.len();
            let closing_bracket_parser = || {
                let mut p = parser_lib::string::char_parser(']');
                for _ in 0..bracket_level_len {
                    p = parser_lib::seq1(p, parser_lib::string::char_parser('='));
                }
                p = parser_lib::seq1(p, parser_lib::string::char_parser(']'));
                p
            };
            parser_lib::fmap(
                move |(_, (_, s))| {
                    (
                        loc,
                        lua_lexemes::LongBrackets {
                            string: s,
                            level: bracket_level_len,
                            ghost_newline: ghost_newline.is_some(),
                        },
                    )
                },
                parser_lib::seq1(
                    parser_lib::string::parsed_string(parser_lib::many(parser_lib::seq2(
                        parser_lib::not_parser(closing_bracket_parser()),
                        parser_lib::satisfies(|_| true),
                    ))),
                    closing_bracket_parser(),
                ),
            )
        },
    )
}

fn number_literal_lexer<'a, 'b: 'a>() -> parser_lib::Parser<'a, LocatedChars<'b>, (Location, f64)> {
    let hexadecimal_parser = || {
        parser_lib::seq_bind(
            parser_lib::string::parsed_string(parser_lib::many1(parser_lib::satisfies(
                |c: &(_, char)| c.1.is_digit(16),
            ))),
            |(_, (_, num_str))| match i64::from_str_radix(num_str, 16) {
                Ok(num) => parser_lib::fmap(move |_| num as f64, parser_lib::empty_parser()),
                Err(_) => parser_lib::fail_parser(),
            },
        )
    };
    let integer_parser =
        || parser_lib::many1(parser_lib::satisfies(|c: &(_, char)| c.1.is_ascii_digit()));
    let point_parser = || parser_lib::string::char_parser('.');
    let exponent_parser = || {
        seqs_![
            parser_lib::choice(
                parser_lib::string::char_parser('E'),
                parser_lib::string::char_parser('e')
            ),
            parser_lib::try_parser(parser_lib::choice(
                parser_lib::string::char_parser('+'),
                parser_lib::string::char_parser('-')
            )),
            integer_parser(),
        ]
    };
    let decimal_parser = || {
        parser_lib::seq_bind(
            parser_lib::string::parsed_string(parser_lib::choices(vec![
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
            |(_, (loc, num_str)): (_, (Location, &'b str))| match num_str.parse::<f64>() {
                Ok(num) => parser_lib::fmap(move |_| (loc, num), parser_lib::empty_parser()),
                Err(_) => parser_lib::fail_parser(),
            },
        )
    };
    parser_lib::map_filter(
        |result| result,
        parser_lib::try_parser(parser_lib::choice(
            parser_lib::seq(
                parser_lib::fmap(|(loc, _)| loc, parser_lib::string::string_parser("0x")),
                hexadecimal_parser(),
            ),
            decimal_parser(),
        )),
    )
}

fn keyword_or_identifier_lexer<'a, 'b: 'a>(
) -> parser_lib::Parser<'a, LocatedChars<'b>, (Location, &'b str)> {
    let first_sym = parser_lib::satisfies(|c: &(_, char)| c.1.is_alphabetic() || c.1 == '_');
    let other_sym = parser_lib::satisfies(|c: &(_, char)| c.1.is_alphanumeric() || c.1 == '_');
    parser_lib::fmap(
        |(_, s)| s,
        parser_lib::string::parsed_string(parser_lib::seq(first_sym, parser_lib::many(other_sym))),
    )
}

fn get_keyword_or_identifier(s: &str) -> lua_lexemes::Token {
    match lua_lexemes::Keyword::ITEMS.iter().find(|x| x.to_str() == s) {
        Some(k) => lua_lexemes::Token::Keyword(*k),
        None => lua_lexemes::Token::Identifier(s),
    }
}

fn token_lexer<'a, 'b: 'a>(
) -> parser_lib::Parser<'a, LocatedChars<'b>, (Location, lua_lexemes::Token<'b>)> {
    parser_lib::choices(vec![
        parser_lib::fmap(
            |(loc, t)| (loc, get_keyword_or_identifier(t)),
            keyword_or_identifier_lexer(),
        ),
        parser_lib::fmap(
            |(loc, t)| {
                (
                    loc,
                    lua_lexemes::Token::Literal(lua_lexemes::Literal::RawStringLiteral(t)),
                )
            },
            long_brackets_lexer(),
        ),
        parser_lib::fmap(
            |(loc, t)| (loc, lua_lexemes::Token::OtherToken(t)),
            other_token_lexer(),
        ),
        parser_lib::fmap(
            |(loc, t)| {
                (
                    loc,
                    lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(t)),
                )
            },
            string_literal_lexer(),
        ),
        parser_lib::fmap(
            |(loc, t)| {
                (
                    loc,
                    lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(t)),
                )
            },
            number_literal_lexer(),
        ),
    ])
}

fn comment_lexer<'a, 'b: 'a>() -> parser_lib::Parser<'a, LocatedChars<'b>, ()> {
    seqs_![
        parser_lib::string::string_parser("--"),
        parser_lib::choice(
            parser_lib::fmap(|_| (), long_brackets_lexer()),
            parser_lib::fmap(
                |_| (),
                parser_lib::many(parser_lib::satisfies(|c: &(_, char)| c.1 != '\n'))
            )
        )
    ]
}

fn whitespace_lexer<'a, 'b: 'a>() -> parser_lib::Parser<'a, LocatedChars<'b>, ()> {
    parser_lib::fmap(
        |_| (),
        parser_lib::many1(parser_lib::satisfies(|c: &(_, char)| {
            c.1.is_ascii_whitespace()
        })),
    )
}

fn located_token_lexer<'a, 'b: 'a>(
) -> parser_lib::Parser<'a, LocatedChars<'b>, lua_lexemes::LocatedToken<'b>> {
    parser_lib::fmap(
        |(location, token)| lua_lexemes::LocatedToken { location, token },
        token_lexer(),
    )
}

fn tokens_lexer<'a, 'b: 'a>(
) -> parser_lib::Parser<'a, LocatedChars<'b>, Vec<lua_lexemes::LocatedToken<'b>>> {
    let shebang_lexer = || {
        seqs_![
            parser_lib::string::string_parser("#!"),
            parser_lib::fmap(
                |_| (),
                parser_lib::many(parser_lib::satisfies(|c: &(_, char)| c.1 != '\n'))
            ),
        ]
    };
    let skip_lexer = || {
        seqs_![
            parser_lib::try_parser(shebang_lexer()),
            parser_lib::many(parser_lib::choice(whitespace_lexer(), comment_lexer())),
        ]
    };
    parser_lib::seq1(
        parser_lib::seq2(
            skip_lexer(),
            parser_lib::many(parser_lib::seq1(located_token_lexer(), skip_lexer())),
        ),
        parser_lib::eof(),
    )
}

#[cfg(test)]
mod tests;
