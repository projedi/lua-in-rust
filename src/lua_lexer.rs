use crate::lua_lexemes;
use crate::parser_lib;

pub fn run_lexer<'a>(input: &'a str) -> Result<Vec<lua_lexemes::Token<'a>>, String> {
    match parser_lib::string::run_parser(input, tokens_lexer()) {
        (Some(result), _) => Ok(result),
        (None, _) => Err("Lexer failed".to_string()),
    }
}

fn keyword_lexer<'a, 'b: 'a>(
) -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, lua_lexemes::Keyword> + 'a> {
    let mut sorted_keywords = lua_lexemes::Keyword::ITEMS;
    sorted_keywords.sort_unstable_by(|lhs, rhs| rhs.to_str().len().cmp(&lhs.to_str().len()));
    parser_lib::choices(
        sorted_keywords
            .iter()
            .map(|item| {
                let i = *item;
                parser_lib::fmap(move |_| i, parser_lib::string::string_parser(item.to_str()))
            })
            .collect(),
    )
}

fn other_token_lexer<'a, 'b: 'a>(
) -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, lua_lexemes::OtherToken> + 'a> {
    let mut sorted_tokens = lua_lexemes::OtherToken::ITEMS;
    sorted_tokens.sort_unstable_by(|lhs, rhs| rhs.to_str().len().cmp(&lhs.to_str().len()));
    parser_lib::choices(
        sorted_tokens
            .iter()
            .map(|item| {
                let i = *item;
                parser_lib::fmap(move |_| i, parser_lib::string::string_parser(item.to_str()))
            })
            .collect(),
    )
}

fn string_literal_lexer<'a, 'b: 'a>(
) -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, String> + 'a> {
    let string_literal_char_lexer =
        |quote: char| -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, char> + 'a> {
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
                            parser_lib::string::parsed_string(parser_lib::seq_(
                                parser_lib::satisfies(|c: &char| c.is_ascii_digit()),
                                parser_lib::try_parser(parser_lib::seq_(
                                    parser_lib::satisfies(|c: &char| c.is_ascii_digit()),
                                    parser_lib::try_parser(parser_lib::satisfies(|c: &char| {
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
                parser_lib::satisfies(move |c: &char| *c != '\\' && *c != quote),
            )
        };
    parser_lib::map_filter(
        |result| result,
        parser_lib::try_parser(parser_lib::seq_bind(
            parser_lib::satisfies(|in_c: &char| *in_c == '\'' || *in_c == '"'),
            move |opening_quote| {
                parser_lib::seq_bind(
                    parser_lib::seq1(
                        parser_lib::many(string_literal_char_lexer(opening_quote)),
                        parser_lib::char_parser(opening_quote),
                    ),
                    |chars| {
                        parser_lib::fmap(
                            move |_| chars.iter().collect(),
                            parser_lib::empty_parser(),
                        )
                    },
                )
            },
        )),
    )
}

fn long_brackets_lexer<'a, 'b: 'a>(
) -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, &'b str> + 'a> {
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

fn number_literal_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, f64> + 'a>
{
    let hexadecimal_parser = || {
        parser_lib::seq_bind(
            parser_lib::string::parsed_string(parser_lib::many1(parser_lib::satisfies(
                |c: &char| c.is_digit(16),
            ))),
            |(_, num_str)| match i64::from_str_radix(num_str, 16) {
                Ok(num) => parser_lib::fmap(move |_| num as f64, parser_lib::empty_parser()),
                Err(_) => parser_lib::fail_parser(),
            },
        )
    };
    let integer_parser = || parser_lib::many1(parser_lib::satisfies(|c: &char| c.is_ascii_digit()));
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
            |(_, num_str)| match num_str.parse::<f64>() {
                Ok(num) => parser_lib::fmap(move |_| num, parser_lib::empty_parser()),
                Err(_) => parser_lib::fail_parser(),
            },
        )
    };
    parser_lib::map_filter(
        |result| result,
        parser_lib::try_parser(parser_lib::choice(
            parser_lib::seq2(
                parser_lib::string::string_parser("0x"),
                hexadecimal_parser(),
            ),
            decimal_parser(),
        )),
    )
}

fn identifier_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, &'b str> + 'a>
{
    let first_sym = parser_lib::satisfies(|c: &char| c.is_alphabetic() || *c == '_');
    let other_sym = parser_lib::satisfies(|c: &char| c.is_alphanumeric() || *c == '_');
    parser_lib::fmap(
        |(_, s)| s,
        parser_lib::string::parsed_string(parser_lib::seq(first_sym, parser_lib::many(other_sym))),
    )
}

fn token_lexer<'a, 'b: 'a>(
) -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, lua_lexemes::Token<'b>> + 'a> {
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

fn comment_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, ()> + 'a> {
    seqs_![
        parser_lib::string::string_parser("--"),
        parser_lib::choice(
            parser_lib::fmap(|_| (), long_brackets_lexer()),
            parser_lib::fmap(
                |_| (),
                parser_lib::many(parser_lib::satisfies(|c: &char| *c != '\n'))
            )
        )
    ]
}

fn whitespace_lexer<'a, 'b: 'a>() -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, ()> + 'a> {
    parser_lib::fmap(
        |_| (),
        parser_lib::many1(parser_lib::satisfies(|c: &char| c.is_ascii_whitespace())),
    )
}

fn tokens_lexer<'a, 'b: 'a>(
) -> Box<dyn parser_lib::Parser<std::str::Chars<'b>, Vec<lua_lexemes::Token<'b>>> + 'a> {
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
mod tests;
