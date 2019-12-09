use super::*;

fn run_parser<'a, T>(
    input: &'a str,
    p: parser_lib::Parser<LocatedChars<'a>, T>,
) -> (Option<T>, &'a str) {
    let (x, i) = parser_lib::run_parser(make_located(input), p);
    // TODO: No.
    (x, i.as_str())
}

fn loc(line: usize, column: usize) -> Location {
    Location { line, column }
}

#[test]
fn test_other_token_lexer() {
    let items = lua_lexemes::OtherToken::ITEMS;
    for item in &items {
        assert_eq!(
            run_parser(item.to_str(), other_token_lexer()),
            (Some((loc(1, 1), *item)), "")
        );
    }
}

#[test]
fn test_string_literal_lexer() {
    assert_eq!(
        run_parser(r#""""#, string_literal_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::QuotedStringLiteral {
                    string: "".to_string(),
                    quote: '"'
                }
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(r#""\"'""#, string_literal_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::QuotedStringLiteral {
                    string: "\"'".to_string(),
                    quote: '"'
                }
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(r#"''"#, string_literal_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::QuotedStringLiteral {
                    string: "".to_string(),
                    quote: '\''
                }
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(r#"'\'"'"#, string_literal_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::QuotedStringLiteral {
                    string: "'\"".to_string(),
                    quote: '\''
                }
            )),
            ""
        )
    );
    assert_eq!(run_parser(r#""'"#, string_literal_lexer()), (None, r#""'"#));
    assert_eq!(run_parser(r#"'""#, string_literal_lexer()), (None, r#"'""#));
    assert_eq!(
        run_parser(r#""\'""#, string_literal_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::QuotedStringLiteral {
                    string: "'".to_string(),
                    quote: '"',
                }
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(r#"'\"'"#, string_literal_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::QuotedStringLiteral {
                    string: "\"".to_string(),
                    quote: '\'',
                }
            )),
            ""
        )
    );

    assert_eq!(
        run_parser(
            "\"a bc\\\" \n \\0 \\10 \\127 \\0010\"",
            string_literal_lexer()
        ),
        (
            Some((
                loc(1, 1),
                lua_lexemes::QuotedStringLiteral {
                    string: "a bc\" \n \x00 \x0a \x7f \x010".to_string(),
                    quote: '"'
                }
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(
            "'a bc\\\' \n \\0 \\10 \\127 \\0010'",
            string_literal_lexer()
        ),
        (
            Some((
                loc(1, 1),
                lua_lexemes::QuotedStringLiteral {
                    string: "a bc' \n \x00 \x0a \x7f \x010".to_string(),
                    quote: '\''
                }
            )),
            ""
        )
    );

    assert_eq!(
        run_parser(
            r#""abc \
  def""#,
            string_literal_lexer()
        ),
        (
            Some((
                loc(1, 1),
                lua_lexemes::QuotedStringLiteral {
                    string: "abc \n  def".to_string(),
                    quote: '"'
                }
            )),
            ""
        )
    );
}

#[test]
fn test_long_brackets_lexer() {
    assert_eq!(
        run_parser(r#"[[abc]]"#, long_brackets_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::LongBrackets {
                    string: r#"abc"#,
                    level: 0,
                    ghost_newline: false
                }
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(r#"[=[abc]]]=]"#, long_brackets_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::LongBrackets {
                    string: r#"abc]]"#,
                    level: 1,
                    ghost_newline: false
                }
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(
            r#"[=[abc
def
]=]"#,
            long_brackets_lexer()
        ),
        (
            Some((
                loc(1, 1),
                lua_lexemes::LongBrackets {
                    string: r#"abc
def
"#,
                    level: 1,
                    ghost_newline: false
                }
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(
            r#"[=[
abc
def
]=]"#,
            long_brackets_lexer()
        ),
        (
            Some((
                loc(1, 1),
                lua_lexemes::LongBrackets {
                    string: r#"abc
def
"#,
                    level: 1,
                    ghost_newline: true
                }
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(r#"[=[ [=[ ]=] ]=]"#, long_brackets_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::LongBrackets {
                    string: r#" [=[ "#,
                    level: 1,
                    ghost_newline: false
                }
            )),
            " ]=]"
        )
    );
    assert_eq!(
        run_parser(r#"[[ \' \" \n]]"#, long_brackets_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::LongBrackets {
                    string: r#" \' \" \n"#,
                    level: 0,
                    ghost_newline: false
                }
            )),
            ""
        )
    );
}

#[test]
fn test_number_literal_lexer1() {
    assert_eq!(
        run_parser("0", number_literal_lexer()),
        (Some((loc(1, 1), 0.0)), "")
    );
    assert_eq!(
        run_parser("012", number_literal_lexer()),
        (Some((loc(1, 1), 12.0)), "")
    );
    assert_eq!(
        run_parser("123", number_literal_lexer()),
        (Some((loc(1, 1), 123.0)), "")
    );
    assert_eq!(
        run_parser("0.", number_literal_lexer()),
        (Some((loc(1, 1), 0.0)), "")
    );
    assert_eq!(
        run_parser("123.", number_literal_lexer()),
        (Some((loc(1, 1), 123.0)), "")
    );
    assert_eq!(
        run_parser("0.0", number_literal_lexer()),
        (Some((loc(1, 1), 0.0)), "")
    );
    assert_eq!(
        run_parser("123.0", number_literal_lexer()),
        (Some((loc(1, 1), 123.0)), "")
    );
    assert_eq!(
        run_parser("123.10", number_literal_lexer()),
        (Some((loc(1, 1), 123.1)), "")
    );
    assert_eq!(
        run_parser(".0", number_literal_lexer()),
        (Some((loc(1, 1), 0.0)), "")
    );
    assert_eq!(
        run_parser(".10", number_literal_lexer()),
        (Some((loc(1, 1), 0.1)), "")
    );

    assert_eq!(
        run_parser("0e+2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e+2)), "")
    );
    assert_eq!(
        run_parser("012e+2", number_literal_lexer()),
        (Some((loc(1, 1), 12.0e+2)), "")
    );
    assert_eq!(
        run_parser("123e+2", number_literal_lexer()),
        (Some((loc(1, 1), 123.0e+2)), "")
    );
    assert_eq!(
        run_parser("0.e+2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e+2)), "")
    );
    assert_eq!(
        run_parser("123.e+2", number_literal_lexer()),
        (Some((loc(1, 1), 123.0e+2)), "")
    );
    assert_eq!(
        run_parser("0.0e+2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e+2)), "")
    );
    assert_eq!(
        run_parser("123.0e+2", number_literal_lexer()),
        (Some((loc(1, 1), 123.0e+2)), "")
    );
    assert_eq!(
        run_parser("123.10e+2", number_literal_lexer()),
        (Some((loc(1, 1), 123.1e+2)), "")
    );
    assert_eq!(
        run_parser(".0e+2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e+2)), "")
    );
    assert_eq!(
        run_parser(".10e+2", number_literal_lexer()),
        (Some((loc(1, 1), 0.1e+2)), "")
    );
}

#[test]
fn test_number_literal_lexer2() {
    assert_eq!(
        run_parser("0e-2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e-2)), "")
    );
    assert_eq!(
        run_parser("012e-2", number_literal_lexer()),
        (Some((loc(1, 1), 12.0e-2)), "")
    );
    assert_eq!(
        run_parser("123e-2", number_literal_lexer()),
        (Some((loc(1, 1), 123.0e-2)), "")
    );
    assert_eq!(
        run_parser("0.e-2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e-2)), "")
    );
    assert_eq!(
        run_parser("123.e-2", number_literal_lexer()),
        (Some((loc(1, 1), 123.0e-2)), "")
    );
    assert_eq!(
        run_parser("0.0e-2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e-2)), "")
    );
    assert_eq!(
        run_parser("123.0e-2", number_literal_lexer()),
        (Some((loc(1, 1), 123.0e-2)), "")
    );
    assert_eq!(
        run_parser("123.10e-2", number_literal_lexer()),
        (Some((loc(1, 1), 123.1e-2)), "")
    );
    assert_eq!(
        run_parser(".0e-2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e-2)), "")
    );
    assert_eq!(
        run_parser(".10e-2", number_literal_lexer()),
        (Some((loc(1, 1), 0.1e-2)), "")
    );

    assert_eq!(
        run_parser("0e2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e2)), "")
    );
    assert_eq!(
        run_parser("012e2", number_literal_lexer()),
        (Some((loc(1, 1), 12.0e2)), "")
    );
    assert_eq!(
        run_parser("123e2", number_literal_lexer()),
        (Some((loc(1, 1), 123.0e2)), "")
    );
    assert_eq!(
        run_parser("0.e2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e2)), "")
    );
    assert_eq!(
        run_parser("123.e2", number_literal_lexer()),
        (Some((loc(1, 1), 123.0e2)), "")
    );
    assert_eq!(
        run_parser("0.0e2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e2)), "")
    );
    assert_eq!(
        run_parser("123.0e2", number_literal_lexer()),
        (Some((loc(1, 1), 123.0e2)), "")
    );
    assert_eq!(
        run_parser("123.10e2", number_literal_lexer()),
        (Some((loc(1, 1), 123.1e2)), "")
    );
    assert_eq!(
        run_parser(".0e2", number_literal_lexer()),
        (Some((loc(1, 1), 0.0e2)), "")
    );
    assert_eq!(
        run_parser(".10e2", number_literal_lexer()),
        (Some((loc(1, 1), 0.1e2)), "")
    );
}

#[test]
fn test_number_literal_lexer3() {
    assert_eq!(
        run_parser("0x00", number_literal_lexer()),
        (Some((loc(1, 1), 0_f64)), "")
    );
    assert_eq!(
        run_parser("0x0a", number_literal_lexer()),
        (Some((loc(1, 1), 10_f64)), "")
    );
    assert_eq!(
        run_parser("0xf0", number_literal_lexer()),
        (Some((loc(1, 1), 240_f64)), "")
    );

    assert_eq!(run_parser(".", number_literal_lexer()), (None, "."));
    assert_eq!(run_parser(".e2", number_literal_lexer()), (None, ".e2"));
    assert_eq!(run_parser("e2", number_literal_lexer()), (None, "e2"));
}

#[test]
fn test_keyword_or_identifier_lexer() {
    assert_eq!(run_parser("", keyword_or_identifier_lexer()), (None, ""));
    assert_eq!(run_parser(" ", keyword_or_identifier_lexer()), (None, " "));
    assert_eq!(
        run_parser("a", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "a")), "")
    );
    assert_eq!(
        run_parser("_", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "_")), "")
    );
    assert_eq!(run_parser("1", keyword_or_identifier_lexer()), (None, "1"));
    assert_eq!(
        run_parser("aa", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "aa")), "")
    );
    assert_eq!(
        run_parser("_a", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "_a")), "")
    );
    assert_eq!(
        run_parser("1a", keyword_or_identifier_lexer()),
        (None, "1a")
    );
    assert_eq!(
        run_parser("a_", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "a_")), "")
    );
    assert_eq!(
        run_parser("__", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "__")), "")
    );
    assert_eq!(
        run_parser("1_", keyword_or_identifier_lexer()),
        (None, "1_")
    );
    assert_eq!(
        run_parser("a1", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "a1")), "")
    );
    assert_eq!(
        run_parser("_1", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "_1")), "")
    );
    assert_eq!(
        run_parser("11", keyword_or_identifier_lexer()),
        (None, "11")
    );
    assert_eq!(
        run_parser("a ", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "a")), " ")
    );
    assert_eq!(
        run_parser("_ ", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "_")), " ")
    );
    assert_eq!(
        run_parser("1 ", keyword_or_identifier_lexer()),
        (None, "1 ")
    );
    assert_eq!(
        run_parser(" a", keyword_or_identifier_lexer()),
        (None, " a")
    );
    assert_eq!(
        run_parser(" _", keyword_or_identifier_lexer()),
        (None, " _")
    );
    assert_eq!(
        run_parser(" 1", keyword_or_identifier_lexer()),
        (None, " 1")
    );

    assert_eq!(
        run_parser("for", keyword_or_identifier_lexer()),
        (Some((loc(1, 1), "for")), "")
    );
}

#[test]
fn test_token_lexer() {
    assert_eq!(
        run_parser("a", token_lexer()),
        (Some((loc(1, 1), lua_lexemes::Token::Identifier("a"))), "")
    );
    assert_eq!(
        run_parser("if", token_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::Token::Keyword(lua_lexemes::Keyword::If)
            )),
            ""
        )
    );
    assert_eq!(
        run_parser("format", token_lexer()),
        (
            Some((loc(1, 1), lua_lexemes::Token::Identifier("format"))),
            ""
        )
    );
    assert_eq!(
        run_parser(".1", token_lexer()),
        (
            Some((
                loc(1, 1),
                lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(0.1))
            )),
            ""
        )
    );
}

#[test]
fn test_keyword_lexer() {
    let items = lua_lexemes::Keyword::ITEMS;
    for item in &items {
        assert_eq!(
            run_parser(item.to_str(), token_lexer()),
            (Some((loc(1, 1), lua_lexemes::Token::Keyword(*item))), "")
        );
    }
}

#[test]
fn test_comment_lexer() {
    assert_eq!(
        run_parser(
            "-- This is a comment. \n And this is not.",
            parser_lib::string::parsed_string(comment_lexer())
        ),
        (
            Some(((), (loc(1, 1), "-- This is a comment. "))),
            "\n And this is not."
        )
    );
    assert_eq!(
        run_parser(
            "--[=[ This is a \n long \n multiline \n comment.]=] And this is not.",
            parser_lib::string::parsed_string(comment_lexer())
        ),
        (
            Some((
                (),
                (
                    loc(1, 1),
                    "--[=[ This is a \n long \n multiline \n comment.]=]"
                )
            )),
            " And this is not."
        )
    );
    assert_eq!(
        run_parser(
            "-- [=[ This is a wrong \n example of multiline comment ]=]",
            parser_lib::string::parsed_string(comment_lexer())
        ),
        (
            Some(((), (loc(1, 1), "-- [=[ This is a wrong "))),
            "\n example of multiline comment ]=]"
        )
    );
}

#[test]
fn test_whitespace_lexer() {
    assert_eq!(
        run_parser(
            " \t\na\n",
            parser_lib::string::parsed_string(whitespace_lexer())
        ),
        (Some(((), (loc(1, 1), " \t\n"))), "a\n")
    );
}

#[test]
fn test_tokens_lexer() {
    assert_eq!(
        run_parser(
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
        (
            Some(vec![
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Local),
                    location: loc(5, 1)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Identifier("x"),
                    location: loc(5, 7)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Semi),
                    location: loc(5, 8)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Keyword(lua_lexemes::Keyword::If),
                    location: loc(6, 1)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Identifier("smth"),
                    location: loc(6, 4)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Eq),
                    location: loc(6, 9)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(
                        lua_lexemes::StringLiteral::QuotedStringLiteral(
                            lua_lexemes::QuotedStringLiteral {
                                string: "a string".to_string(),
                                quote: '\'',
                            }
                        )
                    )),
                    location: loc(6, 12)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::LBrace),
                    location: loc(6, 23)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Identifier("x"),
                    location: loc(8, 1)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Assign),
                    location: loc(8, 3)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(3.0e2)),
                    location: loc(8, 5)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Semi),
                    location: loc(8, 9)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::RBrace),
                    location: loc(9, 1)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Identifier("print"),
                    location: loc(10, 1)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::LParen),
                    location: loc(10, 6)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Identifier("x"),
                    location: loc(10, 7)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::RParen),
                    location: loc(10, 8)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Semi),
                    location: loc(10, 9)
                },
            ]),
            "",
        )
    );
    assert_eq!(
        run_parser(
            r#"
local x;
x = "';
"#,
            tokens_lexer()
        ),
        (None, "\"';\n")
    );
    assert_eq!(
        run_parser(
            r#"# special comment line
local
"#,
            tokens_lexer()
        ),
        (
            Some(vec![lua_lexemes::LocatedToken {
                token: lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Local),
                location: loc(2, 1)
            },]),
            "",
        )
    );
    assert_eq!(
        run_parser(
            r#"
# not a comment line
local
"#,
            tokens_lexer()
        ),
        (
            Some(vec![
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Len),
                    location: loc(2, 1)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Not),
                    location: loc(2, 3)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Identifier("a"),
                    location: loc(2, 7)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Identifier("comment"),
                    location: loc(2, 9)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Identifier("line"),
                    location: loc(2, 17)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Local),
                    location: loc(3, 1)
                },
            ]),
            "",
        )
    );
    assert_eq!(
        run_parser(
            r#"
x = [=[
A string, yo
]=]
"#,
            tokens_lexer()
        ),
        (
            Some(vec![
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Identifier("x"),
                    location: loc(2, 1)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Assign),
                    location: loc(2, 3)
                },
                lua_lexemes::LocatedToken {
                    token: lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(
                        lua_lexemes::StringLiteral::RawStringLiteral(lua_lexemes::LongBrackets {
                            string: "A string, yo\n",
                            level: 1,
                            ghost_newline: true,
                        })
                    )),
                    location: loc(2, 5)
                },
            ]),
            "",
        )
    );
}
