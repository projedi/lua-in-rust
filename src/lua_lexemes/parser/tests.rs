use super::*;

fn run_parser<'a, 'b: 'a, T>(
    input: &'b str,
    p: Box<dyn parser_lib::Parser<LocatedChars<'b>, T> + 'a>,
) -> (Option<T>, &'b str) {
    let (x, i) = parser_lib::run_parser(make_located(input), p);
    // TODO: No.
    (x, i.as_str())
}

fn loc(line: usize, column: usize) -> Location {
    Location { line, column }
}

#[test]
fn test_keyword_lexer() {
    let items = lua_lexemes::Keyword::ITEMS;
    for item in &items {
        assert_eq!(
            run_parser(item.to_str(), keyword_lexer()),
            (Some((loc(1, 1), *item)), "")
        );
    }
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
        (Some((loc(1, 1), "".to_string())), "")
    );
    assert_eq!(
        run_parser(r#""\"'""#, string_literal_lexer()),
        (Some((loc(1, 1), "\"'".to_string())), "")
    );
    assert_eq!(
        run_parser(r#"''"#, string_literal_lexer()),
        (Some((loc(1, 1), "".to_string())), "")
    );
    assert_eq!(
        run_parser(r#"'\'"'"#, string_literal_lexer()),
        (Some((loc(1, 1), "'\"".to_string())), "")
    );
    assert_eq!(run_parser(r#""'"#, string_literal_lexer()), (None, r#""'"#));
    assert_eq!(run_parser(r#"'""#, string_literal_lexer()), (None, r#"'""#));
    assert_eq!(
        run_parser(r#""\'""#, string_literal_lexer()),
        (None, r#""\'""#)
    );
    assert_eq!(
        run_parser(r#"'\"'"#, string_literal_lexer()),
        (None, r#"'\"'"#)
    );

    assert_eq!(
        run_parser(
            "\"a bc\\\" \n \\0 \\10 \\127 \\0010\"",
            string_literal_lexer()
        ),
        (
            Some((loc(1, 1), "a bc\" \n \x00 \x0a \x7f \x010".to_string())),
            ""
        )
    );
    assert_eq!(
        run_parser(
            "'a bc\\\' \n \\0 \\10 \\127 \\0010'",
            string_literal_lexer()
        ),
        (
            Some((loc(1, 1), "a bc' \n \x00 \x0a \x7f \x010".to_string())),
            ""
        )
    );
}

#[test]
fn test_long_brackets_lexer() {
    assert_eq!(
        run_parser(r#"[[abc]]"#, long_brackets_lexer()),
        (Some((loc(1, 1), r#"abc"#)), "")
    );
    assert_eq!(
        run_parser(r#"[=[abc]]]=]"#, long_brackets_lexer()),
        (Some((loc(1, 1), r#"abc]]"#)), "")
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
                r#"abc
def
"#
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
                r#"abc
def
"#
            )),
            ""
        )
    );
    assert_eq!(
        run_parser(r#"[=[ [=[ ]=] ]=]"#, long_brackets_lexer()),
        (Some((loc(1, 1), r#" [=[ "#)), " ]=]")
    );
    assert_eq!(
        run_parser(r#"[[ \' \" \n]]"#, long_brackets_lexer()),
        (Some((loc(1, 1), r#" \' \" \n"#)), "")
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
fn test_identifier_lexer() {
    assert_eq!(run_parser("", identifier_lexer()), (None, ""));
    assert_eq!(run_parser(" ", identifier_lexer()), (None, " "));
    assert_eq!(
        run_parser("a", identifier_lexer()),
        (Some((loc(1, 1), "a")), "")
    );
    assert_eq!(
        run_parser("_", identifier_lexer()),
        (Some((loc(1, 1), "_")), "")
    );
    assert_eq!(run_parser("1", identifier_lexer()), (None, "1"));
    assert_eq!(
        run_parser("aa", identifier_lexer()),
        (Some((loc(1, 1), "aa")), "")
    );
    assert_eq!(
        run_parser("_a", identifier_lexer()),
        (Some((loc(1, 1), "_a")), "")
    );
    assert_eq!(run_parser("1a", identifier_lexer()), (None, "1a"));
    assert_eq!(
        run_parser("a_", identifier_lexer()),
        (Some((loc(1, 1), "a_")), "")
    );
    assert_eq!(
        run_parser("__", identifier_lexer()),
        (Some((loc(1, 1), "__")), "")
    );
    assert_eq!(run_parser("1_", identifier_lexer()), (None, "1_"));
    assert_eq!(
        run_parser("a1", identifier_lexer()),
        (Some((loc(1, 1), "a1")), "")
    );
    assert_eq!(
        run_parser("_1", identifier_lexer()),
        (Some((loc(1, 1), "_1")), "")
    );
    assert_eq!(run_parser("11", identifier_lexer()), (None, "11"));
    assert_eq!(
        run_parser("a ", identifier_lexer()),
        (Some((loc(1, 1), "a")), " ")
    );
    assert_eq!(
        run_parser("_ ", identifier_lexer()),
        (Some((loc(1, 1), "_")), " ")
    );
    assert_eq!(run_parser("1 ", identifier_lexer()), (None, "1 "));
    assert_eq!(run_parser(" a", identifier_lexer()), (None, " a"));
    assert_eq!(run_parser(" _", identifier_lexer()), (None, " _"));
    assert_eq!(run_parser(" 1", identifier_lexer()), (None, " 1"));
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
                        "a string".to_string()
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
}
