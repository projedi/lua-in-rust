use super::*;

fn k_do<'a>(line: usize, column: usize) -> lua_lexemes::LocatedToken<'a> {
    lua_lexemes::LocatedToken {
        token: lua_lexemes::Token::Keyword(lua_lexemes::Keyword::Do),
        location: lua_lexemes::Location { line, column },
    }
}

fn comma<'a>(line: usize, column: usize) -> lua_lexemes::LocatedToken<'a> {
    lua_lexemes::LocatedToken {
        token: lua_lexemes::Token::OtherToken(lua_lexemes::OtherToken::Comma),
        location: lua_lexemes::Location { line, column },
    }
}

fn name(s: &str, line: usize, column: usize) -> lua_lexemes::LocatedToken {
    lua_lexemes::LocatedToken {
        token: lua_lexemes::Token::Identifier(s),
        location: lua_lexemes::Location { line, column },
    }
}

fn string<'a>(
    string: String,
    quote: char,
    line: usize,
    column: usize,
) -> lua_lexemes::LocatedToken<'a> {
    lua_lexemes::LocatedToken {
        token: lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(
            lua_lexemes::StringLiteral::QuotedStringLiteral(lua_lexemes::QuotedStringLiteral {
                string,
                quote,
            }),
        )),
        location: lua_lexemes::Location { line, column },
    }
}

fn raw_string(
    string: &str,
    level: usize,
    ghost_newline: bool,
    line: usize,
    column: usize,
) -> lua_lexemes::LocatedToken {
    lua_lexemes::LocatedToken {
        token: lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(
            lua_lexemes::StringLiteral::RawStringLiteral(lua_lexemes::LongBrackets {
                string,
                level,
                ghost_newline,
            }),
        )),
        location: lua_lexemes::Location { line, column },
    }
}

fn num<'a>(n: f64, line: usize, column: usize) -> lua_lexemes::LocatedToken<'a> {
    lua_lexemes::LocatedToken {
        token: lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(n)),
        location: lua_lexemes::Location { line, column },
    }
}

fn print_tokens<'a>(tokens: &[lua_lexemes::LocatedToken<'a>]) -> String {
    run_printer(tokens).expect("Failed to print tokens")
}

#[test]
fn test_print_tokens() {
    assert_eq!(
        print_tokens(&[k_do(1, 1), comma(1, 3), comma(1, 4), k_do(1, 6),]),
        r#"do,, do"#
    );
    assert_eq!(
        print_tokens(&[k_do(2, 1), comma(2, 3),]),
        r#"
do,"#
    );
    assert_eq!(
        print_tokens(&[k_do(2, 1), comma(4, 4),]),
        r#"
do

   ,"#
    );
    assert_eq!(
        print_tokens(&[
            k_do(2, 1),
            name("a_thing", 2, 4),
            num(123.0, 2, 12),
            comma(2, 15),
            string("a string".to_string(), '"', 2, 17),
        ]),
        r#"
do a_thing 123, "a string""#
    );
    assert_eq!(
        print_tokens(&[string("With\nnew\nlines".to_string(), '"', 2, 1),]),
        r#"
"With\nnew\nlines""#
    );
    assert_eq!(
        print_tokens(&[string("With \" ' \u{7} \u{0} \\ ".to_string(), '"', 2, 1),]),
        r#"
"With \" ' \a \0 \\ ""#
    );
    assert_eq!(
        print_tokens(&[string("With \" ' \u{7} \u{0} \\ ".to_string(), '\'', 2, 1),]),
        r#"
'With " \' \a \0 \\ '"#
    );
    assert_eq!(
        print_tokens(&[raw_string("With\nnew\nlines", 0, false, 2, 1),]),
        r#"
[[With
new
lines]]"#
    );
    assert_eq!(
        print_tokens(&[raw_string("With\nnew\nlines", 2, true, 1, 1),]),
        r#"[==[
With
new
lines]==]"#
    );
}
