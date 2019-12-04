use crate::lua_lexemes;
use std::fmt;

fn fmt_long_brackets<'a>(
    out: &mut dyn fmt::Write,
    token: &lua_lexemes::LongBrackets<'a>,
) -> fmt::Result {
    write!(out, "[")?;
    for _ in 0..token.level {
        write!(out, "=")?;
    }
    write!(out, "[")?;
    if token.ghost_newline {
        writeln!(out)?;
    }
    write!(out, "{}", token.string)?;
    write!(out, "]")?;
    for _ in 0..token.level {
        write!(out, "=")?;
    }
    write!(out, "]")
}

fn fmt_string(out: &mut dyn fmt::Write, token: &lua_lexemes::StringLiteral) -> fmt::Result {
    write!(out, "{}", token.quote)?;

    for c in token.string.chars() {
        match c {
            '\n' => write!(out, "\\n")?,
            '\x00' => write!(out, "\\0")?,
            '\x07' => write!(out, "\\a")?,
            '\\' => write!(out, "\\\\")?,
            _ => {
                if c == token.quote {
                    write!(out, "\\")?
                }
                write!(out, "{}", c)?
            }
        }
    }

    write!(out, "{}", token.quote)
}

fn fmt_token<'a>(out: &mut dyn fmt::Write, token: &lua_lexemes::Token<'a>) -> fmt::Result {
    match token {
        lua_lexemes::Token::Keyword(t) => write!(out, "{}", t.to_str()),
        lua_lexemes::Token::Identifier(t) => write!(out, "{}", t),
        lua_lexemes::Token::OtherToken(t) => write!(out, "{}", t.to_str()),
        lua_lexemes::Token::Literal(lua_lexemes::Literal::RawStringLiteral(t)) => {
            fmt_long_brackets(out, t)
        }
        lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(t)) => fmt_string(out, t),
        lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(t)) => write!(out, "{}", t),
    }
}

fn modify_location(loc: &mut lua_lexemes::Location, s: &str) {
    for c in s.chars() {
        match c {
            '\n' => {
                loc.line += 1;
                loc.column = 0;
            }
            _ => {
                loc.column += 1;
            }
        }
    }
}

fn add_padding(
    out: &mut dyn fmt::Write,
    loc_from: lua_lexemes::Location,
    loc_to: lua_lexemes::Location,
) -> fmt::Result {
    if loc_from.line > loc_to.line {
        Err(fmt::Error)
    } else if loc_from.line < loc_to.line {
        writeln!(out)?;
        add_padding(
            out,
            lua_lexemes::Location {
                line: loc_from.line + 1,
                column: 0,
            },
            loc_to,
        )
    } else if loc_from.column >= loc_to.column {
        Err(fmt::Error)
    } else if loc_from.column < loc_to.column - 1 {
        write!(out, " ")?;
        add_padding(
            out,
            lua_lexemes::Location {
                line: loc_from.line,
                column: loc_from.column + 1,
            },
            loc_to,
        )
    } else {
        Ok(())
    }
}

fn fmt_located_token<'a>(
    out: &mut dyn fmt::Write,
    loc: &mut lua_lexemes::Location,
    token: &lua_lexemes::LocatedToken<'a>,
) -> fmt::Result {
    let mut token_str = String::new();
    add_padding(&mut token_str, *loc, token.location)?;
    fmt_token(&mut token_str, &token.token)?;
    modify_location(loc, &token_str);
    write!(out, "{}", token_str)
}

fn fmt_tokens<'a>(
    out: &mut dyn fmt::Write,
    loc: &mut lua_lexemes::Location,
    tokens: &[lua_lexemes::LocatedToken<'a>],
) -> fmt::Result {
    for token in tokens {
        fmt_located_token(out, loc, token)?;
    }
    Ok(())
}

pub fn run_printer<'a>(tokens: &[lua_lexemes::LocatedToken<'a>]) -> Result<String, String> {
    let mut result = String::new();
    let mut loc = lua_lexemes::Location { line: 1, column: 0 };
    match fmt_tokens(&mut result, &mut loc, tokens) {
        Ok(_) => Ok(result),
        Err(e) => Err(e.to_string()),
    }
}

#[cfg(test)]
mod tests;
