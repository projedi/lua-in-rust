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

fn fmt_quoted_string(
    out: &mut dyn fmt::Write,
    token: &lua_lexemes::QuotedStringLiteral,
) -> fmt::Result {
    write!(out, "{}", token.quote)?;

    for c in token.string.chars() {
        match c {
            '\x07' => write!(out, "\\a")?,
            '\x08' => write!(out, "\\b")?,
            '\x0C' => write!(out, "\\f")?,
            '\n' => write!(out, "\\n")?,
            '\r' => write!(out, "\\r")?,
            '\t' => write!(out, "\\t")?,
            '\x0B' => write!(out, "\\v")?,
            '\\' => write!(out, "\\\\")?,
            '\x00' => write!(out, "\\0")?,
            '\x01' => write!(out, "\\1")?,
            '\x02' => write!(out, "\\2")?,
            '\x03' => write!(out, "\\3")?,
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

pub fn fmt_string<'a>(out: &mut dyn fmt::Write, s: &lua_lexemes::StringLiteral<'a>) -> fmt::Result {
    match s {
        lua_lexemes::StringLiteral::QuotedStringLiteral(s) => fmt_quoted_string(out, s),
        lua_lexemes::StringLiteral::RawStringLiteral(s) => fmt_long_brackets(out, s),
    }
}

type PrinterResult = Result<(), String>;

fn fmt_token<'a>(out: &mut dyn fmt::Write, token: &lua_lexemes::Token<'a>) -> PrinterResult {
    match token {
        lua_lexemes::Token::Keyword(t) => {
            write!(out, "{}", t.to_str()).map_err(|_| "keyword".to_string())
        }
        lua_lexemes::Token::Identifier(t) => {
            write!(out, "{}", t).map_err(|_| "identifier".to_string())
        }
        lua_lexemes::Token::OtherToken(t) => {
            write!(out, "{}", t.to_str()).map_err(|_| "operator".to_string())
        }
        lua_lexemes::Token::Literal(lua_lexemes::Literal::StringLiteral(t)) => {
            fmt_string(out, t).map_err(|_| "string".to_string())
        }
        lua_lexemes::Token::Literal(lua_lexemes::Literal::NumberLiteral(t)) => {
            write!(out, "{}", t).map_err(|_| "number".to_string())
        }
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
) -> PrinterResult {
    if loc_from.line < loc_to.line {
        writeln!(out).map_err(|_| "vertical padding".to_string())?;
        add_padding(
            out,
            lua_lexemes::Location {
                line: loc_from.line + 1,
                column: 0,
            },
            loc_to,
        )
    } else if loc_from.line == loc_to.line && loc_from.column < loc_to.column - 1 {
        write!(out, " ").map_err(|_| "horizontal padding".to_string())?;
        add_padding(
            out,
            lua_lexemes::Location {
                line: loc_from.line,
                column: loc_from.column + 1,
            },
            loc_to,
        )
    } else {
        // No extra padding needed.
        Ok(())
    }
}

fn fmt_located_token<'a>(
    out: &mut dyn fmt::Write,
    loc: &mut lua_lexemes::Location,
    token: &lua_lexemes::LocatedToken<'a>,
) -> PrinterResult {
    let mut token_str = String::new();
    add_padding(&mut token_str, *loc, token.location)?;
    fmt_token(&mut token_str, &token.token)?;
    modify_location(loc, &token_str);
    write!(out, "{}", token_str).map_err(|_| "token with padding".to_string())
}

fn fmt_tokens<'a>(
    out: &mut dyn fmt::Write,
    loc: &mut lua_lexemes::Location,
    tokens: &[lua_lexemes::LocatedToken<'a>],
) -> PrinterResult {
    for token in tokens {
        fmt_located_token(out, loc, token)?;
    }
    Ok(())
}

pub fn run_printer<'a>(tokens: &[lua_lexemes::LocatedToken<'a>]) -> Result<String, String> {
    let mut result = String::new();
    let mut loc = lua_lexemes::Location { line: 1, column: 0 };
    fmt_tokens(&mut result, &mut loc, tokens)?;
    Ok(result)
}

#[cfg(test)]
mod tests;
