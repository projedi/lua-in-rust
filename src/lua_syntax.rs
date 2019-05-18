use ::std::fmt;

macro_rules! plain_enum {
  ($p:vis $n:ident { $($v:ident => $s:literal),* $(,)?}) => {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    $p enum $n {
        $($v),*
    }

    impl fmt::Display for $n {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
              $($n::$v => write!(f, "{}", $s)),*
            }
        }
    }

    impl $n {
        pub const fn items_count() -> usize {
            $(($n::$v, 1).1 + )* 0
        }
        pub const ITEMS: [$n; $n::items_count()] = [
            $($n::$v),*
        ];
        pub fn create_sorted_items() -> [$n; $n::items_count()] {
            let mut sorted_items = $n::ITEMS;
            sorted_items.sort_unstable_by(|lhs, rhs| rhs.to_string().len().cmp(&lhs.to_string().len()));
            return sorted_items;
        }
        pub fn to_str(&self) -> &'static str {
            match self {
              $($n::$v => $s),*
            }
        }
    }
  }
}

plain_enum!(pub Keyword {
    And => "and",
    Break => "break",
    Do => "do",
    Else => "else",
    Elseif => "elseif",
    End => "end",
    False => "false",
    For => "for",
    Function => "function",
    If => "if",
    In => "in",
    Local => "local",
    Nil => "nil",
    Not => "not",
    Or => "or",
    Repeat => "repeat",
    Return => "return",
    Then => "then",
    True => "true",
    Until => "until",
    While => "while",
});

// TODO: Maybe needs to be split up.
plain_enum!(pub OtherToken {
    Add => "+",
    SubOrUnm => "-",
    Mul => "*",
    Div => "/",
    Mod => "%",
    Pow => "^",
    Concat => "..",
    Len => "#",
    Eq => "==",
    Neq => "~=",
    Lt => "<",
    Gt => ">",
    Le => "<=",
    Ge => ">=",
    Assign => "=",
    Vararg => "...",
    LParen => "(",
    RParen => ")",
    LBracket => "[",
    RBracket => "]",
    LBrace => "{",
    RBrace => "}",
    Semi => ";",
    Colon => ":",
    Comma => ",",
    Period => ".",
});

#[derive(Debug, PartialEq)]
pub enum Literal {
    StringLiteral(String),
    NumberLiteral(f64),
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Keyword(Keyword),
    Identifier(&'a str),
    OtherToken(OtherToken),
    Literal(Literal),
}
