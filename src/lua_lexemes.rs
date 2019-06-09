macro_rules! plain_enum {
  ($p:vis $n:ident { $($v:ident),* $(,)?}) => {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    $p enum $n {
        $($v),*
    }

    impl $n {
        pub const ITEMS_COUNT: usize = $(($n::$v, 1).1 + )* 0;
        pub const ITEMS: [$n; $n::ITEMS_COUNT] = [
            $($n::$v),*
        ];
    }
  }
}

plain_enum!(pub Keyword {
    And,
    Break,
    Do,
    Else,
    Elseif,
    End,
    False,
    For,
    Function,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
});

impl Keyword {
    pub fn to_str(self) -> &'static str {
        match self {
            Keyword::And => "and",
            Keyword::Break => "break",
            Keyword::Do => "do",
            Keyword::Else => "else",
            Keyword::Elseif => "elseif",
            Keyword::End => "end",
            Keyword::False => "false",
            Keyword::For => "for",
            Keyword::Function => "function",
            Keyword::If => "if",
            Keyword::In => "in",
            Keyword::Local => "local",
            Keyword::Nil => "nil",
            Keyword::Not => "not",
            Keyword::Or => "or",
            Keyword::Repeat => "repeat",
            Keyword::Return => "return",
            Keyword::Then => "then",
            Keyword::True => "true",
            Keyword::Until => "until",
            Keyword::While => "while",
        }
    }
}

// TODO: Maybe needs to be split up.
plain_enum!(pub OtherToken {
    Add,
    SubOrUnm,
    Mul,
    Div,
    Mod,
    Pow,
    Concat,
    Len,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    Assign,
    Vararg,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Semi,
    Colon,
    Comma,
    Period,
});

impl OtherToken {
    pub fn to_str(self) -> &'static str {
        match self {
            OtherToken::Add => "+",
            OtherToken::SubOrUnm => "-",
            OtherToken::Mul => "*",
            OtherToken::Div => "/",
            OtherToken::Mod => "%",
            OtherToken::Pow => "^",
            OtherToken::Concat => "..",
            OtherToken::Len => "#",
            OtherToken::Eq => "==",
            OtherToken::Neq => "~=",
            OtherToken::Lt => "<",
            OtherToken::Gt => ">",
            OtherToken::Le => "<=",
            OtherToken::Ge => ">=",
            OtherToken::Assign => "=",
            OtherToken::Vararg => "...",
            OtherToken::LParen => "(",
            OtherToken::RParen => ")",
            OtherToken::LBracket => "[",
            OtherToken::RBracket => "]",
            OtherToken::LBrace => "{",
            OtherToken::RBrace => "}",
            OtherToken::Semi => ";",
            OtherToken::Colon => ":",
            OtherToken::Comma => ",",
            OtherToken::Period => ".",
        }
    }
}

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
