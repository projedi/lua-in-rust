pub type Name<'a> = &'a str;

pub type Number = f64;

#[derive(Debug, PartialEq)]
pub struct Block<'a>(pub Vec<Stat<'a>>, pub Option<LastStat<'a>>);

// No guarantees here.
pub type NonEmptyVec<T> = Vec<T>;

#[derive(Debug, PartialEq)]
pub enum Stat<'a> {
    Assign(NonEmptyVec<Var<'a>>, NonEmptyVec<Exp<'a>>),
    FunctionCall(FunctionCall<'a>),
    Block(Block<'a>),
    While(Exp<'a>, Block<'a>),
    Repeat(Block<'a>, Exp<'a>),
    If(
        (Exp<'a>, Block<'a>),
        Vec<(Exp<'a>, Block<'a>)>,
        Option<Block<'a>>,
    ),
    For(Name<'a>, Exp<'a>, Exp<'a>, Option<Exp<'a>>, Block<'a>),
    ForIn(NonEmptyVec<Name<'a>>, NonEmptyVec<Exp<'a>>, Block<'a>),
    Function(FuncName<'a>, FuncBody<'a>),
    LocalFunctionDecl(Name<'a>, FuncBody<'a>),
    LocalVarDecl(NonEmptyVec<Name<'a>>, Option<NonEmptyVec<Exp<'a>>>),
}

#[derive(Debug, PartialEq)]
pub enum LastStat<'a> {
    Return(Option<NonEmptyVec<Exp<'a>>>),
    Break,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncName<'a>(pub Name<'a>, pub Vec<Name<'a>>, pub Option<Name<'a>>);

#[derive(Debug, PartialEq)]
pub enum Var<'a> {
    Name(Name<'a>),
    Index(PrefixExp<'a>, Exp<'a>),
    Field(PrefixExp<'a>, Name<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Exp<'a> {
    Nil,
    False,
    True,
    Number(Number),
    String(String),
    VarArgs,
    Function(Box<FuncBody<'a>>),
    PrefixExp(PrefixExp<'a>),
    TableCtor(TableCtor<'a>),
    BinOp(Box<Exp<'a>>, BinOp, Box<Exp<'a>>),
    UnOp(UnOp, Box<Exp<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum PrefixExp<'a> {
    Var(Box<Var<'a>>),
    FunctionCall(Box<FunctionCall<'a>>),
    Exp(Box<Exp<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum FunctionCall<'a> {
    FunctionCall(PrefixExp<'a>, Args<'a>),
    MethodCall(PrefixExp<'a>, Name<'a>, Args<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Args<'a> {
    Args(Option<NonEmptyVec<Exp<'a>>>),
    TableCtor(TableCtor<'a>),
    String(String),
}

#[derive(Debug, PartialEq)]
pub struct FuncBody<'a>(pub Option<ParList<'a>>, pub Block<'a>);

#[derive(Debug, PartialEq, Eq)]
pub enum ParList<'a> {
    ParList(NonEmptyVec<Name<'a>>, bool),
    VarArgs,
}

#[derive(Debug, PartialEq)]
pub struct TableCtor<'a>(pub Option<NonEmptyVec<Field<'a>>>);

#[derive(Debug, PartialEq)]
pub enum Field<'a> {
    Field(Exp<'a>, Exp<'a>),
    NameField(Name<'a>, Exp<'a>),
    NumberField(Exp<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Concat,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Neq,
    And,
    Or,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnOp {
    Unm,
    Not,
    Len,
}
