// TODO: Remove when https://github.com/rust-lang/rust-clippy/issues/4187
// is resolved.
#![cfg_attr(feature = "cargo-clippy", allow(clippy::redundant_closure))]

#[derive(Clone)]
pub struct ParserState<I> {
    iterator: I,
    consumed_count: usize,
}

pub trait Parser<I, T>: Fn(ParserState<I>) -> Option<(T, ParserState<I>)> {}

impl<I, T, F: Fn(ParserState<I>) -> Option<(T, ParserState<I>)>> Parser<I, T> for F {}

fn run_parser_impl<I, T>(iterator: I, p: impl Parser<I, T>) -> Option<(T, I)> {
    let s = ParserState {
        iterator,
        consumed_count: 0,
    };
    match p(s) {
        Some((result, s)) => Some((result, s.iterator)),
        None => None,
    }
}

pub fn run_parser<I, T>(iterator: I, p: impl Parser<I, T>) -> Option<T> {
    Some(run_parser_impl(iterator, p)?.0)
}

pub fn run_string_parser<'a, T>(
    input: &'a str,
    p: impl Parser<std::str::Chars<'a>, T>,
) -> Option<T> {
    run_parser(input.chars(), p)
}

pub fn empty_parser<I>() -> Box<dyn Parser<I, ()>> {
    Box::new(|s| Some(((), s)))
}

pub fn fail_parser<I, T>() -> Box<dyn Parser<I, T>> {
    Box::new(|_| None)
}

pub fn fmap<'a, I: 'a, T1: 'a, T2: 'a>(
    f: impl Fn(T1) -> T2 + 'a,
    p: Box<dyn Parser<I, T1> + 'a>,
) -> Box<dyn Parser<I, T2> + 'a> {
    Box::new(move |s| {
        let (x, s) = p(s)?;
        Some((f(x), s))
    })
}

pub fn map_filter<'a, I: 'a, T1: 'a, T2: 'a>(
    f: impl Fn(T1) -> Option<T2> + 'a,
    p: Box<dyn Parser<I, T1> + 'a>,
) -> Box<dyn Parser<I, T2> + 'a> {
    Box::new(move |s| {
        let (x, s) = p(s)?;
        let x = f(x)?;
        Some((x, s))
    })
}

pub fn satisfies<'a, T, I: Iterator<Item = T>>(
    f: impl Fn(&T) -> bool + 'a,
) -> Box<dyn Parser<I, T> + 'a> {
    Box::new(move |s: ParserState<I>| {
        let mut out_s = s;
        let c = out_s.iterator.next()?;
        out_s.consumed_count += 1;
        if f(&c) {
            Some((c, out_s))
        } else {
            None
        }
    })
}

pub fn char_parser<I: Iterator<Item = char>>(c: char) -> Box<dyn Parser<I, char>> {
    satisfies(move |in_c| *in_c == c)
}

pub fn seq_bind<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<I, T1> + 'a>,
    fp2: impl Fn(T1) -> Box<dyn Parser<I, T2> + 'a> + 'a,
) -> Box<dyn Parser<I, T2> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s)?;
        let (p2_result, s) = fp2(p1_result)(s)?;
        Some((p2_result, s))
    })
}

pub fn seq<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<I, T1> + 'a>,
    p2: Box<dyn Parser<I, T2> + 'a>,
) -> Box<dyn Parser<I, (T1, T2)> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s)?;
        let (p2_result, s) = p2(s)?;
        Some(((p1_result, p2_result), s))
    })
}

pub fn seq1<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<I, T1> + 'a>,
    p2: Box<dyn Parser<I, T2> + 'a>,
) -> Box<dyn Parser<I, T1> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s)?;
        let (_, s) = p2(s)?;
        Some((p1_result, s))
    })
}

pub fn seq2<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<I, T1> + 'a>,
    p2: Box<dyn Parser<I, T2> + 'a>,
) -> Box<dyn Parser<I, T2> + 'a> {
    Box::new(move |s| {
        let (_, s) = p1(s)?;
        let (p2_result, s) = p2(s)?;
        Some((p2_result, s))
    })
}

pub fn seq_<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<I, T1> + 'a>,
    p2: Box<dyn Parser<I, T2> + 'a>,
) -> Box<dyn Parser<I, ()> + 'a> {
    Box::new(move |s| {
        let (_, s) = p1(s)?;
        let (_, s) = p2(s)?;
        Some(((), s))
    })
}

macro_rules! seqs_ {
    ($($e:expr),* $(,)?) => {
        {
            let mut p = $crate::parser_lib::empty_parser();
            $(p = $crate::parser_lib::seq_(p, $e);)*
            p
        }
    }
}

pub fn choice<'a, I: Clone + 'a, T: 'a>(
    p1: Box<dyn Parser<I, T> + 'a>,
    p2: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, T> + 'a> {
    Box::new(move |s| {
        let p1_result = p1(s.clone());
        match p1_result {
            Some(_) => p1_result,
            None => p2(s),
        }
    })
}

pub fn choices<'a, I: Clone + 'a, T: 'a>(
    ps: Vec<Box<dyn Parser<I, T> + 'a>>,
) -> Box<dyn Parser<I, T> + 'a> {
    let mut r = fail_parser();
    for p in ps {
        r = choice(r, p)
    }
    r
}

pub fn many<'a, I: Clone + 'a, T: 'a>(
    p: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, Vec<T>> + 'a> {
    Box::new(move |s| {
        let mut results: Vec<T> = Vec::new();
        let mut cur_s = s;
        loop {
            let p_result = p(cur_s.clone());
            match p_result {
                Some((x, new_s)) => {
                    results.push(x);
                    cur_s = new_s;
                }
                None => {
                    break;
                }
            }
        }
        Some((results, cur_s))
    })
}

pub fn many1<'a, I: Clone + 'a, T: 'a>(
    p: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, Vec<T>> + 'a> {
    Box::new(move |s| {
        let (x, s) = p(s)?;
        let mut results: Vec<T> = vec![x];
        let mut cur_s = s;
        loop {
            let p_result = p(cur_s.clone());
            match p_result {
                Some((x, new_s)) => {
                    results.push(x);
                    cur_s = new_s;
                }
                None => {
                    break;
                }
            }
        }
        Some((results, cur_s))
    })
}

pub fn try_parser<'a, I: Clone + 'a, T: 'a>(
    p: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, Option<T>> + 'a> {
    Box::new(move |s| {
        let p_result = p(s.clone());
        match p_result {
            None => Some((None, s)),
            Some((x, s)) => Some((Some(x), s)),
        }
    })
}

pub fn not_parser<'a, I: Clone + 'a, T: 'a>(
    p: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, ()> + 'a> {
    Box::new(move |s| {
        let p_result = p(s.clone());
        match p_result {
            None => Some(((), s)),
            Some(_) => None,
        }
    })
}

pub fn sequence_parser<'a, T: PartialEq, S: Iterator<Item = T>, I: Iterator<Item = T>>(
    expected_sequence: impl Fn() -> S + 'a,
) -> Box<dyn Parser<I, ()> + 'a> {
    Box::new(move |mut s| {
        let mut expected_iter = expected_sequence();
        loop {
            match expected_iter.next() {
                Some(expected_t) => {
                    let actual_t = s.iterator.next()?;
                    s.consumed_count += 1;
                    if expected_t != actual_t {
                        return None;
                    }
                }
                None => {
                    return Some(((), s));
                }
            }
        }
    })
}

pub fn parsed_string<'a, 'b: 'a, T: 'a>(
    p: Box<dyn Parser<std::str::Chars<'b>, T> + 'a>,
) -> Box<dyn Parser<std::str::Chars<'b>, (T, &'b str)> + 'a> {
    Box::new(move |s| {
        let (p_result, p_s) = p(s.clone())?;
        Some((
            (
                p_result,
                &s.iterator.as_str()[0..p_s.consumed_count - s.consumed_count],
            ),
            p_s,
        ))
    })
}

pub fn string_parser<'a, 'b: 'a>(
    expected_str: &'a str,
) -> Box<dyn Parser<std::str::Chars<'b>, &'b str> + 'a> {
    fmap(
        |(_, s)| s,
        parsed_string(sequence_parser(move || expected_str.chars())),
    )
}

pub fn eof<'a, I: Iterator + 'a>() -> Box<dyn Parser<I, ()> + 'a> {
    Box::new(move |mut s| match s.iterator.next() {
        None => Some(((), s)),
        Some(_) => None,
    })
}

pub fn allow_recursion<'a, I, T>(
    pf: impl Fn() -> Box<dyn Parser<I, T> + 'a> + Clone + 'a,
) -> Box<dyn Parser<I, T> + 'a> {
    Box::new(move |s| pf()(s))
}

type ExpParser<'a, I, T> = Box<dyn Parser<I, T> + 'a>;
type UnOp<'a, T> = std::rc::Rc<dyn Fn(T) -> T + 'a>;
type BinOp<'a, T> = std::rc::Rc<dyn Fn(T, T) -> T + 'a>;
type TermParser<'a, I, T> = std::rc::Rc<dyn Fn() -> ExpParser<'a, I, T> + 'a>;
type UnOpParsers<'a, I, T> =
    std::rc::Rc<dyn Fn() -> Vec<Box<dyn Fn(ExpParser<'a, I, T>) -> ExpParser<'a, I, T> + 'a>> + 'a>;
type ExpLineParser<'a, I, T> =
    std::rc::Rc<dyn Fn(UnOpParsers<'a, I, T>, TermParser<'a, I, T>) -> ExpParser<'a, I, T> + 'a>;

fn unops_parser<'a, I: Clone + 'a, T: 'a>(
    pf: impl Fn() -> ExpParser<'a, I, T> + Clone + 'a,
    op_parsers: UnOpParsers<'a, I, T>,
) -> ExpParser<'a, I, T> {
    let mut parsers = vec![];
    for op_parser in op_parsers() {
        let pf = pf.clone();
        let op_parsers = op_parsers.clone();
        parsers.push(op_parser(allow_recursion(move || {
            unops_parser(pf.clone(), op_parsers.clone())
        })));
    }
    parsers.push(pf());
    choices(parsers)
}

fn unop_line_parser<'a, I: Clone + 'a, T: 'a>(
    new_unop_parsers: impl Fn() -> Vec<std::rc::Rc<dyn Fn() -> ExpParser<'a, I, UnOp<'a, T>> + 'a>>
        + Clone
        + 'a,
    next_parser: ExpLineParser<'a, I, T>,
    unop_parsers: UnOpParsers<'a, I, T>,
    term_parser: TermParser<'a, I, T>,
) -> ExpParser<'a, I, T> {
    let unop_parser = |c: ExpParser<'a, I, std::rc::Rc<dyn Fn(T) -> T>>,
                       p: ExpParser<'a, I, T>|
     -> ExpParser<'a, I, T> { fmap(|(op, e)| op(e), seq(c, p)) };
    let mut parsers = vec![];
    for op in new_unop_parsers() {
        parsers.push(unop_parser(
            op(),
            unops_parser(
                {
                    let new_unop_parsers = new_unop_parsers.clone();
                    let next_parser = next_parser.clone();
                    let unop_parsers = unop_parsers.clone();
                    let term_parser = term_parser.clone();
                    move || {
                        allow_recursion({
                            let new_unop_parsers = new_unop_parsers.clone();
                            let next_parser = next_parser.clone();
                            let unop_parsers = unop_parsers.clone();
                            let term_parser = term_parser.clone();
                            move || {
                                unop_line_parser(
                                    new_unop_parsers.clone(),
                                    next_parser.clone(),
                                    unop_parsers.clone(),
                                    term_parser.clone(),
                                )
                            }
                        })
                    }
                },
                unop_parsers.clone(),
            ),
        ));
    }
    parsers.push(next_parser(
        std::rc::Rc::new(move || {
            let mut unop_parsers = unop_parsers();
            for op in new_unop_parsers() {
                unop_parsers.push(Box::new(move |p| unop_parser(op(), p)))
            }
            unop_parsers
        }),
        term_parser.clone(),
    ));
    choices(parsers)
}

fn binop_right_line_parser<'a, I: Clone + 'a, T: 'a>(
    binop_parsers: impl Fn() -> Vec<std::rc::Rc<dyn Fn() -> ExpParser<'a, I, BinOp<'a, T>> + 'a>>
        + Clone
        + 'a,
    next_parser: ExpLineParser<'a, I, T>,
    unop_parsers: UnOpParsers<'a, I, T>,
    term_parser: TermParser<'a, I, T>,
) -> ExpParser<'a, I, T> {
    let mut parsers = vec![];
    for op in binop_parsers() {
        parsers.push(fmap(
            move |(lhs, (op, rhs))| op(lhs, rhs),
            seq(
                next_parser(unop_parsers.clone(), term_parser.clone()),
                seq(
                    op(),
                    unops_parser(
                        {
                            let binop_parsers = binop_parsers.clone();
                            let next_parser = next_parser.clone();
                            let unop_parsers = unop_parsers.clone();
                            let term_parser = term_parser.clone();
                            move || {
                                allow_recursion({
                                    let binop_parsers = binop_parsers.clone();
                                    let next_parser = next_parser.clone();
                                    let unop_parsers = unop_parsers.clone();
                                    let term_parser = term_parser.clone();
                                    move || {
                                        binop_right_line_parser(
                                            binop_parsers.clone(),
                                            next_parser.clone(),
                                            unop_parsers.clone(),
                                            term_parser.clone(),
                                        )
                                    }
                                })
                            }
                        },
                        unop_parsers.clone(),
                    ),
                ),
            ),
        ));
    }
    parsers.push(next_parser(unop_parsers.clone(), term_parser.clone()));
    choices(parsers)
}

fn binop_left_op_parser<'a, I: Clone + 'a, T: Clone + 'a>(
    op: std::rc::Rc<dyn Fn() -> ExpParser<'a, I, BinOp<'a, T>> + 'a>,
    lhs: T,
    next_parser: ExpLineParser<'a, I, T>,
    next_op_parser: std::rc::Rc<dyn Fn(T) -> ExpParser<'a, I, T> + 'a>,
    rec_parser: impl Fn(T, UnOpParsers<'a, I, T>, TermParser<'a, I, T>) -> ExpParser<'a, I, T>
        + Clone
        + 'a,
    unop_parsers: UnOpParsers<'a, I, T>,
    term_parser: TermParser<'a, I, T>,
) -> ExpParser<'a, I, T> {
    seq_bind(
        try_parser(seq(
            op(),
            unops_parser(
                {
                    let unop_parsers = unop_parsers.clone();
                    let term_parser = term_parser.clone();
                    move || next_parser(unop_parsers.clone(), term_parser.clone())
                },
                unop_parsers.clone(),
            ),
        )),
        {
            let unop_parsers = unop_parsers.clone();
            let term_parser = term_parser.clone();
            move |rhs| {
                let lhs = lhs.clone();
                match rhs {
                    Some((op, rhs)) => {
                        rec_parser(op(lhs, rhs), unop_parsers.clone(), term_parser.clone())
                    }
                    None => next_op_parser(lhs.clone()),
                }
            }
        },
    )
}

fn binop_left_line_parser_helper<'a, I: Clone + 'a, T: Clone + 'a>(
    ops: impl Fn() -> Vec<std::rc::Rc<dyn Fn() -> ExpParser<'a, I, BinOp<'a, T>> + 'a>> + Clone + 'a,
    lhs: T,
    next_parser: ExpLineParser<'a, I, T>,
    unop_parsers: UnOpParsers<'a, I, T>,
    term_parser: TermParser<'a, I, T>,
) -> ExpParser<'a, I, T> {
    let mut parser: std::rc::Rc<dyn Fn(T) -> ExpParser<'a, I, T> + 'a> =
        std::rc::Rc::new(|lhs: T| fmap(move |_| lhs.clone(), empty_parser()));
    for op in ops() {
        parser = {
            let ops = ops.clone();
            let next_parser = next_parser.clone();
            let parser = parser.clone();
            let unop_parsers = unop_parsers.clone();
            let term_parser = term_parser.clone();
            std::rc::Rc::new(move |lhs| {
                let ops = ops.clone();
                let next_parser = next_parser.clone();
                binop_left_op_parser(
                    op.clone(),
                    lhs,
                    next_parser.clone(),
                    parser.clone(),
                    move |lhs, unop_parsers, term_parser| {
                        binop_left_line_parser_helper(
                            ops.clone(),
                            lhs,
                            next_parser.clone(),
                            unop_parsers,
                            term_parser,
                        )
                    },
                    unop_parsers.clone(),
                    term_parser.clone(),
                )
            })
        };
    }
    parser(lhs)
}

fn binop_left_line_parser<'a, I: Clone + 'a, T: Clone + 'a>(
    ops: impl Fn() -> Vec<std::rc::Rc<dyn Fn() -> ExpParser<'a, I, BinOp<'a, T>> + 'a>> + Clone + 'a,
    next_parser: ExpLineParser<'a, I, T>,
    unop_parsers: UnOpParsers<'a, I, T>,
    term_parser: TermParser<'a, I, T>,
) -> ExpParser<'a, I, T> {
    seq_bind(
        next_parser(unop_parsers.clone(), term_parser.clone()),
        move |lhs| {
            binop_left_line_parser_helper(
                ops.clone(),
                lhs,
                next_parser.clone(),
                unop_parsers.clone(),
                term_parser.clone(),
            )
        },
    )
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Assoc {
    Left,
    Right,
}

pub type OpLineBinOp<'a, I, T> = std::rc::Rc<dyn Fn() -> ExpParser<'a, I, BinOp<'a, T>> + 'a>;
pub type OpLineUnOp<'a, I, T> = std::rc::Rc<dyn Fn() -> ExpParser<'a, I, UnOp<'a, T>> + 'a>;

pub enum OpLine<'a, I, T: 'a> {
    BinOp(Assoc, Vec<OpLineBinOp<'a, I, T>>),
    UnOp(Vec<OpLineUnOp<'a, I, T>>),
}

pub fn build_exp_parser<'a, I: Clone + 'a, T: Clone + 'a>(
    ops: impl Fn() -> Vec<OpLine<'a, I, T>> + Clone + 'a,
    term_parser: TermParser<'a, I, T>,
) -> ExpParser<'a, I, T> {
    let mut parser: ExpLineParser<'a, I, T> =
        std::rc::Rc::new(move |_unop_parsers, term_parser| term_parser());
    for op_line in ops() {
        match op_line {
            OpLine::UnOp(ops) => {
                parser = std::rc::Rc::new(move |unop_parsers, term_parser| {
                    let ops = ops.clone();
                    unop_line_parser(
                        move || ops.clone(),
                        parser.clone(),
                        unop_parsers,
                        term_parser,
                    )
                })
            }
            OpLine::BinOp(Assoc::Right, ops) => {
                parser = std::rc::Rc::new(move |unop_parsers, term_parser| {
                    let ops = ops.clone();
                    binop_right_line_parser(
                        move || ops.clone(),
                        parser.clone(),
                        unop_parsers,
                        term_parser,
                    )
                });
            }
            OpLine::BinOp(Assoc::Left, ops) => {
                parser = std::rc::Rc::new(move |unop_parsers, term_parser| {
                    let ops = ops.clone();
                    binop_left_line_parser(
                        move || ops.clone(),
                        parser.clone(),
                        unop_parsers,
                        term_parser,
                    )
                });
            }
        }
    }
    parser(std::rc::Rc::new(|| vec![]), term_parser)
}

#[cfg(test)]
mod tests;
