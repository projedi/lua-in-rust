// TODO: Remove when https://github.com/rust-lang/rust-clippy/issues/4187
// is resolved.
#![cfg_attr(feature = "cargo-clippy", allow(clippy::redundant_closure))]

use crate::utils::enumerate::{enumerate, Enumerate};

#[derive(Clone)]
struct ParserState<I> {
    iterator: Enumerate<I>,
}

type ParserInput<I> = ParserState<I>;
type ParserOutput<I, T> = (Option<T>, ParserState<I>);

pub struct Parser<'a, I, T> {
    closure: Box<dyn Fn(ParserInput<I>) -> ParserOutput<I, T> + 'a>,
}

impl<'a, I, T> Parser<'a, I, T> {
    fn make(f: impl Fn(ParserState<I>) -> (Option<T>, ParserState<I>) + 'a) -> Parser<'a, I, T> {
        Parser {
            closure: Box::new(f),
        }
    }

    fn run(&self, s: ParserState<I>) -> (Option<T>, ParserState<I>) {
        (self.closure)(s)
    }
}

pub fn run_parser<I: Iterator, T>(iterator: I, p: Parser<I, T>) -> (Option<T>, I) {
    trace_scoped!("parser_lib run_parser");
    let s = ParserState {
        iterator: enumerate(iterator),
    };
    let (result, s) = p.run(s);
    (result, s.iterator.into_inner())
}

pub fn empty_parser<'a, I: 'a>() -> Parser<'a, I, ()> {
    Parser::make(|s| (Some(()), s))
}

pub fn fail_parser<'a, I: 'a, T: 'a>() -> Parser<'a, I, T> {
    Parser::make(|s| (None, s))
}

pub fn fmap<'a, I: 'a, T1: 'a, T2: 'a>(
    f: impl Fn(T1) -> T2 + 'a,
    p: Parser<'a, I, T1>,
) -> Parser<'a, I, T2> {
    Parser::make(move |s| {
        let (x, s) = p.run(s);
        match x {
            Some(x) => (Some(f(x)), s),
            None => (None, s),
        }
    })
}

pub fn map_filter<'a, I: 'a, T1: 'a, T2: 'a>(
    f: impl Fn(T1) -> Option<T2> + 'a,
    p: Parser<'a, I, T1>,
) -> Parser<'a, I, T2> {
    Parser::make(move |s| {
        let (x, s) = p.run(s);
        match x {
            Some(x) => (f(x), s),
            None => (None, s),
        }
    })
}

pub fn satisfies<'a, T: 'a, I: Iterator<Item = T> + Clone + 'a>(
    f: impl Fn(&T) -> bool + 'a,
) -> Parser<'a, I, T> {
    Parser::make(move |s: ParserState<I>| {
        let mut out_s = s.clone();
        let c = out_s.iterator.next();
        match c {
            Some(c) => {
                if f(&c) {
                    (Some(c), out_s)
                } else {
                    (None, s)
                }
            }
            None => (None, s),
        }
    })
}

pub fn map_satisfies<'a, T1: 'a, T2: 'a, I: Iterator<Item = T1> + Clone + 'a>(
    f: impl Fn(T1) -> Option<T2> + 'a,
) -> Parser<'a, I, T2> {
    Parser::make(move |s: ParserState<I>| {
        let mut out_s = s.clone();
        let c = out_s.iterator.next();
        match c {
            Some(c) => match f(c) {
                Some(c) => (Some(c), out_s),
                None => (None, s),
            },
            None => (None, s),
        }
    })
}

pub fn seq_bind<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Parser<'a, I, T1>,
    fp2: impl Fn(T1) -> Parser<'a, I, T2> + 'a,
) -> Parser<'a, I, T2> {
    Parser::make(move |s| {
        let (p1_result, s) = p1.run(s);
        match p1_result {
            Some(p1_result) => fp2(p1_result).run(s),
            None => (None, s),
        }
    })
}

pub fn seq<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Parser<'a, I, T1>,
    p2: Parser<'a, I, T2>,
) -> Parser<'a, I, (T1, T2)> {
    Parser::make(move |s| {
        let (p1_result, s) = p1.run(s);
        match p1_result {
            Some(p1_result) => {
                let (p2_result, s) = p2.run(s);
                match p2_result {
                    Some(p2_result) => (Some((p1_result, p2_result)), s),
                    None => (None, s),
                }
            }
            None => (None, s),
        }
    })
}

pub fn seq1<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Parser<'a, I, T1>,
    p2: Parser<'a, I, T2>,
) -> Parser<'a, I, T1> {
    Parser::make(move |s| {
        let (p1_result, s) = p1.run(s);
        match p1_result {
            Some(p1_result) => {
                let (p2_result, s) = p2.run(s);
                match p2_result {
                    Some(_) => (Some(p1_result), s),
                    None => (None, s),
                }
            }
            None => (None, s),
        }
    })
}

pub fn seq2<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Parser<'a, I, T1>,
    p2: Parser<'a, I, T2>,
) -> Parser<'a, I, T2> {
    Parser::make(move |s| {
        let (p1_result, s) = p1.run(s);
        match p1_result {
            Some(_) => {
                let (p2_result, s) = p2.run(s);
                match p2_result {
                    Some(p2_result) => (Some(p2_result), s),
                    None => (None, s),
                }
            }
            None => (None, s),
        }
    })
}

pub fn seq_<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Parser<'a, I, T1>,
    p2: Parser<'a, I, T2>,
) -> Parser<'a, I, ()> {
    Parser::make(move |s| {
        let (p1_result, s) = p1.run(s);
        match p1_result {
            Some(_) => {
                let (p2_result, s) = p2.run(s);
                match p2_result {
                    Some(_) => (Some(()), s),
                    None => (None, s),
                }
            }
            None => (None, s),
        }
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
    p1: Parser<'a, I, T>,
    p2: Parser<'a, I, T>,
) -> Parser<'a, I, T> {
    Parser::make(move |s: ParserState<I>| {
        let (p1_result, p1_s) = p1.run(s.clone());
        match p1_result {
            Some(_) => (p1_result, p1_s),
            None => p2.run(s),
        }
    })
}

pub fn choices<'a, I: Clone + 'a, T: 'a>(ps: Vec<Parser<'a, I, T>>) -> Parser<'a, I, T> {
    let mut r = fail_parser();
    for p in ps {
        r = choice(r, p)
    }
    r
}

pub fn many<'a, I: Clone + 'a, T: 'a>(p: Parser<'a, I, T>) -> Parser<'a, I, Vec<T>> {
    Parser::make(move |s: ParserState<I>| {
        let mut results: Vec<T> = Vec::new();
        let mut cur_s = s;
        loop {
            let (p_result, new_s) = p.run(cur_s.clone());
            match p_result {
                Some(x) => {
                    results.push(x);
                    cur_s = new_s;
                }
                None => {
                    break;
                }
            }
        }
        (Some(results), cur_s)
    })
}

pub fn many1<'a, I: Clone + 'a, T: 'a>(p: Parser<'a, I, T>) -> Parser<'a, I, Vec<T>> {
    Parser::make(move |s| {
        let (x, s) = p.run(s);
        match x {
            Some(x) => {
                let mut results: Vec<T> = vec![x];
                let mut cur_s = s;
                loop {
                    let (p_result, new_s) = p.run(cur_s.clone());
                    match p_result {
                        Some(x) => {
                            results.push(x);
                            cur_s = new_s;
                        }
                        None => {
                            break;
                        }
                    }
                }
                (Some(results), cur_s)
            }
            None => (None, s),
        }
    })
}

pub fn separated<'a, I: Clone + 'a, T1: 'a, T2: 'a>(
    p: Parser<'a, I, T1>,
    sep_p: Parser<'a, I, T2>,
    allow_hanging: bool,
) -> Parser<'a, I, Vec<T1>> {
    Parser::make(move |s| {
        let (x, s) = p.run(s);
        match x {
            Some(x) => {
                let mut results: Vec<T1> = vec![x];
                let mut cur_s = s;
                loop {
                    let (sep_result, new_s) = sep_p.run(cur_s.clone());
                    match sep_result {
                        None => {
                            break;
                        }
                        Some(_) => {
                            if allow_hanging {
                                cur_s = new_s.clone();
                            }
                            let (p_result, new_s) = p.run(new_s);
                            match p_result {
                                Some(x) => {
                                    results.push(x);
                                    cur_s = new_s;
                                }
                                None => {
                                    break;
                                }
                            }
                        }
                    }
                }
                (Some(results), cur_s)
            }
            None => (None, s),
        }
    })
}

pub fn try_parser<'a, I: Clone + 'a, T: 'a>(p: Parser<'a, I, T>) -> Parser<'a, I, Option<T>> {
    Parser::make(move |s: ParserState<I>| {
        let (p_result, p_s) = p.run(s.clone());
        match p_result {
            None => (Some(None), s),
            Some(x) => (Some(Some(x)), p_s),
        }
    })
}

pub fn not_parser<'a, I: Clone + 'a, T: 'a>(p: Parser<'a, I, T>) -> Parser<'a, I, ()> {
    Parser::make(move |s: ParserState<I>| {
        let (p_result, _) = p.run(s.clone());
        match p_result {
            None => (Some(()), s),
            Some(_) => (None, s),
        }
    })
}

pub fn sequence_parser<'a, T1, T2, S: Iterator<Item = T1>, I: Iterator<Item = T2> + Clone + 'a>(
    f: impl Fn(&T1, &T2) -> bool + 'a,
    expected_sequence: impl Fn() -> S + 'a,
) -> Parser<'a, I, ()> {
    Parser::make(move |mut s: ParserState<I>| {
        let mut expected_iter = expected_sequence();
        loop {
            match expected_iter.next() {
                Some(expected_t) => {
                    let mut new_s = s.clone();
                    let actual_t = new_s.iterator.next();
                    match actual_t {
                        Some(actual_t) => {
                            if !f(&expected_t, &actual_t) {
                                return (None, s);
                            }
                            s = new_s;
                        }
                        None => {
                            return (None, s);
                        }
                    }
                }
                None => {
                    return (Some(()), s);
                }
            }
        }
    })
}

pub fn eof<'a, I: Iterator + Clone + 'a>() -> Parser<'a, I, ()> {
    Parser::make(move |s: ParserState<I>| {
        let mut new_s = s.clone();
        match new_s.iterator.next() {
            None => (Some(()), s),
            Some(_) => (None, s),
        }
    })
}

pub fn allow_recursion<'a, I: 'a, T: 'a>(
    pf: impl Fn() -> Parser<'a, I, T> + Clone + 'a,
) -> Parser<'a, I, T> {
    Parser::make(move |s| pf().run(s))
}

pub fn trace<'a, 'b: 'a, I: 'a, T: 'a>(name: &'b str, p: Parser<'a, I, T>) -> Parser<'a, I, T> {
    Parser::make(move |s| {
        trace_scoped!(name);
        p.run(s)
    })
}

pub mod string;
#[cfg(test)]
mod tests;
