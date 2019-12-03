// TODO: Remove when https://github.com/rust-lang/rust-clippy/issues/4187
// is resolved.
#![cfg_attr(feature = "cargo-clippy", allow(clippy::redundant_closure))]

use crate::utils::enumerate::{enumerate, Enumerate};

#[derive(Clone)]
pub struct ParserState<I> {
    iterator: Enumerate<I>,
}

pub trait Parser<I, T>: Fn(ParserState<I>) -> (Option<T>, ParserState<I>) {}

impl<I, T, F: Fn(ParserState<I>) -> (Option<T>, ParserState<I>)> Parser<I, T> for F {}

pub fn run_parser<I: Iterator, T>(iterator: I, p: impl Parser<I, T>) -> (Option<T>, I) {
    let s = ParserState {
        iterator: enumerate(iterator),
    };
    let (result, s) = p(s);
    (result, s.iterator.into_inner())
}

pub fn empty_parser<I>() -> Box<dyn Parser<I, ()>> {
    Box::new(|s| (Some(()), s))
}

pub fn fail_parser<I, T>() -> Box<dyn Parser<I, T>> {
    Box::new(|s| (None, s))
}

pub fn fmap<'a, I: 'a, T1: 'a, T2: 'a>(
    f: impl Fn(T1) -> T2 + 'a,
    p: Box<dyn Parser<I, T1> + 'a>,
) -> Box<dyn Parser<I, T2> + 'a> {
    Box::new(move |s| {
        let (x, s) = p(s);
        match x {
            Some(x) => (Some(f(x)), s),
            None => (None, s),
        }
    })
}

pub fn map_filter<'a, I: 'a, T1: 'a, T2: 'a>(
    f: impl Fn(T1) -> Option<T2> + 'a,
    p: Box<dyn Parser<I, T1> + 'a>,
) -> Box<dyn Parser<I, T2> + 'a> {
    Box::new(move |s| {
        let (x, s) = p(s);
        match x {
            Some(x) => (f(x), s),
            None => (None, s),
        }
    })
}

pub fn satisfies<'a, T, I: Iterator<Item = T> + Clone>(
    f: impl Fn(&T) -> bool + 'a,
) -> Box<dyn Parser<I, T> + 'a> {
    Box::new(move |s: ParserState<I>| {
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

pub fn seq_bind<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<I, T1> + 'a>,
    fp2: impl Fn(T1) -> Box<dyn Parser<I, T2> + 'a> + 'a,
) -> Box<dyn Parser<I, T2> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s);
        match p1_result {
            Some(p1_result) => fp2(p1_result)(s),
            None => (None, s),
        }
    })
}

pub fn seq<'a, I: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<I, T1> + 'a>,
    p2: Box<dyn Parser<I, T2> + 'a>,
) -> Box<dyn Parser<I, (T1, T2)> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s);
        match p1_result {
            Some(p1_result) => {
                let (p2_result, s) = p2(s);
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
    p1: Box<dyn Parser<I, T1> + 'a>,
    p2: Box<dyn Parser<I, T2> + 'a>,
) -> Box<dyn Parser<I, T1> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s);
        match p1_result {
            Some(p1_result) => {
                let (p2_result, s) = p2(s);
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
    p1: Box<dyn Parser<I, T1> + 'a>,
    p2: Box<dyn Parser<I, T2> + 'a>,
) -> Box<dyn Parser<I, T2> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s);
        match p1_result {
            Some(_) => {
                let (p2_result, s) = p2(s);
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
    p1: Box<dyn Parser<I, T1> + 'a>,
    p2: Box<dyn Parser<I, T2> + 'a>,
) -> Box<dyn Parser<I, ()> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s);
        match p1_result {
            Some(_) => {
                let (p2_result, s) = p2(s);
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
    p1: Box<dyn Parser<I, T> + 'a>,
    p2: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, T> + 'a> {
    Box::new(move |s| {
        let (p1_result, p1_s) = p1(s.clone());
        match p1_result {
            Some(_) => (p1_result, p1_s),
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
            let (p_result, new_s) = p(cur_s.clone());
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

pub fn many1<'a, I: Clone + 'a, T: 'a>(
    p: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, Vec<T>> + 'a> {
    Box::new(move |s| {
        let (x, s) = p(s);
        match x {
            Some(x) => {
                let mut results: Vec<T> = vec![x];
                let mut cur_s = s;
                loop {
                    let (p_result, new_s) = p(cur_s.clone());
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

pub fn try_parser<'a, I: Clone + 'a, T: 'a>(
    p: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, Option<T>> + 'a> {
    Box::new(move |s| {
        let (p_result, p_s) = p(s.clone());
        match p_result {
            None => (Some(None), s),
            Some(x) => (Some(Some(x)), p_s),
        }
    })
}

pub fn not_parser<'a, I: Clone + 'a, T: 'a>(
    p: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, ()> + 'a> {
    Box::new(move |s| {
        let (p_result, _) = p(s.clone());
        match p_result {
            None => (Some(()), s),
            Some(_) => (None, s),
        }
    })
}

pub fn sequence_parser<'a, T1, T2, S: Iterator<Item = T1>, I: Iterator<Item = T2> + Clone>(
    f: impl Fn(&T1, &T2) -> bool + 'a,
    expected_sequence: impl Fn() -> S + 'a,
) -> Box<dyn Parser<I, ()> + 'a> {
    Box::new(move |mut s| {
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

pub fn eof<'a, I: Iterator + Clone + 'a>() -> Box<dyn Parser<I, ()> + 'a> {
    Box::new(move |s| {
        let mut new_s = s.clone();
        match new_s.iterator.next() {
            None => (Some(()), s),
            Some(_) => (None, s),
        }
    })
}

pub fn allow_recursion<'a, I, T>(
    pf: impl Fn() -> Box<dyn Parser<I, T> + 'a> + Clone + 'a,
) -> Box<dyn Parser<I, T> + 'a> {
    Box::new(move |s| pf()(s))
}

pub mod string;
#[cfg(test)]
mod tests;
