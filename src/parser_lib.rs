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

pub mod string;
#[cfg(test)]
mod tests;
