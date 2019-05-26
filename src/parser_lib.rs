#[derive(Copy, Clone)]
pub struct ParserState<'a> {
    input: &'a str,
    index: usize,
}

pub trait Parser<'a, T>: Fn(ParserState<'a>) -> Option<(T, ParserState<'a>)> {}

impl<'a, T, F: Fn(ParserState<'a>) -> Option<(T, ParserState<'a>)>> Parser<'a, T> for F {}

fn run_parser_impl<'a, T>(input: &'a str, p: impl Parser<'a, T>) -> Option<(T, &'a str)> {
    let s = ParserState { input, index: 0 };
    match p(s) {
        Some((result, s)) => Some((result, &input[s.index..])),
        None => None,
    }
}

pub fn run_parser<'a, T>(input: &'a str, p: impl Parser<'a, T>) -> Option<T> {
    Some(run_parser_impl(input, p)?.0)
}

pub fn empty_parser<'a>() -> Box<dyn Parser<'a, ()>> {
    Box::new(|s| Some(((), s)))
}

pub fn fail_parser<'a, T>() -> Box<dyn Parser<'a, T>> {
    Box::new(|_| None)
}

pub fn fmap<'a, 'b: 'a, T1: 'a, T2: 'a>(
    f: impl Fn(T1) -> T2 + 'a,
    p: Box<dyn Parser<'b, T1> + 'a>,
) -> Box<dyn Parser<'b, T2> + 'a> {
    Box::new(move |s| {
        let (x, s) = p(s)?;
        Some((f(x), s))
    })
}

pub fn satisfies<'a, 'b>(f: impl Fn(char) -> bool + 'b) -> Box<dyn Parser<'a, char> + 'b> {
    Box::new(move |s: ParserState<'a>| {
        let c = s.input[s.index..].chars().next()?;
        if f(c) {
            let mut out_s = s;
            out_s.index += 1;
            return Some((c, out_s));
        }
        None
    })
}

pub fn char_parser<'a>(c: char) -> Box<dyn Parser<'a, char>> {
    satisfies(move |in_c| in_c == c)
}

pub fn seq_bind<'a, 'b: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<'b, T1> + 'a>,
    fp2: impl Fn(T1) -> Box<dyn Parser<'b, T2> + 'a> + 'a,
) -> Box<dyn Parser<'b, T2> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s)?;
        let (p2_result, s) = fp2(p1_result)(s)?;
        Some((p2_result, s))
    })
}

pub fn seq<'a, 'b: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<'b, T1> + 'a>,
    p2: Box<dyn Parser<'b, T2> + 'a>,
) -> Box<dyn Parser<'b, (T1, T2)> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s)?;
        let (p2_result, s) = p2(s)?;
        Some(((p1_result, p2_result), s))
    })
}

pub fn seq1<'a, 'b: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<'b, T1> + 'a>,
    p2: Box<dyn Parser<'b, T2> + 'a>,
) -> Box<dyn Parser<'b, T1> + 'a> {
    Box::new(move |s| {
        let (p1_result, s) = p1(s)?;
        let (_, s) = p2(s)?;
        Some((p1_result, s))
    })
}

pub fn seq2<'a, 'b: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<'b, T1> + 'a>,
    p2: Box<dyn Parser<'b, T2> + 'a>,
) -> Box<dyn Parser<'b, T2> + 'a> {
    Box::new(move |s| {
        let (_, s) = p1(s)?;
        let (p2_result, s) = p2(s)?;
        Some((p2_result, s))
    })
}

pub fn seq_<'a, 'b: 'a, T1: 'a, T2: 'a>(
    p1: Box<dyn Parser<'b, T1> + 'a>,
    p2: Box<dyn Parser<'b, T2> + 'a>,
) -> Box<dyn Parser<'b, ()> + 'a> {
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

pub fn choice<'a, 'b: 'a, T: 'a>(
    p1: Box<dyn Parser<'b, T> + 'a>,
    p2: Box<dyn Parser<'b, T> + 'a>,
) -> Box<dyn Parser<'b, T> + 'a> {
    Box::new(move |s| {
        let p1_result = p1(s);
        match p1_result {
            Some(_) => p1_result,
            None => p2(s),
        }
    })
}

pub fn choices<'a, 'b: 'a, T: 'a>(
    ps: Vec<Box<dyn Parser<'b, T> + 'a>>,
) -> Box<dyn Parser<'b, T> + 'a> {
    let mut r = fail_parser();
    for p in ps {
        r = choice(r, p)
    }
    r
}

pub fn many<'a, 'b: 'a, T: 'a>(p: Box<dyn Parser<'b, T> + 'a>) -> Box<dyn Parser<'b, Vec<T>> + 'a> {
    Box::new(move |s| {
        let mut results: Vec<T> = Vec::new();
        let mut cur_s = s;
        loop {
            let p_result = p(cur_s);
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

pub fn many1<'a, 'b: 'a, T: 'a>(
    p: Box<dyn Parser<'b, T> + 'a>,
) -> Box<dyn Parser<'b, Vec<T>> + 'a> {
    Box::new(move |s| {
        let (x, s) = p(s)?;
        let mut results: Vec<T> = vec![x];
        let mut cur_s = s;
        loop {
            let p_result = p(cur_s);
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

pub fn try_parser<'a, 'b: 'a, T: 'a>(
    p: Box<dyn Parser<'b, T> + 'a>,
) -> Box<dyn Parser<'b, Option<T>> + 'a> {
    Box::new(move |s| {
        let p_result = p(s);
        match p_result {
            None => Some((None, s)),
            Some((x, s)) => Some((Some(x), s)),
        }
    })
}

pub fn not_parser<'a, 'b: 'a, T: 'a>(
    p: Box<dyn Parser<'b, T> + 'a>,
) -> Box<dyn Parser<'b, ()> + 'a> {
    Box::new(move |s| {
        let p_result = p(s);
        match p_result {
            None => Some(((), s)),
            Some(_) => None,
        }
    })
}

pub fn string_parser<'a, 'b>(expected_str: &'b str) -> Box<dyn Parser<'a, &'a str> + 'b> {
    Box::new(move |s: ParserState<'a>| {
        let mut expected_iter = expected_str.chars();
        let mut actual_iter = s.input[s.index..].chars();
        loop {
            match (expected_iter.next(), actual_iter.next()) {
                (None, _) => {
                    let mut new_s = s;
                    new_s.index += expected_str.len();
                    return Some((&s.input[s.index..new_s.index], new_s));
                }
                (Some(_), None) => {
                    return None;
                }
                (Some(expected_c), Some(actual_c)) => {
                    if expected_c != actual_c {
                        return None;
                    }
                }
            }
        }
    })
}

pub fn parsed_string<'a, 'b: 'a, T: 'a>(
    p: Box<dyn Parser<'b, T> + 'a>,
) -> Box<dyn Parser<'b, (T, &'b str)> + 'a> {
    Box::new(move |s| {
        let (p_result, p_s) = p(s)?;
        Some(((p_result, &s.input[s.index..p_s.index]), p_s))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        let s: String = "abc".to_string();
        assert_eq!(run_parser_impl("", empty_parser()), Some(((), "")));
        assert_eq!(run_parser_impl("abc", empty_parser()), Some(((), "abc")));
        assert_eq!(run_parser_impl(&s, empty_parser()), Some(((), &s[..])));
    }

    #[test]
    fn test_fail() {
        let s: String = "abc".to_string();
        assert_eq!(run_parser_impl("", fail_parser::<()>()), None);
        assert_eq!(run_parser_impl("abc", fail_parser::<()>()), None);
        assert_eq!(run_parser_impl(&s, fail_parser::<()>()), None);
    }

    #[test]
    fn test_fmap() {
        let i = 5;
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl("", fmap(|_| i, empty_parser())),
            Some((i, ""))
        );
        assert_eq!(
            run_parser_impl("abc", fmap(|_| i, empty_parser())),
            Some((i, "abc"))
        );
        assert_eq!(
            run_parser_impl(&s, fmap(|_| i, empty_parser())),
            Some((i, &s[..]))
        );

        assert_eq!(run_parser_impl("", fmap(|_| i, fail_parser::<()>())), None);
        assert_eq!(
            run_parser_impl("abc", fmap(|_| i, fail_parser::<()>())),
            None
        );
        assert_eq!(run_parser_impl(&s, fmap(|_| i, fail_parser::<()>())), None);
    }

    #[test]
    fn test_satisfies() {
        let s: String = "  abc".to_string();
        assert_eq!(run_parser_impl("", satisfies(|c| c.is_whitespace())), None);
        assert_eq!(
            run_parser_impl("abc", satisfies(|c| c.is_whitespace())),
            None
        );
        assert_eq!(
            run_parser_impl("  abc", satisfies(|c| c.is_whitespace())),
            Some((' ', " abc"))
        );
        assert_eq!(
            run_parser_impl(&s, satisfies(|c| c.is_whitespace())),
            Some((' ', &s[1..]))
        );
    }

    #[test]
    fn test_char_parser() {
        let s: String = "abc".to_string();
        assert_eq!(run_parser_impl("", char_parser('a')), None);
        assert_eq!(run_parser_impl("bca", char_parser('a')), None);
        assert_eq!(run_parser_impl("abc", char_parser('a')), Some(('a', "bc")));
        assert_eq!(run_parser_impl(&s, char_parser('a')), Some(('a', &s[1..])));
    }

    #[test]
    fn test_seq_bind() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl(
                "",
                seq_bind(char_parser('a'), move |c1| fmap(
                    move |c2| (c2, c1),
                    char_parser('b')
                ))
            ),
            None
        );
        assert_eq!(
            run_parser_impl(
                "bac",
                seq_bind(char_parser('a'), move |c1| fmap(
                    move |c2| (c2, c1),
                    char_parser('b')
                ))
            ),
            None
        );
        assert_eq!(
            run_parser_impl(
                "acb",
                seq_bind(char_parser('a'), move |c1| fmap(
                    move |c2| (c2, c1),
                    char_parser('b')
                ))
            ),
            None
        );
        assert_eq!(
            run_parser_impl(
                "abc",
                seq_bind(char_parser('a'), move |c1| fmap(
                    move |c2| (c2, c1),
                    char_parser('b')
                ))
            ),
            Some((('b', 'a'), "c"))
        );
        assert_eq!(
            run_parser_impl(
                &s,
                seq_bind(char_parser('a'), move |c1| fmap(
                    move |c2| (c2, c1),
                    char_parser('b')
                ))
            ),
            Some((('b', 'a'), &s[2..]))
        );
    }

    #[test]
    fn test_seq() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl("", seq(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("bac", seq(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("acb", seq(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("abc", seq(char_parser('a'), char_parser('b'))),
            Some((('a', 'b'), "c"))
        );
        assert_eq!(
            run_parser_impl(&s, seq(char_parser('a'), char_parser('b'))),
            Some((('a', 'b'), &s[2..]))
        );
    }

    #[test]
    fn test_seq1() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl("", seq1(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("bac", seq1(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("acb", seq1(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("abc", seq1(char_parser('a'), char_parser('b'))),
            Some(('a', "c"))
        );
        assert_eq!(
            run_parser_impl(&s, seq1(char_parser('a'), char_parser('b'))),
            Some(('a', &s[2..]))
        );
    }

    #[test]
    fn test_seq2() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl("", seq2(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("bac", seq2(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("acb", seq2(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("abc", seq2(char_parser('a'), char_parser('b'))),
            Some(('b', "c"))
        );
        assert_eq!(
            run_parser_impl(&s, seq2(char_parser('a'), char_parser('b'))),
            Some(('b', &s[2..]))
        );
    }

    #[test]
    fn test_seq_() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl("", seq_(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("bac", seq_(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("acb", seq_(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("abc", seq_(char_parser('a'), char_parser('b'))),
            Some(((), "c"))
        );
        assert_eq!(
            run_parser_impl(&s, seq_(char_parser('a'), char_parser('b'))),
            Some(((), &s[2..]))
        );
    }

    #[test]
    fn test_seqs_() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl("", seqs_![char_parser('a'), char_parser('b')]),
            None
        );
        assert_eq!(
            run_parser_impl("bac", seqs_![char_parser('a'), char_parser('b')]),
            None
        );
        assert_eq!(
            run_parser_impl("acb", seqs_![char_parser('a'), char_parser('b')]),
            None
        );
        assert_eq!(
            run_parser_impl("abc", seqs_![char_parser('a'), char_parser('b')]),
            Some(((), "c"))
        );
        assert_eq!(
            run_parser_impl(&s, seqs_![char_parser('a'), char_parser('b')]),
            Some(((), &s[2..]))
        );
    }

    #[test]
    fn test_choice() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl("", choice(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl("abc", choice(char_parser('a'), char_parser('b'))),
            Some(('a', "bc"))
        );
        assert_eq!(
            run_parser_impl("bac", choice(char_parser('a'), char_parser('b'))),
            Some(('b', "ac"))
        );
        assert_eq!(
            run_parser_impl("cba", choice(char_parser('a'), char_parser('b'))),
            None
        );
        assert_eq!(
            run_parser_impl(&s, choice(char_parser('a'), char_parser('b'))),
            Some(('a', &s[1..]))
        );
    }

    #[test]
    fn test_choices0() {
        let s: String = "abc".to_string();
        assert_eq!(run_parser_impl("", choices::<()>(vec![])), None);
        assert_eq!(run_parser_impl("abc", choices::<()>(vec![])), None);
        assert_eq!(run_parser_impl("bac", choices::<()>(vec![])), None);
        assert_eq!(run_parser_impl(&s, choices::<()>(vec![])), None);
    }

    #[test]
    fn test_choices1() {
        let s: String = "abc".to_string();
        assert_eq!(run_parser_impl("", choices(vec![char_parser('a')])), None);
        assert_eq!(
            run_parser_impl("abc", choices(vec![char_parser('a')])),
            Some(('a', "bc"))
        );
        assert_eq!(
            run_parser_impl("bac", choices(vec![char_parser('a')])),
            None
        );
        assert_eq!(
            run_parser_impl(&s, choices(vec![char_parser('a')])),
            Some(('a', &s[1..]))
        );
    }

    #[test]
    fn test_choices3() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl(
                "",
                choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
            ),
            None
        );
        assert_eq!(
            run_parser_impl(
                "abc",
                choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
            ),
            Some(('a', "bc"))
        );
        assert_eq!(
            run_parser_impl(
                "bac",
                choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
            ),
            Some(('b', "ac"))
        );
        assert_eq!(
            run_parser_impl(
                "cba",
                choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
            ),
            Some(('c', "ba"))
        );
        assert_eq!(
            run_parser_impl(
                "dba",
                choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
            ),
            None
        );
        assert_eq!(
            run_parser_impl(
                &s,
                choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
            ),
            Some(('a', &s[1..]))
        );
    }

    #[test]
    fn test_many() {
        let s: String = "abc def".to_string();
        assert_eq!(
            run_parser_impl("", many(satisfies(|c| c.is_alphanumeric()))),
            Some((vec![], ""))
        );
        assert_eq!(
            run_parser_impl("abc", many(satisfies(|c| c.is_alphanumeric()))),
            Some((vec!['a', 'b', 'c'], ""))
        );
        assert_eq!(
            run_parser_impl(" abc", many(satisfies(|c| c.is_alphanumeric()))),
            Some((vec![], " abc"))
        );
        assert_eq!(
            run_parser_impl("abc def", many(satisfies(|c| c.is_alphanumeric()))),
            Some((vec!['a', 'b', 'c'], " def"))
        );
        assert_eq!(
            run_parser_impl(&s, many(satisfies(|c| c.is_alphanumeric()))),
            Some((vec!['a', 'b', 'c'], &s[3..]))
        );
    }

    #[test]
    fn test_many1() {
        let s: String = "abc def".to_string();
        assert_eq!(
            run_parser_impl("", many1(satisfies(|c| c.is_alphanumeric()))),
            None
        );
        assert_eq!(
            run_parser_impl("abc", many1(satisfies(|c| c.is_alphanumeric()))),
            Some((vec!['a', 'b', 'c'], ""))
        );
        assert_eq!(
            run_parser_impl(" abc", many1(satisfies(|c| c.is_alphanumeric()))),
            None
        );
        assert_eq!(
            run_parser_impl("abc def", many1(satisfies(|c| c.is_alphanumeric()))),
            Some((vec!['a', 'b', 'c'], " def"))
        );
        assert_eq!(
            run_parser_impl(&s, many1(satisfies(|c| c.is_alphanumeric()))),
            Some((vec!['a', 'b', 'c'], &s[3..]))
        );
    }

    #[test]
    fn test_try_parser() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl("", try_parser(char_parser('a'))),
            Some((None, ""))
        );
        assert_eq!(
            run_parser_impl("bca", try_parser(char_parser('a'))),
            Some((None, "bca"))
        );
        assert_eq!(
            run_parser_impl("abc", try_parser(char_parser('a'))),
            Some((Some('a'), "bc"))
        );
        assert_eq!(
            run_parser_impl(&s, try_parser(char_parser('a'))),
            Some((Some('a'), &s[1..]))
        );
    }

    #[test]
    fn test_not_parser() {
        let s: String = "abc".to_string();
        assert_eq!(
            run_parser_impl("", not_parser(char_parser('a'))),
            Some(((), ""))
        );
        assert_eq!(
            run_parser_impl("bca", not_parser(char_parser('a'))),
            Some(((), "bca"))
        );
        assert_eq!(run_parser_impl("abc", not_parser(char_parser('a'))), None);
        assert_eq!(run_parser_impl(&s, not_parser(char_parser('a'))), None);
    }

    #[test]
    fn test_string_parser() {
        let s_before: String = "abc".to_string();
        let s: String = "abc def".to_string();
        let s_after: String = "abc".to_string();
        assert_eq!(run_parser_impl("", string_parser("abc")), None);
        assert_eq!(run_parser_impl("", string_parser("")), Some(("", "")));
        assert_eq!(run_parser_impl("abd", string_parser("abc")), None);
        assert_eq!(run_parser_impl("abd", string_parser("")), Some(("", "abd")));
        assert_eq!(
            run_parser_impl("abc", string_parser("abc")),
            Some(("abc", ""))
        );
        assert_eq!(
            run_parser_impl("abc def", string_parser("abc")),
            Some(("abc", " def"))
        );
        assert_eq!(
            run_parser_impl(&s, string_parser("abc")),
            Some((&s[0..3], &s[3..]))
        );
        assert_eq!(
            run_parser_impl(&s, string_parser(&s_before)),
            Some((&s[0..3], &s[3..]))
        );
        assert_eq!(
            run_parser_impl(&s, string_parser(&s_after)),
            Some((&s[0..3], &s[3..]))
        );
    }

    #[test]
    fn test_parsed_string() {
        let s: String = "abc def".to_string();
        assert_eq!(
            run_parser_impl("", parsed_string(many1(satisfies(|c| c.is_alphanumeric())))),
            None
        );
        assert_eq!(
            run_parser_impl(
                "abc",
                parsed_string(many1(satisfies(|c| c.is_alphanumeric())))
            ),
            Some(((vec!['a', 'b', 'c'], "abc"), ""))
        );
        assert_eq!(
            run_parser_impl(
                "abc def",
                parsed_string(many1(satisfies(|c| c.is_alphanumeric())))
            ),
            Some(((vec!['a', 'b', 'c'], "abc"), " def"))
        );
        assert_eq!(
            run_parser_impl(&s, parsed_string(many1(satisfies(|c| c.is_alphanumeric())))),
            Some(((vec!['a', 'b', 'c'], &s[0..3]), &s[3..]))
        );
    }

    #[test]
    fn test_complex_parser() {
        let mut p = fmap(|_| 3, empty_parser());
        assert_eq!(run_parser_impl("abc def", &p), Some((3, "abc def")));
        p = fmap(|x: (i32, ())| x.0, seq(p, fail_parser()));
        assert_eq!(run_parser_impl("abc def", &p), None);
        let q = fmap(|_| 5, string_parser("abc"));
        assert_eq!(run_parser_impl("abc def", &q), Some((5, " def")));
        p = choice(p, q);
        assert_eq!(run_parser_impl("abc def", &p), Some((5, " def")));
    }
}
