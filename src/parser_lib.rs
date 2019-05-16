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

pub fn empty_parser<'a>() -> impl Parser<'a, ()> {
    |s| Some(((), s))
}

pub fn fail_parser<'a, T>() -> impl Parser<'a, T> {
    |_| None
}

pub fn fmap<'a, T1, T2>(f: impl Fn(T1) -> T2, p: impl Parser<'a, T1>) -> impl Parser<'a, T2> {
    move |s| {
        let (x, s) = p(s)?;
        Some((f(x), s))
    }
}

pub fn satisfies<'a>(f: impl Fn(char) -> bool) -> impl Parser<'a, char> {
    move |s: ParserState<'a>| {
        let c = s.input[s.index..].chars().next()?;
        if f(c) {
            let mut out_s = s;
            out_s.index += 1;
            return Some((c, out_s));
        }
        return None;
    }
}

pub fn char_parser<'a>(c: char) -> impl Parser<'a, char> {
    satisfies(move |in_c| in_c == c)
}

pub fn seq<'a, T1, T2>(
    p1: impl Parser<'a, T1>,
    p2: impl Parser<'a, T2>,
) -> impl Parser<'a, (T1, T2)> {
    move |s| {
        let (p1_result, s) = p1(s)?;
        let (p2_result, s) = p2(s)?;
        Some(((p1_result, p2_result), s))
    }
}

pub fn choice<'a, T>(p1: impl Parser<'a, T>, p2: impl Parser<'a, T>) -> impl Parser<'a, T> {
    move |s| {
        let p1_result = p1(s);
        match p1_result {
            Some(_) => p1_result,
            None => p2(s),
        }
    }
}

pub fn many<'a, T>(p: impl Parser<'a, T>) -> impl Parser<'a, Vec<T>> {
    move |s| {
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
    }
}

pub fn many1<'a, T>(p: impl Parser<'a, T>) -> impl Parser<'a, Vec<T>> {
    move |s| {
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
    }
}

pub fn string_parser<'a, 'b>(expected_str: &'b str) -> impl Parser<'a, &'a str> + 'b {
    move |s: ParserState<'a>| {
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
    }
}

pub fn parsed_string<'a, T>(p: impl Parser<'a, T>) -> impl Parser<'a, (T, &'a str)> {
    move |s| {
        let (p_result, p_s) = p(s)?;
        Some(((p_result, &s.input[s.index..p_s.index]), p_s))
    }
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
}
