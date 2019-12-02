use super::*;

fn run_string_parser_impl<'a, T>(
    input: &'a str,
    p: impl Parser<std::str::Chars<'a>, T>,
) -> Option<(T, &'a str)> {
    let (x, i) = run_parser_impl(input.chars(), p)?;
    Some((x, i.as_str()))
}

#[test]
fn test_empty() {
    let s: String = "abc".to_string();
    assert_eq!(run_string_parser_impl("", empty_parser()), Some(((), "")));
    assert_eq!(
        run_string_parser_impl("abc", empty_parser()),
        Some(((), "abc"))
    );
    assert_eq!(
        run_string_parser_impl(&s, empty_parser()),
        Some(((), &s[..]))
    );
}

#[test]
fn test_fail() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", fail_parser::<std::str::Chars, ()>()),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", fail_parser::<std::str::Chars, ()>()),
        None
    );
    assert_eq!(
        run_string_parser_impl(&s, fail_parser::<std::str::Chars, ()>()),
        None
    );
}

#[test]
fn test_fmap() {
    let i = 5;
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", fmap(|_| i, empty_parser())),
        Some((i, ""))
    );
    assert_eq!(
        run_string_parser_impl("abc", fmap(|_| i, empty_parser())),
        Some((i, "abc"))
    );
    assert_eq!(
        run_string_parser_impl(&s, fmap(|_| i, empty_parser())),
        Some((i, &s[..]))
    );

    assert_eq!(
        run_string_parser_impl("", fmap(|_| i, fail_parser::<std::str::Chars, ()>())),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", fmap(|_| i, fail_parser::<std::str::Chars, ()>())),
        None
    );
    assert_eq!(
        run_string_parser_impl(&s, fmap(|_| i, fail_parser::<std::str::Chars, ()>())),
        None
    );
}

#[test]
fn test_map_filter() {
    let i = 5;
    let s: String = "abc".to_string();
    let some_f = |_: ()| -> Option<i32> { Some(i) };
    let none_f = |_: ()| -> Option<i32> { None };
    assert_eq!(
        run_string_parser_impl("", map_filter(some_f, empty_parser())),
        Some((i, ""))
    );
    assert_eq!(
        run_string_parser_impl("", map_filter(none_f, empty_parser())),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", map_filter(some_f, empty_parser())),
        Some((i, "abc"))
    );
    assert_eq!(
        run_string_parser_impl("abc", map_filter(none_f, empty_parser())),
        None
    );
    assert_eq!(
        run_string_parser_impl(&s, map_filter(some_f, empty_parser())),
        Some((i, &s[..]))
    );
    assert_eq!(
        run_string_parser_impl(&s, map_filter(none_f, empty_parser())),
        None
    );

    assert_eq!(
        run_string_parser_impl("", map_filter(some_f, fail_parser::<std::str::Chars, ()>())),
        None
    );
    assert_eq!(
        run_string_parser_impl("", map_filter(none_f, fail_parser::<std::str::Chars, ()>())),
        None
    );
    assert_eq!(
        run_string_parser_impl(
            "abc",
            map_filter(some_f, fail_parser::<std::str::Chars, ()>())
        ),
        None
    );
    assert_eq!(
        run_string_parser_impl(
            "abc",
            map_filter(none_f, fail_parser::<std::str::Chars, ()>())
        ),
        None
    );
    assert_eq!(
        run_string_parser_impl(&s, map_filter(some_f, fail_parser::<std::str::Chars, ()>())),
        None
    );
    assert_eq!(
        run_string_parser_impl(&s, map_filter(none_f, fail_parser::<std::str::Chars, ()>())),
        None
    );
}

#[test]
fn test_satisfies() {
    let s: String = "  abc".to_string();
    assert_eq!(
        run_string_parser_impl("", satisfies(|c: &char| c.is_whitespace())),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", satisfies(|c: &char| c.is_whitespace())),
        None
    );
    assert_eq!(
        run_string_parser_impl("  abc", satisfies(|c: &char| c.is_whitespace())),
        Some((' ', " abc"))
    );
    assert_eq!(
        run_string_parser_impl(&s, satisfies(|c: &char| c.is_whitespace())),
        Some((' ', &s[1..]))
    );
}

#[test]
fn test_char_parser() {
    let s: String = "abc".to_string();
    assert_eq!(run_string_parser_impl("", char_parser('a')), None);
    assert_eq!(run_string_parser_impl("bca", char_parser('a')), None);
    assert_eq!(
        run_string_parser_impl("abc", char_parser('a')),
        Some(('a', "bc"))
    );
    assert_eq!(
        run_string_parser_impl(&s, char_parser('a')),
        Some(('a', &s[1..]))
    );
}

#[test]
fn test_seq_bind() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl(
            "",
            seq_bind(char_parser('a'), move |c1| fmap(
                move |c2| (c2, c1),
                char_parser('b')
            ))
        ),
        None
    );
    assert_eq!(
        run_string_parser_impl(
            "bac",
            seq_bind(char_parser('a'), move |c1| fmap(
                move |c2| (c2, c1),
                char_parser('b')
            ))
        ),
        None
    );
    assert_eq!(
        run_string_parser_impl(
            "acb",
            seq_bind(char_parser('a'), move |c1| fmap(
                move |c2| (c2, c1),
                char_parser('b')
            ))
        ),
        None
    );
    assert_eq!(
        run_string_parser_impl(
            "abc",
            seq_bind(char_parser('a'), move |c1| fmap(
                move |c2| (c2, c1),
                char_parser('b')
            ))
        ),
        Some((('b', 'a'), "c"))
    );
    assert_eq!(
        run_string_parser_impl(
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
        run_string_parser_impl("", seq(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("bac", seq(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("acb", seq(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", seq(char_parser('a'), char_parser('b'))),
        Some((('a', 'b'), "c"))
    );
    assert_eq!(
        run_string_parser_impl(&s, seq(char_parser('a'), char_parser('b'))),
        Some((('a', 'b'), &s[2..]))
    );
}

#[test]
fn test_seq1() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", seq1(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("bac", seq1(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("acb", seq1(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", seq1(char_parser('a'), char_parser('b'))),
        Some(('a', "c"))
    );
    assert_eq!(
        run_string_parser_impl(&s, seq1(char_parser('a'), char_parser('b'))),
        Some(('a', &s[2..]))
    );
}

#[test]
fn test_seq2() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", seq2(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("bac", seq2(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("acb", seq2(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", seq2(char_parser('a'), char_parser('b'))),
        Some(('b', "c"))
    );
    assert_eq!(
        run_string_parser_impl(&s, seq2(char_parser('a'), char_parser('b'))),
        Some(('b', &s[2..]))
    );
}

#[test]
fn test_seq_() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", seq_(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("bac", seq_(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("acb", seq_(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", seq_(char_parser('a'), char_parser('b'))),
        Some(((), "c"))
    );
    assert_eq!(
        run_string_parser_impl(&s, seq_(char_parser('a'), char_parser('b'))),
        Some(((), &s[2..]))
    );
}

#[test]
fn test_seqs_() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", seqs_![char_parser('a'), char_parser('b')]),
        None
    );
    assert_eq!(
        run_string_parser_impl("bac", seqs_![char_parser('a'), char_parser('b')]),
        None
    );
    assert_eq!(
        run_string_parser_impl("acb", seqs_![char_parser('a'), char_parser('b')]),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", seqs_![char_parser('a'), char_parser('b')]),
        Some(((), "c"))
    );
    assert_eq!(
        run_string_parser_impl(&s, seqs_![char_parser('a'), char_parser('b')]),
        Some(((), &s[2..]))
    );
}

#[test]
fn test_choice() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", choice(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", choice(char_parser('a'), char_parser('b'))),
        Some(('a', "bc"))
    );
    assert_eq!(
        run_string_parser_impl("bac", choice(char_parser('a'), char_parser('b'))),
        Some(('b', "ac"))
    );
    assert_eq!(
        run_string_parser_impl("cba", choice(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_string_parser_impl(&s, choice(char_parser('a'), char_parser('b'))),
        Some(('a', &s[1..]))
    );
}

#[test]
fn test_choices0() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", choices::<std::str::Chars, ()>(vec![])),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", choices::<std::str::Chars, ()>(vec![])),
        None
    );
    assert_eq!(
        run_string_parser_impl("bac", choices::<std::str::Chars, ()>(vec![])),
        None
    );
    assert_eq!(
        run_string_parser_impl(&s, choices::<std::str::Chars, ()>(vec![])),
        None
    );
}

#[test]
fn test_choices1() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", choices(vec![char_parser('a')])),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", choices(vec![char_parser('a')])),
        Some(('a', "bc"))
    );
    assert_eq!(
        run_string_parser_impl("bac", choices(vec![char_parser('a')])),
        None
    );
    assert_eq!(
        run_string_parser_impl(&s, choices(vec![char_parser('a')])),
        Some(('a', &s[1..]))
    );
}

#[test]
fn test_choices3() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl(
            "",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        None
    );
    assert_eq!(
        run_string_parser_impl(
            "abc",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        Some(('a', "bc"))
    );
    assert_eq!(
        run_string_parser_impl(
            "bac",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        Some(('b', "ac"))
    );
    assert_eq!(
        run_string_parser_impl(
            "cba",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        Some(('c', "ba"))
    );
    assert_eq!(
        run_string_parser_impl(
            "dba",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        None
    );
    assert_eq!(
        run_string_parser_impl(
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
        run_string_parser_impl("", many(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec![], ""))
    );
    assert_eq!(
        run_string_parser_impl("abc", many(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], ""))
    );
    assert_eq!(
        run_string_parser_impl(" abc", many(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec![], " abc"))
    );
    assert_eq!(
        run_string_parser_impl("abc def", many(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], " def"))
    );
    assert_eq!(
        run_string_parser_impl(&s, many(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], &s[3..]))
    );
}

#[test]
fn test_many1() {
    let s: String = "abc def".to_string();
    assert_eq!(
        run_string_parser_impl("", many1(satisfies(|c: &char| c.is_alphanumeric()))),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc", many1(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], ""))
    );
    assert_eq!(
        run_string_parser_impl(" abc", many1(satisfies(|c: &char| c.is_alphanumeric()))),
        None
    );
    assert_eq!(
        run_string_parser_impl("abc def", many1(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], " def"))
    );
    assert_eq!(
        run_string_parser_impl(&s, many1(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], &s[3..]))
    );
}

#[test]
fn test_try_parser() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", try_parser(char_parser('a'))),
        Some((None, ""))
    );
    assert_eq!(
        run_string_parser_impl("bca", try_parser(char_parser('a'))),
        Some((None, "bca"))
    );
    assert_eq!(
        run_string_parser_impl("abc", try_parser(char_parser('a'))),
        Some((Some('a'), "bc"))
    );
    assert_eq!(
        run_string_parser_impl(&s, try_parser(char_parser('a'))),
        Some((Some('a'), &s[1..]))
    );
}

#[test]
fn test_not_parser() {
    let s: String = "abc".to_string();
    assert_eq!(
        run_string_parser_impl("", not_parser(char_parser('a'))),
        Some(((), ""))
    );
    assert_eq!(
        run_string_parser_impl("bca", not_parser(char_parser('a'))),
        Some(((), "bca"))
    );
    assert_eq!(
        run_string_parser_impl("abc", not_parser(char_parser('a'))),
        None
    );
    assert_eq!(
        run_string_parser_impl(&s, not_parser(char_parser('a'))),
        None
    );
}

#[test]
fn test_string_parser() {
    let s_before: String = "abc".to_string();
    let s: String = "abc def".to_string();
    let s_after: String = "abc".to_string();
    assert_eq!(run_string_parser_impl("", string_parser("abc")), None);
    assert_eq!(
        run_string_parser_impl("", string_parser("")),
        Some(("", ""))
    );
    assert_eq!(run_string_parser_impl("abd", string_parser("abc")), None);
    assert_eq!(
        run_string_parser_impl("abd", string_parser("")),
        Some(("", "abd"))
    );
    assert_eq!(
        run_string_parser_impl("abc", string_parser("abc")),
        Some(("abc", ""))
    );
    assert_eq!(
        run_string_parser_impl("abc def", string_parser("abc")),
        Some(("abc", " def"))
    );
    assert_eq!(
        run_string_parser_impl(&s, string_parser("abc")),
        Some((&s[0..3], &s[3..]))
    );
    assert_eq!(
        run_string_parser_impl(&s, string_parser(&s_before)),
        Some((&s[0..3], &s[3..]))
    );
    assert_eq!(
        run_string_parser_impl(&s, string_parser(&s_after)),
        Some((&s[0..3], &s[3..]))
    );
}

#[test]
fn test_parsed_string() {
    let s: String = "abc def".to_string();
    assert_eq!(
        run_string_parser_impl(
            "",
            parsed_string(many1(satisfies(|c: &char| c.is_alphanumeric())))
        ),
        None
    );
    assert_eq!(
        run_string_parser_impl(
            "abc",
            parsed_string(many1(satisfies(|c: &char| c.is_alphanumeric())))
        ),
        Some(((vec!['a', 'b', 'c'], "abc"), ""))
    );
    assert_eq!(
        run_string_parser_impl(
            "abc def",
            parsed_string(many1(satisfies(|c: &char| c.is_alphanumeric())))
        ),
        Some(((vec!['a', 'b', 'c'], "abc"), " def"))
    );
    assert_eq!(
        run_string_parser_impl(
            &s,
            parsed_string(many1(satisfies(|c: &char| c.is_alphanumeric())))
        ),
        Some(((vec!['a', 'b', 'c'], &s[0..3]), &s[3..]))
    );
}

#[test]
fn test_complex_parser() {
    let mut p = fmap(|_| 3, empty_parser());
    assert_eq!(run_string_parser_impl("abc def", &p), Some((3, "abc def")));
    p = fmap(|x: (i32, ())| x.0, seq(p, fail_parser()));
    assert_eq!(run_string_parser_impl("abc def", &p), None);
    let q = fmap(|_| 5, string_parser("abc"));
    assert_eq!(run_string_parser_impl("abc def", &q), Some((5, " def")));
    p = choice(p, q);
    assert_eq!(run_string_parser_impl("abc def", &p), Some((5, " def")));
}

#[test]
fn test_eof() {
    assert_eq!(run_string_parser_impl("", eof()), Some(((), "")));
    assert_eq!(run_string_parser_impl("a", eof()), None);
    assert_eq!(
        run_string_parser_impl("a", seq_(char_parser('a'), eof())),
        Some(((), ""))
    );
}

fn recursive_parser<'a, I: Iterator<Item = char> + Clone + 'a>() -> Box<dyn Parser<I, u64> + 'a> {
    choice(
        fmap(
            |i| i + 1,
            seq2(char_parser('1'), allow_recursion(recursive_parser)),
        ),
        fmap(|_| 0, char_parser('0')),
    )
}

#[test]
fn test_recursive_parser() {
    assert_eq!(run_string_parser_impl("", recursive_parser()), None);
    assert_eq!(
        run_string_parser_impl("0", recursive_parser()),
        Some((0, ""))
    );
    assert_eq!(
        run_string_parser_impl("1110", recursive_parser()),
        Some((3, ""))
    );
    assert_eq!(
        run_string_parser_impl("11102", recursive_parser()),
        Some((3, "2"))
    );
    assert_eq!(run_string_parser_impl("1120", recursive_parser()), None);
}

fn mut_rec_parser1<'a, I: Iterator<Item = char> + Clone + 'a>() -> Box<dyn Parser<I, u64> + 'a> {
    choice(
        fmap(
            |i| i + 1,
            seq2(char_parser('1'), allow_recursion(mut_rec_parser2)),
        ),
        fmap(|_| 0, char_parser('0')),
    )
}

fn mut_rec_parser2<'a, I: Iterator<Item = char> + Clone + 'a>() -> Box<dyn Parser<I, u64> + 'a> {
    choice(
        fmap(
            |i| i + 1,
            seq2(char_parser('2'), allow_recursion(mut_rec_parser1)),
        ),
        fmap(|_| 0, char_parser('0')),
    )
}

#[test]
fn test_mut_recursive_parser() {
    assert_eq!(run_string_parser_impl("", mut_rec_parser1()), None);
    assert_eq!(
        run_string_parser_impl("0", mut_rec_parser1()),
        Some((0, ""))
    );
    assert_eq!(
        run_string_parser_impl("120", mut_rec_parser1()),
        Some((2, ""))
    );
    assert_eq!(
        run_string_parser_impl("1210", mut_rec_parser1()),
        Some((3, ""))
    );
    assert_eq!(
        run_string_parser_impl("12120", mut_rec_parser1()),
        Some((4, ""))
    );
    assert_eq!(
        run_string_parser_impl("12103", mut_rec_parser1()),
        Some((3, "3"))
    );
    assert_eq!(run_string_parser_impl("130", mut_rec_parser1()), None);
    assert_eq!(run_string_parser_impl("1230", mut_rec_parser1()), None);

    assert_eq!(run_string_parser_impl("", mut_rec_parser2()), None);
    assert_eq!(
        run_string_parser_impl("0", mut_rec_parser2()),
        Some((0, ""))
    );
    assert_eq!(
        run_string_parser_impl("210", mut_rec_parser2()),
        Some((2, ""))
    );
    assert_eq!(
        run_string_parser_impl("2120", mut_rec_parser2()),
        Some((3, ""))
    );
    assert_eq!(
        run_string_parser_impl("21210", mut_rec_parser2()),
        Some((4, ""))
    );
    assert_eq!(
        run_string_parser_impl("21203", mut_rec_parser2()),
        Some((3, "3"))
    );
    assert_eq!(run_string_parser_impl("230", mut_rec_parser2()), None);
    assert_eq!(run_string_parser_impl("2130", mut_rec_parser2()), None);
}
