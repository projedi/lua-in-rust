use super::*;

fn run_parser<'a, T>(
    input: &'a str,
    p: impl Parser<std::str::Chars<'a>, T>,
) -> Option<(T, &'a str)> {
    let (x, i) = run_parser_impl(input.chars(), p)?;
    Some((x, i.as_str()))
}

#[test]
fn test_empty() {
    assert_eq!(run_parser("", empty_parser()), Some(((), "")));
    assert_eq!(run_parser("abc", empty_parser()), Some(((), "abc")));
}

#[test]
fn test_fail() {
    assert_eq!(run_parser("", fail_parser::<std::str::Chars, ()>()), None);
    assert_eq!(
        run_parser("abc", fail_parser::<std::str::Chars, ()>()),
        None
    );
}

#[test]
fn test_fmap() {
    let i = 5;
    assert_eq!(run_parser("", fmap(|_| i, empty_parser())), Some((i, "")));
    assert_eq!(
        run_parser("abc", fmap(|_| i, empty_parser())),
        Some((i, "abc"))
    );

    assert_eq!(
        run_parser("", fmap(|_| i, fail_parser::<std::str::Chars, ()>())),
        None
    );
    assert_eq!(
        run_parser("abc", fmap(|_| i, fail_parser::<std::str::Chars, ()>())),
        None
    );
}

#[test]
fn test_map_filter() {
    let i = 5;
    let some_f = |_: ()| -> Option<i32> { Some(i) };
    let none_f = |_: ()| -> Option<i32> { None };
    assert_eq!(
        run_parser("", map_filter(some_f, empty_parser())),
        Some((i, ""))
    );
    assert_eq!(run_parser("", map_filter(none_f, empty_parser())), None);
    assert_eq!(
        run_parser("abc", map_filter(some_f, empty_parser())),
        Some((i, "abc"))
    );
    assert_eq!(run_parser("abc", map_filter(none_f, empty_parser())), None);

    assert_eq!(
        run_parser("", map_filter(some_f, fail_parser::<std::str::Chars, ()>())),
        None
    );
    assert_eq!(
        run_parser("", map_filter(none_f, fail_parser::<std::str::Chars, ()>())),
        None
    );
    assert_eq!(
        run_parser(
            "abc",
            map_filter(some_f, fail_parser::<std::str::Chars, ()>())
        ),
        None
    );
    assert_eq!(
        run_parser(
            "abc",
            map_filter(none_f, fail_parser::<std::str::Chars, ()>())
        ),
        None
    );
}

#[test]
fn test_satisfies() {
    assert_eq!(
        run_parser("", satisfies(|c: &char| c.is_whitespace())),
        None
    );
    assert_eq!(
        run_parser("abc", satisfies(|c: &char| c.is_whitespace())),
        None
    );
    assert_eq!(
        run_parser("  abc", satisfies(|c: &char| c.is_whitespace())),
        Some((' ', " abc"))
    );
}

#[test]
fn test_char_parser() {
    assert_eq!(run_parser("", char_parser('a')), None);
    assert_eq!(run_parser("bca", char_parser('a')), None);
    assert_eq!(run_parser("abc", char_parser('a')), Some(('a', "bc")));
}

#[test]
fn test_seq_bind() {
    assert_eq!(
        run_parser(
            "",
            seq_bind(char_parser('a'), move |c1| fmap(
                move |c2| (c2, c1),
                char_parser('b')
            ))
        ),
        None
    );
    assert_eq!(
        run_parser(
            "bac",
            seq_bind(char_parser('a'), move |c1| fmap(
                move |c2| (c2, c1),
                char_parser('b')
            ))
        ),
        None
    );
    assert_eq!(
        run_parser(
            "acb",
            seq_bind(char_parser('a'), move |c1| fmap(
                move |c2| (c2, c1),
                char_parser('b')
            ))
        ),
        None
    );
    assert_eq!(
        run_parser(
            "abc",
            seq_bind(char_parser('a'), move |c1| fmap(
                move |c2| (c2, c1),
                char_parser('b')
            ))
        ),
        Some((('b', 'a'), "c"))
    );
}

#[test]
fn test_seq() {
    assert_eq!(
        run_parser("", seq(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("bac", seq(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("acb", seq(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("abc", seq(char_parser('a'), char_parser('b'))),
        Some((('a', 'b'), "c"))
    );
}

#[test]
fn test_seq1() {
    assert_eq!(
        run_parser("", seq1(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("bac", seq1(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("acb", seq1(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("abc", seq1(char_parser('a'), char_parser('b'))),
        Some(('a', "c"))
    );
}

#[test]
fn test_seq2() {
    assert_eq!(
        run_parser("", seq2(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("bac", seq2(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("acb", seq2(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("abc", seq2(char_parser('a'), char_parser('b'))),
        Some(('b', "c"))
    );
}

#[test]
fn test_seq_() {
    assert_eq!(
        run_parser("", seq_(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("bac", seq_(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("acb", seq_(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("abc", seq_(char_parser('a'), char_parser('b'))),
        Some(((), "c"))
    );
}

#[test]
fn test_seqs_() {
    assert_eq!(
        run_parser("", seqs_![char_parser('a'), char_parser('b')]),
        None
    );
    assert_eq!(
        run_parser("bac", seqs_![char_parser('a'), char_parser('b')]),
        None
    );
    assert_eq!(
        run_parser("acb", seqs_![char_parser('a'), char_parser('b')]),
        None
    );
    assert_eq!(
        run_parser("abc", seqs_![char_parser('a'), char_parser('b')]),
        Some(((), "c"))
    );
}

#[test]
fn test_choice() {
    assert_eq!(
        run_parser("", choice(char_parser('a'), char_parser('b'))),
        None
    );
    assert_eq!(
        run_parser("abc", choice(char_parser('a'), char_parser('b'))),
        Some(('a', "bc"))
    );
    assert_eq!(
        run_parser("bac", choice(char_parser('a'), char_parser('b'))),
        Some(('b', "ac"))
    );
    assert_eq!(
        run_parser("cba", choice(char_parser('a'), char_parser('b'))),
        None
    );
}

#[test]
fn test_choices0() {
    assert_eq!(run_parser("", choices::<std::str::Chars, ()>(vec![])), None);
    assert_eq!(
        run_parser("abc", choices::<std::str::Chars, ()>(vec![])),
        None
    );
    assert_eq!(
        run_parser("bac", choices::<std::str::Chars, ()>(vec![])),
        None
    );
}

#[test]
fn test_choices1() {
    assert_eq!(run_parser("", choices(vec![char_parser('a')])), None);
    assert_eq!(
        run_parser("abc", choices(vec![char_parser('a')])),
        Some(('a', "bc"))
    );
    assert_eq!(run_parser("bac", choices(vec![char_parser('a')])), None);
}

#[test]
fn test_choices3() {
    assert_eq!(
        run_parser(
            "",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        None
    );
    assert_eq!(
        run_parser(
            "abc",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        Some(('a', "bc"))
    );
    assert_eq!(
        run_parser(
            "bac",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        Some(('b', "ac"))
    );
    assert_eq!(
        run_parser(
            "cba",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        Some(('c', "ba"))
    );
    assert_eq!(
        run_parser(
            "dba",
            choices(vec![char_parser('a'), char_parser('b'), char_parser('c')])
        ),
        None
    );
}

#[test]
fn test_many() {
    assert_eq!(
        run_parser("", many(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec![], ""))
    );
    assert_eq!(
        run_parser("abc", many(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], ""))
    );
    assert_eq!(
        run_parser(" abc", many(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec![], " abc"))
    );
    assert_eq!(
        run_parser("abc def", many(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], " def"))
    );
}

#[test]
fn test_many1() {
    assert_eq!(
        run_parser("", many1(satisfies(|c: &char| c.is_alphanumeric()))),
        None
    );
    assert_eq!(
        run_parser("abc", many1(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], ""))
    );
    assert_eq!(
        run_parser(" abc", many1(satisfies(|c: &char| c.is_alphanumeric()))),
        None
    );
    assert_eq!(
        run_parser("abc def", many1(satisfies(|c: &char| c.is_alphanumeric()))),
        Some((vec!['a', 'b', 'c'], " def"))
    );
}

#[test]
fn test_try_parser() {
    assert_eq!(
        run_parser("", try_parser(char_parser('a'))),
        Some((None, ""))
    );
    assert_eq!(
        run_parser("bca", try_parser(char_parser('a'))),
        Some((None, "bca"))
    );
    assert_eq!(
        run_parser("abc", try_parser(char_parser('a'))),
        Some((Some('a'), "bc"))
    );
}

#[test]
fn test_not_parser() {
    assert_eq!(run_parser("", not_parser(char_parser('a'))), Some(((), "")));
    assert_eq!(
        run_parser("bca", not_parser(char_parser('a'))),
        Some(((), "bca"))
    );
    assert_eq!(run_parser("abc", not_parser(char_parser('a'))), None);
}

#[test]
fn test_string_parser() {
    assert_eq!(run_parser("", string_parser("abc")), None);
    assert_eq!(run_parser("", string_parser("")), Some(("", "")));
    assert_eq!(run_parser("abd", string_parser("abc")), None);
    assert_eq!(run_parser("abd", string_parser("")), Some(("", "abd")));
    assert_eq!(run_parser("abc", string_parser("abc")), Some(("abc", "")));
    assert_eq!(
        run_parser("abc def", string_parser("abc")),
        Some(("abc", " def"))
    );
}

#[test]
fn test_parsed_string() {
    assert_eq!(
        run_parser(
            "",
            parsed_string(many1(satisfies(|c: &char| c.is_alphanumeric())))
        ),
        None
    );
    assert_eq!(
        run_parser(
            "abc",
            parsed_string(many1(satisfies(|c: &char| c.is_alphanumeric())))
        ),
        Some(((vec!['a', 'b', 'c'], "abc"), ""))
    );
    assert_eq!(
        run_parser(
            "abc def",
            parsed_string(many1(satisfies(|c: &char| c.is_alphanumeric())))
        ),
        Some(((vec!['a', 'b', 'c'], "abc"), " def"))
    );
}

#[test]
fn test_complex_parser() {
    let mut p = fmap(|_| 3, empty_parser());
    assert_eq!(run_parser("abc def", &p), Some((3, "abc def")));
    p = fmap(|x: (i32, ())| x.0, seq(p, fail_parser()));
    assert_eq!(run_parser("abc def", &p), None);
    let q = fmap(|_| 5, string_parser("abc"));
    assert_eq!(run_parser("abc def", &q), Some((5, " def")));
    p = choice(p, q);
    assert_eq!(run_parser("abc def", &p), Some((5, " def")));
}

#[test]
fn test_eof() {
    assert_eq!(run_parser("", eof()), Some(((), "")));
    assert_eq!(run_parser("a", eof()), None);
    assert_eq!(
        run_parser("a", seq_(char_parser('a'), eof())),
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
    assert_eq!(run_parser("", recursive_parser()), None);
    assert_eq!(run_parser("0", recursive_parser()), Some((0, "")));
    assert_eq!(run_parser("1110", recursive_parser()), Some((3, "")));
    assert_eq!(run_parser("11102", recursive_parser()), Some((3, "2")));
    assert_eq!(run_parser("1120", recursive_parser()), None);
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
    assert_eq!(run_parser("", mut_rec_parser1()), None);
    assert_eq!(run_parser("0", mut_rec_parser1()), Some((0, "")));
    assert_eq!(run_parser("120", mut_rec_parser1()), Some((2, "")));
    assert_eq!(run_parser("1210", mut_rec_parser1()), Some((3, "")));
    assert_eq!(run_parser("12120", mut_rec_parser1()), Some((4, "")));
    assert_eq!(run_parser("12103", mut_rec_parser1()), Some((3, "3")));
    assert_eq!(run_parser("130", mut_rec_parser1()), None);
    assert_eq!(run_parser("1230", mut_rec_parser1()), None);

    assert_eq!(run_parser("", mut_rec_parser2()), None);
    assert_eq!(run_parser("0", mut_rec_parser2()), Some((0, "")));
    assert_eq!(run_parser("210", mut_rec_parser2()), Some((2, "")));
    assert_eq!(run_parser("2120", mut_rec_parser2()), Some((3, "")));
    assert_eq!(run_parser("21210", mut_rec_parser2()), Some((4, "")));
    assert_eq!(run_parser("21203", mut_rec_parser2()), Some((3, "3")));
    assert_eq!(run_parser("230", mut_rec_parser2()), None);
    assert_eq!(run_parser("2130", mut_rec_parser2()), None);
}
