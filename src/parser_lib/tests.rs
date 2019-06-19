use super::*;

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

mod expression_parser_test {
    use super::*;
    use std::rc::Rc;

    #[derive(PartialEq, Eq, Debug, Clone, Copy)]
    enum BinOp {
        Add,
        Sub,
        Mul,
        Pow,
        Comma,
        Semi,
    }

    impl std::fmt::Display for BinOp {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
            match &self {
                BinOp::Add => write!(f, "+"),
                BinOp::Sub => write!(f, "-"),
                BinOp::Mul => write!(f, "*"),
                BinOp::Pow => write!(f, "^"),
                BinOp::Comma => write!(f, ","),
                BinOp::Semi => write!(f, ";"),
            }
        }
    }

    #[derive(PartialEq, Eq, Debug, Clone, Copy)]
    enum UnOp {
        Neg,
        Exclaim,
        Hash,
        Not,
    }

    impl std::fmt::Display for UnOp {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
            match &self {
                UnOp::Neg => write!(f, "-"),
                UnOp::Exclaim => write!(f, "!"),
                UnOp::Hash => write!(f, "#"),
                UnOp::Not => write!(f, "~"),
            }
        }
    }

    #[derive(PartialEq, Debug, Clone)]
    enum Exp {
        Num(f64),
        BinOp(Box<Exp>, BinOp, Box<Exp>),
        UnOp(UnOp, Box<Exp>),
    }

    impl std::fmt::Display for Exp {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
            match &self {
                Exp::Num(n) => write!(f, "{}", n),
                Exp::BinOp(lhs, op, rhs) => write!(f, "({}{}{})", lhs, op, rhs),
                Exp::UnOp(op, e) => write!(f, "({}{})", op, e),
            }
        }
    }

    type Parser<'a, 'b, T> = Box<dyn crate::parser_lib::Parser<std::str::Chars<'b>, T> + 'a>;

    fn num_parser<'a, 'b: 'a>() -> Parser<'a, 'b, f64> {
        seq_bind(
            parsed_string(many1(satisfies(|c: &char| c.is_ascii_digit()))),
            |(_, num_str)| match num_str.parse::<f64>() {
                Ok(num) => fmap(move |_| num, empty_parser()),
                Err(_) => fail_parser(),
            },
        )
    }

    fn exp_parser<'a, 'b: 'a>() -> Parser<'a, 'b, Exp> {
        let create_ops = || -> Vec<OpLine<'a, std::str::Chars<'b>, Exp>> {
            let exp_for_unop = |op: UnOp| -> Rc<dyn Fn(Exp) -> Exp + 'a> {
                Rc::new(move |e| Exp::UnOp(op, Box::new(e)))
            };
            let create_unop = |op: UnOp, c: char| {
                Rc::new(move || fmap(move |_| exp_for_unop(op), char_parser(c)))
            };
            let exp_for_binop = |op: BinOp| -> Rc<dyn Fn(Exp, Exp) -> Exp + 'a> {
                Rc::new(move |lhs, rhs| Exp::BinOp(Box::new(lhs), op, Box::new(rhs)))
            };
            let create_binop = |op: BinOp, c: char| {
                Rc::new(move || fmap(move |_| exp_for_binop(op), char_parser(c)))
            };
            vec![
                OpLine::BinOp(Assoc::Right, vec![create_binop(BinOp::Pow, '^')]),
                OpLine::UnOp(vec![
                    create_unop(UnOp::Neg, '-'),
                    create_unop(UnOp::Not, '~'),
                ]),
                OpLine::BinOp(Assoc::Left, vec![create_binop(BinOp::Mul, '*')]),
                OpLine::BinOp(
                    Assoc::Left,
                    vec![create_binop(BinOp::Add, '+'), create_binop(BinOp::Sub, '-')],
                ),
                OpLine::BinOp(
                    Assoc::Right,
                    vec![
                        create_binop(BinOp::Comma, ','),
                        create_binop(BinOp::Semi, ';'),
                    ],
                ),
                OpLine::UnOp(vec![
                    create_unop(UnOp::Exclaim, '!'),
                    create_unop(UnOp::Hash, '#'),
                ]),
            ]
        };
        build_exp_parser(create_ops, Rc::new(|| fmap(Exp::Num, num_parser())))
    }

    #[derive(Clone, Copy)]
    enum Op {
        BinOp(BinOp, Assoc),
        UnOp(UnOp),
    }

    fn create_ops_with_priorities() -> Vec<(Op, usize)> {
        let ops = vec![
            vec![Op::BinOp(BinOp::Pow, Assoc::Right)],
            vec![Op::UnOp(UnOp::Neg), Op::UnOp(UnOp::Not)],
            vec![Op::BinOp(BinOp::Mul, Assoc::Left)],
            vec![
                Op::BinOp(BinOp::Add, Assoc::Left),
                Op::BinOp(BinOp::Sub, Assoc::Left),
            ],
            vec![
                Op::BinOp(BinOp::Comma, Assoc::Right),
                Op::BinOp(BinOp::Semi, Assoc::Right),
            ],
            vec![Op::UnOp(UnOp::Exclaim), Op::UnOp(UnOp::Hash)],
        ];
        let mut result = vec![];
        for (p, ops) in ops.into_iter().rev().enumerate() {
            for op in ops {
                result.push((op, p))
            }
        }
        result
    }

    struct TestCase {
        input: String,
        output: Exp,
    }

    fn generate_case1(op: (Op, usize)) -> TestCase {
        let num = |n| Exp::Num(n);
        let unop = |unop, e| Exp::UnOp(unop, Box::new(e));
        let binop = |lhs, binop, rhs| Exp::BinOp(Box::new(lhs), binop, Box::new(rhs));
        match op {
            (Op::BinOp(op, _), _) => TestCase {
                input: format!("1{}2", op),
                output: binop(num(1.0), op, num(2.0)),
            },
            (Op::UnOp(op), _) => TestCase {
                input: format!("{}1", op),
                output: unop(op, num(1.0)),
            },
        }
    }

    fn select_association((p1, ass1): (usize, Assoc), (p2, ass2): (usize, Assoc)) -> Assoc {
        if p1 < p2 {
            Assoc::Right
        } else if p1 == p2 {
            assert_eq!(ass1, ass2);
            ass1
        } else {
            Assoc::Left
        }
    }

    fn generate_case2(op1: (Op, usize), op2: (Op, usize)) -> TestCase {
        let num = |n| Exp::Num(n);
        let unop = |unop, e| Exp::UnOp(unop, Box::new(e));
        let binop = |lhs, binop, rhs| Exp::BinOp(Box::new(lhs), binop, Box::new(rhs));
        match (op1, op2) {
            ((Op::BinOp(op1, ass1), p1), (Op::BinOp(op2, ass2), p2)) => TestCase {
                input: format!("1{}2{}3", op1, op2),
                output: match select_association((p1, ass1), (p2, ass2)) {
                    Assoc::Left => binop(binop(num(1.0), op1, num(2.0)), op2, num(3.0)),
                    Assoc::Right => binop(num(1.0), op1, binop(num(2.0), op2, num(3.0))),
                },
            },
            ((Op::BinOp(op1, _), _), (Op::UnOp(op2), _)) => TestCase {
                input: format!("1{}{}2", op1, op2),
                output: binop(num(1.0), op1, unop(op2, num(2.0))),
            },
            ((Op::UnOp(op1), p1), (Op::BinOp(op2, _), p2)) => TestCase {
                input: format!("{}1{}2", op1, op2),
                output: if p1 < p2 {
                    unop(op1, binop(num(1.0), op2, num(2.0)))
                } else {
                    binop(unop(op1, num(1.0)), op2, num(2.0))
                },
            },
            ((Op::UnOp(op1), _), (Op::UnOp(op2), _)) => TestCase {
                input: format!("{}{}1", op1, op2),
                output: unop(op1, unop(op2, num(1.0))),
            },
        }
    }

    fn generate_case3(op1: (Op, usize), op2: (Op, usize), op3: (Op, usize)) -> TestCase {
        let num = |n| Exp::Num(n);
        let unop = |unop, e| Exp::UnOp(unop, Box::new(e));
        let binop = |lhs, binop, rhs| Exp::BinOp(Box::new(lhs), binop, Box::new(rhs));
        match (op1, op2, op3) {
            (
                (Op::BinOp(op1, ass1), p1),
                (Op::BinOp(op2, ass2), p2),
                (Op::BinOp(op3, ass3), p3),
            ) => TestCase {
                input: format!("1{}2{}3{}4", op1, op2, op3),
                output: match (
                    select_association((p1, ass1), (p2, ass2)),
                    select_association((p2, ass2), (p3, ass3)),
                    select_association((p1, ass1), (p3, ass3)),
                ) {
                    (Assoc::Left, Assoc::Left, a) => {
                        assert_eq!(Assoc::Left, a);
                        binop(
                            binop(binop(num(1.0), op1, num(2.0)), op2, num(3.0)),
                            op3,
                            num(4.0),
                        )
                    }
                    (Assoc::Left, Assoc::Right, _) => binop(
                        binop(num(1.0), op1, num(2.0)),
                        op2,
                        binop(num(3.0), op3, num(4.0)),
                    ),
                    (Assoc::Right, _, Assoc::Left) => binop(
                        binop(num(1.0), op1, binop(num(2.0), op2, num(3.0))),
                        op3,
                        num(4.0),
                    ),
                    (_, Assoc::Left, Assoc::Right) => binop(
                        num(1.0),
                        op1,
                        binop(binop(num(2.0), op2, num(3.0)), op3, num(4.0)),
                    ),
                    (Assoc::Right, Assoc::Right, a) => {
                        assert_eq!(Assoc::Right, a);
                        binop(
                            num(1.0),
                            op1,
                            binop(num(2.0), op2, binop(num(3.0), op3, num(4.0))),
                        )
                    }
                },
            },
            ((Op::BinOp(op1, ass1), p1), (Op::BinOp(op2, ass2), p2), (Op::UnOp(op3), _)) => {
                TestCase {
                    input: format!("1{}2{}{}3", op1, op2, op3),
                    output: match select_association((p1, ass1), (p2, ass2)) {
                        Assoc::Left => {
                            binop(binop(num(1.0), op1, num(2.0)), op2, unop(op3, num(3.0)))
                        }
                        Assoc::Right => {
                            binop(num(1.0), op1, binop(num(2.0), op2, unop(op3, num(3.0))))
                        }
                    },
                }
            }
            ((Op::BinOp(op1, ass1), p1), (Op::UnOp(op2), p2), (Op::BinOp(op3, ass3), p3)) => {
                TestCase {
                    input: format!("1{}{}2{}3", op1, op2, op3),
                    output: match select_association((p1, ass1), (p3, ass3)) {
                        Assoc::Left => {
                            binop(binop(num(1.0), op1, unop(op2, num(2.0))), op3, num(3.0))
                        }
                        Assoc::Right => {
                            if p2 < p3 {
                                binop(num(1.0), op1, unop(op2, binop(num(2.0), op3, num(3.0))))
                            } else {
                                binop(num(1.0), op1, binop(unop(op2, num(2.0)), op3, num(3.0)))
                            }
                        }
                    },
                }
            }
            ((Op::BinOp(op1, _), _), (Op::UnOp(op2), _), (Op::UnOp(op3), _)) => TestCase {
                input: format!("1{}{}{}2", op1, op2, op3),
                output: binop(num(1.0), op1, unop(op2, unop(op3, num(2.0)))),
            },
            ((Op::UnOp(op1), p1), (Op::BinOp(op2, ass2), p2), (Op::BinOp(op3, ass3), p3)) => {
                TestCase {
                    input: format!("{}1{}2{}3", op1, op2, op3),
                    output: match select_association((p2, ass2), (p3, ass3)) {
                        Assoc::Left => {
                            if p1 < p3 {
                                unop(op1, binop(binop(num(1.0), op2, num(2.0)), op3, num(3.0)))
                            } else if p1 < p2 {
                                binop(unop(op1, binop(num(1.0), op2, num(2.0))), op3, num(3.0))
                            } else {
                                binop(binop(unop(op1, num(1.0)), op2, num(2.0)), op3, num(3.0))
                            }
                        }
                        Assoc::Right => {
                            if p1 < p2 {
                                unop(op1, binop(num(1.0), op2, binop(num(2.0), op3, num(3.0))))
                            } else {
                                binop(unop(op1, num(1.0)), op2, binop(num(2.0), op3, num(3.0)))
                            }
                        }
                    },
                }
            }
            ((Op::UnOp(op1), p1), (Op::BinOp(op2, _), p2), (Op::UnOp(op3), _)) => TestCase {
                input: format!("{}1{}{}2", op1, op2, op3),
                output: if p1 < p2 {
                    unop(op1, binop(num(1.0), op2, unop(op3, num(2.0))))
                } else {
                    binop(unop(op1, num(1.0)), op2, unop(op3, num(2.0)))
                },
            },
            ((Op::UnOp(op1), p1), (Op::UnOp(op2), p2), (Op::BinOp(op3, _), p3)) => TestCase {
                input: format!("{}{}1{}2", op1, op2, op3),
                output: if p2 < p3 && p1 < p3 {
                    unop(op1, unop(op2, binop(num(1.0), op3, num(2.0))))
                } else if p3 < p1 {
                    binop(unop(op1, unop(op2, num(1.0))), op3, num(2.0))
                } else {
                    unop(op1, binop(unop(op2, num(1.0)), op3, num(2.0)))
                },
            },
            ((Op::UnOp(op1), _), (Op::UnOp(op2), _), (Op::UnOp(op3), _)) => TestCase {
                input: format!("{}{}{}1", op1, op2, op3),
                output: unop(op1, unop(op2, unop(op3, num(1.0)))),
            },
        }
    }

    fn generate_cases() -> Vec<TestCase> {
        let mut cases: Vec<TestCase> = vec![];
        let ops = create_ops_with_priorities();
        for op in ops.iter() {
            cases.push(generate_case1(*op))
        }
        for op1 in ops.iter() {
            for op2 in ops.iter() {
                cases.push(generate_case2(*op1, *op2))
            }
        }
        for op1 in ops.iter() {
            for op2 in ops.iter() {
                for op3 in ops.iter() {
                    cases.push(generate_case3(*op1, *op2, *op3))
                }
            }
        }
        cases
    }

    #[test]
    fn test() {
        for test_case in generate_cases() {
            let actual = run_string_parser_impl(&test_case.input, exp_parser()).unwrap();
            let msg = format!(
                "input: {}, actual: {}, expected: {}",
                &test_case.input, &actual.0, &test_case.output
            );
            assert_eq!(actual, (test_case.output, ""), "{}", msg);
        }
    }
}
