use super::*;

#[test]
fn next() {
    let mut iter = enumerate("abc".chars());
    assert_eq!(0, iter.consumed_count());

    assert_eq!(Some('a'), iter.next());
    assert_eq!(1, iter.consumed_count());

    assert_eq!(Some('b'), iter.next());
    assert_eq!(2, iter.consumed_count());

    assert_eq!(Some('c'), iter.next());
    assert_eq!(3, iter.consumed_count());

    assert_eq!(None, iter.next());
    assert_eq!(3, iter.consumed_count());

    assert_eq!(None, iter.next());
    assert_eq!(3, iter.consumed_count());
}

#[test]
fn into_inner() {
    let mut iter = enumerate("abc".chars());
    iter.next();
    assert_eq!(iter.into_inner().as_str(), "bc");
}
