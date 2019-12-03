use super::*;

fn loc(line: usize, column: usize) -> Location {
    Location { line, column }
}

#[test]
fn next() {
    let mut iter = make_located(
        r#"
ab
d

f
"#,
    );
    assert_eq!(loc(1, 0), iter.current_location());

    assert_eq!(Some((loc(1, 1), '\n')), iter.next());
    assert_eq!(loc(2, 0), iter.current_location());

    assert_eq!(Some((loc(2, 1), 'a')), iter.next());
    assert_eq!(loc(2, 1), iter.current_location());

    assert_eq!(Some((loc(2, 2), 'b')), iter.next());
    assert_eq!(loc(2, 2), iter.current_location());

    assert_eq!(Some((loc(2, 3), '\n')), iter.next());
    assert_eq!(loc(3, 0), iter.current_location());

    assert_eq!(Some((loc(3, 1), 'd')), iter.next());
    assert_eq!(loc(3, 1), iter.current_location());

    assert_eq!(Some((loc(3, 2), '\n')), iter.next());
    assert_eq!(loc(4, 0), iter.current_location());

    assert_eq!(Some((loc(4, 1), '\n')), iter.next());
    assert_eq!(loc(5, 0), iter.current_location());

    assert_eq!(Some((loc(5, 1), 'f')), iter.next());
    assert_eq!(loc(5, 1), iter.current_location());

    assert_eq!(Some((loc(5, 2), '\n')), iter.next());
    assert_eq!(loc(6, 0), iter.current_location());

    assert_eq!(None, iter.next());
    assert_eq!(loc(6, 0), iter.current_location());

    assert_eq!(None, iter.next());
    assert_eq!(loc(6, 0), iter.current_location());
}

#[test]
fn as_str() {
    let mut iter = make_located("abc");
    iter.next();
    assert_eq!(iter.as_str(), "bc");
}
