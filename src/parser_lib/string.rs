use super::*;

pub fn run_parser<'a, T>(
    input: &'a str,
    p: impl Parser<std::str::Chars<'a>, T>,
) -> (Option<T>, &'a str) {
    let (x, i) = super::run_parser(input.chars(), p);
    (x, i.as_str())
}

pub fn parsed_string<'a, 'b: 'a, T: 'a>(
    p: Box<dyn Parser<std::str::Chars<'b>, T> + 'a>,
) -> Box<dyn Parser<std::str::Chars<'b>, (T, &'b str)> + 'a> {
    Box::new(move |s| {
        let (p_result, p_s) = p(s.clone());
        let parsed_str = &s.iterator.as_str()[0..p_s.consumed_count - s.consumed_count];
        let result = match p_result {
            Some(p_result) => Some((p_result, parsed_str)),
            None => None,
        };
        (result, p_s)
    })
}

pub fn string_parser<'a, 'b: 'a>(
    expected_str: &'a str,
) -> Box<dyn Parser<std::str::Chars<'b>, &'b str> + 'a> {
    map_filter(
        |result| match result {
            Some((_, s)) => Some(s),
            None => None,
        },
        try_parser(parsed_string(sequence_parser(move || expected_str.chars()))),
    )
}
