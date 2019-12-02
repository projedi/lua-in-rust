use super::*;

pub fn run_parser<'a, T>(input: &'a str, p: impl Parser<std::str::Chars<'a>, T>) -> Option<T> {
    super::run_parser(input.chars(), p)
}

pub fn parsed_string<'a, 'b: 'a, T: 'a>(
    p: Box<dyn Parser<std::str::Chars<'b>, T> + 'a>,
) -> Box<dyn Parser<std::str::Chars<'b>, (T, &'b str)> + 'a> {
    Box::new(move |s| {
        let (p_result, p_s) = p(s.clone())?;
        Some((
            (
                p_result,
                &s.iterator.as_str()[0..p_s.consumed_count - s.consumed_count],
            ),
            p_s,
        ))
    })
}

pub fn string_parser<'a, 'b: 'a>(
    expected_str: &'a str,
) -> Box<dyn Parser<std::str::Chars<'b>, &'b str> + 'a> {
    fmap(
        |(_, s)| s,
        parsed_string(sequence_parser(move || expected_str.chars())),
    )
}
