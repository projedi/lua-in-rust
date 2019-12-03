use super::*;
use crate::utils::located_chars::{LocatedChars, Location};

pub trait CharIterator: Iterator {
    type SequenceItem;

    fn as_str(&self, len: usize) -> Self::SequenceItem;

    fn matches(lhs: char, rhs: &Self::Item) -> bool;
}

impl<'a> CharIterator for std::str::Chars<'a> {
    type SequenceItem = &'a str;

    fn as_str(&self, len: usize) -> Self::SequenceItem {
        &(self as &std::str::Chars<'a>).as_str()[0..len]
    }

    fn matches(lhs: char, rhs: &Self::Item) -> bool {
        lhs == *rhs
    }
}

impl<'a> CharIterator for LocatedChars<'a> {
    type SequenceItem = (Location, &'a str);

    fn as_str(&self, len: usize) -> Self::SequenceItem {
        let loc = self.current_location();
        let s = (self as &LocatedChars<'a>).as_str();
        (
            Location {
                line: loc.line,
                column: loc.column + 1,
            },
            &s[0..len],
        )
    }

    fn matches(lhs: char, rhs: &Self::Item) -> bool {
        lhs == rhs.1
    }
}

pub fn char_parser<I: CharIterator + Clone>(c: char) -> Box<dyn Parser<I, I::Item>> {
    satisfies(move |in_c| I::matches(c, in_c))
}

pub fn parsed_string<'a, T: 'a, I: CharIterator + Clone + 'a>(
    p: Box<dyn Parser<I, T> + 'a>,
) -> Box<dyn Parser<I, (T, I::SequenceItem)> + 'a> {
    Box::new(move |s| {
        let (p_result, p_s) = p(s.clone());
        let len_before = s.iterator.consumed_count();
        let len_after = p_s.iterator.consumed_count();
        let parsed_str = s.iterator.into_inner().as_str(len_after - len_before);
        let result = match p_result {
            Some(p_result) => Some((p_result, parsed_str)),
            None => None,
        };
        (result, p_s)
    })
}

pub fn string_parser<'a, I: CharIterator + Clone + 'a>(
    expected_str: &'a str,
) -> Box<dyn Parser<I, I::SequenceItem> + 'a> {
    map_filter(
        |result| match result {
            Some((_, s)) => Some(s),
            None => None,
        },
        try_parser(parsed_string(sequence_parser(
            |lhs, rhs| I::matches(*lhs, rhs),
            move || expected_str.chars(),
        ))),
    )
}
