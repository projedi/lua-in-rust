#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone)]
pub struct LocatedChars<'a> {
    inner: std::str::Chars<'a>,
    current_location: Location,
}

impl<'a> Iterator for LocatedChars<'a> {
    type Item = (Location, char);

    fn next(&mut self) -> Option<Self::Item> {
        let loc = self.current_location;
        match self.inner.next() {
            None => None,
            Some('\n') => {
                let newline_loc = Location {
                    line: loc.line,
                    column: loc.column + 1,
                };
                self.current_location = Location {
                    line: loc.line + 1,
                    column: 0,
                };
                Some((newline_loc, '\n'))
            }
            Some(c) => {
                self.current_location = Location {
                    line: loc.line,
                    column: loc.column + 1,
                };
                Some((self.current_location, c))
            }
        }
    }
}

impl<'a> LocatedChars<'a> {
    pub fn as_str(&self) -> &'a str {
        self.inner.as_str()
    }

    pub fn current_location(&self) -> Location {
        self.current_location
    }
}

pub fn make_located(s: &str) -> LocatedChars {
    LocatedChars {
        inner: s.chars(),
        current_location: Location { line: 1, column: 0 },
    }
}

#[cfg(test)]
mod tests;
