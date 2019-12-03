#[derive(Clone)]
pub struct Enumerate<I> {
    inner: I,
    consumed_count: usize,
}

impl<I: Iterator> Iterator for Enumerate<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            None => None,
            Some(x) => {
                self.consumed_count += 1;
                Some(x)
            }
        }
    }
}

impl<I> Enumerate<I> {
    pub fn into_inner(self) -> I {
        self.inner
    }

    pub fn consumed_count(&self) -> usize {
        self.consumed_count
    }
}

pub fn enumerate<I>(iterator: I) -> Enumerate<I> {
    Enumerate {
        inner: iterator,
        consumed_count: 0,
    }
}

#[cfg(test)]
mod tests;
