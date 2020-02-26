use std::iter;

pub struct Spliterator<'a> {
    string: &'a str,
    pattern: &'a [char],
    next: Option<&'a str>,
}

impl<'a> iter::Iterator for Spliterator<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next {
            self.next = None;
            return Some(next);
        }

        if let Some((split, end)) = self
            .string
            .find(self.pattern)
            .map(|i| self.string.split_at(i))
        {
            let (op, tail) = end.split_at(1);
            self.string = tail;
            self.next = Some(op);
            Some(split)
        } else {
            None
        }
    }
}

impl<'a> Spliterator<'a> {
    pub fn new(string: &'a str, pattern: &'a [char]) -> Spliterator<'a> {
        Spliterator {
            string,
            pattern,
            next: None,
        }
    }
}
