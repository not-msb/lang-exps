#[derive(Clone)]
pub struct Cursor<'c, T> {
    data: &'c [T],
    pos: usize,
}

impl<'c, T: Clone> Cursor<'c, T> {
    pub fn save(&self) -> usize {
        self.pos
    }

    pub unsafe fn reset(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn next(&mut self) -> Option<T> {
        if self.pos >= self.data.len() { return None; }
        self.pos += 1;
        Some(self.data[self.pos-1].clone())
    }
}

impl<'c, T> From<&'c [T]> for Cursor<'c, T> {
    fn from(input: &'c [T]) -> Self {
        Cursor {
            data: input,
            pos: 0,
        }
    }
}

#[derive(Default, Clone, Debug, PartialEq)]
pub enum LexError {
    #[default]
    NonAsciiChar,
    ParseInt,
}

impl From<std::num::ParseIntError> for LexError {
    fn from(_err: std::num::ParseIntError) -> Self {
        Self::ParseInt
    }
}
