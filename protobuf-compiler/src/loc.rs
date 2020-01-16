use std::fmt;

pub const FIRST_LINE: u32 = 1;
pub const FIRST_COL: u32 = 1;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Loc {
    /// 1-based
    pub line: u32,
    /// 1-based
    pub col: u32,
}

impl Loc {
    pub fn start() -> Loc {
        Loc {
            line: FIRST_LINE,
            col: FIRST_COL,
        }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}
