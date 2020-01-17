use super::loc::Loc;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // A sequence of letters, digits, and underscores, not
    // starting with a digit. It is an error for a number
    // to be followed by an identifier with no space in
    // between.
    Ident(String),
    // Any other printable character, like '!' or '+'.
    // Symbols are always a single character, so "!+$%" is
    // four tokens.
    Symbol(char),
    // A sequence of digits representing an integer. Normally
    // the digits are decimal, but a prefix of "0x" indicates
    // a hex number and a leading zero indicates octal, just
    // like with C numeric literals. A leading negative sign
    // is NOT included in the token; it's up to the parser to
    // interpret the unary minus operator on its own.
    IntLit(u64),
    // A floating point literal, with a fractional part and/or
    // an exponent. Always in decimal. Again, never
    // negative.
    FloatLit(f64),
    // A quoted sequence of escaped characters. Either single
    // or double quotes can be used, but they must match.
    // A string literal cannot cross a line break.
    StrLit(String),
}

impl Token {
    /// Back to original
    pub fn format(&self) -> String {
        match self {
            Token::Ident(s) => s.clone(),
            Token::Symbol(c) => c.to_string(),
            Token::IntLit(i) => i.to_string(),
            Token::FloatLit(f) => f.to_string(),
            Token::StrLit(s) => s.clone(),
        }
    }
}

#[derive(Clone)]
pub struct TokenWithLocation {
    pub token: Token,
    pub loc: Loc,
}
