use std::num::ParseFloatError;
use std::num::ParseIntError;

use super::loc::{Loc, FIRST_COL};
use crate::float;

#[derive(Debug)]
pub enum LexerError {
    IncorrectInput, // TODO: something better than this
    UnexpectedEof,
    ParseIntError,
    ParseFloatError,
    ExpectChar(char),
    ExpectHexDigit,
    ExpectOctDigit,
    ExpectDecDigit,
}

impl From<ParseIntError> for LexerError {
    fn from(_: ParseIntError) -> Self {
        LexerError::ParseIntError
    }
}

impl From<ParseFloatError> for LexerError {
    fn from(_: ParseFloatError) -> Self {
        LexerError::ParseFloatError
    }
}

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Copy, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    pub loc: Loc,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            pos: 0,
            loc: Loc::start(),
        }
    }

    // Identifiers
    // ident = letter { letter | decimalDigit | "_" }
    // messageName = ident
    // enumName = ident
    // fieldName = ident
    // oneofName = ident
    // mapName = ident
    // serviceName = ident
    // rpcName = ident
    // streamName = ident
    // messageType = [ "." ] { ident "." } messageName
    // enumType = [ "." ] { ident "." } enumName
    // groupName = capitalLetter { letter | decimalDigit | "_" } (proto2 only)

    // ident = letter { letter | decimalDigit | "_" }
    pub fn next_ident_opt(&mut self) -> LexerResult<Option<String>> {
        if let Some(c) = self.next_letter_opt() {
            let mut ident = String::new();
            ident.push(c);
            while let Some(c) = self.next_char_if(|c| c.is_ascii_alphanumeric() || c == '_') {
                ident.push(c);
            }
            Ok(Some(ident))
        } else {
            Ok(None)
        }
    }

    // Letters and digits
    // letter = "A" … "Z" | "a" … "z"
    // capitalLetter =  "A" … "Z" (proto2 only)
    // decimalDigit = "0" … "9"
    // octalDigit   = "0" … "7"
    // hexDigit     = "0" … "9" | "A" … "F" | "a" … "f"

    // letter = "A" … "Z" | "a" … "z"
    fn next_letter_opt(&mut self) -> Option<char> {
        fn is_letter(c: char) -> bool {
            c.is_alphabetic() || c == '_'
        }
        self.next_char_if(is_letter)
    }

    // decimalDigit = "0" … "9"
    fn next_decimal_digit(&mut self) -> LexerResult<u32> {
        self.next_char_expect(|c| c >= '0' && c <= '9', LexerError::ExpectDecDigit)
            .map(|c| c as u32 - '0' as u32)
    }

    // octalDigit = "0" … "7"
    fn next_octal_digit(&mut self) -> LexerResult<u32> {
        self.next_char_expect(|c| c >= '0' && c <= '7', LexerError::ExpectOctDigit)
            .map(|c| c as u32 - '0' as u32)
    }

    // hexDigit = "0" … "9" | "A" … "F" | "a" … "f"
    fn next_hex_digit(&mut self) -> LexerResult<u32> {
        let mut clone = self.clone();
        let r = match clone.next_char()? {
            c if c >= '0' && c <= '9' => c as u32 - b'0' as u32,
            c if c >= 'A' && c <= 'Z' => c as u32 - b'A' as u32 + 10,
            c if c >= 'a' && c <= 'z' => c as u32 - b'a' as u32 + 10,
            _ => return Err(LexerError::ExpectHexDigit),
        };
        *self = clone;
        Ok(r)
    }

    // Floating-point literals
    // floatLit = ( decimals "." [ decimals ] [ exponent ] | decimals exponent | "."decimals [ exponent ] ) | "inf" | "nan"
    // decimals  = decimalDigit { decimalDigit }
    // exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals

    // floatLit = ( decimals "." [ decimals ] [ exponent ] | decimals exponent | "."decimals [ exponent ] ) | "inf" | "nan"
    pub fn next_float_lit_opt(&mut self) -> LexerResult<Option<f64>> {
        // "inf" and "nan" are handled as part of ident
        let mut clone = self.clone();
        let pos = clone.pos;

        if clone.next_char_if_eq('.') {
            clone.next_decimal_digits()?;
            clone.next_exponent_opt()?;
        } else {
            clone.next_decimal_digits()?;
            if clone.next_char_if_eq('.') {
                clone.next_decimal_digits()?;
                clone.next_exponent_opt()?;
            } else {
                if clone.next_exponent_opt()? == None {
                    return Err(LexerError::ParseFloatError);
                }
            }
        }
        let f = float::parse_protobuf_float(&clone.input[pos..clone.pos])?;
        *self = clone;
        Ok(Some(f))
    }

    // decimals  = decimalDigit { decimalDigit }
    fn next_decimal_digits(&mut self) -> LexerResult<()> {
        self.next_decimal_digit()?;
        self.take_while(|c| c >= '0' && c <= '9');
        Ok(())
    }

    // exponent = ( "e" | "E" ) [ "+" | "-" ] decimals
    fn next_exponent_opt(&mut self) -> LexerResult<Option<()>> {
        if self.next_char_if_in("eE") != None {
            self.next_char_if_in("+-");
            self.next_decimal_digit()?;
            Ok(Some(()))
        } else {
            Ok(None)
        }
    }

    // Integer literals
    // intLit = decimalLit | octalLit | hexLit
    // decimalLit = ( "1" … "9" ) { decimalDigit }
    // octalLit = "0" { octalDigit }
    // hexLit = "0" ( "x" | "X" ) hexDigit { hexDigit }

    // intLit = decimalLit | octalLit | hexLit
    pub fn next_int_lit_opt(&mut self) -> LexerResult<Option<u64>> {
        if let Some(i) = self.next_hex_lit_opt()? {
            return Ok(Some(i));
        }
        if let Some(i) = self.next_decimal_octal_lit_opt()? {
            return Ok(Some(i));
        }
        Ok(None)
    }

    // decimalLit = ( "1" … "9" ) { decimalDigit }
    // octalLit = "0" { octalDigit }
    fn next_decimal_octal_lit_opt(&mut self) -> LexerResult<Option<u64>> {
        // do not advance on number parse error
        let mut clone = self.clone();
        let start = clone.pos;

        Ok(if clone.next_char_if(|c| c.is_ascii_digit()) != None {
            clone.take_while(|c| c.is_ascii_digit());
            let value = clone.input[start..clone.pos].parse()?;
            *self = clone;
            Some(value)
        } else {
            None
        })
    }

    // hexLit = "0" ( "x" | "X" ) hexDigit { hexDigit }
    fn next_hex_lit_opt(&mut self) -> LexerResult<Option<u64>> {
        Ok(
            if self.skip_if_lookahead_is_str("0x") || self.skip_if_lookahead_is_str("0X") {
                let s = self.take_while(|c| c.is_ascii_hexdigit());
                Some(u64::from_str_radix(s, 16)? as u64)
            } else {
                None
            },
        )
    }

    // String literals
    // strLit = ( "'" { charValue } "'" ) |  ( '"' { charValue } '"' )
    // charValue = hexEscape | octEscape | charEscape | /[^\0\n\\]/
    // hexEscape = '\' ( "x" | "X" ) hexDigit hexDigit
    // octEscape = '\' octalDigit octalDigit octalDigit
    // charEscape = '\' ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | '\' | "'" | '"' )
    // quote = "'" | '"'

    // https://github.com/google/protobuf/issues/4564
    // strLit = ( "'" { charValue } "'" ) | ( '"' { charValue } '"' )
    pub fn next_str_lit_opt(&mut self) -> LexerResult<Option<String>> {
        let mut clone = self.clone();
        let start = clone.pos;

        let q = match clone.next_char_if_in("'\"") {
            Some(q) => q,
            None => return Ok(None),
        };
        while clone.lookahead_char() != Some(q) {
            clone.next_char_value()?;
        }
        clone.next_char_expect_eq(q)?;

        let mut str = String::new();
        str.push_str(&self.input[start + 1..clone.pos - 1]);
        *self = clone;
        Ok(Some(str))
    }

    // charValue = hexEscape | octEscape | charEscape | /[^\0\n\\]/
    // hexEscape = '\' ( "x" | "X" ) hexDigit hexDigit
    // https://github.com/google/protobuf/issues/4560
    // octEscape = '\' octalDigit octalDigit octalDigit
    // charEscape = '\' ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | '\' | "'" | '"' )
    // quote = "'" | '"'
    fn next_char_value(&mut self) -> LexerResult<char> {
        match self.next_char()? {
            '\\' => {
                match self.next_char()? {
                    '\'' => Ok('\''),
                    '"' => Ok('"'),
                    '\\' => Ok('\\'),
                    'a' => Ok('\x07'),
                    'b' => Ok('\x08'),
                    'f' => Ok('\x0c'),
                    'n' => Ok('\n'),
                    'r' => Ok('\r'),
                    't' => Ok('\t'),
                    'v' => Ok('\x0b'),
                    'x' => {
                        let d1 = self.next_hex_digit()? as u8;
                        let d2 = self.next_hex_digit()? as u8;
                        // TODO: do not decode as char if > 0x80
                        Ok(((d1 << 4) | d2) as char)
                    }
                    d if d >= '0' && d <= '7' => {
                        let mut r = d as u8 - b'0';
                        for _ in 0..2 {
                            match self.next_octal_digit() {
                                Err(_) => break,
                                Ok(d) => r = (r << 3) + d as u8,
                            }
                        }
                        // TODO: do not decode as char if > 0x80
                        Ok(r as char)
                    }
                    // https://github.com/google/protobuf/issues/4562
                    c => Ok(c),
                }
            }
            '\n' | '\0' => Err(LexerError::IncorrectInput),
            c => Ok(c),
        }
    }

    fn next_char(&mut self) -> LexerResult<char> {
        self.next_char_opt().ok_or(LexerError::UnexpectedEof)
    }

    fn next_char_if_eq(&mut self, expect: char) -> bool {
        self.next_char_if(|c| c == expect) != None
    }

    fn next_char_if_in(&mut self, alphabet: &str) -> Option<char> {
        for c in alphabet.chars() {
            if self.next_char_if_eq(c) {
                return Some(c);
            }
        }
        None
    }

    fn next_char_expect<P>(&mut self, expect: P, err: LexerError) -> LexerResult<char>
    where
        P: FnOnce(char) -> bool,
    {
        self.next_char_if(expect).ok_or(err)
    }

    pub fn next_char_if<P>(&mut self, p: P) -> Option<char>
    where
        P: FnOnce(char) -> bool,
    {
        let mut clone = self.clone();
        match clone.next_char_opt() {
            Some(c) if p(c) => {
                *self = clone;
                Some(c)
            }
            _ => None,
        }
    }

    fn next_char_opt(&mut self) -> Option<char> {
        let rem = self.rem_chars();
        if rem.is_empty() {
            return None;
        }
        let mut char_indices = rem.char_indices();
        let (_, c) = char_indices.next().unwrap();
        let c_len = char_indices.next().map(|(len, _)| len).unwrap_or(rem.len());
        self.pos += c_len;
        if c == '\n' {
            self.loc.line += 1;
            self.loc.col = FIRST_COL;
        } else {
            self.loc.col += 1;
        }
        Some(c)
    }

    fn next_char_expect_eq(&mut self, expect: char) -> LexerResult<()> {
        if self.next_char_if_eq(expect) {
            Ok(())
        } else {
            Err(LexerError::ExpectChar(expect))
        }
    }

    pub fn eof(&self) -> bool {
        self.pos == self.input.len()
    }

    pub fn skip_ws(&mut self) -> LexerResult<()> {
        loop {
            let pos = self.pos;
            self.skip_whitespaces();
            self.skip_comment()?;
            if pos == self.pos {
                // Did not advance
                return Ok(());
            }
        }
    }

    // Skip whitespaces
    fn skip_whitespaces(&mut self) {
        self.take_while(|c| c.is_whitespace());
    }

    fn skip_comment(&mut self) -> LexerResult<()> {
        self.skip_c_comment()?;
        self.skip_cpp_comment();
        self.skip_sh_comment();
        Ok(())
    }

    fn skip_c_comment(&mut self) -> LexerResult<()> {
        if self.skip_if_lookahead_is_str("/*") {
            let end = "*/";
            match self.rem_chars().find(end) {
                None => Err(LexerError::UnexpectedEof),
                Some(len) => {
                    let new_pos = self.pos + len + end.len();
                    self.skip_to_pos(new_pos);
                    Ok(())
                }
            }
        } else {
            Ok(())
        }
    }

    fn skip_cpp_comment(&mut self) {
        if self.skip_if_lookahead_is_str("//") {
            loop {
                match self.next_char_opt() {
                    Some('\n') | None => break,
                    _ => {}
                }
            }
        }
    }

    fn skip_sh_comment(&mut self) {
        if self.skip_if_lookahead_is_str("#") {
            loop {
                match self.next_char_opt() {
                    Some('\n') | None => break,
                    _ => {}
                }
            }
        }
    }

    fn skip_if_lookahead_is_str(&mut self, s: &str) -> bool {
        if self.rem_chars().starts_with(s) {
            let new_pos = self.pos + s.len();
            self.skip_to_pos(new_pos);
            true
        } else {
            false
        }
    }

    // properly update line and column
    fn skip_to_pos(&mut self, new_pos: usize) -> &'a str {
        assert!(new_pos >= self.pos);
        assert!(new_pos <= self.input.len());
        let pos = self.pos;
        while self.pos != new_pos {
            self.next_char_opt().unwrap();
        }
        &self.input[pos..new_pos]
    }

    fn take_while<P>(&mut self, p: P) -> &'a str
    where
        P: Fn(char) -> bool,
    {
        let start = self.pos;
        while self.lookahead_char().map(&p) == Some(true) {
            self.next_char_opt().unwrap();
        }
        let end = self.pos;
        &self.input[start..end]
    }

    fn lookahead_char(&self) -> Option<char> {
        self.clone().next_char_opt()
    }

    fn rem_chars(&self) -> &'a str {
        &self.input[self.pos..]
    }
}
