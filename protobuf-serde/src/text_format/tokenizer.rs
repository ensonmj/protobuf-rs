use std::f64;

use super::float::PROTOBUF_INF;
use super::float::PROTOBUF_NAN;
use super::lexer::Lexer;
use super::lexer::LexerError;
use super::loc::Loc;
use super::token::Token;
use super::token::TokenWithLocation;

#[derive(Debug)]
pub enum TokenizerError {
    LexerError(LexerError),
    InternalError,
    IncorrectInput,
    UnexpectedEof,
    ExpectStrLit,
    ExpectIntLit,
    ExpectFloatLit,
    ExpectIdent,
    ExpectedNamedIdent(String),
    ExpectChar(char),
    ExpectAnyChar(Vec<char>),
}

pub type TokenizerResult<R> = Result<R, TokenizerError>;

impl From<LexerError> for TokenizerError {
    fn from(e: LexerError) -> Self {
        TokenizerError::LexerError(e)
    }
}

#[derive(Clone)]
pub struct Tokenizer<'a> {
    lexer: Lexer<'a>,
    next_token: Option<TokenWithLocation>,
    last_token_loc: Option<Loc>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            lexer: Lexer::new(input),
            next_token: None,
            last_token_loc: None,
        }
    }

    /// No more tokens
    pub fn syntax_eof(&mut self) -> TokenizerResult<bool> {
        Ok(self.lookahead()?.is_none())
    }

    pub fn lookahead_some(&mut self) -> TokenizerResult<&Token> {
        match self.lookahead()? {
            Some(token) => Ok(token),
            None => Err(TokenizerError::UnexpectedEof),
        }
    }

    pub fn lookahead_ident_if_in(&mut self, idents: &[&str]) -> TokenizerResult<Option<String>> {
        Ok(match self.lookahead()? {
            Some(Token::Ident(next)) => idents
                .into_iter()
                .find(|&i| i == next)
                .map(|v| v.to_string()),
            _ => None,
        })
    }

    pub fn lookahead_ident_if_eq(&mut self, ident: &str) -> TokenizerResult<bool> {
        Ok(match self.lookahead()? {
            Some(Token::Ident(i)) => i == ident,
            _ => false,
        })
    }

    pub fn lookahead_symbol_if_in(&mut self, symbols: &str) -> TokenizerResult<Option<char>> {
        Ok(match self.lookahead()? {
            Some(Token::Symbol(next)) => symbols.chars().into_iter().find(|i| i == next),
            _ => None,
        })
    }

    pub fn lookahead_symbol_if_eq(&mut self, symbol: char) -> TokenizerResult<bool> {
        Ok(match self.lookahead()? {
            Some(Token::Symbol(c)) => *c == symbol,
            _ => false,
        })
    }

    pub fn lookahead_is_int_lit(&mut self) -> TokenizerResult<bool> {
        Ok(match self.lookahead()? {
            Some(&Token::IntLit(..)) => true,
            _ => false,
        })
    }

    pub fn lookahead_is_float_lit(&mut self) -> TokenizerResult<bool> {
        Ok(match self.lookahead()? {
            Some(&Token::FloatLit(..)) => true,
            _ => false,
        })
    }

    pub fn lookahead_is_str_lit(&mut self) -> TokenizerResult<bool> {
        Ok(match self.lookahead()? {
            Some(&Token::StrLit(..)) => true,
            _ => false,
        })
    }

    pub fn next_some(&mut self) -> TokenizerResult<Token> {
        self.lookahead_some()?;
        self.advance()
    }

    pub fn next_ident(&mut self) -> TokenizerResult<String> {
        self.next_token_expect_map(|token| match token {
            Token::Ident(ident) => Ok(ident.clone()),
            _ => Err(TokenizerError::ExpectIdent),
        })
    }

    pub fn next_ident_if_eq(&mut self, word: &str) -> TokenizerResult<bool> {
        self.next_token_if_map(|token| match token {
            Token::Ident(next) => word == next,
            _ => false,
        })
    }

    pub fn next_ident_expect_eq(&mut self, word: &str) -> TokenizerResult<()> {
        self.next_token_expect_map(|token| match token {
            Token::Ident(next) if word == next => Ok(()),
            _ => Err(TokenizerError::ExpectedNamedIdent(word.to_owned())),
        })
    }

    pub fn next_symbol_if_eq(&mut self, symbol: char) -> TokenizerResult<bool> {
        self.next_token_if_map(|token| match token {
            Token::Symbol(c) => *c == symbol,
            _ => false,
        })
    }

    pub fn next_symbol_expect_eq(&mut self, symbol: char) -> TokenizerResult<()> {
        self.next_token_expect_map(|token| match token {
            Token::Symbol(c) if *c == symbol => Ok(()),
            _ => Err(TokenizerError::ExpectChar(symbol)),
        })
    }

    pub fn next_int_lit(&mut self) -> TokenizerResult<u64> {
        self.next_token_expect_map(|token| match token {
            Token::IntLit(v) => Ok(*v),
            _ => Err(TokenizerError::ExpectIntLit),
        })
    }

    pub fn next_float_lit(&mut self) -> TokenizerResult<f64> {
        self.next_token_expect_map(|token| match token {
            Token::FloatLit(v) => Ok(*v),
            _ => Err(TokenizerError::ExpectFloatLit),
        })
    }

    pub fn next_str_lit(&mut self) -> TokenizerResult<String> {
        self.next_token_expect_map(|token| match token {
            Token::StrLit(s) => Ok(s.clone()),
            _ => Err(TokenizerError::ExpectStrLit),
        })
    }

    pub fn next_token_if_map<P>(&mut self, p: P) -> TokenizerResult<bool>
    where
        P: FnOnce(&Token) -> bool,
    {
        if let Some(token) = self.lookahead()? {
            if p(token) {
                self.next_token = None;
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub fn next_token_expect_map<P, R, E>(&mut self, p: P) -> Result<R, E>
    where
        P: FnOnce(&Token) -> Result<R, E>,
        E: From<TokenizerError>,
    {
        let r = match self.lookahead()? {
            Some(token) => p(token)?,
            None => return Err(TokenizerError::UnexpectedEof.into()),
        };
        self.next_token = None;
        Ok(r)
    }

    /// Can be called only after lookahead, otherwise it's error
    pub fn advance(&mut self) -> TokenizerResult<Token> {
        self.next_token
            .take()
            .map(|TokenWithLocation { token, .. }| token)
            .ok_or(TokenizerError::InternalError)
    }

    pub fn loc(&self) -> Loc {
        // After lookahead return the location of the next token
        self.next_token
            .as_ref()
            .map(|t| t.loc.clone())
            // After token consumed return the location of that token
            .or(self.last_token_loc.clone())
            // Otherwise return the position of lexer
            .unwrap_or(self.lexer.loc)
    }

    fn lookahead(&mut self) -> TokenizerResult<Option<&Token>> {
        Ok(match self.next_token {
            Some(ref token) => Some(&token.token),
            None => {
                self.next_token = self.next_token()?;
                self.last_token_loc = self.next_token.as_ref().map(|t| t.loc.clone());
                match self.next_token {
                    Some(ref token) => Some(&token.token),
                    None => None,
                }
            }
        })
    }

    fn next_token(&mut self) -> TokenizerResult<Option<TokenWithLocation>> {
        self.lexer.skip_ws()?;
        if self.lexer.eof() {
            return Ok(None);
        }
        let loc = self.lexer.loc;
        let token = {
            if let Ok(Some(ident)) = self.lexer.next_ident_opt() {
                if ident == PROTOBUF_NAN {
                    Token::FloatLit(f64::NAN)
                } else if ident == PROTOBUF_INF {
                    Token::FloatLit(f64::INFINITY)
                } else {
                    Token::Ident(ident)
                }
            } else if let Ok(Some(f)) = self.lexer.next_float_lit_opt() {
                Token::FloatLit(f)
            } else if let Ok(Some(i)) = self.lexer.next_int_lit_opt() {
                Token::IntLit(i)
            } else if let Ok(Some(s)) = self.lexer.next_str_lit_opt() {
                Token::StrLit(s)
            } else if let Some(c) = self.lexer.next_char_if(|c| c.is_ascii_punctuation()) {
                // This branch must be after str lit
                Token::Symbol(c)
            } else {
                return Err(TokenizerError::IncorrectInput);
            }
        };
        Ok(Some(TokenWithLocation { token, loc }))
    }
}
