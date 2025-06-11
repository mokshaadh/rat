use std::{
    iter::{Enumerate, Peekable},
    str::Chars,
};

use crate::error::{Location, ParseError, ParseResult};

pub struct Token {
    pub kind: TokenKind,
    pub loc: Location,
}

impl Token {
    pub fn new(kind: TokenKind, loc: Location) -> Self {
        Self { kind, loc }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Ident(String),
    Op(String),
    LParen,
    RParen,
    BSlash,
}

pub type LexInputStream<'a> = Peekable<Enumerate<Chars<'a>>>;

pub fn lex(input: &mut LexInputStream) -> ParseResult<Vec<Token>> {
    let mut retr = vec![];

    while let Some((loc, ch)) = input.next() {
        match ch {
            c if c.is_whitespace() => continue,
            '\\' => retr.push(Token::new(TokenKind::BSlash, Location::new(loc, loc))),
            '(' => retr.push(Token::new(TokenKind::LParen, Location::new(loc, loc))),
            ')' => retr.push(Token::new(TokenKind::RParen, Location::new(loc, loc))),
            c if c.is_ascii_alphanumeric() || c == '_' => retr.push(lex_ident(input, (loc, ch))),
            c if c.is_ascii_graphic() => retr.push(lex_op(input, (loc, ch))),
            _ => {
                return Err(ParseError::new(
                    "unexpected character".into(),
                    Location::new(loc, loc),
                ))
            }
        }
    }

    Ok(retr)
}

fn lex_ident(input: &mut LexInputStream, (start, so_far): (usize, char)) -> Token {
    lex_while(
        input,
        |ch| (ch.is_ascii_alphanumeric() || ch == '_') && !"([\\])".contains(ch),
        |retr| TokenKind::Ident(retr),
        (start, so_far),
    )
}

fn lex_op(input: &mut LexInputStream, (start, so_far): (usize, char)) -> Token {
    lex_while(
        input,
        |ch| ch.is_ascii_graphic() && !ch.is_ascii_alphanumeric() && !"([_\\])".contains(ch),
        |retr| TokenKind::Op(retr),
        (start, so_far),
    )
}

fn lex_while(
    input: &mut LexInputStream,
    cond: impl Fn(char) -> bool,
    tok_kind: impl Fn(String) -> TokenKind,
    (start, so_far): (usize, char),
) -> Token {
    let mut retr = so_far.to_string();
    let mut end = start;

    while let Some((loc, ch)) = input.next_if(|&(_, ch)| cond(ch)) {
        end = loc;
        retr.push(ch);
    }

    Token::new(tok_kind(retr), Location::new(start, end))
}
