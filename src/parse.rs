use std::{
    cmp::{max, min},
    iter::{Enumerate, Peekable},
    slice::Iter,
    str::Chars,
};

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
    pub loc: Location,
}

impl ParseError {
    pub fn new(msg: String, loc: Location) -> Self {
        Self { msg, loc }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub start: usize,
    pub end: usize,
}

impl Location {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn combine(&self, other: &Self) -> Self {
        Self {
            start: min(self.start, other.start),
            end: max(self.end, other.end),
        }
    }
}

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

#[derive(Debug)]
pub enum Ast {
    Ident(String),
    Lambda(String, Box<Ast>),
    App(Box<Ast>, Box<Ast>),
}

// LEX

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
            otherwise => {
                return Err(ParseError::new(
                    format!("Unexpected character `{}`", otherwise),
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

// PARSE

pub type TokenStream<'a> = Peekable<Iter<'a, Token>>;

pub fn parse_ast(tokens: &mut TokenStream) -> ParseResult<Ast> {
    parse_lambda(tokens)
}

fn parse_params(tokens: &mut TokenStream) -> ParseResult<Vec<String>> {
    let mut retr = Vec::new();

    while let Some(next) = tokens.peek() {
        match next.kind {
            TokenKind::Ident(ref id) => {
                if retr.contains(id) {
                    return Err(ParseError::new(
                        format!("Conflicting definitions for `{}`", id),
                        next.loc,
                    ));
                }
                retr.push(id.clone())
            }
            _ => break,
        }
        tokens.next();
    }

    Ok(retr)
}

fn parse_lambda(tokens: &mut TokenStream) -> ParseResult<Ast> {
    match tokens.next_if(|next| next.kind == TokenKind::BSlash) {
        Some(bslash) => {
            let params = parse_params(tokens)?;

            if params.len() == 0 {
                return Err(ParseError::new(
                    format!("Expected at least one parameter in lambda expression"),
                    bslash.loc,
                ));
            }

            match tokens.peek() {
                Some(next) => match next.kind {
                    TokenKind::Op(ref op) if op == "->" => {
                        tokens.next();
                        Ok(params.into_iter().rfold(parse_lambda(tokens)?, |acc, p| {
                            Ast::Lambda(p, Box::new(acc))
                        }))
                    }
                    _ => Err(ParseError::new(
                        format!("Expected `->` in lambda expression"),
                        next.loc,
                    )),
                },
                None => Err(ParseError::new(
                    format!("Expected `->` in lambda expression"),
                    bslash.loc,
                )),
            }
        }
        None => parse_app(tokens),
    }
}

fn parse_app(tokens: &mut TokenStream) -> ParseResult<Ast> {
    let mut ast = parse_atom(tokens)?;

    while let Some(next) = tokens.peek() {
        match next.kind {
            TokenKind::Ident(_) | TokenKind::LParen => {
                ast = Ast::App(Box::new(ast), Box::new(parse_atom(tokens)?))
            }
            _ => break,
        }
    }

    Ok(ast)
}

fn parse_atom(tokens: &mut TokenStream) -> ParseResult<Ast> {
    match tokens.next() {
        Some(next) => match next.kind {
            TokenKind::Ident(ref id) => Ok(Ast::Ident(id.clone())),
            TokenKind::LParen => {
                let retr = parse_ast(tokens)?;
                match tokens.next() {
                    Some(next) if next.kind == TokenKind::RParen => Ok(retr),
                    _ => Err(ParseError::new(format!("Unmatched `(`"), next.loc)),
                }
            }
            TokenKind::RParen => Err(ParseError::new(format!("Unmatched `)`"), next.loc)),
            ref otherwise => Err(ParseError::new(
                format!("Unexpected token `{:?}`", otherwise),
                next.loc,
            )),
        },
        None => Err(ParseError::new(
            format!("Unexpected EOF"),
            Location::new(0, 0),
        )),
    }
}
