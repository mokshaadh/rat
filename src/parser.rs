use std::{iter::Peekable, slice::Iter};

use crate::{
    error::{Location, ParseError, ParseResult},
    lexer::{Token, TokenKind},
};

#[derive(Debug)]
pub enum Ast {
    Ident(String),
    Lambda(String, Box<Ast>),
    App(Box<Ast>, Box<Ast>),
}

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
                        format!("conflicting definitions for `{}`", id),
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
                    "expected at least one parameter in lambda expression".into(),
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
                        "expected `->` in lambda expression".into(),
                        next.loc,
                    )),
                },
                None => Err(ParseError::new(
                    "expected `->` in lambda expression".into(),
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
                    _ => Err(ParseError::new("unmatched delimiter".into(), next.loc)),
                }
            }
            TokenKind::RParen => Err(ParseError::new("unmatched delimiter".into(), next.loc)),
            _ => Err(ParseError::new("unexpected token".into(), next.loc)),
        },
        None => Err(ParseError::new(
            "unexpected EOF".into(),
            Location::new(0, 0),
        )),
    }
}
