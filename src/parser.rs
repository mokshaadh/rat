use crate::lexer::{Token, TokenStream};

pub type ParseResult<T> = Result<T, String>;

#[derive(Debug)]
pub enum Ast {
    Ident(String),
    Lambda(String, Box<Ast>),
    App(Box<Ast>, Box<Ast>),
}

pub fn parse_ast(tokens: &mut TokenStream) -> ParseResult<Ast> {
    parse_lambda(tokens)
}

fn parse_params(tokens: &mut TokenStream) -> ParseResult<Vec<String>> {
    let mut retr = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::Ident(id)) => {
                if retr.contains(id) {
                    return Err(format!("Conflicting definitions for `{}`", id));
                }
                retr.push(id.clone());
            }
            _ => break,
        }
        tokens.next();
    }

    Ok(retr)
}

fn parse_lambda(tokens: &mut TokenStream) -> ParseResult<Ast> {
    if let Some(_) = tokens.next_if(|&t| *t == Token::BSlash) {
        let params = parse_params(tokens)?;

        if params.len() == 0 {
            return Err(format!(
                "Expected at least one parameter in lambda expression"
            ));
        }

        tokens
            .next_if(|&t| matches!(t, Token::Op(op) if op == "->"))
            .map_or_else(
                || Err(format!("Expected `->` in lambda expression")),
                |_| {
                    Ok(params.into_iter().rfold(parse_lambda(tokens)?, |acc, p| {
                        Ast::Lambda(p, Box::new(acc))
                    }))
                },
            )
    } else {
        parse_app(tokens)
    }
}

fn parse_app(tokens: &mut TokenStream) -> ParseResult<Ast> {
    let mut ast = parse_atom(tokens)?;

    while let Some(Token::Ident(_)) | Some(Token::LParen) = tokens.peek() {
        ast = Ast::App(Box::new(ast), Box::new(parse_atom(tokens)?))
    }

    Ok(ast)
}

fn parse_atom(tokens: &mut TokenStream) -> ParseResult<Ast> {
    match tokens.next() {
        Some(Token::Ident(id)) => Ok(Ast::Ident(id.clone())),
        Some(Token::LParen) => {
            let retr = parse_ast(tokens);
            tokens
                .next_if(|&t| matches!(t, Token::RParen))
                .map_or_else(|| Err(format!("Expected `)`")), |_| retr)
        }
        Some(Token::RParen) => Err(format!("Unmatched `)`")),
        None => Err(format!("Unexpected EOF")),
        otherwise => Err(format!("Unexpected token `{:?}`", otherwise)),
    }
}
