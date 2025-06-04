use std::{iter::Peekable, str::Chars};

pub type LexerInputStream<'a> = Peekable<Chars<'a>>;
pub type LexerResult<T> = Result<T, String>;
pub type TokenStream<'a> = Peekable<std::slice::Iter<'a, Token>>;

#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Op(String),
    LParen,
    RParen,
    BSlash,
}

pub fn lex<'a>(input: &mut LexerInputStream) -> LexerResult<Vec<Token>> {
    let mut retr = vec![];

    while let Some(next) = input.next() {
        match next {
            c if c.is_whitespace() => continue,
            '\\' => retr.push(Token::BSlash),
            '(' => retr.push(Token::LParen),
            ')' => retr.push(Token::RParen),
            '[' => todo!(),
            ']' => todo!(),
            c if c.is_ascii_alphanumeric() || c == '_' => retr.push(lex_ident(input, c)),
            c if c.is_ascii_graphic() => retr.push(lex_op(input, c)),
            otherwise => return Err(format!("Unexpected character `{}`", otherwise)),
        }
    }

    Ok(retr)
}

fn lex_ident<'a>(input: &mut LexerInputStream, so_far: char) -> Token {
    let mut retr = so_far.to_string();

    while let Some(peeked) =
        input.next_if(|&p| (p.is_ascii_alphanumeric() || p == '_') && !"([\\])".contains(p))
    {
        retr.push(peeked);
    }

    Token::Ident(retr)
}

fn lex_op<'a>(input: &mut LexerInputStream, so_far: char) -> Token {
    let mut retr = so_far.to_string();

    while let Some(peeked) = input
        .next_if(|&p| p.is_ascii_graphic() && !p.is_ascii_alphanumeric() && !"([_\\])".contains(p))
    {
        retr.push(peeked);
    }

    Token::Op(retr)
}
