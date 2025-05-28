use std::{
    collections::{HashMap, HashSet},
    io::Write,
    iter::Peekable,
    str::Chars,
};

#[derive(Debug, PartialEq)]
enum Token {
    Ident(String),
    Op(String),
    LParen,
    RParen,
    BSlash,
}

#[derive(Debug, Clone)]
enum Expr {
    Ident(String),
    Lambda(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn pretty(&self, level: u32) -> String {
        match self {
            Self::Ident(id) => id.clone(),
            Self::Lambda(param, body) => {
                let mut retr = format!("\\{} -> {}", param, body.pretty(level + 1));
                if level != 0 {
                    retr.insert(0, '(');
                    retr.push(')');
                }
                retr
            }
            Self::App(fun, arg) => {
                let mut retr = format!("{} {}", fun.pretty(level + 1), arg.pretty(level + 1));
                if level != 0 {
                    retr.insert(0, '(');
                    retr.push(')');
                }
                retr
            }
        }
    }
}

type LexerInputStream<'a> = Peekable<Chars<'a>>;
type TokenStream<'a> = Peekable<std::slice::Iter<'a, Token>>;

fn lex<'a>(input: &mut LexerInputStream) -> Vec<Token> {
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
            _ => todo!(),
        }
    }

    retr
}

fn lex_ident<'a>(input: &mut LexerInputStream, so_far: char) -> Token {
    let mut retr = so_far.to_string();

    while let Some(peeked) = input.next_if(|&p| p.is_ascii_alphanumeric() && !"([])".contains(p)) {
        retr.push(peeked);
    }

    Token::Ident(retr)
}

fn lex_op<'a>(input: &mut LexerInputStream, so_far: char) -> Token {
    let mut retr = so_far.to_string();

    while let Some(peeked) = input
        .next_if(|&p| p.is_ascii_graphic() && !p.is_ascii_alphanumeric() && !"([_])".contains(p))
    {
        retr.push(peeked);
    }

    Token::Op(retr)
}

fn parse_expr(tokens: &mut TokenStream) -> Box<Expr> {
    parse_lambda_expr(tokens)
}

fn parse_lambda_expr(tokens: &mut TokenStream) -> Box<Expr> {
    if let Some(Token::BSlash) = tokens.next_if(|&t| *t == Token::BSlash) {
        let param = match tokens.next() {
            Some(Token::Ident(name)) => name,
            _ => panic!("Expected parameter name after `\\`"),
        };

        match tokens.next() {
            Some(Token::Op(op)) if op == "->" => {
                Box::new(Expr::Lambda(param.clone(), parse_lambda_expr(tokens)))
            }
            _ => panic!("Expected `->`"),
        }
    } else {
        parse_app_expr(tokens)
    }
}

fn parse_app_expr(tokens: &mut TokenStream) -> Box<Expr> {
    let mut expr = parse_atom(tokens);

    while let Some(Token::Ident(_)) | Some(Token::LParen) = tokens.peek() {
        expr = Box::new(Expr::App(expr, parse_atom(tokens)))
    }

    expr
}

fn parse_atom(tokens: &mut TokenStream) -> Box<Expr> {
    match tokens.next() {
        Some(Token::Ident(id)) => Box::new(Expr::Ident(id.clone())),
        Some(Token::LParen) => {
            let retr = parse_expr(tokens);
            match tokens.next() {
                Some(Token::RParen) => retr,
                _ => panic!("Expected `)`"),
            }
        }
        Some(Token::RParen) => panic!("Unmatched `)`"),
        None => panic!("Unexpected EOF"),
        other => panic!("Unexpected token: {:?}", other),
    }
}

fn eval(mut expr: Box<Expr>) -> Box<Expr> {
    let mut taken = HashMap::new();
    while let Some(expr2) = beta_reduce(expr.clone(), &mut taken) {
        expr = expr2;
        println!("{}", expr.pretty(0));
    }
    expr
}

fn beta_reduce(expr: Box<Expr>, taken: &mut HashMap<String, u32>) -> Option<Box<Expr>> {
    match *expr {
        Expr::App(fun, arg) => {
            if let Expr::Lambda(param, body) = *fun {
                let fv_arg = free_vars_of(&arg);

                if fv_arg.contains(&param) {
                    let fresh_param = fresh(&param, taken);
                    let renamed =
                        substitute(body, &param, &Box::new(Expr::Ident(fresh_param.clone())));

                    Some(substitute(renamed, &fresh_param, &arg))
                } else {
                    Some(substitute(body, &param, &arg))
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

fn free_vars(expr: &Expr, out: &mut HashSet<String>) {
    match expr {
        Expr::Ident(name) => {
            out.insert(name.clone());
        }
        Expr::Lambda(param, body) => {
            free_vars(body, out);
            out.remove(param);
        }
        Expr::App(fun, arg) => {
            free_vars(&fun, out);
            free_vars(&arg, out);
        }
    }
}

fn free_vars_of(e: &Expr) -> HashSet<String> {
    let mut set = HashSet::new();
    free_vars(e, &mut set);
    set
}

fn fresh(orig: &str, taken: &mut HashMap<String, u32>) -> String {
    let n = taken.entry(orig.to_string()).or_insert(0);
    let retr = format!("{}{}", orig, n);
    *n += 1;
    retr
}

fn substitute(term: Box<Expr>, var: &str, replacement: &Box<Expr>) -> Box<Expr> {
    match *term {
        Expr::Ident(name) if name == var => replacement.clone(),
        Expr::Ident(_) => term.clone(),
        Expr::Lambda(param, body) if param == var => {
            // shadowed
            Box::new(Expr::Lambda(param.clone(), body.clone()))
        }
        Expr::Lambda(param, body) => {
            Box::new(Expr::Lambda(param, substitute(body, var, replacement)))
        }
        Expr::App(fun, arg) => Box::new(Expr::App(
            substitute(fun, var, replacement),
            substitute(arg, var, replacement),
        )),
    }
}

fn main() {
    loop {
        let mut input = String::new();
        print!("~=:> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();

        let mut stream = input.chars().peekable();
        let tokens = lex(&mut stream);
        // println!("{:?}", tokens);

        let mut token_stream = tokens.iter().peekable();
        let expr = parse_expr(&mut token_stream);
        // println!("{}", expr.pretty(0));

        eval(expr);
    }
}
