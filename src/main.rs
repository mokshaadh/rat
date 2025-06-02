use std::{collections::LinkedList, io::Write, iter::Peekable, str::Chars};

#[derive(Debug, PartialEq)]
enum Token {
    Ident(String),
    Op(String),
    LParen,
    RParen,
    BSlash,
}

#[derive(Debug)]
enum Ast {
    Ident(String),
    Lambda(String, Box<Ast>),
    App(Box<Ast>, Box<Ast>),
}

#[derive(Debug, Clone)]
enum Expr {
    Var(usize),
    Free(String),
    Lambda(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn pretty(&self, top_level: bool) -> String {
        match self {
            Self::Var(n) => return n.to_string(),
            Self::Free(name) => return name.clone(),
            Self::App(fun, arg) if top_level => {
                format!("{} {}", fun.pretty(false), arg.pretty(false))
            }
            Self::App(fun, arg) => format!("({} {})", fun.pretty(false), arg.pretty(false)),
            Self::Lambda(body) if top_level => format!("\\ {}", body.pretty(false)),
            Self::Lambda(body) => format!("(\\ {})", body.pretty(false)),
        }
    }
}

type LexerInputStream<'a> = Peekable<Chars<'a>>;
type TokenStream<'a> = Peekable<std::slice::Iter<'a, Token>>;
type EnvList = LinkedList<String>;

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

    while let Some(peeked) = input.next_if(|&p| p.is_ascii_alphanumeric() && !"([\\])".contains(p))
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

fn parse_ast(tokens: &mut TokenStream) -> Ast {
    parse_lambda_ast(tokens)
}

fn parse_params(tokens: &mut TokenStream) -> Vec<String> {
    let mut retr = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::Ident(id)) => {
                if retr.contains(id) {
                    panic!("Conflicting definitions for `{}`", id)
                }
                retr.push(id.clone());
            }
            _ => break,
        }
        tokens.next();
    }

    retr
}

fn parse_lambda_ast(tokens: &mut TokenStream) -> Ast {
    if let Some(_) = tokens.next_if(|&t| *t == Token::BSlash) {
        let params = parse_params(tokens);

        if params.len() == 0 {
            panic!("Expected at least one parameter in lambda expression");
        }

        match tokens.next() {
            Some(Token::Op(op)) if op == "->" => {}
            _ => panic!("Expected `->`"),
        }

        let body = parse_lambda_ast(tokens);

        params
            .into_iter()
            .rfold(body, |acc, p| Ast::Lambda(p, Box::new(acc)))
    } else {
        parse_app_ast(tokens)
    }
}

fn parse_app_ast(tokens: &mut TokenStream) -> Ast {
    let mut ast = parse_atom(tokens);

    while let Some(Token::Ident(_)) | Some(Token::LParen) = tokens.peek() {
        ast = Ast::App(Box::new(ast), Box::new(parse_atom(tokens)))
    }

    ast
}

fn parse_atom(tokens: &mut TokenStream) -> Ast {
    match tokens.next() {
        Some(Token::Ident(id)) => Ast::Ident(id.clone()),
        Some(Token::LParen) => {
            let retr = parse_ast(tokens);
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

fn ast_to_expr(ast: &Ast, env: &mut EnvList) -> Expr {
    match ast {
        Ast::Ident(id) => env
            .iter()
            .rev()
            .position(|param| param == id)
            .map_or_else(|| Expr::Free(id.clone()), |dist| Expr::Var(dist)),
        Ast::App(fun, arg) => Expr::App(
            Box::new(ast_to_expr(fun, env)),
            Box::new(ast_to_expr(arg, env)),
        ),
        Ast::Lambda(param, body) => {
            env.push_back(param.clone());
            let retr = Expr::Lambda(Box::new(ast_to_expr(body, env)));
            env.pop_back();
            retr
        }
    }
}

fn eval(expr: &Expr) -> Vec<Expr> {
    let mut tmp = expr.clone();
    let mut retr = vec![tmp.clone()];

    while let Some(expr2) = beta_reduce(&tmp) {
        tmp = expr2;
        retr.push(tmp.clone());
    }

    retr
}

fn is_value(expr: &Expr) -> bool {
    matches!(expr, Expr::Lambda(_) | Expr::Var(_) | Expr::Free(_))
}

fn beta_reduce(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::App(fun, arg) if !is_value(fun) => {
            beta_reduce(fun).map(|fun2| Expr::App(Box::new(fun2), arg.clone()))
        }
        Expr::App(fun, arg) if !is_value(arg) => {
            beta_reduce(arg).map(|arg2| Expr::App(fun.clone(), Box::new(arg2)))
        }
        Expr::App(fun, arg) => {
            if let Expr::Lambda(body) = fun.as_ref() {
                Some(substitute_top(body, arg))
            } else {
                None
            }
        }
        Expr::Lambda(body) => beta_reduce(body).map(|body2| Expr::Lambda(Box::new(body2))),
        _ => None,
    }
}

fn shift(expr: &Expr, amount: isize) -> Expr {
    fn walk(expr: &Expr, amount: isize, cut: usize) -> Expr {
        match expr {
            Expr::Var(n) if *n >= cut => Expr::Var((*n as isize + amount) as usize),
            Expr::Var(_) | Expr::Free(_) => expr.clone(),
            Expr::App(fun, arg) => Expr::App(
                Box::new(walk(fun, amount, cut)),
                Box::new(walk(arg, amount, cut)),
            ),
            Expr::Lambda(body) => Expr::Lambda(Box::new(walk(body, amount, cut + 1))),
        }
    }
    walk(expr, amount, 0)
}

fn substitute(expr: &Expr, replacement: &Expr, idx: usize) -> Expr {
    fn walk(expr: &Expr, replacement: &Expr, idx: usize, c: usize) -> Expr {
        match expr {
            Expr::Var(n) if *n == idx + c => shift(replacement, c as isize),
            Expr::Var(_) | Expr::Free(_) => expr.clone(),
            Expr::App(fun, arg) => Expr::App(
                Box::new(walk(fun, replacement, idx, c)),
                Box::new(walk(arg, replacement, idx, c)),
            ),
            Expr::Lambda(body) => Expr::Lambda(Box::new(walk(body, replacement, idx, c + 1))),
        }
    }
    walk(expr, replacement, idx, 0)
}

fn substitute_top(expr: &Expr, replacement: &Expr) -> Expr {
    shift(&substitute(expr, &shift(replacement, 1), 0), -1)
}

fn main() {
    loop {
        let mut input = String::new();
        print!("~=:> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();

        let mut stream = input.chars().peekable();
        let tokens = lex(&mut stream);

        let mut token_stream = tokens.iter().peekable();
        let ast = parse_ast(&mut token_stream);

        let mut env = EnvList::new();
        let expr = ast_to_expr(&ast, &mut env);
        let steps = eval(&expr);

        for step in steps {
            println!("<=> {}", step.pretty(true));
        }
    }
}
