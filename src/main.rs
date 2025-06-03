use std::{
    collections::{HashMap, LinkedList, VecDeque},
    io::Write,
    iter::Peekable,
    rc::Rc,
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

#[derive(Debug)]
enum Ast {
    Ident(String),
    Lambda(String, Box<Ast>),
    App(Box<Ast>, Box<Ast>),
}

type Env = Rc<EnvList>;
type EnvList = VecDeque<Expr>;

#[derive(Debug, Clone)]
enum Expr {
    Var(usize),
    Free(String),
    Lambda(Box<Expr>, Env),
    App(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn pretty(&self) -> String {
        match self {
            Self::Var(n) => format!("${}", n),
            Self::Free(name) => format!("<{}>", name.clone()),
            Self::App(fun, arg) => format!("({} {})", fun.pretty(), arg.pretty()),
            Self::Lambda(body, env) if env.is_empty() => format!("(\\ {})", body.pretty()),
            Self::Lambda(body, env) => {
                let mut retr = format!("(\\ {} | [", body.pretty());

                for (i, expr) in env.iter().enumerate() {
                    retr = format!("{}{}", retr, expr.pretty());
                    if i < env.len() - 1 {
                        retr += ", "
                    }
                }

                retr + "])"
            }
        }
    }
}

type LexerInputStream<'a> = Peekable<Chars<'a>>;
type TokenStream<'a> = Peekable<std::slice::Iter<'a, Token>>;
type ParseResult<T> = Result<T, String>;
type Symbs = HashMap<String, Expr>;

fn lex<'a>(input: &mut LexerInputStream) -> ParseResult<Vec<Token>> {
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

fn parse_ast(tokens: &mut TokenStream) -> ParseResult<Ast> {
    parse_lambda_ast(tokens)
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

fn parse_lambda_ast(tokens: &mut TokenStream) -> ParseResult<Ast> {
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
                    Ok(params
                        .into_iter()
                        .rfold(parse_lambda_ast(tokens)?, |acc, p| {
                            Ast::Lambda(p, Box::new(acc))
                        }))
                },
            )
    } else {
        parse_app_ast(tokens)
    }
}

fn parse_app_ast(tokens: &mut TokenStream) -> ParseResult<Ast> {
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

fn ast_to_expr(ast: &Ast, ctx: &mut LinkedList<String>) -> Expr {
    match ast {
        Ast::Ident(id) => ctx
            .iter()
            .rev()
            .position(|param| param == id)
            .map_or_else(|| Expr::Free(id.clone()), |dist| Expr::Var(dist)),
        Ast::App(fun, arg) => Expr::App(
            Box::new(ast_to_expr(fun, ctx)),
            Box::new(ast_to_expr(arg, ctx)),
        ),
        Ast::Lambda(param, body) => {
            ctx.push_back(param.clone());
            let retr = Expr::Lambda(Box::new(ast_to_expr(body, ctx)), Env::new(EnvList::new()));
            ctx.pop_back();
            retr
        }
    }
}

fn beta_reduce(expr: &Expr, env: &Env, symbs: &Symbs) -> Expr {
    match expr {
        Expr::Var(n) => env[*n].clone(),
        Expr::Free(name) => symbs.get(name).unwrap_or(expr).clone(),
        Expr::App(fun, arg) => {
            let new_fun = beta_reduce(fun, env, symbs);
            let new_arg = beta_reduce(arg, env, symbs);

            if let Expr::Lambda(body, fun_env) = new_fun {
                let mut new_env_list = (*fun_env).clone();
                new_env_list.push_front(new_arg.clone());
                let new_env = Env::new(new_env_list);

                beta_reduce(&body, &new_env, symbs)
            } else {
                Expr::App(Box::new(new_fun), Box::new(new_arg))
            }
        }
        Expr::Lambda(body, _) if !env.is_empty() => Expr::Lambda(body.clone(), env.clone()),
        _ => expr.clone(),
    }
}

fn main() {
    loop {
        let mut input = String::new();
        print!("~=:> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();

        let mut stream = input.chars().peekable();
        let tokens = match lex(&mut stream) {
            Ok(tokens) => tokens,
            Err(msg) => {
                println!("Parse Error: {}", msg);
                continue;
            }
        };

        let mut token_stream = tokens.iter().peekable();
        let ast = match parse_ast(&mut token_stream) {
            Ok(ast) => ast,
            Err(msg) => {
                println!("Parse Error: {}", msg);
                continue;
            }
        };

        let mut ctx = LinkedList::new();
        let expr = ast_to_expr(&ast, &mut ctx);

        let mut symbs = Symbs::new();
        for i in 0..11 {
            let mut base = Box::new(Expr::Var(0));
            for _ in 0..i {
                base = Box::new(Expr::App(Box::new(Expr::Var(1)), base));
            }
            symbs.insert(
                i.to_string(),
                Expr::Lambda(
                    Box::new(Expr::Lambda(base, Env::new(EnvList::new()))),
                    Env::new(EnvList::new()),
                ),
            );
        }

        let expr = beta_reduce(&expr, &Env::new(EnvList::new()), &symbs);
        println!("{}", expr.pretty());
    }
}
