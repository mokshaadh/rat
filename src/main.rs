use std::{
    collections::{HashMap, LinkedList, VecDeque},
    io::Write,
    rc::Rc,
};

use error::ParseResult;
use lexer::lex;
use parser::{parse_ast, Ast};

mod error;
mod lexer;
mod parser;

type Symbs = HashMap<String, Expr>;

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

            match new_fun {
                Expr::Lambda(body, fun_env) => {
                    let mut new_env_list = (*fun_env).clone();
                    new_env_list.push_front(new_arg.clone());
                    let new_env = Env::new(new_env_list);

                    beta_reduce(&body, &new_env, symbs)
                }
                _ => Expr::App(Box::new(new_fun), Box::new(new_arg)),
            }
        }
        Expr::Lambda(body, _) if !env.is_empty() => Expr::Lambda(body.clone(), env.clone()),
        _ => expr.clone(),
    }
}

fn str_to_expr(input: &str) -> ParseResult<Expr> {
    let mut char_stream = input.chars().enumerate().peekable();
    let tokens = lex(&mut char_stream)?;
    let mut token_stream = tokens.iter().peekable();
    let ast = parse_ast(&mut token_stream)?;

    let mut ctx = LinkedList::new();
    Ok(ast_to_expr(&ast, &mut ctx))
}

fn add_predefined_symb(symbs: &mut Symbs, s: &str, e: &str) {
    symbs.insert(s.to_string(), str_to_expr(e).unwrap());
}

fn main() {
    let mut symbs = Symbs::new();

    // Church Booleans
    add_predefined_symb(&mut symbs, "True", "\\t -> \\f -> t");
    add_predefined_symb(&mut symbs, "False", "\\t -> \\f -> f");

    add_predefined_symb(&mut symbs, "if_then_else", "\\b -> \\x -> \\y -> b x y");
    add_predefined_symb(&mut symbs, "and", "\\b -> \\c -> b c False");
    add_predefined_symb(&mut symbs, "or", "\\b -> \\c -> b True c");
    add_predefined_symb(&mut symbs, "not", "\\x -> x False True");

    // Church Numerals
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

    // add_predefined_symb(&mut symbs, "succ", "\\n -> \\f -> \\x -> f (n f x)");
    add_predefined_symb(&mut symbs, "succ", "\\n -> \\f -> \\x -> n f (f x)");

    // Pairs
    add_predefined_symb(&mut symbs, "pair", "\\f -> \\s -> \\b -> b f s");
    add_predefined_symb(&mut symbs, "fst", "\\p -> p True");
    add_predefined_symb(&mut symbs, "snd", "\\p -> p False");

    loop {
        let mut input = String::new();
        print!("~=:> ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();

        let expr = match str_to_expr(&input) {
            Ok(expr) => expr,
            Err(msg) => {
                println!("Error: {:?}", msg);
                continue;
            }
        };

        let expr = beta_reduce(&expr, &Env::new(EnvList::new()), &symbs);
        println!("{}", expr.pretty());
    }
}
