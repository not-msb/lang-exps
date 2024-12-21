#![feature(let_chains)]
pub mod tools;
pub mod token;
pub mod ast;
mod ctx;
mod pre;

use std::{
    fs::File,
    io::Read,
    str::FromStr,
    error::Error,
};
use token::*;
use ast::*;
use ctx::*;
use pre::*;
use logos::Logos;

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    let mut file = File::open("main.con")?;
    _ = file.read_to_string(&mut input)?;
    input = PreProcess::strip(&input);

    let lex = Token::lexer(&input);
    let tokens: Vec<Token> = lex
        .into_iter()
        .map(|res| res.unwrap_or_else(|_| panic!("[Error]: Couldn't tokenize")))
        .collect();

    for token in &tokens {
        println!("Token: {:?}", token);
    }

    let ast = Ast::from_tokens(&tokens).unwrap();
    println!("Ast: {:#?}", ast);

    let mut context = Context::new();
    context.insert("add", Type::Function {
        params: vec![Type::U8],
        ret: Box::new(Type::Function {
            params: vec![Type::U8],
            ret: Box::new(Type::U8),
        }),
    });
    context.insert("return", Type::Function {
        params: vec![Type::U8],
        ret: Box::new(Type::NoReturn),
    });
    let _ = context.check(ast);
    println!("Ctx: {:#?}", context);

    Ok(())
}
