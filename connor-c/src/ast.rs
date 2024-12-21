use crate::tools::*;
use crate::token::*;

type Input<'a> = Cursor<'a, Token<'a>>;

#[derive(Clone, Debug)]
pub enum Ast<'a> {
    Integer(usize),
    Identifier(&'a str),
    Call {
        f: Box<Ast<'a>>,
        expr: Box<Ast<'a>>,
    },
    Function {
        name: &'a str,
        exprs: Vec<Ast<'a>>,
        ty: Type,
    },
}

impl<'a> Ast<'a> {
    pub fn from_tokens(input: &'a [Token<'a>]) -> Option<Self> {
        let mut cursor = Cursor::from(input);
        Self::function(&mut cursor)
    }

    fn statement(input: &mut Input<'a>) -> Option<Self> {
        let pos = input.save();

        if let Some(ast) = Self::expr(input) && let Some(Token::SemiColon) = input.next() {
            return Some(ast);
        }

        unsafe { input.reset(pos); }
        None
    }

    fn expr(input: &mut Input<'a>) -> Option<Self> {
        let pos = input.save();

        match input.next() {
            Some(Token::Integer(int)) => return Some(Ast::Integer(int)),
            Some(Token::Word(ident)) => {
                let ast = Ast::Identifier(ident);
                if let Some(expr) = Self::expr(input) {
                    let mut f = Ast::Call { f: Box::new(ast), expr: Box::new(expr) };
                    while let Some(expr) = Self::expr(input) {
                        f = Ast::Call { f: Box::new(f), expr: Box::new(expr) };
                    }
                    return Some(f);
                }
                return Some(ast);
            },
            _ => (),
        }

        unsafe { input.reset(pos); }
        None
    }

    fn fblock(input: &mut Input<'a>) -> Option<Vec<Self>> {
        let pos = input.save();
        let mut exprs = vec![];

        if let Some(Token::LBracket) = input.next() {} else {
            unsafe { input.reset(pos); }
            return None;
        }

        while let Some(ast) = Ast::statement(input) {
            exprs.push(ast);
        }

        if let Some(Token::RBracket) = input.next() {
            return Some(exprs);
        }

        unsafe { input.reset(pos); }
        None
    }

    fn function(input: &mut Input<'a>) -> Option<Self> {
        let pos = input.save();

        if
            let Some(Token::Type(ty)) = input.next() &&
            let Some(Token::Word(ident)) = input.next() &&
            let Some(Token::LParen) = input.next() &&
            let Some(Token::RParen) = input.next()
        {
            let block = Self::fblock(input)?;
            return Some(Ast::Function {
                name: ident,
                exprs: block,
                ty: Type::Function {
                    params: vec![],
                    ret: Box::new(ty),
                },
            });
        }

        unsafe { input.reset(pos); }
        None
    }
}
