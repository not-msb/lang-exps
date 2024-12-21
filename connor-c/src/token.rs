use logos::Logos;
use crate::tools::*;
use crate::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
    },
    Tuple(Vec<Type>),
    NoReturn,
    CompInt,
    U8,
    U32,
}

impl Type {
    fn eql(&self, rhs: &Type) -> bool {
        match (self, rhs) {
            (Type::Function { params: l_params, ret: l_ret }, Type::Function { params: r_params, ret: r_ret })
                => l_ret.eql(r_ret) && Type::Tuple(l_params.to_owned()).eql(&Type::Tuple(r_params.to_owned())),
            (Type::Tuple(a), Type::Tuple(b)) => {
                let len = if a.len() == b.len() { a.len() } else { return false };
                for i in 0..len {
                    if !(a[i].eql(b[i])) { return false; }
                }
                true
            },
            (Type::CompInt, Type::CompInt) |
            (Type::U8, Type::U8) |
            (Type::U32, Type::U32)
                => true,
            _ => false,
        }
    }

    pub fn coercible(&self, rhs: &Type) -> bool {
        match (self, rhs) {
            (l, r) if l.eql(r) => true,
            (Type::NoReturn, _) => true,
            (ty, Type::Tuple(v)) if v.len() == 1 && ty.coercible(&v[0]) => true,
            (Type::CompInt, Type::U8) |
            (Type::CompInt, Type::U32)
                => true,
            _ => false,
        }
    }
}

impl FromStr for Type {
    type Err = LexError;

    fn from_str(src: &str) -> Result<Self, Self::Err> {
        match src {
            "u8" => Ok(Type::U8),
            "u32" => Ok(Type::U32),
            _ => unreachable!("The input shouldn't've been incorrect"),
        }
    }
}

#[derive(Logos, Clone, Debug)]
#[logos(error = LexError)]
#[logos(skip r"\s+")]
pub enum Token<'t> {
    #[regex(r"(?:(u8)|(u32))", |lex| lex.slice().parse())]
    Type(Type),
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBracket,
    #[token("}")]
    RBracket,
    #[token(";")]
    SemiColon,
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    Integer(usize),
    //#[regex(r"([a-zA-Z][a-zA-Z0-9]*)|\+")]
    #[regex(r"[a-zA-Z][a-zA-Z0-9]*")]
    Word(&'t str),
}
