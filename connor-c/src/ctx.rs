use std::collections::HashMap;
use crate::token::Type;
use crate::ast::Ast;

#[derive(Debug)]
pub struct Context<'ctx> {
    types: HashMap<&'ctx str, Type>,
}

impl<'ctx> Context<'ctx> {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: &'ctx str, value: Type) -> Option<Type> {
        self.types.insert(key, value)
    }

    pub fn check(&mut self, expr: Ast<'ctx>) -> Type {
        match expr {
            Ast::Integer(_) => {
                Type::CompInt
            },
            Ast::Identifier(ident) => {
                self.types.get(ident).unwrap_or_else(|| panic!("Undefined identifier")).clone()
            },
            Ast::Call { f, expr } => {
                let ty = self.check(*f);
                let Type::Function { ref params, ref ret } = ty else { panic!("Function call on non-function") };
                let expr_ty = self.check(*expr);
                if !expr_ty.coercible(&Type::Tuple(params.to_owned())) { panic!("Incompatible types found"); };
                *ret.clone()
            },
            Ast::Function { name, exprs, ty } => {
                self.types.insert(name, ty.clone());
                for expr in exprs {
                    self.check(expr);
                }
                ty
            },
        }
    }
}
