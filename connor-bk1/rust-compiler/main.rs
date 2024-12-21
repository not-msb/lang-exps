#![feature(let_chains)]
#![feature(box_patterns)]
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Type {
    Void,
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
    },
    NoReturn,
    U32,
    U8,
}

impl Type {
    fn size(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Function { .. } => 4,
            Type::NoReturn => 0,
            Type::U32 => 4,
            Type::U8 => 1,
        }
    }

    fn reg_size(&self) -> Option<&'static str> {
        match self.size() {
            1 => Some("byte"),
            2 => Some("word"),
            4 => Some("dword"),
            _ => None,
        }
    }
}

fn register_map(reg: usize, ty: Type) -> &'static str {
    const MAP: [[&'static str; 3]; 7] = [
        ["al", "ax", "eax"],
        ["bl", "bx", "ebx"],
        ["cl", "cx", "ecx"],
        ["dl", "dx", "edx"],
        ["dh", "si", "esi"],
        ["bh", "di", "edi"],
        ["ch", "bp", "ebp"],
    ];

    let index = match ty.size() {
        1 => 0,
        2 => 1,
        4 => 2,
        _ => unreachable!(),
    };

    MAP[reg][index]
}

#[derive(Debug, Clone)]
enum Op<'op> {
    Register {
        ty: Type,
        reg: usize,
    },
    Integer {
        ty: Type,
        int: usize,
    },
    Identifier {
        ty: Type,
        ident: &'op str,
    },
    Lambda {
        exprs: Vec<Op<'op>>,
    },
    Call {
        name: &'op str,
        saved: Vec<usize>,
        arg: Box<Op<'op>>,
    },
    Return {
        expr: Box<Op<'op>>
    },
    Assign {
        dst: Box<Op<'op>>,
        src: Box<Op<'op>>,
    },
}

#[derive(Debug, Clone)]
struct Symbol {
    ty: Type,
}

#[derive(Debug, Clone)]
struct Context<'ctx> {
    parameters: HashMap<&'ctx str, Symbol>,
    symbols: HashMap<&'ctx str, Symbol>,
    registers: HashMap<usize, Option<&'ctx str>>,
    allocated: usize,
}

impl<'ctx> Context<'ctx> {
    fn new() -> Self {
        Self {
            parameters: HashMap::new(),
            symbols: HashMap::new(),
            registers: HashMap::new(),
            allocated: 0,
        }
    }

    fn get(&self, key: &str) -> Option<&Symbol> {
        if let Some(s) = self.parameters.get(key) {
            Some(s)
        } else if let Some(s) = self.symbols.get(key) {
            Some(s)
        } else {
            None
        }
    }

    fn alloc_reg(&mut self, name: Option<&'ctx str>) -> usize {
        let mut keys: Vec<usize> = self.registers.clone().into_keys().collect();
        keys.sort();

        let reg = keys
            .windows(2)
            .find_map(|arr| {
                let x = arr[0]+1;
                if x != arr[1] { Some(x) } else { None }
            })
            .unwrap_or(keys.len());

        self.registers.insert(reg, name);
        reg
    }

    fn insert(&mut self, key: &'ctx str, symbol: Symbol) {
        self.symbols.insert(key, symbol);
    }

    fn compile(&mut self, op: &mut Op<'ctx>) {
        match op {
            Op::Integer { ty, int } => {
                let reg = self.alloc_reg(None);
                println!("mov {}, {}", register_map(reg, ty.clone()), int);
                *op = Op::Register { ty: ty.clone(), reg };
            },
            Op::Call { name, saved, arg: box Op::Integer { ty, int } } => {
                println!("; Call {}", name);

                let Type::Function { params, ret } = &self.get(name).unwrap().ty else { unreachable!() };
                let param_size: usize = params.iter().map(|param| param.size()).sum();
                let stack_size = self.allocated + ret.size() + param_size;

                let offset = self.allocated + ret.size();
                let reserved = offset + 4 * saved.len();

                println!("mov {} [esp-{}], {}", ty.reg_size().unwrap(), reserved+ty.size(), int);
                println!("sub esp, {}", offset);
                for register in saved.iter() {
                    println!("push {}", register_map(*register, Type::U32));
                }
                println!("sub esp, {}", ty.size());
                println!("call {}", name);
                println!("add esp, {}", ty.size());
                for register in saved.iter().rev() {
                    println!("pop {}", register_map(*register, Type::U32));
                }
                println!("add esp, {}", offset);

                println!("; End Call {}", name);
            },
            Op::Return { expr: box Op::Register { ty, reg } } => {
                //println!("; Return");
                let reg_size = ty.reg_size().unwrap();
                println!("mov {} [esp+4], {}", reg_size, register_map(*reg, ty.clone()));
                println!("ret");
            },
            Op::Return { expr } => {
                //println!("; Return - Rewrite");
                self.compile(expr);
                self.compile(op);
            },
            Op::Assign {
                dst: box Op::Identifier { ty: _, ident },
                src: box Op::Register { ty, reg },
            } => {
                let r = self.alloc_reg(Some(ident));

                //println!("; Assign - Ident && Int");
                println!("mov {}, {}", register_map(*reg, ty.clone()), register_map(r, ty.clone()));

                self.insert(ident, Symbol { ty: ty.clone() });
                *op = Op::Register { ty: ty.clone(), reg: *reg };
            },
            Op::Assign {
                dst: box Op::Identifier { ty, ident },
                src: box Op::Lambda { exprs },
            } => {
                //println!("; Assign - Ident && Lambda");
                println!("{}:", ident);
                for expr in exprs {
                    self.compile(expr);
                }

                self.insert(ident, Symbol { ty: ty.clone() });
                *op = Op::Identifier { ty: ty.clone(), ident };
            },
            Op::Assign {
                dst: box Op::Identifier { .. },
                src,
            } => {
                //println!("; Assign - Ident - Rewrite");
                self.compile(src);
                self.compile(op);
            },
            _ => todo!(),
        }
    }
}

fn main() {
    let mut ctx = Context::new();
    ctx.insert("@debug", Symbol {
        ty: Type::Function {
            params: vec![
                Type::U8,
            ],
            ret: Box::new(Type::Void),
        },
    });

    let mut op = Op::Assign {
        dst: Box::new(Op::Identifier {
            ty: Type::Function {
                params: vec![],
                ret: Box::new(Type::U8),
            },
            ident: "main",
        }),
        src: Box::new(Op::Lambda {
            exprs: vec![
                Op::Call {
                    name: "@debug",
                    saved: vec![0, 1, 2, 3],
                    arg: Box::new(Op::Integer {
                        ty: Type::U8,
                        int: 65,
                    }),
                },
                Op::Return {
                    expr: Box::new(Op::Integer {
                        ty: Type::U8,
                        int: 0,
                    }),
                }
            ],
        }),
    };

    //let mut op = Op::Assign {
    //    dst: Box::new(Op::Identifier { ty: Type::U8, ident: "y" }),
    //    src: Box::new(Op::Assign {
    //        dst: Box::new(Op::Identifier { ty: Type::U8, ident: "x" }),
    //        src: Box::new(Op::Integer { ty: Type::U8, int: 0 }),
    //    }),
    //};

    ctx.compile(&mut op);
    println!("\n{ctx:#?}");
}
