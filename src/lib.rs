#[macro_use]
extern crate lalrpop_util;

use std::collections::HashMap;

lalrpop_mod!(parser);

pub use parser::ItemsParser;

/*
enum Type {
    Binary { size: u8 },
    Struct { fields: Vec<(String, Type)> },
    Array { content: Box<Type>, number: usize },
    Subtype { name: String, base: Box<Type> },
    Backlink { order: u8 },
}
*/

//type FunId = usize;

pub enum TypeExpr {
    Bitset(u8),
    Struct(HashMap<String, TypeExpr>),
    Array(Box<TypeExpr>, Box<Expr>),
    VarArray(Box<TypeExpr>),
    Ptr(Box<TypeExpr>),
    Ident(String),
}

#[derive(Clone, Debug)]
pub enum Data {
    Binary { size: u8, val: u64 },
    Array(Vec<Data>),
    Struct(HashMap<String, Data>),
    // Function { data: Box<Data>, body: FunId, },
}

pub enum Expr {
    AutoAlloc(TypeExpr),
    GenAlloc(TypeExpr),
    // this is why temp variables are important
    // (or RPN >:) )
    Deref(Box<Expr>),
    Ident(String),
    Field(Box<Expr>, String),
    Index(Box<(Expr, Expr)>),
    Plus(Box<(Expr, Expr)>),
    IntegerLiteral(u64),
    StructLiteral(HashMap<String, Expr>),
    ArrayLiteral(Vec<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}

pub enum Statement {
    Define(String, Expr),
    Write(Expr, Expr),
    // at some point this should only do function calls, I think
    Discard(Expr),
    Return(Expr),
    While(Expr, Vec<Statement>),
}

pub enum Item {
    TypeDef(String, TypeExpr),
    Function(String, Vec<(String, TypeExpr)>, Vec<Statement>),
}

#[derive(Clone)]
pub struct Context {
    pub bindings: HashMap<String, Data>,
    pub stack: Vec<Data>,
    pub heap: Vec<Option<Data>>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            bindings: HashMap::new(),
            stack: Vec::new(),
            heap: Vec::new(),
        }
    }
}

const STACK_SIZE: usize = 1024;

/*
fn size_of(ctx: &mut Context, ty: &TypeExpr) -> Data {
    match ty {
        _ => unimplemented!(),
    }
}
*/

impl Data {
    fn get_num(self: &Self) -> Option<u64> {
        if let &Data::Binary { size: _, val } = self {
            Some(val)
        } else {
            None
        }
    }
}

impl Context {
    fn get_mem(self: &mut Self, index: usize) -> &mut Data {
        if index < self.stack.len() {
            &mut self.stack[index]
        } else if index < STACK_SIZE {
            panic!("Invalid Stack Access: {}", index);
        } else {
            let index = index - STACK_SIZE;
            if index > self.heap.len() || self.heap[index].is_none() {
                panic!("Invalid Heap Access: {}", index);
            } else {
                self.heap[index]
                    .as_mut()
                    .unwrap()
            }
        }
    }
}

fn eval(ctx: &mut Context, expr: &Expr) -> Data {
    match expr {
        Expr::AutoAlloc(_) => {
            let val = ctx.stack.len() as u64;
            ctx.stack.push(Data::Binary { size: 0, val: 0 });
            Data::Binary { size: 64, val }
        },
        Expr::GenAlloc(_) => {
            let mut index = 0;
            while index < ctx.heap.len() && ctx.heap[index].is_some() {
                index += 1;
            }
            let content = Some(Data::Binary { size: 0, val: 0 });
            if ctx.heap.len() < index {
                ctx.heap[index] = content;
            } else {
                ctx.heap.push(content);
            }
            let val = (index + STACK_SIZE) as u64;
            Data::Binary { size: 64, val }
        },
        Expr::Deref(val) => {
            let val = eval(ctx, val);
            let index = val.get_num()
                .expect("Tried to dereference nonscalar");
            ctx.get_mem(index as usize).clone()
        },
        Expr::Ident(name) => ctx.bindings[name].clone(),
        Expr::Field(data, field) => {
            let data = eval(ctx, data);
            if let Data::Struct(mut vals) = data {
                vals.remove(field).expect("Field does not exist")
            } else {
                panic!("Tried to take field of non-struct");
            }
        },
        Expr::Index(data_and_index) => {
            let (data, index) = &**data_and_index;
            let data = eval(ctx, data);
            if let Data::Array(mut data) = data {
                let index = eval(ctx, index);
                let index = index.get_num()
                    .expect("Tried to index by non-scalar");
                data.remove(index as usize)
            } else {
                panic!("Tried to index into non-array");
            }
        },
        Expr::Plus(left_and_right) => {
            let (left, right) = &**left_and_right;
            let left = eval(ctx, left)
                .get_num()
                .expect("Tried to add non-number");
            let right = eval(ctx, right)
                .get_num()
                .expect("Tried to add non-number");
            Data::Binary { size: 64, val: left + right }
        },
        &Expr::IntegerLiteral(val) => Data::Binary { size: 64, val },
        Expr::StructLiteral(fields) => {
            Data::Struct(
                fields
                    .iter()
                    .map(|(name, val)| (
                        name.clone(),
                        eval(ctx, val)
                    ))
                    .collect()
            )
        },
        Expr::ArrayLiteral(elems) => {
            Data::Array(
                elems
                    .iter()
                    .map(|val|eval(ctx, val))
                    .collect()
            )
        },
        Expr::Call(_fun, _args) => {
            unimplemented!();
        },
    }
}

pub fn execute(ctx: &mut Context, program: &Vec<Statement>) -> Data {
    let mut pc = 0;
    while pc < program.len() {
        match &program[pc] {
            Statement::Define(name, val) => {
                let val = eval(ctx, val);
                ctx.bindings.insert(name.clone(), val);
            },
            Statement::Write(lhs, rhs) => {
                let lhs = eval(ctx, lhs);
                let index = lhs.get_num()
                    .expect("Tried to write to nonscalar value");
                let rhs = eval(ctx, rhs);
                *ctx.get_mem(index as usize) = rhs;
            },
            Statement::Discard(expr) => {
                // how literal lol
                eval(ctx, expr);
            },
            Statement::Return(val) => return eval(ctx, val),
            Statement::While(_, _) => {
                unimplemented!();
            },
        }
        pc += 1;
    }
    Data::Binary { size: 0, val: 0 }
}

