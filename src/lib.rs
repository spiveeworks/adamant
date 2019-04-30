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
}

#[derive(Clone)]
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
    Index(Box<Expr>),
    Plus(Box<(Expr, Expr)>),
    IntegerLiteral(u64),
    StructLiteral(HashMap<String, Expr>),
    ArrayLiteral(Vec<Expr>),
    Call(String, Vec<Expr>),
}

pub enum Statement {
    Define(String, Expr),
    Write(Expr, Expr),
    Discard(Expr),
    Return(Expr),
    While(Expr, Vec<Statement>),
}

pub enum Item {
    TypeDef(String, TypeExpr),
    Function(String, Vec<(String, TypeExpr)>, Vec<Statement>),
}

pub fn eval(bindings: &HashMap<String, Data>, expr: &Expr) -> Data {
    match expr {
        Expr::Ident(name) => bindings[name].clone(),
        &Expr::IntegerLiteral(val) => Data::Binary { size: 64, val },
        _ => unimplemented!(),
    }
}

pub fn execute(bindings: &mut HashMap<String, Data>, program: &Vec<Statement>) {
    let mut pc = 0;
    while pc < program.len() {
        match &program[pc] {
            Statement::Define(name, val) => {
                bindings.insert(name.clone(), eval(bindings, val));
            },
            _ => {
                unimplemented!();
            },
        }
        pc += 1;
    }
}

