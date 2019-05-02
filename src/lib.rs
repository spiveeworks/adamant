#[macro_use]
extern crate lalrpop_util;

use std::collections::HashMap;

lalrpop_mod!(parser);

pub use parser::ItemsParser;

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    size: Option<usize>,
    layout: TypeLayout,
}

// might want to pull size out from the box?
// probably won't lol
pub type TypeBox = Box<Type>;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeLayout {
    Binary { size: u8 },
    Struct { fields: Vec<(String, Type)> },
    Array { content: TypeBox, number: usize },
    Pointer { base: TypeBox },  // ideally a subtype of bsize/usize??
    //Subtype { name: String, base: Box<Type> },
    //Backlink { order: u8 },
    Untyped,
}

//type FunId = usize;

pub enum TypeExpr {
    Binary(u8),
    Struct(HashMap<String, TypeExpr>),
    Array(Box<TypeExpr>, Box<Expr>),
    VarArray(Box<TypeExpr>),
    Ptr(Box<TypeExpr>),
    Ident(String),
}

#[derive(Clone, Debug)]
pub enum Data {
    Binary { size: u8, val: u64 }, // size? probably not necessary now that bindings are typed
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
    pub bindings: HashMap<String, (Type, Data)>,
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

fn struct_type(field_tys: Vec<(String, Type)>) -> Type {
    let mut size = Some(0);
    for (name, ty) in &field_tys {
        size = size.and_then(|x|ty.size.map(|y| x + y));
    }
    // padding? alignment?
    Type { size, layout: TypeLayout::Struct { fields: field_tys }}
}

fn array_type(ty: Type, n: usize) -> Type {
    let size = ty.size.map(|x| x * n);
    let layout = TypeLayout::Array{
        content: Box::new(ty),
        number: n as usize,
    };
    Type { size, layout }
}

fn pointer_type(base: Type) -> Type {
    Type {
        size: Some(8),
        layout: TypeLayout::Pointer {
            base: Box::new(base),
        },
    }
}

// what if ty is by reference (in the rust sense... ty: &Type)
fn deref_type(ty: Type) -> Option<Type> {
    if let TypeLayout::Pointer { base } = ty.layout {
        Some(*base)
    } else {
        None
    }
}

fn eval_type(ctx: &mut Context, ty: &TypeExpr) -> Type {
    match ty {
        &TypeExpr::Binary(size) => Type {
            size: Some(8),
            layout: TypeLayout::Binary { size }
        },
        TypeExpr::Struct(fields) => struct_type(
            fields
                .into_iter()
                .map(|(name, field)| (
                    name.clone(),
                    eval_type(ctx, field)
                ))
                .collect()
        ),
        TypeExpr::Array(ty, n) => {
            let ty = eval_type(ctx, ty);
            let n = eval(ctx, n).1.get_num().expect("Array size must be scalar");
            array_type(ty, n as usize)
        },
        TypeExpr::VarArray(ty) => {
            let ty = eval_type(ctx, ty);
            unimplemented!();
        },
        TypeExpr::Ptr(ty) => {
            let ty = eval_type(ctx, ty);
            Type { size: Some(8), layout: TypeLayout::Pointer { base: Box::new(ty) } }
        },
        TypeExpr::Ident(_) => unimplemented!(),
    }
}

fn fresh(ty: &Type) -> Data {
    return Data::Binary { size: 0, val: 0 };
    match ty.layout {
        TypeLayout::Binary { .. } => unimplemented!(),
        TypeLayout::Struct { .. } => unimplemented!(),
        TypeLayout::Array { .. } => unimplemented!(),
        // TypeLayout::VarArray(_) => unimplemented!(),
        TypeLayout::Pointer { .. } => unimplemented!(),
        TypeLayout::Untyped => unimplemented!(),
    }
}

fn lookup<'a, K: PartialEq, V>(data: &'a mut Vec<(K, V)>, key: &K) -> Option<&'a mut V> {
    for (k, v) in data {
        if k == key {
            return Some(v);
        }
    }
    None
}

fn eval_num(val: u64) -> (Type, Data) {
    (
        Type { size: Some(8), layout: TypeLayout::Binary { size: 64 } },
        Data::Binary { size: 64, val },
    )
}

fn eval(ctx: &mut Context, expr: &Expr) -> (Type, Data) {
    match expr {
        Expr::AutoAlloc(ty) => {
            let base_ty = eval_type(ctx, ty);
            let ty = pointer_type(base_ty);

            let val = ctx.stack.len() as u64;
            ctx.stack.push(fresh(&ty));

            (ty, Data::Binary { size: 64, val })
        },
        Expr::GenAlloc(ty) => {
            let base_ty = eval_type(ctx, ty);
            let ty = pointer_type(base_ty);

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

            (ty, Data::Binary { size: 64, val })
        },
        Expr::Deref(val) => {
            let (ty, val) = eval(ctx, val);
            let ty = deref_type(ty).expect("Tried to dereference non-scalar");
            let index = val.get_num()
                .expect("Pointer binding was non-scalar?");
            (ty, ctx.get_mem(index as usize).clone())
        },
        Expr::Ident(name) => ctx.bindings[name].clone(),
        Expr::Field(data, field) => {
            let (data_ty, data) = eval(ctx, data);
            let field_ty = {
                if let TypeLayout::Struct { fields: mut field_tys } = data_ty.layout {
                    lookup(&mut field_tys, field)
                        .expect("Struct does not contain this field")
                        .clone()
                } else {
                    panic!("Tried to access field of non-struct");
                }
            };
            if let Data::Struct(mut vals) = data {
                (field_ty, vals.remove(field).expect("Field does not exist"))
            } else {
                panic!("Struct binding wasn't a struct?");
            }
        },
        Expr::Index(data_and_index) => {
            let (data, index) = &**data_and_index;
            let (ty, data) = eval(ctx, data);
            let (ty, number) = if let TypeLayout::Array { content, number } = ty.layout {
                (*content, number)
            } else {
                panic!("Tried to index non-array");
            };
            if let Data::Array(mut data) = data {
                let (_, index) = eval(ctx, index);
                // check that we arent indexing with a pointer?
                let index = index.get_num()
                    .expect("Tried to index by non-scalar");
                (ty, data.remove(index as usize))
            } else {
                panic!("Array binding wasn't an array?");
            }
        },
        Expr::Plus(left_and_right) => {
            let (left, right) = &**left_and_right;
            let left = eval(ctx, left).1
                .get_num()
                .expect("Tried to add non-number");
            let right = eval(ctx, right).1
                .get_num()
                .expect("Tried to add non-number");
            eval_num(left + right)
        },
        &Expr::IntegerLiteral(val) => eval_num(val),
        Expr::StructLiteral(fields) => {
            let mut tys = Vec::new();
            let mut vals = HashMap::new();
            for (name, val) in fields {
                let (ty, val) = eval(ctx, val);
                tys.push((name.clone(), ty));
                vals.insert(name.clone(), val);
            }
            (struct_type(tys), Data::Struct(vals))
        },
        Expr::ArrayLiteral(elems) => {
            let size = elems.len();
            let mut first_ty = None;
            let mut vals = Vec::with_capacity(size);

            for val in elems {
                let (ty, val) = eval(ctx, val);
                if first_ty.is_none() {
                    first_ty = Some(ty);
                } else if first_ty != Some(ty) {
                    first_ty = Some(Type { size: None, layout: TypeLayout::Untyped } );
                }
                vals.push(val);
            }

            let ty = first_ty.unwrap_or(Type { size: Some(0), layout: TypeLayout::Binary { size: 0 } });
            (array_type(ty, size), Data::Array(vals))
        },
        Expr::Call(_fun, _args) => {
            unimplemented!();
        },
    }
}

pub fn execute(ctx: &mut Context, program: &Vec<Statement>) -> (Type, Data) {
    let mut pc = 0;
    while pc < program.len() {
        match &program[pc] {
            Statement::Define(name, val) => {
                let val = eval(ctx, val);
                ctx.bindings.insert(name.clone(), val);
            },
            Statement::Write(lhs, rhs) => {
                let (lhs_type, lhs) = eval(ctx, lhs);
                let index = lhs.get_num()
                    .expect("Tried to write to nonscalar value");
                let (rhs_type, rhs) = eval(ctx, rhs);
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
    (Type { size: Some(0), layout: TypeLayout::Binary { size: 0 }}, Data::Binary { size: 0, val: 0 })
}

