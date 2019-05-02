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
    // make this bottom heap? that is already how we adress it at the moment
    pub stack: Vec<u8>,
    pub heap: Vec<Vec<u8>>,
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
    fn get_mem(self: &mut Self, index: usize) -> &mut [u8] {
        if index < self.stack.len() {
            self.stack.split_at_mut(index).1
        } else if index < STACK_SIZE {
            panic!("Invalid Stack Access: {}", index);
        } else {
            let index = index - STACK_SIZE;
            let slab = index / STACK_SIZE;
            let local = index % STACK_SIZE;
            self.heap[slab].split_at_mut(local).1
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
            let size = base_ty.size.expect("Cannot allocate unsized type");
            let ty = pointer_type(base_ty);

            let len = ctx.stack.len();
            ctx.stack.resize(len + size, 0xD1);

            (ty, Data::Binary { size: 64, val: len as u64 })
        },
        Expr::GenAlloc(ty) => {
            let base_ty = eval_type(ctx, ty);
            let size = base_ty.size.expect("Cannot allocate unsized type");
            let ty = pointer_type(base_ty);

            if size == 0 {
                // not exactly a null pointer since the stack starts at zero in this interpretor...
                return (ty, Data::Binary { size: 64, val: 0 })
            }
            let mut slab = 0;
            while slab < ctx.heap.len() && ctx.heap[slab].len() != 0 {
                slab += 1;
            }
            let mut slab_data = Vec::new();
            slab_data.resize(size, 0xD1);
            if slab < ctx.heap.len() {
                ctx.heap[slab] = slab_data;
            } else {
                ctx.heap.push(slab_data);
            }
            let val = (slab * STACK_SIZE + STACK_SIZE) as u64;

            (ty, Data::Binary { size: 64, val })
        },
        Expr::Deref(val) => {
            let (ty, val) = eval(ctx, val);
            let ty = deref_type(ty).expect("Tried to dereference non-scalar");
            let index = val.get_num()
                .expect("Pointer binding was non-scalar?");
            let data = read(ctx.get_mem(index as usize), &ty);
            (ty, data)
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

fn write(out: &mut [u8], ty: &Type, data: Data) {
    let size = ty.size.expect("Tried to write unsized value to memory");
    if size == 0 {
        return;
    }
    match data {
        Data::Binary { val, .. } => {
            if size == 8 {
                unsafe {
                    let valbytes: [u8; 8] = std::mem::transmute(val);
                    for i in 0..8 {
                        out[i] = valbytes[i];
                    }
                }
            } else {
                unimplemented!();
            }
        },
        Data::Array(elems) => {
            if let TypeLayout::Array { content, .. } = &ty.layout {
                let chunk_size = content.size
                    .expect("Tried to write array of unsized values to memory");
                for (chunk, elem) in out.chunks_mut(chunk_size).zip(elems) {
                    write(chunk, content, elem);
                }
            }
        },
        Data::Struct(mut fields) => {
            let mut remain = out;
            if let TypeLayout::Struct { fields: field_tys } = &ty.layout {
                for (name, ty) in field_tys {
                    let field = fields.remove(name)
                        .expect("Missing field in struct memory write");

                    let size = ty.size.expect("Cannot write unsized field to memory");
                    let (left, right) = remain.split_at_mut(size);
                    remain = right;
                    write(left, ty, field);
                }
                assert!(fields.len() == 0, "Struct write has too many fields");
            }
        },
    }
}

fn read_u64(data: &[u8]) -> u64 {
    let mut out = [0; 8];
    for i in 0..8 {
        out[i] = data[i];
    }
    unsafe {
        std::mem::transmute(out)
    }
}

fn read(data: &[u8], ty: &Type) -> Data {
    match &ty.layout {
        TypeLayout::Binary { size } => {
            if *size == 0 {
                Data::Binary { size: 0, val: 0 }
            } else if *size == 8 {
                Data::Binary { size: 8, val: read_u64(data) }
            } else {
                unimplemented!();
            }
        },
        TypeLayout::Pointer { .. } => Data::Binary { size: 8, val: read_u64(data) },
        TypeLayout::Struct { fields: field_tys } => {
            let mut fields = HashMap::new();
            let mut remain = data;
            for (name, field_ty) in field_tys {
                let size = field_ty.size.expect("Cannot read unsized struct");
                let (left, right) = remain.split_at(size);
                remain = right;
                fields.insert(name.clone(), read(left, field_ty));
            }
            Data::Struct(fields)
        },
        TypeLayout::Array { content, number } => {
            let mut elems = Vec::new();
            let size = content.size.expect("Cannot read array of unsized");
            for chunk in data.chunks(size).take(*number) {
                elems.push(read(chunk, content));
            }
            Data::Array(elems)
        },
        TypeLayout::Untyped => {
            panic!("Cannot read pointer to untyped?");
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
                let referant_type = deref_type(lhs_type)
                    .expect("Cannot write to non-pointer");
                let index = lhs.get_num()
                    .expect("Tried to write to nonscalar value?");
                let (rhs_type, rhs) = eval(ctx, rhs);
                write(ctx.get_mem(index as usize), &referant_type, rhs);
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

