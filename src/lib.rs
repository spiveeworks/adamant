#[macro_use]
extern crate lalrpop_util;

use std::io;
use std::fmt;
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
    Binary { bits: u8 },
    Struct { fields: Vec<(String, Type)> },
    Array { content: TypeBox, number: usize },
    Function { params: Vec<(String, Type)>, /*result*/ },
    Pointer { base: TypeBox },  // ideally a subtype of bsize/usize??
    //Subtype { name: String, base: Box<Type> },
    //Backlink { order: u8 },
    Untyped,
}

//type FunId = usize;

#[derive(Clone)]
pub enum TypeExpr {
    Binary { bits: u8 },
    Struct(HashMap<String, TypeExpr>),
    Array(Box<TypeExpr>, Box<Expr>),
    VarArray(Box<TypeExpr>),
    Ptr(Box<TypeExpr>),
    Ident(String),
}

#[derive(Clone, Debug)]
pub enum Data {
    Binary { bits: u8, val: u64 }, // size? probably not necessary now that bindings are typed
    Array(Vec<Data>),
    Struct(HashMap<String, Data>),
    Function(String),
}

#[derive(Clone)]
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
    Minus(Box<(Expr, Expr)>),
    LessThan(Box<(Expr, Expr)>),
    IntegerLiteral(u64),
    StructLiteral(HashMap<String, Expr>),
    ArrayLiteral(Vec<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Clone)]
pub enum Statement {
    Define(String, Expr),
    Write(Expr, Expr),
    // at some point this should only do function calls, I think
    Discard(Expr),
    Return(Expr),
    While(Expr, Vec<Statement>),
}

#[derive(Clone)]
pub struct Function {
    params: Vec<(String, TypeExpr)>,
    algorithm: Vec<Statement>,
}

#[derive(Clone)]
pub enum Item {
    TypeDef(String, TypeExpr),
    Function(String, Function),
}

#[derive(Clone)]
pub struct Context {
    pub funs: &'static HashMap<String, Function>,
    pub bindings: HashMap<String, (Type, Data)>,
    // make this bottom heap? that is already how we adress it at the moment
    pub stack: Vec<u8>,
    pub heap: Vec<Vec<u8>>,
}

fn leak_global<T: 'static>(val: T) -> &'static T {
    let as_box = Box::new(val);
    let as_raw = Box::into_raw(as_box);
    unsafe { &*as_raw }
}

impl Context {
    pub fn new(items: Vec<Item>) -> Self {
        let mut funs = HashMap::new();
        for x in items {
            if let Item::Function(name, fun) = x {
                funs.insert(name, fun);
            }
        }

        Context {
            funs: leak_global(funs),
            bindings: HashMap::new(),
            stack: Vec::new(),
            heap: Vec::new(),
        }
    }

    pub fn push_ctx(self: &mut Self) -> (usize, HashMap<String, (Type, Data)>) {
        let len = self.stack.len();
        let bindings = std::mem::replace(&mut self.bindings, HashMap::new());
        (len, bindings)
    }

    pub fn pop_ctx(self: &mut Self, len: usize, bindings: HashMap<String, (Type, Data)>) {
        self.stack.resize(len, 0xD1);
        self.bindings = bindings;
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
        if let &Data::Binary { bits: _, val } = self {
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

fn num_type(bits: u8) -> Type {
    Type { size: Some(8), layout: TypeLayout::Binary { bits }}
}

fn struct_type(field_tys: Vec<(String, Type)>) -> Type {
    let mut size = Some(0);
    for (_name, ty) in &field_tys {
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
        &TypeExpr::Binary { bits } => Type {
            size: Some(8),
            layout: TypeLayout::Binary { bits }
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
    return Data::Binary { bits: 0, val: 0 };
    match ty.layout {
        TypeLayout::Binary { .. } => unimplemented!(),
        TypeLayout::Struct { .. } => unimplemented!(),
        TypeLayout::Array { .. } => unimplemented!(),
        // TypeLayout::VarArray(_) => unimplemented!(),
        TypeLayout::Function { .. } => unimplemented!(),
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
        Type { size: Some(8), layout: TypeLayout::Binary { bits: 64 } },
        Data::Binary { bits: 64, val },
    )
}

fn eval_bool(val: bool) -> (Type, Data) {
    (
        Type { size: Some(8), layout: TypeLayout::Binary { bits: 1 } },
        Data::Binary { bits: 64, val: val as u64 },
    )
}

fn get_last_ptr(ctx: &mut Context, mut ty: Type, mut data: Data) -> (usize, Type, Data) {
    let mut layers = 0;
    while let TypeLayout::Pointer { base } = ty.layout {
        layers += 1;
        ty = *base;
        // condition here so that reads lag behind type stripping
        if layers > 1 {
            let index = data.get_num().expect("Pointer was nonscalar?");
            data = read(ctx.get_mem(index as usize), &ty);
        }
    }
    (layers, ty, data)
}

fn eval(ctx: &mut Context, expr: &Expr) -> (Type, Data) {
    match expr {
        Expr::AutoAlloc(ty) => {
            let base_ty = eval_type(ctx, ty);
            let size = base_ty.size.expect("Cannot allocate unsized type");
            let ty = pointer_type(base_ty);

            let len = ctx.stack.len();
            ctx.stack.resize(len + size, 0xD1);

            (ty, Data::Binary { bits: 64, val: len as u64 })
        },
        Expr::GenAlloc(ty) => {
            let base_ty = eval_type(ctx, ty);
            let size = base_ty.size.expect("Cannot allocate unsized type");
            let ty = pointer_type(base_ty);

            if size == 0 {
                // not exactly a null pointer since the stack starts at zero in this interpretor...
                return (ty, Data::Binary { bits: 64, val: 0 })
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

            (ty, Data::Binary { bits: 64, val })
        },
        Expr::Deref(val) => {
            let (ty, val) = eval(ctx, val);
            let ty = deref_type(ty).expect("Tried to dereference non-scalar");
            let index = val.get_num()
                .expect("Pointer binding was non-scalar?");
            let data = read(ctx.get_mem(index as usize), &ty);
            (ty, data)
        },
        Expr::Ident(name) => {
            if let Some(val) = ctx.bindings.get(name) {
                val.clone()
            } else {
                /*let params = ctx
                    .funs[name]
                    .params
                    .iter()
                    .map(|(param_name, ty)| (param_name.clone(), eval_type(ctx, ty)))
                    .collect();*/
                let ty = Type {
                    size: None,
                    layout: TypeLayout::Function { params: Vec::new() }
                };
                (ty, Data::Function(name.clone()))
            }
        }
        Expr::Field(data, field) => {
            let (ptr_ty, data) = eval(ctx, data);
            let (layers, ty, data) = get_last_ptr(ctx, ptr_ty, data);
            // mut so that i can use a more generally defined method....
            let mut field_tys = {
                if let TypeLayout::Struct { fields: field_tys } = ty.layout {
                    field_tys
                } else {
                    panic!("Tried to access field of non-struct");
                }
            };
            if layers == 0 {
                if let Data::Struct(mut vals) = data {
                    let field_ty = lookup(&mut field_tys, field)
                        .expect("Struct does not contain this field")
                        .clone();
                    (field_ty, vals.remove(field).expect("Field does not exist"))
                } else {
                    panic!("Struct binding wasn't a struct?");
                }
            } else {
                let mut val = data.get_num()
                    .expect("get_last_ptr gave nonscalar?") as usize;
                for (this_name, this_ty) in field_tys {
                    if this_name == *field {
                        let ty = pointer_type(this_ty);
                        let ptr = Data::Binary { bits: 64, val: val as u64 };
                        return (ty, ptr);
                    } else {
                        val += this_ty.size
                            .expect("Can't reference fields past an unsized field");
                    }
                }
                panic!("Tried to reference non-existant field of struct");
            }
        },
        Expr::Index(data_and_index) => {
            let (data, index) = &**data_and_index;
            let (ptr_ty, data) = eval(ctx, data);
            let (layers, ty, data) = get_last_ptr(ctx, ptr_ty, data);
            let (content_ty, number) = if let TypeLayout::Array { content, number } = ty.layout {
                (*content, number)
            } else {
                panic!("Tried to index non-array");
            };
            let (_, index) = eval(ctx, index);
            // check that we arent indexing with a pointer?
            let index = index.get_num()
                .expect("Tried to index by non-scalar");
            if layers == 0 {
                if let Data::Array(mut data) = data {
                    (content_ty, data.remove(index as usize))
                } else {
                    panic!("Array binding wasn't an array?");
                }
            } else {
                let start = data.get_num()
                    .expect("get_last_ptr gave nonscalar?");
                let offset = index * content_ty.size
                    .expect("Can't reference fields past an unsized field") as u64;
                let ty = pointer_type(content_ty);
                let ptr = Data::Binary { bits: 64, val: start + offset };
                return (ty, ptr);
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
        Expr::Minus(left_and_right) => {
            let (left, right) = &**left_and_right;
            let left = eval(ctx, left).1
                .get_num()
                .expect("Tried to subtract non-number");
            let right = eval(ctx, right).1
                .get_num()
                .expect("Tried to subtract non-number");
            eval_num(left - right)
        },
        Expr::LessThan(left_and_right) => {
            let (left, right) = &**left_and_right;
            let left = eval(ctx, left).1
                .get_num()
                .expect("Tried to compare non-number");
            let right = eval(ctx, right).1
                .get_num()
                .expect("Tried to compare non-number");
            eval_bool(left < right)
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

            let ty = first_ty.unwrap_or(Type { size: Some(0), layout: TypeLayout::Binary { bits: 0 } });
            (array_type(ty, size), Data::Array(vals))
        },
        Expr::Call(fun, arg_expr) => {
            let (_, fun) = eval(ctx, fun);
            let Function { ref params, ref algorithm } = {
                if let Data::Function(ref name) = fun {
                    &ctx.funs[name]
                } else {
                    panic!("Tried to call non-function");
                }
            };
            let mut args = HashMap::with_capacity(arg_expr.len());
            for ((param_name, param_ty), arg) in params.iter().zip(arg_expr) {
                args.insert(param_name.clone(), eval(ctx, arg));
            }
            let (stack_len, bindings) = ctx.push_ctx();
            ctx.bindings = args;
            let result = execute(ctx, algorithm);
            ctx.pop_ctx(stack_len, bindings);
            result
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
        Data::Function(_) => {
            unimplemented!();
        }
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
        TypeLayout::Binary { bits } => {
            if *bits == 0 {
                Data::Binary { bits: 0, val: 0 }
            } else if *bits == 64 {
                Data::Binary { bits: 64, val: read_u64(data) }
            } else {
                unimplemented!();
            }
        },
        TypeLayout::Pointer { .. } => Data::Binary { bits: 8, val: read_u64(data) },
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
        TypeLayout::Function { .. } => {
            unimplemented!();
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
            Statement::While(cond, body) => {
                while {
                    let (cond_type, cond) = eval(ctx, cond);
                    let cond = cond
                        .get_num()
                        .expect("Tried to condition nonscalar value");
                    cond != 0
                } {
                    let (stack_len, bindings) = ctx.push_ctx();
                    // @Performance
                    ctx.bindings = bindings.clone();
                    execute(ctx, body);
                    ctx.pop_ctx(stack_len, bindings);
                }
            },
        }
        pc += 1;
    }
    (
        Type { size: Some(0), layout: TypeLayout::Binary { bits: 0 }},
        Data::Binary { bits: 0, val: 0 }
    )
}

enum Ref {
    Named(String),
    Anon(usize),
    IntLiteral(u64),
}

enum Instruction {
    Return { ty: Type, val: Ref },
}

#[derive(Default)]
struct UnpackContext {
    instructions: Vec<Instruction>,
    reg_count: usize,
}

impl UnpackContext {
    fn anon(self: &mut Self) -> Ref {
        let out = Ref::Anon(self.reg_count);
        self.reg_count += 1;
        out
    }
}

fn unpack_expr(out: &mut UnpackContext, expr: &Expr) -> (Type, Ref) {
    match expr {
        Expr::IntegerLiteral(x) => (num_type(64),Ref::IntLiteral(*x)),
        _ => unimplemented!(),
    }
}

fn unpack_alg(alg: &Vec<Statement>) -> Vec<Instruction> {
    let mut ctx = Default::default();
    for statement in alg {
        match statement {
            Statement::Return(x) => {
                let (ty, val) = unpack_expr(&mut ctx, x);
                ctx.instructions.push(Instruction::Return { ty, val });
            }
            _ => unimplemented!(),
        }
    }
    ctx.instructions
}

impl fmt::Display for Ref {
    fn fmt(self: &Self, out: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ref::Named(name) => {
                write!(out, "%{}", name)?;
            },
            Ref::Anon(x) => {
                write!(out, "%{}", x)?;
            },
            Ref::IntLiteral(x) => {
                write!(out, "{}", x)?;
            },
        }
        Ok(())
    }
}

impl fmt::Display for Type {
    fn fmt(self: &Self, out: &mut fmt::Formatter) -> fmt::Result {
        match self.layout {
            TypeLayout::Binary { bits } => {
                write!(out, "i{}", bits)?;
            },
            _ => unimplemented!(),
        }
        Ok(())
    }
}

fn compile_function(out: &mut io::Write, name: &str, f: Function)
    -> io::Result<()>
{
    writeln!(out, "define i64 @{}() {{", name)?;

    for instruction in unpack_alg(&f.algorithm) {
        match instruction {
            Instruction::Return { ty, val} => {
                writeln!(out, "    ret {} {}", ty, val)?;
            }
        }
    }
    writeln!(out, "}}")?;
    Ok(())
}

pub fn compile(out: &mut io::Write, items: Vec<Item>) -> io::Result<()> {
    for item in items {
        match item {
            Item::TypeDef(_, _) => unimplemented!(),
            Item::Function(name, f) => compile_function(out, &name, f)?,
        }
    }
    Ok(())
}
