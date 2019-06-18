extern crate lvalue_free_prototype;

use lvalue_free_prototype as clean_lang;

use std::fs::File;
use std::io::prelude::*;

use clean_lang::ItemsParser;
use clean_lang::compile;
use clean_lang::execute;

fn read_code(parser: &ItemsParser, path: &str) -> Vec<clean_lang::Item>
{
    let mut file = File::open(path).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");

    let programs = parser.parse(&contents);
    programs.expect("Failed to parse")
}

fn main() {
    let mut args = ::std::env::args();
    args.next();  // first argument is executable itself
    let mut args: Vec<_> = args.collect();
    let mut exec = false;
    if args[0] == "--execute" {
        exec = true;
        args.remove(0);
    }

    let parser = ItemsParser::new();

    let mut itemses: Vec<_> = args
        .into_iter()
        .map(|path| read_code(&parser, &path))
        .collect();
    let items = itemses.remove(0);

    if exec {
        let mut ctx = clean_lang::Context::new(items);
        let enter = vec![clean_lang::Statement::Return(clean_lang::Expr::Call(
            Box::new(clean_lang::Expr::Ident("main".into())),
            Vec::new(),
        ))];

        let val = execute(&mut ctx, &enter);

        println!("Got result: {:?}", val);
    } else {
        let mut output = Vec::new();
        compile(&mut output, items).unwrap();
        print!("{}", String::from_utf8(output).unwrap());
    }
}

