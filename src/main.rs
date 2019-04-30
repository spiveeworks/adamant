extern crate clean_lang;

use std::collections::HashMap;

use std::fs::File;
use std::io::prelude::*;

use clean_lang::ItemsParser;
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

    let parser = ItemsParser::new();

    let mut itemses: Vec<_> = args.map(|path| read_code(&parser, &path)).collect();
    let items = itemses.remove(0);
    let mut program = None;
    for item in &items {
        if let clean_lang::Item::Function(name, args, body) = item {
            if name == "main" {
                program = Some(body);
                break;
            }
        }
    }
    let mut bindings = HashMap::new();
    execute(&mut bindings, &program.expect("No main function defined"));

    // println!("Result:\n  {:?}", result);
}

