extern crate clean_lang;

use std::fs::File;
use std::io::prelude::*;

use clean_lang::ProgramParser;
use clean_lang::execute;

fn read_code(parser: &ProgramParser, path: &str)
    -> String
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

    let parser = ProgramParser::new();

    let mut programses: Vec<_> = args.map(|path| read_code(&parser, &path)).collect();
    let expr = execute(programses.remove(0));

    // println!("Result:\n  {:?}", result);
}

