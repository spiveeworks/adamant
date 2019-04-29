extern crate clean_lang;

use std::fs::File;
use std::io::prelude::*;

use lofer_lang::ProgramParser;
use lofer_lang::execute;

fn read_code(parser: &ProgramParser, path: &str)
    -> Vec<readable::Program>
{
    let mut file = File::open(path).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");

    let programs = parser.parse(&contents);
    programs
}

fn main() {
    let mut args = ::std::env::args();
    args.next();  // first argument is executable itself

    let parser = ProgramParser::new();

    let programses: Vec<_> = args.map(|path| read_code(&parser, &path)).collect();
    let expr = execute(programses.next().expect("No path given"));

    // println!("Result:\n  {:?}", result);
}

