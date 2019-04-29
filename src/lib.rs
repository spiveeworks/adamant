mod parser;

pub use parser::ProgramParser;

pub fn execute(val: String) {
    println!("Executing string: {}", val);
}
