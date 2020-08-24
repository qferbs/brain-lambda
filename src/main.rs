#[macro_use]
extern crate lazy_static;

use std::env;
use std::fs::read_to_string;

mod brainfuck_ops;
mod compile;
mod errors;
mod expression;
mod parsing;
mod utils;

fn main() {
    // takes in cli arguments
    let args: Vec<String> = env::args().collect();
    let file = args[1].clone();

    let contents = read_to_string(file).expect("Error reading file.");

    // utils::compile_debug::compile_and_run_augmented_brainfuck(contents);

    let contents = parsing::strip_comments(contents);
    println!("{:?}", contents);
    let tokens = parsing::lex(&contents).unwrap();
    println!("{:?}", tokens);
    let tree = parsing::parse(Box::new(tokens.iter())).unwrap();
    println!("{:?}", tree);
    let expr = parsing::parse_expr(&tree);
    println!("{:?}", expr);
}
