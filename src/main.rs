#[macro_use]
extern crate lazy_static;

use regex::Regex;

use std::env;
use std::fs::read_to_string;

mod errors;
mod expression;
mod parsing;
mod utils;

fn main() {
    // takes in cli arguments
    let args: Vec<String> = env::args().collect();
    let file = args[1].clone();

    let contents = read_to_string(file).expect("Error reading file.");

    let comment_reg = Regex::new(parsing::COMMENT).unwrap();
    let contents = comment_reg.replace_all(&contents, "");
    println!("{:?}", contents);

    let tokens = parsing::lex(&contents).unwrap();
    println!("{:?}", tokens);
    let tree = parsing::parse(Box::new(tokens.iter())).unwrap();
    println!("{:?}", tree);
    let expr = parsing::parse_expr(&tree);
    println!("{:?}", expr);
}
