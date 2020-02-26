#[macro_use]
extern crate lazy_static;

use regex::Regex;

use std::env;
use std::fs::read_to_string;

mod errors;
mod expression;
mod token_tree;
mod utils;

fn main() {
    // takes in cli arguments
    let args: Vec<String> = env::args().collect();
    let file = args[1].clone();

    let contents = read_to_string(file).expect("Error reading file.");

    let comment_reg = Regex::new(token_tree::COMMENT).unwrap();
    let contents = comment_reg.replace_all(&contents, "");
    println!("{:?}", contents);

    let tokens = token_tree::lex(&contents).unwrap();
    println!("{:?}", tokens);
    let tree = token_tree::parse(Box::new(tokens.iter())).unwrap();
    println!("{:?}", tree);
    let expr = token_tree::parse_expr(&tree);
    println!("{:?}", expr);
}
