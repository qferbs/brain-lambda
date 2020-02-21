#[macro_use]
extern crate lazy_static;

use std::env;
use std::fs::read_to_string;
use std::rc::Rc;
use std::cell::RefCell;

mod errors;
mod token_tree;
mod utils;
mod expression;

fn main() {
    // takes in cli arguments
    let args: Vec<String> = env::args().collect();
    let file = args[1].clone();

    let contents = read_to_string(file).expect("Error reading file.");

    let tokens = token_tree::lex(&contents).unwrap();
    let tok = Rc::new(RefCell::new(tokens.iter()));
    let tree = token_tree::parse_tree(tok, &token_tree::Token::Expr);
    println!("{:?}", tokens);
    println!("{:#?}", tree);
}
