use regex::RegexSet;

use crate::errors::TokenError;
use crate::errors::ParsingError;
use crate::utils::split;
use crate::expression::Expression;
use std::rc::Rc;
use std::cell::RefCell;

macro_rules! comb_const {
    ($a:ident, $t:ty; $($i:ident, $($x:literal),+);+;) => {
        $(
            const $i: $t = &[$($x),+];
        )+
        const $a: $t = &[$($($x),+),+];
    };
}

// regex constants defining legal values
const VAR: &str = r"^\w+$";
const KEY: &str = r"^f!$";
const LIT: &str = "^[0-9]+|\".*\"$";
const COMMENT: &str = r"^#.*$";
// whitespace tokens have no meaning
comb_const! {
    SEP_AND_OP, &[char];
    SEP, '{', '}','(',')','[',']', ';', ',';
    OP, '+', '-', '.', '/', '*', '>', '=';
    WHITESPACE, ' ', '\r', '\n', '\t';
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    Var(&'a str),
    Key(&'a str),
    Sep(&'a str),
    Op(&'a str),
    Lit(&'a str),
    Comment(&'a str),
    FuncBody,
    Expr, // generic
}

impl Token<'_> {
    fn get_token(string: &str) -> Result<Token, TokenError>{
        lazy_static! {
            static ref REG: RegexSet = RegexSet::new(
                &[COMMENT, KEY, LIT, VAR]
            ).unwrap();
        }

        if string.len() == 1 {
            if OP.contains(&string.chars().nth(0).unwrap()) {
                return Ok(Token::Op(string));
            } else if SEP.contains(&string.chars().nth(0).unwrap()) {
                return Ok(Token::Sep(string));
            }
        }
        
        match REG.matches(string).iter().next() {
            Some(0) => Ok(Token::Comment(string)),
            Some(1) => Ok(Token::Key(string)),
            Some(2) => Ok(Token::Lit(string)),
            Some(3) => Ok(Token::Var(string)),
            _ => Err(TokenError{string: string}),
        }
    }

    fn get_value(&self) -> &str{
        match self {
            Token::Var(string) => string,
            Token::Op(string) => string,
            Token::Comment(string) => string,
            Token::Key(string) => string,
            Token::Lit(string) => string,
            Token::Sep(string) => string,
            Token::FuncBody => "FuncBody",
            Token::Expr => "Expr",
        }
    }
}

pub fn lex(string: &String) -> Result<Vec<Token>, TokenError> {
    let mut tokens: Vec<Token> = vec!();

    for token_string in split::Spliterator::new(string, SEP_AND_OP)
            .filter(|s| !s.chars().nth(0).and_then(|s| Some(WHITESPACE.contains(&s))).unwrap_or(false) && s != &"") {
        tokens.push(Token::get_token(token_string)?);
    }

    Ok(tokens)
}


fn parse_tokens<'a>(tokens: &'a mut Vec<&'a Token>) -> Result<Expression<'a, u32>, ParsingError> {

    match tokens.pop().unwrap() {
        Token::Lit(val) => Ok(Expression::Literal{value: val.parse().unwrap()}),
        Token::Var(val) => Ok(Expression::Variable{name: val}),
        Token::Key("f") => parse_function(tokens),
        Token::Sep("(") => parse_tokens(tokens),


        tok => Err(ParsingError{string: format!("Invalid token: {:?}", tok)}),
    }
}

fn parse_function<'a>(tokens: &'a mut Vec<&'a Token>) -> Result<Expression<'a, u32>, ParsingError> {

    match tokens.pop().unwrap() {
        Token::Lit(val) => Ok(Expression::Literal{value: val.parse().unwrap()}),
        Token::Var(val) => Ok(Expression::Variable{name: val}),

        tok => Err(ParsingError{string: format!("Invalid token: {:?}", tok)}),
    }
}

pub fn parse_tree<'a>(tokens: Rc<RefCell<dyn Iterator<Item = &'a Token> + 'a>>, value: &'a Token) -> Result<Tree<&'a Token<'a>>, ParsingError> {
    use Token::*;

    let mut tree: Tree<&Token> = Tree{value: value, children: vec!()};
    loop {
        let tok = {
            if let Some(t) = tokens.borrow_mut().next() {t}
            else {return Ok(tree);}
        };

        let child: Tree<&Token> = match tok {
            Sep("(") => parse_tree(Rc::clone(&tokens), &Expr)?,
            Sep("{") => parse_tree(Rc::clone(&tokens), &FuncBody)?,
            Sep(")") => return Ok(tree),
            Sep("}") => parse_tree(Rc::clone(&tokens), &FuncBody)?,
            ref t => Tree{value: t, children: vec!()},
        };

        tree.children.push(child);
    }
}

#[derive(Debug)]
pub struct Tree<V> {
    value: V,
    children: Vec<Tree<V>>,
}

// impl<V> Tree<V> for SepNode<'_, V> {
//     fn walk(&self, tokens: &[Token]) -> Option<Expression<V>> {
//         if tokens.len() != 0 && self.token == tokens[0] {
//             for child in self.children.iter() {
//                 if let Some(out) = child.walk(&tokens[1..]) {
//                     return Some(out);
//                 }
//             }
//         }
// 
//         None
//     }
// }
