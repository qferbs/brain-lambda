use regex::RegexSet;

use crate::errors::TokenError;
use crate::errors::ParsingError;
use crate::utils::split;
use crate::expression::Expression;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

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
const KEY: &str = r"^=>$";
const LIT: &str = "^[0-9]+|\".*\"$";
const COMMENT: &str = r"^#.*$";
// whitespace tokens have no meaning
comb_const! {
    SEP_AND_OP, &[char];
    SEP, '{', '}','(',')','[',']', ';', ',';
    OP, '+', '-', '.', '/', '*';
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
    EOF,
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
            _ => Err(TokenError(string)),
        }
    }
}


pub fn lex(string: &String) -> Result<Vec<Token>, TokenError> {
    let mut tokens: Vec<Token> = vec!();

    for token_string in split::Spliterator::new(string, SEP_AND_OP)
        .filter(|s| !s.chars().nth(0)
                    .and_then(|s| Some(WHITESPACE.contains(&s)))
                    .unwrap_or(false) && s != &"") {
        tokens.push(Token::get_token(token_string)?);
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}


pub fn parse<'a>(tokens: Box<dyn Iterator<Item = &'a Token> + 'a>) -> Result<Tree<&'a Token<'a>>, ParsingError<'a>> {

    parse_tree(Rc::new(RefCell::new(tokens)), &Token::Expr)
}


pub fn parse_tree<'a>(tokens: Rc<RefCell<dyn Iterator<Item = &'a Token> + 'a>>,
                      value: &'a Token) -> Result<Tree<&'a Token<'a>>, ParsingError<'a>> {
    use Token::*;

    let mut children: Vec<Tree<&Token>> = vec!();

    loop {
        let tok = {
            if let Some(t) = tokens.borrow_mut().next() {t}
            else {return Err(ParsingError("Misaligned delimiters."));}
        };

        let child: Tree<&Token> = match tok {
            Sep("(") => parse_tree(Rc::clone(&tokens), &Expr)?,
            Sep("{") => parse_tree(Rc::clone(&tokens), &FuncBody)?,
            Sep(")") => return Ok(Tree::new(value, children)),
            Sep("}") => return Ok(Tree::new(value, children)),
            EOF => return Ok(Tree::new(value, children)),
            ref t => Tree::new(t, vec!()),
        };

        children.push(child);
    }
}

pub fn parse_expr<'a>(token_tree: &'a Tree<&'a Token>)
    -> Result<Expression<'a, u32>, ParsingError<'a>> {
    use Token::*;
    use Expression::*;

    if token_tree.children.len() == 0 {
        return match token_tree.value {
            Var(var) => Ok(Variable(var)),
            Lit(lit) => Ok(Literal(lit.parse()
                    .or(Err(ParsingError("Illegal literal.")))?
                )),
            Expr => parse_expr(token_tree),
            _ => Err(ParsingError("Unexpected token, expected lit or var.")),
        }
    }

    let mut last_expr: Option<Expression<u32>> = None;
    let mut tree_iter = token_tree.children.iter();

    while let Some(child) = tree_iter.next() {
        if !match last_expr {
            Some(Function{vars, lambda_term}) => {
                last_expr = Some(Application{
                    func: Box::new(Function{vars, lambda_term}),
                    inputs: vec!(parse_expr(child)?),
                });
                true
            },
            Some(Application{func: _, ref mut inputs}) => {
                inputs.push(parse_expr(child)?);
                true
            },
            _ => false,
        } {
            last_expr = match child.value {
                Op(op) => Some(Operator{
                    tok: op,
                    op: Box::new(get_op(&Op(op))?),
                    lhs_expr:  Box::new(last_expr.take()
                        .ok_or(ParsingError("Operator missing lhs argument."))?
                    ),
                    rhs_expr: Box::new(
                        parse_expr(tree_iter.next()
                            .ok_or(ParsingError("Operator missing rhs argument."))?
                        )?
                    ),
                }),
                Key("=>") => Some(Function{
                    vars: get_func_args(last_expr.take()
                        .ok_or(ParsingError("Function missing variable list."))?
                    )?,
                    lambda_term: Box::new(
                        parse_expr(tree_iter.next()
                            .ok_or(ParsingError("Function missing lambda term."))?
                        )?
                    ),
                }),
                Expr => Some(parse_expr(child)?),
                Var(var) => if let Some(Variable(first_arg)) = last_expr {
                    // If to vars are next to each other, assume all tokens in scope are vars
                    let mut args: Vec<&'a str> = vec!(first_arg, var);
                    while let Some(ch) = tree_iter.next() {
                        args.push(match ch.value {
                            Var(arg) => arg,
                            _ => return Err(ParsingError("Illegal FuncArgs.")),
                        });
                    }
                    Some(FuncArgs(args.into_boxed_slice()))
                } else {
                    Some(Variable(var))
                },
                Lit(lit) => Some(Literal(lit.parse()
                    .or(Err(ParsingError("Illegal literal.")))?
                )),
                _ => return Err(ParsingError("Invalid token.")),
            };
        }
    }

    last_expr.ok_or(ParsingError("Invalid expression. Empty expressions are not allowed."))
}

fn get_op<'a>(op: &Token) -> Result<Box<dyn Fn(u32, u32) -> u32>, ParsingError<'a>> {
    use Token::*;

    match op {
        Op("+") => Ok(Box::new(|x, y| x + y)),
        Op("-") => Ok(Box::new(|x, y| x - y)),
        Op("*") => Ok(Box::new(|x, y| x * y)),
        Op("/") => Ok(Box::new(|x, y| x / y)),
        _ => Err(ParsingError("Illegal operator.")),
    }
}

fn get_func_args<'a>(func_args: Expression<'a, u32>) -> Result<Box<[&'a str]>, ParsingError<'a>> {

    Ok( match func_args {
        Expression::FuncArgs(args) => args,
        // to account for possibility of single arg
        Expression::Variable(arg) => Box::new([arg]),
        _ => return Err(ParsingError("Illegal function arguments.")),
    })
}

pub struct Tree<V> {
    value: V,
    pub children: Box<[Tree<V>]>,
}

impl<V> Tree<V> {
    pub fn new(value: V, children: Vec<Tree<V>>) -> Tree<V> {
        Tree {value: value, children: children.into_boxed_slice()}
    }
}

impl<V: fmt::Debug> fmt::Debug for Tree<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        fn pprint<V: fmt::Debug>(slf: &Tree<V>, tabs: &str) -> String {
            let mut st = String::new();
            st.push_str(&format!("{}-{:?}", tabs, slf.value));
            let tabs = format!("{}    ", tabs);

            for child in slf.children.iter() {
                st.push_str(&format!("\n{}", pprint(child, &tabs)));
            }
            st
        }

        write!(f, "Tree:\n{}", pprint(self, "  "))
    }
}
