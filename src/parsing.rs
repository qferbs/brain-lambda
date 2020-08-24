use regex::Regex;
use regex::RegexSet;

use crate::errors::ParsingError;
use crate::errors::TokenError;
use crate::expression::Expression;
use crate::utils::split;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

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
// TODO: add hex
const LIT: &str = "^[0-9]+|\'.\'$";
pub const COMMENT: &str = r"(?m)#.*$";
// whitespace tokens have no meaning
comb_const! {
    SEP_AND_OP, &[char];
    SEP, '{', '}','(',')';
    OP, '+', '-', '/', '*';
    WHITESPACE, ' ', '\r', '\n', '\t';
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    Var(&'a str),
    Key(&'a str),
    Sep(&'a str),
    Op(&'a str),
    Lit(&'a str),
    FuncBody,
    EOF,
    Expr, // generic
}

impl Token<'_> {
    fn get_token(string: &str) -> Result<Token, TokenError> {
        lazy_static! {
            static ref REG: RegexSet = RegexSet::new(&[KEY, LIT, VAR]).unwrap();
        }

        if string.len() == 1 {
            if OP.contains(&string.chars().nth(0).unwrap()) {
                return Ok(Token::Op(string));
            } else if SEP.contains(&string.chars().nth(0).unwrap()) {
                return Ok(Token::Sep(string));
            }
        }

        match REG.matches(string).iter().next() {
            Some(0) => Ok(Token::Key(string)),
            Some(1) => Ok(Token::Lit(string)),
            Some(2) => Ok(Token::Var(string)),
            _ => Err(TokenError(String::from(string))),
        }
    }
}

pub fn lex(string: &str) -> Result<Vec<Token>, TokenError> {
    let mut tokens: Vec<Token> = vec![];

    for token_string in split::Spliterator::new(string, SEP_AND_OP).filter(|s| {
        !s.chars()
            .nth(0)
            .map(|s| WHITESPACE.contains(&s))
            .unwrap_or(false)
            && s != &""
    }) {
        tokens.push(Token::get_token(token_string)?);
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}

pub fn parse<'a>(
    tokens: Box<dyn Iterator<Item = &'a Token> + 'a>,
) -> Result<Tree<&'a Token<'a>>, ParsingError> {
    parse_tree(Rc::new(RefCell::new(tokens)), &Token::Expr)
}

pub fn parse_tree<'a>(
    tokens: Rc<RefCell<dyn Iterator<Item = &'a Token> + 'a>>,
    value: &'a Token,
) -> Result<Tree<&'a Token<'a>>, ParsingError> {
    use Token::*;

    let mut children: Vec<Tree<&Token>> = vec![];

    loop {
        let tok = {
            if let Some(t) = tokens.borrow_mut().next() {
                t
            } else {
                return Err(ParsingError("Misaligned delimiters.".to_string()));
            }
        };

        let child: Tree<&Token> = match tok {
            Sep("(") => parse_tree(Rc::clone(&tokens), &Expr)?,
            Sep("{") => parse_tree(Rc::clone(&tokens), &FuncBody)?,
            Sep(")") => return Ok(Tree::new(value, children)),
            Sep("}") => return Ok(Tree::new(value, children)),
            EOF => return Ok(Tree::new(value, children)),
            ref t => Tree::new(t, vec![]),
        };

        children.push(child);
    }
}

pub fn parse_expr<'a>(
    token_tree: &'a Tree<&'a Token>,
) -> Result<Expression<'a, u32>, ParsingError> {
    use Expression::*;
    use Token::*;

    if token_tree.children.len() == 0 {
        return match token_tree.value {
            Var(var) => Ok(Variable(var)),
            Lit(lit) => Ok(Literal(parse_lit(lit)?)),
            Expr => parse_expr(token_tree),
            _ => Err(ParsingError(
                "Unexpected token, expected lit or var.".to_string(),
            )),
        };
    }

    let mut last_expr: Option<Expression<u32>> = None;
    let mut tree_iter = token_tree.children.iter();

    while let Some(child) = tree_iter.next() {
        if !match last_expr {
            Some(Function { vars, lambda_term }) => {
                last_expr = Some(Application {
                    func: Box::new(Function { vars, lambda_term }),
                    inputs: vec![parse_expr(child)?],
                });
                true
            }
            Some(Application { ref mut inputs, .. }) => {
                inputs.push(parse_expr(child)?);
                true
            }
            _ => false,
        } {
            last_expr = match child.value {
                Op(op) => Some(Operator {
                    op,
                    lhs_expr: Box::new(last_expr.take().ok_or_else(|| {
                        ParsingError("Operator missing lhs argument.".to_string())
                    })?),
                    rhs_expr: Box::new(parse_expr(tree_iter.next().ok_or_else(
                        || ParsingError("Operator missing rhs argument.".to_string()),
                    )?)?),
                }),
                Key("=>") => Some(Function {
                    vars: get_func_args(last_expr.take().ok_or_else(|| {
                        ParsingError("Function missing variable list.".to_string())
                    })?)?,
                    lambda_term: Box::new(parse_expr(tree_iter.next().ok_or_else(
                        || ParsingError("Function missing lambda term.".to_string()),
                    )?)?),
                }),
                Expr => Some(parse_expr(child)?),
                Var(var) => {
                    if let Some(Variable(first_arg)) = last_expr {
                        // If two vars are next to each other, assume all tokens in scope are vars
                        let mut args: Vec<&'a str> = vec![first_arg, var];
                        for ch in &mut tree_iter {
                            args.push(match ch.value {
                                Var(arg) => arg,
                                _ => {
                                    return Err(ParsingError(
                                        "Illegal FuncArgs.".to_string(),
                                    ))
                                }
                            });
                        }
                        Some(FuncArgs(args.into_boxed_slice()))
                    } else {
                        Some(Variable(var))
                    }
                }
                Lit(lit) => {
                    Some(Literal(lit.parse().or_else(|_| {
                        Err(ParsingError("Illegal literal.".to_string()))
                    })?))
                }
                _ => return Err(ParsingError("Invalid token.".to_string())),
            };
        }
    }

    last_expr.ok_or_else(|| {
        ParsingError(
            "Invalid expression. Empty expressions are not allowed.".to_string(),
        )
    })
}

fn get_func_args<'a>(
    func_args: Expression<'a, u32>,
) -> Result<Box<[&'a str]>, ParsingError> {
    Ok(match func_args {
        Expression::FuncArgs(args) => args,
        // to account for possibility of single arg
        Expression::Variable(arg) => Box::new([arg]),
        _ => return Err(ParsingError("Illegal function arguments.".to_string())),
    })
}

pub fn parse_lit(lit_str: &str) -> Result<u32, ParsingError> {
    // TODO: add hex
    if let Ok(x) = lit_str.parse::<u32>() {
        Ok(x)
    } else {
        Ok(lit_str
            .chars()
            .nth(1)
            .ok_or_else(|| ParsingError("Illegal literal.".to_string()))?
            as u32)
    }
}

pub fn strip_comments(string: String) -> String {
    let comment_reg = Regex::new(COMMENT).unwrap();
    comment_reg.replace_all(&string, "").to_string()
}

#[derive(Eq, PartialEq)]
pub struct Tree<V> {
    value: V,
    pub children: Box<[Tree<V>]>,
}

impl<V> Tree<V> {
    pub fn new(value: V, children: Vec<Tree<V>>) -> Tree<V> {
        Tree {
            value,
            children: children.into_boxed_slice(),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn lex_test() {
        let input = "\tvar => \n(030 + 'c')";
        assert_eq!(
            lex(input).unwrap(),
            vec!(
                Var("var"),
                Key("=>"),
                Sep("("),
                Lit("030"),
                Op("+"),
                Lit("'c'"),
                Sep(")"),
                EOF
            )
        );
    }

    #[test]
    fn lex_test_complex() {
        let input = "\n\t(a c_c) => (2 + a*c_c) 6 'f'";
        assert_eq!(
            lex(input).unwrap(),
            vec!(
                Sep("("),
                Var("a"),
                Var("c_c"),
                Sep(")"),
                Key("=>"),
                Sep("("),
                Lit("2"),
                Op("+"),
                Var("a"),
                Op("*"),
                Var("c_c"),
                Sep(")"),
                Lit("6"),
                Lit("'f'"),
                EOF
            )
        );
    }

    #[test]
    fn parse_test() {
        // "x => (a + (b c))"
        let input = vec![
            Var("x"),
            Key("=>"),
            Sep("("),
            Lit("a"),
            Op("+"),
            Sep("("),
            Lit("b"),
            Var("c"),
            Sep(")"),
            Sep(")"),
            EOF,
        ];
        assert_eq!(
            parse(Box::new(input.iter())).unwrap(),
            Tree::new(
                &Expr,
                vec!(
                    Tree::new(&Var("x"), vec!()),
                    Tree::new(&Key("=>"), vec!()),
                    Tree::new(
                        &Expr,
                        vec!(
                            Tree::new(&Lit("a"), vec!()),
                            Tree::new(&Op("+"), vec!()),
                            Tree::new(
                                &Expr,
                                vec!(
                                    Tree::new(&Lit("b"), vec!()),
                                    Tree::new(&Var("c"), vec!()),
                                )
                            )
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn parse_expr_test() {
        let input = Tree::new(
            &Expr,
            vec![
                Tree::new(
                    &Expr,
                    vec![Tree::new(&Var("x"), vec![]), Tree::new(&Var("why"), vec![])],
                ),
                Tree::new(&Key("=>"), vec![]),
                Tree::new(
                    &Expr,
                    vec![
                        Tree::new(&Lit("030"), vec![]),
                        Tree::new(&Op("+"), vec![]),
                        Tree::new(&Lit("'b'"), vec![]),
                    ],
                ),
                Tree::new(&Lit("30"), vec![]),
            ],
        );
        parse_expr(&input).unwrap();
    }
}
