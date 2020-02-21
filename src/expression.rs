use std::collections::HashMap;
use crate::token_tree::Token;

pub enum Expression<'a, V> {
    Variable {name: &'a str},
    Literal {value: V},
    Function {vars: &'a [&'a str], lambda_term: &'a Expression<'a, V>, inputs: HashMap<&'a str, &'a Expression<'a, V>>},
    Operator {op: &'a dyn Fn(V, V) -> V, lhs_expr: &'a Expression<'a, V>, rhs_expr: &'a Expression<'a, V>}
}

// impl<'a, V> Expression<'a, V> {
//     fn construct(&self, tokens: Vec<Token>) -> Option<Expression<V>> {
//         match &self {
//             Expression::Variable{name: _} if &tokens[..]=> None,
//             Expression::Literal{value: _} => None,
//             Expression::Function{vars: _, lambda_term: _, inputs: _} => None,
//             Expression::Operator{op: _, lhs_expr: _, rhs_expr: _} => None,
//             _ => None,
//         }
//     }
// }