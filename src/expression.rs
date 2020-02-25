use std::fmt;

pub enum Expression<'a, V> {
    Variable (&'a str),
    Literal (V),
    FuncArgs(Box<[&'a str]>),
    Function {
        vars: Box<[&'a str]>,
        lambda_term: Box<Expression<'a, V>>,
    },
    Operator {
        tok: &'a str,
        op: Box<dyn Fn(V, V) -> V>,
        lhs_expr: Box<Expression<'a, V>>,
        rhs_expr: Box<Expression<'a, V>>,
    },
    Application{
        func: Box<Expression<'a, V>>,
        inputs: Vec<Expression<'a, V>>,
    },
}

impl<V> Expression<'_, V> {
    pub fn is_variant(&self, other: &Expression<V>) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
} 

impl<V: fmt::Display> fmt::Debug for Expression<'_, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Variable(name) =>
                write!(f, "Var({})", name),
            Expression::Literal(value) =>
                write!(f, "Lit({})", value),
            Expression::FuncArgs(args) =>
                write!(f, "Args({:?})", args),
            Expression::Function {vars, lambda_term} =>
                write!(f, "Func({:?} => {:?})", vars, lambda_term),
            Expression::Operator{tok, op: _, lhs_expr, rhs_expr} =>
                write!(f, "({:?} {} {:?})", lhs_expr, tok, rhs_expr),
            Expression::Application{func, inputs} =>
                write!(f, "App({:?} {:?})", func, inputs),
        }?;
        Ok(())
    }
}

/*
#[allow(dead_code)]
impl<'a, V> Expression<'a, V> {
    pub fn new_var(name: &str) -> Expression<V> {
        Expression::Variable{name}
    }

    pub fn default_var() -> Expression<'a, V> {
        Expression::Variable{name: ""}
    }

    pub fn new_lit(value: V) -> Expression<'a, V> {
        Expression::Literal{value}
    }

    pub fn new_func(vars: Box<Vec<&'a str>>,
        lambda_term: Box<Expression<'a, V>>) -> Expression<'a, V> {
        Expression::Function{vars, lambda_term}
    }

    pub fn default_func() -> Expression<'a, V> {
        Expression::Function{
            vars: Box::new(vec!()),
            lambda_term: Box::new(Expression::default_var())
        }
    }

    pub fn new_op(tok: &'a str,
        op: Box<dyn Fn(V, V) -> V>,
        lhs_expr: Box<Expression<'a, V>>,
        rhs_expr: Box<Expression<'a, V>>) -> Expression<'a, V> {
        Expression::Operator{tok, op, lhs_expr, rhs_expr}
    }

    pub fn default_op() -> Expression<'a, V> {
        Expression::Operator{
            tok: "",
            op: Box::new(|x, _y| x), 
            lhs_expr: Box::new(Expression::default_var()),
            rhs_expr: Box::new(Expression::default_var()),
        }
    }

    pub fn new_app(func: Box<Expression<'a, V>>,
        inputs: Vec<Expression<'a, V>>) -> Expression<'a, V> {
            Expression::Application{func, inputs}
        }

    pub fn default_app() -> Expression<'a, V> {
            Expression::Application{func: Box::new(Expression::default_func()), inputs: vec!()}
        }
}
*/