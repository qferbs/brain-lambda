use std::fmt;

pub enum Expression<'a, V> {
    Variable(&'a str),
    Literal(V),
    FuncArgs(Box<[&'a str]>),
    Function {
        vars: Box<[&'a str]>,
        lambda_term: Box<Expression<'a, V>>,
    },
    Operator {
        op: &'a str,
        lhs_expr: Box<Expression<'a, V>>,
        rhs_expr: Box<Expression<'a, V>>,
    },
    Application {
        func: Box<Expression<'a, V>>,
        inputs: Vec<Expression<'a, V>>,
    },
}

#[allow(dead_code)]
impl<V> Expression<'_, V> {
    pub fn is_variant(&self, other: &Expression<V>) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl<V: fmt::Display> fmt::Debug for Expression<'_, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Variable(name) => write!(f, "Var({})", name),
            Expression::Literal(value) => write!(f, "Lit({})", value),
            Expression::FuncArgs(args) => write!(f, "Args({:?})", args),
            Expression::Function { vars, lambda_term } => {
                write!(f, "Func({:?} => {:?})", vars, lambda_term)
            }
            Expression::Operator {
                op,
                lhs_expr,
                rhs_expr,
                ..
            } => write!(f, "({:?} {} {:?})", lhs_expr, op, rhs_expr),
            Expression::Application { func, inputs } => {
                write!(f, "App({:?} {:?})", func, inputs)
            }
        }?;
        Ok(())
    }
}
