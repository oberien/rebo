use crate::parser::{Expr, ExprType, Binding};
use crate::common::{Value, FunctionImpl};
use crate::scope::{Scopes, Scope};

pub struct Vm {
    scopes: Scopes,
}

impl Vm {
    pub fn new(root_scope: Scope) -> Self {
        let mut scopes = Scopes::new();
        scopes.push_scope(root_scope);
        Vm {
            scopes,
        }
    }

    pub fn run(mut self, ast: &Vec<&Expr>) -> Value {
        trace!("run");
        let mut value = None;
        for expr in ast {
            value = Some(self.eval_expr(expr, 1));
        }
        return value.unwrap_or(Value::Unit);
    }


    fn load_binding(&self, binding: &Binding, depth: usize) -> Value {
        trace!("{}load_binding: {}", "|".repeat(depth), binding.ident);
        self.scopes.get(binding.id).unwrap().clone()
    }

    fn bind(&mut self, binding: &Binding, value: Value, depth: usize) -> Value {
        trace!("{}bind {} = {:?}", "|".repeat(depth), binding.ident, value);
        self.scopes.create(binding.id, value);
        Value::Unit
    }

    fn assign(&mut self, binding: &Binding, value: Value, depth: usize) -> Value {
        trace!("{}assign {} = {:?}", "|".repeat(depth), binding.ident, value);
        self.scopes.assign(binding.id, value);
        Value::Unit
    }

    fn eval_expr(&mut self, expr: &Expr, depth: usize) -> Value {
        trace!("{}eval_expr: {}", "|".repeat(depth), expr);
        match expr {
            Expr { span: _, typ: ExprType::Unit } => Value::Unit,
            Expr { span: _, typ: ExprType::Variable(binding) } => self.load_binding(binding, depth+1),
            &Expr { span: _, typ: ExprType::Integer(i) } => Value::Integer(i),
            &Expr { span: _, typ: ExprType::Float(f) } => Value::Float(f),
            &Expr { span: _, typ: ExprType::Bool(b) } => Value::Bool(b),
            Expr { span: _, typ: ExprType::String(s) } => Value::String(s.clone()),
            Expr { span: _, typ: ExprType::Assign((binding, _), expr) } => {
                let value = self.eval_expr(expr, depth+1);
                self.assign(binding, value, depth+1)
            },
            Expr { span: _, typ: ExprType::Bind(binding, expr)} => {
                let value = self.eval_expr(expr, depth+1);
                self.bind(binding, value, depth+1)
            },
            Expr { span: _, typ: ExprType::Equals(left, right) } => match (self.eval_expr(left, depth+1), self.eval_expr(right, depth+1)) {
                (Value::Unit, Value::Unit) => Value::Bool(true),
                (Value::Integer(l), Value::Integer(r)) => Value::Bool(l == r),
                (Value::Float(l), Value::Float(r)) => Value::Bool(l == r),
                (Value::Bool(l), Value::Bool(r)) => Value::Bool(l == r),
                (Value::String(l), Value::String(r)) => Value::Bool(l == r),
                (Value::Function(FunctionImpl::Rust(l)), Value::Function(FunctionImpl::Rust(r))) => Value::Bool(l as usize == r as usize),
                _ => unreachable!("can't compare different types"),
            }
            Expr { span: _, typ: ExprType::Add(a, b) } => math::<Add>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr { span: _, typ: ExprType::Sub(a, b) } => math::<Sub>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr { span: _, typ: ExprType::Mul(a, b) } => math::<Mul>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr { span: _, typ: ExprType::Div(a, b) } => math::<Div>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr { span: _, typ: ExprType::BoolNot(b) } => {
                match self.eval_expr(b, depth+1) {
                    Value::Bool(b) => Value::Bool(!b),
                    _ => unreachable!("boolean NOT called with non-bool"),
                }
            }
            Expr { span: _, typ: ExprType::BoolAnd(a, b) } => {
                let res = self.eval_expr(a, depth+1).expect_bool("boolean AND called with non-bool")
                    && self.eval_expr(b, depth+1).expect_bool("boolean AND called with non-bool");
                Value::Bool(res)
            }
            Expr { span: _, typ: ExprType::BoolOr(a, b) } => {
                let res = self.eval_expr(a, depth+1).expect_bool("boolean OR called with non-bool")
                    || self.eval_expr(b, depth+1).expect_bool("boolean OR called with non-bool");
                Value::Bool(res)
            }
            Expr { span: _, typ: ExprType::Statement(expr) } => {
                self.eval_expr(expr, depth+1);
                Value::Unit
            }
            Expr { span: _, typ: ExprType::Parenthezised(expr) } => self.eval_expr(expr, depth+1),
            Expr { span: _, typ: ExprType::Block(exprs) } => {
                let mut val = Value::Unit;
                for expr in exprs {
                    val = self.eval_expr(expr, depth+1);
                }
                val
            }
            Expr { span: _, typ: ExprType::FunctionCall((binding, _), args) } => self.call_function(binding, args, depth+1),
        }
    }

    fn call_function(&mut self, binding: &Binding, args: &Vec<&Expr>, depth: usize) -> Value {
        trace!("{}call_function: {}({:?})", "|".repeat(depth), binding.ident, args);
        let args = args.iter().map(|expr| self.eval_expr(expr, depth+1)).collect();
        match self.load_binding(&binding, depth+1) {
            Value::Function(imp) => match imp {
                FunctionImpl::Rust(f) => f(&mut self.scopes, args),
            },
            _ => unreachable!("call_function called with a binding that isn't a function"),
        }
    }
}

trait MathOp {
    fn integer(a: i64, b: i64) -> i64;
    fn float(a: f64, b: f64) -> f64;
    fn str() -> &'static str;
}

enum Add {}
enum Sub {}
enum Mul {}
enum Div {}

impl MathOp for Add {
    fn integer(a: i64, b: i64) -> i64 { a + b }
    fn float(a: f64, b: f64) -> f64 { a + b }
    fn str() -> &'static str { "+" }
}
impl MathOp for Sub {
    fn integer(a: i64, b: i64) -> i64 { a - b }
    fn float(a: f64, b: f64) -> f64 { a - b }
    fn str() -> &'static str { "-" }
}
impl MathOp for Mul {
    fn integer(a: i64, b: i64) -> i64 { a * b }
    fn float(a: f64, b: f64) -> f64 { a * b }
    fn str() -> &'static str { "*" }
}
impl MathOp for Div {
    fn integer(a: i64, b: i64) -> i64 { a / b }
    fn float(a: f64, b: f64) -> f64 { a / b }
    fn str() -> &'static str { "/" }
}

fn math<O: MathOp>(a: Value, b: Value) -> Value {
    trace!("math: {:?} {} {:?}", a, O::str(), b);
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Value::Integer(O::integer(a, b)),
        (Value::Float(a), Value::Float(b)) => Value::Float(O::float(a, b)),
        _ => todo!("error handling"),
    }
}
