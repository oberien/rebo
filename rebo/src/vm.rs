use crate::parser::{Expr, ExprType, Binding};
use crate::types::{Value, Function, FunctionType};
use crate::scope::{Scopes, RootScope};

pub struct Vm {
    scopes: Scopes,
}

impl Vm {
    pub fn new(root_scope: RootScope) -> Self {
        Vm {
            scopes: Scopes::new(root_scope),
        }
    }

    pub fn run(mut self, ast: &Vec<&Expr>) -> Value {
        trace!("run");
        let mut value = None;
        for expr in ast {
            value = Some(self.eval_expr(expr));
        }
        return value.unwrap_or(Value::Unit);
    }


    fn load_binding(&self, binding: &Binding) -> Value {
        trace!("load_binding: {}", binding.ident);
        self.scopes.get(binding.id).unwrap().clone()
    }

    fn bind(&mut self, binding: &Binding, value: Value) -> Value {
        trace!("bind {} = {:?}", binding.ident, value);
        self.scopes.create(binding.id, value);
        Value::Unit
    }

    fn assign(&mut self, binding: &Binding, value: Value) -> Value {
        trace!("assign {} = {:?}", binding.ident, value);
        self.scopes.assign(binding.id, value);
        Value::Unit
    }

    fn eval_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr { span: _, typ: ExprType::Unit } => Value::Unit,
            Expr { span: _, typ: ExprType::Variable((binding, _)) } => self.load_binding(binding),
            &Expr { span: _, typ: ExprType::Integer(i) } => Value::Integer(i),
            &Expr { span: _, typ: ExprType::Float(f) } => Value::Float(f),
            Expr { span: _, typ: ExprType::String(s) } => Value::String(s.clone()),
            Expr { span: _, typ: ExprType::Assign((binding, _), expr) } => {
                let value = self.eval_expr(expr);
                self.assign(binding, value)
            },
            Expr { span: _, typ: ExprType::Bind(binding, expr)} => {
                let value = self.eval_expr(expr);
                self.bind(binding, value)
            },
            Expr { span: _, typ: ExprType::Add(a, b) } => math::<Add>(self.eval_expr(a), self.eval_expr(b)),
            Expr { span: _, typ: ExprType::Sub(a, b) } => math::<Sub>(self.eval_expr(a), self.eval_expr(b)),
            Expr { span: _, typ: ExprType::Mul(a, b) } => math::<Mul>(self.eval_expr(a), self.eval_expr(b)),
            Expr { span: _, typ: ExprType::Div(a, b) } => math::<Div>(self.eval_expr(a), self.eval_expr(b)),
            Expr { span: _, typ: ExprType::Statement(expr) } => {
                self.eval_expr(expr);
                Value::Unit
            }
            Expr { span: _, typ: ExprType::FunctionCall((binding, _), args) } => self.call_function(binding, args),
        }
    }

    fn call_function(&mut self, binding: &Binding, args: &Vec<&Expr>) -> Value {
        trace!("call_function: {}({:?})", binding.ident, args);
        let args = args.iter().map(|expr| self.eval_expr(expr)).collect();
        match self.load_binding(&binding) {
            Value::Function(Function { typ: FunctionType::Rust(f), .. }) => f(&mut self.scopes, args),
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
