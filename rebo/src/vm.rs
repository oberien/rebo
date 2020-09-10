use crate::parser::{Expr, ExprType, Ast};
use crate::types::{Value, Function, FunctionType};
use crate::scope::Scope;

pub fn run(scope: &mut Scope, ast: &Ast) -> Value {
    trace!("run");
    let mut value = None;
    for expr in &ast.exprs {
        value = Some(eval_expr(scope, expr));
    }
    return value.unwrap_or(Value::Unit);
}

fn eval_expr(scope: &mut Scope, expr: &Expr) -> Value {
    match expr {
        Expr { span: _, typ: ExprType::Ident(ident) } => load_ident(scope, ident),
        &Expr { span: _, typ: ExprType::Integer(i) } => Value::Integer(i),
        &Expr { span: _, typ: ExprType::Float(f) } => Value::Float(f),
        Expr { span: _, typ: ExprType::String(s) } => Value::String(s.clone()),
        &Expr { span: _, typ: ExprType::Assign((ident, _), expr) }
        | &Expr { span: _, typ: ExprType::Bind((ident, _), _, expr)} => {
            let value = eval_expr(scope, expr);
            assign(scope, ident.to_string(), value)
        },
        Expr { span: _, typ: ExprType::Add(a, b) } => math::<Add>(eval_expr(scope, a), eval_expr(scope, b)),
        Expr { span: _, typ: ExprType::Sub(a, b) } => math::<Sub>(eval_expr(scope, a), eval_expr(scope, b)),
        Expr { span: _, typ: ExprType::Mul(a, b) } => math::<Mul>(eval_expr(scope, a), eval_expr(scope, b)),
        Expr { span: _, typ: ExprType::Div(a, b) } => math::<Div>(eval_expr(scope, a), eval_expr(scope, b)),
        Expr { span: _, typ: ExprType::Statement(expr) } => {
            eval_expr(scope, expr);
            Value::Unit
        }
        Expr { span: _, typ: ExprType::FunctionCall((name, _), args) } => call_function(scope, name, args),
    }
}

fn load_ident(scope: &mut Scope, ident: &str) -> Value {
    trace!("load_ident: {}", ident);
    scope.get(ident).unwrap().clone()
}

fn assign(scope: &mut Scope, ident: String, value: Value) -> Value {
    trace!("assign {} = {:?}", ident, value);
    scope.set(ident, value);
    Value::Unit
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

fn call_function(scope: &mut Scope, name: &str, args: &Vec<&Expr>) -> Value {
    trace!("call_function: {}({:?})", name, args);
    let args = args.iter().map(|expr| eval_expr(scope, expr)).collect();
    match scope.get(name).unwrap() {
        Value::Function(Function { typ: FunctionType::Rust(f), .. }) => f(scope, args),
        _ => todo!("error handling"),
    }
}