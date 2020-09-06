use std::collections::HashMap;
use std::mem;

use crate::parser::{Expr, ExprType, Ast};

struct Scope<'i> {
    parent: Option<Box<Scope<'i>>>,
    variables: HashMap<&'i str, Value>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Integer(i64),
    Float(f64),
    String(String),
    Function(Function),
}

#[derive(Debug, Clone)]
enum Type {
    Unit,
    Integer,
    Float,
    String,
    Function,
    Any,
    Varargs,
}

#[derive(Debug, Clone)]
pub struct Function {
    arg_types: Vec<Type>,
    return_type: Type,
    code: FunctionCode,
}

#[derive(Debug, Clone)]
pub enum FunctionCode {
    Rust(fn(Vec<Value>) -> Value),
}

impl<'i> Default for Scope<'i> {
    fn default() -> Scope<'i> {
        let mut vars = HashMap::new();
        vars.insert("print", Value::Function(Function {
            arg_types: vec![Type::Varargs],
            return_type: Type::Unit,
            code: FunctionCode::Rust(print),
        }));

        Scope {
            parent: None,
            variables: vars,
        }
    }
}

impl<'i> Scope<'i> {
    fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(name).or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }
    fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        let parent = self.parent.as_mut();
        self.variables.get_mut(name).or_else(|| parent.and_then(|parent| parent.get_mut(name)))
    }
    fn set(&mut self, name: &'i str, value: Value) -> Option<Value> {
        match self.get_mut(name) {
            Some(val) => Some(mem::replace(val, value)),
            None => self.variables.insert(name, value),
        }
    }
}

fn print(vals: Vec<Value>) -> Value {
    for val in vals {
        match val {
            Value::Unit => print!("{:<8?}", ()),
            Value::Integer(i) => print!("{:<8}", i),
            Value::Float(i) => print!("{:<8}", i),
            Value::String(s) => print!("{:<8?}", s),
            Value::Function(i) => todo!("function print representation"),
        }
    }
    println!();
    Value::Unit
}

impl From<&'_ Value> for Type {
    fn from(val: &Value) -> Self {
        match val {
            Value::Unit => Type::Unit,
            Value::Integer(_) => Type::Integer,
            Value::Float(_) => Type::Float,
            Value::String(_) => Type::String,
            Value::Function(_) => Type::Function,
        }
    }
}

pub fn run(ast: &Ast) -> Value {
    trace!("run");
    let mut scope = Scope::default();
    let mut value = None;
    for expr in &ast.exprs {
        value = Some(eval_expr(&mut scope, expr));
    }
    return value.unwrap_or(Value::Unit);
}

fn eval_expr<'i>(scope: &mut Scope<'i>, expr: &Expr<'_, 'i>) -> Value {
    match expr {
        Expr { span, typ: ExprType::Ident(ident) } => load_ident(scope, ident),
        &Expr { span, typ: ExprType::Integer(i) } => Value::Integer(i),
        &Expr { span, typ: ExprType::Float(f) } => Value::Float(f),
        Expr { span, typ: ExprType::String(s) } => Value::String(s.clone()),
        &Expr { span, typ: ExprType::Assign((ident, ident_span), expr) }
        | &Expr { span, typ: ExprType::Bind((ident, ident_span), _, expr)} => {
            let value = eval_expr(scope, expr);
            assign(scope, ident, value)
        },
        Expr { span, typ: ExprType::Add(a, b) } => math::<Add>(eval_expr(scope, a), eval_expr(scope, b)),
        Expr { span, typ: ExprType::Sub(a, b) } => math::<Sub>(eval_expr(scope, a), eval_expr(scope, b)),
        Expr { span, typ: ExprType::Mul(a, b) } => math::<Mul>(eval_expr(scope, a), eval_expr(scope, b)),
        Expr { span, typ: ExprType::Div(a, b) } => math::<Div>(eval_expr(scope, a), eval_expr(scope, b)),
        Expr { span, typ: ExprType::Statement(expr) } => {
            eval_expr(scope, expr);
            Value::Unit
        }
        Expr { span, typ: ExprType::FunctionCall((name, fspan), args) } => call_function(scope, name, args),
    }
}

fn load_ident(scope: &mut Scope, ident: &str) -> Value {
    trace!("load_ident: {}", ident);
    scope.get(ident).unwrap().clone()
}

fn assign<'i>(scope: &mut Scope<'i>, ident: &'i str, value: Value) -> Value {
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

fn call_function<'i>(scope: &mut Scope<'i>, name: &str, args: &Vec<&Expr<'_, 'i>>) -> Value {
    trace!("call_function: {}({:?})", name, args);
    let args = args.iter().map(|expr| eval_expr(scope, expr)).collect();
    match scope.variables[name] {
        Value::Function(Function { code: FunctionCode::Rust(f), .. }) => f(args),
        _ => todo!("error handling"),
    }
}