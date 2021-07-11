use crate::parser::{Expr, Binding, ExprVariable, ExprInteger, ExprFloat, ExprBool, ExprString, ExprAssign, ExprBind, ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprLessThan, ExprLessEquals, ExprEquals, ExprNotEquals, ExprFuzzyEquals, ExprFuzzyNotEquals, ExprGreaterEquals, ExprGreaterThan, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolNot, ExprBoolAnd, ExprBoolOr, ExprParenthesized, ExprBlock, ExprFunctionCall, Separated, ExprFunctionDefinition, BlockBody};
use crate::common::{Value, FunctionImpl, PreTypeInfo};
use crate::scope::{Scopes, BindingId, Scope};
use std::collections::HashMap;
use crate::lexer::{TokenInteger, TokenFloat, TokenBool, TokenDqString, TokenComma};

pub struct Vm<'a, 'i> {
    scopes: Scopes,
    rebo_functions: HashMap<BindingId, &'a ExprBlock<'a, 'i>>,
}

impl<'a, 'i> Vm<'a, 'i> {
    pub fn new(pre_info: PreTypeInfo<'a, 'i>) -> Self {
        let PreTypeInfo { bindings: _, rebo_functions, root_scope } = pre_info;
        let mut scopes = Scopes::new();
        scopes.push_scope(root_scope);
        Vm {
            scopes,
            rebo_functions,
        }
    }

    pub fn run(mut self, ast: &[&Expr]) -> Value {
        trace!("run");
        let mut value = None;
        for expr in ast {
            value = Some(self.eval_expr(expr, 1));
        }
        value.unwrap_or(Value::Unit)
    }


    fn load_binding(&self, binding: &Binding, depth: usize) -> Value {
        trace!("{}load_binding: {}", "|".repeat(depth), binding.ident.ident);
        self.scopes.get(binding.id).unwrap().clone()
    }

    fn bind(&mut self, binding: &Binding, value: Value, depth: usize) -> Value {
        trace!("{}bind {} = {:?}", "|".repeat(depth), binding.ident.ident, value);
        self.scopes.create(binding.id, value);
        Value::Unit
    }

    fn assign(&mut self, binding: &Binding, value: Value, depth: usize) -> Value {
        trace!("{}assign {} = {:?}", "|".repeat(depth), binding.ident.ident, value);
        self.scopes.assign(binding.id, value);
        Value::Unit
    }

    fn eval_expr(&mut self, expr: &Expr, depth: usize) -> Value {
        trace!("{}eval_expr: {}", "|".repeat(depth), expr);
        match expr {
            Expr::Unit(_) => Value::Unit,
            Expr::Variable(ExprVariable { binding, .. }) => self.load_binding(binding, depth+1),
            Expr::Integer(ExprInteger { int: TokenInteger { value, .. } }) => Value::Integer(*value),
            Expr::Float(ExprFloat { float: TokenFloat { value, .. }}) => Value::Float(*value),
            Expr::Bool(ExprBool { b: TokenBool { value, .. } }) => Value::Bool(*value),
            Expr::String(ExprString { string: TokenDqString { string, .. } }) => Value::String(string.clone()),
            Expr::Assign(ExprAssign { variable: ExprVariable { binding, .. }, expr, .. }) => {
                let value = self.eval_expr(expr, depth+1);
                self.assign(binding, value, depth+1)
            },
            Expr::Bind(ExprBind { pattern: ExprPattern::Typed(ExprPatternTyped { pattern: ExprPatternUntyped { binding }, .. }), expr, .. })
            | Expr::Bind(ExprBind { pattern: ExprPattern::Untyped(ExprPatternUntyped { binding }), expr, .. }) => {
                let value = self.eval_expr(expr, depth+1);
                self.bind(binding, value, depth+1)
            },
            Expr::LessThan(ExprLessThan { a, b, .. }) => cmp::<Lt>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::LessEquals(ExprLessEquals { a, b, .. }) => cmp::<Le>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::Equals(ExprEquals { a, b, .. }) => cmp::<Eq>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::NotEquals(ExprNotEquals { a, b, .. }) => cmp::<Neq>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::FuzzyEquals(ExprFuzzyEquals { a, b, .. }) => cmp::<Feq>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::FuzzyNotEquals(ExprFuzzyNotEquals { a, b, .. }) => cmp::<Fneq>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::GreaterEquals(ExprGreaterEquals { a, b, .. }) => cmp::<Ge>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::GreaterThan(ExprGreaterThan { a, b, .. }) => cmp::<Gt>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::Add(ExprAdd { a, b, .. }) => math::<Add>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::Sub(ExprSub { a, b, .. }) => math::<Sub>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::Mul(ExprMul { a, b, .. }) => math::<Mul>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::Div(ExprDiv { a, b, .. }) => math::<Div>(self.eval_expr(a, depth+1), self.eval_expr(b, depth+1)),
            Expr::BoolNot(ExprBoolNot { expr, .. }) => {
                match self.eval_expr(expr, depth+1) {
                    Value::Bool(expr) => Value::Bool(!expr),
                    _ => unreachable!("boolean NOT called with non-bool"),
                }
            }
            Expr::BoolAnd(ExprBoolAnd { a, b, .. }) => {
                let res = self.eval_expr(a, depth+1).expect_bool("boolean AND called with non-bool")
                    && self.eval_expr(b, depth+1).expect_bool("boolean AND called with non-bool");
                Value::Bool(res)
            }
            Expr::BoolOr(ExprBoolOr { a, b, .. }) => {
                let res = self.eval_expr(a, depth+1).expect_bool("boolean OR called with non-bool")
                    || self.eval_expr(b, depth+1).expect_bool("boolean OR called with non-bool");
                Value::Bool(res)
            }
            Expr::Parenthesized(ExprParenthesized { expr, .. }) => self.eval_expr(expr, depth+1),
            Expr::Block(ExprBlock { body: BlockBody { exprs, terminated }, .. }) => {
                let mut val = Value::Unit;
                for expr in exprs {
                    val = self.eval_expr(expr, depth+1);
                }
                if *terminated {
                    Value::Unit
                } else {
                    val
                }
            }
            Expr::FunctionCall(ExprFunctionCall { variable: ExprVariable { binding, .. }, args, .. }) => self.call_function(binding, args, depth+1),
            // ignore function definitions as we have those handled already
            Expr::FunctionDefinition(ExprFunctionDefinition { .. }) => Value::Unit,
        }
    }

    fn call_function(&mut self, binding: &Binding, args: &Separated<&Expr<'_, '_>, TokenComma>, depth: usize) -> Value {
        trace!("{}call_function: {}({:?})", "|".repeat(depth), binding.ident.ident, args);
        let args = args.iter().map(|expr| self.eval_expr(expr, depth+1)).collect();
        match self.load_binding(&binding, depth+1) {
            Value::Function(imp) => match imp {
                FunctionImpl::Rust(f) => f(&mut self.scopes, args),
                FunctionImpl::Rebo(binding_id, arg_binding_ids) => {
                    let mut scope = Scope::new();
                    for (id, val) in arg_binding_ids.into_iter().zip(args) {
                        scope.create(id, val);
                    }
                    self.scopes.push_scope(scope);

                    let mut last = None;
                    for expr in &self.rebo_functions[&binding_id].body.exprs {
                        last = Some(self.eval_expr(expr, depth+1));
                    }

                    self.scopes.pop_scope();
                    last.unwrap_or(Value::Unit)
                }
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

trait CmpOp {
    fn unit() -> bool;
    fn integer(a: i64, b: i64) -> bool;
    fn float(a: f64, b: f64) -> bool;
    fn bool(a: bool, b: bool) -> bool;
    fn string(a: &str, b: &str) -> bool;
    fn str() -> &'static str;
}

enum Lt {}
enum Le {}
enum Eq {}
enum Neq {}
enum Feq {}
enum Fneq {}
enum Ge {}
enum Gt {}

impl CmpOp for Lt {
    fn unit() -> bool { false }
    fn integer(a: i64, b: i64) -> bool { a < b }
    fn float(a: f64, b: f64) -> bool { a < b }
    #[allow(clippy::bool_comparison)]
    fn bool(a: bool, b: bool) -> bool { a < b }
    fn string(a: &str, b: &str) -> bool { a < b }
    fn str() -> &'static str { "<" }
}
impl CmpOp for Le {
    fn unit() -> bool { true }
    fn integer(a: i64, b: i64) -> bool { a <= b }
    fn float(a: f64, b: f64) -> bool { a <= b }
    fn bool(a: bool, b: bool) -> bool { a <= b }
    fn string(a: &str, b: &str) -> bool { a <= b }
    fn str() -> &'static str { "<=" }
}
impl CmpOp for Eq {
    fn unit() -> bool { true }
    fn integer(a: i64, b: i64) -> bool { a == b }
    fn float(_: f64, _: f64) -> bool { unreachable!("can't float == float") }
    fn bool(a: bool, b: bool) -> bool { a == b }
    fn string(a: &str, b: &str) -> bool { a == b }
    fn str() -> &'static str { "==" }
}
impl CmpOp for Neq {
    fn unit() -> bool { false }
    fn integer(a: i64, b: i64) -> bool { a != b }
    fn float(_: f64, _: f64) -> bool { unreachable!("can't float != float") }
    fn bool(a: bool, b: bool) -> bool { a != b }
    fn string(a: &str, b: &str) -> bool { a != b }
    fn str() -> &'static str { "!=" }
}
impl CmpOp for Feq {
    fn unit() -> bool { unreachable!("can't () ~~ ()") }
    fn integer(_: i64, _: i64) -> bool { unreachable!("can't int ~~ int") }
    fn float(a: f64, b: f64) -> bool {
        // https://stackoverflow.com/a/4915891
        let epsilon = 1e-10;
        let abs_a = a.abs();
        let abs_b = b.abs();
        let diff = (a - b).abs();
        #[allow(clippy::float_cmp)]
        if a == b { // shortcut, handles infinities
            true
        } else if a == 0. || b == 0. || diff < f64::MIN_POSITIVE {
            // a or b is zero or both are extremely close to it
            // relative error is less meaningful here
            diff < (epsilon * f64::MIN_POSITIVE)
        } else { // use relative error
            diff / (abs_a + abs_b) < epsilon
        }
    }
    fn bool(_: bool, _: bool) -> bool { unreachable!("can't bool ~~ bool") }
    fn string(a: &str, b: &str) -> bool {
        let a = lexical_sort::iter::iterate_lexical(a);
        let b = lexical_sort::iter::iterate_lexical(b);
        a.zip(b).all(|(a, b)| a == b)
    }
    fn str() -> &'static str { "~~" }
}
impl CmpOp for Fneq {
    fn unit() -> bool { unreachable!("can't () !~ ()") }
    fn integer(_: i64, _: i64) -> bool { unreachable!("can't int !~ int") }
    fn float(a: f64, b: f64) -> bool { !Feq::float(a, b) }
    fn bool(_: bool, _: bool) -> bool { unreachable!("can't bool !~ bool") }
    fn string(a: &str, b: &str) -> bool { !Feq::string(a, b) }
    fn str() -> &'static str { "!~" }
}
impl CmpOp for Ge {
    fn unit() -> bool { true }
    fn integer(a: i64, b: i64) -> bool { a >= b }
    fn float(a: f64, b: f64) -> bool { a >= b }
    fn bool(a: bool, b: bool) -> bool { a >= b }
    fn string(a: &str, b: &str) -> bool { a >= b }
    fn str() -> &'static str { ">=" }
}
impl CmpOp for Gt {
    fn unit() -> bool { false }
    fn integer(a: i64, b: i64) -> bool { a > b }
    fn float(a: f64, b: f64) -> bool { a > b }
    #[allow(clippy::bool_comparison)]
    fn bool(a: bool, b: bool) -> bool { a > b }
    fn string(a: &str, b: &str) -> bool { a > b }
    fn str() -> &'static str { ">" }
}
fn cmp<O: CmpOp>(a: Value, b: Value) -> Value {
    trace!("math: {:?} {} {:?}", a, O::str(), b);
    match (a, b) {
        (Value::Unit, Value::Unit) => Value::Bool(O::unit()),
        (Value::Integer(a), Value::Integer(b)) => Value::Bool(O::integer(a, b)),
        (Value::Float(a), Value::Float(b)) => Value::Bool(O::float(a, b)),
        (Value::Bool(a), Value::Bool(b)) => Value::Bool(O::bool(a, b)),
        (Value::String(a), Value::String(b)) => Value::Bool(O::string(&a, &b)),
        _ => todo!("error handling"),
    }
}
