use crate::parser::{Expr, Binding, ExprVariable, ExprInteger, ExprFloat, ExprBool, ExprString, ExprAssign, ExprBind, ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprLessThan, ExprLessEquals, ExprEquals, ExprNotEquals, ExprGreaterEquals, ExprGreaterThan, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolNot, ExprBoolAnd, ExprBoolOr, ExprParenthesized, ExprBlock, ExprFunctionCall, Separated, ExprFunctionDefinition, BlockBody, ExprStructDefinition, ExprStructInitialization, ExprAssignLhs, ExprFieldAccess, ExprIfElse};
use crate::common::{Value, FunctionImpl, PreInfo, Depth, Struct, StructType, FuzzyFloat, StructArc};
use crate::scope::{Scopes, BindingId, Scope};
use crate::lexer::{TokenInteger, TokenFloat, TokenBool, TokenDqString, TokenComma, TokenIdent};
use indexmap::map::IndexMap;
use std::sync::Arc;
use diagnostic::Span;
use parking_lot::ReentrantMutex;
use std::cell::RefCell;

pub struct Vm<'a, 'i> {
    scopes: Scopes,
    rebo_functions: IndexMap<BindingId, &'a ExprFunctionDefinition<'a, 'i>>,
    structs: IndexMap<&'i str, (StructType, Span)>,
}

impl<'a, 'i> Vm<'a, 'i> {
    pub fn new(pre_info: PreInfo<'a, 'i>) -> Self {
        let PreInfo { bindings: _, rebo_functions, structs, root_scope } = pre_info;
        let mut scopes = Scopes::new();
        scopes.push_scope(root_scope);
        Vm {
            scopes,
            structs,
            rebo_functions,
        }
    }

    pub fn run(mut self, ast: &[&Expr]) -> Value {
        trace!("run");
        let mut value = None;
        for expr in ast {
            value = Some(self.eval_expr(expr, Depth::start()));
        }
        value.unwrap_or(Value::Unit)
    }


    fn load_binding(&self, binding: &Binding, depth: Depth) -> Value {
        trace!("{}load_binding: {}", depth, binding.ident.ident);
        self.scopes.get(binding.id).unwrap().clone()
    }

    fn bind(&mut self, binding: &Binding, value: Value, depth: Depth) -> Value {
        trace!("{}bind {} = {:?}", depth, binding.ident.ident, value);
        self.scopes.create(binding.id, value);
        Value::Unit
    }

    fn assign(&mut self, binding: &Binding, value: Value, depth: Depth) {
        trace!("{}assign {} = {:?}", depth, binding.ident.ident, value);
        self.scopes.assign(binding.id, value);
    }

    fn eval_expr(&mut self, expr: &Expr, depth: Depth) -> Value {
        trace!("{}eval_expr: {}", depth, expr);
        match expr {
            Expr::Unit(_) => Value::Unit,
            Expr::Variable(ExprVariable { binding, .. }) => self.load_binding(binding, depth.last()),
            Expr::FieldAccess(ExprFieldAccess { variable: ExprVariable { binding, .. }, fields, .. }) => {
                let mut struct_arc = match self.load_binding(binding, depth.last()) {
                    Value::Struct(s) => s,
                    _ => unreachable!("typechecker ensures this is a struct"),
                };
                for TokenIdent { ident, .. } in fields {
                    let field_value = struct_arc.s.lock().borrow_mut().fields.iter()
                        .filter(|(name, _value)| name == ident)
                        .map(|(_name, value)| value.clone())
                        .next()
                        .expect("typechecker ensures all fields exist");
                    match field_value {
                        Value::Struct(s) => struct_arc = s,
                        value => return value,
                    }
                }
                Value::Struct(struct_arc)
            },
            Expr::Integer(ExprInteger { int: TokenInteger { value, .. } }) => Value::Integer(*value),
            Expr::Float(ExprFloat { float: TokenFloat { value, .. }}) => Value::Float(FuzzyFloat(*value)),
            Expr::Bool(ExprBool { b: TokenBool { value, .. } }) => Value::Bool(*value),
            Expr::String(ExprString { string: TokenDqString { string, .. } }) => Value::String(string.clone()),
            Expr::Assign(ExprAssign { lhs, expr, .. }) => {
                let value = self.eval_expr(expr, depth.next());
                match lhs {
                    ExprAssignLhs::Variable(ExprVariable { binding, .. }) => self.assign(binding, value, depth.last()),
                    ExprAssignLhs::FieldAccess(ExprFieldAccess { variable: ExprVariable { binding, .. }, fields, .. }) => {

                        let mut struct_arc = match self.load_binding(&binding, depth.next()) {
                            Value::Struct(s) => s,
                            _ => unreachable!("typechecker should have ensured that this is a struct"),
                        };
                        for TokenIdent { ident, .. } in fields {
                            let new_struct = {
                                let s = struct_arc.s.lock();
                                let mut s = s.borrow_mut();
                                let field_value = s.fields.iter_mut()
                                    .filter(|(name, _value)| name == ident)
                                    .map(|(_name, value)| value)
                                    .next()
                                    .expect("typechecker ensured that all fields exist");
                                match field_value {
                                    Value::Struct(s) => s.clone(),
                                    var => {
                                        *var = value.clone();
                                        break;
                                    },
                                }
                            };
                            struct_arc = new_struct;
                        }
                    }
                }
                Value::Unit
            },
            Expr::Bind(ExprBind { pattern: ExprPattern::Typed(ExprPatternTyped { pattern: ExprPatternUntyped { binding }, .. }), expr, .. })
            | Expr::Bind(ExprBind { pattern: ExprPattern::Untyped(ExprPatternUntyped { binding }), expr, .. }) => {
                let value = self.eval_expr(expr, depth.next());
                self.bind(binding, value, depth.last())
            },
            Expr::LessThan(ExprLessThan { a, b, .. }) => cmp::<Lt>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::LessEquals(ExprLessEquals { a, b, .. }) => cmp::<Le>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::Equals(ExprEquals { a, b, .. }) => cmp::<Eq>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::NotEquals(ExprNotEquals { a, b, .. }) => cmp::<Neq>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::GreaterEquals(ExprGreaterEquals { a, b, .. }) => cmp::<Ge>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::GreaterThan(ExprGreaterThan { a, b, .. }) => cmp::<Gt>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::Add(ExprAdd { a, b, .. }) => math::<Add>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::Sub(ExprSub { a, b, .. }) => math::<Sub>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::Mul(ExprMul { a, b, .. }) => math::<Mul>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::Div(ExprDiv { a, b, .. }) => math::<Div>(self.eval_expr(a, depth.next()), self.eval_expr(b, depth.next()), depth.last()),
            Expr::BoolNot(ExprBoolNot { expr, .. }) => {
                match self.eval_expr(expr, depth.last()) {
                    Value::Bool(expr) => Value::Bool(!expr),
                    _ => unreachable!("boolean NOT called with non-bool"),
                }
            }
            Expr::BoolAnd(ExprBoolAnd { a, b, .. }) => {
                let res = self.eval_expr(a, depth.next()).expect_bool("boolean AND called with non-bool")
                    && self.eval_expr(b, depth.last()).expect_bool("boolean AND called with non-bool");
                Value::Bool(res)
            }
            Expr::BoolOr(ExprBoolOr { a, b, .. }) => {
                let res = self.eval_expr(a, depth.next()).expect_bool("boolean OR called with non-bool")
                    || self.eval_expr(b, depth.last()).expect_bool("boolean OR called with non-bool");
                Value::Bool(res)
            }
            Expr::Parenthesized(ExprParenthesized { expr, .. }) => self.eval_expr(expr, depth.last()),
            Expr::Block(block) => {
                self.eval_block(block, depth)
            }
            Expr::IfElse(ExprIfElse { condition, then, else_ifs, els, .. }) => {
                let branches = ::std::iter::once((condition, then))
                    .chain(else_ifs.iter().map(|(_, _, cond, block)| (cond, block)));
                for (cond, block) in branches {
                    let cond = self.eval_expr(cond, depth.next()).expect_bool("if-condition doesn't evaluate to bool");
                    if cond {
                        return self.eval_block(block, depth.last());
                    }
                }
                if let Some((_, block)) = els {
                    return self.eval_block(block, depth.last());
                }
                Value::Unit
            }
            Expr::FunctionCall(ExprFunctionCall { variable: ExprVariable { binding, .. }, args, .. }) => self.call_function(binding, args, depth.last()),
            // ignore function definitions as we have those handled already
            Expr::FunctionDefinition(ExprFunctionDefinition { .. }) => Value::Unit,
            Expr::StructDefinition(ExprStructDefinition { .. }) => Value::Unit,
            Expr::StructInitialization(ExprStructInitialization { name, fields, .. }) => {
                let mut field_values = Vec::new();
                for (field, _colon, expr) in fields {
                    field_values.push((field.ident.to_string(), self.eval_expr(expr, depth.next())));
                }
                // TODO: O(n²log(n)) probably isn't the best but we need the correct order of the type-definition for comparisons
                let (typ, _) = &self.structs[name.ident];
                field_values.sort_by_key(|(field, _)| typ.fields.iter().position(|(name, _)| field == name));
                Value::Struct(StructArc { s: Arc::new(ReentrantMutex::new(RefCell::new(Struct {
                    name: name.ident.to_string(),
                    fields: field_values,
                })))})
            }
        }
    }

    fn eval_block(&mut self, ExprBlock { body: BlockBody { exprs, terminated }, .. }: &ExprBlock, depth: Depth) -> Value {
        let mut val = Value::Unit;
        for expr in exprs {
            val = self.eval_expr(expr, depth.next());
        }
        if *terminated {
            Value::Unit
        } else {
            val
        }
    }
    fn call_function(&mut self, binding: &Binding, args: &Separated<&Expr<'_, '_>, TokenComma>, depth: Depth) -> Value {
        trace!("{}call_function: {}({:?})", depth, binding.ident.ident, args);
        let args = args.iter().map(|expr| self.eval_expr(expr, depth.next())).collect();
        match self.load_binding(&binding, depth.next()) {
            Value::Function(imp) => match imp {
                FunctionImpl::Rust(f) => f(&mut self.scopes, args),
                FunctionImpl::Rebo(binding_id, arg_binding_ids) => {
                    let mut scope = Scope::new();
                    for (id, val) in arg_binding_ids.into_iter().zip(args) {
                        scope.create(id, val);
                    }
                    self.scopes.push_scope(scope);

                    let mut last = None;
                    for expr in &self.rebo_functions[&binding_id].body.body.exprs {
                        last = Some(self.eval_expr(expr, depth.next()));
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
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> FuzzyFloat;
    fn str() -> &'static str;
}

enum Add {}
enum Sub {}
enum Mul {}
enum Div {}

impl MathOp for Add {
    fn integer(a: i64, b: i64) -> i64 { a + b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> FuzzyFloat { a + b }
    fn str() -> &'static str { "+" }
}
impl MathOp for Sub {
    fn integer(a: i64, b: i64) -> i64 { a - b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> FuzzyFloat { a - b }
    fn str() -> &'static str { "-" }
}
impl MathOp for Mul {
    fn integer(a: i64, b: i64) -> i64 { a * b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> FuzzyFloat { a * b }
    fn str() -> &'static str { "*" }
}
impl MathOp for Div {
    fn integer(a: i64, b: i64) -> i64 { a / b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> FuzzyFloat { a / b }
    fn str() -> &'static str { "/" }
}

fn math<O: MathOp>(a: Value, b: Value, depth: Depth) -> Value {
    trace!("{} math: {:?} {} {:?}", depth, a, O::str(), b);
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Value::Integer(O::integer(a, b)),
        (Value::Float(a), Value::Float(b)) => Value::Float(O::float(a, b)),
        _ => todo!("error handling"),
    }
}

trait CmpOp {
    fn unit() -> bool;
    fn integer(a: i64, b: i64) -> bool;
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool;
    fn bool(a: bool, b: bool) -> bool;
    fn string(a: &str, b: &str) -> bool;
    fn structs(a: StructArc, b: StructArc) -> bool;
    fn str() -> &'static str;
}

enum Lt {}
enum Le {}
enum Eq {}
enum Neq {}
enum Ge {}
enum Gt {}

impl CmpOp for Lt {
    fn unit() -> bool { false }
    fn integer(a: i64, b: i64) -> bool { a < b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { a < b }
    #[allow(clippy::bool_comparison)]
    fn bool(a: bool, b: bool) -> bool { a < b }
    fn string(a: &str, b: &str) -> bool { a < b }
    fn structs(a: StructArc, b: StructArc) -> bool { a < b }
    fn str() -> &'static str { "<" }
}
impl CmpOp for Le {
    fn unit() -> bool { true }
    fn integer(a: i64, b: i64) -> bool { a <= b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { a <= b }
    fn bool(a: bool, b: bool) -> bool { a <= b }
    fn string(a: &str, b: &str) -> bool { a <= b }
    fn structs(a: StructArc, b: StructArc) -> bool { a <= b }
    fn str() -> &'static str { "<=" }
}
impl CmpOp for Eq {
    fn unit() -> bool { true }
    fn integer(a: i64, b: i64) -> bool { a == b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { a == b }
    fn bool(a: bool, b: bool) -> bool { a == b }
    fn string(a: &str, b: &str) -> bool { a == b }
    fn structs(a: StructArc, b: StructArc) -> bool { a == b }
    fn str() -> &'static str { "==" }
}
impl CmpOp for Neq {
    fn unit() -> bool { false }
    fn integer(a: i64, b: i64) -> bool { a != b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { !Eq::float(a, b) }
    fn bool(a: bool, b: bool) -> bool { a != b }
    fn string(a: &str, b: &str) -> bool { a != b }
    fn structs(a: StructArc, b: StructArc) -> bool { a != b }
    fn str() -> &'static str { "!=" }
}
impl CmpOp for Ge {
    fn unit() -> bool { true }
    fn integer(a: i64, b: i64) -> bool { a >= b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { a >= b }
    fn bool(a: bool, b: bool) -> bool { a >= b }
    fn string(a: &str, b: &str) -> bool { a >= b }
    fn structs(a: StructArc, b: StructArc) -> bool { a >= b }
    fn str() -> &'static str { ">=" }
}
impl CmpOp for Gt {
    fn unit() -> bool { false }
    fn integer(a: i64, b: i64) -> bool { a > b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { a > b }
    #[allow(clippy::bool_comparison)]
    fn bool(a: bool, b: bool) -> bool { a > b }
    fn string(a: &str, b: &str) -> bool { a > b }
    fn structs(a: StructArc, b: StructArc) -> bool { a > b }
    fn str() -> &'static str { ">" }
}
fn cmp<O: CmpOp>(a: Value, b: Value, depth: Depth) -> Value {
    trace!("{} cmp: {:?} {} {:?}", depth, a, O::str(), b);
    match (a, b) {
        (Value::Unit, Value::Unit) => Value::Bool(O::unit()),
        (Value::Integer(a), Value::Integer(b)) => Value::Bool(O::integer(a, b)),
        (Value::Float(a), Value::Float(b)) => Value::Bool(O::float(a, b)),
        (Value::Bool(a), Value::Bool(b)) => Value::Bool(O::bool(a, b)),
        (Value::String(a), Value::String(b)) => Value::Bool(O::string(&a, &b)),
        (Value::Struct(a), Value::Struct(b)) => Value::Bool(O::structs(a, b)),
        _ => todo!("error handling"),
    }
}
