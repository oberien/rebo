use std::borrow::Cow;
use std::cell::RefCell;
use std::sync::Arc;

use indexmap::map::IndexMap;
use parking_lot::ReentrantMutex;

use crate::common::{Depth, Enum, EnumArc, Function, FuzzyFloat, MetaInfo, Struct, StructArc, Value};
use crate::lexer::{TokenBool, TokenDqString, TokenFloat, TokenIdent, TokenInteger};
use crate::parser::{Binding, BlockBody, Expr, ExprAdd, ExprAssign, ExprAssignLhs, ExprBind, ExprBlock, ExprBool, ExprBoolAnd, ExprBoolNot, ExprBoolOr, ExprDiv, ExprEnumDefinition, ExprEnumInitialization, ExprEquals, ExprFieldAccess, ExprFloat, ExprFormatString, ExprFormatStringPart, ExprFunctionCall, ExprFunctionDefinition, ExprGreaterEquals, ExprGreaterThan, ExprIfElse, ExprInteger, ExprLessEquals, ExprLessThan, ExprLiteral, ExprMatch, ExprMatchPattern, ExprMul, ExprNotEquals, ExprParenthesized, ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprString, ExprStructDefinition, ExprStructInitialization, ExprSub, ExprVariable, ExprWhile, ExprAccess, FieldOrMethod, Spanned};
use crate::typeck::types::StructType;
pub use crate::vm::scope::{Scopes, Scope};
use diagnostic::{Diagnostics, Span};
use std::ops::{Deref, DerefMut};

mod scope;

pub struct Vm<'a, 'i> {
    functions: IndexMap<Cow<'i, str>, Function>,
    ctx: VmContext<'a, 'i>,
}

impl<'a, 'i> Deref for Vm<'a, 'i> {
    type Target = VmContext<'a, 'i>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}
impl<'a, 'i> DerefMut for Vm<'a, 'i> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ctx
    }
}

pub struct VmContext<'a, 'i> {
    diagnostics: &'i Diagnostics,
    scopes: Scopes,
    rebo_functions: IndexMap<Cow<'i, str>, &'a ExprFunctionDefinition<'a, 'i>>,
    struct_types: IndexMap<&'i str, StructType>,
}

impl<'a, 'i> VmContext<'a, 'i> {
    pub fn scopes(&mut self) -> &mut Scopes {
        &mut self.scopes
    }

    pub fn diagnostics(&self) -> &Diagnostics {
        self.diagnostics
    }
}

impl<'a, 'i> Vm<'a, 'i> {
    pub fn new(diagnostics: &'i Diagnostics, meta_info: MetaInfo<'a, 'i>) -> Self {
        let MetaInfo { functions, rebo_functions, user_types: _, function_types: _, struct_types, enum_types: _, types: _ } = meta_info;
        let mut scopes = Scopes::new();
        let root_scope = Scope::new();
        scopes.push_scope(root_scope);
        Vm { functions, ctx: VmContext { diagnostics, scopes, rebo_functions, struct_types } }
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
        self.scopes.get(binding.id).unwrap_or_else(|| panic!("binding {}[{}] is None", binding.ident.ident, binding.id)).clone()
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
            Expr::Literal(ExprLiteral::Unit(_)) => Value::Unit,
            Expr::Literal(ExprLiteral::Integer(ExprInteger { int: TokenInteger { value, .. } })) => Value::Integer(*value),
            Expr::Literal(ExprLiteral::Float(ExprFloat { float: TokenFloat { value, .. }})) => Value::Float(FuzzyFloat(*value)),
            Expr::Literal(ExprLiteral::Bool(ExprBool { b: TokenBool { value, .. } })) => Value::Bool(*value),
            Expr::Literal(ExprLiteral::String(ExprString { string: TokenDqString { string, .. } })) => Value::String(string.clone()),
            Expr::Variable(ExprVariable { binding, .. }) => self.load_binding(binding, depth.last()),
            Expr::Access(ExprAccess { variable: ExprVariable { binding, .. }, accesses, .. }) => {
                let mut val = self.load_binding(binding, depth.next());
                for acc in accesses {
                    val = match acc {
                        FieldOrMethod::Field(field) => {
                            let struct_arc = match val {
                                Value::Struct(s) => s,
                                val => unreachable!("typechecker ensures this is a struct: {:?}", val),
                            };
                            let val = struct_arc.s.lock().borrow_mut().fields.iter()
                                .filter(|(name, _value)| name == field.ident)
                                .map(|(_name, value)| value.clone())
                                .next()
                                .expect("typechecker ensures all fields exist");
                            val
                        },
                        FieldOrMethod::Method(ExprFunctionCall { name, args, .. }) => {
                            let fn_name = format!("{}::{}", val.type_name(), name.ident);
                            let args = std::iter::once(val)
                                .chain(args.iter().map(|expr| self.eval_expr(expr, depth.next())))
                                .collect();
                            self.call_function(&fn_name, acc.span(), args, depth.last())
                        }
                    };
                }
                val
            }
            Expr::FormatString(ExprFormatString { parts, .. }) => {
                let mut res = String::new();
                for part in parts {
                    match part {
                        ExprFormatStringPart::Str(s) => res.push_str(s),
                        ExprFormatStringPart::Escaped(s) => res.push_str(s),
                        ExprFormatStringPart::FmtArg(expr) => {
                            let val = self.eval_expr(expr, depth.next());
                            res.push_str(&val.to_string());
                        }
                    }
                }
                Value::String(res)
            },
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
                                        *var = value;
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
            Expr::Match(ExprMatch { expr, arms, .. }) => {
                let to_match = self.eval_expr(expr, depth.next());
                let mut val = None;
                for (pattern, _arrow, expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(lit) => if &to_match == lit {
                            self.scopes.push_scope(Scope::new());
                            val = Some(self.eval_expr(expr, depth.last()));
                            self.scopes.pop_scope();
                            break;
                        }
                        ExprMatchPattern::Variant(variant) => {
                            // lock and release enum value before evaluating the match arm
                            {
                                let enum_value = match &to_match {
                                    Value::Enum(enum_value) => enum_value,
                                    _ => unreachable!(),
                                };
                                let enum_value = enum_value.e.lock();
                                let enum_value = enum_value.borrow();

                                assert_eq!(enum_value.name, variant.enum_name.ident);
                                if enum_value.variant != variant.variant_name.ident {
                                    continue;
                                }

                                self.scopes.push_scope(Scope::new());
                                if let Some((_open, fields, _close)) = &variant.fields {
                                    for (binding, value) in fields.iter().zip(&enum_value.fields) {
                                        self.scopes.create(binding.id, value.clone());
                                    }
                                }
                            }
                            val = Some(self.eval_expr(expr, depth.last()));
                            self.scopes.pop_scope();
                            break;
                        }
                        ExprMatchPattern::Binding(binding) => {
                            self.scopes.push_scope(Scope::new());
                            self.scopes.create(binding.id, to_match);
                            val = Some(self.eval_expr(expr, depth.last()));
                            self.scopes.pop_scope();
                            break;
                        }
                        ExprMatchPattern::Wildcard(_) => {
                            self.scopes.push_scope(Scope::new());
                            val = Some(self.eval_expr(expr, depth.last()));
                            self.scopes.pop_scope();
                            break;
                        },
                    }
                }
                val.unwrap()
            }
            Expr::While(ExprWhile { condition, block, .. }) => {
                while self.eval_expr(condition, depth.next()).expect_bool("while condition not a bool") {
                    self.eval_block(block, depth.next());
                }
                Value::Unit
            }
            Expr::FunctionCall(ExprFunctionCall { name, args, .. }) => {
                let args = args.iter().map(|expr| self.eval_expr(expr, depth.next())).collect();
                self.call_function(name.ident, expr.span(), args, depth.last())
            },
            // ignore function definitions as we have those handled already
            Expr::FunctionDefinition(ExprFunctionDefinition { .. }) => Value::Unit,
            Expr::StructDefinition(ExprStructDefinition { .. }) => Value::Unit,
            Expr::StructInitialization(ExprStructInitialization { name, fields, .. }) => {
                let mut field_values = Vec::new();
                for (field, _colon, expr) in fields {
                    field_values.push((field.ident.to_string(), self.eval_expr(expr, depth.next())));
                }
                // TODO: O(n²log(n)) probably isn't the best but we need the correct order of the type-definition for comparisons
                let typ = &self.struct_types[name.ident];
                field_values.sort_by_key(|(field, _)| typ.fields.iter().position(|(name, _)| field == name));
                Value::Struct(StructArc { s: Arc::new(ReentrantMutex::new(RefCell::new(Struct {
                    name: name.ident.to_string(),
                    fields: field_values,
                })))})
            }
            Expr::EnumDefinition(ExprEnumDefinition { .. }) => Value::Unit,
            Expr::EnumInitialization(ExprEnumInitialization { enum_name, variant_name, .. }) => {
                Value::Enum(EnumArc { e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
                    name: enum_name.ident.to_string(),
                    variant: variant_name.ident.to_string(),
                    fields: vec![],
                })))})
            }
            Expr::ImplBlock(_) => Value::Unit,
        }
    }

    fn eval_block(&mut self, ExprBlock { body: BlockBody { exprs, terminated_with_semicolon }, .. }: &ExprBlock, depth: Depth) -> Value {
        self.scopes.push_scope(Scope::new());
        let mut val = Value::Unit;
        for expr in exprs {
            val = self.eval_expr(expr, depth.next());
        }
        self.scopes.pop_scope();
        if *terminated_with_semicolon {
            Value::Unit
        } else {
            val
        }
    }
    fn call_function(&mut self, name: &str, expr_span: Span, args: Vec<Value>, depth: Depth) -> Value {
        trace!("{}call_function: {}({:?})", depth, name, args);
        match &self.functions[name] {
            Function::Rust(f) => f(expr_span, &mut self.ctx, args),
            Function::Rebo(name, arg_binding_ids) => {
                let mut scope = Scope::new();
                for (&id, val) in arg_binding_ids.iter().zip(args) {
                    scope.create(id, val);
                }
                self.ctx.scopes.push_scope(scope);

                let mut last = None;
                let fun = self.rebo_functions[name.as_str()];
                for expr in &fun.body.body.exprs {
                    last = Some(self.eval_expr(expr, depth.next()));
                }

                self.ctx.scopes.pop_scope();
                last.unwrap_or(Value::Unit)
            }
            Function::EnumInitializer(enum_name, variant_name) => {
                Value::Enum(EnumArc { e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
                    name: enum_name.clone(),
                    variant: variant_name.clone(),
                    fields: args,
                })))})
            }
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
    fn enums(a: EnumArc, b: EnumArc) -> bool;
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
    fn enums(a: EnumArc, b: EnumArc) -> bool { a < b }
    fn str() -> &'static str { "<" }
}
impl CmpOp for Le {
    fn unit() -> bool { true }
    fn integer(a: i64, b: i64) -> bool { a <= b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { a <= b }
    fn bool(a: bool, b: bool) -> bool { a <= b }
    fn string(a: &str, b: &str) -> bool { a <= b }
    fn structs(a: StructArc, b: StructArc) -> bool { a <= b }
    fn enums(a: EnumArc, b: EnumArc) -> bool { a <= b }
    fn str() -> &'static str { "<=" }
}
impl CmpOp for Eq {
    fn unit() -> bool { true }
    fn integer(a: i64, b: i64) -> bool { a == b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { a == b }
    fn bool(a: bool, b: bool) -> bool { a == b }
    fn string(a: &str, b: &str) -> bool { a == b }
    fn structs(a: StructArc, b: StructArc) -> bool { a == b }
    fn enums(a: EnumArc, b: EnumArc) -> bool { a == b }
    fn str() -> &'static str { "==" }
}
impl CmpOp for Neq {
    fn unit() -> bool { false }
    fn integer(a: i64, b: i64) -> bool { a != b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { !Eq::float(a, b) }
    fn bool(a: bool, b: bool) -> bool { a != b }
    fn string(a: &str, b: &str) -> bool { a != b }
    fn structs(a: StructArc, b: StructArc) -> bool { a != b }
    fn enums(a: EnumArc, b: EnumArc) -> bool { a != b }
    fn str() -> &'static str { "!=" }
}
impl CmpOp for Ge {
    fn unit() -> bool { true }
    fn integer(a: i64, b: i64) -> bool { a >= b }
    fn float(a: FuzzyFloat, b: FuzzyFloat) -> bool { a >= b }
    fn bool(a: bool, b: bool) -> bool { a >= b }
    fn string(a: &str, b: &str) -> bool { a >= b }
    fn structs(a: StructArc, b: StructArc) -> bool { a >= b }
    fn enums(a: EnumArc, b: EnumArc) -> bool { a >= b }
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
    fn enums(a: EnumArc, b: EnumArc) -> bool { a > b }
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
        (Value::Enum(a), Value::Enum(b)) => Value::Bool(O::enums(a, b)),
        _ => todo!("error handling"),
    }
}
