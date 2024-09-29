use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use parking_lot::ReentrantMutex;

use crate::common::{Depth, Enum, EnumArc, Function, FunctionValue, FuzzyFloat, MetaInfo, RequiredReboFunction, RequiredReboFunctionStruct, Struct, StructArc, Value};
use crate::lexer::{TokenBool, TokenDqString, TokenFloat, TokenIdent, TokenInteger};
use crate::parser::{Binding, BlockBody, Expr, ExprAccess, ExprAdd, ExprAddAssign, ExprAssign, ExprAssignLhs, ExprBind, ExprBlock, ExprBool, ExprBoolAnd, ExprBoolAndAssign, ExprBoolNot, ExprBoolOr, ExprBoolOrAssign, ExprBreak, ExprContinue, ExprDiv, ExprDivAssign, ExprEnumDefinition, ExprEnumInitialization, ExprEquals, ExprFieldAccess, ExprFloat, ExprFor, ExprFormatString, ExprFormatStringPart, ExprFunctionCall, ExprGreaterEquals, ExprGreaterThan, ExprIfElse, ExprInteger, ExprLabel, ExprLessEquals, ExprLessThan, ExprLiteral, ExprLoop, ExprMatch, ExprMatchPattern, ExprMethodCall, ExprMod, ExprModAssign, ExprMul, ExprMulAssign, ExprNeg, ExprNotEquals, ExprParenthesized, ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprReturn, ExprString, ExprStructDefinition, ExprStructInitialization, ExprSub, ExprSubAssign, ExprVariable, ExprWhile, ExprXor, ExprXorAssign, FieldOrMethod};
pub use crate::vm::scope::{Scope, Scopes};
use diagnostic::Diagnostics;
use rt_format::{Specifier, Substitution};
use rebo::common::SpanWithId;
use crate::{ErrorCode, EXTERNAL_SPAN, IncludeDirectory};
use crate::common::Spanned;

mod scope;

pub struct Vm<'i, 'b> {
    interrupt_interval: u32,
    instructions_since_last_interrupt: u32,
    interrupt_function: fn(&mut VmContext<'i, '_, '_>) -> Result<(), ExecError<'i>>,
    diagnostics: &'i Diagnostics<ErrorCode>,
    scopes: Scopes,
    // read in the voice of Brüggi
    callstack: Rc<RefCell<Vec<SpanWithId>>>,
    meta_info: &'b MetaInfo<'i>,
    include_directory: IncludeDirectory,
}

#[derive(Debug)]
pub enum ExecError<'i> {
    Break(Option<&'i ExprLabel<'i>>, Value),
    Continue(Option<&'i ExprLabel<'i>>),
    Return(Value),
    Panic,
}

/// Passed to external functions
pub struct VmContext<'i, 'b, 'vm> {
    vm: &'vm mut Vm<'i, 'b>,
}

impl<'i, 'b, 'vm> VmContext<'i, 'b, 'vm> {
    pub fn diagnostics(&self) -> &Diagnostics<ErrorCode> {
        self.vm.diagnostics
    }

    pub fn include_directory(&self) -> &IncludeDirectory {
        &self.vm.include_directory
    }

    pub fn call_required_rebo_function<T: RequiredReboFunction>(&mut self, args: Vec<Value>) -> Result<Value, ExecError<'i>> {
        if !self.vm.meta_info.required_rebo_functions.contains(&RequiredReboFunctionStruct::from_required_rebo_function::<T>()) {
            panic!("required rebo function `{}` wasn't registered via `ReboConfig`", T::NAME);
        }
        self.vm.call_function(&FunctionValue::Named(T::NAME.to_string()), *EXTERNAL_SPAN, args, Depth::start())
    }

    pub fn callstack(&self) -> Vec<SpanWithId> {
        self.vm.callstack.borrow().clone()
    }
}

#[must_use]
struct CallstackGuard {
    callstack: Rc<RefCell<Vec<SpanWithId>>>,
}
impl Drop for CallstackGuard {
    fn drop(&mut self) {
        self.callstack.borrow_mut().pop().expect("callstack guard dropped without callstack element left");
    }
}

impl<'i, 'b> Vm<'i, 'b> {
    pub fn new(include_directory: IncludeDirectory, diagnostics: &'i Diagnostics<ErrorCode>, meta_info: &'b MetaInfo<'i>, interrupt_interval: u32, interrupt_function: fn(&mut VmContext<'i, '_, '_>) -> Result<(), ExecError<'i>>) -> Self {
        let scopes = Scopes::new();
        let root_scope = Scope::new();
        // we don't want to drop the root-scope, it should exist at all times
        scopes.push_scope(root_scope).dont_remove();
        Vm {
            instructions_since_last_interrupt: 0,
            interrupt_interval,
            interrupt_function,
            diagnostics,
            scopes,
            callstack: Rc::new(RefCell::new(Vec::new())),
            meta_info,
            include_directory,
        }
    }

    pub fn run(mut self, ast: &[&'i Expr<'i>]) -> Result<Value, ExecError<'i>> {
        trace!("run");
        // add functions
        for (binding, name) in &self.meta_info.function_bindings {
            self.bind(binding, Value::Function(FunctionValue::Named(name.clone())), Depth::start());
        }
        // add statics
        for static_def in self.meta_info.statics.clone().values() {
            let binding = match &static_def.sig.pattern {
                ExprPattern::Typed(typed) => typed.pattern.binding,
                ExprPattern::Untyped(untyped) => untyped.binding,
            };
            let value = self.eval_expr(static_def.expr, Depth::start())?;
            self.bind(&binding, value, Depth::start());
        }
        // add external values
        for (binding, value) in self.meta_info.external_values.clone() {
            self.bind(&binding, value, Depth::start());
        }

        let mut value = None;
        for expr in ast {
            value = Some(self.eval_expr(expr, Depth::start())?);
        }
        Ok(value.unwrap_or(Value::Unit))
    }

    fn push_callstack(&mut self, expr_span: SpanWithId) -> CallstackGuard {
        self.callstack.borrow_mut().push(expr_span);
        CallstackGuard {
            callstack: Rc::clone(&self.callstack),
        }
    }

    fn load_binding(&self, binding: &Binding, span: SpanWithId, depth: Depth) -> Value {
        trace!("{}load_binding: {}", depth, binding.ident.ident);
        self.scopes.get(binding.id)
            .unwrap_or_else(|| {
                let msg = format!("binding {}[{}] is None", binding.ident.ident, binding.id);
                self.diagnostics.bug(ErrorCode::VmBindingIsNone)
                    .with_error_label(span.diagnostics_span(), &msg)
                    .emit();
                panic!("{}", msg);
            })
    }

    fn bind(&mut self, binding: &Binding, value: Value, depth: Depth) -> Value {
        trace!("{}bind {} = {:?}", depth, binding.ident.ident, value);
        self.scopes.create(binding.id, value);
        Value::Unit
    }

    fn assign_binding(&mut self, binding: &Binding, value: Value, depth: Depth) {
        trace!("{}assign {} = {:?}", depth, binding.ident.ident, value);
        self.scopes.assign(binding.id, value);
    }

    fn interrupt_counter(&mut self) -> Result<(), ExecError<'i>> {
        self.instructions_since_last_interrupt += 1;
        if self.instructions_since_last_interrupt > self.interrupt_interval {
            self.instructions_since_last_interrupt = 0;
            let interrupt_function = self.interrupt_function;
            let mut ctx = VmContext { vm: self };
            interrupt_function(&mut ctx)?;
        }
        Ok(())
    }

    fn eval_expr(&mut self, expr: &'i Expr<'i>, depth: Depth) -> Result<Value, ExecError<'i>> {
        trace!("{}eval_expr: {}", depth, expr);
        self.interrupt_counter()?;
        match expr {
            Expr::Literal(ExprLiteral::Unit(_)) => Ok(Value::Unit),
            Expr::Literal(ExprLiteral::Integer(ExprInteger { int: TokenInteger { value, .. } })) => Ok(Value::Integer(*value)),
            Expr::Literal(ExprLiteral::Float(ExprFloat { float: TokenFloat { value, .. }})) => Ok(Value::Float(FuzzyFloat(*value))),
            Expr::Literal(ExprLiteral::Bool(ExprBool { b: TokenBool { value, .. } })) => Ok(Value::Bool(*value)),
            Expr::Literal(ExprLiteral::String(ExprString { string: TokenDqString { string, .. } })) => Ok(Value::String(string.clone())),
            Expr::Variable(var @ ExprVariable { binding, .. }) => Ok(self.load_binding(binding, var.span_with_id(), depth.last())),
            Expr::Access(ExprAccess { variable, accesses, .. }) => self.load_access(variable, accesses, depth),
            Expr::FormatString(ExprFormatString { parts, .. }) => {
                let mut res = String::new();
                for part in parts {
                    match part {
                        ExprFormatStringPart::Str(s) => res.push_str(s),
                        ExprFormatStringPart::Escaped(s) => res.push_str(s),
                        ExprFormatStringPart::FmtArg(expr, spec) => {
                            let val = self.eval_expr(expr, depth.next())?;
                            let spec = match spec {
                                Some((_colon, spec, _)) => *spec,
                                None => Specifier::default(),
                            };
                            let arg = Substitution::new(spec, &val).unwrap();
                            res.push_str(&format!("{}", arg));
                        }
                    }
                }
                Ok(Value::String(res))
            },
            Expr::Assign(ExprAssign { lhs, expr, .. }) => {
                let value = self.eval_expr(expr, depth.next())?;
                self.assign(lhs, value, depth.last());
                Ok(Value::Unit)
            },
            Expr::Bind(ExprBind { pattern: ExprPattern::Typed(ExprPatternTyped { pattern: ExprPatternUntyped { binding }, .. }), expr, .. })
            | Expr::Bind(ExprBind { pattern: ExprPattern::Untyped(ExprPatternUntyped { binding }), expr, .. }) => {
                let value = self.eval_expr(expr, depth.next())?;
                Ok(self.bind(binding, value, depth.last()))
            },
            // handled already
            Expr::Static(_) => Ok(Value::Unit),
            Expr::LessThan(ExprLessThan { a, b, .. }) => Ok(cmp::<Lt>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::LessEquals(ExprLessEquals { a, b, .. }) => Ok(cmp::<Le>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::Equals(ExprEquals { a, b, .. }) => Ok(cmp::<Eq>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::NotEquals(ExprNotEquals { a, b, .. }) => Ok(cmp::<Neq>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::GreaterEquals(ExprGreaterEquals { a, b, .. }) => Ok(cmp::<Ge>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::GreaterThan(ExprGreaterThan { a, b, .. }) => Ok(cmp::<Gt>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::Add(ExprAdd { a, b, .. }) => Ok(math::<Add>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::Sub(ExprSub { a, b, .. }) => Ok(math::<Sub>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::Mul(ExprMul { a, b, .. }) => Ok(math::<Mul>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::Div(ExprDiv { a, b, .. }) => Ok(math::<Div>(self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?, depth.last())),
            Expr::Mod(ExprMod { a, b, .. }) => {
                let a = self.eval_expr(a, depth.next())?.expect_int("mod only allows ints");
                let b = self.eval_expr(b, depth.next())?.expect_int("mod only allows ints");
                Ok(Value::Integer(a % b))
            },
            Expr::Xor(ExprXor { a, b, .. }) => {
                let (a, b) = (self.eval_expr(a, depth.next())?, self.eval_expr(b, depth.next())?);
                match (a, b) {
                    (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a ^ b)),
                    (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a ^ b)),
                    _ => unreachable!("xor called on non-int and non-bool"),
                }
            },
            Expr::AddAssign(ExprAddAssign { lhs, expr, .. }) => {
                let value = math::<Add>(self.load_lhs_value(lhs, depth.next())?, self.eval_expr(expr, depth.next())?, depth.next());
                self.assign(lhs, value, depth.last());
                Ok(Value::Unit)
            },
            Expr::SubAssign(ExprSubAssign { lhs, expr, .. }) => {
                let value = math::<Sub>(self.load_lhs_value(lhs, depth.next())?, self.eval_expr(expr, depth.next())?, depth.next());
                self.assign(lhs, value, depth.last());
                Ok(Value::Unit)
            },
            Expr::MulAssign(ExprMulAssign { lhs, expr, .. }) => {
                let value = math::<Mul>(self.load_lhs_value(lhs, depth.next())?, self.eval_expr(expr, depth.next())?, depth.next());
                self.assign(lhs, value, depth.last());
                Ok(Value::Unit)
            },
            Expr::DivAssign(ExprDivAssign { lhs, expr, .. }) => {
                let value = math::<Div>(self.load_lhs_value(lhs, depth.next())?, self.eval_expr(expr, depth.next())?, depth.next());
                self.assign(lhs, value, depth.last());
                Ok(Value::Unit)
            },
            Expr::ModAssign(ExprModAssign { lhs, expr, .. }) => {
                let a = self.load_lhs_value(lhs, depth.next())?.expect_int("mod only allows ints");
                let b = self.eval_expr(expr, depth.next())?.expect_int("mod only allows ints");
                let value = Value::Integer(a % b);
                self.assign(lhs, value, depth.last());
                Ok(Value::Unit)
            },
            Expr::XorAssign(ExprXorAssign { lhs, expr, .. }) => {
                let (a, b) = (self.load_lhs_value(lhs, depth.next())?, self.eval_expr(expr, depth.next())?);
                let value = match (a, b) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a ^ b),
                    (Value::Bool(a), Value::Bool(b)) => Value::Bool(a ^ b),
                    _ => unreachable!("xor called on non-int and non-bool"),
                };
                self.assign(lhs, value, depth.last());
                Ok(Value::Unit)
            },
            Expr::BoolNot(ExprBoolNot { expr, .. }) => {
                let b = self.eval_expr(expr, depth.last())?.expect_bool("boolean NOT called with non-bool");
                Ok(Value::Bool(!b))
            }
            Expr::Neg(ExprNeg { expr, .. }) => {
                match self.eval_expr(expr, depth.last())? {
                    Value::Integer(i) => Ok(Value::Integer(-i)),
                    Value::Float(f) => Ok(Value::Float(FuzzyFloat(-f.0))),
                    _ => unreachable!("NEG called with non-int non-float"),
                }
            }
            Expr::BoolAnd(ExprBoolAnd { a, b, .. }) => {
                let res = self.eval_expr(a, depth.next())?.expect_bool("boolean AND called with non-bool")
                    && self.eval_expr(b, depth.last())?.expect_bool("boolean AND called with non-bool");
                Ok(Value::Bool(res))
            }
            Expr::BoolOr(ExprBoolOr { a, b, .. }) => {
                let res = self.eval_expr(a, depth.next())?.expect_bool("boolean OR called with non-bool")
                    || self.eval_expr(b, depth.last())?.expect_bool("boolean OR called with non-bool");
                Ok(Value::Bool(res))
            }
            Expr::BoolAndAssign(ExprBoolAndAssign { lhs, expr, .. }) => {
                let res = self.load_lhs_value(lhs, depth.next())?.expect_bool("boolean AND called with non-bool")
                    && self.eval_expr(expr, depth.next())?.expect_bool("boolean AND called with non-bool");
                self.assign(lhs, Value::Bool(res), depth.last());
                Ok(Value::Unit)
            }
            Expr::BoolOrAssign(ExprBoolOrAssign { lhs, expr, .. }) => {
                let res = self.load_lhs_value(lhs, depth.next())?.expect_bool("boolean OR called with non-bool")
                    || self.eval_expr(expr, depth.next())?.expect_bool("boolean OR called with non-bool");
                self.assign(lhs, Value::Bool(res), depth.last());
                Ok(Value::Unit)
            }
            Expr::Parenthesized(ExprParenthesized { expr, .. }) => self.eval_expr(expr, depth.last()),
            Expr::Block(block) => {
                self.eval_block(block, depth)
            }
            Expr::IfElse(ExprIfElse { condition, then, else_ifs, els, .. }) => {
                let branches = ::std::iter::once((condition, then))
                    .chain(else_ifs.iter().map(|(_, _, cond, block)| (cond, block)));
                for (cond, block) in branches {
                    let cond = self.eval_expr(cond, depth.next())?.expect_bool("if-condition doesn't evaluate to bool");
                    if cond {
                        return self.eval_block(block, depth.last());
                    }
                }
                if let Some((_, block)) = els {
                    return self.eval_block(block, depth.last());
                }
                Ok(Value::Unit)
            }
            Expr::Match(ExprMatch { expr, arms, .. }) => {
                let to_match = self.eval_expr(expr, depth.next())?;
                let mut val = None;
                for (pattern, _arrow, expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(lit) => if &to_match == lit {
                            let _guard = self.scopes.push_scope(Scope::new());
                            val = Some(self.eval_expr(expr, depth.last())?);
                            break;
                        }
                        ExprMatchPattern::Variant(variant) => {
                            // lock and release enum value before evaluating the match arm
                            let _guard = self.scopes.push_scope(Scope::new());
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

                                if let Some((_open, fields, _close)) = &variant.fields {
                                    for (binding, value) in fields.iter().zip(&enum_value.fields) {
                                        self.scopes.create(binding.id, value.clone());
                                    }
                                }
                            }
                            val = Some(self.eval_expr(expr, depth.last())?);
                            break;
                        }
                        ExprMatchPattern::Binding(binding) => {
                            let _guard = self.scopes.push_scope(Scope::new());
                            self.scopes.create(binding.id, to_match);
                            val = Some(self.eval_expr(expr, depth.last())?);
                            break;
                        }
                        ExprMatchPattern::Wildcard(_) => {
                            let _guard = self.scopes.push_scope(Scope::new());
                            val = Some(self.eval_expr(expr, depth.last())?);
                            break;
                        },
                    }
                }
                Ok(val.unwrap())
            }
            Expr::Loop(ExprLoop { label, block, .. }) => {
                loop {
                    match self.eval_block(block, depth.next()) {
                        Ok(_) => (),
                        Err(ExecError::Break(None, val)) => break Ok(val),
                        Err(ExecError::Break(l, val)) if l == label.as_ref().map(|l| &l.label) => break Ok(val),
                        Err(ExecError::Continue(None)) => (),
                        Err(ExecError::Continue(l)) if l == label.as_ref().map(|l| &l.label) => (),
                        Err(e) => break Err(e),
                    }
                }
            }
            Expr::While(ExprWhile { label, condition, block, .. }) => {
                while self.eval_expr(condition, depth.next())?.expect_bool("while condition not a bool") {
                    match self.eval_block(block, depth.next()) {
                        Ok(_) => (),
                        Err(ExecError::Break(None, _)) => break,
                        Err(ExecError::Break(l, _)) if l == label.as_ref().map(|l| &l.label) => break,
                        Err(ExecError::Continue(None)) => (),
                        Err(ExecError::Continue(l)) if l == label.as_ref().map(|l| &l.label) => (),
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Unit)
            }
            Expr::For(ExprFor { label, binding, expr, block, .. }) => {
                let list = self.eval_expr(expr, depth.next())?.expect_list("for expr is not a list");
                let list = list.list.lock();
                let list = list.borrow();
                let _guard = self.scopes.push_scope(Scope::new());
                self.bind(binding, Value::Unit, depth.next());
                for value in list.iter().cloned() {
                    self.assign_binding(binding, value, depth.next());
                    match self.eval_block(block, depth.next()) {
                        Ok(_) => (),
                        Err(ExecError::Break(None, _)) => break,
                        Err(ExecError::Break(l, _)) if l == label.as_ref().map(|l| &l.label) => break,
                        Err(ExecError::Continue(None)) => (),
                        Err(ExecError::Continue(l)) if l == label.as_ref().map(|l| &l.label) => (),
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Unit)
            }
            Expr::Break(ExprBreak { label, expr, .. }) => {
                let val = match expr {
                    Some(expr) => self.eval_expr(expr, depth.next())?,
                    None => Value::Unit,
                };
                Err(ExecError::Break(label.as_ref(), val))
            }
            Expr::Continue(ExprContinue { label, .. }) => {
                Err(ExecError::Continue(label.as_ref()))
            }
            Expr::Return(ExprReturn { expr, .. }) => {
                let val = match expr {
                    Some(expr) => self.eval_expr(expr, depth.next())?,
                    None => Value::Unit,
                };
                Err(ExecError::Return(val))
            }
            Expr::Yield(_) => unreachable!("encountered a yield expression in the VM"),
            Expr::FunctionCall(ExprFunctionCall { name, args, .. }) => {
                let fun = self.load_binding(&name.binding, name.span_with_id(), depth.next()).expect_function("called a function on a binding that's not a function");
                let args = args.iter().map(|expr| self.eval_expr(expr, depth.next())).collect::<Result<_, _>>()?;
                self.call_function(&fun, expr.span_with_id(), args, depth.last())
            },
            Expr::FunctionDefinition(fun) => match fun.sig.name {
                // ignore function definitions as we have those handled already
                Some(_) => Ok(Value::Unit),
                None => {
                    let mut scope = Scope::new();
                    for binding in &fun.captures {
                        let val = self.load_binding(binding, fun.span_with_id(), depth.next());
                        scope.create(binding.id, val);
                    }
                    Ok(Value::Function(FunctionValue::Anonymous(scope, fun.span)))
                },
            },
            Expr::StructDefinition(ExprStructDefinition { .. }) => Ok(Value::Unit),
            Expr::StructInitialization(ExprStructInitialization { name, fields, .. }) => {
                let mut field_values = Vec::new();
                for (field, _colon, expr) in fields {
                    field_values.push((field.ident.to_string(), self.eval_expr(expr, depth.next())?));
                }
                // TODO: O(n²log(n)) probably isn't the best but we need the correct order of the type-definition for comparisons
                let typ = &self.meta_info.struct_types[name.ident];
                field_values.sort_by_key(|(field, _)| typ.fields.iter().position(|(name, _)| field == name));
                Ok(Value::Struct(StructArc::new(Struct {
                    name: name.ident.to_string(),
                    fields: field_values,
                })))
            }
            Expr::EnumDefinition(ExprEnumDefinition { .. }) => Ok(Value::Unit),
            Expr::EnumInitialization(ExprEnumInitialization { enum_name, variant_name, .. }) => {
                Ok(Value::Enum(EnumArc { e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
                    name: enum_name.ident.to_string(),
                    variant_index: self.meta_info.enum_types[enum_name.ident].variants.iter()
                        .position(|(name, _)| name == variant_name.ident)
                        .expect("lints ensured that variant name exists"),
                    variant: variant_name.ident.to_string(),
                    fields: vec![],
                })))}))
            }
            Expr::ImplBlock(_) => Ok(Value::Unit),
        }
    }

    fn eval_block(&mut self, ExprBlock { body: BlockBody { exprs, terminated_with_semicolon }, .. }: &ExprBlock<'i>, depth: Depth) -> Result<Value, ExecError<'i>> {
        self.interrupt_counter()?;
        let _guard = self.scopes.push_scope(Scope::new());
        let mut val = Value::Unit;
        for expr in exprs {
            val = self.eval_expr(expr, depth.next())?;
        }
        if *terminated_with_semicolon {
            Ok(Value::Unit)
        } else {
            Ok(val)
        }
    }
    fn call_function(&mut self, fun: &FunctionValue, expr_span: SpanWithId, args: Vec<Value>, depth: Depth) -> Result<Value, ExecError<'i>> {
        trace!("{}call_function: {:?}({:?})", depth, fun, args);
        let _guard = self.push_callstack(expr_span);
        let (arg_binding_ids, mut scope, fun) = match fun {
            FunctionValue::Anonymous(scope, span) => {
                let (arg_binding_ids, fun) = &self.meta_info.anonymous_rebo_functions[span];
                (arg_binding_ids, scope.clone(), *fun)
            },
            FunctionValue::Named(name) => match &self.meta_info.functions.get(name.as_str()).unwrap_or_else(|| panic!("can't find function {}", name.as_str())) {
                Function::Rust(f) => return (*f)(expr_span.diagnostics_span(), &mut VmContext { vm: self }, args),
                Function::EnumInitializer(enum_name, variant_name) => {
                    return Ok(Value::Enum(EnumArc { e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
                        name: enum_name.clone(),
                        variant_index: self.meta_info.enum_types[enum_name.as_str()].variants.iter()
                            .position(|(name, _)| name == variant_name)
                            .expect("lints ensured that variant name exists"),
                        variant: variant_name.clone(),
                        fields: args,
                    })))}));
                }
                Function::Rebo(name, arg_binding_ids) => {
                    (arg_binding_ids, Scope::new(), self.meta_info.rebo_functions[name.as_str()])
                }
            }
        };

        for (&id, val) in arg_binding_ids.iter().zip(args) {
            scope.create(id, val);
        }
        let _guard = self.scopes.push_scope(scope);

        match self.eval_block(&fun.body, depth.next()) {
            Ok(val) => Ok(val),
            Err(ExecError::Return(val)) => Ok(val),
            Err(e) => Err(e),
        }
    }

    fn assign(&mut self, lhs: &ExprAssignLhs, value: Value, depth: Depth) {
        match lhs {
            ExprAssignLhs::Variable(ExprVariable { binding, .. }) => self.assign_binding(binding, value, depth.last()),
            ExprAssignLhs::FieldAccess(ExprFieldAccess { variable, fields, .. }) => {
                let mut struct_arc = match self.load_binding(&variable.binding, variable.span_with_id(), depth.next()) {
                    Value::Struct(s) => s,
                    _ => unreachable!("typechecker should have ensured that this is a struct"),
                };
                for (i, TokenIdent { ident, .. }) in fields.iter().enumerate() {
                    let new_struct = {
                        let s = struct_arc.s.lock();
                        let mut s = s.borrow_mut();
                        let field_value = s.fields.iter_mut()
                            .filter(|(name, _value)| name == ident)
                            .map(|(_name, value)| value)
                            .next()
                            .expect("typechecker ensured that all fields exist");
                        match field_value {
                            val if i == fields.len() - 1 => {
                                *val = value;
                                break;
                            }
                            Value::Struct(s) => s.clone(),
                            val => unreachable!("typechecker ensured that all non-last fields are structs, but we got {:?}", val),
                        }
                    };
                    struct_arc = new_struct;
                }
            }
        }
    }

    fn load_lhs_value(&mut self, lhs: &ExprAssignLhs<'i>, depth: Depth) -> Result<Value, ExecError<'i>> {
        match lhs {
            ExprAssignLhs::Variable(variable) => Ok(self.load_binding(&variable.binding, variable.span_with_id(), depth.next())),
            ExprAssignLhs::FieldAccess(ExprFieldAccess { variable, fields, .. }) => {
                self.load_access(variable, fields.iter().map(|ident| FieldOrMethod::Field(*ident)), depth)
            }
        }
    }

    fn load_access(&mut self, variable: &ExprVariable, accesses: impl IntoIterator<Item = impl ::std::borrow::Borrow<FieldOrMethod<'i>>>, depth: Depth) -> Result<Value, ExecError<'i>> {
        let mut val = self.load_binding(&variable.binding, variable.span_with_id(), depth.next());
        for acc in accesses {
            let acc = acc.borrow();
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
                FieldOrMethod::Method(ExprMethodCall { name, args, .. }) => {
                    let fn_name = format!("{}::{}", val.type_name(), name.ident);
                    let args = std::iter::once(Ok(val))
                        .chain(args.iter().map(|expr| self.eval_expr(expr, depth.next())))
                        .collect::<Result<_, _>>()?;
                    self.call_function(&FunctionValue::Named(fn_name), acc.span_with_id(), args, depth.last())?
                }
            };
        }
        Ok(val)
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
