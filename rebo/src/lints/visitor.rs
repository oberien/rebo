use crate::parser::{ExprLiteral, ExprFormatString, ExprBind, ExprAssign, ExprBoolNot, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprMod, ExprXor, ExprBoolAnd, ExprBoolOr, ExprLessThan, ExprLessEquals, ExprEquals, ExprNotEquals, ExprGreaterEquals, ExprGreaterThan, ExprBlock, ExprVariable, ExprParenthesized, ExprIfElse, ExprMatch, ExprWhile, ExprFunctionCall, ExprFunctionDefinition, ExprStructDefinition, ExprStructInitialization, ExprImplBlock, Expr, ExprFormatStringPart, ExprEnumDefinition, ExprEnumInitialization, ExprAccess, FieldOrMethod, ExprFor, ExprStatic, ExprNeg, ExprAddAssign, ExprSubAssign, ExprMulAssign, ExprDivAssign, ExprModAssign, ExprXorAssign, ExprBoolAndAssign, ExprBoolOrAssign, ExprLoop, ExprContinue, ExprBreak, ExprReturn};
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, BlockStack, BlockType};
use crate::ErrorCode;

pub struct VisitorDriver<'a, 'b, 'i, 'v> {
    diagnostics: &'b Diagnostics<ErrorCode>,
    meta_info: &'b MetaInfo<'a, 'i>,
    block_stack: BlockStack<'a, 'i, ()>,
    visitors: Vec<&'v dyn Visitor>,
}

pub trait Visitor {
    fn visit_literal(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprLiteral) {}
    fn visit_format_string(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprFormatString) {}
    fn visit_bind(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprBind) {}
    fn visit_static(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprStatic) {}
    fn visit_assign(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprAssign) {}
    fn visit_bool_not(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprBoolNot) {}
    fn visit_neg(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprNeg) {}
    fn visit_add(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprAdd) {}
    fn visit_sub(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprSub) {}
    fn visit_mul(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprMul) {}
    fn visit_div(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprDiv) {}
    fn visit_mod(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprMod) {}
    fn visit_xor(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprXor) {}
    fn visit_bool_and(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprBoolAnd) {}
    fn visit_bool_or(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprBoolOr) {}
    fn visit_add_assign(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprAddAssign) {}
    fn visit_sub_assign(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprSubAssign) {}
    fn visit_mul_assign(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprMulAssign) {}
    fn visit_div_assign(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprDivAssign) {}
    fn visit_mod_assign(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprModAssign) {}
    fn visit_xor_assign(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprXorAssign) {}
    fn visit_bool_and_assign(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprBoolAndAssign) {}
    fn visit_bool_or_assign(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprBoolOrAssign) {}
    fn visit_less_than(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprLessThan) {}
    fn visit_less_equals(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprLessEquals) {}
    fn visit_equals(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprEquals) {}
    fn visit_not_equals(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprNotEquals) {}
    fn visit_greater_equals(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprGreaterEquals) {}
    fn visit_greater_than(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprGreaterThan) {}
    fn visit_block(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprBlock) {}
    fn visit_variable(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprVariable) {}
    fn visit_access(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprAccess) {}
    fn visit_parenthesized(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprParenthesized) {}
    fn visit_if_else(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprIfElse) {}
    fn visit_match(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprMatch) {}
    fn visit_loop(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprLoop) {}
    fn visit_while(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprWhile) {}
    fn visit_for(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprFor) {}
    fn visit_break(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprBreak) {}
    fn visit_continue(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprContinue) {}
    fn visit_return(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprReturn) {}
    fn visit_function_call(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprFunctionCall) {}
    fn visit_function_definition(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprFunctionDefinition) {}
    fn visit_struct_definition(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprStructDefinition) {}
    fn visit_struct_initialization(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprStructInitialization) {}
    fn visit_enum_definition(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprEnumDefinition) {}
    fn visit_enum_initialization(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprEnumInitialization) {}
    fn visit_impl_block(&self, _: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, _: &ExprImplBlock) {}
}

macro_rules! visit {
    ($self:ident, $function:ident, $var: expr) => {{
        for visitor in &$self.visitors {
            visitor.$function($self.diagnostics, $self.meta_info, &$self.block_stack, $var);
        }
    }}
}

impl<'a, 'b, 'i, 'v> VisitorDriver<'a, 'b, 'i, 'v> {
    pub fn new(diagnostics: &'b Diagnostics<ErrorCode>, meta_info: &'b MetaInfo<'a, 'i>) -> Self {
        VisitorDriver {
            diagnostics,
            meta_info,
            block_stack: BlockStack::new(),
            visitors: Vec::new(),
        }
    }
    pub fn add_visitors(&mut self, visitors: impl IntoIterator<Item = &'v dyn Visitor>) {
        for visitor in visitors {
            self.add_visitor(visitor);
        }
    }
    pub fn add_visitor(&mut self, visitor: &'v dyn Visitor) {
        self.visitors.push(visitor);
    }

    pub fn visit_exprs(&self, exprs: &[&'a Expr<'a, 'i>]) {
        for expr in exprs {
            self.visit_expr(expr);
        }
    }
    fn visit_expr(&self, expr: &'a Expr<'a, 'i>) {
        match expr {
            Expr::Literal(lit) => visit!(self, visit_literal, lit),
            Expr::FormatString(fs) => {
                visit!(self, visit_format_string, fs);
                for part in &fs.parts {
                    match part {
                        ExprFormatStringPart::Str(_) | ExprFormatStringPart::Escaped(_) => (),
                        ExprFormatStringPart::FmtArg(expr, _) => self.visit_expr(expr),
                    }
                }
            }
            Expr::Bind(bind) => {
                visit!(self, visit_bind, bind);
                self.visit_expr(bind.expr);
            }
            Expr::Static(stati) => {
                visit!(self, visit_static, stati);
                self.visit_expr(stati.expr);
            }
            Expr::Assign(assign) => {
                visit!(self, visit_assign, assign);
                self.visit_expr(assign.expr);
            }
            Expr::BoolNot(bool_not) => {
                visit!(self, visit_bool_not, bool_not);
                self.visit_expr(bool_not.expr);
            }
            Expr::Neg(neg) => {
                visit!(self, visit_neg, neg);
                self.visit_expr(neg.expr);
            }
            Expr::Add(add) => {
                visit!(self, visit_add, add);
                self.visit_expr(add.a);
                self.visit_expr(add.b);
            }
            Expr::Sub(sub) => {
                visit!(self, visit_sub, sub);
                self.visit_expr(sub.a);
                self.visit_expr(sub.b);
            }
            Expr::Mul(mul) => {
                visit!(self, visit_mul, mul);
                self.visit_expr(mul.a);
                self.visit_expr(mul.b);
            }
            Expr::Div(div) => {
                visit!(self, visit_div, div);
                self.visit_expr(div.a);
                self.visit_expr(div.b);
            }
            Expr::Mod(m) => {
                visit!(self, visit_mod, m);
                self.visit_expr(m.a);
                self.visit_expr(m.b);
            }
            Expr::Xor(xor) => {
                visit!(self, visit_xor, xor);
                self.visit_expr(xor.a);
                self.visit_expr(xor.b);
            }
            Expr::BoolAnd(bool_and) => {
                visit!(self, visit_bool_and, bool_and);
                self.visit_expr(bool_and.a);
                self.visit_expr(bool_and.b);
            }
            Expr::BoolOr(bool_or) => {
                visit!(self, visit_bool_or, bool_or);
                self.visit_expr(bool_or.a);
                self.visit_expr(bool_or.b);
            }
            Expr::AddAssign(add) => {
                visit!(self, visit_add_assign, add);
                self.visit_expr(add.expr);
            }
            Expr::SubAssign(sub) => {
                visit!(self, visit_sub_assign, sub);
                self.visit_expr(sub.expr);
            }
            Expr::MulAssign(mul) => {
                visit!(self, visit_mul_assign, mul);
                self.visit_expr(mul.expr);
            }
            Expr::DivAssign(div) => {
                visit!(self, visit_div_assign, div);
                self.visit_expr(div.expr);
            }
            Expr::ModAssign(m) => {
                visit!(self, visit_mod_assign, m);
                self.visit_expr(m.expr);
            }
            Expr::XorAssign(xor) => {
                visit!(self, visit_xor_assign, xor);
                self.visit_expr(xor.expr);
            }
            Expr::BoolAndAssign(bool_and) => {
                visit!(self, visit_bool_and_assign, bool_and);
                self.visit_expr(bool_and.expr);
            }
            Expr::BoolOrAssign(bool_or) => {
                visit!(self, visit_bool_or_assign, bool_or);
                self.visit_expr(bool_or.expr);
            }
            Expr::LessThan(less_than) => {
                visit!(self, visit_less_than, less_than);
                self.visit_expr(less_than.a);
                self.visit_expr(less_than.b);
            }
            Expr::LessEquals(less_equals) => {
                visit!(self, visit_less_equals, less_equals);
                self.visit_expr(less_equals.a);
                self.visit_expr(less_equals.b);
            }
            Expr::Equals(equals) => {
                visit!(self, visit_equals, equals);
                self.visit_expr(equals.a);
                self.visit_expr(equals.b);
            }
            Expr::NotEquals(not_equals) => {
                visit!(self, visit_not_equals, not_equals);
                self.visit_expr(not_equals.a);
                self.visit_expr(not_equals.b);
            }
            Expr::GreaterEquals(greater_equals) => {
                visit!(self, visit_greater_equals, greater_equals);
                self.visit_expr(greater_equals.a);
                self.visit_expr(greater_equals.b);
            }
            Expr::GreaterThan(greater_than) => {
                visit!(self, visit_greater_than, greater_than);
                self.visit_expr(greater_than.a);
                self.visit_expr(greater_than.b);
            }
            Expr::Block(block) => {
                visit!(self, visit_block, block);
                self.visit_exprs(&block.body.exprs);
            }
            Expr::Variable(var) => visit!(self, visit_variable, var),
            Expr::Access(acc) => {
                visit!(self, visit_access, acc);
                for acc in &acc.accesses {
                    match acc {
                        FieldOrMethod::Field(_) => (),
                        FieldOrMethod::Method(fn_call) => {
                            for arg in &fn_call.args {
                                self.visit_expr(arg);
                            }
                        }
                    }
                }
            }
            Expr::Parenthesized(par) => {
                visit!(self, visit_parenthesized, par);
                self.visit_expr(par.expr);
            }
            Expr::IfElse(ifelse) => {
                visit!(self, visit_if_else, ifelse);
                for (cond, block) in ifelse.iter_branches() {
                    if let Some(cond) = cond {
                        self.visit_expr(cond);
                    }
                    self.visit_exprs(&block.body.exprs);
                }
            }
            Expr::Match(mat) => {
                visit!(self, visit_match, mat);
                self.visit_expr(mat.expr);
                for (_pat, _arrow, expr) in &mat.arms {
                    self.visit_expr(expr);
                }
            }
            Expr::Loop(lo) => {
                let _guard = self.block_stack.push_block(BlockType::Loop(lo.label.as_ref().map(|l| &l.label)), ());
                visit!(self, visit_loop, lo);
                self.visit_exprs(&lo.block.body.exprs);
            }
            Expr::While(wh) => {
                let _guard = self.block_stack.push_block(BlockType::While(wh.label.as_ref().map(|l| &l.label)), ());
                visit!(self, visit_while, wh);
                self.visit_expr(wh.condition);
                self.visit_exprs(&wh.block.body.exprs);
            }
            Expr::For(fo) => {
                let _guard = self.block_stack.push_block(BlockType::For(fo.label.as_ref().map(|l| &l.label)), ());
                visit!(self, visit_for, fo);
                self.visit_expr(fo.expr);
                self.visit_exprs(&fo.block.body.exprs);
            }
            Expr::Break(br) => {
                visit!(self, visit_break, br);
                if let Some(expr) = br.expr {
                    self.visit_expr(expr);
                }
            }
            Expr::Continue(co) => {
                visit!(self, visit_continue, co);
            }
            Expr::Return(ret) => {
                visit!(self, visit_return, ret);
                if let Some(expr) = ret.expr {
                    self.visit_expr(expr);
                }
            }
            Expr::FunctionCall(fc) => {
                visit!(self, visit_function_call, fc);
                for arg in &fc.args {
                    self.visit_expr(arg);
                }
            }
            Expr::FunctionDefinition(fd) => {
                let _guard = self.block_stack.push_block(BlockType::Function, ());
                visit!(self, visit_function_definition, fd);
                self.visit_exprs(&fd.body.body.exprs);
            }
            Expr::StructDefinition(sd) => visit!(self, visit_struct_definition, sd),
            Expr::StructInitialization(si) => {
                visit!(self, visit_struct_initialization, si);
                for (_ident, _colon, expr) in &si.fields {
                    self.visit_expr(expr);
                }
            }
            Expr::EnumDefinition(ed) => visit!(self, visit_enum_definition, ed),
            Expr::EnumInitialization(ei) => visit!(self, visit_enum_initialization, ei),
            Expr::ImplBlock(impl_block) => {
                visit!(self, visit_impl_block, impl_block);
                for fd in &impl_block.functions {
                    visit!(self, visit_function_definition, fd);
                    self.visit_exprs(&fd.body.body.exprs);
                }
            }
        }
    }
}
