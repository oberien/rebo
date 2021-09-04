use crate::parser::{ExprLiteral, ExprFormatString, ExprBind, ExprAssign, ExprBoolNot, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolAnd, ExprBoolOr, ExprLessThan, ExprLessEquals, ExprEquals, ExprNotEquals, ExprGreaterEquals, ExprGreaterThan, ExprBlock, ExprVariable, ExprFieldAccess, ExprParenthesized, ExprIfElse, ExprMatch, ExprWhile, ExprFunctionCall, ExprFunctionDefinition, ExprStructDefinition, ExprStructInitialization, ExprImplBlock, Expr};
use diagnostic::Diagnostics;
use crate::common::MetaInfo;

pub struct VisitorDriver<'a, 'b, 'i, 'v> {
    diagnostics: &'b Diagnostics,
    meta_info: &'b MetaInfo<'a, 'i>,
    visitors: Vec<&'v dyn Visitor>,
}

pub trait Visitor {
    fn visit_literal(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprLiteral) {}
    fn visit_format_string(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprFormatString) {}
    fn visit_bind(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprBind) {}
    fn visit_assign(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprAssign) {}
    fn visit_bool_not(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprBoolNot) {}
    fn visit_add(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprAdd) {}
    fn visit_sub(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprSub) {}
    fn visit_mul(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprMul) {}
    fn visit_div(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprDiv) {}
    fn visit_bool_and(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprBoolAnd) {}
    fn visit_bool_or(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprBoolOr) {}
    fn visit_less_than(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprLessThan) {}
    fn visit_less_equals(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprLessEquals) {}
    fn visit_equals(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprEquals) {}
    fn visit_not_equals(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprNotEquals) {}
    fn visit_greater_equals(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprGreaterEquals) {}
    fn visit_greater_than(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprGreaterThan) {}
    fn visit_block(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprBlock) {}
    fn visit_variable(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprVariable) {}
    fn visit_field_access(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprFieldAccess) {}
    fn visit_parenthesized(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprParenthesized) {}
    fn visit_if_else(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprIfElse) {}
    fn visit_match(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprMatch) {}
    fn visit_while(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprWhile) {}
    fn visit_function_call(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprFunctionCall) {}
    fn visit_function_definition(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprFunctionDefinition) {}
    fn visit_struct_definition(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprStructDefinition) {}
    fn visit_struct_initialization(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprStructInitialization) {}
    fn visit_impl_block(&self, _: &Diagnostics, _: &MetaInfo, _: &ExprImplBlock) {}
}

macro_rules! gen_visit_fn {
    ($($variant:ident => $function:ident,)*) => {
        fn visit_expr(&self, expr: &'a Expr<'a, 'i>) {
            match expr {
                $(
                    Expr::$variant(expr) => for visitor in &self.visitors {
                        visitor.$function(self.diagnostics, self.meta_info, expr);
                    }
                )*
            }
        }
    }
}

impl<'a, 'b, 'i, 'v> VisitorDriver<'a, 'b, 'i, 'v> {
    pub fn new(diagnostics: &'b Diagnostics, meta_info: &'b MetaInfo<'a, 'i>) -> Self {
        VisitorDriver {
            diagnostics,
            meta_info,
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
    gen_visit_fn! {
        Literal => visit_literal,
        FormatString => visit_format_string,
        Bind => visit_bind,
        Assign => visit_assign,
        BoolNot => visit_bool_not,
        Add => visit_add,
        Sub => visit_sub,
        Mul => visit_mul,
        Div => visit_div,
        BoolAnd => visit_bool_and,
        BoolOr => visit_bool_or,
        LessThan => visit_less_than,
        LessEquals => visit_less_equals,
        Equals => visit_equals,
        NotEquals => visit_not_equals,
        GreaterEquals => visit_greater_equals,
        GreaterThan => visit_greater_than,
        Block => visit_block,
        Variable => visit_variable,
        FieldAccess => visit_field_access,
        Parenthesized => visit_parenthesized,
        IfElse => visit_if_else,
        Match => visit_match,
        While => visit_while,
        FunctionCall => visit_function_call,
        FunctionDefinition => visit_function_definition,
        StructDefinition => visit_struct_definition,
        StructInitialization => visit_struct_initialization,
        ImplBlock => visit_impl_block,
    }
}
