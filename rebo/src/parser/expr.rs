use std::fmt::{self, Write};

use itertools::Itertools;
use diagnostic::Span;

use crate::scope::BindingId;
use crate::util::PadFmt;
use crate::common::SpecificType;

#[derive(Debug)]
pub struct Expr<'a, 'i> {
    pub span: Span,
    pub typ: ExprType<'a, 'i>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Binding<'i> {
    pub id: BindingId,
    pub ident: &'i str,
    pub mutable: bool,
    /// Span of the original declaration binding
    pub span: Span,
    /// If this is a rogue binding that was created by the parser when an error occurred.
    /// If there is a further error involving this binding, it shouldn't be emitted.
    pub rogue: bool,
}


#[derive(Debug)]
pub enum ExprType<'a, 'i> {
    Unit,
    /// foo
    Variable(Binding<'i>),
    /// 0
    Integer(i64),
    /// 0.
    Float(f64),
    /// true
    Bool(bool),
    /// "foo"
    String(String),
    /// let ident = expr
    Bind(Binding<'i>, &'a Expr<'a, 'i>),
    /// ident = expr
    Assign((Binding<'i>, Span), &'a Expr<'a, 'i>),
    /// expr + expr
    Add(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr - expr
    Sub(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr * expr
    Mul(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr / expr
    Div(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr && expr
    BoolAnd(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr || bar
    BoolOr(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// !expr
    BoolNot(&'a Expr<'a, 'i>),
    /// expr < expr
    LessThan(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr <= expr
    LessEquals(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr == expr
    Equals(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr != expr
    NotEquals(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr ~~ expr
    FloatEquals(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr !~ expr
    FloatNotEquals(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr >= expr
    GreaterEquals(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr > expr
    GreaterThan(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    /// expr;
    Statement(&'a Expr<'a, 'i>),
    /// { expr... }
    Block(Vec<&'a Expr<'a, 'i>>),
    /// (expr)
    Parenthezised(&'a Expr<'a, 'i>),
    /// ident(expr, expr, ...)
    FunctionCall((Binding<'i>, Span), Vec<&'a Expr<'a, 'i>>),
    /// ident(ident: typ, ident: typ, ...) -> typ { expr... }
    FunctionDefinition(Binding<'i>, Vec<(Binding<'i>, SpecificType)>, SpecificType, Vec<&'a Expr<'a, 'i>>),
}

impl<'a, 'i> fmt::Display for Expr<'a, 'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl<'a, 'i> fmt::Display for ExprType<'a, 'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::Unit => write!(f, "()", ),
            ExprType::Variable(binding) => write!(f, "{}", binding.ident),
            ExprType::Integer(i) => write!(f, "{}", i),
            ExprType::Float(fl) => write!(f, "{:2.1}", fl),
            ExprType::Bool(b) => write!(f, "{}", b),
            ExprType::String(s) => write!(f, "{:?}", s),
            ExprType::Bind(binding, expr) => {
                write!(f, "let ")?;
                if binding.mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{} = {}", binding.ident, expr)
            }
            ExprType::Assign((binding, _), expr) => write!(f, "({} = {})", binding.ident, expr),
            ExprType::LessThan(left, right) => write!(f, "({} < {})", left, right),
            ExprType::LessEquals(left, right) => write!(f, "({} <= {})", left, right),
            ExprType::Equals(left, right) => write!(f, "({} == {})", left, right),
            ExprType::NotEquals(left, right) => write!(f, "({} != {})", left, right),
            ExprType::FloatEquals(left, right) => write!(f, "({} ~~ {})", left, right),
            ExprType::FloatNotEquals(left, right) => write!(f, "({} !~ {})", left, right),
            ExprType::GreaterEquals(left, right) => write!(f, "({} >= {})", left, right),
            ExprType::GreaterThan(left, right) => write!(f, "({} > {})", left, right),
            ExprType::Add(a, b) => write!(f, "({} + {})", a, b),
            ExprType::Sub(a, b) => write!(f, "({} - {})", a, b),
            ExprType::Mul(a, b) => write!(f, "({} * {})", a, b),
            ExprType::Div(a, b) => write!(f, "({} / {})", a, b),
            ExprType::BoolAnd(a, b) => write!(f, "({} && {})", a, b),
            ExprType::BoolOr(a, b) => write!(f, "({} || {})", a, b),
            ExprType::BoolNot(b) => write!(f, "!{}", b),
            ExprType::Statement(expr) => write!(f, "{};", expr),
            ExprType::Block(exprs) => {
                writeln!(f, "{{")?;
                let mut padded = PadFmt::new(&mut *f);
                for expr in exprs {
                    writeln!(&mut padded, "{}", expr)?;
                }
                write!(f, "}}")
            },
            ExprType::Parenthezised(expr) => write!(f, "({})", expr),
            ExprType::FunctionCall((binding, _), exprs) => {
                write!(f, "{}({})", binding.ident, exprs.iter().join(", "))
            }
            ExprType::FunctionDefinition(name, args, ret_type, body) => {
                let args = args.iter()
                    .map(|(arg, typ)| format!("{}{}: {}", if arg.mutable { "mut "} else { "" }, arg.ident, typ)).join(", ");
                writeln!(f, "fn {}({}) -> {} {{", name.ident, args, ret_type)?;
                let mut padded = PadFmt::new(&mut *f);
                for expr in body {
                    writeln!(&mut padded, "{}", expr)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl<'a, 'i> Expr<'a, 'i> {
    pub fn new(span: Span, typ: ExprType<'a, 'i>) -> Expr<'a, 'i> {
        Expr { span, typ }
    }
}
