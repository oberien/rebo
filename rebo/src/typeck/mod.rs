use std::collections::HashMap;
use std::fmt;
use std::cmp::PartialEq;

use crate::diagnostics::{Diagnostics, Span};
use crate::parser::{Expr, Binding};

mod constraints;
mod solver;

use constraints::ConstraintCreator;
use crate::typeck::solver::ConstraintSolver;

#[derive(Debug, Clone)]
pub struct BindingTypes<'i> {
    pub types: HashMap<Binding<'i>, (Type, Span)>,
}

impl<'i> BindingTypes<'i> {
    pub fn new() -> BindingTypes<'i> {
        BindingTypes {
            types: HashMap::new(),
        }
    }

    pub fn get(&self, binding: Binding<'i>) -> Option<&(Type, Span)> {
        self.types.get(&binding)
    }
    pub fn insert(&mut self, binding: Binding<'i>, typ: Type, span: Span) {
        self.types.insert(binding, (typ, span));
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Integer,
    Float,
    String,
    Function(Box<FunctionType>),
    Any,
    Varargs,
    Bottom,
}
#[derive(Debug, Clone)]
pub struct FunctionType {
    pub args: &'static [Type],
    pub ret: Type,
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Any, _) | (_, Type::Any) | (Type::Varargs, _) | (_, Type::Varargs) => true,
            (Type::Bottom, _) | (_, Type::Bottom) => false,
            (Type::Unit, Type::Unit) => true,
            (Type::Integer, Type::Integer) => true,
            (Type::Float, Type::Float) => true,
            (Type::String, Type::String) => true,
            (Type::Function(a), Type::Function(b)) => {
                let FunctionType { args: args_a, ret: ret_a } = &**a;
                let FunctionType { args: args_b, ret: ret_b } = &**b;
                args_a == args_b && ret_a == ret_b
            },
            _ => false,
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Integer => write!(f, "integer"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Function(fun) => {
                let FunctionType { args, ret } = &**fun;
                write!(f, "fn(")?;
                for arg in *args {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ") -> {}", ret)
            },
            Type::Any => write!(f, "any"),
            Type::Varargs => write!(f, "varargs..."),
            Type::Bottom => write!(f, "⊥"),
        }
    }
}

pub struct Typechecker<'i> {
    diagnostics: &'i Diagnostics<'i>,
    binding_types: &'i mut BindingTypes<'i>,
}

impl<'i> Typechecker<'i> {
    pub fn new(diagnostics: &'i Diagnostics<'i>, binding_types: &'i mut BindingTypes<'i>) -> Typechecker<'i> {
        Typechecker {
            diagnostics,
            binding_types,
        }
    }

    // Step 1: Create constraint set
    // Step 2: Solve constraint set and output unification errors
    // Step 3: Check math operations and function calls, outputting type errors
    pub fn typeck(&mut self, exprs: &Vec<&Expr<'_, 'i>>) {
        let cc = ConstraintCreator::new(self.binding_types);
        let constraints = cc.get_constraints(exprs);
        trace!("got constraints: {:#?}", constraints.iter().map(ToString::to_string).collect::<Vec<_>>());
        let cs = ConstraintSolver::new(&self.diagnostics, self.binding_types);
        cs.solve(constraints);

        // self.diagnostics.error(ErrorCode::IncompatibleMathTypes)
        //     .with_error_label(expr.span, "in this math operation")
        //     .with_info_label(span_a, format!("this has type {}", typ_a))
        //     .with_info_label(span_b, format!("while this has type {}", typ_b))
        //     .with_note("math operations are only supported for integers with integers and floats with floats")
        //     .with_note("help: try casting operands like `... as integer` or `... as float`")
    }

}

