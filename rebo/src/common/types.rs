use std::fmt;
use crate::parser::Binding;
use std::collections::HashMap;
use itertools::Either;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Top-type, not yet unified, could be any type.
    Top,
    /// Bottom-type, unreachable type, return type of non-returning functions, can indicate unification error.
    Bottom,
    /// Varargs used by functions.
    Varargs,
    Specific(SpecificType),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SpecificType {
    Unit,
    Integer,
    Float,
    Bool,
    String,
    Function(Box<FunctionType>),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub args: &'static [Type],
    pub ret: Type,
}

/// Info needed before parsing / before typechecking
pub struct PreTypeInfo<'i> {
    /// bindings of the root scope / stdlib and function definitions of the first parser pass
    pub bindings: HashMap<Binding<'i>, SpecificType>,
}

impl Type {
    pub fn is_specific(&self) -> bool {
        matches!(self, Type::Specific(_))
    }
    pub fn try_unify(&self, other: &Self) -> Result<Either<Type, Type>, ()> {
        match (self, other) {
            (t, Type::Top) => Ok(Either::Left(t.clone())),
            (Type::Top, t) => Ok(Either::Right(t.clone())),
            (Type::Bottom, _) => Ok(Either::Left(Type::Bottom)),
            (_, Type::Bottom) => Ok(Either::Right(Type::Bottom)),
            (t, Type::Varargs) => Ok(Either::Left(t.clone())),
            (Type::Varargs, t) => Ok(Either::Right(t.clone())),
            (t @ Type::Specific(SpecificType::Unit), Type::Specific(SpecificType::Unit)) => Ok(Either::Left(t.clone())),
            (t @ Type::Specific(SpecificType::Integer), Type::Specific(SpecificType::Integer)) => Ok(Either::Left(t.clone())),
            (t @ Type::Specific(SpecificType::Float), Type::Specific(SpecificType::Float)) => Ok(Either::Left(t.clone())),
            (t @ Type::Specific(SpecificType::Bool), Type::Specific(SpecificType::Bool)) => Ok(Either::Left(t.clone())),
            (t @ Type::Specific(SpecificType::String), Type::Specific(SpecificType::String)) => Ok(Either::Left(t.clone())),
            (Type::Specific(SpecificType::Function(a)), Type::Specific(SpecificType::Function(b))) => {
                let FunctionType { args: args_a, ret: ret_a } = &**a;
                let FunctionType { args: args_b, ret: ret_b } = &**b;
                let same_args = args_a.iter().zip(args_b.iter()).all(|(a, b)| a == b);
                let same_ret = ret_a == ret_b;
                if same_args && same_ret {
                    Ok(Either::Left(Type::Specific(SpecificType::Function(a.clone()))))
                } else {
                    Err(())
                }
            },
            _ => Err(()),
        }
    }
}

impl fmt::Display for SpecificType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SpecificType::Unit => write!(f, "()"),
            SpecificType::Integer => write!(f, "integer"),
            SpecificType::Float => write!(f, "float"),
            SpecificType::Bool => write!(f, "bool"),
            SpecificType::String => write!(f, "string"),
            SpecificType::Function(fun) => {
                let FunctionType { args, ret } = &**fun;
                write!(f, "fn(")?;
                for arg in *args {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ") -> {}", ret)
            },
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Specific(t) => t.fmt(f),
            Type::Varargs => write!(f, "varargs..."),
            Type::Top => write!(f, "any"),
            Type::Bottom => write!(f, "⊥"),
        }
    }
}

