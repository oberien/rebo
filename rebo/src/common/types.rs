use std::fmt;
use crate::parser::ExprType;
use itertools::Either;
use std::borrow::Cow;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type {
    /// Top-type, not yet unified, could be any type.
    Top,
    /// Bottom-type, unreachable type, return type of non-returning functions, can indicate unification error.
    Bottom,
    /// Varargs used by functions.
    Varargs,
    Specific(SpecificType),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum SpecificType {
    Unit,
    Integer,
    Float,
    Bool,
    String,
    Function(Box<FunctionType>),
    /// struct name
    Struct(String),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub args: Cow<'static, [Type]>,
    pub ret: Type,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, SpecificType)>,
}
impl From<&ExprType<'_>> for SpecificType {
    fn from(typ: &ExprType) -> Self {
        match typ {
            ExprType::String(_) => SpecificType::String,
            ExprType::Int(_) => SpecificType::Integer,
            ExprType::Float(_) => SpecificType::Float,
            ExprType::Bool(_) => SpecificType::Bool,
            ExprType::Unit(_, _) => SpecificType::Unit,
            ExprType::Struct(s) => SpecificType::Struct(s.ident.to_string()),
        }
    }
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
            (Type::Specific(SpecificType::Struct(a)), Type::Specific(SpecificType::Struct(b))) if a == b => {
                Ok(Either::Left(Type::Specific(SpecificType::Struct(a.clone()))))
            }
            _ => Err(()),
        }
    }
}

impl fmt::Display for SpecificType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SpecificType::Unit => write!(f, "()"),
            SpecificType::Integer => write!(f, "int"),
            SpecificType::Float => write!(f, "float"),
            SpecificType::Bool => write!(f, "bool"),
            SpecificType::String => write!(f, "string"),
            SpecificType::Function(fun) => {
                let FunctionType { args, ret } = &**fun;
                write!(f, "fn(")?;
                for arg in &**args {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ") -> {}", ret)
            },
            SpecificType::Struct(name) => write!(f, "{}", name),
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

