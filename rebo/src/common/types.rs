use std::fmt;

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

impl Type {
    pub fn is_unifyable_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Top, _) | (_, Type::Top) => true,
            (Type::Bottom, _) | (_, Type::Bottom) => true,
            (Type::Varargs, _) | (_, Type::Varargs) => true,
            (Type::Specific(SpecificType::Unit), Type::Specific(SpecificType::Unit)) => true,
            (Type::Specific(SpecificType::Integer), Type::Specific(SpecificType::Integer)) => true,
            (Type::Specific(SpecificType::Float), Type::Specific(SpecificType::Float)) => true,
            (Type::Specific(SpecificType::Bool), Type::Specific(SpecificType::Bool)) => true,
            (Type::Specific(SpecificType::String), Type::Specific(SpecificType::String)) => true,
            (Type::Specific(SpecificType::Function(a)), Type::Specific(SpecificType::Function(b))) => {
                let FunctionType { args: args_a, ret: ret_a } = &**a;
                let FunctionType { args: args_b, ret: ret_b } = &**b;
                args_a.into_iter().zip(args_b.into_iter()).all(|(a, b)| a.is_unifyable_with(b))
                    && ret_a.is_unifyable_with(ret_b)
            },
            _ => false,
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

