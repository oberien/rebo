use std::fmt;
use crate::parser::ExprType;
use itertools::Either;
use std::borrow::Cow;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type {
    /// Top-type, not yet unified, could be any type.
    Top,
    /// Bottom-type, unreachable type, return type of non-returning functions.
    Bottom,
    /// Varargs used by functions.
    Varargs(Mutability),
    Specific(SpecificType),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum SpecificType {
    Unit,
    Integer,
    Float,
    Bool,
    String,
    /// struct name
    Struct(Mutability, String),
    // /// enum name
    // Enum(String),
}
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Mutability {
    Mutable,
    Immutable,
}
impl From<bool> for Mutability {
    fn from(b: bool) -> Self {
        if b { Mutability::Mutable } else { Mutability::Immutable }
    }
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
impl SpecificType {
    pub fn from(mutability: Mutability, typ: &ExprType) -> Self {
        match typ {
            ExprType::String(_) => SpecificType::String,
            ExprType::Int(_) => SpecificType::Integer,
            ExprType::Float(_) => SpecificType::Float,
            ExprType::Bool(_) => SpecificType::Bool,
            ExprType::Unit(_, _) => SpecificType::Unit,
            ExprType::Struct(s) => SpecificType::Struct(mutability, s.ident.to_string()),
            // ExprType::Enum(s) => SpecificType::Enum(s.ident.to_string()),
        }
    }
}

pub enum UnificationError {
    IncompatibleTypes,
    IncompatibleMutability,
}

enum Check {
    Exactly,
    Similar,
    Subtype,
}

impl Type {
    pub fn is_specific(&self) -> bool {
        matches!(self, Type::Specific(_))
    }
    /// Try to unify self with other exactly
    pub fn try_unify_exactly(&self, other: &Self) -> Result<Either<Type, Type>, UnificationError> {
        self.try_unify(other, Check::Exactly)
    }
    /// Try to unify self with other ignoring subtyping
    pub fn try_unify_similar(&self, other: &Self) -> Result<Either<Type, Type>, UnificationError> {
        self.try_unify(other, Check::Similar)
    }
    /// Try to unify self with other as self ⊆ other (self is a subtype of or equal to other)
    pub fn try_unify_subeq(&self, other: &Self) -> Result<Either<Type, Type>, UnificationError> {
        self.try_unify(other, Check::Subtype)
    }
    /// Try to unify self with other as self ⊇ other (self is a supertype of or equal to other)
    pub fn try_unify_supereq(&self, other: &Self) -> Result<Either<Type, Type>, UnificationError> {
        match other.try_unify(self, Check::Subtype) {
            Err(e) => Err(e),
            Ok(Either::Left(l)) => Ok(Either::Right(l)),
            Ok(Either::Right(l)) => Ok(Either::Left(l)),
        }
    }
    fn try_unify(&self, other: &Self, check: Check) -> Result<Either<Type, Type>, UnificationError> {
        match (self, other) {
            (t, Type::Top) => Ok(Either::Left(t.clone())),
            (Type::Top, t) => Ok(Either::Right(t.clone())),
            (t, Type::Bottom) => Ok(Either::Left(t.clone())),
            (Type::Bottom, t) => Ok(Either::Right(t.clone())),
            (t, Type::Varargs(_)) => Ok(Either::Left(t.clone())),
            (Type::Varargs(_), t) => Ok(Either::Right(t.clone())),
            (t @ Type::Specific(SpecificType::Unit), Type::Specific(SpecificType::Unit)) => Ok(Either::Left(t.clone())),
            (t @ Type::Specific(SpecificType::Integer), Type::Specific(SpecificType::Integer)) => Ok(Either::Left(t.clone())),
            (t @ Type::Specific(SpecificType::Float), Type::Specific(SpecificType::Float)) => Ok(Either::Left(t.clone())),
            (t @ Type::Specific(SpecificType::Bool), Type::Specific(SpecificType::Bool)) => Ok(Either::Left(t.clone())),
            (t @ Type::Specific(SpecificType::String), Type::Specific(SpecificType::String)) => Ok(Either::Left(t.clone())),
            (Type::Specific(SpecificType::Struct(a_mut, a)), Type::Specific(SpecificType::Struct(b_mut, b))) => {
                let left = Ok(Either::Left(Type::Specific(SpecificType::Struct(*a_mut, a.clone()))));
                let right = Ok(Either::Right(Type::Specific(SpecificType::Struct(*b_mut, b.clone()))));
                match check {
                    Check::Exactly => {
                        if a == b && a_mut == b_mut {
                            left
                        } else if a != b {
                            Err(UnificationError::IncompatibleTypes)
                        } else {
                            Err(UnificationError::IncompatibleMutability)
                        }
                    }
                    Check::Subtype => {
                        if *a_mut == Mutability::Mutable && *b_mut != Mutability::Mutable {
                            Err(UnificationError::IncompatibleMutability)
                        } else if a == b && *a_mut == Mutability::Mutable {
                            left
                        } else if a == b && *a_mut == Mutability::Immutable && *b_mut == Mutability::Mutable {
                            right
                        } else if a == b {
                            left
                        } else {
                            Err(UnificationError::IncompatibleTypes)
                        }
                    }
                    Check::Similar => {
                        if a == b && *a_mut == Mutability::Immutable && *b_mut == Mutability::Mutable {
                            right
                        } else if a == b {
                            left
                        } else {
                            Err(UnificationError::IncompatibleTypes)
                        }
                    }
                }
            }
            _ => Err(UnificationError::IncompatibleTypes),
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
            SpecificType::Struct(_mutability, name) => write!(f, "{}", name),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Specific(t) => t.fmt(f),
            Type::Varargs(mutability) => {
                if *mutability == Mutability::Mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "varargs...")
            },
            Type::Top => write!(f, "any"),
            Type::Bottom => write!(f, "⊥"),
        }
    }
}

