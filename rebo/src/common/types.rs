use std::fmt;
use crate::parser::{ExprType, ExprLiteral};
use std::borrow::Cow;
use strum_macros::{EnumDiscriminants, EnumIter};

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type {
    /// Top-type, not yet unified, could be any type.
    Top,
    /// Bottom-type, unreachable type, return type of non-returning functions.
    Bottom,
    /// Varargs used by functions.
    Varargs,
    Specific(SpecificType),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, EnumDiscriminants)]
#[strum_discriminants(derive(EnumIter))]
pub enum SpecificType {
    Unit,
    Integer,
    Float,
    Bool,
    String,
    /// struct name
    Struct(String),
    // /// enum name
    // Enum(String),
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
impl StructType {
    pub fn get_field(&self, name: &str) -> Option<&SpecificType> {
        self.fields.iter()
            .filter(|(field_name, _typ)| field_name == name)
            .map(|(_name, typ)| typ)
            .next()
    }
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
            // ExprType::Enum(s) => SpecificType::Enum(s.ident.to_string()),
        }
    }
}
impl From<&ExprLiteral> for SpecificType {
    fn from(lit: &ExprLiteral) -> Self {
        match lit {
            ExprLiteral::Unit(_) => SpecificType::Unit,
            ExprLiteral::Integer(_) => SpecificType::Integer,
            ExprLiteral::Float(_) => SpecificType::Float,
            ExprLiteral::Bool(_) => SpecificType::Bool,
            ExprLiteral::String(_) => SpecificType::String,
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
            SpecificType::Struct(name) => write!(f, "{}", name),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Specific(t) => t.fmt(f),
            Type::Varargs => {
                write!(f, "varargs...")
            },
            Type::Top => write!(f, "any"),
            Type::Bottom => write!(f, "⊥"),
        }
    }
}

