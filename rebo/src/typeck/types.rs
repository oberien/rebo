use std::fmt;
use std::borrow::Cow;
use strum_macros::{EnumDiscriminants, EnumIter};
use crate::parser::ExprLiteral;
use diagnostic::Span;
use crate::CowVec;
use crate::typeck::graph::Node;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type {
    /// Top-type, could be any type. Used on type-conflict to allow linter to run.
    Top,
    /// Bottom-type, unreachable type, return type of non-returning functions.
    Bottom,
    /// Varargs used by functions.
    UntypedVarargs,
    /// Typed Varargs used by functions; all arguments must have the same type.
    TypedVarargs(SpecificType),
    Specific(SpecificType),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum SpecificType {
    Unit,
    Integer,
    Float,
    Bool,
    String,
    /// struct name, generics
    Struct(Cow<'static, str>, CowVec<'static, (Span, Type)>),
    /// enum name, generics
    Enum(Cow<'static, str>, CowVec<'static, (Span, Type)>),
    /// def_ident-span
    Generic(Span),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, EnumDiscriminants)]
#[strum_discriminants(derive(EnumIter))]
pub enum ResolvableSpecificType {
    Unit,
    Integer,
    Float,
    Bool,
    String,
    /// name, generics
    Struct(String, Vec<Node>),
    /// name, generics
    Enum(String, Vec<Node>),
    /// def_ident-span: a generic inside a function definition or impl-block definition, which must not
    /// unify except with itself (or some other generics)
    UnUnifyableGeneric(Span),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub is_method: bool,
    pub generics: Cow<'static, [Span]>,
    pub args: Cow<'static, [Type]>,
    pub ret: Type,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub generics: Cow<'static, [Span]>,
    pub name: String,
    pub fields: Vec<(String, Type)>,
}
impl StructType {
    pub fn get_field(&self, name: &str) -> Option<&Type> {
        self.fields.iter()
            .filter(|(field_name, _typ)| field_name == name)
            .map(|(_name, typ)| typ)
            .next()
    }
}
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash)]
pub struct EnumType {
    pub generics: Cow<'static, [Span]>,
    pub name: String,
    pub variants: Vec<(String, EnumTypeVariant)>,
}
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash)]
pub enum EnumTypeVariant {
    CLike,
    TupleVariant(Vec<Type>),
}
impl EnumTypeVariant {
    pub fn num_fields(&self) -> usize {
        match self {
            EnumTypeVariant::CLike => 0,
            EnumTypeVariant::TupleVariant(fields) => fields.len(),
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
impl From<&ExprLiteral> for ResolvableSpecificType {
    fn from(lit: &ExprLiteral) -> Self {
        match lit {
            ExprLiteral::Unit(_) => ResolvableSpecificType::Unit,
            ExprLiteral::Integer(_) => ResolvableSpecificType::Integer,
            ExprLiteral::Float(_) => ResolvableSpecificType::Float,
            ExprLiteral::Bool(_) => ResolvableSpecificType::Bool,
            ExprLiteral::String(_) => ResolvableSpecificType::String,
        }
    }
}

impl SpecificType {
    pub fn type_name(&self) -> String {
        match self {
            SpecificType::Unit => "()".to_string(),
            SpecificType::Integer => "int".to_string(),
            SpecificType::Float => "float".to_string(),
            SpecificType::Bool => "bool".to_string(),
            SpecificType::String => "string".to_string(),
            SpecificType::Struct(name, _) => name.clone().into_owned(),
            SpecificType::Enum(name, _) => name.clone().into_owned(),
            SpecificType::Generic(Span { file, start, end }) => format!("<{}:{}:{}>", file, start, end),
        }
    }
}
impl ResolvableSpecificType {
    pub fn type_name(&self) -> String {
        match self {
            ResolvableSpecificType::Unit => "()".to_string(),
            ResolvableSpecificType::Integer => "int".to_string(),
            ResolvableSpecificType::Float => "float".to_string(),
            ResolvableSpecificType::Bool => "bool".to_string(),
            ResolvableSpecificType::String => "string".to_string(),
            ResolvableSpecificType::Struct(name, _) => name.clone(),
            ResolvableSpecificType::Enum(name, _) => name.clone(),
            ResolvableSpecificType::UnUnifyableGeneric(Span { file, start, end }) => format!("<{}:{}:{}>", file, start, end),
        }
    }
    pub fn generics(&self) -> Vec<Node> {
        match self {
            ResolvableSpecificType::Unit
            | ResolvableSpecificType::Integer
            | ResolvableSpecificType::Float
            | ResolvableSpecificType::Bool
            | ResolvableSpecificType::String
            | ResolvableSpecificType::UnUnifyableGeneric(_) => Vec::new(),
            ResolvableSpecificType::Struct(_, generics) => generics.clone(),
            ResolvableSpecificType::Enum(_, generics) => generics.clone(),
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
            SpecificType::Struct(name, _) => write!(f, "struct {}", name),
            SpecificType::Enum(name, _) => write!(f, "enum {}", name),
            SpecificType::Generic(Span { file, start, end }) => write!(f, "<{}:{}:{}>", file, start, end),
        }
    }
}
impl fmt::Display for ResolvableSpecificType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (typ, name, generics) = match self {
            ResolvableSpecificType::Unit => return write!(f, "()"),
            ResolvableSpecificType::Integer => return write!(f, "int"),
            ResolvableSpecificType::Float => return write!(f, "float"),
            ResolvableSpecificType::Bool => return write!(f, "bool"),
            ResolvableSpecificType::String => return write!(f, "string"),
            ResolvableSpecificType::Struct(name, generics) => ("struct", name, generics),
            ResolvableSpecificType::Enum(name, generics) => ("enum", name, generics),
            ResolvableSpecificType::UnUnifyableGeneric(Span { file, start, end }) => return write!(f, "<{}:{}:{}>", file, start, end),
        };
        write!(f, "{} {}", typ, name)?;
        if !generics.is_empty() {
            write!(f, "<")?;
            for generic in generics {
                match generic {
                    Node::TypeVar(var) => write!(f, "[{}:{}:{}]", var.span.file, var.span.start, var.span.end)?,
                    Node::Synthetic(_, id) => write!(f, "[{}]", id)?,
                }
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Specific(t) => t.fmt(f),
            Type::TypedVarargs(t) => {
                write!(f, "{}...", t)
            },
            Type::UntypedVarargs => {
                write!(f, "varargs...")
            },
            Type::Top => write!(f, "any"),
            Type::Bottom => write!(f, "!"),
        }
    }
}

