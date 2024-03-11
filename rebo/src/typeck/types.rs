use std::fmt;
use std::borrow::Cow;
use strum_macros::{EnumDiscriminants, EnumIter};
use crate::parser::ExprLiteral;
use itertools::Itertools;
use crate::typeck::graph::Node;
use crate::common::SpanId;

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
    Struct(String, Vec<(SpanId, Type)>),
    /// enum name, generics
    Enum(String, Vec<(SpanId, Type)>),
    /// generics, args, ret
    Function(Box<FunctionType>),
    /// def_ident-span
    Generic(SpanId),
    /// def-span
    Any(SpanId),
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
    /// None means could be any function
    Function(Option<FunctionType>),
    /// def_ident-span: a generic inside a function definition or impl-block definition, which must not
    /// unify except with itself (or some other generics)
    UnUnifyableGeneric(SpanId),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub is_method: bool,
    pub generics: Cow<'static, [SpanId]>,
    pub args: Cow<'static, [Type]>,
    pub ret: Type,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub generics: Cow<'static, [SpanId]>,
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
    pub generics: Cow<'static, [SpanId]>,
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
            SpecificType::Function(_) => "function".to_string(),
            SpecificType::Generic(span_id) => format!("<{span_id}>"),
            SpecificType::Any(span_id) => format!("any<{span_id}>"),
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            SpecificType::Unit
            | SpecificType::Integer
            | SpecificType::Float
            | SpecificType::Bool
            | SpecificType::String => true,
            SpecificType::Struct(..)
            | SpecificType::Enum(..)
            | SpecificType::Function(_)
            | SpecificType::Generic(..)
            | SpecificType::Any(_) => false,
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
            ResolvableSpecificType::Function(_) => "function".to_string(),
            ResolvableSpecificType::UnUnifyableGeneric(span_id) => format!("<{span_id}>"),
        }
    }
    pub fn generics(&self) -> Vec<Node> {
        match self {
            ResolvableSpecificType::Unit
            | ResolvableSpecificType::Integer
            | ResolvableSpecificType::Float
            | ResolvableSpecificType::Bool
            | ResolvableSpecificType::String
            | ResolvableSpecificType::UnUnifyableGeneric(_)
            | ResolvableSpecificType::Function(_) => Vec::new(),
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
            SpecificType::Struct(name, generics) => {
                let generics = generics.iter().map(|(g, _)| format!("<{g}>")).join(", ");
                write!(f, "struct {}<{}>", name, generics)
            },
            SpecificType::Enum(name, generics) => {
                let generics = generics.iter().map(|(g, _)| format!("<{g}>")).join(", ");
                write!(f, "enum {}<{}>", name, generics)
            },
            SpecificType::Function(fun) => {
                let generics = fun.generics.iter().map(|g| format!("<{g}>")).join(", ");
                write!(f, "fn<{}>({}) -> {}", generics, fun.args.iter().join(", "), fun.ret)
            }
            SpecificType::Generic(span_id) => write!(f, "<{span_id}>"),
            SpecificType::Any(span_id) => write!(f, "any<{span_id}>"),
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
            ResolvableSpecificType::Function(None) => return write!(f, "fn?"),
            ResolvableSpecificType::Function(Some(fun)) => {
                let generics = fun.generics.iter().map(|g| format!("<{g}>")).join(", ");
                return write!(f, "fn<{}>({}) -> {}", generics, fun.args.iter().join(", "), fun.ret)
            }
            ResolvableSpecificType::UnUnifyableGeneric(span_id) => return write!(f, "<{span_id}>"),
        };
        write!(f, "{} {}", typ, name)?;
        if !generics.is_empty() {
            write!(f, "<")?;
            for generic in generics {
                match generic {
                    Node::TypeVar(var) => write!(f, "[{}]", var)?,
                    Node::Synthetic(span, id) => write!(f, "[{}<{}:{}:{}>]", id, span.file, span.start, span.end)?,
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

