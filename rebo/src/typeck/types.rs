use std::fmt;
use std::borrow::Cow;
use strum_macros::{EnumDiscriminants, EnumIter};
use crate::parser::ExprLiteral;
use diagnostic::Span;
use crate::common::MetaInfo;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Type {
    /// Top-type, could be any type. Used on type-conflict to allow linter to run.
    Top,
    /// Bottom-type, unreachable type, return type of non-returning functions.
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
    /// struct name
    Struct(String),
    /// enum name
    Enum(String),
    Generic(Span),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum ResolvableType {
    Top,
    Bottom,
    Varargs,
    Specific(ResolvableSpecificType),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, EnumDiscriminants)]
#[strum_discriminants(derive(EnumIter))]
pub enum ResolvableSpecificType {
    Unit,
    Integer,
    Float,
    Bool,
    String,
    Struct(String),
    Enum(String),
    /// a generic inside a function definition or impl-block definition, which must not
    /// unify except with itself (or some other generics)
    UnUnifyableGeneric(Span),
    /// a generic from a binding that should unify
    UnifyableGeneric(Span),
}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub args: Cow<'static, [Type]>,
    pub ret: Type,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
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
    pub fn get_field_path(&self, meta_info: &MetaInfo, fields: impl IntoIterator<Item = impl AsRef<str>>) -> Result<Type, usize> {
        let mut typ = Type::Specific(SpecificType::Struct(self.name.clone()));
        for (i, field) in fields.into_iter().enumerate() {
            let struct_name = match &typ {
                Type::Specific(SpecificType::Struct(name)) => name,
                _ => return Err(i),
            };
            let struct_type = &meta_info.struct_types[struct_name.as_str()];
            let field_type = struct_type.get_field(field.as_ref());
            match field_type {
                Some(field_typ) => typ = field_typ.clone(),
                None => return Err(i),
            }
        }
        Ok(typ)
    }
}
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash)]
pub struct EnumType {
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
impl From<&ResolvableSpecificType> for SpecificType {
    fn from(typ: &ResolvableSpecificType) -> Self {
        match typ {
            ResolvableSpecificType::Unit => SpecificType::Unit,
            ResolvableSpecificType::Bool => SpecificType::Bool,
            ResolvableSpecificType::Integer => SpecificType::Integer,
            ResolvableSpecificType::Float => SpecificType::Float,
            ResolvableSpecificType::String => SpecificType::String,
            ResolvableSpecificType::Struct(name) => SpecificType::Struct(name.clone()),
            ResolvableSpecificType::Enum(name) => SpecificType::Enum(name.clone()),
            &ResolvableSpecificType::UnUnifyableGeneric(span)
            | &ResolvableSpecificType::UnifyableGeneric(span) => SpecificType::Generic(span),
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
            SpecificType::Struct(name) => name.clone(),
            SpecificType::Enum(name) => name.clone(),
            SpecificType::Generic(Span { file, start, end }) => format!("<{},{},{}>", file, start, end),
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
            ResolvableSpecificType::Struct(name) => name.clone(),
            ResolvableSpecificType::Enum(name) => name.clone(),
            ResolvableSpecificType::UnUnifyableGeneric(Span { file, start, end }) => format!("X<{},{},{}>", file, start, end),
            ResolvableSpecificType::UnifyableGeneric(Span { file, start, end }) => format!("V<{},{},{}>", file, start, end),
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
            SpecificType::Struct(name) => write!(f, "struct {}", name),
            SpecificType::Enum(name) => write!(f, "enum {}", name),
            SpecificType::Generic(Span { file, start, end }) => write!(f, "<{},{},{}>", file, start, end),
        }
    }
}
impl fmt::Display for ResolvableSpecificType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolvableSpecificType::Unit => write!(f, "()"),
            ResolvableSpecificType::Integer => write!(f, "int"),
            ResolvableSpecificType::Float => write!(f, "float"),
            ResolvableSpecificType::Bool => write!(f, "bool"),
            ResolvableSpecificType::String => write!(f, "string"),
            ResolvableSpecificType::Struct(name) => write!(f, "struct {}", name),
            ResolvableSpecificType::Enum(name) => write!(f, "enum {}", name),
            ResolvableSpecificType::UnUnifyableGeneric(Span { file, start, end }) => write!(f, "X<{},{},{}>", file, start, end),
            ResolvableSpecificType::UnifyableGeneric(Span { file, start, end }) => write!(f, "V<{},{},{}>", file, start, end),
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

