use std::fmt;
use std::borrow::Cow;
use strum_macros::{EnumDiscriminants, EnumIter};
use crate::parser::{ExprLiteral, ExprType, Spanned};
use diagnostic::{Diagnostics, Span};
use crate::common::{MetaInfo, UserType};
use crate::error_codes::ErrorCode;

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
    /// enum name
    Enum(String),
    /// a generic variable inside a function or impl-block
    Generic(Span),
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
impl Type {
    pub fn from_expr_type(typ: &ExprType, diagnostics: &Diagnostics, meta_info: &MetaInfo) -> Self {
        match typ {
            ExprType::String(_) => Type::Specific(SpecificType::String),
            ExprType::Int(_) => Type::Specific(SpecificType::Integer),
            ExprType::Float(_) => Type::Specific(SpecificType::Float),
            ExprType::Bool(_) => Type::Specific(SpecificType::Bool),
            ExprType::Unit(_, _) => Type::Specific(SpecificType::Unit),
            ExprType::UserType(ut, _generics) => {
                match meta_info.user_types.get(ut.ident) {
                    Some(UserType::Struct(s)) => Type::Specific(SpecificType::Struct(s.name.ident.to_string())),
                    Some(UserType::Enum(e)) => Type::Specific(SpecificType::Enum(e.name.ident.to_string())),
                    None => {
                        let similar = crate::util::similar_name(ut.ident, meta_info.user_types.keys());
                        let mut diag = diagnostics.error(ErrorCode::UnknownType)
                            .with_error_label(typ.span(), "can't find type with this name");
                        if let Some(similar) = similar {
                            diag = diag.with_info_label(typ.span(), format!("did you mean `{}`", similar));
                        }
                        diag.emit();
                        // hack to make the type resolve regularly even though we don't have any information
                        Type::Top
                    }
                }
            },
            ExprType::Generic(g) => Type::Specific(SpecificType::Generic(g.def_ident.span)),
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
            SpecificType::Generic(Span { file, start, end }) => write!(f, "<{},{},{}>", file, start, end)
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

