use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, SpecificType};
use crate::parser::ExprStructDefinition;
use crate::error_codes::ErrorCode;
use crate::lexer::TokenIdent;
use itertools::Itertools;

pub struct RecursiveStruct;

impl Visitor for RecursiveStruct {
    fn visit_struct_definition(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, def: &ExprStructDefinition) {
        let ExprStructDefinition { name, fields, .. } = def;
        for (ident, _colon, typ) in fields {
            check_struct_recursion(diagnostics, meta_info, name, &SpecificType::from(typ), vec![ident.ident]);
        }
    }
}

fn check_struct_recursion(diagnostics: &Diagnostics, meta_info: &MetaInfo, struct_name: &TokenIdent, field_typ: &SpecificType, field_path: Vec<&str>) {
    let field_struct_name = match &field_typ {
        SpecificType::Struct(s) => s.as_str(),
        _ => return,
    };
    // As we check every single struct, we only need to check if any recursive field is of the
    // same type as the struct we're checking.
    // We do not need to check if a subfield is recursive to one of its subfields, as that
    // will be caught when checking the struct definition of the type that subfield has.
    if struct_name.ident == field_struct_name {
        let path = field_path.iter().join(".");
        diagnostics.error(ErrorCode::RecursiveStruct)
            .with_error_label(struct_name.span, format!("this struct is recursive via `{}.{}`", struct_name.ident, path))
            .with_note("recursive structs can never be initialized")
            .emit();
        return
    }
    let typ = match meta_info.structs.get(field_struct_name) {
        Some((typ, _span)) => typ,
        None => return,
    };
    for (name, typ) in &typ.fields {
        let mut field_path = field_path.clone();
        field_path.push(name);
        check_struct_recursion(diagnostics, meta_info, struct_name, typ, field_path);
    }
}
