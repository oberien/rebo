use proc_macro2::TokenStream;
use proc_macro_error::abort;
use syn::{GenericParam, Generics, Ident};

pub fn generic_idents(generics: &Generics, context: &str) -> Vec<Ident> {
    let mut generic_idents = Vec::new();
    for generic in &generics.params {
        match generic {
            GenericParam::Lifetime(lifetime) => abort!(lifetime, "lifetimes are not allowed in {}", context),
            GenericParam::Const(const_param) => abort!(const_param, "const generics are not allowed in {}", context),
            GenericParam::Type(typ) => if !typ.bounds.is_empty() {
                abort!(typ.bounds, "generic bounds are not allowed in {}", context)
            } else {
                generic_idents.push(typ.ident.clone())
            }
        }
    }
    generic_idents
}

pub fn generic_spans(generic_idents: &Vec<Ident>, code_filename: &str, code_string: &str) -> Vec<TokenStream> {
    if generic_idents.is_empty() {
        Vec::new()
    } else {
        let start = code_string.find('<').unwrap() + 1;
        let end = code_string.find('>').unwrap();
        code_string[start..end].split(',')
            .map(str::trim)
            .map(|g| (g.as_ptr() as usize - code_string.as_ptr() as usize, g.len()))
            .map(|(start, len)| (start, start + len))
            .map(|(start, end)| quote::quote!(::rebo::Span::new(::rebo::FileId::synthetic(#code_filename), #start, #end)))
            .collect()
    }
}