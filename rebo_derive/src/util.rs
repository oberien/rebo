use proc_macro2::TokenStream;
use proc_macro_error::abort;
use syn::{GenericArgument, GenericParam, Generics, Ident, PathArguments, Type};
use syn::__private::TokenStream2;

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

pub fn generic_spans(generic_idents: &[Ident], code_filename: &str, code_string: &str) -> Vec<TokenStream> {
    if generic_idents.is_empty() {
        Vec::new()
    } else {
        let start = code_string.find('<').unwrap() + 1;
        let end = code_string.find('>').unwrap();
        code_string[start..end].split(',')
            .map(str::trim)
            .map(|g| (g.as_ptr() as usize - code_string.as_ptr() as usize, g.len()))
            .map(|(start, len)| (start, start + len))
            .map(|(start, end)| quote::quote!(::rebo::Span::new(::rebo::SyntheticFileId::new(#code_filename), #start, #end)))
            .collect()
    }
}

pub fn transform_path_type(typ: &Type, callback: &impl Fn(&Ident) -> Option<TokenStream2>) -> TokenStream2 {
    let path = match typ {
        Type::Path(path) => path,
        typ => return quote::quote!(#typ),
    };
    let mut res = quote::quote!();
    if let Some(ident) = path.path.get_ident() {
        if let Some(res) = callback(ident) {
            return res;
        }
    }

    for segment in &path.path.segments {
        if !res.is_empty() {
            res = quote::quote!(#res::);
        }
        let ident = match callback(&segment.ident) {
            Some(name) => name,
            None => {
                let ident = &segment.ident;
                quote::quote!(#ident)
            },
        };
        res = quote::quote!(#res #ident);
        let generics = match &segment.arguments {
            PathArguments::None => continue,
            PathArguments::Parenthesized(par) => abort!(par, "generics can only be <...>"),
            PathArguments::AngleBracketed(generics) => generics,
        };
        if generics.args.is_empty() {
            continue;
        }

        res = quote::quote!(#res<);
        for arg in &generics.args {
            match arg {
                GenericArgument::Type(typ) => {
                    let transformed = transform_path_type(typ, callback);
                    res = quote::quote!(#res #transformed,);
                }
                _ => abort!(arg, "generics can only be types"),
            }
        }
        res = quote::quote!(#res>);
    }
    res
}

pub fn convert_type_to_rebo(typ: &Type) -> TokenStream2 {
    transform_path_type(typ, &|ident| {
        #[allow(clippy::if_same_then_else)]
        if ident == "f32" { Some(quote::quote!(float)) }
        else if ident == "f64" { Some(quote::quote!(float)) }
        else if ident == "FuzzyFloat" { Some(quote::quote!(float)) }
        else if ident == "u8" { Some(quote::quote!(int)) }
        else if ident == "i8" { Some(quote::quote!(int)) }
        else if ident == "u16" { Some(quote::quote!(int)) }
        else if ident == "i16" { Some(quote::quote!(int)) }
        else if ident == "u32" { Some(quote::quote!(int)) }
        else if ident == "i32" { Some(quote::quote!(int)) }
        else if ident == "u64" { Some(quote::quote!(int)) }
        else if ident == "i64" { Some(quote::quote!(int)) }
        else if ident == "usize" { Some(quote::quote!(int)) }
        else if ident == "isize" { Some(quote::quote!(int)) }
        else if ident == "String" { Some(quote::quote!(string)) }
        else if ident == "Vec" { Some(quote::quote!(List)) }
        else { None }
    })
}

