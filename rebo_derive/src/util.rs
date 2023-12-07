use std::collections::HashMap;
use proc_macro2::{TokenStream, Span};
use proc_macro_error::abort;
use quote::ToTokens;
use syn::{GenericArgument, GenericParam, Generics, Ident, PathArguments, Type, spanned::Spanned, Signature, FnArg, Pat, PatType, ReturnType};

/// Convert the Generics of a type or function to a list of their Idents and types
pub fn parse_generics(generics: &Generics, context: &str) -> (Vec<Ident>, Vec<TokenStream>) {
    generics.params.iter().map(|generic| {
        match generic {
            GenericParam::Lifetime(lifetime) => abort!(lifetime, "lifetimes are not allowed in {}", context),
            GenericParam::Const(const_param) => abort!(const_param, "const generics are not allowed in {}", context),
            GenericParam::Type(typ) => {
                // if !typ.bounds.is_empty() {
                //     abort!(typ.bounds, "generic bounds are not allowed in {}", context)
                // }
                if typ.default.is_some() {
                    abort!(typ.default, "default generics not allowed in {}", context);
                }
                (typ.ident.clone(), typ.bounds.to_token_stream())
            }
        }
    }).unzip()
}

/// Get rebo-Synthetic-Spans inside the code_string to the passed Generic-Idents
///
/// code_string contains the generated stubbed rebo code for a type including its generics.
/// This function returns the TokenStreams representing the rebo-compiler-Spans of the generics.
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
            .map(|(start, end)| quote::quote!(::rebo::Span::new(::rebo::FileId::synthetic(#code_filename), #start, #end)))
            .collect()
    }
}

/// Convert a syn-Type to a TokenStream with the callback being called for each encountered Ident.
///
/// For `Option<HashMap<i32, String>>` the callback will be called with `Option`, `HashMap`,
/// `i32` and `String`.
pub fn transform_path_type(typ: &Type, callback: &impl Fn(&Ident) -> Option<TokenStream>) -> TokenStream {
    let path = match typ {
        Type::Path(path) => path,
        typ => return quote::quote_spanned!(typ.span()=> #typ),
    };
    let mut res = quote::quote_spanned!(typ.span()=> );
    if let Some(ident) = path.path.get_ident() {
        if let Some(res) = callback(ident) {
            return res;
        }
    }

    for segment in &path.path.segments {
        if !res.is_empty() {
            res = quote::quote_spanned!(segment.span()=> #res::);
        }
        let ident = match callback(&segment.ident) {
            Some(name) => name,
            None => {
                let ident = &segment.ident;
                quote::quote_spanned!(ident.span()=> #ident)
            },
        };
        res = quote::quote_spanned!(segment.span()=> #res #ident);
        let generics = match &segment.arguments {
            PathArguments::None => continue,
            PathArguments::Parenthesized(par) => abort!(par, "generics can only be <...>"),
            PathArguments::AngleBracketed(generics) => generics,
        };
        if generics.args.is_empty() {
            continue;
        }

        res = quote::quote_spanned!(segment.span()=> #res<);
        for arg in &generics.args {
            match arg {
                GenericArgument::Type(typ) => {
                    let transformed = transform_path_type(typ, callback);
                    res = quote::quote_spanned!(arg.span()=> #res #transformed,);
                }
                _ => abort!(arg, "generics can only be types"),
            }
        }
        res = quote::quote_spanned!(segment.span()=> #res>);
    }
    quote::quote_spanned!(typ.span()=> #res)
}

/// Convert a syn-Type to its corresponding rebo-code type, including Generics (e.g. `Vec<i32>` -> `List<int>`)
pub fn convert_type_to_rebo(typ: &Type) -> TokenStream {
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

pub struct FunctionSignature {
    pub ident: Ident,
    pub generic_idents: Vec<Ident>,
    pub generic_bounds: Vec<TokenStream>,
    pub is_method: bool,
    pub arg_idents: Vec<Ident>,
    pub arg_types: Vec<Type>,
    pub varargs: Option<Varargs>,
    pub output: ReturnType,
}

pub struct Varargs {
    pub span: Span,
    pub kind: VarargsKind,
}
impl Varargs {
    pub fn untyped(span: Span) -> Varargs { Varargs { span, kind: VarargsKind::Untyped } }
    pub fn typed(typ: Type, span: Span) -> Varargs { Varargs { span, kind: VarargsKind::Typed(typ) } }
}

#[allow(clippy::large_enum_variant)]
pub enum VarargsKind {
    Typed(Type),
    Untyped,
}

pub fn parse_function_signature(sig: Signature, context: &str) -> FunctionSignature {
    let Signature {
        constness,
        asyncness,
        unsafety,
        abi,
        fn_token: _,
        ident,
        generics,
        paren_token: _,
        inputs,
        variadic,
        output,
    } = sig;

    if let Some(where_clause) = generics.where_clause {
        abort!(where_clause, "where clauses are not allowed in {}", context);
    }
    if let Some(constness) = constness {
        abort!(constness, "const {} are not allowed", context);
    }
    if let Some(asyncness) = asyncness {
        abort!(asyncness, "async {} are not allowed", context);
    }
    if let Some(unsafety) = unsafety {
        abort!(unsafety, "unsafe {} are not allowed; use an internal unsafe block instead", context);
    }
    if let Some(abi) = abi {
        abort!(abi, "non-default abi {} are not allowed", context);
    }
    if let Some(variadic) = variadic {
        abort!(variadic, "if you want to use varargs for {ctx}, use `..: _` or `..: T` instead");
    }

    let (generic_idents, generic_bounds) = parse_generics(&generics, context);

    let mut varargs = None;
    let mut is_method = false;
    let inputs_len = inputs.len();
    let (arg_idents, arg_types) = inputs.into_iter().enumerate().filter_map(|(i, arg)| match arg {
        FnArg::Receiver(recv) => abort!(recv, "self-arguments aren't allowed; if you want a self-argument, use `this` as name of the first argument instead"),
        FnArg::Typed(PatType { attrs, pat, colon_token: _, ty }) => {
            if !attrs.is_empty() {
                abort!(attrs[0], "argument attributes are not supported for {}", context);
            }
            let is_last_argument = i+1 == inputs_len;
            match *pat {
                Pat::Ident(pat) => {
                    if pat.ident == "this" {
                        if i != 0 {
                            abort!(pat, "this-argument must be the first argument in {}", context);
                        }
                        is_method = true;
                    }
                    Some((pat.ident, *ty))
                }
                Pat::Rest(rest) => {
                    if !is_last_argument {
                        abort!(rest, "varargs are only allowed as last argument in {}", context);
                    }
                    match &*ty {
                        Type::Infer(_) => varargs = Some(Varargs::untyped(variadic.span())),
                        _ => varargs = Some(Varargs::typed(*ty, variadic.span())),
                    }
                    None
                }
                _ => abort!(pat, "arguments must be identifiers or `..` for varargs in {}", context),
            }
        }
    }).unzip::<_, _, Vec<_>, Vec<_>>();

    FunctionSignature {
        ident,
        generic_idents,
        generic_bounds,
        is_method,
        arg_idents,
        arg_types,
        varargs,
        output,
    }
}

/// Replace generics recursively in the passed type with `::rebo::Value`
///
/// Both `T` will and the `T` in `Option<T>` will be replaced.
pub fn replace_generics_with_value(typ: &Type, generic_idents: &[Ident]) -> TokenStream {
    replace_generics_with(typ, generic_idents, |_| quote::quote!(::rebo::Value))
}
/// Replace generics in the passed type with the TokenStream returned from the callback
///
/// Both `T` will and the `T` in `Option<T>` will be replaced.
pub fn replace_generics_with(typ: &Type, generic_idents: &[Ident], with: impl Fn(&Ident) -> TokenStream) -> TokenStream {
    transform_path_type(typ, &|ident| if generic_idents.iter().any(|i| i == ident) {
        let with = with(ident);
        Some(quote::quote_spanned!(ident.span()=> #with))
    } else {
        Some(ident.into_token_stream())
    })
}
/// Convert a syn::Type to a rebo::Type
///
/// * `T` -> `Type::Specific(SpecificType::Generic(span))`
/// * `Option<T>` -> `Type::Specific(<Option<Value> as Typed>::TYPE)`
pub fn convert_type_to_reboc_type(typ: &Type, generic_idents: &[Ident], generics_spans: &HashMap<Ident, TokenStream>) -> TokenStream {
    match typ {
        Type::Path(path) if path.path.get_ident().is_some() && generics_spans.contains_key(path.path.get_ident().unwrap()) => {
            let ident = path.path.get_ident().unwrap();
            let span = &generics_spans.get(ident)
                .expect(&format!("ident `{ident}` not found in generics_spans {generics_spans:?}"));
            quote::quote_spanned!(typ.span()=> ::rebo::Type::Specific(::rebo::SpecificType::Generic(#span)))
        }
        Type::Never(_) => quote::quote_spanned!(typ.span()=> ::rebo::Type::Bottom),
        _ => {
            let cleaned = replace_generics_with_value(typ, generic_idents);
            quote::quote_spanned!(typ.span()=> ::rebo::Type::Specific(<#cleaned as ::rebo::Typed>::TYPE))
        }
    }
}

