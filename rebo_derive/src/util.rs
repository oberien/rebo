use std::collections::HashMap;
use proc_macro2::{TokenStream, Span};
use proc_macro_error::abort;
use quote::ToTokens;
use syn::{GenericArgument, GenericParam, Generics, Ident, PathArguments, Type, spanned::Spanned, Signature, FnArg, Pat, PatType, ReturnType, Token, TypeBareFn, BareFnArg};
use syn::punctuated::{Pair, Punctuated};

#[derive(PartialEq)]
pub enum Bounds {
    Reject,
    #[allow(unused)]
    Allow,
}

/// Convert the Generics of a type or function to a list of their Idents and trait bounds (if bounds should not be rejected)
///
/// Given `A: Foo, B, C: Bar` this function will return `(vec![A, B, C], vec![Foo, , Bar])`
pub fn parse_generics(generics: &Generics, context: &str, bounds: Bounds) -> (Vec<Ident>, Vec<TokenStream>) {
    generics.params.iter().map(|generic| {
        match generic {
            GenericParam::Lifetime(lifetime) => abort!(lifetime, "lifetimes are not allowed in {}", context),
            GenericParam::Const(const_param) => abort!(const_param, "const generics are not allowed in {}", context),
            GenericParam::Type(typ) => {
                if !typ.bounds.is_empty() && bounds == Bounds::Reject {
                    abort!(typ.bounds, "generic bounds are not allowed in {}", context)
                }
                if typ.default.is_some() {
                    abort!(typ.default, "default generics not allowed in {}", context);
                }
                (typ.ident.clone(), typ.bounds.to_token_stream())
            }
        }
    }).unzip()
}

/// Get rebo-SpanWithId with synthetic rebo-Span for the generics inside the code_string
///
/// code_string contains the generated stubbed rebo code for a type including its generics.
/// This function returns the TokenStreams representing the rebo-compiler-Spans of the generics.
pub fn generic_span_with_ids(generic_idents: &[Ident], code_filename: &str, code_string: &str) -> Vec<TokenStream> {
    if generic_idents.is_empty() {
        Vec::new()
    } else {
        let start = code_string.find('<').unwrap() + 1;
        let end = code_string.find('>').unwrap();
        code_string[start..end].split(',')
            .map(str::trim)
            .inspect(|g| assert!(generic_idents.iter().any(|expected| expected == *g), "generic {} not found in generic-list {:?}", g, generic_idents))
            .map(|g| (g.as_ptr() as usize - code_string.as_ptr() as usize, g.len()))
            .map(|(start, len)| (start, start + len))
            .map(|(start, end)| quote::quote!(::rebo::SpanWithId::new(::rebo::FileId::synthetic_named(#code_filename), #start, #end)))
            .collect()
    }
}

enum Transform {
    ContinueWithGenerics(TokenStream),
    IgnoreGenerics(TokenStream),
}
impl Transform {
    fn into_token_stream(self) -> TokenStream {
        match self {
            Transform::ContinueWithGenerics(ts) => ts,
            Transform::IgnoreGenerics(ts) => ts,
        }
    }
}

/// Convert a syn-Type to a TokenStream with the callback being called for each encountered Ident.
///
/// For `Option<HashMap<i32, String>>` the callback will be called with `Option`, `HashMap`,
/// `i32` and `String`.
fn transform_path_type(typ: &Type, callback: &impl Fn(&Ident, Option<&Punctuated<GenericArgument, Token![,]>>) -> Option<Transform>) -> TokenStream {
    let path = match typ {
        Type::Path(path) => path,
        typ => return quote::quote_spanned!(typ.span()=> #typ),
    };
    let mut res = quote::quote_spanned!(typ.span()=> );
    if let Some(ident) = path.path.get_ident() {
        if let Some(res) = callback(ident, None) {
            return res.into_token_stream();
        }
    }

    for segment in &path.path.segments {
        if !res.is_empty() {
            res = quote::quote_spanned!(segment.span()=> #res::);
        }

        let generics = match &segment.arguments {
            PathArguments::None => None,
            PathArguments::Parenthesized(par) => abort!(par, "generics can only be <...>"),
            PathArguments::AngleBracketed(generics) => Some(&generics.args),
        };

        let (ignore_generics, ident) = match callback(&segment.ident, generics) {
            Some(Transform::ContinueWithGenerics(name)) => (false, name),
            Some(Transform::IgnoreGenerics(name)) => (true, name),
            None => {
                let ident = &segment.ident;
                let name = quote::quote_spanned!(ident.span()=> #ident);
                (false, name)
            },
        };
        res = quote::quote_spanned!(segment.span()=> #res #ident);
        if ignore_generics || generics.is_none() || generics.unwrap().is_empty() {
            continue;
        }
        let generic_args = match generics {
            _ if ignore_generics => continue,
            None => continue,
            Some(args) => args,
        };

        res = quote::quote_spanned!(segment.span()=> #res<);
        for arg in generic_args {
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
    transform_path_type(typ, &|ident, generics| {
        #[allow(clippy::if_same_then_else)]
        Some(Transform::ContinueWithGenerics(
            if ident == "f32" { quote::quote_spanned!(ident.span()=> float) }
            else if ident == "f64" { quote::quote_spanned!(ident.span()=> float) }
            else if ident == "FuzzyFloat" { quote::quote_spanned!(ident.span()=> float) }
            else if ident == "u8" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "i8" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "u16" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "i16" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "u32" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "i32" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "u64" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "i64" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "usize" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "isize" { quote::quote_spanned!(ident.span()=> int) }
            else if ident == "String" { quote::quote_spanned!(ident.span()=> string) }
            else if ident == "Vec" { quote::quote_spanned!(ident.span()=> List) }
            else if ident == "TypedFunctionValue" {
                let generics = generics.expect("TypedFunctionValue must have one generic");
                assert_eq!(generics.len(), 1);
                let TypeBareFn {
                    lifetimes, unsafety, abi, fn_token, paren_token: _, inputs, variadic, output
                } = match &generics[0] {
                    GenericArgument::Type(Type::BareFn(function)) => function,
                    arg => abort!(arg, "TypedFunctionValue allows only a single function type as generic"),
                };
                if let Some(lifetimes) = lifetimes {
                    abort!(lifetimes, "lifetimes are not allowed in TypedFunctionValue");
                }
                if let Some(unsafety) = unsafety {
                    abort!(unsafety, "unsafety is not allowed in TypedFunctionValue");
                }
                if let Some(abi) = abi {
                    abort!(abi, "abi is not allowed in TypedFunctionValue");
                }
                if let Some(variadic) = variadic {
                    abort!(variadic, "variadic is not allowed in TypedFunctionValue");
                }
                let inputs: Punctuated<TokenStream, Token![,]> = inputs.clone().into_pairs().map(|pair| {
                    let (BareFnArg { attrs, name: _, ty }, comma) = match pair {
                        Pair::Punctuated(typ, comma) => (typ, Some(comma)),
                        Pair::End(typ) => (typ, None),
                    };
                    if !attrs.is_empty() {
                        abort!(attrs[0], "attributes are not allowed in TypedReboFunctionn");
                    }
                    let typ = convert_type_to_rebo(&ty);
                    match comma {
                        Some(comma) => Pair::Punctuated(typ, comma),
                        None => Pair::End(typ),
                    }
                }).collect();
                let output = match output {
                    ReturnType::Default => quote::quote_spanned!(output.span()=> ),
                    ReturnType::Type(arrow, typ) => {
                        let typ = convert_type_to_rebo(typ);
                        quote::quote_spanned!(output.span()=> #arrow #typ)
                    }
                };
                return Some(Transform::IgnoreGenerics(quote::quote_spanned! { generics.span()=> #fn_token(#inputs) #output }))
            }
            else { return None }
        ))
    })
}

pub struct FunctionSignature {
    pub ident: Ident,
    pub generic_idents: Vec<Ident>,
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

    let (generic_idents, _) = parse_generics(&generics, context, Bounds::Reject);

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
        is_method,
        arg_idents,
        arg_types,
        varargs,
        output,
    }
}

/// Replace generics recursively in the passed type with `::rebo::Value`
///
/// Both `T` and the `T` in `Option<T>` will be replaced.
pub fn replace_generics_with_value(typ: &Type, generic_idents: &[Ident]) -> TokenStream {
    replace_generics_with(typ, generic_idents, |ident| quote::quote_spanned!(ident.span()=> ::rebo::Value))
}
/// Replace generics in the passed type with the TokenStream returned from the callback
///
/// Both `T` will and the `T` in `Option<T>` will be replaced.
pub fn replace_generics_with(typ: &Type, generic_idents: &[Ident], with: impl Fn(&Ident) -> TokenStream) -> TokenStream {
    transform_path_type(typ, &|ident, _| if generic_idents.iter().any(|i| i == ident) {
        let with = with(ident);
        Some(Transform::ContinueWithGenerics(quote::quote_spanned!(ident.span()=> #with)))
    } else {
        Some(Transform::ContinueWithGenerics(ident.into_token_stream()))
    })
}
/// Convert a syn::Type to a rebo::Type, using the passed `SpanWithId`(-getter).
///
/// The `SpanWithId` getter could be the span directly, or anything returning the SpanWithId
/// (e.g. a value cached in a static `OnceLock`):
/// * ::rebo::SpanWithId::new(...)
/// * OnceLock::get_or_init(|| ...)
///
/// Does the following transformation:
/// * `T` -> `Type::Specific(SpecificType::Generic(generic_span_with_id))`
/// * `Option<T>` -> `Type::Specific(<Option<Value> as Typed>::typ())`
pub fn convert_type_to_reboc_type(typ: &Type, generic_idents: &[Ident], generics_span_with_ids: &HashMap<Ident, TokenStream>) -> TokenStream {
    match typ {
        Type::Path(path) if path.path.get_ident().is_some() && generics_span_with_ids.contains_key(path.path.get_ident().unwrap()) => {
            let ident = path.path.get_ident().unwrap();
            let span_with_id = &generics_span_with_ids.get(ident)
                .expect(&format!("ident `{ident}` not found in generic_idents {generic_idents:?}"));
            quote::quote_spanned!(typ.span()=> ::rebo::Type::Specific(::rebo::SpecificType::Generic(*#span_with_id)))
        }
        Type::Never(_) => quote::quote_spanned!(typ.span()=> ::rebo::Type::Bottom),
        _ => {
            let cleaned = replace_generics_with_value(typ, generic_idents);
            quote::quote_spanned!(typ.span()=> ::rebo::Type::Specific(<#cleaned as ::rebo::Typed>::typ()))
        }
    }
}

