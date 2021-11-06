use proc_macro_error::abort;
use syn::{ItemFn, Signature, FnArg, PatType, Ident, ReturnType, Type, Token, parse_macro_input, Expr, Result, Pat, LitStr, PathArguments, GenericArgument};
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::token::Paren;
use itertools::Itertools;
use proc_macro2::TokenStream as TokenStream2;
use crate::util;

enum Args {
    Raw {
        name: String,
        ret_typ: Option<Expr>,
    },
    Name(String),
}
impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Ident) {
            let ident: Ident = input.parse()?;
            if ident != "raw" {
                abort!(input.parse::<Expr>().unwrap(), "rebo::function only allows a name-string or a raw-specifier");
            }
            let content;
            let _: Paren = syn::parenthesized!(content in input);
            let name = content.parse::<LitStr>()?.value();

            // ret_typ
            if !content.peek(Token![,]) {
                return Ok(Args::Raw { name, ret_typ: None });
            }
            let _: Token![,] = content.parse()?;
            let _: Token![return] = content.parse()?;
            let ret_typ = content.parse()?;
            Ok(Args::Raw { name, ret_typ: Some(ret_typ) })
        } else {
            Ok(Args::Name(input.parse::<LitStr>()?.value()))
        }
    }
}
impl Args {
    fn name(&self) -> &str {
        match self {
            Args::Raw { name, .. } => name,
            Args::Name(name) => name,
        }
    }
    fn is_raw(&self) -> bool {
        match self {
            Args::Raw { .. } => true,
            Args::Name(_) => false,
        }
    }
    fn unwrap_raw_ret_typ(&self) -> Option<Expr> {
        match self {
            Args::Raw { ret_typ, .. } => ret_typ.as_ref().cloned(),
            _ => panic!("Args::unwrap_raw_ret_typ called with a non-raw Args"),
        }
    }
}

fn clean_generics(typ: &Type, generic_idents: &Vec<Ident>) -> TokenStream2 {
    let path = match typ {
        Type::Path(path) => path,
        typ => return quote::quote!(#typ),
    };
    let mut res = quote::quote!();
    for segment in &path.path.segments {
        let ident = &segment.ident;
        if !res.is_empty() {
            res = quote::quote!(#res::);
        }
        if generic_idents.iter().any(|i| ident == i) {
            res = quote::quote!(#res Value);
        } else {
            res = quote::quote!(#res #ident);
        }
        let generics = match &segment.arguments {
            PathArguments::None => continue,
            PathArguments::Parenthesized(_) => unreachable!("PathArguments::Parenthesized"),
            PathArguments::AngleBracketed(generics) => generics,
        };
        if !generics.args.is_empty() {
            res = quote::quote!(#res::<);
        }
        for arg in &generics.args {
            match arg {
                GenericArgument::Lifetime(_) => unreachable!("GenericArgument::Lifetime"),
                GenericArgument::Type(Type::Path(path)) if generic_idents.iter().any(|i| path.path.is_ident(i)) => {
                    res = quote::quote!(#res Value,);
                }
                GenericArgument::Type(typ) => {
                    let clean = clean_generics(&typ, generic_idents);
                    res = quote::quote!(#res #clean,);
                }
                GenericArgument::Binding(_) => unreachable!("GenericArgument::Binding"),
                GenericArgument::Constraint(_) => unreachable!("GenericArgument::Constraint"),
                GenericArgument::Const(_) => unreachable!("GenericArgument::Const"),
            }
        }
        if !generics.args.is_empty() {
            res = quote::quote!(#res>);
        }
    }
    res
}

enum Varargs {
    Typed(Type),
    Untyped,
}

pub fn function(args: TokenStream, input: TokenStream) -> TokenStream {
    let macro_args = parse_macro_input!(args as Args);
    let input = parse_macro_input!(input as ItemFn);
    let ItemFn {
        attrs: _,
        vis,
        sig: Signature {
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
        },
        block,
    } = input;

    if let Some(where_clause) = generics.where_clause {
        abort!(where_clause, "where clauses are not allowed in rebo functions");
    }
    if let Some(constness) = constness {
        abort!(constness, "const rebo functions are not allowed");
    }
    if let Some(asyncness) = asyncness {
        abort!(asyncness, "async rebo functions are not allowed");
    }
    if let Some(unsafety) = unsafety {
        abort!(unsafety, "unsafe rebo functions are not allowed; use an internal unsafe block instead");
    }
    if let Some(abi) = abi {
        abort!(abi, "non-default abi rebo functions are not allowed");
    }
    if let Some(variadic) = variadic {
        abort!(variadic, "if you want to use varargs, use `..: _` or `..: T` instead");
    }

    let generic_idents = util::generic_idents(&generics, "rebo functions");

    let fn_ident = format!("{}_fn", ident);
    let fn_ident = Ident::new(&fn_ident, ident.span());
    // https://github.com/rust-lang/rust/issues/86672
    let workaround_ident = format!("{}_workaround_issue_86672", ident);
    let workaround_ident = Ident::new(&workaround_ident, ident.span());
    let mut args = Vec::new();
    let mut varargs = None;
    let mut is_first_argument = true;
    let mut is_method = false;

    let inputs_len = inputs.len();
    for (i, arg) in inputs.into_iter().enumerate() {
        let is_last_argument = i+1 == inputs_len;
        match arg {
            FnArg::Receiver(recv) => abort!(recv, "self-arguments aren't allowed; if you want a self-argument, use `this` as name of the first argument instead"),
            FnArg::Typed(PatType { attrs, pat, colon_token: _, ty }) => {
                if !attrs.is_empty() {
                    abort!(attrs[0], "argument attributes are not supported for static rebo functions");
                }
                match *pat {
                    Pat::Ident(pat) => {
                        if pat.ident.to_string() == "this" {
                            if !is_first_argument {
                                abort!(pat, "this-argument must be the first argument");
                            }
                            is_method = true;
                        }
                        args.push((pat.ident, ty));
                    }
                    Pat::Rest(rest) => {
                        if !is_last_argument {
                            abort!(rest, "varargs are only allowed as last argument in a function")
                        }
                        match &*ty {
                            Type::Infer(_) => varargs = Some(Varargs::Untyped),
                            _ => varargs = Some(Varargs::Typed(*ty)),
                        }
                    }
                    _ => abort!(pat, "arguments must be identifiers or `..` for varargs in rebo functions"),
                }
            }
        }
        is_first_argument = false;
    }

    let rebo_name = macro_args.name();
    let code_filename = format!("external-{}.rs", rebo_name);
    // TODO: manually convert syn::Type to string to not have spaces in `Option < T >`
    let generics_string = if generic_idents.is_empty() {
        "".to_string()
    } else {
        format!("<{}>", generic_idents.iter().join(", "))
    };
    let args_string = args.iter()
        .map(|(pat, typ)| format!("{}: {}", pat, quote::quote!(#typ)))
        .chain(match &varargs {
            Some(Varargs::Typed(typ)) => Some(format!("{}...", quote::quote!(#typ))),
            Some(Varargs::Untyped) => Some("...".to_string()),
            None => None,
        }).join(", ");
    let code_string = format!("fn {}{}({}) {} {{\n    ...\n}}", ident, generics_string, args_string, quote::quote!(#output));
    let generic_spans = util::generic_spans(&generic_idents, &code_filename, &code_string);

    let (arg_transforms, argument_rebo_types) = args.into_iter()
        .map(|(pat, typ)| match &*typ {
            Type::Path(path) if generic_idents.iter().any(|i| path.path.is_ident(i)) => {
                let i = generic_idents.iter().position(|i| path.path.is_ident(i)).unwrap();
                let generic_span = generic_spans[i].clone();
                let input = quote::quote!(let #pat: Value = args.next().unwrap(););
                let typ = quote::quote!(::rebo::Type::Specific(::rebo::SpecificType::Generic(#generic_span)));
                (input, typ)
            }
            _ => {
                let clean = clean_generics(&typ, &generic_idents);
                let input = quote::quote!(let #pat: #clean = ::rebo::FromValue::from_value(args.next().unwrap()););
                let typ = quote::quote!(::rebo::Type::Specific(<#clean as ::rebo::Typed>::TYPE));
                (input, typ)
            }
        }).unzip::<_, _, Vec<_>, Vec<_>>();
    let varargs = match varargs {
        Some(Varargs::Typed(Type::Path(path))) if generic_idents.iter().any(|i| path.path.is_ident(i)) => {
            let i = generic_idents.iter().position(|i| path.path.is_ident(i)).unwrap();
            let generic_span = generic_spans[i].clone();
            quote::quote!(::rebo::Type::TypedVarargs(::rebo::SpecificType::Generic(#generic_span)))
        }
        Some(Varargs::Typed(typ)) => {
            let clean = clean_generics(&typ, &generic_idents);
            quote::quote!(::rebo::Type::TypedVarargs(<#clean as ::rebo::Typed>::TYPE))
        },
        Some(Varargs::Untyped) => quote::quote!(::rebo::Type::UntypedVarargs),
        None => quote::quote!(),
    };

    let (output_type, ret_val, ret_rebo_type) = match output {
        ReturnType::Default => (
            quote::quote!{ () },
            quote::quote!(Ok(::rebo::IntoValue::into_value(res))),
            quote::quote!(::rebo::Type::Specific(<() as ::rebo::Typed>::TYPE)),
        ),
        ReturnType::Type(_, typ) => match &*typ {
            _ if macro_args.is_raw() && macro_args.unwrap_raw_ret_typ().is_some() => {
                let ret_rebo_typ = macro_args.unwrap_raw_ret_typ().unwrap();
                (
                    quote::quote!(::std::result::Result<::rebo::Value, ::rebo::ExecError>),
                    quote::quote!(res),
                    quote::quote!(#ret_rebo_typ),
                )
            },
            Type::Path(path) if generic_idents.iter().any(|i| path.path.is_ident(i)) => {
                let i = generic_idents.iter().position(|i| path.path.is_ident(i)).unwrap();
                let generic_span = generic_spans[i].clone();
                let output_type = quote::quote!(Value);
                let ret_val = quote::quote!(Ok(res));
                let ret_rebo_type = quote::quote!(::rebo::Type::Specific(::rebo::SpecificType::Generic(#generic_span)));
                (output_type, ret_val, ret_rebo_type)
            }
            _ => {
                let clean = clean_generics(&typ, &generic_idents);
                let ret_rebo_type = quote::quote!(::rebo::Type::Specific(<#clean as ::rebo::Typed>::TYPE));
                (
                    clean,
                    quote::quote!(Ok(::rebo::IntoValue::into_value(res))),
                    ret_rebo_type,
                )
            }
        },
    };

    let shadow_code = if macro_args.is_raw() {
        quote::quote!()
    } else {
        quote::quote! {
            let (expr_span, vm, args) = ((), (), ());
        }
    };

    (quote::quote! {
        fn #fn_ident (expr_span: ::rebo::Span, vm: &mut ::rebo::VmContext, args: ::std::vec::Vec<::rebo::Value>) -> ::std::result::Result<::rebo::Value, ::rebo::ExecError> {
            let mut args = args.into_iter();
            #(
                #arg_transforms
            )*
            #shadow_code
            let res: #output_type = #block;
            #ret_val
        }

        #[allow(non_upper_case_globals)]
        const #workaround_ident: &'static [::rebo::Type] =
            &[#(#argument_rebo_types,)* #varargs];
        #[allow(non_upper_case_globals)]
        #vis const #ident: ::rebo::ExternalFunction = ::rebo::ExternalFunction {
            name: #rebo_name,
            code: #code_string,
            file_name: #code_filename,
            typ: ::rebo::FunctionType {
                is_method: #is_method,
                generics: ::std::borrow::Cow::Borrowed(&[#(#generic_spans),*]),
                args: ::std::borrow::Cow::Borrowed(#workaround_ident),
                ret: #ret_rebo_type,
            },
            imp: #fn_ident,
        };
    }).into()
}
