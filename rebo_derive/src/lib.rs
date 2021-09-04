use proc_macro_error::{proc_macro_error, abort};
use syn::{ItemFn, Signature, FnArg, PatType, Ident, ReturnType, Type, DeriveInput, Token, parse_macro_input, Expr, Result, Attribute, Data, DataUnion, DataStruct, Visibility, Pat, PatIdent};
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::token::{Union, Struct};

#[proc_macro_error]
#[proc_macro_attribute]
pub fn function(_args: TokenStream, input: TokenStream) -> TokenStream {
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

    let fn_ident = format!("{}_fn", ident);
    let fn_ident = Ident::new(&fn_ident, ident.span());
    // https://github.com/rust-lang/rust/issues/86672
    let workaround_ident = format!("{}_workaround_issue_86672", ident);
    let workaround_ident = Ident::new(&workaround_ident, ident.span());
    let mut input_pats = Vec::new();
    let mut input_muts = Vec::new();
    let mut input_types = Vec::new();

    for arg in inputs {
        match arg {
            FnArg::Receiver(recv) => abort!(recv, "self-arguments aren't allowed in static rebo functions"),
            FnArg::Typed(PatType { attrs, pat, colon_token: _, ty }) => {
                if !attrs.is_empty() {
                    abort!(attrs[0], "argument attributes are not supported for static rebo functions");
                }
                if let Pat::Ident(PatIdent { mutability, .. }) = &*pat {
                    match mutability {
                        Some(_) => input_muts.push(quote::quote!(::rebo::common::Mutability::Mutable)),
                        None => input_muts.push(quote::quote!(::rebo::common::Mutability::Immutable)),
                    }
                } else {
                    abort!(pat, "arguments must be identifiers for static rebo functions");
                }
                input_pats.push(pat);
                input_types.push(ty);
            }
        }
    }

    let output = match output {
        ReturnType::Default => Type::Verbatim(quote::quote!{ () }),
        ReturnType::Type(_, typ) => *typ,
    };

    if variadic.is_some() {
        abort!(variadic, "variadics not allowed for static rebo functions");
    }

    let res = quote::quote! {
        #vis #constness #asyncness #unsafety #abi fn #fn_ident #generics (scopes: &mut ::rebo::scope::Scopes, args: ::std::vec::Vec<::rebo::common::Value>) -> ::rebo::common::Value {
            let (#(#input_pats,)*): (#(#input_types,)*) = ::rebo::common::FromValues::from_values(args.into_iter());
            let res: #output = #block;
            ::rebo::common::IntoValue::into_value(res)
        }

        #[allow(non_upper_case_globals)]
        const #workaround_ident: &'static [::rebo::common::Type] =
            &[#(::rebo::common::Type::Specific(<#input_types as ::rebo::common::FromValue>::TYPE)),*];
        #[allow(non_upper_case_globals)]
        const #ident: ::rebo::common::Function = ::rebo::common::Function {
            typ: ::rebo::common::FunctionType {
                args: ::std::borrow::Cow::Borrowed(#workaround_ident),
                ret: ::rebo::common::Type::Specific(<#output as ::rebo::common::FromValue>::TYPE),
            },
            imp: ::rebo::common::FunctionImpl::Rust(#fn_ident),
        };
    }.into();
    // eprintln!("{}", res);
    res
}

/// For internal use only
#[doc(hidden)]
#[proc_macro_derive(Functions, attributes(function))]
pub fn functions(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    struct Function {
        visibility: Visibility,
        fn_signature: Signature,
        _eq: Token![=],
        name: Ident,
        _arrow: Token![=>],
        expr: Expr,
    }
    impl Parse for Function {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            syn::parenthesized!(content in input);
            Ok(Function {
                visibility: content.parse()?,
                fn_signature: content.parse()?,
                _eq: content.parse()?,
                name: content.parse()?,
                _arrow: content.parse()?,
                expr: content.parse()?,
            })
        }
    }

    let enum_name = input.ident;
    let generics = input.generics;
    let functions: Result<Vec<_>> = input.attrs.into_iter()
        .map(|Attribute { tokens, .. }| syn::parse2::<Function>(tokens))
        .collect();
    let functions = match functions {
        Ok(f) => f,
        Err(err) => return TokenStream::from(err.to_compile_error())
    };
    let variants: Vec<Ident> = match input.data {
        Data::Enum(e) => e.variants.into_iter()
            .map(|variant| variant.ident)
            .collect(),
        Data::Struct(DataStruct { struct_token: Struct { span }, ..})
        | Data::Union(DataUnion { union_token: Union { span }, ..}) => abort!(span, "only enums are supported"),
    };
    let num_variants = variants.len();
    let variants = vec![variants; functions.len()];

    let mut function_visibilities = Vec::new();
    let mut function_signatures = Vec::new();
    let mut variable_names = Vec::new();
    let mut exprs = Vec::new();
    for function in functions {
        function_visibilities.push(function.visibility);
        function_signatures.push(function.fn_signature);
        variable_names.push(vec![function.name; num_variants]);
        exprs.push(vec![function.expr; num_variants]);
    }

    let res = quote::quote! {
        impl #generics #enum_name #generics {
            #(
                #function_visibilities #function_signatures {
                    match self {
                        #(
                            #enum_name::#variants(#variable_names) => #exprs,
                        )*
                    }
                }
            )*
        }
    }.into();
    // eprintln!("{}", res);
    res
}
