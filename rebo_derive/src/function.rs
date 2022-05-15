use proc_macro_error::abort;
use syn::{ItemFn, Signature, FnArg, PatType, Ident, ReturnType, Type, parse_macro_input, Expr, Result, Pat, LitStr};
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::token::Paren;
use itertools::Itertools;
use proc_macro2::TokenStream as TokenStream2;
use crate::util;

struct Args {
    is_raw: bool,
    name: String,
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
            Ok(Args { is_raw: true, name })
        } else {
            Ok(Args { is_raw: false, name: input.parse::<LitStr>()?.value() })
        }
    }
}

fn clean_generics(typ: &Type, generic_idents: &Vec<Ident>) -> TokenStream2 {
    util::transform_path_type(typ, &|ident| if generic_idents.iter().any(|i| i == ident) {
        Some(quote::quote!(::rebo::Value))
    } else {
        None
    })
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
    let mut args = Vec::new();
    let mut varargs = None;
    let mut is_first_argument = true;

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
                        }
                        args.push((pat, ty));
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

    let rebo_name = macro_args.name;
    let code_filename = format!("external-{}.rs", rebo_name);
    // TODO: manually convert syn::Type to string to not have spaces in `Option < T >`
    let generics_string = if generic_idents.is_empty() {
        "".to_string()
    } else {
        format!("<{}>", generic_idents.iter().join(", "))
    };
    let args_string = args.iter()
        .map(|(pat, typ)| format!("{}: {}", pat.ident, util::convert_type_to_rebo(typ)))
        .chain(match &varargs {
            Some(Varargs::Typed(typ)) => Some(format!("{}...", util::convert_type_to_rebo(typ))),
            Some(Varargs::Untyped) => Some("...".to_string()),
            None => None,
        }).join(", ");
    let output_string = match &output {
        ReturnType::Default => "".to_string(),
        ReturnType::Type(_, typ) => format!("-> {}", util::convert_type_to_rebo(typ)),
    };
    let code_string = format!("fn {}{}({}) {} {{\n    ...\n}}", ident, generics_string, args_string, output_string);

    let arg_transforms: Vec<_> = args.into_iter()
        .map(|(pat, typ)| {
            let clean = clean_generics(&typ, &generic_idents);
            quote::quote!(let #pat: #clean = ::rebo::FromValue::from_value(args.next().unwrap());)
        }).collect();

    let (output_type, ret_val) = match output {
        ReturnType::Default => (
            quote::quote!{ () },
            quote::quote!(Ok(::rebo::IntoValue::into_value(res))),
        ),
        ReturnType::Type(_, typ) => match &*typ {
            Type::Never(_) => (
                quote::quote!(_),
                quote::quote!(res),
            ),
            _ => {
                let clean = clean_generics(&typ, &generic_idents);
                (clean, quote::quote!(Ok(::rebo::IntoValue::into_value(res))))
            }
        },
    };

    let shadow_code = if macro_args.is_raw {
        quote::quote!()
    } else {
        quote::quote! {
            let (expr_span, vm, args) = ((), (), ());
        }
    };
    let block = if macro_args.is_raw {
        quote::quote! { #block }
    } else {
        quote::quote! { (|| #block)() }
    };

    (quote::quote! {
        fn #fn_ident <'a, 'i> (expr_span: ::rebo::Span, vm: &mut ::rebo::VmContext<'a, '_, '_, 'i>, args: ::std::vec::Vec<::rebo::Value>) -> ::std::result::Result<::rebo::Value, ::rebo::ExecError<'a, 'i>> {
            let mut args = args.into_iter();
            #(
                #arg_transforms
            )*
            #shadow_code
            let res: #output_type = #block;
            #ret_val
        }

        #[allow(non_upper_case_globals)]
        #vis const #ident: ::rebo::ExternalFunction = ::rebo::ExternalFunction {
            name: #rebo_name,
            code: #code_string,
            file_name: #code_filename,
            imp: #fn_ident,
        };
    }).into()
}
