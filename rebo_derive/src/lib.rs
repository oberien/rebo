use proc_macro_error::{proc_macro_error, abort};
use syn::{ItemFn, Signature, FnArg, PatType, Ident, ReturnType, Type};
use proc_macro::TokenStream;

#[proc_macro_error]
#[proc_macro_attribute]
pub fn function(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as ItemFn);
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
    let mut input_pats = Vec::new();
    let mut input_types = Vec::new();

    for arg in inputs {
        match arg {
            FnArg::Receiver(recv) => abort!(recv, "self-arguments aren't allowed in static rebo functions"),
            FnArg::Typed(PatType { attrs, pat, colon_token: _, ty }) => {
                if !attrs.is_empty() {
                    abort!(attrs[0], "argument attributes are not supported for static rebo functions");
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
        #vis #constness #asyncness #unsafety #abi fn #fn_ident #generics (scopes: &mut ::rebo::scope::Scopes, args: ::std::vec::Vec<::rebo::types::Value>) -> ::rebo::types::Value {
            let (#(#input_pats,)*): (#(#input_types,)*) = ::rebo::types::FromValues::from_values(args.into_iter());
            let res: #output = #block;
            ::rebo::types::IntoValue::into_value(res)
        }

        #[allow(non_upper_case_globals)]
        const #ident: ::rebo::types::Function = ::rebo::types::Function {
            typ: ::rebo::typeck::FunctionType {
                args: &[#(<#input_types as ::rebo::types::FromValue>::TYPE),*],
                ret: <#output as ::rebo::types::FromValue>::TYPE,
            },
            imp: ::rebo::types::FunctionImpl::Rust(#fn_ident),
        };
    }.into();
    // eprintln!("{}", res);
    res
}
