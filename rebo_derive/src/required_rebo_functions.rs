use proc_macro_error::abort;
use syn::{parse_macro_input, Ident, ItemForeignMod, ForeignItem, ForeignItemFn, ReturnType, parse_quote_spanned, Visibility, spanned::Spanned, Type};
use proc_macro::TokenStream;
use std::collections::HashMap;
use proc_macro2::TokenStream as TokenStream2;
use crate::util;
use crate::util::{VarargsKind, FunctionSignature};

pub fn required_rebo_functions(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemForeignMod);
    let ItemForeignMod { attrs: _, abi, brace_token: _, items } = input;
    if abi.name.is_none() || abi.name.as_ref().unwrap().value() != "rebo" {
        abort!(abi, "required rebo functions must have abi \"rebo\": `extern \"rebo\" { ... }");
    }

    let mut res = quote::quote!();
    for item in items {
        let f = match item {
            ForeignItem::Fn(f) => f,
            _ => abort!(item, "extern \"rebo\" blocks only support functions"),
        };
        let ForeignItemFn { attrs, vis, sig, semi_token: _ } = f;

        if !attrs.is_empty() {
            abort!(attrs[0], "required rebo functions can't have attributes");
        }

        let sig = util::parse_function_signature(sig, "required rebo functions");
        let fn_ident = format!("{}_fn", sig.ident);
        let fn_ident = Ident::new(&fn_ident, sig.ident.span());

        if sig.varargs.is_some() && sig.arg_idents.len() > 1 {
            abort!(sig.arg_idents[1], "required rebo functions don't support varargs as non-only argument (yet)");
        }


        let function = generate_fn(&fn_ident, &sig);
        let struc = generate_struct(vis, fn_ident, &sig);
        let imp = generate_impl(&sig);

        res = quote::quote! {
            #res

            #function
            #struc
            #imp
        };
    }

    res.into()
}


fn generate_fn(fn_ident: &Ident, sig: &FunctionSignature) -> TokenStream2 {
    let ident = &sig.ident;
    let generic_idents = &sig.generic_idents;
    let (arg_idents, arg_types, into_value_conversions) = match &sig.varargs {
        Some(varargs) => (
            vec![Ident::new("args", varargs.span)],
            vec![parse_quote_spanned!(varargs.span=> ::std::vec::Vec<::rebo::Value>)],
            quote::quote_spanned!(varargs.span=> args),
        ),
        None => {
            let arg_types = &sig.arg_types;
            let arg_idents = &sig.arg_idents;
            let into_value_code = quote::quote!(vec![#(
                <#arg_types as ::rebo::IntoValue>::into_value(#arg_idents),
            )*]);
            (arg_idents.clone(), sig.arg_types.clone(), into_value_code)
        },
    };
    let ret_type = match &sig.output {
        ReturnType::Default => quote::quote_spanned!(sig.output.span()=> ()),
        ReturnType::Type(_arrow, typ) => match &**typ {
            Type::Never(_) => quote::quote_spanned!(typ.span()=> ::std::convert::Infallible),
            _ => quote::quote!(#typ),
        }
    };
    quote::quote! {
        fn #fn_ident<'a, 'i, #(#generic_idents: ::rebo::FromValue + ::rebo::IntoValue),*>(vm: &mut ::rebo::VmContext<'a, '_, '_, 'i>, #(#arg_idents: #arg_types),*) -> Result<#ret_type, ::rebo::ExecError<'a, 'i>> {
            let values = #into_value_conversions;
            let res = vm.call_required_rebo_function::<#ident>(values)?;
            // unreachable_code annotation required if the function returns the never type
            #[allow(unreachable_code)]
            Ok(<#ret_type as ::rebo::FromValue>::from_value(res))
        }
    }
}

fn generate_struct(vis: Visibility, fn_ident: Ident, sig: &FunctionSignature) -> TokenStream2 {
    let ident = &sig.ident;
    let value_arg_types = sig.arg_types.iter()
        .map(|typ| util::replace_generics_with_value(typ, &sig.generic_idents))
        .collect::<Vec<_>>();
    let value_arg_types = match &sig.varargs {
        Some(varargs) => vec![quote::quote_spanned!(varargs.span=> ::std::vec::Vec<::rebo::Value>)],
        None => value_arg_types,
    };
    let value_ret_type = match &sig.output {
        ReturnType::Default => quote::quote!(()),
        ReturnType::Type(_arrow, typ) => match &**typ {
            Type::Never(_) => quote::quote_spanned!(typ.span()=> ::std::convert::Infallible),
            _ => util::replace_generics_with_value(typ, &sig.generic_idents),
        }
    };
    quote::quote! {
        #[allow(non_camel_case_types)]
        #vis struct #ident;
        impl ::std::ops::Deref for #ident {
            type Target = for<'a, 'i> fn(&mut ::rebo::VmContext<'a, '_, '_, 'i>, #(#value_arg_types),*) -> Result<#value_ret_type, ::rebo::ExecError<'a, 'i>>;
            fn deref(&self) -> &Self::Target {
                &(#fn_ident as Self::Target)
            }
        }
    }
}

fn generate_impl(sig: &FunctionSignature) -> TokenStream2 {
    let ident = &sig.ident;
    let is_method = sig.is_method;
    let rebo_function_name = ident.to_string();
    // TODO: handle methods via an annotation (?)
    // let rebo_function_name = ident.to_string().replace("__", "::");
    // let receiver_type_string = rebo_function_name.split("::").next().unwrap();
    // let receiver_type_string = if receiver_type_string == rebo_function_name { None } else { Some(receiver_type_string) };
    // if receiver_type_string.is_none() {
    //     abort!(pat, "rebo methods are named `Type::method(...)`, so the extern function name should be `Type__method`");
    // }
    // match &*ty {
    //     Type::Path(path) => if path.path.segments.iter().next().unwrap().ident != receiver_type_string.as_ref().unwrap() {
    //         abort!(pat, "rebo method named `Type::method(...)` take `Type` as first param; the first extern function argument should be `this: {}` or `this: {0}<...>`", receiver_type_string.unwrap());
    //     }
    //     _ => abort!(ty, "this-argument must be {}", receiver_type_string.as_ref().unwrap()),
    // }

    // example: `fn id<T>(t: T) -> T { t }`
    // We may need to use the same generic both in the arguments and in the return type.
    // The SpanId associated with the generic must be the same in both cases.
    // Therefore, we need to create them once as a OnceLock and use that instance wherever the
    // generic is used as type anywhere as (sub-)type.

    // Names of the `static T: OnceLock = OnceLock::new();`
    let generics_mod = Ident::new(&format!("{ident}_generics_mod"), ident.span());
    let generics_static_once_names: Vec<_> = sig.generic_idents.iter()
        .map(|ident| Ident::new(&format!("{ident}_SPAN_ONCE").to_uppercase(), ident.span()))
        .collect();
    let generic_ident_strings = sig.generic_idents.iter().map(|ident| ident.to_string()).collect::<Vec<_>>();
    let generics_file_name = format!("external-required-rebo-function-{ident}-generics.re");
    // Getters of the static generic `SpanWithId`s: `OnceLock::get_or_init(|| ...)
    let mut generics_spans = HashMap::new();
    let mut generics_file_content = String::new();
    for (generic_ident, static_once_name) in sig.generic_idents.iter().zip(&generics_static_once_names) {
        let start = generics_file_content.len();
        generics_file_content.push_str(&generic_ident.to_string());
        let end = generics_file_content.len();
        generics_file_content.push_str("\n\n");
        generics_spans.insert(generic_ident.clone(), quote::quote_spanned!{generic_ident.span()=>
            self::#generics_mod::#static_once_name.get_or_init(|| ::rebo::SpanWithId::new(::rebo::FileId::synthetic_named(#generics_file_name), #start, #end))
        });
    }

    let reboc_arg_types = sig.arg_types.iter()
        .map(|typ| util::convert_type_to_reboc_type(typ, &sig.generic_idents, &generics_spans))
        .collect::<Vec<_>>();
    let reboc_arg_types = match &sig.varargs {
        Some(varargs) => match &varargs.kind {
            VarargsKind::Typed(typ) => quote::quote_spanned!(varargs.span=> vec![::rebo::Type::TypedVarargs(::rebo::Type::Specific(<#typ as ::rebo::Typed>::typ()))]),
            VarargsKind::Untyped => quote::quote_spanned!(varargs.span=> vec![::rebo::Type::UntypedVarargs]),
        }
        None => quote::quote!(vec![#(#reboc_arg_types),*]),
    };
    let reboc_return_type = match &sig.output {
        ReturnType::Default => quote::quote_spanned!(sig.output.span()=> ::rebo::Type::Specific(<() as ::rebo::Typed>::typ())),
        ReturnType::Type(_, typ) => util::convert_type_to_reboc_type(typ, &sig.generic_idents, &generics_spans),
    };
    quote::quote! {
        mod #generics_mod {
            #(
                pub static #generics_static_once_names: ::std::sync::OnceLock<::rebo::SpanWithId> = ::std::sync::OnceLock::new();
            )*
        }
        impl ::rebo::RequiredReboFunction for #ident {
            const NAME: &'static str = #rebo_function_name;
            const IS_METHOD: bool = #is_method;
            const GENERICS: &'static [&'static str] = &[#(#generic_ident_strings),*];
            const GENERICS_FILE_NAME: &'static str = #generics_file_name;
            const GENERICS_FILE_CONTENT: &'static str = #generics_file_content;
            fn arg_types() -> Vec<::rebo::Type> {
                #reboc_arg_types
            }
            fn ret_type() -> ::rebo::Type {
                #reboc_return_type
            }
        }
    }
}
