use proc_macro_error::abort;
use syn::{parse_macro_input, Ident, ItemForeignMod, ForeignItem, Signature, GenericParam, FnArg, PatType, Pat, ForeignItemFn, ReturnType, Type};
use proc_macro::TokenStream;

pub fn extern_rebo(_args: TokenStream, input: TokenStream) -> TokenStream {
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
        let Signature {
            constness,
            asyncness,
            unsafety,
            abi: _,
            fn_token: _,
            ident,
            generics,
            paren_token: _,
            inputs,
            variadic,
            output,
        } = sig;
        if constness.is_some() {
            abort!(constness, "required rebo functions can't be const");
        }
        if asyncness.is_some() {
            abort!(asyncness, "required rebo functions can't be async");
        }
        if unsafety.is_some() {
            abort!(unsafety, "required rebo functions can't be unsafe");
        }
        if variadic.is_some() {
            abort!(variadic, "rebo functions can't have varargs");
        }
        if generics.where_clause.is_some() {
            abort!(generics.where_clause, "required rebo functions can't have a `where` clause");
        }

        let rebo_function_name = ident.to_string().replace("__", "::");
        let fn_ident = format!("{}_fn", ident);
        let fn_ident = Ident::new(&fn_ident, ident.span());
        let receiver_type_string = rebo_function_name.split("::").next().unwrap();
        let receiver_type_string = if receiver_type_string == rebo_function_name { None } else { Some(receiver_type_string) };

        let generic_names = generics.params.into_iter().map(|param| match param {
            GenericParam::Type(typ) => {
                if !typ.bounds.is_empty() {
                    abort!(typ.bounds, "generic type bounds not allowed for required rebo functions");
                }
                if typ.default.is_some() {
                    abort!(typ.default, "default generics not allowed for required rebo functions");
                }
                typ.ident.to_string()
            },
            GenericParam::Lifetime(_) => abort!(param, "lifetimes not allowed for required rebo functions"),
            GenericParam::Const(_) => abort!(param, "const generics not allowed for required rebo functions"),
        }).collect::<Vec<_>>();

        let mut is_method = false;
        let (arg_idents, arg_types) = inputs.into_iter().enumerate().map(|(i, arg)| match arg {
            FnArg::Receiver(recv) => abort!(recv, "self-arguments aren't allowed; if you want a self-argument, use `this` as name of the first argument instead"),
            FnArg::Typed(PatType { attrs, pat, colon_token: _, ty }) => {
                if !attrs.is_empty() {
                    abort!(attrs[0], "argument attributes are not supported for static rebo functions");
                }
                match *pat {
                    Pat::Ident(pat) => {
                        if pat.ident.to_string() == "this" {
                            if i != 0 {
                                abort!(pat, "this-argument must be the first argument");
                            }
                            if receiver_type_string.is_none() {
                                abort!(pat, "rebo methods are named `Type::method(...)`, so the extern function name should be `Type__method`");
                            }
                            match &*ty {
                                Type::Path(path) => if path.path.segments.iter().next().unwrap().ident != receiver_type_string.as_ref().unwrap() {
                                    abort!(pat, "rebo method named `Type::method(...)` take `Type` as first param; the first extern function argument should be `this: {}` or `this: {0}<...>`", receiver_type_string.unwrap());
                                }
                                _ => abort!(ty, "this-argument must be {}", receiver_type_string.as_ref().unwrap()),
                            }
                            is_method = true;
                        }
                        (pat.ident, ty)
                    }
                    _ => abort!(pat, "arguments must be identifiers for required rebo functions"),
                }
            }
        }).unzip::<_, _, Vec<_>, Vec<_>>();

        let ret_type = match output {
            ReturnType::Default => quote::quote!(()),
            ReturnType::Type(_arrow, typ) => quote::quote!(#typ),
        };

        res = quote::quote! {
            #res

            fn #fn_ident<'a, 'i>(vm: &mut ::rebo::VmContext<'a, '_, '_, 'i>, #(#arg_idents: #arg_types),*) -> Result<#ret_type, ::rebo::ExecError<'a, 'i>> {
                let values = vec![#(
                    <#arg_types as ::rebo::IntoValue>::into_value(#arg_idents),
                )*];
                let res = vm.call_required_rebo_function::<#ident>(values)?;
                Ok(<#ret_type as ::rebo::FromValue>::from_value(res))
            }
            #[allow(non_camel_case_types)]
            #vis struct #ident;
            impl ::std::ops::Deref for #ident {
                type Target = for<'a, 'i> fn(&mut ::rebo::VmContext<'a, '_, '_, 'i>, #(#arg_types),*) -> Result<#ret_type, ::rebo::ExecError<'a, 'i>>;
                fn deref(&self) -> &Self::Target {
                    &(#fn_ident as Self::Target)
                }
            }
            impl ::rebo::RequiredReboFunction for #ident {
                const NAME: &'static str = #rebo_function_name;
                const IS_METHOD: bool = #is_method;
                const GENERICS: &'static [&'static str] = &[#(#generic_names),*];
                const ARGS: &'static [::rebo::Type] = &[#(::rebo::Type::Specific(<#arg_types as ::rebo::Typed>::TYPE)),*];
                const RET: ::rebo::Type = ::rebo::Type::Specific(<#ret_type as ::rebo::Typed>::TYPE);
            }
        };
    }

    res.into()
}
