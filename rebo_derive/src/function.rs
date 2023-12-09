use proc_macro_error::abort;
use syn::{ItemFn, Ident, ReturnType, Type, parse_macro_input, Expr, Result, LitStr};
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::token::Paren;
use itertools::Itertools;
use syn::spanned::Spanned;
use crate::util::{self, Varargs, VarargsKind};

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
            let lit = content.parse::<LitStr>()?;
            Ok(Args { is_raw: true, name: lit.value() })
        } else {
            let lit = input.parse::<LitStr>()?;
            Ok(Args { is_raw: false, name: lit.value() })
        }
    }
}

pub fn function(args: TokenStream, input: TokenStream) -> TokenStream {
    let macro_args = parse_macro_input!(args as Args);
    let input = parse_macro_input!(input as ItemFn);
    let ItemFn { attrs: _, vis, sig, block } = input;

    let util::FunctionSignature {
        ident, generic_idents, is_method: _, arg_idents, arg_types, varargs, output
    } = util::parse_function_signature(sig, "rebo functions");

    let fn_ident = format!("{}_fn", ident);
    let fn_ident = Ident::new(&fn_ident, ident.span());

    let rebo_name = macro_args.name;
    let code_filename = format!("external-{}.rs", rebo_name);
    // TODO: manually convert syn::Type to string to not have spaces in `Option < T >`
    let code_string = generate_rebo_function_stub(&ident, &generic_idents, &arg_idents, &arg_types, &varargs, &output);

    let arg_transforms: Vec<_> = arg_idents.iter().zip(arg_types.iter())
        .map(|(ident, typ)| {
            let clean = util::replace_generics_with_value(&typ, &generic_idents);
            quote::quote!(let #ident: #clean = ::rebo::FromValue::from_value(args.next().unwrap());)
        }).collect();

    let (output_type, ret_val) = match &output {
        ReturnType::Default => (
            quote::quote_spanned!{ output.span()=> () },
            quote::quote!(Ok(::rebo::IntoValue::into_value(res))),
        ),
        ReturnType::Type(_, typ) => match &**typ {
            Type::Never(_) => (
                quote::quote_spanned!(output.span()=> _),
                quote::quote!(res),
            ),
            _ => {
                let clean = util::replace_generics_with_value(&typ, &generic_idents);
                (quote::quote_spanned!(output.span()=> #clean), quote::quote!(Ok(::rebo::IntoValue::into_value(res))))
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
        quote::quote_spanned! { block.span()=> #block }
    } else {
        quote::quote_spanned! { block.span()=>  (|| #block)() }
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

fn generate_rebo_function_stub(ident: &Ident, generic_idents: &[Ident], arg_idents: &[Ident], arg_types: &[Type], varargs: &Option<Varargs>, output: &ReturnType) -> String {
    let generics_string = if generic_idents.is_empty() {
        "".to_string()
    } else {
        format!("<{}>", generic_idents.iter().join(", "))
    };
    let args_string = arg_idents.iter().zip(arg_types.iter())
        .map(|(ident, typ)| format!("{}: {}", ident, util::convert_type_to_rebo(typ)))
        .chain(match &varargs {
            Some(Varargs { kind: VarargsKind::Typed(typ), .. }) => Some(format!("{}...", util::convert_type_to_rebo(typ))),
            Some(Varargs { kind: VarargsKind::Untyped, .. }) => Some("...".to_string()),
            None => None,
        }).join(", ");
    let output_string = match &output {
        ReturnType::Default => "".to_string(),
        ReturnType::Type(_, typ) => format!("-> {}", util::convert_type_to_rebo(typ)),
    };
    format!("fn {}{}({}) {} {{\n    ...\n}}", ident, generics_string, args_string, output_string)
}
