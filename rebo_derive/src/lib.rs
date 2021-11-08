use proc_macro_error::{proc_macro_error, abort};
use syn::{Signature, Ident, DeriveInput, Token, parse_macro_input, Expr, Result, Attribute, Data, DataUnion, DataStruct, Visibility, Item};
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::token::{Union, Struct};

mod function;
mod rebo_type;
mod extern_rebo;
mod util;

#[proc_macro_error]
#[proc_macro_attribute]
pub fn function(args: TokenStream, input: TokenStream) -> TokenStream {
    function::function(args, input)
}
#[proc_macro_error]
#[proc_macro_attribute]
pub fn required_rebo_functions(args: TokenStream, input: TokenStream) -> TokenStream {
    extern_rebo::extern_rebo(args, input)
}

#[proc_macro_error]
#[proc_macro_derive(ExternalType)]
pub fn rebo_type(input: TokenStream) -> TokenStream {
    match parse_macro_input!(input as Item) {
        Item::Enum(e) => rebo_type::enum_type(e),
        Item::Struct(s) => rebo_type::struct_type(s),
        item => abort!(item, "ExternalType can only be derived on enums and structs"),
    }
}

/// For internal use only
#[doc(hidden)]
#[proc_macro_error]
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
