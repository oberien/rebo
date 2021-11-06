use proc_macro_error::{proc_macro_error, abort};
use syn::{ItemFn, Signature, FnArg, PatType, Ident, ReturnType, Type, DeriveInput, Token, parse_macro_input, Expr, Result, Attribute, Data, DataUnion, DataStruct, Visibility, Pat, GenericParam, LitStr, ItemEnum, ItemStruct, Fields};
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::token::{Union, Struct, Paren};
use itertools::Itertools;
use proc_macro2::Span;
use unzip3::Unzip3;
use crate::util;

pub fn enum_type(e: ItemEnum) -> TokenStream {
    unimplemented!()
}
pub fn struct_type(s: ItemStruct) -> TokenStream {
    let ItemStruct { attrs: _, vis: _, struct_token: _, ident, generics, fields, semi_token: _ } = s;
    let ident_string = ident.to_string();

    let (field_names, field_name_lit_strs, field_types) = match &fields {
        Fields::Named(named) => named.named.iter()
            .map(|f| (f.ident.as_ref().unwrap(), LitStr::new(&f.ident.as_ref().unwrap().to_string(), Span::call_site()), &f.ty))
            .unzip3::<Vec<_>, Vec<_>, Vec<_>>(),

        Fields::Unnamed(_) => abort!(fields, "only named fields are allowed for rebo structs"),
        Fields::Unit => abort!(fields, "only named fields are allowed for rebo structs"),
    };

    let mut generic_idents = util::generic_idents(&generics, "rebo structs");

    // let code_filename = format!("external-{}.rs", rebo_name);
    // TODO: manually convert syn::Type to string to not have spaces in `Option < T >`
    let generics_string = if generic_idents.is_empty() {
        "".to_string()
    } else {
        format!("<{}>", generic_idents.iter().join(", "))
    };

    (quote::quote! {
        impl ::rebo::ExternalType for #ident {
            const CODE: &'static str;
            const FILE_NAME: &'static str;
        }
        impl ::rebo::FromValue for #ident {
            fn from_value(value: ::rebo::Value) -> Self {
                match value {
                    ::rebo::Value::Struct(s) => {
                        let s = s.lock().unwap();
                        let s = s.borrow();
                        #ident {
                            #(
                                #field_names: <#field_types as ::rebo::FromValue>::from_value(
                                    s.fields.iter().find(|name| name == #field_name_lit_strs).unwrap().clone()
                                )
                            ,)*
                        }
                    }
                    _ => unreachable!("{}::from_value called with non-{}", #ident_string, #ident_string),
                }
            }
        }
        impl ::rebo::IntoValue for #ident {
            fn into_value(self) -> ::rebo::Value {

            }
        }
        impl ::rebo::Typed for #ident {
            const TYPE: ::rebo::SpecificType;
        }
    }).into()
}
