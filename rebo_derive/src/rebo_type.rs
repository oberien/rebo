use proc_macro_error::abort;
use syn::{Ident, ItemEnum, ItemStruct, Fields};
use proc_macro::TokenStream;
use itertools::Itertools;
use proc_macro2::Span;
use crate::util;

pub fn enum_type(e: ItemEnum) -> TokenStream {
    let ItemEnum { attrs: _, vis, enum_token: _, ident, generics, brace_token: _, variants } = e;
    let ident_string = ident.to_string();
    let value_ident = format!("_{}UnitStruct", ident);
    let value_ident = Ident::new(&value_ident, ident.span());

    let (variant_names, variant_indexes, variant_name_strings, field_types) = variants.iter()
        .enumerate()
        .map(|(variant_index, variant)| {
            match &variant.fields {
                Fields::Unnamed(unnamed) => (
                    &variant.ident,
                    variant_index,
                    variant.ident.to_string(),
                    unnamed.unnamed.iter().map(|f| &f.ty).collect::<Vec<_>>(),
                ),
                Fields::Named(_) => abort!(variant.fields, "named fields are not supported for rebo structs"),
                Fields::Unit => (&variant.ident, variant_index, variant.ident.to_string(), vec![]),
            }
        }).multiunzip::<(Vec<_>, Vec<_>, Vec<_>, Vec<_>)>();
    let idents = vec![&ident; variant_names.len()];
    let ident_strings = vec![&ident_string; variant_names.len()];
    let field_names = field_types.iter()
        .map(|types| {
            types.iter().enumerate()
                .map(|(i, _)| Ident::new(&format!("f{}", i), Span::call_site()))
                .collect::<Vec<_>>()
        }).collect::<Vec<_>>();
    let field_names_concat: Vec<_> = field_names.iter().map(|field_names| {
        if field_names.is_empty() {
            quote::quote!()
        } else {
            quote::quote!((#(#field_names,)*))
        }
    }).collect();

    let (generic_idents, _) = util::parse_generics(&generics, "rebo enums");

    let code_filename = format!("external-{}.rs", ident);
    // TODO: manually convert syn::Type to string to not have spaces in `Option < T >`
    let generics_string = if generic_idents.is_empty() {
        "".to_string()
    } else {
        format!("<{}>", generic_idents.iter().join(", "))
    };
    let variants_string = variant_names.iter().zip(field_types.iter())
        .map(|(name, types)| format!("    {}{},\n", name, if types.is_empty() {
            "".to_string()
        } else {
            format!("({})", types.iter().map(|typ| util::convert_type_to_rebo(typ).to_string()).join(", "))
        })).join("");
    let code = format!("enum {}{} {{\n{}}}", ident, generics_string, variants_string);

    let generic_spans = util::generic_spans(&generic_idents, &code_filename, &code);
    let from_values = field_types.iter()
        .map(|types| if types.is_empty() {
            quote::quote!()
        } else {
            quote::quote! {
                (#(
                    <#types as ::rebo::FromValue>::from_value(fields.next().unwrap()),
                )*)
            }
        }).collect::<Vec<_>>();

    (quote::quote! {
        #vis struct #value_ident;
        #[allow(non_upper_case_globals)]
        #vis const #ident: #value_ident = #value_ident;
        impl ::rebo::ExternalTypeType for #value_ident {
            type Type = #ident;
        }
        impl<#(#generic_idents: ::rebo::FromValue + ::rebo::IntoValue),*> ::rebo::ExternalType for #ident<#(#generic_idents),*> {
            const CODE: &'static str = #code;
            const FILE_NAME: &'static str = #code_filename;
        }
        impl<#(#generic_idents: ::rebo::FromValue),*> ::rebo::FromValue for #ident<#(#generic_idents),*> {
            fn from_value(value: ::rebo::Value) -> Self {
                match value {
                    ::rebo::Value::Enum(e) => {
                        let e = e.e.lock();
                        let e = e.borrow();
                        let mut fields = e.fields.iter().cloned();
                        match e.variant.as_str() {
                            #(
                                #variant_name_strings => #idents::#variant_names#from_values,
                            )*
                            var => unreachable!("{}::from_value called with unknown variant `{}`", #ident_string, var),
                        }
                    }
                    _ => unreachable!("{}::from_value called with non-{}", #ident_string, #ident_string),
                }
            }
        }
        impl<#(#generic_idents: ::rebo::IntoValue),*> ::rebo::IntoValue for #ident<#(#generic_idents),*> {
            fn into_value(self) -> ::rebo::Value {
                match self {
                    #(
                        #idents::#variant_names#field_names_concat => ::rebo::Value::Enum(::rebo::EnumArc::new(::rebo::Enum {
                            name: #ident_strings.to_string(),
                            variant_index: #variant_indexes,
                            variant: #variant_name_strings.to_string(),
                            fields: vec![
                                #(
                                    <#field_types as ::rebo::IntoValue>::into_value(#field_names),
                                )*
                            ],
                        })),
                    )*
                }
            }
        }
        impl<#(#generic_idents),*> ::rebo::Typed for #ident<#(#generic_idents),*> {
            const TYPE: ::rebo::SpecificType = ::rebo::SpecificType::Enum(
                ::std::borrow::Cow::Borrowed(#ident_string),
                ::rebo::CowVec::Borrowed(&[
                    #(
                        (#generic_spans, ::rebo::Type::Top),
                    )*
                ]),
            );
        }
    }).into()
}

pub fn struct_type(s: ItemStruct) -> TokenStream {
    let ItemStruct { attrs: _, vis, struct_token: _, ident, generics, fields, semi_token: _ } = s;
    let ident_string = ident.to_string();
    let value_ident = format!("_{}UnitStruct", ident);
    let value_ident = Ident::new(&value_ident, ident.span());

    let (field_names, field_name_strings, field_types) = match &fields {
        Fields::Named(named) => named.named.iter()
            .map(|f| (f.ident.as_ref().unwrap(), f.ident.as_ref().unwrap().to_string(), &f.ty))
            .multiunzip::<(Vec<_>, Vec<_>, Vec<_>)>(),

        Fields::Unnamed(_) => abort!(fields, "only named fields are allowed for rebo structs"),
        Fields::Unit => abort!(fields, "only named fields are allowed for rebo structs"),
    };

    let (generic_idents, _) = util::parse_generics(&generics, "rebo structs");
    let generic_values = vec![quote::quote!(::rebo::Value); generic_idents.len()];

    let code_filename = format!("external-{}.rs", ident);
    // TODO: manually convert syn::Type to string to not have spaces in `Option < T >`
    let generics_string = if generic_idents.is_empty() {
        "".to_string()
    } else {
        format!("<{}>", generic_idents.iter().join(", "))
    };
    let fields_string = field_names.iter().zip(field_types.iter())
        .map(|(name, typ)| format!("    {}: {},\n", name, util::convert_type_to_rebo(typ)))
        .join("");
    let code = format!("struct {}{} {{\n{}}}", ident, generics_string, fields_string);

    let generic_spans = util::generic_spans(&generic_idents, &code_filename, &code);

    (quote::quote! {
        #vis struct #value_ident<#(#generic_idents),*>(::std::marker::PhantomData<(#(#generic_idents),*)>);
        #[allow(non_upper_case_globals)]
        #vis const #ident: #value_ident<#(#generic_values),*> = #value_ident(::std::marker::PhantomData);
        impl<#(#generic_idents: ::rebo::FromValue + ::rebo::IntoValue),*> ::rebo::ExternalTypeType for #value_ident<#(#generic_idents),*> {
            type Type = #ident<#(#generic_idents),*>;
        }
        impl<#(#generic_idents: ::rebo::FromValue + ::rebo::IntoValue),*> ::rebo::ExternalType for #ident<#(#generic_idents),*> {
            const CODE: &'static str = #code;
            const FILE_NAME: &'static str = #code_filename;
        }
        impl<#(#generic_idents: ::rebo::FromValue),*> ::rebo::FromValue for #ident<#(#generic_idents),*> {
            fn from_value(value: ::rebo::Value) -> Self {
                match value {
                    ::rebo::Value::Struct(s) => {
                        let s = s.s.lock();
                        let s = s.borrow();
                        let mut fields = s.fields.iter().map(|(_name, value)| value).cloned();
                        #ident {
                            #(
                                #field_names: <#field_types as ::rebo::FromValue>::from_value(fields.next().unwrap()),
                            )*
                        }
                    }
                    _ => unreachable!("{}::from_value called with non-{}", #ident_string, #ident_string),
                }
            }
        }
        impl<#(#generic_idents: ::rebo::IntoValue),*> ::rebo::IntoValue for #ident<#(#generic_idents),*> {
            fn into_value(self) -> ::rebo::Value {
                ::rebo::Value::Struct(::rebo::StructArc::new(::rebo::Struct {
                    name: #ident_string.to_string(),
                    fields: vec![
                        #(
                            (#field_name_strings.to_string(), <#field_types as ::rebo::IntoValue>::into_value(self.#field_names)),
                        )*
                    ],
                }))
            }
        }
        impl<#(#generic_idents),*> ::rebo::Typed for #ident<#(#generic_idents),*> {
            const TYPE: ::rebo::SpecificType = ::rebo::SpecificType::Struct(
                ::std::borrow::Cow::Borrowed(#ident_string),
                ::rebo::CowVec::Borrowed(&[
                    #(
                        (#generic_spans, ::rebo::Type::Top),
                    )*
                ]),
            );
        }
    }).into()
}
