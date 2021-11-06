use std::ops::Deref;
use crate::lints::visitor::Visitor;
use crate::parser::{Spanned, ExprAccess, FieldOrMethod};
use crate::common::{MetaInfo, Function};
use diagnostic::Diagnostics;
use crate::error_codes::ErrorCode;
use crate::typeck::types::{Type, SpecificType};
use crate::typeck::TypeVar;

pub struct UnknownAccess;

impl Visitor for UnknownAccess {
    fn visit_access(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, access: &ExprAccess) {
        let ExprAccess { variable, accesses, .. } = access;

        let mut type_var = TypeVar::new(variable.binding.ident.span);

        for access in accesses {
            let typ = match &meta_info.types[&type_var] {
                Type::Specific(specific) => specific,
                _ => return,
            };
            type_var = match access {
                FieldOrMethod::Field(field) => {
                    let struct_name = match typ {
                        SpecificType::Struct(name, _) => name,
                        typ => {
                            diagnostics.error(ErrorCode::NonStructFieldAccess)
                                .with_error_label(type_var.span, format!("`{}` is of type `{}`, which is not a struct", diagnostics.resolve_span(type_var.span), typ))
                                .emit();
                            return
                        }
                    };
                    match meta_info.struct_types[struct_name.deref()].get_field(field.ident) {
                        Some(_) => TypeVar::new(field.span),
                        None => {
                            diagnostics.error(ErrorCode::UnknownFieldAccess)
                                .with_error_label(type_var.span, format!("tried to access non-existent field `{}` of `struct {}`", field.ident, struct_name))
                                .emit();
                            return
                        }
                    }
                }
                FieldOrMethod::Method(fn_call) => {
                    let fn_name = format!("{}::{}", typ.type_name(), fn_call.name.ident);

                    match meta_info.functions.get(fn_name.as_str()) {
                        None => {
                            let similar = crate::util::similar_name(&fn_name, meta_info.functions.keys());
                            let mut diag = diagnostics.error(ErrorCode::UnknownMethod)
                                .with_error_label(fn_call.name.span, format!("can't find method `{}`", fn_name));
                            if let Some(similar) = similar {
                                diag = diag.with_info_label(fn_call.name.span, format!("did you mean `{}`", similar));
                            }
                            diag.emit();
                            return
                        }
                        Some(Function::Rust(_)) => {
                            let fn_typ = &meta_info.function_types[fn_name.as_str()];
                            if !fn_typ.is_method {
                                diagnostics.error(ErrorCode::NotAMethod)
                                    .with_error_label(fn_call.name.span, format!("`{}` is an external function and not a method", fn_name))
                                    .emit();
                            }
                            TypeVar::new(fn_call.span())
                        },
                        Some(Function::EnumInitializer(_, _)) => unreachable!("can't call an EnumInitializer as self-method"),
                        Some(Function::Rebo(_, _)) => {
                            let fun = &meta_info.rebo_functions[fn_name.as_str()];
                            if fun.self_arg.is_none() {
                                diagnostics.error(ErrorCode::NotAMethod)
                                    .with_error_label(fn_call.name.span, format!("`{}` is a function and not a method", fn_name))
                                    .with_info_label(fn_call.name.span, "methods must have `self` as first argument")
                                    .with_info_label(fun.arg_span(), "this function doesn't have `self` as first argument")
                                    .emit();
                            }
                            TypeVar::new(fn_call.span())
                        }
                    }
                }
            }
        }
    }
}