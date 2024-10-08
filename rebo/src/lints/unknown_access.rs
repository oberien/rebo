use crate::lints::visitor::Visitor;
use crate::parser::{ExprAccess, FieldOrMethod};
use crate::common::{BlockStack, Function, MetaInfo};
use diagnostic::Diagnostics;
use rebo::lints::invalid_number_of_arguments::check_function_call_arg_num;
use crate::common::Spanned;
use crate::error_codes::ErrorCode;
use crate::lints::invalid_number_of_arguments::CallType;
use crate::typeck::types::{SpecificType, Type};
use crate::typeck::TypeVar;

pub struct UnknownAccess;

impl Visitor for UnknownAccess {
    fn visit_access(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, ()>, access: &ExprAccess) {
        let ExprAccess { variable, accesses, .. } = access;

        let mut type_var = TypeVar::from_spanned(variable);

        for access in accesses {
            let typ = match &meta_info.types[&type_var] {
                Type::Specific(specific) => specific,
                typ => {
                    diagnostics.error(ErrorCode::AccessOfUnknown)
                        .with_error_label(access.diagnostics_span(), "can't infer a specific type for what is accessed here")
                        .with_info_label(access.diagnostics_span(), format!("inferred {}", typ))
                        .emit();
                    return
                },
            };
            type_var = match access {
                FieldOrMethod::Field(field) => {
                    let struct_name = match typ {
                        SpecificType::Struct(name, _) if name != "struct" => name,
                        SpecificType::Struct(_, _) => {
                            diagnostics.error(ErrorCode::NonStructFieldAccess)
                                .with_error_label(type_var.diagnostics_span(), "can't infer type of this struct")
                                .with_info_label(type_var.diagnostics_span(), "type annotation needed")
                                .emit();
                            return
                        }
                        typ => {
                            diagnostics.error(ErrorCode::NonStructFieldAccess)
                                .with_error_label(type_var.diagnostics_span(), format!("`{}` is of type `{}`, which is not a struct", diagnostics.resolve_span(type_var.diagnostics_span()), typ))
                                .emit();
                            return
                        }
                    };
                    match meta_info.struct_types[struct_name.as_str()].get_field(field.ident) {
                        Some(_) => TypeVar::from_spanned(field),
                        None => {
                            diagnostics.error(ErrorCode::UnknownFieldAccess)
                                .with_error_label(field.diagnostics_span(), format!("tried to access non-existent field `{}` of `struct {}`", field.ident, struct_name))
                                .emit();
                            return
                        }
                    }
                }
                FieldOrMethod::Method(fn_call) => {
                    let fn_name = format!("{}::{}", typ.type_name(), fn_call.name.ident);

                    if let Some(fun) = meta_info.function_types.get(fn_name.as_str()) {
                        check_function_call_arg_num(diagnostics, fun, CallType::MethodCall, fn_call.name.span_with_id(), &fn_call.open, &fn_call.args, &fn_call.close)
                    }

                    match meta_info.functions.get(fn_name.as_str()) {
                        None => {
                            let similar = crate::util::similar_name(&fn_name, meta_info.functions.keys());
                            let mut diag = diagnostics.error(ErrorCode::UnknownMethod)
                                .with_error_label(fn_call.name.diagnostics_span(), format!("can't find method `{}`", fn_name));
                            if let Some(similar) = similar {
                                diag = diag.with_info_label(fn_call.name.diagnostics_span(), format!("did you mean `{}`", similar));
                            }
                            diag.emit();
                            return
                        }
                        Some(Function::Rust(_)) => {
                            let fn_typ = &meta_info.function_types[fn_name.as_str()];
                            if !fn_typ.is_method {
                                diagnostics.error(ErrorCode::NotAMethod)
                                    .with_error_label(fn_call.name.diagnostics_span(), format!("`{}` is an external function and not a method", fn_name))
                                    .emit();
                            }
                            TypeVar::from_spanned(fn_call)
                        },
                        Some(Function::EnumInitializer(_, _)) => unreachable!("can't call an EnumInitializer as self-method"),
                        Some(Function::Rebo(_, _)) => {
                            let fun = &meta_info.rebo_functions[fn_name.as_str()];
                            if fun.sig.self_arg.is_none() {
                                diagnostics.error(ErrorCode::NotAMethod)
                                    .with_error_label(fn_call.name.diagnostics_span(), format!("`{}` is a function and not a method", fn_name))
                                    .with_info_label(fn_call.name.diagnostics_span(), "methods must have `self` as first argument")
                                    .with_info_label(fun.arg_diagnostics_span(), "this function doesn't have `self` as first argument")
                                    .emit();
                            }
                            TypeVar::from_spanned(fn_call)
                        }
                    }
                }
            }
        }
    }
}