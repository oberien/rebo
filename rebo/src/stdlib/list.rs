use diagnostic::{Diagnostics, Span};
use typed_arena::Arena;
use crate::parser::{Expr, Parser};
use crate::common::{MetaInfo, Value, ExternalFunction, ListArc, EnumArc, Enum};
use crate::lexer::Lexer;
use crate::vm::VmContext;
use crate::typeck::types::{FunctionType, Type, SpecificType};
use std::borrow::Cow;
use parking_lot::ReentrantMutex;
use std::sync::Arc;
use std::cell::RefCell;

pub fn add_list<'a, 'i>(diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>, option_generic_span: Span) {
    let code = "struct List<T> {}".to_string();
    let (file, _) = diagnostics.add_file("list.rs".to_string(), code);

    let lexer = Lexer::new(diagnostics, file);
    let parser = Parser::new(arena, lexer, diagnostics, meta_info);
    let ast = parser.parse_ast().unwrap();
    assert_eq!(ast.exprs.len(), 1);

    let struct_def = match ast.exprs[0] {
        Expr::StructDefinition(struct_def) => struct_def,
        _ => unreachable!(),
    };
    let generic_span = struct_def.generics.as_ref().unwrap().generics.as_ref().unwrap().iter().next().unwrap().def_ident.span;

    meta_info.add_external_function(diagnostics, "List::new", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Owned(vec![generic_span]),
            args: Cow::Borrowed(&[]),
            ret: Type::Specific(SpecificType::Struct("List".to_string(), vec![(generic_span, Type::Top)]))
        },
        imp: list_new,
    });
    meta_info.add_external_function(diagnostics, "List::push", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Owned(vec![generic_span]),
            args: Cow::Owned(vec![
                Type::Specific(SpecificType::Struct("List".to_string(), vec![(generic_span, Type::Top)])),
                Type::Specific(SpecificType::Generic(generic_span)),
            ]),
            ret: Type::Specific(SpecificType::Unit),
        },
        imp: list_push,
    });
    meta_info.add_external_function(diagnostics, "List::get", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Owned(vec![generic_span]),
            args: Cow::Owned(vec![
                Type::Specific(SpecificType::Struct("List".to_string(), vec![(generic_span, Type::Top)])),
                Type::Specific(SpecificType::Integer),
            ]),
            ret: Type::Specific(SpecificType::Enum("Option".to_string(), vec![(option_generic_span, Type::Specific(SpecificType::Generic(generic_span)))])),
        },
        imp: list_get,
    });
}

fn list_new(_expr_span: Span, _vm: &mut VmContext, _values: Vec<Value>) -> Value {
    Value::List(ListArc { list: Arc::new(ReentrantMutex::new(RefCell::new(Vec::new()))) })
}

fn list_push(_expr_span: Span, _vm: &mut VmContext, mut values: Vec<Value>) -> Value {
    let list = values.remove(0).expect_list("List::push called with non-list self argument");
    let value = values.remove(0);
    let list = list.list.lock();
    list.borrow_mut().push(value);
    Value::Unit
}

fn list_get(_expr_span: Span, _vm: &mut VmContext, mut values: Vec<Value>) -> Value {
    let list = values.remove(0).expect_list("List::get called with non-list self argument");
    let index = values.remove(0).expect_int("List::get called with non-int index");
    let list = list.list.lock();
    let (variant, fields) = match list.borrow().get(index as usize) {
        Some(v) => ("Some".to_string(), vec![v.clone()]),
        None => ("None".to_string(), vec![]),
    };
    Value::Enum(EnumArc { e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
        name: "Option".to_string(),
        variant,
        fields,
    })))})
}

