use diagnostic::{Diagnostics, Span};
use typed_arena::Arena;
use crate::parser::{Expr, ExprStructDefinition, ExprGenerics, Separated, Generic};
use crate::common::{MetaInfo, Value, ExternalFunction, ListArc};
use crate::lexer::{TokenStruct, TokenIdent, TokenLessThan, TokenGreaterThan, TokenOpenCurly, TokenCloseCurly};
use crate::vm::VmContext;
use crate::typeck::types::{FunctionType, Type, SpecificType};
use std::borrow::Cow;
use parking_lot::ReentrantMutex;
use std::sync::Arc;
use std::cell::RefCell;

pub fn add_list<'a, 'i>(diagnostics: &Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    let code = "struct List<T> { ... }".to_string();
    let (file, _) = diagnostics.add_file("list.rs".to_string(), code);
    let generic_span = Span::new(file, 12, 13);
    let struct_def = ExprStructDefinition {
        struct_token: TokenStruct { span: Span { file, start: 0, end: 6 } },
        name: TokenIdent { span: Span { file, start: 7, end: 11 }, ident: "List" },
        generics: Some(ExprGenerics {
            open: TokenLessThan { span: Span { file, start: 11, end: 12 } },
            generics: Some({
                let mut sep = Separated::default();
                let generic_ident = TokenIdent { span: generic_span, ident: "T" };
                sep.prepend(Generic { def_ident: generic_ident, ident: generic_ident }, None);
                sep
            }),
            close: TokenGreaterThan { span: Span { file, start: 13, end: 14 } }
        }),
        open: TokenOpenCurly { span: Span { file, start: 15, end: 16 } },
        fields: Separated::default(),
        close: TokenCloseCurly { span: Span { file, start: 21, end: 22 } },
    };
    let struct_def = match arena.alloc(Expr::StructDefinition(struct_def)) {
        Expr::StructDefinition(struct_def) => struct_def,
        _ => unreachable!(),
    };
    meta_info.add_struct(diagnostics, struct_def);

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
