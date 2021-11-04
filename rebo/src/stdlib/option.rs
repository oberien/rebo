use diagnostic::{Diagnostics, Span};
use typed_arena::Arena;
use crate::parser::{Expr, Parser};
use crate::common::MetaInfo;
use crate::lexer::Lexer;
use std::path::PathBuf;

const CODE: &str = r#"
enum Option<T> {
    Some(T),
    None,
}

impl Option<T> {
    fn unwrap(self) -> T {
        match self {
            Option::Some(t) => t,
            Option::None => panic("tried to unwrap a None value"),
        }
    }
}
"#;

pub fn add_option<'a, 'i>(diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) -> Span {
    let (file, _) = diagnostics.add_file("option.rs".to_string(), CODE.to_string());

    let lexer = Lexer::new(diagnostics, file);
    let parser = Parser::new(PathBuf::new(), arena, lexer, diagnostics, meta_info);
    let ast = parser.parse_ast().unwrap();
    match ast.exprs[0] {
        Expr::EnumDefinition(enum_def) => {
            enum_def.generics.as_ref().unwrap().generics.as_ref().unwrap().iter().next().unwrap().def_ident.span
        }
        _ => unreachable!(),
    }
}
