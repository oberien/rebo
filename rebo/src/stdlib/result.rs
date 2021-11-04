use diagnostic::Diagnostics;
use typed_arena::Arena;
use crate::parser::{Expr, Parser};
use crate::common::MetaInfo;
use crate::lexer::Lexer;
use std::path::PathBuf;

const CODE: &str = r#"
enum Result<T, E> {
    Ok(T),
    Err(E),
}

impl Result<T, E> {
    fn unwrap(self) -> T {
        match self {
            Result::Ok(t) => t,
            Result::Err(e) => panic(f"tried to unwrap an error: {e}"),
        }
    }
}
"#;

pub fn add_result<'a, 'i>(diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    let (file, _) = diagnostics.add_file("result.rs".to_string(), CODE.to_string());

    let lexer = Lexer::new(diagnostics, file);
    let parser = Parser::new(PathBuf::new(), arena, lexer, diagnostics, meta_info);
    parser.parse_ast().unwrap();
}
