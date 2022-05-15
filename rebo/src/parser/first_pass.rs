use std::cell::RefCell;
use std::rc::Rc;
use indexmap::IndexMap;
use rebo::parser::ExprInclude;
use rebo::{ErrorCode, Lexer, util};
use rebo::util::ResolveFileError;
use crate::common::Depth;
use crate::lexer::{Token, TokenIdent, TokenImpl, TokenOpenCurly};
use crate::parser::{Expr, ExprEnumDefinition, ExprFunctionSignature, ExprGenerics, ExprPattern, ExprStaticSignature, ExprStructDefinition, InternalError, Parse, Parser, Spanned};
use crate::parser::scope::{Scope, ScopeType};

enum StackElement {
    Block,
    ImplBlock(String),
}

impl<'a, 'b, 'i> Parser<'a, 'b, 'i> {
    /// Parse struct and enum definitions
    pub(super) fn first_pass(&mut self, depth: Depth) {
        debug!("first_pass");
        let functions: &[for<'x> fn(&'x mut Parser<'a, 'b, 'i>, &Vec<StackElement>, _) -> Result<_, InternalError>] = &[
            // struct definitions
            |parser: &mut Parser<'a, '_, 'i>, _, depth| {
                let struct_def = &*parser.arena.alloc(Expr::StructDefinition(ExprStructDefinition::parse_reset(parser, depth)?));
                match struct_def {
                    Expr::StructDefinition(struct_def) => {
                        trace!("found struct {}", struct_def.name.ident);
                        parser.meta_info.add_struct(parser.diagnostics, struct_def);
                    },
                    _ => unreachable!("we just created you"),
                }
                Ok(Some(struct_def))
            },
            // enum definitions
            |parser: &mut Parser<'a, '_, 'i>, _, depth| {
                let enum_def = &*parser.arena.alloc(Expr::EnumDefinition(ExprEnumDefinition::parse_reset(parser, depth)?));
                match enum_def {
                    Expr::EnumDefinition(enum_def) => {
                        trace!("found enum {}", enum_def.name.ident);
                        parser.meta_info.add_enum(parser.diagnostics, enum_def);
                    },
                    _ => unreachable!("we just created you"),
                }
                Ok(Some(enum_def))
            },
            // function signatures
            |parser: &mut Parser<'a, '_, 'i>, stack: &Vec<StackElement>, depth| {
                let old_scopes = std::mem::take(&mut parser.scopes);
                // let scope_guard = parser.push_scope(ScopeType::Global);
                let scope_guard = parser.push_scope(ScopeType::Synthetic);
                let result = <(ExprFunctionSignature, TokenOpenCurly)>::parse_reset(parser, depth);
                drop(scope_guard);
                parser.scopes = old_scopes;
                let (function_sig, _open) = result?;
                let name = match function_sig.name {
                    Some(fn_name) => match stack.last() {
                        Some(StackElement::ImplBlock(name)) => Some(format!("{}::{}", name, fn_name.ident)),
                        _ => Some(fn_name.ident.to_string()),
                    }
                    None => None,
                };
                if let Some(name) = name {
                    trace!("found function {}", name);
                    parser.meta_info.rebo_function_names.insert((name, function_sig.name.unwrap()));
                }
                Ok(None)
            },
            // includes -> first-pass
            |parser: &mut Parser<'a, '_, 'i>, _, depth| {
                let include = ExprInclude::parse_reset(parser, depth.duplicate())?;
                let path = match util::try_resolve_file(&parser.include_directory, &include.file.string) {
                    Ok(path) => path,
                    Err(ResolveFileError::Canonicalize(path, e)) => {
                        parser.diagnostics.error(ErrorCode::ErrorReadingIncludedFile)
                            .with_error_label(include.span(), format!("error canonicalizing `{}`", path.display()))
                            .with_error_label(include.span(), e.to_string())
                            .emit();
                        return Ok(None);
                    }
                    Err(ResolveFileError::StartsWith(path)) => {
                        parser.diagnostics.error(ErrorCode::ErrorReadingIncludedFile)
                            .with_error_label(include.span(), "the file is not in the include directory")
                            .with_info_label(include.span(), format!("this file resolved to {}", path.display()))
                            .with_error_label(include.span(), format!("included files must be in {}", parser.include_directory.unwrap_path().display()))
                            .emit();
                        return Ok(None);
                    }
                };
                let code = match ::std::fs::read_to_string(&path) {
                    Ok(code) => code,
                    Err(e) => {
                        parser.diagnostics.error(ErrorCode::ErrorReadingIncludedFile)
                            .with_error_label(include.span(), format!("error reading file `{}`", path.display()))
                            .with_error_label(include.span(), e.to_string())
                            .emit();
                        return Ok(None);
                    }
                };
                let (file, _) = parser.diagnostics.add_file(include.file.string.clone(), code);
                parser.meta_info.included_files.insert(include.span(), file);
                let lexer = Lexer::new(parser.diagnostics, file);
                let old_lexer = ::std::mem::replace(&mut parser.lexer, lexer);
                parser.first_pass(depth.next());
                parser.lexer = old_lexer;
                Ok(None)
            },
        ];
        self.do_pass(functions, depth);
        debug!("first_pass done");
    }
    pub(super) fn second_pass(&mut self, depth: Depth) {
        debug!("second_pass");
        let functions: &[for<'x> fn(&'x mut Parser<'a, 'b, 'i>, &Vec<StackElement>, _) -> Result<_, InternalError>] = &[
            // static signatures
            |parser: &mut Parser<'a, '_, 'i>, _, depth| {
                let static_sig = ExprStaticSignature::parse_reset(parser, depth)?;
                let binding = match static_sig.pattern {
                    ExprPattern::Typed(typed) => typed.pattern.binding,
                    ExprPattern::Untyped(untyped) => untyped.binding,
                };
                trace!("found static {}", binding.ident.ident);
                parser.meta_info.static_bindings.insert(binding);
                Ok(None)
            },
            // includes -> second-pass
            |parser: &mut Parser<'a, '_, 'i>, _, depth| {
                let include = ExprInclude::parse_reset(parser, depth.duplicate())?;
                let file = match parser.meta_info.included_files.get(&include.span()) {
                    Some(&file) => file,
                    None => return Ok(None),
                };
                let lexer = Lexer::new(parser.diagnostics, file);
                let old_lexer = ::std::mem::replace(&mut parser.lexer, lexer);
                parser.second_pass(depth.next());
                parser.lexer = old_lexer;
                Ok(None)
            },
        ];
        self.do_pass(functions, depth);
        debug!("second pass done");
    }

    fn do_pass(&mut self, functions: &[for<'x> fn(&'x mut Parser<'a, 'b, 'i>, &Vec<StackElement>, Depth) -> Result<Option<&'a Expr<'a, 'i>>, InternalError>], depth: Depth) {
        let mut stack: Vec<StackElement> = Vec::new();
        // create rogue scopes
        let old_scopes = ::std::mem::replace(&mut self.scopes, Rc::new(RefCell::new(vec![Scope { idents: IndexMap::new(), generics: IndexMap::new(), typ: ScopeType::Global }])));
        let mark = self.lexer.mark();

        self.add_statics();
        let _guard = self.push_scope(ScopeType::Synthetic);

        while self.peek_token(0).is_ok() && !matches!(self.peek_token(0).unwrap(), Token::Eof(_)) {
            let scope = self.push_scope(ScopeType::Synthetic);
            if let Ok(impl_block_sig) = ImplBlockSignature::parse(self, depth.duplicate()) {
                trace!("entering impl-block {}", impl_block_sig.name.ident);
                stack.push(StackElement::ImplBlock(impl_block_sig.name.ident.to_string()));
                std::mem::forget(scope);
                continue;
            }
            if let Ok(Token::OpenCurly(_)) = self.peek_token(0) {
                drop(self.next_token().unwrap());
                trace!("entering block");
                stack.push(StackElement::Block);
                std::mem::forget(scope);
                continue;
            }
            drop(scope);
            if let Ok(Token::CloseCurly(_)) = self.peek_token(0) {
                drop(self.next_token().unwrap());
                trace!("leaving impl-block or block");
                stack.pop();
                self.scopes.borrow_mut().pop();
                continue;
            }

            for function in functions {
                let expr = match function(&mut *self, &stack, depth.duplicate()) {
                    Ok(expr) => expr,
                    Err(_) => continue,
                };
                if let Some(expr) = expr {
                    self.pre_parsed.insert((expr.span().file, expr.span().start), expr);
                    // consume tokens except last one as that's consumed after the for loop
                    while self.peek_token(0).unwrap().span().end < expr.span().end {
                        drop(self.next_token());
                    }
                }
                break;
            }
            drop(self.next_token())
        }

        // reset token lookahead
        drop(mark);
        self.scopes = old_scopes;
    }
}

#[derive(Clone, Debug)]
pub struct ImplBlockSignature<'a, 'i> {
    pub impl_token: TokenImpl,
    pub name: TokenIdent<'i>,
    pub generics: Option<ExprGenerics<'a, 'i>>,
    pub open: TokenOpenCurly,
}
impl<'a, 'i> Parse<'a, 'i> for ImplBlockSignature<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ImplBlockSignature {
            impl_token: parser.parse(depth.next())?,
            name: parser.parse(depth.next())?,
            generics: parser.parse(depth.next())?,
            open: parser.parse(depth.last())?,
        })
    }
}
