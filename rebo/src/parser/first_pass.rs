use std::cell::RefCell;
use std::rc::Rc;
use indexmap::IndexMap;
use rebo::parser::ExprInclude;
use rebo::{ErrorCode, Lexer, util};
use rebo::util::ResolveFileError;
use crate::common::Depth;
use crate::common::Spanned;
use crate::lexer::{Token, TokenIdent, TokenImpl, TokenOpenCurly};
use crate::parser::{Expr, ExprEnumDefinition, ExprFunctionSignature, ExprGenerics, ExprPattern, ExprStaticSignature, ExprStructDefinition, InternalError, Parse, Parser};
use crate::parser::scope::{Scope, ScopeType};

enum StackElement {
    Block,
    ImplBlock(String),
}

type DoPassFunction<'i, 'b> = for<'x> fn(&'x mut Parser<'i, 'b>, &Vec<StackElement>, Depth) -> Result<Option<&'i Expr<'i>>, InternalError>;
impl<'i, 'b> Parser<'i, 'b> {
    /// Parse struct and enum definitions
    pub(super) fn first_pass(&mut self, depth: Depth) {
        debug!("first_pass");
        let functions: &[DoPassFunction<'i, 'b>] = &[
            // struct definitions
            |parser: &mut Parser<'i, '_>, _, depth| {
                let expr = &*parser.arena.alloc(Expr::StructDefinition(ExprStructDefinition::parse_reset(parser, depth)?));
                let struct_def = match expr {
                    Expr::StructDefinition(struct_def) => struct_def,
                    _ => unreachable!("we just created you"),
                };
                trace!("found struct {}", struct_def.name.ident);
                parser.add_struct_to_meta_info(struct_def);
                Ok(Some(expr))
            },
            // enum definitions
            |parser: &mut Parser<'i, '_>, _, depth| {
                let expr = &*parser.arena.alloc(Expr::EnumDefinition(ExprEnumDefinition::parse_reset(parser, depth)?));
                let enum_def = match expr {
                    Expr::EnumDefinition(enum_def) => enum_def,
                    _ => unreachable!("we just created you"),
                };
                trace!("found enum {}", enum_def.name.ident);
                parser.add_enum_to_meta_info(enum_def);
                Ok(Some(expr))
            },
            // function signatures
            |parser: &mut Parser<'i, '_>, stack: &Vec<StackElement>, depth| {
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
            |parser: &mut Parser<'i, '_>, _, depth| {
                let include = ExprInclude::parse_reset(parser, depth.duplicate())?;
                let code = if let Some(code) = parser.external_includes.get(&include.file.string) {
                    code.clone()
                } else {
                    let path = match util::try_resolve_file(&parser.include_config, &include.file.string) {
                        Ok(path) => path,
                        Err(ResolveFileError::CanonicalizeFailed(path, e)) => {
                            parser.diagnostics.error(ErrorCode::ErrorReadingIncludedFile)
                                .with_error_label(include.diagnostics_span(), format!("error canonicalizing `{}`", path.display()))
                                .with_error_label(include.diagnostics_span(), e.to_string())
                                .emit();
                            return Ok(None);
                        }
                        Err(ResolveFileError::NotInIncludeDirectory(include_path, path)) => {
                            parser.diagnostics.error(ErrorCode::ErrorReadingIncludedFile)
                                .with_error_label(include.diagnostics_span(), "the file is not in the include directory")
                                .with_info_label(include.diagnostics_span(), format!("this file resolved to {}", path.display()))
                                .with_error_label(include.diagnostics_span(), format!("included files must be in {}", include_path.display()))
                                .emit();
                            return Ok(None);
                        }
                        Err(ResolveFileError::IncludesDisallowed) => {
                            parser.diagnostics.error(ErrorCode::ErrorReadingIncludedFile)
                                .with_error_label(include.diagnostics_span(), "includes are disallowed")
                                .emit();
                            return Ok(None);
                        }
                    };
                    match ::std::fs::read_to_string(&path) {
                        Ok(code) => code,
                        Err(e) => {
                            parser.diagnostics.error(ErrorCode::ErrorReadingIncludedFile)
                                .with_error_label(include.diagnostics_span(), format!("error reading file `{}`", path.display()))
                                .with_error_label(include.diagnostics_span(), e.to_string())
                                .emit();
                            return Ok(None);
                        }
                    }
                };
                let (file, _) = parser.diagnostics.add_file(include.file.string.clone(), code);
                parser.meta_info.included_files.insert(include.diagnostics_span(), file);
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
        let functions: &[DoPassFunction<'i, 'b>] = &[
            // static signatures
            |parser: &mut Parser<'i, '_>, _, depth| {
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
            |parser: &mut Parser<'i, '_>, _, depth| {
                let include = ExprInclude::parse_reset(parser, depth.duplicate())?;
                let file = match parser.meta_info.included_files.get(&include.diagnostics_span()) {
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

    fn do_pass(&mut self, functions: &[DoPassFunction<'i, 'b>], depth: Depth) {
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
                scope.dont_remove();
                continue;
            }
            if let Ok(Token::OpenCurly(_)) = self.peek_token(0) {
                drop(self.next_token().unwrap());
                trace!("entering block");
                stack.push(StackElement::Block);
                scope.dont_remove();
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
                    self.pre_parsed.insert((expr.file_id(), expr.start()), expr);
                    // consume tokens except last one as that's consumed after the for loop
                    while self.peek_token(0).unwrap().diagnostics_span().end < expr.diagnostics_span().end {
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
pub struct ImplBlockSignature<'i> {
    pub name: TokenIdent<'i>,
}
impl<'i> Parse<'i> for ImplBlockSignature<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        // during the first pass when we find function signatures, we only need the impl-block-target
        let _: TokenImpl = parser.parse(depth.next())?;
        let name: TokenIdent<'i> = parser.parse(depth.next())?;
        let _: Option<ExprGenerics<'i>> = parser.parse(depth.next())?;
        let _: TokenOpenCurly = parser.parse(depth.last())?;
        Ok(ImplBlockSignature { name })
    }
}
