use std::cell::RefCell;
use std::collections::HashMap;
use petgraph::dot::Dot;
use petgraph::graph::{DiGraph, NodeIndex};
use uuid::Uuid;
use crate::lexer::Radix;
use super::*;
use std::process::{Command, Stdio};
use std::io::Write;
use std::mem;
use diagnostic::FileId;
use log::Level;
use petgraph::{Direction, visit::EdgeRef};
use rebo::lexer::TokenUnderscore;


//! Transforms a gen-fn into a generator-struct, its next-method-impl, and makes the original function return an instance
//! of the generator-struct.
//!
//! IMPORTANT: All expression-spans of the original code are preserved.
//! * if no yield is encountered, all original spans stay the same
//! * if a yield is encountered, it's ensured that all original expressions keep their span
//!     * tail-expression: wrap in parens: (<expr>)

pub fn transform_generator<'a, 'i>(parser: &mut Parser<'a, '_, 'i>, gen_token: TokenGen, fun: ExprFunctionDefinition<'a, 'i>) -> ExprFunctionDefinition<'a, 'i> {
    let generator_file_name = Box::leak(Box::new(format!("Generator_{}_{}_{}_{}.re", fun.sig.name.map(|i| i.ident).unwrap_or(""), fun.span().file, fun.span().start, fun.span().end)));
    let generator_file_code = "mut self ".to_string();
    let mut trafo = GeneratorTransformator {
        parser,
        graph: DiGraph::new(),
        gen_token,
        self_binding: Binding {
            id: BindingId::unique(),
            mutable: Some(TokenMut { span: Span::new(FileId::synthetic(generator_file_name), 0, 3) }),
            ident: TokenIdent {
                span: Span::new(FileId::synthetic(generator_file_name), 4, 8),
                ident: "self",
            },
            rogue: false,
        },
        match_bindings: RefCell::new(HashMap::new()),
        fields: RefCell::new(HashMap::new()),
        generator_file_name,
        generator_file_code: RefCell::new(generator_file_code),
    };

    trafo.transform(fun)
}

enum TrafoResult<'a, 'i> {
    Expr(&'a Expr<'a, 'i>),
    // first node of yield-subgraph, node of the block executed after the yield
    Yielded(NodeId, NodeId),
}
enum Edge<'a, 'i> {
    Pattern(ExprMatchPattern<'a, 'i>),
    // marker edge for visualization and debugging
    Yield,
}
impl<'a, 'i> Display for Edge<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Edge::Pattern(p) => Display::fmt(p, f),
            Edge::Yield => write!(f, "yield"),
        }
    }
}
type NodeId = NodeIndex<u32>;

struct GeneratorTransformator<'a, 'i, 'p, 'm> {
    parser: &'p mut Parser<'a, 'm, 'i>,
    graph: DiGraph<&'a Expr<'a, 'i>, Edge<'a, 'i>, u32>,
    gen_token: TokenGen,
    self_binding: Binding<'i>,
    /// each node needs its own match-binding as the types can be different
    match_bindings: RefCell<HashMap<NodeId, Binding<'i>>>,
    /// List of all fields this generator needs to store temporaries
    fields: RefCell<HashMap<Binding<'i>, TokenIdent<'i>>>,
    generator_file_name: &'static str,
    generator_file_code: RefCell<String>,
}

impl<'a, 'i, 'p, 'm> GeneratorTransformator<'a, 'i, 'p, 'm> {
    fn next_fake_span(&self, ctx: &str) -> Span {
        assert_ne!(ctx.len(), 0);
        let mut code = self.generator_file_code.borrow_mut();
        let start = code.len();
        code.push_str(ctx);
        let end = code.len();
        Span::new(FileId::synthetic(self.generator_file_name), start, end)
    }
    pub fn transform(&mut self, fun: ExprFunctionDefinition<'a, 'i>) -> ExprFunctionDefinition<'a, 'i> {
        let fun_span = fun.span();
        let open = fun.body.open;
        let close = fun.body.close;

        let trafo_result = self.transform_block(&fun.body);

        let (start, end) = match trafo_result {
            TrafoResult::Expr(expr) => {
                let node = self.graph.add_node(expr);
                (node, node)
            },
            TrafoResult::Yielded(start, end) => (start, end),
        };
        let return_none = self.parser.arena.alloc(Expr::Return(ExprReturn {
            return_token: TokenReturn { span: self.next_fake_span("return ") },
            expr: Some(self.parser.arena.alloc(Expr::EnumInitialization(ExprEnumInitialization {
                enum_name: TokenIdent {
                    span: self.next_fake_span("Option"),
                    ident: "Option",
                },
                double_colon: TokenDoubleColon { span: self.next_fake_span("::") },
                variant_name: TokenIdent {
                    span: self.next_fake_span("None "),
                    ident: "None",
                },
            })))
        }));
        let node = self.graph.add_node(return_none);
        self.graph.add_edge(end, node, Edge::Pattern(self.gen_pattern_binding(node)));

        // generate state machine
        let yield_type = match fun.sig.ret_type {
            Some((_, typ)) => typ,
            None => ExprType::Unit(
                TokenOpenParen { span: fun.sig.span() },
                TokenCloseParen { span: fun.sig.span() },
            ),
        };
        let struct_ident = self.create_ident(format!("Generator_{}_{}_{}_{}", fun.sig.name.map(|i| i.ident).unwrap_or(""), fun_span.file, fun_span.start, fun_span.end));
        // ORDER IS RELEVANT
        let (impl_block_expr, impl_block) = self.generate_state_machine_impl_block(struct_ident, yield_type);
        let (struct_def_expr, struct_def) = self.generate_generator_struct(struct_ident);

        // generate replacement function body for original gen fn
        let mut function_body = self.generate_generator_function_block(start, open, close, struct_ident);
        function_body.body.exprs.insert(0, struct_def_expr);
        function_body.body.exprs.insert(1, impl_block_expr);


        if log_enabled!(Level::Trace) {
            self.xdot();
        }

        // generate fake diagnostics file for fake spans
        self.parser.diagnostics.add_synthetic_file(self. generator_file_name, self.generator_file_code.borrow().clone());

        // add stuff to meta_info only after we added the synthetic file in case of a diagnostic
        self.parser.add_struct_to_meta_info(struct_def);
        self.parser.add_impl_block_functions_to_meta_info(impl_block);

        ExprFunctionDefinition {
            sig: ExprFunctionSignature {
                gen_token: Some(self.gen_token),
                fn_token: fun.sig.fn_token,
                name: fun.sig.name,
                generics: fun.sig.generics,
                open: fun.sig.open,
                self_arg: fun.sig.self_arg,
                self_arg_comma: fun.sig.self_arg_comma,
                args: fun.sig.args,
                varargs: fun.sig.varargs,
                close: fun.sig.close,
                ret_type: Some((TokenArrow { span: struct_ident.span }, ExprType::UserType(struct_ident, None))),
            },
            captures: fun.captures,
            body: function_body,
        }
    }
    fn transform_expr(&mut self, expr: &'a Expr<'a, 'i>) -> TrafoResult<'a, 'i> {
        match expr {
            lit @ Expr::Literal(_) => TrafoResult::Expr(lit),
            Expr::FormatString(_) => todo!(),
            Expr::Bind(_) => todo!(),
            Expr::Static(_) => todo!(),
            Expr::Assign(_) => todo!(),
            // unops
            &Expr::BoolNot(ExprBoolNot { bang, expr }) => match self.transform_expr(expr) {
                TrafoResult::Expr(expr) => TrafoResult::Expr(self.parser.arena.alloc(Expr::BoolNot(ExprBoolNot { bang, expr }))),
                TrafoResult::Yielded(start, end) => {
                    let node = self.empty_node();
                    let expr = &*self.parser.arena.alloc(Expr::BoolNot(ExprBoolNot {
                        bang: TokenBang { span: self.next_fake_span("! ") },
                        expr: self.gen_access_self(self.match_binding(node)),
                    }));
                    self.graph[node] = expr;
                    self.graph.add_edge(end, node, Edge::Pattern(self.gen_pattern_binding(node)));
                    TrafoResult::Yielded(start, node)
                }
            },
            Expr::Neg(_) => todo!(),
            // binops
            Expr::Add(_) => todo!(),
            Expr::Sub(_) => todo!(),
            Expr::Mul(_) => todo!(),
            Expr::Div(_) => todo!(),
            Expr::Mod(_) => todo!(),
            Expr::Xor(_) => todo!(),
            Expr::BoolAnd(_) => todo!(),
            Expr::BoolOr(_) => todo!(),
            // binop-assign
            Expr::AddAssign(_) => todo!(),
            Expr::SubAssign(_) => todo!(),
            Expr::MulAssign(_) => todo!(),
            Expr::DivAssign(_) => todo!(),
            Expr::ModAssign(_) => todo!(),
            Expr::XorAssign(_) => todo!(),
            Expr::BoolAndAssign(_) => todo!(),
            Expr::BoolOrAssign(_) => todo!(),
            // comparison ops
            Expr::LessThan(_) => todo!(),
            Expr::LessEquals(_) => todo!(),
            Expr::Equals(_) => todo!(),
            Expr::NotEquals(_) => todo!(),
            Expr::GreaterEquals(_) => todo!(),
            Expr::GreaterThan(_) => todo!(),
            Expr::Block(block) => self.transform_block(block),
            Expr::Variable(_) => todo!(),
            Expr::Access(_) => todo!(),
            Expr::Parenthesized(_) => todo!(),
            Expr::IfElse(_) => todo!(),
            Expr::Match(_) => todo!(),
            Expr::While(_) => todo!(),
            Expr::For(_) => todo!(),
            Expr::Loop(_) => todo!(),
            Expr::Break(_) => todo!(),
            Expr::Continue(_) => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Yield(ExprYield { yield_token, expr }) => {
                let node = self.empty_node();
                let (expr, inner_start_end) = match expr {
                    Some(expr) => match self.transform_expr(expr) {
                        TrafoResult::Expr(e) => (e, None),
                        TrafoResult::Yielded(start, end) => (self.gen_access_self(self.match_binding(node)), Some((start, end))),
                    },
                    None => (self.gen_unit(yield_token.span), None),
                };
                let dummy_node = self.add_dummy_node();
                let ret = self.gen_return_some(expr);
                let block = self.gen_block(self.next_fake_span(&format!("{{ {expr} }} ")), vec![
                    self.gen_assign_next_state(dummy_node),
                    ret,
                ]);
                self.graph[node] = block;
                self.graph.add_edge(node, dummy_node, Edge::Yield);
                let start = if let Some((start, end)) = inner_start_end {
                    self.graph.add_edge(end, node, Edge::Pattern(self.gen_pattern_binding(node)));
                    start
                } else {
                    node
                };
                TrafoResult::Yielded(start, dummy_node)
            },
            Expr::FunctionCall(_) => todo!(),
            Expr::FunctionDefinition(_) => todo!(),
            Expr::StructDefinition(_) => todo!(),
            Expr::StructInitialization(_) => todo!(),
            Expr::EnumDefinition(_) => todo!(),
            Expr::EnumInitialization(_) => todo!(),
            Expr::ImplBlock(_) => todo!(),
        }
    }
    fn empty_node(&mut self) -> NodeId {
        self.graph.add_node(&Expr::Literal(ExprLiteral::Bool(ExprBool { b: TokenBool { span: EXTERNAL_SPAN, value: false } })))
    }
    fn transform_block(&mut self, block: &ExprBlock<'a, 'i>) -> TrafoResult<'a, 'i> {
        let mut mapped = Vec::new();
        let mut orig = block.body.exprs.iter().peekable();
        let mut span_start = block.span().start + 1;
        let mut current_start_end = None;
        while let Some(next) = orig.next() {
            let (start, end) = match self.transform_expr(next) {
                TrafoResult::Expr(e) => {
                    mapped.push(e);
                    continue
                },
                TrafoResult::Yielded(start, end) => (start, end),
            };
            let span = Span::new(block.span().file, span_start, next.span().end);
            span_start = next.span().end;
            let block = self.gen_block(span, mem::take(&mut mapped));
            let node = self.graph.add_node(block);
            self.graph.add_edge(node, start, Edge::Pattern(self.gen_pattern_binding(start)));
            match &mut current_start_end {
                Some((_, current_end)) => {
                    self.graph.add_edge(*current_end, node, Edge::Pattern(self.gen_pattern_binding(node)));
                    *current_end = end;
                }
                None => current_start_end = Some((node, end)),
            }
        }

        if let Some((start, end)) = current_start_end {
            let span = Span::new(block.span().file, span_start, block.span().end - 1);
            let block = self.gen_block(span, mapped);
            let node = self.graph.add_node(block);
            self.graph.add_edge(end, node, Edge::Pattern(self.gen_pattern_binding(node)));
            TrafoResult::Yielded(start, node)
        } else {
            TrafoResult::Expr(self.parser.arena.alloc(Expr::Block(block.clone())))
        }
    }

    fn add_dummy_node(&mut self) -> NodeId {
        let unit = self.gen_unit(self.next_fake_span("() "));
        self.graph.add_node(unit)
    }
    fn gen_match_binding_expr(&self, node: NodeId) -> &'a Expr<'a, 'i> {
        let match_binding = self.match_binding(node);
        self.parser.arena.alloc(Expr::Variable(ExprVariable {
            binding: match_binding,
            span: self.next_fake_span(&format!("{match_binding} ")),
        }))
    }
    fn gen_unit(&mut self, span: Span) -> &'a Expr<'a, 'i> {
        self.parser.arena.alloc(Expr::Literal(ExprLiteral::Unit(ExprUnit {
            open: TokenOpenParen { span },
            close: TokenCloseParen { span },
        })))
    }

    fn gen_block(&self, span: Span, exprs: Vec<&'a Expr<'a, 'i>>) -> &'a Expr<'a, 'i> {
        self.parser.arena.alloc(Expr::Block(ExprBlock {
            open: TokenOpenCurly { span: Span::new(span.file, span.start, span.start + 1) },
            body: BlockBody { exprs, terminated_with_semicolon: false },
            close: TokenCloseCurly { span: Span::new(span.file, span.end - 1, span.end) },
        }))
    }

    fn field_ident(&self, binding: Binding<'i>) -> TokenIdent<'i> {
        *self.fields.borrow_mut().entry(binding)
            .or_insert_with(|| self.create_ident(format!("binding_{}", binding.id)))
    }
    fn create_ident(&self, name: String) -> TokenIdent<'i> {
        let len = name.len();
        let (file, ident) = self.parser.diagnostics.add_file(Uuid::new_v4().to_string(), name);
        TokenIdent {
            span: Span::new(file, 0, len),
            ident,
        }
    }
    fn gen_pattern_binding(&self, node: NodeId) -> ExprMatchPattern<'a, 'i> {
        ExprMatchPattern::Binding(self.match_binding(node))
    }
    fn match_binding(&self, node: NodeId) -> Binding<'i> {
        self.match_bindings.borrow_mut().entry(node).or_insert_with(|| Binding {
            id: BindingId::unique(),
            mutable: None,
            ident: TokenIdent {
                span: self.next_fake_span("_match_binding "),
                ident: "_match_binding",
            },
            rogue: false,
        }).clone()
    }

    fn gen_assign_self(&self, binding: Binding<'i>, assign: TokenAssign, expr: &'a Expr<'a, 'i>) -> &'a Expr<'a, 'i> {
        self.parser.arena.alloc(Expr::Assign(ExprAssign {
            lhs: ExprAssignLhs::FieldAccess(ExprFieldAccess {
                variable: ExprVariable {
                    binding: self.self_binding,
                    span: self.next_fake_span(&format!("{} ", self.self_binding)),
                },
                dot: TokenDot { span: self.next_fake_span(".") },
                fields: Separated::default().append(None, {
                    let ident = self.field_ident(binding);
                    let span = self.next_fake_span(ident.ident);
                    TokenIdent { ident: ident.ident, span }
                }),
            }),
            assign,
            expr: self.gen_option_some_call(expr),
        }))
    }
    fn gen_access_self(&self, binding: Binding<'i>) -> &'a Expr<'a, 'i> {
        self.parser.arena.alloc(Expr::Access(ExprAccess {
            variable: ExprVariable {
                binding: self.self_binding,
                span: self.next_fake_span(&format!("{}", self.self_binding)),
            },
            dot: TokenDot { span: self.next_fake_span(".") },
            accesses: Separated::default()
                .append(None, FieldOrMethod::Field(self.field_ident(binding)))
                .append(Some(TokenDot { span: self.next_fake_span(".") }), FieldOrMethod::Method(ExprMethodCall {
                    name: self.parser.meta_info.rebo_functions["Option::unwrap"].sig.name.unwrap(),
                    open: TokenOpenParen { span: self.next_fake_span("(") },
                    args: Separated::default(),
                    close: TokenCloseParen { span: self.next_fake_span(") ") },
                })),
        }))
    }

    fn gen_assign_state_expr(&self, expr: &'a Expr<'a, 'i>) -> &'a Expr<'a, 'i> {
        self.parser.arena.alloc(Expr::Assign(ExprAssign {
            lhs: ExprAssignLhs::FieldAccess(ExprFieldAccess {
                variable: ExprVariable {
                    binding: self.self_binding,
                    span: self.next_fake_span(&format!("{} ", self.self_binding)),
                },
                dot: TokenDot { span: self.next_fake_span(".") },
                fields: Separated::default().append(None, TokenIdent {
                    span: self.next_fake_span("state"),
                    ident: "state",
                }),
            }),
            assign: TokenAssign { span: self.next_fake_span("=") },
            expr,
        }))
    }
    fn gen_assign_next_state(&self, id: NodeId) -> &'a Expr<'a, 'i> {
        self.gen_assign_state_expr(self.parser.arena.alloc(
            Expr::Literal(ExprLiteral::Integer(ExprInteger {
                int: TokenInteger {
                    span: self.next_fake_span(&format!("{} ", id.index())),
                    value: id.index() as i64,
                    radix: Radix::Dec,
                },
            }))
        ))
    }

    fn gen_option_some_call(&self, expr: &'a Expr<'a, 'i>) -> &'a Expr<'a, 'i> {
        let option_some = self.parser.get_binding_unsafe_unsafe_unsafe("Option::Some").unwrap();
        self.parser.arena.alloc(Expr::FunctionCall(ExprFunctionCall {
            name: ExprVariable {
                binding: option_some,
                span: self.next_fake_span("Option::Some"),
            },
            open: TokenOpenParen { span: self.next_fake_span("(") },
            args: Separated::default().append(None, expr),
            close: TokenCloseParen { span: self.next_fake_span(")") },
        }))
    }

    fn gen_return_some(&mut self, expr: &'a Expr<'a, 'i>) -> &'a Expr<'a, 'i> {
        self.parser.arena.alloc(Expr::Return(ExprReturn {
            return_token: TokenReturn { span: self.next_fake_span("return ") },
            expr: Some(self.gen_option_some_call(expr)),
        }))
    }

    fn generate_generator_struct(&mut self, struct_ident: TokenIdent<'i>) -> (&'a Expr<'a, 'i>, &'a ExprStructDefinition<'a, 'i>) {
        self.next_fake_span("\nBEGIN GENERATOR STRUCT\n");
        let mut fields = Separated::default();
        fields.push_back(None, (
            TokenIdent { span: self.next_fake_span("state "), ident: "state" },
            TokenColon { span: self.next_fake_span(": ") },
            ExprType::Int(TokenIntType { span: self.next_fake_span("int ") }),
        ));
        for &field in self.fields.borrow().values() {
            fields.push_back(
                Some(TokenComma { span: self.next_fake_span(", ") }),
                (
                    field,
                    TokenColon { span: self.next_fake_span(": ") },
                    ExprType::UserType(
                        self.parser.meta_info.user_types["Option"].ident(),
                        Some((
                            TokenLessThan { span: self.next_fake_span("<") },
                            Box::new(Separated::default().append(None, ExprType::Any)),
                            TokenGreaterThan { span: self.next_fake_span("> ") },
                        )),
                    )
                )
            );
        }
        let expr = &*self.parser.arena.alloc(Expr::StructDefinition(ExprStructDefinition {
            struct_token: TokenStruct { span: self.next_fake_span("struct ") },
            name: struct_ident,
            generics: None,
            open: TokenOpenCurly { span: self.next_fake_span("{ ") },
            fields,
            close: TokenCloseCurly { span: self.next_fake_span(" } ") },
        }));
        let struct_def = match expr {
            Expr::StructDefinition(s) => s,
            _ => unreachable!("we literally just created you"),
        };
        self.next_fake_span("\nEND GENERATOR STRUCT\n");
        (expr, struct_def)
    }

    fn generate_generator_function_block(&mut self, start: NodeId, open: TokenOpenCurly, close: TokenCloseCurly, struct_ident: TokenIdent<'i>) -> ExprBlock<'a, 'i> {
        // TODO: captures
        // TODO: arguments
        self.next_fake_span("\nBEGIN FUNCTION BLOCK\n");
        let option_none = || &*self.parser.arena.alloc(Expr::EnumInitialization(ExprEnumInitialization {
            enum_name: TokenIdent {
                span: self.next_fake_span("Option"),
                ident: "Option",
            },
            double_colon: TokenDoubleColon { span: self.next_fake_span("::") },
            variant_name: TokenIdent {
                span: self.next_fake_span("None "),
                ident: "None",
            },
        }));
        let mut fields = Separated::default();
        fields.push_back(None, (
            TokenIdent {
                span: self.next_fake_span("state"),
                ident: "state",
            },
            TokenColon { span: self.next_fake_span(" : ") },
            &*self.parser.arena.alloc(Expr::Literal(ExprLiteral::Integer(ExprInteger { int: TokenInteger {
                span: self.next_fake_span(&format!("{} ", start.index())),
                value: start.index() as i64,
                radix: Radix::Dec,
            }}))),
        ));
        for &field in self.fields.borrow().values() {
            fields.push_back(
                Some(TokenComma { span: self.next_fake_span(", ") }),
                (field, TokenColon { span: self.next_fake_span(" : ") }, option_none()),
            );
        }
        let block = ExprBlock {
            open,
            body: BlockBody {
                exprs: vec![
                    self.parser.arena.alloc(Expr::StructInitialization(ExprStructInitialization {
                        name: TokenIdent { ident: struct_ident.ident, span: self.next_fake_span(struct_ident.ident) },
                        open: TokenOpenCurly { span: self.next_fake_span("{ ") },
                        fields,
                        close: TokenCloseCurly { span: self.next_fake_span(" } ") },
                    }))
                ],
                terminated_with_semicolon: false
            },
            close,
        };
        self.next_fake_span("\nEND FUNCTION BLOCK\n");
        block
    }
    fn generate_state_machine_impl_block(&mut self, struct_ident: TokenIdent<'i>, yield_type: ExprType<'a, 'i>) -> (&'a Expr<'a, 'i>, &'a ExprImplBlock<'a, 'i>) {
        self.next_fake_span("\nBEGIN STATE MACHINE IMPL BLOCK\n");
        let expr = &*self.parser.arena.alloc(Expr::ImplBlock(ExprImplBlock {
            impl_token: TokenImpl { span: self.next_fake_span("impl ") },
            name: struct_ident,
            generics: None,
            open: TokenOpenCurly { span: self.next_fake_span("{ ") },
            functions: vec![
                ExprFunctionDefinition {
                    sig: ExprFunctionSignature {
                        gen_token: None,
                        fn_token: TokenFn { span: self.next_fake_span("fn ") },
                        name: Some(self.create_ident("next".to_string())),
                        generics: None,
                        open: TokenOpenParen { span: self.next_fake_span("(") },
                        self_arg: Some(self.self_binding),
                        self_arg_comma: None,
                        args: Separated::default().append(None, ExprPatternTyped {
                            pattern: ExprPatternUntyped { binding: self.self_binding },
                            colon_token: TokenColon {
                                span: Span::new(self.self_binding.ident.span.file, self.self_binding.ident.span.end, self.self_binding.ident.span.end),
                            },
                            typ: ExprType::UserType(struct_ident, None),
                        }),
                        varargs: None,
                        close: TokenCloseParen { span: self.next_fake_span(") ") },
                        ret_type: Some((
                            TokenArrow { span: self.next_fake_span("-> ") },
                            ExprType::UserType(
                                self.parser.meta_info.user_types["Option"].ident(),
                                Some((
                                    TokenLessThan { span: self.next_fake_span("<") },
                                    Box::new(Separated::default().append(None, yield_type)),
                                    TokenGreaterThan { span: self.next_fake_span("> ") },
                                )),
                            )
                        )),
                    },
                    captures: IndexSet::new(),
                    body: ExprBlock {
                        open: TokenOpenCurly { span: self.next_fake_span("{ ") },
                        body: BlockBody {
                            exprs: vec![
                                self.parser.arena.alloc(Expr::Loop(ExprLoop {
                                    label: None,
                                    loop_token: TokenLoop { span: self.next_fake_span("loop ") },
                                    block: ExprBlock {
                                        open: TokenOpenCurly { span: self.next_fake_span("{ ") },
                                        body: BlockBody {
                                            exprs: vec![
                                                self.gen_assign_state_expr(self.parser.arena.alloc(Expr::Match(ExprMatch {
                                                    match_token: TokenMatch { span: self.next_fake_span("match ") },
                                                    expr: self.parser.arena.alloc(Expr::Access(ExprAccess {
                                                        variable: ExprVariable {
                                                            binding: self.self_binding,
                                                            span: self.next_fake_span(&format!("{} ", self.self_binding)),
                                                        },
                                                        dot: TokenDot { span: self.next_fake_span(".") },
                                                        accesses: Separated::default().append(None, FieldOrMethod::Field(TokenIdent {
                                                            span: self.next_fake_span("state "),
                                                            ident: "state",
                                                        })),
                                                    })),
                                                    open: TokenOpenCurly { span: self.next_fake_span("{ ") },
                                                    arms: self.graph.node_indices()
                                                        .map(|node_id| self.generate_arm_for_node(node_id))
                                                        .chain([(
                                                            ExprMatchPattern::Wildcard(TokenUnderscore { span: self.next_fake_span("_ ") }),
                                                            TokenFatArrow { span: self.next_fake_span("=> ") },
                                                            &*self.parser.arena.alloc(Expr::FunctionCall(ExprFunctionCall {
                                                                name: ExprVariable {
                                                                    binding: self.parser.get_binding_unsafe_unsafe_unsafe("panic").unwrap(),
                                                                    span: self.next_fake_span("panic "),
                                                                },
                                                                open: TokenOpenParen { span: self.next_fake_span("(") },
                                                                args: Separated::default().append(None, self.parser.arena.alloc(Expr::Literal(ExprLiteral::String(ExprString {
                                                                    string: TokenDqString {
                                                                        span: self.next_fake_span("\"unreachable\""),
                                                                        string: "unreachable".to_string()
                                                                    }
                                                                })))),
                                                                close: TokenCloseParen { span: self.next_fake_span(") ") },
                                                            }))
                                                        )]).collect(),
                                                    close: TokenCloseCurly { span: self.next_fake_span(" } ") },
                                                })))
                                            ],
                                            terminated_with_semicolon: false,
                                        },
                                        close: TokenCloseCurly { span: self.next_fake_span(" } ") },
                                    },
                                }))
                            ],
                            terminated_with_semicolon: false
                        },
                        close: TokenCloseCurly { span: self.next_fake_span(" } ") },
                    },
                }
            ],
            close: TokenCloseCurly { span: self.next_fake_span(" } ") },
        }));

        let impl_block = match expr {
            Expr::ImplBlock(impl_block) => impl_block,
            _ => unreachable!("we just created you"),
        };
        self.next_fake_span("\nEND STATE MACHINE IMPL BLOCK\n");
        (expr, impl_block)
    }

    fn generate_arm_for_node(&self, node_id: NodeId) -> (ExprMatchPattern<'a, 'i>, TokenFatArrow, &'a Expr<'a, 'i>) {
        let node = self.graph[node_id];
        let expr = self.parser.arena.alloc(Expr::Match(ExprMatch {
            match_token: TokenMatch { span: self.next_fake_span("match ") },
            expr: node,
            open: TokenOpenCurly { span: self.next_fake_span(" { ") },
            arms: self.graph.edges_directed(node_id, Direction::Outgoing)
                .filter_map(|edge| match edge.weight() {
                    Edge::Pattern(pattern) => Some((edge, pattern)),
                    Edge::Yield => None,
                }).map(|(edge, pattern)| {
                    assert_eq!(edge.source(), node_id);
                    (
                        pattern.clone(),
                        TokenFatArrow { span: self.next_fake_span(" => ") },
                        self.gen_block(
                            self.next_fake_span("{ [block] }"),
                        {
                                let mut vec = self.generate_pattern_binding_store_code(pattern);
                                vec.push(self.parser.arena.alloc(Expr::Literal(ExprLiteral::Integer(ExprInteger {
                                    int: TokenInteger {
                                        span: self.next_fake_span(&format!("{} ", edge.target().index())),
                                        value: edge.target().index() as i64,
                                        radix: Radix::Dec,
                                    }
                                }))));
                                vec
                            },
                        ),
                    )
                }).collect(),
            close: TokenCloseCurly { span: self.next_fake_span(" } ") },
        }));
        self.generate_state_machine_arm(node_id, expr)
    }

    fn generate_pattern_binding_store_code(&self, pattern: &ExprMatchPattern<'a, 'i>) -> Vec<&'a Expr<'a, 'i>> {
        let gen_binding = |&binding: &Binding<'i>| self.gen_assign_self(
            binding,
            TokenAssign { span: self.next_fake_span(" = ") },
            self.parser.arena.alloc(Expr::Variable(ExprVariable { binding, span: self.next_fake_span(&format!("{} ", binding)) })),
        );
        match pattern {
            ExprMatchPattern::Variant(ExprMatchPatternVariant { fields: Some((_open, fields, _close)), .. }) => {
                fields.iter().map(gen_binding).collect()
            },
            ExprMatchPattern::Binding(binding) => vec![gen_binding(binding)],
            ExprMatchPattern::Literal(_)
            | ExprMatchPattern::Wildcard(_)
            | ExprMatchPattern::Variant(_) => vec![],
        }
    }

    fn generate_state_machine_arm(&self, node_id: NodeId, expr: &'a Expr<'a, 'i>) -> (ExprMatchPattern<'a, 'i>, TokenFatArrow, &'a Expr<'a, 'i>) {
        (
            ExprMatchPattern::Literal(ExprLiteral::Integer(ExprInteger {
                int: TokenInteger {
                    span: self.next_fake_span(&format!("{} ", node_id.index())),
                    value: node_id.index() as i64,
                    radix: Radix::Dec,
                }
            })),
            TokenFatArrow { span: self.next_fake_span(" => ") },
            expr
        )
    }

    pub fn xdot(&self) {
        let mut xdot = Command::new("xdot")
            .arg("-")
            .stdin(Stdio::piped())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap();
        {
            let dot = Dot::with_attr_getters(
                &self.graph,
                &[],
                &|_, edge| match edge.weight() {
                    Edge::Pattern(p) => format!("label = \"{}\"", p),
                    Edge::Yield => "style = dotted".to_string(),
                },
                &|_, (node_id, expr)| format!("label = \"[{}]\n{}\"", node_id.index(), expr),
            );
            writeln!(xdot.stdin.take().unwrap(), "{}", dot).unwrap();
        }
        xdot.wait().unwrap();
    }
}
