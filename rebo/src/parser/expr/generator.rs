use std::cell::RefCell;
use std::collections::HashMap;
use petgraph::graph::{DiGraph, NodeIndex};
use uuid::Uuid;
use super::*;
use std::mem;
use log::Level;
use petgraph::{Direction, visit::EdgeRef};
use rebo::common::expr_gen::{ExprBlockBuilder, ExprBuilder, ExprImplBlockBuilder, ExprMatchPatternBuilder, ExprStructDefinitionBuilder};
use crate::common::expr_gen::ExprBuilderBinding;


// Transforms a gen-fn into a generator-struct, its next-method-impl, and makes the original function return an instance
// of the generator-struct.
//
// IMPORTANT: All expression-spans of the original code are preserved.
// * if no yield is encountered, all original spans stay the same
// * if a yield is encountered, it's ensured that all original expressions keep their span
//     * tail-expression: wrap in parens: (<expr>)

pub fn transform_generator<'a, 'i>(parser: &mut Parser<'a, '_, 'i>, gen_token: TokenGen, fun: ExprFunctionDefinition<'a, 'i>) -> ExprFunctionDefinition<'a, 'i> {
    let trafo = GeneratorTransformator {
        parser,
        graph: DiGraph::new(),
        gen_token,
        self_binding: ExprBuilder::binding_new("self", true),
        match_bindings: RefCell::new(HashMap::new()),
        fields: RefCell::new(HashMap::new()),
    };

    trafo.transform(fun)
}

enum TrafoResult<'a, 'i> {
    Expr(&'a Expr<'a, 'i>),
    // first node of yield-subgraph, node of the block executed after the yield
    Yielded(NodeId, NodeId),
}
enum Edge<'a, 'i> {
    Pattern(ExprMatchPatternBuilder<'a, 'i>),
    // marker edge for visualization and debugging
    Yield,
}

type NodeId = NodeIndex<u32>;

struct GeneratorTransformator<'a, 'i, 'p, 'm> {
    parser: &'p mut Parser<'a, 'm, 'i>,
    graph: DiGraph<Option<ExprBuilder<'a, 'i>>, Edge<'a, 'i>, u32>,
    gen_token: TokenGen,
    self_binding: ExprBuilderBinding<'i>,
    /// each node needs its own match-binding as the types can be different
    match_bindings: RefCell<HashMap<NodeId, ExprBuilderBinding<'i>>>,
    /// List of all fields this generator needs to store temporaries
    fields: RefCell<HashMap<BindingId, TokenIdent<'i>>>,
}

impl<'a, 'i, 'p, 'm> GeneratorTransformator<'a, 'i, 'p, 'm> {
    pub fn transform(mut self, fun: ExprFunctionDefinition<'a, 'i>) -> ExprFunctionDefinition<'a, 'i> {
        let fun_span = fun.span();
        let body_open = fun.body.open.span;
        let body_close = fun.body.close.span;

        let trafo_result = self.transform_block(&fun.body);

        let (start, end) = match trafo_result {
            TrafoResult::Expr(expr) => {
                let block = match expr {
                    Expr::Block(block) => block,
                    _ => unreachable!("GeneratorTransformator::transform_block returned TrafoResult::Expr which is not a block: {:?}", expr),
                };
                let node = self.graph.add_node(Some(ExprBuilder::from_block_with_new_spans(block.body.clone())));
                (node, node)
            },
            TrafoResult::Yielded(start, end) => (start, end),
        };
        let option_none = ExprBuilder::enum_initialization("Option", "None");
        let return_none = ExprBuilder::return_(Some(option_none));
        let node = self.graph.add_node(Some(return_none));
        self.graph.add_edge(end, node, Edge::Pattern(self.get_match_pattern_into_node(node)));

        // graph is finished

        // generate state machine
        let yield_type = match fun.sig.ret_type {
            Some((_, typ)) => typ,
            None => ExprType::Unit(
                TokenOpenParen { span: fun.sig.span() },
                TokenCloseParen { span: fun.sig.span() },
            ),
        };
        let struct_ident = self.create_ident(format!("Generator_{}_{}_{}_{}", fun.sig.name.map(|i| i.ident).unwrap_or(""), fun_span.file, fun_span.start, fun_span.end));
        let impl_block_builder = self.generate_state_machine_impl_block(struct_ident, yield_type);
        let struct_builder = self.generate_generator_struct(struct_ident);

        if log_enabled!(Level::Trace) {
            // self.xdot();
        }


        // generate replacement function body for original gen fn
        let mut function_body = self.generate_generator_function_block(start, struct_ident);
        function_body.insert_expr(0, struct_builder.build());
        function_body.insert_expr(1, impl_block_builder.build());

        // actually build everything
        let generator_file_name = String::leak(format!("Generator_{}_{}_{}_{}.re", fun.sig.name.map(|i| i.ident).unwrap_or(""), fun_span.file, fun_span.start, fun_span.end));
        let mut function_body = ExprBuilder::generate(function_body, self.parser.arena, self.parser.diagnostics, generator_file_name);

        // add stuff to meta_info
        let struct_def = match function_body.body.exprs[0] {
            Expr::StructDefinition(struct_def) => struct_def,
            _ => unreachable!("we just created you"),
        };
        let impl_block = match function_body.body.exprs[1] {
            Expr::ImplBlock(impl_block) => impl_block,
            _ => unreachable!("we just created you"),
        };
        self.parser.add_struct_to_meta_info(struct_def);
        self.parser.add_impl_block_functions_to_meta_info(impl_block);

        // The parser takes the end of the current Expr as the start from where parsing should continue.
        // The generated function-body contains fake spans.
        // Thus, we need to modify the body's open and close curly to be the original spans.
        function_body.open.span = body_open;
        function_body.close.span = body_close;

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

    // helper functions

    fn empty_node(&mut self) -> NodeId {
        self.graph.add_node(Some(ExprBuilder::unit()))
    }


    // actual transformation functions

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
                    let inner_expr = self.gen_access_self_field_for_binding(self.get_match_binding_into_node(node).id());
                    let expr = ExprBuilder::bool_not(inner_expr);
                    self.graph[node] = Some(expr);
                    self.graph.add_edge(end, node, Edge::Pattern(self.get_match_pattern_into_node(node)));
                    TrafoResult::Yielded(start, node)
                }
            },
            Expr::Neg(_) => todo!(),
            // &Expr::Neg(ExprNeg { minus, expr }) => match self.transform_expr(expr) {
            //     TrafoResult::Expr(expr) => TrafoResult::Expr(self.parser.arena.alloc(Expr::Neg(ExprNeg { minus, expr }))),
            //     TrafoResult::Yielded(start, end) => {
            //         let node = self.empty_node();
            //         let expr = &*self.parser.arena.alloc(Expr::Neg(ExprNeg {
            //             minus: TokenMinus { span: self.next_fake_span("-") },
            //             expr:
            //         }))
            //     }
            // },

            // binops
            Expr::Add(ExprAdd { a, op, b }) => todo!(),
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
            Expr::Yield(ExprYield { expr, .. }) => {
                let node = self.empty_node();
                let (return_value_expr, inner_start_end) = match expr {
                    Some(expr) => match self.transform_expr(expr) {
                        TrafoResult::Expr(e) => (ExprBuilder::from_expr(e), None),
                        TrafoResult::Yielded(start, end) => (
                            self.gen_access_self_field_for_binding(self.get_match_binding_into_node(node).id()),
                            Some((start, end))
                        ),
                    },
                    None => (ExprBuilder::unit(), None),
                };
                let dummy_node = self.empty_node();
                let block = ExprBuilder::block()
                    .expr(self.gen_assign_next_state(dummy_node))
                    .expr(self.gen_return_some(return_value_expr))
                    .without_terminating_semicolon()
                    .build();
                self.graph[node] = Some(block);
                self.graph.add_edge(node, dummy_node, Edge::Yield);
                let start = if let Some((start, end)) = inner_start_end {
                    self.graph.add_edge(end, node, Edge::Pattern(self.get_match_pattern_into_node(node)));
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
    fn transform_block(&mut self, block: &ExprBlock<'a, 'i>) -> TrafoResult<'a, 'i> {
        let mut mapped_block = ExprBuilder::block();
        let mut orig_exprs = block.body.exprs.iter().peekable();
        let mut current_start_end = None;
        while let Some(next) = orig_exprs.next() {
            let (start, end) = match self.transform_expr(next) {
                TrafoResult::Expr(e) => {
                    mapped_block.push_expr(ExprBuilder::from_expr(e));
                    continue
                },
                TrafoResult::Yielded(start, end) => (start, end),
            };
            let block = mem::replace(&mut mapped_block, ExprBuilder::block())
                .with_terminating_semicolon()
                .build();
            let node = self.graph.add_node(Some(block));
            self.graph.add_edge(node, start, Edge::Pattern(self.get_match_pattern_into_node(start)));
            match &mut current_start_end {
                Some((_, current_end)) => {
                    self.graph.add_edge(*current_end, node, Edge::Pattern(self.get_match_pattern_into_node(node)));
                    *current_end = end;
                }
                None => current_start_end = Some((node, end)),
            }
        }

        if let Some((start, end)) = current_start_end {
            if block.body.terminated_with_semicolon {
                mapped_block.terminate_with_semicolon();
            } else {
                mapped_block.terminate_without_semicolon();
            }
            let block = mapped_block.build();
            let node = self.graph.add_node(Some(block));
            self.graph.add_edge(end, node, Edge::Pattern(self.get_match_pattern_into_node(node)));
            TrafoResult::Yielded(start, node)
        } else {
            TrafoResult::Expr(self.parser.arena.alloc(Expr::Block(block.clone())))
        }
    }

    fn field_ident(&self, binding_id: BindingId) -> TokenIdent<'i> {
        *self.fields.borrow_mut().entry(binding_id)
            .or_insert_with(|| self.create_ident(format!("binding_{}", binding_id)))
    }
    fn create_ident(&self, name: String) -> TokenIdent<'i> {
        let len = name.len();
        let (file, ident) = self.parser.diagnostics.add_file(Uuid::new_v4().to_string(), name);
        TokenIdent {
            span: Span::new(file, 0, len),
            ident,
        }
    }
    fn get_match_pattern_into_node(&self, node: NodeId) -> ExprMatchPatternBuilder<'a, 'i> {
        ExprBuilder::match_pattern_binding(self.get_match_binding_into_node(node))
    }
    fn get_match_binding_into_node(&self, node: NodeId) -> ExprBuilderBinding<'i> {
        self.match_bindings.borrow_mut().entry(node)
            .or_insert_with(|| ExprBuilder::binding_new("_match_binding", false))
            .clone()
    }

    fn gen_assign_self_binding_field(&self, binding_id: BindingId, expr: ExprBuilder<'a, 'i>) -> ExprBuilder<'a, 'i> {
        ExprBuilder::assign(&self.self_binding)
            .access_field(self.field_ident(binding_id).ident)
            .assign(self.gen_option_some_call(expr))
    }
    fn gen_access_self_field_for_binding(&self, binding_id: BindingId) -> ExprBuilder<'a, 'i> {
        ExprBuilder::access(&self.self_binding)
            .field(self.field_ident(binding_id).ident)
            .call_method("unwrap", Vec::new())
            .build()
    }

    fn gen_assign_next_state_to_expr(&self, expr: ExprBuilder<'a, 'i>) -> ExprBuilder<'a, 'i> {
        ExprBuilder::assign(&self.self_binding)
            .access_field("state")
            .assign(expr)
    }
    fn gen_assign_next_state(&self, id: NodeId) -> ExprBuilder<'a, 'i> {
        self.gen_assign_next_state_to_expr(ExprBuilder::literal(id.index() as i64))
    }

    fn gen_option_some_call(&self, expr: ExprBuilder<'a, 'i>) -> ExprBuilder<'a, 'i> {
        let option_some = self.parser.get_binding_unsafe_unsafe_unsafe("Option::Some").unwrap();
        ExprBuilder::function_call(option_some).arg(expr).call()
    }

    fn gen_return_some(&mut self, expr: ExprBuilder<'a, 'i>) -> ExprBuilder<'a, 'i> {
        ExprBuilder::return_(Some(self.gen_option_some_call(expr)))
    }

    // code generation functions for the actual final code

    fn generate_generator_struct(&mut self, struct_ident: TokenIdent<'i>) -> ExprStructDefinitionBuilder<'a, 'i> {
        let mut struct_builder = ExprBuilder::struct_definition(struct_ident.ident);
        struct_builder.push_field("state", ExprBuilder::int_type());
        for field in self.fields.borrow().values() {
            struct_builder.push_field(
                field.ident,
                ExprBuilder::user_type("Option").generic(ExprBuilder::any_type()).build(),
            );
        }
        struct_builder
    }

    fn generate_generator_function_block(&mut self, start: NodeId, struct_ident: TokenIdent<'i>) -> ExprBlockBuilder<'a, 'i> {
        // TODO: captures
        // TODO: arguments

        let mut struct_init = ExprBuilder::struct_initialization(struct_ident.ident)
            .field("state", ExprBuilder::literal(start.index() as i64));
        for field in self.fields.borrow().values() {
            struct_init.push_field(field.ident, ExprBuilder::enum_initialization("Option", "None"));
        }
        ExprBuilder::block()
            .expr(struct_init.build())
            .without_terminating_semicolon()
    }
    fn generate_state_machine_impl_block(&mut self, struct_ident: TokenIdent<'i>, yield_type: ExprType<'a, 'i>) -> ExprImplBlockBuilder<'a, 'i> {
        let self_type = ExprBuilder::user_type(struct_ident.ident).build();

        let fun_sig = ExprBuilder::function_definition("next")
            .self_arg(&self.self_binding, self_type)
            .ret_type(ExprBuilder::user_type("Option").generic(ExprBuilder::from_expr_type(yield_type)).build());

        let mut state_machine_match = ExprBuilder::match_(ExprBuilder::access(&self.self_binding).field("state").build());
        // we generate the following code as state-machine
        // ```rust
        // self.state = match self.state {
        //     0 => node[0]-code,
        //     1 => node[1]-code,
        //     _ => panic("unreachable state"),
        // }
        // ```
        for node_id in self.graph.node_indices() {
            // each node generates code like this:
            // ```rust
            // match { expr() } {
            //     pattern => {
            //         store all bindings from pattern in self-struct-fields
            //         new-state-id
            //     }
            // }
            // ```
            let node = self.graph[node_id].take().unwrap();
            let mut inner_match = ExprBuilder::match_(node);
            for edge in self.graph.edges_directed(node_id, Direction::Outgoing) {
                let pattern = match edge.weight() {
                    Edge::Pattern(pattern) => pattern,
                    Edge::Yield => continue,
                };

                let mut inner_match_block = ExprBuilder::block().without_terminating_semicolon();

                // if the pattern contains bindings, store them in self-struct-fields
                // e.g. the pattern `Foo::Bar(a, b, c) => ...` shall be transformed to
                // ```
                // self.binding_a = a;
                // self.binding_b = b;
                // self.binding_c = c;
                // ```
                match pattern {
                    // TODO: match pattern enum variants
                    // ExprMatchPatternBuilder::Variant(ExprMatchPatternVariant { fields: Some((_, fields, _)), .. }) => {
                    //     for &field in fields {
                    //         inner_match_block.push_expr(self.gen_assign_self_binding_field(field, ExprBuilder::variable(field)));
                    //     }
                    // },
                    ExprMatchPatternBuilder::Binding(binding) => {
                        inner_match_block.push_expr(self.gen_assign_self_binding_field(binding.id(), ExprBuilder::variable(binding)));
                    },
                    ExprMatchPatternBuilder::Literal(_)
                    | ExprMatchPatternBuilder::Wildcard
                    | ExprMatchPatternBuilder::Variant(_) => (),
                }
                inner_match_block.push_expr(ExprBuilder::literal(edge.target().index() as i64));
                inner_match.push_arm(pattern.clone(), inner_match_block.build());
            }

            // add this node's match-arm to the state-machine
            state_machine_match.push_arm(ExprBuilder::match_pattern_literal(node_id.index() as i64), inner_match.build());
        }

        // add "unreachable state" wildcard arm
        let panic_binding = self.parser.get_binding_unsafe_unsafe_unsafe("panic").unwrap();
        state_machine_match.push_arm(
            ExprBuilder::match_pattern_wildcard(),
            ExprBuilder::function_call(panic_binding)
                .arg(ExprBuilder::literal("unreachable state"))
                .call(),
        );

        let state_assign = ExprBuilder::assign(&self.self_binding)
            .access_field("state")
            .assign(state_machine_match.build());
        let state_loop = ExprBuilder::loop_(None, ExprBuilder::block().expr(state_assign));

        let fun_body = ExprBuilder::block().without_terminating_semicolon().expr(state_loop);

        ExprBuilder::impl_block(struct_ident.ident)
            .function(fun_sig.body(fun_body))
    }

    // pub fn xdot(&self) {
    //     use petgraph::dot::Dot;
    //     use std::process::{Command, Stdio};
    //     use std::io::Write;
    //     let mut xdot = Command::new("xdot")
    //         .arg("-")
    //         .stdin(Stdio::piped())
    //         .stdout(Stdio::inherit())
    //         .stderr(Stdio::inherit())
    //         .spawn()
    //         .unwrap();
    //     {
    //         let dot = Dot::with_attr_getters(
    //             &self.graph,
    //             &[],
    //             &|_, edge| match edge.weight() {
    //                 Edge::Pattern(p) => format!("label = \"{}\"", p),
    //                 Edge::Yield => "style = dotted".to_string(),
    //             },
    //             &|_, (node_id, expr)| format!("label = \"[{}]\n{}\"", node_id.index(), expr),
    //         );
    //         writeln!(xdot.stdin.take().unwrap(), "{}", dot).unwrap();
    //     }
    //     xdot.wait().unwrap();
    // }
}
