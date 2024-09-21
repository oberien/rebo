use crate::typeck::graph::{Graph, Constraint, Node, PossibleTypes};
use crate::common::{MetaInfo, Spanned};
use std::collections::{VecDeque, HashSet};
use std::iter::FromIterator;
use log::Level;
use crate::typeck::types::{ResolvableSpecificType, Type, FunctionType};
use petgraph::prelude::EdgeIndex;
use crate::SpecificType;
use crate::typeck::graph::create::FunctionGenerics;

struct WorkQueue {
    queue: VecDeque<Node>,
    set: HashSet<Node>,
}

impl WorkQueue {
    pub fn new() -> Self {
        WorkQueue {
            queue: VecDeque::new(),
            set: HashSet::new(),
        }
    }
    pub fn add(&mut self, graph: &Graph, node: Node) {
        for node in graph.outgoing_neighbors(node).chain(std::iter::once(node)) {
            self.add_single(node);
        }
    }
    pub fn add_single(&mut self, node: Node) {
        if self.set.contains(&node) {
            if log_enabled!(Level::Trace) {
                // depth-first search-ish during debugging
                let pos = self.queue.iter().position(|&n| n == node).unwrap();
                self.queue.remove(pos);
            } else {
                return;
            }
        }
        self.set.insert(node);
        self.queue.push_front(node);
        trace!("add {} to worklist", node);
    }
    pub fn next(&mut self) -> Option<Node> {
        let node = self.queue.pop_front()?;
        self.set.remove(&node);
        Some(node)
    }
}
impl FromIterator<Node> for WorkQueue {
    fn from_iter<T: IntoIterator<Item=Node>>(iter: T) -> Self {
        let mut queue = WorkQueue::new();
        for e in iter {
            queue.add_single(e);
        }
        queue
    }
}

impl<'i> Graph<'i> {
    pub fn solve(&mut self, meta_info: &mut MetaInfo) {
        let mut todos: WorkQueue = self.nodes().collect();

        while let Some(node) = todos.next() {
            for (edge_index, constraint, source) in self.incoming(node) {
                trace!("{} -> {}: {:?}", source, node, constraint);
                match constraint {
                    Constraint::Eq => self.unify_assign(&mut todos, node, source),
                    Constraint::Reduce(reduce) => self.reduce(&mut todos, node, &reduce),
                    Constraint::FieldAccess(field) => {
                        self.field_access(&mut todos, edge_index, meta_info, source, node, field);
                    },
                    Constraint::FunctionCallArg(call_index, arg_index) => {
                        self.function_call_arg(&mut todos, edge_index, source, node, call_index, arg_index);
                    }
                    Constraint::FunctionCallReturnType(call_index) => {
                        self.function_call_ret(&mut todos, edge_index, source, node, call_index);
                    }
                    Constraint::Method => {
                        self.method(&mut todos, edge_index, meta_info, source, node);
                    }
                    Constraint::MethodCallArg(call_index, arg_index) => {
                        self.method_call_arg(&mut todos, edge_index, source, node, call_index, arg_index);
                    }
                    Constraint::MethodCallReturnType(call_index) => {
                        self.method_call_ret(&mut todos, edge_index, source, node, call_index);
                    }
                    Constraint::Generic | Constraint::GenericEqSource => (),
                };
                // self.dot();
            }
        }
    }

    fn reduce(&mut self, todos: &mut WorkQueue, node: Node, reduce: &[ResolvableSpecificType]) {
        assert!(!reduce.is_empty());
        self.reduce_internal(todos, node, reduce)
    }

    fn unify_assign(&mut self, todos: &mut WorkQueue, into: Node, from: Node) {
        let from = self.possible_types[&from].clone();
        self.reduce_internal(todos, into, &from.0)
    }

    fn field_access(&mut self, todos: &mut WorkQueue, edge_index: EdgeIndex, meta_info: &MetaInfo, struct_node: Node, field_node: Node, field: String) {
        self.reduce_internal(todos, struct_node, &[ResolvableSpecificType::Struct("struct".to_string(), vec![])]);
        let possible_types = self.possible_types.get_mut(&struct_node).unwrap();
        if possible_types.0.len() != 1 {
            return;
        }
        let (struct_name, struct_generics) = if let ResolvableSpecificType::Struct(name, generics) = &possible_types.0[0] {
            (name, generics.clone())
        } else {
            return;
        };
        if struct_name == "struct" {
            return;
        }

        let function_generics = FunctionGenerics::new();
        for &generic in &struct_generics {
            function_generics.insert_generic(generic);
        }

        let struct_typ = match meta_info.struct_types.get(struct_name.as_str()) {
            Some(struct_typ) => struct_typ,
            // no struct with the given name, don't continue type inference surrounding this type
            None => return,
        };

        for (&expected, &gotten) in struct_typ.generics.iter().zip(&struct_generics) {
            if expected.span_id() != gotten.span_id() {
                let synthetic = Node::synthetic(expected);
                self.add_node(synthetic);
                self.add_generic_constraint(field_node, synthetic);
                self.add_generic_eq_source_constraint(field_node, gotten);
                self.add_generic_eq_source_constraint(field_node, synthetic);
                self.add_eq_constraint(gotten, synthetic);
                function_generics.insert_generic(synthetic);
            }
        }

        let typ = match struct_typ.get_field(&field) {
            Some(typ) => typ,
            // no field with the given name, don't continue type inference surrounding this type
            None => return,
        };

        for generic in struct_generics {
            todos.add_single(generic);
        }
        todos.add(self, struct_node);
        todos.add(self, field_node);

        self.graph.remove_edge(edge_index);
        function_generics.apply_type_reduce(struct_node, struct_node, field_node, typ, self);
    }
    fn get_function_function_generics(&mut self, source_node: Node, call_index: u64) -> Option<(FunctionGenerics, FunctionType)> {
        let possible_types = &mut self.possible_types[&source_node];
        if possible_types.0.len() != 1 {
            return None;
        }
        let fn_typ = match &possible_types.0[0] {
            ResolvableSpecificType::Function(Some(fn_typ)) => fn_typ.clone(),
            ResolvableSpecificType::Function(None) => return None,
            _ => {
                possible_types.0.clear();
                return None;
            }
        };
        self.get_function_generics(source_node, fn_typ, None, call_index)
    }
    fn get_method_function_generics<'a>(&mut self, method_node: Node, call_index: u64) -> Option<(FunctionGenerics, FunctionType)> {
        let possible_types = &self.possible_types[&method_node];
        if possible_types.0.len() != 1 {
            return None;
        }

        let fn_typ = match possible_types.0[0].clone() {
            ResolvableSpecificType::Function(Some(f)) => f,
            t => unreachable!("method node with non-function type {:?}", t),
        };

        // self.get_function_generics(method_node, fn_typ, Some(typ), call_index)
        self.get_function_generics(method_node, fn_typ, None, call_index)
    }
    fn get_function_generics(&mut self, source_node: Node, fn_typ: FunctionType, typ: Option<ResolvableSpecificType>, call_index: u64) -> Option<(FunctionGenerics, FunctionType)> {
        // function generics
        let default_function_generics = FunctionGenerics::new();
        if let Some(typ) = typ {
            for generic_node in typ.generics() {
                default_function_generics.insert_generic(generic_node);
            }
        }
        for &generic_span in &*fn_typ.generics {
            if default_function_generics.contains_generic_span(generic_span) {
                continue;
            }
            let node = Node::synthetic(generic_span);
            self.add_node(node);
            self.add_generic_constraint(source_node, node);
            default_function_generics.insert_generic(node);
        }
        let function_generics = self.method_function_generics.entry(call_index)
            .or_insert(default_function_generics)
            .clone();
        Some((function_generics, fn_typ))
    }
    fn function_call_arg(&mut self, todos: &mut WorkQueue, edge_index: EdgeIndex, source: Node, arg: Node, call_index: u64, arg_index: usize) {
        let (function_generics, fn_typ) = match self.get_function_function_generics(source, call_index) {
            Some(t) => t,
            None => return,
        };
        self.call_arg(todos, edge_index, function_generics, &fn_typ, source, arg, arg_index)
    }
    fn function_call_ret(&mut self, todos: &mut WorkQueue, edge_index: EdgeIndex, source: Node, function_call: Node, call_index: u64) {
        let (function_generics, fn_typ) = match self.get_function_function_generics(source, call_index) {
            Some(t) => t,
            None => return,
        };

        self.graph.remove_edge(edge_index);
        function_generics.apply_type_reduce(function_call, source, function_call, &fn_typ.ret, self);
        todos.add(self, source);
        todos.add(self, function_call);
        for generic in function_generics.generics() {
            todos.add(self, generic);
        }
    }
    #[allow(clippy::too_many_arguments)]
    fn method(&mut self, todos: &mut WorkQueue, edge_index: EdgeIndex, meta_info: &MetaInfo, source_node: Node, method_node: Node) {
        let possible_types = &self.possible_types[&source_node];
        if possible_types.0.len() != 1 {
            return;
        }

        let method_name = self.diagnostics.resolve_span(method_node.span_());
        let typ = possible_types.0[0].clone();
        let type_name = typ.type_name();
        let fn_name = format!("{}::{}", type_name, method_name);
        let fn_typ = match meta_info.function_types.get(fn_name.as_str()) {
            Some(fn_typ) => fn_typ.clone(),
            None => return,
        };
        self.graph.remove_edge(edge_index);
        self.add_reduce_constraint(source_node, method_node, vec![ResolvableSpecificType::Function(Some(fn_typ))]);
        todos.add_single(method_node);
    }
    #[allow(clippy::too_many_arguments)]
    fn method_call_arg(&mut self, todos: &mut WorkQueue, edge_index: EdgeIndex, method_node: Node, arg: Node, call_index: u64, arg_index: usize) {
        let (function_generics, fn_typ) = match self.get_method_function_generics(method_node, call_index) {
            Some(t) => t,
            None => return,
        };
        self.call_arg(todos, edge_index, function_generics, &fn_typ, method_node, arg, arg_index)
    }
    #[allow(clippy::too_many_arguments)]
    fn method_call_ret(&mut self, todos: &mut WorkQueue, edge_index: EdgeIndex, method_node: Node, method_call: Node, call_index: u64) {
        let (function_generics, fn_typ) = match self.get_method_function_generics(method_node, call_index) {
            Some(t) => t,
            None => return,
        };

        self.graph.remove_edge(edge_index);
        function_generics.apply_type_reduce(method_call, method_node, method_call, &fn_typ.ret, self);
        todos.add(self, method_node);
        todos.add(self, method_call);
        for generic in function_generics.generics() {
            todos.add(self, generic);
        }
    }
    #[allow(clippy::too_many_arguments)]
    fn call_arg(&mut self, todos: &mut WorkQueue, edge_index: EdgeIndex, function_generics: FunctionGenerics, fn_typ: &FunctionType, source: Node, arg: Node, arg_index: usize) {
        let expected_arg_type = match fn_typ.args.get(arg_index) {
            Some(typ) => typ,
            None => match fn_typ.args.last() {
                Some(t @ Type::TypedVarargs(_)) => t,
                _ => &Type::UntypedVarargs,
            },
        };

        self.graph.remove_edge(edge_index);
        function_generics.apply_type_reduce(arg, source, arg, expected_arg_type, self);
        todos.add(self, source);
        todos.add(self, arg);
        for generic in function_generics.generics() {
            todos.add(self, generic);
        }
    }
    /// Reduces the given node with the type-array
    fn reduce_internal(&mut self, todos: &mut WorkQueue, node: Node, reduce: &[ResolvableSpecificType]) {
        let possible_types = self.possible_types.get_mut(&node).unwrap();

        if reduce.is_empty() {
            return;
        }

        // sanity check generics
        let reduce_generic = reduce.iter().filter_map(|typ| match typ {
            ResolvableSpecificType::UnUnifyableGeneric(span) => Some(*span),
            _ => None,
        }).next();
        let self_generic = possible_types.0.iter().filter_map(|typ| match typ {
            ResolvableSpecificType::UnUnifyableGeneric(span) => Some(*span),
            _ => None,
        }).next();
        if reduce_generic.is_some() {
            assert_eq!(reduce.len(), 1);
        }
        if self_generic.is_some() {
            assert_eq!(possible_types.0.len(), 1);
        }

        // handle ununifyable generics

        // make sure all branches are covered
        struct Unit;
        let _: Unit = match (self_generic, reduce_generic) {
            (Some(span), Some(span2)) => if span == span2 {
                return;
            } else {
                possible_types.0.clear();
                todos.add(self, node);
                return;
            }
            (Some(_), None) => if reduce.len() == 1 {
                possible_types.0.clear();
                todos.add(self, node);
                return;
            } else {
                return;
            }
            (None, Some(_)) if *possible_types == PossibleTypes::any() => {
                possible_types.0.clear();
                possible_types.0.push(reduce[0].clone());
                todos.add(self, node);
                return;
            }
            (None, Some(_)) if possible_types.len() == 1 => {
                possible_types.0.clear();
                todos.add(self, node);
                return;
            },
            (None, Some(_)) => return,
            (None, None) => Unit,
        };

        // handle rest

        fn function_types_unifyable(f1: &FunctionType, f2: &FunctionType) -> bool {
            let FunctionType { is_method: _, generics: generics1, args: args1, ret: ret1 } = f1;
            let FunctionType { is_method: _, generics: generics2, args: args2, ret: ret2 } = f2;
            let type_equal = |t1: &Type, t2: &Type| {
                let pos1 = |span1| generics1.iter().position(|s| s == span1).unwrap();
                let pos2 = |span2| generics2.iter().position(|s| s == span2).unwrap();
                match (t1, t2) {
                    (Type::Specific(SpecificType::Generic(span1)), Type::Specific(SpecificType::Generic(span2))) => {
                        pos1(span1) == pos2(span2)
                    }
                    (Type::Specific(SpecificType::Function(f1)), Type::Specific(SpecificType::Function(f2))) => function_types_unifyable(&*f1, &*f2),
                    (Type::Specific(SpecificType::Struct(name1, g1)), Type::Specific(SpecificType::Struct(name2, g2)))
                    | (Type::Specific(SpecificType::Enum(name1, g1)), Type::Specific(SpecificType::Enum(name2, g2))) => {
                        name1 == name2 && g1.len() == g2.len()
                            && g1.iter().zip(g2.iter()).all(|((s1,_), (s2,_))| s1 == s2 || pos1(s1) == pos2(s2))
                    }
                    _ => t1 == t2,
                }
            };
            generics1.len() == generics2.len()
                && args1.len() == args2.len()
                && args1.iter().zip(args2.iter()).all(|(t1, t2)| type_equal(t1, t2))
                && type_equal(ret1, ret2)
        }

        let mut reduced = Vec::new();
        for typ in &possible_types.0.clone() {
            match typ {
                ResolvableSpecificType::Unit
                | ResolvableSpecificType::Bool
                | ResolvableSpecificType::Float
                | ResolvableSpecificType::Integer
                | ResolvableSpecificType::String => if reduce.contains(typ) {
                    reduced.push(typ.clone())
                }
                ResolvableSpecificType::Function(f) => {
                    let reduce_function = reduce.iter()
                        .filter_map(|typ| match typ {
                            ResolvableSpecificType::Function(f) => Some(f),
                            _ => None,
                        }).next();

                    match (f, reduce_function) {
                        (f, Some(None)) => reduced.push(ResolvableSpecificType::Function(f.clone())),
                        (None, Some(f)) => reduced.push(ResolvableSpecificType::Function(f.clone())),
                        (Some(f1), Some(Some(f2))) => if function_types_unifyable(f1, f2) {
                            reduced.push(ResolvableSpecificType::Function(Some(f1.clone())));
                        }
                        (_, None) => (),
                    }
                }
                ResolvableSpecificType::Struct(name, generics) => {
                    let reduce_struct = reduce.iter()
                        .filter_map(|typ| match typ {
                            ResolvableSpecificType::Struct(name, generics) => Some((name.as_str(), generics)),
                            _ => None,
                        }).next();
                    match (name.as_str(), generics, reduce_struct) {
                        ("struct", _, Some((name, generics)))
                        | (name, generics, Some(("struct", _))) => reduced.push(ResolvableSpecificType::Struct(name.to_string(), generics.clone())),
                        (a, a_generics, Some((b, b_generics))) if a == b => {
                            for (&ag, &bg) in a_generics.iter().zip(b_generics).filter(|(ag, bg)| ag != bg) {
                                self.add_generic_eq_source_constraint(node, ag);
                                self.add_generic_eq_source_constraint(node, bg);
                                self.add_eq_constraint(ag, bg);
                                todos.add_single(ag);
                                todos.add_single(bg);
                            }
                            reduced.push(ResolvableSpecificType::Struct(a.to_string(), a_generics.clone()));
                        },
                        (_, _, _) => (),
                    }
                }
                ResolvableSpecificType::Enum(name, generics) => {
                    let reduce_enum = reduce.iter()
                        .filter_map(|typ| match typ {
                            ResolvableSpecificType::Enum(name, generics) => Some((name.as_str(), generics)),
                            _ => None,
                        }).next();
                    match (name.as_str(), generics, reduce_enum) {
                        ("enum", _, Some((name, generics)))
                        | (name, generics, Some(("enum", _))) => reduced.push(ResolvableSpecificType::Enum(name.to_string(), generics.clone())),
                        (a, a_generics, Some((b, b_generics))) if a == b => {
                            for (&ag, &bg) in a_generics.iter().zip(b_generics).filter(|(ag, bg)| ag != bg) {
                                self.add_generic_eq_source_constraint(node, ag);
                                self.add_generic_eq_source_constraint(node, bg);
                                self.add_eq_constraint(ag, bg);
                                todos.add_single(ag);
                                todos.add_single(bg);
                            }
                            reduced.push(ResolvableSpecificType::Enum(a.to_string(), a_generics.clone()));
                        },
                        (_, _, _) => (),
                    }
                }
                ResolvableSpecificType::UnUnifyableGeneric(_) => unreachable!("handled above"),
            }
        }
        // fetch again for the borrow checker
        let possible_types = self.possible_types.get_mut(&node).unwrap();
        let old = std::mem::replace(&mut possible_types.0, reduced);
        if old != possible_types.0 {
            todos.add(self, node);
        }
    }
}
