use crate::typeck::graph::{Graph, Constraint, Node, PossibleTypes};
use crate::common::MetaInfo;
use std::collections::{VecDeque, HashSet};
use std::iter::FromIterator;
use crate::typeck::types::{ResolvableSpecificType, Type};
use petgraph::prelude::EdgeRef;
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
            return;
        }
        self.set.insert(node);
        self.queue.push_back(node);
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
            for (constraint, source) in self.incoming(node) {
                trace!("{} -> {}: {:?}", source, node, constraint);
                match constraint {
                    Constraint::Eq => self.unify_assign(&mut todos, node, source),
                    Constraint::Reduce(reduce) => self.reduce(&mut todos, node, &reduce),
                    Constraint::FieldAccess(field) => {
                        self.field_access(&mut todos, meta_info, source, node, field);
                    },
                    Constraint::MethodCallArg(name, method_call_index, arg_index) => {
                        self.method_call_arg(&mut todos, meta_info, source, node, &name, method_call_index, arg_index);
                    }
                    Constraint::MethodCallReturnType(name, method_call_index) => {
                        self.method_call_ret(&mut todos, meta_info, source, node, &name, method_call_index);
                    }
                    Constraint::Generic => (),
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

    fn field_access(&mut self, todos: &mut WorkQueue, meta_info: &MetaInfo, struct_node: Node, field_node: Node, field: String) {
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
            if expected != gotten.span() {
                let synthetic = Node::synthetic(expected);
                self.add_node(synthetic);
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

        self.remove_single_edge(struct_node, field_node);
        function_generics.apply_type_reduce(struct_node, field_node, &typ, self);
    }
    fn method_call_arg(&mut self, todos: &mut WorkQueue, meta_info: &MetaInfo, field_access: Node, arg: Node, method_name: &str, method_call_index: u64, arg_index: usize) {
        let possible_types = &self.possible_types[&field_access];
        if possible_types.0.len() != 1 {
            return;
        }
        let type_name = possible_types.0[0].type_name();
        let fn_name = format!("{}::{}", type_name, method_name);
        let fn_typ = match meta_info.function_types.get(fn_name.as_str()) {
            Some(fn_typ) => fn_typ,
            None => return,
        };

        // function generics
        let default_function_generics = FunctionGenerics::new();
        for &generic_span in &*fn_typ.generics {
            let node = Node::synthetic(generic_span);
            self.add_node(node);
            default_function_generics.insert_generic(node);
        }
        let function_generics = self.method_function_generics.entry(method_call_index)
            .or_insert(default_function_generics)
            .clone();
        let expected_arg_type = match fn_typ.args.get(arg_index) {
            Some(typ) => typ,
            None => &Type::Varargs,
        };

        self.remove_single_edge(field_access, arg);
        function_generics.apply_type_reduce(field_access, arg, expected_arg_type, self);
        todos.add(self, field_access);
        todos.add(self, arg);
        for generic in function_generics.generics() {
            todos.add(self, generic);
        }
    }
    fn method_call_ret(&mut self, todos: &mut WorkQueue, meta_info: &MetaInfo, field_access: Node, method_call: Node, method_name: &str, method_call_index: u64) {
        let possible_types = &self.possible_types[&field_access];
        if possible_types.0.len() != 1 {
            return;
        }
        let type_name = possible_types.0[0].type_name();
        let fn_name = format!("{}::{}", type_name, method_name);
        let fn_typ = match meta_info.function_types.get(fn_name.as_str()) {
            Some(fn_typ) => fn_typ,
            None => return,
        };

        // function generics
        let default_function_generics = FunctionGenerics::new();
        for &generic_span in &*fn_typ.generics {
            let node = Node::synthetic(generic_span);
            self.add_node(node);
            default_function_generics.insert_generic(node);
        }
        let function_generics = self.method_function_generics.entry(method_call_index)
            .or_insert(default_function_generics)
            .clone();

        self.remove_single_edge(field_access, method_call);
        function_generics.apply_type_reduce(field_access, method_call, &fn_typ.ret, self);
        todos.add(self, field_access);
        todos.add(self, method_call);
        for generic in function_generics.generics() {
            todos.add(self, generic);
        }
    }
    fn remove_single_edge(&mut self, from: Node, to: Node) {
        let mut edges = self.graph.edges_connecting(self.graph_indices[&from], self.graph_indices[&to]);
        let edge_idx = edges.next().unwrap().id();
        assert!(edges.next().is_none(), "remove_single_edge called with two nodes with multiple edges");
        self.graph.remove_edge(edge_idx);
    }
    /// Reduces the given node with the type-array
    fn reduce_internal(&mut self, todos: &mut WorkQueue, node: Node, reduce: &[ResolvableSpecificType]) {
        let possible_types = self.possible_types.get_mut(&node).unwrap();

        if reduce.is_empty() {
            return;
        }

        // check generics
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

        // handle generics
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
