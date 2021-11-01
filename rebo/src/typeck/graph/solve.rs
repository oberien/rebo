use crate::typeck::graph::{Graph, Constraint, UnifyResult, Node, PossibleTypes};
use crate::common::MetaInfo;
use std::collections::{VecDeque, HashSet};
use crate::typeck::TypeVar;
use std::iter::FromIterator;
use crate::typeck::types::{ResolvableSpecificType, Type, ResolvableType, SpecificType};
use log::Level;
use petgraph::graph::EdgeIndex;
use petgraph::Direction;
use petgraph::prelude::EdgeRef;
use std::cell::RefCell;
use std::ops::BitOr;
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
            break;
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
            queue.add(e);
        }
        queue
    }
}

impl<'i> Graph<'i> {
    pub fn solve(&mut self, meta_info: &mut MetaInfo) {
        let mut todos: WorkQueue = self.nodes().collect();

        while let Some(node) = todos.next() {
            for (constraint, source) in self.incoming(node) {
                match constraint {
                    Constraint::Eq => self.unify_assign(&mut todos, node, source),
                    Constraint::Reduce(reduce) => self.reduce(&mut todos, node, &reduce),
                    Constraint::FieldAccess(fields) => {
                        self.field_access(&mut todos, meta_info, source, node, &fields);
                    },
                    Constraint::MethodCallArg(name, method_call_index, arg_index) => {
                        self.method_call_arg(&mut todos, meta_info, source, node, &name, method_call_index, arg_index);
                    }
                    Constraint::MethodCallReturnType(name, method_call_index) => {
                        self.method_call_ret(&mut todos, meta_info, source, node, &name, method_call_index);
                    }
                    Constraint::Generic => (),
                };
            }
        }
    }

    fn reduce(&mut self, todos: &mut WorkQueue, node: Node, reduce: &[ResolvableSpecificType]) -> UnifyResult {
        assert!(!reduce.is_empty());
        self.reduce_internal(todos, node, reduce)
    }

    fn unify_assign(&mut self, todos: &mut WorkQueue, into: Node, from: Node) -> UnifyResult {
        let from = self.possible_types[&from].clone();
        self.reduce_internal(todos, into, &from.0)
    }

    fn field_access(&mut self, todos: &mut WorkQueue, meta_info: &MetaInfo, struct_node: Node, field_node: Node, fields: &[String]) {
        self.reduce_internal(todos, struct_node, &[ResolvableSpecificType::Struct("struct".to_string())]);
        let possible_types = self.possible_types.get_mut(&struct_node).unwrap();
        if possible_types.0.len() != 1 {
            return;
        }
        let (struct_name, struct_generics) = if let ResolvableSpecificType::Struct(name, generics) = &possible_types.0[0] {
            (name, generics)
        } else {
            return;
        };
        if struct_name == "struct" {
            return;
        }

        let mut function_generics = FunctionGenerics::new();
        for &generic in struct_generics {
            function_generics.insert_generic(generic);
        }

        let mut typ = possible_types.0[0].clone();
        for field in fields {
            let struct_typ = match typ {
                ResolvableSpecificType::Struct(name, _) => match meta_info.struct_types.get(name.as_str()) {
                    Some(struct_typ) => struct_typ,
                    None => return res,
                }
                _ => return res,
            };
            typ = match struct_typ.get_field(field) {
                Some(Type::Specific(typ)) => function_generics.apply_specific_type_reduce(&typ, field_node),
                _ => return res,
            }
        }
        self.remove_single_edge(struct_node, field_node);
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
        let function_generics = self.method_function_generics.entry(method_call_index)
            .or_insert_with(|| {
                let mut function_generics = FunctionGenerics::new();
                for &generic_span in &*fun_typ.generics {
                    let node = Node::synthetic(generic_span);
                    self.add_node(node);
                    function_generics.insert_generic(node);
                }
                function_generics
            });
        let expected_arg_type = match fn_typ.args.get(arg_index) {
            Some(typ) => typ,
            None => Type::Varargs,
        };

        self.remove_single_edge(field_access, arg);
        assert!(self.graph.edges_connecting(self.graph_indices[&field_access], self.graph_indices[&arg]).count() == 0);
        function_generics.apply_type_reduce(field_access, arg, expected_arg_type, self);
        assert!(self.graph.edges_connecting(self.graph_indices[&field_access], self.graph_indices[&arg]).count() > 0);
        todos.add(self, field_access);
        todos.add(self, arg);
        for generic in function_generics.generics() {
            todos.add(self, generic);
        }
    }
    fn method_call_ret(&mut self, todos: &mut WorkQueue, meta_info: &MetaInfo, field_access: Node, method_call: Node, method_name: &str, method_call_index: u64) {
        let possible_types = &self.possible_types[&field_access];
        if possible_types.0.len() != 1 {
            return UnifyResult::Unchanged;
        }
        let type_name = possible_types.0[0].type_name();
        let fn_name = format!("{}::{}", type_name, method_name);
        let fn_typ = match meta_info.function_types.get(fn_name.as_str()) {
            Some(fn_typ) => fn_typ,
            None => return UnifyResult::Unchanged,
        };
        let function_generics = self.method_function_generics.entry(method_call_index)
            .or_insert_with(|| {
                let mut function_generics = FunctionGenerics::new();
                for &generic_span in &*fun_typ.generics {
                    let node = Node::synthetic(generic_span);
                    self.add_node(node);
                    function_generics.insert_generic(node);
                }
                function_generics
            });

        self.remove_single_edge(field_access, method_call);
        assert!(self.graph.edges_connecting(self.graph_indices[&field_access], self.graph_indices[&method_call]).count() == 0);
        function_generics.apply_type_reduce(field_access, method_call, &fn_typ.ret, self);
        assert!(self.graph.edges_connecting(self.graph_indices[&field_access], self.graph_indices[&method_call]).count() > 0);
        todos.add(self, field_access);
        todos.add(self, method_call);
        for generic in function_generics.generics() {
            todos.add(selfge, neric);
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
        let self_generic = self.0.iter().filter_map(|typ| match typ {
            ResolvableSpecificType::UnUnifyableGeneric(span) => Some(*span),
            _ => None,
        }).next();
        if reduce_generic.is_some() {
            assert_eq!(reduce.len(), 1);
        }
        if self_generic.is_some() {
            assert_eq!(self.0.len(), 1);
        }

        // handle generics
        // make sure all branches are covered
        struct Unit;
        let _: Unit = match (self_generic, reduce_generic) {
            (Some(span), Some(span2)) => if span == span2 {
                return;
            } else {
                self.0.clear();
                todos.add(self, node);
                return;
            }
            (Some(_), None) => if reduce.len() == 1 {
                self.0.clear();
                todos.add(self, node);
                return;
            } else {
                return;
            }
            (None, Some(_)) if *self == PossibleTypes::any() => {
                self.0.clear();
                self.0.push(reduce[0].clone());
                todos.add(self, node);
                return;
            }
            (None, Some(_)) if self.len() == 1 => {
                self.0.clear();
                todos.add(self, node);
                return;
            },
            (None, Some(_)) => return,
            (None, None) => Unit,
        };

        // handle rest
        let mut reduced = Vec::new();
        for typ in &possible_types.0 {
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
                        | (name, generics, Some("struct")) => reduced.push(ResolvableSpecificType::Struct(name.to_string(), generics)),
                        (a, a_generics, Some((b, b_generics))) if a == b => {
                            for (ag, bg) in a_generics.iter().zip(b_generics) {
                                self.add_eq_constraint(ag, bg);
                                todos.add_single(ag);
                                todos.add_single(bg);
                            }
                            reduced.push(ResolvableSpecificType::Struct(a.to_string(), a_generics));
                        },
                        (_, _) => (),
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
                        | (name, generics, Some("enum")) => reduced.push(ResolvableSpecificType::Enum(name.to_string())),
                        (a, a_generics, Some((b, b_generics))) if a == b => {
                            for (ag, bg) in a_generics.iter().zip(b_generics) {
                                self.add_eq_constraint(ag, bg);
                                todos.add_single(ag);
                                todos.add_single(bg);
                            }
                            reduced.push(ResolvableSpecificType::Enum(a.to_string(), a_generics));
                        },
                        (_, _) => (),
                    }
                }
                ResolvableSpecificType::UnUnifyableGeneric(_) => unreachable!("handled above"),
            }
        }
        let old = std::mem::replace(&mut possible_types.0, reduced);
        if old == possible_types.0 {
            todos.add(self, node);
        }
    }

    fn make_type_resolvable(&mut self, typ: &Type, from: TypeVar) -> ResolvableType {
        match typ {
            Type::Top => ResolvableType::Top,
            Type::Bottom => ResolvableType::Bottom,
            Type::Varargs => ResolvableType::Varargs,
            Type::Specific(specific) => ResolvableType::Specific(self.make_specific_resolvable(specific, from)),
        }
    }
    fn make_specific_resolvable(&mut self, typ: &SpecificType, from: TypeVar) -> ResolvableSpecificType {
        match typ {
            SpecificType::Unit => ResolvableSpecificType::Unit,
            SpecificType::Bool => ResolvableSpecificType::Bool,
            SpecificType::Integer => ResolvableSpecificType::Integer,
            SpecificType::Float => ResolvableSpecificType::Float,
            SpecificType::String => ResolvableSpecificType::String,
            SpecificType::Struct(name) => ResolvableSpecificType::Struct(name.clone()),
            SpecificType::Enum(name) => ResolvableSpecificType::Enum(name.clone()),
            &SpecificType::Generic(span, depth) => {
                if let Some(typ) = self.resolved_generics.get(&(from, span, depth)) {
                    return typ.clone();
                }
                let is_un_unifyable = RefCell::new(false);
                self.search_from(
                    from,
                    |this, var| {
                        for typ in &this.possible_types[&var].0 {
                            if let &ResolvableSpecificType::UnUnifyableGeneric(span2, depth2) = typ {
                                if span == span2 && depth == depth2 {
                                    *is_un_unifyable.borrow_mut() = true;
                                }
                            }
                        }
                    },
                    |this, edge_index| {
                        match &this.graph[edge_index] {
                            Constraint::Reduce(types) => for typ in types {
                                if let &ResolvableSpecificType::UnUnifyableGeneric(span2, depth2) = typ {
                                    if span == span2 && depth == depth2 {
                                        *is_un_unifyable.borrow_mut() = true;
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                );
                if *is_un_unifyable.borrow() {
                    ResolvableSpecificType::UnUnifyableGeneric(span, depth)
                } else {
                    ResolvableSpecificType::UnifyableGeneric(span, depth)
                }
            }
        }
    }

    /// Breadth-first search over all nodes and edges (some edges are visited multiple times), returning a list of all visited nodes
    fn search_from(&mut self, var: TypeVar, mut node_visitor: impl FnMut(&mut Self, TypeVar), mut edge_visitor: impl FnMut(&mut Self, EdgeIndex)) -> Vec<TypeVar> {
        let mut visited = HashSet::new();
        let mut todo = VecDeque::new();
        todo.push_back(var);

        while let Some(var) = todo.pop_front() {
            if visited.contains(&var) {
                continue;
            }
            visited.insert(var);
            // visit node
            node_visitor(self, var);

            // visit edges
            let node_index = self.graph_indices[&var];
            let edges_outgoing = self.graph.edges_directed(node_index, Direction::Outgoing)
                .map(|edge_ref| (edge_ref.id(), edge_ref.target()));
            let edges_incoming = self.graph.edges_directed(node_index, Direction::Incoming)
                .map(|edge_ref| (edge_ref.id(), edge_ref.source()));
            let edge_indexes: Vec<_> = edges_outgoing
                .chain(edges_incoming)
                .collect();

            for (edge_index, target) in edge_indexes {
                // add target node to todos
                todo.push_back(*self.graph.node_weight(target).unwrap());

                // change edge
                edge_visitor(self, edge_index);
            }
        }
        visited.into_iter().collect()
    }
}
