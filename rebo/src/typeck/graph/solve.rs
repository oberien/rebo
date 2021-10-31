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
    pub fn add(&mut self, node: Node) {
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
            queue.add(e);
        }
        queue
    }
}

impl<'i> Graph<'i> {
    pub fn solve(graph: &mut Graph, meta_info: &mut MetaInfo) {
        let mut todos: WorkQueue = graph.nodes().collect();

        while let Some(var) = todos.next() {
            let mut changed = false;
            for (constraint, source) in graph.incoming(var) {
                let unify_result = match constraint {
                    Constraint::Eq => graph.unify_assign(var, source),
                    Constraint::Reduce(reduce) => graph.reduce(var, &reduce),
                    Constraint::FieldAccess(fields) => {
                        graph.field_access(meta_info, source, var, &fields)
                    },
                    Constraint::MethodCallArg(name, arg_index) => {
                        graph.method_call_arg(meta_info, source, var, &name, arg_index)
                    }
                    Constraint::MethodCallReturnType(name) => {
                        graph.method_call_ret(meta_info, source, var, &name)
                    }
                };
                match unify_result {
                    UnifyResult::Changed => {
                        todos.add(source);
                        changed = true;
                    },
                    UnifyResult::Unchanged => (),
                    UnifyResult::Multiple(to_add) => for to_add in to_add {
                        todos.add(to_add);
                        changed = true;
                    }
                }
            }
            if changed {
                todos.add(var);
                for todo in graph.outgoing_neighbors(var) {
                    todos.add(todo);
                }
            }
        }
    }

    fn reduce(&mut self, var: TypeVar, reduce: &[ResolvableSpecificType]) -> UnifyResult {
        assert!(!reduce.is_empty());
        self.reduce_internal(var, reduce)
    }

    fn unify_assign(&mut self, into: TypeVar, from: TypeVar) -> UnifyResult {
        // TODO: is there really no hashmap allowing mutable access to two values at the same time?
        let from = self.possible_types[&from].clone();
        self.reduce_internal(into, &from.0)
    }

    fn field_access(&mut self, meta_info: &MetaInfo, struct_var: TypeVar, field_var: TypeVar, fields: &[String]) -> UnifyResult {
        let res = self.reduce_internal(struct_var, &[ResolvableSpecificType::Struct("struct".to_string())]);
        let possible_types = self.possible_types.get_mut(&struct_var).unwrap();
        if possible_types.0.len() != 1 {
            return res;
        }
        let struct_name = if let ResolvableSpecificType::Struct(name) = &possible_types.0[0] {
            name
        } else {
            return res;
        };
        if struct_name == "struct" {
            return res;
        }

        let mut typ = possible_types.0[0].clone();
        for field in fields {
            let struct_typ = match typ {
                ResolvableSpecificType::Struct(name) => match meta_info.struct_types.get(name.as_str()) {
                    Some(struct_typ) => struct_typ,
                    None => return res,
                }
                _ => return res,
            };
            typ = match struct_typ.get_field(field) {
                Some(Type::Specific(typ)) => self.make_specific_resolvable(&typ, field_var),
                _ => return res,
            }
        }
        res | self.reduce_replace_edge(struct_var, field_var, typ)
    }
    fn method_call_arg(&mut self, meta_info: &MetaInfo, field_access: TypeVar, arg: TypeVar, method_name: &str, arg_index: usize) -> UnifyResult {
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
        let expected_arg_type = match fn_typ.args.get(arg_index) {
            Some(typ) => self.make_type_resolvable(typ, arg),
            None => ResolvableType::Varargs,
        };
        match expected_arg_type {
            ResolvableType::Top | ResolvableType::Varargs => UnifyResult::Unchanged,
            ResolvableType::Bottom => unreachable!("fn arg is bottom"),
            ResolvableType::Specific(specific) => {
                self.reduce_replace_edge(field_access, arg, specific)
            }
        }
    }
    fn method_call_ret(&mut self, meta_info: &MetaInfo, field_access: TypeVar, method_call: TypeVar, method_name: &str) -> UnifyResult {
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
        match self.make_type_resolvable(&fn_typ.ret, method_call) {
            ResolvableType::Top | ResolvableType::Varargs => unreachable!("fn ret is Top or Varargs"),
            ResolvableType::Bottom => UnifyResult::Unchanged,
            ResolvableType::Specific(specific) => {
                self.reduce_replace_edge(field_access, method_call, specific)
            }
        }
    }
    fn reduce_replace_edge(&mut self, from: TypeVar, to: TypeVar, typ: ResolvableSpecificType) -> UnifyResult {
        let mut edges = self.graph.edges_connecting(self.graph_indices[&from], self.graph_indices[&to]);
        let edge_idx = edges.next().unwrap().id();
        assert!(edges.next().is_none(), "reduce_replace_edge called with two nodes with multiple edges");
        let reduce = vec![typ];
        let res = self.reduce_internal(to, &reduce);
        *self.graph.edge_weight_mut(edge_idx).unwrap() = Constraint::Reduce(reduce);
        res | UnifyResult::Changed
    }
    fn reduce_internal(&mut self, var: TypeVar, reduce: &[ResolvableSpecificType]) -> UnifyResult {
        let possible_types = self.possible_types.get_mut(&var).unwrap();

        if reduce.is_empty() {
            return UnifyResult::Unchanged;
        }

        // check generics
        let reduce_generic = reduce.iter().filter_map(|typ| match typ {
            ResolvableSpecificType::UnUnifyableGeneric(span2, depth2) => Some((*span2, *depth2)),
            _ => None,
        }).next();
        let self_generic = self.0.iter().filter_map(|typ| match typ {
            ResolvableSpecificType::UnUnifyableGeneric(span2, depth2) => Some((*span2, *depth2)),
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
            (Some((span, depth)), Some((span2, depth2))) => if span == span2 && depth == depth2 {
                return UnifyResult::Unchanged;
            } else {
                self.0.clear();
                return UnifyResult::Changed;
            }
            (Some((_, _)), None) => if reduce.len() == 1 {
                self.0.clear();
                return UnifyResult::Changed;
            } else {
                return UnifyResult::Unchanged;
            }
            (None, Some(_)) if *self == PossibleTypes::any() => {
                self.0.clear();
                self.0.push(reduce[0].clone());
                return UnifyResult::Changed;
            }
            (None, Some(_)) if self.len() == 1 => {
                self.0.clear();
                return UnifyResult::Changed;
            },
            (None, Some(_)) => return UnifyResult::Unchanged,
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
                ResolvableSpecificType::Struct(name) => {
                    let reduce_struct = reduce.iter()
                        .filter_map(|typ| match typ {
                            ResolvableSpecificType::Struct(name) => Some(name.as_str()),
                            _ => None,
                        }).next();
                    match (name.as_str(), reduce_struct) {
                        ("struct", Some(name))
                        | (name, Some("struct")) => reduced.push(ResolvableSpecificType::Struct(name.to_string())),
                        (a, Some(b)) if a == b => reduced.push(ResolvableSpecificType::Struct(a.to_string())),
                        (_, _) => (),
                    }
                }
                ResolvableSpecificType::Enum(name) => {
                    let reduce_enum = reduce.iter()
                        .filter_map(|typ| match typ {
                            ResolvableSpecificType::Enum(name) => Some(name.as_str()),
                            _ => None,
                        }).next();
                    match (name.as_str(), reduce_enum) {
                        ("enum", Some(name))
                        | (name, Some("enum")) => reduced.push(ResolvableSpecificType::Enum(name.to_string())),
                        (a, Some(b)) if a == b => reduced.push(ResolvableSpecificType::Enum(a.to_string())),
                        (_, _) => (),
                    }
                }
                ResolvableSpecificType::UnUnifyableGeneric(_, _) => unreachable!("handled above"),
            }
        }
        let old = std::mem::replace(&mut possible_types.0, reduced);
        match old == possible_types.0 {
            true => return UnifyResult::Unchanged,
            false => return UnifyResult::Changed,
        }

        match reduced {
            ReduceResult::Unchanged => UnifyResult::Unchanged,
            ReduceResult::Changed => UnifyResult::Changed,
            ReduceResult::GenericSolved(span, depth, typ) => {
                trace!("generic solved: {:?} = {}", span, typ);
                // iterate over all reachable fields and edges and exchange the generic with the specific ResolvableType
                let visited_nodes = self.search_from(
                    var,
                    |this, var| {
                        this.resolved_generics.insert((var, span, depth), typ.clone());
                        let possible_types = &mut this.possible_types[&var];
                        if possible_types.len() == 1 {
                            if let ResolvableSpecificType::UnifyableGeneric(span2, depth2) = possible_types.0[0] {
                                if span2 == span && depth == depth2 {
                                    possible_types.0[0] = typ.clone();
                                }
                            }
                        }
                    },
                    |this, edge_index| {
                        let constraint = this.graph.edge_weight_mut(edge_index).unwrap();
                        match constraint {
                            Constraint::Eq
                            | Constraint::FieldAccess(_)
                            | Constraint::MethodCallArg(..)
                            | Constraint::MethodCallReturnType(_) => (),
                            Constraint::Reduce(reduce) => if reduce.len() == 1 {
                                if let ResolvableSpecificType::UnifyableGeneric(span2, depth2) = reduce[0] {
                                    if span == span2 && depth == depth2 {
                                        reduce[0] = typ.clone();
                                    }
                                }
                            }
                        }
                    }
                );

                if Level::Trace <= log::max_level() {
                    self.dot();
                }
                UnifyResult::Multiple(visited_nodes)
            },
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
