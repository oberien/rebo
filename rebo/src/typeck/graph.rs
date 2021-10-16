use petgraph::prelude::{DiGraph, NodeIndex, EdgeRef};
use crate::typeck::TypeVar;
use crate::common::MetaInfo;
use crate::typeck::types::{ResolvableSpecificType, ResolvableSpecificTypeDiscriminants, ResolvableType, Type, SpecificType};
use strum::IntoEnumIterator;
use std::process::{Command, Stdio};
use petgraph::dot::{Dot, Config};
use std::fmt::{Display, Formatter};
use std::io::Write;
use itertools::Itertools;
use diagnostic::{Diagnostics, Span};
use petgraph::graph::{EdgeReference, EdgeIndex};
use petgraph::Direction;
use indexmap::map::IndexMap;
use std::collections::{HashSet, VecDeque};
use log::Level;
use std::cell::RefCell;
use std::ops::BitOr;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PossibleTypes(Vec<ResolvableSpecificType>);

impl PartialEq<[ResolvableSpecificType]> for PossibleTypes {
    fn eq(&self, other: &[ResolvableSpecificType]) -> bool {
        self.0 == other
    }
}
impl PartialEq<PossibleTypes> for &[ResolvableSpecificType] {
    fn eq(&self, other: &PossibleTypes) -> bool {
        *self == other.0
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum UnifyResult {
    Changed,
    Unchanged,
    Multiple(Vec<TypeVar>),
}
impl BitOr<UnifyResult> for UnifyResult {
    type Output = UnifyResult;

    fn bitor(self, rhs: UnifyResult) -> UnifyResult {
        match (self, rhs) {
            (UnifyResult::Multiple(vec), _)
            | (_, UnifyResult::Multiple(vec)) => UnifyResult::Multiple(vec),
            (UnifyResult::Changed, _)
            | (_, UnifyResult::Changed) => UnifyResult::Changed,
            (UnifyResult::Unchanged, UnifyResult::Unchanged) => UnifyResult::Unchanged,
        }
    }
}

enum ReduceResult {
    Changed,
    Unchanged,
    GenericSolved(Span, ResolvableSpecificType),
}

impl Default for PossibleTypes {
    fn default() -> Self {
        Self::any()
    }
}

impl Display for PossibleTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0.iter().join(", "))
    }
}

enum Generic {
    UnUnifyable(Span),
    Unifyable(Span),
}

impl PossibleTypes {
    pub fn any() -> PossibleTypes {
        let mut possible_types = Vec::new();
        // make sure we have every ResolvableType covered
        for kind in ResolvableSpecificTypeDiscriminants::iter() {
            match kind {
                ResolvableSpecificTypeDiscriminants::Unit => possible_types.push(ResolvableSpecificType::Unit),
                ResolvableSpecificTypeDiscriminants::Integer => possible_types.push(ResolvableSpecificType::Integer),
                ResolvableSpecificTypeDiscriminants::Float => possible_types.push(ResolvableSpecificType::Float),
                ResolvableSpecificTypeDiscriminants::Bool => possible_types.push(ResolvableSpecificType::Bool),
                ResolvableSpecificTypeDiscriminants::String => possible_types.push(ResolvableSpecificType::String),
                // There can never be a struct named "struct" as it's a keyword.
                // That's why we use it meaning "any struct".
                ResolvableSpecificTypeDiscriminants::Struct => possible_types.push(ResolvableSpecificType::Struct("struct".to_string())),
                // Similarly to above, we use "enum" as any enum.
                ResolvableSpecificTypeDiscriminants::Enum => possible_types.push(ResolvableSpecificType::Enum("enum".to_string())),
                ResolvableSpecificTypeDiscriminants::UnUnifyableGeneric => (),
                ResolvableSpecificTypeDiscriminants::UnifyableGeneric => (),
            }
        }
        PossibleTypes(possible_types)
    }
    fn len(&self) -> usize {
        self.0.len()
    }

    fn set_exact_type(&mut self, typ: ResolvableSpecificType) {
        self.reduce(&[typ.clone()]);
        assert_eq!(self.len(), 1);
        // self.0.clear();
        // self.0.push(typ);
    }

    fn reduce(&mut self, reduce: &[ResolvableSpecificType]) -> ReduceResult {
        if reduce.is_empty() {
            return ReduceResult::Unchanged;
        }

        // check generics
        fn generics<'b>(iter: impl IntoIterator<Item = &'b ResolvableSpecificType> + Clone) -> Option<Generic> {
            let generic = iter.clone().into_iter()
                .filter_map(|typ| match typ {
                    ResolvableSpecificType::UnifyableGeneric(span2) => Some(Generic::Unifyable(*span2)),
                    _ => None,
                }).next();
            let un_generic = iter.into_iter()
                .filter_map(|typ| match typ {
                    ResolvableSpecificType::UnUnifyableGeneric(span2) => Some(Generic::UnUnifyable(*span2)),
                    _ => None,
                }).next();
            assert!(!(generic.is_some() && un_generic.is_some()));
            generic.or(un_generic)
        }

        let reduce_generic = generics(reduce);
        let self_generic = generics(&self.0);
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
            (Some(Generic::UnUnifyable(span)), Some(Generic::UnUnifyable(span2))) => if span == span2 {
                return ReduceResult::Unchanged;
            } else {
                self.0.clear();
                return ReduceResult::Changed;
            }
            (Some(Generic::UnUnifyable(_)), Some(Generic::Unifyable(_))) => return ReduceResult::Unchanged,
            (Some(Generic::Unifyable(_)), Some(Generic::UnUnifyable(_))) => return ReduceResult::Unchanged,
            (Some(Generic::Unifyable(_)), Some(Generic::Unifyable(_))) => return ReduceResult::Unchanged,
            (Some(Generic::UnUnifyable(_)), None) => if reduce.len() == 1 {
                self.0.clear();
                return ReduceResult::Changed;
            } else {
                return ReduceResult::Unchanged;
            }
            (Some(Generic::Unifyable(span)), None) => if reduce.len() == 1 {
                self.0.clear();
                self.0.push(reduce[0].clone());
                return ReduceResult::GenericSolved(span, reduce[0].clone());
            } else {
                return ReduceResult::Unchanged;
            }
            (None, Some(_)) if *self == PossibleTypes::any() => {
                self.0.clear();
                self.0.push(reduce[0].clone());
                return ReduceResult::Changed;
            }
            (None, Some(Generic::Unifyable(span))) if self.len() == 1 => return ReduceResult::GenericSolved(span, self.0[0].clone()),
            (None, Some(Generic::UnUnifyable(_))) if self.len() == 1 => {
                self.0.clear();
                return ReduceResult::Changed;
            },
            (None, Some(_)) => return ReduceResult::Unchanged,
            (None, None) => Unit,
        };

        // handle rest
        let mut res = Vec::new();
        for typ in &self.0 {
            match typ {
                ResolvableSpecificType::Unit
                | ResolvableSpecificType::Bool
                | ResolvableSpecificType::Float
                | ResolvableSpecificType::Integer
                | ResolvableSpecificType::String => if reduce.contains(typ) {
                    res.push(typ.clone())
                }
                ResolvableSpecificType::Struct(name) => {
                    let reduce_struct = reduce.iter()
                        .filter_map(|typ| match typ {
                            ResolvableSpecificType::Struct(name) => Some(name.as_str()),
                            _ => None,
                        }).next();
                    match (name.as_str(), reduce_struct) {
                        ("struct", Some(name))
                        | (name, Some("struct")) => res.push(ResolvableSpecificType::Struct(name.to_string())),
                        (a, Some(b)) if a == b => res.push(ResolvableSpecificType::Struct(a.to_string())),
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
                        | (name, Some("enum")) => res.push(ResolvableSpecificType::Enum(name.to_string())),
                        (a, Some(b)) if a == b => res.push(ResolvableSpecificType::Enum(a.to_string())),
                        (_, _) => (),
                    }
                }
                ResolvableSpecificType::UnUnifyableGeneric(_)
                | ResolvableSpecificType::UnifyableGeneric(_) => unreachable!("handled above"),
            }
        }
        let old = std::mem::replace(&mut self.0, res);
        match old == self.0 {
            true => ReduceResult::Unchanged,
            false => ReduceResult::Changed,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Eq,
    Reduce(Vec<ResolvableSpecificType>),
    /// field names of access path
    FieldAccess(Vec<String>),
    /// name of the method (not fully qualified yet), arg-index
    MethodCallArg(String, usize),
    /// name of the method (not fully qualified yet)
    MethodCallReturnType(String),
}
impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Eq => write!(f, "="),
            Constraint::Reduce(reduce) => {
                write!(f, "[{}]", reduce.iter().join(","))
            }
            Constraint::FieldAccess(fields) => {
                write!(f, ".{}", fields.join("."))
            }
            Constraint::MethodCallArg(name, arg) => write!(f, "{}({})", name, arg),
            Constraint::MethodCallReturnType(name) => write!(f, "{}(...) -> ret", name),
        }
    }
}

pub struct Graph<'i> {
    diagnostics: &'i Diagnostics,
    graph: DiGraph<TypeVar, Constraint>,
    graph_indices: IndexMap<TypeVar, NodeIndex<u32>>,
    possible_types: IndexMap<TypeVar, PossibleTypes>,
    resolved_generics: IndexMap<(TypeVar, Span), ResolvableSpecificType>,
}

impl<'i> Graph<'i> {
    pub fn new(diagnostics: &'i Diagnostics) -> Graph {
        Graph {
            diagnostics,
            graph: DiGraph::new(),
            graph_indices: IndexMap::new(),
            possible_types: IndexMap::new(),
            resolved_generics: IndexMap::new(),
        }
    }

    pub fn add_type_var(&mut self, var: TypeVar) {
        if self.graph_indices.contains_key(&var) {
            return;
        }
        let ix = self.graph.add_node(var);
        self.graph_indices.insert(var, ix);
        self.possible_types.insert(var, PossibleTypes::any());
    }

    pub fn set_exact_type(&mut self, var: TypeVar, typ: ResolvableSpecificType) {
        self.possible_types.get_mut(&var).unwrap().set_exact_type(typ.clone());
        self.add_reduce_constraint(var, var, vec![typ])
    }

    pub fn add_eq_constraint(&mut self, from: TypeVar, to: TypeVar) {
        let from = self.graph_indices[&from];
        let to = self.graph_indices[&to];
        self.graph.add_edge(from, to, Constraint::Eq);
        self.graph.add_edge(to, from, Constraint::Eq);
    }
    pub fn add_unidirectional_eq_constraint(&mut self, from: TypeVar, to: TypeVar) {
        let from = self.graph_indices[&from];
        let to = self.graph_indices[&to];
        self.graph.add_edge(from, to, Constraint::Eq);
    }

    pub fn add_reduce_constraint(&mut self, from: TypeVar, to: TypeVar, reduce: Vec<ResolvableSpecificType>) {
        let from = self.graph_indices[&from];
        let to = self.graph_indices[&to];
        self.graph.add_edge(from, to, Constraint::Reduce(reduce));
    }

    pub fn add_field_access(&mut self, struc: TypeVar, field: TypeVar, fields: Vec<String>) {
        let struc = self.graph_indices[&struc];
        let field = self.graph_indices[&field];
        self.graph.add_edge(struc, field, Constraint::FieldAccess(fields));
        self.graph.add_edge(field, struc, Constraint::Reduce(vec![ResolvableSpecificType::Struct("struct".to_string())]));
    }

    pub fn add_method_call_arg(&mut self, source: TypeVar, arg: TypeVar, method_name: String, arg_index: usize) {
        let source = self.graph_indices[&source];
        let arg = self.graph_indices[&arg];
        self.graph.add_edge(source, arg, Constraint::MethodCallArg(method_name, arg_index));
    }
    pub fn add_method_call_ret(&mut self, source: TypeVar, call_expr: TypeVar, method_name: String) {
        let source = self.graph_indices[&source];
        let call_expr = self.graph_indices[&call_expr];
        self.graph.add_edge(source, call_expr, Constraint::MethodCallReturnType(method_name));
    }

    pub fn type_vars(&self) -> impl Iterator<Item = TypeVar> + '_ {
        assert_eq!(self.graph_indices.len(), self.graph.node_count());
        assert_eq!(self.possible_types.len(), self.graph.node_count());
        self.graph_indices.keys().copied()
    }

    pub fn possible_types(&self, var: TypeVar) -> &[ResolvableSpecificType] {
        &self.possible_types[&var].0
    }

    pub fn reduce(&mut self, var: TypeVar, reduce: &[ResolvableSpecificType]) -> UnifyResult {
        assert!(!reduce.is_empty());
        self.reduce_internal(var, reduce)
    }

    pub fn unify_assign(&mut self, into: TypeVar, from: TypeVar) -> UnifyResult {
        // TODO: is there really no hashmap allowing mutable access to two values at the same time?
        let from = self.possible_types[&from].clone();
        self.reduce_internal(into, &from.0)
    }

    pub fn field_access(&mut self, meta_info: &MetaInfo, struct_var: TypeVar, field_var: TypeVar, fields: &[String]) -> UnifyResult {
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
    pub fn method_call_arg(&mut self, meta_info: &MetaInfo, field_access: TypeVar, arg: TypeVar, method_name: &str, arg_index: usize) -> UnifyResult {
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
    pub fn method_call_ret(&mut self, meta_info: &MetaInfo, field_access: TypeVar, method_call: TypeVar, method_name: &str) -> UnifyResult {
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
            &SpecificType::Generic(span) => {
                if let Some(typ) = self.resolved_generics.get(&(from, span)) {
                    return typ.clone();
                }
                let is_un_unifyable = RefCell::new(false);
                self.search_from(
                    from,
                    |this, var| {
                        for typ in &this.possible_types[&var].0 {
                            if let &ResolvableSpecificType::UnUnifyableGeneric(span2) = typ {
                                if span == span2 {
                                    *is_un_unifyable.borrow_mut() = true;
                                }
                            }
                        }
                    },
                    |this, edge_index| {
                        match &this.graph[edge_index] {
                            Constraint::Reduce(types) => for typ in types {
                                if let &ResolvableSpecificType::UnUnifyableGeneric(span2) = typ {
                                    if span == span2 {
                                        *is_un_unifyable.borrow_mut() = true;
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                );
                if *is_un_unifyable.borrow() {
                    ResolvableSpecificType::UnUnifyableGeneric(span)
                } else {
                    ResolvableSpecificType::UnifyableGeneric(span)
                }
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
        let res = self.possible_types.get_mut(&var).unwrap().reduce(reduce);
        match res {
            ReduceResult::Unchanged => UnifyResult::Unchanged,
            ReduceResult::Changed => UnifyResult::Changed,
            ReduceResult::GenericSolved(span, typ) => {
                trace!("generic solved: {:?} = {}", span, typ);
                // iterate over all reachable fields and edges and exchange the generic with the specific ResolvableType
                let visited_nodes = self.search_from(
                    var,
                    |this, var| {
                        this.resolved_generics.insert((var, span), typ.clone());
                        let possible_types = &mut this.possible_types[&var];
                        if possible_types.len() == 1 {
                            if let ResolvableSpecificType::UnifyableGeneric(span2) = possible_types.0[0] {
                                if span2 == span {
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
                                if let ResolvableSpecificType::UnifyableGeneric(span2) = reduce[0] {
                                    if span == span2 {
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

    pub fn incoming(&self, var: TypeVar) -> Vec<(Constraint, TypeVar)> {
        let ix = self.graph_indices[&var];
        self.graph.edges_directed(ix, Direction::Incoming)
            .map(|edge| (edge.weight().clone(), *self.graph.node_weight(edge.source()).unwrap()))
            .collect()
    }

    pub fn outgoing_neighbors(&self, var: TypeVar) -> impl Iterator<Item = TypeVar> + '_ {
        let ix = self.graph_indices[&var];
        self.graph.neighbors_directed(ix, Direction::Outgoing)
            .map(move |ix| *self.graph.node_weight(ix).unwrap())
    }

    pub fn dot(&self) {
        let f1 = &|_, e: EdgeReference<Constraint>| format!("label = {:?}", e.weight().to_string());
        let f2 = &|_, (_, n): (NodeIndex<u32>, &TypeVar)| {
            let code = format!("{:?}", self.diagnostics.resolve_span(n.span));
            format!("label = \"{}\\n{}\"", &code[1..code.len()-1], self.possible_types[n])
        };
        let dot = Dot::with_attr_getters(
            &self.graph,
            &[Config::NodeNoLabel, Config::EdgeNoLabel, Config::GraphContentOnly],
            f1,
            f2,
        );
        let dot = {
            let mut vec = Vec::new();
            writeln!(vec, "digraph {{").unwrap();
            writeln!(vec, "    ranksep = .1;").unwrap();
            writeln!(vec, "rankdir = LR;").unwrap();
            writeln!(vec, "{:?}", dot).unwrap();
            writeln!(vec, "}}").unwrap();
            String::from_utf8(vec).unwrap()
        };
        println!("{}", dot);
        let mut xdot = Command::new("xdot")
            .arg("-")
            .stdin(Stdio::piped())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap();
        {
            xdot.stdin.take().unwrap().write_all(dot.as_bytes()).unwrap();
        }
        xdot.wait().unwrap();
    }
}
