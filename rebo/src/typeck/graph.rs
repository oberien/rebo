use petgraph::prelude::{DiGraph, NodeIndex, EdgeRef};
use crate::typeck::TypeVar;
use crate::common::MetaInfo;
use crate::typeck::types::{SpecificType, SpecificTypeDiscriminants, Type};
use strum::IntoEnumIterator;
use std::process::{Command, Stdio};
use petgraph::dot::{Dot, Config};
use std::fmt::{Display, Formatter};
use std::io::Write;
use itertools::Itertools;
use diagnostic::{Diagnostics, Span};
use petgraph::graph::EdgeReference;
use petgraph::Direction;
use indexmap::map::IndexMap;
use crate::EXTERNAL_SPAN;

#[derive(Debug, Clone)]
pub struct PossibleTypes(Vec<SpecificType>);

impl PartialEq<[SpecificType]> for PossibleTypes {
    fn eq(&self, other: &[SpecificType]) -> bool {
        self.0 == other
    }
}
impl PartialEq<PossibleTypes> for &[SpecificType] {
    fn eq(&self, other: &PossibleTypes) -> bool {
        *self == other.0
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub enum UnifyResult {
    Changed,
    Unchanged,
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

impl PossibleTypes {
    pub fn any() -> PossibleTypes {
        let mut possible_types = Vec::new();
        // make sure we have every type covered
        for kind in SpecificTypeDiscriminants::iter() {
            match kind {
                SpecificTypeDiscriminants::Unit => possible_types.push(SpecificType::Unit),
                SpecificTypeDiscriminants::Integer => possible_types.push(SpecificType::Integer),
                SpecificTypeDiscriminants::Float => possible_types.push(SpecificType::Float),
                SpecificTypeDiscriminants::Bool => possible_types.push(SpecificType::Bool),
                SpecificTypeDiscriminants::String => possible_types.push(SpecificType::String),
                // There can never be a struct named "struct" as it's a keyword.
                // That's why we use it meaning "any struct".
                SpecificTypeDiscriminants::Struct => possible_types.push(SpecificType::Struct("struct".to_string())),
                // Similarly to above, we use "enum" as any enum.
                SpecificTypeDiscriminants::Enum => possible_types.push(SpecificType::Enum("enum".to_string())),
                SpecificTypeDiscriminants::Generic => possible_types.push(SpecificType::Generic(Self::unrestricted_generic_span())),
            }
        }
        PossibleTypes(possible_types)
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    fn unrestricted_generic_span() -> Span {
        Span {
            file: EXTERNAL_SPAN.lock().unwrap().unwrap().file,
            start: 0,
            end: 0,
        }
    }

    pub fn is_unifyable_with(&self, typ: &SpecificType) -> bool {
        match typ {
            SpecificType::Struct(name) if name == "struct" => self.0.iter()
                .any(|typ| matches!(typ, SpecificType::Struct(_))),
            SpecificType::Struct(name) => self.0.iter()
                .any(|typ| matches!(typ, SpecificType::Struct(n) if n == name || n == "struct")),
            SpecificType::Enum(name) if name == "enum" => self.0.iter()
                .any(|typ| matches!(typ, SpecificType::Enum(_))),
            SpecificType::Enum(name) => self.0.iter()
                .any(|typ| matches!(typ, SpecificType::Enum(n) if n == name || n == "enum")),
            SpecificType::Generic(span) if *span == Self::unrestricted_generic_span() => self.0.iter()
                .any(|typ| matches!(typ, SpecificType::Generic(_))),
            SpecificType::Generic(span) => self.0.iter()
                .any(|typ| matches!(typ, SpecificType::Generic(span2) if *span2 == Self::unrestricted_generic_span() || span2 == span)),
            SpecificType::Unit
            | SpecificType::Bool
            | SpecificType::Integer
            | SpecificType::Float
            | SpecificType::String => self.0.contains(typ),
        }
    }

    pub fn set_exact_type(&mut self, typ: SpecificType) {
        assert!(self.is_unifyable_with(&typ), "{:?} not unifyable with {}", self, typ);
        self.0.clear();
        self.0.push(typ);
    }

    pub fn reduce(&mut self, reduce: &[SpecificType]) -> UnifyResult {
        if reduce.is_empty() {
            return UnifyResult::Unchanged;
        }
        let res = self.0.iter().filter_map(|typ| {
            match typ {
                SpecificType::Unit
                | SpecificType::Bool
                | SpecificType::Float
                | SpecificType::Integer
                | SpecificType::String => if reduce.contains(typ) { Some(typ.clone()) } else { None }
                SpecificType::Struct(name) => {
                    let reduce_struct = reduce.iter()
                        .filter_map(|typ| match typ {
                            SpecificType::Struct(name) => Some(name.as_str()),
                            _ => None,
                        }).next();
                    match (name.as_str(), reduce_struct) {
                        (_, None) => None,
                        ("struct", Some(name))
                        | (name, Some("struct")) => Some(SpecificType::Struct(name.to_string())),
                        (a, Some(b)) if a == b => Some(SpecificType::Struct(a.to_string())),
                        (_, _) => None,
                    }
                }
                SpecificType::Enum(name) => {
                    let reduce_enum = reduce.iter()
                        .filter_map(|typ| match typ {
                            SpecificType::Enum(name) => Some(name.as_str()),
                            _ => None,
                        }).next();
                    match (name.as_str(), reduce_enum) {
                        (_, None) => None,
                        ("enum", Some(name))
                        | (name, Some("enum")) => Some(SpecificType::Enum(name.to_string())),
                        (a, Some(b)) if a == b => Some(SpecificType::Enum(a.to_string())),
                        (_, _) => None,
                    }
                }
                SpecificType::Generic(span) => {
                    let reduce_generic = reduce.iter()
                        .filter_map(|typ| match typ {
                            SpecificType::Generic(span2) => Some(span2),
                            _ => None,
                        }).next();
                    match (span, reduce_generic) {
                        (_, None) => None,
                        (span, Some(span2))
                        | (span2, Some(span)) if *span == Self::unrestricted_generic_span() => {
                            Some(SpecificType::Generic(*span2))
                        }
                        (a, Some(b)) if a == b => Some(SpecificType::Generic(*a)),
                        (_, _) => None,
                    }
                }
            }
        }).collect();
        let old = std::mem::replace(&mut self.0, res);
        match old == self.0 {
            true => UnifyResult::Unchanged,
            false => UnifyResult::Changed,
        }
    }

    pub fn unify_assign(&mut self, other: &PossibleTypes) -> UnifyResult {
        self.reduce(&other.0)
    }
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Eq,
    Reduce(Vec<SpecificType>),
    /// field names of access path
    FieldAccess(Vec<String>),
    /// Reverse-Edge of FieldAccess, indicating that a variable must be a struct
    Struct,
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
            Constraint::Struct => write!(f, "struct"),
            Constraint::MethodCallArg(name, arg) => write!(f, "{}({})", name, arg),
            Constraint::MethodCallReturnType(name) => write!(f, "{}(...) -> ret", name),
        }
    }
}

pub struct Graph {
    graph: DiGraph<TypeVar, Constraint>,
    graph_indices: IndexMap<TypeVar, NodeIndex<u32>>,
    possible_types: IndexMap<TypeVar, PossibleTypes>,
}

impl Graph {
    pub fn new() -> Graph {
        Graph {
            graph: DiGraph::new(),
            graph_indices: IndexMap::new(),
            possible_types: IndexMap::new(),
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

    pub fn set_exact_type(&mut self, var: TypeVar, typ: SpecificType) {
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

    pub fn add_reduce_constraint(&mut self, from: TypeVar, to: TypeVar, reduce: Vec<SpecificType>) {
        let from = self.graph_indices[&from];
        let to = self.graph_indices[&to];
        self.graph.add_edge(from, to, Constraint::Reduce(reduce));
    }

    pub fn add_field_access(&mut self, struc: TypeVar, field: TypeVar, fields: Vec<String>) {
        let struc = self.graph_indices[&struc];
        let field = self.graph_indices[&field];
        self.graph.add_edge(struc, field, Constraint::FieldAccess(fields));
        self.graph.add_edge(field, struc, Constraint::Struct);
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

    pub fn possible_types(&self, var: TypeVar) -> &[SpecificType] {
        &self.possible_types[&var].0
    }

    pub fn reduce(&mut self, var: TypeVar, reduce: &[SpecificType]) -> UnifyResult {
        assert!(!reduce.is_empty());
        self.possible_types.get_mut(&var).unwrap().reduce(reduce)
    }

    pub fn unify_assign(&mut self, into: TypeVar, from: TypeVar) -> UnifyResult {
        // TODO: is there really no hashmap allowing mutable access to two values at the same time?
        let from = self.possible_types[&from].clone();
        self.possible_types.get_mut(&into).unwrap().unify_assign(&from)
    }

    pub fn field_access(&mut self, meta_info: &MetaInfo, struct_var: TypeVar, field_var: TypeVar, fields: &[String]) -> UnifyResult {
        let possible_types = self.possible_types.get_mut(&struct_var).unwrap();
        let res = possible_types.reduce(&[SpecificType::Struct("struct".to_string())]);
        if possible_types.0.len() != 1 {
            return res;
        }
        let struct_name = if let SpecificType::Struct(name) = &possible_types.0[0] {
            name
        } else {
            return res;
        };
        if struct_name == "struct" {
            return res;
        }

        let mut typ = &possible_types.0[0];
        for field in fields {
            let struct_typ = match typ {
                SpecificType::Struct(name) => match meta_info.struct_types.get(name.as_str()) {
                    Some(struct_typ) => struct_typ,
                    None => return res,
                }
                _ => return res,
            };
            typ = match struct_typ.get_field(field) {
                Some(Type::Specific(typ)) => typ,
                _ => return res,
            }
        }
        let typ = typ.clone();
        self.possible_types.get_mut(&field_var).unwrap().reduce(&[typ]);
        res
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
            Some(typ) => typ,
            None => &Type::Varargs,
        };
        match expected_arg_type {
            Type::Top | Type::Varargs => UnifyResult::Unchanged,
            Type::Bottom => unreachable!("fn arg is bottom"),
            Type::Specific(specific) => self.possible_types.get_mut(&arg).unwrap().reduce(&[specific.clone()]),
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
        match &fn_typ.ret {
            Type::Top | Type::Varargs => unreachable!("fn ret is Top or Varargs"),
            Type::Bottom => UnifyResult::Unchanged,
            Type::Specific(specific) => self.possible_types.get_mut(&method_call).unwrap().reduce(&[specific.clone()]),
        }
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

    pub fn dot(&self, diagnosics: &Diagnostics) {
        let f1 = &|_, e: EdgeReference<Constraint>| format!("label = {:?}", e.weight().to_string());
        let f2 = &|_, (_, n): (NodeIndex<u32>, &TypeVar)| {
            let code = format!("{:?}", diagnosics.resolve_span(n.span));
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
