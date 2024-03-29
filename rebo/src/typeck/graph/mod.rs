use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};

use diagnostic::{Diagnostics, Span};
use indexmap::map::IndexMap;
use itertools::Itertools;
use petgraph::Direction;
use petgraph::dot::{Config, Dot};
use petgraph::graph::{EdgeIndex, EdgeReference};
use petgraph::prelude::{DiGraph, EdgeRef, NodeIndex};
use petgraph::visit::NodeFiltered;
use strum::IntoEnumIterator;

use crate::typeck::types::{ResolvableSpecificType, ResolvableSpecificTypeDiscriminants};
use crate::typeck::TypeVar;
use crate::typeck::graph::create::FunctionGenerics;
use crate::ErrorCode;

mod create;
mod solve;
mod check;

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
                ResolvableSpecificTypeDiscriminants::Struct => possible_types.push(ResolvableSpecificType::Struct("struct".to_string(), Vec::new())),
                // Similarly to above, we use "enum" as any enum.
                ResolvableSpecificTypeDiscriminants::Enum => possible_types.push(ResolvableSpecificType::Enum("enum".to_string(), Vec::new())),
                ResolvableSpecificTypeDiscriminants::Function => possible_types.push(ResolvableSpecificType::Function(None)),
                ResolvableSpecificTypeDiscriminants::UnUnifyableGeneric => (),
            }
        }
        PossibleTypes(possible_types)
    }
    fn len(&self) -> usize {
        self.0.len()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constraint {
    Eq,
    Reduce(Vec<ResolvableSpecificType>),
    /// accessed field
    FieldAccess(String),
    /// call_index (to identify FunctionGenerics), arg-index
    FunctionCallArg(u64, usize),
    /// call_index (to identify FunctionGenerics)
    FunctionCallReturnType(u64),
    /// indicates where a method-name access node stems from; call_index (to identify FunctionGenerics)
    Method,
    /// name of the method (not fully qualified yet), call_index (to identify FunctionGenerics), arg-index
    MethodCallArg(u64, usize),
    /// name of the method (not fully qualified yet), call_index (to identify FunctionGenerics)
    MethodCallReturnType(u64),
    /// indicates where a generic stems from / which node created the generic
    ///
    /// can be ignored for everything, just there to make the graph look better
    Generic,
    /// for type error messages: indicates where the equality between two generics stems from
    GenericEqSource,
}
impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constraint::Eq => write!(f, "="),
            Constraint::Reduce(reduce) => {
                write!(f, "[{}]", reduce.iter().join(","))
            }
            Constraint::FieldAccess(field) => {
                write!(f, ".{}", field)
            }
            Constraint::FunctionCallArg(idx, arg) => write!(f, "...[{}]({})", idx, arg),
            Constraint::FunctionCallReturnType(idx) => write!(f, "...[{}](...) -> ret", idx),
            Constraint::MethodCallArg(idx, arg) => write!(f, "...[{}]({})", idx, arg),
            Constraint::MethodCallReturnType(idx) => write!(f, "...[{}](...) -> ret", idx),
            Constraint::Method => write!(f, "Method"),
            Constraint::Generic => write!(f, "Generic"),
            Constraint::GenericEqSource => write!(f, "GenericEqSource"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Node {
    TypeVar(TypeVar),
    /// origin, unique-id
    Synthetic(Span, u64),
}
static SYNTHETIC_NODE_IDX: AtomicU64 = AtomicU64::new(0);
impl Node {
    pub fn type_var(origin: Span) -> Node {
        Node::TypeVar(TypeVar::new(origin))
    }
    pub fn synthetic(origin: Span) -> Node {
        Node::Synthetic(origin, SYNTHETIC_NODE_IDX.fetch_add(1, Ordering::SeqCst))
    }
    pub fn span(self) -> Span {
        match self {
            Node::TypeVar(var) => var.span,
            Node::Synthetic(span, _) => span,
        }
    }
}
impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::TypeVar(var) => write!(f, "[{}:{}:{}]", var.span.file, var.span.start, var.span.end),
            Node::Synthetic(span, id) => write!(f, "[{}<{}:{}:{}>]", id, span.file, span.start, span.end),
        }
    }
}

pub struct Graph<'i> {
    diagnostics: &'i Diagnostics<ErrorCode>,
    graph: DiGraph<Node, Constraint>,
    graph_indices: IndexMap<Node, NodeIndex<u32>>,
    possible_types: IndexMap<Node, PossibleTypes>,
    method_function_generics: IndexMap<u64, FunctionGenerics>,
    any_nodes: HashMap<Span, Node>,
}

impl<'i> Graph<'i> {
    fn new(diagnostics: &'i Diagnostics<ErrorCode>) -> Graph {
        Graph {
            diagnostics,
            graph: DiGraph::new(),
            graph_indices: IndexMap::new(),
            possible_types: IndexMap::new(),
            method_function_generics: IndexMap::new(),
            any_nodes: HashMap::new(),
        }
    }

    fn nodes(&self) -> impl Iterator<Item = Node> + '_ {
        assert_eq!(self.graph_indices.len(), self.graph.node_count());
        assert_eq!(self.possible_types.len(), self.graph.node_count());
        self.graph_indices.keys().copied()
    }

    fn incoming(&self, node: Node) -> Vec<(EdgeIndex, Constraint, Node)> {
        let ix = self.graph_indices[&node];
        self.graph.edges_directed(ix, Direction::Incoming)
            .map(|edge| (edge.id(), edge.weight().clone(), *self.graph.node_weight(edge.source()).unwrap()))
            .collect()
    }

    fn outgoing_neighbors(&self, node: Node) -> impl Iterator<Item = Node> + '_ {
        let ix = self.graph_indices[&node];
        self.graph.neighbors_directed(ix, Direction::Outgoing)
            .map(move |ix| *self.graph.node_weight(ix).unwrap())
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
            writeln!(xdot.stdin.take().unwrap(), "{}", self).unwrap();
        }
        xdot.wait().unwrap();
    }
}

impl<'i> Display for Graph<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // create Dot graph
        let f1 = &|_, e: EdgeReference<Constraint>| format!("label = {:?}", e.weight().to_string());
        let f2 = &|_, (_, n): (NodeIndex<u32>, &Node)| {
            let code = format!("{:?}", self.diagnostics.resolve_span(n.span()));
            format!("label = \"[{}:{}:{}]: {}\\n{}\"", self.diagnostics.file_name(n.span().file), n.span().start, n.span().end, &code[1..code.len()-1], self.possible_types[n])
        };
        let graph = NodeFiltered::from_fn(&self.graph, |nid| {
            let fileid = self.graph.node_weight(nid).unwrap().span().file;
            let file_name = self.diagnostics.file_name(fileid);
            !file_name.starts_with("external-")
        });
        let dot = Dot::with_attr_getters(
            &graph,
            &[Config::NodeNoLabel, Config::EdgeNoLabel, Config::GraphContentOnly],
            f1,
            f2,
        );

        // write Dot Graph
        writeln!(f, "digraph {{")?;
        writeln!(f, "    ranksep = .1;")?;
        writeln!(f, "rankdir = LR;")?;
        writeln!(f, "{:?}", dot)?;
        writeln!(f, "}}")?;
        Ok(())
    }
}
