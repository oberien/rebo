mod values;
mod types;

pub use values::{Value, Function, FunctionImpl, IntoValue, FromValues, FromValue, Struct, StructArc, FuzzyFloat};
pub use types::{Type, SpecificType, FunctionType, StructType};
use std::fmt::{self, Display, Formatter};
use indexmap::map::IndexMap;
use diagnostic::Span;
use crate::parser::{Binding, ExprFunctionDefinition};
use crate::scope::{Scope, BindingId};
use crate::lexer::TokenIdent;

/// Info needed before parsing / before typechecking
pub struct PreInfo<'a, 'i> {
    /// types of bindings of the root scope / stdlib and function definitions of the first parser pass
    pub bindings: IndexMap<Binding<'i>, SpecificType>,
    /// functions found in the code
    pub rebo_functions: IndexMap<BindingId, &'a ExprFunctionDefinition<'a, 'i>>,
    /// associated functions found in the code
    pub rebo_associated_functions: IndexMap<BindingId, (&'a TokenIdent<'i>, &'a ExprFunctionDefinition<'a, 'i>)>,
    /// struct definitions found in the code
    pub structs: IndexMap<&'i str, (StructType, Span)>,
    pub root_scope: Scope,
}
impl<'a, 'i> PreInfo<'a, 'i> {
    pub fn new() -> Self {
        PreInfo {
            bindings: IndexMap::new(),
            rebo_functions: IndexMap::new(),
            rebo_associated_functions: IndexMap::new(),
            structs: IndexMap::new(),
            root_scope: Scope::new(),
        }
    }
}

pub struct Depth {
    last_list: Vec<bool>,
}
impl Depth {
    pub fn start() -> Depth {
        Depth {
            last_list: vec![false],
        }
    }
    pub fn next(&self) -> Depth {
        Depth {
            last_list: self.last_list.iter().cloned().chain(Some(false)).collect(),
        }
    }
    pub fn last(&self) -> Depth {
        Depth {
            last_list: self.last_list.iter().cloned().chain(Some(true)).collect(),
        }
    }
}

impl Display for Depth {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for last in &self.last_list[..self.last_list.len() - 1] {
            let last = match last {
                false => "│   ",
                true => "    ",
            };
            write!(f, "{}", last)?;
        }
        let last = match self.last_list.last().unwrap() {
            false => "├──",
            true => "└──",
        };
        write!(f, "{}", last)
    }
}
