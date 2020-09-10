use std::collections::HashMap;
use std::mem;

use crate::types::{Value, FromValues, IntoValue, Function, FunctionType};

pub struct Scope {
    parent: Option<Box<Scope>>,
    variables: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            parent: None,
            variables: HashMap::new(),
        }
    }
    pub fn with_parent_scope(parent: Scope) -> Scope {
        Scope {
            parent: Some(Box::new(parent)),
            variables: HashMap::new(),
        }
    }

    pub fn add_function(&mut self, name: String, f: Function) {
        self.variables.insert(name, Value::Function(f));
    }
}

impl Scope {
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(name).or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }
    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        let parent = self.parent.as_mut();
        self.variables.get_mut(name).or_else(|| parent.and_then(|parent| parent.get_mut(name)))
    }
    pub fn set(&mut self, name: String, value: Value) -> Option<Value> {
        match self.get_mut(&name) {
            Some(val) => Some(mem::replace(val, value)),
            None => self.variables.insert(name, value),
        }
    }
}

