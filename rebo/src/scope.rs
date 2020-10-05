use std::collections::HashMap;
use std::fmt;

use crate::common::{Value, Function, SpecificType, Type};
use std::sync::atomic::{AtomicU32, Ordering};
use crate::typeck::BindingTypes;
use crate::parser::Binding;
use crate::diagnostics::Span;

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct BindingId(u32);
impl fmt::Display for BindingId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

static NEXT_BINDING_ID: AtomicU32 = AtomicU32::new(0);

impl BindingId {
    pub fn new() -> BindingId {
        let id = NEXT_BINDING_ID.fetch_add(1, Ordering::SeqCst);
        // why do I even have this check?!
        if id == u32::MAX {
            panic!("binding id overflow");
        }
        BindingId(id)
    }
}

pub struct Scopes {
    scopes: Vec<Scope>,
}

pub struct RootScope<'a, 'i> {
    scope: Scope,
    binding_types: &'a mut BindingTypes<'i>,
    binding_id_mapping: HashMap<&'static str, BindingId>,
}

pub struct Scope {
    variables: HashMap<BindingId, Value>,
}

impl<'a, 'i> RootScope<'a, 'i> {
    pub fn new(binding_types: &'a mut BindingTypes<'i>) -> Self {
        RootScope {
            scope: Scope::new(),
            binding_types,
            binding_id_mapping: HashMap::new(),
        }
    }

    pub(crate) fn into_inner(self) -> (Scope, HashMap<&'static str, BindingId>) {
        (self.scope, self.binding_id_mapping)
    }

    pub fn add_function(&mut self, name: &'static str, f: Function) {
        let binding_id = BindingId::new();
        let binding = Binding {
            id: binding_id,
            ident: name,
            mutable: false,
            span: Span::external(),
            rogue: false,
        };
        self.scope.create(binding_id, Value::Function(f.imp));
        self.binding_types.insert(binding, Type::Specific(SpecificType::Function(Box::new(f.typ))), binding.span);
        self.binding_id_mapping.insert(name, binding_id);
    }
}


impl Scopes {
    pub fn new() -> Self {
        Scopes {
            scopes: Vec::new(),
        }
    }

    pub fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }
    pub fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }
    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }
    pub fn scopes_mut(&mut self) -> &mut [Scope] {
        &mut self.scopes
    }

    pub fn create(&mut self, binding_id: BindingId, value: Value) {
        self.scopes.last_mut().unwrap().create(binding_id, value);
    }
    pub fn assign(&mut self, binding_id: BindingId, value: Value) {
        let val = self.get_mut(binding_id)
            .expect(&format!("binding_id {:?} doesn't exist but was assigned to", binding_id));
        *val = value;
    }
    pub fn get(&self, binding_id: BindingId) -> Option<&Value> {
        self.scopes.iter().rev()
            .filter_map(|scope| scope.get(binding_id))
            .next()
    }
    pub fn get_mut(&mut self, binding_id: BindingId) -> Option<&mut Value> {
        self.scopes.iter_mut().rev()
            .filter_map(|scope| scope.get_mut(binding_id))
            .next()
    }
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
        }
    }

    pub fn get(&self, binding_id: BindingId) -> Option<&Value> {
        self.variables.get(&binding_id)
    }
    pub fn get_mut(&mut self, binding_id: BindingId) -> Option<&mut Value> {
        self.variables.get_mut(&binding_id)
    }
    pub fn create(&mut self, binding_id: BindingId, value: Value) {
        let prev = self.variables.insert(binding_id, value);
        assert!(prev.is_none());
    }
}

