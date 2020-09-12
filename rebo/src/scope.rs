use std::collections::HashMap;

use crate::types::{Value, Function};
use std::sync::atomic::{AtomicU32, Ordering};

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct BindingId(u32);

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

pub struct RootScope {
    scope: Scope,
    binding_id_mapping: HashMap<&'static str, BindingId>,
}

pub struct Scope {
    variables: HashMap<BindingId, Value>,
}

impl RootScope {
    pub fn new() -> Self {
        RootScope {
            scope: Scope::new(),
            binding_id_mapping: HashMap::new(),
        }
    }

    pub(crate) fn binding_id_mapping(&self) -> &HashMap<&'static str, BindingId> {
        &self.binding_id_mapping
    }

    pub fn add_function(&mut self, name: &'static str, f: Function) {
        let binding_id = BindingId::new();
        self.scope.create(binding_id, Value::Function(f));
        self.binding_id_mapping.insert(name, binding_id);
    }
}


impl Scopes {
    pub fn new(root_scope: RootScope) -> Self {
        Scopes { scopes: vec![root_scope.scope] }
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

