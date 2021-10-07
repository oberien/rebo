use std::collections::HashMap;
use crate::common::Value;
use crate::parser::BindingId;

pub struct Scopes {
    scopes: Vec<Scope>,
}

pub struct Scope {
    variables: HashMap<BindingId, Value>,
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
    #[allow(dead_code)]
    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }
    #[allow(dead_code)]
    pub fn scopes_mut(&mut self) -> &mut [Scope] {
        &mut self.scopes
    }

    pub fn create(&mut self, binding_id: BindingId, value: Value) {
        self.scopes.last_mut().unwrap().create(binding_id, value);
    }
    pub fn assign(&mut self, binding_id: BindingId, value: Value) {
        let val = self.get_mut(binding_id)
            .unwrap_or_else(|| panic!("binding_id {:?} doesn't exist but was assigned to", binding_id));
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

    // runtime functions
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

