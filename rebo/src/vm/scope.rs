use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::common::Value;
use crate::parser::BindingId;

pub struct Scopes {
    scopes: Rc<RefCell<Vec<Scope>>>,
}

pub struct Scope {
    variables: HashMap<BindingId, Value>,
}
pub struct ScopeGuard {
    scopes: Rc<RefCell<Vec<Scope>>>,
}
impl Drop for ScopeGuard {
    fn drop(&mut self) {
        self.scopes.borrow_mut().pop().unwrap();
    }
}

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            scopes: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn push_scope(&self, scope: Scope) -> ScopeGuard {
        self.scopes.borrow_mut().push(scope);
        ScopeGuard {
            scopes: Rc::clone(&self.scopes),
        }
    }

    pub fn create(&self, binding_id: BindingId, value: Value) {
        self.scopes.borrow_mut().last_mut().unwrap().create(binding_id, value);
    }
    pub fn assign(&self, binding_id: BindingId, value: Value) {
        let mut scopes = self.scopes.borrow_mut();
        let val = scopes.iter_mut().rev()
            .filter_map(|scope| scope.get_mut(binding_id))
            .next()
            .unwrap_or_else(|| panic!("binding_id {:?} doesn't exist but was assigned to", binding_id));
        *val = value;
    }
    pub fn get(&self, binding_id: BindingId) -> Option<Value> {
        self.scopes.borrow().iter().rev()
            .filter_map(|scope| scope.get(binding_id))
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
    pub fn get(&self, binding_id: BindingId) -> Option<Value> {
        self.variables.get(&binding_id).cloned()
    }
    pub fn get_mut(&mut self, binding_id: BindingId) -> Option<&mut Value> {
        self.variables.get_mut(&binding_id)
    }
    pub fn create(&mut self, binding_id: BindingId, value: Value) {
        let prev = self.variables.insert(binding_id, value);
        assert!(prev.is_none());
    }
}

