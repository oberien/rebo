use indexmap::map::IndexMap;
use crate::parser::{Binding, Generic};
use std::fmt::{self, Formatter, Display};
use std::sync::atomic::{AtomicU32, Ordering};

pub struct Scope<'i> {
    pub idents: IndexMap<&'i str, Binding<'i>>,
    pub generics: IndexMap<&'i str, Generic<'i>>,
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct BindingId(u32);
impl Display for BindingId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

static NEXT_BINDING_ID: AtomicU32 = AtomicU32::new(0);

impl BindingId {
    pub fn unique() -> BindingId {
        let id = NEXT_BINDING_ID.fetch_add(1, Ordering::SeqCst);
        // why do I even have this check?!
        if id == u32::MAX {
            panic!("binding id overflow");
        }
        BindingId(id)
    }
}
