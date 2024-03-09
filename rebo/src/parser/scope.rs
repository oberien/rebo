use indexmap::map::IndexMap;
use crate::parser::{Binding, Generic, ExprLabelDef};
use std::fmt::{self, Formatter, Display};
use std::sync::atomic::{AtomicU32, Ordering};

pub struct Scope<'i> {
    pub idents: IndexMap<String, Binding<'i>>,
    pub generics: IndexMap<&'i str, Generic<'i>>,
    pub typ: ScopeType<'i>,
}

#[derive(Debug)]
pub enum ScopeType<'i> {
    Global,
    File,
    Function,
    While(Option<ExprLabelDef<'i>>),
    For(Option<ExprLabelDef<'i>>),
    Loop(Option<ExprLabelDef<'i>>),
    Synthetic,
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
        // why do we even have this check?!
        if id == u32::MAX {
            panic!("binding id overflow");
        }
        BindingId(id)
    }
}
