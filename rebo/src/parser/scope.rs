use indexmap::map::IndexMap;
use crate::parser::{Binding, Generic, ExprLabelDef};
use std::fmt::{self, Formatter, Display};
use std::sync::atomic::{AtomicU64, Ordering};

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
    // the inner label is only used for debugging
    #[expect(unused)]
    While(Option<ExprLabelDef<'i>>),
    #[expect(unused)]
    For(Option<ExprLabelDef<'i>>),
    #[expect(unused)]
    Loop(Option<ExprLabelDef<'i>>),
    Synthetic,
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct BindingId(u64);
impl Display for BindingId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

static NEXT_BINDING_ID: AtomicU64 = AtomicU64::new(0);

impl BindingId {
    pub fn unique() -> BindingId {
        let id = NEXT_BINDING_ID.fetch_add(1, Ordering::SeqCst);
        // why do we even have this check?!
        if id == u64::MAX {
            panic!("binding id overflow");
        }
        BindingId(id)
    }
}
