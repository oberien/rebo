use crate::typeck::graph::{Graph, Constraint, UnifyResult};
use crate::common::MetaInfo;
use std::collections::{VecDeque, HashSet};
use crate::typeck::TypeVar;
use std::iter::FromIterator;

struct WorkQueue {
    queue: VecDeque<TypeVar>,
    set: HashSet<TypeVar>,
}

impl WorkQueue {
    pub fn new() -> Self {
        WorkQueue {
            queue: VecDeque::new(),
            set: HashSet::new(),
        }
    }
    pub fn add(&mut self, var: TypeVar) {
        if self.set.contains(&var) {
            return;
        }
        self.set.insert(var);
        self.queue.push_back(var);
    }
    pub fn next(&mut self) -> Option<TypeVar> {
        let var = self.queue.pop_front()?;
        self.set.remove(&var);
        Some(var)
    }
}
impl FromIterator<TypeVar> for WorkQueue {
    fn from_iter<T: IntoIterator<Item=TypeVar>>(iter: T) -> Self {
        let mut queue = WorkQueue::new();
        for e in iter {
            queue.add(e);
        }
        queue
    }
}

pub fn solve(graph: &mut Graph, meta_info: &mut MetaInfo) {
    let mut todos: WorkQueue = graph.type_vars().collect();

    while let Some(var) = todos.next() {
        let mut changed = false;
        for (constraint, source) in graph.incoming(var) {
            let unify_result = match constraint {
                Constraint::Eq => graph.unify_assign(var, source),
                Constraint::Reduce(reduce) => graph.reduce(var, &reduce),
                Constraint::FieldAccess(fields) => {
                    graph.field_access(meta_info, source, var, &fields)
                },
                Constraint::MethodCallArg(name, arg_index) => {
                    graph.method_call_arg(meta_info, source, var, &name, arg_index)
                }
                Constraint::MethodCallReturnType(name) => {
                    graph.method_call_ret(meta_info, source, var, &name)
                }

            };
            match unify_result {
                UnifyResult::Changed => {
                    todos.add(source);
                    changed = true;
                },
                UnifyResult::Unchanged => (),
                UnifyResult::Multiple(to_add) => for to_add in to_add {
                    todos.add(to_add);
                    changed = true;
                }
            }
        }
        if changed {
            todos.add(var);
           for todo in graph.outgoing_neighbors(var) {
                todos.add(todo);
            }
        }
    }
}