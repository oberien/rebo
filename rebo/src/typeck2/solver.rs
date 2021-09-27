use crate::typeck2::graph::{Graph, Constraint, UnifyResult};
use crate::common::{MetaInfo, SpecificType};
use std::collections::{VecDeque, HashSet};
use crate::typeck2::TypeVar;
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
                Constraint::Struct => graph.reduce(var, &[SpecificType::Struct("struct".to_string())]),
                Constraint::Reduce(reduce) => graph.reduce(var, &reduce),
                Constraint::FieldAccess(fields) => {
                    let res = graph.field_access(meta_info, source, var, &fields);
                    // `field_access` can also change the struct-source.
                    // If it has, add the source as well.
                    if res == UnifyResult::Changed {
                        todos.add(source);
                    }
                    res
                },
            };
            match unify_result {
                UnifyResult::Changed => changed = true,
                UnifyResult::Unchanged => (),
            }
        }
        if changed {
           for todo in graph.outgoing_neighbors(var) {
                todos.add(todo);
            }
        }
    }
}