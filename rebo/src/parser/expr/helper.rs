use crate::parser::{InternalError, Backtrack};

pub fn last_error(errors: &[InternalError]) -> InternalError {
    fn last_error_internal(err1: InternalError, err2: InternalError) -> InternalError {
        match (err1, err2) {
            (InternalError::Error(_), err2) => err2,
            (err1, InternalError::Error(_)) => err1,
            (
                InternalError::Backtrack(b1 @ Backtrack { span: span1, expected: _ }),
                InternalError::Backtrack(b2 @ Backtrack { span: span2, expected: _ })
            ) => if span1 >= span2 {
                InternalError::Backtrack(b1)
            } else {
                InternalError::Backtrack(b2)
            }
        }
    }
    let mut last = errors[0].clone();
    for error in &errors[1..] {
        last = last_error_internal(last, error.clone());
    }
    last
}