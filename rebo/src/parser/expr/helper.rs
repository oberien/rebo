use crate::parser::InternalError;

pub fn last_error(errors: &[InternalError]) -> InternalError {
    fn last_error_internal(err1: InternalError, err2: InternalError) -> InternalError {
        match (err1, err2) {
            (InternalError::Error(_), err2) => err2,
            (err1, InternalError::Error(_)) => err1,
            (InternalError::Backtrack(span1, ex1), InternalError::Backtrack(span2, ex2)) => if span1 >= span2 {
                InternalError::Backtrack(span1, ex1)
            } else {
                InternalError::Backtrack(span2, ex2)
            }
        }
    }
    let mut last = errors[0].clone();
    for error in &errors[1..] {
        last = last_error_internal(last, error.clone());
    }
    last
}