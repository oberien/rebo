# Merge Sort

```rust
fn merge_sort<T>(array: List<T>) -> List<T> {
    merge_sort_internal(array, 0, array.len())
}

fn merge_sort_internal<T>(array: List<T>, start: int, end: int) -> List<T> {
    let len = end - start;
    if len == 1 {
        return List::of(array.get(start).unwrap());
    }
    
    // split
    
    let middle = start + len/2;
    let left = merge_sort_internal(array, start, middle);
    let right = merge_sort_internal(array, middle, end);
    let sorted = List::new();
    let mut left_index = 0;
    let mut right_index = 0;
    
    // merge
    
    while left_index < left.len() && right_index < right.len() {
        let left = left.get(left_index).unwrap();
        let right = right.get(right_index).unwrap();
        if left <= right {
            sorted.push(left);
            left_index += 1;
        } else {
            sorted.push(right);
            right_index += 1;
        }
    }
    
    while left_index < left.len() {
        sorted.push(left.get(left_index).unwrap());
        left_index += 1;
    }
    while right_index < right.len() {
        sorted.push(right.get(right_index).unwrap());
        right_index += 1;
    }
    
    sorted
}

let array = List::of(5, 4, 7, 2, 1, 3, 2);
let sorted = merge_sort(array);
assert_eq(sorted, List::of(1, 2, 2, 3, 4, 5, 7));
```
