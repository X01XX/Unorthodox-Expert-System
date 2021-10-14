//! Remove an element from a vector, caring more bout cycles than order.
//! If iterating over a vector, accumulating a varcot of indices to remove,
//! call this in reverse order.
pub fn remove_unordered<T>(avec: &mut Vec<T>, inx: usize) {
    assert!(inx < avec.len());
    let last_item = avec.pop().unwrap();

    if inx < avec.len() {
        avec[inx] = last_item;
    }
}

