//! Remove an element from a vector, not caring about order, avoid the 
//! copying that the remove command does.
//! If iterating over a vector, accumulating a vector of indices to remove,
//! afterwards call this with indices in reverse order, iter().rev()
pub fn remove_unordered<T>(avec: &mut Vec<T>, inx: usize) {
    assert!(inx < avec.len());
    let last_item = avec.pop().unwrap();

    if inx < avec.len() {
        avec[inx] = last_item;
    }
}

