//! Remove an element from a vector, not caring about order, avoid the
//! copying that the remove command does.
//! If iterating over a vector, accumulating a vector of indices to remove,
//! afterwards call this with indices in descending order.
pub fn remove_unordered<T>(avec: &mut Vec<T>, inx: usize) {
    assert!(inx < avec.len());
    let last_item = avec.pop().unwrap();

    if inx < avec.len() {
        // avec.len() is now equal to the index of the last item before the pop operation.
        avec[inx] = last_item;
    }
}
