use crate::removeunordered::remove_unordered;

/// Modify a vector by deleting duplicates, given an equal function pointer.
#[allow(dead_code)]
pub fn vec_remove_dups<T>(avec: &mut Vec<T>, testfn: fn(&T, &T) -> bool) {
    loop {
        if avec.len() < 2 { return; }

        // Check every possible item pair.
        let mut x = 0;
        let mut y = 0;
        'top_loop: for inx in 0..(avec.len() - 1) {
            for iny in (inx + 1)..avec.len() {
                if testfn(&avec[inx], &avec[iny]) {
                    x = inx;
                    y = iny;
                    break 'top_loop;
                }
            }
        }
        if x == y { return; }
        remove_unordered(avec, y);
    } // Find next duplicate.
}

/// Return true if a vector contains a given item, given a equal function pointer.
#[allow(dead_code)]
pub fn vec_contains<T>(avec: &[T], item: &T, testfn: fn(&T, &T) -> bool) -> bool {
    for itemx in avec.iter() {
        if testfn(item, itemx) { return true; }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_remove_dups() -> Result<(), String> {
        let mut vecx: Vec::<usize> = vec![1, 2, 3, 1, 4, 2, 2];
        vec_remove_dups(&mut vecx, | a, b | a == b);
        assert_eq!(vecx, vec![1, 2, 3, 4]);

        let mut vecx: Vec::<&str> = vec!["A", "B", "A", "C", "B"];
        vec_remove_dups(&mut vecx, | a, b | a == b);
        assert_eq!(vecx, vec!["A", "B", "C"]);

        vec_remove_dups(&mut Vec::<usize>::new(), | a, b | a == b);

        Ok(())
    }

    #[test]
    fn test_vec_contains() -> Result<(), String> {
        let vecx: Vec::<usize> = vec![1, 2, 3, 4];
        (assert_eq!(vec_contains(&vecx, &2, | a, b | a == b), true));
        (assert_eq!(vec_contains(&vecx, &5, | a, b | a == b), false));

        let vecx: Vec::<&str> = vec!["A", "B", "C", "D"];
        (assert_eq!(vec_contains(&vecx, &"B", | a, b | a == b), true));
        (assert_eq!(vec_contains(&vecx, &"E", | a, b | a == b), false));

        assert_eq!(vec_contains(&vec![], &"E", | a, b | a == b), false);
        Ok(())
    }
}
