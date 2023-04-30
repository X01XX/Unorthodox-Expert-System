use crate::removeunordered::remove_unordered;

/// Modify a vector by deleting duplicates, given an equal function pointer.
#[allow(dead_code)]
pub fn vec_remove_dups<T>(avec: &mut Vec<T>, testfn: fn(&T, &T) -> bool) {
    loop {
        if avec.len() < 2 {
            return;
        }

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
        if x == y {
            return;
        }
        remove_unordered(avec, y);
    } // Find next duplicate.
}

/// Return true if a vector contains a given item, given a equal function pointer.
#[allow(dead_code)]
pub fn vec_contains<T>(avec: &[T], item: &T, testfn: fn(&T, &T) -> bool) -> bool {
    for itemx in avec.iter() {
        if testfn(item, itemx) {
            return true;
        }
    }
    false
}

/// Return a list of any x item combinations of items in a vector of N (<= x) items, order matters.
///
/// e.g. any 3 of [1, 2, 3, 4, 5].
///
/// The number of lists in the result is like a factorial truncated by the number of items sought.
///
/// Any 1 of 5 = 5.
/// Any 2 of 5 = 5 * 4 = 20;
/// Any 3 of 5 = 5 * 4 * 3 = 60;
/// Any 4 of 5 = 5 * 4 * 3 * 2 = 120;
/// Any 5 of 5 = 5 * 4 * 3 * 2 * 1 = 120;
#[allow(dead_code)]
pub fn anyxofvec_order_matters<T: Copy>(num_items: usize, vecx: Vec<T>) -> Vec<Vec<T>> {
    debug_assert!(num_items <= vecx.len());

    let mut limit = 1;
    for num_of in 1..=vecx.len() {
        for place in (1..=vecx.len()).skip(limit - num_of + 1) {
            limit *= place;
        }
    }

    let mut rlst = Vec::<Vec<Vec<T>>>::with_capacity(limit);
    rlst.push(vec![vec![], vecx]);

    for _ in 0..num_items {
        // Calc number of results.
        let mut cap = 0;
        for itemx in rlst.iter() {
            cap += itemx[1].len();
        }
        // Init result vector.
        let mut rlst2 = Vec::<Vec<Vec<T>>>::with_capacity(cap);

        for itemx in rlst.iter() {
            rlst2.append(&mut proc_vec_pair(itemx));
        }
        rlst = rlst2;
    }

    // Extract first item in each pair.
    let mut newlst = Vec::<Vec<T>>::with_capacity(rlst.len());
    for itemx in rlst.into_iter() {
        if let Some(first_item) = itemx.into_iter().next() {
            newlst.push(first_item);
        }
    }
    newlst
}

/// Return the factorial of a number.
fn factorial(num: usize) -> usize {
    if num == 0 {
        return 1;
    }
    if num < 3 {
        return num;
    }
    let mut ret = num;
    let mut count = num;
    while count > 1 {
        count -= 1;
        ret *= count;
    }
    ret
}

/// Return a list of any x item combinations of items in a vector of N (<= x) items, order does not matter.
///
/// e.g. any 3 of (a, b, c, d).
///
/// The number lists returned = N! / (x! (N-x)!).
///
/// 4! / (3! (4-3)!) = 24 / (6 (1)!) = 24 / 6 = 4
///
/// =  (a, b, c) (a, b, d) (a, c, d) (b, c, d)
///
#[allow(dead_code)]
pub fn anyxofvec<T: Copy>(num_items: usize, vecx: Vec<T>) -> Vec<Vec<T>> {
    debug_assert!(num_items <= vecx.len());

    anyxofvec2(num_items, Vec::<T>::new(), vecx)
}
fn anyxofvec2<T: Copy>(num_items: usize, vecy: Vec<T>, vecx: Vec<T>) -> Vec<Vec<T>> {
    if num_items < 1 {
        return vec![vecy];
    }
    // Calc final size of return vector.
    let cap: usize =
        factorial(vecx.len()) / (factorial(num_items) * factorial(vecx.len() - num_items));

    // Init return vector.
    let mut vec_rc = Vec::<Vec<T>>::with_capacity(cap);

    let numx = vecx.len() - num_items;

    for inx in 0..(numx + 1) {
        // Calc size of toright vector.
        let cap: usize = vecx.len() - inx - 1;

        // Init toright vector.
        let mut toright = Vec::<T>::with_capacity(cap);

        // If cap > 0, push items to toright vector.
        for itemx in vecx.iter().skip(inx + 1) {
            toright.push(*itemx);
        }

        // Form copy of vecy vector plus an item from vecx.
        let mut vecz = Vec::<T>::with_capacity(vecy.len() + 1);
        for itx in vecy.iter() {
            vecz.push(*itx);
        }
        vecz.push(vecx[inx]);

        vec_rc.append(&mut anyxofvec2(num_items - 1, vecz, toright));
    }
    vec_rc
}

/// Given a ref to a vector of a pair of vectors,
/// return a vector of pairs of [(list1 + one item in list2), (list2 - item added to list1)]
fn proc_vec_pair<T: Copy>(pair: &[Vec<T>]) -> Vec<Vec<Vec<T>>> {
    let newvec = &pair[0];
    let sampls = &pair[1];

    let mut rlst2 = Vec::<Vec<Vec<T>>>::with_capacity(sampls.len());
    for inx in 0..sampls.len() {
        let mut veccpy: Vec<T> = Vec::with_capacity(newvec.len() + 1);
        for itemx in newvec.iter() {
            veccpy.push(*itemx);
        }
        veccpy.push(sampls[inx]);

        let newsmpls = copyvecminus(sampls, inx);

        rlst2.push(vec![veccpy, newsmpls]);
    }
    rlst2
}

/// Return a copy of a list, with one element
/// removed.
fn copyvecminus<T: Copy>(vecx: &Vec<T>, num: usize) -> Vec<T> {
    let mut newvec = Vec::<T>::with_capacity(vecx.len() - 1);
    for (inx, itemx) in vecx.iter().enumerate() {
        if inx == num {
            continue;
        }
        newvec.push(*itemx);
    }

    newvec
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_anyxofvec_order_matters() -> Result<(), String> {
        let x = anyxofvec_order_matters(3, vec![0, 1, 2, 3]);
        println!("x {:?}", x);
        assert!(x.len() == 24);
        Ok(())
    }

    #[test]
    fn test_anyxofvec() -> Result<(), String> {
        let x = anyxofvec(3, vec![0, 1, 2, 3]);
        println!("x {:?}", x);
        assert!(x == vec![vec![0, 1, 2], vec![0, 1, 3], vec![0, 2, 3], vec![1, 2, 3]]);
        Ok(())
    }

    #[test]
    fn test_factorial() -> Result<(), String> {
        assert!(factorial(0) == 1);
        assert!(factorial(1) == 1);
        assert!(factorial(2) == 2);
        assert!(factorial(3) == 6);
        assert!(factorial(4) == 24);
        Ok(())
    }

    #[test]
    fn test_proc_vec_pair() -> Result<(), String> {
        let x = proc_vec_pair(&vec![vec![0, 1], vec![3, 4]]);
        println!("{:?}", x);
        assert!(x == vec![vec![vec![0, 1, 3], vec![4]], vec![vec![0, 1, 4], vec![3]]]);
        Ok(())
    }

    #[test]
    fn test_copyvecminus() -> Result<(), String> {
        let nums = vec![0, 1, 2, 3];
        assert_eq!(copyvecminus(&nums, 2), vec![0, 1, 3]);
        assert_eq!(copyvecminus(&nums, 0), vec![1, 2, 3]);
        assert_eq!(copyvecminus(&nums, 3), vec![0, 1, 2]);

        Ok(())
    }

    #[test]
    fn test_vec_remove_dups() -> Result<(), String> {
        let mut vecx: Vec<usize> = vec![1, 2, 3, 1, 4, 2, 2];
        vec_remove_dups(&mut vecx, |a, b| a == b);
        assert_eq!(vecx, vec![1, 2, 3, 4]);

        let mut vecx: Vec<&str> = vec!["A", "B", "A", "C", "B"];
        vec_remove_dups(&mut vecx, |a, b| a == b);
        assert_eq!(vecx, vec!["A", "B", "C"]);

        vec_remove_dups(&mut Vec::<usize>::new(), |a, b| a == b);

        Ok(())
    }

    #[test]
    fn test_vec_contains() -> Result<(), String> {
        let vecx: Vec<usize> = vec![1, 2, 3, 4];
        (assert_eq!(vec_contains(&vecx, &2, |a, b| a == b), true));
        (assert_eq!(vec_contains(&vecx, &5, |a, b| a == b), false));

        let vecx: Vec<&str> = vec!["A", "B", "C", "D"];
        (assert_eq!(vec_contains(&vecx, &"B", |a, b| a == b), true));
        (assert_eq!(vec_contains(&vecx, &"E", |a, b| a == b), false));

        assert_eq!(vec_contains(&vec![], &"E", |a, b| a == b), false);
        Ok(())
    }
}