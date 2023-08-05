/// Return true if a vector contains a given item, given a equal function pointer.
pub fn vec_contains<T>(avec: &[T], item: &T, testfn: fn(&T, &T) -> bool) -> bool {
    for itemx in avec.iter() {
        if testfn(itemx, item) {
            return true;
        }
    }
    false
}

/// Given a non-empty vector, of non-empty vectors, return all possible any-1-of-each combinations.
/// Any one of [[0], [1, 2, 3, 4], [5, 6]] is 1 * 4 * 2 = 8 vectors,
/// [[0, 1, 5], [0, 2, 5], [0, 3, 5], [0, 4, 5], [0, 1, 6], [0, 2, 6], [0, 3, 6], [0, 4, 6]]
/// May also be used with shared references to anything.
pub fn any_one_of_each<T: Copy>(tvec: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    // Sanity checks.
    assert!(!tvec.is_empty());
    for itemx in tvec {
        assert!(!itemx.is_empty());
    }

    // Calc number of options.
    let num_options = any_one_of_result_len(tvec);
    //println!("num options is {num_options}");

    // Init first vector of vectors.
    let mut options = Vec::<Vec<T>>::with_capacity(num_options);
    for numx in tvec[0].iter() {
        let mut newvec = Vec::<T>::with_capacity(tvec.len());
        newvec.push(*numx);
        options.push(newvec);
    }

    if tvec.len() == 1 {
        return options;
    }

    add_one_of_next(&tvec[1..], &options)
}

/// Calc the number of optionsi in the final result.
fn any_one_of_result_len<T>(avec: &[Vec<T>]) -> usize {
    let mut num_options = 1;
    for vecx in avec.iter() {
        num_options *= vecx.len();
    }
    num_options
}

/// Given vectors not yet processed, and the current intermediate results, process one more vector.
/// For each item in the vector to process, copy each intermediate result, and add the item.
/// So the next imtermediate result length is the vector to process length times the intermediate
/// results length.
fn add_one_of_next<T: Copy>(avec: &[Vec<T>], options: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let mut next_options = Vec::<Vec<T>>::with_capacity(options.len() * avec[0].len());

    // Copy each vec in avec, adding an item from the current vector.
    for numx in &avec[0] {
        for vecx in options.iter() {
            // Copy each options vector, and add the current item from the first vector in the
            // slice.
            let mut tmp = Vec::<T>::with_capacity(vecx.len() + 1);
            for itemx in vecx.iter() {
                tmp.push(*itemx);
            }
            tmp.push(*numx);

            // Save result for next cycle.
            next_options.push(tmp);
        }
    }

    if avec.len() == 1 {
        return next_options;
    }

    add_one_of_next(&avec[1..], &next_options)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_contains() -> Result<(), String> {
        let vecx: Vec<usize> = vec![1, 2, 3, 4];
        println!("vecx: {:?}", vecx);
        assert_eq!(vec_contains(&vecx, &2, |a, b| a == b), true);
        assert_eq!(vec_contains(&vecx, &5, |a, b| a == b), false);

        let vecx: Vec<&str> = vec!["A", "B", "C", "D"];
        println!("vecx: {:?}", vecx);
        assert_eq!(vec_contains(&vecx, &"B", |a, b| a == b), true);
        assert_eq!(vec_contains(&vecx, &"E", |a, b| a == b), false);

        assert_eq!(vec_contains(&vec![], &"E", |a, b| a == b), false);
        Ok(())
    }
}
