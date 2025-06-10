#![allow(dead_code)]
/// Given a vector of vectors, of elements that have the Copy trait, return all possible
/// choices of any 1 of each vector.
///
/// let options = any_one_of(&[[0], [1, 2, 3, 4], [5, 6]]);
/// [[0, 1, 5], [0, 2, 5], [0, 3, 5], [0, 4, 5], [0, 1, 6], [0, 2, 6], [0, 3, 6], [0, 4, 6]]
///
/// An interesting regularity is that the first option is always a list of all first elements
/// of each vector.
/// ########################################################################################
pub fn any_one_of<T: Copy>(tvec: &Vec::<Vec::<T>>) -> Vec::<Vec::<T>> {

    // Calc number of options.
    let num_options = any_one_of_result_len(tvec);
    //println!("num options is {num_options}");

    if num_options == 0 {
        panic!("zero length vector?");
    }
    
    // Init first vector of vectors.
    let mut options = Vec::<Vec::<T>>::with_capacity(num_options);
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

/// Calc the number of options in the final result.
/// ###############################################
fn any_one_of_result_len<T>(avec: &Vec<Vec::<T>>) -> usize {
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
/// ###############################################################################################
fn add_one_of_next<T: Copy>(avec: &[Vec::<T>], options: &Vec::<Vec::<T>>) -> Vec::<Vec::<T>> {

    let mut next_options = Vec::<Vec::<T>>::with_capacity(options.len() * avec[0].len());

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

            // Save resutl for next cycle.
            next_options.push(tmp);
        }
    }
    
    if avec.len() == 1 {
        return next_options;
    }

    add_one_of_next(&avec[1..], &next_options)
}
