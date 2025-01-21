use crate::bits::NumBits;
use crate::mask::SomeMask;
use crate::state::SomeState;
use std::fmt;
use unicode_segmentation::UnicodeSegmentation;

/// Return true if a vector contains an item, that passes a test as the first argument of a given function, and a second given item.
pub fn vec_contains<T, U>(avec: &[T], testfn: fn(&T, &U) -> bool, item: &U) -> bool {
    for itemx in avec.iter() {
        if testfn(itemx, item) {
            return true;
        }
    }
    false
}

/// Remove an element from a vector, not caring about order, avoid the
/// copying that the remove command does.
/// If iterating over a vector, accumulating a vector of indices to remove,
/// afterwards call this with indices in descending order.
pub fn remove_unordered<T>(avec: &mut Vec<T>, inx: usize) {
    assert!(inx < avec.len());
    let last_item = avec.pop().unwrap();

    if inx < avec.len() {
        // avec.len() is now equal to the index of the last item before the pop operation.
        avec[inx] = last_item;
    }
}

/// Implement a random pick vector
///
/// This is meant to allow a set of numbers to be chosen randomly,
/// while allowing the remaining numbers to be chosen later,
/// potentially until all numbers are chosen.
///
use rand::Rng;

#[derive(Debug)]
pub struct RandomPick {
    items: Vec<usize>,
}

impl RandomPick {
    /// Return a new instance of a RandomPick struct.
    /// The argument should be GT zero, the vector with be populated with that range of numbers, starting at zero.
    pub fn new(number_items: usize) -> Self {
        assert!(number_items > 0);

        Self {
            items: (0..number_items).collect(),
        }
    }

    /// Pick a random item from a RandomPick vector.
    /// If the item chosen is not at the end, the values will be swapped.
    /// Return the last item.
    pub fn pick(&mut self) -> Option<usize> {
        if self.items.is_empty() {
            return None;
        }

        // Make a random pick
        let inx = rand::thread_rng().gen_range(0..self.items.len());

        let anumber = self.items[inx];

        remove_unordered(&mut self.items, inx);

        Some(anumber)
    }
} // End RandomPick

/// Return a string representing a vector of items.
pub fn vec_string<T: fmt::Display + StrLen>(avec: &[T]) -> String {
    let mut len = 2; // Length of brackets.
    if avec.is_empty() {
    } else {
        // Length of items may be different, as in corresponding region, or state, vectors.
        for itmx in avec.iter() {
            len += itmx.strlen(); // Length of item.
        }
        len += (avec.len() - 1) * 2; // Length of separators.
    }
    let mut rc_str = String::with_capacity(len);
    rc_str.push('[');

    let mut first = true;
    for itmx in avec.iter() {
        if first {
            first = false;
        } else {
            rc_str.push_str(", ");
        }
        rc_str.push_str(&itmx.to_string());
    }

    rc_str.push(']');

    rc_str
}

/// Return a string representing a vector of references to items.
pub fn vec_ref_string<T: fmt::Display + StrLen>(avec: &[&T]) -> String {
    let mut len = 2; // Length of brackets.
    if avec.is_empty() {
    } else {
        // Length of items may be different, as in corresponding region, or state, vectors.
        for itmx in avec.iter() {
            len += itmx.strlen(); // Length of item.
        }
        len += (avec.len() - 1) * 2; // Length of separators.
    }
    let mut rc_str = String::with_capacity(len);
    rc_str.push('[');

    let mut first = true;
    for itmx in avec.iter() {
        if first {
            first = false;
        } else {
            rc_str.push_str(", ");
        }
        rc_str.push_str(&itmx.to_string());
    }

    rc_str.push(']');

    rc_str
}

/// Define the StrLen trait, so structs can return their expected string length for display.
pub trait StrLen {
    fn strlen(&self) -> usize;
}

/// Define a trail that allows getting a reference to vector of items that implemant the NumBits trait.
pub trait AvecRef {
    fn avec_ref(&self) -> &Vec<impl NumBits>;
}

/// Define the AccessStates trait, so operations on Regions, States, Squares and Groups are smoother.
/// A region defined by a single state, is similar to a single state.
pub trait AccessStates {
    fn one_state(&self) -> bool;
    fn first_state(&self) -> &SomeState;
    fn x_mask(&self) -> SomeMask;
    fn edge_mask(&self) -> SomeMask;
    fn high_state(&self) -> SomeState;
    fn low_state(&self) -> SomeState;
    fn diff_edge_mask(&self, other: &impl AccessStates) -> SomeMask;
    fn intersects(&self, other: &impl AccessStates) -> bool;
    fn is_subset_of(&self, other: &impl AccessStates) -> bool;
    fn is_superset_of(&self, other: &impl AccessStates) -> bool;
    fn num_bits(&self) -> usize;
}

/// Define a trait that allows checking correspondance with bit_nums used by items.
pub trait CorrespondingItems {
    fn num_bits_vec(&self) -> Vec<usize>;
}

/// Given a number of items, return any x number
/// of unique combinations, where x >= 0 and <= number items,
/// order does not matter.
///
/// Call with num_items = Number of items in possible combinations.
///           items     = Items references to find combinations of.
///
/// Number lists returned = N! / ((N-x)! * x!)
///
/// e.g. any 3 of 4 items.
///
/// 4! / (4-3)! * 3! = 4! / 1!3! = 24 / 6 = 4
///
/// = ((&0, &1, &2) (&0, &1, &3) (&0, &2, &3) (&1, &2, &3))
///
/// ################################################################
pub fn anyxofn<'a, T>(xitems: usize, nitems: &'a [&T]) -> Vec<Vec<&'a T>> {
    assert!(xitems <= nitems.len());

    if xitems == 0 {
        return vec![];
    }

    // Call anyxofn2 with a vector of possible items.
    anyxofn2(xitems, &[], nitems)
}

/// Continue making possible combinations.
/// ######################################
fn anyxofn2<'a, T>(xitems: usize, xlist: &[&'a T], nlist: &[&'a T]) -> Vec<Vec<&'a T>> {
    let mut ret_vec = Vec::<Vec<&T>>::new();

    if xitems < 1 || xitems > nlist.len() {
        return vec![xlist.to_vec()];
    }

    let numx = nlist.len() - xitems;

    for x in 0..(numx + 1) {
        let toright = &nlist[x + 1..].to_vec();

        let mut listz = xlist.to_vec();

        listz.push(nlist[x]);

        let avec = anyxofn2(xitems - 1, &listz, toright);

        for avecx in avec {
            ret_vec.push(avecx);
        }
    }

    ret_vec
}

/// Return a vector of references, from a given vector.
pub fn vec_refs<T>(avec: &[T]) -> Vec<&T> {
    let mut ret = Vec::<&T>::with_capacity(avec.len());

    for x in avec.iter() {
        ret.push(x);
    }
    ret
}

// Remove comments from a string.
pub fn remove_comments(str_in: &str) -> String {
    let mut str_out = String::new(); // String to return.
    let mut line = String::new(); // Current line being processed.
    let mut last_chr = false; // True if the last character was a slash.

    let mut skip_to_end = false; // Set when a comment is detected, to stop adding characters to the line.
                                 // Until end-of-line or end-of-file.

    for chr in str_in.graphemes(true) {
        if skip_to_end {
            if chr == "\n" {
                if !line.is_empty() {
                    line.push('\n');
                    str_out.push_str(&line);
                    line = String::new();
                }
                skip_to_end = false;
                last_chr = false;
            }
            continue;
        }

        // Check if "//" has been detected.
        if chr == "/" && last_chr {
            line.remove(line.len() - 1); // remove the first slash character from the line.
            skip_to_end = true;
            continue;
        }

        // Accumulate characters in the line.
        line.push_str(chr);

        // End of line reached without finding a comment.
        if chr == "\n" {
            str_out.push_str(&line);
            line = String::new();
            last_chr = false;
            continue;
        }

        // Save char flag for check in next loop cycle.
        last_chr = chr == "/";
    } // next chr.

    // Check for line at end, without a trailing newline.
    if !line.is_empty() {
        str_out.push_str(&line);
    }
    // Return the string with comments removed.
    str_out
}

/// Parse an input string into tokens.
/// Separators are spaces and commas, between groups of balanced (or none) brackets.
pub fn parse_input(str_in: &str) -> Result<Vec<String>, String> {
    //println!("tools::parse_input: {str_in}");
    let mut left = 0;
    let mut right = 0;
    let mut tokens = vec![];
    let mut tmp_token = String::new();

    for chr in str_in.chars() {
        if chr == '\n' {
            continue;
        }
        if chr == '[' {
            left += 1;
            tmp_token.push(chr);
            continue;
        }
        if chr == ']' {
            right += 1;
            tmp_token.push(chr);
            if right > left {
                return Err("tools::parse_input: unbalanced brackets".to_string());
            }
            continue;
        }

        if chr == ' ' || chr == ',' {
            if tmp_token.is_empty() {
                continue;
            }
            if left == right {
                tokens.push(tmp_token);
                tmp_token = String::new();
                continue;
            }
        }

        tmp_token.push(chr);
    }
    if left != right {
        return Err("tools::parse_input: unbalanced brackets".to_string());
    }
    if tmp_token.is_empty() {
    } else {
        tokens.push(tmp_token);
    }

    Ok(tokens)
}

/// Given a vector of vectors, of elements that have the Copy trait, return all possible
/// choices of any 1 of each vector.
///
/// let options = any_one_of(&[[0], [1, 2, 3, 4], [5, 6]]);
/// [[0, 1, 5], [0, 2, 5], [0, 3, 5], [0, 4, 5], [0, 1, 6], [0, 2, 6], [0, 3, 6], [0, 4, 6]]
///
/// An interesting regularity is that the first option is always a list of all first elements
/// of each vector.
/// ########################################################################################
pub fn any_one_of_each<T: Copy>(tvec: &[Vec<T>]) -> Vec<Vec<T>> {
    // Calc number of options.
    let num_options = any_one_of_result_len(tvec);

    if num_options == 0 {
        panic!("zero length vector?");
    }

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

/// Calc the number of options expected in the final result.
/// ########################################################
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
/// ###############################################################################################
fn add_one_of_next<T: Copy>(avec: &[Vec<T>], options: &[Vec<T>]) -> Vec<Vec<T>> {
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
    use crate::state::SomeState;
    use std::str::FromStr;

    #[test]
    fn test_any_one_of_each() -> Result<(), String> {
        let avec: Vec<Vec<usize>> = vec![vec![0], vec![1, 2, 3, 4], vec![5, 6]];

        println!("For {avec:?}");
        let options = any_one_of_each(&avec);

        println!(
            "options {:?} len {} calced {}",
            &options,
            options.len(),
            any_one_of_result_len(&avec)
        );

        assert!(options.len() == any_one_of_result_len(&avec));
        assert!(format!("{:?}", options) == "[[0, 1, 5], [0, 2, 5], [0, 3, 5], [0, 4, 5], [0, 1, 6], [0, 2, 6], [0, 3, 6], [0, 4, 6]]");

        Ok(())
    }

    #[test]
    fn parse_input() -> Result<(), String> {
        let tokens = match super::parse_input(&"arg1, arg2, arg3".to_string()) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(errstr),
        };
        println!("tokens {:?}", tokens);
        assert!(tokens.len() == 3);
        assert!(tokens[0] == "arg1");
        assert!(tokens[1] == "arg2");
        assert!(tokens[2] == "arg3");

        let tokens =
            match super::parse_input(&"arg1, [arg2, arg3], [arg4, [arg5, arg6]], ".to_string()) {
                Ok(tokenvec) => tokenvec,
                Err(errstr) => return Err(errstr),
            };
        println!("tokens {:?}", tokens);
        assert!(tokens.len() == 3);
        assert!(tokens[0] == "arg1");
        assert!(tokens[1] == "[arg2, arg3]");
        assert!(tokens[2] == "[arg4, [arg5, arg6]]");

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn anyxofn() -> Result<(), String> {
        let vals = vec![&0, &1, &2, &3];
        let options = super::anyxofn(3, &vals);
        println!("{:?}", options);
        assert!(options.len() == 4);

        assert!(options.contains(&vec![&0, &1, &2]));
        assert!(options.contains(&vec![&0, &1, &3]));
        assert!(options.contains(&vec![&0, &2, &3]));
        assert!(options.contains(&vec![&1, &2, &3]));

        let options = super::anyxofn(1, &vals);
        println!("{:?}", options);
        assert!(options.len() == 4);
        assert!(options.contains(&vec![&0]));
        assert!(options.contains(&vec![&1]));
        assert!(options.contains(&vec![&2]));
        assert!(options.contains(&vec![&3]));

        let sta1 = SomeState::from_str("s0001")?;
        let sta2 = SomeState::from_str("s0010")?;
        let sta4 = SomeState::from_str("s0100")?;
        let sta7 = SomeState::from_str("s0111")?;

        let vals = vec![&sta1, &sta2, &sta4, &sta7];
        let options: Vec<Vec<&SomeState>> = super::anyxofn(2, &vals);
        for optx in options.iter() {
            println!("{}", vec_ref_string(&optx));
        }
        assert!(options.len() == 6);
        assert!(options.contains(&vec![&sta1, &sta2]));
        assert!(options.contains(&vec![&sta1, &sta4]));
        assert!(options.contains(&vec![&sta1, &sta7]));
        assert!(options.contains(&vec![&sta2, &sta4]));
        assert!(options.contains(&vec![&sta2, &sta7]));
        assert!(options.contains(&vec![&sta4, &sta7]));

        Ok(())
    }

    #[test]
    fn vec_contains() -> Result<(), String> {
        let vecx: Vec<usize> = vec![1, 2, 3, 4];
        println!("vecx: {:?}", vecx);
        assert_eq!(super::vec_contains(&vecx, |a, b| a == b, &2), true);
        assert_eq!(super::vec_contains(&vecx, |a, b| a == b, &5), false);

        let vecx: Vec<&str> = vec!["A", "B", "C", "D"];
        println!("vecx: {:?}", vecx);
        assert_eq!(super::vec_contains(&vecx, |a, b| a == b, &"B"), true);
        assert_eq!(super::vec_contains(&vecx, |a, b| a == b, &"E"), false);

        assert_eq!(
            super::vec_contains(&Vec::<&str>::new(), |a, b| a == b, &"E"),
            false
        );
        Ok(())
    }

    #[test]
    //  Randomly pick numbers, as indices into a vector, until the vector value satisfies some criteria.
    fn pick_an_odd_number() -> Result<(), String> {
        // Init a number vector.
        let avec = vec![20, 21, 22, 23, 24, 254, 26]; // <a vector of options>

        // Init a RandomPick struct, with the range 0..avec.len().
        let mut rp1 = RandomPick::new(avec.len());

        // Pick index numbers until the vector item is an odd number.
        while let Some(inx) = rp1.pick() {
            println!("num picked is {} vector value is {}", inx, avec[inx]);
            if avec[inx] % 2 == 1 {
                return Ok(());
            }
        }
        Err("No odd  number was found".to_string())
    }
}
