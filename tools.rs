use crate::bits::NumBits;
use std::fmt;

/// Return true if a vector contains an item, that passes a test as the first argument of a given function, and a second given item.
pub fn vec_contains<T, U>(avec: &[T], testfn: fn(&T, &U) -> bool, item: &U) -> bool {
    for itemx in avec.iter() {
        if testfn(itemx, item) {
            return true;
        }
    }
    false
}

/// Return true if a vector contains the same reference given.
pub fn vec_contains_ref<T>(avec: &[&T], item: &T) -> bool {
    for itemx in avec.iter() {
        if std::ptr::eq(*itemx, item) {
            return true;
        }
    }
    false
}

/// Return a vector of references to items in a vector.
//pub fn ref_vec<T>(avec: &[T]) -> Vec<&T> {
//    let mut ref_vec = Vec::<&T>::with_capacity(avec.len());
//    for itemx in avec.iter() {
//        ref_vec.push(itemx);
//    }
//    ref_vec
//}

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

    for (inx, itmx) in avec.iter().enumerate() {
        if inx > 0 {
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

    for (inx, itmx) in avec.iter().enumerate() {
        if inx > 0 {
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

/// Return true if corresponding items in two vectors have the same number of bits.
pub fn corresponding_num_bits(one: &impl AvecRef, other: &impl AvecRef) -> bool {
    for (item1, item2) in one.avec_ref().iter().zip(other.avec_ref().iter()) {
        if item1.num_bits() != item2.num_bits() {
            return false;
        }
    }
    true
}

pub trait AvecRef {
    fn avec_ref(&self) -> &Vec<impl NumBits>;
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

// Return a vector of references, from a given vector.
#[allow(dead_code)]
pub fn vec_refs<T>(avec: &[T]) -> Vec<&T> {
    let mut ret = Vec::<&T>::with_capacity(avec.len());

    for x in avec.iter() {
        ret.push(x);
    }
    ret
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::SomeState;

    #[test]
    fn test_anyxofn() -> Result<(), String> {
        let vals = vec![0, 1, 2, 3];
        let vals_refs = vec_refs(&vals);
        let options = anyxofn(3, &vals_refs);
        println!("{:?}", options);
        assert!(options.len() == 4);

        assert!(options.contains(&vec![&0, &1, &2]));
        assert!(options.contains(&vec![&0, &1, &3]));
        assert!(options.contains(&vec![&0, &2, &3]));
        assert!(options.contains(&vec![&1, &2, &3]));

        let options = anyxofn(1, &vals_refs);
        println!("{:?}", options);
        assert!(options.len() == 4);
        assert!(options.contains(&vec![&0]));
        assert!(options.contains(&vec![&1]));
        assert!(options.contains(&vec![&2]));
        assert!(options.contains(&vec![&3]));

        let sta1 = SomeState::new_from_string("0x1")?;
        let sta2 = SomeState::new_from_string("0x2")?;
        let sta4 = SomeState::new_from_string("0x4")?;
        let sta7 = SomeState::new_from_string("0x7")?;

        let vals = vec![&sta1, &sta2, &sta4, &sta7];
        let options: Vec<Vec<&SomeState>> = anyxofn(2, &vals);
        for optx in options.iter() {
            println!("{}", vec_ref_string(&optx));
        }

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn test_vec_contains() -> Result<(), String> {
        let vecx: Vec<usize> = vec![1, 2, 3, 4];
        println!("vecx: {:?}", vecx);
        assert_eq!(vec_contains(&vecx, |a, b| a == b, &2), true);
        assert_eq!(vec_contains(&vecx, |a, b| a == b, &5), false);

        let vecx: Vec<&str> = vec!["A", "B", "C", "D"];
        println!("vecx: {:?}", vecx);
        assert_eq!(vec_contains(&vecx, |a, b| a == b, &"B"), true);
        assert_eq!(vec_contains(&vecx, |a, b| a == b, &"E"), false);

        assert_eq!(
            vec_contains(&Vec::<&str>::new(), |a, b| a == b, &"E"),
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
