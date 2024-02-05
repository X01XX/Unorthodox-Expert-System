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
pub fn ref_vec<T>(avec: &[T]) -> Vec<&T> {
    let mut ref_vec = Vec::<&T>::with_capacity(avec.len());
    for itemx in avec.iter() {
        ref_vec.push(itemx);
    }
    ref_vec
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

    for (inx, itmx) in avec.iter().enumerate() {
        if inx > 0 {
            rc_str.push_str(", ");
        }
        rc_str.push_str(&itmx.to_string());
    }

    rc_str.push(']');
    //    if rc_str.len() != len {
    //        println!("{}", rc_str);
    //        panic!("len {} ne calc len {len}", rc_str.len());
    //    }
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
    //    if rc_str.len() != len {
    //        println!("{}", rc_str);
    //        panic!("ref len {} ne calc len {len}", rc_str.len());
    //    }
    rc_str
}

/// Allow more obvious not operation than the exclamation point.
pub fn not(val: bool) -> bool {
    !val
}

/// Define the StrLen trait, so structs can return their expected string length for display.
pub trait StrLen {
    fn strlen(&self) -> usize;
}

#[cfg(test)]
mod tests {
    use super::*;

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
