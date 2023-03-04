//! Implement a random pick vector
//!
//! This is meant to allow a set of numbers to be chosen randomly,
//! while allowing the remaining numbers to be chosen later,
//! potentially until all numbers are chosen.
//!

use crate::removeunordered::remove_unordered;
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
        let mut ret = RandomPick {
            items: Vec::<usize>::with_capacity(number_items),
        };

        for num in 0..number_items {
            ret.items.push(num);
        }

        ret
    }

    /// Return the current length of the vector.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
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

/// Get a random choice of a number of unique numbers in a range.
/// e.g. random_x_of_n(2, 5) -> [0, 3]
pub fn random_x_of_n(x: usize, n: usize) -> Vec<usize> {
    if x < 1 || x >= n {
        panic!(
            "random_x_of_n: Number results {} is not right for length {}",
            &x, &n
        );
    }

    let mut x_vec = Vec::<usize>::with_capacity(x);

    let mut rp1 = RandomPick::new(n);

    for _ in 0..x {
        x_vec.push(rp1.pick().unwrap());
    }

    x_vec
} // end random_x_of_n

#[cfg(test)]
mod tests {
    use super::*;

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
        Err("No odd number was found".to_string())
    }
}
