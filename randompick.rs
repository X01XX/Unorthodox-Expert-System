//! Implement a random pick vector
//!
//! This is meant to allow a set of numbers to be chosen randomly,
//! while allowing the remaining numbers to be chosen later,
//! potentially until all numbers are chosen.
//
// Example
// ..
// Cargo.toml
//  [dependencies]
//  rand = "0.7.3"
//
// main.rs
//  mod randompick;
//  use crate::randompick::RandomPick;
//
//  let avec = vec![20, 21, 22, 23, 24, 254, 26]; // <a vector of options>
//  let mut rp1 = RandomPick::new(avec.len());    // put numbers 0..avec.len() into a vector.
//
//  Randomly pick three numbers, as indicies into the vector
//
//  for _ in 0..3 {
//      let inx = rp1.pick().unwrap();
//      println!("num picked is {} vector value is {}", inx, avec[inx]); 
//  }
// ..
// num picked is 2 vector value is 22
// num picked is 0 vector value is 20
// num picked is 4 vector value is 24

use crate::removeunordered::remove_unordered;
extern crate rand;
use rand::Rng;

#[derive(Debug)]
pub struct RandomPick {
    items: Vec<usize>,
}

impl RandomPick {
    /// Return a new instance of a RandomPick struct.
    /// The argument should be GT zero, the vector with be populated with that range of numbers, starting at zero.
    pub fn new(nums: usize) -> Self {
        assert!(nums > 0);
        let mut ret = RandomPick {
            items: Vec::<usize>::with_capacity(nums),
        };

        for i in 0..nums {
            ret.items.push(i);
        }

        return ret;
    }

    /// Return the current length of the vector.
    pub fn len(&self) -> usize {
        return self.items.len();
    }

    /// Pick a random item from a RandomPick vector.
    /// If the item chosen is not at the end, the values will be swapped. 
    /// Return the last item.
    pub fn pick(&mut self) -> Option<usize> {
        if self.items.len() == 0 {
            return None;
        }

        // Make a random pick
        let inx = rand::thread_rng().gen_range(0, self.items.len());

        let tmp = self.items[inx];

        remove_unordered(&mut self.items, inx);

        Some(tmp)
    }
} // End RandomPick

/// Get a random choice of a number of unique numbers (num_results) to a
/// given number of positions, 0, 1 .. -> the_len (exclusive).
/// random 2 of 5 -> [0, 3]
    pub fn random_x_of_n(num_results: usize, the_len: usize) -> Vec<usize> {

    if num_results < 1 || num_results >= the_len {
        panic!(
            "random_x_of_n: Number results {} is not right for length {}",
            &num_results, &the_len
        );
    }

    let mut yvec = Vec::<usize>::with_capacity(num_results);

    let mut rp1 = RandomPick::new(the_len);

    for _ in 0..num_results {
        yvec.push(rp1.pick().unwrap());
    }

    yvec
} // end random_x_of_n
