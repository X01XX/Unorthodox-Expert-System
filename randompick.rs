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

    /// Pick a random item from a RandomPick vector.
    /// If the item chosen is not at the end, the values will be swapped. 
    /// Return the last item.
    pub fn pick(&mut self) -> Option<usize> {
        if self.items.len() == 0 {
            return None;
        }

        // Make a random pick
        let inx = rand::thread_rng().gen_range(0, self.items.len());

        let anumber = self.items[inx];

        remove_unordered(&mut self.items, inx);

        Some(anumber)
    }
} // End RandomPick

/// Get a random choice of a number of unique numbers (num_results) to a
/// given number of positions, 0, 1 .. -> the_len (exclusive).
/// random 2 of 5 -> [0, 3]
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
