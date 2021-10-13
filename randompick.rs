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

extern crate rand;
use rand::Rng;

#[derive(Debug)]
pub struct RandomPick {
    items: Vec<usize>,
}

impl RandomPick {
    /// Return a new instance of a RandomPick struct.
    /// The argument is GT zero, the vector with be populated with that range of numbers, starting at zero.
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

    /// Return the current pseudo length of the vector.
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

        let last_inx = self.items.len() - 1;

        if inx != last_inx {
            let tmp = self.items[inx];
            self.items[inx] = self.items[last_inx];
            self.items[last_inx] = tmp;
        }

        self.items.pop()
    }
} // End RandomPick