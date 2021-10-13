//! Implement a random pick vector
//!
//! This is meant to allow a set of numbers to be chosen randomly,
//! while allowing the remaining numbers to be chosen later,
//! potentially until all numbers are chosen.
//
// Example
// ..
// Cargo.toml
// [dependencies]
// rand = "0.7.3"
//
// main.rs
// mod randompick;
// use crate::randompick::RandomPick;
// ..
// let avec = vec![20, 21, 22, 23, 24, 254, 26]; // <a vector of options>
// let mut rp1 = RandomPick::new(avec.len());    // put numbers 0..avec.len() into a vector.
//
// Randomly pick three numbers, as indicies into the original vector
// ..
// for _ in 0..3 {
//     let inx = rp1.pick().unwrap();
//     println!("num picked is {} value is {}", inx, avec[inx]); 
// }
// ..
// num picked is 2 value is 22
// num picked is 0 value is 20
// num picked is 4 value is 24

extern crate rand;
use rand::Rng;

#[derive(Debug)]
pub struct RandomPick {
    items: Vec<usize>,
}

impl RandomPick {
    /// Return a new instance of a RandomPick struct.
    /// If the argument is GT zero, the vector with that range of numbers starting at zero.
    pub fn new(nums: usize) -> Self {
        let mut ret = RandomPick {
            items: Vec::<usize>::new(),
        };

        if nums > 0 {
            for i in 0..nums {
                ret.items.push(i);
            }
        }

        return ret;
    }

    /// Return the current pseudo length of the vector.
    pub fn len(&self) -> usize {
        return self.items.len();
    }

    /// Add a number to the RandomPick store.
    /// At least one number must be added before the first pick.
    /// A number, or numbers, can be added after a pick.
    pub fn _push(&mut self, newnum: usize) {
        self.items.push(newnum);
    }

    /// Pick a random item from a RandomPick vector.
    /// If the item chosen is not at the end, the values will be swapped. 
    /// Return the last item.
    pub fn pick(&mut self) -> Option<usize> {
        if self.items.len() == 0 {
            return None;
        }

        // Make a random pick
        let inx = rand::thread_rng().gen_range(0, self.len());

        let last_inx = self.items.len() - 1;

        if inx != last_inx {
            let tmp = self.items[inx];
            self.items[inx] = self.items[last_inx];
            self.items[last_inx] = tmp;
        }

        self.items.pop()
    }
} // End RandomPick
