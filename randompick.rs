//! Implement a random pick vector
//!
//! This is meant to allow numbers to be chosen randomly,
//! while allowing the remaining numbers to be chosen later,
//! potentially until all numbers are chosen.
//
// Example
// ..
// mod randompick;
// use crate::randompick::RandomPick;
// ..
// let avec = vec![20, 21, 22, 23, 24, 254, 26]; // <a vector of options>
// let mut rp1 = RandomPick::new(avec.len());    // put numbers 0..avec.len() into a vector.
//
// Randomly pick three numbers, as indicies into the original vector
// ..
// for _ in 0..3 {
//     let inx = rp1.pick();
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
    length: usize,
}

impl RandomPick {
    /// Return a new instance of a RandomPick struct.
    /// If GT zero, the argument will fill the vector with that range of numbers.
    pub fn _new(nums: usize) -> Self {
        let mut ret = RandomPick {
            items: Vec::<usize>::new(),
            length: 0,
        };

        if nums > 0 {
            for i in 0..nums {
                ret.items.push(i);
            }
            ret.length = nums;
        }

        return ret;
    }

    /// Return the current pseudo length of the vector.
    pub fn _len(&self) -> usize {
        return self.length;
    }

    /// Add a number to the RandomPick store.
    /// The pseudo length will be incremented.
    /// At least one number must be added before the first pick.
    /// A number, or numbers, can be added after a pick.
    pub fn _add(&mut self, newnum: usize) {
        if self.length == self.items.len() {
            self.items.push(newnum);
        } else {
            self.items[self.length] = newnum;
        }
        self.length += 1;
    }

    /// Pick a random item from a RandomPick vector.
    /// If the item chosen is not at the end, the value of the item at the
    /// end will replace the chosen item.  The pseudo length will be decremented.
    pub fn _pick(&mut self) -> usize {
        if self.length == 0 {
            println!("RandomPick::pick: The items vector is empty!");
            return 0;
        }
        if self.length == 1 {
            self.length = 0;
            return self.items[0];
        }
        // Make a random pick

        let inx = rand::thread_rng().gen_range(0, self.length);

        let ret_num = self.items[inx];

        self.length -= 1;

        if inx != self.length {
            self.items[inx] = self.items[self.length];
        }

        return ret_num;
    }
} // End RandomPick
