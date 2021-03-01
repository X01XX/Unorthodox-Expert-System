//! The NeedStore struct, a vector of SomeNeeds structs.

use crate::need::SomeNeed;
use std::fmt;
use std::ops::Index; // IndexMut
use std::slice::IterMut;

impl fmt::Display for NeedStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n[");

        for stax in &self.avec {
            if flg == 1 {
                rc_str.push_str(&String::from(",\n "));
            }
            rc_str.push_str(&format!("{}", &stax));
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

pub struct NeedStore {
    /// A vector od SomeNeed instances.
    avec: Vec<SomeNeed>,
}

impl NeedStore {
    /// Return a new NeedStore instance.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeNeed>::with_capacity(5),
        }
    }

    //    pub fn new_with_capacity(num: usize) -> Self {
    //        Self {
    //            avec: Vec::<SomeNeed>::with_capacity(num),
    //        }
    //    }

    /// Return the length of the SomeNeed vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if a need is already in a SeedStore.
    pub fn contains(&self, new_need: &SomeNeed) -> bool {
        for nedx in &self.avec {
            if *nedx == *new_need {
                return true;
            }
        }

        false
    }

    /// Add a need to the vector.
    pub fn push(&mut self, val: SomeNeed) {
        if self.contains(&val) == false {
            self.avec.push(val);
        }
    }

    /// Append a Needstore.
    pub fn append(&mut self, other: &mut NeedStore) {
        self.avec.append(&mut other.avec);
    }

    //    pub fn iter(&self) -> Iter<SomeNeed> {
    //        self.avec.iter()
    //    }

    /// Return a mutable iterator.
    pub fn iter_mut(&mut self) -> IterMut<SomeNeed> {
        self.avec.iter_mut()
    }

    //    /// Return the lowest need priority number, gt a given number,
    //    /// and the number of needs with that priority.
    //    pub fn lowest_priority_gt(&self, pri: usize) -> (usize, usize) {
    //
    //		let mut minp: usize = 9999;
    //		let mut num: usize = 0;
    //
    //		for ndx in self.avec.iter() {
    //			let prx = ndx.priority();
    //
    //			if prx < minp {
    //				if prx > pri {
    //					minp = prx;
    //					num = 1;
    //				}
    //			} else if prx == minp {
    //				num += 1;
    //			}
    //		} // next ndx
    //
    //		(minp, num)
    //	}

    //    /// Return a vector of inicies of needs with a given priority.
    //    pub fn inx_pri_eq(&self, pri: usize) -> Vec<usize> {
    //
    //		let mut avec = Vec::<usize>::new();
    //
    //		let mut inx = 0;
    //		for ndx in self.avec.iter() {
    //			if ndx.priority() == pri {
    //                avec.push(inx);
    //			}
    //			inx += 1;
    //		} // next ndx
    //
    //		avec
    //	}
} // end impl NeedStore

impl Index<usize> for NeedStore {
    type Output = SomeNeed;
    fn index<'a>(&'a self, i: usize) -> &'a SomeNeed {
        &self.avec[i]
    }
}
