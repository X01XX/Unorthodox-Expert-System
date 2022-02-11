//! The NeedStore struct, a vector of SomeNeed structs.

use crate::need::SomeNeed;
use crate::region::SomeRegion;
use crate::removeunordered::remove_unordered;

use std::fmt;
use std::ops::Index; // IndexMut
use std::slice::{ Iter, IterMut};

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

    /// Return the length of the SomeNeed vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if a need is already in a NeedStore.
    pub fn contains(&self, aneed: &SomeNeed) -> bool {
        self.avec.contains(aneed)
    }

    /// Return true if a need with a given type and target is in a NeedStore.
    /// Used in tests.rs, so far.
    pub fn contains_similar_need(&self, type_string: &str, target: &SomeRegion) -> bool {
        for nedx in &self.avec {
            if nedx.type_string() == type_string {
                if nedx.target() == *target {
                    return true;
                }
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
        self.avec.append(&mut other.avec);  // empties other.avec
    }

    /// Return a mutable iterator.
    pub fn iter_mut(&mut self) -> IterMut<SomeNeed> {
        self.avec.iter_mut()
    }

    /// Return a vactor iterator.
    pub fn iter(&self) -> Iter<SomeNeed> {
        self.avec.iter()
    }

    /// Remove a need from a NeedStore
    pub fn remove_unordered(&mut self, inx: usize) {
        remove_unordered(&mut self.avec, inx);
    }

} // end impl NeedStore

impl Index<usize> for NeedStore {
    type Output = SomeNeed;
    fn index<'a>(&'a self, i: usize) -> &'a SomeNeed {
        &self.avec[i]
    }
}
