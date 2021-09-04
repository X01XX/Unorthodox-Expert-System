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

    /// Return a mutable iterator.
    pub fn iter_mut(&mut self) -> IterMut<SomeNeed> {
        self.avec.iter_mut()
    }

} // end impl NeedStore

impl Index<usize> for NeedStore {
    type Output = SomeNeed;
    fn index<'a>(&'a self, i: usize) -> &'a SomeNeed {
        &self.avec[i]
    }
}
