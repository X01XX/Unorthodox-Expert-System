//! The NeedStore struct, a vector of SomeNeed structs.

use crate::need::SomeNeed;
use crate::region::SomeRegion;
use crate::removeunordered;

use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing
use std::ops::Index; // IndexMut
use std::slice::{Iter, IterMut};

impl fmt::Display for NeedStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n[");

        for needx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n ");
            }
            let _ = write!(rc_str, "{}", &needx);
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{rc_str}")
    }
}
use serde::{Deserialize, Serialize};
#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct NeedStore {
    /// A vector of SomeNeed instances.
    avec: Vec<SomeNeed>,
}

impl NeedStore {
    /// Return a new NeedStore instance.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeNeed>::new(),
        }
    }

    /// Return a new NeedStore with a given capacity.
    pub fn new_with_capacity(size: usize) -> Self {
        Self {
            avec: Vec::<SomeNeed>::with_capacity(size),
        }
    }

    /// Return the length of the SomeNeed vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Return true if a need is already in a NeedStore.
    pub fn contains(&self, aneed: &SomeNeed) -> bool {
        self.avec.contains(aneed)
    }

    /// Return true if a need with a given type and target is in a NeedStore.
    /// Used in tests.rs, so far.
    pub fn contains_similar_need(&self, name: &str, target: &SomeRegion) -> bool {
        for nedx in &self.avec {
            if nedx.name() == name {
                for targx in nedx.target().iter() {
                    if targx.region == *target {
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Add a need to the vector.
    pub fn push(&mut self, val: SomeNeed) {
        if !self.contains(&val) {
            self.avec.push(val);
        }
    }

    /// Append a Needstore.
    pub fn append(&mut self, other: &mut NeedStore) {
        self.avec.append(&mut other.avec); // empties other.avec
    }

    /// Return a mutable iterator.
    pub fn iter_mut(&mut self) -> IterMut<SomeNeed> {
        self.avec.iter_mut()
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeNeed> {
        self.avec.iter()
    }

    /// Remove a need from a NeedStore
    pub fn remove_unordered(&mut self, inx: usize) {
        removeunordered::remove_unordered(&mut self.avec, inx);
    }
} // end impl NeedStore

impl Index<usize> for NeedStore {
    type Output = SomeNeed;
    fn index(&self, i: usize) -> &SomeNeed {
        &self.avec[i]
    }
}
