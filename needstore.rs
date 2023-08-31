//! The NeedStore struct, a vector of SomeNeed structs.

use crate::need::SomeNeed;
use crate::removeunordered;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index; // IndexMut
use std::slice::{Iter, IterMut};

impl fmt::Display for NeedStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct NeedStore {
    /// A vector of SomeNeed instances.
    pub avec: Vec<SomeNeed>,
}

impl NeedStore {
    /// Return a new NeedStore instance.
    pub fn new(avec: Vec<SomeNeed>) -> Self {
        Self { avec }
    }

    /// Return a new NeedStore with a given capacity.
    pub fn with_capacity(size: usize) -> Self {
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

    /// Add a need to the vector.
    pub fn push(&mut self, val: SomeNeed) {
        self.avec.push(val);
    }

    /// Extend a NeedStore by emptying another NeedStore..
    pub fn append(&mut self, mut other: Self) {
        self.avec.append(&mut other.avec);
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

    /// Sort needs by priority.
    pub fn sort_by_priority(&mut self) {
        self.avec.sort_by_key(|ndx| ndx.priority());
    }

    /// Return a String representation of a NeedStore.
    pub fn formatted_string(&self) -> String {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n[");

        for needx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n ");
            }
            rc_str.push_str(&format!("{}", &needx));
            flg = 1;
        }
        rc_str.push(']');
        rc_str
    }
} // end impl NeedStore

impl Index<usize> for NeedStore {
    type Output = SomeNeed;
    fn index(&self, i: usize) -> &SomeNeed {
        &self.avec[i]
    }
}
