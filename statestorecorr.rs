//! The StateStoreCorr struct. A vector of SomeState structs,
//! corresponding to the damainstore vector.
//! States will use the same number of bits as the corresponding domain,
//! which may be different from other states in the vector.
use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::ops::{Index, IndexMut};
use std::slice::Iter;

use std::fmt;

impl fmt::Display for StateStoreCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.avec))
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct StateStoreCorr {
    /// A vector of states.
    avec: Vec<SomeState>,
}

impl StateStoreCorr {
    /// Return a new, empty, StateStoreCorr instance.
    pub fn new(avec: Vec<SomeState>) -> Self {
        Self { avec }
    }

    /// Return a new StateStoreCorr instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeState>::with_capacity(num),
        }
    }

    /// Return the number of states.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a state to a StateStoreCorr.
    pub fn push(&mut self, val: SomeState) {
        self.avec.push(val);
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return an immuable iterator.
    pub fn iter(&self) -> Iter<SomeState> {
        self.avec.iter()
    }
} // end impl StateStoreCorr

impl Index<usize> for StateStoreCorr {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.avec[i]
    }
}

impl IndexMut<usize> for StateStoreCorr {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}
