//! The StateStore struct. A vector of SomeState structs.

use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;

use std::fmt;

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.avec))
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct StateStore {
    /// A vector of states.
    avec: Vec<SomeState>,
}

impl StateStore {
    /// Return a new, empty, StateStore instance.
    pub fn new(avec: Vec<SomeState>) -> Self {
        Self { avec }
    }

    /// Return a new, empty, StateStore instance, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeState>::with_capacity(num),
        }
    }

    /// Return the number of states in a StateStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a state to a StateStore.
    pub fn push(&mut self, val: SomeState) {
        self.avec.push(val);
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Return an immuable iterator.
    pub fn iter(&self) -> Iter<SomeState> {
        self.avec.iter()
    }

    /// Return true if a StateStore contains a given state.
    pub fn contains(&self, stax: &SomeState) -> bool {
        self.avec.contains(stax)
    }
} // end impl StateStore

impl Index<usize> for StateStore {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.avec[i]
    }
}
