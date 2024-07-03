//! The StateStore struct. A vector of SomeState structs.

use crate::bits::vec_same_num_bits;
use crate::region::SomeRegion;
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
        debug_assert!(vec_same_num_bits(&avec));

        Self { avec }
    }

    /// Add a state to a StateStore.
    pub fn push(&mut self, val: SomeState) {
        debug_assert!(self.is_empty() || val.num_bits() == self.avec[0].num_bits());

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

    /// Return true if a StateStore contains a state.
    pub fn contains(&self, stax: &SomeState) -> bool {
        debug_assert!(self.is_empty() || stax.num_bits() == self.avec[0].num_bits());

        self.avec.contains(stax)
    }

    /// Return the number of states.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return states in a given region.
    pub fn in_reg(&self, targ_reg: &SomeRegion) -> Self {
        debug_assert!(self.is_empty() || targ_reg.num_bits() == self.avec[0].num_bits());

        let mut ret_stas = Self::new(vec![]);

        for stax in self.avec.iter() {
            if targ_reg.is_subset_of(stax) {
                ret_stas.push(stax.clone());
            }
        }
        ret_stas
    }
} // end impl StateStore

impl Index<usize> for StateStore {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.avec[i]
    }
}
