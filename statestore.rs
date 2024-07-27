//! The StateStore struct. A vector of SomeState structs.
//! Duplicates are suppressed.

use crate::bits::vec_same_num_bits;
use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;

use std::fmt;

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.items))
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct StateStore {
    /// A vector of states.
    items: Vec<SomeState>,
}

impl StateStore {
    /// Return a new, empty, StateStore instance.
    pub fn new(items: Vec<SomeState>) -> Self {
        debug_assert!(vec_same_num_bits(&items));

        let mut ret = Self {
            items: Vec::<SomeState>::with_capacity(items.len()),
        };
        for stax in items {
            ret.push(stax);
        }
        ret
    }

    /// Add a state to a StateStore.
    pub fn push(&mut self, val: SomeState) {
        debug_assert!(self.is_empty() || val.num_bits() == self.items[0].num_bits());

        if !self.contains(&val) {
            self.items.push(val);
        }
    }

    /// Push a state if it is not between any pair of states already in the store.
    pub fn push_no_between(&mut self, sta: SomeState) {
        if self.len() < 2 {
            self.push(sta);
            return;
        }
        // Check if the new state is between any states.
        for inx in 0..(self.len() - 1) {
            for iny in (inx + 1)..self.len() {
                if sta.is_between(&self[inx], &self[iny]) {
                    return;
                }
            }
        }
        // Check if the new state causes other states to be between it and another state.
        let mut remv = Vec::<usize>::new();
        for (inx, stax) in self.items.iter().enumerate() {
            for (iny, stay) in self.items.iter().enumerate().skip(1) {
                if iny != inx && stay.is_between(&sta, stax) && !remv.contains(&iny) {
                    remv.push(iny);
                }
            }
        }
        if remv.is_empty() {
        } else {
            // Sort idicies higher to lower, remove items.
            remv.sort_by(|a, b| b.cmp(a));
            for inx in remv.iter() {
                tools::remove_unordered(&mut self.items, *inx);
            }
        }

        self.push(sta);
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Return an immuable iterator.
    pub fn iter(&self) -> Iter<SomeState> {
        self.items.iter()
    }

    /// Return true if a StateStore contains a state.
    pub fn contains(&self, stax: &SomeState) -> bool {
        debug_assert!(self.is_empty() || stax.num_bits() == self.items[0].num_bits());

        self.items.contains(stax)
    }

    /// Return the number of states.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return a reference to the first state.
    pub fn first(&self) -> Option<&SomeState> {
        if self.items.is_empty() {
            None
        } else {
            Some(&self.items[0])
        }
    }
} // end impl StateStore

impl Index<usize> for StateStore {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.items[i]
    }
}
