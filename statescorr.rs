//! The StatesCorr struct. A store of SomeState structs,
//! corresponding, in order, to domains in a DomainStore instance.
//!
//! States will use the same number of bits as the corresponding domain,
//! which may be different from other states in the vector.
use crate::bits::NumBits;
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use tools::AvecRef;

use std::fmt;

impl fmt::Display for StatesCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.states)
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct StatesCorr {
    /// A vector of states.
    states: StateStore,
}

impl StatesCorr {
    /// Return a new, empty, StatesCorr instance.
    pub fn new(states: Vec<SomeState>) -> Self {
        debug_assert!(!states.is_empty());
        Self {
            states: StateStore::new(states),
        }
    }

    /// Return a new, empty, StatesCorr instance, with a specified capacity.
    pub fn with_capacity(cap: usize) -> Self {
        debug_assert!(cap > 0);
        Self {
            states: StateStore::with_capacity(cap),
        }
    }

    /// Return the number of states.
    pub fn len(&self) -> usize {
        self.states.len()
    }

    /// Add a state.
    pub fn push(&mut self, val: SomeState) {
        self.states.push(val);
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.states.is_empty()
    }

    /// Return an immuable iterator.
    pub fn iter(&self) -> Iter<SomeState> {
        self.states.iter()
    }
} // end impl StatesCorr

impl Index<usize> for StatesCorr {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.states[i]
    }
}

impl IndexMut<usize> for StatesCorr {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.states[i]
    }
}

impl AvecRef for StatesCorr {
    fn avec_ref(&self) -> &Vec<impl NumBits> {
        self.states.avec_ref()
    }
}
