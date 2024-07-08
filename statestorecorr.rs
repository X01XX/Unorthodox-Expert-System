//! The StateStoreCorr struct. A vector of SomeState structs,
//! corresponding to the DomainStore vector.
//! States will use the same number of bits as the corresponding domain,
//! which may be different from other states in the vector.
use crate::bits::NumBits;
use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use tools::AvecRef;

use std::fmt;

impl fmt::Display for StateStoreCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.items))
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct StateStoreCorr {
    /// A vector of states.
    items: Vec<SomeState>,
}

impl StateStoreCorr {
    /// Return a new, empty, StateStoreCorr instance.
    pub fn new(items: Vec<SomeState>) -> Self {
        Self { items }
    }

    /// Return a new StateStoreCorr instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SomeState>::with_capacity(num),
        }
    }

    /// Return the number of states.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Add a state.
    pub fn push(&mut self, val: SomeState) {
        self.items.push(val);
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Return an immuable iterator.
    pub fn iter(&self) -> Iter<SomeState> {
        self.items.iter()
    }
} // end impl StateStoreCorr

impl Index<usize> for StateStoreCorr {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.items[i]
    }
}

impl IndexMut<usize> for StateStoreCorr {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}

impl AvecRef for StateStoreCorr {
    fn avec_ref(&self) -> &Vec<impl NumBits> {
        &self.items
    }
}
