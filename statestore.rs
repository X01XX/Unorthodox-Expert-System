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
    /// Do not allow duplicates.
    pub fn push(&mut self, val: SomeState) {
        debug_assert!(self.is_empty() || val.num_bits() == self.items[0].num_bits());

        if !self.contains(&val) {
            self.items.push(val);
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() -> Result<(), String> {
        let sta1 = SomeState::new_from_string("0b0001")?;
        let sta1a = SomeState::new_from_string("0b0001")?;

        let sta2 = SomeState::new_from_string("0b0010")?;

        // Create a one-state store.
        let store = StateStore::new(vec![sta1.clone()]);
        println!("store {store}");
        assert!(store.len() == 1);

        // Create a one-state store, deleting dupliactes.
        let store = StateStore::new(vec![sta1.clone(), sta1a.clone()]);
        println!("store {store}");
        assert!(store.len() == 1);

        // Create a two-state store.
        let store = StateStore::new(vec![sta1.clone(), sta2.clone()]);
        println!("store {store}");
        assert!(store.len() == 2);

        // Create a two-state store, deleting duplicates.
        let store = StateStore::new(vec![sta1.clone(), sta2.clone(), sta1a.clone()]);
        println!("store {store}");
        assert!(store.len() == 2);

        Ok(())
    }
}
