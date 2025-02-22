//! The StateStore struct. A vector of SomeState structs.

use crate::bits::NumBits;
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;
use crate::tools::{self, vec_refs, vec_string, AvecRef};

use serde::{Deserialize, Serialize};
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use std::str::FromStr;

use std::fmt;

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", vec_string(&self.items))
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct StateStore {
    /// A vector of states.
    items: Vec<SomeState>,
}

impl StateStore {
    /// Return a new StateStore instance.
    pub fn new(items: Vec<SomeState>) -> Self {
        Self { items }
    }

    /// Return a new StateStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SomeState>::with_capacity(num),
        }
    }

    /// Add a state to a StateStore.
    /// Do not allow duplicates.
    pub fn push(&mut self, val: SomeState) {
        self.items.push(val);
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

    /// Return a reference to the first state.
    pub fn last(&self) -> Option<&SomeState> {
        self.items.last()
    }

    /// Return a vector of references.
    pub fn vec_refs(&self) -> Vec<&SomeState> {
        vec_refs(&self.items[..])
    }

    /// Return the region formed by states in a non-empty StateStore.
    pub fn x_mask(&self) -> SomeMask {
        debug_assert!(self.is_not_empty());

        let mut ret = SomeMask::new(self[0].bts.new_low());
        for stax in self.iter().skip(1) {
            ret = ret.bitwise_or(&stax.bitwise_xor(&self[0]));
        }
        ret
    }

    /// Return a region containing all squares in a non-empty StateStore.
    pub fn as_region(&self) -> SomeRegion {
        debug_assert!(self.is_not_empty());

        SomeRegion::new(self.items.clone())
    }

    /// Return a vector of state refs that are in a given region.
    pub fn stas_in_reg(&self, regx: &SomeRegion) -> Vec<&SomeState> {
        let mut ret = Vec::<&SomeState>::new();
        for stax in self.iter() {
            if regx.is_superset_of(stax) {
                ret.push(stax);
            }
        }
        ret
    }
} // end impl StateStore

impl Index<usize> for StateStore {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.items[i]
    }
}

impl IndexMut<usize> for StateStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}

impl AvecRef for StateStore {
    fn avec_ref(&self) -> &Vec<impl NumBits> {
        &self.items
    }
}

impl FromStr for StateStore {
    type Err = String;
    /// Return a StateStore, given a string representation.
    /// Like [] or [s1010, s0101].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("statestore::from_str: {str_in}");
        let str_in2 = str_in.trim();
        if str_in2.len() < 2 {
            return Err("statestore::from_str: string should be at least = []".to_string());
        }

        if str_in2 == "[]" {
            return Ok(Self::new(vec![]));
        }

        if str_in2[0..1] != *"[" {
            return Err("statestore::from_str: string should begin with [".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("statestore::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[1..(str_in2.len() - 1)];

        // Split string into SomeState tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("statestore::from_str: {errstr}")),
        };
        //println!("tokens {:?}", tokens);

        // Tally up tokens.
        let mut state_vec = Vec::<SomeState>::with_capacity(tokens.len());

        for tokenx in tokens {
            state_vec.push(
                SomeState::from_str(&tokenx).expect("statestore::from_str: invalid region token"),
            );
        }

        Ok(Self::new(state_vec))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() -> Result<(), String> {
        let sta1 = SomeState::from_str("s0001")?;

        let sta2 = SomeState::from_str("s0010")?;

        // Create a one-state store.
        let store = StateStore::new(vec![sta1.clone()]);
        println!("store {store}");
        assert!(store.len() == 1);

        // Create a two-state store.
        let store = StateStore::new(vec![sta1.clone(), sta2.clone()]);
        println!("store {store}");
        assert!(store.len() == 2);

        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let stast1 = StateStore::from_str("[]")?;
        println!("stast1 {stast1}");
        assert!(format!("{stast1}") == "[]");

        let stast2 = StateStore::from_str("[s1010]")?;
        println!("stast2 {stast2}");
        assert!(format!("{stast2}") == "[s1010]");

        let stast3_str = "[s1010, s1111]";
        let stast3 = StateStore::from_str(&stast3_str)?;
        println!("stast3 {stast3}");
        assert!(format!("{stast3}") == stast3_str);

        //assert!(1 == 2);
        Ok(())
    }
}
