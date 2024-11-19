//! The StateStore struct. A vector of SomeState structs.
//! Duplicates are suppressed.

use crate::bits::NumBits;
use crate::state::SomeState;
use crate::tools::{vec_string, AvecRef};

use serde::{Deserialize, Serialize};
use std::ops::{Index, IndexMut};
use std::slice::Iter;

use std::fmt;

use unicode_segmentation::UnicodeSegmentation;

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
        let mut ret = Self {
            items: Vec::<SomeState>::with_capacity(items.len()),
        };
        for stax in items {
            ret.push(stax);
        }
        ret
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

    /// Return a statestore, given a string representation.
    /// Like [] or [s1010, s0101].
    pub fn from(statestore_str: &str) -> Result<Self, String> {
        //println!("statestore::from: {statestore_str}");

        let mut statestore_str2 = String::new();
        let mut last_chr = false;

        for (inx, chr) in statestore_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "[" {
                    continue;
                } else {
                    return Err("Invalid string, should start with [".to_string());
                }
            }
            if chr == "]" {
                last_chr = true;
                continue;
            }

            if last_chr {
                return Err("Invalid string, should end with ]".to_string());
            }
            statestore_str2.push_str(chr);
        }
        if !last_chr {
            return Err("Invalid string, should end with ]".to_string());
        }

        if statestore_str2.is_empty() {
            return Ok(StateStore::new(vec![]));
        }

        // Split string into <region> tokens.
        let mut token = String::new();
        let mut token_list = Vec::<String>::new();

        for chr in statestore_str2.graphemes(true) {
            if chr == "," || chr == " " {
                if token.is_empty() {
                } else {
                    token_list.push(token);
                    token = String::new();
                }
            } else {
                token.push_str(chr);
            }
        }
        if token.is_empty() {
        } else {
            token_list.push(token);
        }
        //println!("token_list {:?}", token_list);

        // println!("token_list2 {:?}", token_list2);

        // Tally up tokens.
        let mut regions = Vec::<SomeState>::new();

        for tokenx in token_list.into_iter() {
            regions.push(SomeState::from(&tokenx).expect("Invalid region token"));
        }
        let ret_statestore = StateStore::new(regions);
        //println!("ret_statestore {ret_statestore}");

        Ok(ret_statestore)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() -> Result<(), String> {
        let sta1 = SomeState::from("0b0001")?;

        let sta2 = SomeState::from("0b0010")?;

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
    fn from() -> Result<(), String> {
        let stast1 = StateStore::from("[]")?;
        println!("stast1 {stast1}");
        assert!(format!("{stast1}") == "[]");

        let stast2 = StateStore::from("[s1010]")?;
        println!("stast2 {stast2}");
        assert!(format!("{stast2}") == "[s1010]");

        let stast3 = StateStore::from("[s1010, s1111]")?;
        println!("stast3 {stast3}");
        assert!(format!("{stast3}") == "[s1010, s1111]");

        //assert!(1 == 2);
        Ok(())
    }
}
