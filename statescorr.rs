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
extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

impl fmt::Display for StatesCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SSC{}", self.states)
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

    /// Return a statescorr, given a string representation.
    /// Like SSC[], SSC[s1010], or SSC[s101, s100].
    pub fn new_from_string(statescorr_str: &str) -> Result<Self, String> {
        //println!("statescorr::new_from_string: {statescorr_str}");

        let mut statescorr_str2 = String::new();
        let mut last_chr = false;

        for (inx, chr) in statescorr_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "S" {
                    continue;
                } else {
                    return Err("Invalid string, should start with SSC[".to_string());
                }
            }
            if inx == 1 {
                if chr == "S" {
                    continue;
                } else {
                    return Err("Invalid string, should start with SSC[".to_string());
                }
            }
            if inx == 2 {
                if chr == "C" {
                    continue;
                } else {
                    return Err("Invalid string, should start with SSC[".to_string());
                }
            }
            if chr == "]" {
                last_chr = true;
                statescorr_str2.push_str(chr);
                continue;
            }

            if last_chr {
                return Err("Invalid string, should end with ]".to_string());
            }
            statescorr_str2.push_str(chr);
        }
        if !last_chr {
            return Err("Invalid string, should end with ]".to_string());
        }

        //println!("statescorr_str2 {statescorr_str2}");
        let states = StateStore::new_from_string(&statescorr_str2)?;

        let ret_statescorr = Self { states };
        //println!("ret_statescorr {ret_statescorr}");

        Ok(ret_statescorr)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_from_string() -> Result<(), String> {
        let stast1 = StatesCorr::new_from_string("SSC[]")?;
        println!("stast1 {stast1}");
        assert!(format!("{stast1}") == "SSC[]");

        let stast2 = StatesCorr::new_from_string("SSC[s1010]")?;
        println!("stast2 {stast2}");
        assert!(format!("{stast2}") == "SSC[s1010]");

        let stast3 = StatesCorr::new_from_string("SSC[s1010, s1111]")?;
        println!("stast3 {stast3}");
        assert!(format!("{stast3}") == "SSC[s1010, s1111]");

        //assert!(1 == 2);
        Ok(())
    }
}
