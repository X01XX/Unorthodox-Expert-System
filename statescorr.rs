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
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use std::str::FromStr;

use unicode_segmentation::UnicodeSegmentation;

impl fmt::Display for StatesCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SC{}", self.states)
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct StatesCorr {
    /// A vector of states.
    states: StateStore,
}

impl StatesCorr {
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

    /// Return a vector of corresponding num_bits.
    pub fn num_bits_vec(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::with_capacity(self.len());
        for regx in self.states.iter() {
            ret_vec.push(regx.num_bits());
        }
        ret_vec
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

impl tools::AvecRef for StatesCorr {
    fn avec_ref(&self) -> &Vec<impl NumBits> {
        self.states.avec_ref()
    }
}

impl tools::CorrespondingItems for StatesCorr {
    fn num_bits_vec(&self) -> Vec<usize> {
        self.num_bits_vec()
    }
}

impl FromStr for StatesCorr {
    type Err = String;
    /// Return a StatesCorr, given a string representation.
    /// Like SC[], SC[s1010], or SC[s101, s100].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("statescorr::from_str: {str_in}");
        let sc_str = str_in.trim();

        if sc_str.is_empty() {
            return Err("StatesCorr::from_str: Empty string?".to_string());
        }

        let mut sc_str2 = String::new();
        let mut last_chr = false;

        for (inx, chr) in sc_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "S" {
                    continue;
                } else {
                    return Err(format!(
                        "StatesCorr::from_str: Invalid string, {sc_str} should start with SC["
                    ));
                }
            }
            if inx == 1 {
                if chr == "C" {
                    continue;
                } else {
                    return Err(format!(
                        "StatesCorr::from_str: Invalid string, {sc_str} should start with SC["
                    ));
                }
            }
            if chr == "]" {
                last_chr = true;
                sc_str2.push_str(chr);
                continue;
            }

            if last_chr {
                return Err(format!(
                    "StatesCorr::from_str: Invalid string, {sc_str} should end with ]"
                ));
            }
            sc_str2.push_str(chr);
        }
        if !last_chr {
            return Err(format!(
                "StatesCorr::from_str: Invalid string, {sc_str} should end with ]"
            ));
        }

        //println!("sc_str2 {sc_str2}");
        match StateStore::from_str(&sc_str2) {
            Ok(states) => Ok(Self { states }),
            Err(errstr) => Err(format!("StatesCorr::from_str: {errstr}")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_str() -> Result<(), String> {
        let stast1 = StatesCorr::from_str("SC[]")?;
        println!("stast1 {stast1}");
        assert!(format!("{stast1}") == "SC[]");

        let stast2 = StatesCorr::from_str("SC[s1010]")?;
        println!("stast2 {stast2}");
        assert!(format!("{stast2}") == "SC[s1010]");

        let stast3_str = "SC[s1010, s1111]";
        let stast3 = StatesCorr::from_str(&stast3_str)?;
        println!("stast3 {stast3}");
        assert!(format!("{stast3}") == stast3_str);

        Ok(())
    }
}
