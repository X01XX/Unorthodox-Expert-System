//! The MaskStore struct. A vector of SomeMask structs.

use crate::mask::SomeMask;
use crate::tools::{self, vec_string};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use std::str::FromStr;

impl fmt::Display for MaskStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", vec_string(&self.items))
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct MaskStore {
    /// A vector of masks.
    items: Vec<SomeMask>,
}

impl MaskStore {
    /// Return a new MaskStore instance.
    pub fn new(items: Vec<SomeMask>) -> Self {
        Self { items }
    }

    /// Return a new MaskStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SomeMask>::with_capacity(num),
        }
    }

    /// Add a mask to a MaskStore.
    /// Do not allow duplicates.
    pub fn push(&mut self, val: SomeMask) {
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
    pub fn iter(&self) -> Iter<SomeMask> {
        self.items.iter()
    }

    /// Return true if a MaskStore contains a mask.
    pub fn contains(&self, mskx: &SomeMask) -> bool {
        self.items.contains(mskx)
    }

    /// Return the number of masks.
    pub fn len(&self) -> usize {
        self.items.len()
    }
} // end impl MaskStore

impl Index<usize> for MaskStore {
    type Output = SomeMask;
    fn index(&self, i: usize) -> &SomeMask {
        &self.items[i]
    }
}

impl IndexMut<usize> for MaskStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}

impl FromStr for MaskStore {
    type Err = String;
    /// Return a maskstore, given a string representation.
    /// Like [], [m1010] or [m1010, m1111].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("maskstore::from_str: {str_in}");
        let str_in2 = str_in.trim();

        if str_in2.len() < 2 {
            return Err("maskstore::from_str: string should be at least = []".to_string());
        }

        if str_in2 == "[]" {
            return Ok(Self::new(vec![]));
        }

        if str_in2[0..1] != *"[" {
            return Err("masktore::from_str: string should begin with [".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("masktore::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[1..(str_in2.len() - 1)];

        // Split string into SomeMask tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("maskstore::from_str: {errstr}")),
        };
        //println!("tokens {:?}", tokens);

        // Tally up tokens.
        let mut regions = Vec::<SomeMask>::new();

        for tokenx in tokens {
            regions.push(
                SomeMask::from_str(&tokenx).expect("maskstore::from_str: Invalid region token"),
            );
        }

        Ok(Self::new(regions))
    }
}

/// Implement the trait StrLen for MasksStore.
impl tools::StrLen for MaskStore {
    fn strlen(&self) -> usize {
        let mut rc_len = 2;

        if self.is_not_empty() {
            rc_len += self.items.len() * self.items[0].strlen();
            rc_len += (self.items.len() - 1) * 2;
        }

        rc_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() -> Result<(), String> {
        let msk1 = SomeMask::from_str("m0001")?;

        let msk2 = SomeMask::from_str("m0010")?;

        // Create a one-mask store.
        let store = MaskStore::new(vec![msk1.clone()]);
        println!("store {store}");
        assert!(store.len() == 1);

        // Create a two-mask store.
        let store = MaskStore::new(vec![msk1.clone(), msk2.clone()]);
        println!("store {store}");
        assert!(store.len() == 2);

        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let mskst1_str = "[]";
        let mskst1 = MaskStore::from_str(&mskst1_str)?;
        println!("mskst1 {mskst1}");
        assert!(format!("{mskst1}") == mskst1_str);

        let mskst2_str = "[m1010]";
        let mskst2 = MaskStore::from_str(&mskst2_str)?;
        println!("mskst2 {mskst2}");
        assert!(format!("{mskst2}") == mskst2_str);

        let mskst3_str = "[m1010, m1111]";
        let mskst3 = MaskStore::from_str(&mskst3_str)?;
        println!("mskst3 {mskst3}");
        assert!(format!("{mskst3}") == mskst3_str);

        Ok(())
    }
}
