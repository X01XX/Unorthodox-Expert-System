//! The MaskStore struct. A vector of SomeMask structs.

use crate::mask::SomeMask;
use crate::tools::vec_string;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use std::str::FromStr;
use unicode_segmentation::UnicodeSegmentation;

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
    /// Like [] or [s1010, s0101].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("maskstore::from_str: {str_in}");
        let maskstore_str = str_in.trim();

        if maskstore_str.is_empty() {
            return Err("MaskStore::from_str: Empty string?".to_string());
        }

        let mut maskstore_str2 = String::new();
        let mut last_chr = false;

        for (inx, chr) in maskstore_str.graphemes(true).enumerate() {
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
            maskstore_str2.push_str(chr);
        }
        if !last_chr {
            return Err("Invalid string, should end with ]".to_string());
        }

        if maskstore_str2.is_empty() {
            return Ok(MaskStore::new(vec![]));
        }

        // Split string into <region> tokens.
        let mut token = String::new();
        let mut token_list = Vec::<String>::new();

        for chr in maskstore_str2.graphemes(true) {
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

        // Tally up tokens.
        let mut regions = Vec::<SomeMask>::new();

        for tokenx in token_list.into_iter() {
            regions.push(SomeMask::from_str(&tokenx).expect("Invalid region token"));
        }
        let ret_maskstore = MaskStore::new(regions);
        //println!("ret_maskstore {ret_maskstore}");

        Ok(ret_maskstore)
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
        let mskst1 = MaskStore::from_str("[]")?;
        println!("mskst1 {mskst1}");
        assert!(format!("{mskst1}") == "[]");

        let mskst2 = MaskStore::from_str("[m1010]")?;
        println!("mskst2 {mskst2}");
        assert!(format!("{mskst2}") == "[m1010]");

        let mskst3_str = "[m1010, m1111]";
        let mskst3 = MaskStore::from_str(&mskst3_str)?;
        println!("mskst3 {mskst3}");
        assert!(format!("{mskst3}") == mskst3_str);

        //assert!(1 == 2);
        Ok(())
    }
}
