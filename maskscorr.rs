//! The MasksCorr struct. A store of SomeMask structs,
//! corresponding, in order, to domains in a DomainStore inmsknce.
//!
//! Masks will use the same number of bits as the corresponding domain,
//! which may be different from other masks in the vector.
use crate::mask::SomeMask;
use crate::maskstore::MaskStore;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::ops::{Index, IndexMut};
use std::slice::Iter;

use std::fmt;

use unicode_segmentation::UnicodeSegmentation;

impl fmt::Display for MasksCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MC{}", self.masks)
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct MasksCorr {
    /// A vector of masks.
    masks: MaskStore,
}

impl MasksCorr {
    /// Return a new, empty, MasksCorr inmsknce.
    pub fn new(masks: Vec<SomeMask>) -> Self {
        Self {
            masks: MaskStore::new(masks),
        }
    }

    /// Return a new, empty, MasksCorr inmsknce, with a specified capacity.
    pub fn with_capacity(cap: usize) -> Self {
        debug_assert!(cap > 0);
        Self {
            masks: MaskStore::with_capacity(cap),
        }
    }

    /// Return the number of masks.
    pub fn len(&self) -> usize {
        self.masks.len()
    }

    /// Add a mask.
    pub fn push(&mut self, val: SomeMask) {
        self.masks.push(val);
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.masks.is_empty()
    }

    /// Return an immuable iterator.
    pub fn iter(&self) -> Iter<SomeMask> {
        self.masks.iter()
    }

    /// Return a maskscorr, given a string representation.
    /// Like SC[], SC[s1010], or SC[s101, s100].
    pub fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("maskscorr::from_str: {str_in}");
        let mc_str = str_in.trim();

        if mc_str.is_empty() {
            return Err("MasksCorr::from_str: Empty string?".to_string());
        }

        let mut mc_str2 = String::new();
        let mut last_chr = false;

        for (inx, chr) in mc_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "M" {
                    continue;
                } else {
                    return Err(format!(
                        "MasksCorr::from_str: Invalid string, {mc_str} should mskrt with MC["
                    ));
                }
            }
            if inx == 1 {
                if chr == "C" {
                    continue;
                } else {
                    return Err(format!(
                        "MasksCorr::from_str: Invalid string, {mc_str} should mskrt with MC["
                    ));
                }
            }
            if chr == "]" {
                last_chr = true;
                mc_str2.push_str(chr);
                continue;
            }

            if last_chr {
                return Err(format!(
                    "MasksCorr::from_str: Invalid string, {mc_str} should end with ]"
                ));
            }
            mc_str2.push_str(chr);
        }
        if !last_chr {
            return Err(format!(
                "MasksCorr::from_str: Invalid string, {mc_str} should end with ]"
            ));
        }

        //println!("mc_str2 {mc_str2}");
        match MaskStore::from_str(&mc_str2) {
            Ok(masks) => Ok(Self { masks }),
            Err(errstr) => Err(format!("MasksCorr::from_str: {errstr}")),
        }
    }

    /// Return true if corresponding regions in two MasksCorr have the same number of bits.
    pub fn is_congruent(&self, other: &impl tools::CorrespondingItems) -> bool {
        self.num_bits_vec() == other.num_bits_vec()
    }

    /// Return a vector of corresponding num_bits.
    pub fn num_bits_vec(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::with_capacity(self.len());
        for regx in self.masks.iter() {
            ret_vec.push(regx.num_bits());
        }
        ret_vec
    }

    /// Return the number of bits set to one.
    pub fn num_one_bits(&self) -> usize {
        let mut count = 0;
        for mskx in self.iter() {
            count += mskx.num_one_bits();
        }
        count
    }
} // end impl MasksCorr

impl Index<usize> for MasksCorr {
    type Output = SomeMask;
    fn index(&self, i: usize) -> &SomeMask {
        &self.masks[i]
    }
}

impl IndexMut<usize> for MasksCorr {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.masks[i]
    }
}

impl tools::CorrespondingItems for MasksCorr {
    fn num_bits_vec(&self) -> Vec<usize> {
        self.num_bits_vec()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_str() -> Result<(), String> {
        let mskst1 = MasksCorr::from_str("MC[]")?;
        println!("mskst1 {mskst1}");
        assert!(format!("{mskst1}") == "MC[]");

        let mskst2 = MasksCorr::from_str("MC[m1010]")?;
        println!("mskst2 {mskst2}");
        assert!(format!("{mskst2}") == "MC[m1010]");

        let mskst3_str = "MC[m1010, m1111]";
        let mskst3 = MasksCorr::from_str(&mskst3_str)?;
        println!("mskst3 {mskst3}");
        assert!(format!("{mskst3}") == mskst3_str);

        //assert!(1 == 2);
        Ok(())
    }
}
