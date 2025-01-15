//! The MasksCorr struct. A store of SomeMask structs,
//! corresponding, in order, to domains in a DomainStore inmsknce.
//!
//! Masks will use the same number of bits as the corresponding domain,
//! which may be different from other masks in the vector.
use crate::mask::SomeMask;
use crate::maskstore::MaskStore;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use std::str::FromStr;

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
    /// Return a new, empty, MasksCorr instance, with a specified capacity.
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

    /// Return true if a MasksCorr is all low.
    pub fn is_low(&self) -> bool {
        for mskx in self.iter() {
            if mskx.is_not_low() {
                return false;
            }
        }
        true
    }

    /// Return true if corresponding regions in two MasksCorr have the same number of bits.
    pub fn is_congruent(&self, other: &impl tools::CorrespondingItems) -> bool {
        self.num_bits_vec() == other.num_bits_vec()
    }

    /// Return a vector of corresponding num_bits.
    pub fn num_bits_vec(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::with_capacity(self.len());
        for mskx in self.masks.iter() {
            ret_vec.push(mskx.num_bits());
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

    /// Return the bitwise OR of two MasksCorrs.
    pub fn bitwise_or(&self, other: &Self) -> Self {
        let mut ret_masks = Self::with_capacity(self.len());

        for (mskx, msky) in self.iter().zip(other.iter()) {
            ret_masks.push(mskx.bitwise_or(msky));
        }
        ret_masks
    }

    /// Return the bitwise AND of two MasksCorrs.
    pub fn bitwise_and(&self, other: &Self) -> Self {
        let mut ret_masks = Self::with_capacity(self.len());

        for (mskx, msky) in self.iter().zip(other.iter()) {
            ret_masks.push(mskx.bitwise_and(msky));
        }
        ret_masks
    }

    /// Return a MasksCorr split into single-1 bit masks.
    pub fn split(&self) -> Vec<Self> {
        let mut ret_masks = Vec::<Self>::new();

        for (inx, mskx) in self.iter().enumerate() {
            let masks = mskx.split();
            for msky in masks.iter() {
                let mut tmp_msk = Self::with_capacity(self.len());
                for iny in 0..self.len() {
                    if iny == inx {
                        tmp_msk.push(msky.clone());
                    } else {
                        tmp_msk.push(self[iny].new_low());
                    }
                }
                ret_masks.push(tmp_msk);
            }
        }
        ret_masks
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

impl FromStr for MasksCorr {
    type Err = String;
    /// Return a MasksCorr, given a string representation.
    /// Like MC[], MC[m1010], or MC[m101, m100].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("maskscorr::from_str: {str_in}");
        let str_in2 = str_in.trim();

        if str_in2.len() < 4 {
            return Err("maskscorr::from_str: string should be at least = MC[]".to_string());
        }

        if str_in2 == "MC[]" {
            return Ok(Self::with_capacity(1));
        }

        if str_in2[0..3] != *"MC[" {
            return Err("maskscorr::from_str: string should begin with MC[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("maskscorr::from_str: string should end with ]".to_string());
        }

        // Strip off the id.
        let token_str = &str_in2[2..];

        match MaskStore::from_str(token_str) {
            Ok(masks) => Ok(Self { masks }),
            Err(errstr) => Err(format!("maskscorr::from_str: {errstr}")),
        }
    }
}

/// Implement the trait StrLen for RegionsCorr.
impl tools::StrLen for MasksCorr {
    fn strlen(&self) -> usize {
        2 + self.masks.strlen()
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

        let mskst3_str = "MC[m101, m100]";
        let mskst3 = MasksCorr::from_str(&mskst3_str)?;
        println!("mskst3 {mskst3}");
        assert!(format!("{mskst3}") == mskst3_str);

        Ok(())
    }
}
