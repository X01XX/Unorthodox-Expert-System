// Mask struct, for an Unorthodox Expert System.
//
// The difference between a Mask and a State is the intended use.

use crate::bits::SomeBits;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct SomeMask {
    pub bts: SomeBits,
}

impl fmt::Display for SomeMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeMask {
    // Return a new Mask struct instance
    pub fn new(val: SomeBits) -> Self {
        Self { bts: val }
    }

    pub fn new_low(num_ints: usize) -> Self {
        Self {
            bts: SomeBits::new_low(num_ints),
        }
    }

    // Return the xor or two maks
    pub fn m_xor(&self, other: &Self) -> Self {
        Self::new(self.bts.b_xor(&other.bts))
    }

    // Return the or or two maks
    pub fn m_or(&self, other: &Self) -> Self {
        Self::new(self.bts.b_or(&other.bts))
    }

    // Return the and or two maks
    pub fn m_and(&self, other: &Self) -> Self {
        Self::new(self.bts.b_and(&other.bts))
    }

    // Return result of a not operation
    pub fn m_not(&self) -> Self {
        Self::new(self.bts.b_not())
    }

    // Return true if the mask is low, that is all zeros
    pub fn is_low(&self) -> bool {
        self.bts.is_low()
    }

    pub fn is_not_low(&self) -> bool {
        self.bts.is_not_low()
    }

    // Return true if the mask is high, that is all ones
    pub fn is_high(&self) -> bool {
        self.bts.is_high()
    }

    // Return true is a given bit is one at a given position
    pub fn is_bit_set(&self, b: usize) -> bool {
        self.bts.is_bit_set(b)
    }

    // Return true is a mask is a subset of a second mask
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.bts.is_subset_of(&other.bts)
    }

    // Return true is a mask is a superset of a second mask
    pub fn is_superset_of(&self, other: &Self) -> bool {
        self.bts.is_superset_of(&other.bts)
    }

    pub fn num_one_bits(&self) -> usize {
        self.bts.num_one_bits()
    }

    pub fn just_one_bit(&self) -> bool {
        self.bts.just_one_bit()
    }

    // Return a vector of one-bit masks
    // Should be called like MaskStore { avec: <a mask object>.split() }
    pub fn split(&self) -> Vec<Self> {
        let bitsx = self.bts.split();

        let mut rc_vec: Vec<Self> = Vec::with_capacity(bitsx.len());

        for bitx in bitsx.iter() {
            rc_vec.push(SomeMask::new(bitx.clone()));
        } // next bitx

        rc_vec
    }

    // Return the number of ints used to express a SomeMask instance
    pub fn num_ints(&self) -> usize {
        self.bts.num_ints()
    }

    pub fn formatted_string_length(&self) -> usize {
        self.bts.formatted_string_length()
    }

    pub fn formatted_string(&self) -> String {
        self.bts.formatted_string('m')
    }
} // end SomeMask

impl Clone for SomeMask {
    fn clone(&self) -> Self {
        Self {
            bts: self.bts.clone(),
        }
    }
}
