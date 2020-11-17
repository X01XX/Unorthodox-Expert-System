// Mask struct for an Unorthodox Expert System

use crate::bits::SomeBits;
use std::fmt;

impl PartialEq for SomeMask {
    fn eq(&self, other: &Self) -> bool {
        self.bts == other.bts
    }
}
impl Eq for SomeMask {}

#[derive(Debug)]
pub struct SomeMask {
    pub bts: SomeBits,
}

impl fmt::Display for SomeMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "m{}", self.bts)
    }
}

impl SomeMask {
    // Return a new Mask struct instance
    pub fn new(val: SomeBits) -> Self {
        Self { bts: val }
    }

    pub fn clone(&self) -> Self {
        Self {
            bts: self.bts.clone(),
        }
    }

    // Return true if two masks are equal
    pub fn is_eq(&self, other: &Self) -> bool {
        self.bts == other.bts
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
        !self.is_low()
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
}
