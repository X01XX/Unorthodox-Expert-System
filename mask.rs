//! The SomeMask struct, a mask of bits.
//!
//! The SomeState struct is very similar, the difference is in the intended use, and the xor operation.
//!
//! A Mask of differences between two states would be calculated like:
//!
//! let diff_mask = SomeMask { ints: state1.bts.b_xor(&state2.bts) };
//!
//! A difference mask applied to a state, to get a new state, would be calculated like:
//!
//! let state2 = SomeState { ints: diff_mask.bts.b_xor(&state1.bts) };
//!
//! Since ones matter in a mask, and zeros are place-holders, the xor operation only works as
//! expected if one mask is a superset of the other.
//!
//! Instead of xor, you probably should use <1>.m_and(<2>.not()) to find ones in <1> that are not in <2>.

use crate::bits::SomeBits;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct SomeMask {
    /// Bits set to one are significant.
    bts: SomeBits,
}

impl fmt::Display for SomeMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeMask {
    /// Return a new Mask struct instance.
    pub fn new(val: SomeBits) -> Self {
        Self { bts: val }
    }

    /// Return a new mask set to all zeros.
    pub fn new_low(num_ints: usize) -> Self {
        Self {
            bts: SomeBits::new_low(num_ints),
        }
    }

    /// Accessor, return a read-onlu reference to the bts field
    pub fn get_bts(&self) -> &SomeBits {
        &self.bts
    }

    /// Convert a mask into astate
    pub fn to_state(&self) -> SomeState {
        SomeState::new(self.bts.clone())
    }

//  pub fn m_xor(&self, other: &Self) -> Self {
//      Self::new(self.bts.b_xor(&other.bts))
//  }

    /// Return the or of two maks.
    pub fn m_or(&self, other: &Self) -> Self {
        Self::new(self.bts.b_or(&other.bts))
    }

    /// Return the and of two masks.
    pub fn m_and(&self, other: &Self) -> Self {
        Self::new(self.bts.b_and(&other.bts))
    }

    /// Return the result of a not operation.
    pub fn m_not(&self) -> Self {
        Self::new(self.bts.b_not())
    }

    /// Return true if the mask is low, that is all zeros.
    pub fn is_low(&self) -> bool {
        self.bts.is_low()
    }

    /// Return true if the mask is not low, that is not all zeros.
    pub fn is_not_low(&self) -> bool {
        self.bts.is_not_low()
    }

    /// Return true if the mask is high, that is all ones/
    pub fn is_high(&self) -> bool {
        self.bts.is_high()
    }

    /// Return true if a given bit is one at a given position.
    pub fn is_bit_set(&self, b: usize) -> bool {
        self.bts.is_bit_set(b)
    }

    /// Return true if a mask is a subset of a second mask.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.bts.is_subset_of(&other.bts)
    }

    /// Return true if a mask is a superset of a second mask.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        self.bts.is_superset_of(&other.bts)
    }

    /// Return the number of bits set to one.
    pub fn num_one_bits(&self) -> usize {
        self.bts.num_one_bits()
    }

    /// Return true if only one bit is set to one.
    pub fn just_one_bit(&self) -> bool {
        self.bts.just_one_bit()
    }

    /// Return a vector of one-bit masks.
    /// Could be called like MaskStore { avec: <a mask object>.split() }
    pub fn split(&self) -> Vec<Self> {
        let bitsx = self.bts.split();

        let mut rc_vec: Vec<Self> = Vec::with_capacity(bitsx.len());

        for bitx in bitsx.iter() {
            rc_vec.push(SomeMask::new(bitx.clone()));
        } // next bitx

        rc_vec
    }

    /// Return the number of ints used to express a SomeMask instance.
    pub fn num_ints(&self) -> usize {
        self.bts.num_ints()
    }

    /// Return the expected length of a formatted string.
    pub fn formatted_string_length(&self) -> usize {
        self.bts.formatted_string_length()
    }

    /// Return a formatted string.
    pub fn formatted_string(&self) -> String {
        self.bts.formatted_string('m')
    }

    pub fn str2(&self) -> String {
        self.bts.str2(' ')
    }
} // end SomeMask

impl Clone for SomeMask {
    fn clone(&self) -> Self {
        Self {
            bts: self.bts.clone(),
        }
    }
}
