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

use crate::bits::SomeBits;
use crate::state::SomeState;
use crate::randompick::random_x_of_n;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
/// SomeMask struct, just some bits.
pub struct SomeMask {
    bts: SomeBits,
}

/// Display trait for SomeMask
impl fmt::Display for SomeMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeMask {
    /// Return a new SomeMask instance.
    pub fn new(val: SomeBits) -> Self {
        Self { bts: val }
    }

    /// Return a new mask set to all zeros.
    pub fn new_low(num_ints: usize) -> Self {
        Self {
            bts: SomeBits::new(num_ints),
        }
    }

    /// Convert a mask into astate
    pub fn to_state(&self) -> SomeState {
        SomeState::new(self.bts.clone())
    }

    /// Return the bitwise OR of two masks.
    pub fn m_or(&self, other: &Self) -> Self {
        Self::new(self.bts.b_or(&other.bts))
    }

    /// Return the bitwize AND of two masks.
    pub fn m_and(&self, other: &Self) -> Self {
        Self::new(self.bts.b_and(&other.bts))
    }

    /// Return the bitwize XOR of two masks.
    pub fn m_xor(&self, other: &Self) -> Self {
        Self::new(self.bts.b_xor(&other.bts))
    }

    /// Return the bitwize NOT of a mask.
    pub fn m_not(&self) -> Self {
        Self::new(self.bts.b_not())
    }

    /// Return true if the mask is all zeros.
    pub fn is_low(&self) -> bool {
        self.bts.is_low()
    }

    /// Return true if the mask is not all zeros.
    pub fn is_not_low(&self) -> bool {
        self.bts.is_not_low()
    }

    /// Return true if the mask is all ones.
    pub fn is_high(&self) -> bool {
        self.bts.is_high()
    }

    /// Return true if a given bit position is one.
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

    /// Create a formatted string to display under an instance,
    /// to indicate specific bits positions.
    pub fn str2(&self) -> String {
        self.bts.str2(' ')
    }

    /// Return a Mask from a string.
    /// Left-most, consecutive, zeros can be omitted.
    ///
    /// if let Ok(msk) = SomeMask::from_string(1, "m0101")) {
    ///    println!("Mask {}", &msk);
    /// } else {
    ///    panic!("Invalid Mask");
    /// }
    /// A prefix of "m0x" can be used to specify hexadecimal characters.
    pub fn _from_string(num_ints: usize, str: &str) -> Result<SomeMask, String> {
        for chr in str.chars() {
            if chr != 'm' && chr != 'M' {
                return Err(String::from("initial character should be m"));
            }
            break;
        }

        Ok(SomeMask::new(SomeBits::from_string(num_ints, &str[1..]).unwrap()))
    } // end from_string

    /// Given a mask of more than one bit, return a mask that is a random selection of
    /// roughly half the bits.
    pub fn half_mask(&self) -> Self {

        let one_bits: Vec<SomeBits> = self.bts.split();

        let indicies: Vec<usize> = random_x_of_n(one_bits.len() / 2, one_bits.len());

        let mut or_bts = SomeBits::new(self.num_ints());

        for inx in indicies.iter() {
            or_bts = or_bts.b_or(&one_bits[*inx]);
        }
        SomeMask::new(or_bts)
    }

} // end SomeMask

impl Clone for SomeMask {
    fn clone(&self) -> Self {
        Self {
            bts: self.bts.clone(),
        }
    }
}
