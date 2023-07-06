//! The SomeState struct. Represents a bit-pattern/square/state on a pseudo Karnaugh Map.
//!
//! The structure of a State and a Mask are the same,
//! the difference is in the intended use.
//!
//! A different mask for two states would be calculated like:
//!
//! let diff_mask = SomeMask::new(state1.bts.b_xor(&state2.bts));
//!
//! A difference mask applied to a state, to get a new state, would be calculated like:
//!
//! let state2 = SomeState::new(diff_mask.bts.b_xor(&state1.bts))

use crate::bits::BitsRef;
use crate::bits::SomeBits;
use crate::mask::SomeMask;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

//#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq, Clone)]
/// A State, a set of bits.  It could be thought of as a square on a Karnaugh Map.
pub struct SomeState {
    pub bts: SomeBits,
}

/// Implement the fmt::Display Trait for a SomeState instance.
impl fmt::Display for SomeState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeState {
    /// Return a new SomeState instance, given a SomeBits instance.
    pub fn new(bts: SomeBits) -> Self {
        assert!(bts.num_ints() > 0);
        Self { bts }
    }

    /// Return a State from a string.
    /// Left-most, consecutive, zeros can be omitted.
    ///
    /// if let Ok(sta) = SomeState::new_from_string(1, "s0b0101")) {
    ///    println!("State {}", &sta);
    /// } else {
    ///    panic!("Invalid State");
    /// }
    /// A prefix of "s0x" can be used to specify hexadecimal characters.
    pub fn new_from_string(num_ints: usize, str: &str) -> Result<Self, String> {
        assert!(num_ints > 0);
        let mut rest = String::new();

        for (inx, chr) in str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "s" || chr == "S" {
                    continue;
                }
                return Err(format!(
                    "Did not understand the string {str}, first character?"
                ));
            }

            rest.push_str(chr);
        }

        match SomeBits::new_from_string(num_ints, &rest) {
            Ok(bts) => Ok(Self { bts }),
            Err(error) => Err(error),
        }
    } // end new_from_string

    /// Return a random state value.
    pub fn new_random(num_ints: usize) -> SomeState {
        assert!(num_ints > 0);
        SomeState {
            bts: SomeBits::new_random(num_ints),
        }
    }

    /// Return a new state, all zeros.
    pub fn new_low(num_ints: usize) -> Self {
        Self::new(SomeBits::new_low(num_ints))
    }

    /// Return true if a given bit in a state is set to one.
    pub fn is_bit_set(&self, b: usize) -> bool {
        self.bts.is_bit_set(b)
    }

    /// Return the number of integers used to represent a state.
    pub fn num_ints(&self) -> usize {
        self.bts.num_ints()
    }

    /// Return the number of bits used to represent a state.
    pub fn num_bits(&self) -> usize {
        self.bts.num_bits()
    }

    /// Return true if two squares are adjacent, that is there is exactly one bit difference.
    pub fn is_adjacent(&self, other: &SomeState) -> bool {
        self.bts.is_adjacent(&other.bts)
    }

    /// Return the number of one bits that are different between two states.
    pub fn distance(&self, other: &SomeState) -> usize {
        self.bts.distance(&other.bts)
    }

    /// Return the expected length of a string used to represent a state.
    pub fn formatted_string_length(&self) -> usize {
        self.bts.formatted_string_length()
    }

    /// Return a string used to represent a state.
    pub fn formatted_string(&self) -> String {
        self.bts.formatted_string('s')
    }

    /// Return a SomeState instance, representing a bitwise And of a state and another instance that supports the BitsRef Trait.
    pub fn bitwise_and(&self, other: &impl BitsRef) -> Self {
        SomeState {
            bts: self.bts.b_and(other.bitsref()),
        }
    }

    /// Return a SomeState instance, representing a bitwise Or of a state and another instance that supports the BitsRef Trait.
    pub fn bitwise_or(&self, other: &impl BitsRef) -> Self {
        SomeState {
            bts: self.bts.b_or(other.bitsref()),
        }
    }

    /// Return a SomeState instance, representing a bitwise XOr of a state and another instance that supports the BitsRef Trait.
    pub fn bitwise_xor(&self, other: &impl BitsRef) -> Self {
        SomeState {
            bts: self.bts.b_xor(other.bitsref()),
        }
    }

    /// Return a copy of an instance, with a bit position changed.
    pub fn change_bit(&self, num: usize) -> Self {
        SomeState {
            bts: self.bts.change_bit(num),
        }
    }

    /// Return a copy of an instance, with a bit position set to 1.
    pub fn set_bit_to_1(&self, num: usize) -> Self {
        SomeState {
            bts: self.bts.set_bit_to_1(num),
        }
    }

    /// Return a copy of an instance, with a bit position set to 0.
    pub fn set_bit_to_0(&self, num: usize) -> Self {
        SomeState {
            bts: self.bts.set_bit_to_0(num),
        }
    }

    /// Return a mask of the bits values that are the same.
    pub fn bitwise_eqv(&self, other: &impl BitsRef) -> SomeMask {
        SomeMask::new(self.bts.b_eqv(other.bitsref()))
    }

    /// Return the bitwise Not of a SomeState instance.
    pub fn bitwise_not(&self) -> Self {
        SomeState {
            bts: self.bts.b_not(),
        }
    }

    /// Return a SomeMask instance from a SomeState instance.
    pub fn to_mask(&self) -> SomeMask {
        SomeMask::new(self.bts.clone())
    }

    /// Return a string to display a vector of SomeStates.
    pub fn vec_string(avec: &[SomeState]) -> String {
        let mut ret_str = String::from("[");

        for (inx, stax) in avec.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&format!("{stax}"));
        }
        ret_str.push(']');
        ret_str
    }

    /// Return a string to display a vector of SomeState references.
    pub fn vec_ref_string(avec: &[&SomeState]) -> String {
        let mut ret_str = String::from("[");

        for (inx, stax) in avec.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&format!("{stax}"));
        }
        ret_str.push(']');
        ret_str
    }

    /// Return an X mask for a non-empty vector of states.
    pub fn vec_x_mask(avec: &Vec<SomeState>) -> SomeMask {
        assert!(!avec.is_empty());

        let mut x_mask = SomeMask::new_low(avec[0].num_ints());
        for stax in avec.iter().skip(1) {
            x_mask = x_mask.bitwise_or(&stax.bitwise_xor(&avec[0]));
        }
        x_mask
    }

    /// Check for duplicate states in a vector of states.
    /// Panic if any found.
    pub fn vec_check_for_duplicates(avec: &Vec<SomeState>) -> bool {
        if avec.len() < 2 {
            return false;
        }
        for inx in 0..(avec.len() - 1) {
            for iny in (inx + 1)..avec.len() {
                if avec[inx] == avec[iny] {
                    return true;
                }
            }
        }
        false
    }

    /// Check if any items are between another two.
    /// So each item adds at least one X-bit position to a region formed by the states.
    pub fn vec_check_for_unneeded(avec: &Vec<SomeState>) -> bool {
        if avec.len() < 2 {
            return false;
        }
        for inx in 0..(avec.len() - 1) {
            for iny in (inx + 1)..avec.len() {
                for inz in 0..avec.len() {
                    if inz == inx || inz == iny {
                        continue;
                    }
                    let diff = (avec[inz].bitwise_xor(&avec[inx]))
                        .to_mask()
                        .bitwise_and(&avec[inz].bitwise_xor(&avec[iny]));
                    if diff.is_low() {
                        return true;
                    }
                } // next inz
            } // next iny
        } // next inx
        false
    }
} // end impl SomeState

/// Trait to allow SomeState to return a reference to its bits.
impl BitsRef for SomeState {
    fn bitsref(&self) -> &SomeBits {
        &self.bts
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn distance() -> Result<(), String> {
        let sta1 = SomeState::new_from_string(2, "s0xabc4")?;
        let sta2 = SomeState::new_from_string(2, "s0x5430")?;

        let dist = sta1.distance(&sta2);
        println!("sta1 {sta1}");
        println!("sta2 {sta2}");
        assert!(13 == dist);
        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let sta1 = SomeState::new_from_string(1, "s0b1010")?;
        let sta2 = SomeState::new_from_string(1, "s0b1010")?;
        println!("sta1 {sta1}");
        println!("sta2 {sta2}");
        assert!(sta1 == sta2);

        let sta3 = SomeState::new_from_string(1, "s0b1001")?;
        println!("sta3 {sta3}");
        assert!(sta1 != sta3);

        Ok(())
    }
}
