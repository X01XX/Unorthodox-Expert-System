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

use crate::bits::SomeBits;
use crate::bits::{BitsRef, NumBits};
use crate::mask::SomeMask;
use crate::region::AccessStates;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

#[readonly::make]
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
        Self { bts }
    }

    /// Return a State from a string.
    /// Each bit must be specified.
    ///
    /// if let Ok(sta) = SomeState::new_from_string("0b0101")) {
    ///    println!("State {sta}");
    /// } else {
    ///    panic!("Invalid State");
    /// }
    /// A prefix of "0x" can be used to specify hexadecimal characters.
    ///
    /// A first character of "s" is supported for cut-and-paste from output on console.
    pub fn new_from_string(str: &str) -> Result<Self, String> {
        // Check for first character.
        if let Some(char0) = str.graphemes(true).nth(0) {
            // Check the first character.
            if char0 == "s" || char0 == "S" {
                // Create the result from the not-first characters.
                match SomeBits::new_from_string(&str.to_string()[1..]) {
                    Ok(bts) => Ok(Self { bts }),
                    Err(error) => Err(error),
                }
            } else {
                match SomeBits::new_from_string(str) {
                    Ok(bts) => Ok(Self { bts }),
                    Err(error) => Err(error),
                }
            }
        } else {
            Err(format!(
                "SomeState::new_from_string: String {str}, no valid character?"
            ))
        }
    } // end new_from_string

    /// Return a random state value.
    pub fn new_random(&self) -> Self {
        Self {
            bts: self.bts.new_random(),
        }
    }

    /// Return a new state, all zeros.
    pub fn new_low(&self) -> Self {
        Self::new(self.bts.new_low())
    }

    /// Return a new state, all ones.
    pub fn new_high(&self) -> Self {
        Self::new(self.bts.new_high())
    }

    /// Return true if two squares are adjacent, that is there is exactly one bit difference.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        self.bts.is_adjacent(&other.bts)
    }

    /// Return the number of one bits that are different between two states.
    pub fn distance(&self, other: &Self) -> usize {
        self.bts.distance(&other.bts)
    }

    /// Return a string used to represent a state.
    fn formatted_string(&self) -> String {
        format!("s{}", self.bts)
    }

    /// Return a SomeState instance, representing a bitwise And of a state and another instance that supports the BitsRef Trait.
    pub fn bitwise_and(&self, other: &impl BitsRef) -> Self {
        debug_assert_eq!(self.num_bits(), other.bitsref().num_bits as usize);

        Self {
            bts: self.bts.b_and(other.bitsref()),
        }
    }

    /// Return a SomeState instance, representing a bitwise Or of a state and another instance that supports the BitsRef Trait.
    pub fn bitwise_or(&self, other: &impl BitsRef) -> Self {
        debug_assert_eq!(self.num_bits(), other.bitsref().num_bits as usize);

        Self {
            bts: self.bts.b_or(other.bitsref()),
        }
    }

    /// Return a SomeState instance, representing a bitwise XOr of a state and another instance that supports the BitsRef Trait.
    pub fn bitwise_xor(&self, other: &impl BitsRef) -> Self {
        debug_assert_eq!(self.num_bits(), other.bitsref().num_bits as usize);

        Self {
            bts: self.bts.b_xor(other.bitsref()),
        }
    }

    /// Return a mask of the bits values that are the same.
    pub fn bitwise_eqv(&self, other: &impl BitsRef) -> SomeMask {
        debug_assert_eq!(self.num_bits(), other.bitsref().num_bits as usize);

        SomeMask::new(self.bts.b_eqv(other.bitsref()))
    }

    /// Return the bitwise Not of a SomeState instance.
    pub fn bitwise_not(&self) -> Self {
        Self {
            bts: self.bts.b_not(),
        }
    }

    /// Return a SomeMask instance from a SomeState instance.
    pub fn convert_to_mask(self) -> SomeMask {
        SomeMask::new(self.bts)
    }

    /// Return true if a state is between two given states, exclusive.
    pub fn is_between(&self, sta1: &Self, sta2: &Self) -> bool {
        debug_assert_eq!(self.num_bits(), sta1.num_bits());
        debug_assert_eq!(self.num_bits(), sta2.num_bits());

        self.bts.is_between(&sta1.bts, &sta2.bts)
    }

    /// Return a difference mask between a state and another item.
    fn diff_edge_mask(&self, other: &impl AccessStates) -> SomeMask {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        other.edge_mask().bitwise_xor(self)
    }

    /// Return the number of bits used to describe a state.
    pub fn num_bits(&self) -> usize {
        self.bts.num_bits as usize
    }

    /// Return a SomeMask instance, representing a bitwise And-not of a mask and the invert of another instance that supports the BitsRef Trait.
    pub fn bitwise_and_not(&self, other: &impl BitsRef) -> Self {
        debug_assert_eq!(self.num_bits(), other.bitsref().num_bits as usize);

        Self {
            bts: self.bts.b_and_not(other.bitsref()),
        }
    }
} // end impl SomeState

/// Trait to allow SomeState to return a reference to its bits.
impl BitsRef for SomeState {
    fn bitsref(&self) -> &SomeBits {
        &self.bts
    }
}

/// Implement the trait StrLen for SomeState.
impl StrLen for SomeState {
    fn strlen(&self) -> usize {
        self.bts.strlen() + 1
    }
}

/// Implement the NumBits trait for SomeState.
impl NumBits for SomeState {
    fn num_bits(&self) -> usize {
        self.num_bits()
    }
}

/// Implement the trait AccessStates for SomeState.
impl AccessStates for SomeState {
    fn one_state(&self) -> bool {
        true
    }
    fn first_state(&self) -> &SomeState {
        self
    }
    fn x_mask(&self) -> SomeMask {
        self.new_low().convert_to_mask()
    }
    fn edge_mask(&self) -> SomeMask {
        self.new_high().convert_to_mask()
    }
    fn high_state(&self) -> SomeState {
        self.clone()
    }
    fn low_state(&self) -> SomeState {
        self.clone()
    }
    fn diff_edge_mask(&self, other: &impl AccessStates) -> SomeMask {
        debug_assert!(self.num_bits() == other.num_bits());

        self.diff_edge_mask(other)
    }
    fn intersects(&self, other: &impl AccessStates) -> bool {
        debug_assert!(self.num_bits() == other.num_bits());

        self.diff_edge_mask(other).is_low()
    }
    fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        debug_assert!(self.num_bits() == other.num_bits());

        if other.one_state() {
            self == other.first_state()
        } else {
            self.diff_edge_mask(other).is_low()
        }
    }
    fn is_superset_of(&self, other: &impl AccessStates) -> bool {
        debug_assert!(self.num_bits() == other.num_bits());

        if other.one_state() {
            self == other.first_state()
        } else {
            false
        }
    }
    fn num_bits(&self) -> usize {
        self.num_bits()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));
        let strrep = format!("{tmp_sta}");
        let len = strrep.len();
        let calc_len = tmp_sta.strlen();
        println!("str {tmp_sta} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_sta = SomeState::new(SomeBits::new_from_string("0+0")?);
        let strrep = format!("{tmp_sta}");
        let len = strrep.len();
        let calc_len = tmp_sta.strlen();
        println!("str {tmp_sta} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_sta = SomeState::new(SomeBits::new(6));
        let strrep = format!("{tmp_sta}");
        let len = strrep.len();
        let calc_len = tmp_sta.strlen();
        println!("str {tmp_sta} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_sta = SomeState::new(SomeBits::new(5));
        let strrep = format!("{tmp_sta}");
        let len = strrep.len();
        let calc_len = tmp_sta.strlen();
        println!("str {tmp_sta} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_sta = SomeState::new(SomeBits::new(4));
        let strrep = format!("{tmp_sta}");
        let len = strrep.len();
        let calc_len = tmp_sta.strlen();
        println!("str {tmp_sta} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let sta1 = SomeState::new_from_string("s0b10+10")?;
        let sta2 = SomeState::new_from_string("s0b10+10")?;
        println!("sta1 {sta1}");
        println!("sta2 {sta2}");
        assert!(sta1 == sta2);

        let sta3 = SomeState::new_from_string("s0b10+01")?;
        println!("sta3 {sta3}");
        assert!(sta1 != sta3);

        Ok(())
    }
}
