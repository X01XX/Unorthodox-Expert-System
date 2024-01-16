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
use crate::change::SomeChange;
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
        write!(f, "s{}", self.formatted_string())
    }
}

impl SomeState {
    /// Return a new SomeState instance, given a SomeBits instance.
    pub fn new(bts: SomeBits) -> Self {
        Self { bts }
    }

    /// Return a State from a string.
    /// Left-most, consecutive, zeros can be omitted.
    ///
    /// if let Ok(sta) = SomeState::new_from_string(1, "s0b0101")) {
    ///    println!("State {sta}");
    /// } else {
    ///    panic!("Invalid State");
    /// }
    /// A prefix of "s0x" can be used to specify hexadecimal characters.
    pub fn new_from_string(&self, str: &str) -> Result<Self, String> {
        let mut rest = String::new();

        let mut leading_zero = false;
        let mut base_indicator = false;
        for (inx, chr) in str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "s" || chr == "S" {
                    continue;
                }
                return Err(format!(
                    "Did not understand the string {str}, first character?"
                ));
            }
            if inx == 1 && chr == "0" {
                leading_zero = true;
            }
            if inx == 2 && (chr == "b" || chr == "x") {
                base_indicator = true;
            }
            rest.push_str(chr);
        }

        if leading_zero && base_indicator {
        } else {
            rest = "0b".to_owned() + &rest;
        }
        match self.bts.new_from_string(&rest) {
            Ok(bts) => Ok(Self { bts }),
            Err(error) => Err(error),
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

    /// Return true if a given bit in a state is set to one.
    pub fn is_bit_set(&self, b: usize) -> bool {
        self.bts.is_bit_set(b)
    }

    /// Return the number of bits used to represent a state.
    pub fn num_bits(&self) -> usize {
        self.bts.num_bits
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
        self.bts.to_string()
    }

    /// Return a SomeState instance, representing a bitwise And of a state and another instance that supports the BitsRef Trait.
    pub fn bitwise_and(&self, other: &impl BitsRef) -> Self {
        Self {
            bts: self.bts.b_and(other.bitsref()),
        }
    }

    /// Return a SomeState instance, representing a bitwise Or of a state and another instance that supports the BitsRef Trait.
    pub fn bitwise_or(&self, other: &impl BitsRef) -> Self {
        Self {
            bts: self.bts.b_or(other.bitsref()),
        }
    }

    /// Return a SomeState instance, representing a bitwise XOr of a state and another instance that supports the BitsRef Trait.
    pub fn bitwise_xor(&self, other: &impl BitsRef) -> Self {
        Self {
            bts: self.bts.b_xor(other.bitsref()),
        }
    }

    /// Return a copy of an instance, with a bit position changed.
    pub fn change_bit(&self, num: usize) -> Self {
        Self {
            bts: self.bts.change_bit(num),
        }
    }

    /// Return a mask of the bits values that are the same.
    pub fn bitwise_eqv(&self, other: &impl BitsRef) -> SomeMask {
        SomeMask::new(self.bts.b_eqv(other.bitsref()))
    }

    /// Return the bitwise Not of a SomeState instance.
    pub fn bitwise_not(&self) -> Self {
        Self {
            bts: self.bts.b_not(),
        }
    }

    /// Return a SomeMask instance from a SomeState instance.
    pub fn to_mask(&self) -> SomeMask {
        SomeMask::new(self.bts.clone())
    }

    /// Return true if a state is between two given states, exclusive.
    pub fn is_between(&self, sta1: &Self, sta2: &Self) -> bool {
        if self == sta1 {
            return false;
        }
        if self == sta2 {
            return false;
        }

        self.bitwise_xor(sta1)
            .bitwise_and(&self.bitwise_xor(sta2))
            .to_mask()
            .is_low()
    }

    /// Return a difference mask between a satate and another item.
    fn diff_mask(&self, other: &impl AccessStates) -> SomeMask {
        match other.one_state() {
            true => self.bitwise_xor(other.first_state()).to_mask(),
            false => other
                .edge_mask()
                .bitwise_and(&self.bitwise_xor(other.first_state())),
        }
    }

    /// Return the result of applying a change to a state.
    pub fn apply_changes(&self, changes: &SomeChange) -> SomeState {
        self.bitwise_xor(
            &self
                .bitwise_and(&changes.b10)
                .bitwise_or(&self.bitwise_not().bitwise_and(&changes.b01)),
        )
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

/// Implement the trait AccessStates for SomeState.
impl AccessStates for SomeState {
    fn one_state(&self) -> bool {
        true
    }
    fn first_state(&self) -> &SomeState {
        self
    }
    fn x_mask(&self) -> SomeMask {
        self.new_low().to_mask()
    }
    fn edge_mask(&self) -> SomeMask {
        self.new_high().to_mask()
    }
    fn high_state(&self) -> SomeState {
        self.clone()
    }
    fn low_state(&self) -> SomeState {
        self.clone()
    }
    fn diff_mask(&self, other: &impl AccessStates) -> SomeMask {
        self.diff_mask(other)
    }
    fn intersects(&self, other: &impl AccessStates) -> bool {
        self.diff_mask(other).is_low()
    }
    fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        match other.one_state() {
            true => self == other.first_state(),
            false => self.diff_mask(other).is_low(),
        }
    }
    fn is_superset_of(&self, other: &impl AccessStates) -> bool {
        match other.one_state() {
            true => self == other.first_state(),
            false => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strlen() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));
        let strrep = format!("{tmp_sta}");
        let len = strrep.len();
        let calc_len = tmp_sta.strlen();
        println!("str {tmp_sta} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_sta = SomeState::new(SomeBits::new(16));
        let strrep = format!("{tmp_sta}");
        let len = strrep.len();
        let calc_len = tmp_sta.strlen();
        println!("str {tmp_sta} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn test_is_between() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));
        let sta2 = tmp_sta.new_from_string("s0b0010")?;
        let sta3 = tmp_sta.new_from_string("s0b0011")?;
        let sta5 = tmp_sta.new_from_string("s0b0101")?;
        assert!(sta3.is_between(&sta2, &sta5));
        assert!(!sta5.is_between(&sta2, &sta3));
        Ok(())
    }

    #[test]
    fn distance() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(16));

        let sta1 = tmp_sta.new_from_string("s0xabc4")?;
        let sta2 = tmp_sta.new_from_string("s0x5430")?;

        let dist = sta1.distance(&sta2);
        println!("sta1 {sta1}");
        println!("sta2 {sta2}");
        assert!(13 == dist);
        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));

        let sta1 = tmp_sta.new_from_string("s0b1010")?;
        let sta2 = tmp_sta.new_from_string("s0b1010")?;
        println!("sta1 {sta1}");
        println!("sta2 {sta2}");
        assert!(sta1 == sta2);

        let sta3 = tmp_sta.new_from_string("s0b1001")?;
        println!("sta3 {sta3}");
        assert!(sta1 != sta3);

        Ok(())
    }

    #[test]
    fn apply_changes() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let ur_bits = SomeBits::new(8);
        let ur_state = SomeState::new(ur_bits.clone());
        let ur_mask = SomeMask::new(ur_bits);

        let wanted_changes = SomeChange::new(
            ur_mask.new_from_string("m0b1100")?,
            ur_mask.new_from_string("m0b0011")?,
        );
        println!("wanted_changes    {wanted_changes}");

        let sta1 = ur_state.new_from_string("s0011")?;
        let sta2 = sta1.apply_changes(&wanted_changes);

        println!("Sta1 {sta1} changed by {wanted_changes} is {sta2}");
        assert!(sta2 == ur_state.new_from_string("s1100")?);

        Ok(())
    }
}
