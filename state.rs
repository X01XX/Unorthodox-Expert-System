//! The SomeState struct. Represents a bit-pattern/square/state on a pseudo Karnaugh Map.
//!
//! The structure of a State and a Mask are the same,
//! the difference is in the intended use.
//!
//! A different mask for two states would be calculated like:
//!
//! let diff_mask = SomeMask { ints: state1.bts.b_xor(&state2.bts) };
//!
//! A difference mask applied to a state, to get a new state, would be calculated like:
//!
//! let state2 = SomeState { ints: diff_mask.bts.b_xor(&state1.bts) };

use crate::bits::SomeBits;
use crate::mask::SomeMask;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq)]
pub struct SomeState {
    bts: SomeBits,
}

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

    // Return a new state, with random bits set to one.
    pub fn new_random(num_ints: usize) -> Self {
        Self { bts: SomeBits::new_random(num_ints) }
    }

    /// Return a State from a string.
    /// Left-most, consecutive, zeros can be omitted.
    ///
    /// if let Ok(sta) = SomeState::new_from_string(1, "s0101")) {
    ///    println!("State {}", &sta);
    /// } else {
    ///    panic!("Invalid State");
    /// }
    /// A prefix of "s0x" can be used to specify hexadecimal characters.
    pub fn new_from_string(num_ints: usize, str: &str) -> Result<Self, String> {

        for chr in str.chars() {
            if chr != 's' && chr != 'S' {
                return Err(format!("Did not understand the string {}, first character?", str));
            }
            break;
        }

        match SomeBits::new_from_string(num_ints, &str[1..]) {
            Ok(bts) => {
                return Ok(SomeState::new(bts));
            }
            Err(error) => {
                return Err(error);
            }
        }
    } // end new_from_string

    /// Return true is a given bit in a state is set to one.
    pub fn is_bit_set(&self, b: usize) -> bool {
        self.bts.is_bit_set(b)
    }

    /// Return a state and another state.
    pub fn s_and(&self, other: &Self) -> Self {
        Self::new(self.bts.b_and(&other.bts))
    }

    /// Return the result of a not operation.
    pub fn s_not(&self) -> Self {
        Self::new(self.bts.b_not())
    }
    
    /// Return a state or another state.
    pub fn s_or(&self, other: &Self) -> Self {
        Self::new(self.bts.b_or(&other.bts))
    }

    /// Return a state xor another state.
    pub fn s_xor(&self, other: &Self) -> Self {
        Self::new(self.bts.b_xor(&other.bts))
    }

    /// Toggle the bits of a state, given a vector of numbers.
    pub fn toggle_bits(&self, nums: Vec<usize>) -> Self {
        SomeState {
            bts: self.bts.toggle_bits(nums),
        }
    }

    /// Return the number of integers used to represent a state.
    pub fn num_ints(&self) -> usize {
        self.bts.num_ints()
    }

    /// Return true if two squares are adjacent, that is there is exactly one bit difference.
    pub fn is_adjacent(&self, other: &SomeState) -> bool {
        self.s_xor(&other).bts.just_one_bit()
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

    /// Return a mask given a state
    pub fn to_mask(&self) -> SomeMask {
        SomeMask::new(self.bts.clone())
    }

} // end impl SomeState

/// Clone trait for SomeState
impl Clone for SomeState {
    fn clone(&self) -> Self {
        Self {
            bts: self.bts.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::state::SomeState;

    // Test SomeState::distance
    // This uses SomeBits::distance, so only a basic test is done.
    #[test]
    fn test_distance() -> Result<(), String> {
        if 2 != SomeState::new_from_string(2, "s0x0").unwrap().distance(&SomeState::new_from_string(2, "s0x11").unwrap()) {
            return Err(format!("SomeState::distance 1 failed"));
        }
        Ok(())
    }

    // Test SomeState::is_adjacent
    #[test]
    fn test_is_adjacent() -> Result<(), String> {
        if SomeState::new_from_string(2, "s0x0").unwrap().is_adjacent(&SomeState::new_from_string(2, "s0x11").unwrap()) {
            return Err(format!("SomeState::is_adjacent 1 failed"));
        }
        if SomeState::new_from_string(2, "s0x1").unwrap().is_adjacent(&SomeState::new_from_string(2, "s0x11").unwrap())  == false {
            return Err(format!("SomeState::is_adjacent 2 failed"));
        }
        if SomeState::new_from_string(2, "s0x0").unwrap().is_adjacent(&SomeState::new_from_string(2, "s0x1100").unwrap()) {
            return Err(format!("SomeState::is_adjacent 3 failed"));
        }
        if SomeState::new_from_string(2, "s0x100").unwrap().is_adjacent(&SomeState::new_from_string(2, "s0x1100").unwrap())  == false {
            return Err(format!("SomeState::is_adjacent 4 failed"));
        }
        Ok(())
    }

    // Test SomeState::is_bit_set
    // This uses SomeBits::is_bit_set, so only a basic test is done.
    #[test]
    fn test_is_bit_set() -> Result<(), String> {
        let test_sta = SomeState::new_from_string(2, "s0x5aa5").unwrap();

        if test_sta.is_bit_set(0) == false {
            return Err(format!("SomeState::is_bit_set 0 failed"));
        }

        if test_sta.is_bit_set(1) {
            return Err(format!("SomeState::is_bit_set 1 failed"));
        }
        Ok(())
    }

    // Test SomeState::s_and
    // This uses SomeBits::b_and, so only a basic test is done.
    #[test]
    fn test_s_and() -> Result<(), String> {
        let test_and = SomeState::new_from_string(2, "s0x6666").unwrap().s_and(&SomeState::new_from_string(2, "s0xc37d").unwrap());
        if test_and != SomeState::new_from_string(2, "s0x4264").unwrap() {
            return Err(format!("SomeState::s_and 1 failed"));
        }
        Ok(())
    }

    // Test SomeState::s_not
    // This uses SomeBits::b_not, so only a basic test is done.
    #[test]
    fn test_s_not() -> Result<(), String> {
        let test_not = SomeState::new_from_string(2, "s0x5a5a").unwrap().s_not();
        if test_not != SomeState::new_from_string(2, "s0xa5a5").unwrap() {
            return Err(format!("SomeState::s_not 1 failed"));
        }
        Ok(())
    }

    // Test SomeState::s_or
    // This uses SomeBits::b_or, so only a basic test is done.
    #[test]
    fn test_s_or() -> Result<(), String> {
        let test_or = SomeState::new_from_string(2, "s0x2111").unwrap().s_or(&SomeState::new_from_string(2, "s0x428a").unwrap());
        if test_or != SomeState::new_from_string(2, "s0x639b").unwrap() {
            return Err(format!("SomeState::s_or 1 failed"));
        }
        Ok(())
    }

    // Test SomeState::s_xor
    // This uses SomeBits::b_xor, so only a basic test is done.
    #[test]
    fn test_s_xor() -> Result<(), String> {
        let test_xor = SomeState::new_from_string(2, "s0x6666").unwrap().s_xor(&SomeState::new_from_string(2, "s0xc37d").unwrap());
        if test_xor != SomeState::new_from_string(2, "s0xa51b").unwrap() {
            return Err(format!("SomeState::s_xor 1 failed"));
        }
        Ok(())
    }

    // Test SomeState::toggle_bits
    #[test]
    fn test_toggle_bits() -> Result<(), String> {
        if SomeState::new_from_string(2, "s0x505").unwrap().toggle_bits(vec![1,8,11]) !=  SomeState::new_from_string(2, "s0xc07").unwrap() {
            return Err(format!("SomeState::toggle_bits 1 failed"));
        }
        Ok(())
    }

    // Test SomeState.clone
    #[test]
    fn test_clone() -> Result<(), String> {
        let tmp = SomeState::new_from_string(2, "s0x505").unwrap();
        if tmp != tmp.clone() {
            return Err(format!("SomeState::clone 1 failed"));
        }
        Ok(())
    }
}
