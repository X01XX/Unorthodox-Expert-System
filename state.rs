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
use crate::mask::SomeMask;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq, Clone)]
pub struct SomeState {
    pub bts: SomeBits,
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
        Self {
            bts: SomeBits::new_random(num_ints),
        }
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
        if &str[0..1] != "s" {
            return Err(format!(
                "Did not understand the string {}, first character?",
                str
            ));
        }

        if str.len() > 2 && (&str[1..3] == "0b" || &str[1..3] == "0x") {
            match SomeBits::new_from_string(num_ints, &(&str[1..])) {
                Ok(bts) => {
                    return Ok(SomeState::new(bts));
                }
                Err(error) => {
                    return Err(error);
                }
            }
        } else {
            match SomeBits::new_from_string(num_ints, &("0b".to_owned() + &str[1..])) {
                Ok(bts) => {
                    return Ok(SomeState::new(bts));
                }
                Err(error) => {
                    return Err(error);
                }
            }
        }
    } // end new_from_string

    /// Return true is a given bit in a state is set to one.
    pub fn is_bit_set(&self, b: usize) -> bool {
        self.bts.is_bit_set(b)
    }

    /// Toggle the bits of a state, given a vector of numbers.
    pub fn toggle_bits(&self, nums: &str) -> Self {
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

    /// Return a mask from a state.
    pub fn to_mask(&self) -> SomeMask {
        SomeMask::new(self.bts.clone())
    }
} // end impl SomeState
