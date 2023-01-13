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

use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

//#[readonly::make]
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
    // Return a new SomeState instance.
    pub fn new(bts: SomeBits) -> Self {
        Self { bts }
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
        let mut rest = String::from("");

        for (inx, chr) in str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "s" || chr == "S" {
                    continue;
                }
                return Err(format!(
                    "Did not understand the string {}, first character?",
                    str
                ));
            }

            rest.push_str(chr);
        }

        match SomeBits::new_from_string(num_ints, &rest) {
            Ok(bts) => Ok(Self { bts }),
            Err(error) => Err(error),
        }
    } // end new_from_string

    /// Return true is a given bit in a state is set to one.
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
} // end impl SomeState

pub fn somestate_ref_vec_string(avec: &[&SomeState]) -> String {
    let mut ret_str = String::from("[");

    let mut first = true;
    for stax in avec.iter() {
        if first {
            first = false;
        } else {
            ret_str.push_str(", ");
        }
        ret_str.push_str(&format!("{}", stax));
    }
    ret_str.push(']');
    ret_str
}

impl BitsRef for SomeState {
    fn bitsref(&self) -> &SomeBits {
        &self.bts
    }
}
