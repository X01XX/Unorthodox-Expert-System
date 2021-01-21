// State struct for an Unorthodox Expert System

use crate::bits::SomeBits;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;

#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq)]
pub struct SomeState {
    pub bts: SomeBits,
}

impl fmt::Display for SomeState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeState {
    pub fn new(bts: SomeBits) -> Self {
        Self { bts }
    }

    // Return true is a given bit is set to one.
    pub fn is_bit_set(&self, b: usize) -> bool {
        self.bts.is_bit_set(b)
    }

    // Return true if a state is between two other states
    //    pub fn is_between(&self, sta1: &SomeState, sta2: &SomeState) -> bool {
    //        self.bts
    //            .b_xor(&sta1.bts)
    //            .b_and(&self.bts.b_xor(&sta2.bts))
    //            .is_low()
    //    }

    // Return a state and another
    pub fn s_and(&self, other: &Self) -> Self {
        Self::new(self.bts.b_and(&other.bts))
    }

    // Return a state or another
    pub fn s_or(&self, other: &Self) -> Self {
        Self::new(self.bts.b_or(&other.bts))
    }

    // Return a state xor another
    pub fn s_xor(&self, other: &Self) -> Self {
        Self::new(self.bts.b_xor(&other.bts))
    }

    // Return the not of a state
    //    pub fn s_not(&self) -> Self {
    //        Self::new(self.bts.b_not())
    //    }

    pub fn toggle_bits(&self, nums: Vec<usize>) -> Self {
        SomeState {
            bts: self.bts.toggle_bits(nums),
        }
    }

    // Return the number of integers used to express a SomeState innstance
    pub fn num_ints(&self) -> usize {
        self.bts.num_ints()
    }

    // Return true if two squares are adjacent, that is there is exactly one bit difference.
    pub fn is_adjacent(&self, other: &SomeState) -> bool {
        self.s_xor(&other).bts.just_one_bit()
    }

    // Return the number of one bits that are different between two states
    pub fn distance(&self, other: &SomeState) -> usize {
        self.bts.distance(&other.bts)
    }

    pub fn formatted_string_length(&self) -> usize {
        self.bts.formatted_string_length()
    }

    pub fn formatted_string(&self) -> String {
        self.bts.formatted_string('s')
    }

    // Return a State from a string, like "s0101".
    // Left-most, consecutive, zeros can be omitted.
    pub fn from_string(num_ints: usize, str: &str) -> Result<SomeState, String> {
        let mut bts = SomeBits::bits_new_low(num_ints);

        let mut inx = -1;

        for ch in str.chars() {
            inx += 1;

            if inx == 0 {
                if ch == 's' {
                    continue;
                } else {
                    return Err(String::from("initial character should be s"));
                }
            }

            if bts.high_bit_set() {
                return Err(String::from("too long"));
            }

            if ch == '0' {
                bts = bts.shift_left();
            } else if ch == '1' {
                bts = bts.push_1();
            } else if ch == '_' {
                continue;
            } else {
                return Err(String::from("invalid character"));
            }
        } // end for ch

        Ok(SomeState::new(bts))
    } // end state_from_string
} // end impl SomeState

impl Clone for SomeState {
    fn clone(&self) -> Self {
        Self {
            bts: self.bts.clone(),
        }
    }
}
