// State struct for an Unorthodox Expert System

use crate::bits::SomeBits;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug)]
pub struct SomeState {
    pub bts: SomeBits,
}

impl Hash for SomeState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bts.hash(state);
    }
}

impl PartialEq for SomeState {
    fn eq(&self, other: &Self) -> bool {
        self.bts == other.bts
    }
}
impl Eq for SomeState {}

impl fmt::Display for SomeState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "s{}", self.bts)
    }
}

impl SomeState {
    pub fn new(bts: SomeBits) -> Self {
        Self { bts }
    }

    pub fn clone(&self) -> Self {
        Self {
            bts: self.bts.clone(),
        }
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

    pub fn toggle_bits(&self, nums: &[usize]) -> Self {
        SomeState {
            bts: self.bts.toggle_bits(nums),
        }
    }

    // Return the number of integers used to express a SomeState innstance
    pub fn num_ints(&self) -> usize {
        self.bts.num_ints()
    }

    pub fn num_one_bits(&self) -> usize {
        self.bts.num_one_bits()
    }
}
