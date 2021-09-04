//! The SomeChange struct, which stores masks for 0->1 and 1->0 bit changes.
//!
use crate::bits::{SomeBits, NUM_BITS_PER_INT};
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct SomeChange {
    /// A Mask for 0->1 changes.
    pub b01: SomeMask,
    /// A mask for 1->0 changes.
    pub b10: SomeMask,
}

impl fmt::Display for SomeChange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeChange {
    /// Return a change using an initial and result state.
    pub fn new(initial: &SomeState, result: &SomeState) -> Self {
        Self {
            b01: SomeMask::new(initial.bts.b_not().b_and(&result.bts)),
            b10: SomeMask::new(initial.bts.b_and(&result.bts.b_not())),
        }
    }

    /// Return a new SomeChange struct instance, set to zeros.
    pub fn new_low(num_ints: usize) -> Self {
        Self {
            b01: SomeMask::new(SomeBits::new_low(num_ints)),
            b10: SomeMask::new(SomeBits::new_low(num_ints)),
        }
    }

    /// Return the union of two SomeChange instances.
    pub fn union(&self, other: &SomeChange) -> Self {
        SomeChange {
            b01: self.b01.m_or(&other.b01),
            b10: self.b10.m_or(&other.b10),
        }
    }

    /// Return a mask of all bit positions that are one in both masks.
    pub fn x_mask(&self) -> SomeMask {
        self.b01.m_and(&self.b10)
    }

    /// Return true if a SomeChange struct is a ones-subset of anoth.
    pub fn is_subset_of(&self, other: &SomeChange) -> bool {
        if self.b01.is_subset_of(&other.b01) {
            if self.b10.is_subset_of(&other.b10) {
                return true;
            }
        }
        false
    }

    /// Return a string to represent a SomeChange instance.
    pub fn formatted_string(&self) -> String {
        let mut strrc = String::with_capacity(10);

        let num_ints = self.b01.num_ints();
        let num_bits = num_ints * NUM_BITS_PER_INT;

        for i in (0..num_bits).rev() {
            let b01: bool = self.b01.is_bit_set(i);

            let b10: bool = self.b10.is_bit_set(i);

            //            if i != (num_bits - 1) {
            //                strrc.push('/');
            //            }

            if b01 && b10 {
                strrc.push_str(&format!("/bit{}:X->x", &i));
            } else if b01 {
                strrc.push_str(&format!("/bit{}:0->1", &i));
            } else if b10 {
                strrc.push_str(&format!("/bit{}:1->0", &i));
            }
        } // next i

        strrc
    }

    /// Create a change for translating a state to a region.
    pub fn state_to_region(from: &SomeState, to: &SomeRegion) -> Self {

        let t_ones = to.state1.bts.b_and(&to.state2.bts);
        let t_zeros = to.state1.bts.b_or(&to.state2.bts).b_not();

        SomeChange {
            b01: SomeMask::new(from.bts.b_not().b_and(&t_ones)),
            b10: SomeMask::new(from.bts.b_and(&t_zeros)),
        }
    }
} // end impl SomeChange
