//! The SomeChange struct, which stores masks for 0->1 and 1->0 bit changes.

use crate::bits::SomeBits;
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;
use serde::{Deserialize, Serialize};
use std::fmt;

//#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq)]
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

    /// Return a new change with the given masks
    pub fn new(b01: &SomeMask, b10: &SomeMask) -> Self {
        Self {
            b01: b01.clone(),
            b10: b10.clone(),
        }
    }

    /// Return a change from an initial to a result state.
    pub fn new_from_to(initial: &SomeState, result: &SomeState) -> Self {
        Self {
            b01: initial.s_not().s_and(&result).to_mask(),
            b10: initial.s_and(&result.s_not()).to_mask(),
        }
    }

    /// Return a new SomeChange struct instance, set to zeros.
    pub fn new_low(num_ints: usize) -> Self {
        Self {
            b01: SomeMask::new(SomeBits::new(num_ints)),
            b10: SomeMask::new(SomeBits::new(num_ints)),
        }
    }

    /// Return the logical bitwise and of two changes
    pub fn c_and(&self, other: &SomeChange) -> SomeChange {
        Self {
            b01: self.b01.m_and(&other.b01),
            b10: self.b10.m_and(&other.b10),
        }
    }

    /// Return the logical bitwize or of two changes
    pub fn c_or(&self, other: &SomeChange) -> SomeChange {
        Self {
            b01: self.b01.m_or(&other.b01),
            b10: self.b10.m_or(&other.b10),
        }
    }

    // Return the logical bitwize xor of two changes
//    pub fn c_xor(&self, other: &SomeChange) -> SomeChange {
//        Self {
//            b01: self.b01.m_xor(&other.b01),
//            b10: self.b10.m_xor(&other.b10),
//        }
//    }

    // Return the logical bitwize and of a change and a mask
    // The mask is the not-x-mask of a goal, so changes that are 
    // important to consider.
//    pub fn c_and_mask(&self, msk: &SomeMask) -> SomeChange {
//        Self {
//            b01: self.b01.m_and(msk),
//            b10: self.b10.m_and(msk),
//        }
//    }

    // Return the reverse change to a given change
//    pub fn change_reverse(&self) -> SomeChange {
//        Self {
//            b01: self.b10.clone(),
//            b10: self.b01.clone(),
//        }
//    }

    // Return the logical bitwize not of a change
//    pub fn c_not(&self) -> SomeChange {
//        Self {
//            b01: self.b01.m_not(),
//            b10: self.b10.m_not(),
//        }
//    }

    /// Return true if no bits are set
    pub fn is_low(&self) -> bool {
        if self.b01.is_low() == false {
            return false;
        }
        self.b10.is_low()
    }

    // Return the number of changes in a SomeChange instance.
//    pub fn number_changes(&self) -> usize {
//        self.b01.num_one_bits() + self.b10.num_one_bits()
//    }

    // Return true if a change is not low
//    pub fn is_not_low(&self) -> bool {
//        !self.is_low()
//    }
    
    // Return the union of two SomeChange instances.
//    pub fn union(&self, other: &SomeChange) -> Self {
//        SomeChange {
//            b01: self.b01.m_or(&other.b01),
//            b10: self.b10.m_or(&other.b10),
//        }
//    }

    // Return a mask of all bit positions that are one in both masks.
//    pub fn x_mask(&self) -> SomeMask {
//        self.b01.m_and(&self.b10)
//    }

    /// Return true if a SomeChange struct is a ones-subset of another.
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
        
        if self.b01.is_not_low() {
            strrc.push_str(&format!("0->1: {}", self.b01));
            if self.b10.is_not_low() {
                strrc.push_str(&format!(", 1->0: {}", self.b10));
            }
        } else if self.b10.is_not_low() {
            strrc.push_str(&format!("1->0: {}", self.b10));
        } else {
            strrc.push_str(&format!("(none)"));
        }

//        let num_ints = self.b01.num_ints();
//        let num_bits = num_ints * NUM_BITS_PER_INT;

//        for i in (0..num_bits).rev() {
//            let b01: bool = self.b01.is_bit_set(i);

//            let b10: bool = self.b10.is_bit_set(i);

            //            if i != (num_bits - 1) {
            //                strrc.push('/');
            //            }

//            if b01 && b10 {
//                strrc.push_str(&format!("/bit{}:X->x", &i));
//            } else if b01 {
//                strrc.push_str(&format!("/bit{}:0->1", &i));
//            } else if b10 {
//                strrc.push_str(&format!("/bit{}:1->0", &i));
//            }
//        } // next i

//        if strrc.len() == 0 {
//            strrc.push_str(&format!("(none)"));
//        }

        strrc
    }

    /// Create a change for translating one region to another.
    pub fn region_to_region(from: &SomeRegion, to: &SomeRegion) -> Self {
        let f_ones  = from.state1.s_or(&from.state2).to_mask();
        let f_zeros = from.state1.s_not().s_or(&from.state2.s_not()).to_mask();

        let t_ones  = to.state1.s_or(&to.state2).to_mask();
        let t_zeros = to.state1.s_not().s_or(&to.state2.s_not()).to_mask();

        let to_not_x = to.x_mask().m_not();

        SomeChange {
            b01: f_zeros.m_and(&t_ones).m_and(&to_not_x),
            b10: f_ones.m_and(&t_zeros).m_and(&to_not_x),
        }
    }

    // Return true if a change intersects another
//    pub fn intersects(&self, other: &SomeChange) -> bool {
//        if self.b01.m_and(&other.b01).is_not_low() {
//            return true;
//        }

//        self.b10.m_and(&other.b10).is_not_low()
//    }
    
} // end impl SomeChange
