//! The SomeChange struct, which stores masks for 0->1 and 1->0 bit changes.

use crate::bits::bits_or;
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
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
            b01: SomeMask::new(initial.bts.b_not().b_and(&result.bts)),
            b10: SomeMask::new(initial.bts.b_and(&result.bts.b_not())),
        }
    }

    /// Apply a change to a state.
    pub fn apply_to_state(&self, astate: &SomeState) -> SomeState {
        let b01 = astate.bits_not().bits_and(&self.b01);
        let b10 = astate.bits_and(&self.b10);
        let to_change = b01.bits_or(&b10);

        astate.bits_xor(&to_change)
    }

    /// Return a new SomeChange struct instance, set to zeros.
    pub fn new_low(num_ints: usize) -> Self {
        Self {
            b01: SomeMask::new_low(num_ints),
            b10: SomeMask::new_low(num_ints),
        }
    }

    /// Return the logical bitwise and of two changes
    pub fn c_and(&self, other: &SomeChange) -> SomeChange {
        Self {
            b01: self.b01.bits_and(&other.b01),
            b10: self.b10.bits_and(&other.b10),
        }
    }

    /// Return the logical bitwize or of two changes
    pub fn c_or(&self, other: &SomeChange) -> SomeChange {
        Self {
            b01: self.b01.bits_or(&other.b01),
            b10: self.b10.bits_or(&other.b10),
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
    pub fn c_not(&self) -> SomeChange {
        Self {
            b01: self.b01.bits_not(),
            b10: self.b10.bits_not(),
        }
    }

    /// Return true if no bits are set
    pub fn is_low(&self) -> bool {
        if !self.b01.is_low() {
            return false;
        }
        self.b10.is_low()
    }

    /// Return the number of changes in a SomeChange instance.
    pub fn number_changes(&self) -> usize {
        self.b01.num_one_bits() + self.b10.num_one_bits()
    }

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
        if self.b01.is_subset_of(&other.b01) && self.b10.is_subset_of(&other.b10) {
            return true;
        }
        false
    }

    /// Return a string to represent a SomeChange instance.
    pub fn formatted_string(&self) -> String {
        let mut strrc = String::with_capacity(10);

        if self.b01.is_not_low() {
            let _ = write!(strrc, "0->1: {}", self.b01);
            if self.b10.is_not_low() {
                let _ = write!(strrc, ", 1->0: {}", self.b10);
            }
        } else if self.b10.is_not_low() {
            let _ = write!(strrc, "1->0: {}", self.b10);
        } else {
            let _ = write!(strrc, "(none)");
        }

        strrc
    }

    /// Create a change for translating one region to another.
    pub fn region_to_region(from: &SomeRegion, to: &SomeRegion) -> Self {
        let f_ones = SomeMask::new(bits_or(&from.state1, &from.state2));
        let f_zeros = SomeMask::new(bits_or(&from.state1.bits_not(), &from.state2.bits_not()));

        let t_ones = SomeMask::new(bits_or(&to.state1, &to.state2));
        let t_zeros = SomeMask::new(bits_or(&to.state1.bits_not(), &to.state2.bits_not()));

        let to_not_x = to.x_mask().bits_not();

        SomeChange {
            b01: f_zeros.bits_and(&t_ones).bits_and(&to_not_x),
            b10: f_ones.bits_and(&t_zeros).bits_and(&to_not_x),
        }
    }

    /// Return the number of integers used.
    pub fn num_ints(&self) -> usize {
        self.b01.num_ints()
    }
    // Return true if a change intersects another
    //    pub fn intersects(&self, other: &SomeChange) -> bool {
    //        if self.b01.m_and(&other.b01).is_not_low() {
    //            return true;
    //        }

    //        self.b10.m_and(&other.b10).is_not_low()
    //    }
} // end impl SomeChange

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn apply_to_state() -> Result<(), String> {

        // Test changing two bits.
        let reg0 = SomeRegion::new_from_string(1, "r0101").unwrap();
        let reg1 = SomeRegion::new_from_string(1, "r1001").unwrap();

        let cng = SomeChange::region_to_region(&reg0, &reg1);
        let sta = SomeState::new_from_string(1, "s0101").unwrap();
        
        let sta2 = cng.apply_to_state(&sta);

        let sta3 = SomeState::new_from_string(1, "s1001").unwrap();
        if sta2 != sta3 {
            return Err(format!("sta2 {} not {} ?", sta2, sta3));
        }

        // Test changing no bits.
        let cng = SomeChange::region_to_region(&reg0, &reg0);

        let sta2 = cng.apply_to_state(&sta);

        if sta2 != sta {
            return Err(format!("sta2 {} not {} ?", sta2, sta3));
        }
        Ok(())
    }
}
