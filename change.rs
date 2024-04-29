//! The SomeChange struct, which stores masks for 0->1 and 1->0 bit changes.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
/// A struct to indicate 1->0 and 0->1 changes.
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
    pub fn new(b01: SomeMask, b10: SomeMask) -> Self {
        Self { b01, b10 }
    }

    /// Return a new change, with no bits set yet.
    pub fn new_low(&self) -> Self {
        Self {
            b01: self.b01.new_low(),
            b10: self.b10.new_low(),
        }
    }

    /// Return the logical bitwise and of two changes
    pub fn intersection(&self, other: &impl AccessChanges) -> Self {
        Self {
            b01: self.b01.bitwise_and(other.b01()),
            b10: self.b10.bitwise_and(other.b10()),
        }
    }

    /// Return the logical bitwise or of two changes
    pub fn union(&self, other: &impl AccessChanges) -> Self {
        Self {
            b01: self.b01.bitwise_or(other.b01()),
            b10: self.b10.bitwise_or(other.b10()),
        }
    }

    /// Return the difference of two changes
    pub fn difference(&self, other: &impl AccessChanges) -> Self {
        Self {
            b01: self.b01.bitwise_xor(other.b01()),
            b10: self.b10.bitwise_xor(other.b10()),
        }
    }

    /// Return the logical bitwise not of a change
    pub fn bitwise_not(&self) -> Self {
        Self {
            b01: self.b01.bitwise_not(),
            b10: self.b10.bitwise_not(),
        }
    }

    /// Return the logical bitwise and of a mask.
    pub fn bitwise_and(&self, msk: &SomeMask) -> Self {
        Self {
            b01: self.b01.bitwise_and(msk),
            b10: self.b10.bitwise_and(msk),
        }
    }

    /// Return true if no bits are set
    pub fn is_low(&self) -> bool {
        if !self.b01.is_low() {
            return false;
        }
        self.b10.is_low()
    }

    /// Return true if any bits are set
    pub fn is_not_low(&self) -> bool {
        !self.is_low()
    }

    /// Return the number of changes in a SomeChange instance.
    pub fn number_changes(&self) -> usize {
        self.b01.num_one_bits() + self.b10.num_one_bits()
    }

    /// Return true if a SomeChange struct is a ones-subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        if self.b01.is_subset_ones_of(&other.b01) && self.b10.is_subset_ones_of(&other.b10) {
            return true;
        }
        false
    }

    /// Return a string to represent a SomeChange instance.
    fn formatted_string(&self) -> String {
        let mut strrc = String::with_capacity(10);

        if self.b01.is_not_low() {
            strrc.push_str(&format!("0->1: {}", self.b01));
            if self.b10.is_not_low() {
                strrc.push_str(&format!(", 1->0: {}", self.b10));
            }
        } else if self.b10.is_not_low() {
            strrc.push_str(&format!("1->0: {}", self.b10));
        } else {
            strrc.push_str("(none)");
        }

        strrc
    }

    /// Return a change for translating from a region to another region.
    pub fn new_region_to_region(from: &SomeRegion, to: &SomeRegion) -> SomeChange {
        let from_x = from.x_mask();
        let from_1 = from.edge_ones_mask();
        let from_0 = from.edge_zeros_mask();

        let to_1 = to.edge_ones_mask();
        let to_0 = to.edge_zeros_mask();

        let x_to_0 = from_x.bitwise_and(&to_0);
        let x_to_1 = from_x.bitwise_and(&to_1);

        SomeChange {
            b01: from_0.bitwise_and(&to_1).bitwise_or(&x_to_1),
            b10: from_1.bitwise_and(&to_0).bitwise_or(&x_to_0),
        }
    }

    /// Return a change for translating from a state to a region.
    pub fn new_state_to_region(from: &SomeState, to: &SomeRegion) -> SomeChange {
        let from_0 = from.bitwise_not();

        let to_1 = to.edge_ones_mask();
        let to_0 = to.edge_zeros_mask();

        SomeChange {
            b01: to_1.bitwise_and(&from_0),
            b10: to_0.bitwise_and(from),
        }
    }

    /// Return the intersection of the invert of the argument.
    pub fn bitwise_and_not(&self, other: &Self) -> Self {
        SomeChange {
            b01: self.b01.bitwise_and_not(&other.b01),
            b10: self.b10.bitwise_and_not(&other.b10),
        }
    }
} // end impl SomeChange

/// Allow different types, containing 0->1 and 1->0 masks, to interact.
pub trait AccessChanges {
    /// Return a reference to the 0->1 mask.
    fn b01(&self) -> &SomeMask;
    /// Return a reference to the 1->0 mask.
    fn b10(&self) -> &SomeMask;
}

impl AccessChanges for SomeChange {
    fn b01(&self) -> &SomeMask {
        &self.b01
    }
    fn b10(&self) -> &SomeMask {
        &self.b10
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intersection() -> Result<(), String> {
        // Create a mask that uses one integer for bits.

        let cng1 = SomeChange {
            b01: SomeMask::new_from_string("m0b1010")?,
            b10: SomeMask::new_from_string("m0b1010")?,
        };
        let cng2 = SomeChange {
            b01: SomeMask::new_from_string("m0b1001")?,
            b10: SomeMask::new_from_string("m0b0110")?,
        };
        let cng3 = SomeChange {
            b01: SomeMask::new_from_string("m0b1000")?,
            b10: SomeMask::new_from_string("m0b0010")?,
        };
        let cng4 = cng1.intersection(&cng2);
        println!("cng4 {cng4}");

        assert!(cng3 == cng4);

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        // Create a mask that uses one integer for bits.

        let cng1 = SomeChange {
            b01: SomeMask::new_from_string("m0b1010")?,
            b10: SomeMask::new_from_string("m0b1010")?,
        };
        let cng2 = SomeChange {
            b01: SomeMask::new_from_string("m0b1001")?,
            b10: SomeMask::new_from_string("m0b0110")?,
        };
        let cng3 = SomeChange {
            b01: SomeMask::new_from_string("m0b1011")?,
            b10: SomeMask::new_from_string("m0b1110")?,
        };
        let cng4 = cng1.union(&cng2);
        println!("cng4 {cng4}");

        assert!(cng3 == cng4);

        Ok(())
    }
}
