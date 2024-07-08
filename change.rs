//! The SomeChange struct, which stores masks for 0->1 and 1->0 bit changes.

use crate::bits::NumBits;
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
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
        debug_assert_eq!(b01.num_bits(), b10.num_bits());

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
    pub fn intersection(&self, other: &(impl AccessChanges + NumBits)) -> Self {
        debug_assert_eq!(other.num_bits(), self.num_bits());

        Self {
            b01: self.b01.bitwise_and(other.b01()),
            b10: self.b10.bitwise_and(other.b10()),
        }
    }

    /// Return the logical bitwise or of two changes
    pub fn union(&self, other: &(impl AccessChanges + NumBits)) -> Self {
        debug_assert_eq!(other.num_bits(), self.num_bits());

        Self {
            b01: self.b01.bitwise_or(other.b01()),
            b10: self.b10.bitwise_or(other.b10()),
        }
    }

    /// Return the difference of two changes
    pub fn difference(&self, other: &(impl AccessChanges + NumBits)) -> Self {
        debug_assert_eq!(other.num_bits(), self.num_bits());

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
        debug_assert_eq!(other.num_bits(), self.num_bits());

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
        debug_assert_eq!(from.num_bits(), to.num_bits());

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
        debug_assert_eq!(from.num_bits(), to.num_bits());

        SomeChange {
            b01: to.edge_ones_mask().bitwise_and_not(from),
            b10: to.edge_zeros_mask().bitwise_and(from),
        }
    }

    /// Return the intersection of the invert of the argument.
    pub fn bitwise_and_not(&self, other: &Self) -> Self {
        debug_assert_eq!(other.num_bits(), self.num_bits());

        SomeChange {
            b01: self.b01.bitwise_and_not(&other.b01),
            b10: self.b10.bitwise_and_not(&other.b10),
        }
    }

    /// Return the result of applying a change to a state.
    pub fn apply_changes(&self, stax: &SomeState) -> SomeState {
        debug_assert_eq!(stax.num_bits(), self.num_bits());

        stax.bitwise_xor(
            &stax
                .bitwise_and(&self.b10)
                .bitwise_or(&self.b01.bitwise_and_not(stax)),
        )
    }

    /// Return the number of bits used by the change's masks.
    pub fn num_bits(&self) -> usize {
        self.b01.num_bits()
    }

    /// Restrict a change to a given region.
    pub fn restrict_to(&self, regx: &SomeRegion) -> Self {
        SomeChange {
            b01: self.b01.bitwise_and_not(&regx.low_state()),
            b10: self.b10.bitwise_and(&regx.high_state()),
        }
    }

    /// Return a sample from a rule string.
    /// Like "Xx/XX/01/10/00/11".
    pub fn new_from_string(str: &str) -> Result<Self, String> {
        match SomeRule::new_from_string(str) {
            Ok(ruls) => Ok(ruls.to_change()),
            Err(errstr) => Err(errstr),
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

impl NumBits for SomeChange {
    fn num_bits(&self) -> usize {
        self.num_bits()
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

    #[test]
    fn apply_changes() -> Result<(), String> {
        // Create a domain that uses one integer for bits.

        let wanted_changes = SomeChange::new(
            SomeMask::new_from_string("m0b11+00")?,
            SomeMask::new_from_string("m0b00+11")?,
        );
        println!("wanted_changes    {wanted_changes}");

        let sta1 = SomeState::new_from_string("s0b00+11")?;
        let sta2 = wanted_changes.apply_changes(&sta1);

        println!("Sta1 {sta1} changed by {wanted_changes} is {sta2}");
        assert!(sta2 == SomeState::new_from_string("s0b11+00")?);

        Ok(())
    }
}
