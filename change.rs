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

    /// Return true if no bits are set
    pub fn is_low(&self) -> bool {
        self.b01.is_low() && self.b10.is_low()
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

        self.b01.is_subset_ones_of(&other.b01) && self.b10.is_subset_ones_of(&other.b10)
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

    /// Return a change for translating from a state to a region.
    pub fn new_state_to_region(from: &SomeState, to: &SomeRegion) -> SomeChange {
        debug_assert_eq!(from.num_bits(), to.num_bits());

        SomeChange {
            b01: to.edge_ones_mask().bitwise_and_not(from),
            b10: to.edge_zeros_mask().bitwise_and(from),
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

    /// Return a sample from a rule string.
    /// Like "Xx/XX/01/10/00/11".
    pub fn from(str: &str) -> Result<Self, String> {
        match SomeRule::from(str) {
            Ok(ruls) => Ok(ruls.to_change()),
            Err(errstr) => Err(format!("SomeChange::from: {errstr}")),
        }
    }

    /// Return a change after doing a bitwise and operation with a given mask.
    pub fn bitwise_and(&self, amask: &SomeMask) -> Self {
        SomeChange {
            b01: self.b01.bitwise_and(amask),
            b10: self.b10.bitwise_and(amask),
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
            b01: SomeMask::from("0b1010")?,
            b10: SomeMask::from("0b1010")?,
        };
        let cng2 = SomeChange {
            b01: SomeMask::from("0b1001")?,
            b10: SomeMask::from("0b0110")?,
        };
        let cng3 = SomeChange {
            b01: SomeMask::from("0b1000")?,
            b10: SomeMask::from("0b0010")?,
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
            b01: SomeMask::from("0b1010")?,
            b10: SomeMask::from("0b1010")?,
        };
        let cng2 = SomeChange {
            b01: SomeMask::from("0b1001")?,
            b10: SomeMask::from("0b0110")?,
        };
        let cng3 = SomeChange {
            b01: SomeMask::from("0b1011")?,
            b10: SomeMask::from("0b1110")?,
        };
        let cng4 = cng1.union(&cng2);
        println!("cng4 {cng4}");

        assert!(cng3 == cng4);

        Ok(())
    }

    #[test]
    fn apply_changes() -> Result<(), String> {
        // Create a domain that uses one integer for bits.

        let wanted_changes = SomeChange::new(SomeMask::from("0b1100")?, SomeMask::from("0b0011")?);
        println!("wanted_changes    {wanted_changes}");

        let sta1 = SomeState::from("0b0011")?;
        let sta2 = wanted_changes.apply_changes(&sta1);

        println!("Sta1 {sta1} changed by {wanted_changes} is {sta2}");
        assert!(sta2 == SomeState::from("0b1100")?);

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let cng1 = SomeChange::from("X1/X0/XX/Xx_00/01/11/10")?;
        println!("cng1 {cng1}");

        let cng2 = SomeChange::from("11/X0/XX/01_00/01/11/10")?;
        println!("cng2 {cng2}");

        assert!(cng2.is_subset_of(&cng1));
        assert!(!cng1.is_subset_of(&cng2));

        Ok(())
    }

    #[test]
    fn difference() -> Result<(), String> {
        let cng1 = SomeChange::from("X1/X0/XX/Xx_00/01/11/10")?;
        println!("cng1 {cng1}");

        let cng2 = SomeChange::from("11/X0/XX/01_00/01/11/10")?;
        println!("cng2 {cng2}");

        let cng3 = cng1.difference(&cng2);
        println!("cng3 {cng3}");

        let cng4 = SomeChange::from("01/00/00/10_00/00/11/11")?;
        println!("cng4 {cng4}");

        assert!(cng3 == cng4);

        Ok(())
    }

    #[test]
    fn new_state_to_region() -> Result<(), String> {
        let sta1 = SomeState::from("0b01_0110")?;
        println!("sta1 {sta1}");

        let reg1 = SomeRegion::from("xx_1010")?;
        println!("reg1 {reg1}");

        let cng1 = SomeChange::new_state_to_region(&sta1, &reg1);
        println!("cng1 {cng1}");

        let cng2 = SomeChange::from("00/11_/01/10/11/00")?;
        println!("cng2 {cng2}");

        assert!(cng1 == cng2);

        Ok(())
    }
}
