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
    pub m01: SomeMask,
    /// A mask for 1->0 changes.
    pub m10: SomeMask,
}

impl fmt::Display for SomeChange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

impl SomeChange {
    /// Return a new change with the given masks
    pub fn new(m01: SomeMask, m10: SomeMask) -> Self {
        debug_assert_eq!(m01.num_bits(), m10.num_bits());

        Self { m01, m10 }
    }

    /// Return a new change, with no bits set yet.
    pub fn new_low(&self) -> Self {
        Self {
            m01: self.m01.new_low(),
            m10: self.m10.new_low(),
        }
    }

    /// Return the logical bitwise and of two changes
    pub fn intersection(&self, other: &(impl AccessChanges + NumBits)) -> Self {
        debug_assert_eq!(other.num_bits(), self.num_bits());

        Self {
            m01: self.m01.bitwise_and(other.m01()),
            m10: self.m10.bitwise_and(other.m10()),
        }
    }

    /// Return the logical bitwise or of two changes
    pub fn union(&self, other: &(impl AccessChanges + NumBits)) -> Self {
        debug_assert_eq!(other.num_bits(), self.num_bits());

        Self {
            m01: self.m01.bitwise_or(other.m01()),
            m10: self.m10.bitwise_or(other.m10()),
        }
    }

    /// Return the difference of two changes
    pub fn difference(&self, other: &(impl AccessChanges + NumBits)) -> Self {
        debug_assert_eq!(other.num_bits(), self.num_bits());

        Self {
            m01: self.m01.bitwise_xor(other.m01()),
            m10: self.m10.bitwise_xor(other.m10()),
        }
    }

    /// Return true if no bits are set
    pub fn is_low(&self) -> bool {
        self.m01.is_low() && self.m10.is_low()
    }

    /// Return true if any bits are set
    pub fn is_not_low(&self) -> bool {
        !self.is_low()
    }

    /// Return the number of changes in a SomeChange instance.
    pub fn number_changes(&self) -> usize {
        self.m01.num_one_bits() + self.m10.num_one_bits()
    }

    /// Return true if a SomeChange struct is a ones-subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        debug_assert_eq!(other.num_bits(), self.num_bits());

        self.m01.is_subset_ones_of(&other.m01) && self.m10.is_subset_ones_of(&other.m10)
    }

    /// Return a string to represent a SomeChange instance.
    fn formatted_str(&self) -> String {
        SomeRule {
            m00: self.m10.new_low(),
            m01: self.m01.clone(),
            m11: self.m10.new_low(),
            m10: self.m10.clone(),
        }
        .formatted_str()
    }

    /// Return a change for translating from a state to a region.
    pub fn new_state_to_region(from: &SomeState, to: &SomeRegion) -> SomeChange {
        debug_assert_eq!(from.num_bits(), to.num_bits());

        SomeChange {
            m01: to.edge_ones_mask().bitwise_and_not(from),
            m10: to.edge_zeros_mask().bitwise_and(from),
        }
    }

    /// Return the result of applying a change to a state.
    pub fn apply_changes(&self, stax: &SomeState) -> SomeState {
        debug_assert_eq!(stax.num_bits(), self.num_bits());

        stax.bitwise_xor(
            &stax
                .bitwise_and(&self.m10)
                .bitwise_or(&self.m01.bitwise_and_not(stax)),
        )
        .as_state()
    }

    /// Return the number of bits used by the change's masks.
    pub fn num_bits(&self) -> usize {
        self.m01.num_bits()
    }

    /// Return a sample from a rule string.
    /// Like "Xx/XX/01/10/00/11".
    pub fn from_str(str_in: &str) -> Result<Self, String> {
        let str2 = str_in.trim();

        if str2.is_empty() {
            return Err("SomeChange::from_str: Empty string?".to_string());
        }

        match SomeRule::from_str(str2) {
            Ok(ruls) => {
                if ruls.m00.is_not_low() {
                    Err("SomeChange::from_str: invalid token, 00, X0 or XX?".to_string())
                } else if ruls.m11.is_not_low() {
                    Err("SomeChange::from_str: invalid token, 11, X1 or XX?".to_string())
                } else {
                    Ok(ruls.to_change())
                }
            }
            Err(errstr) => Err(format!("SomeChange::from_str: {errstr}")),
        }
    }

    /// Return a change after doing a bitwise and operation with a given mask.
    pub fn bitwise_and(&self, amask: &SomeMask) -> Self {
        SomeChange {
            m01: self.m01.bitwise_and(amask),
            m10: self.m10.bitwise_and(amask),
        }
    }
} // end impl SomeChange

/// Allow different types, containing 0->1 and 1->0 masks, to interact.
pub trait AccessChanges {
    /// Return a reference to the 0->1 mask.
    fn m01(&self) -> &SomeMask;
    /// Return a reference to the 1->0 mask.
    fn m10(&self) -> &SomeMask;
}

impl AccessChanges for SomeChange {
    fn m01(&self) -> &SomeMask {
        &self.m01
    }
    fn m10(&self) -> &SomeMask {
        &self.m10
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
            m01: SomeMask::from_str("m1010")?,
            m10: SomeMask::from_str("m1010")?,
        };
        let cng2 = SomeChange {
            m01: SomeMask::from_str("m1001")?,
            m10: SomeMask::from_str("m0110")?,
        };
        let cng3 = SomeChange {
            m01: SomeMask::from_str("m1000")?,
            m10: SomeMask::from_str("m0010")?,
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
            m01: SomeMask::from_str("m1010")?,
            m10: SomeMask::from_str("m1010")?,
        };
        let cng2 = SomeChange {
            m01: SomeMask::from_str("m1001")?,
            m10: SomeMask::from_str("m0110")?,
        };
        let cng3 = SomeChange {
            m01: SomeMask::from_str("m1011")?,
            m10: SomeMask::from_str("m1110")?,
        };
        let cng4 = cng1.union(&cng2);
        println!("cng4 {cng4}");

        assert!(cng3 == cng4);

        Ok(())
    }

    #[test]
    fn apply_changes() -> Result<(), String> {
        // Create a domain that uses one integer for bits.

        let wanted_changes = SomeChange::from_str("01/01/10/10")?;
        println!("wanted_changes    {wanted_changes}");

        let sta1 = SomeState::from_str("s0011")?;
        let sta2 = wanted_changes.apply_changes(&sta1);

        println!("Sta1 {sta1} changed by {wanted_changes} is {sta2}");
        assert!(sta2 == SomeState::from_str("s1100")?);

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let cng1 = SomeChange::from_str("01/10/../Xx_../01/../10")?;
        println!("cng1 {cng1}");

        let cng2 = SomeChange::from_str("../10/../01_../01/../10")?;
        println!("cng2 {cng2}");

        assert!(cng2.is_subset_of(&cng1));
        assert!(!cng1.is_subset_of(&cng2));

        Ok(())
    }

    #[test]
    fn difference() -> Result<(), String> {
        let cng1 = SomeChange::from_str("01/10/../Xx_../01/../10")?;
        println!("cng1 {cng1}");

        let cng2 = SomeChange::from_str("../10/../01_../01/../10")?;
        println!("cng2 {cng2}");

        let cng3 = cng1.difference(&cng2);
        println!("cng3 {cng3}");

        let cng4 = SomeChange::from_str("01/../../10_../../../..")?;
        println!("cng4 {cng4}");

        assert!(cng3 == cng4);

        Ok(())
    }

    #[test]
    fn new_state_to_region() -> Result<(), String> {
        let sta1 = SomeState::from_str("s01_0110")?;
        println!("sta1 {sta1}");

        let reg1 = SomeRegion::from_str("xx_1010")?;
        println!("reg1 {reg1}");

        let cng1 = SomeChange::new_state_to_region(&sta1, &reg1);
        println!("cng1 {cng1}");

        let cng2 = SomeChange::from_str("../.._/01/10/../..")?;
        println!("cng2 {cng2}");

        assert!(cng1 == cng2);

        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let cng1_str = "01/10/Xx/..";
        let cng1 = SomeChange::from_str(&cng1_str)?;
        println!("str {cng1_str} cng1 {cng1}");
        assert!(format!("{cng1}") == cng1_str);
        //assert!(1 == 2);
        Ok(())
    }
}
