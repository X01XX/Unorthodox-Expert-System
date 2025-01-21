//! The SomeChange struct, which stores masks for 0->1 and 1->0 bit changes.

use crate::bits::NumBits;
use crate::mask::SomeMask;
use crate::rule::SomeRule;
use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;
use unicode_segmentation::UnicodeSegmentation;

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

    /// Return true if there is anf X->x bit positions.
    /// Instances used for aggregated changes may have Xx positions.
    /// Instances used for wanted changes will not have Xx positions,
    /// wanted changes are 01, or 10, but not both in the same position.
    pub fn any_x_to_x_not(&self) -> bool {
        self.m01.bitwise_and(&self.m10).is_not_low()
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

impl FromStr for SomeChange {
    type Err = String;
    /// Return SomeChange from a rule string.
    /// Like "01/10/Xx/..", ".." means no change in that bit position.
    /// Each bit position must be specified.
    /// An underscore, "_", character can be used as a visual separator, and is ignored.
    /// Spaces are ignored.
    fn from_str(str_in: &str) -> Result<Self, String> {
        let str2 = str_in.trim();

        if str2.is_empty() {
            return Err("SomeChange::from_str: Empty string?".to_string());
        }

        // Convert ".." to "XX". 0->0, 1->1 will be discarded later.
        let mut str3 = String::new();
        for chr in str2.graphemes(true) {
            if chr == "." {
                str3.push('X');
            } else {
                str3.push_str(chr);
            }
        }

        match SomeRule::from_str(&str3) {
            Ok(ruls) => Ok(ruls.as_change()),
            Err(errstr) => Err(format!("SomeChange::from_str: {errstr}")),
        }
    }
}

/// Implement the trait StrLen for SomeChange.
impl tools::StrLen for SomeChange {
    fn strlen(&self) -> usize {
        (3 * self.num_bits()) - 1 // Same as rule::strlen.
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intersection() -> Result<(), String> {
        let cng1 = SomeChange::from_str("Xx/../Xx/..")?;

        let cng2 = SomeChange::from_str("01/10/10/01")?;

        let cng3 = cng1.intersection(&cng2);
        println!("cng3 {cng3}");

        assert!(cng3 == SomeChange::from_str("01/../10/..")?);

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let cng1 = SomeChange::from_str("Xx/../Xx/..")?;

        let cng2 = SomeChange::from_str("01/10/10/01")?;

        let cng3 = cng1.union(&cng2);
        println!("cng3 {cng3}");

        assert!(cng3 == SomeChange::from_str("Xx/10/Xx/01")?);

        Ok(())
    }

    #[test]
    fn apply_changes() -> Result<(), String> {
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

        assert!(cng3 == SomeChange::from_str("01/../../10_../../../..")?);

        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let cng1_str = "01/10/Xx/..";
        let cng1 = SomeChange::from_str(&cng1_str)?;
        println!("str {cng1_str} cng1 {cng1}");

        assert!(format!("{cng1}") == cng1_str);

        Ok(())
    }
}
