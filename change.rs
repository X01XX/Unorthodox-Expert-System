//! The SomeChange struct, which stores masks for 0->1 and 1->0 bit changes.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::rule::SomeRule;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
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
    pub fn bitwise_and(&self, other: &Self) -> Self {
        Self {
            b01: self.b01.bitwise_and(&other.b01),
            b10: self.b10.bitwise_and(&other.b10),
        }
    }

    /// Return the logical bitwise and of a change and a rule.
    pub fn bitwise_and_rule(&self, other: &SomeRule) -> Self {
        Self {
            b01: self.b01.bitwise_and(&other.b01),
            b10: self.b10.bitwise_and(&other.b10),
        }
    }

    /// Return the logical bitwize or of two changes
    pub fn bitwise_or(&self, other: &Self) -> Self {
        Self {
            b01: self.b01.bitwise_or(&other.b01),
            b10: self.b10.bitwise_or(&other.b10),
        }
    }

    /// Return the logical bitwize or of a change and a rule.
    pub fn bitwise_or_rule(&self, other: &SomeRule) -> Self {
        Self {
            b01: self.b01.bitwise_or(&other.b01),
            b10: self.b10.bitwise_or(&other.b10),
        }
    }

    /// Return a mask of all bit positions that can change both ways.
    pub fn bits_change_mask(&self) -> SomeMask {
        self.b01.bitwise_and(&self.b10)
    }

    /// Return the logical bitwize not of a change
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
        if self.b01.is_subset_of(&other.b01) && self.b10.is_subset_of(&other.b10) {
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

    /// Create a change for translating one region into a subset of another.
    pub fn region_to_region(from: &SomeRegion, to: &SomeRegion) -> Self {
        let f_xs = from.x_mask();
        let f_ones = from.state1().bitwise_and(from.state2()).to_mask();
        let f_zeros = from
            .state1()
            .bitwise_not()
            .bitwise_and(&from.state2().bitwise_not())
            .to_mask();

        let t_ones = to.state1().bitwise_and(to.state2()).to_mask();
        let t_zeros = to
            .state1()
            .bitwise_not()
            .bitwise_and(&to.state2().bitwise_not())
            .to_mask();

        Self {
            b01: f_zeros.bitwise_or(&f_xs).bitwise_and(&t_ones),
            b10: f_ones.bitwise_or(&f_xs).bitwise_and(&t_zeros),
        }
    }

    /// For a given change, subtract an other change.
    pub fn minus(&self, other: &Self) -> Self {
        Self {
            b01: self.b01.bitwise_xor(&self.b01.bitwise_and(&other.b01)),
            b10: self.b10.bitwise_xor(&self.b10.bitwise_and(&other.b10)),
        }
    }
} // end impl SomeChange

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::SomeDomain;

    #[test]
    fn region_to_region() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let dm0 = SomeDomain::new(1);

        let reg_0x1x = dm0.region_from_string("r0X1X")?;

        let reg_1100 = dm0.region_from_string("r1100")?;

        let cng1 = SomeChange::region_to_region(&reg_0x1x, &reg_1100);

        println!("change from {} to {} is {}", reg_0x1x, reg_1100, cng1);

        if cng1.b01 != cng1.b01.new_from_string("m0b1100")?
            || cng1.b10 != cng1.b10.new_from_string("m0b0011")?
        {
            return Err(format!("change {} to {} = {} ?", reg_0x1x, &reg_1100, cng1));
        }

        let reg_01x1 = dm0.region_from_string("r01X1")?;

        let reg_0x1x = dm0.region_from_string("r0X1X")?;

        let cng1 = SomeChange::region_to_region(&reg_01x1, &reg_0x1x);

        println!("change from {} to {} is {}", reg_01x1, reg_0x1x, cng1);

        if cng1.b01 != cng1.b01.new_from_string("m0b0010")?
            || cng1.b10 != cng1.b10.new_from_string("m0b0000")?
        {
            return Err(format!("change {} to {} = {} ?", reg_0x1x, &reg_1100, cng1));
        }

        Ok(())
    }
}
