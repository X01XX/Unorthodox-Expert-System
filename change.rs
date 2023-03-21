//! The SomeChange struct, which stores masks for 0->1 and 1->0 bit changes.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::state::SomeState;

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
    pub fn new_low(num_ints: usize) -> Self {
        assert!(num_ints > 0);
        Self {
            b01: SomeMask::new_low(num_ints),
            b10: SomeMask::new_low(num_ints),
        }
    }

    /// Return the number of integers used to represent a SomeBits instance, for creating a
    /// new instance.
    pub fn num_ints(&self) -> usize {
        self.b01.num_ints()
    }

    /// Apply a change to a state.
    pub fn apply_to_state(&self, astate: &SomeState) -> SomeState {
        let b01 = self.b01.bitwise_and(&astate.bitwise_not());
        let b10 = self.b10.bitwise_and(astate);
        let to_change = b01.bitwise_or(&b10);

        astate.bitwise_xor(&to_change)
    }

    /// Return the logical bitwise and of two changes
    pub fn bitwise_and(&self, other: &SomeChange) -> SomeChange {
        Self {
            b01: self.b01.bitwise_and(&other.b01),
            b10: self.b10.bitwise_and(&other.b10),
        }
    }

    /// Return the logical bitwise and of a change and a rule.
    pub fn bitwise_and_rule(&self, other: &SomeRule) -> SomeChange {
        Self {
            b01: self.b01.bitwise_and(&other.b01),
            b10: self.b10.bitwise_and(&other.b10),
        }
    }

    /// Return the logical bitwize or of two changes
    pub fn bitwise_or(&self, other: &SomeChange) -> SomeChange {
        Self {
            b01: self.b01.bitwise_or(&other.b01),
            b10: self.b10.bitwise_or(&other.b10),
        }
    }

    /// Return the logical bitwize or of a change and a rule.
    pub fn bitwise_or_rule(&self, other: &SomeRule) -> SomeChange {
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
    pub fn bitwise_not(&self) -> SomeChange {
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

    /// Create a change for translating one region to another.
    pub fn region_to_region(from: &SomeRegion, to: &SomeRegion) -> Self {
        let f_ones = from.state1.bitwise_or(&from.state2).to_mask();
        let f_zeros = from
            .state1
            .bitwise_not()
            .bitwise_or(&from.state2.bitwise_not())
            .to_mask();

        let t_ones = to.state1.bitwise_or(&to.state2);
        let t_zeros = &to
            .state1
            .bitwise_not()
            .bitwise_or(&to.state2.bitwise_not())
            .to_mask();

        let to_not_x = to.x_mask().bitwise_not();

        SomeChange {
            b01: f_zeros.bitwise_and(&t_ones.bitwise_and(&to_not_x)),
            b10: f_ones.bitwise_and(&t_zeros.bitwise_and(&to_not_x)),
        }
    }

    /// For a given change, subtract an other change.
    pub fn minus(&self, other: &SomeChange) -> Self {
        SomeChange {
            b01: self.b01.bitwise_xor(&self.b01.bitwise_and(&other.b01)),
            b10: self.b10.bitwise_xor(&self.b10.bitwise_and(&other.b10)),
        }
    }
} // end impl SomeChange

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SomeDomain;

    #[test]
    fn apply_to_state() -> Result<(), String> {
        // Test changing two bits.
        let reg0 = SomeRegion::new_from_string(1, "r0101")?;
        let reg1 = SomeRegion::new_from_string(1, "r1001")?;

        let cng = SomeChange::region_to_region(&reg0, &reg1);
        let sta = SomeState::new_from_string(1, "s0b0101")?;

        let sta2 = cng.apply_to_state(&sta);

        let sta3 = SomeState::new_from_string(1, "s0b1001")?;
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

    #[test]
    fn region_to_region() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let dm0 = SomeDomain::new(1);

        let reg_0x1x = dm0.region_from_string("r0X1X")?;

        let reg_1100 = dm0.region_from_string("r1100")?;

        let cng1 = SomeChange::region_to_region(&reg_0x1x, &reg_1100);

        println!("change from {} to {} is {}", reg_0x1x, reg_1100, cng1);

        let reg_result = SomeRegion::new(
            cng1.apply_to_state(&reg_0x1x.state1),
            cng1.apply_to_state(&reg_0x1x.state2),
        );

        if reg_result != reg_1100 {
            return Err(format!("result {} ne {}!", reg_result, reg_1100));
        }

        let reg_0x1x = dm0.region_from_string("r0X1X")?;

        let reg_01x1 = dm0.region_from_string("r01X1")?;

        let cng1 = SomeChange::region_to_region(&reg_01x1, &reg_0x1x);

        println!("change from {} to {} is {}", reg_01x1, reg_0x1x, cng1);

        let reg_result = SomeRegion::new(
            cng1.apply_to_state(&reg_01x1.state1),
            cng1.apply_to_state(&reg_01x1.state2),
        );

        if !reg_0x1x.is_superset_of(&reg_result) {
            return Err(format!("result {} ne {}!", reg_result, reg_1100));
        }

        Ok(())
    }
}
