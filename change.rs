//! The SomeChange struct, which stores masks for 0->1 and 1->0 bit changes.

use crate::mask::SomeMask;

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

    /// Return a change that will reverse a given change.
    pub fn reverse(&self) -> Self {
        Self {
            b01: self.b10().clone(),
            b10: self.b01().clone(),
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

    /// Return a mask of bit positions that can change.
    pub fn change_mask(&self) -> SomeMask {
        self.b01.bitwise_and(&self.b10)
    }
} // end impl SomeChange

pub trait AccessChanges {
    fn b01(&self) -> &SomeMask;
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
    use crate::bits::SomeBits;

    #[test]
    fn intersection() -> Result<(), String> {
        // Create a mask that uses one integer for bits.
        let ur_bits = SomeBits::new(8);
        let ur_mask = SomeMask::new(ur_bits.clone());

        let cng1 = SomeChange {
            b01: ur_mask.new_from_string("m0b1010")?,
            b10: ur_mask.new_from_string("m0b1010")?,
        };
        let cng2 = SomeChange {
            b01: ur_mask.new_from_string("m0b1001")?,
            b10: ur_mask.new_from_string("m0b0110")?,
        };
        let cng3 = SomeChange {
            b01: ur_mask.new_from_string("m0b1000")?,
            b10: ur_mask.new_from_string("m0b0010")?,
        };
        let cng4 = cng1.intersection(&cng2);
        println!("cng4 {cng4}");

        assert!(cng3 == cng4);

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        // Create a mask that uses one integer for bits.
        let ur_bits = SomeBits::new(8);
        let ur_mask = SomeMask::new(ur_bits.clone());

        let cng1 = SomeChange {
            b01: ur_mask.new_from_string("m0b1010")?,
            b10: ur_mask.new_from_string("m0b1010")?,
        };
        let cng2 = SomeChange {
            b01: ur_mask.new_from_string("m0b1001")?,
            b10: ur_mask.new_from_string("m0b0110")?,
        };
        let cng3 = SomeChange {
            b01: ur_mask.new_from_string("m0b1011")?,
            b10: ur_mask.new_from_string("m0b1110")?,
        };
        let cng4 = cng1.union(&cng2);
        println!("cng4 {cng4}");

        assert!(cng3 == cng4);

        Ok(())
    }

    #[test]
    fn changes_wanted_unwanted_reverse() -> Result<(), String> {
        // Create a mask that uses one integer for bits.
        let ur_bits = SomeBits::new(8);
        let ur_mask = SomeMask::new(ur_bits.clone());

        let wanted_changes = SomeChange {
            b01: ur_mask.new_from_string("m0b1010")?,
            b10: ur_mask.new_from_string("m0b0100")?,
        };
        println!("wanted_changes    {wanted_changes}");

        let available_changes = SomeChange {
            b01: ur_mask.new_from_string("m0b1001")?,
            b10: ur_mask.new_from_string("m0b1100")?,
        };
        println!("available_changes {available_changes}");

        let possible_changes = wanted_changes.intersection(&available_changes);
        println!("possible_changes  {possible_changes}");
        assert!(
            possible_changes
                == SomeChange {
                    b01: ur_mask.new_from_string("m0b1000")?,
                    b10: ur_mask.new_from_string("m0b0100")?
                }
        );

        let unwanted_changes = possible_changes.difference(&available_changes);
        println!("unwanted_changes  {unwanted_changes}");
        assert!(
            unwanted_changes
                == SomeChange {
                    b01: ur_mask.new_from_string("m0b0001")?,
                    b10: ur_mask.new_from_string("m0b1000")?
                }
        );

        let reverse_changes = unwanted_changes.reverse();
        println!("reverse_changes   {reverse_changes}");
        assert!(
            reverse_changes
                == SomeChange {
                    b01: ur_mask.new_from_string("m0b1000")?,
                    b10: ur_mask.new_from_string("m0b0001")?
                }
        );

        Ok(())
    }
}
