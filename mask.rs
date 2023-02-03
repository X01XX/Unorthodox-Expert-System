//! The SomeMask struct, a mask of bits.
//!
//! The SomeState struct is very similar, the difference is in the intended use, and the xor operation.
//!
//! A Mask of differences between two states would be calculated like:
//!
//! let diff_mask = SomeMask::new(state1.bts.b_xor(&state2.bts);
//!
//! A difference mask applied to a state, to get a new state, would be calculated like:
//!
//! let state2 = SomeState::new(diff_mask.bts.b_xor(&state1.bts));

use crate::bits::BitsRef;
use crate::bits::SomeBits;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

//#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
/// SomeMask struct, just some bits.
pub struct SomeMask {
    pub bts: SomeBits,
}

/// Display trait for SomeMask
impl fmt::Display for SomeMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeMask {
    /// Return a new mask given a SomeBits instance.
    pub fn new(bts: SomeBits) -> Self {
        assert!(bts.num_ints() > 0);
        Self { bts }
    }

    /// Return a new SomeMask instance, all zeros, given the number of integers to use in the SomeBits instance.
    pub fn new_low(num_ints: usize) -> SomeMask {
        assert!(num_ints > 0);
        Self {
            bts: SomeBits::new_low(num_ints),
        }
    }

    /// Return the number of integers used to represent a SomeMask instance.
    pub fn num_ints(&self) -> usize {
        self.bts.num_ints()
    }

    /// Return a Mask from a string.
    /// Left-most, consecutive, zeros can be omitted.
    ///
    /// if let Ok(msk) = SomeMask::from_string(1, "m0101")) {
    ///    println!("Mask {}", &msk);
    /// } else {
    ///    panic!("Invalid Mask");
    /// }
    /// A prefix of "m0x" can be used to specify hexadecimal characters.
    pub fn new_from_string(num_ints: usize, str: &str) -> Result<Self, String> {
        assert!(num_ints > 0);

        let mut rest = String::new();

        for (inx, chr) in str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "m" || chr == "M" {
                    continue;
                }
                return Err("Initial character should be m".to_string());
            }

            rest.push_str(chr);
        }

        match SomeBits::new_from_string(num_ints, &rest) {
            Ok(bts) => Ok(SomeMask { bts }),
            Err(error) => Err(error),
        }
    } // end new_from_string

    /// Return true if the mask is all zeros.
    pub fn is_low(&self) -> bool {
        self.bts.is_low()
    }

    /// Return true if the mask is not all zeros.
    pub fn is_not_low(&self) -> bool {
        self.bts.is_not_low()
    }

    /// Return true if the mask is all ones.
    pub fn is_high(&self) -> bool {
        self.bts.is_high()
    }

    /// Return true if a given bit position is one.
    pub fn is_bit_set(&self, b: usize) -> bool {
        self.bts.is_bit_set(b)
    }

    /// Return true if a mask is a subset of a second mask.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.bts.is_subset_of(&other.bts)
    }

    /// Return true if a mask is a superset of a second mask.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        self.bts.is_superset_of(&other.bts)
    }

    /// Return the number of bits set to one.
    pub fn num_one_bits(&self) -> usize {
        self.bts.num_one_bits()
    }

    /// Return true if only one bit is set to one.
    pub fn just_one_bit(&self) -> bool {
        self.bts.just_one_bit()
    }

    /// Return a vector of one-bit masks.
    /// Could be called like MaskStore { avec: [a mask object].split() }
    pub fn split(&self) -> Vec<Self> {
        let bitsx = self.bts.split();

        bitsx
            .iter()
            .map(|bitx| SomeMask { bts: bitx.clone() })
            .collect()
    }

    /// Return the number of bits used to express a SomeMask instance.
    pub fn num_bits(&self) -> usize {
        self.bts.num_bits()
    }

    /// Return a formatted string.
    pub fn formatted_string(&self) -> String {
        self.bts.formatted_string('m')
    }

    /// Create a formatted string to display under an instance,
    /// to indicate specific bits positions.
    pub fn str2(&self) -> String {
        self.bts.str2()
    }

    /// Given a mask of more than one bit, return a mask that is a random selection of
    /// roughly half the bits.
    pub fn half_mask(&self) -> Self {
        SomeMask {
            bts: self.bts.half_bits(),
        }
    }

    /// Return the mask after shifting left one position, and adding one.
    pub fn push_1(&self) -> Self {
        Self {
            bts: self.bts.push_1(),
        }
    }

    /// Return mask after shifting left one position.
    pub fn push_0(&self) -> Self {
        Self {
            bts: self.bts.push_0(),
        }
    }

    /// Return a SomeMask instance, representing a bitwise And of a mask and another instance that supports the BitsRef Trait.
    pub fn bitwise_and(&self, other: &impl BitsRef) -> Self {
        SomeMask {
            bts: self.bts.b_and(other.bitsref()),
        }
    }

    /// Return a SomeMask instance, representing a bitwise Or of a mask and another instance that supports the BitsRef Trait.
    pub fn bitwise_or(&self, other: &impl BitsRef) -> Self {
        SomeMask {
            bts: self.bts.b_or(other.bitsref()),
        }
    }

    /// Return a SomeMask instance, representing a bitwise XOr of a mask and another instance that supports the BitsRef Trait.
    pub fn bitwise_xor(&self, other: &impl BitsRef) -> Self {
        SomeMask {
            bts: self.bts.b_xor(other.bitsref()),
        }
    }

    /// Return a mask of the bits values that are the same.
    pub fn bitwise_eqv(&self, other: &impl BitsRef) -> Self {
        Self {
            bts: self.bts.b_eqv(other.bitsref()),
        }
    }

    /// Return the bitwise Not of a SomeMask instane.
    pub fn bitwise_not(&self) -> Self {
        SomeMask {
            bts: self.bts.b_not(),
        }
    }

    /// Return a SomeMask instance from a SomeState instance.
    pub fn to_state(&self) -> SomeState {
        SomeState::new(self.bts.clone())
    }

    /// Return a copy, shifted left by 1 bit
    /// The Most Significant Bit value is lost.
    pub fn shift_left(&self) -> Self {
        SomeMask::new(self.bts.shift_left())
    }
} // end impl SomeMask

/// Implement the BitsRef trait for SomeMask.
impl BitsRef for SomeMask {
    fn bitsref(&self) -> &SomeBits {
        &self.bts
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq() -> Result<(), String> {
        let msk1 = SomeMask::new_from_string(1, "m0b1010").unwrap();
        let msk2 = SomeMask::new_from_string(1, "m0b1010").unwrap();
        if msk1 != msk2 {
            return Err(format!("msk1 {} ne msk2 {}?", msk1, msk2));
        }

        let msk3 = SomeMask::new_from_string(1, "m0b1001").unwrap();
        if msk1 == msk3 {
            return Err(format!("msk1 {} eq msk3 {}?", msk1, msk3));
        }

        Ok(())
    }

    #[test]
    fn half_mask() -> Result<(), String> {
        let test_msk = SomeMask::new_from_string(2, "m0x5aa5").unwrap().half_mask();

        if test_msk.num_one_bits() != 4 {
            return Err(format!(
                "SomeMask::test_half_mask num bits {} instead of 4?",
                test_msk.num_one_bits()
            ));
        }

        // Test an odd number of bits
        let test_msk = SomeMask::new_from_string(2, "m0x1011").unwrap().half_mask();
        println!("test_msk: {}", test_msk);

        if test_msk.num_one_bits() != 1 {
            return Err("SomeMask::test_half_mask not one bit?".to_string());
        }

        // Test an odd number of bits
        let test_msk = SomeMask::new_from_string(2, "m0x3031").unwrap().half_mask();
        println!("test_msk: {}", test_msk);

        if test_msk.num_one_bits() != 2 {
            return Err("SomeMask::test_half_mask not two bits?".to_string());
        }
        Ok(())
    }
}
