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

use crate::bits::SomeBits;
use crate::state::SomeState;
use crate::bits::BitsRef;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
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
    /// Return a new SomeMask instance.
    pub fn new(val: SomeBits) -> Self {
        Self { bts: val }
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
    pub fn new_from_string(num_ints: usize, str: &str) -> Result<SomeMask, String> {
        if &str[0..1] != "m" {
            return Err("Initial character should be m".to_string());
        }

        if str.len() > 2 && (&str[1..3] == "0b" || &str[1..3] == "0x") {
            match SomeBits::new_from_string(num_ints, &str[1..]) {
                Ok(bts) => Ok(SomeMask::new(bts)),
                Err(error) => Err(error),
            }
        } else {
            match SomeBits::new_from_string(num_ints, &("0b".to_owned() + &str[1..])) {
                Ok(bts) => Ok(SomeMask::new(bts)),
                Err(error) => Err(error),
            }
        }
    } // end new_from_string

    /// Return a new mask set to all zeros.
    pub fn new_low(num_ints: usize) -> Self {
        Self {
            bts: SomeBits::new_low(num_ints),
        }
    }

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
    /// Could be called like MaskStore { avec: <a mask object>.split() }
    pub fn split(&self) -> Vec<Self> {
        let bitsx = self.bts.split();

        bitsx.iter().map(|bitx| SomeMask::new(bitx.clone())).collect()
    }

    /// Return the number of ints used to express a SomeMask instance.
    pub fn num_ints(&self) -> usize {
        self.bts.num_ints()
    }

    /// Return a formatted string.
    pub fn formatted_string(&self) -> String {
        self.bts.formatted_string('m')
    }

    /// Create a formatted string to display under an instance,
    /// to indicate specific bits positions.
    pub fn str2(&self) -> String {
        self.bts.str2(' ')
    }

    /// Given a mask of more than one bit, return a mask that is a random selection of
    /// roughly half the bits.
    pub fn half_mask(&self) -> Self {
        SomeMask::new(self.bts.half_bits())
    }

    /// Return the mask after shifting left one position, and adding one.
    pub fn push_1(&self) -> Self {
        Self::new(self.bts.push_1())
    }

    /// Return mask after shifting left one position.
    pub fn push_0(&self) -> Self {
        Self::new(self.bts.push_0())
    }

    /// Return a state from a mask.
    pub fn to_state(&self) -> SomeState {
        SomeState::new(self.bts.clone())
    }

    // Return the b_xor of a mask and an structure that implements BitsRef.
    pub fn bits_xor<U: BitsRef>(&self, two: &U) -> SomeMask {
        Self {
            bts: self.bts.b_xor(two.bitsref())
        }
    }

    // Return the b_or of a mask and an structure that implements BitsRef.
    pub fn bits_or<U: BitsRef>(&self, two: &U) -> SomeMask {
        Self {
            bts: self.bts.b_or(two.bitsref())
        }
    }

    // Return the b_and of  a mask and an structure that implements BitsRef.
    pub fn bits_and<U: BitsRef>(&self, two: &U) -> SomeMask {
        Self {
            bts: self.bts.b_and(two.bitsref())
        }
    }

    // Return a mask with the bits reversed.
    pub fn bits_not(&self) -> SomeMask {
        Self {
            bts: self.bts.b_not()
        }
    }

} // end impl SomeMask

impl BitsRef for SomeMask {
    fn bitsref(&self) -> &SomeBits {
        &self.bts
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            return Err(
                "SomeMask::test_half_mask not one bit?".to_string()
            );
        }

        // Test an odd number of bits
        let test_msk = SomeMask::new_from_string(2, "m0x3031").unwrap().half_mask();
        println!("test_msk: {}", test_msk);

        if test_msk.num_one_bits() != 2 {
            return Err(
                "SomeMask::test_half_mask not two bits?".to_string()
            );
        }
        Ok(())
    }
}
