//! The SomeMask struct, a mask of bits.
//!
//! The SomeState struct is very similar, the difference is in the intended use, and the xor operation.
//!
//! A Mask of differences between two states would be calculated like:
//!
//! let diff_mask = state1.bitwise_xor(&state2).to_mask();
//!
//! A difference mask applied to a state, to get a new state, would be calculated like:
//!
//! let state2 =  state1.bitwise_xor(diff_mask);

use crate::bits::BitsRef;
use crate::bits::SomeBits;
use crate::state::SomeState;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;
extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
/// SomeMask struct, just some bits.
pub struct SomeMask {
    pub bts: SomeBits,
}

/// Display trait for SomeMask
impl fmt::Display for SomeMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "m{}", self.formatted_string())
    }
}

impl SomeMask {
    /// Return a new mask given a SomeBits instance.
    pub fn new(bts: SomeBits) -> Self {
        Self { bts }
    }

    /// Return a new SomeMask instance, all zeros, given the number of integers to use in the SomeBits instance.
    pub fn new_low(&self) -> Self {
        Self {
            bts: self.bts.new_low(),
        }
    }

    /// Return a new SomeMask instance, all ones, given the number of integers to use in the SomeBits instance.
    pub fn new_high(&self) -> Self {
        Self {
            bts: self.bts.new_high(),
        }
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
    pub fn new_from_string(&self, str: &str) -> Result<Self, String> {
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

        match self.bts.new_from_string(&rest) {
            Ok(bts) => Ok(Self { bts }),
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
            .map(|bitx| Self { bts: bitx.clone() })
            .collect()
    }

    /// Return the number of bits used to express a SomeMask instance.
    pub fn num_bits(&self) -> usize {
        self.bts.num_bits()
    }

    /// Return a formatted string.
    fn formatted_string(&self) -> String {
        self.bts.to_string()
    }

    /// Create a formatted string to display under an instance,
    /// to indicate specific bits positions.
    pub fn str2(&self) -> String {
        self.bts.str2()
    }

    /// Return the mask after shifting left one position, and adding one.
    pub fn push_1(&self) -> Self {
        Self {
            bts: self.bts.push_1(),
        }
    }

    /// Return a SomeMask instance, representing a bitwise And of a mask and another instance that supports the BitsRef Trait.
    pub fn bitwise_and(&self, other: &impl BitsRef) -> Self {
        Self {
            bts: self.bts.b_and(other.bitsref()),
        }
    }

    /// Return a SomeMask instance, representing a bitwise Or of a mask and another instance that supports the BitsRef Trait.
    pub fn bitwise_or(&self, other: &impl BitsRef) -> Self {
        Self {
            bts: self.bts.b_or(other.bitsref()),
        }
    }

    /// Return a SomeMask instance, representing a bitwise XOr of a mask and another instance that supports the BitsRef Trait.
    pub fn bitwise_xor(&self, other: &impl BitsRef) -> Self {
        Self {
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
        Self {
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
        Self::new(self.bts.shift_left())
    }
} // end impl SomeMask

/// Implement the BitsRef trait for SomeMask.
impl BitsRef for SomeMask {
    fn bitsref(&self) -> &SomeBits {
        &self.bts
    }
}

/// Implement the trait StrLen for SomeMask.
impl StrLen for SomeMask {
    fn strlen(&self) -> usize {
        self.bts.strlen() + 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strlen() -> Result<(), String> {
        let tmp_msk = SomeMask::new(SomeBits::new(vec![0]));
        let strrep = format!("{tmp_msk}");
        let len = strrep.len();
        let calc_len = tmp_msk.strlen();
        println!("str {tmp_msk} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_msk = SomeMask::new(SomeBits::new(vec![0, 0]));
        let strrep = format!("{tmp_msk}");
        let len = strrep.len();
        let calc_len = tmp_msk.strlen();
        println!("str {tmp_msk} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let tmp_msk = SomeMask::new(SomeBits::new(vec![0]));

        let msk1 = tmp_msk.new_from_string("m0b1010")?;
        let msk2 = tmp_msk.new_from_string("m0b1010")?;
        println!("msk1: {msk1} msk2: {msk2}");
        assert!(msk1 == msk2);

        let msk3 = tmp_msk.new_from_string("m0b1001")?;
        println!("msk1: {msk1} msk3: {msk3}");
        assert!(msk1 != msk3);

        Ok(())
    }
}
