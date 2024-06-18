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
        write!(f, "{}", self.formatted_string())
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
    /// All characters must be specified.
    ///
    /// if let Ok(msk) = SomeMask::new_from_string("0b0101")) {
    ///    println!("Mask {}", msk);
    /// } else {
    ///    panic!("Invalid Mask");
    /// }
    /// A prefix of "0x" can be used to specify hexadecimal characters.
    ///
    /// A first character of "m" is supported for cut-and-paste from output on console.
    pub fn new_from_string(str: &str) -> Result<Self, String> {
        // Check for first character.
        if let Some(char0) = str.graphemes(true).nth(0) {
            // Check the first character.
            if char0 == "m" || char0 == "M" {
                // Create the result from the not-first characters.
                match SomeBits::new_from_string(&str.to_string()[1..]) {
                    Ok(bts) => Ok(Self { bts }),
                    Err(error) => Err(error),
                }
            } else {
                match SomeBits::new_from_string(str) {
                    Ok(bts) => Ok(Self { bts }),
                    Err(error) => Err(error),
                }
            }
        } else {
            Err(format!(
                "SomeMask::new_from_string: String {str}, no valid character?"
            ))
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

    /// Return true if a mask is a subset of a second mask.
    pub fn is_subset_ones_of(&self, other: &Self) -> bool {
        self.bitwise_and(other) == *self
    }

    /// Return true if a mask is a superset of a second mask.
    pub fn is_superset_ones_of(&self, other: &Self) -> bool {
        self.bitwise_and(other) == *other
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

    /// Return a formatted string.
    fn formatted_string(&self) -> String {
        format!("m{}", self.bts)
    }

    /// A string to display under a printed state to indicate changes.
    /// Zeros and underscores are printed as spaces.
    /// Ones are printed as "v".
    pub fn mark_ones(&self) -> String {
        let mut astr = String::with_capacity(self.strlen());

        for num_bit in (0..self.bts.num_bits as usize).rev() {
            if self.bts.is_bit_set(num_bit) {
                astr.push('v');
            } else {
                astr.push(' ');
            }
            // Add a space for the "_" separator.
            if num_bit > 0 && (num_bit % 4) == 0 {
                astr.push(' ');
            }
        }
        astr
    }

    /// Return a SomeMask instance, representing a bitwise And of a mask and another instance that supports the BitsRef Trait.
    pub fn bitwise_and(&self, other: &impl BitsRef) -> Self {
        Self {
            bts: self.bts.b_and(other.bitsref()),
        }
    }

    /// Return a SomeMask instance, representing a bitwise And of a mask and the invert of another instance that supports the BitsRef Trait.
    pub fn bitwise_and_not(&self, other: &impl BitsRef) -> Self {
        Self {
            bts: self.bts.b_and_not(other.bitsref()),
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
    fn strlen() -> Result<(), String> {
        let tmp_msk = SomeMask::new(SomeBits::new_from_string("0+0")?);
        let strrep = format!("{tmp_msk}");
        let len = strrep.len();
        let calc_len = tmp_msk.strlen();
        println!("str {tmp_msk} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_msk = SomeMask::new(SomeBits::new(16));
        let strrep = format!("{tmp_msk}");
        let len = strrep.len();
        let calc_len = tmp_msk.strlen();
        println!("str {tmp_msk} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_msk = SomeMask::new(SomeBits::new(6));
        let strrep = format!("{tmp_msk}");
        let len = strrep.len();
        let calc_len = tmp_msk.strlen();
        println!("str {tmp_msk} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_msk = SomeMask::new(SomeBits::new(5));
        let strrep = format!("{tmp_msk}");
        let len = strrep.len();
        let calc_len = tmp_msk.strlen();
        println!("str {tmp_msk} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_msk = SomeMask::new(SomeBits::new(4));
        let strrep = format!("{tmp_msk}");
        let len = strrep.len();
        let calc_len = tmp_msk.strlen();
        println!("str {tmp_msk} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let msk1 = SomeMask::new_from_string("m0b10+10")?;
        let msk2 = SomeMask::new_from_string("m0b10+10")?;
        println!("msk1: {msk1} msk2: {msk2}");
        assert!(msk1 == msk2);

        let msk3 = SomeMask::new_from_string("m0b10+01")?;
        println!("msk1: {msk1} msk3: {msk3}");
        assert!(msk1 != msk3);

        Ok(())
    }
}
