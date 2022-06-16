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
use crate::randompick::random_x_of_n;

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
        for chr in str.chars() {
            if chr != 'm' && chr != 'M' {
                return Err(String::from("initial character should be m"));
            }
            break;
        }

        match SomeBits::new_from_string(num_ints, &str[1..]) {
            Ok(bts) => {
                return Ok(SomeMask::new(bts));
            }
            Err(error) => {
                return Err(error);
            }
        }
    } // end new_from_string

    /// Return a new mask set to all zeros.
    pub fn new_low(num_ints: usize) -> Self {
        Self {
            bts: SomeBits::new(num_ints),
        }
    }

    /// Return the bitwise OR of two masks.
    pub fn m_or(&self, other: &Self) -> Self {
        Self::new(self.bts.b_or(&other.bts))
    }

    /// Return the bitwize AND of two masks.
    pub fn m_and(&self, other: &Self) -> Self {
        Self::new(self.bts.b_and(&other.bts))
    }

    /// Return the bitwize XOR of two masks.
    pub fn m_xor(&self, other: &Self) -> Self {
        Self::new(self.bts.b_xor(&other.bts))
    }

    /// Return the bitwize NOT of a mask.
    pub fn m_not(&self) -> Self {
        Self::new(self.bts.b_not())
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

        let mut rc_vec: Vec<Self> = Vec::with_capacity(bitsx.len());

        for bitx in bitsx.iter() {
            rc_vec.push(SomeMask::new(bitx.clone()));
        } // next bitx

        rc_vec
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
        let one_bits: Vec<SomeBits> = self.bts.split();

        let indicies: Vec<usize> = random_x_of_n(one_bits.len() / 2, one_bits.len());

        let mut or_bts = SomeBits::new(self.num_ints());

        for inx in indicies.iter() {
            or_bts = or_bts.b_or(&one_bits[*inx]);
        }
        SomeMask::new(or_bts)
    }

    /// Return the mask after shifting left one position, and adding one.
    pub fn push_1(&self) -> Self {
        Self::new(self.bts.push_1())
    }

    /// Return mask after shifting left one position.
    pub fn push_0(&self) -> Self {
        Self::new(self.bts.push_0())
    }
} // end impl SomeMask

#[cfg(test)]
mod tests {
    use super::*;

    // Test SomeMask::half_mask, num_one_bits
    #[test]
    fn test_half_mask() -> Result<(), String> {
        let test_msk = SomeMask::new_from_string(2, "m0x5aa5").unwrap().half_mask();

        if test_msk.num_one_bits() != 4 {
            return Err(format!(
                "SomeMask::test_half_mask num bits {} instead of 4?",
                test_msk.num_one_bits()
            ));
        }
        Ok(())
    }

    // Test SomeMask::is_bit_set
    // This uses SomeBits::is_bit_set, so only a basic test is done.
    #[test]
    fn test_is_bit_set() -> Result<(), String> {
        let test_msk = SomeMask::new_from_string(2, "m0x5aa5").unwrap();

        if test_msk.is_bit_set(0) == false {
            return Err(format!("SomeMask::is_bit_set 0 failed"));
        }

        if test_msk.is_bit_set(1) {
            return Err(format!("SomeMask::is_bit_set 1 failed"));
        }
        Ok(())
    }

    // Test SomeMask::is_high
    #[test]
    fn test_is_high() -> Result<(), String> {
        let test_msk = SomeMask::new_from_string(2, "m0xa5a5").unwrap();
        if test_msk.is_high() {
            return Err(format!("SomeMask::test_is_high 1 True?"));
        }

        let test_msk = SomeMask::new_from_string(2, "m0xffff").unwrap();
        if test_msk.is_high() == false {
            return Err(format!("SomeMask::test_is_high 2 False?"));
        }
        Ok(())
    }

    // Test SomeMask::is_low
    #[test]
    fn test_is_low() -> Result<(), String> {
        let test_msk = SomeMask::new_from_string(2, "m0x5aa5").unwrap();

        if test_msk.is_low() {
            return Err(format!("SomeMask::test_is_low 1 True?"));
        }

        let test_msk = SomeMask::new_from_string(2, "m0x0").unwrap();

        if test_msk.is_low() == false {
            return Err(format!("SomeMask::test_is_low 2 False?"));
        }
        Ok(())
    }

    // Test SomeMask::is_not_low
    #[test]
    fn test_is_not_low() -> Result<(), String> {
        let test_msk = SomeMask::new_from_string(2, "m0x0").unwrap();

        if test_msk.is_not_low() {
            return Err(format!("SomeMask::test_is_not_low 1 True?"));
        }

        let test_msk = SomeMask::new_from_string(2, "m0x010").unwrap();

        if test_msk.is_not_low() == false {
            return Err(format!("SomeMask::test_is_not_low 2 False?"));
        }
        Ok(())
    }

    // Test SomeMask::is_subset_of
    #[test]
    fn test_is_subset_of() -> Result<(), String> {
        let test_msk1 = SomeMask::new_from_string(2, "m0x3210").unwrap();
        let test_msk2 = SomeMask::new_from_string(2, "m0x7632").unwrap();

        if test_msk1.is_subset_of(&test_msk2) == false {
            return Err(format!("SomeMask::test_is_subset_of 1 False?"));
        }

        if test_msk2.is_subset_of(&test_msk1) {
            return Err(format!("SomeMask::test_is_subset_of 2 True?"));
        }
        Ok(())
    }

    // Test SomeMask::is_superset_of
    #[test]
    fn test_is_superset_of() -> Result<(), String> {
        let test_msk1 = SomeMask::new_from_string(2, "m0x3210").unwrap();
        let test_msk2 = SomeMask::new_from_string(2, "m0x7632").unwrap();

        if test_msk2.is_superset_of(&test_msk1) == false {
            return Err(format!("SomeMask::test_is_superset_of 1 False?"));
        }

        if test_msk1.is_superset_of(&test_msk2) {
            return Err(format!("SomeMask::test_is_superset_of 2 True?"));
        }
        Ok(())
    }

    // Test SomeMask::just_one_bit
    #[test]
    fn test_just_one_bit() -> Result<(), String> {
        let test_msk1 = SomeMask::new_from_string(2, "m0x3210").unwrap();
        let test_msk2 = SomeMask::new_from_string(2, "m0x40").unwrap();

        if test_msk2.just_one_bit() == false {
            return Err(format!("SomeMask::test_just_one_bit 1 False?"));
        }

        if test_msk1.just_one_bit() {
            return Err(format!("SomeMask::test_just_one_bit 2 True?"));
        }
        Ok(())
    }

    // Test SomeMask::m_and
    // This uses SomeBits::b_and, so only a basic test is done.
    #[test]
    fn test_m_and() -> Result<(), String> {
        let test_and = SomeMask::new_from_string(2, "m0x6666")
            .unwrap()
            .m_and(&SomeMask::new_from_string(2, "m0xc37d").unwrap());
        if test_and != SomeMask::new_from_string(2, "m0x4264").unwrap() {
            return Err(format!("SomeMask::m_and 1 failed"));
        }
        Ok(())
    }

    // Test SomeMask::m_not
    // This uses SomeBits::b_not, so only a basic test is done.
    #[test]
    fn test_m_not() -> Result<(), String> {
        let test_not = SomeMask::new_from_string(2, "m0x5a5a").unwrap().m_not();
        if test_not != SomeMask::new_from_string(2, "m0xa5a5").unwrap() {
            return Err(format!("SomeMask::m_not 1 failed"));
        }
        Ok(())
    }

    // Test SomeMask::m_or
    // This uses SomeBits::b_or, so only a basic test is done.
    #[test]
    fn test_m_or() -> Result<(), String> {
        let test_or = SomeMask::new_from_string(2, "m0x2111")
            .unwrap()
            .m_or(&SomeMask::new_from_string(2, "m0x428a").unwrap());
        if test_or != SomeMask::new_from_string(2, "m0x639b").unwrap() {
            return Err(format!("SomeMask::m_or 1 failed"));
        }
        Ok(())
    }

    // Test SomeMask::m_xor
    // This uses SomeBits::b_xor, so only a basic test is done.
    #[test]
    fn test_m_xor() -> Result<(), String> {
        let test_xor = SomeMask::new_from_string(2, "m0x6666")
            .unwrap()
            .m_xor(&SomeMask::new_from_string(2, "m0xc37d").unwrap());
        if test_xor != SomeMask::new_from_string(2, "m0xa51b").unwrap() {
            return Err(format!("SomeMask::m_xor 1 failed"));
        }
        Ok(())
    }

    #[test]
    fn test_push() -> Result<(), String> {
        let test_msk = SomeMask::new_from_string(2, "m0x3333").unwrap();
        let mut test_msk2 = test_msk.push_1();

        if test_msk2 != SomeMask::new_from_string(2, "m0x6667").unwrap() {
            return Err(format!("SomeMask::push_1 m0x6667?"));
        }

        test_msk2 = test_msk2.push_0();
        println!("msk2 {}", test_msk2);
        if test_msk2 != SomeMask::new_from_string(2, "m0xccce").unwrap() {
            return Err(format!("SomeMask::push_0 m0xccce?"));
        }

        Ok(())
    }

    #[test]
    fn test_split() -> Result<(), String> {
        let test_msk = SomeMask::new_from_string(2, "m0x050a").unwrap();
        let one_bits: Vec<SomeMask> = test_msk.split();

        assert!(one_bits.len() == 4);
        assert!(one_bits.contains(&SomeMask::new_from_string(2, "m0x0400").unwrap()));
        assert!(one_bits.contains(&SomeMask::new_from_string(2, "m0x0100").unwrap()));
        assert!(one_bits.contains(&SomeMask::new_from_string(2, "m0x0008").unwrap()));
        assert!(one_bits.contains(&SomeMask::new_from_string(2, "m0x0002").unwrap()));
        Ok(())
    }

    // Test SomeMask.clone
    #[test]
    fn test_clone() -> Result<(), String> {
        let tmp = SomeMask::new_from_string(2, "m0x505").unwrap();
        if tmp != tmp.clone() {
            return Err(format!("SomeMask::clone 1 failed"));
        }
        Ok(())
    }
}
