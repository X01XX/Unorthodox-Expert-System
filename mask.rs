// Mask struct for an Unorthodox Expert System

use crate::bits::{bits_new_low, SomeBits};
use serde::{Deserialize, Serialize};
use std::fmt;

impl PartialEq for SomeMask {
    fn eq(&self, other: &Self) -> bool {
        self.bts == other.bts
    }
}
impl Eq for SomeMask {}

#[derive(Serialize, Deserialize, Debug)]
pub struct SomeMask {
    pub bts: SomeBits,
}

impl fmt::Display for SomeMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeMask {
    // Return a new Mask struct instance
    pub fn new(val: SomeBits) -> Self {
        Self { bts: val }
    }

    // Return true if two masks are equal
    pub fn is_eq(&self, other: &Self) -> bool {
        self.bts == other.bts
    }

    // Return the xor or two maks
    pub fn m_xor(&self, other: &Self) -> Self {
        Self::new(self.bts.b_xor(&other.bts))
    }

    // Return the or or two maks
    pub fn m_or(&self, other: &Self) -> Self {
        Self::new(self.bts.b_or(&other.bts))
    }

    // Return the and or two maks
    pub fn m_and(&self, other: &Self) -> Self {
        Self::new(self.bts.b_and(&other.bts))
    }

    // Return result of a not operation
    pub fn m_not(&self) -> Self {
        Self::new(self.bts.b_not())
    }

    // Return true if the mask is low, that is all zeros
    pub fn is_low(&self) -> bool {
        self.bts.is_low()
    }

    pub fn is_not_low(&self) -> bool {
        self.bts.is_not_low()
    }

    // Return true if the mask is high, that is all ones
    pub fn is_high(&self) -> bool {
        self.bts.is_high()
    }

    // Return true is a given bit is one at a given position
    pub fn is_bit_set(&self, b: usize) -> bool {
        self.bts.is_bit_set(b)
    }

    // Return true is a mask is a subset of a second mask
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.bts.is_subset_of(&other.bts)
    }

    // Return true is a mask is a superset of a second mask
    pub fn is_superset_of(&self, other: &Self) -> bool {
        self.bts.is_superset_of(&other.bts)
    }

    pub fn num_one_bits(&self) -> usize {
        self.bts.num_one_bits()
    }

    pub fn just_one_bit(&self) -> bool {
        self.bts.just_one_bit()
    }

    // Return a vector of one-bit masks
    // Should be called like MaskStore { avec: <a mask object>.split() }
    pub fn split(&self) -> Vec<Self> {
        let bitsx = self.bts.split();

        let mut rc_vec: Vec<Self> = Vec::with_capacity(bitsx.len());

        for bitx in bitsx.iter() {
            rc_vec.push(SomeMask::new(bitx.clone()));
        } // next bitx

        rc_vec
    }

    // Return the number of ints used to express a SomeMask instance
    pub fn num_ints(&self) -> usize {
        self.bts.num_ints()
    }

    pub fn formatted_string_length(&self) -> usize {
        self.bts.formatted_string_length()
    }

    pub fn formatted_string(&self) -> String {
        self.bts.formatted_string('m')
    }
} // end SomeMask

// Return a Mask from a string, like "m0101".
// Left-most, consecutive, zeros can be omitted.
pub fn _mask_from_string(num_ints: usize, str: &str) -> Result<SomeMask, String> {
    let mut bts = bits_new_low(num_ints);

    let mut inx = -1;

    for ch in str.chars() {
        inx += 1;

        if inx == 0 {
            if ch == 'm' {
                continue;
            } else {
                return Err(String::from("initial character should be m"));
            }
        }

        if bts.high_bit_set() {
            return Err(String::from("too long"));
        }

        if ch == '0' {
            bts = bts.shift_left();
        } else if ch == '1' {
            bts = bts.push_1();
        } else if ch == '_' {
            continue;
        } else {
            return Err(String::from("invalid character"));
        }
    } // end for ch

    Ok(SomeMask::new(bts))
} // end mask_from_string

impl Clone for SomeMask {
    fn clone(&self) -> Self {
        Self {
            bts: self.bts.clone(),
        }
    }
}
