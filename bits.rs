//! The SomeBits struct, storing a bit pattern in one or more unsigned integers.
//!
//! Some conventions:
//!
//!   Display all bits in all integers.
//!
//!   Counting bits starts at the right-most bit of the right-most int,
//!   and proceeds to the left, as in standard integer bit-position reckoning.
//!
//! The integer type/size could be increased, search for and change "u8" and "08b" references in this file,
//! test.rs, and change the constants, below, as needed.
//!

/// The number of bits in an integer used by SomeBits.
pub const NUM_BITS_PER_INT: usize = 8;

/// Masks, powers of 2, to isolate any bit-position of a single integer.
/// Isolate bit 0 with: integer & ALL_BIT_MASKS\[0\]
/// Isolate bit 5 with: integer & ALL_BIT_MASKS\[5\];
const ALL_BIT_MASKS: [u8; NUM_BITS_PER_INT] = [1, 2, 4, 8, 16, 32, 64, 128];

/// The highest bit position in an integer.
const INT_HIGH_BIT: u8 = 1 << (NUM_BITS_PER_INT - 1);

use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;

/// Display trait for SomeBits
impl fmt::Display for SomeBits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string('b'))
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq)]

/// SomeBits struct, just an unsigned integer vector.
/// This structure sets the type of integer.
/// Each Domain sets the number of integers.
pub struct SomeBits {
    ints: Vec<u8>,
}

impl SomeBits {
    /// Create a SomeBits instance with integer(s) set to zero
    pub fn new(num_ints: usize) -> SomeBits {
        assert!(num_ints > 0);
        SomeBits {
            ints: vec![0 as u8; num_ints],
        }
    }

    /// Return a bits instance from a string.
    /// Left-most, consecutive, zeros can be omitted.
    /// Underscore character is ignored.
    /// 0X can be used as a prefix to indicate hexadecimal input.
    ///
    /// if let Ok(bts) = SomeBits::from_string(1, "0101")) {
    ///    println!("bts {}", &bts);
    /// } else {
    ///    panic!("Invalid bits string");
    /// }
    /// A prefix of "0x" can be used to specify hexadecimal characters.
    pub fn new_from_string(num_ints: usize, str: &str) -> Result<Self, String> {
        let mut bts = SomeBits::new(num_ints);

        let mut base = 2;

        let mut zf = false;
        let mut xf = false;

        let mut inx = 0;
        for chr in str.chars() {
            if inx == 0 && chr == '0' {
                zf = true;
            }
            if inx == 1 {
                if chr == 'x' || chr == 'X' {
                    xf = true;
                }
            }
            inx += 1;
            if inx > 1 {
                break;
            }
        }

        let mut str2 = str;
        if zf && xf {
            base = 16;
            str2 = &str2[2..];
        }

        let lsb = num_ints - 1;
        let shift_num = NUM_BITS_PER_INT - 4;

        for chr in str2.chars() {

            if chr == '_' {
                continue;
            }

            if bts.high_bit_set() {
                return Err(format!("Did not understand the string {}, too long?", str));
            }

            if base == 2 {

                if bts.high_bit_set() {
                    return Err(format!("Did not understand the string {}, too long?", str));
                }

                if chr == '0' {
                    bts = bts.shift_left();
                } else if chr == '1' {
                    bts = bts.push_1();
                } else if chr == '_' {
                    continue;
                } else {
                    return Err(format!("Did not understand the string {}, invalid character?", str));
                }
            } else {

                let numx;

                if bts.high_bit_set() {
                    return Err(String::from("too long"));
                }

                if chr >= '0' && chr <= '9' {
                    numx = chr as i32 - 48;
                } else if chr >= 'a' && chr <= 'f' {
                    numx = chr as i32  - 87;
                } else if chr >= 'A' && chr <= 'F' {
                    numx = chr as i32 - 55;
                } else {
                    return Err(format!("Did not understand the string {}, invalid character?", str));
                }

                if bts.ints[0] >> shift_num > 0 {
                    return Err(format!("Did not understand the string {}, too long?", str));
                }
                bts = bts.shift_left4();

                bts.ints[lsb] += numx as u8;
            }
        } // next inx

        Ok(bts)
    } // end new_from_string

    /// Return the number of integers in a SomeBits struct.
    pub fn len(&self) -> usize {
        self.ints.len()
    }

    /// Return a vector of bits where each has only
    /// one 1 bit isolated from the given Bits struct.
    pub fn split(&self) -> Vec<Self> {
        let num_bits = self.num_one_bits();

        let mut rc_vec: Vec<Self> = Vec::with_capacity(num_bits);

        let num_ints = self.num_ints();

        // For each integer in the bits vector
        for int_inx in 0..num_ints {
            if self.ints[int_inx] == 0 {
                continue;
            }

            let mut tmpint = self.ints[int_inx];

            // Make new SomeBits instance for each bit in the integer
            while tmpint > 0 {
                let tmp2 = tmpint - 1;      // zero the bit, rightmost zeros turn to 1

                let abit = tmpint & !tmp2;  // isolate the bit

                let mut btsx = SomeBits::new(num_ints); // new Bits object, all zeros

                btsx.ints[int_inx] = abit;  // update one integer

                rc_vec.push(btsx);  // Save one-bit result

                tmpint = tmpint & tmp2; // remove bit
            }
        } // end for int_inx
        rc_vec
    }

    /// Return a Bits struct with specified bit(s) changed.
    pub fn toggle_bits(&self, bit_nums: Vec<usize>) -> Self {
        let mut ary2 = self.ints.clone();

        let num_ints = self.num_ints();
        let num_bits = num_ints * NUM_BITS_PER_INT as usize;
        let lsi = num_ints - 1; // least significant integer

        for bit_num in bit_nums {
            if bit_num >= num_bits {
                panic!("bit num {} is too large", &bit_num);
            }

            let bit_pos = bit_num % NUM_BITS_PER_INT;   // calc bit index

            let int_num = lsi - (bit_num / NUM_BITS_PER_INT);   // calc integer index

            ary2[int_num] = ary2[int_num] ^ ALL_BIT_MASKS[bit_pos]; // toggle the bit
        }
        Self { ints: ary2 }
    }

    /// Return true if a bit is one at a given position.
    pub fn is_bit_set(&self, bit_num: usize) -> bool {
        let num_ints = self.num_ints();
        let num_bits = num_ints * NUM_BITS_PER_INT;
        let lsi = num_ints - 1;

        if bit_num >= num_bits {
            panic!("bit num too large");
        }

        let bit_pos = bit_num % NUM_BITS_PER_INT;   // calc bit index

        let int_num = lsi - (bit_num / NUM_BITS_PER_INT);   // calc integer index

        self.ints[int_num] & ALL_BIT_MASKS[bit_pos] > 0 // test bit
    }

    /// Bitwise NOT of a Bits stuct.
    pub fn b_not(&self) -> Self {
        let mut ary2 = Vec::<u8>::with_capacity(self.ints.len());

        for intx in self.ints.iter() {
            ary2.push(!intx);
        }

        Self { ints: ary2 }
    }

    /// Bitwise AND of two Bits structs.
    pub fn b_and(&self, other: &Self) -> Self {
        assert!(self.len() == other.len());

        let mut ary2 = Vec::<u8>::with_capacity(self.ints.len());

        for int_inx in 0..self.num_ints() {
            ary2.push(self.ints[int_inx] & other.ints[int_inx]);
        }
        Self { ints: ary2 }
    }

    /// Bitwise OR of two Bits structs.
    pub fn b_or(&self, other: &Self) -> Self {
        assert!(self.len() == other.len());

        let mut ary2 = Vec::<u8>::with_capacity(self.ints.len());

        for int_inx in 0..self.num_ints() {
            ary2.push(self.ints[int_inx] | other.ints[int_inx]);
        }
        Self { ints: ary2 }
    }

    /// Bitwise XOR of two Bits structs.
    pub fn b_xor(&self, other: &Self) -> Self {
        assert!(self.len() == other.len());

        let mut ary2 = Vec::<u8>::with_capacity(self.ints.len());

        for int_inx in 0..self.num_ints() {
            ary2.push(self.ints[int_inx] ^ other.ints[int_inx]);
        }
        Self { ints: ary2 }
    }

    /// Return true if the Bits struct value is low, that is all zeros.
    pub fn is_low(&self) -> bool {
        for int_inx in 0..self.num_ints() {
            if self.ints[int_inx] > 0 {
                return false;
            }
        }
        true
    }

    /// Return true if a Bits struct has at least one bit set to one.
    pub fn is_not_low(&self) -> bool {
        for int_inx in 0..self.num_ints() {
            if self.ints[int_inx] > 0 {
                return true;
            }
        }
        false
    }

    /// Return true if the Bits struct value is high, that is all ones.
    pub fn is_high(&self) -> bool {
        for intx in self.ints.iter() {
            if *intx != u8::MAX {
                return false;
            }
        }
        true
    }

    /// Return true if a Bits struct is a ones-subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        *self == self.b_and(&other)
    }

    /// Return true if a Bits struct is a ones-superset of another.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        *other == self.b_and(&other)
    }

    /// Return the number of bits set to one.
    pub fn num_one_bits(&self) -> usize {
        let mut cnt = 0;

        for intx in self.ints.iter() {
            cnt += intx.count_ones();
        }

        cnt as usize
    }

    /// Return the number of bits that are different.
    /// This can be interpreted as how "far away" two bit patterns are.
    pub fn distance(&self, other: &SomeBits) -> usize {
        self.b_xor(&other).num_one_bits()
    }

    /// Return true if only one bit is set to one.
    pub fn just_one_bit(&self) -> bool {
        let mut cnt = 0;

        for intx in self.ints.iter() {
            cnt += intx.count_ones();

            if cnt > 1 {
                return false;
            }
        }
        cnt == 1
    }

    /// Return a copy, shifted 1 to the left, and 1 added.
    pub fn push_1(&self) -> Self {
        let num_ints = self.num_ints();

        let mut tmp = self.shift_left(); // Shift all bits left, LSB bit becomes zero.

        tmp.ints[num_ints - 1] += 1;
        tmp
    }

    /// Return a copy, shifted left by 1 bit
    /// The Most Significant Bit value is lost.
    pub fn shift_left(&self) -> Self {
        let mut ints2 = vec![0 as u8; self.num_ints()];

        let mut carry: u8 = 0;
        let mut next_carry;

        for int_inx in (0..self.ints.len()).rev() {

            next_carry = self.ints[int_inx] >> (NUM_BITS_PER_INT - 1);

            ints2[int_inx] = (self.ints[int_inx] << 1) + carry;
            carry = next_carry;
        }

        // Overflow check
        //		if carry > 0 {
        //			panic!("Bits shift_left overflow");
        //		}

        Self { ints: ints2 }
    }

    /// Return a copy, shifted left by 1 bit
    /// The Most Significant 4 bit values are lost.
    pub fn shift_left4(&self) -> Self {
        let mut ints2 = vec![0 as u8; self.num_ints()];

        let mut carry: u8 = 0;
        let mut next_carry;

        for int_inx in (0..self.ints.len()).rev() {

            next_carry = self.ints[int_inx] >> (NUM_BITS_PER_INT - 4);

            ints2[int_inx] = (self.ints[int_inx] << 4) + carry;
            carry = next_carry;
        }

        // Overflow check
        //		if carry > 0 {
        //			panic!("Bits shift_left overflow");
        //		}

        Self { ints: ints2 }
    }

    /// Return the number of integers used in the given SomeBits struct.
    pub fn num_ints(&self) -> usize {
        self.ints.len()
    }

    /// Return true if the highest bit is set to 1.
    /// This is used in the from_string functions to detect overflow
    /// from the next shift-left operation.
    pub fn high_bit_set(&self) -> bool {
        if self.ints[0] & INT_HIGH_BIT == 0 {
            return false;
        }
        return true;
    }

    /// Calculate the expected length of a string that represents a SomeBits struct.
    pub fn formatted_string_length(&self) -> usize {
        (NUM_BITS_PER_INT * self.ints.len()) + self.ints.len()
    }

    /// Create a formatted string for the instance.
    pub fn formatted_string(&self, prefix: char) -> String {
        let mut astr = String::with_capacity(self.formatted_string_length());
        astr.push(prefix);

        let mut fil = 0;
        for intx in self.ints.iter() {
            if fil == 1 {
                astr.push('_');
            }
            astr.push_str(&format!("{:08b}", intx)); // increase 08 if the integer size increases

            fil = 1;
        }
        astr
    }

    /// Create a formatted string to display under an instance,
    /// to indicate specific bits positions.
    pub fn str2(&self, prefix: char) -> String {
        let mut astr = String::with_capacity(self.formatted_string_length());
        astr.push(prefix);

        let mut fil = 0;
        for intx in self.ints.iter() {
            if fil == 1 {
                astr.push(' ');
            }
            for i in (0..NUM_BITS_PER_INT).rev() {
                if (intx & ALL_BIT_MASKS[i]) == 0 {
                    astr.push(' ');
                } else {
                    astr.push('v');
                }
            }
            fil = 1;
        }
        astr
    }

    /// Return a copy, shifted 1 to the left.
    pub fn push_0(&self) -> Self {
        self.shift_left() // Shift all bits left, LSB bit becomes zero.
    }

} // end impl SomeBits

/// Create a clone of an instance.
impl Clone for SomeBits {
    fn clone(&self) -> Self {
        let mut v1 = Vec::<u8>::with_capacity(self.len());
        for num in self.ints.iter() {
            v1.push(*num);
        }
        Self { ints: v1 }
    }
}

#[cfg(test)]
mod tests {
    use crate::bits::SomeBits;

    // Test SomeBits::b_and
    #[test]
    fn test_b_and() -> Result<(), String> {
        // 00
        let mut test_and = SomeBits::new_from_string(2, "0x0").unwrap().b_and(&SomeBits::new_from_string(2, "0x0").unwrap());
        if test_and.is_not_low() {
            return Err(format!("SomeBits::b_and 1 failed"));
        }
        // 01
        test_and = SomeBits::new_from_string(2, "0x0").unwrap().b_and(&SomeBits::new_from_string(2, "0xffff").unwrap());
        if test_and.is_not_low() {
            return Err(format!("SomeBits::b_and 2 failed"));
        }
        // 11
        test_and = SomeBits::new_from_string(2, "0xffff").unwrap().b_and(&SomeBits::new_from_string(2, "0xffff").unwrap());
        if test_and != SomeBits::new_from_string(2, "0xffff").unwrap() {
            return Err(format!("SomeBits::b_and 3 failed"));
        }
        // 10
        test_and = SomeBits::new_from_string(2, "0xffff").unwrap().b_and(&SomeBits::new_from_string(2, "0x0").unwrap());
        if test_and.is_not_low() {
            return Err(format!("SomeBits::b_and 4 failed"));
        }
        Ok(())
    }

    // Test SomeBits::b_not
    #[test]
    fn test_b_not() -> Result<(), String> {
        let mut test_not = SomeBits::new_from_string(2, "0x5a5a").unwrap().b_not();
        if test_not != SomeBits::new_from_string(2, "0xa5a5").unwrap() {
            return Err(format!("SomeBits::b_not 1 failed"));
        }
        test_not = SomeBits::new_from_string(2, "0xa5a5").unwrap().b_not();
        if test_not != SomeBits::new_from_string(2, "0x5a5a").unwrap() {
            return Err(format!("SomeBits::b_not 2 failed"));
        }
        Ok(())
    }

    // Test SomeBits::b_or
    #[test]
    fn test_b_or() -> Result<(), String> {
        // 00
        let mut test_or = SomeBits::new_from_string(2, "0x0").unwrap().b_or(&SomeBits::new_from_string(2, "0x0").unwrap());
        if test_or.is_not_low() {
            return Err(format!("SomeBits::b_or 1 failed"));
        }
        // 01
        test_or = SomeBits::new_from_string(2, "0x0").unwrap().b_or(&SomeBits::new_from_string(2, "0xffff").unwrap());
        if test_or != SomeBits::new_from_string(2, "0xffff").unwrap() {
            return Err(format!("SomeBits::b_or 2 failed"));
        }
        // 11
        test_or = SomeBits::new_from_string(2, "0xffff").unwrap().b_or(&SomeBits::new_from_string(2, "0xffff").unwrap());
        if test_or != SomeBits::new_from_string(2, "0xffff").unwrap() {
            return Err(format!("SomeBits::b_or 3 failed"));
        }
        // 10
        test_or = SomeBits::new_from_string(2, "0xffff").unwrap().b_or(&SomeBits::new_from_string(2, "0x0").unwrap());
        if test_or != SomeBits::new_from_string(2, "0xffff").unwrap() {
            return Err(format!("SomeBits::b_or 4 failed"));
        }
        Ok(())
    }

    // Test SomeBits::b_xor
    #[test]
    fn test_b_xor() -> Result<(), String> {
        // 00
        let mut test_xor = SomeBits::new_from_string(2, "0x0").unwrap().b_xor(&SomeBits::new_from_string(2, "0x0").unwrap());
        if test_xor.is_not_low() {
            return Err(format!("SomeBits::b_xor 1 failed"));
        }
        // 01
        test_xor = SomeBits::new_from_string(2, "0x0").unwrap().b_xor(&SomeBits::new_from_string(2, "0xffff").unwrap());
        if test_xor != SomeBits::new_from_string(2, "0xffff").unwrap() {
            return Err(format!("SomeBits::b_xor 2 failed"));
        }
        // 11
        test_xor = SomeBits::new_from_string(2, "0xffff").unwrap().b_xor(&SomeBits::new_from_string(2, "0xffff").unwrap());
        if test_xor.is_not_low() {
            return Err(format!("SomeBits::b_xor 3 failed"));
        }
        // 10
        test_xor = SomeBits::new_from_string(2, "0xffff").unwrap().b_xor(&SomeBits::new_from_string(2, "0x0").unwrap());
        if test_xor != SomeBits::new_from_string(2, "0xffff").unwrap() {
            return Err(format!("SomeBits::b_xor 4 failed"));
        }
        Ok(())
    }

    // Test SomeBits::distance
    #[test]
    fn test_distance() -> Result<(), String> {

        if 0 != SomeBits::new_from_string(2, "0x0").unwrap().distance(&SomeBits::new_from_string(2, "0x0").unwrap()) {
            return Err(format!("SomeBits::distance 1 failed"));
        }

        if 8 != SomeBits::new_from_string(2, "0x5555").unwrap().distance(&SomeBits::new_from_string(2, "0x0").unwrap()) {
            return Err(format!("SomeBits::distance 2 failed"));
        }

        if 7 != SomeBits::new_from_string(2, "0xaaaa").unwrap().distance(&SomeBits::new_from_string(2, "0x02").unwrap()) {
            return Err(format!("SomeBits::distance 3 failed"));
        }

        Ok(())
    }

    // Test SomeBits::high_bit_set
    #[test]
    fn test_high_bit_set() -> Result<(), String> {

        let mut test_bool = SomeBits::high_bit_set(&SomeBits::new_from_string(1, "0x0").unwrap());
        if test_bool {
            return Err(format!("SomeBits::high_bit_set 1 failed"));
        }

        test_bool = SomeBits::high_bit_set(&SomeBits::new_from_string(1, "0x5").unwrap());
        if test_bool {
            return Err(format!("SomeBits::high_bit_set 2 failed"));
        }

        test_bool = SomeBits::high_bit_set(&SomeBits::new_from_string(1, "0x50").unwrap());
        if test_bool {
            return Err(format!("SomeBits::high_bit_set 3 failed"));
        }

        test_bool = SomeBits::high_bit_set(&SomeBits::new_from_string(1, "0xa0").unwrap());
        if test_bool == false {
            return Err(format!("SomeBits::high_bit_set 4 failed"));
        }

        test_bool = SomeBits::high_bit_set(&SomeBits::new_from_string(2, "0x00a0").unwrap());
        if test_bool {
            return Err(format!("SomeBits::high_bit_set 5 failed"));
        }

        test_bool = SomeBits::high_bit_set(&SomeBits::new_from_string(2, "0x5a0").unwrap());
        if test_bool {
            return Err(format!("SomeBits::high_bit_set 6 failed"));
        }

        test_bool = SomeBits::high_bit_set(&SomeBits::new_from_string(2, "0x5000").unwrap());
        if test_bool {
            return Err(format!("SomeBits::high_bit_set 7 failed"));
        }

        test_bool = SomeBits::high_bit_set(&SomeBits::new_from_string(2, "0xa000").unwrap());
        if test_bool == false {
            return Err(format!("SomeBits::high_bit_set 8 failed"));
        }

        Ok(())
    }

    // Test SomeBits::is_bit_set
    #[test]
    fn test_is_bit_set() -> Result<(), String> {
        let test_bits = SomeBits::new_from_string(2, "0x5aa5").unwrap();

        if test_bits.is_bit_set(0) == false {
            return Err(format!("SomeBits::is_bit_set 0 failed"));
        }

        if test_bits.is_bit_set(1) {
            return Err(format!("SomeBits::is_bit_set 1 failed"));
        }

        if test_bits.is_bit_set(2) == false {
            return Err(format!("SomeBits::is_bit_set 2 failed"));
        }

        if test_bits.is_bit_set(3) {
            return Err(format!("SomeBits::is_bit_set 3 failed"));
        }

        if test_bits.is_bit_set(4) {
            return Err(format!("SomeBits::is_bit_set 4 failed"));
        }

        if test_bits.is_bit_set(5) == false {
            return Err(format!("SomeBits::is_bit_set 5 failed"));
        }

        if test_bits.is_bit_set(6) {
            return Err(format!("SomeBits::is_bit_set 6 failed"));
        }

        if test_bits.is_bit_set(7) == false {
            return Err(format!("SomeBits::is_bit_set 7 failed"));
        }

        if test_bits.is_bit_set(8) {
            return Err(format!("SomeBits::is_bit_set 8 failed"));
        }

        if test_bits.is_bit_set(9) == false {
            return Err(format!("SomeBits::is_bit_set 9 failed"));
        }

        if test_bits.is_bit_set(10) {
            return Err(format!("SomeBits::is_bit_set 10 failed"));
        }

        if test_bits.is_bit_set(11) == false {
            return Err(format!("SomeBits::is_bit_set 11 failed"));
        }

        if test_bits.is_bit_set(12) == false {
            return Err(format!("SomeBits::is_bit_set 12 failed"));
        }

        if test_bits.is_bit_set(13)  {
            return Err(format!("SomeBits::is_bit_set 13 failed"));
        }
        
        if test_bits.is_bit_set(14) == false {
            return Err(format!("SomeBits::is_bit_set 14 failed"));
        }

        if test_bits.is_bit_set(15) {
            return Err(format!("SomeBits::is_bit_set 15 failed"));
        }
        Ok(())
    }

    // Test SomeBits::is_high
    #[test]
    fn test_is_high() -> Result<(), String> {
        if SomeBits::new_from_string(1, "0xa5").unwrap().is_high() {
            return Err(format!("SomeBits::is_high 1 failed"));
        }

        if SomeBits::new_from_string(1, "0xff").unwrap().is_high() == false {
            return Err(format!("SomeBits::is_high 2 failed"));
        }

        if SomeBits::new_from_string(2, "0xa5").unwrap().is_high() {
            return Err(format!("SomeBits::is_high 3 failed"));
        }

        if SomeBits::new_from_string(2, "0xffff").unwrap().is_high() == false {
            return Err(format!("SomeBits::is_high 4 failed"));
        }

        Ok(())
    }

    // Test SomeBits::is_low
    #[test]
    fn test_is_low() -> Result<(), String> {
        if SomeBits::new_from_string(1, "0xa5").unwrap().is_low() {
            return Err(format!("SomeBits::is_low 1 failed"));
        }

        if SomeBits::new_from_string(1, "0x0").unwrap().is_low() == false {
            return Err(format!("SomeBits::is_low 2 failed"));
        }

        if SomeBits::new_from_string(2, "0xa5").unwrap().is_low() {
            return Err(format!("SomeBits::is_low 3 failed"));
        }

        if SomeBits::new_from_string(2, "0x0").unwrap().is_low() == false {
            return Err(format!("SomeBits::is_low 4 failed"));
        }

        Ok(())
    }
    
    // Test SomeBits::is_not_low
    #[test]
    fn test_is_not_low() -> Result<(), String> {
        if SomeBits::new_from_string(1, "0xa5").unwrap().is_not_low() == false {
            return Err(format!("SomeBits::is_not_low 1 failed"));
        }

        if SomeBits::new_from_string(1, "0x0").unwrap().is_not_low() {
            return Err(format!("SomeBits::is_not_low 2 failed"));
        }

        if SomeBits::new_from_string(2, "0xa5").unwrap().is_not_low() == false {
            return Err(format!("SomeBits::is_not_low 3 failed"));
        }

        if SomeBits::new_from_string(2, "0x0").unwrap().is_not_low() {
            return Err(format!("SomeBits::is_not_low 4 failed"));
        }

        Ok(())
    }

    // Test SomeBits::is_subset_of
    #[test]
    fn test_is_subset_of() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x0").unwrap().is_subset_of(&SomeBits::new_from_string(2, "0x0").unwrap()) == false {
            return Err(format!("SomeBits::is_subset_of 1 failed"));
        }
        if SomeBits::new_from_string(2, "0x0").unwrap().is_subset_of(&SomeBits::new_from_string(2, "0x5").unwrap()) == false {
            return Err(format!("SomeBits::is_subset_of 2 failed"));
        }
        if SomeBits::new_from_string(2, "0x5555").unwrap().is_subset_of(&SomeBits::new_from_string(2, "0x7777").unwrap()) == false {
            return Err(format!("SomeBits::is_subset_of 3 failed"));
        }
        if SomeBits::new_from_string(2, "0x5").unwrap().is_subset_of(&SomeBits::new_from_string(2, "0x1").unwrap()) {
            return Err(format!("SomeBits::is_subset_of 4 failed"));
        }
        if SomeBits::new_from_string(2, "0x7777").unwrap().is_subset_of(&SomeBits::new_from_string(2, "0x5555").unwrap()) {
            return Err(format!("SomeBits::is_subset_of 5 failed"));
        }
        Ok(())
    }

    // Test SomeBits::is_superset_of
    #[test]
    fn test_is_superset_of() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x0").unwrap().is_superset_of(&SomeBits::new_from_string(2, "0x0").unwrap()) == false {
            return Err(format!("SomeBits::is_superset_of 1 failed"));
        }
        if SomeBits::new_from_string(2, "0x5").unwrap().is_superset_of(&SomeBits::new_from_string(2, "0x0").unwrap()) == false {
            return Err(format!("SomeBits::is_superset_of 2 failed"));
        }
        if SomeBits::new_from_string(2, "0x7777").unwrap().is_superset_of(&SomeBits::new_from_string(2, "0x5555").unwrap()) == false {
            return Err(format!("SomeBits::is_superset_of 3 failed"));
        }
        if SomeBits::new_from_string(2, "0x1").unwrap().is_superset_of(&SomeBits::new_from_string(2, "0x5").unwrap()) {
            return Err(format!("SomeBits::is_superset_of 4 failed"));
        }
        if SomeBits::new_from_string(2, "0x5555").unwrap().is_superset_of(&SomeBits::new_from_string(2, "0x7777").unwrap()) {
            return Err(format!("SomeBits::is_superset_of 5 failed"));
        }
        Ok(())
    }

    // Test SomeBits::just_one_bit
    #[test]
    fn test_just_one_bit() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5555").unwrap().just_one_bit() {
            return Err(format!("SomeBits::just_one_bit 1 failed"));
        }
        if SomeBits::new_from_string(2, "0x3000").unwrap().just_one_bit() {
            return Err(format!("SomeBits::just_one_bit 2 failed"));
        }
        if SomeBits::new_from_string(2, "0x4000").unwrap().just_one_bit() == false {
            return Err(format!("SomeBits::just_one_bit 3 failed"));
        }
        if SomeBits::new_from_string(2, "0x0200").unwrap().just_one_bit() == false {
            return Err(format!("SomeBits::just_one_bit 4 failed"));
        }
        if SomeBits::new_from_string(2, "0x0010").unwrap().just_one_bit() == false {
            return Err(format!("SomeBits::just_one_bit 5 failed"));
        }
        if SomeBits::new_from_string(2, "0x0002").unwrap().just_one_bit() == false {
            return Err(format!("SomeBits::just_one_bit 6 failed"));
        }
        Ok(())
    }

    // Test SomeBits::num_one_bits
    #[test]
    fn test_num_one_bits() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5555").unwrap().num_one_bits() != 8 {
            return Err(format!("SomeBits::num_one_bits 1 failed"));
        }
        Ok(())
    }

    // Test SomeBits::push_1
    #[test]
    fn test_push_1() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5555").unwrap().push_1() !=  SomeBits::new_from_string(2, "0xaaab").unwrap() {
            return Err(format!("SomeBits::push_1 1 failed"));
        }
        Ok(())
    }

    // Test SomeBits::shift_left
    #[test]
    fn test_shift_left() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5555").unwrap().shift_left() !=  SomeBits::new_from_string(2, "0xaaaa").unwrap() {
            return Err(format!("SomeBits::shift_left 1 failed"));
        }
        Ok(())
    }

    // Test SomeBits::shift_left4
    #[test]
    fn test_shift_left4() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x505").unwrap().shift_left4() !=  SomeBits::new_from_string(2, "0x5050").unwrap() {
            return Err(format!("SomeBits::shift_left4 1 failed"));
        }
        Ok(())
    }

    // Test SomeBits::split
    #[test]
    fn test_split() -> Result<(), String> {
        let avec: Vec<SomeBits> = SomeBits::new_from_string(2, "0x5050").unwrap().split();
        if avec.len() != 4 {
            return Err(format!("SomeBits::split 1 failed"));
        }
        if avec.contains(&SomeBits::new_from_string(2, "0x4000").unwrap()) == false {
            return Err(format!("SomeBits::split 2 failed"));
        }
        if avec.contains(&SomeBits::new_from_string(2, "0x1000").unwrap()) == false {
            return Err(format!("SomeBits::split 3 failed"));
        }
        if avec.contains(&SomeBits::new_from_string(2, "0x0040").unwrap()) == false {
            return Err(format!("SomeBits::split 4 failed"));
        }
        if avec.contains(&SomeBits::new_from_string(2, "0x0010").unwrap()) == false {
            return Err(format!("SomeBits::split 5 failed"));
        }
        Ok(())
    }

    // Test SomeBits::toggle_bits
    #[test]
    fn test_toggle_bits() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x505").unwrap().toggle_bits(vec![1,8,11]) !=  SomeBits::new_from_string(2, "0xc07").unwrap() {
            return Err(format!("SomeBits::toggle_bits 1 failed"));
        }
        Ok(())
    }

    // Test bits.clone
    #[test]
    fn test_clone() -> Result<(), String> {
        let tmp = SomeBits::new_from_string(2, "0x505").unwrap();
        if tmp != tmp.clone() {
            return Err(format!("SomeBits::clone 1 failed"));
        }
        Ok(())
    }
}
