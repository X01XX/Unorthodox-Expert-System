//! The SomeBits struct, storing a bit pattern in one or more unsigned integers.
//!
//! Some conventions:
//!
//!   Display all bits in all integers.
//!
//!   Counting bits starts at the right-most bit of the right-most int,
//!   and proceeds to the left, as in standard integer bit-position reckoning.
//!
//! The integer type/size can be increased, change "u8", below, the format string literal "08b" in the formatted_string function.
//!
//! Change the constants, below, as needed.
//!

/// The unsigned integer type used in a vector of bits.
pub type Bitint = u8;

/// The number of bits in an integer used by SomeBits.
const NUM_BITS_PER_INT: usize = u8::BITS as usize;

/// Mask for the highest bit in an integer.
const INT_HIGH_BIT: Bitint = 1 << (NUM_BITS_PER_INT - 1);

/// Mask for the highest nibble in an integer.
const INT_HIGH_NIBBLE: Bitint = 15 << (NUM_BITS_PER_INT - 4);

use crate::randompick;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

/// Display trait for SomeBits
impl fmt::Display for SomeBits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string('b'))
    }
}

#[readonly::make] // DeRef trait has a problem with this.
#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq, Clone)]

/// SomeBits struct, just an unsigned integer vector.
/// This structure sets the type of integer.
/// Each Domain sets the number of integers.
/// The lowest significant bit is the right-most bit or the right-most integer.
/// If there was only one domain, therefore state, this struct may be able to use an array
/// instead of a vector.
pub struct SomeBits {
    pub ints: Vec<Bitint>,
}

impl SomeBits {
    /// Return a new SomeBits instance.
    pub fn new(ints: Vec<Bitint>) -> Self {
        assert!(!ints.is_empty());
        Self { ints }
    }

    /// Create a SomeBits instance with integer(s) set to zero.
    pub fn new_low(num_ints: usize) -> Self {
        assert!(num_ints > 0);
        SomeBits {
            ints: vec![0 as Bitint; num_ints],
        }
    }

    /// Create a SomeBits instance with integer(s) set to all ones.
    pub fn new_high(num_ints: usize) -> Self {
        assert!(num_ints > 0);
        SomeBits {
            ints: vec![Bitint::MAX as Bitint; num_ints],
        }
    }

    /// Return a new bits instance, with a random value.
    pub fn new_random(num_ints: usize) -> Self {
        let mut ints = vec![0 as Bitint; num_ints];
        for intx in ints.iter_mut() {
            *intx = rand::thread_rng().gen_range(0..Bitint::MAX)
        }
        SomeBits { ints }
    }

    /// Return a bits instance from a string.
    /// Left-most, consecutive, zeros can be omitted.
    /// Underscore character is ignored.
    ///
    /// if let Ok(bts) = SomeBits::new_from_string(1, "0b0101")) {
    ///    println!("bts {}", &bts);
    /// } else {
    ///    panic!("Invalid bits string");
    /// }
    /// A prefix of "0x" can be used to specify hexadecimal characters.
    ///
    /// The num_ints argument is a little awkward, but may not be needed if there
    /// is only one domain/state.
    pub fn new_from_string(num_ints: usize, str: &str) -> Result<Self, String> {
        let mut bts = SomeBits::new_low(num_ints);

        let mut base = 2;

        for (inx, chr) in str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "0" {
                    continue;
                }
                return Err(format!("String {str}, should start with 0?"));
            }

            if inx == 1 {
                if chr == "b" || chr == "B" {
                    continue;
                } else if chr == "x" || chr == "X" {
                    base = 16;
                    continue;
                }
                return Err(format!("String {str}, should start with 0b or 0x?"));
            }

            let lsb = num_ints - 1;

            if chr == "_" {
                continue;
            }

            if base == 2 {
                if bts.high_bit_nonzero() {
                    return Err(format!("String {str}, too long?"));
                }

                if chr == "0" {
                    bts = bts.shift_left();
                } else if chr == "1" {
                    bts = bts.push_1();
                } else {
                    return Err(format!("String {str}, invalid character?"));
                }
            } else {
                if bts.high_nibble_nonzero() {
                    return Err(String::from("String {str}, too long?"));
                }

                let Ok(numx) = Bitint::from_str_radix(chr, 16) else {
                    return Err(format!(
                        "String {str}, invalid character?"))  };

                bts = bts.shift_left4();

                bts.ints[lsb] += numx;
            }
        } // next (inx, chr)
        Ok(bts)
    } // end new_from_string

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
                let tmp2 = tmpint - 1; // zero the bit, rightmost zeros turn to 1

                let abit = tmpint & !tmp2; // isolate the bit

                let mut btsx = SomeBits::new_low(num_ints); // new Bits object, all zeros

                btsx.ints[int_inx] = abit; // update one integer

                rc_vec.push(btsx); // Save one-bit result

                tmpint &= tmp2; // remove bit
            }
        } // end for int_inx
        rc_vec
    }

    /// Return true if a bit is one at a given position number.
    /// Match the intuition of a hexidecimal representation.
    /// Like 0xffffe. The least significant bit, in this case its equal 0, is bit number zero.
    /// A minor difficulty is that the most significant integer, in the vector of integers, is index zero.
    pub fn is_bit_set(&self, bit_num: usize) -> bool {
        let num_ints = self.ints.len();

        let num_bits = num_ints * NUM_BITS_PER_INT;

        if bit_num >= num_bits {
            panic!("bit num too large");
        }

        let bit_pos = bit_num % NUM_BITS_PER_INT; // Calc bit index within one integer.

        let int_num = num_ints - 1 - (bit_num / NUM_BITS_PER_INT); // Calc integer index in vector.

        self.ints[int_num] & (1 << bit_pos) > 0
    }

    /// Return the bitwise NOT of a SomeBits stuct.
    pub fn b_not(&self) -> Self {
        let mut ary2 = Vec::<Bitint>::with_capacity(self.ints.len());

        for intx in &self.ints {
            ary2.push(!intx);
        }

        Self { ints: ary2 }
    }

    /// Return the bitwise AND of two SomeBits structs.
    pub fn b_and(&self, other: &Self) -> Self {
        assert_eq!(self.num_ints(), other.num_ints());

        let mut ary2 = Vec::<Bitint>::with_capacity(self.ints.len());

        for (x, y) in self.ints.iter().zip(other.ints.iter()) {
            ary2.push(x & y);
        }

        Self { ints: ary2 }
    }

    /// Return the bitwise OR of two SomeBits structs.
    pub fn b_or(&self, other: &Self) -> Self {
        assert_eq!(self.num_ints(), other.num_ints());

        let mut ary2 = Vec::<Bitint>::with_capacity(self.ints.len());

        for (x, y) in self.ints.iter().zip(other.ints.iter()) {
            ary2.push(x | y);
        }

        Self { ints: ary2 }
    }

    /// Return the bitwise XOR of two SomeBits structs.
    pub fn b_xor(&self, other: &Self) -> Self {
        assert_eq!(self.num_ints(), other.num_ints());

        let mut ary2 = Vec::<Bitint>::with_capacity(self.ints.len());

        for (x, y) in self.ints.iter().zip(other.ints.iter()) {
            ary2.push(x ^ y);
        }

        Self { ints: ary2 }
    }

    /// Return a copy of an instance, with a bit position changed.
    /// X to X-not for a specific bit.
    pub fn change_bit(&self, bit_num: usize) -> Self {
        let num_ints = self.ints.len();

        let num_bits = num_ints * NUM_BITS_PER_INT;

        if bit_num >= num_bits {
            panic!("bit num too large");
        }

        let bit_pos = bit_num % NUM_BITS_PER_INT; // Calc bit index within one integer.

        let int_num = num_ints - 1 - (bit_num / NUM_BITS_PER_INT); // Calc integer index in vector.

        let mut ary2 = self.clone();

        ary2.ints[int_num] ^= (1 << bit_pos) as Bitint;

        ary2
    }

    /// Return a copy of an instance, with a bit position set to 1.
    /// X to 1 for a specific bit.
    pub fn set_bit_to_1(&self, bit_num: usize) -> Self {
        let num_ints = self.ints.len();

        let num_bits = num_ints * NUM_BITS_PER_INT;

        if bit_num >= num_bits {
            panic!("bit num too large");
        }

        let bit_pos = bit_num % NUM_BITS_PER_INT; // Calc bit index within one integer.

        let int_num = num_ints - 1 - (bit_num / NUM_BITS_PER_INT); // Calc integer index in vector.

        let mut ary2 = self.clone();

        ary2.ints[int_num] |= (1 << bit_pos) as Bitint;

        ary2
    }

    /// Return a copy of an instance, with a bit position set to 0.
    /// X to 0, for a specific bit.
    pub fn set_bit_to_0(&self, bit_num: usize) -> Self {
        let num_ints = self.ints.len();

        let num_bits = num_ints * NUM_BITS_PER_INT;

        if bit_num >= num_bits {
            panic!("bit num too large");
        }

        let bit_pos = bit_num % NUM_BITS_PER_INT; // Calc bit index within one integer.

        let int_num = num_ints - 1 - (bit_num / NUM_BITS_PER_INT); // Calc integer index in vector.

        let mut ary2 = self.clone();

        ary2.ints[int_num] &= !(1 << bit_pos) as Bitint;

        ary2
    }

    /// Return Bits that are the same
    pub fn b_eqv(&self, other: &Self) -> Self {
        self.b_xor(other).b_not()
    }

    /// Return true if the Bits struct value is low, that is all zeros.
    pub fn is_low(&self) -> bool {
        for inx in &self.ints {
            if *inx > 0 {
                return false;
            }
        }
        true
    }

    /// Return true if a Bits struct has at least one bit set to one.
    pub fn is_not_low(&self) -> bool {
        !self.is_low()
    }

    /// Return true if the Bits struct value is high, that is all ones.
    pub fn is_high(&self) -> bool {
        for intx in &self.ints {
            if *intx != Bitint::MAX {
                return false;
            }
        }
        true
    }

    /// Return true if a Bits struct has all bits set to one.
    pub fn is_not_high(&self) -> bool {
        !self.is_high()
    }

    /// Return true if a Bits struct is a ones-subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        *self == self.b_and(other)
    }

    /// Return true if a Bits struct is a ones-superset of another.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        *other == self.b_and(other)
    }

    /// Return the number of bits set to one.
    pub fn num_one_bits(&self) -> usize {
        let mut cnt = 0;

        for intx in &self.ints {
            cnt += intx.count_ones();
        }

        cnt as usize
    }

    /// Return the number of bits that are different.
    /// This can be interpreted as how "far away" two bit patterns are.
    pub fn distance(&self, other: &SomeBits) -> usize {
        self.b_xor(other).num_one_bits()
    }

    /// Return true if two bits instances are adjacent.
    pub fn is_adjacent(&self, other: &SomeBits) -> bool {
        self.b_xor(other).just_one_bit()
    }

    /// Return true if only one bit is set to one.
    pub fn just_one_bit(&self) -> bool {
        let mut cnt = 0;

        for intx in &self.ints {
            if *intx == 0 {
                continue;
            }
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
        let mut ints2 = vec![0 as Bitint; self.num_ints()];

        let mut carry: Bitint = 0;

        for int_inx in (0..self.ints.len()).rev() {
            let next_carry: Bitint = self.ints[int_inx] >> (NUM_BITS_PER_INT - 1);

            ints2[int_inx] = (self.ints[int_inx] << 1) + carry;

            carry = next_carry;
        }

        Self { ints: ints2 }
    }

    /// Return a copy, shifted left by 4 bits.
    /// The Most Significant 4 bit value is lost.
    pub fn shift_left4(&self) -> Self {
        let mut ints2 = vec![0 as Bitint; self.num_ints()];

        let mut carry: Bitint = 0;

        for int_inx in (0..self.ints.len()).rev() {
            let next_carry: Bitint = self.ints[int_inx] >> (NUM_BITS_PER_INT - 4);

            ints2[int_inx] = (self.ints[int_inx] << 4) + carry;
            carry = next_carry;
        }

        Self { ints: ints2 }
    }

    /// Return the number of integers used in the given SomeBits struct.
    pub fn num_ints(&self) -> usize {
        self.ints.len()
    }

    /// Return the number of bits in the given SomeBits struct.
    pub fn num_bits(&self) -> usize {
        self.ints.len() * NUM_BITS_PER_INT
    }

    /// Return true if the highest bit is nonzero.
    /// This is used in the from_string functions to detect overflow
    /// from the next shift-left operation.
    pub fn high_bit_nonzero(&self) -> bool {
        self.ints[0] & INT_HIGH_BIT > 0
    }

    /// Return true if the highest nibble is nonzero.
    /// This is used in the from_string functions to detect overflow
    /// from the next shift-left operation.
    pub fn high_nibble_nonzero(&self) -> bool {
        self.ints[0] & INT_HIGH_NIBBLE > 0
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
        for intx in &self.ints {
            if fil == 1 {
                astr.push('_');
            }
            astr.push_str(&format!("{intx:08b}")); // increase 08 if the integer size increases

            fil = 1;
        }
        astr
    }

    /// Create a formatted string to display under an instance,
    /// to indicate specific bits positions.
    pub fn str2(&self) -> String {
        let mut astr = String::with_capacity(self.formatted_string_length());

        for intx in &self.ints {
            astr.push(' ');

            let mut cnt = 0;
            let mut cur_bit = INT_HIGH_BIT;

            while cur_bit > 0 {
                if cnt > 0 && cnt % 4 == 0 {
                    // Adjust for underline character in bits display.
                    astr.push(' ');
                }
                cnt += 1;

                if (intx & cur_bit) == 0 {
                    astr.push(' ');
                } else {
                    astr.push('v');
                }
                cur_bit >>= 1;
            }
        }
        astr
    }

    /// Return a copy, shifted 1 to the left.
    pub fn push_0(&self) -> Self {
        self.shift_left() // Shift all bits left, LSB bit becomes zero.
    }

    /// Given a mask of more than one bit set to one, return a SomeBits instance
    /// that is a random selection of roughly half the bits.
    pub fn half_bits(&self) -> Self {
        let one_bits: Vec<SomeBits> = self.split();

        if one_bits.len() < 2 {
            panic!("Less than two bits {}?", self);
        }

        let indices: Vec<usize> = randompick::random_x_of_n(one_bits.len() / 2, one_bits.len());

        let mut or_bts = SomeBits::new_low(self.num_ints());

        for inx in indices {
            or_bts = or_bts.b_or(&one_bits[inx]);
        }
        or_bts
    }
} // end impl SomeBits

/// Define the BitsRef trait, so SomeBits, SomeMask, SomeState structs can interact at the SomeBits level.
pub trait BitsRef {
    fn bitsref(&self) -> &SomeBits;
}

/// Implement the trait BitsRef for SomeBits.
impl BitsRef for SomeBits {
    fn bitsref(&self) -> &SomeBits {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::Rng;

    // Test new
    #[test]
    fn new() -> Result<(), String> {
        let bitx = SomeBits {
            ints: vec![5 as Bitint, 4 as Bitint],
        };
        if bitx.ints[0] != 5 {
            return Err(String::from("Test 1 failed"));
        }
        if bitx.ints[1] != 4 {
            return Err(String::from("Test 2 failed"));
        }
        Ok(())
    }

    // Test new_from_string, using randomly chosen hexadecimal digits.
    #[test]
    fn new_from_string() -> Result<(), String> {
        // Init bit patterns expected for each hexadecimal character.
        let bit_chars = [
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
        ];

        // Try 16 times.
        for _ in 0..16 {
            // Generate first bin int
            let mut first_str = String::new();
            let mut first_num: Bitint = 0;
            for _ in 0..NUM_BITS_PER_INT {
                let inx = rand::thread_rng().gen_range(0..100) % 2;
                first_str.push(bit_chars[inx]);
                first_num *= 2;
                first_num += inx as Bitint;
            }

            // Generate second bin int
            let mut second_str = String::new();
            let mut second_num = 0;
            for _ in 0..NUM_BITS_PER_INT {
                let inx = rand::thread_rng().gen_range(0..100) % 2;
                second_str.push(bit_chars[inx]);
                second_num *= 2;
                second_num += inx as Bitint;
            }

            // Generate bits instance.
            let bits_inst =
                SomeBits::new_from_string(2, &("0b".to_owned() + &first_str + &second_str))
                    .unwrap();

            // Check bits instance
            assert!(bits_inst.ints[0] == first_num);
            assert!(bits_inst.ints[1] == second_num);

            // Generate first hex int
            let mut first_str = String::new();
            let mut first_num: Bitint = 0;
            for _ in 0..(NUM_BITS_PER_INT / 4) {
                let inx = rand::thread_rng().gen_range(0..100) % 16;
                first_str.push(bit_chars[inx]);
                first_num *= 16;
                first_num += inx as Bitint;
            }

            // Generate second hex int
            let mut second_str = String::new();
            let mut second_num = 0;
            for _ in 0..(NUM_BITS_PER_INT / 4) {
                let inx = rand::thread_rng().gen_range(0..100) % 16;
                second_str.push(bit_chars[inx]);
                second_num *= 16;
                second_num += inx as Bitint;
            }

            // Generate bits instance.
            let bits_inst =
                SomeBits::new_from_string(2, &("0x".to_owned() + &first_str + &second_str))
                    .unwrap();

            // Check bits instance
            assert!(bits_inst.ints[0] == first_num);
            assert!(bits_inst.ints[1] == second_num);
        }

        Ok(())
    }

    // Test b_eqv
    #[test]
    fn b_eqv() -> Result<(), String> {
        let b1 = SomeBits::new_from_string(2, "0x5555")?;
        let b2 = SomeBits::new_from_string(2, "0xa675")?;
        let b3 = b1.b_eqv(&b2);
        println!("b3 = {}", b3);
        let b4 = SomeBits::new_from_string(2, "0x0cdf")?;
        assert_eq!(b3, b4);
        Ok(())
    }

    // Test SomeBits::b_and
    #[test]
    fn b_and() -> Result<(), String> {
        // 00
        let mut bitsx = SomeBits::new_low(2);
        let mut bitsy = SomeBits::new_low(2);
        let mut bitsz = bitsx.b_and(&bitsy);

        if bitsz.is_not_low() {
            return Err(String::from("Result not low?"));
        }
        // 01
        bitsx = SomeBits::new_low(2);
        bitsy = SomeBits::new_high(2);
        bitsz = bitsx.b_and(&bitsy);

        if bitsz.is_not_low() {
            return Err(String::from("Result not low?"));
        }
        // 11
        bitsx = SomeBits::new_high(2);
        bitsy = SomeBits::new_high(2);
        bitsz = bitsx.b_and(&bitsy);

        if bitsz.is_not_high() {
            return Err(String::from("Result not high?"));
        }
        // 10
        bitsx = SomeBits::new_high(2);
        bitsy = SomeBits::new_low(2);
        bitsz = bitsx.b_and(&bitsy);

        if bitsz.is_not_low() {
            return Err(String::from("Result not low?"));
        }
        Ok(())
    }

    // Test SomeBits::b_not
    #[test]
    fn b_not() -> Result<(), String> {
        let mut bitsx = SomeBits::new_low(2);
        let mut bitsz = bitsx.b_not();

        if bitsz.is_not_high() {
            return Err(String::from("Result not high?"));
        }

        bitsx = SomeBits::new_high(2);
        bitsz = bitsx.b_not();

        if bitsz.is_not_low() {
            return Err(String::from("Result not low?"));
        }
        Ok(())
    }

    // Test SomeBits::b_or
    #[test]
    fn b_or() -> Result<(), String> {
        // 00
        let mut bitsx = SomeBits::new_low(2);
        let mut bitsy = SomeBits::new_low(2);
        let mut bitsz = bitsx.b_or(&bitsy);

        if bitsz.is_not_low() {
            return Err(String::from("Result not low?"));
        }
        // 01
        bitsx = SomeBits::new_low(2);
        bitsy = SomeBits::new_high(2);
        bitsz = bitsx.b_or(&bitsy);

        if bitsz.is_not_high() {
            return Err(String::from("Result not high?"));
        }
        // 11
        bitsx = SomeBits::new_high(2);
        bitsy = SomeBits::new_high(2);
        bitsz = bitsx.b_or(&bitsy);

        if bitsz.is_not_high() {
            return Err(String::from("Result not high?"));
        }
        // 10
        bitsx = SomeBits::new_high(2);
        bitsy = SomeBits::new_low(2);
        bitsz = bitsx.b_or(&bitsy);

        if bitsz.is_not_high() {
            return Err(String::from("Result not high?"));
        }
        Ok(())
    }

    // Test SomeBits::b_xor
    #[test]
    fn b_xor() -> Result<(), String> {
        // 00
        let mut bitsx = SomeBits::new_low(2);
        let mut bitsy = SomeBits::new_low(2);
        let mut bitsz = bitsx.b_xor(&bitsy);

        if bitsz.is_not_low() {
            return Err(String::from("Result not low?"));
        }
        // 01
        bitsx = SomeBits::new_low(2);
        bitsy = SomeBits::new_high(2);
        bitsz = bitsx.b_xor(&bitsy);

        if bitsz.is_not_high() {
            return Err(String::from("Result not high?"));
        }
        // 11
        bitsx = SomeBits::new_high(2);
        bitsy = SomeBits::new_high(2);
        bitsz = bitsx.b_xor(&bitsy);

        if bitsz.is_not_low() {
            return Err(String::from("Result not low?"));
        }
        // 10
        bitsx = SomeBits::new_high(2);
        bitsy = SomeBits::new_low(2);
        bitsz = bitsx.b_xor(&bitsy);

        if bitsz.is_not_high() {
            return Err(String::from("Result not high?"));
        }
        Ok(())
    }

    // Test change_bit functions.
    #[test]
    fn change_bits() -> Result<(), String> {
        let bx11 = SomeBits::new_from_string(1, "0x11")?;
        let mut btest = SomeBits::new_low(1);
        btest = btest.change_bit(0).change_bit(4);
        if bx11 != btest {
            return Err(format!(
                "{} NE {} ?",
                bx11.formatted_string('b'),
                btest.formatted_string('b')
            ));
        }

        let bx10 = SomeBits::new_from_string(1, "0x10")?;
        btest = btest.set_bit_to_0(0);
        if bx10 != btest {
            return Err(format!(
                "{} NE {} ?",
                bx10.formatted_string('b'),
                btest.formatted_string('b')
            ));
        }

        btest = btest.set_bit_to_1(0);
        if bx11 != btest {
            return Err(format!(
                "{} NE {} ?",
                bx11.formatted_string('b'),
                btest.formatted_string('b')
            ));
        }
        Ok(())
    }

    // Test SomeBits::distance
    #[test]
    fn distance() -> Result<(), String> {
        if 0 != SomeBits::new_from_string(2, "0x0")?.distance(&SomeBits::new_from_string(2, "0x0")?)
        {
            return Err(String::from("Result  1 not 0?"));
        }

        if 8 != SomeBits::new_from_string(2, "0x5555")?
            .distance(&SomeBits::new_from_string(2, "0x0")?)
        {
            return Err(String::from("Result 2 not 8?"));
        }

        if 7 != SomeBits::new_from_string(2, "0xaaaa")?
            .distance(&SomeBits::new_from_string(2, "0x02")?)
        {
            return Err(String::from("Result 3 not 7?"));
        }

        Ok(())
    }

    // Test SomeBits::high_bit_nonzero
    #[test]
    fn high_bit_nonzero() -> Result<(), String> {
        let mut btest = SomeBits::new_from_string(1, "0x1")?;

        if btest.high_bit_nonzero() {
            return Err(String::from("Result 1 true?"));
        }

        for _ in 0..(NUM_BITS_PER_INT - 1) {
            btest = btest.shift_left();
        }

        if !btest.high_bit_nonzero() {
            return Err(String::from("Result 2 false?"));
        }

        Ok(())
    }

    // Test SomeBits::is_bit_set
    #[test]
    fn is_bit_set() -> Result<(), String> {
        let test_bits = SomeBits::new_from_string(2, "0x5aa5")?;

        if !test_bits.is_bit_set(0) {
            return Err(String::from("Test 0 failed?"));
        }

        if test_bits.is_bit_set(1) {
            return Err(String::from("Test 1 failed?"));
        }

        if !test_bits.is_bit_set(2) {
            return Err(String::from("Test 2 failed?"));
        }

        if test_bits.is_bit_set(3) {
            return Err(String::from("Test 3 failed?"));
        }

        if test_bits.is_bit_set(4) {
            return Err(String::from("Test 4 failed?"));
        }

        if !test_bits.is_bit_set(5) {
            return Err(String::from("Test 5 failed?"));
        }

        if test_bits.is_bit_set(6) {
            return Err(String::from("Test 6 failed?"));
        }

        if !test_bits.is_bit_set(7) {
            return Err(String::from("Test 7 failed?"));
        }

        if test_bits.is_bit_set(8) {
            return Err(String::from("Test 8 failed?"));
        }

        if !test_bits.is_bit_set(9) {
            return Err(String::from("Test 9 failed?"));
        }

        if test_bits.is_bit_set(10) {
            return Err(String::from("Test 10 failed?"));
        }

        if !test_bits.is_bit_set(11) {
            return Err(String::from("Test 11 failed?"));
        }

        if !test_bits.is_bit_set(12) {
            return Err(String::from("Test 12 failed?"));
        }

        if test_bits.is_bit_set(13) {
            return Err(String::from("Test 13 failed?"));
        }

        if !test_bits.is_bit_set(14) {
            return Err(String::from("Test 14 failed?"));
        }

        if test_bits.is_bit_set(15) {
            return Err(String::from("Test 15 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::is_high
    #[test]
    fn is_high() -> Result<(), String> {
        let mut btest = SomeBits::new_from_string(1, "0x1")?;
        let bone = btest.clone();

        if btest.is_high() {
            return Err(String::from("Test 1 failed?"));
        }

        for _ in 0..(NUM_BITS_PER_INT - 1) {
            btest = btest.shift_left().b_or(&bone);
        }
        if !btest.is_high() {
            return Err(String::from("Test 2 failed?"));
        }

        Ok(())
    }

    // Test SomeBits::is_low
    #[test]
    fn is_low() -> Result<(), String> {
        if SomeBits::new_from_string(1, "0xa5")?.is_low() {
            return Err(String::from("Test 1 failed?"));
        }

        if !SomeBits::new_low(1).is_low() {
            return Err(String::from("Test 2 failed?"));
        }

        if SomeBits::new_from_string(2, "0x2000")?.is_low() {
            return Err(String::from("Test 3 failed?"));
        }

        if !SomeBits::new_low(2).is_low() {
            return Err(String::from("Test 4 failed?"));
        }

        Ok(())
    }

    // Test SomeBits::is_subset_of
    #[test]
    fn is_subset_of() -> Result<(), String> {
        if !SomeBits::new_low(2).is_subset_of(&SomeBits::new_from_string(2, "0x0")?) {
            return Err(String::from("Test 1 failed?"));
        }
        if !SomeBits::new_low(2).is_subset_of(&SomeBits::new_from_string(2, "0x5")?) {
            return Err(String::from("Test 2 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x5555")?
            .is_subset_of(&SomeBits::new_from_string(2, "0x7777")?)
        {
            return Err(String::from("Test 3 failed?"));
        }
        if SomeBits::new_from_string(2, "0x5")?.is_subset_of(&SomeBits::new_from_string(2, "0x1")?)
        {
            return Err(String::from("Test 4 failed?"));
        }
        if SomeBits::new_from_string(2, "0x7777")?
            .is_subset_of(&SomeBits::new_from_string(2, "0x5555")?)
        {
            return Err(String::from("Test 5 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::is_superset_of
    #[test]
    fn is_superset_of() -> Result<(), String> {
        if !SomeBits::new_from_string(2, "0x0")?
            .is_superset_of(&SomeBits::new_from_string(2, "0x0")?)
        {
            return Err(String::from("Test 1 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x5")?
            .is_superset_of(&SomeBits::new_from_string(2, "0x0")?)
        {
            return Err(String::from("Test 2 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x7777")?
            .is_superset_of(&SomeBits::new_from_string(2, "0x5555")?)
        {
            return Err(format!("Test 3 failed?"));
        }
        if SomeBits::new_from_string(2, "0x1")?
            .is_superset_of(&SomeBits::new_from_string(2, "0x5")?)
        {
            return Err(String::from("Test 4 failed?"));
        }
        if SomeBits::new_from_string(2, "0x5555")?
            .is_superset_of(&SomeBits::new_from_string(2, "0x7777")?)
        {
            return Err(String::from("Test 5 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::just_one_bit
    #[test]
    fn just_one_bit() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5000")?.just_one_bit() {
            return Err(String::from("Test 1 failed?"));
        }

        if SomeBits::new_from_string(2, "0x0300")?.just_one_bit() {
            return Err(String::from("Test 2 failed?"));
        }

        if SomeBits::new_from_string(2, "0x0060")?.just_one_bit() {
            return Err(String::from("Test 3 failed?"));
        }

        if SomeBits::new_from_string(2, "0x0007")?.just_one_bit() {
            return Err(String::from("Test 4 failed?"));
        }

        if SomeBits::new_from_string(2, "0x0102")?.just_one_bit() {
            return Err(String::from("Test 5 failed?"));
        }

        if SomeBits::new_from_string(2, "0x4080")?.just_one_bit() {
            return Err(String::from("Test 6 failed?"));
        }

        if !SomeBits::new_from_string(2, "0x4000")?.just_one_bit() {
            return Err(String::from("Test 7 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x0800")?.just_one_bit() {
            return Err(String::from("Test 8 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x0010")?.just_one_bit() {
            return Err(String::from("Test 9 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x0002")?.just_one_bit() {
            return Err(String::from("Test 10 failed?"));
        }
        Ok(())
    }

    #[test]
    fn half_bits() -> Result<(), String> {
        let bts = SomeBits::new_from_string(2, "0x5555")?;
        let hbts = bts.half_bits();
        let num_bits = hbts.num_one_bits();
        if num_bits != 4 {
            return Err(format!(
                "num bts {} num hbts {} ne 4?",
                bts.num_one_bits(),
                num_bits
            ));
        }
        Ok(())
    }

    // Test SomeBits::num_one_bits
    #[test]
    fn num_one_bits() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5555")?.num_one_bits() != 8 {
            return Err(String::from("Test 1 failed?"));
        }
        if SomeBits::new_from_string(2, "0xaaaa")?.num_one_bits() != 8 {
            return Err(String::from("Test 2 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::push_1
    #[test]
    fn push_1() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5555")?.push_1()
            != SomeBits::new_from_string(2, "0xaaab")?
        {
            return Err(String::from("Test 1 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::shift_left
    #[test]
    fn shift_left() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0xd555")?.shift_left()
            != SomeBits::new_from_string(2, "0xaaaa")?
        {
            return Err(String::from("Test 1 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::shift_left4
    #[test]
    fn shift_left4() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0xf505")?.shift_left4()
            != SomeBits::new_from_string(2, "0x5050")?
        {
            return Err(String::from("Test 1 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::split
    #[test]
    fn split() -> Result<(), String> {
        let avec: Vec<SomeBits> = SomeBits::new_from_string(2, "0x5050")?.split();
        if avec.len() != 4 {
            return Err(String::from("Test 1 failed?"));
        }
        if !avec.contains(&SomeBits::new_from_string(2, "0x4000")?) {
            return Err(String::from("Test 2 failed?"));
        }
        if !avec.contains(&SomeBits::new_from_string(2, "0x1000")?) {
            return Err(String::from("Test 3 failed?"));
        }
        if !avec.contains(&SomeBits::new_from_string(2, "0x0040")?) {
            return Err(String::from("Test 4 failed?"));
        }
        if !avec.contains(&SomeBits::new_from_string(2, "0x0010")?) {
            return Err(String::from("Test 5 failed?"));
        }
        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x0")?.is_adjacent(&SomeBits::new_from_string(2, "0x11")?)
        {
            return Err(String::from("Test 1 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x1")?.is_adjacent(&SomeBits::new_from_string(2, "0x11")?)
        {
            return Err(String::from("Test 2 failed?"));
        }
        if SomeBits::new_from_string(2, "0x0")?
            .is_adjacent(&SomeBits::new_from_string(2, "0x1100")?)
        {
            return Err(String::from("Test 3 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x100")?
            .is_adjacent(&SomeBits::new_from_string(2, "0x1100")?)
        {
            return Err(String::from("Test 4 failed?"));
        }
        Ok(())
    }
}
