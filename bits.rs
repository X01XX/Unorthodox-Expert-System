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
//! test.rs, and change the constants, below, as needed.
//!

/// The iunsigned integer type used in a vector of bits.
type Bitint = u8;

/// The number of bits in an integer used by SomeBits.
const NUM_BITS_PER_INT: usize = u8::BITS as usize;

/// Mask for the highest bit in an integer.
const INT_HIGH_BIT: Bitint = 1 << (NUM_BITS_PER_INT - 1);

/// Mask for the highest nibble in an integer.
const INT_HIGH_NIBBLE: Bitint = 15 << (NUM_BITS_PER_INT - 4);

use crate::randompick::random_x_of_n;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing
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
    /// Like 0xfa1ce. The least significant bit, in this case its equal 0, is bit number zero.
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
        assert!(self.num_ints() == other.num_ints());

        let mut ary2 = Vec::<Bitint>::with_capacity(self.ints.len());

        for (inx, intx) in self.ints.iter().enumerate() {
            ary2.push(intx & other.ints[inx]);
        }
        Self { ints: ary2 }
    }

    /// Return the bitwise OR of two SomeBits structs.
    pub fn b_or(&self, other: &Self) -> Self {
        assert!(self.num_ints() == other.num_ints());

        let mut ary2 = Vec::<Bitint>::with_capacity(self.ints.len());

        for (inx, intx) in self.ints.iter().enumerate() {
            ary2.push(intx | other.ints[inx]);
        }
        Self { ints: ary2 }
    }

    /// Return the bitwise XOR of two SomeBits structs.
    pub fn b_xor(&self, other: &Self) -> Self {
        assert!(self.num_ints() == other.num_ints());

        let mut ary2 = Vec::<Bitint>::with_capacity(self.ints.len());

        for (inx, intx) in self.ints.iter().enumerate() {
            ary2.push(intx ^ other.ints[inx]);
        }
        Self { ints: ary2 }
    }

    /// Return Bits that are the same
    pub fn b_eqv(&self, other: &Self) -> Self {
        assert!(self.num_ints() == other.num_ints());

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
        self.distance(other) == 1
    }

    /// Return true if only one bit is set to one.
    pub fn just_one_bit(&self) -> bool {
        let mut cnt = 0;

        for intx in &self.ints {
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

        // Overflow check
        if carry > 0 {
            panic!("Bits shift_left overflow, carry {carry}");
        }

        Self { ints: ints2 }
    }

    /// Return a copy, shifted left by 1 bit
    /// The Most Significant 4 bit values are lost.
    pub fn shift_left4(&self) -> Self {
        let mut ints2 = vec![0 as Bitint; self.num_ints()];

        let mut carry: Bitint = 0;

        for int_inx in (0..self.ints.len()).rev() {
            let next_carry: Bitint = self.ints[int_inx] >> (NUM_BITS_PER_INT - 4);

            ints2[int_inx] = (self.ints[int_inx] << 4) + carry;
            carry = next_carry;
        }

        // Overflow check
        if carry > 0 {
            panic!("Bits shift_left4 overflow");
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
            let _ = write!(astr, "{intx:08b}"); // increase 08 if the integer size increases

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
        for intx in &self.ints {
            if fil == 1 {
                astr.push(' ');
            }
            let mut cur_bit = INT_HIGH_BIT;
            while cur_bit > 0 {
                if (intx & cur_bit) == 0 {
                    astr.push(' ');
                } else {
                    astr.push('v');
                }
                cur_bit >>= 1;
            }
            fil = 1;
        }
        astr
    }

    /// Return a copy, shifted 1 to the left.
    pub fn push_0(&self) -> Self {
        self.shift_left() // Shift all bits left, LSB bit becomes zero.
    }

    /// Given a mask of more than one bit, return a SomeBits that is a random selection of
    /// roughly half the bits.
    pub fn half_bits(&self) -> Self {
        let one_bits: Vec<SomeBits> = self.split();

        let indicies: Vec<usize> = random_x_of_n(one_bits.len() / 2, one_bits.len());

        let mut or_bts = SomeBits::new_low(self.num_ints());

        for inx in &indicies {
            or_bts = or_bts.b_or(&one_bits[*inx]);
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
    use crate::randompick::RandomPick;

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
        // Init possible hexadecimal digits.
        let chars = "0123456789abcdefABCDEF";

        // Init bit patterns expected for each hexadecimal character.
        let hex_to_bits = vec![
            "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010",
            "1011", "1100", "1101", "1110", "1111", "1010", "1011", "1100", "1101", "1110", "1111",
        ];

        // Check 16 times.
        for _ in 0..16 {
            let mut inxs = RandomPick::new(hex_to_bits.len());

            // Init strings
            let mut bits_hex_str = String::from("0x");
            let mut bits_bin_str = String::from("0b");

            for _ in 0..4 {
                if let Some(inx) = inxs.pick() {
                    // Add to the source string of a bits instance.
                    bits_hex_str.push(chars.as_bytes()[inx] as char);

                    bits_bin_str.push_str(hex_to_bits[inx]);
                }
            }

            // Get new bits values
            let bits_hex = SomeBits::new_from_string(2, &bits_hex_str).unwrap();
            let bits_bin = SomeBits::new_from_string(2, &bits_bin_str).unwrap();

            // Compare the bits string and predicted string.
            match bits_hex == bits_bin {
                true => {
                    println!("bits  {} hex", bits_hex_str);
                    println!("equal {} bin", bits_bin_str);
                }
                _ => {
                    return Err(format!(
                        "bits hex {} instance not equal bin {}!",
                        bits_hex_str, bits_bin_str
                    ))
                }
            }
        }

        Ok(())
    }

    // Test b_eqv
    #[test]
    fn b_eqv() -> Result<(), String> {
        let b1 = SomeBits::new_from_string(2, "0x5555").unwrap();
        let b2 = SomeBits::new_from_string(2, "0xa675").unwrap();
        let b3 = b1.b_eqv(&b2);
        println!("b3 = {}", b3);
        let b4 = SomeBits::new_from_string(2, "0x0cdf").unwrap();
        assert!(b3 == b4);
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

    // Test SomeBits::distance
    #[test]
    fn distance() -> Result<(), String> {
        if 0 != SomeBits::new_from_string(2, "0x0")
            .unwrap()
            .distance(&SomeBits::new_from_string(2, "0x0").unwrap())
        {
            return Err(String::from("Result  1 not 0?"));
        }

        if 8 != SomeBits::new_from_string(2, "0x5555")
            .unwrap()
            .distance(&SomeBits::new_from_string(2, "0x0").unwrap())
        {
            return Err(String::from("Result 2 not 8?"));
        }

        if 7 != SomeBits::new_from_string(2, "0xaaaa")
            .unwrap()
            .distance(&SomeBits::new_from_string(2, "0x02").unwrap())
        {
            return Err(String::from("Result 3 not 7?"));
        }

        Ok(())
    }

    // Test SomeBits::high_bit_nonzero
    #[test]
    fn high_bit_nonzero() -> Result<(), String> {
        let mut btest = SomeBits::new_from_string(1, "0x1").unwrap();

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
        let test_bits = SomeBits::new_from_string(2, "0x5aa5").unwrap();

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
        let mut btest = SomeBits::new_from_string(1, "0x1").unwrap();
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
        if SomeBits::new_from_string(1, "0xa5").unwrap().is_low() {
            return Err(String::from("Test 1 failed?"));
        }

        if !SomeBits::new_low(1).is_low() {
            return Err(String::from("Test 2 failed?"));
        }

        if SomeBits::new_from_string(2, "0x2000").unwrap().is_low() {
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
        if !SomeBits::new_low(2).is_subset_of(&SomeBits::new_from_string(2, "0x0").unwrap()) {
            return Err(String::from("Test 1 failed?"));
        }
        if !SomeBits::new_low(2).is_subset_of(&SomeBits::new_from_string(2, "0x5").unwrap()) {
            return Err(String::from("Test 2 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x5555")
            .unwrap()
            .is_subset_of(&SomeBits::new_from_string(2, "0x7777").unwrap())
        {
            return Err(String::from("Test 3 failed?"));
        }
        if SomeBits::new_from_string(2, "0x5")
            .unwrap()
            .is_subset_of(&SomeBits::new_from_string(2, "0x1").unwrap())
        {
            return Err(String::from("Test 4 failed?"));
        }
        if SomeBits::new_from_string(2, "0x7777")
            .unwrap()
            .is_subset_of(&SomeBits::new_from_string(2, "0x5555").unwrap())
        {
            return Err(String::from("Test 5 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::is_superset_of
    #[test]
    fn is_superset_of() -> Result<(), String> {
        if !SomeBits::new_from_string(2, "0x0")
            .unwrap()
            .is_superset_of(&SomeBits::new_from_string(2, "0x0").unwrap())
        {
            return Err(String::from("Test 1 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x5")
            .unwrap()
            .is_superset_of(&SomeBits::new_from_string(2, "0x0").unwrap())
        {
            return Err(String::from("Test 2 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x7777")
            .unwrap()
            .is_superset_of(&SomeBits::new_from_string(2, "0x5555").unwrap())
        {
            return Err(format!("Test 3 failed?"));
        }
        if SomeBits::new_from_string(2, "0x1")
            .unwrap()
            .is_superset_of(&SomeBits::new_from_string(2, "0x5").unwrap())
        {
            return Err(String::from("Test 4 failed?"));
        }
        if SomeBits::new_from_string(2, "0x5555")
            .unwrap()
            .is_superset_of(&SomeBits::new_from_string(2, "0x7777").unwrap())
        {
            return Err(String::from("Test 5 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::just_one_bit
    #[test]
    fn just_one_bit() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5000")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 1 failed?"));
        }

        if SomeBits::new_from_string(2, "0x0300")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 2 failed?"));
        }

        if SomeBits::new_from_string(2, "0x0060")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 3 failed?"));
        }

        if SomeBits::new_from_string(2, "0x0007")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 4 failed?"));
        }

        if SomeBits::new_from_string(2, "0x0102")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 5 failed?"));
        }

        if SomeBits::new_from_string(2, "0x4080")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 6 failed?"));
        }

        if !SomeBits::new_from_string(2, "0x4000")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 7 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x0800")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 8 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x0010")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 9 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x0002")
            .unwrap()
            .just_one_bit()
        {
            return Err(String::from("Test 10 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::num_one_bits
    #[test]
    fn num_one_bits() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5555")
            .unwrap()
            .num_one_bits()
            != 8
        {
            return Err(String::from("Test 1 failed?"));
        }
        if SomeBits::new_from_string(2, "0xaaaa")
            .unwrap()
            .num_one_bits()
            != 8
        {
            return Err(String::from("Test 2 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::push_1
    #[test]
    fn push_1() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5555").unwrap().push_1()
            != SomeBits::new_from_string(2, "0xaaab").unwrap()
        {
            return Err(String::from("Test 1 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::shift_left
    #[test]
    fn shift_left() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x5555").unwrap().shift_left()
            != SomeBits::new_from_string(2, "0xaaaa").unwrap()
        {
            return Err(String::from("Test 1 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::shift_left4
    #[test]
    fn shift_left4() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x505").unwrap().shift_left4()
            != SomeBits::new_from_string(2, "0x5050").unwrap()
        {
            return Err(String::from("Test 1 failed?"));
        }
        Ok(())
    }

    // Test SomeBits::split
    #[test]
    fn split() -> Result<(), String> {
        let avec: Vec<SomeBits> = SomeBits::new_from_string(2, "0x5050").unwrap().split();
        if avec.len() != 4 {
            return Err(String::from("Test 1 failed?"));
        }
        if !avec.contains(&SomeBits::new_from_string(2, "0x4000").unwrap()) {
            return Err(String::from("Test 2 failed?"));
        }
        if !avec.contains(&SomeBits::new_from_string(2, "0x1000").unwrap()) {
            return Err(String::from("Test 3 failed?"));
        }
        if !avec.contains(&SomeBits::new_from_string(2, "0x0040").unwrap()) {
            return Err(String::from("Test 4 failed?"));
        }
        if !avec.contains(&SomeBits::new_from_string(2, "0x0010").unwrap()) {
            return Err(String::from("Test 5 failed?"));
        }
        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        if SomeBits::new_from_string(2, "0x0")
            .unwrap()
            .is_adjacent(&SomeBits::new_from_string(2, "0x11").unwrap())
        {
            return Err(String::from("Test 1 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x1")
            .unwrap()
            .is_adjacent(&SomeBits::new_from_string(2, "0x11").unwrap())
        {
            return Err(String::from("Test 2 failed?"));
        }
        if SomeBits::new_from_string(2, "0x0")
            .unwrap()
            .is_adjacent(&SomeBits::new_from_string(2, "0x1100").unwrap())
        {
            return Err(String::from("Test 3 failed?"));
        }
        if !SomeBits::new_from_string(2, "0x100")
            .unwrap()
            .is_adjacent(&SomeBits::new_from_string(2, "0x1100").unwrap())
        {
            return Err(String::from("Test 4 failed?"));
        }
        Ok(())
    }
}
