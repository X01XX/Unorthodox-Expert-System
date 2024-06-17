//! The SomeBits struct, storing a bit pattern in one or more unsigned integers.
//!
//! Counting bits starts at the right-most bit of the right-most int,
//! and proceeds to the left, as in standard integer bit-position reckoning.
//!
//! The integer type/size can be increased, change "u8", below.
//!

/// The unsigned integer type used in a vector of bits, for all domains.
type Bitint = u8;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;
extern crate unicode_segmentation;
use crate::tools::StrLen;
use unicode_segmentation::UnicodeSegmentation;

/// Display trait for SomeBits
impl fmt::Display for SomeBits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Hash, Eq, Clone)]

/// SomeBits struct, just an unsigned integer vector.
/// This structure sets the type of integer.
/// Each Domain sets the number of integers.
/// The lowest significant bit is the right-most bit or the right-most integer.
/// If there was only one domain, this struct may be able to use an array
/// instead of a vector.
pub struct SomeBits {
    /// Number of bits that are active.  So the first integer in the vector
    /// may use one, or more bits.
    pub num_bits: Bitint,
    /// A vector of one, or more, unsigned integers.
    pub ints: Vec<Bitint>,
}

impl SomeBits {
    /// Return a new-low SomeBits instance, given the number of integers.
    pub fn new(num_bits: usize) -> Self {
        assert!(num_bits > 0);

        let num_ints = (num_bits as u32 / Bitint::BITS)
            + if (num_bits as u32 % Bitint::BITS) > 0 {
                1
            } else {
                0
            };

        Self {
            num_bits: num_bits as Bitint,
            ints: vec![0 as Bitint; num_ints as usize],
        }
    }

    /// Create a SomeBits instance with integer(s) set to zero.
    pub fn new_low(&self) -> Self {
        SomeBits::new(self.num_bits as usize)
    }

    /// Create a SomeBits instance with integer(s) set to one.
    pub fn new_high(&self) -> Self {
        let mut ints = vec![Bitint::MAX; self.ints.len()];

        // Shift out highest bits if needed.
        let adjust = self.num_bits as u32 % Bitint::BITS;

        if adjust > 0 {
            ints[0] >>= Bitint::BITS - adjust;
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Return a new bits instance, with a random value.
    pub fn new_random(&self) -> Self {
        let mut ints = vec![0; self.ints.len()];

        for intx in ints.iter_mut() {
            *intx = rand::thread_rng().gen_range(0..Bitint::MAX)
        }

        // Shift out highest bits, if needed.
        let adjust = self.num_bits as u32 % Bitint::BITS;

        if adjust > 0 {
            ints[0] >>= Bitint::BITS - adjust;
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Return a bits instance from a string.
    /// All bits needed must be specified.
    /// Underscore character is ignored.
    /// A prefix, 0b or 0x, may be provided.
    /// Base 2 is assumed, unless a "0x" prefix is given, or a digit 2-F is used.
    ///
    /// if let Ok(bts) = SomeBits::new_from_string("0b0101")) {
    ///    println!("bts {}", &bts);
    /// } else {
    ///    panic!("Invalid bits string");
    /// }
    ///
    /// A prefix of "0x" can be used to specify hexadecimal characters.
    ///
    /// Using multiple integers to represent a SomeBits struct could allow for
    /// a number that is too big for the standard methods of converting a string to an integer.
    pub fn new_from_string(str: &str) -> Result<Self, String> {
        // println!("SomeBits::new_from_string: {str}");
        // Calc base.
        // Binary is assumed, but can be changed to hexadecimal by a prefix or the use of a hexadecimal
        // digit greater than 1.
        let mut base = 2;
        let mut base_specified = false;

        // Check for a prefix.
        if let Some(char0) = str.graphemes(true).nth(0) {
            if char0 == "0" {
                if let Some(char1) = str.graphemes(true).nth(1) {
                    if char1 == "b" || char1 == "B" {
                        base_specified = true;
                    } else if char1 == "x" || char1 == "X" {
                        base_specified = true;
                        base = 16;
                    }
                }
            }
        } else {
            return Err(format!(
                "SomeBits::new_from_string: String {str}, no valid character?"
            ));
        }

        // Check base-unspecified string for a base 16 digit.
        if !base_specified {
            for chr in str.graphemes(true) {
                // Check for visual separator.
                if chr == "_" {
                    continue;
                }

                // Check for integer separator.
                if chr == "+" {
                    // Arbitrary fill end of integer.
                    continue;
                }
                let Ok(digit) = Bitint::from_str_radix(chr, 16) else {
                    return Err(format!(
                        "SomeBits::new_from_string: String {str}, invalid character {chr}?"
                    ));
                };
                if digit > 1 {
                    base = 16;
                    break;
                }
            }
        }

        // Count the number of bits and integers.
        let mut num_ints = 0;
        let mut cur_bits: usize = 0;
        for (inx, chr) in str.graphemes(true).rev().enumerate() {
            if base_specified && inx == (str.len() - 2) && chr == "b"
                || chr == "B"
                || chr == "x"
                || chr == "X"
            {
                break;
            }

            // Check for visual separator.
            if chr == "_" {
                continue;
            }

            // Check for integer separator.
            if chr == "+" {
                num_ints += 1;
                cur_bits = 0;
                continue;
            }

            if base == 2 {
                cur_bits += 1;
            } else {
                cur_bits += 4;
            }

            if cur_bits == Bitint::BITS as usize {
                num_ints += 1;
                cur_bits = 0;
            }
        }
        if cur_bits > 0 {
            num_ints += 1;
        }

        if num_ints == 0 {
            return Err(format!(
                "SomeBits::new_from_string: String {str}, no digits?"
            ));
        }

        let mut ints = Vec::<Bitint>::with_capacity(num_ints);

        // Fill int vec.
        let mut num_bits: usize = 0;
        let mut cur_bits: usize = 0;
        let mut cur_val = 0;

        for (inx, chr) in str.graphemes(true).rev().enumerate() {
            if base_specified && inx == (str.len() - 2) && chr == "b"
                || chr == "B"
                || chr == "x"
                || chr == "X"
            {
                break;
            }

            // Check for visual separator.
            if chr == "_" {
                continue;
            }

            // Check for integer separator.
            if chr == "+" {
                if cur_bits > 0 {
                    ints.push(cur_val);
                    cur_val = 0;
                    cur_bits = 0;
                    num_bits += Bitint::BITS as usize;
                }
                continue;
            }

            // Get character value.
            if let Ok(digit) = Bitint::from_str_radix(chr, base) {
                if base == 2 {
                    cur_val += digit << cur_bits;
                    cur_bits += 1;
                } else {
                    cur_val += digit << cur_bits;
                    cur_bits += 4;
                }

                if cur_bits == Bitint::BITS as usize {
                    ints.push(cur_val);
                    cur_val = 0;
                    cur_bits = 0;
                    num_bits += Bitint::BITS as usize;
                }
            } else {
                return Err(format!(
                    "SomeBits::new_from_string: String {str}, invalid character {chr}?"
                ));
            }
        }
        if cur_bits > 0 {
            ints.push(cur_val);
            num_bits += cur_bits;
        }

        ints.reverse();

        Ok(Self {
            num_bits: num_bits as Bitint,
            ints,
        })
    }

    /// Return a vector of bits where each has only
    /// one 1 bit isolated from the given Bits struct.
    pub fn split(&self) -> Vec<Self> {
        let num_bits = self.num_one_bits();

        let mut rc_vec: Vec<Self> = Vec::with_capacity(num_bits);

        let num_ints = self.ints.len();

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

                let mut btsx = self.new_low(); // new Bits object, all zeros

                btsx.ints[int_inx] = abit; // update one integer

                rc_vec.push(btsx); // Save one-bit result

                tmpint &= tmp2; // remove bit
            }
        } // end for int_inx
        rc_vec
    }

    /// Return true if a bit is one at a given position number.
    /// Match the intuition of a hexadecimal representation.
    /// Like 0xffffe. The least significant bit, in this case its equal 0, is bit number zero.
    /// A minor difficulty is that the most significant integer, in the vector of integers, is index zero.
    pub fn is_bit_set(&self, bit_num: usize) -> bool {
        assert!(bit_num < self.num_bits as usize);

        let bit_pos = bit_num % Bitint::BITS as usize; // Calc bit index within one integer.

        let int_num = self.ints.len() - 1 - (bit_num / Bitint::BITS as usize); // Calc integer index in vector.

        self.ints[int_num] & (1 << bit_pos) > 0
    }

    /// Return the bitwise NOT of a SomeBits stuct.
    pub fn b_not(&self) -> Self {
        self.b_xor(&self.new_high())
    }

    /// Return the bitwise AND of two SomeBits structs.
    pub fn b_and(&self, other: &Self) -> Self {
        assert_eq!(self.num_bits, other.num_bits);

        let mut ints = Vec::<Bitint>::with_capacity(self.ints.len());

        for (x, y) in self.ints.iter().zip(other.ints.iter()) {
            ints.push(x & y);
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Return the bitwise AND-NOT of two SomeBits structs.
    /// Assuming the self instance is valid, taking the invert of
    /// the other will not accidentally introduce one bits outside of
    /// the allowed number of bits.
    pub fn b_and_not(&self, other: &Self) -> Self {
        assert_eq!(self.num_bits, other.num_bits);

        let mut ints = Vec::<Bitint>::with_capacity(self.ints.len());

        for (x, y) in self.ints.iter().zip(other.ints.iter()) {
            ints.push(x & !y);
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Return the bitwise OR of two SomeBits structs.
    pub fn b_or(&self, other: &Self) -> Self {
        assert_eq!(self.num_bits, other.num_bits);

        let mut ints = Vec::<Bitint>::with_capacity(self.ints.len());

        for (x, y) in self.ints.iter().zip(other.ints.iter()) {
            ints.push(x | y);
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Return the bitwise XOR of two SomeBits structs.
    pub fn b_xor(&self, other: &Self) -> Self {
        assert_eq!(self.num_bits, other.num_bits);

        let mut ints = Vec::<Bitint>::with_capacity(self.ints.len());

        for (x, y) in self.ints.iter().zip(other.ints.iter()) {
            ints.push(x ^ y);
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Return Bits that are the same
    pub fn b_eqv(&self, other: &Self) -> Self {
        assert_eq!(self.num_bits, other.num_bits);

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
        // Check non Most Significant Integers.
        for inx in self.ints.iter().skip(1) {
            if *inx != Bitint::MAX {
                return false;
            }
        }

        // Calc highest int, masking out bits if needed.
        let mut int0 = Bitint::MAX;

        let adjust = self.num_bits as u32 % Bitint::BITS;

        if adjust > 0 {
            int0 >>= Bitint::BITS - adjust;
        }

        self.ints[0] == int0
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
    pub fn distance(&self, other: &Self) -> usize {
        assert_eq!(self.num_bits, other.num_bits);

        self.b_xor(other).num_one_bits()
    }

    /// Return true if two bits instances are adjacent.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        assert_eq!(self.num_bits, other.num_bits);

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

    /// Return a copy, shifted left by 1 bit
    /// The Most Significant Bit value shifted out.
    fn _shift_left(&self) -> Self {
        let mut ints = vec![0 as Bitint; self.ints.len()];

        let mut carry: Bitint = 0;

        for int_inx in (0..self.ints.len()).rev() {
            let next_carry: Bitint = self.ints[int_inx] >> (Bitint::BITS - 1);

            ints[int_inx] = (self.ints[int_inx] << 1) + carry;

            carry = next_carry;
        }

        let adjust = self.num_bits as u32 % Bitint::BITS;
        if adjust > 0 {
            ints[0] &= Bitint::MAX >> (Bitint::BITS - adjust);
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Return a copy, shifted left by 4 bits.
    /// The Most Significant 4 bit value is shifted out.
    fn _shift_left4(&self) -> Self {
        let mut ints = vec![0 as Bitint; self.ints.len()];

        let mut carry: Bitint = 0;

        for int_inx in (0..self.ints.len()).rev() {
            let next_carry: Bitint = self.ints[int_inx] >> (Bitint::BITS - 4);

            ints[int_inx] = (self.ints[int_inx] << 4) + carry;
            carry = next_carry;
        }

        let adjust = self.num_bits as u32 % Bitint::BITS;
        if adjust > 0 {
            ints[0] &= Bitint::MAX >> (Bitint::BITS - adjust);
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Create a formatted string for an instance.
    fn formatted_string(&self) -> String {
        let mut astr = String::with_capacity(self.strlen());

        for bit_num in (0..self.num_bits as usize).rev() {
            if self.is_bit_set(bit_num) {
                astr.push('1');
            } else {
                astr.push('0');
            }
            if bit_num != 0 && bit_num % 4 == 0 {
                astr.push('_');
            }
        }
        astr
    }
} // end impl SomeBits

/// Define the BitsRef trait, so SomeBits, SomeMask, and SomeState structs can interact at the SomeBits level.
/// The structs are often used for different things, but may interact,
/// like using a mask to change a state.
pub trait BitsRef {
    fn bitsref(&self) -> &SomeBits;
}

/// Implement the trait BitsRef for SomeBits.
impl BitsRef for SomeBits {
    fn bitsref(&self) -> &SomeBits {
        self
    }
}

/// Implement the trait StrLen for string representations of SomeBits.
impl StrLen for SomeBits {
    fn strlen(&self) -> usize {
        let num_bits = self.num_bits as usize;
        let num_4 = num_bits / 4;
        let extra = num_bits % 4;
        let sep_len = num_4 - if extra > 0 { 0 } else { 1 };
        num_bits + sep_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools;

    #[test]
    fn new_high() -> Result<(), String> {
        let ur_bts = SomeBits::new(7);
        let high = ur_bts.new_high();
        println!("high 7 {high}");
        assert!(high.ints[0] == 127);

        let ur_bts = SomeBits::new(8);
        let high = ur_bts.new_high();
        println!("high 8 {high}");
        assert!(high.ints[0] == 255);

        // Test new_high with multi-int bits, given that the definition of Bitint may change.
        let ur_bits = SomeBits::new_from_string("00+0")?;
        let high = ur_bits.new_high();
        println!("high two ints {high}");
        assert!(high.ints[0] == 3);
        assert!(high.ints[1] == Bitint::MAX);

        Ok(())
    }

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_bts = SomeBits::new(8);
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::new(16);
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::new(6);
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::new(5);
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::new(4);
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::new(3);
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        // Test strlen with multi-int bits, given that the definition of Bitint may change.
        let tmp_bts = SomeBits::new_from_string("00+0")?;
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn new() -> Result<(), String> {
        let bitsx = SomeBits::new(7);
        assert!(bitsx.num_bits == 7);
        assert!(bitsx.ints.len() == 1);
        assert!(bitsx.is_low());

        let bitsx = SomeBits::new(Bitint::BITS as usize);
        assert!(bitsx.ints.len() == 1);

        // Test new with multi-int bits, given that the definition of Bitint may change.
        let bitsx = SomeBits::new(Bitint::BITS as usize + 1);
        assert!(bitsx.ints.len() == 2);

        let bitsx = SomeBits::new((2 * Bitint::BITS) as usize + 1);
        assert!(bitsx.ints.len() == 3);

        Ok(())
    }

    #[test]
    fn new_from_string() -> Result<(), String> {
        // Test 0b prefix, underscore, and multi-int bits.
        let bits1 = SomeBits::new_from_string("0b10+1001_1000")?;
        println!("bits1 {bits1}");
        assert!(bits1.num_bits == Bitint::BITS as Bitint + 2 as Bitint);
        assert!(bits1.ints.len() == 2);

        // Test 0x prefix, underscore, and multi-int bits.
        let bits2 = SomeBits::new_from_string("0x5+33")?;
        assert!(bits2.num_bits == 12);
        assert!(bits1.ints.len() == 2);

        // Test no base, hexadecimal digits.
        if let Ok(bits3) = SomeBits::new_from_string("1102") {
            if bits3.num_bits != 16 {
                return Err(format!(
                    "SomeBits::new_from_string: Did not translate 1102?"
                ));
            }
        } else {
            return Err(format!(
                "SomeBits::new_from_string: Did not translate 1102?"
            ));
        }

        // Test no base, binary digits.
        if let Ok(bits3) = SomeBits::new_from_string("1101") {
            if bits3.num_bits != 4 {
                return Err(format!(
                    "SomeBits::new_from_string: Did not translate 1101?"
                ));
            }
        } else {
            return Err(format!(
                "SomeBits::new_from_string: Did not translate 1101?"
            ));
        }

        // Test invalid binary character.
        if let Ok(_bits3) = SomeBits::new_from_string("0b10102") {
            return Err(format!(
                "SomeBits::new_from_string: bad binary character not detected?"
            ));
        }

        // Test invalid hexadecimal character.
        if let Ok(_bits3) = SomeBits::new_from_string("0x5g") {
            return Err(format!(
                "SomeBits::new_from_string: bad  hexadecimal character not detected?"
            ));
        }

        // Test no binary characters.
        if let Ok(bits3) = SomeBits::new_from_string("0b") {
            return Err(format!(
                "SomeBits::new_from_string: binary characters detected? {bits3}"
            ));
        }

        // Test no characters.
        if let Ok(bits3) = SomeBits::new_from_string("") {
            return Err(format!(
                "SomeBits::new_from_string: characters detected? {bits3}"
            ));
        }

        // assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn b_eqv() -> Result<(), String> {
        // let tmp_bts = SomeBits::new(16);
        let b1 = SomeBits::new_from_string("0x5555")?;
        let b2 = SomeBits::new_from_string("0xa675")?;
        let b3 = b1.b_eqv(&b2).b_and(&SomeBits::new_from_string("0x0fff")?);
        let b4 = SomeBits::new_from_string("0x0cdf")?;
        println!("b3: {b3} b4: {b4}");
        assert_eq!(b3, b4);
        Ok(())
    }

    #[test]
    fn b_and() -> Result<(), String> {
        // 00
        let tmp_bts = SomeBits::new(16);
        let mut bitsx = tmp_bts.new_low();
        let mut bitsy = tmp_bts.new_low();
        let mut bitsz = bitsx.b_and(&bitsy);
        println!("bitsz1: {bitsz}");
        assert!(bitsz.is_low());

        // 01
        bitsx = tmp_bts.new_low();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_and(&bitsy);
        assert!(bitsz.is_low());

        // 11
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_and(&bitsy);
        println!("bitsz3: {bitsz}");
        assert!(bitsz.is_high());

        // 10
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_low();
        bitsz = bitsx.b_and(&bitsy);
        println!("bitsz4: {bitsz}");
        assert!(bitsz.is_low());

        Ok(())
    }

    #[test]
    fn b_not() -> Result<(), String> {
        let tmp_bts = SomeBits::new(16);
        let mut bitsx = tmp_bts.new_low();
        let mut bitsz = bitsx.b_not();
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_high());

        bitsx = tmp_bts.new_high();
        bitsz = bitsx.b_not();
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_low());

        Ok(())
    }

    #[test]
    fn b_or() -> Result<(), String> {
        // 00
        let tmp_bts = SomeBits::new(16);
        let mut bitsx = tmp_bts.new_low();
        let mut bitsy = tmp_bts.new_low();
        let mut bitsz = bitsx.b_or(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_low());

        // 01
        bitsx = tmp_bts.new_low();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_or(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_high());

        // 11
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_or(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_high());

        // 10
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_low();
        bitsz = bitsx.b_or(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_high());

        Ok(())
    }

    #[test]
    fn b_xor() -> Result<(), String> {
        let tmp_bts = SomeBits::new(16);
        // 00
        let mut bitsx = tmp_bts.new_low();
        let mut bitsy = tmp_bts.new_low();
        let mut bitsz = bitsx.b_xor(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_low());

        // 01
        bitsx = tmp_bts.new_low();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_xor(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_high());

        // 11
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_xor(&bitsy);
        println!("bits: {bitsz}");
        assert!(bitsz.is_low());

        // 10
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_low();
        bitsz = bitsx.b_xor(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_high());

        Ok(())
    }

    #[test]
    fn b_and_not() -> Result<(), String> {
        let tmp_bts = SomeBits::new(16);
        // 00
        let mut bitsx = tmp_bts.new_low();
        let mut bitsy = tmp_bts.new_low();
        let mut bitsz = bitsx.b_and_not(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_low());

        // 01
        bitsx = tmp_bts.new_low();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_and_not(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_low());

        // 11
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_and_not(&bitsy);
        println!("bits: {bitsz}");
        assert!(bitsz.is_low());

        // 10
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_low();
        bitsz = bitsx.b_and_not(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_high());

        Ok(())
    }

    #[test]
    fn distance() -> Result<(), String> {
        let dist = SomeBits::new_from_string("0x0")?.distance(&SomeBits::new_from_string("0x0")?);
        println!("dist: {dist}");
        assert!(dist == 0);

        let dist =
            SomeBits::new_from_string("0x5555")?.distance(&SomeBits::new_from_string("0x0000")?);
        println!("dist: {dist}");
        assert!(dist == 8);

        let dist =
            SomeBits::new_from_string("0xaaaa")?.distance(&SomeBits::new_from_string("0x0002")?);
        println!("dist: {dist}");
        assert!(dist == 7);

        Ok(())
    }

    #[test]
    fn is_bit_set() -> Result<(), String> {
        let bitsx = SomeBits {
            num_bits: Bitint::BITS as Bitint * 2,
            ints: vec![10, 10],
        };

        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_bit_set(0));
        assert!(bitsx.is_bit_set(1));
        assert!(!bitsx.is_bit_set(2));
        assert!(bitsx.is_bit_set(3));

        assert!(!bitsx.is_bit_set(Bitint::BITS as usize));
        assert!(bitsx.is_bit_set(Bitint::BITS as usize + 1));
        assert!(!bitsx.is_bit_set(Bitint::BITS as usize + 2));
        assert!(bitsx.is_bit_set(Bitint::BITS as usize + 3));

        Ok(())
    }

    #[test]
    fn is_high() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("0b0_1111_1111")?;
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_high());

        let bitsx = SomeBits::new_from_string("0b1_1111_1111")?;

        println!("bitsx2: {bitsx}");
        assert!(bitsx.is_high());

        Ok(())
    }

    #[test]
    fn is_low() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("0xa500")?;
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_low());

        let bitsx = bitsx.new_low();
        println!("bitsx: {bitsx}");
        assert!(bitsx.is_low());

        let bitsx = SomeBits::new_from_string("0x2000")?;
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_low());

        let bitsx = bitsx.new_low();
        println!("bitsx: {bitsx}");
        assert!(bitsx.is_low());

        Ok(())
    }

    #[test]
    fn num_one_bits() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("0x5555")?;
        println!("bitsx: {bitsx}");
        assert!(bitsx.num_one_bits() == 8);

        let bitsx = SomeBits::new_from_string("0xaaa1")?;
        println!("bitsx: {bitsx}");
        assert!(bitsx.num_one_bits() == 7);

        Ok(())
    }

    #[test]
    fn shift_left() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("10_1010_1010")?;
        let bitsy = bitsx._shift_left();
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsy == SomeBits::new_from_string("01_0101_0100")?);

        Ok(())
    }

    #[test]
    fn shift_left4() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("0x3f505")?;
        let bitsy = bitsx._shift_left4();
        println!("bitsx: {bitsx} bitsy: {bitsy}");

        assert!(bitsy == SomeBits::new_from_string("0xf5050")?);

        Ok(())
    }

    #[test]
    fn split() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("0x5050")?;
        println!("bitsx: {bitsx}");

        let avec: Vec<SomeBits> = bitsx.split();
        println!("split bits: {}", tools::vec_string(&avec));

        assert!(avec.len() == 4);
        assert!(avec.contains(&SomeBits::new_from_string("0x4000")?));
        assert!(avec.contains(&SomeBits::new_from_string("0x1000")?));
        assert!(avec.contains(&SomeBits::new_from_string("0x0040")?));
        assert!(avec.contains(&SomeBits::new_from_string("0x0010")?));

        let bitsx = SomeBits::new_from_string("0x010")?;
        println!("bitsx: {bitsx}");

        let avec: Vec<SomeBits> = bitsx.split();
        println!("split bits: {}", tools::vec_string(&avec));

        assert!(avec.len() == 1);

        let bitsx = SomeBits::new_from_string("0x00")?;
        println!("bitsx: {bitsx}");

        let avec: Vec<SomeBits> = bitsx.split();
        println!("split bits: {}", tools::vec_string(&avec));

        assert!(avec.is_empty());

        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let bitsx = SomeBits::new(16);
        let bitsy = SomeBits::new_from_string("0x0011")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(!bitsx.is_adjacent(&bitsy));

        let bitsx = SomeBits::new_from_string("0x01")?;
        let bitsy = SomeBits::new_from_string("0x11")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_adjacent(&bitsy));

        let bitsx = SomeBits::new_from_string("0x0000")?;
        let bitsy = SomeBits::new_from_string("0x1100")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(!bitsx.is_adjacent(&bitsy));

        let bitsx = SomeBits::new_from_string("0x0100")?;
        let bitsy = SomeBits::new_from_string("0x1100")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_adjacent(&bitsy));

        Ok(())
    }
}
