//! The SomeBits struct, storing a bit pattern in one or more unsigned integers.
//!
//! Counting bits starts at the right-most bit of the right-most int,
//! and proceeds to the left, as in standard integer bit-position reckoning.
//!
//! The integer type/size can be increased, change "u8", below.
//!

/// The unsigned integer type used in a vector of bits, for all domains.
type Bitint = u8;

/// The number of bits in an integer used by SomeBits.
const NUM_BITS_PER_INT: usize = u8::BITS as usize;

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
            num_bits: num_bits as u8,
            ints: vec![0 as Bitint; num_ints as usize],
        }
    }

    /// Create a SomeBits instance with integer(s) set to zero.
    pub fn new_low(&self) -> Self {
        SomeBits::new(self.num_bits as usize)
    }

    /// Create a SomeBits instance with integer(s) set to one.
    pub fn new_high(&self) -> Self {
        let adjust = self.num_bits as u32 % Bitint::BITS;

        let mut ints = vec![Bitint::MAX; self.ints.len()];

        if adjust > 0 {
            ints[0] >>= Bitint::BITS - adjust;
        }

        SomeBits {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Return a new bits instance, with a random value.
    pub fn new_random(&self) -> Self {
        let adjust = self.num_bits as u32 % Bitint::BITS;

        let num_ints = (self.num_bits as u32 / Bitint::BITS) + if adjust > 0 { 1 } else { 0 };

        let mut ints = vec![Bitint::MAX; num_ints as usize];

        for intx in ints.iter_mut() {
            *intx = rand::thread_rng().gen_range(0..Bitint::MAX)
        }

        self.b_and(&self.new_high())
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
        // Set base value, if given.
        let mut num_bits = 0;
        let mut base = 2; // default base.

        let mut base_specified = false;

        // Check the first one, or two, characters.
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
                if chr == "_" {
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

        // Calc the number of bits given.
        for (inx, chr) in str.graphemes(true).enumerate() {
            if base_specified && inx < 2 {
                continue;
            }

            if chr == "_" {
                continue;
            }

            if base == 2 {
                num_bits += 1;
            } else {
                num_bits += 4;
            }
        } // next inx, chr

        if num_bits == 0 {
            return Err(format!(
                "SomeBits::new_from_string: String {str}, no valid character?"
            ));
        }

        // Create new bits instance to fill.
        let mut bts = Self::new(num_bits);

        // Get index of the least significant (last) integer.
        let lsb_inx = bts.num_ints() - 1;

        // Translate digits into bits.
        // Check for invalid bits.
        for (inx, chr) in str.graphemes(true).enumerate() {
            if base_specified && inx < 2 {
                continue;
            }

            if chr == "_" {
                continue;
            }

            if base == 2 {
                bts = bts.shift_left();
                if chr == "0" {
                } else if chr == "1" {
                    bts.ints[lsb_inx] += 1;
                } else {
                    return Err(format!(
                        "SomeBits::new_from_string: String {str}, invalid character {chr}?"
                    ));
                }
            } else if let Ok(numx) = Bitint::from_str_radix(chr, 16) {
                bts = bts.shift_left4();

                bts.ints[lsb_inx] += numx;
            } else {
                return Err(format!(
                    "SomeBits::new_from_string: String {str}, invalid character {chr}?"
                ));
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
    fn is_bit_set(&self, bit_num: usize) -> bool {
        assert!(bit_num < self.num_bits as usize);

        let bit_pos = bit_num % NUM_BITS_PER_INT; // Calc bit index within one integer.

        let int_num = self.ints.len() - 1 - (bit_num / NUM_BITS_PER_INT); // Calc integer index in vector.

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
        self == &self.new_high()
    }

    /// Return true if a Bits struct is a ones-subset of another.
    pub fn is_subset_ones_of(&self, other: &Self) -> bool {
        assert_eq!(self.num_bits, other.num_bits);

        *self == self.b_and(other)
    }

    /// Return true if a Bits struct is a ones-superset of another.
    pub fn is_superset_ones_of(&self, other: &Self) -> bool {
        assert_eq!(self.num_bits, other.num_bits);

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
        let mut ints = vec![0 as Bitint; self.ints.len()];

        let mut carry: Bitint = 0;

        for int_inx in (0..self.ints.len()).rev() {
            let next_carry: Bitint = self.ints[int_inx] >> (NUM_BITS_PER_INT - 1);

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
    /// The Most Significant 4 bit value is lost.
    fn shift_left4(&self) -> Self {
        let mut ints = vec![0 as Bitint; self.ints.len()];

        let mut carry: Bitint = 0;

        for int_inx in (0..self.ints.len()).rev() {
            let next_carry: Bitint = self.ints[int_inx] >> (NUM_BITS_PER_INT - 4);

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

    /// Return the number of integers used in the given SomeBits struct.
    fn num_ints(&self) -> usize {
        self.ints.len()
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

    /// Create a formatted string to display under an instance,
    /// to indicate specific bits positions.
    pub fn str2(&self) -> String {
        let mut astr = String::with_capacity(self.strlen());

        // Add a space under r prefix to region.
        astr.push(' ');

        for num_bit in (0..self.num_bits as usize).rev() {
            if self.is_bit_set(num_bit) {
                astr.push('v');
            } else {
                astr.push(' ');
            }
            // Add a space under the "_" separator.
            if num_bit > 0 && (num_bit % 4) == 0 {
                astr.push(' ');
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

    // Changing Bitint will affect this test.
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

        let ur_bts = SomeBits::new(9);
        let high = ur_bts.new_high();
        println!("high 9 {high}");
        assert!(high.ints[0] == 1);
        assert!(high.ints[1] == 255);

        Ok(())
    }

    #[test]
    fn test_strlen() -> Result<(), String> {
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

        Ok(())
    }

    #[test]
    fn new() -> Result<(), String> {
        let bitsx = SomeBits {
            num_bits: 16,
            ints: vec![5 as Bitint, 4 as Bitint],
        };
        println!("bitsx: {bitsx}");
        assert!(bitsx.ints[0] == 5);
        assert!(bitsx.ints[1] == 4);
        Ok(())
    }

    #[test]
    fn test_new_from_string() -> Result<(), String> {
        // Test 0b prefix, and underscore.
        let bits1 = SomeBits::new_from_string("0b10_10")?;
        assert!(bits1.num_bits == 4);

        // Test 0x prefix, and underscore.
        let bits2 = SomeBits::new_from_string("0x5_3")?;
        assert!(bits2.num_bits == 8);

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
                "SomeBits::new_from_string2: bad binary character not detected?"
            ));
        }

        // Test invalid hexadecimal character.
        if let Ok(_bits3) = SomeBits::new_from_string("0x5g") {
            return Err(format!(
                "SomeBits::new_from_string2: bad  hexadecimal character not detected?"
            ));
        }

        // Test no binary characters.
        if let Ok(_bits3) = SomeBits::new_from_string("0b") {
            return Err(format!(
                "SomeBits::new_from_string2: no binary characters not detected?"
            ));
        }

        // Test no characters.
        if let Ok(_bits3) = SomeBits::new_from_string("") {
            return Err(format!(
                "SomeBits::new_from_string2: no characters not detected?"
            ));
        }

        // assert!(1 == 2);
        Ok(())
    }

    // Test b_eqv
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

    // Test SomeBits::b_and
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

    // Test SomeBits::b_not
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

    // Test SomeBits::b_or
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

    // Test SomeBits::b_xor
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

    // Test SomeBits::distance
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

    // Test SomeBits::is_bit_set
    #[test]
    fn is_bit_set() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("0x5aa5")?;
        println!("bitsx: {bitsx}");
        assert!(bitsx.is_bit_set(0));
        assert!(!bitsx.is_bit_set(1));
        assert!(bitsx.is_bit_set(2));
        assert!(!bitsx.is_bit_set(3));
        assert!(!bitsx.is_bit_set(4));
        assert!(bitsx.is_bit_set(5));
        assert!(!bitsx.is_bit_set(6));
        assert!(bitsx.is_bit_set(7));
        assert!(!bitsx.is_bit_set(8));
        assert!(bitsx.is_bit_set(9));
        assert!(!bitsx.is_bit_set(10));
        assert!(bitsx.is_bit_set(11));
        assert!(bitsx.is_bit_set(12));
        assert!(!bitsx.is_bit_set(13));
        assert!(bitsx.is_bit_set(14));
        assert!(!bitsx.is_bit_set(15));

        Ok(())
    }

    // Test SomeBits::is_high
    #[test]
    fn is_high() -> Result<(), String> {
        let mut bitsx = SomeBits::new_from_string("0x00")?;
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_high());

        for _ in 0..bitsx.num_bits {
            bitsx = bitsx.push_1();
        }
        println!("bitsx: {bitsx}");
        assert!(bitsx.is_high());

        let mut bitsx = SomeBits::new_from_string("0x00")?;
        println!("bitsx2: {bitsx}");
        assert!(!bitsx.is_high());

        for _ in 0..bitsx.num_bits {
            bitsx = bitsx.push_1();
        }
        println!("bitsx2: {bitsx}");
        assert!(bitsx.is_high());

        Ok(())
    }

    // Test SomeBits::is_low
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

    // Test SomeBits::is_subset_of
    #[test]
    fn is_subset_of() -> Result<(), String> {
        let bitsx = SomeBits::new(16);
        let bitsy = SomeBits::new_from_string("0x0000")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_subset_ones_of(&bitsy));

        let bitsy = SomeBits::new_from_string("0x0005")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_subset_ones_of(&bitsy));
        assert!(!bitsy.is_subset_ones_of(&bitsx));

        let bitsx = SomeBits::new_from_string("0x5555")?;
        let bitsy = SomeBits::new_from_string("0x7777")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_subset_ones_of(&bitsy));
        assert!(!bitsy.is_subset_ones_of(&bitsx));

        let bitsx = SomeBits::new_from_string("0x5")?;
        let bitsy = SomeBits::new_from_string("0x1")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsy.is_subset_ones_of(&bitsx));
        assert!(!bitsx.is_subset_ones_of(&bitsy));

        Ok(())
    }

    // Test SomeBits::is_superset_of
    #[test]
    fn is_superset_of() -> Result<(), String> {
        let bitsx = SomeBits::new(16);
        let bitsy = SomeBits::new_from_string("0x0000")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_superset_ones_of(&bitsy));

        let bitsx = SomeBits::new_from_string("0x5")?;
        let bitsy = SomeBits::new_from_string("0x0")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_superset_ones_of(&bitsy));
        assert!(!bitsy.is_superset_ones_of(&bitsx));

        let bitsx = SomeBits::new_from_string("0x7777")?;
        let bitsy = SomeBits::new_from_string("0x5555")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_superset_ones_of(&bitsy));
        assert!(!bitsy.is_superset_ones_of(&bitsx));

        let bitsx = SomeBits::new_from_string("0x5")?;
        let bitsy = SomeBits::new_from_string("0x1")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_superset_ones_of(&bitsy));
        assert!(!bitsy.is_superset_ones_of(&bitsx));

        Ok(())
    }

    // Test SomeBits::num_one_bits
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

    // Test SomeBits::push_1
    #[test]
    fn push_1() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("0x5555")?;
        let bitsy = bitsx.push_1();
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsy == SomeBits::new_from_string("0xaaab")?);

        Ok(())
    }

    // Test SomeBits::shift_left
    #[test]
    fn shift_left1() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("10_1010_1010")?;
        let bitsy = bitsx.shift_left();
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsy == SomeBits::new_from_string("01_0101_0100")?);

        Ok(())
    }

    // Test SomeBits::shift_left4
    #[test]
    fn shift_left4() -> Result<(), String> {
        let bitsx = SomeBits::new_from_string("0x3f505")?;
        let bitsy = bitsx.shift_left4();
        println!("bitsx: {bitsx} bitsy: {bitsy}");

        assert!(bitsy == SomeBits::new_from_string("0xf5050")?);

        Ok(())
    }

    // Test SomeBits::split
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
