//! The SomeBits struct, storing a bit pattern in one or more unsigned integers.
//!
//! Counting bits starts at the right-most bit of the right-most int,
//! and proceeds to the left, as in standard integer bit-position reckoning.
//!
//! The integer type/size can be increased, change "u8", below.
//!

/// The unsigned integer type used in a vector of bits, for all domains.
type Bitint = u8;

use crate::tools::StrLen;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;
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
    /// Return a new-low SomeBits instance, given the number of bits.
    pub fn new(num_bits: Bitint) -> Self {
        assert!(num_bits > 0);

        let num_ints = Self::number_bits_to_ints(num_bits);
        Self {
            num_bits: num_bits as Bitint,
            ints: vec![0 as Bitint; num_ints as usize],
        }
    }

    /// Combine two bit instances, where the num_bits sum of both is LE one BitInt.
    pub fn combine(&self, other: &SomeBits) -> Self {
        assert!(self.num_bits + other.num_bits < Bitint::BITS as Bitint);

        Self {
            num_bits: (self.num_bits + other.num_bits) as Bitint,
            ints: vec![(self.ints[0] << other.num_bits) + other.ints[0]; 1],
        }
    }

    /// Return the number of integers needed to represent a number of bits.
    pub fn number_bits_to_ints(num_bits: Bitint) -> Bitint {
        ((num_bits / Bitint::BITS as Bitint)
            + if (num_bits % Bitint::BITS as Bitint) > 0 {
                1
            } else {
                0
            }) as Bitint
    }

    /// Create a SomeBits instance, from another, to conserve the number bits, with all bits set to zero.
    pub fn new_low(&self) -> Self {
        SomeBits::new(self.num_bits)
    }

    /// Create a SomeBits instance, from another, to conserve the number bits, with all bits set to one.
    pub fn new_high(&self) -> Self {
        let mut ints = vec![Bitint::MAX; self.ints.len()];

        // Shift out highest bits if needed.
        let adjust = self.num_bits % Bitint::BITS as Bitint;

        if adjust > 0 {
            ints[0] >>= Bitint::BITS as Bitint - adjust;
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }

    /// Return a new bits instance, from another, to conserve the number bits, with a random value.
    pub fn new_random(num_bits: Bitint) -> Self {
        assert!(num_bits > 0);

        let num_ints = Self::number_bits_to_ints(num_bits);

        let mut ints = vec![0; num_ints as usize];

        for intx in ints.iter_mut() {
            *intx = rand::thread_rng().gen_range(0..Bitint::MAX)
        }

        // Shift out highest bits, if needed.
        let adjust = num_bits % Bitint::BITS as Bitint;

        if adjust > 0 {
            ints[0] >>= Bitint::BITS as Bitint - adjust;
        }

        Self { num_bits, ints }
    }

    /// Return a bits instance from a string.
    /// All bits needed must be specified.
    /// Underscore character is ignored.
    /// A prefix, 0b or 0x, may be provided.
    /// Base 2 is assumed, unless a "0x" prefix is given, or a hexadecimal digit, 2..F, is used.
    ///
    /// if let Ok(bts) = SomeBits::from("0b0101")) {
    ///    println!("bts {}", &bts);
    /// } else {
    ///    panic!("Invalid bits string");
    /// }
    ///
    /// A prefix of "0x" can be used to specify hexadecimal characters.
    ///
    /// Using multiple integers to represent a SomeBits struct could allow for
    /// a number that is too big for the standard methods of converting a string to an integer.
    ///
    /// A "_" character can be used as a visual separator, and is ignored.
    pub fn from(str: &str) -> Result<Self, String> {
        // println!("SomeBits::from: {str}");
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
            return Err("SomeBits::from: Empty string?".to_string());
        }

        // Check base-unspecified string for a base 16 digit.
        if !base_specified {
            for chr in str.graphemes(true) {
                // Check for visual separator.
                if chr == "_" {
                    continue;
                }

                let Ok(digit) = Bitint::from_str_radix(chr, 16) else {
                    return Err(format!(
                        "SomeBits::from: String {str}, invalid digit {chr}?"
                    ));
                };
                if digit > 1 {
                    base = 16;
                    break;
                }
            }
        }

        // Count the number of bits and integers.
        let mut num_bits: Bitint = 0;
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

            if base == 2 {
                num_bits += 1;
            } else {
                num_bits += 4;
            }
        }

        if num_bits == 0 {
            return Err(format!("SomeBits::from: String {str}, no valid digits?"));
        }

        let mut ints = Vec::<Bitint>::with_capacity(Self::number_bits_to_ints(num_bits) as usize);

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

            // Get character value.
            if base == 2 {
                if let Ok(digit) = Bitint::from_str_radix(chr, 2) {
                    cur_val += digit << cur_bits;
                    cur_bits += 1;
                } else {
                    return Err(format!(
                        "SomeBits::from: String {str}, invalid binary digit {chr}?"
                    ));
                }
            } else if let Ok(digit) = Bitint::from_str_radix(chr, 16) {
                cur_val += digit << cur_bits;
                cur_bits += 4;
            } else {
                return Err(format!(
                    "SomeBits::from: String {str}, invalid hexadecimal digit {chr}?"
                ));
            }

            if cur_bits == Bitint::BITS as usize {
                ints.push(cur_val);
                cur_val = 0;
                cur_bits = 0;
                num_bits += Bitint::BITS as usize;
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
    /// The least significant vector index, 0, is the most significant part of the number.
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
        debug_assert_eq!(self.num_bits, other.num_bits);

        Self {
            num_bits: self.num_bits,
            ints: self
                .ints
                .iter()
                .zip(other.ints.iter())
                .map(|(x, y)| x & y)
                .collect::<Vec<Bitint>>(),
        }
    }

    /// Return the bitwise AND-NOT of two SomeBits structs.
    /// Assuming the self instance is valid, taking the invert of
    /// the other will not accidentally introduce one bits outside of
    /// the allowed number of bits.
    pub fn b_and_not(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits, other.num_bits);

        Self {
            num_bits: self.num_bits,
            ints: self
                .ints
                .iter()
                .zip(other.ints.iter())
                .map(|(x, y)| x & !y)
                .collect::<Vec<Bitint>>(),
        }
    }

    /// Return the bitwise OR of two SomeBits structs.
    pub fn b_or(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits, other.num_bits);

        Self {
            num_bits: self.num_bits,
            ints: self
                .ints
                .iter()
                .zip(other.ints.iter())
                .map(|(x, y)| x | y)
                .collect::<Vec<Bitint>>(),
        }
    }

    /// Return the bitwise XOR of two SomeBits structs.
    pub fn b_xor(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits, other.num_bits);

        Self {
            num_bits: self.num_bits,
            ints: self
                .ints
                .iter()
                .zip(other.ints.iter())
                .map(|(x, y)| x ^ y)
                .collect::<Vec<Bitint>>(),
        }
    }

    /// Return Bits that are the same
    pub fn b_eqv(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits, other.num_bits);

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

        let adjust = self.num_bits % Bitint::BITS as Bitint;

        if adjust > 0 {
            int0 >>= Bitint::BITS as Bitint - adjust;
        }

        self.ints[0] == int0
    }

    /// Return the number of bits set to one.
    pub fn num_one_bits(&self) -> usize {
        self.ints.iter().map(|x| x.count_ones()).sum::<u32>() as usize
    }

    /// Return the number of bits that are different.
    /// This can be interpreted as how "far away" two bit patterns are.
    pub fn distance(&self, other: &Self) -> usize {
        debug_assert_eq!(self.num_bits, other.num_bits);

        self.b_xor(other).num_one_bits()
    }

    /// Return true if two bits instances are adjacent.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        debug_assert_eq!(self.num_bits, other.num_bits);

        self.b_xor(other).num_one_bits() == 1
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

    /// Return true if a bits instance is between, exclusive, two others.
    pub fn is_between(&self, bts1: &SomeBits, bts2: &SomeBits) -> bool {
        debug_assert_eq!(self.num_bits, bts1.num_bits);
        debug_assert_eq!(self.num_bits, bts2.num_bits);

        if self == bts1 {
            return false;
        }
        if self == bts2 {
            return false;
        }

        self.b_xor(bts1).b_and(&self.b_xor(bts2)).is_low()
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

/// Define the NumBits trait, so different structs can return the number bits used by their substructs.
pub trait NumBits {
    fn num_bits(&self) -> usize;
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

/// Return true if the items in a vector all use the same number bits.
pub fn vec_same_num_bits<T: NumBits>(avec: &[T]) -> bool {
    if avec.len() < 2 {
        return true;
    }
    let first_bits = avec[0].num_bits();
    for itmx in avec.iter().skip(1) {
        if itmx.num_bits() != first_bits {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools;

    /// Build a new bits instance, given number bits and integer vector.
    /// The length of the vector must be the minimum needed to cover the given number of bits.
    ///
    /// Used to force the creation of multi-integer SomeBits instances, given that BitInt may be u8, u16, u32, u64.
    fn build_from_vec(num_bits: Bitint, ints: Vec<Bitint>) -> SomeBits {
        // Verify length of integer vector.
        let num_ints = SomeBits::number_bits_to_ints(num_bits);
        assert!(num_ints == ints.len() as Bitint);

        // Check that there is not too large a number of bits in the first integer.
        let bits_left = num_bits % Bitint::BITS as Bitint;

        if bits_left > 0 {
            let int0_high = ((2_u32.pow(bits_left as u32)) - 1) as Bitint;
            assert!(ints[0] <= int0_high);
        }

        SomeBits { num_bits, ints }
    }

    #[test]
    fn eq() -> Result<(), String> {
        let bits1 = SomeBits::from("0x5")?;
        println!("bits1 {bits1}");
        let bits2 = SomeBits::from("0b0101")?;
        println!("bits2 {bits2}");
        assert!(bits1 == bits2);

        let bits1 = build_from_vec(Bitint::BITS as Bitint * 2, vec![5, 1]);
        println!("bits1 {bits1}");

        let bits2 = build_from_vec(Bitint::BITS as Bitint * 2, vec![5, 1]);
        println!("bits2 {bits2}");

        assert!(bits1 == bits2);
        assert!(bits1.num_bits == Bitint::BITS as Bitint * 2);

        let bits1 = build_from_vec(Bitint::BITS as Bitint + 3, vec![5, 1]);
        println!("bits1 {bits1}");

        let bits2 = build_from_vec(Bitint::BITS as Bitint + 3, vec![4, 1]);
        println!("bits2 {bits2}");

        assert!(bits1 != bits2);
        assert!(bits1.num_bits == Bitint::BITS as Bitint + 3);

        Ok(())
    }

    #[test]
    fn new_high() -> Result<(), String> {
        let ur_bts = SomeBits::from("0b000_0000")?;
        let high = ur_bts.new_high();
        println!("high 7 {high}");
        assert!(high.ints[0] == 127);

        let ur_bts = SomeBits::from("0x00")?;
        let high = ur_bts.new_high();
        println!("high 8 {high}");
        assert!(high.ints[0] == 255);

        // Test new_high with multi-int bits, given that the definition of Bitint may change.
        let ur_bits = build_from_vec(Bitint::BITS as Bitint + 2, vec![0, 0]);

        let high = ur_bits.new_high();
        println!("high two ints {high}");
        assert!(high.ints[0] == 3);
        assert!(high.ints[1] == Bitint::MAX);

        Ok(())
    }

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_bts = SomeBits::from("0x00")?;
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::from("0x0000")?;
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::from("0b00_0000")?;
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::from("0b0_0000")?;
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::from("0x0")?;
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::from("0b000")?;
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        // Test strlen with multi-int bits, given that the definition of Bitint may change.
        let tmp_bts = build_from_vec(Bitint::BITS as Bitint + 2, vec![0, 0]);
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn new() -> Result<(), String> {
        let bitsx = SomeBits::from("0b000_0000")?;
        assert!(bitsx.num_bits == 7);
        assert!(bitsx.ints.len() == 1);
        assert!(bitsx.is_low());

        let bitsx = SomeBits::new(Bitint::BITS as Bitint);
        assert!(bitsx.ints.len() == 1);

        // Test new with multi-int bits, given that the definition of Bitint may change.
        let bitsx = SomeBits::new(Bitint::BITS as Bitint + 1);
        assert!(bitsx.ints.len() == 2);

        let bitsx = SomeBits::new((2 * Bitint::BITS) as Bitint + 1);
        assert!(bitsx.ints.len() == 3);

        Ok(())
    }

    #[test]
    fn from() -> Result<(), String> {
        // Test empty string.
        match SomeBits::from("") {
            Ok(regx) => {
                return Err(format!("SomeBits::from: Returned region {regx}?"));
            }
            Err(error) => {
                if error == "SomeBits::from: Empty string?" {
                    println!("{error}");
                } else {
                    return Err(error);
                }
            }
        }

        // Test invalid digit.
        match SomeBits::from("01z") {
            Ok(regx) => {
                return Err(format!("SomeBits::from: Returned region {regx}?"));
            }
            Err(error) => {
                if error == "SomeBits::from: String 01z, invalid digit z?" {
                    println!("{error}");
                } else {
                    return Err(error);
                }
            }
        }

        // Test no valid digits.
        match SomeBits::from("__") {
            Ok(regx) => {
                return Err(format!("SomeBits::from: Returned region {regx}?"));
            }
            Err(error) => {
                if error == "SomeBits::from: String __, no valid digits?" {
                    println!("{error}");
                } else {
                    return Err(error);
                }
            }
        }

        // Test invalid binary digit.
        match SomeBits::from("0b012") {
            Ok(regx) => {
                return Err(format!("SomeBits::from: Returned region {regx}?"));
            }
            Err(error) => {
                if error == "SomeBits::from: String 0b012, invalid binary digit 2?" {
                    println!("{error}");
                } else {
                    return Err(error);
                }
            }
        }

        // Test invalid hexadecimal digit.
        match SomeBits::from("0x012g") {
            Ok(regx) => {
                return Err(format!("SomeBits::from: Returned region {regx}?"));
            }
            Err(error) => {
                if error == "SomeBits::from: String 0x012g, invalid hexadecimal digit g?" {
                    println!("{error}");
                } else {
                    return Err(error);
                }
            }
        }

        // Test no base, hexadecimal digits.
        match SomeBits::from("1102") {
            Ok(regx) => {
                assert!(regx.num_bits == 16);
                assert!(regx == SomeBits::from("0x1102")?);
            }
            Err(error) => {
                return Err(error);
            }
        }

        // Test no base, binary digits.
        match SomeBits::from("1101") {
            Ok(regx) => {
                assert!(regx.num_bits == 4);
                assert!(regx == SomeBits::from("0xd")?);
            }
            Err(error) => {
                return Err(error);
            }
        }

        // Test hexadecimal base.
        match SomeBits::from("0x1102") {
            Ok(regx) => {
                assert!(regx.num_bits == 16);
                assert!(regx == SomeBits::from("0b0001_0001_0000_0010")?);
            }
            Err(error) => {
                return Err(error);
            }
        }

        // Test binary base.
        match SomeBits::from("0b1101") {
            Ok(regx) => {
                assert!(regx.num_bits == 4);
                assert!(regx == SomeBits::from("0xd")?);
            }
            Err(error) => {
                return Err(error);
            }
        }

        Ok(())
    }

    #[test]
    fn b_eqv() -> Result<(), String> {
        let b1 = build_from_vec(Bitint::BITS as Bitint + 8, vec![0x55, 5]);
        println!("b1: {b1}");
        let b2 = build_from_vec(Bitint::BITS as Bitint + 8, vec![0xa6, 5]);
        println!("b2: {b2}");
        let b3 = b1.b_eqv(&b2).b_and(&build_from_vec(
            Bitint::BITS as Bitint + 8,
            vec![0x0f, 0xff],
        ));
        println!("b3: {b3}");
        let b4 = build_from_vec(Bitint::BITS as Bitint + 8, vec![0x0c, 0xff]);
        println!("b4: {b4}");

        assert_eq!(b3, b4);

        Ok(())
    }

    #[test]
    fn b_and() -> Result<(), String> {
        // 00
        let tmp_bts = SomeBits::new(Bitint::BITS as Bitint + 2);
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
        let tmp_bts = SomeBits::new(Bitint::BITS as Bitint + 2);
        let mut bitsx = tmp_bts.new_low();
        let mut bitsz = bitsx.b_not();
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_high());
        assert!(
            bitsz == build_from_vec(Bitint::BITS as Bitint + 2, vec![0x3, Bitint::MAX as Bitint])
        );

        bitsx = tmp_bts.new_high();
        bitsz = bitsx.b_not();
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_low());

        Ok(())
    }

    #[test]
    fn b_or() -> Result<(), String> {
        // 00
        let tmp_bts = SomeBits::new(Bitint::BITS as Bitint + 2);
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
        let tmp_bts = SomeBits::new(Bitint::BITS as Bitint + 2);
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
        let tmp_bts = SomeBits::new(Bitint::BITS as Bitint + 2);
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
        let dist = SomeBits::from("0b0_0000")?.distance(&SomeBits::from("0b0_0000")?);
        println!("dist: {dist}");
        assert!(dist == 0);

        let dist = build_from_vec(Bitint::BITS as Bitint + 3, vec![5, 5])
            .distance(&SomeBits::new(Bitint::BITS as Bitint + 3));
        println!("dist: {dist}");
        assert!(dist == 4);

        let dist = build_from_vec(Bitint::BITS as Bitint + 8, vec![0b10101010, 0b01010]).distance(
            &build_from_vec(Bitint::BITS as Bitint + 8, vec![0b10, 0b010]),
        );

        println!("dist: {dist}");
        assert!(dist == 4);

        Ok(())
    }

    #[test]
    fn is_bit_set() -> Result<(), String> {
        let bitsx = build_from_vec(Bitint::BITS as Bitint + 4, vec![10, 10]);

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
        let bitsx = build_from_vec(Bitint::BITS as Bitint + 1, vec![0, Bitint::MAX as Bitint]);
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_high());

        let bitsx = build_from_vec(
            Bitint::BITS as Bitint + 1,
            vec![1, (Bitint::MAX as Bitint) - 1],
        );
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_high());

        let bitsx = build_from_vec(Bitint::BITS as Bitint + 1, vec![1, Bitint::MAX as Bitint]);
        println!("bitsx2: {bitsx}");
        assert!(bitsx.is_high());

        Ok(())
    }

    #[test]
    fn is_low() -> Result<(), String> {
        let bitsx = build_from_vec(Bitint::BITS as Bitint + 1, vec![0x1, 0]);
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_low());

        let bitsx = build_from_vec(Bitint::BITS as Bitint + 1, vec![0, 0x1]);
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_low());

        let bitsx = build_from_vec(Bitint::BITS as Bitint + 1, vec![0, 0]);
        println!("bitsx: {bitsx}");
        assert!(bitsx.is_low());

        Ok(())
    }

    #[test]
    fn num_one_bits() -> Result<(), String> {
        let bitsx = build_from_vec(Bitint::BITS as Bitint * 2, vec![0, 0]);
        println!("bitsx: {bitsx} num 1 {}", bitsx.num_one_bits());
        assert!(bitsx.num_one_bits() == 0);

        let bitsx = build_from_vec(
            Bitint::BITS as Bitint * 2,
            vec![Bitint::MAX << 1, Bitint::MAX >> 1],
        );
        println!("bitsx: {bitsx} num 1 {}", bitsx.num_one_bits());
        assert!(bitsx.num_one_bits() == ((Bitint::BITS * 2) - 2) as usize);

        Ok(())
    }

    #[test]
    fn split() -> Result<(), String> {
        let bitsx = build_from_vec(Bitint::BITS as Bitint + 8, vec![0x50, 0x5]);
        println!("bitsx: {bitsx}");

        let avec: Vec<SomeBits> = bitsx.split();
        println!("split bits: {}", tools::vec_string(&avec));

        assert!(avec.len() == 4);
        assert!(avec.contains(&build_from_vec(Bitint::BITS as Bitint + 8, vec![0x40, 0])));
        assert!(avec.contains(&build_from_vec(Bitint::BITS as Bitint + 8, vec![0x10, 0])));
        assert!(avec.contains(&build_from_vec(Bitint::BITS as Bitint + 8, vec![0, 0x4])));
        assert!(avec.contains(&build_from_vec(Bitint::BITS as Bitint + 8, vec![0, 1])));

        let bitsx = SomeBits::from("0x010")?;
        println!("bitsx: {bitsx}");

        let avec: Vec<SomeBits> = bitsx.split();
        println!("split bits: {}", tools::vec_string(&avec));

        assert!(avec.len() == 1);

        let bitsx = SomeBits::from("0x00")?;
        println!("bitsx: {bitsx}");

        let avec: Vec<SomeBits> = bitsx.split();
        println!("split bits: {}", tools::vec_string(&avec));

        assert!(avec.is_empty());

        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let bitsx = build_from_vec(Bitint::BITS as Bitint + 8, vec![0, 0]);
        let bitsy = build_from_vec(Bitint::BITS as Bitint + 8, vec![1, 1]);
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(!bitsx.is_adjacent(&bitsy));

        let bitsx = build_from_vec(Bitint::BITS as Bitint + 4, vec![1, 1]);
        let bitsy = build_from_vec(Bitint::BITS as Bitint + 4, vec![3, 1]);
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_adjacent(&bitsy));

        Ok(())
    }

    #[test]
    fn is_between() -> Result<(), String> {
        let bts2 = build_from_vec(Bitint::BITS as Bitint + 8, vec![0, 0x10]);
        let bts3 = build_from_vec(Bitint::BITS as Bitint + 8, vec![0, 0x11]);
        let bts5 = build_from_vec(Bitint::BITS as Bitint + 8, vec![1, 1]);
        assert!(bts3.is_between(&bts2, &bts5));
        assert!(!bts5.is_between(&bts2, &bts3));
        Ok(())
    }

    #[test]
    fn combine() -> Result<(), String> {
        let bits2 = SomeBits::from("10")?;
        let bits3 = SomeBits::from("101")?;
        let bits5 = bits2.combine(&bits3);
        println!("{bits2} combine {bits3} = {bits5}");
        assert!(bits5 == SomeBits::from("10101")?);

        Ok(())
    }
}
