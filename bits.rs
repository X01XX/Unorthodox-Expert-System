//! The SomeBits struct, storing a bit pattern in one or more unsigned integers.
//!
//! Some conventions:
//!
//!   Display all bits in all integers.
//!
//!   Counting bits starts at the right-most bit of the right-most int,
//!   and proceeds to the left, as in standard integer bit-position reckoning.
//!
//! The integer type/size can be increased, change "u8", below, the format string literal "04b" in the formatted_string function.
//!
//! Change the constants, below, as needed.
//!

/// The unsigned integer type used in a vector of bits.
type Bitint = u8;

/// The number of bits in an integer used by SomeBits.
const NUM_BITS_PER_INT: usize = u8::BITS as usize;

/// Mask for the highest bit in an integer.
const INT_HIGH_BIT: Bitint = 1 << (NUM_BITS_PER_INT - 1);

/// Mask for the highest nibble in an integer.
const INT_HIGH_NIBBLE: Bitint = 15 << (NUM_BITS_PER_INT - 4);

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
/// If there was only one domain, therefore state, this struct may be able to use an array
/// instead of a vector.
pub struct SomeBits {
    pub ints: Vec<Bitint>,
}

impl SomeBits {
    /// Return a new-low SomeBits instance, given the number of integers.
    pub fn new(ints: Vec<Bitint>) -> Self {
        assert!(!ints.is_empty());
        Self { ints }
    }

    /// Create a SomeBits instance with integer(s) set to zero.
    pub fn new_low(&self) -> Self {
        SomeBits {
            ints: vec![0 as Bitint; self.num_ints()],
        }
    }

    /// Create a SomeBits instance with integer(s) set to one.
    pub fn new_high(&self) -> Self {
        SomeBits {
            ints: vec![Bitint::MAX; self.num_ints()],
        }
    }

    /// Return a new bits instance, with a random value.
    pub fn new_random(&self) -> Self {
        let mut ints = vec![0 as Bitint; self.num_ints()];
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
    /// Using multiple integers could allow for a number that is too big for
    /// the standard methods of converting a string to an integer.
    pub fn new_from_string(&self, str: &str) -> Result<Self, String> {
        let mut bts = self.new_low();

        let mut base = 2;

        let lsb = self.num_ints() - 1;

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
                    return Err(format!("String {str}, too long?"));
                }

                let Ok(numx) = Bitint::from_str_radix(chr, 16) else {
                    return Err(format!("String {str}, invalid character?"));
                };

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

                let mut btsx = self.new_low(); // new Bits object, all zeros

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
    pub fn distance(&self, other: &Self) -> usize {
        self.b_xor(other).num_one_bits()
    }

    /// Return true if two bits instances are adjacent.
    pub fn is_adjacent(&self, other: &Self) -> bool {
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
    fn shift_left4(&self) -> Self {
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
    fn num_ints(&self) -> usize {
        self.ints.len()
    }

    /// Return the number of bits in the given SomeBits struct.
    pub fn num_bits(&self) -> usize {
        self.ints.len() * NUM_BITS_PER_INT
    }

    /// Return true if the highest bit is nonzero.
    /// This is used in the from_string functions to detect overflow
    /// from the next shift-left operation.
    fn high_bit_nonzero(&self) -> bool {
        self.ints[0] & INT_HIGH_BIT > 0
    }

    /// Return true if the highest nibble is nonzero.
    /// This is used in the from_string functions to detect overflow
    /// from the next shift-left operation.
    fn high_nibble_nonzero(&self) -> bool {
        self.ints[0] & INT_HIGH_NIBBLE > 0
    }

    /// Create a formatted string for the instance.
    fn formatted_string(&self) -> String {
        let mut astr = String::with_capacity(self.strlen());

        let nibbles_per_int = NUM_BITS_PER_INT / 4;
        let mut fil = 0;
        for intx in &self.ints {
            let mut inty = *intx;
            let mut nibbles = Vec::<Bitint>::with_capacity(nibbles_per_int);
            for _ in 0..nibbles_per_int {
                nibbles.push(inty & 15);
                inty >>= 4;
            }
            for nibx in nibbles.iter().rev() {
                if fil == 1 {
                    astr.push('_');
                }
                astr.push_str(&format!("{nibx:04b}"));
                fil = 1;
            }
        }
        astr
    }

    /// Create a formatted string to display under an instance,
    /// to indicate specific bits positions.
    pub fn str2(&self) -> String {
        let mut astr = String::with_capacity(self.strlen());

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

/// Implement the trait StrLen for SomeBits.
impl StrLen for SomeBits {
    fn strlen(&self) -> usize {
        let items_len = NUM_BITS_PER_INT * self.ints.len();
        let sep_len = self.ints.len() * (NUM_BITS_PER_INT / 4) - 1;
        items_len + sep_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools;
    use rand::Rng;

    #[test]
    fn test_strlen() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let strrep = format!("{tmp_bts}");
        let len = strrep.len();
        let calc_len = tmp_bts.strlen();
        println!("str {strrep} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_bts = SomeBits::new(vec![0, 0]);
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
            ints: vec![5 as Bitint, 4 as Bitint],
        };
        println!("bitsx: {bitsx}");
        assert!(bitsx.ints[0] == 5);
        assert!(bitsx.ints[1] == 4);
        Ok(())
    }

    // Test new_from_string, using randomly chosen hexadecimal digits.
    #[test]
    fn new_from_string() -> Result<(), String> {
        // Init bit patterns expected for each hexadecimal character.
        let bit_chars = [
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
        ];

        let tmp_bts = SomeBits::new(vec![0, 0]);

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

            let bitsx = tmp_bts
                .new_from_string(&("0b".to_owned() + &first_str + &second_str))
                .unwrap();

            // Check bits instance
            println!("bitsx: {bitsx}");
            assert!(bitsx.ints[0] == first_num);
            assert!(bitsx.ints[1] == second_num);

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
            let bitsx = tmp_bts
                .new_from_string(&("0x".to_owned() + &first_str + &second_str))
                .unwrap();

            // Check bits instance
            println!("bitsx: {bitsx}");
            assert!(bitsx.ints[0] == first_num);
            assert!(bitsx.ints[1] == second_num);
        }

        Ok(())
    }

    // Test b_eqv
    #[test]
    fn b_eqv() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0, 0]);
        let b1 = tmp_bts.new_from_string("0x5555")?;
        let b2 = tmp_bts.new_from_string("0xa675")?;
        let b3 = b1.b_eqv(&b2).b_and(&tmp_bts.new_from_string("0xfff")?);
        let b4 = tmp_bts.new_from_string("0x0cdf")?;
        println!("b3: {b3} b4: {b4}");
        assert_eq!(b3, b4);
        Ok(())
    }

    // Test SomeBits::b_and
    #[test]
    fn b_and() -> Result<(), String> {
        // 00
        let tmp_bts = SomeBits::new(vec![0, 0]);
        let mut bitsx = tmp_bts.new_low();
        let mut bitsy = tmp_bts.new_low();
        let mut bitsz = bitsx.b_and(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_low());

        // 01
        bitsx = tmp_bts.new_low();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_and(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_low());

        // 11
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_high();
        bitsz = bitsx.b_and(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_high());

        // 10
        bitsx = tmp_bts.new_high();
        bitsy = tmp_bts.new_low();
        bitsz = bitsx.b_and(&bitsy);
        println!("bitsz: {bitsz}");
        assert!(bitsz.is_low());

        Ok(())
    }

    // Test SomeBits::b_not
    #[test]
    fn b_not() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0, 0]);
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
        let tmp_bts = SomeBits::new(vec![0, 0]);
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
        let tmp_bts = SomeBits::new(vec![0, 0]);
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

    // Test change_bit functions.
    #[test]
    fn change_bits() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0, 0]);
        let mut bits11 = SomeBits::new(vec![1, 1]);

        for inx in 0..NUM_BITS_PER_INT {
            let bitsx = tmp_bts.change_bit(inx).change_bit(inx + NUM_BITS_PER_INT);
            println!("bits11 {bits11} bitsx: {bitsx}");
            assert!(bitsx == bits11);
            let bitsy = bits11.change_bit(inx).change_bit(inx + NUM_BITS_PER_INT);
            println!("bitsy {bitsy} tmp_bts: {tmp_bts}");
            assert!(bitsy == tmp_bts);
            bits11 = bits11.shift_left();
        }

        Ok(())
    }

    // Test SomeBits::distance
    #[test]
    fn distance() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0, 0]);
        let dist = tmp_bts
            .new_from_string("0x0")?
            .distance(&tmp_bts.new_from_string("0x0")?);
        println!("dist: {dist}");
        assert!(dist == 0);

        let dist = tmp_bts
            .new_from_string("0x5555")?
            .distance(&tmp_bts.new_from_string("0x0")?);
        println!("dist: {dist}");
        assert!(dist == 8);

        let dist = tmp_bts
            .new_from_string("0xaaaa")?
            .distance(&tmp_bts.new_from_string("0x02")?);
        println!("dist: {dist}");
        assert!(dist == 7);

        Ok(())
    }

    // Test SomeBits::high_bit_nonzero
    #[test]
    fn high_bit_nonzero() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let mut bitsx = tmp_bts.new_from_string("0x1")?;
        println!("bitsx: {bitsx}");
        assert!(!bitsx.high_bit_nonzero());

        // Shift to left-most position.
        for _ in 0..(NUM_BITS_PER_INT - 1) {
            bitsx = bitsx.shift_left();
        }

        println!("bitsx: {bitsx}");
        assert!(bitsx.high_bit_nonzero());

        Ok(())
    }

    // Test SomeBits::is_bit_set
    #[test]
    fn is_bit_set() -> Result<(), String> {
        let bitsx = SomeBits::new(vec![0, 0]).new_from_string("0x5aa5")?;
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
        let mut bitsx = SomeBits::new(vec![1]);
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_high());

        let bitsy = bitsx.clone();

        for _ in 0..(NUM_BITS_PER_INT - 1) {
            bitsx = bitsx.shift_left().b_or(&bitsy);
        }
        println!("bitsx: {bitsx}");
        assert!(bitsx.is_high());

        Ok(())
    }

    // Test SomeBits::is_low
    #[test]
    fn is_low() -> Result<(), String> {
        let bitsx = SomeBits::new(vec![0xa5]);
        println!("bitsx: {bitsx}");
        assert!(!bitsx.is_low());

        let bitsx = bitsx.new_low();
        println!("bitsx: {bitsx}");
        assert!(bitsx.is_low());

        let bitsx = SomeBits::new(vec![0, 0]).new_from_string("0x2000")?;
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
        let bitsx = SomeBits::new(vec![0, 0]);
        let bitsy = bitsx.new_from_string("0x0")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_subset_of(&bitsy));

        let bitsy = bitsy.new_from_string("0x5")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_subset_of(&bitsy));
        assert!(!bitsy.is_subset_of(&bitsx));

        let bitsx = bitsx.new_from_string("0x5555")?;
        let bitsy = bitsy.new_from_string("0x7777")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_subset_of(&bitsy));
        assert!(!bitsy.is_subset_of(&bitsx));

        let bitsx = bitsx.new_from_string("0x5")?;
        let bitsy = bitsx.new_from_string("0x1")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsy.is_subset_of(&bitsx));
        assert!(!bitsx.is_subset_of(&bitsy));

        Ok(())
    }

    // Test SomeBits::is_superset_of
    #[test]
    fn is_superset_of() -> Result<(), String> {
        let bitsx = SomeBits::new(vec![0, 0]);
        let bitsy = bitsx.new_from_string("0x0")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_superset_of(&bitsy));

        let bitsx = bitsx.new_from_string("0x5")?;
        let bitsy = bitsx.new_from_string("0x0")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_superset_of(&bitsy));
        assert!(!bitsy.is_superset_of(&bitsx));

        let bitsx = bitsx.new_from_string("0x7777")?;
        let bitsy = bitsx.new_from_string("0x5555")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_superset_of(&bitsy));
        assert!(!bitsy.is_superset_of(&bitsx));

        let bitsx = bitsx.new_from_string("0x5")?;
        let bitsy = bitsx.new_from_string("0x1")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_superset_of(&bitsy));
        assert!(!bitsy.is_superset_of(&bitsx));

        Ok(())
    }

    // Test SomeBits::num_one_bits
    #[test]
    fn num_one_bits() -> Result<(), String> {
        let bitsx = SomeBits::new(vec![0, 0]).new_from_string("0x5555")?;
        println!("bitsx: {bitsx}");
        assert!(bitsx.num_one_bits() == 8);

        let bitsx = bitsx.new_from_string("0xaaa1")?;
        println!("bitsx: {bitsx}");
        assert!(bitsx.num_one_bits() == 7);

        Ok(())
    }

    // Test SomeBits::push_1
    #[test]
    fn push_1() -> Result<(), String> {
        let bitsx = SomeBits::new(vec![0, 0]).new_from_string("0x5555")?;
        let bitsy = bitsx.push_1();
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsy == bitsx.new_from_string("0xaaab")?);

        Ok(())
    }

    // Test SomeBits::shift_left
    #[test]
    fn shift_left() -> Result<(), String> {
        let bitsx = SomeBits::new(vec![0, 0]).new_from_string("0xd555")?;
        let bitsy = bitsx.shift_left();
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsy.b_and(&bitsx.new_from_string("0xffff")?) == bitsx.new_from_string("0xaaaa")?);

        Ok(())
    }

    // Test SomeBits::shift_left4
    #[test]
    fn shift_left4() -> Result<(), String> {
        let bitsx = SomeBits::new(vec![0, 0]).new_from_string("0xf505")?;
        let bitsy = bitsx.shift_left4();
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsy.b_and(&bitsx.new_from_string("0xffff")?) == bitsx.new_from_string("0x5050")?);

        Ok(())
    }

    // Test SomeBits::split
    #[test]
    fn split() -> Result<(), String> {
        let bitsx = SomeBits::new(vec![0, 0]).new_from_string("0x5050")?;
        println!("bitsx: {bitsx}");

        let avec: Vec<SomeBits> = bitsx.new_from_string("0x5050")?.split();
        println!("split bits: {}", tools::vec_string(&avec));

        assert!(avec.len() == 4);
        assert!(avec.contains(&bitsx.new_from_string("0x4000")?));
        assert!(avec.contains(&bitsx.new_from_string("0x1000")?));
        assert!(avec.contains(&bitsx.new_from_string("0x0040")?));
        assert!(avec.contains(&bitsx.new_from_string("0x0010")?));

        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let bitsx = SomeBits::new(vec![0, 0]);
        let bitsy = bitsx.new_from_string("0x11")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(!bitsx.is_adjacent(&bitsy));

        let bitsx = bitsx.new_from_string("0x01")?;
        let bitsy = bitsx.new_from_string("0x11")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_adjacent(&bitsy));

        let bitsx = bitsx.new_from_string("0x0000")?;
        let bitsy = bitsx.new_from_string("0x1100")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(!bitsx.is_adjacent(&bitsy));

        let bitsx = bitsx.new_from_string("0x0100")?;
        let bitsy = bitsx.new_from_string("0x1100")?;
        println!("bitsx: {bitsx} bitsy: {bitsy}");
        assert!(bitsx.is_adjacent(&bitsy));

        Ok(())
    }
}
