
/// A test of using a base 4 number to represent a region.
///
/// One advantage is allowing a key to lookup a region in a hashmap.

use std::fmt;

type B4int = u8;

/// Display trait for SomeBits
impl fmt::Display for SomeB4 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

pub struct SomeB4 {
    /// Number of bits that are active.  So the first integer in the vector
    /// may use one, or more bits.
    pub num_bits: B4int,
    /// A vector of one, or more, unsigned integers.
    pub ints: Vec<B4int>,
}

impl SomeB4 {
    /// Return a copy, shifted left by 2 bits.
    /// The Most Significant 2 Bit values are lost.
    pub fn shift_left(&self) -> Self {
        let mut ints = vec![0 as B4int; self.ints.len()];

        let mut carry: B4int = 0;

        for int_inx in (0..self.ints.len()).rev() {

            let next_carry: B4int = self.ints[int_inx] >> (B4int::BITS - 2);

            ints[int_inx] = (self.ints[int_inx] << 2) + carry;

            carry = next_carry;
        }

        let adjust = self.num_bits as u32 % B4int::BITS;
        if adjust > 0 {
            ints[0] &= B4int::MAX >> (B4int::BITS - adjust);
        }

        Self {
            num_bits: self.num_bits,
            ints,
        }
    }
}
