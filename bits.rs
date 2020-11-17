// Bits structure for an Unorthodox Expert System
//
// A vector of one or more unsigned 8-bit integers.
//
// The Domain creation, with a starting state, by running something like:
//
//   let mut dm1 = init_domain(SomeState::new(SomeBits::new(vec![2 as u8])));
//    or
//   let mut dm1 = init_domain(SomeState::new(SomeBits::new(vec![0 as u8, 2 as u8])));
//
// initializes the domain cur_state with a given number of u8 ints.
//
// Later SomeBits operations use the same number of ints.
//
// Arrays might be better, but do not yet allow initialization with a variable
// for the length.
//
// Some conventions:
//
//   Display all bits in all integers.
//
//   Counting bits starts at the right-most bit of the right-most int,
//   and proceeds to the left, as in standard integer bit-position reckoning.

// The integer size could be increased, but some of these values must change,
// then search for all "u8" references in the *.rs files.
const NUM_BITS_PER_INT: usize = 8;

const INT_ALL_BITS_MASK: u8 = !(0 as u8);

const ALL_BIT_MASKS: [u8; NUM_BITS_PER_INT] = [1, 2, 4, 8, 16, 32, 64, 128]; // bits 0-N

const INT_HIGH_BIT: u8 = 128;

use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::{Hash, Hasher};

impl fmt::Display for SomeBits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut astr = String::new();

        let num_bits = self.num_ints() * 8;

        for inx in (0..num_bits).rev() {
            if self.is_bit_set(inx) {
                astr.push('1');
            } else {
                astr.push('0');
            }
        }
        write!(f, "{}", astr)
    }
}

impl Hash for SomeBits {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        let mut hasher = DefaultHasher::new();
        Hash::hash_slice(&self.ints, &mut hasher);
        hasher.finish();
    }
}

impl PartialEq for SomeBits {
    fn eq(&self, other: &Self) -> bool {
        for int_inx in 0..self.num_ints() {
            if self.ints[int_inx] != other.ints[int_inx] {
                return false;
            }
        }
        true
    }
}

impl Eq for SomeBits {}

#[derive(Debug)]
pub struct SomeBits {
    pub ints: Vec<u8>, // an array may be better, but less flexible with the current Rust version
}

impl SomeBits {
    // Return a new bits struct, set to a given vector
    pub fn _newx(int_vals: Vec<u8>) -> Self {
        SomeBits { ints: int_vals }
    }

    pub fn new_low(&self) -> Self {
        let num_ints = self.ints.len();

        let mut ints2 = Vec::<u8>::with_capacity(num_ints);
        for _int_inx in 0..num_ints {
            ints2.push(0 as u8);
        }
        Self { ints: ints2 }
    }

    pub fn clone(&self) -> Self {
        let mut v1 = Vec::<u8>::with_capacity(self.ints.len());
        for num in self.ints.iter() {
            v1.push(*num);
        }
        SomeBits { ints: v1 }
    }

    // Return a vector of bits where each has only
    // one 1 bit isolated from the given Bits struct.
    // Should be called like BitsStore { avec: <a bits object>.split() }
    pub fn split(&self) -> Vec<Self> {
        let num_bits = self.num_one_bits();

        let mut rc_vec: Vec<Self> = Vec::with_capacity(num_bits);

        let num_ints = self.num_ints();

        for int_inx in 0..num_ints {
            if self.ints[int_inx] > 0 {
                let mut tmpint = self.ints[int_inx];

                while tmpint > 0 {
                    let tmp2 = tmpint - 1;

                    let abit = tmpint & !tmp2;

                    let mut btsx = self.new_low(); // new Bits object, all zeros
                    btsx.ints[int_inx] = abit; // update one integer
                    rc_vec.push(btsx); // Save result

                    tmpint = tmpint & tmp2;
                }
            }
        } // end for int_inx
        rc_vec
    }

    // Return a Bits struct with specified bit(s) changed
    pub fn toggle_bits(&self, bit_nums: &[usize]) -> Self {
        let mut ary2 = Vec::<u8>::with_capacity(self.ints.len());

        for intx in self.ints.iter() {
            ary2.push(*intx);
        }

        let num_ints = self.num_ints();
        let num_bits = num_ints * 8;
        let lsi = num_ints - 1; // least significant integer

        for bit_num in bit_nums {
            if *bit_num >= num_bits {
                panic!("bit num too large");
            }

            let bit_pos = bit_num % NUM_BITS_PER_INT;
            let int_num = lsi - (bit_num / NUM_BITS_PER_INT);

            ary2[int_num] = self.ints[int_num] ^ ALL_BIT_MASKS[bit_pos];
        }
        Self { ints: ary2 }
    }

    // Return true if a bit is one at a given position
    pub fn is_bit_set(&self, bit_num: usize) -> bool {
        let num_ints = self.num_ints();
        let num_bits = num_ints * 8;
        let lsi = num_ints - 1;

        if bit_num >= num_bits {
            panic!("bit num too large");
        }

        let bit_pos = bit_num % NUM_BITS_PER_INT;

        let int_num = lsi - (bit_num / NUM_BITS_PER_INT);

        self.ints[int_num] & ALL_BIT_MASKS[bit_pos] > 0
    }

    // Bitwise NOT of a Bits stuct
    pub fn b_not(&self) -> Self {
        let mut ary2 = Vec::<u8>::with_capacity(self.ints.len());

        for intx in self.ints.iter() {
            ary2.push(!intx);
        }
        Self { ints: ary2 }
    }

    // Bitwise AND of two Bits structs
    pub fn b_and(&self, other: &Self) -> Self {
        let mut ary2 = Vec::<u8>::with_capacity(self.ints.len());

        for int_inx in 0..self.num_ints() {
            ary2.push(self.ints[int_inx] & other.ints[int_inx]);
        }
        Self { ints: ary2 }
    }

    // Bitwise OR of two Bits structs
    pub fn b_or(&self, other: &Self) -> Self {
        let mut ary2 = Vec::<u8>::with_capacity(self.ints.len());

        for int_inx in 0..self.num_ints() {
            ary2.push(self.ints[int_inx] | other.ints[int_inx]);
        }
        Self { ints: ary2 }
    }

    // Bitwise XOR of two Bits structs
    pub fn b_xor(&self, other: &Self) -> Self {
        let mut ary2 = Vec::<u8>::with_capacity(self.ints.len());

        for int_inx in 0..self.num_ints() {
            ary2.push(self.ints[int_inx] ^ other.ints[int_inx]);
        }
        Self { ints: ary2 }
    }

    // Return true if the Bits struct value is low, that is all zeros
    pub fn is_low(&self) -> bool {
        for int_inx in 0..self.num_ints() {
            if self.ints[int_inx] > 0 {
                return false;
            }
        }
        true
    }

    // Return true if a Bits struct has at least one bit set to one
    //    pub fn is_not_low(&self) -> bool {
    //        for int_inx in 0..NUM_INTS {
    //            if self.ints[int_inx] > 0 {
    //                return true;
    //            }
    //        }
    //        false
    //    }

    // Return true if the Bits struct value is high, that is all ones
    pub fn is_high(&self) -> bool {
        for intx in self.ints.iter() {
            if *intx != INT_ALL_BITS_MASK {
                return false;
            }
        }
        true
    }

    // Return true is a Bits struct is a ones-subset of another
    pub fn is_subset_of(&self, other: &Self) -> bool {
        let btmp = self.b_and(&other);
        self == &btmp
    }

    // Return true if a Bits struct is a ones-superset of another
    pub fn is_superset_of(&self, other: &Self) -> bool {
        other.is_subset_of(&self)
    }

    // Return the number of one bits
    pub fn num_one_bits(&self) -> usize {
        let mut cnt = 0;

        for int_inx in 0..self.num_ints() {
            if self.ints[int_inx] > 0 {
                let mut tmpint = self.ints[int_inx];

                while tmpint > 0 {
                    let tmp2 = tmpint - 1;
                    tmpint = tmpint & tmp2;
                    cnt += 1;
                }
            }
        } // end for int_inx
        cnt
    }

    // Return true if only one bit set
    pub fn just_one_bit(&self) -> bool {
        let mut cnt = 0;

        for int_inx in 0..self.num_ints() {
            if self.ints[int_inx] > 0 {
                let mut tmpint = self.ints[int_inx];

                while tmpint > 0 {
                    let tmp2 = tmpint - 1;
                    tmpint = tmpint & tmp2;

                    if cnt > 0 {
                        return false;
                    }
                    cnt += 1;
                }
            }
        } // end for int_inx

        cnt == 1
    }

    // Push 1 to the LSBit
    pub fn push_1(&self) -> Self {
        let mut ints2 = Vec::<u8>::with_capacity(self.ints.len());

        let tmp = self.shift_left();

        let num_ints = self.num_ints();
        let lsi = num_ints - 1;

        for int_inx in 0..num_ints {
            ints2.push(tmp.ints[int_inx]);
        }
        ints2[lsi] += 1;

        Self { ints: ints2 }
    }

    // Shift bits left by 1 bit
    pub fn shift_left(&self) -> Self {
        let mut ints2 = Vec::<u8>::with_capacity(self.ints.len());
        for _int_inx in (0..self.ints.len()).rev() {
            ints2.push(0 as u8);
        }

        let mut carry: u8 = 0;

        for int_inx in (0..self.ints.len()).rev() {
            ints2[int_inx] = (self.ints[int_inx] << 1) + carry;

            if (self.ints[int_inx] & INT_HIGH_BIT) > 0 {
                carry = 1;
            } else {
                carry = 0;
            }
        }

        // Overflow check
        //		if carry == 1 {
        //			panic!("Bits shift_left overflow");
        //		}

        Self { ints: ints2 }
    }

    // Return the number of integers used to express the SomeBits type
    pub fn num_ints(&self) -> usize {
        self.ints.len()
    }
}
