// Region struct for an Unorthodox Expert System

//use crate::bits::SomeBits;
use crate::mask::SomeMask;
//use crate::maskstore::MaskStore;
use crate::bits::SomeBits;
use crate::state::{state_from_string, SomeState};
use crate::statestore::StateStore;

use std::fmt;

#[derive(Debug)]
pub struct SomeRegion {
    pub state1: SomeState,
    pub state2: SomeState,
    pub active: bool,
}

impl fmt::Display for SomeRegion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s1 = self.str_terse();

        write!(f, "r{}", s1)
    }
}
impl PartialEq for SomeRegion {
    fn eq(&self, other: &Self) -> bool {
        if self.intersects(&other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1 == x2;
        }
        false
    }
}
impl Eq for SomeRegion {}

impl SomeRegion {
    pub fn new(sta1: &SomeState, sta2: &SomeState) -> Self {
        Self {
            state1: sta1.clone(),
            state2: sta2.clone(),
            active: true,
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            state1: self.state1.clone(),
            state2: self.state2.clone(),
            active: true,
        }
    }

    // Print the bits of the Region without any prefix
    pub fn str_terse(&self) -> String {
        let mut s1 = String::new();
        let num_ints = self.state1.num_ints();
        let num_bits = num_ints * 8;

        for valb in (0..num_bits).rev() {
            let b0 = self.state1.is_bit_set(valb);
            let b1 = self.state2.is_bit_set(valb);

            if b0 {
                if b1 {
                    s1.push('1');
                } else {
                    s1.push('X');
                }
            } else {
                if b1 {
                    s1.push('X');
                } else {
                    s1.push('0');
                }
            }
            // println!("a bit is: {} b0 set {} b1 set {} s1: {}", valb, b0, b1, s1);
        }
        s1
    }

    pub fn str_not_x(&self, msk: &SomeMask) -> String {
        let mut s1 = String::new();

        let num_ints = self.state1.num_ints();
        let num_bits = num_ints * 8;

        for valb in (0..num_bits).rev() {
            let b0 = self.state1.is_bit_set(valb);
            let b1 = self.state2.is_bit_set(valb);

            if b0 {
                if b1 {
                    s1.push('1');
                } else {
                    if msk.is_bit_set(valb) {
                        s1.push('x');
                    } else {
                        s1.push('X');
                    }
                }
            } else {
                if b1 {
                    if msk.is_bit_set(valb) {
                        s1.push('x');
                    } else {
                        s1.push('X');
                    }
                } else {
                    s1.push('0');
                }
            }
            // println!("a bit is: {} b0 set {} b1 set {} s1: {}", valb, b0, b1, s1);
        }
        s1
    }

    pub fn inactivate(&mut self) -> bool {
        self.active = false;
        true
    }

    // Return true if two regions are adjacent
    pub fn is_adjacent(&self, other: &Self) -> bool {
        self.diff_bits(&other).just_one_bit()
    }

    // Return true if a region is adjacent to a state
    pub fn is_adjacent_state(&self, other: &SomeState) -> bool {
        self.diff_bits_state(&other).just_one_bit()
    }

    // Return true if two regions intersect
    pub fn intersects(&self, other: &Self) -> bool {
        self.diff_bits(&other).is_low()
    }

    // Return the intersection of two regions
    // call intersects first
    // Strangely, the intersection two adjacent regions produces most of an overlapping part.
    pub fn intersection(&self, other: &Self) -> Self {
        Self::new(
            &SomeState::new(self.high_mask().bts.b_and(&other.high_mask().bts)),
            &SomeState::new(self.low_mask().bts.b_and(&other.low_mask().bts).b_not()),
        )
    }

    // Return true is a region is a superset of a state
    pub fn is_superset_of_state(&self, a_state: &SomeState) -> bool {
        let t1 = self
            .state1
            .bts
            .b_xor(&a_state.bts)
            .b_and(&self.state2.bts.b_xor(&a_state.bts));

        t1.is_low()
    }

    // Return mask of x bits
    pub fn x_mask(&self) -> SomeMask {
        SomeMask::new(self.state1.bts.b_xor(&self.state2.bts))
    }

    // Return the number of ints used to express a state in a SomeRegion instance
    //    pub fn num_ints(&self) -> usize {
    //        self.state1.num_ints()
    //	}

    // Return the numbeer of X bits in a region
    pub fn num_x(&self) -> usize {
        SomeMask::new(self.state1.bts.b_xor(&self.state2.bts)).num_one_bits()
    }

    // Return mask of not x bits
    // In some cases, the caller may need to AND the result with the domain max_region x_mask.
    pub fn not_x_mask(&self) -> SomeMask {
        SomeMask::new(self.state1.bts.b_xor(&self.state2.bts).b_not())
    }

    pub fn far_state(&self, sta: &SomeState) -> SomeState {
        SomeState::new(self.state1.bts.b_xor(&self.state2.bts).b_xor(&sta.bts))
    }

    // Given a region, and a proper subset region, return the
    // far region within the superset region.
    pub fn far_reg(&self, other: &SomeRegion) -> SomeRegion {
        let int_x_msk = self.x_mask();

        let ok_x_msk = other.x_mask();

        assert!(int_x_msk != ok_x_msk);
        assert!(ok_x_msk.is_subset_of(&int_x_msk));

        // Get bit(s) to use to calculate a far-sub-region in reg_int from ok_reg
        // by changing reg_int X over ok_reg 1 to X over 0, or reg_int X over ok_reg 0 to X over 1
        let cng_bits = int_x_msk.m_xor(&ok_x_msk);

        SomeRegion::new(
            &SomeState::new(other.state1.bts.b_xor(&cng_bits.bts)),
            &SomeState::new(other.state2.bts.b_xor(&cng_bits.bts)),
        )
    }

    // Return true if a region is a subset on another region
    pub fn is_subset_of(&self, other: &Self) -> bool {
        if self.intersects(&other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_subset_of(&x2);
        }
        false
    }

    pub fn is_superset_of(&self, other: &Self) -> bool {
        if self.intersects(&other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_superset_of(&x2);
        }
        false
    }

    pub fn union(&self, other: &Self) -> Self {
        let st_low = self
            .state1
            .s_and(&self.state2)
            .s_and(&other.state1)
            .s_and(&other.state2);

        let st_high = self
            .state1
            .s_or(&self.state2)
            .s_or(&other.state1)
            .s_or(&other.state2);

        Self::new(&st_high, &st_low)
    }

    pub fn union_state(&self, other: &SomeState) -> Self {
        let st_low = self.state1.s_and(&self.state2).s_and(&other);

        let st_high = self.state1.s_or(&self.state2).s_or(&other);

        Self::new(&st_high, &st_low)
    }

    // Return a Mask of zero, non-X, bits
    //    pub fn zeros_mask(&self) -> SomeMask {
    //        SomeMask::new(self.state1.bts.b_not().b_and(&self.state2.bts.b_not()))
    //    }

    // Return a Mask of one, non-X,  bits
    //    pub fn ones_mask(&self) -> SomeMask {
    //        SomeMask::new(self.state1.bts.b_and(&self.state2.bts))
    //    }

    // Return a Mask of zero or X bits (which include zero)
    pub fn low_mask(&self) -> SomeMask {
        SomeMask::new(self.state1.bts.b_not().b_or(&self.state2.bts.b_not()))
    }

    // Return a Mask of ones or X bits (which include one).
    pub fn high_mask(&self) -> SomeMask {
        SomeMask::new(self.state1.bts.b_or(&self.state2.bts))
    }

    // Return a region with masked X-bits set to zeros
    //    pub fn set_to_zeros(&self, msk: &SomeMask) -> Self {
    //        Self::new(
    //            &SomeState::new(self.state1.bts.b_and(&msk.bts.b_not())),
    //            &SomeState::new(self.state2.bts.b_and(&msk.bts.b_not())),
    //        )
    //    }

    // Return a region with masked X-bits set to ones
    //    pub fn set_to_ones(&self, msk: &SomeMask) -> Self {
    //        Self::new(
    //            &SomeState::new(self.state1.bts.b_or(&msk.bts)),
    //            &SomeState::new(self.state2.bts.b_or(&msk.bts)),
    //        )
    //    }

    // Return a region with masked X-bits set to zeros
    pub fn set_to_x(&self, msk: &SomeMask) -> Self {
        Self::new(
            &SomeState::new(self.state1.bts.b_or(&msk.bts)),
            &SomeState::new(self.state2.bts.b_and(&msk.bts.b_not())),
        )
    }

    pub fn diff_bits_state(&self, sta1: &SomeState) -> SomeMask {
        SomeMask::new(
            self.state1
                .bts
                .b_xor(&sta1.bts)
                .b_and(&self.state2.bts.b_xor(&sta1.bts)),
        )
    }

    pub fn diff_bits(&self, reg1: &SomeRegion) -> SomeMask {
        self.diff_bits_state(&reg1.state1)
            .m_and(&self.diff_bits_state(&reg1.state2))
    }

    pub fn num_diff_bits(&self, reg1: &SomeRegion) -> usize {
        self.diff_bits_state(&reg1.state1)
            .m_and(&self.diff_bits_state(&reg1.state2))
            .num_one_bits()
    }

    // Return states in region
    pub fn states_in(&self, stas: &StateStore) -> StateStore {
        let mut stsin = StateStore::new();

        for stax in stas.iter() {
            if self.is_superset_of_state(stax) {
                stsin.push(stax.clone());
            }
        } // next stax

        stsin
    }

    // Given two adjacent regions, return an overlapping region
    pub fn overlapping_part(&self, other: &Self) -> Self {
        assert!(self.is_adjacent(&other));

        let adj_bit = self.diff_bits(&other);

        self.intersection(&other).set_to_x(&adj_bit) // a strange use of the intersection logic
    }

    pub fn overlapping_part_state(&self, other: &SomeState) -> SomeState {
        assert!(self.is_adjacent_state(&other));

        let adj_bit = self.diff_bits_state(&other);

        SomeState::new(other.bts.b_xor(&adj_bit.bts))
    }

    pub fn toggle_bits(&self, tbits: &SomeMask) -> Self {
        let stxor = SomeState::new(tbits.bts.clone());
        Self::new(&self.state1.s_xor(&stxor), &self.state2.s_xor(&stxor))
    }

    // Given a region, and a second region, return the
    // first region - the second
    //    pub fn subtract(&self, other: &SomeRegion) -> Vec<Self> {
    //        let mut avec = Vec::<Self>::new();
    //
    //        if self.intersects(&other) == false {
    //            avec.push(self.clone());
    //            return avec;
    //        }
    //
    //        let reg_int = self.intersection(&other);
    //
    //        let x_over_not_xs = MaskStore {
    //            avec: self.x_mask().m_and(&reg_int.not_x_mask()).split(),
    //        }; // split -> Vec<SomeMask>
    //
    //        for mskx in x_over_not_xs.iter() {
    //            if mskx.bts.b_and(&reg_int.state1.bts).is_low() {
    //                // reg_int has a 0 bit in that position
    //                avec.push(self.set_to_ones(&mskx));
    //            } else {
    //                // reg_int has a 1 in that bit position
    //                avec.push(self.set_to_zeros(&mskx));
    //            }
    //        }
    //
    //        avec
    //    }

    // Return a mask of zero-bit positions on a region
    //    pub fn zeros_mask(&self) -> SomeMask {
    //        SomeMask::new(self.state1.bts.b_not().b_and(&self.state2.bts.b_not()))
    //    }

    // Return a mask of one-bit positions on a region
    //    pub fn ones_mask(&self) -> SomeMask {
    //        SomeMask::new(self.state1.bts.b_and(&self.state2.bts))
    //    }

    // Given a region and a second adjacent region, return the
    // adjacent part of the second region to the first region.
    //    pub fn adj_part(&self, other: &SomeRegion) -> SomeRegion {
    //        if self.is_adjacent(&other) == false {
    //            panic!("regions not adjacent?");
    //        }
    //
    //        let not_x_over_xs = self.not_x_mask().m_and(&other.x_mask()); // -> SomeMask
    //
    //        let mut ret_reg = other.clone();
    //
    //        let to_zero_msk = not_x_over_xs.m_and(&self.zeros_mask());
    //
    //        if to_zero_msk.is_not_low() {
    //            ret_reg = ret_reg.set_to_zeros(&to_zero_msk);
    //        }
    //
    //        let to_one_msk = not_x_over_xs.m_and(&self.ones_mask());
    //
    //        if to_one_msk.is_not_low() {
    //            ret_reg = ret_reg.set_to_ones(&to_one_msk);
    //        }
    //
    //        ret_reg
    //    }

    // Given a region, return a vector of adjacent edges
    // Should be called like RegionStore { avec: adj_edges(...) }
    //    pub fn adj_edges(&self, max_reg: &SomeRegion) -> Vec<Self> {
    //        let mut avec = Vec::<Self>::new();
    //
    //        let not_x = self.not_x_mask().m_and(&max_reg.x_mask());
    //
    //        let not_x_masks = MaskStore {
    //            avec: not_x.split(),
    //        };
    //
    //        let ones = self.ones_mask();
    //
    //        for mskbit in not_x_masks.iter() {
    //            if mskbit.m_and(&ones).is_low() {
    //                // then is a zero bit
    //                avec.push(self.set_to_ones(&mskbit));
    //            } else {
    //                // is a one bit
    //                avec.push(self.set_to_zeros(&mskbit));
    //            }
    //        }
    //
    //        avec
    //    }
}

// Return a Region from a string, like "r01X1".
// Left-most, consecutive, zeros can be omitted.
pub fn region_from_string(num_ints: usize, str: &str) -> Result<SomeRegion, String> {
    let mut bts_high = SomeBits {
        ints: vec![0 as u8; num_ints],
    };
    let mut bts_low = SomeBits {
        ints: vec![0 as u8; num_ints],
    };

    let mut inx = -1;

    for ch in str.chars() {
        inx += 1;

        if inx == 0 {
            if ch == 'r' {
                continue;
            } else if ch == 's' {
                let state_r = state_from_string(num_ints, str);
                match state_r {
                    Ok(a_state) => {
                        return Ok(SomeRegion::new(&a_state, &a_state));
                    }
                    Err(error) => {
                        return Err(format!("\nDid not understand state, {}", error));
                    }
                } // end match state_r
            } else {
                return Err(String::from("first character should be r"));
            }
        }

        if bts_high.high_bit_set() {
            return Err(String::from("too long"));
        }

        if bts_low.high_bit_set() {
            return Err(String::from("too long"));
        }

        if ch == '0' {
            bts_high = bts_high.shift_left();
            bts_low = bts_low.shift_left();
        } else if ch == '1' {
            bts_high = bts_high.push_1();
            bts_low = bts_low.push_1();
        } else if ch == 'x' || ch == 'X' {
            bts_high = bts_high.push_1();
            bts_low = bts_low.shift_left();
        } else if ch == '_' {
            continue;
        } else {
            return Err(String::from("invalid character"));
        }
    } // end for ch

    Ok(SomeRegion::new(
        &SomeState::new(bts_high),
        &SomeState::new(bts_low),
    ))
} // end region_from_string
