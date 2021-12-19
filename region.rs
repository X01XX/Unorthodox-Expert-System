//! The SomeRegion struct, representing a region on a pseudo Karnaugh Map.
//!
//! Uses two states (they can be the same) to represent a region, which includes every state between them.
//!
//! Can serve as a store for any two states.

use crate::bits::{SomeBits, NUM_BITS_PER_INT};
use crate::mask::SomeMask;
use crate::state::SomeState;
use crate::statestore::StateStore;
use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug)]
/// SomeRegion struct
pub struct SomeRegion {
    /// First state defining a region, it represents a sempled state.
    pub state1: SomeState,
    /// Second state defining a region, it represents a sempled state.
    /// It may be the same as the first state, for a region with no X-bit positions.
    pub state2: SomeState,
}

impl fmt::Display for SomeRegion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl PartialEq for SomeRegion {
    fn eq(&self, other: &Self) -> bool {
        if self.intersects(&other) {
            return self.x_mask() == other.x_mask();
        }
        false
    }
}
impl Eq for SomeRegion {}

impl SomeRegion {
    /// Create new region from two states.
    ///
    /// Keep the order of the states given. This is used in
    /// SomeAction::create_groups_from_square for prosessing the regions
    /// returned from SomeAction::possible_group_regions.
    pub fn new(sta1: &SomeState, sta2: &SomeState) -> Self {
        Self {
            state1: sta1.clone(),
            state2: sta2.clone(),
        }
    }

    /// Return a Region from a string and a hint as to the number of integers to use.
    /// Left-most, consecutive, zeros can be omitted.
    ///
    /// if let Ok(regx) = <SomeDomain>.region_from_string(1, "r01x1")) {
    ///    println!("Region {}", &regx);
    /// } else {
    ///    panic!("Invalid Region");
    /// }
    ///
    pub fn new_from_string(num_ints: usize, str: &str) -> Result<Self, String> {
        let mut bts_high = SomeBits::new(num_ints);

        let mut bts_low = SomeBits::new(num_ints);

        let mut inx = -1;

        for ch in str.chars() {
            inx += 1;

            if inx == 0 {
                if ch == 'r' || ch == 'R' {
                    continue;
                } else if ch == 's' || ch == 'S' {
                    let state_r = SomeState::new_from_string(num_ints, &str);
                    match state_r {
                        Ok(a_state) => {
                            return Ok(SomeRegion::new(&a_state, &a_state));
                        }
                        Err(error) => {
                            return Err(error);
                        }
                    } // end match state_r
                } else {
                    return Err(format!("Did not understand the string {}, first character?", str));
                }
            }

            if bts_high.high_bit_set() {
                return Err(format!("Did not understand the string {}, too long?", str));
            }

            if bts_low.high_bit_set() {
                return Err(format!("Did not understand the string {}, too long?", str));
            }

            if ch == '0' {
                bts_high = bts_high.shift_left();
                bts_low = bts_low.shift_left();
            } else if ch == '1' {
                bts_high = bts_high.push_1();
                bts_low = bts_low.push_1();
            } else if ch == 'X' {
                bts_high = bts_high.push_1();
                bts_low = bts_low.shift_left();
            } else if ch == 'x' {
                bts_high = bts_high.shift_left();
                bts_low = bts_low.push_1();
            } else if ch == '_' {
                continue;
            } else {
                return Err(format!("Did not understand the string {}, invalid character?", str));
            }
        } // end for ch

        Ok(SomeRegion::new(
            &SomeState::new(bts_high),
        &SomeState::new(bts_low),
        ))
    } // end new_from_string

    /// Return the expected length of a string representing a region, for string alloaction.
    pub fn formatted_string_length(&self) -> usize {
        (NUM_BITS_PER_INT * self.state1.num_ints()) + self.state1.num_ints()
    }

    /// Return a String representation of a Region without any prefix.
    pub fn formatted_string(&self) -> String {

        let mut s1 = String::with_capacity(self.formatted_string_length());
        s1.push('r');
        let num_ints = self.state1.num_ints();
        let num_bits = num_ints * NUM_BITS_PER_INT;

        let mut inx = 0;
        for valb in (0..num_bits).rev() {
            if inx > 0 && inx % NUM_BITS_PER_INT == 0 {
                s1.push('_');
            }
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
                    s1.push('x');
                } else {
                    s1.push('0');
                }
            }
            inx += 1;
            // println!("a bit is: {} b0 set {} b1 set {} s1: {}", valb, b0, b1, s1);
        }
        s1
    }

    /// Return true if two regions are adjacent.
    pub fn is_adjacent(&self, other: &Self) -> bool {

        self.diff_mask(&other).just_one_bit()
    }

    /// Return true if a region is adjacent to a state.
    pub fn is_adjacent_state(&self, other: &SomeState) -> bool {

        self.diff_mask_state(&other).just_one_bit()
    }

    /// Return true if two regions intersect.
    pub fn intersects(&self, other: &Self) -> bool {

        self.diff_mask(&other).is_low()
    }

    /// Return the intersection of two regions.
    /// Check regions for intersection first.
    /// Strangely, the intersection of two adjacent regions produces
    /// most of an overlapping part, except for a 0/1 pair that needs to be changed
    /// to X.
    pub fn intersection(&self, other: &Self) -> Self {

        Self::new(
            &self.high_mask().m_and(&other.high_mask()).to_state(),
            &self.low_mask().m_and(&other.low_mask()).m_not().to_state(),
        )
    }

    /// Return true if a region is a superset of a state.
    pub fn is_superset_of_state(&self, a_state: &SomeState) -> bool {

        let t1 = self
            .state1
            .s_xor(&a_state)
            .s_and(&self.state2.s_xor(&a_state)).to_mask();

        t1.is_low()
    }
    
    /// Return a Mask of zero bits.
    pub fn zeros_mask(&self) -> SomeMask {

        self.state1.s_not().s_and(&self.state2.s_not()).to_mask()
    }

    /// Return a Mask of one bits.
    pub fn ones_mask (&self) -> SomeMask {

        self.state1.s_and(&self.state2).to_mask()
    }
    /// Return mask of x bits.
    pub fn x_mask(&self) -> SomeMask {

        self.state1.s_xor(&self.state2).to_mask()
    }

    /// Return the number of X bits in a region.
    pub fn num_x(&self) -> usize {

        self.state1.distance(&self.state2)
    }

    /// Given a state in a region, return the far state in the region.
    pub fn far_state(&self, sta: &SomeState) -> SomeState {
        self.state1.s_xor(&self.state2).s_xor(&sta)
    }

    /// Given a region, and a proper subset region, return the
    /// far region within the superset region.
    pub fn far_reg(&self, other: &SomeRegion) -> SomeRegion {

        let int_x_msk = self.x_mask();

        let ok_x_msk = other.x_mask();

        assert!(ok_x_msk.is_subset_of(&int_x_msk));

        // Get bit(s) to use to calculate a far-sub-region in reg_int from ok_reg
        // by changing reg_int X over ok_reg 1 to 0 over 1, or reg_int X over ok_reg 0 to 1 over 0
        let cng_bits = int_x_msk.m_and(&ok_x_msk.m_not()).to_state();

        SomeRegion::new(
            &other.state1.s_xor(&cng_bits),
            &other.state2.s_xor(&cng_bits),
        )
    }

    /// Return true if a region is a subset on another region.
    pub fn is_subset_of(&self, other: &Self) -> bool {

        if self.intersects(&other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_subset_of(&x2);
        }
        false
    }

    /// Return true if a region is a superset on another region.
    pub fn is_superset_of(&self, other: &Self) -> bool {

        if self.intersects(&other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_superset_of(&x2);
        }
        false
    }

    /// Return the union of two regions.
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

    /// Return the union of a region and a state.
    pub fn _union_state(&self, other: &SomeState) -> Self {

        let st_low = self.state1.s_and(&self.state2).s_and(&other);

        let st_high = self.state1.s_or(&self.state2).s_or(&other);

        Self::new(&st_high, &st_low)
    }

    /// Return a Mask of zero or X bits (which include a zero).
    pub fn low_mask(&self) -> SomeMask {

        self.state1.s_not().s_or(&self.state2.s_not()).to_mask()
    }

    /// Return a Mask of ones or X bits (which include a one).
    pub fn high_mask(&self) -> SomeMask {

        self.state1.s_or(&self.state2).to_mask()
    }

    /// Return a region with masked X-bits set to zeros.
    pub fn set_to_zeros(&self, msk: &SomeMask) -> Self {

        let smsk = msk.to_state();
        Self::new(
            &self.state1.s_and(&smsk.s_not()),
            &self.state2.s_and(&smsk.s_not()),
        )
    }

    /// Return a region with masked X-bits set to ones.
    pub fn set_to_ones(&self, msk: &SomeMask) -> Self {

        let smsk = msk.to_state();
        Self::new(
            &self.state1.s_or(&smsk),
            &self.state2.s_or(&smsk),
        )
    }

    /// Return a region with masked X-bits set to zeros.
    /// The region states may not represent a samples state.
    pub fn set_to_x(&self, msk: &SomeMask) -> Self {

        let smsk = msk.to_state();
        Self::new(
            &self.state1.s_or(&smsk),
            &self.state2.s_and(&smsk.s_not()),
            )
    }

    /// Return a mask of different bit with a given state.
    pub fn diff_mask_state(&self, sta1: &SomeState) -> SomeMask {

        self.state1.s_xor(&sta1).s_and(&self.state2.s_xor(&sta1)).to_mask()
    }

    /// Return a mask of different (non-x) bits between two regions.
    pub fn diff_mask(&self, reg1: &SomeRegion) -> SomeMask {

        self.diff_mask_state(&reg1.state1)
            .m_and(&self.diff_mask_state(&reg1.state2))
    }

    /// Return the number of different (non-x) bits with another region.
    pub fn distance(&self, reg1: &SomeRegion) -> usize {

        self.diff_mask_state(&reg1.state1)
            .m_and(&self.diff_mask_state(&reg1.state2))
            .num_one_bits()
    }

    /// Return states in a region, given a list of states.
    pub fn states_in(&self, stas: &StateStore) -> StateStore {

        let mut stsin = StateStore::new();

        for stax in stas.iter() {
            if self.is_superset_of_state(stax) {
                stsin.push(stax.clone());
            }
        } // next stax

        stsin
    }

    /// Given two adjacent regions, return an overlapping region.
    pub fn overlapping_part(&self, other: &Self) -> Self {

        assert!(self.is_adjacent(&other));

        let adj_bit = self.diff_mask(&other);

        self.intersection(&other).set_to_x(&adj_bit) // a strange use of the intersection logic
    }

    /// Toggle non-x bits in a region, given a mask.
//    pub fn toggle_bits(&self, tbits: &SomeMask) -> Self {
//        let stxor = tbits.to_state();
//        Self::new(&self.state1.s_xor(&stxor), &self.state2.s_xor(&stxor))
//    }

    /// Given a set of states (square keys),
    ///
    /// Return a StateStore containing  pairs of states,
    /// found to encompass the region.
    ///
    /// An empty result is possible.
    pub fn defining_pairs(&self, stas: &StateStore) -> StateStore {

        // Initialize the StateStore
        let mut store = StateStore::with_capacity(2);

        // Check each possible combination of two states
        for inx in 0..stas.len() {
            for iny in (inx + 1)..stas.len() {
                if SomeRegion::new(&stas[inx], &stas[iny]) == *self {
                    store.push(stas[inx].clone());
                    store.push(stas[iny].clone());
                    //println!("regx {} equals regy {}", &self, SomeRegion::new(&stas[inx], &stas[iny]));
                }
            }
        }
        store
    }

    /// Given a region, and a second region, return the
    /// first region - the second
    pub fn subtract(&self, other: &SomeRegion) -> Vec<Self> {

        let mut ret_vec = Vec::<Self>::new();

        if self.intersects(&other) == false {
            ret_vec.push(self.clone());
            return ret_vec;
        }

        let reg_int = self.intersection(&other);

        let x_over_not_xs: Vec<SomeMask> = self.x_mask().m_and(&reg_int.x_mask().m_not()).split();

        for mskx in x_over_not_xs.iter() {
            if mskx.m_and(&reg_int.state1.to_mask()).is_low() {
                // reg_int has a 0 bit in that position
                ret_vec.push(self.set_to_ones(mskx));
            } else {
                // reg_int has a 1 in that bit position
                ret_vec.push(self.set_to_zeros(mskx));
            }
        }
        ret_vec
    }

    // Return the number of integers used in the states that make up the region
    pub fn num_ints(&self) -> usize {
        self.state1.num_ints()
    }

} // end impl SomeRegion

impl Clone for SomeRegion {
    fn clone(&self) -> Self {
        Self {
            state1: self.state1.clone(),
            state2: self.state2.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::region::SomeRegion;
    use crate::regionstore::RegionStore;
    
    // Test X10X - 0XX1 = X100, 110X.
    #[test]
    fn region_subtraction() -> Result<(), String> {

        let reg0 = SomeRegion::new_from_string(1, "rX10X").unwrap();

        let reg1 = SomeRegion::new_from_string(1, "r0XX1").unwrap();

        let regvec = reg0.subtract(&reg1);
        
        let mut regs = RegionStore::new();
        for regx in &regvec {
            regs.push(regx.clone());
        }

        if regs.len() != 2 {
            return Err(format!("{} minus {} = {} ??", &reg0, &reg1, &regs.formatted_string()));
        }

        if regs.contains(&SomeRegion::new_from_string(1, "rX100").unwrap()) {
        } else {
            return Err(format!("{} minus {} = {} ??", &reg0, &reg1, &regs.formatted_string()));
        }

        if regs.contains(&SomeRegion::new_from_string(1, "r110X").unwrap()) {
        } else {
            return Err(format!("{} minus {} = {} ??", &reg0, &reg1, &regs.formatted_string()));
        }

        Ok(())
    }



}
