//! The SomeRegion struct, representing a region on a pseudo Karnaugh Map.
//!
//! Uses two states (they can be the same) to represent a region, which includes every state between them.
//!
//! The two states used to make the region, can be keys to two squares.

use crate::bits::{SomeBits, NUM_BITS_PER_INT};
use crate::mask::SomeMask;
use crate::rule::SomeRule;
use crate::state::SomeState;
use crate::statestore::StateStore;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
/// SomeRegion struct
pub struct SomeRegion {
    /// First state defining a region.
    pub state1: SomeState,
    /// Second state defining a region.
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

    /// Return a Region from a string and the number of integers to use.
    /// Left-most, consecutive, zeros can be omitted.
    ///
    /// if let Ok(regx) = SomeRegion::new_from_string(1, "r01x1")) {
    ///    println!("Region {}", &regx);
    /// } else {
    ///    panic!("Invalid Region");
    /// }
    ///
    /// A state string can be used, like "s101010" or s0x34", making
    /// a region with no X bit positions.
    pub fn new_from_string(num_ints: usize, str: &str) -> Result<Self, String> {
        let mut bts_high = SomeBits::new_low(num_ints);

        let mut bts_low = SomeBits::new_low(num_ints);

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
                    return Err(format!(
                        "Did not understand the string {}, first character?",
                        str
                    ));
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
                return Err(format!(
                    "Did not understand the string {}, invalid character?",
                    str
                ));
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

    // Return true if two regions are adjacent.
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
            &SomeState::new(self.high_state().bts.b_and(&other.high_state().bts)),
            &SomeState::new(self.low_state().bts.b_or(&other.low_state().bts)),
        )
    }

    /// Return true if a region is a superset of a state.
    pub fn is_superset_of_state(&self, a_state: &SomeState) -> bool {
        let t1 = SomeMask::new(
            self.state1
                .bts
                .b_xor(&a_state.bts)
                .b_and(&self.state2.bts.b_xor(&a_state.bts)),
        );

        t1.is_low()
    }

    /// Return a Mask of zero positions.
    pub fn zeros_mask(&self) -> SomeMask {
        SomeMask::new(self.state1.bts.b_not().b_and(&self.state2.bts.b_not()))
    }

    /// Return a Mask of one positions.
    pub fn ones_mask(&self) -> SomeMask {
        SomeMask::new(self.state1.bts.b_and(&self.state2.bts))
    }
    /// Return mask of x positions.
    pub fn x_mask(&self) -> SomeMask {
        SomeMask::new(self.state1.bts.b_xor(&self.state2.bts))
    }

    /// Return the number of X bits in a region.
    pub fn num_x(&self) -> usize {
        self.state1.distance(&self.state2)
    }

    /// Given a state in a region, return the far state in the region.
    pub fn far_state(&self, sta: &SomeState) -> SomeState {
        SomeState::new(self.state1.bts.b_xor(&self.state2.bts).b_xor(&sta.bts))
    }

    /// Given a region, and a proper subset region, return the
    /// far region within the superset region.
    pub fn far_reg(&self, other: &SomeRegion) -> SomeRegion {
        let int_x_msk = self.x_mask();

        let ok_x_msk = other.x_mask();

        assert!(ok_x_msk.is_subset_of(&int_x_msk));

        // Get bit(s) to use to calculate a far-sub-region in reg_int from ok_reg
        // by changing reg_int X over ok_reg 1 to 0 over 1, or reg_int X over ok_reg 0 to 1 over 0
        let cng_bits = int_x_msk.m_and(&ok_x_msk.m_not());

        SomeRegion::new(
            &SomeState::new(other.state1.bts.b_xor(&cng_bits.bts)),
            &SomeState::new(other.state2.bts.b_xor(&cng_bits.bts)),
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
        let st_low = SomeState::new(self.low_state().bts.b_and(&other.low_state().bts));

        let st_high = SomeState::new(self.high_state().bts.b_or(&other.high_state().bts));

        Self::new(&st_high, &st_low)
    }

    /// Return the union of a region and a state.
    pub fn union_state(&self, other: &SomeState) -> Self {
        let st_low = SomeState::new(self.low_state().bts.b_and(&other.bts));

        let st_high = SomeState::new(self.high_state().bts.b_or(&other.bts));

        Self::new(&st_high, &st_low)
    }

    /// Return the highest state in the region
    pub fn high_state(&self) -> SomeState {
        SomeState::new(self.state1.bts.b_or(&self.state2.bts))
    }

    /// Return lowest state in the region
    pub fn low_state(&self) -> SomeState {
        SomeState::new(self.state1.bts.b_and(&self.state2.bts))
    }

    /// Return a region with masked X-bits set to zeros.
    pub fn set_to_zeros(&self, msk: &SomeMask) -> Self {
        Self::new(
            &SomeState::new(self.state1.bts.b_and(&msk.bts.b_not())),
            &SomeState::new(self.state2.bts.b_and(&msk.bts.b_not())),
        )
    }

    /// Return a region with masked X-bits set to ones.
    pub fn set_to_ones(&self, msk: &SomeMask) -> Self {
        Self::new(
            &SomeState::new(self.state1.bts.b_or(&msk.bts)),
            &SomeState::new(self.state2.bts.b_or(&msk.bts)),
        )
    }

    // Return a region with masked X-bits set to zeros.
    // The region states may not represent a samples state.
    //    pub fn set_to_x(&self, msk: &SomeMask) -> Self {
    //        Self::new(
    //            &SomeState::new(self.state1.bts.b_or(&msk.bts)),
    //            &SomeState::new(self.state2.bts.b_and(&msk.bts.b_not())),
    //        )
    //    }

    /// Return a mask of different bit with a given state.
    fn diff_mask_state(&self, sta1: &SomeState) -> SomeMask {
        SomeMask::new(
            self.state1
                .bts
                .b_xor(&sta1.bts)
                .b_and(&self.state2.bts.b_xor(&sta1.bts)),
        )
    }

    /// Return a mask of different (non-x) bits between two regions.
    pub fn diff_mask(&self, reg1: &SomeRegion) -> SomeMask {
        self.diff_mask_state(&reg1.state1)
            .m_and(&self.diff_mask_state(&reg1.state2))
    }

    // Return the number of different (non-x) bits with another region.
    //    pub fn distance(&self, reg1: &SomeRegion) -> usize {
    //        self.diff_mask(&reg1).num_one_bits()
    //    }

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
            if mskx.bts.b_and(&reg_int.state1.bts).is_low() {
                // reg_int has a 0 bit in that position
                ret_vec.push(self.set_to_ones(mskx));
            } else {
                // reg_int has a 1 in that bit position
                ret_vec.push(self.set_to_zeros(mskx));
            }
        }
        ret_vec
    }

    /// Return the number of integers used in the states that make up the region
    pub fn num_ints(&self) -> usize {
        self.state1.num_ints()
    }

    /// Return a rule for moving from one region to another.
    /// The result of the rule may be equal, or subset, of the second region.
    pub fn rule_to_region(&self, to: &SomeRegion) -> Option<SomeRule> {
        // Check for rule that needs no change.
        if to.is_superset_of(self) {
            // Could return a rule that does nothing, but this probably only
            // happens from a logic problem.
            return None;
        }

        let f_ones = SomeMask::new(self.state1.bts.b_or(&self.state2.bts));
        let f_zeros = SomeMask::new(self.state1.bts.b_not().b_or(&self.state2.bts.b_not()));

        let t_ones = SomeMask::new(to.state1.bts.b_or(&to.state2.bts));
        let t_zeros = SomeMask::new(to.state1.bts.b_not().b_or(&to.state2.bts.b_not()));

        let to_not_x = to.x_mask().m_not();

        Some(SomeRule::new_from_masks(
            // b00
            f_zeros.m_and(&t_zeros),
            // b01
            f_zeros.m_and(&t_ones).m_and(&to_not_x),
            // b11
            f_ones.m_and(&t_ones),
            // b10
            f_ones.m_and(&t_zeros).m_and(&to_not_x),
        ))
    }
} // end impl SomeRegion

#[cfg(test)]
mod tests {
    use super::*;
    use crate::randompick::RandomPick;
    use crate::regionstore::RegionStore;

    // Test new_from_string, using randomly chosen digits.
    #[test]
    fn new_from_string() -> Result<(), String> {
        // Init possible hexadecimal digits.
        let chars = "01Xx01Xx01Xx01Xx";

        // Check 16 times.
        for _ in 0..16 {
            let mut inxs = RandomPick::new(16);

            // Init strings
            let mut reg_from_str = String::from("r");
            let mut reg_expected_str = String::from("r");

            for x in 0..16 {
                if let Some(inx) = inxs.pick() {
                    // Add to the source string of a bits instance.
                    reg_from_str.push(chars.as_bytes()[inx] as char);

                    // Add to the expected output of instance.formatted_string()
                    if x > 0 && x % 8 == 0 {
                        reg_expected_str.push('_');
                    }
                    reg_expected_str.push(chars.as_bytes()[inx] as char);
                }
            }

            // Get new bits instance.
            let reg_instance = SomeRegion::new_from_string(2, &reg_from_str).unwrap();

            // Get string from bits instance.
            let reg_instance_str = reg_instance.formatted_string();

            // Compare the bits string and predicted string.
            match reg_instance_str == reg_expected_str {
                true => {
                    println!("reg   {} instance", reg_instance_str);
                    println!("equal {} expected", reg_expected_str);
                }
                _ => {
                    return Err(format!(
                        "reg {} instance not equal {} expected!",
                        reg_instance_str, reg_expected_str
                    ))
                }
            }
        }
        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let reg1 = SomeRegion::new(&SomeState::new_from_string(1, "s1010").unwrap(), &SomeState::new_from_string(1, "s0101").unwrap());
        let reg2 = SomeRegion::new(&SomeState::new_from_string(1, "s0001").unwrap(), &SomeState::new_from_string(1, "s1110").unwrap());
        assert!(reg1.eq(&reg2));

        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let mut reg0 = SomeRegion::new_from_string(1, "r101XX1").unwrap();
        let mut reg1 = SomeRegion::new_from_string(1, "rXX0011").unwrap();
        assert!(reg0.is_adjacent(&reg1));
        
        reg0 = SomeRegion::new_from_string(1, "rX10X01X").unwrap();
        reg1 = SomeRegion::new_from_string(1, "rX10X10X").unwrap();
        assert!(reg0.is_adjacent(&reg1) == false);

        Ok(())
    }

    #[test]
    fn is_adjacent_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X").unwrap();
        let sta1 = SomeState::new_from_string(1, "s1001100").unwrap();
        println!("{}", &reg0);
        println!("{}", &sta1);
        println!("{}", &reg0.diff_mask_state(&sta1));
        if reg0.is_adjacent_state(&sta1) == false {
            return Err(format!("Result 1 False?"));
        }

        let sta2 = SomeState::new_from_string(1, "s1001110").unwrap();
        if reg0.is_adjacent_state(&sta2) {
            return Err(format!("Result 2 True?"));
        }
        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X").unwrap();
        let reg1 = SomeRegion::new_from_string(1, "r0XX110X").unwrap();

        if reg0.intersects(&reg1) == false {
            return Err(format!("Result 1 False?"));
        }

        let reg2 = SomeRegion::new_from_string(1, "r0XX111X").unwrap();
        if reg0.intersects(&reg2) {
            return Err(format!("Result 2 True?"));
        }
        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X").unwrap();

        let reg1 = SomeRegion::new_from_string(1, "r0XX110X").unwrap();

        if reg0.intersection(&reg1) != SomeRegion::new_from_string(1, "r010110X").unwrap() {
            return Err(format!("Result not r010110X?"));
        }
        Ok(())
    }

    #[test]
    fn is_superset_of_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X").unwrap();

        let sta1 = SomeState::new_from_string(1, "s1100").unwrap();
        if reg0.is_superset_of_state(&sta1) == false {
            return Err(format!("Result 1 False?"));
        }

        let sta2 = SomeState::new_from_string(1, "s0000").unwrap();
        if reg0.is_superset_of_state(&sta2) {
            return Err(format!("Result 2 True?"));
        }

        let sta3 = SomeState::new_from_string(1, "s0010").unwrap();
        if reg0.is_superset_of_state(&sta3) {
            return Err(format!("Result 2 True?"));
        }
        Ok(())
    }

    #[test]
    fn zeros_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101").unwrap();
        if reg0.zeros_mask() != SomeMask::new_from_string(1, "m11001010").unwrap() {
            return Err(format!("Result not m11001010?"));
        }
        Ok(())
    }

    #[test]
    fn ones_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101").unwrap();

        if reg0.ones_mask() != SomeMask::new_from_string(1, "m101").unwrap() {
            return Err(format!("Result not m101?"));
        }
        Ok(())
    }

    #[test]
    fn x_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101").unwrap();
        if reg0.x_mask() != SomeMask::new_from_string(1, "m110000").unwrap() {
            return Err(format!("Result not m110000?"));
        }
        Ok(())
    }

    #[test]
    fn far_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXXX1").unwrap();
        let state0 = SomeState::new_from_string(1, "s1011").unwrap();

        if reg0.far_state(&state0) != SomeState::new_from_string(1, "s101").unwrap() {
            return Err(format!("Result not s101?"));
        }
        Ok(())
    }

    #[test]
    fn far_reg() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(2, "rXXX01000000").unwrap();
        let reg1 = SomeRegion::new_from_string(2, "r01X01000000").unwrap();

        if reg0.far_reg(&reg1) != SomeRegion::new_from_string(2, "r10X01000000").unwrap() {
            return Err(format!("Result not r10X01000000?"));
        }
        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X").unwrap();

        let reg1 = SomeRegion::new_from_string(1, "rX10X").unwrap();

        if reg0.is_subset_of(&reg1) == false {
            return Err(format!("Result 1 False?"));
        }

        let reg2 = SomeRegion::new_from_string(1, "rXXXX").unwrap();

        if reg0.is_subset_of(&reg2) == false {
            return Err(format!("Result 2 False?"));
        }

        if reg2.is_subset_of(&reg0) {
            return Err(format!("Result 3 True?"));
        }
        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X").unwrap();

        let reg1 = SomeRegion::new_from_string(1, "rX10X").unwrap();

        if reg0.is_superset_of(&reg1) == false {
            return Err(format!("Result 1 False?"));
        }

        let reg2 = SomeRegion::new_from_string(1, "rXXXX").unwrap();

        if reg2.is_superset_of(&reg0) == false {
            return Err(format!("Result 2 False?"));
        }

        if reg0.is_superset_of(&reg2) {
            return Err(format!("Result 3 True?"));
        }
        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(2, "r111000XXX").unwrap();
        let reg1 = SomeRegion::new_from_string(2, "r01X01X01X").unwrap();

        if reg0.union(&reg1) != SomeRegion::new_from_string(2, "rX1X0XXXXX").unwrap() {
            return Err(format!("Result not rX1X0XXXXX?"));
        }
        Ok(())
    }

    #[test]
    fn union_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101").unwrap();
        let state0 = SomeState::new_from_string(1, "s101001").unwrap();

        if reg0.union_state(&state0) != SomeRegion::new_from_string(1, "rXXXX01").unwrap() {
            return Err(format!("Result not rXXXX01?"));
        }
        Ok(())
    }

    #[test]
    fn high_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX0X1").unwrap();

        if reg0.high_state() != SomeState::new_from_string(1, "s1011").unwrap() {
            return Err(format!("Result s1011?"));
        }
        Ok(())
    }

    #[test]
    fn low_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX0X1").unwrap();

        if reg0.low_state() != SomeState::new_from_string(1, "s1").unwrap() {
            println!("low state {}", &reg0.low_state());
            return Err(format!("Result not s1?"));
        }
        Ok(())
    }

    #[test]
    fn set_to_zeros() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X").unwrap();
        let msk1 = SomeMask::new_from_string(1, "m111").unwrap();

        if reg0.set_to_zeros(&msk1) != SomeRegion::new_from_string(1, "rX10X000").unwrap() {
            return Err(format!("Result not rX10X000?"));
        }
        Ok(())
    }

    #[test]
    fn set_to_ones() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X").unwrap();
        let msk1 = SomeMask::new_from_string(1, "m111").unwrap();

        if reg0.set_to_ones(&msk1) != SomeRegion::new_from_string(1, "rX10X111").unwrap() {
            return Err(format!("Result not rX10X111?"));
        }
        Ok(())
    }

    #[test]
    fn diff_mask_state() -> Result<(), String> {
        let reg0  = SomeRegion::new_from_string(1, "rXX0011").unwrap();
        let state0 = SomeState::new_from_string(1, "s010101").unwrap();

        if reg0.diff_mask_state(&state0) != SomeMask::new_from_string(1, "m110").unwrap() {
            return Err(format!("Result not m110?"));
        }
        Ok(())
    }

    #[test]
    fn diff_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(2, "r1XX000111").unwrap();
        let reg1 = SomeRegion::new_from_string(2, "r01X01X01X").unwrap();
        let diff = reg0.diff_mask(&reg1);

        if diff != SomeMask::new_from_string(2, "m100010100").unwrap() {
            return Err(format!("Result {} not m100010100?", diff));
        }
        Ok(())
    }

    #[test]
    fn states_in() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX1XX").unwrap();
        let st1 = SomeState::new_from_string(1, "s1").unwrap();
        let st6 = SomeState::new_from_string(1, "s110").unwrap();
        let st7 = SomeState::new_from_string(1, "s0111").unwrap();
        let st8 = SomeState::new_from_string(1, "s1000").unwrap();
        let std = SomeState::new_from_string(1, "s1101").unwrap();
        let ste = SomeState::new_from_string(1, "s1110").unwrap();
        let mut sta_str = StateStore::new();

        sta_str.push(st1);
        sta_str.push(st6.clone());
        sta_str.push(st7.clone());
        sta_str.push(st8);
        sta_str.push(std.clone());
        sta_str.push(ste.clone());

        let sta_str2 = reg0.states_in(&sta_str);
        assert!(sta_str2.len() == 4);

        assert!(sta_str2.contains(&st6));
        assert!(sta_str2.contains(&st7));
        assert!(sta_str2.contains(&std));
        assert!(sta_str2.contains(&ste));

        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X").unwrap();

        let reg1 = SomeRegion::new_from_string(1, "r0XX1").unwrap();

        let regvec = reg0.subtract(&reg1);

        let mut regs = RegionStore::new();
        for regx in &regvec {
            regs.push(regx.clone());
        }

        if regs.len() != 2 {
            return Err(format!(
                "{} minus {} = {} ??",
                &reg0,
                &reg1,
                &regs.formatted_string()
            ));
        }

        if regs.contains(&SomeRegion::new_from_string(1, "rX100").unwrap()) {
        } else {
            return Err(format!(
                "{} minus {} = {} ??",
                &reg0,
                &reg1,
                &regs.formatted_string()
            ));
        }

        if regs.contains(&SomeRegion::new_from_string(1, "r110X").unwrap()) {
        } else {
            return Err(format!(
                "{} minus {} = {} ??",
                &reg0,
                &reg1,
                &regs.formatted_string()
            ));
        }
        Ok(())
    }

    // Test num_x.
    #[test]
    fn num_x() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101").unwrap();

        if reg0.num_x() != 2 {
            return Err(format!("Result not 2?"));
        }
        Ok(())
    }

    #[test]
    fn rule_to_region() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string(2, "r000_111_xxx").unwrap();
        let reg2 = SomeRegion::new_from_string(2, "r01x_01x_01x").unwrap();

        let rul1 = reg1.rule_to_region(&reg2).unwrap();
        println!("rule is {}", &rul1);
        assert!(reg2.is_superset_of(&rul1.result_from_initial_region(&reg1)));

        // Test proper subset region.
        let reg1 = SomeRegion::new_from_string(1, "r0011").unwrap();
        let reg2 = SomeRegion::new_from_string(1, "rx01x").unwrap();
        if let Some(_) = reg1.rule_to_region(&reg2) {
            return Err(format!("Result not None?"));
        }

        // Test intersecting regions.
        let reg1 = SomeRegion::new_from_string(1, "r010x").unwrap();
        let reg2 = SomeRegion::new_from_string(1, "rx1x1").unwrap();
        let rul1 = reg1.rule_to_region(&reg2).unwrap();
        println!("rul1 {}", &rul1);
        assert!(rul1.result_region() == SomeRegion::new_from_string(1, "r0101").unwrap());

        Ok(())
    }

} // end tests
