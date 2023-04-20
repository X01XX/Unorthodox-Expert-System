//! The SomeRegion struct, representing a region on a pseudo Karnaugh Map.
//!
//! Uses two states (they can be the same) to represent a region, which includes every state between them.
//!
//! The two states used to make the region, can be keys to two squares.

use crate::mask::SomeMask;
use crate::state::SomeState;
use crate::statestore::StateStore;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

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

/// Implement the fmt::Display trait.
impl fmt::Display for SomeRegion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

/// Implement the PartialEq trait, since two SomeRegion structs may be the same while defined
/// by different states.
impl PartialEq for SomeRegion {
    fn eq(&self, other: &Self) -> bool {
        if self.intersects(other) {
            return self.x_mask() == other.x_mask();
        }
        false
    }
}
impl Eq for SomeRegion {}

impl SomeRegion {
    /// Create new region from two states.
    ///
    /// For a region used to define a group, the states have corresponding squares that have been sampled.
    pub fn new(sta1: SomeState, sta2: SomeState) -> Self {
        Self {
            state1: sta1,
            state2: sta2,
        }
    }

    /// Return a Region from a string and the number of integers to use.
    /// Left-most, consecutive, positions that are omitted will be padded with zeros.
    ///
    /// if let Ok(regx) = SomeRegion::new_from_string(1, "r01x1")) {
    ///    println!("Region {}", &regx);
    /// } else {
    ///    panic!("Invalid Region");
    /// }
    ///
    /// A state string can be used, like "s101010" or s0x34", making
    /// a region with no X bit positions.
    ///
    /// The case of an X bit position gives information about the two states that form the region.
    /// X = (1, 0).
    /// x = (0, 1).
    /// XxXx = (1010, 0101).
    pub fn new_from_string(num_ints: usize, str: &str) -> Result<Self, String> {
        assert!(num_ints > 0);
        let mut msk_high = SomeMask::new_low(num_ints);

        let mut msk_low = SomeMask::new_low(num_ints);

        let mut num_bits = 0;

        for (inx, chr) in str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "r" {
                    continue;
                } else if chr == "s" {
                    let state_r = SomeState::new_from_string(num_ints, str);
                    match state_r {
                        Ok(a_state) => {
                            return Ok(SomeRegion::new(a_state.clone(), a_state));
                        }
                        Err(error) => {
                            return Err(error);
                        }
                    } // end match state_r
                } else {
                    return Err(format!(
                        "Did not understand the string {str}, first character?"
                    ));
                }
            }

            if chr == "0" {
                msk_high = msk_high.shift_left();
                msk_low = msk_low.shift_left();
            } else if chr == "1" {
                msk_high = msk_high.push_1();
                msk_low = msk_low.push_1();
            } else if chr == "X" {
                msk_high = msk_high.push_1();
                msk_low = msk_low.shift_left();
            } else if chr == "x" {
                msk_high = msk_high.shift_left();
                msk_low = msk_low.push_1();
            } else if chr == "_" {
                continue;
            } else {
                return Err(format!(
                    "Did not understand the string {str}, invalid character?"
                ));
            }
            num_bits += 1;
        } // end for ch

        if num_bits > (num_ints * crate::bits::NUM_BITS_PER_INT) {
            return Err(format!("String {str}, too long?"));
        }

        Ok(SomeRegion::new(msk_high.to_state(), msk_low.to_state()))
    } // end new_from_string

    /// Return the number of integers used to implement a region.
    pub fn num_ints(&self) -> usize {
        self.state1.num_ints()
    }

    /// Return a Region from a string and the number of integers to use.
    /// Left-most, consecutive, positions that are omitted will be padded with Xs.
    ///
    /// if let Ok(regx) = SomeRegion::new_from_string(1, "r01x1")) {
    ///    println!("Region {}", &regx);
    /// } else {
    ///    panic!("Invalid Region");
    /// }
    ///
    /// A state string can be used, like "s0b101010" or s0x34", making
    /// a region with no X bit positions.
    pub fn new_from_string_pad_x(num_ints: usize, str: &str) -> Result<Self, String> {
        assert!(num_ints > 0);
        let mut msk_high_not = SomeMask::new_low(num_ints);

        let mut msk_low = SomeMask::new_low(num_ints);

        for (inx, chr) in str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "r" {
                    continue;
                } else if chr == "s" {
                    let state_r = SomeState::new_from_string(num_ints, str);
                    match state_r {
                        Ok(a_state) => {
                            return Ok(SomeRegion::new(a_state.clone(), a_state));
                        }
                        Err(error) => {
                            return Err(error);
                        }
                    } // end match state_r
                } else {
                    return Err(format!(
                        "Did not understand the string {str}, first character?"
                    ));
                }
            }

            if chr == "0" {
                msk_high_not = msk_high_not.push_1();
                msk_low = msk_low.shift_left();
            } else if chr == "1" {
                msk_high_not = msk_high_not.shift_left();
                msk_low = msk_low.push_1();
            } else if chr == "X" {
                msk_high_not = msk_high_not.shift_left();
                msk_low = msk_low.shift_left();
            } else if chr == "x" {
                msk_high_not = msk_high_not.push_1();
                msk_low = msk_low.push_1();
            } else if chr == "_" {
                continue;
            } else {
                return Err(format!(
                    "Did not understand the string {str}, invalid character?"
                ));
            }
        } // next (inx, chr)

        Ok(SomeRegion::new(
            msk_high_not.bitwise_not().to_state(),
            msk_low.to_state(),
        ))
    } // end new_from_string_pad_x

    /// Return the expected length of a string representing a region, for string alloaction.
    pub fn formatted_string_length(&self) -> usize {
        self.state1.num_bits() + 1 + (self.state1.num_bits() / 4)
    }

    /// Return a String representation of a Region without any prefix.
    pub fn formatted_string(&self) -> String {
        let mut s1 = String::with_capacity(self.formatted_string_length());
        s1.push('r');

        let num_bits = self.state1.num_bits();

        for (inx, valb) in (0..num_bits).rev().enumerate() {
            if inx > 0 && inx % 4 == 0 {
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
            } else if b1 {
                s1.push('x');
            } else {
                s1.push('0');
            }
            // println!("a bit is: {} b0 set {} b1 set {} s1: {}", valb, b0, b1, s1);
        }
        s1
    }

    /// Return true if two regions are adjacent.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        self.diff_mask(other).just_one_bit()
    }

    /// Return true if a region is adjacent to a state.
    pub fn is_adjacent_state(&self, other: &SomeState) -> bool {
        self.diff_mask_state(other).just_one_bit()
    }

    /// Return true if two regions intersect.
    pub fn intersects(&self, other: &Self) -> bool {
        self.diff_mask(other).is_low()
    }

    /// Return the intersection of two regions.
    /// Check regions for intersection first.
    /// Strangely, the intersection of two adjacent regions produces
    /// most of an overlapping part, except for a 0/1 pair that needs to be changed
    /// to X.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        if !self.intersects(other) {
            return None;
        }
        Some(Self::new(
            self.high_state().bitwise_and(&other.high_state()),
            self.low_state().bitwise_or(&other.low_state()),
        ))
    }

    /// Return true if a region is a superset of a state.
    pub fn is_superset_of_state(&self, a_state: &SomeState) -> bool {
        self.state1
            .bitwise_xor(a_state)
            .bitwise_and(&self.state2.bitwise_xor(a_state))
            .to_mask()
            .is_low()
    }

    /// Return a Mask of zero positions.
    pub fn zeros_mask(&self) -> SomeMask {
        self.state1
            .bitwise_not()
            .bitwise_and(&self.state2.bitwise_not())
            .to_mask()
    }

    /// Return a Mask of one positions.
    pub fn ones_mask(&self) -> SomeMask {
        self.state1.bitwise_and(&self.state2).to_mask()
    }

    /// Return a mask of edge (non-X) bits.
    pub fn edge_mask(&self) -> SomeMask {
        self.state1.bitwise_eqv(&self.state2)
    }

    /// Return mask of x positions.
    pub fn x_mask(&self) -> SomeMask {
        self.state1.bitwise_xor(&self.state2).to_mask()
    }

    /// Return the number of X bits in a region.
    pub fn num_x(&self) -> usize {
        self.state1.distance(&self.state2)
    }

    /// Given a state in a region, return the far state in the region.
    pub fn far_state(&self, sta: &SomeState) -> SomeState {
        self.state1.bitwise_xor(&self.state2).bitwise_xor(sta)
    }

    /// Given a region, and a proper subset region, return the
    /// far region within the superset region.
    pub fn far_reg(&self, other: &SomeRegion) -> SomeRegion {
        let int_x_msk = self.x_mask();

        let ok_x_msk = other.x_mask();

        assert!(ok_x_msk.is_subset_of(&int_x_msk));

        // Get bit(s) to use to calculate a far-sub-region in reg_int from ok_reg
        // by changing reg_int X over ok_reg 1 to 0 over 1, or reg_int X over ok_reg 0 to 1 over 0
        let cng_bits = int_x_msk.bitwise_and(&ok_x_msk.bitwise_not());

        SomeRegion::new(
            other.state1.bitwise_xor(&cng_bits),
            other.state2.bitwise_xor(&cng_bits),
        )
    }

    /// Return true if a region is a subset on another region.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        if self.intersects(other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_subset_of(&x2);
        }
        false
    }

    /// Return true if a region is a superset on another region.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        if self.intersects(other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_superset_of(&x2);
        }
        false
    }

    /// Return the union of two regions.
    pub fn union(&self, other: &Self) -> Self {
        let st_low = self.low_state().bitwise_and(&other.low_state());

        let st_high = self.high_state().bitwise_or(&other.high_state());

        Self::new(st_high, st_low)
    }

    /// Return the union of a region and a state.
    pub fn union_state(&self, other: &SomeState) -> Self {
        let st_low = self.low_state().bitwise_and(other);

        let st_high = self.high_state().bitwise_or(other);

        Self::new(st_high, st_low)
    }

    /// Return the highest state in the region
    pub fn high_state(&self) -> SomeState {
        self.state1.bitwise_or(&self.state2)
    }

    /// Return lowest state in the region
    pub fn low_state(&self) -> SomeState {
        self.state1.bitwise_and(&self.state2)
    }

    /// Return a region with masked X-bits set to zeros.
    pub fn set_to_zeros(&self, msk: &SomeMask) -> Self {
        Self::new(
            self.state1.bitwise_and(&msk.bitwise_not()),
            self.state2.bitwise_and(&msk.bitwise_not()),
        )
    }

    /// Return a region with masked X-bits set to ones.
    pub fn set_to_ones(&self, msk: &SomeMask) -> Self {
        Self::new(self.state1.bitwise_or(msk), self.state2.bitwise_or(msk))
    }

    // Return a region with masked bit positions set to X.
    //    pub fn set_to_x(&self, msk: &SomeMask) -> Self {
    //        Self::new(
    //            self.state1.bitwise_or(msk),
    //            self.state2.bitwise_and(&msk.bitwise_not()),
    //        )
    //    }

    /// Return the distance from a region to a state.
    pub fn distance_state(&self, stax: &SomeState) -> usize {
        self.diff_mask_state(stax).num_one_bits()
    }

    /// Return a mask of different bits with a given state.
    pub fn diff_mask_state(&self, sta1: &SomeState) -> SomeMask {
        self.state1
            .bitwise_xor(sta1)
            .bitwise_and(&self.state2.bitwise_xor(sta1))
            .to_mask()
    }

    /// Return a mask of different (non-x) bits between two regions.
    pub fn diff_mask(&self, reg1: &SomeRegion) -> SomeMask {
        self.state1
            .bitwise_xor(&reg1.state1)
            .bitwise_and(
                &self.state1.bitwise_xor(&reg1.state2).bitwise_and(
                    &self
                        .state2
                        .bitwise_xor(&reg1.state2)
                        .bitwise_and(&self.state2.bitwise_xor(&reg1.state2)),
                ),
            )
            .to_mask()
    }

    // Return the number of different (non-x) bits with another region.
    //    pub fn distance(&self, reg1: &SomeRegion) -> usize {
    //        self.diff_mask(&reg1).num_one_bits()
    //    }

    /// Return states in a region, given a list of states.
    pub fn states_in(&self, stas: &StateStore) -> StateStore {
        let mut stsin = Vec::<SomeState>::new();

        for stax in stas.iter() {
            if self.is_superset_of_state(stax) {
                stsin.push(stax.clone());
            }
        } // next stax

        StateStore::new(stsin)
    }

    /// Given a region, and a second region, return the
    /// first region - the second
    pub fn subtract(&self, other: &SomeRegion) -> Vec<Self> {
        let mut ret_vec = Vec::<Self>::new();

        let Some(reg_int) = self.intersection(other) else {
            ret_vec.push(self.clone());
            return ret_vec;
        };

        let x_over_not_xs: Vec<SomeMask> = self.x_mask().bitwise_and(&reg_int.edge_mask()).split();

        for mskx in x_over_not_xs.iter() {
            if mskx.bitwise_and(&reg_int.state1).is_low() {
                // reg_int has a 0 bit in that position
                ret_vec.push(self.set_to_ones(mskx));
            } else {
                // reg_int has a 1 in that bit position
                ret_vec.push(self.set_to_zeros(mskx));
            }
        }
        ret_vec
    }

    /// Return a string representing a vector of regions.
    pub fn vec_ref_string(avec: &[&SomeRegion]) -> String {
        let mut rc_str = String::new();
        rc_str.push('[');

        for (inx, regx) in avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &regx));
        }

        rc_str.push(']');

        rc_str
    }
} // end impl SomeRegion

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regionstore::RegionStore;
    use rand::Rng;

    #[test]
    fn edge_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "r101XX0")?;
        let msk0 = SomeMask::new_from_string(1, "m0b1111_1001")?;
        let edges = reg0.edge_mask();
        if msk0 != edges {
            return Err(format!(
                "same bits {} not EQ to expected mask {}",
                edges, msk0
            ));
        }

        Ok(())
    }

    // Test new_from_string, using randomly chosen digits.
    #[test]
    fn new_from_string() -> Result<(), String> {
        let chars = ['0', '1', 'X', 'x']; // Possible chars to use.

        // Check 32 random regions.
        for _ in 0..32 {
            // Init region string
            let mut reg_from_str = String::from("r");

            for _ in 0..16 {
                let inx = rand::thread_rng().gen_range(0..100);
                // Add random character to string.
                reg_from_str.push(chars[inx % 4]);
            }

            // Get new bits instance.
            let reg_instance = SomeRegion::new_from_string(2, &reg_from_str)?;

            // Check for the expected states forming the region.
            let state1_str =
                "s0b".to_string() + &reg_from_str.replace("x", "0").replace("X", "1")[1..];
            let state1 = SomeState::new_from_string(2, &state1_str)?;
            let state2 = reg_instance.far_state(&state1);
            assert!(reg_instance.state1 == state1);
            assert!(reg_instance.state2 == state2);
        }
        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let reg1 = SomeRegion::new(
            SomeState::new_from_string(1, "s0b1010")?,
            SomeState::new_from_string(1, "s0b0101")?,
        );
        let reg2 = SomeRegion::new(
            SomeState::new_from_string(1, "s0b0001")?,
            SomeState::new_from_string(1, "s0b1110")?,
        );
        assert!(reg1.eq(&reg2));

        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let mut reg0 = SomeRegion::new_from_string(1, "r101XX1")?;
        let mut reg1 = SomeRegion::new_from_string(1, "rXX0011")?;
        assert!(reg0.is_adjacent(&reg1));

        reg0 = SomeRegion::new_from_string(1, "rX10X01X")?;
        reg1 = SomeRegion::new_from_string(1, "rX10X10X")?;
        assert!(!reg0.is_adjacent(&reg1));

        Ok(())
    }

    #[test]
    fn is_adjacent_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;
        let sta1 = SomeState::new_from_string(1, "s0b1001100")?;
        println!("{}", &reg0);
        println!("{}", &sta1);
        println!("{}", &reg0.diff_mask_state(&sta1));
        if !reg0.is_adjacent_state(&sta1) {
            return Err(String::from("Result 1 False?"));
        }

        let sta2 = SomeState::new_from_string(1, "s0b1001110")?;
        if reg0.is_adjacent_state(&sta2) {
            return Err(String::from("Result 2 True?"));
        }
        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;
        let reg1 = SomeRegion::new_from_string(1, "r0XX110X")?;

        if !reg0.intersects(&reg1) {
            return Err(String::from("Result 1 False?"));
        }

        let reg2 = SomeRegion::new_from_string(1, "r0XX111X")?;
        if reg0.intersects(&reg2) {
            return Err(String::from("Result 2 True?"));
        }
        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;

        let reg1 = SomeRegion::new_from_string(1, "r0XX110X")?;

        if let Some(reg_int) = reg0.intersection(&reg1) {
            if reg_int != SomeRegion::new_from_string(1, "r010110X")? {
                return Err(String::from("Result not r010110X?"));
            }
        } else {
            return Err("Regions should intersect".to_string());
        }
        Ok(())
    }

    #[test]
    fn is_superset_of_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X")?;

        let sta1 = SomeState::new_from_string(1, "s0b1100")?;
        if !reg0.is_superset_of_state(&sta1) {
            return Err(String::from("Result 1 False?"));
        }

        let sta2 = SomeState::new_from_string(1, "s0b0000")?;
        if reg0.is_superset_of_state(&sta2) {
            return Err(String::from("Result 2 True?"));
        }

        let sta3 = SomeState::new_from_string(1, "s0b0010")?;
        if reg0.is_superset_of_state(&sta3) {
            return Err(String::from("Result 2 True?"));
        }
        Ok(())
    }

    #[test]
    fn zeros_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101")?;
        let zmask = SomeMask::new_from_string(1, "m0b1010")?;

        if !zmask.is_subset_of(&reg0.zeros_mask()) {
            return Err(String::from("1010 not a subset?"));
        }
        Ok(())
    }

    #[test]
    fn ones_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101")?;

        if reg0.ones_mask() != SomeMask::new_from_string(1, "m0b101")? {
            return Err(String::from("Result not m0b101?"));
        }
        Ok(())
    }

    #[test]
    fn x_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101")?;
        if reg0.x_mask() != SomeMask::new_from_string(1, "m0b110000")? {
            return Err(String::from("Result not m0b110000?"));
        }
        Ok(())
    }

    #[test]
    fn far_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXXX1")?;
        let state0 = SomeState::new_from_string(1, "s0b1011")?;

        if reg0.far_state(&state0) != SomeState::new_from_string(1, "s0b101")? {
            return Err(String::from("Result not s0b101?"));
        }
        Ok(())
    }

    #[test]
    fn far_reg() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(2, "rXXX01000000")?;
        let reg1 = SomeRegion::new_from_string(2, "r01X01000000")?;

        if reg0.far_reg(&reg1) != SomeRegion::new_from_string(2, "r10X01000000")? {
            return Err(String::from("Result not r10X01000000?"));
        }
        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X")?;

        let reg1 = SomeRegion::new_from_string(1, "rX10X")?;

        if !reg0.is_subset_of(&reg1) {
            return Err(String::from("Result 1 False?"));
        }

        let reg2 = SomeRegion::new_from_string(1, "rXXXX")?;

        if !reg0.is_subset_of(&reg2) {
            return Err(String::from("Result 2 False?"));
        }

        if reg2.is_subset_of(&reg0) {
            return Err(String::from("Result 3 True?"));
        }
        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X")?;

        let reg1 = SomeRegion::new_from_string(1, "rX10X")?;

        if !reg0.is_superset_of(&reg1) {
            return Err(String::from("Result 1 False?"));
        }

        let reg2 = SomeRegion::new_from_string(1, "rXXXX")?;

        if !reg2.is_superset_of(&reg0) {
            return Err(String::from("Result 2 False?"));
        }

        if reg0.is_superset_of(&reg2) {
            return Err(String::from("Result 3 True?"));
        }
        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(2, "r111000XXX")?;
        let reg1 = SomeRegion::new_from_string(2, "r01X01X01X")?;

        if reg0.union(&reg1) != SomeRegion::new_from_string(2, "rX1X0XXXXX")? {
            return Err(String::from("Result not rX1X0XXXXX?"));
        }
        Ok(())
    }

    #[test]
    fn union_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101")?;
        let state0 = SomeState::new_from_string(1, "s0b101001")?;

        if reg0.union_state(&state0) != SomeRegion::new_from_string(1, "rXXXX01")? {
            return Err(String::from("Result not rXXXX01?"));
        }
        Ok(())
    }

    #[test]
    fn high_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX0X1")?;

        if reg0.high_state() != SomeState::new_from_string(1, "s0b1011")? {
            return Err(String::from("Result s0b1011?"));
        }
        Ok(())
    }

    #[test]
    fn low_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX0X1")?;

        if reg0.low_state() != SomeState::new_from_string(1, "s0b1")? {
            println!("low state {}", &reg0.low_state());
            return Err(String::from("Result not s1?"));
        }
        Ok(())
    }

    #[test]
    fn set_to_zeros() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;
        let msk1 = SomeMask::new_from_string(1, "m0b111")?;

        if reg0.set_to_zeros(&msk1) != SomeRegion::new_from_string(1, "rX10X000")? {
            return Err(String::from("Result not rX10X000?"));
        }
        Ok(())
    }

    #[test]
    fn set_to_ones() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;
        let msk1 = SomeMask::new_from_string(1, "m0b111")?;

        if reg0.set_to_ones(&msk1) != SomeRegion::new_from_string(1, "rX10X111")? {
            return Err(String::from("Result not rX10X111?"));
        }
        Ok(())
    }

    #[test]
    fn diff_mask_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0011")?;
        let state0 = SomeState::new_from_string(1, "s0b010101")?;

        if reg0.diff_mask_state(&state0) != SomeMask::new_from_string(1, "m0b110")? {
            return Err(String::from("Result not m0b110?"));
        }
        Ok(())
    }

    #[test]
    fn distance_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0011")?;
        let state0 = SomeState::new_from_string(1, "s0b010101")?;

        let dist = reg0.distance_state(&state0);
        if dist != 2 {
            return Err(format!("Result {} not 2?", dist));
        }
        Ok(())
    }

    #[test]
    fn diff_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(2, "r1XX000111")?;
        let reg1 = SomeRegion::new_from_string(2, "r01X01X01X")?;
        let diff = reg0.diff_mask(&reg1);

        if diff != SomeMask::new_from_string(2, "m0b100010100")? {
            return Err(format!("Result {} not m0b100010100?", diff));
        }
        Ok(())
    }

    #[test]
    fn states_in() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX1XX")?;
        let st1 = SomeState::new_from_string(1, "s0b1")?;
        let st6 = SomeState::new_from_string(1, "s0b110")?;
        let st7 = SomeState::new_from_string(1, "s0b0111")?;
        let st8 = SomeState::new_from_string(1, "s0b1000")?;
        let std = SomeState::new_from_string(1, "s0b1101")?;
        let ste = SomeState::new_from_string(1, "s0b1110")?;
        let sta_str = StateStore::new(vec![
            st1,
            st6.clone(),
            st7.clone(),
            st8,
            std.clone(),
            ste.clone(),
        ]);

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
        let reg0 = SomeRegion::new_from_string(1, "rX10X")?;

        let reg1 = SomeRegion::new_from_string(1, "r0XX1")?;

        let regvec = reg0.subtract(&reg1);

        let mut regs = RegionStore::new(vec![]);
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

        if regs.contains(&SomeRegion::new_from_string(1, "rX100")?) {
        } else {
            return Err(format!(
                "{} minus {} = {} ??",
                &reg0,
                &reg1,
                &regs.formatted_string()
            ));
        }

        if regs.contains(&SomeRegion::new_from_string(1, "r110X")?) {
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
        let reg0 = SomeRegion::new_from_string(1, "rXX0101")?;

        if reg0.num_x() != 2 {
            return Err(String::from("Result not 2?"));
        }
        Ok(())
    }
} // end tests
