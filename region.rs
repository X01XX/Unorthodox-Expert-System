//! The SomeRegion struct, representing a region on a pseudo Karnaugh Map.
//!
//! Uses two states (they can be the same) to represent a region, which includes every state between them.
//!
//! The two states used to make the region, can be keys to two squares.

use crate::mask::SomeMask;
use crate::regionstore::RegionStore;
use crate::rule::SomeRule;
use crate::state::SomeState;
use crate::tools::{self, not, StrLen};

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
/// SomeRegion struct
pub struct SomeRegion {
    /// Vector for one, or more, states.
    /// If more than one state is used, the last state will be the farthest from the first state.
    pub states: Vec<SomeState>,
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
    /// For a group region, the states have corresponding squares that have been sampled.
    /// A group region may have more than two states, so the far_state is calculated.
    /// For reasons outside of this module, the first state in the vector is not deleted or moved.
    pub fn new(mut states: Vec<SomeState>) -> Self {
        assert!(!states.is_empty());

        // Check for single-state region.
        if states.len() == 1 {
            return Self { states };
        }

        // Remove duplicate states, if any.
        // Could be the result of intersecting something like 000X and 00X1 = 0001.
        let mut remv = Vec::<usize>::new();
        for (inx, stax) in states.iter().enumerate() {
            for (iny, stay) in states.iter().enumerate().skip(inx + 1) {
                if stax == stay && !remv.contains(&iny) {
                    remv.push(iny);
                }
            }
        }
        if remv.is_empty() {
        } else {
            // Sort idicies higher to lower.
            remv.sort_by(|a, b| b.cmp(a));
            for inx in remv.iter() {
                tools::remove_unordered(&mut states, *inx);
            }
        }

        if states.len() < 3 {
            return Self { states };
        }

        // Remove unneeded states, if any.
        // If GT 2 states, it might remove all but two states.
        remv = Vec::<usize>::new();
        for (inx, stax) in states.iter().enumerate().skip(1) {
            for (iny, stay) in states.iter().enumerate().skip(1) {
                if iny == inx {
                    continue;
                }
                if stay.is_between(&states[0], stax) && !remv.contains(&iny) {
                    remv.push(iny);
                }
            }
        }
        if remv.is_empty() {
        } else {
            // Sort idicies higher to lower.
            remv.sort_by(|a, b| b.cmp(a));
            for inx in remv.iter() {
                tools::remove_unordered(&mut states, *inx);
            }
        }

        // Check for easy result.
        if states.len() == 2 {
            return Self { states };
        }

        // Calculate a state far from the first state.
        let mut dif = states[0].new_low().to_mask();
        for stax in states.iter().skip(1) {
            dif = dif.bitwise_or(&stax.bitwise_xor(&states[0]));
        }

        // Return region with more than two states.
        let far_state = states[0].bitwise_xor(&dif);
        let mut states2 = states.clone();
        states2.push(far_state);
        Self { states: states2 }
    }

    /// Return a reference to the first state.
    pub fn state1(&self) -> &SomeState {
        &self.states[0]
    }

    /// Return a reference to the second state.
    pub fn state2(&self) -> &SomeState {
        self.states.last().unwrap()
    }

    /// Return a Region from a string and the number of integers to use.
    /// Left-most, consecutive, positions that are omitted will be padded with zeros.
    ///
    /// if let Ok(regx) = SomeRegion::new_from_string(1, "r01x1")) {
    ///    println!("Region {}", regx);
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
    pub fn new_from_string(&self, str: &str) -> Result<Self, String> {
        self.new_from_string2(str, self.state1().to_mask().new_low())
    }

    /// Return a Region from a string and the number of integers to use.
    /// Left-most, consecutive, positions that are omitted will be padded with Xs.
    pub fn new_from_string_pad_x(&self, str: &str) -> Result<Self, String> {
        self.new_from_string2(str, self.state1().new_high().to_mask())
    }

    /// Return a Region from a string and the number of integers to use.
    /// Left-most, consecutive, positions that are omitted will be padded with zeros,
    /// if msk_high is all zeros, Xs if msk_high is all ones.
    fn new_from_string2(&self, str: &str, mut msk_high: SomeMask) -> Result<Self, String> {
        let mut msk_low = msk_high.new_low();

        let mut num_bits = 0;

        for (inx, chr) in str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "r" {
                    continue;
                } else if chr == "s" {
                    let state_r = msk_low.to_state().new_from_string(str);
                    match state_r {
                        Ok(a_state) => {
                            return Ok(Self::new(vec![a_state]));
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

        if num_bits > msk_high.num_bits() {
            return Err(format!("String {str}, too long?"));
        }

        Ok(Self::new(vec![msk_high.to_state(), msk_low.to_state()]))
    } // end new_from_string

    /// Return a String representation of a Region.
    fn formatted_string(&self) -> String {
        let mut s1 = String::with_capacity(self.strlen());
        s1.push('r');

        let num_bits = self.state1().num_bits();

        for (inx, valb) in (0..num_bits).rev().enumerate() {
            if inx > 0 && inx % 4 == 0 {
                s1.push('_');
            }
            let b0 = self.state1().is_bit_set(valb);
            let b1 = self.state2().is_bit_set(valb);

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
        if self.states.len() > 2 {
            s1.push('+');
        }
        s1
    }

    /// Return true if a region and a region/state are adjacent.
    pub fn is_adjacent(&self, other: &impl AccessStates) -> bool {
        //println!("is_adjacent");
        self.diff_mask(other).just_one_bit()
    }

    /// Return true if two regions intersect.
    pub fn intersects(&self, other: &impl AccessStates) -> bool {
        //println!("intersects");
        self.diff_mask(other).is_low()
    }

    /// Return the intersection of two regions.
    /// Check regions for intersection first.
    /// Strangely, the intersection of two adjacent regions produces
    /// most of an overlapping part, except for a 0/1 pair that needs to be changed
    /// to X.
    pub fn intersection(&self, other: &impl AccessStates) -> Option<Self> {
        if !self.intersects(other) {
            return None;
        }
        if self.one_state() {
            Some(Self::new(vec![self.first_state().clone()]))
        } else if other.one_state() {
            Some(Self::new(vec![other.first_state().clone()]))
        } else {
            let state1 = self.high_state().bitwise_and(&other.high_state());
            let state2 = self.low_state().bitwise_or(&other.low_state());
            Some(Self::new(vec![state1, state2]))
        }
    }

    /// Return a Mask of zero positions.
    pub fn zeros_mask(&self) -> SomeMask {
        self.state1()
            .bitwise_not()
            .bitwise_and(&self.state2().bitwise_not())
            .to_mask()
    }

    /// Return a Mask of one positions.
    pub fn ones_mask(&self) -> SomeMask {
        self.state1().bitwise_and(self.state2()).to_mask()
    }

    /// Return a mask of edge (non-X) bits.
    pub fn edge_mask(&self) -> SomeMask {
        self.state1().bitwise_eqv(self.state2())
    }

    /// Return mask of x positions.
    pub fn x_mask(&self) -> SomeMask {
        self.state1().bitwise_xor(self.state2()).to_mask()
    }

    /// Given a state in a region, return the far state in the region.
    pub fn state_far_from(&self, sta: &SomeState) -> SomeState {
        assert!(self.is_superset_of(sta));
        self.state1().bitwise_xor(self.state2()).bitwise_xor(sta)
    }

    /// Given a region, and a proper subset region, return the
    /// far region within the superset region.
    pub fn far_reg(&self, other: &Self) -> Self {
        assert!(self.is_superset_of(other));
        let int_x_msk = self.x_mask();

        let ok_x_msk = other.x_mask();

        assert!(ok_x_msk.is_subset_ones_of(&int_x_msk));

        // Get bit(s) to use to calculate a far-sub-region in reg_int from ok_reg
        // by changing reg_int X over ok_reg 1 to 0 over 1, or reg_int X over ok_reg 0 to 1 over 0
        let cng_bits = int_x_msk.bitwise_and(&ok_x_msk.bitwise_not());

        let state1 = other.state1().bitwise_xor(&cng_bits);
        let state2 = other.state2().bitwise_xor(&cng_bits);

        Self::new(vec![state1, state2])
    }

    /// Return true if a region is a subset on another region.
    pub fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        if self.intersects(other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_subset_ones_of(&x2);
        }
        false
    }

    /// Return true if a region is a superset on another region.
    pub fn is_superset_of(&self, other: &impl AccessStates) -> bool {
        if self.intersects(other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_superset_ones_of(&x2);
        }
        false
    }

    /// Return the union of a region and a region/state.
    pub fn union(&self, other: &impl AccessStates) -> Self {
        //println!("union {} and {}", self, other);
        match (self.one_state(), other.one_state()) {
            (true, true) => {
                let st_low = self.first_state().bitwise_or(other.first_state());
                let st_high = self.first_state().bitwise_and(other.first_state());
                Self::new(vec![st_high, st_low])
            }
            (false, true) => {
                let st_low = self.high_state().bitwise_or(other.first_state());
                let st_high = self.low_state().bitwise_and(other.first_state());
                Self::new(vec![st_high, st_low])
            }
            (true, false) => {
                let other_high = other.high_state();
                let other_low = other.low_state();
                let st_low = self
                    .first_state()
                    .bitwise_or(&other_high.bitwise_or(&other_low));
                let st_high = self
                    .first_state()
                    .bitwise_and(&other_high.bitwise_and(&other_low));
                Self::new(vec![st_high, st_low])
            }
            (false, false) => {
                let other_high = other.high_state();
                let other_low = other.low_state();
                let st_low = self
                    .high_state()
                    .bitwise_or(&other_high.bitwise_or(&other_low));
                let st_high = self
                    .low_state()
                    .bitwise_and(&other_high.bitwise_and(&other_low));
                Self::new(vec![st_high, st_low])
            }
        }
    }

    /// Return the highest state in the region
    pub fn high_state(&self) -> SomeState {
        self.state1().bitwise_or(self.state2())
    }

    /// Return lowest state in the region
    pub fn low_state(&self) -> SomeState {
        self.state1().bitwise_and(self.state2())
    }

    /// Return a region with masked X-bits set to zeros.
    pub fn set_to_zeros(&self, msk: &SomeMask) -> Self {
        let state1 = self.state1().bitwise_and(&msk.bitwise_not());
        let state2 = self.state2().bitwise_and(&msk.bitwise_not());

        Self::new(vec![state1, state2])
    }

    /// Return a region with masked X-bits set to ones.
    pub fn set_to_ones(&self, msk: &SomeMask) -> Self {
        let state1 = self.state1().bitwise_or(msk);
        let state2 = self.state2().bitwise_or(msk);

        Self::new(vec![state1, state2])
    }

    /// Return a region with masked bit positions set to X.
    pub fn set_to_x(&self, msk: &SomeMask) -> Self {
        let state1 = self.state1().bitwise_or(msk);
        let state2 = self.state2().bitwise_and(&msk.bitwise_not());

        Self::new(vec![state1, state2])
    }

    /// Return the distance from a region to a region/state.
    pub fn distance(&self, other: &impl AccessStates) -> usize {
        self.diff_mask(other).num_one_bits()
    }

    /// Return a mask of different, non-x, bits between a region and a region/state.
    pub fn diff_mask(&self, other: &impl AccessStates) -> SomeMask {
        match (self.one_state(), other.one_state()) {
            (true, true) => self
                .first_state()
                .bitwise_xor(other.first_state())
                .to_mask(),
            (false, true) => self
                .edge_mask()
                .bitwise_and(&self.first_state().bitwise_xor(other.first_state())),
            (true, false) => other
                .edge_mask()
                .bitwise_and(&self.first_state().bitwise_xor(other.first_state())),
            (false, false) => self
                .edge_mask()
                .bitwise_and(&other.edge_mask())
                .bitwise_and(&self.first_state().bitwise_xor(other.first_state())),
        }
    }

    /// Given a region, and a second region, return the
    /// first region - the second
    pub fn subtract(&self, other: &impl AccessStates) -> Vec<Self> {
        let mut ret_vec = Vec::<Self>::new();

        // If no intersection, return self.
        let Some(reg_int) = self.intersection(other) else {
            ret_vec.push(self.clone());
            return ret_vec;
        };

        // If other is a superset, return empty vector.
        if reg_int == *self {
            return ret_vec;
        }

        // Split by X over 0 or 1.
        let x_over_not_xs: Vec<SomeMask> = self.x_mask().bitwise_and(&reg_int.edge_mask()).split();

        for mskx in x_over_not_xs.iter() {
            if mskx.bitwise_and(reg_int.state1()).is_low() {
                // reg_int has a 0 bit in that position
                ret_vec.push(self.set_to_ones(mskx));
            } else {
                // reg_int has a 1 in that bit position
                ret_vec.push(self.set_to_zeros(mskx));
            }
        }
        ret_vec
    }

    /// Return the result of region minus state, supersets of a second state.
    pub fn subtract_state_to_supersets_of(
        &self,
        substa: &SomeState,
        supsta: &SomeState,
    ) -> Vec<Self> {
        let mut ret_vec = Vec::<Self>::new();

        // If region is not a superset of the state, return self.
        if !self.is_superset_of(substa) {
            ret_vec.push(self.clone());
            return ret_vec;
        };

        // If region minus state is null, return empty vector.
        if self.states.len() == 1 {
            return ret_vec;
        }

        // Split by X over 0 or 1, where the result will be a superset of the second argument state.
        let x_over_not_xs: Vec<SomeMask> = self
            .x_mask()
            .bitwise_and(&substa.bitwise_xor(supsta))
            .split();

        for mskx in x_over_not_xs.iter() {
            if mskx.bitwise_and(substa).is_low() {
                // reg_int has a 0 bit in that position
                ret_vec.push(self.set_to_ones(mskx));
            } else {
                // reg_int has a 1 in that bit position
                ret_vec.push(self.set_to_zeros(mskx));
            }
        }
        ret_vec
    }

    /// Return the complement of a region.
    pub fn complement(&self) -> RegionStore {
        let nonxbits = self.edge_mask().split();
        let mut ret = RegionStore::with_capacity(nonxbits.len());

        let high_sta = self.state1().new_high();
        let low_sta = high_sta.new_low();

        for nbit in &nonxbits {
            if nbit.bitwise_and(self.state1()).is_low() {
                // The bit is a zero, force a one in that bit position.
                ret.push(SomeRegion::new(vec![high_sta.clone(), nbit.to_state()]));
            } else {
                // The bit is a one, force a zero in that bit position.
                ret.push(SomeRegion::new(vec![
                    high_sta.bitwise_xor(nbit),
                    low_sta.clone(),
                ]));
            }
        }
        ret
    }

    /// Return true if a region is all X.
    pub fn all_x(&self) -> bool {
        self.edge_mask().is_low()
    }

    /// Return the number of bits used to define a Region.
    pub fn num_bits(&self) -> usize {
        self.state1().num_bits()
    }

    /// Return a rule for translating from a region to another region.
    /// The result of the rule may be equal to, or subset of (1->1 instead of 1->X,
    /// 0->0 instead of 0->X), the second region.
    /// The minimum changes are sought, so X->x-not becomes X->X.
    /// It can be thought that:
    /// 0->1 and 1->0 changes are required, but compared to another change may be missing,
    /// or if in the other change may be unwanted.
    /// For X->0, the change is optional, a 0 input will be no change.
    /// For X->1, the change is optional, a 1 input will be no change.
    /// Anything -> X, is a don't care.
    pub fn rule_to_region(&self, to: &SomeRegion) -> SomeRule {
        let self_x = self.x_mask();
        let self_1 = self.ones_mask();
        let self_0 = self.zeros_mask();

        let to_x = to.x_mask();
        let to_1 = to.ones_mask();
        let to_0 = to.zeros_mask();

        let x_to_0 = self_x.bitwise_and(&to_0);
        let x_to_1 = self_x.bitwise_and(&to_1);
        let x_to_x = self_x.bitwise_and(&to_x);
        let zero_to_x = self_0.bitwise_and(&to_x);
        let one_to_x = self_1.bitwise_and(&to_x);

        SomeRule {
            b00: self_0
                .bitwise_and(&to_0)
                .bitwise_or(&x_to_0)
                .bitwise_or(&x_to_x)
                .bitwise_or(&zero_to_x),
            b01: self_0.bitwise_and(&to_1).bitwise_or(&x_to_1),
            b11: self_1
                .bitwise_and(&to_1)
                .bitwise_or(&x_to_1)
                .bitwise_or(&x_to_x)
                .bitwise_or(&one_to_x),
            b10: self_1.bitwise_and(&to_0).bitwise_or(&x_to_0),
        }
    }

    /// Return the number of edges in a region.
    pub fn num_edges(&self) -> usize {
        self.edge_mask().num_one_bits()
    }

    /// Return the shared symmetric region between two regions, if any.
    pub fn shared_symmetric_region(&self, other: &Self) -> Option<SomeRegion> {
        // Regions must be adjacent.
        if not(self.is_adjacent(other)) {
            return None;
        }
        let x1 = self.x_mask();
        let x2 = other.x_mask();
        // Regions must have at last one position of X/non-x.
        if x1 == x2 || x1.is_superset_ones_of(&x2) || x2.is_superset_ones_of(&x1) {
            return None;
        }
        // Get dif mask.
        let diff = self.diff_mask(other);

        // Prepare the regions to make a valid intersection.
        let reg1 = self.set_to_x(&diff);
        let reg2 = other.set_to_x(&diff);

        reg1.intersection(&reg2)
    }
} // end impl SomeRegion

/// Implement the trait StrLen for SomeRegion.
impl StrLen for SomeRegion {
    fn strlen(&self) -> usize {
        self.state1().strlen()
    }
}

/// Define the AccessStates trait, so operations on Regions and States are smoother.
/// A region defined by a single state, is similar to a single state.
pub trait AccessStates {
    fn one_state(&self) -> bool;
    fn first_state(&self) -> &SomeState;
    fn x_mask(&self) -> SomeMask;
    fn edge_mask(&self) -> SomeMask;
    fn high_state(&self) -> SomeState;
    fn low_state(&self) -> SomeState;
    fn diff_mask(&self, other: &impl AccessStates) -> SomeMask;
    fn intersects(&self, other: &impl AccessStates) -> bool;
    fn is_subset_of(&self, other: &impl AccessStates) -> bool;
    fn is_superset_of(&self, other: &impl AccessStates) -> bool;
}

/// Implement the trait AccessStates for SomeRegion.
impl AccessStates for SomeRegion {
    fn one_state(&self) -> bool {
        1 == self.states.len()
    }
    fn first_state(&self) -> &SomeState {
        self.states.first().expect("SNH")
    }
    fn x_mask(&self) -> SomeMask {
        self.x_mask()
    }
    fn edge_mask(&self) -> SomeMask {
        self.edge_mask()
    }
    fn high_state(&self) -> SomeState {
        self.high_state()
    }
    fn low_state(&self) -> SomeState {
        self.low_state()
    }
    fn diff_mask(&self, other: &impl AccessStates) -> SomeMask {
        self.diff_mask(other)
    }
    fn intersects(&self, other: &impl AccessStates) -> bool {
        self.intersects(other)
    }
    fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        self.is_subset_of(other)
    }
    fn is_superset_of(&self, other: &impl AccessStates) -> bool {
        self.is_superset_of(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::regionstore::RegionStore;
    use crate::sample::SomeSample;
    use rand::Rng;

    #[test]
    fn shared_symmetric_region() -> Result<(), String> {
        let ur_bits = SomeBits::new(vec![0]);
        let ur_region = SomeRegion::new(vec![SomeState::new(ur_bits.clone())]);

        let reg1 = ur_region.new_from_string("rX101").expect("SNH");
        let reg2 = ur_region.new_from_string("rX111").expect("SNH");
        let reg3 = ur_region.new_from_string("r0X10").expect("SNH");
        let reg4 = ur_region.new_from_string("r011X").expect("SNH");

        if let Some(result) = reg1.shared_symmetric_region(&reg2) {
            println!("result {result}");
            return Err(format!("bad result from {reg1} {reg2}"));
        }

        if let Some(result) = reg1.shared_symmetric_region(&reg3) {
            println!("result {result}");
            return Err(format!("bad result from {reg1} {reg3}"));
        }

        if let Some(result) = reg2.shared_symmetric_region(&reg3) {
            println!("result {result}");
            if result != reg4 {
                return Err(format!("bad result from {reg2} {reg3}"));
            }
        } else {
            return Err(format!("bad result {reg2} {reg3} = None"));
        }
        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn complement() -> Result<(), String> {
        let ur_bits = SomeBits::new(vec![0]);
        let ur_region = SomeRegion::new(vec![SomeState::new(ur_bits.clone())]);

        let reg1 = ur_region.new_from_string_pad_x("r10XX_X101").expect("SNH");

        let comp1 = reg1.complement();
        println!("comp1: {}", comp1);

        assert!(comp1.len() == 5);
        assert!(comp1.contains(&ur_region.new_from_string("r0xxx_xxxx").expect("SNH")));
        assert!(comp1.contains(&ur_region.new_from_string("rxxxx_xx1x").expect("SNH")));

        Ok(())
    }

    #[test]
    fn test_strlen() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_reg = SomeRegion::new(vec![
            SomeState::new(SomeBits::new(vec![0])),
            SomeState::new(SomeBits::new(vec![0])),
        ]);
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn subtract_state_to_supersets_of() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0]));

        let sta0 = tmp_sta.new_from_string("s0b0000")?;
        let staf = tmp_sta.new_from_string("s0b1111")?;

        let reg1 = SomeRegion::new(vec![sta0, staf]);

        let sta5 = tmp_sta.new_from_string("s0b0101")?;
        let sta6 = tmp_sta.new_from_string("s0b0110")?;
        let regs = reg1.subtract_state_to_supersets_of(&sta6, &sta5);

        println!("reg1 {reg1} minus {sta6}, to supersets of {sta5}");
        print!("regs ");
        assert!(regs.len() == 2);

        for regx in regs.iter() {
            print!("{regx} ");
            assert!(regx.is_superset_of(&sta5));
        }
        println!(" ");

        Ok(())
    }

    #[test]
    fn test_new() -> Result<(), String> {
        // Single state region.
        let tmp_sta = SomeState::new(SomeBits::new(vec![0]));

        let sta1 = tmp_sta.new_from_string("s0b0001")?;

        let reg1 = SomeRegion::new(vec![sta1.clone(), sta1.clone()]);
        println!("reg1 is {}", reg1);
        assert!(reg1.states.len() == 1);
        assert!(reg1.state2() == &sta1);

        // Two state region.
        let sta7 = tmp_sta.new_from_string("s0b0111")?;

        let reg2 = SomeRegion::new(vec![sta1.clone(), sta7.clone()]);
        println!("reg2 is {}", reg2);
        assert!(reg2.states.len() == 2);
        assert!(reg2.state2() == &sta7);

        // Three state region.
        let sta2 = tmp_sta.new_from_string("s0b0010")?;
        let reg3 = SomeRegion::new(vec![sta1.clone(), sta7.clone(), sta2.clone()]);
        println!("reg3 is {}", reg3);
        assert!(reg3.states.len() == 4);

        println!("reg3 state1 = {}", reg3.state1());
        assert!(reg3.state1() == &sta1);

        let sta6 = tmp_sta.new_from_string("s0b0110")?;
        println!("reg3 state2 = {}", reg3.state2());
        assert!(reg3.state2() == &sta6);

        // Three states, only two needed due to far state being in the list.
        let sta2 = tmp_sta.new_from_string("s0b0010")?;
        let reg4 = SomeRegion::new(vec![sta1.clone(), sta6.clone(), sta2.clone()]);
        println!("reg4 is {}", reg4);
        assert!(reg4.states.len() == 2);

        println!("reg4 state1 = {}", reg4.state1());
        assert!(reg4.state1() == &sta1);

        println!("reg4 state2 = {}", reg4.state2());
        assert!(reg4.state2() == &sta6);

        // Three state region, five states given.
        // State 1, between 0 and 5, will be deleted.
        // State 2, between 6 and 0, will be deleted.
        let sta0 = tmp_sta.new_from_string("s0b0000")?;
        let sta5 = tmp_sta.new_from_string("s0b0101")?;
        let reg5 = SomeRegion::new(vec![
            sta0.clone(),
            sta1.clone(),
            sta2.clone(),
            sta5.clone(),
            sta6.clone(),
        ]);

        println!("reg5 is {}", reg5);
        assert!(reg5.states.len() == 4);

        println!("reg5 state2 = {}", reg5.state2());
        assert!(reg5.state2() == &sta7);

        // Three state region, with duplicates.
        let reg6 = SomeRegion::new(vec![
            sta1.clone(),
            sta1.clone(),
            sta7.clone(),
            sta2.clone(),
            sta7.clone(),
        ]);
        println!("reg6 is {}", reg6);
        assert!(reg6.states.len() == 4);

        println!("reg6 state1 = {}", reg6.state1());
        assert!(reg6.state1() == &sta1);

        println!("reg6 state2 = {}", reg6.state2());
        assert!(reg6.state2() == &sta6);

        Ok(())
    }

    #[test]
    fn edge_mask() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts.clone())]);
        let tmp_msk = SomeMask::new(tmp_bts.clone());

        let reg0 = tmp_reg.new_from_string("r101XX0")?;
        let edges = reg0.edge_mask();
        println!("Edges of {reg0} are {edges}");
        assert!(
            edges.bitwise_and(&tmp_bts.new_from_string("0xff")?)
                == tmp_msk.new_from_string("m0b1111_1001")?
        );
        Ok(())
    }

    // Test new_from_string, using randomly chosen digits.
    #[test]
    fn new_from_string() -> Result<(), String> {
        let ur_sta = SomeState::new(SomeBits::new(vec![0, 0]));
        let ur_reg = SomeRegion::new(vec![ur_sta.clone()]);

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
            let reg_instance = ur_reg.new_from_string(&reg_from_str)?;

            // Check for the expected states forming the region.
            let state1_str =
                "s0b".to_string() + &reg_from_str.replace("x", "0").replace("X", "1")[1..];
            let state1 = ur_sta.new_from_string(&state1_str)?;
            let state2 = reg_instance.state_far_from(&state1);
            println!("{} should equal {state1}", reg_instance.state1());
            assert!(reg_instance.state1() == &state1);
            println!("{} should equal {state1}", reg_instance.state2());
            assert!(reg_instance.state2() == &state2);
        }

        //let reg2 = ur_reg.new_from_string("rx0x1.1x0x");

        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0]));

        let reg1 = SomeRegion::new(vec![
            tmp_sta.new_from_string("s0b1010")?,
            tmp_sta.new_from_string("s0b0101")?,
        ]);
        let reg2 = SomeRegion::new(vec![
            tmp_sta.new_from_string("s0b0001")?,
            tmp_sta.new_from_string("s0b1110")?,
        ]);
        println!("{reg1} should equal {reg2}");
        assert!(reg1.eq(&reg2));

        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let mut reg0 = tmp_reg.new_from_string("r101XX1")?;
        let mut reg1 = tmp_reg.new_from_string("rXX0011")?;
        println!("{reg0} s/b adjacent {reg1}");
        assert!(reg0.is_adjacent(&reg1));

        reg0 = tmp_reg.new_from_string("rX10X01X")?;
        reg1 = tmp_reg.new_from_string("rX10X10X")?;
        println!("{reg0} s/b adjacent {reg1}");
        assert!(!reg0.is_adjacent(&reg1));

        let tmp_sta = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        let reg0 = tmp_reg.new_from_string("rX10X10X")?;
        let sta1 = tmp_sta.new_from_string("s0b1001100")?;
        println!("{reg0} s/b adjacent {sta1}");
        assert!(reg0.is_adjacent(&sta1));

        let sta2 = tmp_sta.new_from_string("s0b1001110")?;
        println!("{reg0} s/nb adjacent {sta2}");
        assert!(!reg0.is_adjacent(&sta2));

        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let reg0 = tmp_reg.new_from_string("rX10X10X")?;
        let reg1 = tmp_reg.new_from_string("r0XX110X")?;
        println!("{reg0} should intersect {reg1}");
        assert!(reg0.intersects(&reg1));

        let reg2 = tmp_reg.new_from_string("r0XX111X")?;
        println!("{reg0} should not intersect {reg2}");
        assert!(!reg0.intersects(&reg2));

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let reg0 = tmp_reg.new_from_string("rX10X10X")?;
        let reg1 = tmp_reg.new_from_string("r0XX110X")?;

        if let Some(reg_int) = reg0.intersection(&reg1) {
            println!("Intersection of {reg0} and {reg1} is {reg_int}");
            assert!(reg_int == tmp_reg.new_from_string("r010110X")?);
        } else {
            return Err(format!("{reg0} does not intersect {reg1}?"));
        }
        Ok(())
    }

    #[test]
    fn zeros_mask() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_msk = SomeMask::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts.clone())]);

        let reg0 = tmp_reg.new_from_string("r00XX0101")?;
        let m1 = reg0.zeros_mask();
        println!("zeros_mask is {m1}");
        assert!(
            m1.bitwise_and(&tmp_bts.new_from_string("0xff")?)
                == tmp_msk.new_from_string("m0b11001010")?
        );
        Ok(())
    }

    #[test]
    fn ones_mask() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_msk = SomeMask::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts.clone())]);

        let reg0 = tmp_reg.new_from_string("r00XX0101")?;
        let m1 = reg0.ones_mask();
        println!("ones_mask is {m1}");
        assert!(m1 == tmp_msk.new_from_string("m0b101")?);
        Ok(())
    }

    #[test]
    fn x_mask() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_msk = SomeMask::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts.clone())]);

        let reg0 = tmp_reg.new_from_string("r00XX0101")?;
        let m1 = reg0.x_mask();
        println!("x_mask is {m1}");
        assert!(m1 == tmp_msk.new_from_string("m0b110000")?);
        Ok(())
    }

    #[test]
    fn non_x_mask() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_msk = SomeMask::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts.clone())]);

        let reg0 = tmp_reg.new_from_string("r0000xx01")?;
        let m1 = reg0.edge_mask();
        println!("non_x_mask is {m1}");
        assert!(
            m1.bitwise_and(&tmp_bts.new_from_string("0xff")?)
                == tmp_msk.new_from_string("m0b11110011")?
        );
        Ok(())
    }

    #[test]
    fn far_state() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts.clone())]);

        let reg0 = tmp_reg.new_from_string("r0000XXX1")?;
        let state0 = tmp_sta.new_from_string("s0b00001011")?;
        let far_state = reg0.state_far_from(&state0);
        println!("far state is {far_state}");
        assert!(far_state == tmp_sta.new_from_string("s0b101")?);
        Ok(())
    }

    #[test]
    fn far_reg() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0, 0]))]);

        let reg0 = tmp_reg.new_from_string("rXXX01000000")?;
        let reg1 = tmp_reg.new_from_string("r01X01000000")?;
        let far_reg = reg0.far_reg(&reg1);
        println!("far_reg is {far_reg}");
        assert!(far_reg == tmp_reg.new_from_string("r10X01000000")?);
        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let reg0 = tmp_reg.new_from_string("rX10X")?;
        let reg1 = tmp_reg.new_from_string("rX10X")?;
        if !reg0.is_subset_of(&reg1) {
            return Err(format!("{reg0} not subset {reg1}?"));
        }

        let reg2 = tmp_reg.new_from_string("rXXXX")?;
        if !reg0.is_subset_of(&reg2) {
            return Err(format!("{reg0} not subset {reg2}?"));
        }

        if reg2.is_subset_of(&reg0) {
            return Err(format!("{reg2} is subset {reg0}?"));
        }
        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let reg0 = tmp_reg.new_from_string("rX10X")?;
        let reg1 = tmp_reg.new_from_string("rX10X")?;

        if !reg0.is_superset_of(&reg1) {
            return Err(format!("{reg0} not superset {reg1}?"));
        }

        let reg2 = tmp_reg.new_from_string("rXXXX")?;

        if !reg2.is_superset_of(&reg0) {
            return Err(format!("{reg2} not superset {reg0}?"));
        }

        if reg0.is_superset_of(&reg2) {
            return Err(format!("{reg0} is superset {reg2}?"));
        }

        let tmp_sta = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        let reg0 = tmp_reg.new_from_string("rX10X")?;
        let sta1 = tmp_sta.new_from_string("s0b1100")?;
        println!("{reg0} s/b superset of {sta1}");
        assert!(reg0.is_superset_of(&sta1));

        let sta2 = tmp_sta.new_from_string("s0b0000")?;
        println!("{reg0} s/nb superset of {sta2}");
        assert!(!reg0.is_superset_of(&sta2));

        let sta3 = tmp_sta.new_from_string("s0b0010")?;
        println!("{reg0} s/nb superset of {sta3}");
        assert!(!reg0.is_superset_of(&sta3));

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let ur_bts = SomeBits::new(vec![0]);
        let ur_sta = SomeState::new(ur_bts.clone());
        let ur_reg = SomeRegion::new(vec![SomeState::new(ur_bts)]);

        let reg2a = ur_reg.new_from_string("rXX0011")?; // Region >1 state.
        let reg2b = ur_reg.new_from_string("rXX0111")?; // Region >1 state.
        let reg1a = ur_reg.new_from_string("r000101")?; // Region =1 state.
        let reg1b = ur_reg.new_from_string("r000011")?; // Region =1 state.
        let sta0 = ur_sta.new_from_string("s0b010101")?; // State, =1 state.

        // Region >1 state, Region >1 state.
        let union = reg2a.union(&reg2b);
        println!("{reg2a} distance {reg2b} is {union}");
        assert!(union == ur_reg.new_from_string("rxx0x11")?);

        // Region >1 state, Region =1 state.
        let union = reg2a.union(&reg1a);
        println!("{reg2a} distance {reg1a} is {union}");
        assert!(union == ur_reg.new_from_string("rxx0xx1")?);

        // Region >1 state, state.
        let union = reg2a.union(&sta0);
        println!("{reg2a} distance {sta0} is {union}");
        assert!(union == ur_reg.new_from_string("rxx0xx1")?);

        // Region =1 state, Region =1 state.
        let union = reg1a.union(&reg1b);
        println!("{reg1a} distance {reg1b} is {union}");
        assert!(union == ur_reg.new_from_string("r0xx1")?);

        // Region =1 state, Region >1 state.
        let union = reg1a.union(&reg2a);
        println!("{reg1a} distance {reg2a} is {union}");
        assert!(union == ur_reg.new_from_string("rXX0xx1")?);

        // Region =1 state, state.
        let union = reg1a.union(&sta0);
        println!("{reg1a} distance {sta0} is {union}");
        assert!(union == ur_reg.new_from_string("r000x_0101")?);

        Ok(())
    }

    #[test]
    fn high_state() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts)]);

        let reg0 = tmp_reg.new_from_string("rX0X1")?;
        let hs = reg0.high_state();
        println!("High state of {reg0} is {hs}");
        assert!(hs == tmp_sta.new_from_string("s0b1011")?);

        Ok(())
    }

    #[test]
    fn low_state() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts)]);

        let reg0 = tmp_reg.new_from_string("rX0X1")?;
        let ls = reg0.low_state();
        println!("Low state of {reg0} is {ls}");
        assert!(ls == tmp_sta.new_from_string("s0b1")?);

        Ok(())
    }

    #[test]
    fn set_to_zeros() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_msk = SomeMask::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts)]);

        let reg0 = tmp_reg.new_from_string("rX10X10X")?;
        let msk1 = tmp_msk.new_from_string("m0b111")?;
        let reg1 = reg0.set_to_zeros(&msk1);
        println!("{reg0} set_to_zeros {msk1} is {reg1}");
        assert!(reg1 == tmp_reg.new_from_string("rX10X000")?);

        Ok(())
    }

    #[test]
    fn set_to_ones() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_msk = SomeMask::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![SomeState::new(tmp_bts)]);

        let reg0 = tmp_reg.new_from_string("rX10X10X")?;
        let msk1 = tmp_msk.new_from_string("m0b111")?;
        let reg1 = reg0.set_to_ones(&msk1);
        println!("{reg0} set_to_ones {msk1} is {reg1}");
        assert!(reg1 == tmp_reg.new_from_string("rX10X111")?);

        Ok(())
    }

    #[test] // Also tests diff_mask.
    fn distance() -> Result<(), String> {
        let ur_bts = SomeBits::new(vec![0]);
        let ur_sta = SomeState::new(ur_bts.clone());
        let ur_reg = SomeRegion::new(vec![SomeState::new(ur_bts)]);

        let reg2a = ur_reg.new_from_string("rXX0011")?; // Region >1 state.
        let reg2b = ur_reg.new_from_string("rXX0111")?; // Region >1 state.
        let reg1a = ur_reg.new_from_string("r000101")?; // Region =1 state.
        let reg1b = ur_reg.new_from_string("r000011")?; // Region =1 state.
        let sta0 = ur_sta.new_from_string("s0b010101")?; // State, =1 state.

        // Region >1 state, Region >1 state.
        let dist = reg2a.distance(&reg2b);
        println!("{reg2a} distance {reg2b} is {dist}");
        assert!(dist == 1);

        // Region >1 state, Region =1 state.
        let dist = reg2a.distance(&reg1a);
        println!("{reg2a} distance {reg1a} is {dist}");
        assert!(dist == 2);

        // Region >1 state, state.
        let dist = reg2a.distance(&sta0);
        println!("{reg2a} distance {sta0} is {dist}");
        assert!(dist == 2);

        // Region =1 state, Region =1 state.
        let dist = reg1a.distance(&reg1b);
        println!("{reg1a} distance {reg1b} is {dist}");
        assert!(dist == 2);

        // Region =1 state, Region >1 state.
        let dist = reg1a.distance(&reg2a);
        println!("{reg1a} distance {reg2a} is {dist}");
        assert!(dist == 2);

        // Region =1 state, state.
        let dist = reg1a.distance(&sta0);
        println!("{reg1a} distance {sta0} is {dist}");
        assert!(dist == 1);

        Ok(())
    }

    #[test]
    fn diff_mask() -> Result<(), String> {
        let ur_bts = SomeBits::new(vec![0]);
        let ur_msk = SomeMask::new(ur_bts.clone());
        let ur_sta = SomeState::new(ur_bts.clone());
        let ur_reg = SomeRegion::new(vec![SomeState::new(ur_bts)]);

        let reg2a = ur_reg.new_from_string("rXX0011")?; // Region >1 state.
        let reg2b = ur_reg.new_from_string("rXX0111")?; // Region >1 state.
        let reg1a = ur_reg.new_from_string("r000101")?; // Region =1 state.
        let reg1b = ur_reg.new_from_string("r000011")?; // Region =1 state.
        let sta0 = ur_sta.new_from_string("s0b010101")?; // State, =1 state.

        // Region >1 state, Region >1 state.
        let diff = reg2a.diff_mask(&reg2b);
        println!("{reg2a} diff_mask {reg2b} is {diff}");
        assert!(diff == ur_msk.new_from_string("m0b0100")?);

        // Region >1 state, Region =1 state.
        let diff = reg2a.diff_mask(&reg1a);
        println!("{reg2a} diff_mask {reg1a} is {diff}");
        assert!(diff == ur_msk.new_from_string("m0b0110")?);

        // Region >1 state, state.
        let diff = reg2a.diff_mask(&sta0);
        println!("{reg2a} diff_mask {sta0} is {diff}");
        assert!(diff == ur_msk.new_from_string("m0b0110")?);

        // Region =1 state, Region =1 state.
        let diff = reg1a.diff_mask(&reg1b);
        println!("{reg1a} diff_mask {reg1b} is {diff}");
        assert!(diff == ur_msk.new_from_string("m0b0110")?);

        // Region =1 state, Region >1 state.
        let diff = reg1a.diff_mask(&reg2a);
        println!("{reg1a} diff_mask {reg2a} is {diff}");
        assert!(diff == ur_msk.new_from_string("m0b0110")?);

        // Region =1 state, state.
        let diff = reg1a.diff_mask(&sta0);
        println!("{reg1a} diff_mask {sta0} is {diff}");
        assert!(diff == ur_msk.new_from_string("m0b10000")?);

        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let reg0 = tmp_reg.new_from_string("rX10X")?;
        let reg1 = tmp_reg.new_from_string("r0XX1")?;
        let regs = RegionStore::new(reg0.subtract(&reg1));
        println!("{reg0} subtract {reg1} = {regs}");

        assert!(regs.len() == 2);

        assert!(regs.contains(&tmp_reg.new_from_string("rX100")?));
        assert!(regs.contains(&tmp_reg.new_from_string("r110X")?));

        // Test subtract a superset.
        let reg3 = tmp_reg.new_from_string("rXXX1")?;
        let regs = RegionStore::new(reg1.subtract(&reg3));
        println!("{reg0} subtract {reg3} = {regs}");
        assert!(regs.is_empty());

        // Test no intersection.
        let reg3 = tmp_reg.new_from_string("rXX11")?;
        let regs = RegionStore::new(reg0.subtract(&reg3));
        println!("{reg0} subtract {reg3} = {regs}");
        assert!(regs.len() == 1);
        assert!(regs.contains(&reg0));

        Ok(())
    }

    #[test]
    fn rule_to_region() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);
        let tmp_rul = SomeRule::new(&SomeSample::new(
            SomeState::new(tmp_bts.clone()),
            SomeState::new(tmp_bts.clone()),
        ));

        let reg1 = tmp_reg.new_from_string("r000")?;
        let reg2 = tmp_reg.new_from_string("r01X")?;
        let rul1 = reg1.rule_to_region(&reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1: {rul1}");
        let rul2 = tmp_rul.new_from_string("00/01/00")?;
        assert!(rul1 == rul2);

        let reg1 = tmp_reg.new_from_string("r111")?;
        let reg2 = tmp_reg.new_from_string("r01X")?;
        let rul1 = reg1.rule_to_region(&reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1: {rul1}");
        let rul2 = tmp_rul.new_from_string("10/11/11")?;
        assert!(rul1 == rul2);

        let reg1 = tmp_reg.new_from_string("rXXX")?;
        let reg2 = tmp_reg.new_from_string("r01X")?;
        let rul1 = reg1.rule_to_region(&reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1: {rul1}");
        let rul2 = tmp_rul.new_from_string("X0/X1/XX")?;
        assert!(rul1 == rul2);

        // Test proper subset region.
        let reg1 = tmp_reg.new_from_string("r0011")?;
        let reg2 = tmp_reg.new_from_string("rx01x")?;
        let rul1 = reg1.rule_to_region(&reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1 is {rul1}");
        let rul2 = tmp_rul.new_from_string("00/00/11/11")?;
        assert!(rul1 == rul2);

        // Test intersecting regions.
        let reg1 = tmp_reg.new_from_string("r010x")?;
        let reg2 = tmp_reg.new_from_string("rx1x1")?;
        let rul1 = reg1.rule_to_region(&reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1 is {rul1}");
        let rul2 = tmp_rul.new_from_string("00/11/00/X1")?;
        assert!(rul1 == rul2);

        Ok(())
    }
} // end tests
