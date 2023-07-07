//! The SomeRegion struct, representing a region on a pseudo Karnaugh Map.
//!
//! Uses two states (they can be the same) to represent a region, which includes every state between them.
//!
//! The two states used to make the region, can be keys to two squares.

use crate::mask::SomeMask;
use crate::removeunordered;
use crate::state::SomeState;
use crate::tools;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
/// SomeRegion struct
pub struct SomeRegion {
    /// Vector for one, or more, states.
    pub states: Vec<SomeState>,
    /// For regions defined by more that 2 squares, the state farthest in the region
    /// from the first state.
    far: Option<SomeState>,
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
    pub fn new(mut states: Vec<SomeState>) -> Self {
        assert!(!states.is_empty());

        let (states2, far) = match states.len() {
            1 => (states, None),
            2 => {
                if states[0] == states[1] {
                    (vec![states.pop().unwrap()], None)
                } else {
                    (states, None)
                }
            }
            _ => {
                if SomeState::vec_check_for_duplicates(&states) {
                    println!("dups {:?}!", SomeState::vec_string(&states));
                    panic!("Done");
                }
                if SomeState::vec_check_for_unneeded(&states) {
                    println!("unneeded {:?}!", SomeState::vec_string(&states));
                    panic!("Done");
                }
                // Calc x-mask for the region.
                let mut x_mask = SomeMask::new_low(states[0].num_ints());
                for stax in states.iter().skip(1) {
                    x_mask = x_mask.bitwise_or(&stax.bitwise_xor(&states[0]));
                }
                // Calc state in region, far from the first state, as if the region was made of two states.
                let far = states[0].bitwise_xor(&x_mask);

                (states, Some(far))
            }
        };

        Self {
            states: states2,
            far,
        }
    }

    /// Return a reference to the first state.
    pub fn state1(&self) -> &SomeState {
        &self.states[0]
    }

    /// Return a reference to the second state.
    pub fn state2(&self) -> &SomeState {
        match self.states.len() {
            1 => &self.states[0],
            2 => &self.states[1],
            _ => {
                if let Some(x) = &self.far {
                    x
                } else {
                    panic!("should not happen");
                }
            }
        }

        //        if self.states.len() == 1 {
        //            &self.states[0]
        //        } else {
        //            &self.states[1]
        //        }
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
        SomeRegion::new_from_string2(num_ints, str, SomeMask::new_low(num_ints))
    }

    /// Return a Region from a string and the number of integers to use.
    /// Left-most, consecutive, positions that are omitted will be padded with Xs.
    pub fn new_from_string_pad_x(num_ints: usize, str: &str) -> Result<Self, String> {
        assert!(num_ints > 0);
        SomeRegion::new_from_string2(num_ints, str, SomeMask::new_high(num_ints))
    }

    /// Return a Region from a string and the number of integers to use.
    /// Left-most, consecutive, positions that are omitted will be padded with zeros,
    /// if msk_high is all zeros, Xs if msk_high is all ones.
    fn new_from_string2(
        num_ints: usize,
        str: &str,
        mut msk_high: SomeMask,
    ) -> Result<Self, String> {
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
                            return Ok(SomeRegion::new(vec![a_state]));
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

        if msk_high == msk_low {
            Ok(SomeRegion::new(vec![msk_high.to_state()]))
        } else {
            Ok(SomeRegion::new(vec![
                msk_high.to_state(),
                msk_low.to_state(),
            ]))
        }
    } // end new_from_string

    /// Return the number of integers used to implement a region.
    pub fn num_ints(&self) -> usize {
        self.state1().num_ints()
    }

    /// Return the expected length of a string representing a region, for string alloaction.
    pub fn formatted_string_length(&self) -> usize {
        self.state1().num_bits() + 1 + (self.state1().num_bits() / 4)
    }

    /// Return a String representation of a Region without any prefix.
    pub fn formatted_string(&self) -> String {
        let mut s1 = String::with_capacity(self.formatted_string_length());
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
        let state1 = self.high_state().bitwise_and(&other.high_state());
        let state2 = self.low_state().bitwise_or(&other.low_state());

        if state1 == state2 {
            Some(Self::new(vec![state1]))
        } else {
            Some(Self::new(vec![state1, state2]))
        }
    }

    /// Return true if a region is a superset of a state.
    pub fn is_superset_of_state(&self, a_state: &SomeState) -> bool {
        self.state1()
            .bitwise_xor(a_state)
            .bitwise_and(&self.state2().bitwise_xor(a_state))
            .to_mask()
            .is_low()
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

    /// Return the number of X bits in a region.
    pub fn num_x(&self) -> usize {
        self.state1().distance(self.state2())
    }

    /// Given a state in a region, return the far state in the region.
    pub fn far_state(&self, sta: &SomeState) -> SomeState {
        self.state1().bitwise_xor(self.state2()).bitwise_xor(sta)
    }

    /// Given a region, and a proper subset region, return the
    /// far region within the superset region.
    pub fn far_reg(&self, other: &Self) -> Self {
        let int_x_msk = self.x_mask();

        let ok_x_msk = other.x_mask();

        assert!(ok_x_msk.is_subset_of(&int_x_msk));

        // Get bit(s) to use to calculate a far-sub-region in reg_int from ok_reg
        // by changing reg_int X over ok_reg 1 to 0 over 1, or reg_int X over ok_reg 0 to 1 over 0
        let cng_bits = int_x_msk.bitwise_and(&ok_x_msk.bitwise_not());

        let state1 = other.state1().bitwise_xor(&cng_bits);
        let state2 = other.state2().bitwise_xor(&cng_bits);

        if state1 == state2 {
            SomeRegion::new(vec![state1])
        } else {
            SomeRegion::new(vec![state1, state2])
        }
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

        Self::new(vec![st_high, st_low])
    }

    /// Return the union of a region and a state.
    pub fn union_state(&self, other: &SomeState) -> Self {
        let st_low = self.low_state().bitwise_and(other);

        let st_high = self.high_state().bitwise_or(other);

        Self::new(vec![st_high, st_low])
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

        if state1 == state2 {
            Self::new(vec![state1])
        } else {
            Self::new(vec![state1, state2])
        }
    }

    /// Return a region with masked X-bits set to ones.
    pub fn set_to_ones(&self, msk: &SomeMask) -> Self {
        let state1 = self.state1().bitwise_or(msk);
        let state2 = self.state2().bitwise_or(msk);

        if state1 == state2 {
            Self::new(vec![state1])
        } else {
            Self::new(vec![state1, state2])
        }
    }

    /// Return a region with masked bit positions set to X.
    pub fn set_to_x(&self, msk: &SomeMask) -> Self {
        let state1 = self.state1().bitwise_or(msk);
        let state2 = self.state2().bitwise_and(&msk.bitwise_not());

        if state1 == state2 {
            Self::new(vec![state1])
        } else {
            Self::new(vec![state1, state2])
        }
    }

    /// Return the distance from a region to a state.
    pub fn distance_state(&self, stax: &SomeState) -> usize {
        self.diff_mask_state(stax).num_one_bits()
    }

    /// Return the distance from a region to another.
    pub fn distance(&self, regx: &SomeRegion) -> usize {
        self.diff_mask(regx).num_one_bits()
    }

    /// Return a mask of different bits with a given state.
    pub fn diff_mask_state(&self, sta1: &SomeState) -> SomeMask {
        self.state1()
            .bitwise_xor(sta1)
            .bitwise_and(&self.state2().bitwise_xor(sta1))
            .to_mask()
    }

    /// Return a non-x mask for a region.
    pub fn non_x_mask(&self) -> SomeMask {
        self.state1().bitwise_eqv(self.state2())
    }

    /// Return a mask of different, non-x, bits between two regions.
    pub fn diff_mask(&self, other: &SomeRegion) -> SomeMask {
        self.non_x_mask()
            .bitwise_and(&other.non_x_mask())
            .bitwise_and(&self.state1().bitwise_xor(other.state1()))
    }

    /// Given a region, and a second region, return the
    /// first region - the second
    pub fn subtract(&self, other: &SomeRegion) -> Vec<Self> {
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

    /// Return a string representing a vector of references to regions.
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

    /// Return a string representing a vector of regions.
    pub fn vec_string(avec: &[SomeRegion]) -> String {
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

    // Check any two region vectors for an intersection in their items.
    // Return intersection region.
    pub fn vec_ref_intersections(arg1: &[&Self], arg2: &[&Self]) -> Option<Self> {
        for regx in arg1.iter() {
            for regy in arg2.iter() {
                if regx.intersects(regy) {
                    return regx.intersection(regy);
                }
            }
        }
        None
    }

    /// Add a region, removing subset regions.
    pub fn vec_push_nosubs(sr_vec: &mut Vec<Self>, reg: Self) -> bool {
        // Check for supersets, which probably is an error
        if tools::vec_contains(sr_vec, &reg, Self::is_superset_of) {
            //println!("skipped adding region {}, a superset exists in {}", reg, self);
            return false;
        }

        // Identify subsets.
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in sr_vec.iter().enumerate() {
            if regx.is_subset_of(&reg) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in descending index order.
        for inx in rmvec.iter().rev() {
            removeunordered::remove_unordered(sr_vec, *inx);
        }

        sr_vec.push(reg);

        true
    }

    /// Split corresponding Regions by intersections, producing a result where each region is a subset
    /// of any intersecting original regions. All parts of the original Regions are accounted for in the
    /// result.
    pub fn vec_ref_split_to_subsets(sr_vec: &[&Self]) -> Vec<Self> {
        // Init return vector of RegionStores.
        let mut ret_str = Vec::<Self>::new();

        // Init temp vector, for RegionStore fragments.
        let mut tmp_str = Vec::<Self>::new();

        // Init vector to note indices of RegionStores that are split.
        // There may be a few duplicates, but that will not affect the use of the vector.
        let mut int_vec = Vec::<usize>::new();

        // Get initial fragments, from any two non-equal intersecting RegionStores.
        for inx in 0..(sr_vec.len() - 1) {
            for iny in (inx + 1)..sr_vec.len() {
                // Skip two equal regions.  If no other intersections, one will make it into the return vector.
                if sr_vec[inx] == sr_vec[iny] {
                    continue;
                }

                // If there is an intersection, split the RegionStores into fragments.
                if let Some(int) = sr_vec[inx].intersection(sr_vec[iny]) {
                    int_vec.push(inx);
                    int_vec.push(iny);

                    for rsz in sr_vec[inx].subtract(&int) {
                        Self::vec_push_nosubs(&mut tmp_str, rsz);
                    }

                    for rsz in sr_vec[iny].subtract(&int) {
                        Self::vec_push_nosubs(&mut tmp_str, rsz);
                    }
                    Self::vec_push_nosubs(&mut tmp_str, int);
                }
            }
        }
        // For Regions with no intersections, add to the return RS.
        for (iny, rsy) in sr_vec.iter().enumerate() {
            if !int_vec.contains(&iny) {
                Self::vec_push_nosubs(&mut ret_str, (*rsy).clone());
            }
        }

        // Look for additional non-subset intersections.
        loop {
            // Init vector for next pass.
            let mut next_pass = Vec::<Self>::new();

            // Check remaining fragments for additional intersections.
            // If no intersections are found, add the fragment to the return vector,
            // else add fragments of fragments to the next_pass vector.
            for rsy in tmp_str {
                let mut split = false;

                // Check for intersecting RegionStore from original argument.
                for rsx in sr_vec.iter() {
                    if let Some(int) = rsy.intersection(rsx) {
                        // Skip rsy if it is a subset, thats the end we are looking for.
                        if int == rsy {
                            continue;
                        }
                        // Split intersection into fragments.
                        // Add fragments to the next_pass vector.
                        for rsz in rsy.subtract(&int) {
                            Self::vec_push_nosubs(&mut next_pass, rsz);
                        }
                        // Add the intersection to the next_pass vector.
                        Self::vec_push_nosubs(&mut next_pass, int);
                        split = true;
                    }
                } // next rsx
                  // If no intersectiosn, add the fragment to the return vector.
                if !split {
                    Self::vec_push_nosubs(&mut ret_str, rsy);
                }
            } // next rsy

            // If no more fragments to check, return.
            if next_pass.is_empty() {
                return ret_str;
            }
            // Set up next fragments to check.
            tmp_str = next_pass;
        } // End loop
    }

    /// Return the adjacent part of two regions.
    /// A region implied, if the regions are held to be similar in some property.
    /// If the regions can form a non-optimistic union, the result will be that union.
    pub fn adjacent_part(&self, other: &Self) -> Self {
        assert!(self.is_adjacent(other));

        let msk = self.diff_mask(other);
        let reg1 = self.set_to_x(&msk);
        let reg2 = other.set_to_x(&msk);

        reg1.intersection(&reg2).unwrap()
    }

    /// Return true if two regions have the same edge and X bit positions.
    pub fn is_congruent(&self, other: &Self) -> bool {
        self.x_mask() == other.x_mask()
    }
} // end impl SomeRegion

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regionstore::RegionStore;
    use rand::Rng;

    #[test]
    fn test_new() -> Result<(), String> {
        let sta1 = SomeState::new_from_string(1, "s0b0000")?;
        let sta2 = SomeState::new_from_string(1, "s0b0011")?;
        let sta3 = SomeState::new_from_string(1, "s0b0101")?;

        let reg1 = SomeRegion::new(vec![sta1, sta2, sta3]);
        println!("reg1 is {}", reg1);

        let second = reg1.state2();
        println!(
            "reg1 first is {}, second is {}",
            reg1.state1(),
            reg1.state2()
        );

        assert!(second == &SomeState::new_from_string(1, "s0b0111")?);

        Ok(())
    }

    #[test]
    fn test_is_congruent() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        let reg2 = SomeRegion::new_from_string(1, "rx11x")?;
        assert!(reg1.is_congruent(&reg2));

        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        let reg2 = SomeRegion::new_from_string(1, "rx1x1")?;
        assert!(!reg1.is_congruent(&reg2));

        Ok(())
    }

    #[test]
    fn test_adjacent_part() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        let reg2 = SomeRegion::new_from_string(1, "rx11x")?;
        let reg3 = reg1.adjacent_part(&reg2);
        println!("adjacent part of {} and {} is {}", reg1, reg2, reg3);
        assert!(reg3 == SomeRegion::new_from_string(1, "rx1xx").unwrap());

        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        let reg2 = SomeRegion::new_from_string(1, "rx110")?;
        let reg3 = reg1.adjacent_part(&reg2);
        println!("adjacent part of {} and {} is {}", reg1, reg2, reg3);
        assert!(reg3 == SomeRegion::new_from_string(1, "rx1x0").unwrap());

        let reg1 = SomeRegion::new_from_string(1, "r000x")?;
        let reg2 = SomeRegion::new_from_string(1, "rxx11")?;
        let reg3 = reg1.adjacent_part(&reg2);
        println!("adjacent part of {} and {} is {}", reg1, reg2, reg3);
        assert!(reg3 == SomeRegion::new_from_string(1, "r00x1").unwrap());

        Ok(())
    }

    #[test]
    fn test_vec_ref_split_to_subsets() -> Result<(), String> {
        let mut rs_str = Vec::<&SomeRegion>::with_capacity(1);

        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        rs_str.push(&reg1);

        println!("Initial1: {}", SomeRegion::vec_ref_string(&rs_str));

        let rslt = SomeRegion::vec_ref_split_to_subsets(&rs_str);

        println!("Result1: {}", SomeRegion::vec_string(&rslt));

        assert!(rslt.len() == 1);
        assert!(rslt.contains(&reg1));

        let mut rs_str = Vec::<&SomeRegion>::with_capacity(2);

        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        rs_str.push(&reg1);
        rs_str.push(&reg1);

        println!("Initial2: {}", SomeRegion::vec_ref_string(&rs_str));

        let rslt = SomeRegion::vec_ref_split_to_subsets(&rs_str);

        println!("Result2: {}", SomeRegion::vec_string(&rslt));
        assert!(rslt.len() == 1);
        assert!(rslt.contains(&reg1));

        let mut rs_str = Vec::<&SomeRegion>::with_capacity(2);
        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        rs_str.push(&reg1);

        let reg2 = SomeRegion::new_from_string(1, "r010x")?;
        rs_str.push(&reg2);

        println!("Initial3: {}", SomeRegion::vec_ref_string(&rs_str));

        let rslt = SomeRegion::vec_ref_split_to_subsets(&rs_str);

        println!("Result3: {}", SomeRegion::vec_string(&rslt));
        assert!(rslt.len() == 2);
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r110x")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r010x")?));

        let mut rs_str = Vec::<&SomeRegion>::with_capacity(2);

        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        let reg2 = SomeRegion::new_from_string(1, "r010x")?;

        rs_str.push(&reg2);
        rs_str.push(&reg1);

        println!("Initial4: {}", SomeRegion::vec_ref_string(&rs_str));

        let rslt = SomeRegion::vec_ref_split_to_subsets(&rs_str);

        println!("Result4: {}", SomeRegion::vec_string(&rslt));
        assert!(rslt.len() == 2);
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r110x")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r010x")?));

        let mut rs_str = Vec::<&SomeRegion>::with_capacity(2);

        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        let reg2 = SomeRegion::new_from_string(1, "r1xx1")?;

        rs_str.push(&reg2);
        rs_str.push(&reg1);

        println!("Initial5: {}", SomeRegion::vec_ref_string(&rs_str));

        let rslt = SomeRegion::vec_ref_split_to_subsets(&rs_str);

        println!("Result5: {}", SomeRegion::vec_string(&rslt));
        assert!(rslt.len() == 5);
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "rx100")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r010x")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r1101")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r10x1")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r1101")?));

        let mut rs_str = Vec::<&SomeRegion>::with_capacity(2);

        let reg1 = SomeRegion::new_from_string(1, "rx10x")?;
        let reg2 = SomeRegion::new_from_string(1, "r01xx")?;
        let reg3 = SomeRegion::new_from_string(1, "rxxx1")?;

        rs_str.push(&reg2);
        rs_str.push(&reg1);
        rs_str.push(&reg3);

        println!("Initial6: {}", SomeRegion::vec_ref_string(&rs_str));

        let rslt = SomeRegion::vec_ref_split_to_subsets(&rs_str);

        println!("Result6: {}", SomeRegion::vec_string(&rslt));
        assert!(rslt.len() == 8);

        assert!(rslt.contains(&SomeRegion::new_from_string(1, "rx0x1")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r1100")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r1101")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r0110")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r0111")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r0100")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r0101")?));
        assert!(rslt.contains(&SomeRegion::new_from_string(1, "r1x11")?));

        Ok(())
    }

    #[test]
    fn edge_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "r101XX0")?;
        let edges = reg0.edge_mask();
        println!("Edges of {reg0} are {edges}");
        assert!(edges == SomeMask::new_from_string(1, "m0b1111_1001")?);
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
            println!("{} should equal {state1}", reg_instance.state1());
            assert!(reg_instance.state1() == &state1);
            println!("{} should equal {state1}", reg_instance.state2());
            assert!(reg_instance.state2() == &state2);
        }
        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let reg1 = SomeRegion::new(vec![
            SomeState::new_from_string(1, "s0b1010")?,
            SomeState::new_from_string(1, "s0b0101")?,
        ]);
        let reg2 = SomeRegion::new(vec![
            SomeState::new_from_string(1, "s0b0001")?,
            SomeState::new_from_string(1, "s0b1110")?,
        ]);
        println!("{reg1} should equal {reg2}");
        assert!(reg1.eq(&reg2));

        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let mut reg0 = SomeRegion::new_from_string(1, "r101XX1")?;
        let mut reg1 = SomeRegion::new_from_string(1, "rXX0011")?;
        println!("{reg0} s/b adjacent {reg1}");
        assert!(reg0.is_adjacent(&reg1));

        reg0 = SomeRegion::new_from_string(1, "rX10X01X")?;
        reg1 = SomeRegion::new_from_string(1, "rX10X10X")?;
        println!("{reg0} s/b adjacent {reg1}");
        assert!(!reg0.is_adjacent(&reg1));

        Ok(())
    }

    #[test]
    fn is_adjacent_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;
        let sta1 = SomeState::new_from_string(1, "s0b1001100")?;
        println!("{reg0} s/b adjacent {sta1}");
        assert!(reg0.is_adjacent_state(&sta1));

        let sta2 = SomeState::new_from_string(1, "s0b1001110")?;
        println!("{reg0} s/nb adjacent {sta2}");
        assert!(!reg0.is_adjacent_state(&sta2));

        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;
        let reg1 = SomeRegion::new_from_string(1, "r0XX110X")?;
        println!("{reg0} should intersect {reg1}");
        assert!(reg0.intersects(&reg1));

        let reg2 = SomeRegion::new_from_string(1, "r0XX111X")?;
        println!("{reg0} should not intersect {reg2}");
        assert!(!reg0.intersects(&reg2));

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;
        let reg1 = SomeRegion::new_from_string(1, "r0XX110X")?;

        if let Some(reg_int) = reg0.intersection(&reg1) {
            println!("Intersection of {reg0} and {reg1} is {reg_int}");
            assert!(reg_int == SomeRegion::new_from_string(1, "r010110X")?);
        } else {
            return Err(format!("{reg0} does not intersect {reg1}?"));
        }
        Ok(())
    }

    #[test]
    fn is_superset_of_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X")?;
        let sta1 = SomeState::new_from_string(1, "s0b1100")?;
        println!("{reg0} s/b superset of {sta1}");
        assert!(reg0.is_superset_of_state(&sta1));

        let sta2 = SomeState::new_from_string(1, "s0b0000")?;
        println!("{reg0} s/nb superset of {sta2}");
        assert!(!reg0.is_superset_of_state(&sta2));

        let sta3 = SomeState::new_from_string(1, "s0b0010")?;
        println!("{reg0} s/nb superset of {sta3}");
        assert!(!reg0.is_superset_of_state(&sta3));

        Ok(())
    }

    #[test]
    fn zeros_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "r00XX0101")?;
        let m1 = reg0.zeros_mask();
        println!("zeros_mask is {m1}");
        assert!(m1 == SomeMask::new_from_string(1, "m0b11001010")?);
        Ok(())
    }

    #[test]
    fn ones_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "r00XX0101")?;
        let m1 = reg0.ones_mask();
        println!("ones_mask is {m1}");
        assert!(m1 == SomeMask::new_from_string(1, "m0b101")?);
        Ok(())
    }

    #[test]
    fn x_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "r00XX0101")?;
        let m1 = reg0.x_mask();
        println!("x_mask is {m1}");
        assert!(m1 == SomeMask::new_from_string(1, "m0b110000")?);
        Ok(())
    }

    #[test]
    fn non_x_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "r0000xx01")?;
        let m1 = reg0.non_x_mask();
        println!("non_x_mask is {m1}");
        assert!(m1 == SomeMask::new_from_string(1, "m0b11110011")?);
        Ok(())
    }

    #[test]
    fn far_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "r0000XXX1")?;
        let state0 = SomeState::new_from_string(1, "s0b00001011")?;
        let far_state = reg0.far_state(&state0);
        println!("far state is {far_state}");
        assert!(far_state == SomeState::new_from_string(1, "s0b101")?);
        Ok(())
    }

    #[test]
    fn far_reg() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(2, "rXXX01000000")?;
        let reg1 = SomeRegion::new_from_string(2, "r01X01000000")?;
        let far_reg = reg0.far_reg(&reg1);
        println!("far_reg is {far_reg}");
        assert!(far_reg == SomeRegion::new_from_string(2, "r10X01000000")?);
        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X")?;
        let reg1 = SomeRegion::new_from_string(1, "rX10X")?;
        if !reg0.is_subset_of(&reg1) {
            return Err(format!("{reg0} not subset {reg1}?"));
        }

        let reg2 = SomeRegion::new_from_string(1, "rXXXX")?;
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
        let reg0 = SomeRegion::new_from_string(1, "rX10X")?;
        let reg1 = SomeRegion::new_from_string(1, "rX10X")?;

        if !reg0.is_superset_of(&reg1) {
            return Err(format!("{reg0} not superset {reg1}?"));
        }

        let reg2 = SomeRegion::new_from_string(1, "rXXXX")?;

        if !reg2.is_superset_of(&reg0) {
            return Err(format!("{reg2} not superset {reg0}?"));
        }

        if reg0.is_superset_of(&reg2) {
            return Err(format!("{reg0} is superset {reg2}?"));
        }
        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(2, "r111000XXX")?;
        let reg1 = SomeRegion::new_from_string(2, "r01X01X01X")?;
        let reg2 = reg0.union(&reg1);
        println!("union is {reg2}");
        assert!(reg2 == SomeRegion::new_from_string(2, "rX1X0XXXXX")?);
        Ok(())
    }

    #[test]
    fn union_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101")?;
        let state0 = SomeState::new_from_string(1, "s0b101001")?;
        let reg2 = reg0.union_state(&state0);
        println!("{reg0} union {state0} is {reg2}");
        assert!(reg2 == SomeRegion::new_from_string(1, "rXXXX01")?);

        Ok(())
    }

    #[test]
    fn high_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX0X1")?;
        let hs = reg0.high_state();
        println!("High state of {reg0} is {hs}");
        assert!(hs == SomeState::new_from_string(1, "s0b1011")?);

        Ok(())
    }

    #[test]
    fn low_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX0X1")?;
        let ls = reg0.low_state();
        println!("Low state of {reg0} is {ls}");
        assert!(ls == SomeState::new_from_string(1, "s0b1")?);

        Ok(())
    }

    #[test]
    fn set_to_zeros() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;
        let msk1 = SomeMask::new_from_string(1, "m0b111")?;
        let reg1 = reg0.set_to_zeros(&msk1);
        println!("{reg0} set_to_zeros {msk1} is {reg1}");
        assert!(reg1 == SomeRegion::new_from_string(1, "rX10X000")?);

        Ok(())
    }

    #[test]
    fn set_to_ones() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X10X")?;
        let msk1 = SomeMask::new_from_string(1, "m0b111")?;
        let reg1 = reg0.set_to_ones(&msk1);
        println!("{reg0} set_to_ones {msk1} is {reg1}");
        assert!(reg1 == SomeRegion::new_from_string(1, "rX10X111")?);

        Ok(())
    }

    #[test]
    fn diff_mask_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0011")?;
        let state0 = SomeState::new_from_string(1, "s0b010101")?;
        let dif_msk = reg0.diff_mask_state(&state0);
        println!("{reg0} diff_mask {state0} is {dif_msk}");
        assert!(dif_msk == SomeMask::new_from_string(1, "m0b110")?);

        Ok(())
    }

    #[test]
    fn distance_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0011")?;
        let state0 = SomeState::new_from_string(1, "s0b010101")?;
        let dist = reg0.distance_state(&state0);
        println!("{reg0} distance {state0} is {dist}");
        assert!(dist == 2);

        Ok(())
    }

    #[test]
    fn diff_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(2, "r1XX000111")?;
        let reg1 = SomeRegion::new_from_string(2, "r01X01X01X")?;
        let diff = reg0.diff_mask(&reg1);
        println!("{reg0} diff_mask {reg1} is {diff}");
        assert!(diff == SomeMask::new_from_string(2, "m0b100010100")?);

        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rX10X")?;
        let reg1 = SomeRegion::new_from_string(1, "r0XX1")?;
        let regs = RegionStore::new(reg0.subtract(&reg1));
        println!("{reg0} subtract {reg1} = {regs}");

        assert!(regs.len() == 2);

        assert!(regs.contains(&SomeRegion::new_from_string(1, "rX100")?));
        assert!(regs.contains(&SomeRegion::new_from_string(1, "r110X")?));

        // Test subtract a superset.
        let reg3 = SomeRegion::new_from_string(1, "rXXX1")?;
        let regs = RegionStore::new(reg1.subtract(&reg3));
        println!("{reg0} subtract {reg3} = {regs}");
        assert!(regs.is_empty());

        // Test no intersection.
        let reg3 = SomeRegion::new_from_string(1, "rXX11")?;
        let regs = RegionStore::new(reg0.subtract(&reg3));
        println!("{reg0} subtract {reg3} = {regs}");
        assert!(regs.len() == 1);
        assert!(regs.contains(&reg0));

        Ok(())
    }

    // Test num_x.
    #[test]
    fn num_x() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "rXX0101")?;
        let numx = reg0.num_x();
        println!("Num_x {reg0} is {numx}");
        assert!(numx == 2);

        Ok(())
    }
} // end tests
