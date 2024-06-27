//! The SomeRegion struct, representing a region on a pseudo Karnaugh Map.
//!
//! Uses one, or more, states to represent a region.
//!

use crate::bits::{vec_same_num_bits, BitsRef, NumBits, SomeBits};
use crate::mask::SomeMask;
use crate::state::SomeState;
use crate::tools::{self, not, StrLen};

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
/// SomeRegion struct.
/// Vector for one, or more, states.
/// If more than one state is used, the last state will be the farthest from the first state.
pub struct SomeRegion {
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
    /// Create new region from one, or more, states.
    ///
    /// For a group region, the first state will correspond to a square that has been sampled.
    ///
    /// Duplicate, and uneeded states (states between two states), are removed.
    ///
    /// If a region has more than two states, the far_state, from the first state, is calculated and added.
    pub fn new(mut states: Vec<SomeState>) -> Self {
        assert!(!states.is_empty());
        debug_assert!(vec_same_num_bits(&states));

        // Check for single-state region.
        if states.len() == 1 {
            return Self { states };
        }

        // Check state num_bits.
        let first_bits = states[0].num_bits();
        for stax in states.iter().skip(1) {
            assert!(stax.num_bits() == first_bits);
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
        states.push(far_state);

        Self { states }
    }

    /// Return a reference to the first state.
    pub fn first_state(&self) -> &SomeState {
        &self.states[0]
    }

    /// Return a reference to the far state.
    pub fn far_state(&self) -> &SomeState {
        self.states.last().expect("SNH")
    }

    /// Return a Region from a string.
    /// All bits must be specified.
    ///
    /// if let Ok(regx) = SomeRegion::new_from_string("1_01x1")) {
    ///    println!("Region {}", regx);
    /// } else {
    ///    panic!("Invalid Region");
    /// }
    ///
    /// A state string can be used, like "s0b101010" or s0x34", making
    /// a region with no X bit positions.
    ///
    /// The case of an X bit position gives information about the two states that form the region.
    /// X = (1, 0).
    /// x = (0, 1).
    /// XxXx = (1010, 0101).
    ///
    /// A first character of "r", or "s", is supported for cut-and-paste from output on console.
    pub fn new_from_string(str: &str) -> Result<Self, String> {
        let mut skip = 0;
        // Check the first character.
        if let Some(char0) = str.graphemes(true).nth(0) {
            if char0 == "r" || char0 == "R" {
                skip = 1;
            } else if char0 == "s" || char0 == "S" {
                // Create a region from a single state.
                // An advantage is that hexadecimal digits can be used if there are no X-bit positions.
                let stax = SomeState::new_from_string(str)?;
                return Ok(SomeRegion::new(vec![stax]));
            }
        } else {
            return Err(format!(
                "SomeRegion::new_from_string: String {str}, no valid character?"
            ));
        }

        // Translate the region string into two state strings.
        let mut b_high = String::from("0b");
        let mut b_low = String::from("0b");

        for chr in str.graphemes(true).skip(skip) {
            if chr == "0" {
                b_high.push('0');
                b_low.push('0');
            } else if chr == "1" {
                b_high.push('1');
                b_low.push('1');
            } else if chr == "X" {
                b_high.push('1');
                b_low.push('0');
            } else if chr == "x" {
                b_high.push('0');
                b_low.push('1');
            } else if chr == "_" {
                continue;
            } else if chr == "+" {
                b_high.push('+');
                b_low.push('+');
            } else {
                return Err(format!(
                    "Did not understand the string {str}, invalid character?"
                ));
            }
        } // end for chr

        // Translate state strings to state instances.
        let sta_high = SomeState::new_from_string(&b_high)?;
        let sta_low = SomeState::new_from_string(&b_low)?;

        // Return region from states.
        Ok(Self::new(vec![sta_high, sta_low]))
    } // end new_from_string

    /// Return a String representation of a Region.
    /// The case of an X bit position gives information about the first state, and far state, of the region.
    /// X = (1, 0).
    /// x = (0, 1).
    /// XxXx = (1010, ... 0101).
    fn formatted_string(&self) -> String {
        let mut s1 = String::with_capacity(self.strlen());

        let first_state_str = format!("{}", self.first_state());
        let far_state_str = format!("{}", self.far_state());

        for (chr1, chr2) in first_state_str
            .graphemes(true)
            .zip(far_state_str.graphemes(true))
        {
            if chr1 == "s" {
                s1.push('r');
                continue;
            }
            if chr1 == "_" {
                s1.push('_');
                continue;
            }
            let b0 = chr1 == "1";
            let b1 = chr2 == "1";

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
        debug_assert_eq!(self.num_bits(), other.num_bits());

        self.diff_edge_mask(other).just_one_bit()
    }

    /// Return true if two regions intersect.
    pub fn intersects(&self, other: &impl AccessStates) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        self.diff_edge_mask(other).is_low()
    }

    /// Return the intersection a region and a region or state..
    pub fn intersection(&self, other: &impl AccessStates) -> Option<Self> {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if !self.intersects(other) {
            return None;
        }
        if self.one_state() {
            Some(Self::new(vec![self.first_state().clone()]))
        } else if other.one_state() {
            Some(Self::new(vec![other.first_state().clone()]))
        } else {
            let lower_high_state = self.high_state().bitwise_and(&other.high_state());
            let higher_low_state = self.low_state().bitwise_or(&other.low_state());
            Some(Self::new(vec![lower_high_state, higher_low_state]))
        }
    }

    /// Return a Mask of zero positions.
    pub fn edge_zeros_mask(&self) -> SomeMask {
        self.first_state()
            .bitwise_not()
            .bitwise_and_not(self.far_state())
            .to_mask()
    }

    /// Return a Mask of one positions.
    pub fn edge_ones_mask(&self) -> SomeMask {
        self.first_state().bitwise_and(self.far_state()).to_mask()
    }

    /// Return a mask of edge (non-X) bits.
    pub fn edge_mask(&self) -> SomeMask {
        self.first_state().bitwise_eqv(self.far_state())
    }

    /// Return mask of x positions.
    pub fn x_mask(&self) -> SomeMask {
        self.first_state().bitwise_xor(self.far_state()).to_mask()
    }

    /// Given a state in a region, return the far state in the region.
    pub fn state_far_from(&self, sta: &SomeState) -> SomeState {
        debug_assert_eq!(self.num_bits(), sta.num_bits());
        assert!(self.is_superset_of(sta));
        self.first_state()
            .bitwise_xor(self.far_state())
            .bitwise_xor(sta)
    }

    /// Given a region, and a proper subset region, return the
    /// far region within the superset region.
    pub fn far_reg(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());
        assert!(self.is_superset_of(other));
        assert!(self != other);

        let cng_mask = self.x_mask().bitwise_xor(&other.x_mask());

        SomeRegion::new(vec![
            other.first_state().bitwise_xor(&cng_mask),
            other.far_state().bitwise_xor(&cng_mask),
        ])
    }

    /// Return true if a region is subset, or equal, on another region.
    pub fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if self.intersects(other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_subset_ones_of(&x2);
        }
        false
    }

    /// Return true if a region is superset, or equal, on another region.
    pub fn is_superset_of(&self, other: &impl AccessStates) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if self.intersects(other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_superset_ones_of(&x2);
        }
        false
    }

    /// Return the union of a region and a region/state.
    pub fn union(&self, other: &impl AccessStates) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        //println!("union {} and {}", self, other);
        let st_low = self.low_state().bitwise_and(&other.low_state());
        let st_high = self.high_state().bitwise_or(&other.high_state());
        Self::new(vec![st_high, st_low])
    }

    /// Return the highest state in the region
    pub fn high_state(&self) -> SomeState {
        self.first_state().bitwise_or(self.far_state())
    }

    /// Return lowest state in the region
    pub fn low_state(&self) -> SomeState {
        self.first_state().bitwise_and(self.far_state())
    }

    /// Return a region with masked X-bits set to zeros.
    pub fn set_to_zeros(&self, msk: &SomeMask) -> Self {
        debug_assert_eq!(self.num_bits(), msk.num_bits());

        let first_state = self.first_state().bitwise_and_not(msk);
        let far_state = self.far_state().bitwise_and_not(msk);

        Self::new(vec![first_state, far_state])
    }

    /// Return a region with masked bit positions set to X.
    pub fn set_to_x(&self, msk: &SomeMask) -> Self {
        debug_assert_eq!(self.num_bits(), msk.num_bits());

        let first_state = self.first_state().bitwise_or(msk);
        let far_state = self.far_state().bitwise_and(&msk.bitwise_not());

        Self::new(vec![first_state, far_state])
    }

    /// Return a region with masked X-bits set to ones.
    pub fn set_to_ones(&self, msk: &SomeMask) -> Self {
        debug_assert_eq!(self.num_bits(), msk.num_bits());

        let first_state = self.first_state().bitwise_or(msk);
        let far_state = self.far_state().bitwise_or(msk);

        Self::new(vec![first_state, far_state])
    }

    /// Return the distance from a region to a region/state.
    pub fn distance(&self, other: &impl AccessStates) -> usize {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        self.diff_edge_mask(other).num_one_bits()
    }

    /// Return a mask of different edge bits between a region and a region/state.
    pub fn diff_edge_mask(&self, other: &impl AccessStates) -> SomeMask {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        self.edge_mask()
            .bitwise_and(&other.edge_mask())
            .bitwise_and(&self.first_state().bitwise_xor(other.first_state()))
    }

    /// Given a region, and a second region, return the
    /// first region minus the second.
    pub fn subtract(&self, other: &impl AccessStates) -> Vec<Self> {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        let mut ret_vec = Vec::<Self>::new();

        // If no intersection, return self.
        if not(self.intersects(other)) {
            return vec![self.clone()];
        }

        // If other is a superset, return empty vector.
        if other.is_superset_of(self) {
            return ret_vec;
        }

        let reg_int = self.intersection(other).unwrap();

        // Split by X over 0 or 1.
        let x_over_not_xs: Vec<SomeMask> = self.x_mask().bitwise_and(&reg_int.edge_mask()).split();

        for mskx in x_over_not_xs.iter() {
            if mskx.bitwise_and(reg_int.first_state()).is_low() {
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
        debug_assert_eq!(self.num_bits(), substa.num_bits());
        debug_assert_eq!(self.num_bits(), supsta.num_bits());

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

    /// Return the number of edges in a region.
    pub fn num_edges(&self) -> usize {
        self.edge_mask().num_one_bits()
    }

    /// Return the number of states defining the region.
    pub fn len(&self) -> usize {
        self.states.len()
    }

    /// Translate one region into another.
    pub fn translate_to(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        // Calc self bit position masks.
        let self_x = self.x_mask();
        let self_1 = self.edge_ones_mask();
        let self_0 = self.edge_zeros_mask();

        // Calc other bit position masks.
        let other_1 = other.edge_ones_mask();
        let other_0 = other.edge_zeros_mask();

        // Calc needed changes.
        let to_0 = self_x.bitwise_or(&self_1).bitwise_and(&other_0);
        let to_1 = self_x.bitwise_or(&self_0).bitwise_and(&other_1);

        self.set_to_ones(&to_1).set_to_zeros(&to_0)
    }

    /// Retun the number of bits used to describe region states.
    pub fn num_bits(&self) -> usize {
        self.states[0].num_bits()
    }

    /// Return a bridge between two regions.
    pub fn bridge(&self, other: &Self) -> Option<Self> {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if self.intersects(other) {
            return None;
        }

        let to_x_mask = self.diff_edge_mask(other);
        self.set_to_x(&to_x_mask)
            .intersection(&other.set_to_x(&to_x_mask))
    }
} // end impl SomeRegion

/// Implement the trait StrLen for SomeRegion.
impl StrLen for SomeRegion {
    fn strlen(&self) -> usize {
        self.first_state().strlen()
    }
}

/// Define the AccessStates trait, so operations on Regions, States, Squares and Groups are smoother.
/// A region defined by a single state, is similar to a single state.
pub trait AccessStates {
    fn one_state(&self) -> bool;
    fn first_state(&self) -> &SomeState;
    fn x_mask(&self) -> SomeMask;
    fn edge_mask(&self) -> SomeMask;
    fn high_state(&self) -> SomeState;
    fn low_state(&self) -> SomeState;
    fn diff_edge_mask(&self, other: &impl AccessStates) -> SomeMask;
    fn intersects(&self, other: &impl AccessStates) -> bool;
    fn is_subset_of(&self, other: &impl AccessStates) -> bool;
    fn is_superset_of(&self, other: &impl AccessStates) -> bool;
    fn num_bits(&self) -> usize;
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
    fn diff_edge_mask(&self, other: &impl AccessStates) -> SomeMask {
        self.diff_edge_mask(other)
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
    fn num_bits(&self) -> usize {
        self.num_bits()
    }
}

/// Implement the trait BitsRef for SomeGroup.
impl BitsRef for SomeRegion {
    fn bitsref(&self) -> &SomeBits {
        self.first_state().bitsref()
    }
}

/// Implement the NumBits trait for SomeRegion.
impl NumBits for SomeRegion {
    fn num_bits(&self) -> usize {
        self.first_state().num_bits()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::regionstore::RegionStore;
    use rand::Rng;

    #[test]
    fn translate_to() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string("XX0+101X")?;
        let reg2 = SomeRegion::new_from_string("10X+X10X")?;
        println!("reg1 {reg1}");
        println!("reg2 {reg2}");

        let reg3 = reg1.translate_to(&reg2);
        println!("reg3 {reg3}");
        if reg3 != SomeRegion::new_from_string("r100+110X")? {
            return Err(format!("{reg3} ??"));
        }
        Ok(())
    }

    #[test]
    fn bridge() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string("rX101")?;
        let reg2 = SomeRegion::new_from_string("rX111")?;
        let reg3 = SomeRegion::new_from_string("r0X10")?;

        if let Some(result) = reg1.bridge(&reg2) {
            if result == SomeRegion::new_from_string("rX1X1")? {
            } else {
                return Err(format!("{reg1} bridge {reg2} == {result}"));
            }
        } else {
            return Err(format!("{reg1} bridge {reg2} == None?"));
        }

        if let Some(result) = reg1.bridge(&reg3) {
            if result == SomeRegion::new_from_string("r01XX")? {
            } else {
                return Err(format!("{reg1} bridge {reg3} == {result}"));
            }
        } else {
            return Err(format!("{reg1} bridge {reg3} == None?"));
        }

        if let Some(result) = reg2.bridge(&reg3) {
            println!("result {result}");
            if result != SomeRegion::new_from_string("r011X")? {
                return Err(format!("{reg2} bridge {reg3} = {result}?"));
            }
        } else {
            return Err(format!("{reg2} {reg3} = None?"));
        }

        Ok(())
    }

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(8))]);
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(16))]);
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(6))]);
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(5))]);
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(4))]);
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn subtract_state_to_supersets_of() -> Result<(), String> {
        let sta0 = SomeState::new_from_string("s0b0000")?;
        let staf = SomeState::new_from_string("s0b1111")?;

        let reg1 = SomeRegion::new(vec![sta0, staf]);

        let sta5 = SomeState::new_from_string("s0b0101")?;
        let sta6 = SomeState::new_from_string("s0b0110")?;
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
    fn new() -> Result<(), String> {
        // Single state region.

        let sta1 = SomeState::new_from_string("s0b0001")?;

        let reg1 = SomeRegion::new(vec![sta1.clone(), sta1.clone()]);
        println!("reg1 is {}", reg1);
        assert!(reg1.states.len() == 1);
        assert!(reg1.far_state() == &sta1);

        // Two state region.
        let sta7 = SomeState::new_from_string("s0b0111")?;

        let reg2 = SomeRegion::new(vec![sta1.clone(), sta7.clone()]);
        println!("reg2 is {}", reg2);
        assert!(reg2.states.len() == 2);
        assert!(reg2.far_state() == &sta7);

        // Three state region.
        let sta2 = SomeState::new_from_string("s0b0010")?;
        let reg3 = SomeRegion::new(vec![sta1.clone(), sta7.clone(), sta2.clone()]);
        println!("reg3 is {}", reg3);
        assert!(reg3.states.len() == 4);

        println!("reg3 first_state = {}", reg3.first_state());
        assert!(reg3.first_state() == &sta1);

        let sta6 = SomeState::new_from_string("s0b0110")?;
        println!("reg3 far_state = {}", reg3.far_state());
        assert!(reg3.far_state() == &sta6);

        // Three states, only two needed due to far state being in the list.
        let sta2 = SomeState::new_from_string("s0b0010")?;
        let reg4 = SomeRegion::new(vec![sta1.clone(), sta6.clone(), sta2.clone()]);
        println!("reg4 is {}", reg4);
        assert!(reg4.states.len() == 2);

        println!("reg4 first_state = {}", reg4.first_state());
        assert!(reg4.first_state() == &sta1);

        println!("reg4 far_state = {}", reg4.far_state());
        assert!(reg4.far_state() == &sta6);

        // Three state region, five states given.
        // State 1, between 0 and 5, will be deleted.
        // State 2, between 6 and 0, will be deleted.
        let sta0 = SomeState::new_from_string("s0b0000")?;
        let sta5 = SomeState::new_from_string("s0b0101")?;
        let reg5 = SomeRegion::new(vec![
            sta0.clone(),
            sta1.clone(),
            sta2.clone(),
            sta5.clone(),
            sta6.clone(),
        ]);

        println!("reg5 is {}", reg5);
        assert!(reg5.states.len() == 4);

        println!("reg5 far_state = {}", reg5.far_state());
        assert!(reg5.far_state() == &sta7);

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

        println!("reg6 first_state = {}", reg6.first_state());
        assert!(reg6.first_state() == &sta1);

        println!("reg6 far_state = {}", reg6.far_state());
        assert!(reg6.far_state() == &sta6);

        Ok(())
    }

    #[test]
    fn edge_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("r00101XX0")?;
        let edges = reg0.edge_mask();
        println!("Edges of {reg0} are {edges}");
        assert!(
            edges.bitwise_and(&SomeBits::new_from_string("0xff")?)
                == SomeMask::new_from_string("m0b1111_1001")?
        );
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
            let reg_instance = SomeRegion::new_from_string(&reg_from_str)?;

            // Check for the expected states forming the region.
            let first_state_str =
                "s0b".to_string() + &reg_from_str.replace("x", "0").replace("X", "1")[1..];
            let first_state = SomeState::new_from_string(&first_state_str)?;
            let far_state = reg_instance.state_far_from(&first_state);
            println!("{} should equal {first_state}", reg_instance.first_state());
            assert!(reg_instance.first_state() == &first_state);
            println!("{} should equal {first_state}", reg_instance.far_state());
            assert!(reg_instance.far_state() == &far_state);
        }

        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let reg1 = SomeRegion::new(vec![
            SomeState::new_from_string("s0b1010")?,
            SomeState::new_from_string("s0b0101")?,
        ]);
        let reg2 = SomeRegion::new(vec![
            SomeState::new_from_string("s0b0001")?,
            SomeState::new_from_string("s0b1110")?,
        ]);
        println!("{reg1} should equal {reg2}");
        assert!(reg1.eq(&reg2));

        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let mut reg0 = SomeRegion::new_from_string("r101XX1")?;
        let mut reg1 = SomeRegion::new_from_string("rXX0011")?;
        println!("{reg0} s/b adjacent {reg1}");
        assert!(reg0.is_adjacent(&reg1));

        reg0 = SomeRegion::new_from_string("rX10X01X")?;
        reg1 = SomeRegion::new_from_string("rX10X10X")?;
        println!("{reg0} s/b adjacent {reg1}");
        assert!(!reg0.is_adjacent(&reg1));

        let reg0 = SomeRegion::new_from_string("rX10X10X")?;
        let sta1 = SomeState::new_from_string("s0b1001100")?;
        println!("{reg0} s/b adjacent {sta1}");
        assert!(reg0.is_adjacent(&sta1));

        let sta2 = SomeState::new_from_string("s0b1001110")?;
        println!("{reg0} s/nb adjacent {sta2}");
        assert!(!reg0.is_adjacent(&sta2));

        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("rX10X10X")?;
        let reg1 = SomeRegion::new_from_string("r0XX110X")?;
        println!("{reg0} should intersect {reg1}");
        assert!(reg0.intersects(&reg1));

        let reg2 = SomeRegion::new_from_string("r0XX111X")?;
        println!("{reg0} should not intersect {reg2}");
        assert!(!reg0.intersects(&reg2));

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        // Test normal intersection.
        let reg0 = SomeRegion::new_from_string("rX10X")?;
        let reg1 = SomeRegion::new_from_string("r110X")?;

        let reg_int = reg0.intersection(&reg1).ok_or("SNH")?;
        println!("Intersection of {reg0} and {reg1} is {reg_int}");
        assert!(reg_int == SomeRegion::new_from_string("r110X")?);

        // Test non-intersection "intersection"
        let reg0 = SomeRegion::new_from_string("rX101")?;
        let reg1 = SomeRegion::new_from_string("r0X10")?;

        if let Some(reg_int) = reg0.intersection(&reg1) {
            return Err(format!("Intersection of {reg0} and {reg1} is {reg_int}?"));
        }

        Ok(())
    }

    #[test]
    fn edge_zeros_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("r00XX0101")?;
        let m1 = reg0.edge_zeros_mask();
        println!("edge_zeros_mask is {m1}");
        assert!(
            m1.bitwise_and(&SomeBits::new_from_string("0xff")?)
                == SomeMask::new_from_string("m0b11001010")?
        );
        Ok(())
    }

    #[test]
    fn edge_ones_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("r00XX0101")?;
        let m1 = reg0.edge_ones_mask();
        println!("edge_ones_mask is {m1}");
        assert!(m1 == SomeMask::new_from_string("m0b00000101")?);
        Ok(())
    }

    #[test]
    fn x_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("r00XX0101")?;
        let m1 = reg0.x_mask();
        println!("x_mask is {m1}");
        assert!(m1 == SomeMask::new_from_string("m0b00110000")?);
        Ok(())
    }

    #[test]
    fn non_x_mask() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("r0000xx01")?;
        let m1 = reg0.edge_mask();
        println!("non_x_mask is {m1}");
        assert!(
            m1.bitwise_and(&SomeBits::new_from_string("0xff")?)
                == SomeMask::new_from_string("m0b11110011")?
        );
        Ok(())
    }

    #[test]
    fn far_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("r0000XXX1")?;
        let state0 = SomeState::new_from_string("s0b00001011")?;
        let far_state = reg0.state_far_from(&state0);
        println!("far state is {far_state}");
        assert!(far_state == SomeState::new_from_string("s0b00000101")?);
        Ok(())
    }

    #[test]
    fn far_reg() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("rXXX01")?;
        let reg1 = SomeRegion::new_from_string("r01X01")?;
        let far_reg = reg0.far_reg(&reg1);
        println!("far_reg is {far_reg}");
        assert!(far_reg == SomeRegion::new_from_string("r10X01")?);
        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("rX10X")?;
        let reg1 = SomeRegion::new_from_string("rX10X")?;
        if !reg0.is_subset_of(&reg1) {
            return Err(format!("{reg0} not subset {reg1}?"));
        }

        let reg2 = SomeRegion::new_from_string("rXXXX")?;
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
        let reg0 = SomeRegion::new_from_string("rX10X")?;
        let reg1 = SomeRegion::new_from_string("rX10X")?;

        if !reg0.is_superset_of(&reg1) {
            return Err(format!("{reg0} not superset {reg1}?"));
        }

        let reg2 = SomeRegion::new_from_string("rXXXX")?;

        if !reg2.is_superset_of(&reg0) {
            return Err(format!("{reg2} not superset {reg0}?"));
        }

        if reg0.is_superset_of(&reg2) {
            return Err(format!("{reg0} is superset {reg2}?"));
        }

        let reg0 = SomeRegion::new_from_string("rX10X")?;
        let sta1 = SomeState::new_from_string("s0b1100")?;
        println!("{reg0} s/b superset of {sta1}");
        assert!(reg0.is_superset_of(&sta1));

        let sta2 = SomeState::new_from_string("s0b0000")?;
        println!("{reg0} s/nb superset of {sta2}");
        assert!(!reg0.is_superset_of(&sta2));

        let sta3 = SomeState::new_from_string("s0b0010")?;
        println!("{reg0} s/nb superset of {sta3}");
        assert!(!reg0.is_superset_of(&sta3));

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let reg2a = SomeRegion::new_from_string("rXX0011")?; // Region >1 state.
        let reg2b = SomeRegion::new_from_string("rXX0111")?; // Region >1 state.
        let reg1a = SomeRegion::new_from_string("r000101")?; // Region =1 state.
        let reg1b = SomeRegion::new_from_string("r000011")?; // Region =1 state.
        let sta0 = SomeState::new_from_string("s0b010101")?; // State, =1 state.

        // Region >1 state, Region >1 state.
        let union = reg2a.union(&reg2b);
        println!("{reg2a} union {reg2b} is {union}");
        assert!(union == SomeRegion::new_from_string("rxx0x11")?);

        // Region >1 state, Region =1 state.
        let union = reg2a.union(&reg1a);
        println!("{reg2a} union {reg1a} is {union}");
        assert!(union == SomeRegion::new_from_string("rxx0xx1")?);

        // Region >1 state, state.
        let union = reg2a.union(&sta0);
        println!("{reg2a} union {sta0} is {union}");
        assert!(union == SomeRegion::new_from_string("rxx0xx1")?);

        // Region =1 state, Region =1 state.
        let union = reg1a.union(&reg1b);
        println!("{reg1a} union {reg1b} is {union}");
        assert!(union == SomeRegion::new_from_string("r000xx1")?);

        // Region =1 state, Region >1 state.
        let union = reg1a.union(&reg2a);
        println!("{reg1a} union {reg2a} is {union}");
        assert!(union == SomeRegion::new_from_string("rXX0xx1")?);

        // Region =1 state, state.
        let union = reg1a.union(&sta0);
        println!("{reg1a} union {sta0} is {union}");
        assert!(union == SomeRegion::new_from_string("r0x_0101")?);

        Ok(())
    }

    #[test]
    fn high_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("rX0X1")?;
        let hs = reg0.high_state();
        println!("High state of {reg0} is {hs}");
        assert!(hs == SomeState::new_from_string("s0b1011")?);

        Ok(())
    }

    #[test]
    fn low_state() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("rX0X1")?;
        let ls = reg0.low_state();
        println!("Low state of {reg0} is {ls}");
        assert!(ls == SomeState::new_from_string("s0b0001")?);

        Ok(())
    }

    #[test]
    fn set_to_zeros() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("rX10X10X")?;
        let msk1 = SomeMask::new_from_string("m0b0000111")?;
        let reg1 = reg0.set_to_zeros(&msk1);
        println!("{reg0} set_to_zeros {msk1} is {reg1}");
        assert!(reg1 == SomeRegion::new_from_string("rX10X000")?);

        Ok(())
    }

    #[test]
    fn set_to_ones() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("rX10X10X")?;
        let msk1 = SomeMask::new_from_string("m0b0000111")?;
        let reg1 = reg0.set_to_ones(&msk1);
        println!("{reg0} set_to_ones {msk1} is {reg1}");
        assert!(reg1 == SomeRegion::new_from_string("rX10X111")?);

        Ok(())
    }

    #[test] // Also tests diff_mask.
    fn distance() -> Result<(), String> {
        let reg2a = SomeRegion::new_from_string("rXX0011")?; // Region >1 state.
        let reg2b = SomeRegion::new_from_string("rXX0111")?; // Region >1 state.
        let reg1a = SomeRegion::new_from_string("r000101")?; // Region =1 state.
        let reg1b = SomeRegion::new_from_string("r000011")?; // Region =1 state.
        let sta0 = SomeState::new_from_string("s0b010101")?; // State, =1 state.

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
        let reg2a = SomeRegion::new_from_string("rXX0011")?; // Region >1 state.
        let reg2b = SomeRegion::new_from_string("rXX0111")?; // Region >1 state.
        let reg1a = SomeRegion::new_from_string("r000101")?; // Region =1 state.
        let reg1b = SomeRegion::new_from_string("r000011")?; // Region =1 state.
        let sta0 = SomeState::new_from_string("s0b010101")?; // State, =1 state.

        // Region >1 state, Region >1 state.
        let diff = reg2a.diff_edge_mask(&reg2b);
        println!("{reg2a} diff_mask {reg2b} is {diff}");
        assert!(diff == SomeMask::new_from_string("m0b000100")?);

        // Region >1 state, Region =1 state.
        let diff = reg2a.diff_edge_mask(&reg1a);
        println!("{reg2a} diff_mask {reg1a} is {diff}");
        assert!(diff == SomeMask::new_from_string("m0b000110")?);

        // Region >1 state, state.
        let diff = reg2a.diff_edge_mask(&sta0);
        println!("{reg2a} diff_mask {sta0} is {diff}");
        assert!(diff == SomeMask::new_from_string("m0b000110")?);

        // Region =1 state, Region =1 state.
        let diff = reg1a.diff_edge_mask(&reg1b);
        println!("{reg1a} diff_mask {reg1b} is {diff}");
        assert!(diff == SomeMask::new_from_string("m0b000110")?);

        // Region =1 state, Region >1 state.
        let diff = reg1a.diff_edge_mask(&reg2a);
        println!("{reg1a} diff_mask {reg2a} is {diff}");
        assert!(diff == SomeMask::new_from_string("m0b000110")?);

        // Region =1 state, state.
        let diff = reg1a.diff_edge_mask(&sta0);
        println!("{reg1a} diff_mask {sta0} is {diff}");
        assert!(diff == SomeMask::new_from_string("m0b010000")?);

        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string("rX10X")?;
        let reg1 = SomeRegion::new_from_string("r0XX1")?;
        let regs = RegionStore::new(reg0.subtract(&reg1));
        println!("{reg0} subtract {reg1} = {regs}");

        assert!(regs.len() == 2);

        assert!(regs.contains(&SomeRegion::new_from_string("rX100")?));
        assert!(regs.contains(&SomeRegion::new_from_string("r110X")?));

        // Test subtract a superset.
        let reg3 = SomeRegion::new_from_string("rXXX1")?;
        let regs = RegionStore::new(reg1.subtract(&reg3));
        println!("{reg0} subtract {reg3} = {regs}");
        assert!(regs.is_empty());

        // Test no intersection.
        let reg3 = SomeRegion::new_from_string("rXX11")?;
        let regs = RegionStore::new(reg0.subtract(&reg3));
        println!("{reg0} subtract {reg3} = {regs}");
        assert!(regs.len() == 1);
        assert!(regs.contains(&reg0));

        Ok(())
    }
} // end tests
