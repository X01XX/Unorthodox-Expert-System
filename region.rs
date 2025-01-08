//! The SomeRegion struct, representing a region on a pseudo Karnaugh Map.
//!
//! Uses one, or more, states to represent a region.
//!

use crate::bits::{vec_same_num_bits, BitsRef, NumBits, SomeBits};
use crate::mask::SomeMask;
use crate::regionstore::RegionStore;
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::tools::{self, AccessStates, StrLen};

use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;
use std::str::FromStr;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
/// SomeRegion struct.
/// Vector for one, or more, states.
/// There will be no duplicates, or anf states that are between any other states.
pub struct SomeRegion {
    pub states: StateStore,
}

/// Implement the fmt::Display trait.
impl fmt::Display for SomeRegion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

/// Implement the PartialEq trait, since two SomeRegion struct instances.
/// They may be the same, even when defined by different states.
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
    /// For a group region, all states will correspond to a square that has been sampled.
    ///
    /// Duplicate states are deleted.
    ///
    /// If more than two states are used, they should be a minimum combination, as generated
    /// in action::check_region_for_group.
    pub fn new(states: Vec<SomeState>) -> Self {
        assert!(!states.is_empty());
        debug_assert!(vec_same_num_bits(&states));

        // Return new region.
        Self {
            states: Self::minimum_states(states),
        }
    }

    /// Combine two region instances, in the order given, into an equal, or larger, instance.
    pub fn combine(&self, other: &SomeRegion) -> Self {
        Self::new(vec![
            self.high_state().combine(&other.high_state()),
            self.low_state().combine(&other.low_state()),
        ])
    }

    /// Return a reference to the first state.
    pub fn first_state(&self) -> &SomeState {
        &self.states[0]
    }

    /// Return the state, farthest from the current first state.
    /// Most regions will be two-state, and so have a first and second state.
    /// Non-two state regions are also handeled.
    pub fn far_state(&self) -> SomeState {
        if self.states.len() == 1 {
            self.states[0].clone()
        } else if self.states.len() == 2 {
            self.states[1].clone()
        } else {
            self.states[0].bitwise_xor(&self.x_mask()).as_state()
        }
    }

    /// Return a String representation of a Region.
    /// The case of an X bit position gives information about the first state, and far state, of the region.
    /// X = (1, 0).
    /// x = (0, 1).
    /// XxXx = (1010, ... 0101).
    fn formatted_str(&self) -> String {
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
    pub fn is_adjacent(&self, other: &impl tools::AccessStates) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        self.diff_edge_mask(other).num_one_bits() == 1
    }

    /// Return true if two regions intersect.
    pub fn intersects(&self, other: &impl tools::AccessStates) -> bool {
        //println!("region::intersects: num b s {} num b o {}", self.num_bits(), other.num_bits());
        debug_assert_eq!(self.num_bits(), other.num_bits());

        self.diff_edge_mask(other).is_low()
    }

    /// Return the intersection a region and a region or state..
    pub fn intersection(&self, other: &impl tools::AccessStates) -> Option<Self> {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if !self.intersects(other) {
            return None;
        }
        if self.one_state() {
            Some(Self::new(vec![self.first_state().clone()]))
        } else if other.one_state() {
            Some(Self::new(vec![other.first_state().clone()]))
        } else {
            let lower_high_state = self
                .high_state()
                .bitwise_and(&other.high_state())
                .as_state();
            let higher_low_state = self.low_state().bitwise_or(&other.low_state()).as_state();
            Some(Self::new(vec![lower_high_state, higher_low_state]))
        }
    }

    /// Return a Mask of zero positions.
    pub fn edge_zeros_mask(&self) -> SomeMask {
        self.high_state().bitwise_not()
    }

    /// Return a Mask of one positions.
    pub fn edge_ones_mask(&self) -> SomeMask {
        self.low_state().as_mask()
    }

    /// Return a mask of edge (non-X) bits.
    pub fn edge_mask(&self) -> SomeMask {
        self.first_state().bitwise_eqv(&self.far_state())
    }

    /// Return mask of x bit positions.
    pub fn x_mask(&self) -> SomeMask {
        if self.states.len() == 1 {
            self.states[0].new_low().as_mask()
        } else if self.states.len() == 2 {
            self.states[0].bitwise_xor(&self.states[1])
        } else {
            self.high_state().bitwise_xor(&self.low_state())
        }
    }

    /// Return mask of one-bit positions.
    pub fn ones_edges(&self) -> SomeMask {
        if self.states.len() == 1 {
            self.states[0].as_mask()
        } else if self.states.len() == 2 {
            self.states[0].bitwise_and(&self.states[1])
        } else {
            self.high_state().bitwise_and(&self.low_state())
        }
    }

    /// Given a state in a region, return the far state in the region.
    pub fn far_from(&self, sta: &SomeState) -> SomeState {
        debug_assert_eq!(self.num_bits(), sta.num_bits());
        assert!(self.is_superset_of(sta));

        sta.bitwise_xor(&self.x_mask()).as_state()
    }

    /// Given a region, and a proper subset region, return the
    /// region within the superset region farthest from the subset region.
    pub fn far_from_reg(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());
        assert!(self.is_superset_of(other));
        assert!(self != other);

        let cng_mask = self.x_mask().bitwise_xor(&other.x_mask());

        SomeRegion::new(vec![
            other.high_state().bitwise_xor(&cng_mask).as_state(),
            other.low_state().bitwise_xor(&cng_mask).as_state(),
        ])
    }

    /// Return true if a region is subset, or equal, on another region.
    pub fn is_subset_of(&self, other: &impl tools::AccessStates) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if self.intersects(other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_subset_ones_of(&x2);
        }
        false
    }

    /// Return true if a region is superset, or equal, on another region.
    pub fn is_superset_of(&self, other: &impl tools::AccessStates) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if self.intersects(other) {
            let x1 = self.x_mask();
            let x2 = other.x_mask();
            return x1.is_superset_ones_of(&x2);
        }
        false
    }

    /// Return the union of a region and a region/state.
    pub fn union(&self, other: &impl tools::AccessStates) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        //println!("union {} and {}", self, other);
        let st_low = self.low_state().bitwise_and(&other.low_state()).as_state();
        let st_high = self.high_state().bitwise_or(&other.high_state()).as_state();
        Self::new(vec![st_high, st_low])
    }

    /// Return the highest state in the region
    pub fn high_state(&self) -> SomeState {
        if self.states.len() == 1 {
            self.states[0].clone()
        } else if self.states.len() == 2 {
            self.states[0].bitwise_or(&self.states[1]).as_state()
        } else {
            let mut most_ones = self.states[0].new_low().as_mask();
            for stax in self.states.iter() {
                most_ones = most_ones.bitwise_or(stax);
            }
            most_ones.as_state()
        }
    }

    /// Return lowest state in the region
    pub fn low_state(&self) -> SomeState {
        if self.states.len() == 1 {
            self.states[0].clone()
        } else if self.states.len() == 2 {
            self.states[0].bitwise_and(&self.states[1]).as_state()
        } else {
            let mut least_ones = self.states[0].new_high().as_mask();
            for stax in self.states.iter() {
                least_ones = least_ones.bitwise_and(stax);
            }
            least_ones.as_state()
        }
    }

    /// Return a region with masked X-bits set to zeros.
    pub fn set_to_zeros(&self, msk: &SomeMask) -> Self {
        debug_assert_eq!(self.num_bits(), msk.num_bits());

        let high_state = self.high_state().bitwise_and_not(msk).as_state();
        let low_state = self.low_state().bitwise_and_not(msk).as_state();

        Self::new(vec![high_state, low_state])
    }

    /// Return a region with masked bit positions set to X.
    pub fn set_to_x(&self, msk: &SomeMask) -> Self {
        debug_assert_eq!(self.num_bits(), msk.num_bits());

        let high_state = self.high_state().bitwise_or(msk).as_state();
        let low_state = self.low_state().bitwise_and(&msk.bitwise_not()).as_state();

        Self::new(vec![high_state, low_state])
    }

    /// Return a region with masked X-bits set to ones.
    pub fn set_to_ones(&self, msk: &SomeMask) -> Self {
        debug_assert_eq!(self.num_bits(), msk.num_bits());

        let high_state = self.high_state().bitwise_or(msk).as_state();
        let low_state = self.low_state().bitwise_or(msk).as_state();

        Self::new(vec![high_state, low_state])
    }

    /// Return the distance from a region to a region/state.
    pub fn distance(&self, other: &impl tools::AccessStates) -> usize {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        self.diff_edge_mask(other).num_one_bits()
    }

    /// Return a mask of different edge bits between a region and a region/state.
    pub fn diff_edge_mask(&self, other: &impl tools::AccessStates) -> SomeMask {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        self.edge_mask()
            .bitwise_and(&other.edge_mask())
            .bitwise_and(&self.first_state().bitwise_xor(other.first_state()))
    }

    /// Given a region, and a second region, return the
    /// first region minus the second.
    pub fn subtract(&self, other: &impl tools::AccessStates) -> RegionStore {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        let mut ret_vec = RegionStore::new(vec![]);

        // If no intersection, return self.
        if !self.intersects(other) {
            return RegionStore::new(vec![self.clone()]);
        }

        // If other is a superset, return empty vector.
        if other.is_superset_of(self) {
            return ret_vec;
        }

        let reg_int = self.intersection(other).unwrap();

        // Split by X over 0 or 1.
        let x_over_not_xs = self.x_mask().bitwise_and(&reg_int.edge_mask()).split();

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
        let x_over_not_xs = self
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

    /// Retun the number of bits used to describe region states.
    pub fn num_bits(&self) -> usize {
        self.states[0].num_bits()
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeState> {
        self.states.iter()
    }

    /// Return the number of squares in a region.
    pub fn number_squares(&self) -> usize {
        2_u32.pow(self.x_mask().num_one_bits() as u32) as usize
    }

    /// Given a non-empty vector of states, return a StateStore of the minimum
    /// states required to define the region they imply.
    pub fn minimum_states(states: Vec<SomeState>) -> StateStore {
        assert!(!states.is_empty());

        // Remove duplicates.
        let mut states2 = StateStore::with_capacity(states.len());
        for stax in states {
            if !states2.contains(&stax) {
                states2.push(stax);
            }
        }
        // Easy result.
        if states2.len() < 3 {
            return states2;
        }

        let xmsk = states2.x_mask();

        let state_refs = states2.vec_refs();

        for any in 2..states2.len() {
            let options = tools::anyxofn(any, &state_refs);

            for opx in options.iter() {
                let mut xmsk2 = xmsk.new_low();
                for stx in opx.iter().skip(1) {
                    xmsk2 = xmsk2.bitwise_or(&stx.bitwise_xor(opx[0]));
                }

                // Return the first match.
                if xmsk2 == xmsk {
                    let mut ret = StateStore::with_capacity(any);
                    for stax in opx.iter() {
                        ret.push((*stax).clone());
                    }
                    return ret;
                }
            }
        }
        // states2 is already at the minimum.
        states2
    }

    /// Return the symmetrical overlapping region for two adjacent regions.
    pub fn symmetrical_overlapping_region(&self, other: &SomeRegion) -> Self {
        debug_assert!(self.is_adjacent(other));

        let msk = self.diff_edge_mask(other);

        self.set_to_x(&msk)
            .intersection(&other.set_to_x(&msk))
            .expect("SNH")
    }

    /// Toggle bit positions in a region.
    /// 1->0, 0->1, X->x, x->X.
    fn bitwise_xor_mask(&self, msk: &SomeMask) -> Self {
        let mut states = Vec::<SomeState>::with_capacity(self.len());
        for stax in self.states.iter() {
            states.push(stax.bitwise_xor(msk).as_state());
        }
        Self::new(states)
    }

    /// Make the minimum changes to a regions' edge positions to cause it to intersect a second region.
    pub fn translate_to_intersect(&self, to_intersect: &Self) -> Self {
        self.bitwise_xor_mask(&self.diff_edge_mask(to_intersect))
    }
} // end impl SomeRegion

/// Implement the trait StrLen for SomeRegion.
impl tools::StrLen for SomeRegion {
    fn strlen(&self) -> usize {
        self.first_state().strlen()
    }
}

/// Implement the trait AccessStates for SomeRegion.
impl tools::AccessStates for SomeRegion {
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
    fn diff_edge_mask(&self, other: &impl tools::AccessStates) -> SomeMask {
        self.diff_edge_mask(other)
    }
    fn intersects(&self, other: &impl tools::AccessStates) -> bool {
        self.intersects(other)
    }
    fn is_subset_of(&self, other: &impl tools::AccessStates) -> bool {
        self.is_subset_of(other)
    }
    fn is_superset_of(&self, other: &impl tools::AccessStates) -> bool {
        self.is_superset_of(other)
    }
    fn num_bits(&self) -> usize {
        self.num_bits()
    }
}

impl Index<usize> for SomeRegion {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.states[i]
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

impl FromStr for SomeRegion {
    type Err = String;
    /// Return SomeRegion from a string.
    /// Each bit must be specified.
    /// A first character of "r" is optional, and supported, for cut-and-paste from output on console.
    /// A state string can be used, like "s1010", making a region with no X bit positions.
    /// An underscore, "_", character can be used as a visual separator, and is ignored.
    /// Spaces are ignored.
    ///
    /// The case of an X bit position gives information about the two states, and their order, that form the region.
    /// X = (1, 0).
    /// x = (0, 1).
    /// XxXx = (1010, 0101), xxxX = (0001, 1110).
    fn from_str(str_in: &str) -> Result<Self, String> {
        let str2 = str_in.trim();

        if str2.is_empty() {
            return Err("SomeRegion::from_str: Empty string?".to_string());
        }

        let mut skip = 0;
        // Check the first character.
        if let Some(char0) = str2.graphemes(true).nth(0) {
            if char0 == "r" {
                skip = 1;
            } else if char0 == "s" {
                // Create a region from a single state.
                // An advantage is that hexadecimal digits can be used if there are no X-bit positions.
                let stax = SomeState::from_str(str2)?;
                return Ok(SomeRegion::new(vec![stax]));
            }
        } else {
            return Err("SomeRegion::from_str: Empty string?".to_string());
        }

        // Translate the region string into two state strings.
        let mut b_high = String::from("s");
        let mut b_low = String::from("s");

        for chr in str2.graphemes(true).skip(skip) {
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
                continue; // A region copied from the console might end with a +
            } else {
                return Err(format!(
                    "SomeRegion::from_str: String {str2}, invalid character {chr}?"
                ));
            }
        } // end for chr

        if b_high.len() == 1 {
            return Err(format!(
                "SomeRegion::from_str: String {str2}, no valid character?"
            ));
        }

        // Translate state strings to state instances.
        let sta_high = SomeState::from_str(&b_high)?;
        let sta_low = SomeState::from_str(&b_low)?;

        // Return region from states.
        Ok(Self::new(vec![sta_high, sta_low]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use std::str::FromStr;

    #[test]
    fn state_far_from() -> Result<(), String> {
        let reg1 = SomeRegion::from_str("X10X01X")?;
        let sta1 = SomeState::from_str("s1101010")?;
        let sta2 = reg1.far_from(&sta1);
        println!("reg {reg1} state far from {sta1} is {sta2}");

        assert!(sta2 == SomeState::from_str("s0100011")?);

        Ok(())
    }

    #[test]
    fn num_edges() -> Result<(), String> {
        let reg1 = SomeRegion::from_str("X10X01X")?;
        let num_e = reg1.num_edges();
        println!("reg {reg1} num edges {num_e}");

        assert!(num_e == 4);

        Ok(())
    }

    #[test]
    fn set_to_x() -> Result<(), String> {
        let reg1 = SomeRegion::from_str("XX0101X")?;
        let msk1 = SomeMask::from_str("b1111000")?;

        let reg2 = reg1.set_to_x(&msk1);
        println!("reg1 {reg1} set to x {msk1} is {reg2}");

        assert!(reg2 == SomeRegion::from_str("XXXX01X")?);

        Ok(())
    }

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_reg = SomeRegion::from_str("r0000_0000")?;
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_reg = SomeRegion::from_str("r0000_0000_0000_0000")?;
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_reg = SomeRegion::from_str("00_0000")?;
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_reg = SomeRegion::from_str("0_0000")?;
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_reg = SomeRegion::from_str("0000")?;
        let strrep = format!("{tmp_reg}");
        let len = strrep.len();
        let calc_len = tmp_reg.strlen();
        println!("str {tmp_reg} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn subtract_state_to_supersets_of() -> Result<(), String> {
        let sta0 = SomeState::from_str("s0000")?;
        let staf = SomeState::from_str("s1111")?;

        let reg1 = SomeRegion::new(vec![sta0, staf]);

        let sta5 = SomeState::from_str("s0101")?;
        let sta6 = SomeState::from_str("s0110")?;
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
        // Single state region

        let sta1 = SomeState::from_str("s0001")?;

        let reg1 = SomeRegion::new(vec![sta1.clone()]);
        println!("reg1 is {}", reg1);
        assert!(reg1.states.len() == 1);
        assert!(reg1.far_state() == sta1);

        // Two state region.
        let sta7 = SomeState::from_str("s0111")?;

        let reg2 = SomeRegion::new(vec![sta1.clone(), sta7.clone()]);
        println!("reg2 is {}", reg2);
        assert!(reg2.states.len() == 2);

        // Three state region.
        let sta2 = SomeState::from_str("s0010")?;
        let reg3 = SomeRegion::new(vec![sta1.clone(), sta7.clone(), sta2.clone()]);
        println!("reg3 is {}", reg3);
        assert!(reg3.states.len() == 3);

        Ok(())
    }

    #[test]
    fn edge_mask() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("r00101XX0")?;
        let edges = reg0.edge_mask();
        println!("Edges of {reg0} are {edges}");
        assert!(
            edges.bitwise_and(&SomeBits::from_str("b1111_1111")?)
                == SomeMask::from_str("m1111_1001")?
        );
        Ok(())
    }

    // Test from, using randomly chosen digits.
    #[test]
    fn from_str() -> Result<(), String> {
        match SomeRegion::from_str("") {
            Ok(regx) => {
                return Err(format!("SomeRegion::from_str: regx {regx}?"));
            }
            Err(error) => {
                if error == "SomeRegion::from_str: Empty string?" {
                    println!("{error}");
                } else {
                    return Err(error);
                }
            }
        }

        match SomeRegion::from_str("__") {
            Ok(regx) => {
                return Err(format!("SomeRegion::from_str: regx {regx}?"));
            }
            Err(error) => {
                if error == "SomeRegion::from_str: String __, no valid character?" {
                    println!("{error}");
                } else {
                    return Err(error);
                }
            }
        }

        match SomeRegion::from_str("r00z1") {
            Ok(regx) => {
                return Err(format!("SomeRegion::from_str: regx {regx}?"));
            }
            Err(error) => {
                if error == "SomeRegion::from_str: String r00z1, invalid character z?" {
                    println!("{error}");
                } else {
                    return Err(error);
                }
            }
        }

        match SomeRegion::from_str("r00X1") {
            Ok(regx) => {
                println!("regx {regx}");
                assert!(regx.num_bits() == 4);
            }
            Err(error) => {
                return Err(error);
            }
        }

        match SomeRegion::from_str("s0001") {
            Ok(regx) => {
                println!("regx {regx}");
                assert!(regx.num_bits() == 4);
            }
            Err(error) => {
                return Err(error);
            }
        }

        match SomeRegion::from_str("r0000_0000_0000_0001") {
            Ok(regx) => {
                println!("regx {regx}");
                assert!(regx.num_bits() == 16);
            }
            Err(error) => {
                return Err(error);
            }
        }

        // Test reflection.
        let reg_str = "r1101";
        assert!(format!("{}", SomeRegion::from_str(&reg_str)?) == reg_str);

        let reg_str = "r01_1101";
        assert!(format!("{}", SomeRegion::from_str(&reg_str)?) == reg_str);

        Ok(())
    }

    #[test]
    fn eq() -> Result<(), String> {
        let reg1 = SomeRegion::from_str("rXxXx")?;
        let reg2 = SomeRegion::from_str("rxxxX")?;
        println!("{reg1} should equal {reg2}");
        assert!(reg1.eq(&reg2));

        Ok(())
    }

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let mut reg0 = SomeRegion::from_str("r101XX1")?;
        let mut reg1 = SomeRegion::from_str("rXX0011")?;
        println!("{reg0} s/b adjacent {reg1}");
        assert!(reg0.is_adjacent(&reg1));

        reg0 = SomeRegion::from_str("rX10X01X")?;
        reg1 = SomeRegion::from_str("rX10X10X")?;
        println!("{reg0} s/b adjacent {reg1}");
        assert!(!reg0.is_adjacent(&reg1));

        let reg0 = SomeRegion::from_str("rX10X10X")?;
        let sta1 = SomeState::from_str("s1001100")?;
        println!("{reg0} s/b adjacent {sta1}");
        assert!(reg0.is_adjacent(&sta1));

        let sta2 = SomeState::from_str("s1001110")?;
        println!("{reg0} s/nb adjacent {sta2}");
        assert!(!reg0.is_adjacent(&sta2));

        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("rX10X10X")?;
        let reg1 = SomeRegion::from_str("r0XX110X")?;
        println!("{reg0} should intersect {reg1}");
        assert!(reg0.intersects(&reg1));

        let reg2 = SomeRegion::from_str("r0XX111X")?;
        println!("{reg0} should not intersect {reg2}");
        assert!(!reg0.intersects(&reg2));

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        // Test normal intersection.
        let reg0 = SomeRegion::from_str("rX10X")?;
        let reg1 = SomeRegion::from_str("r110X")?;

        let reg_int = reg0.intersection(&reg1).ok_or("SNH")?;
        println!("Intersection of {reg0} and {reg1} is {reg_int}");
        assert!(reg_int == SomeRegion::from_str("r110X")?);

        // Test non-intersection "intersection"
        let reg0 = SomeRegion::from_str("rX101")?;
        let reg1 = SomeRegion::from_str("r0X10")?;

        if let Some(reg_int) = reg0.intersection(&reg1) {
            return Err(format!("Intersection of {reg0} and {reg1} is {reg_int}?"));
        }

        Ok(())
    }

    #[test]
    fn edge_zeros_mask() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("r00XX0101")?;
        let m1 = reg0.edge_zeros_mask();
        println!("edge_zeros_mask is {m1}");
        assert!(
            m1.bitwise_and(&SomeBits::from_str("b1111_1111")?) == SomeMask::from_str("m11001010")?
        );
        Ok(())
    }

    #[test]
    fn edge_ones_mask() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("r00XX0101")?;
        let m1 = reg0.edge_ones_mask();
        println!("edge_ones_mask is {m1}");
        assert!(m1 == SomeMask::from_str("m00000101")?);
        Ok(())
    }

    #[test]
    fn x_mask() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("r00XX0101")?;
        let m1 = reg0.x_mask();
        println!("x_mask is {m1}");
        assert!(m1 == SomeMask::from_str("m00110000")?);
        Ok(())
    }

    #[test]
    fn far_state() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("r0000XXX1")?;
        let state0 = SomeState::from_str("s00001011")?;
        let far_state = reg0.far_from(&state0);
        println!("far state is {far_state}");
        assert!(far_state == SomeState::from_str("s00000101")?);
        Ok(())
    }

    #[test]
    fn far_reg() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("rXXX01")?;
        let reg1 = SomeRegion::from_str("r01X01")?;
        let far_reg = reg0.far_from_reg(&reg1);
        println!("far_reg is {far_reg}");
        assert!(far_reg == SomeRegion::from_str("r10X01")?);
        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("rX10X")?;
        let reg1 = SomeRegion::from_str("rX10X")?;
        if !reg0.is_subset_of(&reg1) {
            return Err(format!("{reg0} not subset {reg1}?"));
        }

        let reg2 = SomeRegion::from_str("rXXXX")?;
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
        let reg0 = SomeRegion::from_str("rX10X")?;
        let reg1 = SomeRegion::from_str("rX10X")?;

        if !reg0.is_superset_of(&reg1) {
            return Err(format!("{reg0} not superset {reg1}?"));
        }

        let reg2 = SomeRegion::from_str("rXXXX")?;

        if !reg2.is_superset_of(&reg0) {
            return Err(format!("{reg2} not superset {reg0}?"));
        }

        if reg0.is_superset_of(&reg2) {
            return Err(format!("{reg0} is superset {reg2}?"));
        }

        let reg0 = SomeRegion::from_str("rX10X")?;
        let sta1 = SomeState::from_str("s1100")?;
        println!("{reg0} s/b superset of {sta1}");
        assert!(reg0.is_superset_of(&sta1));

        let sta2 = SomeState::from_str("s0000")?;
        println!("{reg0} s/nb superset of {sta2}");
        assert!(!reg0.is_superset_of(&sta2));

        let sta3 = SomeState::from_str("s0010")?;
        println!("{reg0} s/nb superset of {sta3}");
        assert!(!reg0.is_superset_of(&sta3));

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let reg2a = SomeRegion::from_str("rXX0011")?; // Region >1 state.
        let reg2b = SomeRegion::from_str("rXX0111")?; // Region >1 state.
        let reg1a = SomeRegion::from_str("r000101")?; // Region =1 state.
        let reg1b = SomeRegion::from_str("r000011")?; // Region =1 state.
        let sta0 = SomeState::from_str("s010101")?; // State, =1 state.

        // Region >1 state, Region >1 state.
        let union = reg2a.union(&reg2b);
        println!("{reg2a} union {reg2b} is {union}");
        assert!(union == SomeRegion::from_str("rxx0x11")?);

        // Region >1 state, Region =1 state.
        let union = reg2a.union(&reg1a);
        println!("{reg2a} union {reg1a} is {union}");
        assert!(union == SomeRegion::from_str("rxx0xx1")?);

        // Region >1 state, state.
        let union = reg2a.union(&sta0);
        println!("{reg2a} union {sta0} is {union}");
        assert!(union == SomeRegion::from_str("rxx0xx1")?);

        // Region =1 state, Region =1 state.
        let union = reg1a.union(&reg1b);
        println!("{reg1a} union {reg1b} is {union}");
        assert!(union == SomeRegion::from_str("r000xx1")?);

        // Region =1 state, Region >1 state.
        let union = reg1a.union(&reg2a);
        println!("{reg1a} union {reg2a} is {union}");
        assert!(union == SomeRegion::from_str("rXX0xx1")?);

        // Region =1 state, state.
        let union = reg1a.union(&sta0);
        println!("{reg1a} union {sta0} is {union}");
        assert!(union == SomeRegion::from_str("r0x_0101")?);

        Ok(())
    }

    #[test]
    fn high_state() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("rX0X1")?;
        let hs = reg0.high_state();
        println!("High state of {reg0} is {hs}");
        assert!(hs == SomeState::from_str("s1011")?);

        Ok(())
    }

    #[test]
    fn low_state() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("rX0X1")?;
        let ls = reg0.low_state();
        println!("Low state of {reg0} is {ls}");
        assert!(ls == SomeState::from_str("s0001")?);

        Ok(())
    }

    #[test]
    fn set_to_zeros() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("rX10X10X")?;
        let msk1 = SomeMask::from_str("m0000111")?;
        let reg1 = reg0.set_to_zeros(&msk1);
        println!("{reg0} set_to_zeros {msk1} is {reg1}");
        assert!(reg1 == SomeRegion::from_str("rX10X000")?);

        Ok(())
    }

    #[test]
    fn set_to_ones() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("rX10X10X")?;
        let msk1 = SomeMask::from_str("m0000111")?;
        let reg1 = reg0.set_to_ones(&msk1);
        println!("{reg0} set_to_ones {msk1} is {reg1}");
        assert!(reg1 == SomeRegion::from_str("rX10X111")?);

        Ok(())
    }

    #[test] // Also tests diff_mask.
    fn distance() -> Result<(), String> {
        let reg2a = SomeRegion::from_str("rXX0011")?; // Region >1 state.
        let reg2b = SomeRegion::from_str("rXX0111")?; // Region >1 state.
        let reg1a = SomeRegion::from_str("r000101")?; // Region =1 state.
        let reg1b = SomeRegion::from_str("r000011")?; // Region =1 state.
        let sta0 = SomeState::from_str("s010101")?; // State, =1 state.

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
    fn diff_edge_mask() -> Result<(), String> {
        let reg2a = SomeRegion::from_str("rXX0011")?; // Region >1 state.
        let reg2b = SomeRegion::from_str("rXX0111")?; // Region >1 state.
        let reg1a = SomeRegion::from_str("r000101")?; // Region =1 state.
        let reg1b = SomeRegion::from_str("r000011")?; // Region =1 state.
        let sta0 = SomeState::from_str("s010101")?; // State, =1 state.

        // Region >1 state, Region >1 state.
        let diff = reg2a.diff_edge_mask(&reg2b);
        println!("{reg2a} diff_mask {reg2b} is {diff}");
        assert!(diff == SomeMask::from_str("m000100")?);

        // Region >1 state, Region =1 state.
        let diff = reg2a.diff_edge_mask(&reg1a);
        println!("{reg2a} diff_mask {reg1a} is {diff}");
        assert!(diff == SomeMask::from_str("m000110")?);

        // Region >1 state, state.
        let diff = reg2a.diff_edge_mask(&sta0);
        println!("{reg2a} diff_mask {sta0} is {diff}");
        assert!(diff == SomeMask::from_str("m000110")?);

        // Region =1 state, Region =1 state.
        let diff = reg1a.diff_edge_mask(&reg1b);
        println!("{reg1a} diff_mask {reg1b} is {diff}");
        assert!(diff == SomeMask::from_str("m000110")?);

        // Region =1 state, Region >1 state.
        let diff = reg1a.diff_edge_mask(&reg2a);
        println!("{reg1a} diff_mask {reg2a} is {diff}");
        assert!(diff == SomeMask::from_str("m000110")?);

        // Region =1 state, state.
        let diff = reg1a.diff_edge_mask(&sta0);
        println!("{reg1a} diff_mask {sta0} is {diff}");
        assert!(diff == SomeMask::from_str("m010000")?);

        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let reg0 = SomeRegion::from_str("rX10X")?;
        let reg1 = SomeRegion::from_str("r0XX1")?;
        let regs = reg0.subtract(&reg1);
        println!("{reg0} subtract {reg1} = {regs}");

        assert!(regs.len() == 2);

        assert!(regs.contains(&SomeRegion::from_str("rX100")?));
        assert!(regs.contains(&SomeRegion::from_str("r110X")?));

        // Test subtract a superset.
        let reg3 = SomeRegion::from_str("rXXX1")?;
        let regs = reg1.subtract(&reg3);
        println!("{reg0} subtract {reg3} = {regs}");
        assert!(regs.is_empty());

        // Test no intersection.
        let reg3 = SomeRegion::from_str("rXX11")?;
        let regs = reg0.subtract(&reg3);
        println!("{reg0} subtract {reg3} = {regs}");
        assert!(regs.len() == 1);
        assert!(regs.contains(&reg0));

        Ok(())
    }

    #[test]
    fn combine() -> Result<(), String> {
        let reg2 = SomeRegion::from_str("10")?;
        let reg3 = SomeRegion::from_str("101")?;
        let reg5 = reg2.combine(&reg3);
        println!("{reg2} combine {reg3} = {reg5}");
        assert!(reg5 == SomeRegion::from_str("10101")?);

        Ok(())
    }

    #[test]
    fn minimum_states() -> Result<(), String> {
        // Test just one.
        let stas0 = vec![SomeState::from_str("s1010")?];
        let mins0 = SomeRegion::minimum_states(stas0);
        println!("0 {mins0}");
        assert!(mins0.len() == 1);

        // Test dups.
        let stas1 = vec![SomeState::from_str("s1010")?, SomeState::from_str("s1010")?];
        let mins1 = SomeRegion::minimum_states(stas1);
        println!("1 {mins1}");
        assert!(mins1.len() == 1);

        // test dups.
        let stas2 = vec![
            SomeState::from_str("s1010")?,
            SomeState::from_str("s1000")?,
            SomeState::from_str("s1010")?,
        ];
        let mins2 = SomeRegion::minimum_states(stas2);
        println!("2 {mins2}");
        assert!(mins2.len() == 2);

        // Test skip between state.
        let stas3 = vec![
            SomeState::from_str("s1010")?,
            SomeState::from_str("s1000")?,
            SomeState::from_str("s1111")?,
        ];
        let mins3 = SomeRegion::minimum_states(stas3);
        println!("3 {mins3}");
        assert!(mins3.len() == 2);

        // Test three states, all required.
        let stas4 = vec![
            SomeState::from_str("s1000")?,
            SomeState::from_str("s1101")?,
            SomeState::from_str("s1011")?,
        ];
        let mins4 = SomeRegion::minimum_states(stas4);
        println!("4 {mins4}");
        assert!(mins4.len() == 3);

        // Test skip between state.
        let stas5 = vec![
            SomeState::from_str("s1000")?,
            SomeState::from_str("s0101")?,
            SomeState::from_str("s0001")?,
            SomeState::from_str("s0010")?,
        ];
        let mins5 = SomeRegion::minimum_states(stas5);
        println!("5 {mins5}");
        assert!(mins5.len() == 3);

        Ok(())
    }
} // end tests
