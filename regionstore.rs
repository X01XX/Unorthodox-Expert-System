//! The RegionStore, a vector of SomeRegion structs.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;
use crate::tools;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

impl fmt::Display for RegionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}
#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct RegionStore {
    /// A vector of regions.
    pub avec: Vec<SomeRegion>,
}

impl PartialEq for RegionStore {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for regx in &self.avec {
            if !other.contains(regx) {
                return false;
            }
        }
        true
    }
}
impl Eq for RegionStore {}

impl RegionStore {
    /// Return a new, RegionStore.
    pub fn new(avec: Vec<SomeRegion>) -> Self {
        Self { avec }
    }

    /// Return a new RegionStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeRegion>::with_capacity(num),
        }
    }

    /// Return the number of regions.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Add a region to the vector.
    pub fn push(&mut self, val: SomeRegion) {
        self.avec.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRegion> {
        self.avec.iter()
    }

    /// Return true if any region is a superset, or equal, to a region.
    pub fn any_superset_of(&self, reg: &SomeRegion) -> bool {
        tools::vec_contains(&self.avec, SomeRegion::is_superset_of, reg)
    }

    /// Return true if any region is a subset, or equal, to a region.
    pub fn any_subset_of(&self, reg: &SomeRegion) -> bool {
        tools::vec_contains(&self.avec, SomeRegion::is_subset_of, reg)
    }

    /// Return true if any region intersects a given region.
    pub fn any_intersection(&self, reg: &SomeRegion) -> bool {
        tools::vec_contains(&self.avec, SomeRegion::intersects, reg)
    }

    /// Return true if any region is a superset of a state.
    pub fn any_superset_of_state(&self, sta: &SomeState) -> bool {
        tools::vec_contains(&self.avec, SomeRegion::is_superset_of_state, sta)
    }

    /// Return vector of regions that are a superset of a given region.
    pub fn supersets_of(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        self.avec
            .iter()
            .filter(|regx| regx.is_superset_of(reg))
            .collect()
    }

    /// Return true if a RegionStore contains a region.
    /// Regions may be equal, without matching states.
    /// A region formed by 0 and 5 will equal a region formed by 4 and 1.
    pub fn contains(&self, reg: &SomeRegion) -> bool {
        self.avec.contains(reg)
    }

    /// Implement swap-remove function for RegionStore.
    pub fn swap_remove(&mut self, inx: usize) -> SomeRegion {
        self.avec.swap_remove(inx)
    }

    /// Add a region, removing subset regions.
    pub fn push_nosubs(&mut self, reg: SomeRegion) -> bool {
        // Check for supersets.
        if self.any_superset_of(&reg) {
            //println!("skipped adding region {}, a superset exists in {}", reg, self);
            return false;
        }

        // Identify subsets.
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in self.avec.iter().enumerate() {
            if regx.is_subset_of(&reg) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in descending index order.
        for inx in rmvec.iter().rev() {
            tools::remove_unordered(&mut self.avec, *inx);
        }

        self.avec.push(reg);

        true
    }

    /// Add a region, removing superset (and equal) regions.
    pub fn push_nosups(&mut self, reg: SomeRegion) -> bool {
        // Check for subsets.
        if self.any_subset_of(&reg) {
            // println!("skipped adding region {}, a superset exists", reg.str());
            return false;
        }

        // Identify supersets
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in self.avec.iter().enumerate() {
            if regx.is_superset_of(&reg) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in reverse (highest index) order
        for inx in rmvec.iter().rev() {
            tools::remove_unordered(&mut self.avec, *inx);
        }

        self.avec.push(reg);

        true
    }

    /// Return a string representing a RegionStore.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::with_capacity(self.strlen());
        rc_str.push('[');

        for (inx, regx) in self.avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &regx));
        }

        rc_str.push(']');

        rc_str
    }

    /// Subtract a region from a RegionStore
    pub fn subtract_region(&self, regx: &SomeRegion) -> Self {
        let mut ret_str = Self::new(vec![]);

        for regy in &self.avec {
            if regx.intersects(regy) {
                for regz in regy.subtract(regx) {
                    ret_str.push_nosubs(regz);
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Return a RegionStore of regions that are superset of a state.
    pub fn supersets_of_state(&self, stax: &SomeState) -> Self {
        let mut ret_str = Self::new(vec![]);

        for regy in &self.avec {
            if regy.is_superset_of_state(stax) {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Subtract a state from a RegionStore.
    pub fn subtract_state(&self, stax: &SomeState) -> Self {
        let mut ret_str = Self::new(vec![]);

        for regy in &self.avec {
            if regy.is_superset_of_state(stax) {
                for regz in regy.subtract_state(stax) {
                    ret_str.push_nosubs(regz);
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Subtract a state from a RegionStore, with results being supersets of a second state.
    /// Assumes all regions are supersets of the second state before doing the subtraction.
    pub fn subtract_state_to_supersets_of(&self, substa: &SomeState, supsta: &SomeState) -> Self {
        let mut ret_str = Self::new(vec![]);

        for regy in &self.avec {
            if regy.is_superset_of_state(substa) {
                for regz in regy.subtract_state_to_supersets_of(substa, supsta) {
                    ret_str.push_nosubs(regz);
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Subtract a RegionStore from a RegionStore
    pub fn subtract(&self, subtrahend: &Self) -> Self {
        let mut ret_str = self.clone();

        for regx in subtrahend.iter() {
            if ret_str.any_intersection(regx) {
                ret_str = ret_str.subtract_region(regx);
            }
        }
        ret_str
    }

    // Return the union of two RegionStores.
    pub fn union(&self, other: &Self) -> Self {
        let mut ret = self.clone();
        for regx in other.avec.iter() {
            ret.push_nosubs(regx.clone());
        }
        ret
    }

    // Return the intersection of two RegionStores.
    pub fn intersection(&self, other: &Self) -> Self {
        let mut ret = Self::new(vec![]);
        for regx in self.avec.iter() {
            for regy in other.iter() {
                if regx.intersects(regy) {
                    ret.push_nosubs(regx.intersection(regy).unwrap());
                }
            }
        }
        ret
    }

    /// Split Regions by intersections, producing a result where each region is a subset
    /// of any intersecting original regions. All parts of the original Regions are accounted for in the
    /// result.
    pub fn split_to_subsets(&self) -> Self {
        // Init return vector of RegionStores.
        let mut ret_str = Self::new(vec![]);

        // Init temp vector, for RegionStore fragments.
        let mut tmp_str = Self::new(vec![]);

        // Init vector to note indices of RegionStores that are split.
        // There may be a few duplicates, but that will not affect the use of the vector.
        let mut int_vec = Vec::<usize>::new();

        // Get initial fragments, from any two non-equal intersecting RegionStores.
        for inx in 0..(self.len() - 1) {
            for iny in (inx + 1)..self.len() {
                // Skip two equal regions.  If no other intersections, one will make it into the return vector.
                if self[inx] == self[iny] {
                    continue;
                }

                // If there is an intersection, split the RegionStores into fragments.
                if let Some(int) = self[inx].intersection(&self[iny]) {
                    int_vec.push(inx);
                    int_vec.push(iny);

                    for rsz in self[inx].subtract(&int) {
                        tmp_str.push_nosubs(rsz);
                    }

                    for rsz in self[iny].subtract(&int) {
                        tmp_str.push_nosubs(rsz);
                    }
                    tmp_str.push_nosubs(int);
                }
            }
        }
        // For Regions with no intersections, add to the return RS.
        for (iny, rsy) in self.iter().enumerate() {
            if !int_vec.contains(&iny) {
                ret_str.push_nosubs((*rsy).clone());
            }
        }

        // Look for additional non-subset intersections.
        loop {
            // Init vector for next pass.
            let mut next_pass = Self::new(vec![]);

            // Check remaining fragments for additional intersections.
            // If no intersections are found, add the fragment to the return vector,
            // else add fragments of fragments to the next_pass vector.
            for rsy in tmp_str.avec {
                let mut split = false;

                // Check for intersecting RegionStore from original argument.
                for rsx in self.iter() {
                    if let Some(int) = rsy.intersection(rsx) {
                        // Skip rsy if it is a subset, thats the end we are looking for.
                        if rsy == int {
                            continue;
                        }
                        // Split intersection into fragments.
                        // Add fragments to the next_pass vector.
                        for rsz in rsy.subtract(&int) {
                            next_pass.push_nosubs(rsz);
                        }
                        // Add the intersection to the next_pass vector.
                        next_pass.push_nosubs(int);
                        split = true;
                    }
                } // next rsx
                  // If no intersectiosn, add the fragment to the return vector.
                if !split {
                    ret_str.push_nosubs(rsy);
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

    /// Corresponding functions.
    /// A RegionStore has a region for each domain.

    /// Return True if a RegionStore is a superset of all corresponding states in a SomeState vector.
    /// Used in optimal regionstore calculations.
    pub fn is_superset_states_corr(&self, stas: &[&SomeState]) -> bool {
        debug_assert!(self.len() == stas.len());

        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of_state(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return true if RegionStore is a subset of another RegionStore.
    /// Used in optimal regionstore calculations.
    pub fn is_subset_of_corr(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_subset_of(y) {
            } else {
                return false;
            }
        }
        true
    }

    /// Return True if a RegionStore is a superset of another RSC.
    pub fn is_superset_of_corr(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_superset_of(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return true if there is an intersection of corresponding regions, of two RegionStores.
    pub fn intersects_corr(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if !x.intersects(y) {
                return false;
            }
        }
        true
    }

    /// Calculate the distance between a RegionStore and another.
    pub fn distance_corr(&self, regs: &Self) -> usize {
        debug_assert!(self.len() == regs.len());

        let mut dist = 0;
        for (x, y) in self.iter().zip(regs.iter()) {
            if x.is_superset_of(y) {
            } else {
                dist += x.distance(y);
            }
        }

        dist
    }

    /// Return a vector of difference masks for two RegionStores.
    pub fn diff_masks_corr(&self, other: &Self) -> Vec<SomeMask> {
        debug_assert!(self.len() == other.len());

        let mut ret = Vec::<SomeMask>::with_capacity(self.len());

        for (x, y) in self.iter().zip(other.iter()) {
            ret.push(x.diff_mask(y));
        }

        ret
    }

    /// Return a RegionStore with certain bit positions set to X.
    pub fn set_to_x_corr(&self, other: &Vec<SomeMask>) -> Self {
        debug_assert!(self.len() == other.len());

        let mut ret = Self::new(Vec::<SomeRegion>::with_capacity(self.len()));

        for (x, y) in self.iter().zip(other.iter()) {
            ret.push(x.set_to_x(y));
        }

        ret
    }

    /// Return the intersection, if any, of two RegionStores.
    pub fn intersection_corr(&self, other: &Self) -> Option<Self> {
        debug_assert!(self.len() == other.len());

        let mut ret = Self::with_capacity(self.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x.intersects(y) {
                if let Some(reg_int) = x.intersection(y) {
                    ret.push(reg_int);
                }
            } else {
                return None;
            }
        }

        Some(ret)
    }

    /// Calculate the distance between a RegionStore and the current state.
    pub fn distance_states_corr(&self, stas: &[&SomeState]) -> usize {
        debug_assert!(self.len() == stas.len());

        let mut dist = 0;
        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of_state(y) {
            } else {
                dist += x.distance_state(y);
            }
        }

        dist
    }

    /// Return self - a given RegionStore.
    pub fn subtract_corr(&self, subtrahend: &Self) -> Vec<Self> {
        debug_assert!(self.len() == subtrahend.len());

        let mut ret = Vec::<Self>::new();

        if self.is_subset_of_corr(subtrahend) {
            return ret;
        }

        if !self.intersects_corr(subtrahend) {
            ret.push(self.clone());
            return ret;
        }

        for (inx, (regx, regy)) in self.iter().zip(subtrahend.iter()).enumerate() {
            let xb_msk = regx.x_mask().bitwise_and(&regy.non_x_mask());
            if xb_msk.is_low() {
                continue;
            }
            // At least one X over non-X bit found.

            // Isolate each X over non-X bit.
            let single_bits = xb_msk.split();

            // Generate a new RegionStore for each isolated bit.
            for sbitx in single_bits.iter() {
                // Alter one X bit in self/regx to the opposite of the corresponding non-X bit in subtrahend/regy.
                let regz = if sbitx.bitwise_and(regy.state1()).is_low() {
                    // Other/regy bit is zero, in regy.state1 (and regy.state2, since its non-X).
                    regx.set_to_ones(sbitx)
                } else {
                    regx.set_to_zeros(sbitx)
                };

                // Copy self, except for one region with one bit changed.
                let mut one_result = Self::with_capacity(self.len());
                for (iny, regm) in self.iter().enumerate() {
                    if iny == inx {
                        one_result.push(regz.clone());
                    } else {
                        one_result.push(regm.clone());
                    }
                }
                // Save fragment to return.
                ret.push(one_result);
            }
        }
        ret
    }

    pub fn eq_corr(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (regx, regy) in self.avec.iter().zip(other.iter()) {
            if regx != regy {
                return false;
            }
        }
        true
    }

    /// Return true if at least one corresponding pair in two RegionStore is adjacent,
    /// while other corresponding pairs are adjacent or intersect.
    pub fn is_adjacent_corr(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        let mut num_dif = 0;
        for (regx, regy) in self.iter().zip(other.iter()) {
            let dif = regx.diff_mask(regy).num_one_bits();

            if dif > 1 {
                return false;
            }
            num_dif += dif;
        }
        num_dif > 0
    }

    /// Return the adjacent part of two RegionStores.
    /// Presumably, at least one pair of corresponding regions will be adjacent, calc the adjacent part.
    /// If a pair of corresponding regions intersect, calc the intersection.
    pub fn adjacent_part_corr(&self, other: &Self) -> Self {
        debug_assert!(self.is_adjacent_corr(other));

        let mut ret_select = Self::new(Vec::<SomeRegion>::with_capacity(self.len()));

        for (reg_s, reg_o) in self.iter().zip(other.iter()) {
            if reg_s.is_adjacent(reg_o) {
                ret_select.push(reg_s.adjacent_part(reg_o));
            } else if let Some(reg_int) = reg_s.intersection(reg_o) {
                ret_select.push(reg_int);
            } else {
                panic!("SNH");
            }
        }

        ret_select
    }
} // End impl RegionStore.

impl Index<usize> for RegionStore {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.avec[i]
    }
}

impl IndexMut<usize> for RegionStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}

/// Implement the trait StrLen for SomeBits.
impl StrLen for RegionStore {
    fn strlen(&self) -> usize {
        // Length of two brackets.
        let mut rc_len = 2; // Brackets.

        let alen = self.avec.len();

        if alen > 0 {
            rc_len += alen * self.avec[0].strlen(); // Items length.
            rc_len += (alen - 1) * 2; // Separators length.
        }
        rc_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;

    #[test]
    fn test_strlen() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);
        let mut tmp_regst = RegionStore::new(vec![tmp_reg.clone()]);

        let strrep = format!("{tmp_regst}");
        let len = strrep.len();
        let calc_len = tmp_regst.strlen();
        println!("str {tmp_regst} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        tmp_regst.push(tmp_reg);
        let strrep = format!("{tmp_regst}");
        let len = strrep.len();
        let calc_len = tmp_regst.strlen();
        println!("str {tmp_regst} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn test_is_superset_states_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x00").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x1x").expect("SNH"));

        let sta1 = SomeState::new(SomeBits::new(vec![1]));
        let sta8 = SomeState::new(SomeBits::new(vec![8]));
        let stas = vec![&sta1, &sta8];

        println!("regstr1 {}", regstr1);
        println!("stas    {}", tools::vec_ref_string(&stas));

        assert!(!regstr1.is_superset_states_corr(&stas));

        let sta4 = SomeState::new(SomeBits::new(vec![4]));
        let sta10 = SomeState::new(SomeBits::new(vec![10]));
        let stas2 = vec![&sta4, &sta10];

        println!("regstr1 {}", regstr1);
        println!("stas2   {}", tools::vec_ref_string(&stas2));

        assert!(regstr1.is_superset_states_corr(&stas2));

        Ok(())
    }

    #[test]
    fn test_distance_states_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x00").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x1x").expect("SNH"));

        let sta1 = SomeState::new(SomeBits::new(vec![1]));
        let sta8 = SomeState::new(SomeBits::new(vec![8]));
        let stas = vec![&sta1, &sta8];

        let dist = regstr1.distance_states_corr(&stas);
        println!("Distance = {dist}");

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn test_diff_masks_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x00").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x1x").expect("SNH"));

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(tmp_reg0.new_from_string("r0101").expect("SNH"));
        regstr2.push(tmp_reg0.new_from_string("r1x01").expect("SNH"));

        let diffs = regstr1.diff_masks_corr(&regstr2);

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);
        println!("diff masks {}", tools::vec_string(&diffs));
        assert!(diffs.len() == 2);
        assert!(tools::vec_contains(
            &diffs,
            SomeMask::eq,
            &tmp_sta0.to_mask().new_from_string("m0b0001").expect("SNH")
        ));
        assert!(tools::vec_contains(
            &diffs,
            SomeMask::eq,
            &tmp_sta0.to_mask().new_from_string("m0b0010").expect("SNH")
        ));

        Ok(())
    }

    #[test]
    fn test_distance_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x00").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x1x").expect("SNH"));

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(tmp_reg0.new_from_string("r0101").expect("SNH"));
        regstr2.push(tmp_reg0.new_from_string("r1x01").expect("SNH"));

        let dist = regstr1.distance_corr(&regstr2);

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);
        println!("distance {dist}");

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn test_is_superset_subset_of_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x0x").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x0x").expect("SNH"));

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(tmp_reg0.new_from_string("r0101").expect("SNH"));
        regstr2.push(tmp_reg0.new_from_string("r1x01").expect("SNH"));

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);

        assert!(regstr1.is_superset_of_corr(&regstr2));
        assert!(!regstr2.is_superset_of_corr(&regstr1));

        assert!(!regstr1.is_subset_of_corr(&regstr2));
        assert!(regstr2.is_subset_of_corr(&regstr1));

        Ok(())
    }

    #[test]
    fn test_intersection_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x0x").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x0x").expect("SNH"));

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(tmp_reg0.new_from_string("rx1x1").expect("SNH"));
        regstr2.push(tmp_reg0.new_from_string("r1xx1").expect("SNH"));

        let intreg = regstr1.intersection_corr(&regstr2).expect("SNH");
        println!("int part {}", intreg);

        assert!(intreg.eq_corr(&RegionStore::new(vec![
            tmp_reg0.new_from_string("r0101").expect("SNH"),
            tmp_reg0.new_from_string("r1x01").expect("SNH")
        ])));
        Ok(())
    }

    #[test]
    fn test_adjacent_part_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x0x").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x0x").expect("SNH"));

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(tmp_reg0.new_from_string("rx11x").expect("SNH"));
        regstr2.push(tmp_reg0.new_from_string("r1xx1").expect("SNH"));

        let adjreg = regstr1.adjacent_part_corr(&regstr2);
        println!("adj part {}", adjreg);

        assert!(adjreg.eq_corr(&RegionStore::new(vec![
            tmp_reg0.new_from_string("r01xx").expect("SNH"),
            tmp_reg0.new_from_string("r1x01").expect("SNH")
        ])));
        Ok(())
    }

    #[test]
    fn test_is_adjacent_corr() -> Result<(), String> {
        let tmp_sta1 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg1 = SomeRegion::new(vec![tmp_sta1.clone()]);

        let tmp_sta2 = SomeState::new(SomeBits::new(vec![0, 0]));
        let tmp_reg2 = SomeRegion::new(vec![tmp_sta2.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg1.new_from_string("r0000_x10x")?);
        regstr1.push(tmp_reg2.new_from_string("r0000_000x_0000_000x")?);

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(tmp_reg1.new_from_string("r0000_x11x")?);
        regstr2.push(tmp_reg2.new_from_string("r0000_0000_0000_000x")?);

        assert!(regstr1.is_adjacent_corr(&regstr2));
        assert!(!regstr1.is_adjacent_corr(&regstr1));

        Ok(())
    }

    #[test]
    fn test_subtract_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("rX10x").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("rX1xx").expect("SNH"));

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(tmp_reg0.new_from_string("r01XX").expect("SNH"));
        regstr2.push(tmp_reg0.new_from_string("rXx10").expect("SNH"));

        let subs: Vec<RegionStore> = regstr1.subtract_corr(&regstr2);
        println!("subs: {}", tools::vec_string(&subs));
        assert!(subs.len() == 3);
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                tmp_reg0.new_from_string("r110x").expect("SNH"),
                tmp_reg0.new_from_string("rX1xx").expect("SNH")
            ])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                tmp_reg0.new_from_string("rX10x").expect("SNH"),
                tmp_reg0.new_from_string("rX1x1").expect("SNH")
            ])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                tmp_reg0.new_from_string("rX10x").expect("SNH"),
                tmp_reg0.new_from_string("rx10x").expect("SNH")
            ])
        ));

        let subs: Vec<RegionStore> = regstr2.subtract_corr(&regstr1);
        println!("subs: {}", tools::vec_string(&subs));
        assert!(subs.len() == 2);
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                tmp_reg0.new_from_string("r011x").expect("SNH"),
                tmp_reg0.new_from_string("rXx10").expect("SNH")
            ])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                tmp_reg0.new_from_string("r01xx").expect("SNH"),
                tmp_reg0.new_from_string("rX010").expect("SNH")
            ])
        ));
        Ok(())
    }

    #[test]
    fn test_intersects_corr() -> Result<(), String> {
        let tmp_sta1 = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg1 = SomeRegion::new(vec![tmp_sta1.clone()]);

        let tmp_sta2 = SomeState::new(SomeBits::new(vec![0, 0]));
        let tmp_reg2 = SomeRegion::new(vec![tmp_sta2.clone()]);

        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(tmp_reg1.new_from_string("r0000_x10x")?);
        regstr1.push(tmp_reg2.new_from_string("r0000_000x_0000_000x")?);

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(tmp_reg1.new_from_string("r0000_1x01")?);
        regstr2.push(tmp_reg2.new_from_string("r0000_0000_0000_00x1")?);

        assert!(regstr1.intersects_corr(&regstr2));

        let mut regstr3 = RegionStore::with_capacity(2);
        regstr3.push(tmp_reg1.new_from_string("r0000_x11x")?);
        regstr3.push(tmp_reg2.new_from_string("r0000_0000_0000_000x")?);

        assert!(!regstr1.intersects_corr(&regstr3));

        Ok(())
    }

    #[test]
    fn test_split_to_subsets() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let reg1 = tmp_reg.new_from_string("rx10x")?;
        let rs_str = RegionStore::new(vec![reg1.clone()]);

        println!("Initial1: {}", rs_str);

        let rslt = rs_str.split_to_subsets();

        println!("Result1: {}", rslt);

        assert!(rslt.len() == 1);
        assert!(rslt.contains(&reg1));

        let reg1 = tmp_reg.new_from_string("rx10x")?;
        let rs_str = RegionStore::new(vec![reg1.clone(), reg1.clone()]);

        println!("Initial2: {}", rs_str);

        let rslt = rs_str.split_to_subsets();

        println!("Result2: {}", rslt);
        assert!(rslt.len() == 1);
        assert!(rslt.contains(&reg1));

        let reg1 = tmp_reg.new_from_string("rx10x")?;
        let reg2 = tmp_reg.new_from_string("r010x")?;
        let rs_str = RegionStore::new(vec![reg1.clone(), reg2.clone()]);

        println!("Initial3: {}", rs_str);

        let rslt = rs_str.split_to_subsets();

        println!("Result3: {}", rslt);
        assert!(rslt.len() == 2);
        assert!(rslt.contains(&tmp_reg.new_from_string("r110x")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r010x")?));

        let reg1 = tmp_reg.new_from_string("rx10x")?;
        let reg2 = tmp_reg.new_from_string("r010x")?;
        let rs_str = RegionStore::new(vec![reg2.clone(), reg1.clone()]);

        println!("Initial4: {}", rs_str);

        let rslt = rs_str.split_to_subsets();

        println!("Result4: {}", rslt);
        assert!(rslt.len() == 2);
        assert!(rslt.contains(&tmp_reg.new_from_string("r110x")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r010x")?));

        let reg1 = tmp_reg.new_from_string("rx10x")?;
        let reg2 = tmp_reg.new_from_string("r1xx1")?;
        let rs_str = RegionStore::new(vec![reg1.clone(), reg2.clone()]);

        println!("Initial5: {}", rs_str);

        let rslt = rs_str.split_to_subsets();

        println!("Result5: {}", rslt);
        assert!(rslt.len() == 5);
        assert!(rslt.contains(&tmp_reg.new_from_string("rx100")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r010x")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r1101")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r10x1")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r1101")?));

        let reg1 = tmp_reg.new_from_string("rx10x")?;
        let reg2 = tmp_reg.new_from_string("r01xx")?;
        let reg3 = tmp_reg.new_from_string("rxxx1")?;
        let rs_str = RegionStore::new(vec![reg2.clone(), reg1.clone(), reg3.clone()]);

        println!("Initial6: {}", rs_str);

        let rslt = rs_str.split_to_subsets();

        println!("Result6: {}", rslt);
        assert!(rslt.len() == 8);

        assert!(rslt.contains(&tmp_reg.new_from_string("rx0x1")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r1100")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r1101")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r0110")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r0111")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r0100")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r0101")?));
        assert!(rslt.contains(&tmp_reg.new_from_string("r1x11")?));

        Ok(())
    }

    #[test]
    fn subtract_region() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(tmp_reg.new_from_string("r0x0x")?);
        regstr.push(tmp_reg.new_from_string("r0xx1")?);
        regstr.push(tmp_reg.new_from_string("rx1x1")?);
        regstr.push(tmp_reg.new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let regx = tmp_reg.new_from_string("r0101")?;

        let reg_rslt = regstr.subtract_region(&regx);
        println!("results {}", reg_rslt);

        assert!(reg_rslt.len() == 7);
        assert!(reg_rslt.contains(&tmp_reg.new_from_string("r0x11")?));
        assert!(reg_rslt.contains(&tmp_reg.new_from_string("r00x1")?));
        assert!(reg_rslt.contains(&tmp_reg.new_from_string("rx111")?));
        assert!(reg_rslt.contains(&tmp_reg.new_from_string("r11x1")?));
        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(tmp_reg.new_from_string("r0x0x")?);
        regstr.push(tmp_reg.new_from_string("r0xx1")?);
        regstr.push(tmp_reg.new_from_string("rx1x1")?);
        regstr.push(tmp_reg.new_from_string("rx11x")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(tmp_reg.new_from_string("r01x1")?);
        regstr2.push(tmp_reg.new_from_string("r0x01")?);

        let regstr3 = regstr.subtract(&regstr2);
        println!("regstr3: {}", &regstr3);

        assert!(regstr3.len() == 5);
        assert!(regstr3.contains(&tmp_reg.new_from_string("r0x00")?));
        assert!(regstr3.contains(&tmp_reg.new_from_string("r0011")?));
        assert!(regstr3.contains(&tmp_reg.new_from_string("r11x1")?));
        assert!(regstr3.contains(&tmp_reg.new_from_string("rx110")?));
        assert!(regstr3.contains(&tmp_reg.new_from_string("r111x")?));
        Ok(())
    }

    #[test]
    fn push_nosups() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(tmp_reg.new_from_string("r0x0x")?);
        regstr.push(tmp_reg.new_from_string("r0xx1")?);
        regstr.push(tmp_reg.new_from_string("rx1x1")?);
        regstr.push(tmp_reg.new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosups(tmp_reg.new_from_string("r0111")?);
        println!("results {}", &regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&tmp_reg.new_from_string("r0x0x")?));
        assert!(regstr.contains(&tmp_reg.new_from_string("r1110")?));
        assert!(regstr.contains(&tmp_reg.new_from_string("r0111")?));
        Ok(())
    }

    #[test]
    fn push_nosubs() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(tmp_reg.new_from_string("r0x0x")?);
        regstr.push(tmp_reg.new_from_string("r0xx1")?);
        regstr.push(tmp_reg.new_from_string("rx1x1")?);
        regstr.push(tmp_reg.new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosubs(tmp_reg.new_from_string("rxxx1")?);
        println!("results {}", &regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&tmp_reg.new_from_string("r0x0x")?));
        assert!(regstr.contains(&tmp_reg.new_from_string("r1110")?));
        assert!(regstr.contains(&tmp_reg.new_from_string("rxxx1")?));
        Ok(())
    }

    #[test]
    fn any_superset_of_state() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(tmp_reg.new_from_string("r0x0x")?);
        regstr.push(tmp_reg.new_from_string("r0xx1")?);
        regstr.push(tmp_reg.new_from_string("rx1x1")?);
        regstr.push(tmp_reg.new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of_state(&tmp_sta.new_from_string("s0b0111")?));
        assert!(!regstr.any_superset_of_state(&tmp_sta.new_from_string("s0b1011")?));
        Ok(())
    }

    #[test]
    fn any_intersection() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(tmp_reg.new_from_string("r0x0x")?);
        regstr.push(tmp_reg.new_from_string("r0xx1")?);
        regstr.push(tmp_reg.new_from_string("rx1x1")?);
        regstr.push(tmp_reg.new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_intersection(&tmp_reg.new_from_string("r1xx1")?));
        assert!(!regstr.any_intersection(&tmp_reg.new_from_string("r10x1")?));
        Ok(())
    }

    #[test]
    fn any_subset_of() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(tmp_reg.new_from_string("r0x0x")?);
        regstr.push(tmp_reg.new_from_string("r0xx1")?);
        regstr.push(tmp_reg.new_from_string("rx1x1")?);
        regstr.push(tmp_reg.new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_subset_of(&tmp_reg.new_from_string("rx11x")?));
        assert!(!regstr.any_subset_of(&tmp_reg.new_from_string("r1xx1")?));
        Ok(())
    }

    #[test]
    fn any_superset_of() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(tmp_reg.new_from_string("r0x0x")?);
        regstr.push(tmp_reg.new_from_string("r0xx1")?);
        regstr.push(tmp_reg.new_from_string("rx1x1")?);
        regstr.push(tmp_reg.new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of(&tmp_reg.new_from_string("r01x1")?));
        assert!(!regstr.any_superset_of(&tmp_reg.new_from_string("r1xx1")?));
        Ok(())
    }

    // Test calculation of two dissimilar pairs of states effect on possible regions.
    #[test]
    fn two_dissimilar_pairs() -> Result<(), String> {
        let tmp_state = SomeState::new(SomeBits::new(vec![0]));

        let max_reg = SomeRegion::new(vec![
            tmp_state.new_from_string("s0b1111")?,
            tmp_state.new_from_string("s0b0000")?,
        ]);

        let rslt = RegionStore::new(vec![max_reg.clone()]);

        let state_6 = tmp_state.new_from_string("s0b0110")?;
        let state_a = tmp_state.new_from_string("s0b1010")?;

        let not_state_6 = RegionStore::new(max_reg.subtract_state(&state_6));
        let not_state_a = RegionStore::new(max_reg.subtract_state(&state_a));
        let rslt = rslt.intersection(&not_state_6.union(&not_state_a));

        let state_4 = tmp_state.new_from_string("s0b0100")?;
        let state_d = tmp_state.new_from_string("s0b1101")?;

        let not_state_4 = RegionStore::new(max_reg.subtract_state(&state_4));
        let not_state_d = RegionStore::new(max_reg.subtract_state(&state_d));

        let rslt = rslt.intersection(&not_state_4.union(&not_state_d));

        println!("result regions {}", rslt);

        assert!(rslt.len() == 7);

        // Test all possible 4-bit regions, with at least one X bit position.
        // 3^4 - 2^4 = 65 regions.
        for x in 0..16 {
            for y in 0..16 {
                if y == x {
                    continue;
                }

                let regx = SomeRegion::new(vec![
                    SomeState::new(SomeBits::new(vec![x])),
                    SomeState::new(SomeBits::new(vec![y])),
                ]);

                if (regx.is_superset_of_state(&state_6) && regx.is_superset_of_state(&state_a))
                    || (regx.is_superset_of_state(&state_4) && regx.is_superset_of_state(&state_d))
                {
                    // Check there is no superset region in the result.
                    if rslt.any_superset_of(&regx) {
                        return Err(format!("Superset of {} found in {}", regx, rslt));
                    }
                } else {
                    // Check there is a superset in the result.
                    if !rslt.any_superset_of(&regx) {
                        return Err(format!("Superset of {} NOT found in {}", regx, rslt));
                    }
                }
            } // next y
        } // next x

        Ok(())
    }
}
