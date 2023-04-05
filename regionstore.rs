//! The RegionStore, a vector of SomeRegion structs.
//!
//! In the case of Optimal Regions, per domain, the regions in a RegionStore
//! may not have the same number of integers, hence the *_each functions.

use crate::region::SomeRegion;
use crate::removeunordered;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
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
    /// Return a new, empty, RegionStore.
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
        for regx in &self.avec {
            if regx.is_superset_of(reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region is a subset, or equal, to a region.
    pub fn any_subset_of(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if regx.is_subset_of(reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region intersects a given region.
    pub fn any_intersection(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if regx.intersects(reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region is a superset of a state.
    pub fn any_superset_of_state(&self, sta: &SomeState) -> bool {
        for regx in &self.avec {
            if regx.is_superset_of_state(sta) {
                return true;
            }
        }
        false
    }

    /// Return the number of supersets of a state.
    pub fn number_supersets_of_state(&self, sta: &SomeState) -> usize {
        self.avec
            .iter()
            .map(|regx| usize::from(regx.is_superset_of_state(sta)))
            .sum()
    }

    /// Return true if a RegionStore contains a region.
    /// Regions may be equal, without matching states.
    /// A region formed by 0 and 5 will equal a region formed by 4 and 1.
    pub fn contains(&self, reg: &SomeRegion) -> bool {
        self.avec.contains(reg)
    }

    /// Return true if a given state is only in one region.
    pub fn state_in_1_region(&self, sta: &SomeState) -> bool {
        self.avec
            .iter()
            .map(|regx| usize::from(regx.is_superset_of_state(sta)))
            .sum::<usize>()
            == 1
    }

    /// Find and remove a given region.
    pub fn remove_region(&mut self, reg: &SomeRegion) -> bool {
        // Find a matching region
        let mut fnd = false;
        let mut inx = 0;

        for regx in &self.avec {
            if regx == reg {
                fnd = true;
                break;
            }
            inx += 1;
        }

        // Remove the region
        if fnd {
            removeunordered::remove_unordered(&mut self.avec, inx);
        }

        fnd
    }

    // Add a region, removing subset (and equal) regions.
    //    pub fn push_no_dup(&mut self, reg: SomeRegion) -> bool {
    //        if self.contains(&reg) {
    //            return false;
    //        }
    //
    //        self.avec.push(reg);
    //
    //        true
    //    }

    /// Add a region, removing subset (and equal) regions.
    pub fn push_nosubs(&mut self, reg: SomeRegion) -> bool {
        // Check for supersets, which probably is an error
        if self.any_superset_of(&reg) {
            //println!("skipped adding region {}, a superset exists in {}", reg, self);
            return false;
        }

        // Identify subsets
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in self.avec.iter().enumerate() {
            if regx.is_subset_of(&reg) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in reverse order
        for inx in rmvec.iter().rev() {
            removeunordered::remove_unordered(&mut self.avec, *inx);
        }

        self.avec.push(reg);

        true
    }

    /// Add a region, removing superset (and equal) regions.
    pub fn push_nosups(&mut self, reg: SomeRegion) -> bool {
        // Check for subsets, which probably is an error
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
            removeunordered::remove_unordered(&mut self.avec, *inx);
        }

        self.avec.push(reg);

        true
    }

    /// Return the expected length of a string representing a RegionStore.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        let alen = self.avec.len();

        if alen > 0 {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string representing a RegionStore.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::with_capacity(self.formatted_string_length());
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

    // Return the result of intersection of two region stores
    //    pub fn intersection(&self, other: &RegionStore) -> Self {
    //
    //        let mut ret_store = Self::new();
    //
    //        for regx in self.iter() {
    //
    //            for regy in other.iter() {
    //
    //                if regx.intersects(&regy) {
    //                    ret_store.push_nosubs(regx.intersection(&regy));
    //                }
    //            }
    //        }
    //        ret_store
    //    }

    /// Subtract a region from a RegionStore
    pub fn subtract_region(&self, regx: &SomeRegion) -> Self {
        let mut ret_str = RegionStore::new(vec![]);

        for regy in &self.avec {
            if regx.intersects(regy) {
                let avec = regy.subtract(regx);
                for regz in &avec {
                    ret_str.push_nosubs(regz.clone());
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Subtract a RegionStore from a RegionStore
    pub fn subtract(&self, other: &RegionStore) -> RegionStore {
        let mut ret_str = self.clone();

        for regx in other.iter() {
            if ret_str.any_intersection(regx) {
                ret_str = ret_str.subtract_region(regx);
            }
        }
        ret_str
    }

    /// Return True if a RegionStore is a superset of all corresponding states in a StateStore.
    /// Used in optimal regionstore calculations.
    pub fn is_superset_corr_states(&self, stas: &[&SomeState]) -> bool {
        assert!(self.len() == stas.len());

        for (inx, regx) in self.avec.iter().enumerate() {
            if regx.is_superset_of_state(stas[inx]) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return the sum of distances between corresponding regions and states.
    /// Used in optimal regionstore calculations.
    pub fn distance_corr_states(&self, stas: &[&SomeState]) -> usize {
        debug_assert!(self.len() == stas.len());

        let mut dist = 0;
        for (inx, regx) in self.avec.iter().enumerate() {
            if regx.is_superset_of_state(stas[inx]) {
            } else {
                dist += regx.distance_state(stas[inx]);
            }
        }

        dist
    }

    /// Return True if a RegionStore is a superset of corresponding regions in a given RegionStore.
    /// Used in optimal regionstore calculations.
    pub fn is_superset_corr(&self, regs: &RegionStore) -> bool {
        assert!(self.len() == regs.len());

        for inx in 0..self.len() {
            if self.avec[inx].is_superset_of(&regs[inx]) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return true if corresponding regions in two RegionStores are equal.
    /// Used in optimal regionstore calculations.
    pub fn equal_corr(&self, other: &RegionStore) -> bool {
        for inx in 0..self.len() {
            if self[inx] == other[inx] {
            } else {
                return false;
            }
        }
        true
    }

    /// Return an intersection of corresponding regions, of two RegionStores.
    /// Used in optimal regionstore calculations.
    pub fn intersect_corr(&self, other: &RegionStore) -> Option<RegionStore> {
        assert!(self.len() == other.len());

        let mut ret = RegionStore::with_capacity(self.len());

        for inx in 0..self.len() {
            if self[inx].intersects(&other[inx]) {
                if let Some(reg_int) = self[inx].intersection(&other[inx]) {
                    ret.push(reg_int);
                }
            } else {
                return None;
            }
        }

        Some(ret)
    }

    /// Return true if each region in a RegionStore is a subset, in order, of two RegionStores.
    /// Used in optimal regionstore calculations.
    pub fn subset_corr(&self, other: &RegionStore) -> bool {
        for inx in 0..self.len() {
            if self[inx].is_subset_of(&other[inx]) {
            } else {
                return false;
            }
        }
        true
    }
}

impl Index<usize> for RegionStore {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.avec[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn remove_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(3);

        let reg1 = SomeRegion::new_from_string(1, "r0x0x")?;
        let reg2 = SomeRegion::new_from_string(1, "r0xx1")?;
        let reg3 = SomeRegion::new_from_string(1, "rx1x1")?;

        regstr.push(reg1.clone());
        regstr.push(reg2.clone());

        assert!(!regstr.remove_region(&reg3));
        assert!(regstr.remove_region(&reg2));
        assert!(regstr.len() == 1);
        assert!(regstr.contains(&reg1));
        Ok(())
    }

    #[test]
    fn subtract_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let regx = SomeRegion::new_from_string(1, "r0101")?;

        let reg_rslt = regstr.subtract_region(&regx);
        println!("results {}", reg_rslt);

        assert!(reg_rslt.len() == 7);
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "r0x11")?));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "r00x1")?));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "rx111")?));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "r11x1")?));
        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx11x")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string(1, "r01x1")?);
        regstr2.push(SomeRegion::new_from_string(1, "r0x01")?);

        let regstr3 = regstr.subtract(&regstr2);
        println!("regstr3: {}", &regstr3);

        assert!(regstr3.len() == 5);
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r0x00")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r0011")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r11x1")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "rx110")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r111x")?));
        Ok(())
    }

    #[test]
    fn push_nosups() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosups(SomeRegion::new_from_string(1, "r0111")?);
        println!("results {}", &regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r0x0x")?));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r1110")?));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r0111")?));
        Ok(())
    }

    #[test]
    fn push_nosubs() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosubs(SomeRegion::new_from_string(1, "rxxx1")?);
        println!("results {}", &regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r0x0x")?));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r1110")?));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "rxxx1")?));
        Ok(())
    }

    #[test]
    fn state_in_1_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.state_in_1_region(&SomeState::new_from_string(1, "s0b0100")?));
        assert!(!regstr.state_in_1_region(&SomeState::new_from_string(1, "s0b0111")?));
        Ok(())
    }

    #[test]
    fn any_superset_of_state() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of_state(&SomeState::new_from_string(1, "s0b0111")?));
        assert!(!regstr.any_superset_of_state(&SomeState::new_from_string(1, "s0b1011")?));
        Ok(())
    }

    #[test]
    fn any_intersection() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_intersection(&SomeRegion::new_from_string(1, "r1xx1")?));
        assert!(!regstr.any_intersection(&SomeRegion::new_from_string(1, "r10x1")?));
        Ok(())
    }

    #[test]
    fn any_subset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_subset_of(&SomeRegion::new_from_string(1, "rx11x")?));
        assert!(!regstr.any_subset_of(&SomeRegion::new_from_string(1, "r1xx1")?));
        Ok(())
    }

    #[test]
    fn any_superset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of(&SomeRegion::new_from_string(1, "r01x1")?));
        assert!(!regstr.any_superset_of(&SomeRegion::new_from_string(1, "r1xx1")?));
        Ok(())
    }
}
