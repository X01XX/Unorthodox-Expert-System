//! The RegionStore, a vector of SomeRegion structs.

use crate::region::SomeRegion;
use crate::removeunordered;
use crate::state::SomeState;
use crate::tools;

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
        tools::vec_contains(&self.avec, reg, SomeRegion::is_superset_of)
    }

    /// Return true if any region is a subset, or equal, to a region.
    pub fn any_subset_of(&self, reg: &SomeRegion) -> bool {
        tools::vec_contains(&self.avec, reg, SomeRegion::is_subset_of)
    }

    /// Return true if any region intersects a given region.
    pub fn any_intersection(&self, reg: &SomeRegion) -> bool {
        tools::vec_contains(&self.avec, reg, SomeRegion::intersects)
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

    /// Return vector of regions that are a superset of a given region.
    pub fn supersets_of(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        self.avec
            .iter()
            .filter(|regx| regx.is_superset_of(reg))
            .collect()
    }

    /// Return vector of regions that intersect a given region.
    pub fn intersects_of(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        self.avec
            .iter()
            .filter(|regx| regx.intersects(reg))
            .collect()
    }

    /// Return vector of regions that are a adjacent to a given region.
    pub fn adjacent_to(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        self.avec
            .iter()
            .filter(|regx| regx.is_adjacent(reg))
            .collect()
    }

    /// Return vector of regions that are closest to a given region.
    pub fn closest_to(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        let mut min_distance = usize::MAX;
        for regx in &self.avec {
            let dist = regx.distance(reg);
            if dist < min_distance {
                min_distance = dist;
            }
        }
        self.avec
            .iter()
            .filter(|regx| regx.distance(reg) == min_distance)
            .collect()
    }

    /// Return vector of regions that are a superset of a given state.
    pub fn supersets_of_state(&self, sta: &SomeState) -> Vec<&SomeRegion> {
        let mut ret_vec = Vec::<&SomeRegion>::new();
        for regx in &self.avec {
            if regx.is_superset_of_state(sta) {
                ret_vec.push(regx);
            }
        }
        ret_vec
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

    /// Add a region, removing subset regions.
    pub fn push_nosubs(&mut self, reg: SomeRegion) -> bool {
        // Check for supersets, which probably is an error
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
    pub fn vec_ref_string(avec: &[&RegionStore]) -> String {
        let mut rc_str = String::new();
        rc_str.push('[');

        for (inx, regx) in avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", regx));
        }

        rc_str.push(']');

        rc_str
    }

    /// Subtract a region from a RegionStore
    pub fn subtract_region(&self, regx: &SomeRegion) -> Self {
        let mut ret_str = RegionStore::new(vec![]);

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

    /// Subtract a RegionStore from a RegionStore
    pub fn subtract(&self, subtrahend: &RegionStore) -> RegionStore {
        let mut ret_str = self.clone();

        for regx in subtrahend.iter() {
            if ret_str.any_intersection(regx) {
                ret_str = ret_str.subtract_region(regx);
            }
        }
        ret_str
    }

    /// Return a simplified set of regions, from a non-empty RegionStore.
    pub fn simplify(&self) -> Self {
        assert!(self.is_not_empty());
        if self.len() == 1 {
            return self.clone();
        }
        self.complement().complement()
    }
    /// Return the complement of a non-empty RegionStore.
    pub fn complement(&self) -> Self {
        assert!(self.is_not_empty());

        let state_low = SomeState::new_low(self[0].num_ints());
        let max_region = RegionStore::new(vec![SomeRegion::new(
            state_low,
            SomeState::new_low(self[0].num_ints()).bitwise_not(),
        )]);

        max_region.subtract(self)
    }
} // End impl RegionStore.

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
    fn test_simplify() -> Result<(), String> {
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "r001x")?);
        regstr1.push(SomeRegion::new_from_string(1, "r0x10")?);
        regstr1.push(SomeRegion::new_from_string(1, "rx111")?);
        regstr1.push(SomeRegion::new_from_string(1, "r11x1")?);
        regstr1.push(SomeRegion::new_from_string(1, "r10x1")?);

        let simp = regstr1.simplify();
        println!("simp {}", simp);
        assert!(simp.len() == 3);

        assert!(simp.contains(&SomeRegion::new_from_string(1, "rxx11")?));
        assert!(simp.contains(&SomeRegion::new_from_string(1, "r1xx1")?));
        assert!(simp.contains(&SomeRegion::new_from_string(1, "r0x1x")?));

        Ok(())
    }

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
