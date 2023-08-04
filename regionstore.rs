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
            removeunordered::remove_unordered(&mut self.avec, *inx);
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

    /// Return a RegionStore of regions that are superset of a state.
    pub fn supersets_of_state(&self, stax: &SomeState) -> Self {
        let mut ret_str = RegionStore::new(vec![]);

        for regy in &self.avec {
            if regy.is_superset_of_state(stax) {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Subtract a state from a RegionStore.
    pub fn subtract_state(&self, stax: &SomeState) -> Self {
        let mut ret_str = RegionStore::new(vec![]);

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
        let mut ret_str = RegionStore::new(vec![]);

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
    pub fn subtract(&self, subtrahend: &RegionStore) -> RegionStore {
        let mut ret_str = self.clone();

        for regx in subtrahend.iter() {
            if ret_str.any_intersection(regx) {
                ret_str = ret_str.subtract_region(regx);
            }
        }
        ret_str
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
    use crate::bits::SomeBits;

    #[test]
    fn remove_region() -> Result<(), String> {
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(1))]);

        let mut regstr = RegionStore::with_capacity(3);

        let reg1 = tmp_reg.new_from_string("r0x0x")?;
        let reg2 = tmp_reg.new_from_string("r0xx1")?;
        let reg3 = tmp_reg.new_from_string("rx1x1")?;

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
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(1))]);

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
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(1))]);

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
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(1))]);

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
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(1))]);

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
        let tmp_sta = SomeState::new(SomeBits::new(1));
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
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(1))]);

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
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(1))]);

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
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(1))]);

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
}
