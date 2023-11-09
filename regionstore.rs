//! The RegionStore, a vector of SomeRegion structs.

use crate::region::{AccessStates, SomeRegion};
use crate::state::SomeState;
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

impl fmt::Display for RegionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.avec))
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
        tools::vec_contains(&self.avec, SomeRegion::is_superset_of, sta)
    }

    /// Return vector of regions that are a superset of a given item.
    pub fn supersets_of(&self, itmx: &impl AccessStates) -> Vec<&SomeRegion> {
        self.avec
            .iter()
            .filter(|regx| regx.is_superset_of(itmx))
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

    /// Subtract a group/region/square/state from a RegionStore.
    pub fn subtract_item(&self, itmx: &impl AccessStates) -> Self {
        let mut ret_str = Self::new(vec![]);

        for regy in &self.avec {
            if itmx.intersects(regy) {
                for regz in regy.subtract(itmx) {
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
                ret_str = ret_str.subtract_item(regx);
            }
        }
        ret_str
    }

    /// Subtract a state from a RegionStore, with results being supersets of a second state.
    /// Assumes all regions are supersets of the second state before doing the subtraction.
    pub fn subtract_state_to_supersets_of(&self, substa: &SomeState, supsta: &SomeState) -> Self {
        assert!(self.any_superset_of_state(substa));

        let mut ret_str = Self::new(vec![]);

        for regy in &self.avec {
            if regy.is_superset_of(substa) {
                for regz in regy.subtract_state_to_supersets_of(substa, supsta) {
                    ret_str.push_nosubs(regz);
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

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

    /// Return the intersection of two RegionStores.
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
}

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

/// Implement the trait StrLen for RegionStore.
impl StrLen for RegionStore {
    fn strlen(&self) -> usize {
        let mut rc_len = 2;

        if self.is_not_empty() {
            rc_len += self.avec.len() * self.avec[0].strlen();
            rc_len += (self.avec.len() - 1) * 2;
        }

        rc_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;

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

        let reg_rslt = regstr.subtract_item(&regx);
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

        let not_state_6 = RegionStore::new(max_reg.subtract(&state_6));
        let not_state_a = RegionStore::new(max_reg.subtract(&state_a));
        let rslt = rslt.intersection(&not_state_6.union(&not_state_a));

        let state_4 = tmp_state.new_from_string("s0b0100")?;
        let state_d = tmp_state.new_from_string("s0b1101")?;

        let not_state_4 = RegionStore::new(max_reg.subtract(&state_4));
        let not_state_d = RegionStore::new(max_reg.subtract(&state_d));

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

                if (regx.is_superset_of(&state_6) && regx.is_superset_of(&state_a))
                    || (regx.is_superset_of(&state_4) && regx.is_superset_of(&state_d))
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
