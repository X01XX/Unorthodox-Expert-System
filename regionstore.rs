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

    /// Extend a NeedStore by emptying another NeedStore..
    pub fn append(&mut self, mut other: Self) {
        self.avec.append(&mut other.avec);
    }

    /// Return the intersection of two RegionStores.
    /// Regions overlapping adjacent regions, in the result, are not found.
    /// If that is wanted, it would be max-reg.subtract(&max-reg.subtract(ret))
    pub fn intersection(&self, other: &Self) -> Self {
        let mut ret = Self::new(vec![]);
        for regx in self.avec.iter() {
            for regy in other.iter() {
                if let Some(regz) = regx.intersection(regy) {
                    ret.push_nosubs(regz);
                }
            }
        }
        ret
    }

    /// Return self fragmented by intersections.
    pub fn split_by_intersections(&self) -> Self {
        if self.len() < 2 {
            return self.clone();
        }

        // Get first level ints.
        let mut tmp_ints = Self::new(vec![]);

        // Check each possible pair.
        for inx in 0..(self.len() - 1) {
            for iny in (inx + 1)..self.len() {
                if let Some(regz) = self[inx].intersection(&self[iny]) {
                    tmp_ints.push_nosups(regz);
                }
            }
        }

        if tmp_ints.is_empty() {
            return self.clone();
        }
        let mut remainder = self.subtract(&tmp_ints);

        loop {
            // Get first level ints.
            let mut next_ints = Self::new(vec![]);

            // Check each possible pair.
            for inx in 0..(tmp_ints.len() - 1) {
                for iny in (inx + 1)..tmp_ints.len() {
                    if let Some(regz) = tmp_ints[inx].intersection(&tmp_ints[iny]) {
                        next_ints.push_nosups(regz);
                    }
                } // next iny
            } // next inx

            if next_ints.is_empty() {
                remainder.append(tmp_ints);
                return remainder;
            }

            let remain2 = tmp_ints.subtract(&next_ints);
            remainder.append(remain2);
            tmp_ints = next_ints;
        } // end loop
    }
} // end impl RegionStore.

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
    fn split_by_intersections() -> Result<(), String> {
        // Try with empty RegionStore.
        let regstr1 = RegionStore::new(vec![]);

        let frags = regstr1.split_by_intersections();
        println!("fragments of {regstr1} is {frags}");
        assert!(frags == regstr1);

        // Try with no intersections
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("rx1x0")?);
        regstr1.push(SomeRegion::new_from_string("rx1x1")?);
        regstr1.push(SomeRegion::new_from_string("rx0x1")?);

        let frags = regstr1.split_by_intersections();
        println!("fragments of {regstr1} is {frags}");
        assert!(frags == regstr1);

        // Try one-level intersections.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("rx100")?);
        regstr1.push(SomeRegion::new_from_string("r1x0x")?);
        regstr1.push(SomeRegion::new_from_string("r0x1x")?);
        regstr1.push(SomeRegion::new_from_string("rx11x")?);

        let frags = regstr1.split_by_intersections();
        println!("fragments of {regstr1} is {frags}");
        assert!(frags.len() == 7);
        assert!(frags.contains(&SomeRegion::new_from_string("r0100")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r1x01")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r100x")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r001x")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r111x")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r1100")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r011x")?));

        // Try two-level intersections.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("rx1xx")?);
        regstr1.push(SomeRegion::new_from_string("rxx01")?);
        regstr1.push(SomeRegion::new_from_string("r1xx1")?);

        let frags = regstr1.split_by_intersections();
        println!("fragments of {regstr1} is {frags}");
        assert!(frags.len() == 8);
        assert!(frags.contains(&SomeRegion::new_from_string("rx1x0")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r011x")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r0001")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r1011")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r0101")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r1111")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r1001")?));
        assert!(frags.contains(&SomeRegion::new_from_string("r1101")?));

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("rx10x")?);

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("r01x1")?);
        regstr2.push(SomeRegion::new_from_string("r11x1")?);

        // Without additional processing, in RegionStore::intersection, the result would be [0101, 1101].
        let regstr3 = regstr1.intersection(&regstr2);
        println!("results {}", regstr3);
        assert!(regstr3.len() == 2);
        assert!(regstr3.contains(&SomeRegion::new_from_string("r0101")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string("r1101")?));

        Ok(())
    }

    #[test]
    fn subtract_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::new_from_string("r0x0x")?);
        regstr.push(SomeRegion::new_from_string("r0xx1")?);
        regstr.push(SomeRegion::new_from_string("rx1x1")?);
        regstr.push(SomeRegion::new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let regx = SomeRegion::new_from_string("r0101")?;

        let reg_rslt = regstr.subtract_item(&regx);
        println!("results {}", reg_rslt);

        assert!(reg_rslt.len() == 7);
        assert!(reg_rslt.contains(&SomeRegion::new_from_string("r0x11")?));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string("r00x1")?));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string("rx111")?));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string("r11x1")?));
        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::new_from_string("r0x0x")?);
        regstr.push(SomeRegion::new_from_string("r0xx1")?);
        regstr.push(SomeRegion::new_from_string("rx1x1")?);
        regstr.push(SomeRegion::new_from_string("rx11x")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("r01x1")?);
        regstr2.push(SomeRegion::new_from_string("r0x01")?);

        let regstr3 = regstr.subtract(&regstr2);
        println!("regstr3: {}", regstr3);

        assert!(regstr3.len() == 5);
        assert!(regstr3.contains(&SomeRegion::new_from_string("r0x00")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string("r0011")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string("r11x1")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string("rx110")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string("r111x")?));
        Ok(())
    }

    #[test]
    fn push_nosups() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string("r0x0x")?);
        regstr.push(SomeRegion::new_from_string("r0xx1")?);
        regstr.push(SomeRegion::new_from_string("rx1x1")?);
        regstr.push(SomeRegion::new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosups(SomeRegion::new_from_string("r0111")?);
        println!("results {}", regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::new_from_string("r0x0x")?));
        assert!(regstr.contains(&SomeRegion::new_from_string("r1110")?));
        assert!(regstr.contains(&SomeRegion::new_from_string("r0111")?));
        Ok(())
    }

    #[test]
    fn push_nosubs() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string("r0x0x")?);
        regstr.push(SomeRegion::new_from_string("r0xx1")?);
        regstr.push(SomeRegion::new_from_string("rx1x1")?);
        regstr.push(SomeRegion::new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosubs(SomeRegion::new_from_string("rxxx1")?);
        println!("results {}", regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::new_from_string("r0x0x")?));
        assert!(regstr.contains(&SomeRegion::new_from_string("r1110")?));
        assert!(regstr.contains(&SomeRegion::new_from_string("rxxx1")?));
        Ok(())
    }

    #[test]
    fn any_superset_of_state() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string("r0x0x")?);
        regstr.push(SomeRegion::new_from_string("r0xx1")?);
        regstr.push(SomeRegion::new_from_string("rx1x1")?);
        regstr.push(SomeRegion::new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of_state(&SomeState::new_from_string("s0b0111")?));
        assert!(!regstr.any_superset_of_state(&SomeState::new_from_string("s0b1011")?));
        Ok(())
    }

    #[test]
    fn any_intersection() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string("r0x0x")?);
        regstr.push(SomeRegion::new_from_string("r0xx1")?);
        regstr.push(SomeRegion::new_from_string("rx1x1")?);
        regstr.push(SomeRegion::new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_intersection(&SomeRegion::new_from_string("r1xx1")?));
        assert!(!regstr.any_intersection(&SomeRegion::new_from_string("r10x1")?));
        Ok(())
    }

    #[test]
    fn any_subset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string("r0x0x")?);
        regstr.push(SomeRegion::new_from_string("r0xx1")?);
        regstr.push(SomeRegion::new_from_string("rx1x1")?);
        regstr.push(SomeRegion::new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_subset_of(&SomeRegion::new_from_string("rx11x")?));
        assert!(!regstr.any_subset_of(&SomeRegion::new_from_string("r1xx1")?));
        Ok(())
    }

    #[test]
    fn any_superset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string("r0x0x")?);
        regstr.push(SomeRegion::new_from_string("r0xx1")?);
        regstr.push(SomeRegion::new_from_string("rx1x1")?);
        regstr.push(SomeRegion::new_from_string("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of(&SomeRegion::new_from_string("r01x1")?));
        assert!(!regstr.any_superset_of(&SomeRegion::new_from_string("r1xx1")?));
        Ok(())
    }

    // Test calculation of two dissimilar pairs of states effect on possible regions.
    #[test]
    fn two_dissimilar_pairs() -> Result<(), String> {
        let max_reg = SomeRegion::new(vec![
            SomeState::new_from_string("s0b1111")?,
            SomeState::new_from_string("s0b0000")?,
        ]);

        let rslt = RegionStore::new(vec![max_reg.clone()]);

        let state_6 = SomeState::new_from_string("s0b0110")?;
        let state_a = SomeState::new_from_string("s0b1010")?;

        let not_state_6 = RegionStore::new(max_reg.subtract(&state_6));
        let not_state_a = RegionStore::new(max_reg.subtract(&state_a));
        let rslt = rslt.intersection(&not_state_6.union(&not_state_a));

        let state_4 = SomeState::new_from_string("s0b0100")?;
        let state_d = SomeState::new_from_string("s0b1101")?;

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
                    SomeState::new(SomeBits::new_from_string(&format!("0x{:x}", x))?),
                    SomeState::new(SomeBits::new_from_string(&format!("0x{:x}", y))?),
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
