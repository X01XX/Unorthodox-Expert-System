//! The RegionStore, a vector of SomeRegion structs.

use crate::region::SomeRegion;
use crate::removeunordered::remove_unordered;
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
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RegionStore {
    /// A vector of regions.
    avec: Vec<SomeRegion>,
}

impl PartialEq for RegionStore {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for regx in self.iter() {
            if other.contains(regx) == false {
                return false;
            }
        }
        true
    }
}
impl Eq for RegionStore {}

impl RegionStore {
    /// Return a new, empty, RegionStore.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeRegion>::with_capacity(5),
        }
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
            if regx.is_superset_of(&reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region is a subset, or equal, to a region.
    pub fn any_subset_of(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if regx.is_subset_of(&reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region intersects a given region.
    pub fn any_intersection(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if regx.intersects(&reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region is a superset of a state.
    pub fn any_superset_of_state(&self, sta: &SomeState) -> bool {
        for regx in &self.avec {
            if regx.is_superset_of_state(&sta) {
                return true;
            }
        }
        false
    }

    /// Return a RegionStore of supersets of a state.
    pub fn supersets_of_state(&self, sta: &SomeState) -> Self {
        let mut ret_store = Self::new();

        for regx in &self.avec {
            if regx.is_superset_of_state(&sta) {
                ret_store.push(regx.clone());
            }
        }
        ret_store
    }

    /// Return a RegionStore of not supersets of a state.
    pub fn not_supersets_of_state(&self, sta: &SomeState) -> Self {
        let mut ret_store = Self::new();

        for regx in &self.avec {
            if regx.is_superset_of_state(&sta) {
            } else {
                ret_store.push(regx.clone());
            }
        }
        ret_store
    }

    /// Return true if a RegionStore contains a region.
    /// Regions may be equal, without matching states.
    /// A region formed by 0 and 5 will equal a region formed by 4 and 1.
    pub fn contains(&self, reg: &SomeRegion) -> bool {
        self.avec.contains(reg)
    }

    /// Return true if a given state is only in one region.
    pub fn state_in_1_region(&self, sta: &SomeState) -> bool {
        let mut cnt = 0;

        for regx in &self.avec {
            if regx.is_superset_of_state(&sta) {
                cnt += 1;
            }
        }
        cnt == 1
    }

    /// Find and remove a given region.
    pub fn remove_region(&mut self, reg: &SomeRegion) -> bool {
        // Find a matching region
        let mut fnd = false;
        let mut inx = 0;

        for regx in &mut self.avec {
            if regx == reg {
                fnd = true;
                break;
            }
            inx += 1;
        }

        // Remove the region
        if fnd {
            remove_unordered(&mut self.avec, inx);
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
        let mut inx = 0;
        for regx in &mut self.avec {
            if regx.is_subset_of(&reg) {
                rmvec.push(inx);
            }
            inx += 1;
        }

        // Remove identified regions, in reverse order
        for inx in rmvec.iter().rev() {
            remove_unordered(&mut self.avec, *inx);
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
        let mut inx = 0;
        for regx in &mut self.avec {
            if regx.is_superset_of(&reg) {
                rmvec.push(inx);
            }
            inx += 1;
        }

        // Remove identified regions, in reverse (highest index) order
        for inx in rmvec.iter().rev() {
            remove_unordered(&mut self.avec, *inx);
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
        let mut flg = 0;
        let mut rc_str = String::with_capacity(self.formatted_string_length());
        rc_str.push('[');

        for regx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &regx));
            flg = 1;
        }

        rc_str.push(']');

        rc_str
    }

    // Return the result of intersectong two region stores
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

    // Return the union of regions in the store
    pub fn union(&self) -> Option<SomeRegion> {
        if self.len() == 0 {
            return None;
        }

        if self.len() == 1 {
            return Some(self[0].clone());
        }

        let mut ret_reg = self[0].clone();
        for regx in self.iter() {
            ret_reg = ret_reg.union(regx);
        }

        Some(ret_reg)
    }

    /// Subtract a region from a RegionStore
    pub fn subtract_region(&self, regx: &SomeRegion) -> Self {
        let mut ret_str = RegionStore::new();

        for regy in self.iter() {
            if regx.intersects(regy) {
                let avec = regy.subtract(&regx);
                for regz in avec.iter() {
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

    /// Return a RegionStore with the same regions as those given, plus intersections, and intersections of intersections.
    pub fn and_intersections(&self) -> Self {
        // Start with a copy of the given RegionStore.
        let mut ret_str = self.clone();

        // Need at least two regions to make intersections.
        if ret_str.len() < 2 {
            return ret_str;
        }

        // You should not delete or insert items in a list while traversing it, but pushes to
        // the end of the list are usually OK.
        let mut try_again = true;
        while try_again {
            try_again = false;

            // Get the next round of intersections, by comparing all possible
            // combinations of two regions.

            let limit = ret_str.len(); // Only consider current items, not additions made below.

            for inx1 in 0..(limit - 1) {
                // Skip the last item, it does not have a next item to compare with.

                // Compare the current inx1 item with all next items.
                for inx2 in (inx1 + 1)..limit {
                    if ret_str[inx1].intersects(&ret_str[inx2]) == false {
                        continue;
                    }

                    let regx = ret_str[inx1].intersection(&ret_str[inx2]);

                    // Skip previously seen intersections
                    if ret_str.contains(&regx) {
                        continue;
                    }

                    ret_str.push(regx);
                    try_again = true; // At least one new intersection has been found.
                } // next inx2
            } // next inx1
        } // end while

        //println!("and_intersections: returning {} for {}", &ret_str, &self);
        ret_str
    }
}

impl Index<usize> for RegionStore {
    type Output = SomeRegion;
    fn index<'a>(&'a self, i: usize) -> &'a SomeRegion {
        &self.avec[i]
    }
}

#[cfg(test)]
mod tests {
    use crate::region::SomeRegion;
    use crate::regionstore::RegionStore;
    use crate::state::SomeState;

    #[test]
    fn test_and_intersections() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let reg_ints = regstr.and_intersections();
        println!("results {}", reg_ints);

        assert!(reg_ints.len() == 7);
        assert!(reg_ints.contains(&SomeRegion::new_from_string(1, "r0101").unwrap()));
        assert!(reg_ints.contains(&SomeRegion::new_from_string(1, "r0X01").unwrap()));
        assert!(reg_ints.contains(&SomeRegion::new_from_string(1, "r01x1").unwrap()));
        Ok(())
    }

    #[test]
    fn test_remove_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(3);

        let reg1 = SomeRegion::new_from_string(1, "r0x0x").unwrap();
        let reg2 = SomeRegion::new_from_string(1, "r0xx1").unwrap();
        let reg3 = SomeRegion::new_from_string(1, "rx1x1").unwrap();

        regstr.push(reg1.clone());
        regstr.push(reg2.clone());

        assert!(regstr.remove_region(&reg3) == false);
        assert!(regstr.remove_region(&reg2));
        assert!(regstr.len() == 1);
        assert!(regstr.contains(&reg1));
        Ok(())
    }

    #[test]
    fn test_subtract_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let regx = SomeRegion::new_from_string(1, "r0101").unwrap();

        let reg_rslt = regstr.subtract_region(&regx);
        println!("results {}", reg_rslt);

        assert!(reg_rslt.len() == 7);
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "r0x11").unwrap()));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "r00x1").unwrap()));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "rx111").unwrap()));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "r11x1").unwrap()));
        Ok(())
    }

    #[test]
    fn test_subtract() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx11x").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string(1, "r01x1").unwrap());
        regstr2.push(SomeRegion::new_from_string(1, "r0x01").unwrap());

        let regstr3 = regstr.subtract(&regstr2);
        println!("regstr3: {}", &regstr3);

        assert!(regstr3.len() == 5);
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r0x00").unwrap()));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r0011").unwrap()));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r11x1").unwrap()));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "rx110").unwrap()));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r111x").unwrap()));
        Ok(())
    }

    #[test]
    fn test_union() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(2);
        assert!(regstr.union() == None);

        regstr.push(SomeRegion::new_from_string(2, "r000111xxx").unwrap());
        regstr.push(SomeRegion::new_from_string(2, "r01x01x01x").unwrap());

        if let Some(regx) = regstr.union() {
            println!("regx {}", &regx);
            assert!(regx == SomeRegion::new_from_string(2, "rxxx1xxxx").unwrap());
        } else {
            return Err(format!("Union returns None?"));
        }
        Ok(())
    }

    #[test]
    fn test_push_nosups() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosups(SomeRegion::new_from_string(1, "r0111").unwrap());
        println!("results {}", &regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r0x0x").unwrap()));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r1110").unwrap()));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r0111").unwrap()));
        Ok(())
    }

    #[test]
    fn test_push_nosubs() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosubs(SomeRegion::new_from_string(1, "rxxx1").unwrap());
        println!("results {}", &regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r0x0x").unwrap()));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r1110").unwrap()));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "rxxx1").unwrap()));
        Ok(())
    }

    #[test]
    fn test_state_in_1_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.state_in_1_region(&SomeState::new_from_string(1, "s0100").unwrap()));
        assert!(
            regstr.state_in_1_region(&SomeState::new_from_string(1, "s0111").unwrap()) == false
        );
        Ok(())
    }

    #[test]
    fn test_not_supersets_of_state() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let regstr2 =
            regstr.not_supersets_of_state(&SomeState::new_from_string(1, "s0111").unwrap());
        println!("not supersets {}", &regstr2);

        assert!(regstr2.len() == 2);
        assert!(regstr2.contains(&SomeRegion::new_from_string(1, "r0x0x").unwrap()));
        assert!(regstr2.contains(&SomeRegion::new_from_string(1, "r1110").unwrap()));
        Ok(())
    }

    #[test]
    fn test_supersets_of_state() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let regstr2 = regstr.supersets_of_state(&SomeState::new_from_string(1, "s0111").unwrap());
        println!("supersets {}", &regstr2);

        assert!(regstr2.len() == 2);
        assert!(regstr2.contains(&SomeRegion::new_from_string(1, "r0xx1").unwrap()));
        assert!(regstr2.contains(&SomeRegion::new_from_string(1, "rx1x1").unwrap()));
        Ok(())
    }

    #[test]
    fn test_any_superset_of_state() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of_state(&SomeState::new_from_string(1, "s0111").unwrap()));
        assert!(
            regstr.any_superset_of_state(&SomeState::new_from_string(1, "s1011").unwrap()) == false
        );
        Ok(())
    }

    #[test]
    fn test_any_intersection() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_intersection(&SomeRegion::new_from_string(1, "r1xx1").unwrap()));
        assert!(
            regstr.any_intersection(&SomeRegion::new_from_string(1, "r10x1").unwrap()) == false
        );
        Ok(())
    }

    #[test]
    fn test_any_subset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_subset_of(&SomeRegion::new_from_string(1, "rx11x").unwrap()));
        assert!(regstr.any_subset_of(&SomeRegion::new_from_string(1, "r1xx1").unwrap()) == false);
        Ok(())
    }

    #[test]
    fn test_any_superset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of(&SomeRegion::new_from_string(1, "r01x1").unwrap()));
        assert!(regstr.any_superset_of(&SomeRegion::new_from_string(1, "r1xx1").unwrap()) == false);
        Ok(())
    }
}
