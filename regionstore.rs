//! The RegionStore, a vector of SomeRegion structs.

use crate::region::SomeRegion;
use crate::state::SomeState;
use crate::removeunordered::remove_unordered;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for RegionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct RegionStore {
    /// A vector of regions.
    avec: Vec<SomeRegion>,
}

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

    /// Return a vactor iterator.
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

        for regx in &self.avec {
            if regx == reg {
                return true;
            }
        }
        false
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

    /// Add a region, inactivating subset regions.
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

    /// Add a region, inactivating superset regions.
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
    
    /// Return the result of intersectong two region stores
    pub fn intersection(&self, other: &RegionStore) -> Self {

        let mut ret_store = Self::new();
        
        for regx in self.iter() {

            for regy in other.iter() {

                if regx.intersects(&regy) {
                    ret_store.push_nosubs(regx.intersection(&regy));
                }
            }
        }
        ret_store
    }

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

    // Subtract a region from a RegionStore
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
} // end impl RegionStore

impl Index<usize> for RegionStore {
    type Output = SomeRegion;
    fn index<'a>(&'a self, i: usize) -> &'a SomeRegion {
        &self.avec[i]
    }
}
