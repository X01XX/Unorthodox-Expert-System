//! The RegionStore, a vector of SomeRegion structs.
//!
//! To avoid a lot of vector copying, setting a region to inactive is kind of
//! like deleting it.  But it remains until it is overwritten by a region with the active
//! indicator set to true.

use crate::region::SomeRegion;
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

#[derive(Serialize, Deserialize, Debug)]
pub struct RegionStore {
    /// A vector of regions.
    pub avec: Vec<SomeRegion>,
}

impl RegionStore {
    /// Return a new, empty, RegionStore.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeRegion>::with_capacity(5),
        }
    }

    /// Return the number of active, and inactive, regions.
    pub fn len(&self) -> usize {
        self.avec.len()
    }
    
    /// Return the number active regions.
    pub fn num_active(&self) -> usize {
        let mut cnt = 0;
        for regx in &self.avec {
            if regx.active {
                cnt += 1;
            }
        }
        cnt
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
            if reg.active && regx.is_superset_of(&reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region is a subset, or equal, to a region.
    pub fn any_subset_of(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if regx.active && regx.is_subset_of(&reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region is a superset of a state.
    pub fn any_superset_of_state(&self, sta: &SomeState) -> bool {
        for regx in &self.avec {
            if regx.active && regx.is_superset_of_state(&sta) {
                return true;
            }
        }
        false
    }

    /// Return true if a RegionStore contains a region.
    /// Regions may be equal, without matching states.
    /// A region formed by 0 and 5 will equal a region formed by 4 and 1.
    pub fn contains(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if reg.active && regx == reg {
                return true;
            }
        }
        false
    }

    /// Return true if a given state is only in one region.
    pub fn state_in_1_region(&self, sta: &SomeState) -> bool {
        let mut cnt = 0;

        for regx in &self.avec {
            if regx.active && regx.is_superset_of_state(&sta) {
                cnt += 1;
            }
        }
        cnt == 1
    }

    /// Find and make inactive any subset regions.
    fn inactivate_subsets_of(&mut self, reg: &SomeRegion) -> bool {
        let mut fnd = false;
        for regx in &mut self.avec {
            if regx.active && regx.is_subset_of(&reg) {
                regx.inactivate();
                //println!("Inactivated region {}", regx);
                fnd = true;
            }
        }
        fnd
    }

    /// Find and make inactive any superset regions.
    fn inactivate_supersets_of(&mut self, reg: &SomeRegion) -> bool {
        let mut fnd = false;
        for regx in &mut self.avec {
            if regx.active && regx.is_superset_of(&reg) {
                regx.inactivate();
                //println!("Inactivated region {}", regx);
                fnd = true;
            }
        }
        fnd
    }

    /// Find and make inactive a given region.
    pub fn inactivate(&mut self, reg: &SomeRegion) -> bool {
        let mut fnd = false;
        for regx in &mut self.avec {
            if regx.active && regx == reg {
                regx.inactivate();
                //println!("Inactivated region {}", regx);
                fnd = true;
            }
        }
        fnd
    }

    /// Find index to first inactive group, or return -1 if none found.
    fn first_inactive_index(&self) -> Option<usize> {
        let mut cnt = 0;
        for regx in &self.avec {
            if regx.active == false {
                return Some(cnt);
            }
            cnt += 1;
        }
        None
    }

    /// Return true if any region is active.
    pub fn any_active(&self) -> bool {
        for regx in &self.avec {
            if regx.active {
                return true;
            }
        }
        false
    }

    /// Add a region, inactivating subset regions.
    pub fn push_nosubs(&mut self, reg: SomeRegion) -> bool {
        // Check for supersets, which probably is an error
        if self.any_superset_of(&reg) {
            // println!("skipped adding region {}, a superset exists", reg.str());
            return false;
        }

        // Mark any subset regions as inactive
        self.inactivate_subsets_of(&reg);

        // Get index to the first inactive region
        if let Some(inx) = self.first_inactive_index() {
            self.avec[inx] = reg;
        } else {
            self.avec.push(reg);
        }

        true
    }

    /// Add a region, inactivating superset regions.
    pub fn push_nosups(&mut self, reg: SomeRegion) -> bool {
        // Check for subsets, which probably is an error
        if self.any_subset_of(&reg) {
            // println!("skipped adding region {}, a superset exists", reg.str());
            return false;
        }

        // Mark any subset regions as inactive
        self.inactivate_supersets_of(&reg);

        // Get index to the first inactive region
        if let Some(inx) = self.first_inactive_index() {
            self.avec[inx] = reg;
        } else {
            self.avec.push(reg);
        }

        true
    }

    /// Return the expected length of a string representing a RegionStore.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        let mut alen = 0;
        for regx in &self.avec {
            if regx.active {
                alen += 1;
            }
        }

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
            if regx.active == false {
                continue;
            }

            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &regx));
            flg = 1;
        }

        rc_str.push(']');

        rc_str
    }
} // end impl RegionStore

impl Index<usize> for RegionStore {
    type Output = SomeRegion;
    fn index<'a>(&'a self, i: usize) -> &'a SomeRegion {
        &self.avec[i]
    }
}
