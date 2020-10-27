// Implement a store for regions

use crate::region::SomeRegion;
use crate::state::SomeState;

use std::fmt;
use std::ops::Index; // IndexMut
use std::slice::{Iter, IterMut};

impl fmt::Display for RegionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::from("[");

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

        write!(f, "{}", rc_str)
    }
}

pub struct RegionStore {
    avec: Vec<SomeRegion>,
}

impl RegionStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeRegion>::with_capacity(5),
        }
    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    pub fn push(&mut self, val: SomeRegion) {
        self.avec.push(val);
    }

    pub fn iter(&self) -> Iter<SomeRegion> {
        self.avec.iter()
    }

    pub fn iter_mut(&mut self) -> IterMut<SomeRegion> {
        self.avec.iter_mut()
    }

    // Return true if any region is a superset, or equal, to a region
    pub fn any_superset_of(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if reg.active && regx.is_superset_of(&reg) {
                return true;
            }
        }
        false
    }

    // Return true if any region is a subset, or equal, to a region
    pub fn any_subset_of(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if reg.active && regx.is_subset_of(&reg) {
                return true;
            }
        }
        false
    }

    // Return true if any region is a superset of a state
    pub fn any_superset_of_state(&self, sta: &SomeState) -> bool {
        for regx in &self.avec {
            if regx.active && regx.is_superset_of_state(&sta) {
                return true;
            }
        }
        false
    }

    pub fn state_in_1_region(&self, sta: &SomeState) -> bool {
        let mut cnt = 0;

        for regx in &self.avec {
            if regx.active && regx.is_superset_of_state(&sta) {
                cnt += 1;
            }
        }

        cnt == 1
    }

    // Find and make inactive any subset regions
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

    // Find and make inactive any superset regions
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

    // Find and make inactive any superset regions
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

    // Find index to first inactive group, or return -1 is none found
    fn first_inactive_index(&self) -> i32 {
        let mut cnt = 0;
        for regx in &self.avec {
            if regx.active == false {
                return cnt;
            }
            cnt += 1;
        }

        -1
    }

    pub fn any_active(&self) -> bool {
        for regx in &self.avec {
            if regx.active {
                return true;
            }
        }

        false
    }

    // Return true if a region is in the RegionStore
    //    pub fn contains(&self, regx: &SomeRegion) -> bool {
    //        for regy in &self.avec {
    //            if regy.active && regy == regx {
    //                return true;
    //            }
    //        }
    //        false
    //    }

    pub fn push_nosubs(&mut self, reg: SomeRegion) -> bool {
        // Check for supersets, which probably is an error
        if self.any_superset_of(&reg) {
            // println!("skipped adding region {}, a superset exists", reg.str());
            return false;
        }

        // Mark any subset regions as inactive
        self.inactivate_subsets_of(&reg);

        // Get index to the first inactive region
        let inx = self.first_inactive_index();

        // If no inactive region found, push the new region
        if inx < 0 {
            // println!("adding region {}", reg.str());
            self.avec.push(reg);
        } else {
            // Replace the inactive region with the new region
            let inx = inx as usize;
            //println!("deleting region {}", self.avec[inx]);
            // println!("adding region {}", reg);
            self.avec[inx] = reg;
        }

        true
    }

    // Push no supersets
    pub fn push_nosups(&mut self, reg: SomeRegion) -> bool {
        // Check for subsets, which probably is an error
        if self.any_subset_of(&reg) {
            // println!("skipped adding region {}, a superset exists", reg.str());
            return false;
        }

        // Mark any subset regions as inactive
        self.inactivate_supersets_of(&reg);

        // Get index to the first inactive region
        let inx = self.first_inactive_index();

        // If no inactive region found, push the new region
        if inx < 0 {
            // println!("adding region {}", reg.str());
            self.avec.push(reg);
        } else {
            // Replace the inactive region with the new region
            let inx = inx as usize;
            //println!("deleting region {}", self.avec[inx]);
            // println!("adding region {}", reg);
            self.avec[inx] = reg;
        }

        true
    }
}

//impl Iterator for RegionStore {
//  type item = &SomeRegion;

//  fn next(&mut self) -> Option<SomeRegion> {

//      let new_next = self.curr + self.next;

//      self.curr = self.next;
//      self.next = new_next;

//      if self.curr {
//          Some(self.cur)
//      } else {
//          None
//      }
//  }
//}

impl Index<usize> for RegionStore {
    type Output = SomeRegion;
    fn index<'a>(&'a self, i: usize) -> &'a SomeRegion {
        &self.avec[i]
    }
}
