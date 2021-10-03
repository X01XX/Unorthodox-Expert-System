//! The GroupStore struct, a vector of SomeGroup structs.

use crate::group::SomeGroup;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::pn::Pn;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

impl fmt::Display for GroupStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::new();

        for grpx in &self.avec {
            if grpx.get_active() {
                if flg == 1 {
                    rc_str.push_str(",\n              ");
                }
                rc_str.push_str(&format!("{}", &grpx.formatted_string()));
                flg = 1;
            }
        }

        write!(f, "{}", rc_str)
    }
}

#[derive(Serialize, Deserialize)]
pub struct GroupStore {
    /// Vector of SomeGroup structs.
    avec: Vec<SomeGroup>,
    /// If a group is added or deleted, the changed flag is set to true.
    changed: bool,
}

impl GroupStore {
    /// Return a new GroupStore.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeGroup>::with_capacity(5),
            changed: false,
        }
    }

    /// Accessor, set the value of the changed field.
    pub fn set_changed(&mut self, aval: bool ) {
        self.changed = aval;
    }
    
    /// Check groups with a recently changed sqaure.
    /// Return the number of active groups inactivated by a square.
    pub fn check_square(&mut self, sqrx: &SomeSquare, dom: usize, act: usize) -> RegionStore {
        let mut regs_invalid = RegionStore::new();

        for grpx in &mut self.avec {
            if grpx.get_active() {
                if grpx.get_region().is_superset_of_state(sqrx.get_state()) {
                    if grpx.square_is_ok(&sqrx) == false {
                        if *grpx.get_pn() == Pn::Two {
                        println!(
                            "\npn2 sqr {} {} invalidates group {} {}",
                            sqrx.get_state(), sqrx.get_rules().formatted_string(), grpx.get_region(), grpx.get_rules().formatted_string()
                        );
                        } else {
                        println!(
                            "\nsqr {} {} invalidates group {} {}",
                            sqrx.get_state(), sqrx.get_rules().formatted_string() , grpx.get_region(), grpx.get_rules().formatted_string()
                        );
                        }
                        regs_invalid.push(grpx.get_region().clone());
                        grpx.inactivate(dom, act);
                    }
                }
            }
        }
        regs_invalid
    }

    /// Return the number of active groups a state is in.
    pub fn num_groups_state_in(&self, stax: &SomeState) -> usize {
        let mut num_grps = 0;

        for grpx in &self.avec {
            if grpx.get_active() {
                if grpx.get_region().is_superset_of_state(stax) {
                    num_grps += 1;
                }
            }
        }
        num_grps
    }

    /// Return true if any group is a superset, or equal, to a region.
    pub fn any_superset_of(&self, reg: &SomeRegion) -> bool {
        for grpx in &self.avec {
            if grpx.get_active() && reg.is_subset_of(&grpx.get_region()) {
                return true;
            }
        }
        false
    }

    /// Return regions of any group is a superset, or equal, to a region.
    pub fn supersets_of(&self, reg: &SomeRegion) -> RegionStore {
        let mut rs = RegionStore::new();

        for grpx in &self.avec {
            if grpx.get_active() && reg.is_subset_of(grpx.get_region()) {
                rs.push(grpx.get_region().clone());
            }
        }
        rs
    }

    /// Find and make inactive any subset groups.
    fn inactivate_subsets_of(&mut self, reg: &SomeRegion, dom: usize, act: usize) -> bool {
        let mut fnd = false;

        for grpx in &mut self.avec {
            if grpx.get_active() && grpx.get_region().is_superset_of(&reg) {
                println!("Active Superset of {} found in {}", &reg, grpx.get_region());
            }
        }

        for grpx in &mut self.avec {
            if grpx.get_active() && grpx.get_region().is_subset_of(&reg) {
                grpx.inactivate(dom, act);
                // println!("Inactivating group {}", grpx.str_terse());
                fnd = true;
            }
        }

        if fnd {
            self.changed = true;
        }

        fnd
    }

    /// Find index to first inactive group, or return -1 is none found.
    fn first_inactive_index(&mut self) -> i32 {
        let mut cnt = 0;
        for grpx in &mut self.avec {
            if grpx.get_active() == false {
                return cnt;
            }
            cnt += 1;
        }

        -1
    }

    /// Add a group.
    pub fn push(&mut self, grp: SomeGroup, dom: usize, act: usize) -> bool {
        // Check for supersets, which probably is an error
        if self.any_superset_of(grp.get_region()) {
            let regs = self.supersets_of(grp.get_region());
            println!(
                "Dom {} Act {} skipped adding group {}, a superset exists in {}",
                &dom, &act, &grp.get_region(), &regs
            );
            return false;
        }

        // Mark any subset groups as inactive
        self.inactivate_subsets_of(grp.get_region(), dom, act);

        // Get index to the first inactive group
        let inx = self.first_inactive_index();

        // If no inactive group found, push the new group
        if inx < 0 {
            println!("\nDom {} Act {} Adding group {}", &dom, &act, grp);
            self.avec.push(grp);
        } else {
            // Replace the inactive group with the new group
            let inx = inx as usize;
            //println!("Deleting group {}", self.avec[inx].str_terse());
            println!("\nDom {} Act {} Adding group {}", &dom, &act, grp);
            self.avec[inx] = grp;
        }

        self.changed = true;

        true
    }

    /// Check groups with a given sample.
    /// Return the number of active groups that are inactivated.
    pub fn check_sample(
        &mut self,
        init: &SomeState,
        rslt: &SomeState,
        dom: usize,
        act: usize,
    ) -> usize {
        let mut num_grps = 0;

        for grpx in &mut self.avec {
            if grpx.get_active() {
                if grpx.get_region().is_superset_of_state(&init) {
                    if !grpx.sample_is_ok(&init, &rslt) {
                        num_grps += 1;
                        grpx.inactivate(dom, act);
                    }
                }
            }
        }
        if num_grps > 0 {
            self.changed = true;
        }
        num_grps
    }

    /// Return a RegionStore of regions of each group.
    pub fn regions(&self) -> RegionStore {
        let mut regs = RegionStore::new();

        for grpx in &self.avec {
            if grpx.get_active() {
                regs.push(grpx.get_region().clone());
            }
        }
        regs
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<SomeGroup> {
        self.avec.iter()
    }

    /// Return the number of active, and inactive, groups.
    pub fn len(&self) -> usize {
        self.avec.len()
    }
    
    /// Return the number of active groups.
    pub fn num_active(&self) -> usize {
        let mut cnt = 0;
        for grpx in &self.avec {
            if grpx.get_active() {
                cnt += 1;
            }
        }
        cnt
    }

    /// Find a group that matches a region, return a mutable reference.
    pub fn find_mut(&mut self, val: &SomeRegion) -> Option<&mut SomeGroup> {
        for grpx in &mut self.avec {
            if grpx.get_active() && grpx.get_region() == val {
                return Some(grpx);
            }
        }
        None
    }

    /// Find a group that matches a region, return a reference.
    pub fn find(&self, val: &SomeRegion) -> Option<&SomeGroup> {
        for grpx in &self.avec {
            if grpx.get_active() && grpx.get_region() == val {
                return Some(grpx);
            }
        }
        None
    }
} // end impl GroupStore

impl Index<usize> for GroupStore {
    type Output = SomeGroup;
    fn index<'a>(&'a self, i: usize) -> &'a SomeGroup {
        &self.avec[i]
    }
}

impl IndexMut<usize> for GroupStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}
