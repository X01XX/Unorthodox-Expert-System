//! The GroupStore struct, a vector of SomeGroup structs.

use crate::group::SomeGroup;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::square::SomeSquare;
use crate::squarestore::SquareStore;
use crate::state::SomeState;
//use crate::pn::Pn;
use crate::removeunordered::remove_unordered;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

impl fmt::Display for GroupStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::new();

        for grpx in &self.avec {

            if flg == 1 {
                rc_str.push_str(",\n              ");
            }
            rc_str.push_str(&format!("{}", &grpx.formatted_string()));
            flg = 1;
        }

        write!(f, "{}", rc_str)
    }
}

#[derive(Serialize, Deserialize)]
pub struct GroupStore {
    /// Vector of SomeGroup structs.
    pub avec: Vec<SomeGroup>,
}

impl GroupStore {
    /// Return a new GroupStore.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeGroup>::with_capacity(10),
        }
    }

    /// Check groups with a recently changed sqaure.
    /// Return the references to groups that are inactivated by a square.
    pub fn check_square(&mut self, sqrx: &SomeSquare, dom: usize, act: usize, squares: &SquareStore) -> RegionStore {
        let mut regs_invalid = RegionStore::new();

        let mut rmvec = Vec::<usize>::new();
        let mut inx = 0;

        for grpx in &mut self.avec {

            if grpx.region.is_superset_of_state(&sqrx.state) {

                if grpx.check_square(&sqrx, squares) == false {
                    if sqrx.results.pn > grpx.pn {
                        println!(
                            "\nDom {} Act {} square {} pn: {} invalidates\n             group {} pn: {}",
                            dom, act, sqrx.state, sqrx.results.pn , &grpx.region, grpx.pn
                        );
                    } else if sqrx.results.pn < grpx.pn && sqrx.results.pnc {
                        println!(
                            "\nDom {} Act {} square {} pn: {} pnc: true invalidates\n             group {} pn: {}",
                            dom, act, sqrx.state, sqrx.results.pn , &grpx.region, grpx.pn
                        );
                    } else {
                        println!(
                            "\nDom {} Act {} square {} {} invalidates\n             group {} {}",
                            dom, act, sqrx.state, sqrx.rules.formatted_string() , &grpx.region, grpx.rules.formatted_string()
                        );
                    }

                    regs_invalid.push(grpx.region.clone());
                    rmvec.push(inx);
                }
            }
            inx += 1;
        } // next grpx

        // Remove the groups
        for inx in rmvec.iter().rev() {
            println!("\nDom {} Act {} Group {} deleted", dom, act, self.avec[*inx].region);
            remove_unordered(&mut self.avec, *inx);
        }

        regs_invalid
    }

    /// Return the number of groups a state is in.
    pub fn num_groups_state_in(&self, stax: &SomeState) -> usize {
        let mut num_grps = 0;

        for grpx in &self.avec {
            if grpx.region.is_superset_of_state(stax) {
                num_grps += 1;
            }
        }
        num_grps
    }

    /// Return true if any group is a superset, or equal, to a region.
    pub fn any_superset_of(&self, reg: &SomeRegion) -> bool {

        for grpx in &self.avec {
            if grpx.region.is_superset_of(&reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any group is a superset, or equal, to a region.
    pub fn any_superset_of_state(&self, stax: &SomeState) -> bool {
        for grpx in &self.avec {
            if grpx.region.is_superset_of_state(stax) {
                return true;
            }
        }
        false
    }

    /// Return regions of any group is a superset, or equal, to a region.
    pub fn supersets_of(&self, reg: &SomeRegion) -> RegionStore {

        let mut rs = RegionStore::new();

        for grpx in &self.avec {
            if reg.is_subset_of(&grpx.region) {
                rs.push(grpx.region.clone());
            }
        }
        rs
    }

//    /// Find and remove a given group, identified by region.
//    pub fn remove_group(&mut self, reg: &SomeRegion) -> bool {
//
//        // Find a matching grop region
//        let mut fnd = false;
//        let mut inx = 0;
//
//        for grpx in &mut self.avec {
//            if grpx.region == *reg {
//                fnd = true;
//                break;
//            }
//            inx += 1;
//        }
//
//        // Remove the group
//        if fnd {
//            remove_unordered(&mut self.avec, inx);
//        }
//
//        fnd
//    }

    /// Find and remove any subset groups.
    fn remove_subsets_of(&mut self, reg: &SomeRegion, dom: usize, act: usize) -> bool {

        // Accumulate indicies of groups that are subsets
        let mut rmvec = Vec::<usize>::new();
        let mut inx = 0;
        for grpx in &mut self.avec {
            if grpx.region.is_subset_of(&reg) {
                rmvec.push(inx);
            }
            inx += 1;
        }

        // Remove the groups
        for inx in rmvec.iter().rev() {
            println!("\nDom {} Act {} Group {} deleted", dom, act, self.avec[*inx].region);
            remove_unordered(&mut self.avec, *inx);
        }

        rmvec.len() > 0
    }

    /// Add a group.
    pub fn push(&mut self, grp: SomeGroup, dom: usize, act: usize) -> bool {

        // Check for supersets, which probably is an error
        if self.any_superset_of(&grp.region) {
            let regs = self.supersets_of(&grp.region);
            println!(
                "Dom {} Act {} skipped adding group {}, a superset exists in {}",
                &dom, &act, &grp.region, &regs
            );
            return false;
        }

        // Remove subset groups
        self.remove_subsets_of(&grp.region, dom, act);

        // push the new group
        println!("\nDom {} Act {} Adding group {}", &dom, &act, grp);
        self.avec.push(grp);

        true
    }

    /// Check groups with a given sample.
    /// Return the number of groups that are deleted.
    pub fn check_sample(
        &mut self,
        init: &SomeState,
        rslt: &SomeState,
        dom: usize,
        act: usize,
    ) -> usize {

        let mut rmvec = Vec::<usize>::new();
        let mut inx = 0;

        for grpx in &mut self.avec {

            if grpx.region.is_superset_of_state(&init) {

                if !grpx.check_sample(&init, &rslt) {
                    rmvec.push(inx);
                }
            }
            inx += 1;
        }

        for inx in rmvec.iter().rev() {
            println!("\nDom {} Act {} Group {} deleted", dom, act, self.avec[*inx].region);
            remove_unordered(&mut self.avec, *inx);
        }

        rmvec.len()
    }

    /// Return a RegionStore of regions of each group.
    pub fn regions(&self) -> RegionStore {
        let mut regs = RegionStore::new();

        for grpx in &self.avec {
            regs.push(grpx.region.clone());
        }
        regs
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<SomeGroup> {
        self.avec.iter()
    }

    /// Return an iterator
//    pub fn iter_mut(&mut self) -> IterMut<SomeGroup> {
//        self.avec.iter_mut()
//    }

    /// Return the number of groups.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Find a group that matches a region, return a mutable reference.
    pub fn find_mut(&mut self, val: &SomeRegion) -> Option<&mut SomeGroup> {

        for grpx in &mut self.avec {
            if grpx.region == *val {
                return Some(grpx);
            }
        }
        None
    }

    /// Find a group that matches a region, return a reference.
    pub fn find(&self, val: &SomeRegion) -> Option<&SomeGroup> {

        for grpx in &self.avec {
            if grpx.region == *val {
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
