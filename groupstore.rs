//! The GroupStore struct, a vector of SomeGroup structs.

use crate::change::SomeChange;
use crate::group::SomeGroup;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::removeunordered::remove_unordered;
use crate::square::SomeSquare;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};

impl fmt::Display for GroupStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::new();

        for grpx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n              ");
            }
            rc_str.push_str(&grpx.formatted_string());
            flg = 1;
        }

        write!(f, "{rc_str}")
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct GroupStore {
    /// Vector of SomeGroup structs.
    pub avec: Vec<SomeGroup>,
    /// Changes possible for all groups.
    pub aggregate_changes: SomeChange,
    /// Changes possible were recently updated.
    pub agg_chgs_updated: bool,
}

impl GroupStore {
    /// Return a new, empty, GroupStore.
    pub fn new(num_ints: usize) -> Self {
        assert!(num_ints > 0);
        Self {
            avec: Vec::<SomeGroup>::with_capacity(10),
            aggregate_changes: SomeChange::new_low(num_ints),
            agg_chgs_updated: false,
        }
    }

    /// Set the agg_chgs_updated flag to false, after incorporating
    /// it into the parent ActionStore struct.
    pub fn reset_agg_chgs_updated(&mut self) {
        self.agg_chgs_updated = false;
    }

    /// Calculate and set the aggregate changes and updated flag.
    fn calc_aggregate_changes(&mut self) {
        let mut ret_chn = SomeChange::new_low(self.aggregate_changes.num_ints());

        for grpx in &self.avec {
            for rulx in grpx.rules.iter() {
                ret_chn = ret_chn.bitwise_or_rule(rulx);
            }
        }
        self.aggregate_changes = ret_chn;
        self.agg_chgs_updated = true;
        self.check_limited();
    }

    /// Check groups with a recently changed sqaure.
    /// Return the references to groups that are inactivated by a square.
    pub fn check_square(&mut self, sqrx: &SomeSquare, dom: usize, act: usize) -> RegionStore {
        let mut regs_invalid = RegionStore::new();

        let mut rmvec = Vec::<usize>::new();

        for (inx, grpx) in self.avec.iter().enumerate() {
            if grpx.region.is_superset_of_state(&sqrx.state) && !grpx.check_square(sqrx) {
                if sqrx.pn > grpx.pn {
                    println!(
                        "\nDom {} Act {} square {} pn: {} invalidates\n             group {} pn: {}",
                        dom, act, sqrx.state, sqrx.pn , &grpx.region, grpx.pn
                    );
                } else if sqrx.pn < grpx.pn && sqrx.pnc {
                    println!(
                        "\nDom {} Act {} square {} pn: {} pnc: true invalidates\n             group {} pn: {}",
                        dom, act, sqrx.state, sqrx.pn , &grpx.region, grpx.pn
                    );
                } else {
                    println!(
                        "\nDom {} Act {} square {} {} invalidates\n             group {} {}",
                        dom,
                        act,
                        sqrx.state,
                        sqrx.rules.formatted_string(),
                        &grpx.region,
                        grpx.rules.formatted_string()
                    );
                }

                regs_invalid.push(grpx.region.clone());
                rmvec.push(inx);
            }
        } // next grpx

        // Remove the groups
        for inx in rmvec.iter().rev() {
            println!(
                "\nDom {} Act {} Group {} deleted",
                dom, act, self.avec[*inx].region
            );
            remove_unordered(&mut self.avec, *inx);
        }

        if !rmvec.is_empty() {
            self.calc_aggregate_changes();
        }

        //println!("GroupStore::check_square: {} groups removed", regs_invalid.len());
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

    /// Return the groups regions a state is in.
    pub fn groups_state_in(&self, stax: &SomeState) -> RegionStore {
        RegionStore {
            avec: self
                .avec
                .iter()
                .filter_map(|grpx| {
                    if grpx.region.is_superset_of_state(stax) {
                        Some(grpx.region.clone())
                    } else {
                        None
                    }
                })
                .collect(),
        }
    }

    /// Return true if any group is a superset, or equal, to a region.
    pub fn any_superset_of(&self, reg: &SomeRegion) -> bool {
        for grpx in &self.avec {
            if grpx.region.is_superset_of(reg) {
                return true;
            }
        }
        false
    }

    /// Return a list of anchor states.
    pub fn anchor_states(&self) -> Vec<&SomeState> {
        self.avec
            .iter()
            .filter_map(|grpx| grpx.anchor.as_ref())
            .collect()
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
        RegionStore {
            avec: self
                .avec
                .iter()
                .filter_map(|grpx| {
                    if reg.is_subset_of(&grpx.region) {
                        Some(grpx.region.clone())
                    } else {
                        None
                    }
                })
                .collect(),
        }
    }

    //    /// Find and remove a given group, identified by region.
    //    pub fn remove_group(&mut self, reg: &SomeRegion) -> bool {
    //
    //        // Find a matching group region
    //        let mut fnd = false;
    //        let mut inx = 0;
    //
    //        for grpx in &mut self.avec {
    //
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
        // Accumulate indices of groups that are subsets
        let mut rmvec = Vec::<usize>::new();

        for (inx, grpx) in &mut self.avec.iter().enumerate() {
            if grpx.region.is_subset_of(reg) {
                rmvec.push(inx);
            }
        }

        // Remove the groups
        for inx in rmvec.iter().rev() {
            println!(
                "\nDom {} Act {} Group {} deleted",
                dom, act, self.avec[*inx].region
            );
            remove_unordered(&mut self.avec, *inx);
        }

        !rmvec.is_empty()
    }

    /// Add a group to the end of the list.
    /// So older, more likely groups are first in the list.
    /// The push command in LISP puts an item at the beginning of the list.
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
        if grp.region.state1 != grp.region.state2 {
            println!(
                "\nDom {} Act {} Adding group {} from {} and {}",
                &dom, &act, grp, grp.region.state1, grp.region.state2
            );
        } else {
            println!("\nDom {} Act {} Adding group {}", &dom, &act, grp);
        }
        self.avec.push(grp);

        self.calc_aggregate_changes();

        true
    }

    /// Check groups with a given sample.
    /// Return true if any groups are invalidated.
    pub fn any_groups_invalidated(&mut self, init: &SomeState, rslt: &SomeState) -> bool {
        for grpx in &mut self.avec {
            if grpx.region.is_superset_of_state(init) && !grpx.check_sample(init, rslt) {
                return true;
            }
        }
        false
    }

    /// Return a RegionStore of regions of each group.
    pub fn regions(&self) -> RegionStore {
        RegionStore {
            avec: self.avec.iter().map(|grpx| grpx.region.clone()).collect(),
        }
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<SomeGroup> {
        self.avec.iter()
    }

    /// Return an iterator
    pub fn iter_mut(&mut self) -> IterMut<SomeGroup> {
        self.avec.iter_mut()
    }

    /// Return the number of groups.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Find a group that matches a region, return a mutable reference.
    pub fn find_mut(&mut self, val: &SomeRegion) -> Option<&mut SomeGroup> {
        self.avec.iter_mut().find(|grpx| grpx.region == *val)
    }

    /// Find a group that matches a region, return a reference.
    pub fn find(&self, val: &SomeRegion) -> Option<&SomeGroup> {
        self.avec.iter().find(|&grpx| grpx.region == *val)
    }

    /// Set the anchor for a group.
    pub fn set_anchor(&mut self, grp_reg: &SomeRegion, anchor: &SomeState) {
        for grpx in &mut self.avec {
            if grpx.region == *grp_reg {
                grpx.set_anchor(anchor); //, rate);
                return;
            }
        }
        panic!("Group not found");
    }

    /// Check limited setting in groups due to new bit that can change.
    pub fn check_limited(&mut self) {
        for grpx in &mut self.avec {
            if grpx.limited {
                grpx.check_limited(&self.aggregate_changes);
            }
        }
    }
} // end impl GroupStore

impl Index<usize> for GroupStore {
    type Output = SomeGroup;
    fn index(&self, i: usize) -> &SomeGroup {
        &self.avec[i]
    }
}

impl IndexMut<usize> for GroupStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}
