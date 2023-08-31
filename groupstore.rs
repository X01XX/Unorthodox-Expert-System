//! The GroupStore struct, a vector of SomeGroup structs.

use crate::change::SomeChange;
use crate::group::SomeGroup;
use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::removeunordered;
use crate::sample::SomeSample;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};

impl fmt::Display for GroupStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
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
    pub fn new(avec: Vec<SomeGroup>, aggregate_changes: SomeChange) -> Self {
        Self {
            avec,
            aggregate_changes,
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
        let mut new_chgs = self.aggregate_changes.new_low();

        for grpx in &self.avec {
            if grpx.pn == Pn::Unpredictable {
                continue;
            }
            for rulx in grpx.rules.as_ref().expect("SNH").iter() {
                new_chgs = new_chgs.bitwise_or_rule(rulx);
            }
        }

        self.aggregate_changes = new_chgs;
        self.agg_chgs_updated = true;
    }

    /// Check groups with a recently changed sqaure.
    /// Return the references to groups that are inactivated by a square.
    pub fn check_square(&mut self, sqrx: &SomeSquare, dom: usize, act: usize) -> RegionStore {
        let mut regs_invalid = RegionStore::new(vec![]);

        let mut rmvec = Vec::<usize>::new();

        for (inx, grpx) in self.avec.iter_mut().enumerate() {
            if grpx.region.is_superset_of_state(&sqrx.state) && !grpx.check_subset_square(sqrx) {
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
                        if let Some(rules) = &sqrx.rules {
                            rules.formatted_string()
                        } else {
                            String::from("None")
                        },
                        &grpx.region,
                        if let Some(rules) = &grpx.rules {
                            rules.formatted_string()
                        } else {
                            String::from("None")
                        },
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
            removeunordered::remove_unordered(&mut self.avec, *inx);
        }

        if !rmvec.is_empty() {
            self.calc_aggregate_changes();
        }

        // Check limited status of groups.
        for grpx in self.avec.iter_mut() {
            if !grpx.limited || !grpx.region.is_adjacent_state(&sqrx.state) {
                continue;
            }
            if let Some(anchor) = &grpx.anchor {
                if anchor.is_adjacent(&sqrx.state) {
                    if !sqrx.pnc || (grpx.pn == Pn::Unpredictable && sqrx.pn == Pn::Unpredictable) {
                        grpx.set_limited_off();
                    } else if grpx.pn == sqrx.pn {
                        if let Some(grpx_ruls) = &grpx.rules {
                            if let Some(sqr_ruls) = &sqrx.rules {
                                if grpx_ruls.union(sqr_ruls).is_some() {
                                    grpx.set_limited_off();
                                }
                            }
                        }
                    }
                }
            }
        } // next grpx

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

    /// Return true if a state is in exactly one group.
    pub fn state_in_1_group(&self, stax: &SomeState) -> bool {
        let mut num_grps = 0;

        for grpx in &self.avec {
            if grpx.region.is_superset_of_state(stax) {
                if num_grps > 0 {
                    return false;
                }
                num_grps += 1;
            }
        }
        num_grps == 1
    }

    /// Return the groups regions a state is in.
    pub fn groups_state_in(&self, stax: &SomeState) -> Vec<&SomeRegion> {
        self.avec
            .iter()
            .filter_map(|grpx| {
                if grpx.region.is_superset_of_state(stax) {
                    Some(&grpx.region)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Check if a state is in only one group, and if so, it is equal to the anchor.
    /// Some(true) = State is in only one group, and is equal to the anchor of that group.
    /// Some(false) = State is in only one group.
    /// None = neither case is true.
    pub fn in_one_anchor(&self, astate: &SomeState) -> Option<bool> {
        let mut num = 0;
        let mut index = 0;

        for (inx, grpx) in self.avec.iter().enumerate() {
            if grpx.region.is_superset_of_state(astate) {
                num += 1;
                if num > 1 {
                    return None;
                }
                index = inx;
            }
        }
        if num == 1 {
            if let Some(anchor) = &self.avec[index].anchor {
                if astate == anchor {
                    return Some(true);
                }
            }
            return Some(false);
        }
        None
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

    /// Return the number of groups a state is in.
    pub fn num_state_in(&self, stax: &SomeState) -> usize {
        let mut count = 0;
        for grpx in &self.avec {
            if grpx.region.is_superset_of_state(stax) {
                count += 1;
            }
        }
        count
    }

    /// Return true if any group is a superset, or equal, to a region.
    pub fn any_superset_of_state(&self, stax: &SomeState) -> bool {
        tools::vec_contains(&self.avec, SomeGroup::is_superset_of_state, stax)
    }

    /// Return regions of any group is a superset, or equal, to a region.
    pub fn supersets_of(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        self.avec
            .iter()
            .filter_map(|grpx| {
                if reg.is_subset_of(&grpx.region) {
                    Some(&grpx.region)
                } else {
                    None
                }
            })
            .collect()
    }

    //  Find and remove a given group, identified by region.
    //  pub fn remove_group(&mut self, reg: &SomeRegion) -> bool {
    //
    //      Find a matching group region
    //      let mut fnd = false;
    //      let mut inx = 0;
    //
    //      for grpx in &mut self.avec {
    //
    //          if grpx.region == *reg {
    //              fnd = true;
    //              break;
    //          }
    //          inx += 1;
    //      }
    //
    //      Remove the group
    //      if fnd {
    //          remove_unordered(&mut self.avec, inx);
    //      }
    //
    //      fnd
    //  }

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
            removeunordered::remove_unordered(&mut self.avec, *inx);
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
                &dom,
                &act,
                &grp.region,
                SomeRegion::vec_ref_string(&regs)
            );
            return false;
        }

        // Remove subset groups
        self.remove_subsets_of(&grp.region, dom, act);

        // push the new group
        if grp.region.states.len() > 1 {
            println!(
                "\nDom {} Act {} Adding group {} from {}",
                &dom,
                &act,
                grp,
                SomeState::vec_string(&grp.region.states),
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
    pub fn any_groups_invalidated(&mut self, smpl: &SomeSample) -> bool {
        for grpx in &mut self.avec {
            if grpx.region.is_superset_of_state(&smpl.initial) && !grpx.check_subset_sample(smpl) {
                return true;
            }
        }
        false
    }

    /// Return a RegionStore of regions of each group.
    pub fn regions(&self) -> Vec<&SomeRegion> {
        self.avec.iter().map(|grpx| &grpx.region).collect()
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

    /// Check limited setting in groups due to new bit that can change.
    pub fn check_limited(&mut self, change_mask: &SomeMask) {
        for grpx in &mut self.avec {
            if grpx.limited {
                grpx.check_limited(change_mask);
            }
        }
    }

    /// Return a String representation of a GroupStore.
    pub fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::new();

        for grpx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n              ");
            }
            rc_str.push_str(&grpx.formatted_string());
            flg = 1;
        }

        rc_str
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
