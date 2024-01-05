//! The GroupStore struct, a vector of SomeGroup structs.

use crate::change::SomeChange;
use crate::group::SomeGroup;
use crate::pn::Pn;
use crate::region::{AccessStates, SomeRegion};
use crate::regionstore::RegionStore;
use crate::rule::SomeRule;
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
    pub aggregate_changes: Option<SomeChange>,
    /// Changes possible were recently updated.
    pub agg_chgs_updated: bool,
}

impl GroupStore {
    /// Return a new, empty, GroupStore.
    pub fn new(avec: Vec<SomeGroup>) -> Self {
        Self {
            avec,
            aggregate_changes: None,
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
        self.aggregate_changes = None;

        for grpx in &self.avec {
            if grpx.pn == Pn::Unpredictable {
                continue;
            }
            for rulx in grpx.rules.as_ref().expect("SNH").iter() {
                if let Some(changes) = &self.aggregate_changes {
                    self.aggregate_changes = Some(changes.union(rulx));
                } else {
                    self.aggregate_changes = Some(rulx.change());
                }
            }
        }

        self.agg_chgs_updated = true;
    }

    /// Check groups with a recently changed sqaure.
    /// Return the references to groups that are inactivated by a square.
    pub fn check_square(&mut self, sqrx: &SomeSquare, dom_id: usize, act_id: usize) -> RegionStore {
        let mut regs_invalid = RegionStore::new(vec![]);

        let mut rmvec = Vec::<usize>::new();

        for (inx, grpx) in self.avec.iter_mut().enumerate() {
            if !grpx.check_square(sqrx) {
                if sqrx.pn > grpx.pn {
                    println!(
                        "\nDom {} Act {} square {} pn: {} invalidates\n             group {} pn: {}",
                        dom_id, act_id, sqrx.state, sqrx.pn , grpx.region, grpx.pn
                    );
                } else if sqrx.pn < grpx.pn && sqrx.pnc {
                    println!(
                        "\nDom {} Act {} square {} pn: {} pnc: true invalidates\n             group {} pn: {}",
                        dom_id, act_id, sqrx.state, sqrx.pn , grpx.region, grpx.pn
                    );
                } else {
                    println!(
                        "\nDom {} Act {} square {} {} invalidates\n             group {} {}",
                        dom_id,
                        act_id,
                        sqrx.state,
                        if let Some(rules) = &sqrx.rules {
                            rules.to_string()
                        } else {
                            String::from("None")
                        },
                        grpx.region,
                        if let Some(rules) = &grpx.rules {
                            rules.to_string()
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
                dom_id, act_id, self.avec[*inx].region
            );
            tools::remove_unordered(&mut self.avec, *inx);
        }

        if rmvec.is_empty() {
        } else {
            self.calc_aggregate_changes();
        }

        // Check limited status of groups.
        for grpx in self.avec.iter_mut() {
            if !grpx.limited || !grpx.region.is_adjacent(&sqrx.state) {
                continue;
            }
            if let Some(anchor) = &grpx.anchor {
                if anchor.is_adjacent(&sqrx.state) {
                    if !sqrx.pnc || (grpx.pn == Pn::Unpredictable && sqrx.pn == Pn::Unpredictable) {
                        println!("at 4");
                        grpx.set_limited_off(dom_id, act_id);
                    } else if grpx.pn == sqrx.pn {
                        if let Some(grpx_ruls) = &grpx.rules {
                            if let Some(sqr_ruls) = &sqrx.rules {
                                if grpx_ruls.union(sqr_ruls).is_some() {
                                    println!("at 3");
                                    grpx.set_limited_off(dom_id, act_id);
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

    /// Return true if an item is in exactly one group.
    pub fn in_1_group(&self, itmx: &impl AccessStates) -> bool {
        let mut num_grps = 0;

        for grpx in &self.avec {
            if grpx.is_superset_of(itmx) {
                if num_grps > 0 {
                    return false;
                }
                num_grps += 1;
            }
        }
        num_grps == 1
    }

    /// Return the remainder of a group region after subtracting other group regions.
    pub fn remainder(&self, grp_inx: usize) -> RegionStore {
        let mut ret = RegionStore::new(vec![self.avec[grp_inx].region.clone()]);

        for (inx, grpx) in self.avec.iter().enumerate() {
            if inx == grp_inx {
                continue;
            }
            if ret.any_intersection(&grpx.region) {
                ret = ret.subtract_item(&grpx.region);
            }
        }
        ret
    }

    /// Return the groups regions an item is in.
    pub fn groups_in(&self, itmx: &impl AccessStates) -> Vec<&SomeRegion> {
        self.avec
            .iter()
            .filter_map(|grpx| {
                if grpx.is_superset_of(itmx) {
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
            if grpx.is_superset_of(astate) {
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

    /// Return the number of groups an item is in.
    pub fn num_groups_in(&self, itmx: &impl AccessStates) -> usize {
        let mut count = 0;
        for grpx in &self.avec {
            if grpx.is_superset_of(itmx) {
                count += 1;
            }
        }
        count
    }

    /// Return true if any group is a superset of, or equal to, an item.
    pub fn any_superset_of(&self, itmx: &impl AccessStates) -> bool {
        tools::vec_contains(&self.avec, SomeGroup::is_superset_of, itmx)
    }

    /// Return regions of any group is a superset, or equal, to a region.
    pub fn supersets_of(&self, itmx: &impl AccessStates) -> Vec<&SomeRegion> {
        self.avec
            .iter()
            .filter_map(|grpx| {
                if grpx.is_subset_of(itmx) {
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
    pub fn remove_subsets_of(&mut self, reg: &SomeRegion, dom_id: usize, act_id: usize) -> bool {
        // Accumulate indices of groups that are subsets
        let mut rmvec = Vec::<usize>::new();

        for (inx, grpx) in &mut self.avec.iter().enumerate() {
            if grpx.is_subset_of(reg) {
                rmvec.push(inx);
            }
        }

        // Remove the groups
        for inx in rmvec.iter().rev() {
            println!(
                "\nDom {} Act {} Group {} deleted",
                dom_id, act_id, self.avec[*inx].region
            );
            tools::remove_unordered(&mut self.avec, *inx);
        }

        !rmvec.is_empty()
    }

    /// Add a group to the end of the list.
    /// So older, more likely groups are first in the list.
    /// The push command in LISP puts an item at the beginning of the list.
    pub fn push_nosubs(&mut self, grp: SomeGroup, dom_id: usize, act_id: usize) -> bool {
        // Check for supersets, which probably is an error
        if self.any_superset_of(&grp.region) {
            let regs = self.supersets_of(&grp.region);
            println!(
                "Dom {} Act {} skipped adding group {}, a superset exists in {}",
                dom_id,
                act_id,
                grp.region,
                tools::vec_ref_string(&regs)
            );
            return false;
        }

        // Remove subset groups
        self.remove_subsets_of(&grp.region, dom_id, act_id);

        // push the new group
        if grp.region.states.len() > 1 {
            println!(
                "\nDom {} Act {} Adding group {} from {}",
                dom_id,
                act_id,
                grp,
                tools::vec_string(&grp.region.states),
            );
        } else {
            println!("\nDom {} Act {} Adding group {}", dom_id, act_id, grp);
        }

        self.avec.push(grp);

        self.calc_aggregate_changes();

        true
    }

    /// Check groups with a given sample.
    /// Return true if any groups are invalidated.
    pub fn any_groups_invalidated(&mut self, smpl: &SomeSample) -> bool {
        for grpx in &mut self.avec {
            if !grpx.check_sample(smpl) {
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
    pub fn check_limited(&mut self, max_reg: &SomeRegion) {
        for grpx in &mut self.avec {
            if grpx.limited {
                grpx.check_limited(max_reg);
            }
        }
    }

    /// Return a String representation of a GroupStore.
    fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::new();

        for grpx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n              ");
            }
            rc_str.push_str(&grpx.to_string());
            flg = 1;
        }

        rc_str
    }

    /// Return a vector of rules for the "rx" command.
    pub fn all_rules(&self) -> Vec<&SomeRule> {
        let mut ret = Vec::<&SomeRule>::new();
        for grpx in self.avec.iter() {
            if let Some(rules) = &grpx.rules {
                if rules.len() == 1 {
                    if rules[0].change().is_not_low() {
                        ret.push(&rules[0]);
                    }
                    continue;
                }
                if rules[0].change().is_low() && rules[1].change().is_not_low() {
                    ret.push(&rules[1]);
                }
                if rules[0].change().is_not_low() && rules[1].change().is_low() {
                    ret.push(&rules[0]);
                }
            }
        }
        ret
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
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
