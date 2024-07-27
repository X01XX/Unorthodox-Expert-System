//! The GroupStore struct, a vector of SomeGroup structs.

use crate::bits::vec_same_num_bits;
use crate::change::SomeChange;
use crate::group::SomeGroup;
use crate::pn::Pn;
use crate::region::{AccessStates, SomeRegion};
use crate::regionstore::RegionStore;
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
/// A vector of SomeGroup structs, SomeGroup-specific functions,
/// and aggregate changes allowed by the structs that are predictable.
pub struct GroupStore {
    /// Vector of SomeGroup structs.
    pub items: Vec<SomeGroup>,
    /// Changes possible for all groups.
    pub aggregate_changes: Option<SomeChange>,
    /// An indicator that the changes possible were recently updated.
    pub agg_chgs_updated: bool,
}

impl GroupStore {
    /// Return a new, empty, GroupStore.
    pub fn new(items: Vec<SomeGroup>) -> Self {
        debug_assert!(vec_same_num_bits(&items));

        Self {
            items,
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

        for grpx in &self.items {
            if grpx.pn == Pn::Unpredictable {
                continue;
            }
            for rulx in grpx.rules.as_ref().expect("SNH").iter() {
                if let Some(changes) = &self.aggregate_changes {
                    self.aggregate_changes = Some(changes.union(rulx));
                } else {
                    self.aggregate_changes = Some(rulx.to_change());
                }
            }
        }

        self.agg_chgs_updated = true;
    }

    /// Check groups with a recently changed sqaure.
    /// Return the references to groups that are inactivated by a square.
    pub fn check_square(&mut self, sqrx: &SomeSquare, dom_id: usize, act_id: usize) -> RegionStore {
        //println!("GroupStore:check_square: {}", sqrx.state);
        let mut regs_invalid = RegionStore::new(vec![]);

        let mut rmvec = Vec::<usize>::new();

        for (inx, grpx) in self.items.iter_mut().enumerate() {
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
                dom_id, act_id, self.items[*inx].region
            );
            tools::remove_unordered(&mut self.items, *inx);
        }

        if rmvec.is_empty() {
        } else {
            self.calc_aggregate_changes();
        }

        // Check limited status of groups.
        for grpx in self.items.iter_mut() {
            if !grpx.limited || !grpx.region.is_adjacent(&sqrx.state) {
                continue;
            }
            if let Some(anchor) = &grpx.anchor {
                if anchor.is_adjacent(&sqrx.state) {
                    if !sqrx.pnc || (grpx.pn == Pn::Unpredictable && sqrx.pn == Pn::Unpredictable) {
                        //println!("at 4");
                        grpx.set_limited_off(dom_id, act_id);
                    } else if grpx.pn == sqrx.pn {
                        if let Some(grpx_ruls) = &grpx.rules {
                            if let Some(sqr_ruls) = &sqrx.rules {
                                if grpx_ruls.union(sqr_ruls).is_some() {
                                    //println!("at 3");
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

        for grpx in &self.items {
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
        let mut ret = RegionStore::new(vec![self.items[grp_inx].region.clone()]);

        for (inx, grpx) in self.items.iter().enumerate() {
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
        self.items
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

        for (inx, grpx) in self.items.iter().enumerate() {
            if grpx.is_superset_of(astate) {
                num += 1;
                if num > 1 {
                    return None;
                }
                index = inx;
            }
        }
        if num == 1 {
            if let Some(anchor) = &self.items[index].anchor {
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
        for grpx in &self.items {
            if grpx.is_superset_of(itmx) {
                count += 1;
            }
        }
        count
    }

    /// Return true if any group is a superset of, or equal to, an item.
    pub fn any_superset_of(&self, itmx: &impl AccessStates) -> bool {
        tools::vec_contains(&self.items, SomeGroup::is_superset_of, itmx)
    }

    /// Return regions of any group is a superset, or equal, to a region.
    pub fn supersets_of(&self, itmx: &impl AccessStates) -> Vec<&SomeRegion> {
        self.items
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
    //      for grpx in &mut self.items {
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
    //          remove_unordered(&mut self.items, inx);
    //      }
    //
    //      fnd
    //  }

    /// Find and remove any subset groups.
    pub fn remove_subsets_of(&mut self, reg: &SomeRegion, dom_id: usize, act_id: usize) -> bool {
        // Accumulate indices of groups that are subsets
        let mut rmvec = Vec::<usize>::new();

        for (inx, grpx) in &mut self.items.iter().enumerate() {
            if grpx.is_subset_of(reg) {
                rmvec.push(inx);
            }
        }

        // Remove the groups
        for inx in rmvec.iter().rev() {
            println!(
                "\nDom {} Act {} Group {} deleted, subset of {reg}",
                dom_id, act_id, self.items[*inx].region
            );
            tools::remove_unordered(&mut self.items, *inx);
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
                dom_id, act_id, grp, grp.region.states,
            );
        } else {
            println!("\nDom {} Act {} Adding group {}", dom_id, act_id, grp);
        }

        self.items.push(grp);

        self.calc_aggregate_changes();

        true
    }

    /// Check groups with a given sample.
    /// Return true if any groups are invalidated.
    pub fn any_groups_invalidated(&mut self, smpl: &SomeSample) -> bool {
        for grpx in &mut self.items {
            if !grpx.check_sample(smpl) {
                return true;
            }
        }
        false
    }

    /// Return a RegionStore of regions of each group.
    pub fn regions(&self) -> Vec<&SomeRegion> {
        self.items.iter().map(|grpx| &grpx.region).collect()
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<SomeGroup> {
        self.items.iter()
    }

    /// Return a mutable iterator
    pub fn iter_mut(&mut self) -> IterMut<SomeGroup> {
        self.items.iter_mut()
    }

    /// Return the number of groups.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Find a group that matches a region, return a mutable reference.
    pub fn find_mut(&mut self, val: &SomeRegion) -> Option<&mut SomeGroup> {
        self.items.iter_mut().find(|grpx| grpx.region == *val)
    }

    /// Find a group that matches a region, return a reference.
    pub fn find(&self, val: &SomeRegion) -> Option<&SomeGroup> {
        self.items.iter().find(|&grpx| grpx.region == *val)
    }

    /// Check limited setting in groups due to new bit that can change.
    pub fn check_limited(&mut self, max_reg: &SomeRegion) {
        for grpx in &mut self.items {
            if grpx.limited {
                grpx.check_limited(max_reg);
            }
        }
    }

    /// Return a String representation of a GroupStore.
    fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::new();

        for grpx in &self.items {
            if flg == 1 {
                rc_str.push_str(",\n              ");
            }
            rc_str.push_str(&grpx.to_string());
            flg = 1;
        }

        rc_str
    }
} // end impl GroupStore

impl Index<usize> for GroupStore {
    type Output = SomeGroup;
    fn index(&self, i: usize) -> &SomeGroup {
        &self.items[i]
    }
}

impl IndexMut<usize> for GroupStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}
