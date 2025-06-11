//! The GroupStore struct, a vector of SomeGroup structs.

use crate::bits::vec_same_num_bits;
use crate::change::SomeChange;
use crate::group::SomeGroup;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};

impl fmt::Display for GroupStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
/// A vector of SomeGroup structs, SomeGroup-specific functions,
/// and aggregate changes allowed by the structs that are predictable.
pub struct GroupStore {
    /// Vector of SomeGroup structs.
    pub items: Vec<SomeGroup>,
}

impl GroupStore {
    /// Return a new, empty, GroupStore.
    pub fn new(items: Vec<SomeGroup>) -> Self {
        debug_assert!(vec_same_num_bits(&items));

        Self { items }
    }

    /// Calculate and return the aggregate changes of all group rules.
    pub fn calc_aggregate_changes(&self) -> Option<SomeChange> {
        let mut aggregate_changes: Option<SomeChange> = None;

        for grpx in &self.items {
            if grpx.pn == Pn::Unpredictable {
                continue;
            }
            for rulx in grpx.rules.as_ref().expect("SNH").iter() {
                if let Some(changes) = aggregate_changes {
                    aggregate_changes = Some(changes.union(rulx));
                } else {
                    aggregate_changes = Some(rulx.as_change());
                }
            }
        }
        aggregate_changes
    }

    /// Return true if an item is in exactly one group.
    pub fn in_1_group(&self, itmx: &impl tools::AccessStates) -> bool {
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

    /// Return the groups regions an item is in.
    pub fn groups_in(&self, itmx: &impl tools::AccessStates) -> Vec<&SomeRegion> {
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
                if astate == &anchor.pinnacle {
                    return Some(true);
                }
            }
            return Some(false);
        }
        None
    }

    /// Return the number of groups an item is in.
    pub fn num_groups_in(&self, itmx: &impl tools::AccessStates) -> usize {
        let mut count = 0;
        for grpx in &self.items {
            if grpx.is_superset_of(itmx) {
                count += 1;
            }
        }
        count
    }

    /// Return true if any group is a superset of, or equal to, an item.
    pub fn any_superset_of(&self, itmx: &impl tools::AccessStates) -> bool {
        tools::vec_contains(&self.items, SomeGroup::is_superset_of, itmx)
    }

    /// Return regions of any group is a superset, or equal, to a region.
    pub fn supersets_of(&self, itmx: &impl tools::AccessStates) -> RegionStore {
        let mut ret_str = RegionStore::new(vec![]);

        for grpx in self.items.iter() {
            if grpx.region.is_superset_of(itmx) {
                ret_str.push(grpx.region.clone());
            }
        }
        ret_str
    }

    /// Return regions of any group is a subset, or equal, to a region.
    pub fn subsets_of(&self, itmx: &impl tools::AccessStates) -> RegionStore {
        let mut ret_str = RegionStore::new(vec![]);

        for grpx in self.items.iter() {
            if grpx.region.is_subset_of(itmx) {
                ret_str.push(grpx.region.clone());
            }
        }
        ret_str
    }

    /// Find and remove a group, given a group region.
    /// Maintain group item order.
    pub fn remove_group(&mut self, reg: &SomeRegion) -> bool {
        // Find a matching group region
        let mut fnd = false;
        let mut inx = 0;

        for grpx in &mut self.items {
            if grpx.region == *reg {
                fnd = true;
                break;
            }
            inx += 1;
        }

        // Remove the group
        if fnd {
            self.items.remove(inx);
        }
        fnd
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
    fn formatted_str(&self) -> String {
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

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Add a group, removing subset groups.
    pub fn push_nosubs(&mut self, grpx: SomeGroup) -> bool {
        debug_assert!(self.is_empty() || grpx.region.num_bits() == self[0].region.num_bits());

        // Check for supersets.
        if self.any_superset_of(&grpx) {
            //println!("skipped adding group {}, a superset exists in {}", grpx, self);
            return false;
        }

        // Identify subsets.
        let mut rmvec = Vec::<usize>::new();

        for (inx, grpy) in self.items.iter().enumerate() {
            if grpy.is_subset_of(&grpx) {
                rmvec.push(inx);
            }
        }

        // Remove identified groups, in descending index order.
        for inx in rmvec.iter().rev() {
            tools::remove_unordered(&mut self.items, *inx);
        }

        self.items.push(grpx);

        true
    }
} // end impl GroupStore

impl IntoIterator for GroupStore {
    type Item = SomeGroup;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

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
