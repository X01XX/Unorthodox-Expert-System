//! Implement a struct of SelectRegionStore.

use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::regionstore::RegionStore;
use crate::selectregions::SelectRegions;
use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::{Iter, IterMut};

impl fmt::Display for SelectRegionsStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
/// A struct of SelectRegions.
pub struct SelectRegionsStore {
    pub regionstores: Vec<SelectRegions>,
}

impl SelectRegionsStore {
    /// Return a new SelectRegionsStores instance.
    pub fn new(regionstores: Vec<SelectRegions>) -> Self {
        Self { regionstores }
    }

    /// Return a new SelectRegionStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            regionstores: Vec::<SelectRegions>::with_capacity(num),
        }
    }

    /// Add a SelectRegionsStore.
    pub fn push(&mut self, select: SelectRegions) {
        if !self.contains(&select.regions) {
            self.regionstores.push(select);
        }
    }

    /// Add a SelectRegionsStore, deleting subsets.
    pub fn push_nosubs(&mut self, select: SelectRegions) {
        // Don't add a subset.
        if self.any_supersets_of(&select.regions) {
            return;
        }
        // Identify subsets.
        let mut del = Vec::<usize>::new();
        for (inx, regstx) in self.regionstores.iter().enumerate() {
            if regstx.regions.is_subset_of_corr(&select.regions) {
                del.push(inx);
            }
        }
        // Remove subsets, highest indicies first.
        for inx in del.iter().rev() {
            tools::remove_unordered(&mut self.regionstores, *inx);
        }
        // Add new select instance.
        self.regionstores.push(select);
    }

    /// Return the length of an instance.
    pub fn len(&self) -> usize {
        self.regionstores.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.regionstores.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.regionstores.is_empty()
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<SelectRegions> {
        self.regionstores.iter()
    }

    /// Return an mut iterator
    pub fn iter_mut(&mut self) -> IterMut<SelectRegions> {
        self.regionstores.iter_mut()
    }

    /// Return the sum of values and times visited of Select Regions thaot are superset of a given RegionStore.
    pub fn rate_regions(&self, regs: &RegionStore) -> (isize, usize) {
        let mut times_visited: usize = 0;
        let mut value: isize = 0;
        for regsx in self.regionstores.iter() {
            if regsx.regions.is_superset_of_corr(regs) {
                value += regsx.value;
                times_visited += regsx.times_visited;
            }
        }
        (value, times_visited)
    }

    /// Return a Vector of SelectRegions not supersets of a given StateStore.
    pub fn not_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_states_corr(stas))
            .collect()
    }

    /// Return true if any SelectRegion is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &[&SomeState]) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_states_corr(stas) {
                return true;
            }
        }
        false
    }

    /// Return the aggregate value of select regions the current states are in.
    pub fn value_supersets_of_states(&self, stas: &[&SomeState]) -> isize {
        let mut val: isize = 0;
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_states_corr(stas) {
                val += regsx.value;
            }
        }
        val
    }

    /// Return true if any SelectRegion is a superset of a given RegionStore.
    pub fn any_supersets_of(&self, regs: &RegionStore) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_of_corr(regs) {
                return true;
            }
        }
        false
    }

    /// Return true if any SelectRegion intersects a given RegionStore.
    pub fn any_intersection_of(&self, regs: &RegionStore) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.intersects_corr(regs) {
                return true;
            }
        }
        false
    }

    /// Return list of select regions that are superset of a State vector.
    pub fn supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| regsx.regions.is_superset_states_corr(stas))
            .collect()
    }

    /// Return a string represeting an SelectRegionsStore.
    fn formatted_string(&self) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in self.regionstores.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&orx.to_string());
        }
        ret_str.push(']');
        ret_str
    }

    /// Return true if an equal RegionStore is already in the SelectRegionsStore.
    fn contains(&self, regstr: &RegionStore) -> bool {
        for regstrx in &self.regionstores {
            if regstrx.regions.eq_corr(regstr) {
                return true;
            }
        }
        false
    }

    /// Return the sum of all negative select regions values a plan goes through.
    /// This ignores the select regions a plan starts, or end, in.
    pub fn rate_plan<'a>(&self, aplan: &'a SomePlan, current_states: &[&'a SomeState]) -> isize {
        if aplan.len() < 2 {
            return 0;
        }

        // Create a mutable state ref vector.
        let mut all_states = Vec::<&SomeState>::with_capacity(current_states.len());
        for statex in current_states.iter() {
            all_states.push(statex);
        }

        let dom_num = aplan.dom_num;

        // Store rate for each step.
        let mut rates = Vec::<isize>::with_capacity(aplan.len());

        for stepx in aplan.iter() {
            all_states[dom_num] = stepx.initial.state1();
            let valx = self.value_supersets_of_states(&all_states);
            // Print violations.
            //for selx in self.regionstores.iter() {
            //    if selx.regions.is_superset_states(&all_states) {
            //        println!("step {} of {} violates {} at {}", stepx, aplan, selx, SomeState::vec_ref_string(&all_states));
            //    }
            //}

            rates.push(valx);
        }

        // Delete consecutive negative rates on end.
        // The goal may be in a negative region.
        while let Some(ratex) = rates.last() {
            if *ratex < 0 {
                rates.pop();
            } else {
                break;
            }
        }
        // Delete consecutive negative rates at beginning.
        // The initial region may be in a negative region.
        rates.reverse();
        while let Some(ratex) = rates.last() {
            if *ratex < 0 {
                rates.pop();
            } else {
                break;
            }
        }

        if rates.is_empty() {
            return 0;
        }
        rates.iter().sum()
    }

    /// Return the sum of all select negative regions values a plan goes through.
    /// This ignores the select regions a plan starts, or ends, in.
    pub fn rate_plans<'a>(&self, plans: &'a PlanStore, current_states: &[&'a SomeState]) -> isize {
        let mut non_empty_plans = Vec::<usize>::new();
        for (iny, plnx) in plans.iter().enumerate() {
            if !plnx.is_empty() {
                non_empty_plans.push(iny);
            }
        }
        if non_empty_plans.is_empty() {
            return 0;
        }

        if non_empty_plans.len() == 1 {
            return self.rate_plan(&plans[non_empty_plans[0]], current_states);
        }

        // Rate a multi-plan PlanStore.

        // Create mutable current_states vector.
        let mut all_states = Vec::<&SomeState>::with_capacity(current_states.len());
        for stateref in current_states.iter() {
            all_states.push(stateref);
        }

        // Rate each option.
        let mut rate: isize = 0;
        for planx in plans.iter() {
            // Check that plan starts in the right state.
            let start = planx.initial_region();
            if start.state1() != all_states[planx.dom_num]
                || start.state2() != all_states[planx.dom_num]
            {
                panic!("plans not in sync!");
            }
            rate += self.rate_plan(planx, &all_states);
            all_states[planx.dom_num] = planx.result_region().state1();
        }
        rate
    }

    /// Subtract a SelectRegions.
    pub fn subtract(&self, subtrahend: &SelectRegions) -> Self {
        let mut ret_str = Self::new(vec![]);

        for regy in self.iter() {
            if subtrahend.intersects(regy) {
                for regz in regy.regions.subtract_corr(&subtrahend.regions) {
                    ret_str.push_nosubs(SelectRegions::new(regz, 0));
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Append from another store.
    pub fn append(&mut self, mut val: Self) {
        self.regionstores.append(&mut val.regionstores);
    }
} // End impl SelectRegionsStore

impl Index<usize> for SelectRegionsStore {
    type Output = SelectRegions;
    fn index(&self, i: usize) -> &SelectRegions {
        &self.regionstores[i]
    }
}
