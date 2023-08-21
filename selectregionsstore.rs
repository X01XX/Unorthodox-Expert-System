//! Implement a struct of Select RegionStores.

use crate::regionstorecorr::RegionStoreCorr;

use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::{Iter, IterMut};

impl fmt::Display for SelectRegions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = self.regions.formatted_string();
        str.push_str(&format!(", value: {:+}", self.value));
        str.push_str(&format!(", times visited {}", self.times_visited));
        write!(f, "{}", str)
    }
}

#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct SelectRegions {
    /// Regions, in domain order, describing the requirements for an select state.
    /// If the regions are all X, except for one, then it affects only one domain.
    /// Otherwise, it affects a combination of two, or more, domains.
    pub regions: RegionStoreCorr,
    /// A value for being in the select state.
    /// A Positive value is, so far, given to a goal state.
    /// A negative value is, so far, given to a plan that passes through the regions,
    /// not counting the beginning and end state.
    pub value: isize,
    /// A cond of the number of time a SelectRegion has been visited due to satisfying a need.
    pub times_visited: usize,
}

impl Index<usize> for SelectRegions {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.regions[i]
    }
}

impl fmt::Display for SelectRegionsStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SelectRegions {
    /// Return a string representation for a vector of SelectRegions references.
    pub fn vec_ref_string(avec: &[&SelectRegions]) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in avec.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&format!("{}", orx));
        }
        ret_str.push(']');
        ret_str
    }
    /// Increment times visited.
    pub fn inc_times_visited(&mut self) {
        self.times_visited += 1;
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

    /// Add a RegionsStore.
    pub fn push(&mut self, regions: RegionStoreCorr, value: isize) {
        if value != 0 && !self.contains(&regions) {
            self.regionstores.push(SelectRegions {
                regions,
                value,
                times_visited: 0,
            });
        }
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

    /// Return the sum of values and times visited of Select Regions thaot are superset of a given RegionStoreCorr.
    pub fn rate_regions(&self, regs: &RegionStoreCorr) -> (isize, usize) {
        let mut times_visited: usize = 0;
        let mut value: isize = 0;
        for regsx in self.regionstores.iter() {
            if regsx.regions.is_superset_of(regs) {
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
            .filter(|regsx| !regsx.regions.is_superset_states(stas))
            .collect()
    }

    /// Return true if any SelectRegion is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &[&SomeState]) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_states(stas) {
                return true;
            }
        }
        false
    }

    /// Return the aggregate value of select regions the current states are in.
    pub fn value_supersets_of_states(&self, stas: &[&SomeState]) -> isize {
        let mut val: isize = 0;
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_states(stas) {
                val += regsx.value;
            }
        }
        val
    }

    /// Return true if any SelectRegion is a superset of a given RegionStoreCorr.
    pub fn any_supersets_of(&self, regs: &RegionStoreCorr) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_of(regs) {
                return true;
            }
        }
        false
    }

    /// Return list of select regions that are superset of a State vector.
    pub fn supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| regsx.regions.is_superset_states(stas))
            .collect()
    }

    /// Return a string represeting an SelectRegionsStore.
    pub fn formatted_string(&self) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in self.regionstores.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&format!("{}", orx));
        }
        ret_str.push(']');
        ret_str
    }

    /// Return a list of RegionStoreCorrs, split by intersections, until
    /// none have a partial intersection with the original RegionStoreCorrs.
    /// Some result RegionStoreCorrs may overlap each other.
    /// Each result regionstore will be a subset of one, or more, of the original regionstores,
    /// where the sum of the SelectRegion values is greater than zero.
    pub fn split_to_subsets(&self) -> Vec<RegionStoreCorr> {
        let mut rs = Vec::<&RegionStoreCorr>::with_capacity(self.len());
        for reg_valx in &self.regionstores {
            rs.push(&reg_valx.regions);
        }

        RegionStoreCorr::vec_ref_split_to_subsets(&rs)
    }

    /// Return true if an equal RegionStore is already in the SelectRegionsStore.
    fn contains(&self, regstr: &RegionStoreCorr) -> bool {
        for regstrx in &self.regionstores {
            if regstrx.regions == *regstr {
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
} // End impl SelectRegionsStore

impl Index<usize> for SelectRegionsStore {
    type Output = SelectRegions;
    fn index(&self, i: usize) -> &SelectRegions {
        &self.regionstores[i]
    }
}
