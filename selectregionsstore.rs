//! Implement a struct of Select RegionStores.

use crate::regionstorecorr::RegionStoreCorr;

use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::state::SomeState;
use crate::statestore::StateStore;

use rand::Rng;
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

//#[readonly::make]
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

    /// Return the number of supersets of a StateStore
    pub fn number_supersets_of_states(&self, stas: &[&SomeState]) -> usize {
        self.regionstores
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_states(stas)))
            .sum()
    }

    /// Return the rate of a vector of states, the result/goal of a number of domain plans.
    pub fn rate_states(&self, stas: &[&SomeState]) -> isize {
        self.regionstores
            .iter()
            .map(|regsx| {
                if regsx.regions.is_superset_states(stas) {
                    regsx.value
                } else {
                    0
                }
            })
            .sum()
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

    /// Return list of negative select regions that are superset of a State vector.
    pub fn negative_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| regsx.value < 0 && regsx.regions.is_superset_states(stas))
            .collect()
    }

    /// Return a Vector of SelectRegions not supersets of a given StateStore.
    pub fn not_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_states(stas))
            .collect()
    }

    /// Return a Vector of positive value SelectRegions not supersets of a given StateStore.
    pub fn positive_not_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_states(stas) && regsx.value > 0)
            .collect()
    }

    /// Return a Vector of positive value SelectRegions.
    pub fn positive_select_regions(&self) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| regsx.value > 0)
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

    /// Return list of positive value select regions that are superset of a State vector.
    pub fn positive_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| regsx.regions.is_superset_states(stas) && regsx.value > 0)
            .collect()
    }

    /// Return list of negative select regions that are superset of a State vector.
    pub fn number_negative_supersets_of_states(&self, stas: &[&SomeState]) -> usize {
        self.regionstores
            .iter()
            .map(|regsx| usize::from(regsx.value < 0 && regsx.regions.is_superset_states(stas)))
            .sum()
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
        let mut rs = Vec::<RegionStoreCorr>::with_capacity(self.len());
        for reg_valx in &self.regionstores {
            rs.push(reg_valx.regions.clone());
        }

        RegionStoreCorr::vec_split_to_subsets(&rs)
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

    /// Return needs to exit negative select regions.
    pub fn choose_select_exit_needs(
        &self,
        all_states: &[&SomeState],
        changes: &[&SomeChange],
    ) -> Option<NeedStore> {
        let sup_store = self.negative_supersets_of_states(all_states);
        if sup_store.is_empty() {
            //println!("choose_select_exit_needs: Current states not in any negative regions?");
            return None;
        }

        // Init target masks.
        let mut target_masks = Vec::<SomeMask>::with_capacity(all_states.len());
        for statex in all_states.iter() {
            target_masks.push(SomeMask::new_low(statex.num_ints()));
        }

        // Make masks for what can change.
        let mut change_mask = Vec::<SomeMask>::with_capacity(changes.len());
        for (cngx, stax) in changes.iter().zip(all_states.iter()) {
            change_mask.push(
                cngx.b10
                    .bitwise_and(*stax)
                    .bitwise_or(&cngx.b01.bitwise_and(&stax.bitwise_not())),
            );
        }

        // Try to find bits to change, up to 5 times.
        for _ in 0..5 {
            for selx in sup_store.iter() {
                // Get edges mask for a negative superset
                let mut edges = selx.regions.edge_masks();

                for (edgex, mskx) in edges.iter_mut().zip(change_mask.iter()) {
                    //println!("choose_select_exit_needs: edge {} and b10 {} and b01 {}", edges[inx], changes[inx].b01, changes[inx].b10);
                    *edgex = edgex.bitwise_and(mskx);
                }

                // Identify non-zero edge masks.
                let mut non_zero_edge_dom_ids = Vec::<usize>::new();
                for (dom_numx, dom_edge_mask) in edges.iter().enumerate() {
                    if dom_edge_mask.is_not_low() {
                        non_zero_edge_dom_ids.push(dom_numx);
                    }
                }

                // Check if stuck.
                if non_zero_edge_dom_ids.is_empty() {
                    //println!("choose_select_exit_needs: non zero edge masks not found?");
                    return None;
                }
                //println!("choose_select_exit_needs: non zero edge masks found");

                // Randomly choose a non-zero domain edge mask.
                let dom_num = non_zero_edge_dom_ids
                    [rand::thread_rng().gen_range(0..non_zero_edge_dom_ids.len())];

                // Split the edge mask bits of the selected domain edge mask.
                let single_bit_edge_masks = edges[dom_num].split();

                // Randomly choose a mask.
                let single_bit_edge_mask = &single_bit_edge_masks
                    [rand::thread_rng().gen_range(0..single_bit_edge_masks.len())];
                //println!("choose_select_exit_needs: mask {} chosen", single_bit_edge_mask);

                // Do a Boolean OR with target masks, as two regions could have one edge in common, an XOR with the current state could be undone.
                target_masks[dom_num] = target_masks[dom_num].bitwise_or(single_bit_edge_mask);
            } // Next selx.

            // Generate target states.
            let mut target_states = Vec::<SomeState>::with_capacity(all_states.len());
            for (statex, maskx) in all_states.iter().zip(target_masks.iter()) {
                target_states.push(statex.bitwise_xor(maskx));
            }

            let mut all_states2 = Vec::<&SomeState>::with_capacity(all_states.len());
            for stax in target_states.iter() {
                all_states2.push(stax);
            }
            if self.negative_supersets_of_states(&all_states2).is_empty() {
                let mut needx = SomeNeed::ExitSelectRegion {
                    target_states: StateStore::new(target_states),
                    priority: 0,
                };
                needx.set_priority();
                let ret_nds = NeedStore::new(vec![needx]);

                //println!("choose_select_exit_needs: returning need: {}", ret_nds[0]);
                return Some(ret_nds);
            }
        }
        None
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

        let mut sum: isize = 0;

        for (inx, stepx) in aplan.iter().enumerate() {
            if inx == 0 {
                continue;
            }
            all_states[dom_num] = &stepx.initial.state1;
            let valx = self.value_supersets_of_states(&all_states);
            // Print violations.
            //for selx in self.regionstores.iter() {
            //    if selx.regions.is_superset_states(&all_states) {
            //        println!("step {} of {} violates {} at {}", stepx, aplan, selx, SomeState::vec_ref_string(&all_states));
            //    }
            //}

            if valx < 0 {
                sum += valx;
            }
        }

        sum
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
        let mut rate = 0;
        for planx in plans.iter() {
            // Check that plan starts in the right state.
            let start = planx.initial_region();
            if start.state1 != *all_states[planx.dom_num]
                || start.state2 != *all_states[planx.dom_num]
            {
                panic!("plans not in sync!");
            }
            rate += self.rate_plan(planx, &all_states);
            all_states[planx.dom_num] = &planx.result_region().state1;
        }

        rate
    }

    /// Return the number of negative select regions a plan passes though, restricted to the domain number
    /// part of select regions.
    ///
    /// This ignores the first step initial region, and the last step result reigon.
    ///
    /// If running multiple domain plans, and at least one plan never passes through a
    /// negative domain-restricted region (result = 0), the the plans can be run in parallel.
    pub fn number_negative_regions(&self, aplan: &SomePlan) -> usize {
        if aplan.len() < 2 {
            return 0;
        }
        let mut ret: usize = 0;
        for inx in 1..aplan.len() {
            for regsx in self.regionstores.iter() {
                if regsx.value >= 0 {
                    continue;
                }
                if regsx[aplan.dom_num].is_superset_of(&aplan[inx].initial) {
                    ret += 1;
                }
            }
        }
        ret
    }
} // End impl SelectRegionsStore

impl Index<usize> for SelectRegionsStore {
    type Output = SelectRegions;
    fn index(&self, i: usize) -> &SelectRegions {
        &self.regionstores[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::SomeDomain;
    use crate::domainstore::DomainStore;

    // Test choose_select_exit_needs with two overlapping negative select regions.
    // from s0111 to s1000.
    #[test]
    fn test_choose_select_exit_needs() -> Result<(), String> {
        // Init a DomainStore.
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1)]);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        let reg1 = dmxs[0]
            .region_from_string_pad_x("rX1XX")
            .expect("String should be formatted correctly");

        regstr1.push(reg1.clone());

        // Add select regionstores.
        dmxs.add_select(regstr1, -1);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        let reg2 = dmxs[0]
            .region_from_string_pad_x("r1XX1")
            .expect("String should be formatted correctly");
        regstr1.push(reg2.clone());

        // Add select regionstores.
        dmxs.add_select(regstr1, -1);

        // Set state for domain 0, using 1 integer for bits.
        let state1 = dmxs[0].state_from_string("s0xd")?;
        dmxs[0].set_state(&state1);

        // Finish select regions setup.
        dmxs.calc_select();

        // Get exit needs.
        let all_states = vec![&dmxs[0].cur_state];
        let changex = SomeChange::new(
            SomeMask::new_from_string(1, "m0b1111")?,
            SomeMask::new_from_string(1, "m0b1111")?,
        );
        let changes = vec![&changex];
        if let Some(nds) = dmxs.select.choose_select_exit_needs(&all_states, &changes) {
            println!("needs len {}", nds.len());
            assert!(nds.len() == 1);
            println!("need: {}", nds[0]);
            assert!(!reg1.is_superset_of_state(&nds[0].target()[0].region.state1));
            assert!(!reg2.is_superset_of_state(&nds[0].target()[0].region.state1));
        } else {
            return Err(format!("Needs are None?"));
        }

        Ok(())
    }
}
