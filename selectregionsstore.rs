//! Implement a struct of Select RegionStores.

use crate::regionstore::{vec_rs_corr_split_by_partial_intersection, RegionStore};

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
use std::slice::Iter;

impl fmt::Display for SelectRegions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = self.regions.formatted_string();
        str.push_str(&format!("/{}", self.value));
        write!(f, "{}", str)
    }
}

//#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct SelectRegions {
    /// Regions, in domain order, describing the requirements for an select state.
    pub regions: RegionStore,
    /// A value for being in the select state.
    pub value: isize,
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
/// A struct of select RegionStores, where each RegionStore contains a list
/// of domain select regions, in domain number order.
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
    pub fn push(&mut self, regions: RegionStore, value: isize) {
        if value != 0 && !self.contains(&regions) {
            self.regionstores.push(SelectRegions { regions, value });
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

    /// Return the number of supersets of a StateStore
    pub fn number_supersets_of_states(&self, stas: &[&SomeState]) -> usize {
        self.regionstores
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_corr_states(stas)))
            .sum()
    }

    /// Return list of negative select regions that are superset of a State vector.
    pub fn negative_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| regsx.value < 0 && regsx.regions.is_superset_corr_states(stas))
            .collect()
    }

    /// Return a Vector of RegionStores not supersets of a given StateStore.
    pub fn not_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_corr_states(stas))
            .collect()
    }

    /// Return true if any RegionStore is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &[&SomeState]) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_corr_states(stas) {
                return true;
            }
        }
        false
    }

    // Return the aggregate value of select regions the current states are in.
    pub fn value_supersets_of_states(&self, stas: &[&SomeState]) -> isize {
        let mut val: isize = 0;
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_corr_states(stas) {
                val += regsx.value;
            }
        }
        val
    }

    /// Return true if any RegionStore is a superset of a given RegionStore.
    pub fn any_supersets_of(&self, regs: &RegionStore) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_corr(regs) {
                return true;
            }
        }
        false
    }

    /// Return list of select regions that are superset of a State vector.
    pub fn supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| regsx.regions.is_superset_corr_states(stas))
            .collect()
    }

    /// Return list of negative select regions that are superset of a State vector.
    pub fn number_negative_supersets_of_states(&self, stas: &[&SomeState]) -> usize {
        self.regionstores
            .iter()
            .map(|regsx| {
                usize::from(regsx.value < 0 && regsx.regions.is_superset_corr_states(stas))
            })
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

    /// Return a list of regionstores, split by intersections, until
    /// none have a partial intersection with the original regionstores.
    /// Some result regionstores may overlap each other.
    /// The original regionstores minus the result regionstores should be null.
    /// Each result regionstore will be a subset of one, or more, of the original regionstores.
    pub fn split_by_intersections(&self) -> Self {
        let mut rs = Vec::<RegionStore>::with_capacity(self.len());
        for reg_valx in &self.regionstores {
            rs.push(reg_valx.regions.clone());
        }

        let mut ret = Self::new(vec![]);
        for reg_strx in vec_rs_corr_split_by_partial_intersection(&rs) {
            let mut val = 0;
            for reg_valx in &self.regionstores {
                if reg_valx.regions.is_superset_corr(&reg_strx) {
                    val += reg_valx.value;
                }
            }
            ret.push(reg_strx, val);
        }
        ret
    }

    /// Return true if an equal RegionStore is already in the SelectRegionsStore.
    fn contains(&self, regstr: &RegionStore) -> bool {
        for regstrx in &self.regionstores {
            if regstrx.regions.equal_corr(regstr) {
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

        for selx in sup_store.iter() {
            // Get edges mask for a negative superset
            let mut edges = selx.regions.edge_mask_corr();

            // Restrict masks, based on what can change.
            for inx in 0..edges.len() {
                //println!("choose_select_exit_needs: edge {} and b10 {} and b01 {}", edges[inx], changes[inx].b01, changes[inx].b10);
                edges[inx] =
                    edges[inx].bitwise_and(&changes[inx].b01.bitwise_and(&changes[inx].b10));
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
            let dom_num =
                non_zero_edge_dom_ids[rand::thread_rng().gen_range(0..non_zero_edge_dom_ids.len())];

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

        //let sup_store2 = self.negative_supersets_of_states(&all_states2);
        //println!("choose_select_exit_needs: target states are in {} negative select regions", sup_store2.len());

        let mut ret_nds = NeedStore::new(vec![]);
        ret_nds.push(SomeNeed::FromSelectRegion {
            target_states: StateStore::new(target_states),
        });

        //println!("choose_select_exit_needs: returning need: {}", ret_nds[0]);
        Some(ret_nds)
    }

    /// Return the index of the highest rating of a plan, in a vector of plans.
    /// All plans should be for the same domain.
    pub fn rate_plans_vec<'a>(
        &self,
        plan_vec: Vec<&'a SomePlan>,
        mut all_states: Vec<&'a SomeState>,
    ) -> usize {
        assert!(!plan_vec.is_empty());

        if plan_vec.len() == 1 {
            return 0;
        }

        let mut rates = Vec::<isize>::with_capacity(plan_vec.len());

        for planx in plan_vec.iter() {
            rates.push(self.rate_plan(planx, &mut all_states));
        }

        let max_rate = rates.iter().min().unwrap();

        let mut max_rate_inxs = Vec::<usize>::new();
        for (inx, rtx) in rates.iter().enumerate() {
            if rtx == max_rate {
                max_rate_inxs.push(inx);
            }
        }

        if max_rate_inxs.len() == 1 {
            return max_rate_inxs[0];
        }

        max_rate_inxs[rand::thread_rng().gen_range(0..max_rate_inxs.len())]
    }

    /// Return the sum of all select regions values a plan goes through.
    /// This ignores the select regions a plan starts, or end, in.
    pub fn rate_plan<'a>(&self, aplan: &'a SomePlan, all_states: &mut [&'a SomeState]) -> isize {
        if aplan.len() < 2 {
            return 0;
        }

        let dom_num = aplan.dom_num;

        let mut sum: isize = 0;

        for (inx, stepx) in aplan.iter().enumerate() {
            if inx == 0 {
                continue;
            }
            all_states[dom_num] = &stepx.initial.state1;
            let valx = self.value_supersets_of_states(all_states);
            if valx < 0 {
                sum += valx;
            }
        }

        sum
    }

    /// Return the sum of all select regions values a plan goes through.
    /// This ignores the select regions a plan starts, or end, in.
    /// If GT 1 plan is in the PlanStore, the result will be zero, which is a problem to be delt with later.
    pub fn rate_plans_store<'a>(
        &self,
        aplan: &'a PlanStore,
        mut all_states: Vec<&'a SomeState>,
    ) -> isize {
        let mut inx = 0;
        let mut num_inx = 0;
        for (iny, plnx) in aplan.iter().enumerate() {
            if !plnx.is_empty() {
                inx = iny;
                num_inx += 1;
            }
        }
        if num_inx != 1 {
            return 0;
        }

        self.rate_plan(&aplan[inx], &mut all_states)
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

        let mut regstr1 = RegionStore::with_capacity(1);
        let reg1 = dmxs[0]
            .region_from_string_pad_x("rX1XX")
            .expect("String should be formatted correctly");

        regstr1.push(reg1.clone());

        // Add select regionstores.
        dmxs.add_select(regstr1, -1);

        let mut regstr1 = RegionStore::with_capacity(1);
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
            return Err(format!("Needs are None"));
        }

        //assert!(1 == 2);
        Ok(())
    }
}
