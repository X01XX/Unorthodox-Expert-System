//! Implement a struct of SelectRegionStore.

use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
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
        if !self.contains(&select) {
            self.regionstores.push(select);
        }
    }

    /// Add a SelectRegionsStore, deleting subsets.
    pub fn push_nosubs(&mut self, select: SelectRegions) {
        // Don't add a subset.
        if self.any_supersets_of(&select) {
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

    /// Add a SelectRegionsStore, deleting supersets.
    pub fn push_nosups(&mut self, select: SelectRegions) {
        // Don't add a superset.
        if self.any_subsets_of(&select) {
            return;
        }
        // Identify supersets.
        let mut del = Vec::<usize>::new();
        for (inx, regstx) in self.regionstores.iter().enumerate() {
            if regstx.regions.is_superset_of_corr(&select.regions) {
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
                value += regsx.value();
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

    /// Return the aggregate value of negative select regions the current states are in.
    pub fn value_supersets_of_states(&self, stas: &[&SomeState]) -> isize {
        let mut val: isize = 0;
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_states_corr(stas) && regsx.value() < 0 {
                val += regsx.value();
            }
        }
        val
    }

    /// Return true if any SelectRegion is a region superset of another..
    pub fn any_supersets_of(&self, other: &SelectRegions) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_of_corr(&other.regions) {
                return true;
            }
        }
        false
    }

    /// Return true if any SelectRegion is a region subset of another.
    pub fn any_subsets_of(&self, other: &SelectRegions) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_subset_of_corr(&other.regions) {
                return true;
            }
        }
        false
    }

    /// Return true if any SelectRegion has regions equal to another.
    pub fn any_equal_regions(&self, other: &SelectRegions) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.eq_corr(&other.regions) {
                return true;
            }
        }
        false
    }

    /// Return the intersection of a SelectRegionStore and a Selectregions instance.
    pub fn intersection(&self, slrx: &SelectRegions) -> Option<Self> {
        let mut ret = Self::new(vec![]);
        for inx in 0..self.len() {
            if let Some(regs) = self[inx].intersection(slrx) {
                ret.push_nosubs(regs);
            }
        }
        if ret.is_empty() {
            return None;
        }
        Some(ret)
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
    fn contains(&self, selx: &SelectRegions) -> bool {
        for regstrx in &self.regionstores {
            if regstrx.regions.eq_corr(&selx.regions) {
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
    pub fn subtract(&self, subtrahend: &SelectRegions) -> Option<Self> {
        //println!("subtract {subtrahend} from {self}");
        let mut ret_str = Self::new(vec![]);

        for regy in self.iter() {
            if subtrahend.intersects(regy) {
                if subtrahend.regions.eq_corr(&regy.regions) {
                    continue;
                }
                for regz in regy.regions.subtract_corr(&subtrahend.regions) {
                    ret_str.push_nosubs(SelectRegions::new(regz, regy.pos, regy.neg));
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        //println!(
        //    "subtract {} from {} giving {}",
        //    subtrahend.regions,
        //    self.regions(),
        //    ret_str.regions()
        //);
        if ret_str.is_empty() {
            return None;
        }
        Some(ret_str)
    }

    /// Append from another store.
    pub fn append(&mut self, mut val: Self) {
        self.regionstores.append(&mut val.regionstores);
    }

    /// Pop the last item.
    pub fn pop(&mut self) -> Option<SelectRegions> {
        self.regionstores.pop()
    }

    /// Return SelectRegions split into subsets.
    /// All subsets will add up to the original regions.
    /// All subsets will be a subset of one, or more, of the original regions,
    /// No subsets will be a partial intersection of any of the original regions.
    pub fn split_to_subsets(&self) -> Vec<RegionStore> {
        //println!("split_to_subsets: {self}");

        let mut orig_stores = Vec::<RegionStore>::with_capacity(self.len());
        let mut cur_stores = Vec::<RegionStore>::with_capacity(self.len());
        for selregsx in self.regionstores.iter() {
            cur_stores.push(selregsx.regions.clone());
            orig_stores.push(selregsx.regions.clone());
        }
        // Handle nothing to split.
        if self.len() < 2 {
            return cur_stores;
        }
        // Split until nothing left to split.
        loop {
            // Process all intersections between any two SelectRegions.
            let mut split = Vec::<usize>::with_capacity(self.len());
            let mut next_stores = Vec::<RegionStore>::new();

            // Compare each combination of RegionStores.
            for (inx, curx) in cur_stores.iter().enumerate() {
                for origy in orig_stores.iter() {
                    if curx.eq_corr(origy) {
                        continue;
                    }
                    if curx.is_subset_of_corr(origy) {
                        continue;
                    }
                    //println!("x {} y {}", curx, origy);

                    if let Some(intx) = curx.intersection_corr(origy) {
                        //println!("x {} y {} ntx {intx}", curx, origy);

                        if !split.contains(&inx) {
                            split.push(inx);
                        }

                        // Init vector for region-by-region split.
                        let mut x_regs = Vec::<RegionStore>::new();

                        // Handle inx leftovers.
                        for (regstr_x, regstr_int) in curx.iter().zip(intx.iter()) {
                            let x_left: Vec<SomeRegion> = regstr_x.subtract(regstr_int);
                            //println!(
                            //    "subtractx {regstr_int} from {regstr_x} giving {}",
                            //    tools::vec_string(&x_left)
                            //);
                            if x_left.is_empty() {
                                x_regs.push(RegionStore::new(vec![regstr_x.clone()]));
                            } else {
                                x_regs.push(RegionStore::new(x_left));
                            }
                        }

                        // Generate a list of x_regs indicies.
                        let mut x_nums = Vec::<Vec<usize>>::with_capacity(x_regs.len());
                        for regsx in x_regs.iter() {
                            x_nums.push((0..regsx.len()).collect());
                        }
                        //println!("x nums {:?}", x_nums);

                        let any1ofx = tools::any_one_of(&x_nums);
                        //println!("any1of {:?}", any1ofx);

                        for inxs in any1ofx.iter() {
                            let mut tmpx = RegionStore::with_capacity(x_nums.len());
                            for (inx, choice) in inxs.iter().enumerate() {
                                tmpx.push(x_regs[inx][*choice].clone());
                            }
                            //println!("tmpx: {tmpx}");
                            RegionStore::vec_push_nosubs_corr(&mut next_stores, tmpx);
                        }

                        // Save intersection.
                        //println!("intx: {intx}");
                        RegionStore::vec_push_nosubs_corr(&mut next_stores, intx);
                    }
                } // next origy
            } // next inx, curx
              // If no splits done, return.
            if split.is_empty() {
                return cur_stores;
            }
            // Copy unsplit stores.
            for (inx, strx) in cur_stores.iter().enumerate() {
                if !split.contains(&inx) {
                    // println!("Adding {strx}");
                    RegionStore::vec_push_nosubs_corr(&mut next_stores, strx.clone());
                }
            }
            //println!("cur_stores:  {}", tools::vec_string(&cur_stores));
            //println!("next_stores: {}", tools::vec_string(&next_stores));
            cur_stores = next_stores;
        } // end loop
    } // end split_to_subsets
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
    use crate::bits::SomeBits;
    use crate::region::SomeRegion;

    #[test]
    fn test_split_to_subsets() -> Result<(), String> {
        let ur_reg1 = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);
        let ur_reg2 = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);

        let mut srs = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::new(
            RegionStore::new(vec![ur_reg1.new_from_string("r0xx1").expect("SNH")]),
            0,
            0,
        );
        srs.push(regstr1);

        let regstr2 = SelectRegions::new(
            RegionStore::new(vec![ur_reg1.new_from_string("r0x1x").expect("SNH")]),
            0,
            0,
        );
        srs.push(regstr2);

        let regstr3 = SelectRegions::new(
            RegionStore::new(vec![ur_reg1.new_from_string("rx1xx").expect("SNH")]),
            0,
            0,
        );
        srs.push(regstr3);

        let subs = srs.split_to_subsets();
        println!("subs {}", tools::vec_string(&subs));
        assert!(subs.len() < 9);

        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![ur_reg1.new_from_string("r0001").expect("SNH"),])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![ur_reg1.new_from_string("r0101").expect("SNH"),])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![ur_reg1.new_from_string("r0011").expect("SNH"),])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![ur_reg1.new_from_string("r0111").expect("SNH"),])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![ur_reg1.new_from_string("r0010").expect("SNH"),])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![ur_reg1.new_from_string("r0110").expect("SNH"),])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![ur_reg1.new_from_string("rx100").expect("SNH"),])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![ur_reg1.new_from_string("r11xx").expect("SNH"),])
        ));

        let mut srs = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::new(
            RegionStore::new(vec![
                ur_reg1.new_from_string("r0xx1").expect("SNH"),
                ur_reg2.new_from_string("rx10x").expect("SNH"),
            ]),
            0,
            0,
        );
        srs.push(regstr1);

        let regstr2 = SelectRegions::new(
            RegionStore::new(vec![
                ur_reg1.new_from_string("r0x1x").expect("SNH"),
                ur_reg2.new_from_string("r0xx1").expect("SNH"),
            ]),
            0,
            0,
        );
        srs.push(regstr2);

        let subs = srs.split_to_subsets();
        println!("subs {}", tools::vec_string(&subs));
        assert!(subs.len() < 6);

        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                ur_reg1.new_from_string("r0x01").expect("SNH"),
                ur_reg2.new_from_string("rx100").expect("SNH"),
            ])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                ur_reg1.new_from_string("r0x01").expect("SNH"),
                ur_reg2.new_from_string("r110x").expect("SNH"),
            ])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                ur_reg1.new_from_string("r0x11").expect("SNH"),
                ur_reg2.new_from_string("r0101").expect("SNH"),
            ])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                ur_reg1.new_from_string("r0x10").expect("SNH"),
                ur_reg2.new_from_string("r0x11").expect("SNH"),
            ])
        ));
        assert!(tools::vec_contains(
            &subs,
            RegionStore::eq_corr,
            &RegionStore::new(vec![
                ur_reg1.new_from_string("r0x10").expect("SNH"),
                ur_reg2.new_from_string("r00x1").expect("SNH"),
            ])
        ));

        //assert!(1 == 2);
        Ok(())
    }
}
