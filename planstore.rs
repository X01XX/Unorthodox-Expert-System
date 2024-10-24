//! The PlanStore struct, a vector of SomePlan struct instances.
//!
//! The plans in the PlanStore may be different plans to go from the same region to the same goal region.
//!
//! Or the plans are linked, in that the result region of a plan for a given Domain ID is the same
//! as the initial region for a following plan with the same Domain ID.

use crate::plan::SomePlan;
use crate::regionscorr::RegionsCorr;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::{Iter, IterMut};

impl fmt::Display for PlanStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Debug, Default, Clone, Deserialize, Serialize)]
/// A vector of SomePlan struct instances.
/// When the plans in a planstore are run, each plan will be for a domain,
/// and run in the order they appear in the vector.
/// For some situations, the creator of the PlanStore may split a domain plan and intermingle
/// it with other domain plans.  This is due to the Boolen AND relationship of
/// domain positions in the SelectRegions struct, and some combinations of domain positions
/// have positive, or negative, values. This is like moving different Chess pieces, at different times,
/// to reach a desired position, without a major mess-up inbetween.
pub struct PlanStore {
    /// A vector of SomePlan instances.
    pub items: Vec<SomePlan>,
}

impl PlanStore {
    /// Return a new PlanStore instance.
    /// If more than one plan, plans will be run in order.
    pub fn new(items: Vec<SomePlan>) -> Self {
        Self { items }
    }

    /// Return a new PlanStore instance, with a given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            items: Vec::<SomePlan>::with_capacity(cap),
        }
    }

    /// Return the length of the SomePlan vector.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return true if the store does not contain at least one non-empty plan.
    pub fn is_empty(&self) -> bool {
        if self.items.is_empty() {
            return true;
        }
        for planx in self.items.iter() {
            if planx.is_not_empty() {
                return false;
            }
        }
        true
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Add a plan to the PlanStore.
    pub fn push(&mut self, planx: SomePlan) {
        if !self.items.contains(&planx) {
            self.items.push(planx);
        }
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomePlan> {
        self.items.iter()
    }

    /// Return a reference to the las plan.
    pub fn last(&self) -> Option<&SomePlan> {
        self.items.last()
    }

    /// Return a more restricted display version of a PlanStore.
    pub fn str_terse(&self) -> String {
        let mut rc_str = String::new();

        rc_str.push('(');

        for (inx, planx) in self.items.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&planx.str_terse());
        }
        rc_str.push(')');

        rc_str
    }

    /// Return the number of steps in the plans of the PlanStore.
    pub fn number_steps(&self) -> usize {
        self.items.iter().map(|planx| planx.len()).sum()
    }

    /// Return a String representation of a PlanStore.
    fn formatted_string(&self) -> String {
        let mut flg = 0;

        let mut rc_str = String::with_capacity(self.strlen());

        rc_str.push('[');
        for planx in &self.items {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&planx.to_string());
            flg = 1;
        }
        rc_str.push(']');

        rc_str
    }

    /// Extend a StepStore by pushing another StepStore.
    pub fn append(&mut self, other: Self) {
        for planx in other.items {
            self.push(planx);
        }
    }

    /// Return number bits changed running plans in store.
    pub fn num_bits_changed(&self) -> usize {
        self.items
            .iter()
            .map(|planx| planx.num_bits_changed())
            .sum()
    }

    /// Return the result regions of a Planstore.
    /// The current regions should intersect the plan initial regions,
    /// and will be a default region in case a PlanStore does not have a plan for
    /// a domain.
    pub fn result_regions(&self, default: &RegionsCorr) -> RegionsCorr {
        let mut ret_regs = default.clone();

        for planx in self.items.iter() {
            if planx.is_empty() {
                continue;
            }
            ret_regs[planx.dom_id] = planx.result_region().clone();
        }
        ret_regs
    }

    /// Return true if a PlanStore remains within a given RegionsCorr.
    pub fn remains_within(&self, within: &RegionsCorr) -> bool {
        let mut cur_regs = self.initial_regions(within);

        if !within.is_superset_of(&cur_regs) {
            return false;
        }

        for planx in self.items.iter() {
            for stepx in planx.iter() {
                cur_regs[planx.dom_id] = stepx.result.clone();
                if !within.is_superset_of(&cur_regs) {
                    return false;
                }
            }
        }
        true
    }

    /// Return the initial_regions of a Planstore.
    /// The current regions should intersect the plan initial regions,
    /// and will be a default region in case a PlanStore does not have a plan for
    /// a domain.
    pub fn initial_regions(&self, default: &RegionsCorr) -> RegionsCorr {
        let mut ret_regs = default.clone();
        for planx in self.items.iter() {
            if planx.is_empty() {
                continue;
            }
            ret_regs[planx.dom_id] = planx.initial_region().clone();
        }
        ret_regs
    }

    /// Validate a PlanStore, given start and goal regions.
    pub fn validate(&self, start_regs: &RegionsCorr, goal_regs: &RegionsCorr) -> bool {
        let mut cur_regs = start_regs.clone();
        for planx in self.items.iter() {
            if planx.is_empty() {
                continue;
            }
            let dom_id = planx.dom_id;
            for stepx in planx.iter() {
                if stepx.initial.intersects(&cur_regs[dom_id]) {
                    cur_regs[dom_id] = stepx.rule.result_from_initial_region(&cur_regs[dom_id]);
                } else {
                    println!("Validate (1)");
                    println!("Plans: {self}");
                    println!("start    {start_regs}");
                    println!("goal     {goal_regs}");
                    println!("cur_regs {cur_regs}");
                    println!("sub plan {planx}");
                    return false;
                }
            }
        }
        if goal_regs.is_superset_of(&cur_regs) {
            true
        } else {
            println!("Validate (2)");
            println!("Plans: {self}");
            println!("start    {start_regs}");
            println!("goal     {goal_regs}");
            println!("cur_regs {cur_regs}");
            false
        }
    }

    /// Return a PlanStore with duplicates deleted.
    pub fn delete_duplicates(&self) -> Self {
        let mut ret_store = PlanStore::new(vec![]);
        for planx in self.iter() {
            ret_store.push(planx.clone()); // only adds non-duplicates.
        }
        ret_store
    }

    /// Return true if a PlanStore contains a plan.
    pub fn contains(&self, planx: &SomePlan) -> bool {
        self.items.contains(planx)
    }

    /// Return a mutable iterator
    pub fn iter_mut(&mut self) -> IterMut<SomePlan> {
        self.items.iter_mut()
    }

    /// Remove a plan from a PlanStore
    pub fn remove(&mut self, inx: usize) -> SomePlan {
        assert!(inx < self.items.len(), "Index out of bounds");

        let last_inx = self.items.len() - 1;

        if inx == last_inx {
            return self.items.pop().unwrap();
        }

        self.items.swap(inx, last_inx);
        self.items.pop().unwrap()
    }
} // end impl PlanStore

impl Index<usize> for PlanStore {
    type Output = SomePlan;
    fn index(&self, i: usize) -> &SomePlan {
        &self.items[i]
    }
}

/// Implement the trait StrLen for SomePlan.
impl StrLen for PlanStore {
    fn strlen(&self) -> usize {
        let mut cnt = 2; // brackets.
        for (inx, planx) in self.items.iter().enumerate() {
            if inx > 0 {
                cnt += 3; // comma newline space
            }
            cnt += planx.strlen();
        }
        cnt
    }
}

impl IntoIterator for PlanStore {
    type Item = SomePlan;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::region::SomeRegion;
    use crate::rule::SomeRule;
    use crate::sample::SomeSample;
    use crate::step::{AltRuleHint, SomeStep};

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_rul = SomeRule::new(&SomeSample::new_from_string("0b0000_0000->0b0000_0000")?); //(tmp_sta.clone(), tmp_sta.clone()));
        let tmp_stp = SomeStep::new(0, tmp_rul, AltRuleHint::NoAlt {});

        let tmp_pln = SomePlan::new(0, vec![tmp_stp.clone()]);

        let mut plnstr = PlanStore::new(vec![tmp_pln]);
        let fstr = plnstr.formatted_string();
        let sb = plnstr.strlen();
        println!("{}", plnstr);
        if fstr.len() != sb {
            return Err(format!("str {} NE calced {}", fstr.len(), sb));
        }

        plnstr.push(SomePlan::new(0, vec![tmp_stp.clone()]));
        let fstr = plnstr.formatted_string();
        let sb = plnstr.strlen();
        println!("{}", plnstr);
        if fstr.len() != sb {
            return Err(format!("str {} NE calced {}", fstr.len(), sb));
        }

        Ok(())
    }

    #[test]
    fn delete_duplicates() -> Result<(), String> {
        let reg_b = SomeRegion::new_from_string("r1011")?;
        let reg_e = SomeRegion::new_from_string("r0110")?;
        let reg_f = SomeRegion::new_from_string("r1111")?;

        let step1 = SomeStep::new(
            0,
            SomeRule::new_region_to_region(&reg_b, &reg_f),
            AltRuleHint::NoAlt {},
        );

        let step2 = SomeStep::new(
            1,
            SomeRule::new_region_to_region(&reg_f, &reg_e),
            AltRuleHint::NoAlt {},
        );

        let pln1 = SomePlan::new(0, vec![step1, step2]);

        let step3 = SomeStep::new(
            0,
            SomeRule::new_region_to_region(&reg_b, &reg_f),
            AltRuleHint::NoAlt {},
        );

        let step4 = SomeStep::new(
            1,
            SomeRule::new_region_to_region(&reg_f, &reg_e),
            AltRuleHint::NoAlt {},
        );

        let pln2 = SomePlan::new(0, vec![step3, step4]);

        let plnstr1 = PlanStore::new(vec![pln1, pln2]);
        println!("plnstr1 {plnstr1}");

        let plnstr2 = plnstr1.delete_duplicates();
        println!("plnstr2 {plnstr2}");

        assert!(plnstr2.len() == 1);

        Ok(())
    }
}
