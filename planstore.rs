//! The PlanStore struct, a vector of SomePlan struct instances.

use crate::plan::SomePlan;
use crate::region::SomeRegion;
use crate::regionstorecorr::RegionStoreCorr;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

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
    pub avec: Vec<SomePlan>,
}

impl PlanStore {
    /// Return a new PlanStore instance.
    /// If more than one plan, plans will be run in order.
    pub fn new(avec: Vec<SomePlan>) -> Self {
        Self { avec }
    }

    /// Return the length of the SomePlan vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store does not contain at least one non-empty plan.
    pub fn is_empty(&self) -> bool {
        if self.avec.is_empty() {
            return true;
        }
        for planx in self.avec.iter() {
            if planx.is_not_empty() {
                return false;
            }
        }
        true
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Return the index of the last plan with a given domain number.
    fn last_dom(&self, dom_id: usize) -> Option<usize> {
        let mut ret: Option<usize> = None;
        for (inx, planx) in self.avec.iter().enumerate() {
            if planx.dom_id == dom_id {
                ret = Some(inx);
            }
        }
        ret
    }

    /// Add a plan to the vector.
    /// The plan should be linkable with a previously existing plan.
    pub fn push(&mut self, planx: SomePlan) {
        if planx.is_empty() {
            return;
        }
        //println!("planstore:push for {} push {}", self, planx);

        // Check if successive plans of the same domain can be combined.
        if !self.is_empty() {
            let inx = self.len() - 1;
            if self.avec[inx].dom_id == planx.dom_id {
                if let Some(plany) = self.avec[inx].link(&planx) {
                    self.avec[inx] = plany;
                    return;
                } else {
                    panic!("plan {} not linked to {}", self.avec[inx], planx);
                }
            }
        }

        // Verify a domain plan that is split into parts.
        if let Some(inx) = self.last_dom(planx.dom_id) {
            if self[inx].result_region() == planx.initial_region() {
            } else if self[inx].result_region().intersects(planx.initial_region()) {
                self.avec.push(
                    planx
                        .restrict_initial_region(self[inx].result_region())
                        .unwrap(),
                );
                return;
            }
            //println!("checking {} and {}", self[inx].result_region(), planx.initial_region());
            assert!(self[inx].result_region() == planx.initial_region());
        }

        self.avec.push(planx);
        //println!("new planstore {self}");
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomePlan> {
        self.avec.iter()
    }

    /// Return a reference to the las plan.
    pub fn last(&self) -> Option<&SomePlan> {
        self.avec.last()
    }

    /// Return a more restricted display version of a PlanStore.
    pub fn str_terse(&self) -> String {
        let mut rc_str = String::new();

        rc_str.push('(');

        for (inx, planx) in self.avec.iter().enumerate() {
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
        let mut ret = 0;
        for planx in &self.avec {
            ret += planx.len();
        }
        ret
    }

    /// Return a String representation of a PlanStore.
    fn formatted_string(&self) -> String {
        let mut flg = 0;

        let mut rc_str = String::with_capacity(self.strlen());

        rc_str.push_str("\n(");

        for planx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n ");
            }
            rc_str.push_str(&planx.to_string());
            flg = 1;
        }
        rc_str.push(')');

        rc_str
    }

    /// Extend a StepStore by pushing another StepStore.
    pub fn append(&mut self, other: Self) {
        for planx in other.avec {
            self.push(planx);
        }
    }

    /// Return the result of the last plan for a domain, if any.
    pub fn domain_result(&self, dom_id: usize) -> Option<&SomeRegion> {
        let mut rslt: Option<&SomeRegion> = None;

        for planx in self.iter() {
            if planx.dom_id == dom_id && planx.is_not_empty() {
                rslt = Some(planx.result_region());
            }
        }
        rslt
    }

    /// Return number bits changed running plans in store.
    pub fn num_bits_changed(&self) -> usize {
        let mut ret_num = 0;
        for planx in self.avec.iter() {
            if planx.is_empty() {
                continue;
            }
            ret_num += planx.num_bits_changed();
        }
        ret_num
    }

    /// Return the result regions of a Planstore.
    /// The current regions should intersect the plan initial regions,
    /// and will be a default region in case a PlanStore does not have a plan for
    /// a domain.
    pub fn result_regions(&self, default: &RegionStoreCorr) -> RegionStoreCorr {
        let mut ret_regs = default.clone();
        for planx in self.avec.iter() {
            if planx.is_empty() {
                continue;
            }
            ret_regs[planx.dom_id] = planx.result_region().clone();
        }
        ret_regs
    }

    /// Return the initial_regions of a Planstore.
    /// The current regions should intersect the plan initial regions,
    /// and will be a default region in case a PlanStore does not have a plan for
    /// a domain.
    pub fn initial_regions(&self, default: &RegionStoreCorr) -> RegionStoreCorr {
        let mut ret_regs = default.clone();
        for planx in self.avec.iter() {
            if planx.is_empty() {
                continue;
            }
            ret_regs[planx.dom_id] = planx.initial_region().clone();
        }
        ret_regs
    }

    /// Validate a PlanStore, given start and goal regions.
    pub fn validate(&self, start_regs: &RegionStoreCorr, goal_regs: &RegionStoreCorr) -> bool {
        let mut cur_regs = start_regs.clone();
        for planx in self.avec.iter() {
            if planx.is_empty() {
                continue;
            }
            let dom_id = planx.dom_id;
            for stepx in planx.iter() {
                if stepx.initial.intersects(&cur_regs[dom_id]) {
                    cur_regs[dom_id] = stepx.rule.result_from(&cur_regs[dom_id]);
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

    /// Link two Planstores.
    pub fn link(&self, other: &Self) -> Option<Self> {
        let mut ret_plans = self.clone();

        for planx in other.avec.iter() {
            if planx.is_empty() {
                continue;
            }
            if let Some(regx) = ret_plans.domain_result(planx.dom_id) {
                if let Some(plany) = planx.restrict_initial_region(regx) {
                    ret_plans.push(plany);
                } else {
                    return None;
                }
            } else {
                ret_plans.push(planx.clone());
            }
        }

        Some(ret_plans)
    }
} // end impl PlanStore

impl Index<usize> for PlanStore {
    type Output = SomePlan;
    fn index(&self, i: usize) -> &SomePlan {
        &self.avec[i]
    }
}

/// Implement the trait StrLen for SomePlan.
impl StrLen for PlanStore {
    fn strlen(&self) -> usize {
        let mut cnt = 2; // parens
        for (inx, planx) in self.avec.iter().enumerate() {
            if inx == 0 {
                cnt += 1; // newline
            } else {
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
        self.avec.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::rule::SomeRule;
    use crate::sample::SomeSample;
    use crate::state::SomeState;
    use crate::step::{AltRuleHint, SomeStep};

    #[test]
    fn test_strlen() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));
        let tmp_stp = SomeStep::new(0, tmp_rul, AltRuleHint::NoAlt {}, 0);

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
}
