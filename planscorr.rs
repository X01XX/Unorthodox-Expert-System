//! The PlansCorr struct. A store of SomePlan structs,
//! corresponding, in order, to domains in a DomainStore instance.
//!
//! Plans, per domain, will be run in parallel.
//!
//! PlansCorr instances will be linked together for movement between SelectRegions, via intersections.
//! Linking PlansCorr instances requires non-change plans, so a non-change step, for some domains.
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::regionscorr::RegionsCorr;
use crate::statescorr::StatesCorr;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;

use std::fmt;

impl fmt::Display for PlansCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PlansCorr {}", self.plans)
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct PlansCorr {
    /// A vector of plans.
    pub plans: PlanStore,
}

impl PlansCorr {
    /// Return a new, empty, PlansCorr instance.
    pub fn new(plans: Vec<SomePlan>) -> Self {
        debug_assert!(!plans.is_empty());
        Self {
            plans: PlanStore::new(plans),
        }
    }

    /// Return a new, empty, PlansCorr instance, with a given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        debug_assert!(cap > 0);

        Self {
            plans: PlanStore::with_capacity(cap),
        }
    }

    /// Return the number of plans in a PlansCorr.
    pub fn len(&self) -> usize {
        self.plans.len()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.plans.is_empty()
    }

    /// Push a plan into a PlansCorr.
    pub fn push(&mut self, aplan: SomePlan) {
        self.plans.push(aplan);
    }

    /// Return the initial regions of the PlansCorr.
    pub fn initial_regions(&self) -> RegionsCorr {
        let mut ret_regs = RegionsCorr::with_capacity(self.len());

        for plnx in self.plans.iter() {
            ret_regs.push(plnx.initial_region().clone());
        }
        ret_regs
    }

    /// Return the result regions of the PlansCorr.
    pub fn result_regions(&self) -> RegionsCorr {
        let mut ret_regs = RegionsCorr::with_capacity(self.len());

        for plnx in self.plans.iter() {
            ret_regs.push(plnx.result_region().clone());
        }
        ret_regs
    }

    /// Restrict the initial regions of a PlansCorr plans.
    pub fn restrict_initial_regions(&self, regx: &RegionsCorr) -> Option<Self> {
        let mut ret_plans = Self::with_capacity(self.len());

        for (plnx, regx) in self.plans.iter().zip(regx.iter()) {
            if let Some(plny) = plnx.restrict_initial_region(regx) {
                ret_plans.push(plny);
            } else {
                return None;
            }
        }
        Some(ret_plans)
    }

    /// Restrict the result regions of a PlansCorr plans.
    pub fn restrict_result_regions(&self, regx: &RegionsCorr) -> Option<Self> {
        let mut ret_plans = Self::with_capacity(self.len());

        for (plnx, regx) in self.plans.iter().zip(regx.iter()) {
            if let Some(plny) = plnx.restrict_result_region(regx) {
                ret_plans.push(plny);
            } else {
                return None;
            }
        }
        Some(ret_plans)
    }

    /// Return true if two PlansCorrs are linked, result regions to initial regions.
    pub fn is_linked(&self, other: &Self) -> bool {
        self.result_regions() == other.initial_regions()
    }

    /// Return true if a PlansCorr can be linked to another.
    pub fn _can_be_linked(&self, other: &Self) -> bool {
        self.result_regions().intersects(&other.initial_regions())
    }

    /// Return the expected result, given initial regions.
    pub fn result_from_initial_states(&self, states: &StatesCorr) -> Option<StatesCorr> {
        //println!("planscorr::result_from_initial_states self {self} states {states}");
        debug_assert!(self.len() == states.len());
        let mut ret_stas = StatesCorr::with_capacity(states.len());
        for (planx, stax) in self.iter().zip(states.iter()) {
            if planx.initial_region().is_superset_of(stax) {
                ret_stas.push(planx.result_from_initial_state(stax));
            } else {
                return None;
            }
        }
        Some(ret_stas)
    }

    /// Return the expected result, given initial regions.
    pub fn result_from_initial_regions(&self, regions: &RegionsCorr) -> Option<RegionsCorr> {
        let mut ret_regs = RegionsCorr::with_capacity(regions.len());
        for (planx, regx) in self.iter().zip(regions.iter()) {
            if regx.intersects(planx.initial_region()) {
                if let Some(regy) = planx.result_from_initial_region(regx) {
                    ret_regs.push(regy);
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        Some(ret_regs)
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomePlan> {
        self.plans.iter()
    }

    /// Return the number of bits changed by a PlansCorr.
    pub fn num_bits_changed(&self) -> usize {
        let mut numc = 0;

        for plnx in self.plans.iter() {
            numc += plnx.num_bits_changed();
        }
        numc
    }

    /// Return the number of steps to run for a PlansCorr.
    pub fn number_steps_to_run(&self) -> usize {
        let mut numr = 0;
        for plnx in self.iter() {
            numr += plnx.number_steps_to_run();
        }
        numr
    }

    /// Return a range for a PlansCorr.
    /// When plans are run in parallel, the whole range of possible results
    /// must be considered for intersection with negative SelectRegions.
    pub fn plans_range(&self) -> RegionsCorr {
        // Form new RegionsCorr representing the whole range of possible regions,
        // since the PlansCorr will be crun in prallel.
        let mut plans_range = RegionsCorr::with_capacity(self.len());
        for plnx in self.iter() {
            plans_range.push(plnx.range());
        }
        plans_range
    }

    /// Return the number of steps with AltRuleHint::AltRule set.
    /// A step with an AltRule hint is less reliable (50%) than one without.
    pub fn num_altrules(&self) -> isize {
        let mut num_alt = 0;
        for plnx in self.iter() {
            num_alt += plnx.num_altrules();
        }
        num_alt
    }
}

impl Index<usize> for PlansCorr {
    type Output = SomePlan;
    fn index(&self, i: usize) -> &SomePlan {
        &self.plans[i]
    }
}

/// Implement the trait StrLen for PlansCorr.
impl StrLen for PlansCorr {
    fn strlen(&self) -> usize {
        let mut rc_len = 2;

        if self.is_not_empty() {
            rc_len += self.plans.len() * self.plans[0].strlen();
            rc_len += (self.plans.len() - 1) * 2;
        }

        rc_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::region::SomeRegion;
    use crate::rule::SomeRule;
    use crate::step::{AltRuleHint, SomeStep};

    #[test]
    fn new() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("00/X0/10")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn initial_regions() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("00/X0/10")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        let initial_regs = plnsc1.initial_regions();
        println!("initial_regs {initial_regs}");
        assert!(initial_regs[0] == SomeRegion::new_from_string("r0X")?);
        assert!(initial_regs[1] == SomeRegion::new_from_string("r0X1")?);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn result_regions() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("00/X0/10")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        let result_regs = plnsc1.result_regions();
        println!("result_regs {result_regs}");
        assert!(result_regs[0] == SomeRegion::new_from_string("r00")?);
        assert!(result_regs[1] == SomeRegion::new_from_string("r000")?);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn restrict_initial_regions() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("Xx/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("00/X0/Xx")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        let restrict = RegionsCorr::new(vec![
            SomeRegion::new_from_string("r11")?,
            SomeRegion::new_from_string("rX10")?,
        ]);

        if let Some(plnsc2) = plnsc1.restrict_initial_regions(&restrict) {
            println!("plnsc2 {plnsc2}");
            assert!(*plnsc2[0].initial_region() == SomeRegion::new_from_string("r11")?);
            assert!(*plnsc2[1].initial_region() == SomeRegion::new_from_string("r010")?);
        } else {
            return Err("restrict failed?".to_string());
        }

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn restrict_result_regions() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("Xx/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("00/X0/Xx")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        let restrict = RegionsCorr::new(vec![
            SomeRegion::new_from_string("r10")?,
            SomeRegion::new_from_string("rXX1")?,
        ]);

        if let Some(plnsc2) = plnsc1.restrict_result_regions(&restrict) {
            println!("plnsc2 {plnsc2}");
            assert!(*plnsc2[0].result_region() == SomeRegion::new_from_string("r10")?);
            assert!(*plnsc2[1].result_region() == SomeRegion::new_from_string("r001")?);
        } else {
            return Err("restrict failed?".to_string());
        }

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn plans_range() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/10/00/01")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/01/01/11")?,
            AltRuleHint::NoAlt {},
        );
        let stp3 = SomeStep::new(
            1,
            SomeRule::new_from_string("11/00/00/01")?,
            AltRuleHint::NoAlt {},
        );
        let stp4 = SomeStep::new(
            1,
            SomeRule::new_from_string("11/00/01/11")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1, stp2]),
            SomePlan::new(1, vec![stp3, stp4]),
        ]);
        println!("{plnsc1}");

        let rng = plnsc1.plans_range();
        println!("range {rng}");

        assert!(rng.len() == 2);
        assert!(rng[0] == SomeRegion::new_from_string("r0XXX")?);
        assert!(rng[1] == SomeRegion::new_from_string("r10XX")?);

        //assert!(1 == 2);
        Ok(())
    }
}
