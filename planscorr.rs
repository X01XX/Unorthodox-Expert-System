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
use crate::stepscorr::StepsCorr;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;
use std::str::FromStr;

use std::fmt;

impl fmt::Display for PlansCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.rate == 0 {
            write!(f, "PC[{}]", self.plans)
        } else {
            write!(f, "PC[{}, {}]", self.plans, self.rate)
        }
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct PlansCorr {
    /// A vector of plans.
    pub plans: PlanStore,
    /// Value assigned based on the largest negative SRs needed to find a plan.
    pub rate: isize,
}

impl PlansCorr {
    /// Return a new, empty, PlansCorr instance, with a given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        debug_assert!(cap > 0);

        Self {
            plans: PlanStore::with_capacity(cap),
            rate: 0,
        }
    }

    /// Return a new instance from a StepsCorr instance.
    pub fn new_from_stepscorr(stepsc: &StepsCorr) -> Self {
        let mut ret = Self::with_capacity(stepsc.len());

        for (inx, stpx) in stepsc.iter().enumerate() {
            ret.push(SomePlan::new(inx, vec![stpx.clone()]));
        }
        ret
    }

    /// Set the value of the planscorr.
    pub fn set_rate(&mut self, rate: isize) {
        self.rate = rate;
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
        debug_assert!(aplan.dom_id == self.plans.len());

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

    /// Return a vector of corresponding num_bits.
    pub fn num_bits_vec(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::with_capacity(self.len());
        for plnx in self.plans.iter() {
            ret_vec.push(plnx.num_bits().expect("SNH"));
        }
        ret_vec
    }

    /// Restrict the initial regions of a PlansCorr plans.
    pub fn restrict_initial_regions(&self, regx: &RegionsCorr) -> Option<Self> {
        debug_assert!(self.is_congruent(regx));

        let mut ret_plans = Self::with_capacity(self.len());

        for (plnx, regx) in self.plans.iter().zip(regx.iter()) {
            //println!("plan {plnx} reg  {regx}");
            if let Some(plny) = plnx.restrict_initial_region(regx) {
                ret_plans.push(plny);
            } else {
                return None;
            }
        }
        ret_plans.set_rate(self.rate);
        Some(ret_plans)
    }

    /// Restrict the result regions of a PlansCorr plans.
    pub fn restrict_result_regions(&self, regx: &RegionsCorr) -> Option<Self> {
        debug_assert!(self.is_congruent(regx));

        let mut ret_plans = Self::with_capacity(self.len());

        for (plnx, regx) in self.plans.iter().zip(regx.iter()) {
            if let Some(plny) = plnx.restrict_result_region(regx) {
                ret_plans.push(plny);
            } else {
                return None;
            }
        }
        ret_plans.set_rate(self.rate);
        Some(ret_plans)
    }

    /// Return true if two PlansCorrs are linked, result regions to initial regions.
    pub fn is_linked(&self, other: &Self) -> bool {
        debug_assert!(self.is_congruent(other));

        self.result_regions() == other.initial_regions()
    }

    /// Return the expected result, given initial regions.
    pub fn result_from_initial_states(&self, states: &StatesCorr) -> Option<StatesCorr> {
        //println!("planscorr::result_from_initial_states self {self} states {states}");
        debug_assert!(self.is_congruent(states));

        let mut ret_stas = StatesCorr::with_capacity(states.len());
        for (planx, stax) in self.iter().zip(states.iter()) {
            if planx.initial_region().is_superset_of(stax) {
                if let Some(stay) = planx.result_from_initial_state(stax) {
                    ret_stas.push(stay);
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        Some(ret_stas)
    }

    /// Return the expected result, given initial regions.
    pub fn result_from_initial_regions(&self, regions: &RegionsCorr) -> Option<RegionsCorr> {
        debug_assert!(self.is_congruent(regions));

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

    /// Return the number of steps with AltRuleHint::AltRule set.
    /// A step with an AltRule hint is less reliable (50%) than one without.
    pub fn num_altrules(&self) -> isize {
        let mut num_alt = 0;
        for plnx in self.iter() {
            num_alt += plnx.num_altrules();
        }
        num_alt
    }

    /// Return true if corresponding regions in two vectors have the same number of bits.
    pub fn is_congruent(&self, other: &impl tools::CorrespondingItems) -> bool {
        self.num_bits_vec() == other.num_bits_vec()
    }

    /// Return true if PlansCorr result regions intersect a given RegionsCorr.
    pub fn result_regions_intersect(&self, rcx: &RegionsCorr) -> bool {
        for (plnx, regx) in self.iter().zip(rcx.iter()) {
            if plnx.result_region().intersects(regx) {
            } else {
                return false;
            }
        }
        true
    }

    /// Return true if PlansCorr initial regions intersect a given RegionsCorr.
    pub fn initial_regions_intersect(&self, rcx: &RegionsCorr) -> bool {
        for (plnx, regx) in self.iter().zip(rcx.iter()) {
            if plnx.initial_region().intersects(regx) {
            } else {
                return false;
            }
        }
        true
    }

    /// Return a string representation in the from  (<-) format.
    pub fn formatted_str_from(&self) -> String {
        if self.rate == 0 {
            format!("PC[{}]", self.plans.formatted_str_from())
        } else {
            format!("PC[{}, {}]", self.plans.formatted_str_from(), self.rate)
        }
    }
}

impl Index<usize> for PlansCorr {
    type Output = SomePlan;
    fn index(&self, i: usize) -> &SomePlan {
        &self.plans[i]
    }
}

/// Implement the trait StrLen for PlansCorr.
impl tools::StrLen for PlansCorr {
    fn strlen(&self) -> usize {
        let mut rtlen = 4; // PC[]
        rtlen += self.plans.strlen();

        if self.rate != 0 {
            rtlen += format!("{}", self.rate).len() + 2;
        };

        rtlen
    }
}

impl tools::CorrespondingItems for PlansCorr {
    fn num_bits_vec(&self) -> Vec<usize> {
        self.num_bits_vec()
    }
}

impl FromStr for PlansCorr {
    type Err = String;
    /// Return a PlansCorr instance, given a string representation.
    /// Like PC[], PC[[P[r001-0>r101]], -1] or PC[[P[r001-0>r101], P[r101-0>r101]], 0].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("planscorr::from_str: {str_in}");
        let str_in2 = str_in.trim();

        // Strip off surrounding id and brackets.
        if str_in2.len() < 4 {
            return Err("planscorr::from_str: string should be at least = PC[]".to_string());
        }

        if str_in2[0..3] != *"PC[" {
            return Err("planscorr::from_str: string should begin with PC[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("planscorr::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[3..(str_in2.len() - 1)];

        // Split string into PlanStore tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("planscorr::from_str: {errstr}")),
        };
        //println!("tokens {:?}", tokens);

        // Process tokens.
        let mut val = 0;
        let mut plans = PlanStore::new(vec![]);
        for tokx in tokens.iter() {
            if tokx[0..1] == *"[" {
                match PlanStore::from_str(tokx) {
                    Ok(plansx) => plans = plansx,
                    Err(errstr) => return Err(format!("planscorr::from_str: {errstr}")),
                }
            } else {
                match tokx.parse::<isize>() {
                    Ok(aval) => val = aval,
                    Err(errstr) => return Err(format!("planscorr::from_str: {errstr}")),
                }
            }
        }

        Ok(Self { plans, rate: val })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::region::SomeRegion;
    use tools::StrLen;

    #[test]
    fn strlen() -> Result<(), String> {
        let plncr1 = PlansCorr::from_str("PC[[], 0]")?;
        let lenx = plncr1.strlen();
        let strx = format!("{plncr1}");
        println!("plncr1 {plncr1} len {} calced len {lenx}", strx.len());
        assert!(lenx == strx.len());

        let plncr2 = PlansCorr::from_str("PC[[P[0, r0000-0->r1111]], 0]")?;
        let lenx = plncr2.strlen();
        let strx = format!("{plncr2}");
        println!("plncr2 {plncr2} len {} calced len {lenx}", strx.len());
        assert!(lenx == strx.len());

        let plncr3 = PlansCorr::from_str("PC[[P[0, r0000-0->r1111], P[1, r0000-0->r1100]], -1]")?;
        let lenx = plncr3.strlen();
        let strx = format!("{plncr3}");
        println!("plncr3 {plncr3} len {} calced len {lenx}", strx.len());
        assert!(lenx == strx.len());

        Ok(())
    }

    #[test]
    fn initial_regions() -> Result<(), String> {
        let plnsc1 = PlansCorr::from_str("PC[[P[0, r0X-0->r00], P[1, r0X1-1->r000]]]")?;
        println!("{plnsc1}");

        let initial_regs = plnsc1.initial_regions();
        println!("initial_regs {initial_regs}");
        assert!(initial_regs == RegionsCorr::from_str("RC[r0X, r0X1]")?);

        Ok(())
    }

    #[test]
    fn result_regions() -> Result<(), String> {
        let plnsc1 =
            PlansCorr::from_str("PC[[P[0, r0X-0->r00-1->r11], P[1, r0X1-1->r000-4->r101]]]")?;
        println!("{plnsc1}");

        let result_regs = plnsc1.result_regions();
        println!("result_regs {result_regs}");
        assert!(result_regs == RegionsCorr::from_str("RC[r11, r101]")?);

        Ok(())
    }

    #[test]
    fn restrict_initial_regions() -> Result<(), String> {
        let plnsc1 =
            PlansCorr::from_str("PC[[P[0, rXX-0->rx0], P[1, r0X0x-1->r0x11-2->r1X11]], 0]")?;
        println!("{plnsc1}");

        let restrict = RegionsCorr::from_str("RC[r11, r0100]")?;
        println!("restrict to {restrict}");

        if let Some(plnsc2) = plnsc1.restrict_initial_regions(&restrict) {
            println!("plnsc2 {plnsc2}");
            assert!(*plnsc2[0].result_region() == SomeRegion::from_str("r10")?);
            assert!(*plnsc2[1].result_region() == SomeRegion::from_str("r1111")?);
        } else {
            return Err("restrict failed?".to_string());
        }
        Ok(())
    }

    #[test]
    fn restrict_result_regions() -> Result<(), String> {
        let plnsc1 =
            PlansCorr::from_str("PC[[P[0, rXX-0->rx0], P[1, r0X0x-1->r0x11-2->1X11]], 0]")?;
        println!("{plnsc1}");

        let restrict = RegionsCorr::from_str("RC[r00, r1111]")?;
        println!("restrict to {restrict}");

        if let Some(plnsc2) = plnsc1.restrict_result_regions(&restrict) {
            println!("plnsc2 {plnsc2}");
            assert!(*plnsc2[0].initial_region() == SomeRegion::from_str("r0X")?);
            assert!(*plnsc2[1].initial_region() == SomeRegion::from_str("r010X")?);
        } else {
            return Err("restrict failed?".to_string());
        }
        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let plncr1_str = "PC[]";
        let plncr1 = PlansCorr::from_str(&plncr1_str)?;
        println!("plncr1 {plncr1}");
        assert!(format!("{plncr1}") == "PC[[]]");

        let plncr2_str = "PC[[P[0, r0000-0->r1111]], 1]";
        let plncr2 = PlansCorr::from_str(&plncr2_str)?;
        println!("plncr2 {plncr2}");
        assert!(format!("{plncr2}") == plncr2_str);

        let plncr3_str = "PC[[P[0, r0000-0->r1111], P[1, r0000-0->r1100]], -1]";
        let plncr3 = PlansCorr::from_str(&plncr3_str)?;
        println!("plncr3 {plncr3}");
        assert!(format!("{plncr3}") == plncr3_str);

        //assert!(1 == 2);
        Ok(())
    }
}
