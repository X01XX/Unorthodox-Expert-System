//! The Plan struct.
//!
//! A plan with zero, or more, steps, in a given domain.
//!
//! A Plan uses a StepStore, but enforces relatedness,
//! where the result of one step is equal to the initial region
//! of the next step.
//!
//! A finished plan can be considered to be a "forward chaining" plan from
//! a given region (often a state, or a region with no X-bit positions) to
//! an end-region.
//!
//! This is often a "pre-positioning", to change the current state to a state where a sample
//! is needed.  The final sample taken is not part of the plan, at least so far.
//!
//! An empty plan means that the domain current state satisfies the need the plan is for, just
//! sample the current state.

use crate::region::SomeRegion;
use crate::state::SomeState;
use crate::step::{AltRuleHint, SomeStep};
use crate::stepstore::StepStore;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;

use std::fmt;

impl fmt::Display for SomePlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

/// Implement the PartialEq trait.
impl PartialEq for SomePlan {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        if self.dom_id != other.dom_id {
            return false;
        }
        if self.is_empty() {
            return true;
        }
        if self[0].initial != other[0].initial {
            return false;
        }
        for (stpx, stpy) in self.iter().zip(other.iter()) {
            if stpx != stpy {
                return false;
            }
        }
        true
    }
}
impl Eq for SomePlan {}

#[readonly::make]
#[derive(Debug, Clone, Deserialize, Serialize)]
/// A struct containing a domain id and steps.
pub struct SomePlan {
    /// Domain indicator.
    pub dom_id: usize,
    /// A StepStore instance.
    pub steps: StepStore, // Do some steps
    pub shortcut: bool,
}

impl SomePlan {
    /// Return a new, empty, plan.
    pub fn new(dom_id: usize, stepvec: Vec<SomeStep>) -> Self {
        if stepvec.len() > 1 {
            let last_inx = stepvec.len() - 1;
            let mut initial_regs = vec![];
            for (inx, stepx) in stepvec.iter().enumerate() {
                assert!(!initial_regs.contains(&stepx.result));
                if inx < last_inx {
                    assert!(stepx.result == stepvec[inx + 1].initial);
                }
                initial_regs.push(stepx.initial.clone());
            }
        }

        Self {
            dom_id,
            steps: StepStore::new(stepvec),
            shortcut: false,
        }
    }

    /// Set shortcut flag.
    pub fn set_shortcut(&mut self) {
        self.shortcut = true;
    }

    /// Return a plan after restricting the result region.
    pub fn restrict_result_region(&self, regx: &SomeRegion) -> Option<Self> {
        if self.is_empty() || !regx.intersects(self.result_region()) {
            return None;
        }

        let mut steps = StepStore::with_capacity(self.len());

        let mut cur_reg = regx.clone();

        for inx in (0..self.len()).rev() {
            let stpx = &self.steps[inx];

            if !cur_reg.intersects(&stpx.result) {
                return None;
            }

            let stpy = stpx.restrict_result_region(&cur_reg);

            cur_reg = stpy.initial.clone();

            steps.push(stpy);
        } //next inx

        if steps.len() > 1 {
            steps.reverse_order();
        }

        Some(Self {
            dom_id: self.dom_id,
            steps,
            shortcut: false,
        })
    }

    /// Return a plan after restricting its initial region.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Option<Self> {
        if self.is_empty() || !regx.intersects(self.initial_region()) {
            return None;
        }

        let mut steps = StepStore::with_capacity(self.len());

        let mut cur_reg = regx.clone();

        for stepx in self.iter() {
            if !cur_reg.intersects(&stepx.initial) {
                return None;
            }

            let stepy = stepx.restrict_initial_region(&cur_reg);

            cur_reg = stepy.result.clone();

            steps.push(stepy);
        } //next stepx

        Some(Self {
            dom_id: self.dom_id,
            steps,
            shortcut: false,
        })
    }

    /// Add a step to a SomePlan.
    fn push(&mut self, stepx: SomeStep) -> Result<(), String> {
        if self.is_not_empty() {
            if self.result_region() != &stepx.initial {
                return Err(format!("plan {self} does not intersect step {stepx}"));
            }
            if self.any_initial_intersects(&stepx.result) {
                return Err(format!("plan {self} step {stepx} circles back"));
            }
        }

        self.steps.push(stepx);
        Ok(())
    }

    /// Return the result of linking two plans together, that are known to have a result/initial intersection.
    pub fn link(&self, other: &Self) -> Option<Self> {
        //println!("link: {self} and {other}");
        debug_assert!(self.dom_id == other.dom_id);

        // Sanity checks
        if self.is_empty() && other.is_empty() {
            return None;
        }

        if self.is_empty() && other.is_not_empty() {
            return Some(other.clone());
        }

        if self.is_not_empty() && other.is_empty() {
            return Some(self.clone());
        }

        // Restrict the StepStores, forward and backward.
        let reg1 = self.result_region();
        let reg2 = other.initial_region();

        let regx = reg1.intersection(reg2)?;

        let mut steps1 = self.restrict_result_region(&regx)?;
        //println!("steps1 {steps1}");

        let steps2 = other.restrict_initial_region(&regx)?;
        //println!("steps2 {steps2}");

        // Push each step to check each step for problems.
        for stepx in steps2.steps.into_iter() {
            match steps1.push(stepx) {
                Ok(()) => continue,
                Err(_errstr) => {
                    //println!("    push failed {errstr}");
                    return None;
                }
            }
        }

        //println!("stepstore:link: 2 {} and {} giving {}", self, other, rc_steps);
        Some(steps1)
    } // end link

    /// Return a string of action numbers to represent a plan.
    pub fn str_terse(&self) -> String {
        let mut rs = format!("P:{}[", self.dom_id);

        let mut flg = 0;
        for stpx in self.steps.iter() {
            if flg == 0 {
                flg = 1;
            } else {
                rs.push(',');
            }
            if let Some(act_id) = stpx.act_id {
                rs.push_str(&format!("{}", act_id));
            } else {
                rs.push_str("no");
            }
        }
        rs.push(']');
        if self.shortcut {
            rs.push('s');
        }
        rs
    }

    /// Return the number of steps in a plan.
    pub fn len(&self) -> usize {
        self.steps.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.steps.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.steps.is_empty()
    }

    /// Return a step iterator.
    pub fn iter(&self) -> Iter<SomeStep> {
        self.steps.iter()
    }

    /// Return the initial region of a plan that contains at least one step.
    pub fn initial_region(&self) -> &SomeRegion {
        assert!(self.is_not_empty());
        &self.steps[0].initial
    }

    /// Return the result region of a plan that contains at least one step.
    pub fn result_region(&self) -> &SomeRegion {
        assert!(self.is_not_empty());
        &self[self.len() - 1].result
    }

    /// Return the result state of a plan that contains at least one step.
    pub fn result_from_initial_state(&self, astate: &SomeState) -> SomeState {
        assert!(self.is_not_empty());
        let mut ret_state = astate.clone();
        for stepx in self.iter() {
            if stepx.initial.is_superset_of(&ret_state) {
                ret_state = stepx.rule.result_from_initial_state(&ret_state);
            } else {
                panic!("plan problem! plan {self} step {stepx} state {ret_state}");
            }
        }
        ret_state
    }

    /// Return the result state of a plan.
    pub fn result_from_initial_region(&self, regx: &SomeRegion) -> Option<SomeRegion> {
        if self.is_empty() || !self.initial_region().intersects(regx) {
            return None;
        }
        let mut cur_reg = regx.clone();
        for stepx in self.iter() {
            if stepx.initial.intersects(&cur_reg) {
                cur_reg = stepx.rule.result_from_initial_region(&cur_reg);
            } else {
                return None;
            }
        }
        Some(cur_reg)
    }

    /// Return a String representation of SomePlan.
    fn formatted_string(&self) -> String {
        if self.shortcut {
            format!("{}s P:{}", self.steps, self.dom_id)
        } else {
            format!("{} P:{}", self.steps, self.dom_id)
        }
    }

    /// Return the number of bits changed through each step of a plan.
    pub fn num_bits_changed(&self) -> usize {
        let mut ret_num = 0;
        for stepx in self.iter() {
            ret_num += stepx.num_bits_changed();
        }
        ret_num
    }

    /// Return true if a plan contains an initial region.
    fn any_initial_intersects(&self, regx: &SomeRegion) -> bool {
        for stepx in self.iter() {
            if stepx.initial.intersects(regx) {
                return true;
            }
        }
        false
    }

    /// Return the number of bits used in plan steps, for argument checking.
    pub fn num_bits(&self) -> Option<usize> {
        if self.is_empty() {
            return None;
        }
        Some(self[0].num_bits())
    }

    /// Return true if a Plan remains within a given Region.
    pub fn remains_within(&self, within: &SomeRegion) -> bool {
        if !within.is_superset_of(self.initial_region()) {
            return false;
        }

        for stepx in self.iter() {
            if !within.is_superset_of(&stepx.result) {
                return false;
            }
        }
        true
    }

    /// Return the number of steps to run for a Plan.
    pub fn number_steps_to_run(&self) -> usize {
        let mut numr = 0;
        for stepx in self.iter() {
            if stepx.act_id.is_some() {
                numr += 1;
            }
        }
        numr
    }

    /// Return true if a plan causes change.
    pub fn causes_change(&self) -> bool {
        if self.is_empty() {
            return false;
        }
        if self.len() > 1 {
            return true;
        }
        self[0].act_id.is_some()
    }

    /// Return the aggregate region of a plan path.
    pub fn path_for(&self, regx: &SomeRegion) -> SomeRegion {
        debug_assert!(self.initial_region().intersects(regx));

        let mut cur_reg = regx.clone();
        let mut agg_reg = regx.clone();

        for stepx in self.iter() {
            debug_assert!(stepx.initial.intersects(&cur_reg));
            cur_reg = stepx.rule.result_from_initial_region(&cur_reg);
            agg_reg = agg_reg.union(&cur_reg);
        }
        agg_reg
    }

    /// Return the number of steps with AltRuleHint::AltRule set.
    pub fn num_altrules(&self) -> isize {
        let mut num_alt = 0;
        for stepx in self.iter() {
            num_alt += match stepx.alt_rule {
                AltRuleHint::AltRule { .. } => 1,
                _ => 0,
            }
        }
        num_alt
    }

    /// Return the range traversed by a plan.
    pub fn range(&self) -> SomeRegion {
        let mut rng = self.initial_region().clone();
        for stepx in self.iter() {
            rng = rng.union(&stepx.result);
        }
        rng
    }
} // end impl SomePlan

impl Index<usize> for SomePlan {
    type Output = SomeStep;
    fn index(&self, i: usize) -> &SomeStep {
        &self.steps[i]
    }
}

/// Implement the trait StrLen for SomePlan.
impl StrLen for SomePlan {
    fn strlen(&self) -> usize {
        4 + self.steps.strlen() + if self.shortcut { 1 } else { 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rule::SomeRule;
    use crate::sample::SomeSample;
    use crate::step::AltRuleHint;

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_rul = SomeRule::new(&SomeSample::new_from_string("0b0000->0b1111")?); //(tmp_sta.clone(), tmp_stb.clone()));
        let tmp_stp = SomeStep::new(0, tmp_rul, AltRuleHint::NoAlt {});

        let tmp_pln = SomePlan::new(0, vec![tmp_stp.clone()]);
        println!("tmp_pln {tmp_pln}");

        let strrep = format!("{tmp_pln}");
        let len = strrep.len();
        let calc_len = tmp_pln.strlen();
        println!("str {tmp_pln} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_rul = SomeRule::new(&SomeSample::new_from_string("0b1111->0b0011")?); //(tmp_stb.clone(), tmp_st3.clone()));
        let tmp_stp2 = SomeStep::new(0, tmp_rul, AltRuleHint::NoAlt {});

        let mut tmp_pln = SomePlan::new(0, vec![tmp_stp.clone(), tmp_stp2]);
        tmp_pln.set_shortcut();
        println!("tmp_pln {tmp_pln}");

        let strrep = format!("{tmp_pln}");
        let len = strrep.len();
        let calc_len = tmp_pln.strlen();
        println!("str {tmp_pln} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    // Test the link function. This also tests the len, push, result_region, initial_region,
    // restrict_initial_region and restrict_result_region functions.
    #[test]
    fn link() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string("r0x0x")?;
        let reg2 = SomeRegion::new_from_string("r0x1x")?;
        let reg3 = SomeRegion::new_from_string("r1x1x")?;
        let reg4 = SomeRegion::new_from_string("r111x")?;
        let reg5 = SomeRegion::new_from_string("r101x")?;
        let reg6 = SomeRegion::new_from_string("r000x")?;

        let step1 = SomeStep::new(
            0,
            SomeRule::new_region_to_region(&reg1, &reg2),
            AltRuleHint::NoAlt {},
        );
        let step2 = SomeStep::new(
            0,
            SomeRule::new_region_to_region(&reg2, &reg3),
            AltRuleHint::NoAlt {},
        );
        let stp_str1 = SomePlan::new(0, vec![step1, step2]);

        let step4 = SomeStep::new(
            0,
            SomeRule::new_region_to_region(&reg4, &reg5),
            AltRuleHint::NoAlt {},
        );
        let step5 = SomeStep::new(
            0,
            SomeRule::new_region_to_region(&reg5, &reg6),
            AltRuleHint::NoAlt {},
        );
        let stp_str2 = SomePlan::new(0, vec![step4, step5]);

        println!("stp1 {}", stp_str1);
        println!("stp2 {}", stp_str2);

        let Some(stp_str3) = stp_str1.link(&stp_str2) else {
            return Err("Link error?".to_string());
        };
        println!("stp3 {}", stp_str3);
        assert!(stp_str3.len() == 4);

        assert!(*stp_str3.initial_region() == SomeRegion::new_from_string("r010x")?);

        assert!(*stp_str3.result_region() == reg6);

        Ok(())
    }

    #[test]
    fn nop() -> Result<(), String> {
        let tmp_reg = SomeRegion::new_from_string("r0X0X")?;
        let tmp_stp = SomeStep::new_no_op(&tmp_reg);
        println!("nop stop {tmp_stp}");

        let pln1 = SomePlan::new(0, vec![tmp_stp]);
        println!("pln1 {pln1}");

        //assert!(1 == 2);
        Ok(())
    }
}
