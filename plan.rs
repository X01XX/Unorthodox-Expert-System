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
use crate::rule::SomeRule;
use crate::state::SomeState;
use crate::step::{AltRuleHint, SomeStep};
use crate::stepstore::StepStore;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;

use std::fmt;
extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

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
    /// A StepStore instance.
    pub steps: StepStore, // Do some steps
}

impl SomePlan {
    /// Return a new, empty, plan.
    pub fn new(stepvec: Vec<SomeStep>) -> Self {
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
            steps: StepStore::new(stepvec),
        }
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

        Some(Self { steps })
    }

    /// Return a plan after restricting its initial region.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Option<Self> {
        //println!("plan::restrict_initial_region: {self} {regx}");
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

        Some(Self { steps })
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
    pub fn link(&self, other: &Self) -> Result<Self, String> {
        //println!("link: {self} and {other}");

        // Sanity checks
        if self.is_empty() && other.is_empty() {
            return Err("plan::link: both plans are empty".to_string());
        }

        if self.is_empty() && other.is_not_empty() {
            return Ok(other.clone());
        }

        if self.is_not_empty() && other.is_empty() {
            return Ok(self.clone());
        }

        debug_assert!(self.num_bits() == other.num_bits());

        // Restrict the StepStores, forward and backward.
        let reg1 = self.result_region();
        let reg2 = other.initial_region();

        let regx = match reg1.intersection(reg2) {
            Some(regy) => regy,
            None => return Err(format!("plan::link: reg {reg1} does not intersect {reg2}")),
        };

        let mut steps1 = match self.restrict_result_region(&regx) {
            Some(stepsx) => stepsx,
            None => {
                return Err(format!(
                    "plan::link: self {self} restrict result to {regx} failed"
                ))
            }
        };
        //println!("steps1 {steps1}");

        let steps2 = match other.restrict_initial_region(&regx) {
            Some(stepsx) => stepsx,
            None => {
                return Err(format!(
                    "plan::link: other {other} restrict initial to {regx} failed"
                ))
            }
        };

        //println!("steps2 {steps2}");

        // Push each step to check each step for problems.
        for stepx in steps2.steps.into_iter() {
            match steps1.push(stepx) {
                Ok(()) => continue,
                Err(errstr) => {
                    //println!("    push failed {errstr}");
                    return Err(errstr);
                }
            }
        }

        //println!("stepstore:link: 2 {} and {} giving {}", self, other, rc_steps);
        Ok(steps1)
    } // end link

    /// Return a string of action numbers to represent a plan.
    pub fn str_terse(&self) -> String {
        let mut rs = String::new();

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
        if self.is_empty() {
            return String::from("Plan[]");
        }
        let mut str = format!("Plan[{}", self.initial_region());
        for stpx in self.iter() {
            if let Some(act_id) = stpx.act_id {
                str.push_str(&format!("-{}->{}", act_id, stpx.result));
            } else {
                str.push_str(&format!("-no->{}", stpx.result));
            }
            if let AltRuleHint::AltRule { .. } = stpx.alt_rule {
                str.push('*')
            };
        }
        str.push(']');
        str
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

    /// Return a plan, given a string representation.
    /// Like Plan[], Plan[r1010-0->0101], or Plan[r101-0->r000-1->r100].
    pub fn new_from_string(plan_str: &str) -> Result<Self, String> {
        //println!("plan::new_from_string: {plan_str}");

        let mut plan_str2 = String::new();
        let mut last_chr = false;

        for (inx, chr) in plan_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "P" {
                    continue;
                } else {
                    return Err("Invalid string, should start with Plan[".to_string());
                }
            }
            if inx == 1 {
                if chr == "l" {
                    continue;
                } else {
                    return Err("Invalid string, should start with Plan[".to_string());
                }
            }
            if inx == 2 {
                if chr == "a" {
                    continue;
                } else {
                    return Err("Invalid string, should start with Plan[".to_string());
                }
            }
            if inx == 3 {
                if chr == "n" {
                    continue;
                } else {
                    return Err("Invalid string, should start with Plan[".to_string());
                }
            }
            if inx == 4 {
                if chr == "[" {
                    continue;
                } else {
                    return Err("Invalid string, should start with Plan[".to_string());
                }
            }
            if chr == "]" {
                last_chr = true;
                continue;
            }

            if last_chr {
                return Err("Invalid string, should end with ]".to_string());
            }
            plan_str2.push_str(chr);
        }
        if !last_chr {
            return Err("Invalid string, should end with ]".to_string());
        }

        if plan_str2.is_empty() {
            return Ok(SomePlan::new(vec![]));
        }

        // Split string into <region>-<action number> tokens, plus region at end.
        let mut token = String::new();
        let mut token_list = Vec::<String>::new();

        for chr in plan_str2.graphemes(true) {
            if chr == ">" {
                token_list.push(token);
                token = String::new();
            } else {
                token.push_str(chr);
            }
        }
        token_list.push(token);
        //println!("token_list {:?}", token_list);

        if token_list.len() < 2 {
            panic!("plan::new_from_string: invalid string");
        }

        // Split tokens between region and action, plus region at end.
        token = String::new();
        let mut token_list2 = Vec::<String>::new();

        for tokenx in token_list.iter() {
            for chr in tokenx.graphemes(true) {
                if chr == "-" {
                    token_list2.push(token);
                    token = String::new();
                } else {
                    token.push_str(chr);
                }
            }
        }
        token_list2.push(token);

        // println!("token_list2 {:?}", token_list2);

        // Tally up tokens and actions.
        let mut steps = Vec::<SomeStep>::new();
        let mut regions = Vec::<SomeRegion>::new();
        let mut actions = Vec::<Option<usize>>::new();

        for (inx, tokenx) in token_list2.into_iter().enumerate() {
            if inx % 2 == 0 {
                regions.push(SomeRegion::new_from_string(&tokenx).expect("Invalid region token"));
            } else if tokenx == "no" {
                actions.push(None);
            } else {
                let act_id = match tokenx.parse::<usize>() {
                    Ok(act_id) => act_id,
                    Err(error) => return Err(format!("Invalid action token {error}")),
                };
                actions.push(Some(act_id));
            }

            // Check if enough data to form a step.
            if regions.len() == 2 && actions.len() == 1 {
                let stepx = if let Some(act_id) = actions[0] {
                    SomeStep::new(
                        act_id,
                        SomeRule::new_region_to_region(&regions[0], &regions[1]),
                        AltRuleHint::NoAlt {},
                    )
                } else {
                    assert!(regions[0] == regions[1]);
                    SomeStep::new_no_op(&regions[0])
                };
                //println!("step {stepx}");
                steps.push(stepx);
                regions = vec![regions[1].clone()];
                actions = vec![];
            }
        }
        let ret_plan = SomePlan::new(steps);
        //println!("ret_plan {ret_plan}");

        Ok(ret_plan)
    }

    // Return true if a plan is valid.
    pub fn is_valid(&self) -> bool {
        let mut last_step: Option<&SomeStep> = None;
        for stepx in self.iter() {
            if let Some(stepy) = last_step {
                if stepy.result != stepx.initial {
                    return false;
                }
            }
            last_step = Some(stepx);
        }
        true
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
        if self.is_empty() {
            return 6;
        }
        let reg_len = self.initial_region().strlen();
        reg_len * (self.len() + 1) + (4 * self.len()) + 6
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_pln = SomePlan::new_from_string("Plan[]")?;
        println!("tmp_pln {tmp_pln}");

        let str_len = format!("{tmp_pln}").len();
        let calc_len = tmp_pln.strlen();
        println!("str {tmp_pln} len {str_len} calculated len {calc_len}");
        assert!(str_len == calc_len);

        let tmp_pln = SomePlan::new_from_string("Plan[r0000-0->r1111-0->r0011]")?;
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
        let pln1 = SomePlan::new_from_string("Plan[r0x0x-0->r0x1x-1->r1x1x]")?;
        println!("pln1 {}", pln1);

        let pln2 = SomePlan::new_from_string("Plan[r111x-2->r101x-3->r000x]")?;
        println!("pln2 {}", pln2);

        let Ok(pln3) = pln1.link(&pln2) else {
            return Err("Link error?".to_string());
        };
        println!("pln3 {}", pln3);

        assert!(format!("{pln3}") == "Plan[r010X-0->r011X-1->r111X-2->r101X-3->r000X]");

        Ok(())
    }

    #[test]
    fn nop() -> Result<(), String> {
        let pln1 = SomePlan::new_from_string("Plan[r0X0X-no->r0X0X]")?;
        println!("pln1 {pln1}");

        Ok(())
    }

    #[test]
    fn new_from_string() -> Result<(), String> {
        let plan1_str = "Plan[]";
        let plan1 = match SomePlan::new_from_string(plan1_str) {
            Ok(planx) => planx,
            Err(errstr) => return Err(errstr),
        };
        println!("plan1 {plan1}");
        assert!(format!("{plan1}") == plan1_str);

        let plan2_str = "Plan[r01X-0->r101-1->r011]";
        let plan2 = match SomePlan::new_from_string(plan2_str) {
            Ok(planx) => planx,
            Err(errstr) => return Err(errstr),
        };
        println!("plan2 {plan2}");

        assert!(format!("{plan2}") == plan2_str);

        Ok(())
    }
}
