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
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;
use std::str::FromStr;
use unicode_segmentation::UnicodeSegmentation;

impl fmt::Display for SomePlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

/// Implement the PartialEq trait.
impl PartialEq for SomePlan {
    fn eq(&self, other: &Self) -> bool {
        if self.dom_id != other.dom_id {
            return false;
        }
        if self.len() != other.len() {
            return false;
        }
        if self.is_empty() {
            return true;
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
    /// Domain id number.
    pub dom_id: usize,
    /// A StepStore instance.
    pub steps: StepStore, // Do some steps
}

impl SomePlan {
    /// Return a new, empty, plan.
    pub fn new(dom_id: usize, stepvec: Vec<SomeStep>) -> Self {
        let ret = Self {
            dom_id,
            steps: StepStore::new(stepvec),
        };
        match ret.valid_step_sequence() {
            Ok(()) => ret,
            Err(errstr) => panic!("{errstr}"),
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

        Some(Self {
            dom_id: self.dom_id,
            steps,
        })
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

        Some(Self {
            dom_id: self.dom_id,
            steps,
        })
    }

    /// Add a step to a SomePlan.
    pub fn push(&mut self, stepx: SomeStep) -> Result<(), String> {
        if self.is_not_empty() {
            if !self.result_region().intersects(&stepx.initial) {
                return Err(format!("plan {self} does not intersect step {stepx}"));
            }
            if self.any_initial_intersects(&stepx.result) {
                return Err(format!("plan {self} step {stepx} circles back"));
            }
        }

        self.steps.push(stepx);
        Ok(())
    }

    /// Remove an item from a plan.
    pub fn pop(&mut self) -> Option<SomeStep> {
        self.steps.pop()
    }

    /// Add a step to the beginning of a plan..
    pub fn push_first(&mut self, stepx: SomeStep) -> Result<(), String> {
        if self.is_not_empty() {
            if !self.steps[0].initial.intersects(&stepx.result) {
                return Err(format!("plan {self} does not intersect step {stepx}"));
            }
            if self.any_result_intersects(&stepx.initial) {
                return Err(format!("plan {self} step {stepx} circles back"));
            }
        }

        self.steps.push_first(stepx);

        Ok(())
    }

    /// Remove an item from the beginning of a plan.
    pub fn pop_first(&mut self) -> Option<SomeStep> {
        self.steps.pop_first()
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

        debug_assert!(self.dom_id == other.dom_id);

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
        for stepx in steps2.steps {
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
        let mut rs = format!("P[{}:", self.dom_id);

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

    /// Return the result state of a plan, given an initial state..
    pub fn result_from_initial_state(&self, astate: &SomeState) -> Option<SomeState> {
        assert!(self.is_not_empty());
        let mut ret_state = astate.clone();
        for stepx in self.iter() {
            if stepx.initial.is_superset_of(&ret_state) {
                ret_state = stepx.rule.result_from_initial_state(&ret_state);
            } else {
                return None;
            }
        }
        Some(ret_state)
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
    /// Initial region to goal region.
    fn formatted_str(&self) -> String {
        if self.is_empty() || (self.len() == 1 && self[0].act_id.is_none()) {
            return format!("P[{}]", self.dom_id);
        }
        let mut str = format!("P[{}, {}", self.dom_id, self.initial_region());
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

    /// Return a String representation of SomePlan,
    /// Goal region from initial region.
    pub fn formatted_str_from(&self) -> String {
        if self.is_empty() {
            return String::from("P[]");
        }
        let mut str = format!("P[{}", self.initial_region());
        for stpx in self.iter() {
            if let Some(act_id) = stpx.act_id {
                str.push_str(&format!("<-{}-{}", act_id, stpx.result));
            } else {
                str.push_str(&format!("<-no-{}", stpx.result));
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

    /// Return true if a plan contains an intersecting initial region.
    fn any_initial_intersects(&self, regx: &SomeRegion) -> bool {
        for stepx in self.iter() {
            if stepx.initial.intersects(regx) {
                return true;
            }
        }
        false
    }

    /// Return true if a plan contains an intersecting result region.
    fn any_result_intersects(&self, regx: &SomeRegion) -> bool {
        for stepx in self.iter() {
            if stepx.result.intersects(regx) {
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

    /// Return true if a plan causes a change.
    pub fn causes_change(&self) -> bool {
        if self.is_empty() {
            return false;
        }
        self.initial_region() != self.result_region()
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
    pub fn _range(&self) -> SomeRegion {
        let mut rng = self.initial_region().clone();
        for stepx in self.iter() {
            rng = rng.union(&stepx.result);
        }
        rng
    }

    // Return true if a plan is valid.
    pub fn valid_step_sequence(&self) -> Result<(), String> {
        let mut last_step: Option<&SomeStep> = None;
        for stepx in self.iter() {
            if let Some(stepy) = last_step {
                if stepy.result != stepx.initial {
                    return Err(format!(
                        "SomePlan::valid_step_sequence: step result {} != following step initial {}",
                        stepy.result, stepx.initial
                    ));
                }
            }
            last_step = Some(stepx);
        }
        Ok(())
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
        if self.is_empty() || (self.len() == 1 && self[0].act_id.is_none()) {
            // P[] + dom_id.
            if self.dom_id < 10 {
                return 4;
            } else {
                return 5;
            }
        }
        let mut reg_len = self.initial_region().strlen();

        reg_len = reg_len * (self.len() + 1) + (4 * self.len()) + 3;

        // Add space for <dom_id>,
        if self.dom_id < 10 {
            reg_len += 3;
        } else {
            reg_len += 4;
        }

        reg_len
    }
}

impl FromStr for SomePlan {
    type Err = String;
    /// Return SomePlan, given a string representation.
    /// Like P[1], P[1, r1010-0->r0101], or P[1, r101-0->r000-1->r100].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("plan::from_str: {str_in}");
        let str_in2 = str_in.trim();

        if str_in2.len() < 4 {
            return Err("SomePlan::from_str: string must be at least P[<dom_id>]".to_string());
        }

        if str_in2[0..2] != *"P[" {
            return Err("plan::from_str: string should begin with P[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("plan::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[2..(str_in2.len() - 1)];

        // Split string into SomeState tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("statestore::from_str: {errstr}")),
        };
        //println!("tokens {:?}", tokens);

        // Get domain id.
        let dom_id = match tokens[0].parse::<usize>() {
            Ok(val) => val,
            Err(errstr) => return Err(format!("plan::from_str: {errstr}")),
        };

        // Split string into <region>-<action number> tokens, plus region at end.
        let mut token = String::new();
        let mut token_list = Vec::<String>::new();

        if tokens.len() == 1 {
            return Ok(Self::new(dom_id, vec![]));
        }

        if tokens.len() != 2 {
            return Err(format!(
                "plan::from_str: incorrect number of tokens {token_str}"
            ));
        }

        for chr in tokens[1].graphemes(true) {
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
            return Err(format!("plan::from_str: invalid string {token_str}"));
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
                match SomeRegion::from_str(&tokenx) {
                    Ok(regx) => regions.push(regx),
                    Err(errstr) => {
                        return Err(format!("SomePlan::from_str: {errstr}"));
                    }
                }
            } else if tokenx == "no" {
                actions.push(None);
            } else {
                match tokenx.parse::<usize>() {
                    Ok(act_id) => actions.push(Some(act_id)),
                    Err(error) => return Err(format!("SomePlan::from_str: {error}")),
                };
            }

            // Check if enough data to form a step.
            if regions.len() == 2 && actions.len() == 1 {
                let stepx = if let Some(act_id) = actions[0] {
                    SomeStep::new(
                        act_id,
                        SomeRule::new_region_to_region_min(&regions[0], &regions[1]),
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
        let ret_plan = SomePlan::new(dom_id, steps);
        //println!("ret_plan {ret_plan}");

        match ret_plan.valid_step_sequence() {
            Ok(()) => Ok(ret_plan),
            Err(errstr) => Err(format!("SomePlan::from_str: {errstr}")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_pln = SomePlan::from_str("P[1]")?;
        println!("tmp_pln {tmp_pln}");

        let str_len = format!("{tmp_pln}").len();
        let calc_len = tmp_pln.strlen();
        println!("str {tmp_pln} len {str_len} calculated len {calc_len}");
        assert!(str_len == calc_len);

        let tmp_pln = SomePlan::from_str("P[10, r0000-0->r1111-0->r0011]")?;
        println!("tmp_pln {tmp_pln}");

        let strrep = format!("{tmp_pln}");
        let len = strrep.len();
        let calc_len = tmp_pln.strlen();
        println!("str {tmp_pln} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_pln = SomePlan::from_str("P[10, r0000-no->r0000]")?; // -> P[10]
        println!("tmp_pln {tmp_pln}");

        let strrep = format!("{tmp_pln}");
        let len = strrep.len();
        let calc_len = tmp_pln.strlen();
        println!("str {tmp_pln} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        let pln1 = SomePlan::from_str(
            "P[1, rX_1001-2->rX_1101-0->rX_1100-1->rX_1110-3->r1_1110-4->r1_0110]",
        )?;
        println!("pln1 {}", pln1);

        if let Some(plan2) = pln1.restrict_initial_region(&SomeRegion::from_str("r1_1001")?) {
            println!("plan found: {plan2}");
            return Ok(());
        };
        return Err("restrict_initial_region failed".to_string());
    }

    #[test]
    fn restrict_result_region() -> Result<(), String> {
        let pln1 = SomePlan::from_str("P[1, rX_1001-2->rX_1101-0->rX_1100]")?;
        println!("pln1 {}", pln1);

        if let Some(plan2) = pln1.restrict_result_region(&SomeRegion::from_str("r1_1100")?) {
            println!("plan found: {plan2}");
            return Ok(());
        };
        return Err("restrict_initial_region failed".to_string());
    }

    #[test]
    fn result_from_initial_region() -> Result<(), String> {
        let pln1 = SomePlan::from_str("P[1, rX_1001-2->rX_1101-0->rX_1100]")?;
        println!("pln1 {}", pln1);

        if let Some(initial) = pln1.result_from_initial_region(&SomeRegion::from_str("r1_1001")?) {
            println!("initial: {initial}");
            assert!(initial == SomeRegion::from_str("r1_1100")?);
            return Ok(());
        };
        return Err("restrict_initial_region failed".to_string());
    }

    #[test]
    fn result_from_initial_state() -> Result<(), String> {
        let pln1 = SomePlan::from_str("P[1, rX_1001-2->rX_1101-0->rX_1100]")?;
        println!("pln1 {}", pln1);

        if let Some(initial) = pln1.result_from_initial_state(&SomeState::from_str("s1_1001")?) {
            println!("initial: {initial}");
            assert!(initial == SomeState::from_str("s1_1100")?);
            return Ok(());
        };
        return Err("restrict_initial_region failed".to_string());
    }

    #[test]
    fn causes_change() -> Result<(), String> {
        let pln1 = SomePlan::from_str("P[0]")?;
        println!("pln1 {}", pln1);

        assert!(!pln1.causes_change());

        let pln2 = SomePlan::from_str("P[0, rX_1001-no->rX_1001]")?;
        println!("pln2 {}", pln2);

        assert!(!pln2.causes_change());

        let pln3 = SomePlan::from_str("P[0, rX_1001-2->rX_1101-0->rX_1000]")?;
        println!("pln3 {}", pln3);

        assert!(pln3.causes_change());
        Ok(())
    }

    // Test the link function. This also tests the len, push, result_region, initial_region,
    // restrict_initial_region and restrict_result_region functions.
    #[test]
    fn link() -> Result<(), String> {
        let pln1 = SomePlan::from_str("P[0, r0x0x-0->r0x1x-1->r1x1x]")?;
        println!("pln1 {}", pln1);

        let pln2 = SomePlan::from_str("P[0, r111x-2->r101x-3->r000x]")?;
        println!("pln2 {}", pln2);

        let Ok(pln3) = pln1.link(&pln2) else {
            return Err("Link error?".to_string());
        };
        println!("pln3 {}", pln3);

        assert!(format!("{pln3}") == "P[0, r010X-0->r011X-1->r111X-2->r101X-3->r000X]");

        Ok(())
    }

    #[test]
    fn nop() -> Result<(), String> {
        let pln1 = SomePlan::from_str("P[0, r0X0X-no->r0X0X]")?;
        println!("pln1 {pln1}");

        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let plan1_str = "P[1]";
        let plan1 = match SomePlan::from_str(plan1_str) {
            Ok(planx) => planx,
            Err(errstr) => return Err(errstr),
        };
        println!("plan1 {plan1}");
        assert!(format!("{plan1}") == plan1_str);

        let plan2_str = "P[1, r01X-0->r101-1->r011]";
        let plan2 = match SomePlan::from_str(plan2_str) {
            Ok(planx) => planx,
            Err(errstr) => return Err(errstr),
        };
        println!("plan2 {plan2}");

        assert!(format!("{plan2}") == plan2_str);

        Ok(())
    }
}
