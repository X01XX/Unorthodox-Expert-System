//! The SomeStep struct.  Indicates an initial region, and action, and a result region..

use crate::region::SomeRegion;
use crate::rule::SomeRule;
//use crate::state::SomeState;
use crate::change::SomeChange;

use std::fmt;

impl fmt::Display for SomeStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[derive(Debug)]
pub struct SomeStep {
    pub initial: SomeRegion,
    pub act_num: usize,
    pub result: SomeRegion,
    pub rule: SomeRule,
    pub alt_rule: bool,
    pub group_reg: SomeRegion,
}

impl SomeStep {
    /// Return a new Step struct instance.
    pub fn new(act_num: usize, rule: SomeRule, alt_rule: bool, group_reg: SomeRegion) -> Self {
        let initial = rule.initial_region();

        let result = rule.result_region();
        Self {
            initial,
            act_num,
            result,
            rule,
            alt_rule,
            group_reg,
        }
    }

    /// Return a new step, by taking a given step and restricting the initial region.
    pub fn restrict_initial_region(&self, reg: &SomeRegion) -> Self {
        assert!(self.initial.intersects(&reg));

        let init_reg = self.initial.intersection(&reg);

        Self {
            initial: init_reg.clone(),
            act_num: self.act_num,
            result: self.rule.result_from_initial_region(&init_reg),
            rule: self.rule.restrict_initial_region(&init_reg),
            alt_rule: self.alt_rule,
            group_reg: self.group_reg.clone(),
        }
    }

    /// Return a new step, by taking a given step and restricting the result region
    pub fn restrict_result_region(&self, reg: &SomeRegion) -> Self {
        assert!(self.result.intersects(&reg));

        let rulx = self.rule.restrict_result_region(reg);

        Self {
            initial: rulx.initial_region(),
            act_num: self.act_num,
            result: rulx.result_region(),
            rule: rulx,
            alt_rule: self.alt_rule,
            group_reg: self.group_reg.clone(),
        }
    }

    /// Return the expected length of a string representing a step.
    pub fn formatted_string_length(&self) -> usize {
        8 + (2 * self.initial.formatted_string_length())
    }

    /// Return a string representing a step.
    pub fn formatted_string(&self) -> String {
        let mut rcstr = String::with_capacity(self.formatted_string_length());
        rcstr.push('[');
        rcstr.push_str(&format!("{}", self.initial));
        rcstr.push_str(&format!(" -{:02}> ", self.act_num));
        rcstr.push_str(&format!("{}", self.result));
        rcstr.push(']');
        rcstr
    }

    // Return the result of a step when applied to a given initial region.
    // The given initial region must intersect the step initial region.
//    pub fn result_from_initial(&self, i_reg: &SomeRegion) -> SomeRegion {
//        assert!(self.initial.intersects(&i_reg));
//        self.rule.result_from_initial_region(i_reg)
//    }

    /// Return true if two steps are mutually exclusive.  That is the change of either
    /// must be reversed to use (intersect the initial region) of the other.
    pub fn mutually_exclusive(&self, other: &SomeStep, wanted: &SomeChange) -> bool {

        self.rule.mutually_exclusive(&other.rule, wanted)
    }

    // Return the change struct for a step
//    pub fn change(&self) -> SomeChange {
//        self.rule.change()
//    }

    /// Return true is a step is a superset of another step.
    pub fn _is_superset_of(&self, other: &SomeStep) -> bool {
        self.rule._is_superset_of(&other.rule)
    }
} // end impl SomeStep

impl Clone for SomeStep {
    fn clone(&self) -> Self {
        Self {
            initial: self.initial.clone(),
            act_num: self.act_num,
            result: self.result.clone(),
            rule: self.rule.clone(),
            alt_rule: self.alt_rule,
            group_reg: self.group_reg.clone(),
        }
    }
}
