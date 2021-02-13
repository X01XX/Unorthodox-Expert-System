//! The SomeStep struct.  Indicates an initial region, and action, and a result region..
//!

use crate::region::SomeRegion;
use crate::rule::SomeRule;
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
        let init_reg = self.initial.intersection(&reg);

        Self {
            initial: init_reg.clone(),
            act_num: self.act_num,
            result: self.rule.result_from_initial(&init_reg),
            rule: self.rule.clone(),
            alt_rule: self.alt_rule,
            group_reg: self.group_reg.clone(),
        }
    }

    /// Return a new step, by taking a given step and restricting the result region
    pub fn restrict_result_region(&self, reg: &SomeRegion) -> Self {
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
        rcstr.push_str(
            &self
                .result
                .formatted_string_not_x(&self.rule.b01.m_or(&self.rule.b10)),
        );
        rcstr.push(']');
        rcstr
    }
}

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
