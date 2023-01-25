//! The SomeStep struct.  Indicates an initial region, and action, and a result region..

use crate::change::SomeChange;
use crate::region::SomeRegion;
use crate::rule::SomeRule;

use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing

impl fmt::Display for SomeStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Debug, Clone)]
pub struct SomeStep {
    pub act_num: usize,
    pub initial: SomeRegion,
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
            act_num,
            initial,
            result,
            rule,
            alt_rule,
            group_reg,
        }
    }

    /// Return the number of integers used to impolment step values.
    pub fn num_ints(&self) -> usize {
        self.initial.num_ints()
    }

    /// Return a new step, by taking a given step and restricting the initial region.
    pub fn restrict_initial_region(&self, reg: &SomeRegion) -> Self {
        assert!(self.initial.intersects(reg));

        let rule_new = self.rule.restrict_initial_region(reg);

        Self {
            act_num: self.act_num,
            initial: rule_new.initial_region(),
            result: rule_new.result_region(),
            rule: rule_new,
            alt_rule: self.alt_rule,
            group_reg: self.group_reg.clone(),
        }
    }

    /// Return a new step, by taking a given step and restricting the result region
    pub fn restrict_result_region(&self, reg: &SomeRegion) -> Self {
        assert!(self.result.intersects(reg));

        let rule_new = self.rule.restrict_result_region(reg);

        Self {
            act_num: self.act_num,
            initial: rule_new.initial_region(),
            result: rule_new.result_region(),
            rule: rule_new,
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
        let _ = write!(rcstr, "{}", self.initial);
        let _ = write!(rcstr, " -{:02}> ", self.act_num);
        let _ = write!(rcstr, "{}", self.result);
        rcstr.push(']');
        rcstr
    }

    /// Return true if two steps are mutually exclusive.  That is the change of either
    /// must be reversed to use (intersect the initial region) of the other.
    pub fn mutually_exclusive(&self, other: &SomeStep, wanted: &SomeChange) -> bool {
        self.rule.mutually_exclusive(&other.rule, wanted)
    }
} // end impl SomeStep
