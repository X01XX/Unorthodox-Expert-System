//! The SomeStep struct.  Indicates an initial region, and action, and a result region..

use crate::change::SomeChange;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SomeStep {
    pub act_id: usize,
    pub initial: SomeRegion,
    pub result: SomeRegion,
    pub rule: SomeRule,
    pub alt_rule: bool,
    pub group_reg: SomeRegion,
}

impl SomeStep {
    /// Return a new Step struct instance.
    pub fn new(act_id: usize, rule: SomeRule, alt_rule: bool, group_reg: SomeRegion) -> Self {
        let initial = rule.initial_region();

        let result = rule.result_region();
        Self {
            act_id,
            initial,
            result,
            rule,
            alt_rule,
            group_reg,
        }
    }

    /// Return a new step, by taking a given step and restricting the initial region.
    pub fn restrict_initial_region(&self, reg: &SomeRegion) -> Self {
        assert!(self.initial.intersects(reg));

        let rule_new = self.rule.restrict_initial_region(reg);

        Self {
            act_id: self.act_id,
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
            act_id: self.act_id,
            initial: rule_new.initial_region(),
            result: rule_new.result_region(),
            rule: rule_new,
            alt_rule: self.alt_rule,
            group_reg: self.group_reg.clone(),
        }
    }

    /// Return a string representing a step.
    fn formatted_string(&self) -> String {
        let mut rcstr = String::with_capacity(self.strlen());
        rcstr.push('[');
        rcstr.push_str(&self.initial.to_string());
        rcstr.push_str(&format!(" -{:02}> ", self.act_id));
        rcstr.push_str(&self.result.to_string());
        rcstr.push(']');
        rcstr
    }

    /// Return true if two steps are mutually exclusive.  That is the change of either
    /// must be reversed to use (intersect the initial region) of the other.
    pub fn mutually_exclusive(&self, other: &Self, wanted: &SomeChange) -> bool {
        self.rule.mutually_exclusive(&other.rule, wanted)
    }
} // end impl SomeStep

/// Implement the trait StrLen for SomeStep.
impl StrLen for SomeStep {
    fn strlen(&self) -> usize {
        8 + (2 * self.initial.strlen())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::region::SomeRegion;
    use crate::rule::SomeRule;
    use crate::sample::SomeSample;
    use crate::state::SomeState;
    use crate::step::SomeStep;

    #[test]
    fn test_strlen() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0]));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));
        let tmp_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(vec![0]))]);
        let tmp_stp = SomeStep::new(0, tmp_rul, true, tmp_reg);

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }
}
