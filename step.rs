//! The SomeStep struct.  Indicates an initial region, and action, and a result region..

use crate::bits::NumBits;
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

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum AltRuleHint {
    NoAlt {},
    AltNoChange {},
    AltRule { rule: SomeRule },
}

impl fmt::Display for AltRuleHint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Self::NoAlt {} => String::from("Alt: none"),
            Self::AltNoChange {} => String::from("Alt: No change"),
            Self::AltRule { rule } => format!("Alt: {rule}"),
        };
        write!(f, "{}", str)
    }
}

#[readonly::make]
#[derive(Debug, Clone, Deserialize, Serialize)]
/// A step that changes a state to another.
pub struct SomeStep {
    /// Action number.
    pub act_id: usize,
    /// Initial region of rule.
    pub initial: SomeRegion,
    /// Result region of rule.
    pub result: SomeRegion,
    /// Rule used.
    pub rule: SomeRule,
    /// Alternate rule hint.
    pub alt_rule: AltRuleHint,
    /// Group index in current group store.
    pub group_inx: usize,
}

impl PartialEq for SomeStep {
    /// Return true if two steps ar equal.
    fn eq(&self, other: &Self) -> bool {
        if self.act_id != other.act_id {
            return false;
        }
        if self.initial != other.initial {
            return false;
        }
        if self.result != other.result {
            return false;
        }
        true
    }
}
impl Eq for SomeStep {}

impl SomeStep {
    /// Return a new Step struct instance.
    pub fn new(act_id: usize, rule: SomeRule, alt_rule: AltRuleHint, group_inx: usize) -> Self {
        debug_assert!(match &alt_rule {
            AltRuleHint::NoAlt {} => true,
            AltRuleHint::AltNoChange {} => true,
            AltRuleHint::AltRule { rule } => rule.num_bits() == rule.num_bits(),
        });

        let initial = rule.initial_region();

        let result = rule.result_region();

        Self {
            act_id,
            initial,
            result,
            rule,
            alt_rule,
            group_inx,
        }
    }

    /// Return a new step, by taking a given step and restricting the initial region.
    pub fn restrict_initial_region(&self, reg: &SomeRegion) -> Self {
        debug_assert_eq!(self.num_bits(), reg.num_bits());
        assert!(self.initial.intersects(reg));

        let rule_new = self.rule.restrict_initial_region(reg);

        Self {
            act_id: self.act_id,
            initial: rule_new.initial_region(),
            result: rule_new.result_region(),
            rule: rule_new,
            alt_rule: self.alt_rule.clone(),
            group_inx: self.group_inx,
        }
    }

    /// Return a new step, by taking a given step and restricting the result region
    pub fn restrict_result_region(&self, reg: &SomeRegion) -> Self {
        debug_assert_eq!(self.num_bits(), reg.num_bits());
        assert!(self.result.intersects(reg));

        let rule_new = self.rule.restrict_result_region(reg);

        Self {
            act_id: self.act_id,
            initial: rule_new.initial_region(),
            result: rule_new.result_region(),
            rule: rule_new,
            alt_rule: self.alt_rule.clone(),
            group_inx: self.group_inx,
        }
    }

    /// Return a string representing a step.
    fn formatted_string(&self) -> String {
        let mut rcstr = String::with_capacity(self.strlen());
        rcstr.push('[');
        rcstr.push_str(&self.initial.to_string());
        rcstr.push_str(&format!(" -{:02}> ", self.act_id));
        rcstr.push_str(&self.result.to_string());
        rcstr.push_str(&format!(" {}", self.alt_rule));
        rcstr.push(']');
        rcstr
    }

    /// Return true if two steps are mutually exclusive.  That is the change of either
    /// must be reversed to use (intersect the initial region) of the other.
    pub fn mutually_exclusive(&self, other: &Self, wanted: &SomeChange) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());
        debug_assert_eq!(self.num_bits(), wanted.num_bits());
        // Groups that change more than one bit may end up being compared.
        if self.act_id == other.act_id && self.group_inx == other.group_inx {
            return false;
        }
        self.rule.mutually_exclusive(&other.rule, wanted)
    }

    pub fn sequence_blocks_changes(&self, other: &Self, wanted: &SomeChange) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());
        debug_assert_eq!(self.num_bits(), wanted.num_bits());
        // Groups that change more than one bit may end up being compared.
        if self.act_id == other.act_id && self.group_inx == other.group_inx {
            return false;
        }
        self.rule.sequence_blocks_changes(&other.rule, wanted)
    }
    /// Return the number of bits changed in a step.
    pub fn num_bits_changed(&self) -> usize {
        self.rule.num_bits_changed()
    }

    /// Return the number of bits used in the step parts.
    pub fn num_bits(&self) -> usize {
        self.initial.num_bits()
    }
} // end impl SomeStep

/// Implement the trait StrLen for SomeStep.
impl StrLen for SomeStep {
    fn strlen(&self) -> usize {
        let mut len = 8 + (2 * self.initial.strlen());
        // 1 = space separator. 5 = "Alt: ".
        len += match &self.alt_rule {
            AltRuleHint::NoAlt {} => 1 + 5 + 4,
            AltRuleHint::AltNoChange {} => 1 + 5 + 9,
            AltRuleHint::AltRule { rule } => 1 + 5 + rule.strlen(),
        };
        len
    }
}

/// Implement the NumBits trait for SomeStep.
impl NumBits for SomeStep {
    fn num_bits(&self) -> usize {
        self.num_bits()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rule::SomeRule;
    use crate::sample::SomeSample;
    use crate::step::SomeStep;

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_rul = SomeRule::new(&SomeSample::new_from_string("0b0000->0b0010")?); //(tmp_sta.clone(), tmp_sta2.clone()));
        let tmp_stp = SomeStep::new(0, tmp_rul.clone(), AltRuleHint::NoAlt {}, 0);

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_stp = SomeStep::new(0, tmp_rul.clone(), AltRuleHint::AltNoChange {}, 0);

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_alt = SomeRule::new(&SomeSample::new_from_string("0b0010->0b0000")?); //(tmp_sta2.clone(), tmp_sta));
        let tmp_stp = SomeStep::new(0, tmp_rul, AltRuleHint::AltRule { rule: tmp_alt }, 0);

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);
        //assert!(1 == 2);
        Ok(())
    }
}
