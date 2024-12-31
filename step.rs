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
        write!(f, "{}", self.formatted_str())
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
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
    pub act_id: Option<usize>,
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
        if self.group_inx != other.group_inx {
            return false;
        }
        if self.alt_rule != other.alt_rule {
            return false;
        }
        true
    }
}
impl Eq for SomeStep {}

impl SomeStep {
    /// Return a new Step struct instance.
    pub fn new(act_id: usize, rule: SomeRule, alt_rule: AltRuleHint) -> Self {
        debug_assert!(match &alt_rule {
            AltRuleHint::NoAlt {} => true,
            AltRuleHint::AltNoChange {} => true,
            AltRuleHint::AltRule { rule } => rule.num_bits() == rule.num_bits(),
        });
        debug_assert!(rule.is_valid_union());
        debug_assert!(rule.is_valid_intersection());

        let initial = rule.initial_region();

        let result = rule.result_region();

        Self {
            act_id: Some(act_id),
            initial,
            result,
            rule,
            alt_rule,
            group_inx: 0,
        }
    }

    /// Return a no-op step.
    pub fn new_no_op(regx: &SomeRegion) -> Self {
        Self {
            act_id: None,
            initial: regx.clone(),
            result: regx.clone(),
            rule: SomeRule::new_region_to_region_min(regx, regx),
            alt_rule: AltRuleHint::NoAlt {},
            group_inx: 0,
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
    fn formatted_str(&self) -> String {
        let mut rcstr = String::with_capacity(self.strlen());
        rcstr.push('[');
        rcstr.push_str(&self.initial.to_string());
        if let Some(act_id) = self.act_id {
            rcstr.push_str(&format!(" -{:02}> ", act_id));
        } else {
            rcstr.push_str(" -no> ");
        }
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

    /// Return true if all wanted changes in a step are reversed by a second step.
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

    /// Set the group index.
    pub fn set_group_inx(&mut self, inx: usize) {
        self.group_inx = inx;
    }
} // end impl SomeStep

/// Implement the trait StrLen for SomeStep.
impl StrLen for SomeStep {
    fn strlen(&self) -> usize {
        let mut len = 2; // [...]
        len += 6; // " -00> "
        len += 2 * self.initial.strlen(); // two regions.
                                          // 6 = " Alt: ".
        len += match &self.alt_rule {
            AltRuleHint::NoAlt {} => 6 + 4,
            AltRuleHint::AltNoChange {} => 6 + 9,
            AltRuleHint::AltRule { rule } => 6 + rule.strlen(),
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
    use std::str::FromStr;

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_rul = SomeRule::new(&SomeSample::from_str("s0000->s0010")?);
        let tmp_stp = SomeStep::new(0, tmp_rul.clone(), AltRuleHint::NoAlt {});

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_stp = SomeStep::new(0, tmp_rul.clone(), AltRuleHint::AltNoChange {});

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_alt = SomeRule::new(&SomeSample::from_str("s0010->s0000")?);
        let tmp_stp = SomeStep::new(0, tmp_rul, AltRuleHint::AltRule { rule: tmp_alt });

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_rul = SomeRule::new(&SomeSample::from_str("s0000_0000->s0000_0000")?);
        let tmp_stp = SomeStep::new(0, tmp_rul, AltRuleHint::NoAlt {});
        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn nop() -> Result<(), String> {
        let tmp_reg = SomeRegion::from_str("r0X0X")?;
        let tmp_stp = SomeStep::new_no_op(&tmp_reg);
        println!("nop stop {tmp_stp}");

        let stpx = tmp_stp.restrict_initial_region(&SomeRegion::from_str("r0XX1")?);
        println!("stpx: {stpx}");
        assert!(stpx.initial == SomeRegion::from_str("r0X01")?);

        let stpx = tmp_stp.restrict_result_region(&SomeRegion::from_str("rX00X")?);
        println!("stpx: {stpx}");
        assert!(stpx.initial == SomeRegion::from_str("r000X")?);

        //assert!(1 == 2);
        Ok(())
    }
}
