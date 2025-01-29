//! The SomeStep struct.  Indicates an initial region, and action, and a result region..

pub use crate::altrulehint::AltRuleHint;
use crate::bits::NumBits;
use crate::change::SomeChange;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

impl fmt::Display for SomeStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
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
        if self.alt_rule != other.alt_rule {
            return false;
        }
        true
    }
}
impl Eq for SomeStep {}

impl SomeStep {
    /// Return a new Step struct instance.
    pub fn new(act_id: usize, rule: SomeRule, alt_rule: Option<&SomeRule>) -> Self {
        //println!("SomeStep::new: rule {rule}");
        //println!("SomeStep::new: alt_rule {alt_rule}");
        debug_assert!(rule.is_valid_union());
        debug_assert!(rule.is_valid_intersection());

        let initial = rule.initial_region();
        let result = rule.result_region();

        // Check if an alt_rule is given.
        if let Some(alt_rul) = alt_rule {
            let alt_initial = alt_rul.initial_region();

            debug_assert!(initial.intersects(&alt_initial));

            // Restrict alt_rule, if needed.
            let mut alt_rul2 = alt_rul.clone();
            if alt_initial != initial {
                alt_rul2 = alt_rul.restrict_initial_region(&initial);
            }

            debug_assert!(alt_rul2 != rule);

            if alt_rul.causes_predictable_change() {
                Self {
                    act_id,
                    initial,
                    result,
                    rule,
                    alt_rule: AltRuleHint::AltRule {
                        rule: alt_rul.clone(),
                    },
                }
            } else {
                Self {
                    act_id,
                    initial,
                    result,
                    rule,
                    alt_rule: AltRuleHint::AltNoChange {},
                }
            }
        } else {
            Self {
                act_id,
                initial,
                result,
                rule,
                alt_rule: AltRuleHint::NoAlt {},
            }
        }
    }

    /// Return a no-op step.
    pub fn new_no_op(regx: &SomeRegion) -> Self {
        Self {
            act_id: 0,
            initial: regx.clone(),
            result: regx.clone(),
            rule: SomeRule::new_region_to_region_min(regx, regx),
            alt_rule: AltRuleHint::NoAlt {},
        }
    }

    /// Return a new step, by taking a given step and restricting the initial region.
    pub fn restrict_initial_region(&self, reg: &SomeRegion) -> Self {
        debug_assert_eq!(self.num_bits(), reg.num_bits());
        assert!(self.initial.intersects(reg));

        let rule_new = self.rule.restrict_initial_region(reg);

        let alt_rule = match &self.alt_rule {
            AltRuleHint::NoAlt {} => AltRuleHint::NoAlt {},
            AltRuleHint::AltNoChange {} => AltRuleHint::AltNoChange {},
            AltRuleHint::AltRule { rule: arule } => {
                let tmp_rule = arule.restrict_initial_region(reg);
                if tmp_rule == rule_new {
                    AltRuleHint::NoAlt {}
                } else if tmp_rule.causes_predictable_change() {
                    AltRuleHint::AltRule { rule: tmp_rule }
                } else {
                    AltRuleHint::AltNoChange {}
                }
            }
        };

        Self {
            act_id: self.act_id,
            initial: rule_new.initial_region(),
            result: rule_new.result_region(),
            rule: rule_new,
            alt_rule,
        }
    }

    /// Return a new step, by taking a given step and restricting the result region
    pub fn restrict_result_region(&self, reg: &SomeRegion) -> Self {
        debug_assert_eq!(self.num_bits(), reg.num_bits());
        assert!(self.result.intersects(reg));

        let rule_new = self.rule.restrict_result_region(reg);

        let alt_rule = match &self.alt_rule {
            AltRuleHint::NoAlt {} => AltRuleHint::NoAlt {},
            AltRuleHint::AltNoChange {} => AltRuleHint::AltNoChange {},
            AltRuleHint::AltRule { rule: arule } => {
                let tmp_rule = arule.restrict_initial_region(&rule_new.initial_region());
                if tmp_rule == rule_new {
                    AltRuleHint::NoAlt {}
                } else if tmp_rule.causes_predictable_change() {
                    AltRuleHint::AltRule { rule: tmp_rule }
                } else {
                    AltRuleHint::AltNoChange {}
                }
            }
        };

        Self {
            act_id: self.act_id,
            initial: rule_new.initial_region(),
            result: rule_new.result_region(),
            rule: rule_new,
            alt_rule,
        }
    }

    /// Return a string representing a step.
    fn formatted_str(&self) -> String {
        let mut rcstr = String::with_capacity(self.strlen());
        rcstr.push_str("STP[");
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

        self.rule.mutually_exclusive(&other.rule, wanted)
    }

    /// Return true if all wanted changes in a step are reversed by a second step.
    pub fn sequence_blocks_changes(&self, other: &Self, wanted: &SomeChange) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());
        debug_assert_eq!(self.num_bits(), wanted.num_bits());

        self.rule
            .sequence_blocks_all_wanted_changes(&other.rule, wanted)
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
        let mut len = 5; // STP[...]
        len += 6; // " -..> "
        len += 2 * self.initial.strlen();
        len += 1 + self.alt_rule.strlen();
        len
    }
}

/// Implement the NumBits trait for SomeStep.
impl NumBits for SomeStep {
    fn num_bits(&self) -> usize {
        self.num_bits()
    }
}

impl FromStr for SomeStep {
    type Err = String;

    /// Return a SomeStep instance, given a string representation.
    ///
    /// "[X010 -01> 0110 Alt: None]"
    ///
    /// "[X010 -01> 0110 Alt: NoChange]"
    ///
    /// "[X010 -01> 0110 Alt: XX/00/10/00]"
    ///
    /// "[X010 -00> X010 Alt: None]"
    ///
    /// The number bit positions used in the initial regian, result region, and alternate rule (if given), must match.
    ///
    /// The initial region of the alternate rule (if given), must match the initial region.
    ///
    /// The alternate rule (if given), cannot be the same as the initial->result rule.
    ///
    /// If no change, indicated by "-0>", the initial region must be the same as the result region, the alternate rule must be None.
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("SomeStep::from_str: {str_in}");
        let str_in2 = str_in.trim();

        if str_in2.len() < 5 {
            return Err("step::from_str: invalid string".to_string());
        }

        if str_in2[0..4] != *"STP[" {
            return Err("step::from_str: string should begin with STP[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("step::from_str: string should end with ]".to_string());
        }

        let token_str = &str_in2[4..(str_in2.len() - 1)];

        //println!("token_str: {token_str}");

        // Split string into tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("step::from_str: {errstr}")),
        };
        //println!("tokens {:?}", tokens);

        if tokens.len() != 5 {
            return Err("step::from_str: invalid string, number tokens s/b 5".to_string());
        }

        let initial = SomeRegion::from_str(&tokens[0])?;

        // parse -n> or -nn>
        if tokens[1][0..1] != *"-" {
            return Err("step::from_str: action token should begin with -".to_string());
        }
        let act_str = if tokens[1].len() == 3 {
            if tokens[1][2..3] != *">" {
                return Err("step::from_str: action token should end with >".to_string());
            }
            &tokens[1][1..2]
        } else if tokens[1].len() == 4 {
            if tokens[1][3..4] != *">" {
                return Err("step::from_str: action token should end with >".to_string());
            }
            &tokens[1][1..3]
        } else {
            return Err("step::from_str: do not understand action token".to_string());
        };

        let result = SomeRegion::from_str(&tokens[2])?;

        if result.num_bits() != initial.num_bits() {
            return Err(
                "step::from_str: initial and result use a different number of bits".to_string(),
            );
        }

        if initial
            .edge_mask()
            .bitwise_and(&result.x_mask())
            .is_not_low()
        {
            return Err("step::from_str: result has invalid 1/X or 0/X position".to_string());
        }

        if act_str == "0" {
            if initial != result {
                return Err(
                    "step::from_str: for action zero, initial and result must be eq".to_string(),
                );
            }
            return Ok(Self::new_no_op(&initial));
        }

        let act_id = match act_str.parse::<usize>() {
            Ok(num) => num,
            Err(errstr) => return Err(format!("step::from_str: action token problem: {errstr}")),
        };

        if tokens[3].to_uppercase() == "ALT:" {
        } else {
            return Err("step::from_str: fourth token s/b Alt:".to_string());
        }

        let ir_rule = SomeRule::new_region_to_region_min(&initial, &result);

        if tokens[4].to_lowercase() == "none" {
            Ok(Self {
                act_id,
                initial,
                result: ir_rule.result_region(),
                rule: ir_rule,
                alt_rule: AltRuleHint::NoAlt {},
            })
        } else if tokens[4].to_lowercase() == "nochange" {
            Ok(Self {
                act_id,
                initial,
                result: ir_rule.result_region(),
                rule: ir_rule,
                alt_rule: AltRuleHint::AltNoChange {},
            })
        } else {
            let arule = SomeRule::from_str(&tokens[4])?;
            if arule.num_bits() != initial.num_bits() {
                return Err(
                    "step::from_str: initial/result and alt rule use a different number of bits"
                        .to_string(),
                );
            }
            if arule.initial_region() != initial {
                return Err(
                    "step::from_str: alt rule initial region must be eq initial region."
                        .to_string(),
                );
            }
            if arule == ir_rule {
                return Err(
                    "step::from_str: alt rule cannot be eq initial->result rule".to_string()
                );
            }
            Ok(Self::new(act_id, ir_rule, Some(&arule)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::step::SomeStep;
    use std::str::FromStr;

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_stp = SomeStep::from_str("STP[s0000 -1> s0010 Alt: None]")?;

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_stp = SomeStep::from_str("STP[s0010 -0> s0010 Alt: NoChange]")?;

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_stp = SomeStep::from_str("STP[s0000 -1> s0010 Alt: 00/00/01/01]")?;
        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_stp = SomeStep::from_str("STP[s0000_0000 -0> s0000_0000 Alt: None]")?;

        let strrep = format!("{tmp_stp}");
        let len = strrep.len();
        let calc_len = tmp_stp.strlen();
        println!("str {tmp_stp} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn nop() -> Result<(), String> {
        let tmp_reg = SomeRegion::from_str("r0X0X")?;
        let tmp_stp = SomeStep::new_no_op(&tmp_reg);
        println!("nop step {tmp_stp}");

        assert!(tmp_stp.initial == SomeRegion::from_str("r0X0X")?);
        assert!(tmp_stp.initial == tmp_stp.result);
        assert!(tmp_stp.act_id == 0);

        Ok(())
    }
}
