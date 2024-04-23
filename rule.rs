//! Rule struct for, representing a change of bits between aninitial and result state.
//!
//! Rules for two squares can be combined to create a rule for a SomeGroup struct.
//!
//! If the initial region of two groups intersect, and intersection of their rules can be taken,
//! which may be valid, invalid, or partially valid.
//!
//! In combining rules, the results 0->X and 1->X for a bit position are disallowed, since
//! there has to be some limit on combination that leaves a rule with predictive power.
//!
//! The rule can be used in a way that is like "forward chaining" (result_from_initial) and
//! "backward chaining" (intitial_from_result).

use crate::change::{AccessChanges, SomeChange};
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::tools::{not, StrLen};

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct SomeRule {
    /// A mask for bit change 0->0
    pub b00: SomeMask,
    /// A mask for bit change 0->1
    pub b01: SomeMask,
    /// A mask for bit change 1->1
    pub b11: SomeMask,
    /// A mask for bit change 1->0
    pub b10: SomeMask,
}

impl fmt::Display for SomeRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeRule {
    /// Return a new SomeRule instance given an initial state and the corresponding result state.
    pub fn new(smpl: &SomeSample) -> Self {
        Self {
            b00: smpl
                .initial
                .bitwise_not()
                .bitwise_and(&smpl.result.bitwise_not())
                .to_mask(),
            b01: smpl
                .initial
                .bitwise_not()
                .bitwise_and(&smpl.result)
                .to_mask(),
            b11: smpl.initial.bitwise_and(&smpl.result).to_mask(),
            b10: smpl
                .initial
                .bitwise_and(&smpl.result.bitwise_not())
                .to_mask(),
        }
    }

    /// Generate a rule from a string.
    /// Al bit positions must be specified.
    /// like SomeRule::new_from_string("00/01/11/10/XX/xx/Xx/xX/X0/X1")
    pub fn new_from_string(rep: &str) -> Result<Self, String> {
        let mut b00 = String::from("m0b");
        let mut b01 = String::from("m0b");
        let mut b11 = String::from("m0b");
        let mut b10 = String::from("m0b");

        let mut token = String::with_capacity(2);

        for bt in rep.graphemes(true) {
            if bt == "/" || bt == "_" {
                continue;
            }
            token.push_str(bt);
            if token.len() == 2 {
                if token == "00" {
                    b00.push('1');
                    b01.push('0');
                    b11.push('0');
                    b10.push('0');
                } else if token == "01" {
                    b00.push('0');
                    b01.push('1');
                    b11.push('0');
                    b10.push('0');
                } else if token == "11" {
                    b00.push('0');
                    b01.push('0');
                    b11.push('1');
                    b10.push('0');
                } else if token == "10" {
                    b00.push('0');
                    b01.push('0');
                    b11.push('0');
                    b10.push('1');
                } else if token == "XX" || token == "xx" {
                    b00.push('1');
                    b01.push('0');
                    b11.push('1');
                    b10.push('0');
                } else if token == "Xx" || token == "xX" {
                    b00.push('0');
                    b01.push('1');
                    b11.push('0');
                    b10.push('1');
                } else if token == "X0" || token == "x0" {
                    b00.push('1');
                    b01.push('0');
                    b11.push('0');
                    b10.push('1');
                } else if token == "X1" || token == "x1" {
                    b00.push('0');
                    b01.push('1');
                    b11.push('1');
                    b10.push('0');
                } else {
                    return Err(format!("unrecognized token {}", &token));
                }
                token.clear();
            }
        }
        if token.is_empty() {
        } else {
            return Err(format!("Did not understand token {}", &token));
        }

        let b00 = SomeMask::new_from_string(&b00)?;
        let b01 = SomeMask::new_from_string(&b01)?;
        let b11 = SomeMask::new_from_string(&b11)?;
        let b10 = SomeMask::new_from_string(&b10)?;

        Ok(Self { b00, b01, b11, b10 })
    }

    /// Return true if a rule is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        let Some(tmprul) = self.intersection(other) else {
            return false;
        };

        *self == tmprul
    }

    /// Return true if a rule is a superset of another.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        let Some(tmprul) = self.intersection(other) else {
            return false;
        };
        *other == tmprul
    }

    /// Return true if a rule is valid after a union (no 1X or 0X bits)
    pub fn is_valid_union(&self) -> bool {
        self.b00.bitwise_and(&self.b01).is_low() && self.b11.bitwise_and(&self.b10).is_low()
    }

    /// Return true if a rule is valid after an intersection,
    /// that is no bit positions are zero in all 4 masks.
    pub fn is_valid_intersection(&self) -> bool {
        self.b00
            .bitwise_or(&self.b01.bitwise_or(&self.b11.bitwise_or(&self.b10)))
            .is_high()
    }

    /// Return a valid union, if possible, by restricting
    /// the initial region as needed to get rid of 0/X and 1/X bit
    /// positions.
    /// XX + X0 -> 00, XX + X1 -> 11.
    /// Xx + X0 -> 10, Xx + X1 -> 01.
    /// X0 + 01 -> 10, X0 + 11 -> 00.
    /// X1 + 00 -> 11, X1 + 10 -> 01.
    /// XX + 01 -> 11, XX + 10 -> 00.
    /// Xx + 00 -> 10, Xx + 11 -> 01.
    pub fn parsed_union(&self, other: &Self) -> Option<Self> {
        let rule2 = Self {
            b00: self.b00.bitwise_or(&other.b00),
            b01: self.b01.bitwise_or(&other.b01),
            b11: self.b11.bitwise_or(&other.b11),
            b10: self.b10.bitwise_or(&other.b10),
        };
        if rule2.is_valid_union() {
            return Some(rule2);
        }

        // Pare down rule.
        let zeros_mask = rule2.b00.bitwise_and(&rule2.b01).bitwise_not();
        let ones_mask = rule2.b11.bitwise_and(&rule2.b10).bitwise_not();

        let rule3 = Self {
            b00: rule2.b00.bitwise_and(&zeros_mask),
            b01: rule2.b01.bitwise_and(&zeros_mask),
            b11: rule2.b11.bitwise_and(&ones_mask),
            b10: rule2.b10.bitwise_and(&ones_mask),
        };
        if rule3.is_valid_intersection() {
            return Some(rule3);
        }
        None
    }

    /// Return a logical OR of two rules. The result may be invalid.
    pub fn union(&self, other: &Self) -> Option<Self> {
        let ret_rule = Self {
            b00: self.b00.bitwise_or(&other.b00),
            b01: self.b01.bitwise_or(&other.b01),
            b11: self.b11.bitwise_or(&other.b11),
            b10: self.b10.bitwise_or(&other.b10),
        };
        if ret_rule.is_valid_union() {
            return Some(ret_rule);
        }
        None
    }

    /// Return a logical AND of two rules.  The result may be invalid.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        let ret_rule = Self {
            b00: self.b00.bitwise_and(&other.b00),
            b01: self.b01.bitwise_and(&other.b01),
            b11: self.b11.bitwise_and(&other.b11),
            b10: self.b10.bitwise_and(&other.b10),
        };
        if ret_rule.is_valid_intersection() {
            return Some(ret_rule);
        }
        None
    }

    /// Return the initial region of a rule.
    pub fn initial_region(&self) -> SomeRegion {
        let st_high = self.b11.bitwise_or(&self.b10).to_state();
        let st_low = self.b00.bitwise_or(&self.b01).bitwise_not().to_state();

        SomeRegion::new(vec![st_high, st_low])
    }

    /// Return the result region of a rule.
    pub fn result_region(&self) -> SomeRegion {
        let st_high = self.b11.bitwise_or(&self.b01).to_state();
        let st_low = self.b00.bitwise_or(&self.b10).bitwise_not().to_state();

        SomeRegion::new(vec![st_high, st_low])
    }

    /// Return the result state of a rule.
    pub fn result_state(&self, astate: &SomeState) -> SomeState {
        assert!(self.initial_region().is_superset_of(astate));

        let sta_cng1 = astate.bitwise_and(&self.b10);
        let sta_cng0 = astate.bitwise_not().bitwise_and(&self.b01);

        astate.bitwise_xor(&sta_cng1.bitwise_or(&sta_cng0))
    }

    /// Return result from an input region.
    pub fn result_from(&self, regx: &SomeRegion) -> SomeRegion {
        assert!(self.initial_region().intersects(regx));

        self.restrict_initial_region(regx).result_region()
    }

    /// Return the result region after applying an initial state to a rule.
    /// This could be called "forward chaining".
    pub fn result_from_initial_state(&self, sta: &SomeState) -> SomeState {
        if !self.initial_region().is_superset_of(sta) {
            panic!(
                "result_from_initial_state: given state is not a subset of the ruls initial region"
            );
        };
        let toggle: SomeMask = self
            .b01
            .bitwise_and(&sta.bitwise_not())
            .bitwise_or(&self.b10.bitwise_and(sta));
        sta.bitwise_xor(&toggle)
    }

    /// Restrict the initial region to an intersection of the
    /// given region.  Assuming the region given is not a superset
    /// this will also change the result region.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Self {
        let init_reg = self.initial_region();

        if not(regx.intersects(&init_reg)) {
            panic!("{regx} does not intersect rule initial region {init_reg}");
        };
        let reg_int = regx.intersection(&init_reg);

        let zeros = reg_int.low_state().bitwise_not().to_mask();
        let ones = reg_int.high_state().to_mask();

        Self {
            b00: self.b00.bitwise_and(&zeros),
            b01: self.b01.bitwise_and(&zeros),
            b11: self.b11.bitwise_and(&ones),
            b10: self.b10.bitwise_and(&ones),
        }
    }

    /// Restrict the result region to an intersection of the
    /// given region.  Assuming the region given is not a superset
    /// this will also change the initial region.
    pub fn restrict_result_region(&self, regx: &SomeRegion) -> Self {
        //println!("restricting result region of {} to {}", self, regx);

        let rslt_reg = self.result_region();

        if not(regx.intersects(&rslt_reg)) {
            panic!("{regx} does not intersect rule result region {rslt_reg}");
        };
        let reg_int = regx.intersection(&rslt_reg);

        let zeros = reg_int.low_state().bitwise_not().to_mask();
        let ones = reg_int.high_state().to_mask();

        Self {
            b00: self.b00.bitwise_and(&zeros),
            b01: self.b01.bitwise_and(&ones),
            b11: self.b11.bitwise_and(&ones),
            b10: self.b10.bitwise_and(&zeros),
        }
    }

    /// Return a string representation of SomeRule.
    fn formatted_string(&self) -> String {
        let mut strrc = String::with_capacity(self.strlen());

        let m00 = format!("{}", self.b00);
        let m01 = format!("{}", self.b01);
        let m11 = format!("{}", self.b11);
        let m10 = format!("{}", self.b10);

        for (((c00, c01), c11), c10) in m00
            .graphemes(true)
            .zip(m01.graphemes(true))
            .zip(m11.graphemes(true))
            .zip(m10.graphemes(true))
        {
            if c00 == "m" {
                continue;
            }
            if c00 == "_" {
                strrc.pop();
                strrc.push('_');
                continue;
            }

            let b00 = c00 == "1";
            let b01 = c01 == "1";
            let b11 = c11 == "1";
            let b10 = c10 == "1";

            if b00 && !b01 && b11 && !b10 {
                strrc.push_str("XX/");
            } else if b00 && !b01 && !b11 && b10 {
                strrc.push_str("X0/");
            } else if !b00 && b01 && b11 && !b10 {
                strrc.push_str("X1/");
            } else if !b00 && b01 && !b11 && b10 {
                strrc.push_str("Xx/");
            } else if b00 && !b01 && !b11 && !b10 {
                strrc.push_str("00/");
            } else if !b00 && !b01 && b11 && !b10 {
                strrc.push_str("11/");
            } else if !b00 && !b01 && !b11 && b10 {
                strrc.push_str("10/");
            } else if !b00 && b01 && !b11 && !b10 {
                strrc.push_str("01/");
            } else if b00 && b01 && !b11 && !b10 {
                strrc.push_str("0X/");
            } else if !b00 && !b01 && b11 && b10 {
                strrc.push_str("1X/");
            } else if !b00 && !b01 && !b11 && !b10 {
                strrc.push_str("dc/");
            } else {
                strrc.push_str("**");
            }
        } // next i

        strrc.pop();
        strrc
    }

    /// Of the wanted changes in a rule, the rule initial region may be manipulated to
    /// focus in on the desired changes.
    ///
    /// The change argument contains masks of changes (b01, b10) that are sought.
    ///
    /// For changes that are sought, for bit positions that are X->1, X->0, or X->x,
    /// the X value can be changed to focus on the desired change.
    ///
    /// X->1 is 1->1 and 0->1, the X can be changed to 0.
    /// X->0 is 0->0 and 1->0, the X can be changed to 1.
    /// X->x is 1->0 and 0->1, the X can be changed to 1 or 0, depending on the change sought.
    ///
    /// For a change not sought:
    /// X->1 is changed to 1->1.
    /// X->0 is changed to 0->0.
    ///
    /// For a restriction on the region the rule should stay within:
    /// X->X may remain the same, or be changed to 1->1 or 0->0.
    /// X->x may remain the same, or it will be an excursion outside the desired region.
    /// A 0 edge for X->x, or 0->1 will result in a 0->1 excursion.
    /// A 1 edge for X->x, or 1->0 will result in a 1->0 excursion.
    pub fn restrict_for_changes(
        &self,
        change_needed: &SomeChange,
        within: Option<&SomeRegion>,
    ) -> Vec<SomeRule> {
        debug_assert!(change_needed.b01.bitwise_and(&change_needed.b10).is_low());

        // Init return RuleStore.
        let mut ret_rules = Vec::<SomeRule>::new();

        // Restrict the rule, if needed.
        let mut s_rule = self.clone();

        if let Some(restrict) = within {
            if s_rule.initial_region().intersects(restrict) {
                s_rule = s_rule.restrict_initial_region(restrict);
            } else {
                return ret_rules;
            }
            if s_rule.result_region().intersects(restrict) {
                s_rule = s_rule.restrict_result_region(restrict);
            } else {
                return ret_rules;
            }
        }

        // Get rule initial region.
        let init_reg = s_rule.initial_region();
        let init_reg_zeros = init_reg.zeros_mask();
        let init_reg_ones = init_reg.ones_mask();
        let init_reg_xs = init_reg.x_mask();

        if change_needed.b01.is_not_low() {
            // Process individual 0->1 changes needed.
            let change_bits = change_needed.b01.split();

            for m01x in change_bits.iter() {
                // Check if the rule applies to the needed change.
                if m01x.bitwise_and(&s_rule.b01).is_low() {
                    continue;
                }

                // Restrict (if needed) and store rule.
                if m01x.bitwise_and(&init_reg_zeros).is_not_low() {
                    if ret_rules.contains(&s_rule) {
                    } else {
                        ret_rules.push(s_rule.clone());
                    }
                } else if m01x.bitwise_and(&init_reg_xs).is_not_low() {
                    let ireg = init_reg.set_to_zeros(m01x);
                    let ruley = s_rule.restrict_initial_region(&ireg);
                    ret_rules.push(ruley);
                }
            }
        }

        if change_needed.b10.is_not_low() {
            // Process individual 1->0 changes needed.
            let change_bits = change_needed.b10.split();

            for m10x in change_bits.iter() {
                // Check if the rule applies to the needed change.
                if m10x.bitwise_and(&s_rule.b10).is_low() {
                    continue;
                }

                // Restrict (if needed) and store rule.
                if m10x.bitwise_and(&init_reg_ones).is_not_low() {
                    if ret_rules.contains(&s_rule) {
                    } else {
                        ret_rules.push(s_rule.clone());
                    }
                } else if m10x.bitwise_and(&init_reg_xs).is_not_low() {
                    let ireg = init_reg.set_to_ones(m10x);
                    let ruley = s_rule.restrict_initial_region(&ireg);
                    ret_rules.push(ruley);
                }
            }
        }

        ret_rules
    }

    /// Return true if two rules are mutually exclusive.
    /// Both rules lose wanted changes, running them in any order.
    ///
    /// For a change to pass from one rule through a second rule:
    ///    A wanted 0->1 change in first rule should correspond to a 1->1 in the second rule.
    ///    A wanted 1->0 change in first rule should correspond to a 0->0 in the second rule.
    pub fn mutually_exclusive(&self, other: &Self, wanted: &SomeChange) -> bool {
        // println!("starting, self {self} other {other} wanted {wanted}");

        self.sequence_blocks_changes(other, wanted) && other.sequence_blocks_changes(self, wanted)
    }

    /// Return true if running a rule after another blocks all wanted changes in the first rule.
    ///
    /// The result region of the first rule may not intersect the initial region of the second rule.
    ///
    /// For a change to pass from one rule through a second rule:
    ///    A wanted 0->1 change in first rule should correspond to a 1->1 in the second rule.
    ///    A wanted 1->0 change in first rule should correspond to a 0->0 in the second rule.
    pub fn sequence_blocks_changes(&self, other: &Self, wanted: &SomeChange) -> bool {
        // println!("sequence_blocks_change: {} to {} change wanted {}", self.formatted_string(), other.formatted_string(), wanted.formatted_string());

        debug_assert!(wanted.is_not_low());

        let msk01 = self.b01.bitwise_and(&other.b11).bitwise_and(&wanted.b01);
        if msk01.is_not_low() {
            return false;
        }

        let msk10 = self.b10.bitwise_and(&other.b00).bitwise_and(&wanted.b10);
        if msk10.is_not_low() {
            return false;
        }

        true
    }

    /// Return a SomeChange instance.
    pub fn change(&self) -> SomeChange {
        SomeChange::new(self.b01.clone(), self.b10.clone())
    }

    /// Return a rule for translating from a region to another region.
    /// The result of the rule may be equal to, or subset of (1->1 instead of 1->X,
    /// 0->0 instead of 0->X), the second region.
    /// The minimum changes are sought, so X->x-not becomes X->X.
    /// It can be thought that:
    /// 0->1 and 1->0 changes are required, but compared to another change may be missing,
    /// or if in the other change may be unwanted.
    /// For X->0, the change is optional, a 0 input will be no change.
    /// For X->1, the change is optional, a 1 input will be no change.
    /// Anything -> X, is a don't care.
    pub fn rule_region_to_region(from: &SomeRegion, to: &SomeRegion) -> SomeRule {
        let from_x = from.x_mask();
        let from_1 = from.ones_mask();
        let from_0 = from.zeros_mask();

        let to_x = to.x_mask();
        let to_1 = to.ones_mask();
        let to_0 = to.zeros_mask();

        let x_to_0 = from_x.bitwise_and(&to_0);
        let x_to_1 = from_x.bitwise_and(&to_1);
        let x_to_x = from_x.bitwise_and(&to_x);
        let zero_to_x = from_0.bitwise_and(&to_x);
        let one_to_x = from_1.bitwise_and(&to_x);

        SomeRule {
            b00: from_0
                .bitwise_and(&to_0)
                .bitwise_or(&x_to_0)
                .bitwise_or(&x_to_x)
                .bitwise_or(&zero_to_x),
            b01: from_0.bitwise_and(&to_1).bitwise_or(&x_to_1),
            b11: from_1
                .bitwise_and(&to_1)
                .bitwise_or(&x_to_1)
                .bitwise_or(&x_to_x)
                .bitwise_or(&one_to_x),
            b10: from_1.bitwise_and(&to_0).bitwise_or(&x_to_0),
        }
    }

    /// Return a rule for translating from a state to a region.
    /// The result of the rule may be equal to, or subset of (1->1 instead of 1->X,
    /// 0->0 instead of 0->X), the second region.
    /// The minimum changes are sought, so X->x-not becomes X->X.
    /// It can be thought that:
    /// 0->1 and 1->0 changes are required, but compared to another change may be missing,
    /// or if in the other change may be unwanted.
    /// For X->0, the change is optional, a 0 input will be no change.
    /// For X->1, the change is optional, a 1 input will be no change.
    /// Anything -> X, is a don't care.
    pub fn rule_state_to_region(from: &SomeState, to: &SomeRegion) -> SomeRule {
        let from_1 = from.to_mask();
        let from_0 = from.bitwise_not().to_mask();

        let to_x = to.x_mask();
        let to_1 = to.ones_mask();
        let to_0 = to.zeros_mask();

        let zero_to_x = from_0.bitwise_and(&to_x);
        let one_to_x = from_1.bitwise_and(&to_x);

        SomeRule {
            b00: from_0.bitwise_and(&to_0).bitwise_or(&zero_to_x),
            b01: from_0.bitwise_and(&to_1),
            b11: from_1.bitwise_and(&to_1).bitwise_or(&one_to_x),
            b10: from_1.bitwise_and(&to_0),
        }
    }

    /// Return the number of bits changed in a rule.
    pub fn num_bits_changed(&self) -> usize {
        self.b01.bitwise_or(&self.b10).num_one_bits()
    }

    /// Return true if a rule causes predictable change.
    pub fn causes_predictable_change(&self) -> bool {
        !(self.b10.is_low() && self.b01.is_low())
    }
} // end impl SomeRule

/// Implement the trait StrLen for SomeRule.
impl StrLen for SomeRule {
    fn strlen(&self) -> usize {
        let len1 = self.b00.strlen();
        if len1 < 6 {
            ((len1 - 1) * 3) - 1 // rightmost position is 2 chars.
        } else {
            let num4 = len1 / 5;
            let mut extra = len1 % 5;
            extra = extra.saturating_sub(1); // factor out the 'm' prefix.
            (num4 * 4 * 3) + (extra * 3) - 1 // rightmost position is 2 chars.
        }
    }
}

impl AccessChanges for SomeRule {
    fn b01(&self) -> &SomeMask {
        &self.b01
    }
    fn b10(&self) -> &SomeMask {
        &self.b10
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::tools;

    #[test]
    fn test_parsed_union() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("XX/XX/Xx/Xx/X0/X0")?;
        let rul2 = SomeRule::new_from_string("X0/X1/X0/X1/01/11")?;
        if let Some(rint) = rul1.parsed_union(&rul2) {
            println!("{} int {} = {}", rul1, rul2, rint);
            assert!(rint == SomeRule::new_from_string("00/11/10/01/10/00")?);
        } else {
            return Err(format!("{} int {} is None?", rul1, rul2));
        }

        let rul1 = SomeRule::new_from_string("X1/X1/XX/XX/Xx/Xx")?;
        let rul2 = SomeRule::new_from_string("00/10/01/10/00/11")?;
        if let Some(rint) = rul1.parsed_union(&rul2) {
            println!("{} int {} = {}", rul1, rul2, rint);
            assert!(rint == SomeRule::new_from_string("11/01/11/00/10/01")?);
        } else {
            return Err(format!("{} int {} is None?", rul1, rul2));
        }

        Ok(())
    }

    #[test]
    fn test_strlen() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_sta = SomeState::new(SomeBits::new(16));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_sta = SomeState::new(SomeBits::new(5));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_sta = SomeState::new(SomeBits::new(4));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn new_all() -> Result<(), String> {
        let rule_from_states = SomeRule::new(&SomeSample::new(
            SomeState::new_from_string("s0b0101")?,
            SomeState::new_from_string("s0b0011")?,
        ));

        let rule_from_masks = SomeRule {
            b00: SomeMask::new_from_string("m0x7")?.bitwise_not(),
            b01: SomeMask::new_from_string("m0b0010")?,
            b11: SomeMask::new_from_string("m0b0001")?,
            b10: SomeMask::new_from_string("m0b0100")?,
        };

        let rule_from_string = SomeRule::new_from_string("00/10/01/11")?;

        println!("rule_from_states: {rule_from_states} rule_from_masks: {rule_from_masks}");
        assert!(rule_from_states == rule_from_masks);

        println!("rule_from_states: {rule_from_states} rule_from_string: {rule_from_string}");
        assert!(rule_from_states == rule_from_string);

        Ok(())
    }

    #[test]
    fn initial_region_result_region() -> Result<(), String> {
        let sta = SomeState::new_from_string("s0b1010")?;
        let st6 = SomeState::new_from_string("s0b0110")?;
        let rulx = SomeRule::new(&SomeSample::new(
            sta.clone(),
            SomeState::new_from_string("s0b1001")?,
        ));
        let ruly = SomeRule::new(&SomeSample::new(
            st6.clone(),
            SomeState::new_from_string("s0b0101")?,
        ));

        println!("rulx: {rulx}");
        assert!(rulx.initial_region() == SomeRegion::new(vec![sta.clone()]));

        println!("ruly: {ruly}");
        if let Some(rulz) = rulx.union(&ruly) {
            println!("rulz: {rulz}");
            assert!(rulz.initial_region() == SomeRegion::new_from_string("rXX10")?);
        } else {
            return Err("Regions should form a union".to_string());
        }

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("01/01/01/00/00/00/11/11/11/10/10/10/XX/XX/XX/XX/XX/Xx/Xx/Xx/Xx/Xx/X0/X0/X0/X0/X0/X1/X1/X1/X1/X1")?;
        let rul2 = SomeRule::new_from_string("01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx")?;
        let rul3 = SomeRule::new_from_string("01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11")?;
        let Some(rul_int) = rul1.intersection(&rul2) else {
            panic!("this should work!");
        };

        println!("rul1: {rul1} rul2: {rul2} rul_int: {rul_int}");
        assert!(rul_int == rul3);

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx")?;
        let rul2 = SomeRule::new_from_string("01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11")?;

        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul2.is_subset_of(&rul1));

        Ok(())
    }

    #[test]
    fn is_valid_intersection() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("XX")?;
        let rul2 = SomeRule::new_from_string("X1")?;
        let rul3 = SomeRule::new_from_string("00")?;

        println!("rul1: {rul1} rul2: {rul2} rul3: {rul3}");
        assert!(rul1.intersection(&rul2).is_some());
        assert!(rul2.intersection(&rul3).is_none());

        Ok(())
    }

    #[test]
    fn is_valid_union() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("00")?;
        let rul2 = SomeRule::new_from_string("01")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_none());

        let rul1 = SomeRule::new_from_string("11")?;
        let rul2 = SomeRule::new_from_string("10")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_none());

        let rul1 = SomeRule::new_from_string("11")?;
        let rul2 = SomeRule::new_from_string("01")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_some());

        let rul1 = SomeRule::new_from_string("x1")?;
        let rul2 = SomeRule::new_from_string("00")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_none());

        Ok(())
    }

    #[test]
    fn mutually_exclusive() -> Result<(), String> {
        let tmp_bts = SomeBits::new(4);
        let tmp_msk = SomeMask::new(tmp_bts.clone());

        // The results of rules, (10 vs 01) do not intersect the initial regions (00, 00).
        // Running either rule precludes running the other.
        let rul1 = SomeRule::new_from_string("00/00/01/00")?;
        let rul2 = SomeRule::new_from_string("00/00/00/01")?;
        let chg1 = SomeChange::new(SomeMask::new_from_string("m0b0011")?, tmp_msk.new_low());
        println!("rul1: {rul1} rul2: {rul2} chg1: {chg1}");
        assert!(rul1.mutually_exclusive(&rul2, &chg1));

        // The results of rules (10, 01) intersect one of the initial regions (00, 10),
        // so one rul1 should be run before rul2.
        let rul1 = SomeRule::new_from_string("00/00/01/00")?;
        let rul2 = SomeRule::new_from_string("00/00/10/01")?;
        let chg1 = SomeChange::new(SomeMask::new_from_string("m0b0011")?, tmp_msk.new_low());
        println!("rul1: {rul1} rul2: {rul2} chg1: {chg1}");
        assert!(rul1.mutually_exclusive(&rul2, &chg1));

        // The results of rules (1X, X0) intersects both of the initial regions (XX, XX),
        // so either rule can be run before the other.
        let rul1 = SomeRule::new_from_string("xx/xx/x1/xx")?;
        let rul2 = SomeRule::new_from_string("xx/xx/xx/x0")?;
        let chg1 = SomeChange::new(SomeMask::new_from_string("m0b0011")?, tmp_msk.new_low());
        println!("rul1: {rul1} rul2: {rul2} chg1: {chg1}");
        assert!(!rul1.mutually_exclusive(&rul2, &chg1));

        Ok(())
    }

    #[test]
    fn sequence_blocks_changes() -> Result<(), String> {
        let tmp_bts = SomeBits::new(4);
        let tmp_msk = SomeMask::new(tmp_bts.clone());

        // The results of rules (10, 01) intersect one of the initial regions (00, 10),
        // so one rul1 should be run before rul2.
        let rul1 = SomeRule::new_from_string("00/00/01/00")?;
        let rul2 = SomeRule::new_from_string("00/00/10/01")?;
        let rul3 = SomeRule::new_from_string("xx/xx/xx/xx")?;
        let chg1 = SomeChange::new(SomeMask::new_from_string("m0b0011")?, tmp_msk.new_low());
        let chg2 = SomeChange::new(tmp_msk.new_low(), SomeMask::new_from_string("m0b0011")?);
        println!("rul1: {rul1} rul2: {rul2} rul3: {rul3} chg1: {chg1}");

        println!("1->2 {}", rul1.sequence_blocks_changes(&rul2, &chg1));
        println!("2->1 {}", rul2.sequence_blocks_changes(&rul1, &chg1));
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));
        assert!(rul2.sequence_blocks_changes(&rul1, &chg2));
        assert!(!rul1.sequence_blocks_changes(&rul3, &chg1));
        assert!(!rul2.sequence_blocks_changes(&rul3, &chg1));

        Ok(())
    }

    #[test]
    fn restrict_for_changes() -> Result<(), String> {
        // Change wanted   X->1 to 0->1.
        // Change depends  X->1 to X->1.
        // Change wanted   X->0 to 1->0.
        // Change depends  X->0 to X->0.
        let rul1 = SomeRule::new_from_string("X1/X1/X0/X0")?;
        let chg1 = SomeChange::new(
            SomeMask::new_from_string("m0b1000")?,
            SomeMask::new_from_string("m0b0010")?,
        );

        let rul2 = rul1.restrict_for_changes(&chg1, None);
        if rul2.is_empty() {
            panic!("rul2 restriction should succeed");
        };
        println!("rul2 {}", tools::vec_string(&rul2));

        assert!(rul2.len() == 2);

        assert!(
            (rul2[0] == SomeRule::new_from_string("01/X1/X0/X0")?
                && rul2[1] == SomeRule::new_from_string("X1/X1/10/X0")?)
                || (rul2[1] == SomeRule::new_from_string("01/X1/X0/X0")?
                    && rul2[0] == SomeRule::new_from_string("X1/X1/10/X0")?)
        );

        // Change X->X to 0->0 by within restriction.
        // Change X->X to 1->1 by within restriction.
        // Leave one X->X unaffected.
        let rul1 = SomeRule::new_from_string("01/XX/XX/XX")?;
        let chg1 = SomeChange::new(
            SomeMask::new_from_string("m0b1000")?,
            SomeMask::new_from_string("m0b0000")?,
        );
        let within = SomeRegion::new_from_string("rxx01")?;

        let rul3 = rul1.restrict_for_changes(&chg1, Some(&within));
        if rul3.is_empty() {
            panic!("rul3 restriction should succeed");
        };
        println!("rul3 {}", tools::vec_string(&rul3));

        assert!(rul3[0] == SomeRule::new_from_string("01/XX/00/11")?);

        // Change X->x to 1->0.
        // Change X->x to 0->1.
        // Leave one X->x unaffected.
        let rul1 = SomeRule::new_from_string("00/Xx/Xx/Xx")?;
        let chg1 = SomeChange::new(
            SomeMask::new_from_string("m0b0010")?,
            SomeMask::new_from_string("m0b0100")?,
        );

        let rul4 = rul1.restrict_for_changes(&chg1, None);
        if rul4.is_empty() {
            panic!("rul4 restriction should succeed");
        };
        println!("rul4 {}", tools::vec_string(&rul4));

        assert!(rul4.len() == 2);

        assert!(
            (rul4[0] == SomeRule::new_from_string("00/Xx/01/Xx")?
                && rul4[1] == SomeRule::new_from_string("00/10/Xx/Xx")?)
                || (rul4[1] == SomeRule::new_from_string("00/Xx/01/Xx")?
                    && rul4[0] == SomeRule::new_from_string("00/10/Xx/Xx")?)
        );

        // Detect X-x excursion.
        let rul1 = SomeRule::new_from_string("01/Xx")?;
        let chg1 = SomeChange::new(
            SomeMask::new_from_string("m0b10")?,
            SomeMask::new_from_string("m0b00")?,
        );

        let within = SomeRegion::new_from_string("rx1")?;

        let rul5 = rul1.restrict_for_changes(&chg1, Some(&within));
        if rul5.is_empty() {
        } else {
            println!("rul5 ? {}", tools::vec_string(&rul5));
            panic!("rul5 restriction should not succeed");
        };

        Ok(())
    }

    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("X1/X0/Xx/Xx")?;
        let rul2 = rul1.restrict_initial_region(&SomeRegion::new_from_string("r10X1")?);

        println!("rul2: {rul2}");
        assert!(rul2 == SomeRule::new_from_string("11/00/Xx/10")?);

        Ok(())
    }

    #[test]
    fn restrict_result_region() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("Xx/Xx/XX/XX")?;
        let rul2 = rul1.restrict_result_region(&SomeRegion::new_from_string("r1010")?);

        println!("rul2: {rul2}");
        assert!(rul2 == SomeRule::new_from_string("01/10/11/00")?);

        Ok(())
    }

    #[test]
    fn result_from_initial_state() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("Xx/XX/x1/x0/xx/xx")?;
        let sta1 = SomeState::new_from_string("s0b000110")?;

        let sta2 = rul1.result_from_initial_state(&sta1);
        println!("rul1: {rul1} sta1: {sta1} sta2: {sta2}");

        assert!(sta2 == SomeState::new_from_string("s0b101010")?);

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("00/01/00/01/xx")?;
        let rul2 = SomeRule::new_from_string("00/01/10/10/11")?;

        let Some(rul3) = rul1.union(&rul2) else {
            panic!("This should work!");
        };
        println!("rul3 = {rul3}");
        assert!(rul3 == SomeRule::new_from_string("00/01/x0/Xx/xx")?);

        Ok(())
    }
} // end tests
