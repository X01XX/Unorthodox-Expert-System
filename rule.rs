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

use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;

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
    pub fn new(initial: &SomeState, result: &SomeState) -> Self {
        assert!(initial.num_ints() == result.num_ints());
        Self {
            b00: initial
                .bitwise_not()
                .bitwise_and(&result.bitwise_not())
                .to_mask(),
            b01: initial.bitwise_not().bitwise_and(result).to_mask(),
            b11: initial.bitwise_and(result).to_mask(),
            b10: initial.bitwise_and(&result.bitwise_not()).to_mask(),
        }
    }

    /// Generate a rule from a number of integers and a string,
    /// like (2, "00/01/11/10/XX/xx/Xx/xX/X0/X1").
    /// Leading "00" tokens can be omitted.
    /// If no tokens supplied, the rule will be all "00".
    pub fn new_from_string(num_ints: usize, rep: &str) -> Result<Self, String> {
        assert!(num_ints > 0);
        let mut b00not = SomeMask::new_low(num_ints); // If starting with all bits set, left-shift will be a problem.
        let mut b01 = SomeMask::new_low(num_ints);
        let mut b11 = SomeMask::new_low(num_ints);
        let mut b10 = SomeMask::new_low(num_ints);

        let mut token = String::with_capacity(2);

        for bt in rep.graphemes(true) {
            if bt == "/" || bt == "_" {
                continue;
            }
            token.push_str(bt);
            if token.len() == 2 {
                if token == "00" {
                    b00not = b00not.push_0();
                    b01 = b01.push_0();
                    b11 = b11.push_0();
                    b10 = b10.push_0();
                } else if token == "01" {
                    b00not = b00not.push_1();
                    b01 = b01.push_1();
                    b11 = b11.push_0();
                    b10 = b10.push_0();
                } else if token == "11" {
                    b00not = b00not.push_1();
                    b01 = b01.push_0();
                    b11 = b11.push_1();
                    b10 = b10.push_0();
                } else if token == "10" {
                    b00not = b00not.push_1();
                    b01 = b01.push_0();
                    b11 = b11.push_0();
                    b10 = b10.push_1();
                } else if token == "XX" || token == "xx" {
                    b00not = b00not.push_0();
                    b01 = b01.push_0();
                    b11 = b11.push_1();
                    b10 = b10.push_0();
                } else if token == "Xx" || token == "xX" {
                    b00not = b00not.push_1();
                    b01 = b01.push_1();
                    b11 = b11.push_0();
                    b10 = b10.push_1();
                } else if token == "X0" || token == "x0" {
                    b00not = b00not.push_0();
                    b01 = b01.push_0();
                    b11 = b11.push_0();
                    b10 = b10.push_1();
                } else if token == "X1" || token == "x1" {
                    b00not = b00not.push_1();
                    b01 = b01.push_1();
                    b11 = b11.push_1();
                    b10 = b10.push_0();
                } else {
                    return Err(format!("unrecognized token {}", &token));
                }
                token.clear();
            }
        }
        if !token.is_empty() {
            return Err(format!("Did not understand token {}", &token));
        }

        Ok(SomeRule {
            b00: b00not.bitwise_not(),
            b01,
            b11,
            b10,
        })
    }

    /// Return true if a rule is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        let Some(tmprul) = self.intersection(other) else { return false; };

        *self == tmprul
    }

    /// Return true if a rule is a superset of another.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        let Some(tmprul) = self.intersection(other) else { return false; };
        *other == tmprul
    }

    /// Return true is a rule is valid
    pub fn is_valid(&self) -> bool {
        self.is_valid_union() && self.is_valid_intersection()
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

        SomeRegion::new(st_high, st_low)
    }

    /// Return the result region of a rule.
    pub fn result_region(&self) -> SomeRegion {
        let st_high = self.b11.bitwise_or(&self.b01).to_state();
        let st_low = self.b00.bitwise_or(&self.b10).bitwise_not().to_state();

        SomeRegion::new(st_high, st_low)
    }

    /// Return the result region after applying an initial region to a rule.
    /// This could be called "forward chaining".
    pub fn result_from_initial_region(&self, reg: &SomeRegion) -> SomeRegion {
        if !reg.intersects(&self.initial_region()) {
            panic!("result_from_initial: given region does not intersect the ruls initial region");
        }

        self.restrict_initial_region(reg).result_region()
    }

    /// Return the result region after applying an initial state to a rule.
    /// This could be called "forward chaining".
    pub fn result_from_initial_state(&self, sta: &SomeState) -> SomeState {
        if !self.initial_region().is_superset_of_state(sta) {
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

        let Some(reg_int) = regx.intersection(&init_reg) else {
            panic!(
                "{regx} does not intersect rule initial region {init_reg}");
        };

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
        //println!("restricting result region of {} to {}", &self, &regx);

        let rslt_reg = self.result_region();

        let Some(reg_int) = regx.intersection(&rslt_reg) else {
            panic!(
                "{regx} does not intersect rule result region {rslt_reg}");
        };

        let zeros = reg_int.low_state().bitwise_not().to_mask();
        let ones = reg_int.high_state().to_mask();

        Self {
            b00: self.b00.bitwise_and(&zeros),
            b01: self.b01.bitwise_and(&ones),
            b11: self.b11.bitwise_and(&ones),
            b10: self.b10.bitwise_and(&zeros),
        }
    }

    /// Return the expected length of a string representation of SomeRule.
    pub fn formatted_string_length(&self) -> usize {
        (self.b00.num_bits() * 3) - 1
    }

    /// Return a string representation of SomeRule.
    pub fn formatted_string(&self) -> String {
        let mut strrc = String::with_capacity(self.formatted_string_length());

        let num_bits = self.b00.num_bits();

        for i in (0..num_bits).rev() {
            let b00: bool = self.b00.is_bit_set(i);
            let b01: bool = self.b01.is_bit_set(i);
            let b11: bool = self.b11.is_bit_set(i);
            let b10: bool = self.b10.is_bit_set(i);

            if i != (num_bits - 1) {
                if (i + 1) % 4 == 0 {
                    strrc.push('_');
                } else {
                    strrc.push('/');
                }
            }

            if b00 && !b01 && b11 && !b10 {
                strrc.push_str("XX");
            } else if b00 && !b01 && !b11 && b10 {
                strrc.push_str("X0");
            } else if !b00 && b01 && b11 && !b10 {
                strrc.push_str("X1");
            } else if !b00 && b01 && !b11 && b10 {
                strrc.push_str("Xx");
            } else if b00 && !b01 && !b11 && !b10 {
                strrc.push_str("00");
            } else if !b00 && !b01 && b11 && !b10 {
                strrc.push_str("11");
            } else if !b00 && !b01 && !b11 && b10 {
                strrc.push_str("10");
            } else if !b00 && b01 && !b11 && !b10 {
                strrc.push_str("01");
            } else if b00 && b01 && !b11 && !b10 {
                strrc.push_str("0X");
            } else if !b00 && !b01 && b11 && b10 {
                strrc.push_str("1X");
            } else if !b00 && !b01 && !b11 && !b10 {
                // Return a new Square instance
                strrc.push_str("dc");
            } else {
                strrc.push_str("**");
            }
        } // next i

        strrc
    }

    /// Of the changes in a rule, the rule initial region may be manipulated to
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
    pub fn parse_for_changes(&self, change_needed: &SomeChange) -> Option<Self> {
        let cng_int = change_needed.bitwise_and_rule(self);

        if cng_int.is_low() {
            // No change, or no change is needed
            return None;
        }

        // Get rule initial region and x mask.
        let mut i_reg = self.initial_region();
        let i_reg_xes = i_reg.x_mask();

        // Figure region bit positions to change from X to 1
        let to_ones = i_reg_xes.bitwise_and(&cng_int.b01);

        if to_ones.is_not_low() {
            i_reg = i_reg.set_to_zeros(&to_ones);
        }

        // Figure region bit positions to change from X to 0
        let to_zeros = i_reg_xes.bitwise_and(&cng_int.b10);

        if to_zeros.is_not_low() {
            i_reg = i_reg.set_to_ones(&to_zeros);
        }

        // Return a restricted rule
        Some(self.restrict_initial_region(&i_reg))
    }

    /// Return true if two rules are mutually exclusive.
    /// Both rules lose wanted changes, running them in any order.
    pub fn mutually_exclusive(&self, other: &SomeRule, wanted: &SomeChange) -> bool {
        if self.order_bad(other, wanted) && other.order_bad(self, wanted) {
            return true;
        }
        false
    }

    /// Return true if the target rule, run before the second rule, will lose all desired changes.
    ///
    ///    A change can be lost by:
    ///        A wanted 0->1 change in rule1 (self) corresponds with a 0 in the initial-region of step2.
    ///        A wanted 1->0 change in rule1 (self) corresponds with a 1 in the initial region of step2.
    pub fn order_bad(&self, other: &SomeRule, wanted: &SomeChange) -> bool {
        // println!("order_bad: {} to {} change wanted {}", &self.formatted_string(), &step2.formatted_string(), &wnated.formatted_string());

        // Calc aggregate rule.
        let rulx = self.then_to(other);

        // Get a mask of the wanted changes in this rule.
        let s_wanted = wanted.bitwise_and_rule(self);

        // Get a mask of the wanted changes after running both rules.
        let a_wanted = wanted.bitwise_and_rule(&rulx);

        // Get a mask of wanted changes in this rule that remain after running the second rule.
        let rslt = s_wanted.bitwise_and(&a_wanted);

        // Return true, the oreder is bad, if no wanted changes remain.
        rslt.is_low()
    }

    /// Combine two rules in sequence.
    /// The result region of the first rule may not intersect the initial region of the second rule.
    pub fn then_to(&self, other: &SomeRule) -> Self {
        if self.result_region().intersects(&other.initial_region()) {
            return self.then_to2(other);
        }
        let rul_between =
            SomeRule::region_to_region(&self.result_region(), &other.initial_region());
        self.then_to2(&rul_between).then_to2(other)
    }

    /// Combine two rules in sequence.
    /// The result region of the first rule must intersect the initial region of the second rule.
    fn then_to2(&self, other: &SomeRule) -> Self {
        assert!(self.result_region().intersects(&other.initial_region()));

        Self {
            b00: self
                .b00
                .bitwise_and(&other.b00)
                .bitwise_or(&self.b01.bitwise_and(&other.b10)),
            b01: self
                .b01
                .bitwise_and(&other.b11)
                .bitwise_or(&self.b00.bitwise_and(&other.b01)),
            b11: self
                .b11
                .bitwise_and(&other.b11)
                .bitwise_or(&self.b10.bitwise_and(&other.b01)),
            b10: self
                .b10
                .bitwise_and(&other.b00)
                .bitwise_or(&self.b11.bitwise_and(&other.b10)),
        }
    }

    /// Return a rule for translating from region to region.
    /// The result of the rule may be equal to, or subset of (1->1 instead of 1->X,
    /// 0->0 instead of 0->X), the second region.
    /// The minimum changes are sought, so X->x and x->X become X->X.
    pub fn region_to_region(from: &SomeRegion, to: &SomeRegion) -> SomeRule {
        let from_x = from.x_mask();
        let from_1 = from.ones_mask().bitwise_or(&from_x);
        let from_0 = from.zeros_mask().bitwise_or(&from_x);

        let to_x = to.x_mask();
        let to_1 = to.ones_mask();
        let to_0 = to.zeros_mask();

        Self {
            b00: from_0.bitwise_and(&to_0.bitwise_or(&to_x)),
            b01: from_0.bitwise_and(&to_1),
            b11: from_1.bitwise_and(&to_1.bitwise_or(&to_x)),
            b10: from_1.bitwise_and(&to_0),
        }
    }

    /// Return a rule for translating from region to state.
    pub fn region_to_state(from: &SomeRegion, to: &SomeState) -> SomeRule {
        let from_x = from.x_mask();
        let from_1 = from.ones_mask().bitwise_or(&from_x);
        let from_0 = from.zeros_mask().bitwise_or(&from_x);

        let to_0 = to.bitwise_not();

        Self {
            b00: from_0.bitwise_and(&to_0),
            b01: from_0.bitwise_and(to),
            b11: from_1.bitwise_and(to),
            b10: from_1.bitwise_and(&to_0),
        }
    }

    /// Return a rule for translating from state to region.
    pub fn state_to_region(from: &SomeState, to: &SomeRegion) -> SomeRule {
        let from_0 = from.bitwise_not();

        let to_x = to.x_mask();
        let to_1 = to.ones_mask();
        let to_0 = to.zeros_mask();

        Self {
            b00: to_0.bitwise_or(&to_x).bitwise_and(&from_0),
            b01: to_1.bitwise_and(&from_0),
            b11: to_1.bitwise_or(&to_x).bitwise_and(from),
            b10: to_0.bitwise_and(from),
        }
    }

    /// Return a rule for translating from state to state.
    pub fn state_to_state(from: &SomeState, to: &SomeState) -> SomeRule {
        let from_0 = from.to_mask().bitwise_not();

        let to_0 = to.to_mask().bitwise_not();

        Self {
            b00: from_0.bitwise_and(&to_0),
            b01: from_0.bitwise_and(to),
            b11: from.bitwise_and(to).to_mask(),
            b10: to_0.bitwise_and(from),
        }
    }
} // end impl SomeRule

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_all() -> Result<(), String> {
        let rule_from_states = SomeRule::new(
            &SomeState::new_from_string(1, "s0b0101").unwrap(),
            &SomeState::new_from_string(1, "s0b0011").unwrap(),
        );

        let rule_from_masks = SomeRule {
            b00: SomeMask::new_from_string(1, "m0xf8").unwrap(),
            b01: SomeMask::new_from_string(1, "m0b0010").unwrap(),
            b11: SomeMask::new_from_string(1, "m0b0001").unwrap(),
            b10: SomeMask::new_from_string(1, "m0b0100").unwrap(),
        };

        let rule_from_string = SomeRule::new_from_string(1, "00/10/01/11").unwrap();

        if rule_from_states != rule_from_masks {
            return Err(format!(
                "rule_from_states {} != rule_from_masks {} ?",
                rule_from_states, rule_from_masks
            ));
        }

        if rule_from_states != rule_from_string {
            return Err(format!(
                "rule_from_stats {} != rule_from_string {} ?",
                rule_from_states, rule_from_string
            ));
        }

        Ok(())
    }

    #[test]
    fn initial_region_result_region() -> Result<(), String> {
        let sta = SomeState::new_from_string(1, "s0b1010").unwrap();
        let st6 = SomeState::new_from_string(1, "s0b0110").unwrap();
        let regx = SomeRule::new(&sta, &SomeState::new_from_string(1, "s0b1001").unwrap());
        let regy = SomeRule::new(&st6, &SomeState::new_from_string(1, "s0b0101").unwrap());

        if regx.initial_region() != SomeRegion::new(sta.clone(), sta.clone()) {
            return Err(String::from("Region not r1010?"));
        }

        let regz = regx.union(&regy).unwrap();
        if regz.initial_region() != SomeRegion::new_from_string(1, "rXX10").unwrap() {
            return Err(String::from("Region not rXX10?"));
        }

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(4, "01/01/01/00/00/00/11/11/11/10/10/10/XX/XX/XX/XX/XX/Xx/Xx/Xx/Xx/Xx/X0/X0/X0/X0/X0/X1/X1/X1/X1/X1").unwrap();
        let rul2 = SomeRule::new_from_string(4, "01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx").unwrap();
        let rul3 = SomeRule::new_from_string(4, "01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11").unwrap();

        if rul1.intersection(&rul2) != Some(rul3) {
            return Err(format!("intersection rul1 and rul2 ne rul3?"));
        }

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let rul2 = SomeRule::new_from_string(4, "01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx").unwrap();
        let rul3 = SomeRule::new_from_string(4, "01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11").unwrap();

        if !rul3.is_subset_of(&rul2) {
            return Err(String::from("Result 1 false?"));
        }

        Ok(())
    }

    #[test]
    fn is_valid_intersection() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "XX").unwrap();
        let rul2 = SomeRule::new_from_string(1, "X1").unwrap();
        let rul3 = SomeRule::new_from_string(1, "00").unwrap();

        let Some(_int12) = rul1.intersection(&rul2) else {
            return Err(String::from("Result 1 False?"));
        };

        if let Some(_int23) = rul2.intersection(&rul3) {
            return Err(String::from("Result 2  True?"));
        }

        Ok(())
    }

    #[test]
    fn is_valid_union() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "00").unwrap();
        let rul2 = SomeRule::new_from_string(1, "01").unwrap();
        if rul1.union(&rul2).is_some() {
            return Err(String::from("Result 1 True?"));
        }

        let rul1 = SomeRule::new_from_string(1, "11").unwrap();
        let rul2 = SomeRule::new_from_string(1, "10").unwrap();
        if rul1.union(&rul2).is_some() {
            return Err(String::from("Result 2 True?"));
        }

        let rul1 = SomeRule::new_from_string(1, "11").unwrap();
        let rul2 = SomeRule::new_from_string(1, "01").unwrap();
        if rul1.union(&rul2).is_none() {
            return Err(String::from("Result 3 False?"));
        }

        let rul1 = SomeRule::new_from_string(1, "x1").unwrap();
        let rul2 = SomeRule::new_from_string(1, "00").unwrap();
        if rul1.union(&rul2).is_some() {
            return Err(String::from("Result 4 True?"));
        }

        Ok(())
    }

    #[test]
    fn mutually_exclusive() -> Result<(), String> {
        // The results of rules, (10 vs 01) do not intersect the initial regions (00, 00).
        // Running either rule precludes running the other.
        let rul1 = SomeRule::new_from_string(1, "01/00").unwrap();
        let rul2 = SomeRule::new_from_string(1, "00/01").unwrap();
        let chg1 = SomeChange::new(
            SomeMask::new_from_string(1, "m0b11").unwrap(),
            SomeMask::new_low(1),
        );
        if !rul1.mutually_exclusive(&rul2, &chg1) {
            return Err(String::from("Result 1 False?"));
        }

        // The results of rules (10, 01) intersect one of the initial regions (00, 10),
        // so one rul1 should be run before rul2.
        let rul1 = SomeRule::new_from_string(1, "01/00").unwrap();
        let rul2 = SomeRule::new_from_string(1, "10/01").unwrap();
        let chg1 = SomeChange::new(
            SomeMask::new_from_string(1, "m0b11").unwrap(),
            SomeMask::new_low(1),
        );
        if !rul1.mutually_exclusive(&rul2, &chg1) {
            return Err(String::from("Result 2 False?"));
        }

        // The results of rules (1X, X0) intersects both of the initial regions (XX, XX),
        // so either rule can be run before the other.
        let rul1 = SomeRule::new_from_string(1, "x1/xx").unwrap();
        let rul2 = SomeRule::new_from_string(1, "xx/x0").unwrap();
        let chg1 = SomeChange::new(
            SomeMask::new_from_string(1, "m0b11").unwrap(),
            SomeMask::new_low(1),
        );
        if rul1.mutually_exclusive(&rul2, &chg1) {
            return Err(String::from("Result 3 True?"));
        }

        Ok(())
    }

    #[test]
    fn order_bad() -> Result<(), String> {
        // The results of rules (10, 01) intersect one of the initial regions (00, 10),
        // so one rul1 should be run before rul2.
        let rul1 = SomeRule::new_from_string(1, "01/00").unwrap();
        let rul2 = SomeRule::new_from_string(1, "10/01").unwrap();
        let rul3 = SomeRule::new_from_string(1, "xx/xx").unwrap();
        let chg1 = SomeChange::new(
            SomeMask::new_from_string(1, "m0b11").unwrap(),
            SomeMask::new_low(1),
        );

        println!("1->2 {}", rul1.order_bad(&rul2, &chg1));
        println!("2->1 {}", rul2.order_bad(&rul1, &chg1));
        if !rul1.order_bad(&rul2, &chg1) {
            return Err(String::from("Result 1 False?"));
        }
        if !rul2.order_bad(&rul1, &chg1) {
            return Err(String::from("Result 2 False?"));
        }
        if rul1.order_bad(&rul3, &chg1) {
            return Err(String::from("Result 3 True?"));
        }
        if rul2.order_bad(&rul3, &chg1) {
            return Err(String::from("Result 4 True?"));
        }

        Ok(())
    }

    #[test]
    fn parse_for_changes() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "X1/X0/Xx/Xx").unwrap();
        let chg1 = SomeChange::new(
            SomeMask::new_from_string(1, "m0b1010").unwrap(),
            SomeMask::new_from_string(1, "m0b0101").unwrap(),
        );
        let rul2 = rul1.parse_for_changes(&chg1).unwrap();

        println!("rul2 {}", &rul2);

        if rul2 != SomeRule::new_from_string(1, "01/10/01/10").unwrap() {
            return Err(String::from("rul2 not 01/10/01/10?"));
        }

        Ok(())
    }

    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "X1/X0/Xx/Xx").unwrap();
        let rul2 = rul1.restrict_initial_region(&SomeRegion::new_from_string(1, "r10X1").unwrap());

        println!("rul2 = {}", rul2);
        if rul2 != SomeRule::new_from_string(1, "11/00/Xx/10").unwrap() {
            return Err(String::from("rul2 not 11/00/Xx/10?"));
        }

        Ok(())
    }

    #[test]
    fn restrict_result_region() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "Xx/Xx/XX/XX").unwrap();
        let rul2 = rul1.restrict_result_region(&SomeRegion::new_from_string(1, "r1010").unwrap());

        println!("rul = {}", rul2);
        if rul2 != SomeRule::new_from_string(1, "01/10/11/00").unwrap() {
            return Err(String::from("rul2 not 01/10/11/00?"));
        }

        Ok(())
    }

    // Also tests result_region
    #[test]
    fn result_from_initial_region() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "Xx/XX/x1/x0/xx/xx").unwrap();
        let reg1 = SomeRegion::new_from_string(1, "rx00110").unwrap();

        let reg2 = rul1.result_from_initial_region(&reg1);
        println!("reg2 {}", reg2);

        if reg2 != SomeRegion::new_from_string(1, "rX01010").unwrap() {
            return Err(String::from("reg2 not X01010?"));
        }

        Ok(())
    }

    #[test]
    fn result_from_initial_state() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "Xx/XX/x1/x0/xx/xx").unwrap();
        let sta1 = SomeState::new_from_string(1, "s0b000110").unwrap();

        let sta2 = rul1.result_from_initial_state(&sta1);
        println!("rul1 {}", &rul1);
        println!("sta2 {}", &sta2);

        if sta2 != SomeState::new_from_string(1, "s0b101010").unwrap() {
            return Err(String::from("sta2 not 001010?"));
        }

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "00/01/00/01/xx").unwrap();
        let rul2 = SomeRule::new_from_string(1, "00/01/10/10/11").unwrap();
        let rul3 = rul1.union(&rul2).unwrap();

        println!("rul3 = {}", &rul3);
        if rul3 != SomeRule::new_from_string(1, "00/01/x0/Xx/xx").unwrap() {
            return Err(String::from("rul3 not 00/01/x0/Xx/xx?"));
        }
        Ok(())
    }

    #[test]
    fn region_to_region() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string(2, "r000_111_xxx").unwrap();
        let reg2 = SomeRegion::new_from_string(2, "r01x_01x_01x").unwrap();

        let rul1 = SomeRule::region_to_region(&reg1, &reg2);
        println!("rul1 is {}", &rul1);
        if !reg2.is_superset_of(&rul1.result_from_initial_region(&reg1)) {
            return Err(String::from("reg2 not superset rul1 initial region?"));
        }

        // Test proper subset region.
        let reg1 = SomeRegion::new_from_string(1, "r0011").unwrap();
        let reg2 = SomeRegion::new_from_string(1, "rx01x").unwrap();
        let rul2 = SomeRule::region_to_region(&reg1, &reg2);
        println!("rul2 is {}", &rul2);
        if rul2.result_region() != reg1 {
            return Err(String::from("Result not r0011?"));
        }

        // Test intersecting regions.
        let reg1 = SomeRegion::new_from_string(1, "r010x").unwrap();
        let reg2 = SomeRegion::new_from_string(1, "rx1x1").unwrap();
        let rul1 = SomeRule::region_to_region(&reg1, &reg2);
        println!("rul1 {}", &rul1);
        if rul1.result_region() != SomeRegion::new_from_string(1, "r0101").unwrap() {
            return Err(String::from("rul1 esult ne r0101?"));
        }

        Ok(())
    }

    #[test]
    fn region_to_state() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string(1, "r0101xx").unwrap();
        let sta1 = SomeState::new_from_string(1, "s0b011010").unwrap();

        let rul1 = SomeRule::region_to_state(&reg1, &sta1);
        let rul2 = SomeRule::new_from_string(1, "00/11/01/10/x1/x0").unwrap();
        println!("rul1 {}", &rul1);
        println!("rul2 {}", &rul2);
        if rul1 != rul2 {
            return Err(String::from("rul1 != rul2?"));
        }

        Ok(())
    }

    #[test]
    fn state_to_region() -> Result<(), String> {
        let sta1 = SomeState::new_from_string(1, "s0b011010").unwrap();
        let reg1 = SomeRegion::new_from_string(1, "r0101xx").unwrap();

        let rul1 = SomeRule::state_to_region(&sta1, &reg1);
        let rul2 = SomeRule::new_from_string(1, "00/11/10/01/11/00").unwrap();
        println!("rul1 {}", &rul1);
        println!("rul2 {}", &rul2);
        if rul1 != rul2 {
            return Err(String::from("rul1 ne rul2?"));
        }

        Ok(())
    }

    #[test]
    fn state_to_state() -> Result<(), String> {
        let sta1 = SomeState::new_from_string(1, "s0b0110").unwrap();
        let sta2 = SomeState::new_from_string(1, "s0b0101").unwrap();

        let rul1 = SomeRule::state_to_state(&sta1, &sta2);
        let rul2 = SomeRule::new_from_string(1, "00/11/10/01").unwrap();
        println!("rul1 {}", &rul1);
        println!("rul2 {}", &rul2);
        if rul1 != rul2 {
            return Err(String::from("rul1 ne rul2?"));
        }

        Ok(())
    }
} // end tests
