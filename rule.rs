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
use crate::tools::StrLen;

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;

//#[readonly::make]
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

    /// Generate a rule from a number of integers and a string,
    /// like (2, "00/01/11/10/XX/xx/Xx/xX/X0/X1").
    /// Leading "00" tokens can be omitted.
    /// If no tokens supplied, the rule will be all "00".
    pub fn new_from_string(&self, rep: &str) -> Result<Self, String> {
        let mut b00 = self.b00.new_high();
        let mut b01 = self.b00.new_low();
        let mut b11 = self.b00.new_low();
        let mut b10 = self.b00.new_low();

        let mut token = String::with_capacity(2);

        for bt in rep.graphemes(true) {
            if bt == "/" || bt == "_" {
                continue;
            }
            token.push_str(bt);
            if token.len() == 2 {
                if token == "00" {
                    b00 = b00.push_1();
                    b01 = b01.shift_left();
                    b11 = b11.shift_left();
                    b10 = b10.shift_left();
                } else if token == "01" {
                    b00 = b00.shift_left();
                    b01 = b01.push_1();
                    b11 = b11.shift_left();
                    b10 = b10.shift_left();
                } else if token == "11" {
                    b00 = b00.shift_left();
                    b01 = b01.shift_left();
                    b11 = b11.push_1();
                    b10 = b10.shift_left();
                } else if token == "10" {
                    b00 = b00.shift_left();
                    b01 = b01.shift_left();
                    b11 = b11.shift_left();
                    b10 = b10.push_1();
                } else if token == "XX" || token == "xx" {
                    b00 = b00.push_1();
                    b01 = b01.shift_left();
                    b11 = b11.push_1();
                    b10 = b10.shift_left();
                } else if token == "Xx" || token == "xX" {
                    b00 = b00.shift_left();
                    b01 = b01.push_1();
                    b11 = b11.shift_left();
                    b10 = b10.push_1();
                } else if token == "X0" || token == "x0" {
                    b00 = b00.push_1();
                    b01 = b01.shift_left();
                    b11 = b11.shift_left();
                    b10 = b10.push_1();
                } else if token == "X1" || token == "x1" {
                    b00 = b00.shift_left();
                    b01 = b01.push_1();
                    b11 = b11.push_1();
                    b10 = b10.shift_left();
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

        Ok(Self { b00, b01, b11, b10 })
    }

    /// Return the number of bits used in a rule.
    pub fn num_bits(&self) -> usize {
        self.b00.num_bits()
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

        let Some(reg_int) = regx.intersection(&init_reg) else {
            panic!("{regx} does not intersect rule initial region {init_reg}");
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
        //println!("restricting result region of {} to {}", self, regx);

        let rslt_reg = self.result_region();

        let Some(reg_int) = regx.intersection(&rslt_reg) else {
            panic!("{regx} does not intersect rule result region {rslt_reg}");
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

    /// Return a string representation of SomeRule.
    fn formatted_string(&self) -> String {
        let mut strrc = String::with_capacity(self.strlen());

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
    ) -> Option<Self> {
        debug_assert!(change_needed.b01.bitwise_and(&change_needed.b10).is_low());

        // Init working copy of rule.
        let mut rulex = self.clone();

        // Get rule initial region.
        let mut p_reg = self.initial_region();

        // Alter rule if a within-limit is given.
        if let Some(regx) = within {
            // Restrict rule initial region, if needed.
            if regx.is_superset_of(&p_reg) {
            } else {
                p_reg = p_reg.intersection(regx)?;
                rulex = rulex.restrict_initial_region(regx);
            }
            // Check result is within limit.
            if regx.is_superset_of(&rulex.result_region()) {
            } else {
                return None;
            }
        }

        // Check at least one needed change is in rule.
        let cng_int = change_needed.intersection(&rulex);
        if cng_int.is_low() {
            return None;
        }

        // Get rule x-mask.
        let p_reg_xes = p_reg.x_mask();

        // Figure region bit positions to change from X to 1
        let to_ones = p_reg_xes.bitwise_and(&cng_int.b10);
        if to_ones.is_not_low() {
            p_reg = p_reg.set_to_ones(&to_ones);
        }

        // Figure region bit positions to change from X to 0
        let to_zeros = p_reg_xes.bitwise_and(&cng_int.b01);
        if to_zeros.is_not_low() {
            p_reg = p_reg.set_to_zeros(&to_zeros);
        }

        // Form return rule.
        let ret = rulex.restrict_initial_region(&p_reg);

        // Return the restricted rule
        Some(ret)
    }

    /// Return true if two rules are mutually exclusive.
    /// Both rules lose wanted changes, running them in any order.
    pub fn mutually_exclusive(&self, other: &Self, wanted: &SomeChange) -> bool {
        if self.sequence_reverses_change(other, wanted)
            && other.sequence_reverses_change(self, wanted)
        {
            return true;
        }
        false
    }

    /// Return true if the target rule, run before the second rule, will lose one, or more, desired changes.
    ///
    ///    A change can be lost by:
    ///        A wanted 0->1 change in rule1 (self) corresponds with a 0 in the initial-region of step2.
    ///        A wanted 1->0 change in rule1 (self) corresponds with a 1 in the initial region of step2.
    pub fn sequence_reverses_change(&self, other: &Self, wanted: &SomeChange) -> bool {
        // println!("sequence_reverses_change: {} to {} change wanted {}", self.formatted_string(), step2.formatted_string(), wanted.formatted_string());

        debug_assert!(wanted.change_mask().is_low()); // Check for a change in the same position of both masks.

        // Get a mask of the wanted changes in this rule.
        let s_wanted = wanted.intersection(self);
        debug_assert!(s_wanted.is_not_low());

        // Calc aggregate rule.
        let rulx = self.combine_sequence(other);

        // Get a mask of the wanted changes after running both rules.
        let a_wanted = wanted.intersection(&rulx);

        // Get a mask of wanted changes in this rule that remain after running the second rule.
        let rslt = s_wanted.intersection(&a_wanted);

        // Return true, if any wanted changes from the first rule are lost.
        rslt != s_wanted
    }

    /// Combine two rules in sequence.
    /// The result region of the first rule is not required to intersect the initial region of the second rule.
    /// Changes in the first rule may be reversed in the second rule.
    pub fn combine_sequence(&self, other: &Self) -> Self {
        if self.result_region().intersects(&other.initial_region()) {
            return self.combine_pair(other);
        }
        let rul_between = self.result_region().rule_to_region(&other.initial_region());

        self.combine_pair(&rul_between).combine_pair(other)
    }

    /// Combine two rules.
    /// The result region of the first rule must intersect the initial region of the second rule.
    /// Changes in the first rule may be reversed in the second rule.
    pub fn combine_pair(&self, other: &Self) -> Self {
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

    /// Return a SomeChange instance.
    pub fn change(&self) -> SomeChange {
        SomeChange::new(self.b01.clone(), self.b10.clone())
    }
} // end impl SomeRule

/// Implement the trait StrLen for SomeRule.
impl StrLen for SomeRule {
    fn strlen(&self) -> usize {
        (self.b00.num_bits() * 2) + (self.b00.num_bits() - 1)
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

    #[test]
    fn test_wanted_unwanted() -> Result<(), String> {
        // For a given rule.
        // With no intersection of start and rule initial region.
        // With no intersection of rule result region and goal.
        // Calc wanted, unwanted, missing and don't care changes.

        let ur_bits = SomeBits::new(8);
        let ur_sta = SomeState::new(ur_bits.clone());
        let ur_mask = SomeMask::new(ur_bits.clone());
        let ur_rul = SomeRule::new(&SomeSample::new(ur_sta.clone(), ur_sta.clone()));

        // Init start, 0.
        let start = SomeRegion::new(vec![ur_sta.new_from_string("s0000")?]);
        println!("start {start}");

        // Set up goal, 10X1.
        let goal = SomeRegion::new(vec![
            ur_sta.new_from_string("s1001")?,
            ur_sta.new_from_string("s1011")?,
        ]);
        println!("goal {goal}");

        // Random rule.
        let rul1 = ur_rul.new_from_string("01/11/01/10")?; // 5 -> E
        println!("rul1 {rul1}");

        // Make a sequence rule, start to rul1 initial region, to rul1 result region.
        // Given the changes wanted, start to goal, it may not be worth it to translate to the rule
        // initial region.
        let rul2 = start
            .rule_to_region(&rul1.initial_region())
            .combine_pair(&rul1);
        println!("rul2 {rul2}");

        // Figure start to goal, 0 -> 10X1, to contrast with rul2.
        let rul3 = ur_rul.new_from_string("01/00/00/01")?; // 0 -> 10X1
        println!("rul3 {rul3}");

        // Figure mask for changes we care about.
        let care = goal.edge_mask();
        println!("Changes we care about mask {care}");

        // Figure wanted changes.
        let wanted = rul2
            .change()
            .intersection(&rul3.change())
            .bitwise_and(&care);
        println!(
            "wanted change {wanted}, start {start} to {}",
            start.apply_changes(&wanted)
        );
        assert!(
            wanted
                == SomeChange::new(
                    ur_mask.new_from_string("m0b1000")?,
                    ur_mask.new_from_string("m0b0000")?
                )
        ); // b01, b10

        // Figure unwanted changes.
        let unwanted = rul2
            .change()
            .intersection(&rul3.change().bitwise_not())
            .bitwise_and(&care);
        println!(
            "unwanted change {unwanted}, start {start} to {}",
            start.apply_changes(&unwanted)
        );
        assert!(
            unwanted
                == SomeChange::new(
                    ur_mask.new_from_string("m0b0100")?,
                    ur_mask.new_from_string("m0b0000")?
                )
        ); // b01, b10

        // Figure missing change.
        let missing = rul2
            .change()
            .bitwise_not()
            .intersection(&rul3.change())
            .bitwise_and(&care);
        println!(
            "missing change {missing}, start {start} to {}",
            start.apply_changes(&missing)
        );
        assert!(
            missing
                == SomeChange::new(
                    ur_mask.new_from_string("m0b0001")?,
                    ur_mask.new_from_string("m0b0000")?
                )
        ); // b01, b10

        // Detect don't care change.
        let dont = rul2.change().bitwise_and(&goal.x_mask());
        println!("don't care {dont}");
        assert!(
            dont == SomeChange::new(
                ur_mask.new_from_string("m0b0010")?,
                ur_mask.new_from_string("m0b0000")?
            )
        ); // b01, b10

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn combine_sequence() -> Result<(), String> {
        let ur_sta = SomeState::new(SomeBits::new(8));
        let ur_rul = SomeRule::new(&SomeSample::new(ur_sta.clone(), ur_sta.clone()));

        // Test C->9, 9->A implied, A->F = C->F
        let rul1 = ur_rul.new_from_string("11/10/00/01")?;
        let rul2 = ur_rul.new_from_string("11/01/11/01")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());
        let rul4 = ur_rul.new_from_string("11/11/01/01")?;
        assert!(rul3 == rul4);

        Ok(())
    }

    #[test]
    fn combine_pair() -> Result<(), String> {
        let ur_sta = SomeState::new(SomeBits::new(8));
        let ur_rul = SomeRule::new(&SomeSample::new(ur_sta.clone(), ur_sta.clone()));

        // Test 0->0
        let rul1 = ur_rul.new_from_string("00/00/00/00/00/00")?;
        let rul2 = ur_rul.new_from_string("00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("00/01/00/01/00/01")?;
        assert!(rul3 == rul4);

        // Test 0->1
        let rul1 = ur_rul.new_from_string("01/01/01/01/01/01")?;
        let rul2 = ur_rul.new_from_string("11/10/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("01/00/00/01/01/00")?;
        assert!(rul3 == rul4);

        // Test 1->1
        let rul1 = ur_rul.new_from_string("11/11/11/11/11/11")?;
        let rul2 = ur_rul.new_from_string("11/10/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("11/10/10/11/11/10")?;
        assert!(rul3 == rul4);

        // Test 1->0
        let rul1 = ur_rul.new_from_string("10/10/10/10/10/10")?;
        let rul2 = ur_rul.new_from_string("00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("10/11/10/11/10/11")?;
        assert!(rul3 == rul4);

        // Test X->0
        let rul1 = ur_rul.new_from_string("X0/X0/X0/X0/X0/X0")?;
        let rul2 = ur_rul.new_from_string("00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("X0/X1/X0/X1/X0/X1")?;
        assert!(rul3 == rul4);

        // Test X->1
        let rul1 = ur_rul.new_from_string("X1/X1/X1/X1/X1/X1")?;
        let rul2 = ur_rul.new_from_string("11/10/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("X1/X0/X0/X1/X1/X0")?;
        assert!(rul3 == rul4);

        // Test X->X
        let rul1 = ur_rul.new_from_string("XX/XX/XX/XX/XX/XX/XX/XX")?;
        let rul2 = ur_rul.new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        assert!(rul3 == rul4);

        // Test X->X
        let rul1 = ur_rul.new_from_string("XX/XX/XX/XX/XX/XX/XX/XX")?;
        let rul2 = ur_rul.new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        assert!(rul3 == rul4);

        // Test X->X
        let rul1 = ur_rul.new_from_string("XX/XX/XX/XX/XX/XX/XX/XX")?;
        let rul2 = ur_rul.new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        assert!(rul3 == rul4);

        // Test X->x
        let rul1 = ur_rul.new_from_string("Xx/Xx/Xx/Xx/Xx/Xx/Xx/Xx")?;
        let rul2 = ur_rul.new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = ur_rul.new_from_string("01/00/10/11/X0/X1/Xx/XX")?;
        assert!(rul3 == rul4);

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

        Ok(())
    }

    #[test]
    fn new_all() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));
        let tmp_msk = SomeMask::new(SomeBits::new(8));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rule_from_states = SomeRule::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0011")?,
        ));

        let rule_from_masks = SomeRule {
            b00: tmp_msk.new_from_string("m0x7")?.bitwise_not(),
            b01: tmp_msk.new_from_string("m0b0010")?,
            b11: tmp_msk.new_from_string("m0b0001")?,
            b10: tmp_msk.new_from_string("m0b0100")?,
        };

        let rule_from_string = tmp_rul.new_from_string("00/10/01/11")?;

        println!("rule_from_states: {rule_from_states} rule_from_masks: {rule_from_masks}");
        assert!(rule_from_states == rule_from_masks);

        println!("rule_from_states: {rule_from_states} rule_from_string: {rule_from_string}");
        assert!(rule_from_states == rule_from_string);

        Ok(())
    }

    #[test]
    fn initial_region_result_region() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        let sta = tmp_sta.new_from_string("s0b1010")?;
        let st6 = tmp_sta.new_from_string("s0b0110")?;
        let rulx = SomeRule::new(&SomeSample::new(
            sta.clone(),
            tmp_sta.new_from_string("s0b1001")?,
        ));
        let ruly = SomeRule::new(&SomeSample::new(
            st6.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));

        println!("rulx: {rulx}");
        assert!(rulx.initial_region() == SomeRegion::new(vec![sta.clone()]));

        println!("ruly: {ruly}");
        if let Some(rulz) = rulx.union(&ruly) {
            println!("rulz: {rulz}");
            assert!(rulz.initial_region() == tmp_reg.new_from_string("rXX10")?);
        } else {
            return Err("Regions should form a union".to_string());
        }

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(32));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rul1 = tmp_rul.new_from_string("01/01/01/00/00/00/11/11/11/10/10/10/XX/XX/XX/XX/XX/Xx/Xx/Xx/Xx/Xx/X0/X0/X0/X0/X0/X1/X1/X1/X1/X1")?;
        let rul2 = tmp_rul.new_from_string("01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx")?;
        let rul3 = tmp_rul.new_from_string("01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11")?;
        let Some(rul_int) = rul1.intersection(&rul2) else {
            panic!("this should work!");
        };

        println!("rul1: {rul1} rul2: {rul2} rul_int: {rul_int}");
        assert!(rul_int == rul3);

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(32));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rul1 = tmp_rul.new_from_string("01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx")?;
        let rul2 = tmp_rul.new_from_string("01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11")?;

        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul2.is_subset_of(&rul1));

        Ok(())
    }

    #[test]
    fn is_valid_intersection() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rul1 = tmp_rul.new_from_string("XX")?;
        let rul2 = tmp_rul.new_from_string("X1")?;
        let rul3 = tmp_rul.new_from_string("00")?;

        println!("rul1: {rul1} rul2: {rul2} rul3: {rul3}");
        assert!(rul1.intersection(&rul2).is_some());
        assert!(rul2.intersection(&rul3).is_none());

        Ok(())
    }

    #[test]
    fn is_valid_union() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rul1 = tmp_rul.new_from_string("00")?;
        let rul2 = tmp_rul.new_from_string("01")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_none());

        let rul1 = tmp_rul.new_from_string("11")?;
        let rul2 = tmp_rul.new_from_string("10")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_none());

        let rul1 = tmp_rul.new_from_string("11")?;
        let rul2 = tmp_rul.new_from_string("01")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_some());

        let rul1 = tmp_rul.new_from_string("x1")?;
        let rul2 = tmp_rul.new_from_string("00")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_none());

        Ok(())
    }

    #[test]
    fn mutually_exclusive() -> Result<(), String> {
        let tmp_bts = SomeBits::new(8);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_msk = SomeMask::new(tmp_bts.clone());
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        // The results of rules, (10 vs 01) do not intersect the initial regions (00, 00).
        // Running either rule precludes running the other.
        let rul1 = tmp_rul.new_from_string("01/00")?;
        let rul2 = tmp_rul.new_from_string("00/01")?;
        let chg1 = SomeChange::new(tmp_msk.new_from_string("m0b11")?, tmp_msk.new_low());
        println!("rul1: {rul1} rul2: {rul2} chg1: {chg1}");
        assert!(rul1.mutually_exclusive(&rul2, &chg1));

        // The results of rules (10, 01) intersect one of the initial regions (00, 10),
        // so one rul1 should be run before rul2.
        let rul1 = tmp_rul.new_from_string("01/00")?;
        let rul2 = tmp_rul.new_from_string("10/01")?;
        let chg1 = SomeChange::new(tmp_msk.new_from_string("m0b11")?, tmp_msk.new_low());
        println!("rul1: {rul1} rul2: {rul2} chg1: {chg1}");
        assert!(rul1.mutually_exclusive(&rul2, &chg1));

        // The results of rules (1X, X0) intersects both of the initial regions (XX, XX),
        // so either rule can be run before the other.
        let rul1 = tmp_rul.new_from_string("x1/xx")?;
        let rul2 = tmp_rul.new_from_string("xx/x0")?;
        let chg1 = SomeChange::new(tmp_msk.new_from_string("m0b11")?, tmp_msk.new_low());
        println!("rul1: {rul1} rul2: {rul2} chg1: {chg1}");
        assert!(!rul1.mutually_exclusive(&rul2, &chg1));

        Ok(())
    }

    #[test]
    fn sequence_reverses_change() -> Result<(), String> {
        let tmp_bts = SomeBits::new(8);
        let tmp_msk = SomeMask::new(tmp_bts.clone());
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        // The results of rules (10, 01) intersect one of the initial regions (00, 10),
        // so one rul1 should be run before rul2.
        let rul1 = tmp_rul.new_from_string("01/00")?;
        let rul2 = tmp_rul.new_from_string("10/01")?;
        let rul3 = tmp_rul.new_from_string("xx/xx")?;
        let chg1 = SomeChange::new(tmp_msk.new_from_string("m0b11")?, tmp_msk.new_low());
        println!("rul1: {rul1} rul2: {rul2} rul3: {rul3} chg1: {chg1}");

        println!("1->2 {}", rul1.sequence_reverses_change(&rul2, &chg1));
        println!("2->1 {}", rul2.sequence_reverses_change(&rul1, &chg1));
        assert!(rul1.sequence_reverses_change(&rul2, &chg1));
        assert!(rul2.sequence_reverses_change(&rul1, &chg1));
        assert!(!rul1.sequence_reverses_change(&rul3, &chg1));
        assert!(!rul2.sequence_reverses_change(&rul3, &chg1));

        Ok(())
    }

    #[test]
    fn restrict_for_changes() -> Result<(), String> {
        let tmp_bts = SomeBits::new(8);
        let tmp_msk = SomeMask::new(tmp_bts.clone());
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        // Change wanted   X->1 to 0->1.
        // Change depends  X->1 to X->1.
        // Change wanted   X->0 to 1->0.
        // Change depends  X->0 to X->0.
        let rul1 = tmp_rul.new_from_string("X1/X1/X0/X0")?;
        let chg1 = SomeChange::new(
            tmp_msk.new_from_string("m0b1000")?,
            tmp_msk.new_from_string("m0b0010")?,
        );

        let Some(rul2) = rul1.restrict_for_changes(&chg1, None) else {
            panic!("rul2 restriction should succeed");
        };
        println!("rul2 {}", rul2);

        assert!(rul2 == tmp_rul.new_from_string("01/X1/10/X0")?);

        // Change X->X to 0->0 by within restriction.
        // Change X->X to 1->1 by within restriction.
        // Leave one X->X unaffected.
        let rul1 = tmp_rul.new_from_string("01/XX/XX/XX")?;
        let chg1 = SomeChange::new(
            tmp_msk.new_from_string("m0b1000")?,
            tmp_msk.new_from_string("m0b0000")?,
        );
        let within = tmp_reg.new_from_string("rxx01")?;

        let Some(rul3) = rul1.restrict_for_changes(&chg1, Some(&within)) else {
            panic!("rul3 restriction should succeed");
        };
        println!("rul3 {}", rul3);

        assert!(rul3 == tmp_rul.new_from_string("01/XX/00/11")?);

        // Change X->x to 1->0.
        // Change X->x to 0->1.
        // Leave one X->x unaffected.
        let rul1 = tmp_rul.new_from_string("00/Xx/Xx/Xx")?;
        let chg1 = SomeChange::new(
            tmp_msk.new_from_string("m0b0010")?,
            tmp_msk.new_from_string("m0b0100")?,
        );

        let Some(rul4) = rul1.restrict_for_changes(&chg1, None) else {
            panic!("rul4 restriction should succeed");
        };
        println!("rul4 {}", rul4);

        assert!(rul4 == tmp_rul.new_from_string("00/10/01/Xx")?);

        // Detect X-x excursion.
        let rul1 = tmp_rul.new_from_string("01/Xx")?;
        let chg1 = SomeChange::new(
            tmp_msk.new_from_string("m0b10")?,
            tmp_msk.new_from_string("m0b00")?,
        );

        let within = tmp_reg.new_from_string("rx1")?;

        if let Some(rul5) = rul1.restrict_for_changes(&chg1, Some(&within)) {
            println!("rul5 ? {rul5}");
            panic!("rul5 restriction should not succeed");
        };

        Ok(())
    }

    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        let tmp_bts = SomeBits::new(8);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rul1 = tmp_rul.new_from_string("X1/X0/Xx/Xx")?;
        let rul2 = rul1.restrict_initial_region(&tmp_reg.new_from_string("r10X1")?);

        println!("rul2: {rul2}");
        assert!(rul2 == tmp_rul.new_from_string("11/00/Xx/10")?);

        Ok(())
    }

    #[test]
    fn restrict_result_region() -> Result<(), String> {
        let tmp_bts = SomeBits::new(8);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rul1 = tmp_rul.new_from_string("Xx/Xx/XX/XX")?;
        let rul2 = rul1.restrict_result_region(&tmp_reg.new_from_string("r1010")?);

        println!("rul2: {rul2}");
        assert!(rul2 == tmp_rul.new_from_string("01/10/11/00")?);

        Ok(())
    }

    #[test]
    fn result_from_initial_state() -> Result<(), String> {
        let tmp_bts = SomeBits::new(8);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rul1 = tmp_rul.new_from_string("Xx/XX/x1/x0/xx/xx")?;
        let sta1 = tmp_sta.new_from_string("s0b000110")?;

        let sta2 = rul1.result_from_initial_state(&sta1);
        println!("rul1: {rul1} sta1: {sta1} sta2: {sta2}");

        assert!(sta2 == tmp_sta.new_from_string("s0b101010")?);

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let tmp_bts = SomeBits::new(8);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rul1 = tmp_rul.new_from_string("00/01/00/01/xx")?;
        let rul2 = tmp_rul.new_from_string("00/01/10/10/11")?;

        let Some(rul3) = rul1.union(&rul2) else {
            panic!("This should work!");
        };
        println!("rul3 = {rul3}");
        assert!(rul3 == tmp_rul.new_from_string("00/01/x0/Xx/xx")?);

        Ok(())
    }
} // end tests
