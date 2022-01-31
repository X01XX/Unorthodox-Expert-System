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

use crate::bits::NUM_BITS_PER_INT;
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;
use crate::change::SomeChange;
use serde::{Deserialize, Serialize};
use std::fmt;

//#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
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
        Self {
            b00: initial.s_not().s_and(&result.s_not()).to_mask(),
            b01: initial.s_not().s_and(&result).to_mask(),
            b11: initial.s_and(&result).to_mask(),
            b10: initial.s_and(&result.s_not()).to_mask(),
        }
    }

    /// Generate a rule from a number of integers and a string,
    /// like (2, "00/01/11/10/XX/xx/Xx/xX/X0/X1").
    /// Leading "00" tokens can be omitted.
    /// If no tokens supplied, the rule will be all "00".
    pub fn new_from_string(num_ints: usize, rep: &str) -> Result<Self, String> {
 
        let mut b00 = SomeMask::new_low(num_ints).m_not();
        let mut b01 = SomeMask::new_low(num_ints);
        let mut b11 = SomeMask::new_low(num_ints);
        let mut b10 = SomeMask::new_low(num_ints);

        let mut token = String::with_capacity(2);

        for bt in rep.chars() {
            if bt == '/' {
                continue;
            }
            token.push(bt);
            if token.len() == 2 {
                if token == "00" {
                    b00 = b00.push_1();
                    b01 = b01.push_0();
                    b11 = b11.push_0();
                    b10 = b10.push_0();
                } else if token == "01" {
                    b00 = b00.push_0();
                    b01 = b01.push_1();
                    b11 = b11.push_0();
                    b10 = b10.push_0();
                } else if token == "11" {
                    b00 = b00.push_0();
                    b01 = b01.push_0();
                    b11 = b11.push_1();
                    b10 = b10.push_0();
                } else if token == "10" {
                    b00 = b00.push_0();
                    b01 = b01.push_0();
                    b11 = b11.push_0();
                    b10 = b10.push_1();
                } else if token == "XX" || token == "xx" {
                    b00 = b00.push_1();
                    b01 = b01.push_0();
                    b11 = b11.push_1();
                    b10 = b10.push_0();
                } else if token == "Xx" || token == "xX" {
                    b00 = b00.push_0();
                    b01 = b01.push_1();
                    b11 = b11.push_0();
                    b10 = b10.push_1();
                } else if token == "X0" || token == "x0" {
                    b00 = b00.push_1();
                    b01 = b01.push_0();
                    b11 = b11.push_0();
                    b10 = b10.push_1();
                } else if token == "X1" || token == "x1" {
                    b00 = b00.push_0();
                    b01 = b01.push_1();
                    b11 = b11.push_1();
                    b10 = b10.push_0();
                } else {
                    return Err(format!("unrecognized token {}", &token));
                }
                token = String::with_capacity(2);
            }
        }
        if token.len() != 0 {
            return Err(format!("Did not understand token {}", &token));
        }

        Ok(SomeRule {
                b00: b00,
                b01: b01,
                b11: b11,
                b10: b10,
            })
    }

    /// Return true if a rule is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        let tmprul = self.intersection(other);

        *self == tmprul
    }

    /// Return true if a rule is a superset of another.
    pub fn _is_superset_of(&self, other: &Self) -> bool {
        let tmprul = self.intersection(other);

        *other == tmprul
    }

    /// Return true if a rule is valid after a union (no 1X or 0X bits)
    pub fn is_valid_union(&self) -> bool {
        if self.b00.m_and(&self.b01).is_low() {
            if self.b11.m_and(&self.b10).is_low() {
                return true;
            }
        }
        false
    }

    /// Return true if a rule is valid after an intersection,
    /// that is no bit positions are zero in all 4 masks.
    pub fn is_valid_intersection(&self) -> bool {
        if self
            .b00
            .m_or(&self.b01)
            .m_or(&self.b11)
            .m_or(&self.b10)
            .is_high()
        {
            return true;
        }

        false
    }

    /// Return a logical OR of two rules. The result may be invalid.
    pub fn union(&self, other: &Self) -> Self {
        Self {
            b00: self.b00.m_or(&other.b00),
            b01: self.b01.m_or(&other.b01),
            b11: self.b11.m_or(&other.b11),
            b10: self.b10.m_or(&other.b10),
        }
    }

    /// Take an invalid rule, see if a valid subset exists.
    ///
    /// 00 + X1 = 11, 0X, return 11.
    /// 00 + Xx = 10, 0X, return 10.
    /// 01 + X0 = 10, 0X, return 10.
    /// 01 + XX = 11, 0X, return 11.
    /// 11 + X0 = 00, 1X, return 00.
    /// 11 + Xx = 01, 1X, return 01.
    /// 10 + X1 = 01, 1X, return 01.
    /// 10 + XX = 00, 1X, return 00.
    pub fn valid_subset(&self) -> Option<Self> {
        assert!(self.is_valid_union() == false);

        let ones_to_x  = self.b10.m_and(&self.b11);
        let zeros_to_x = self.b01.m_and(&self.b00);

        // Check for 4 change bit-position.
        if ones_to_x.m_and(&zeros_to_x).is_not_low() { return None; }

        // Get zeros one change mask.
        let zeros_1_change = self.b01.m_xor(&self.b00);

        // Check 1X bits are subset zeros one change mask.
        if ones_to_x.is_subset_of(&zeros_1_change) {
        } else {
            return None;
        }

        // Get ones one change mask.
        let ones_1_change = self.b10.m_xor(&self.b11);

        // Check 0X bits are subset ones one change mask.
        if zeros_to_x.is_subset_of(&ones_1_change) {
        } else {
            return None;
        }

        Some(Self {
            b00: self.b00.m_xor(&zeros_to_x),
            b01: self.b01.m_xor(&zeros_to_x),
            b11: self.b11.m_xor(&ones_to_x),
            b10: self.b10.m_xor(&ones_to_x),
        })
    }
    
    /// Return a logical AND of two rules.  The result may be invalid.
    pub fn intersection(&self, other: &Self) -> Self {
        Self {
            b00: self.b00.m_and(&other.b00),
            b01: self.b01.m_and(&other.b01),
            b11: self.b11.m_and(&other.b11),
            b10: self.b10.m_and(&other.b10),
        }
    }

    /// Return the initial region of a rule.
    pub fn initial_region(&self) -> SomeRegion {
        let st_high = self.b11.m_or(&self.b10).to_state();
        let st_low = self.b00.m_or(&self.b01).m_not().to_state();

        SomeRegion::new(&st_high, &st_low)
    }

    /// Return the result region of a rule.
    pub fn result_region(&self) -> SomeRegion {
        let st_high = self.b11.m_or(&self.b01).to_state();
        let st_low = self.b00.m_or(&self.b10).m_not().to_state();

        SomeRegion::new(&st_high, &st_low)
    }

    /// Return the result region after applying an initial region to a rule.
    /// This could be called "forward chaining".
    pub fn result_from_initial_region(&self, reg: &SomeRegion) -> SomeRegion {
        if reg.intersects(&self.initial_region()) == false {
            panic!("result_from_initial: given region does not intersect the ruls initial region");
        }

        self.restrict_initial_region(reg).result_region()
    }

    /// Return the result region after applying an initial state to a rule.
    /// This could be called "forward chaining".
    pub fn result_from_initial_state(&self, sta: &SomeState) -> SomeState {
        if self.initial_region().is_superset_of_state(&sta) == false {
            panic!(
                "result_from_initial_state: given state is not a subset of the ruls initial region"
            );
        }

        let mut toggle = self.b01.to_state().s_and(&sta.s_not());
        toggle = toggle.s_or(&self.b10.to_state().s_and(sta));
        sta.s_xor(&toggle)
    }

    /// Restrict the initial region to an intersection of the
    /// given region.  Assuming the region given is not a superset
    /// this will also change the result region.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Self {
        let init_reg = self.initial_region();

        if init_reg.intersects(&regx) == false {
            panic!(
                "{} does not intersect rule initial region {}",
                regx, init_reg
            );
        }

        let reg_int = regx.intersection(&init_reg);

        let zeros = reg_int.low_mask();
        let ones = reg_int.high_mask();

        Self {
            b00: self.b00.m_and(&zeros),
            b01: self.b01.m_and(&zeros),
            b11: self.b11.m_and(&ones),
            b10: self.b10.m_and(&ones),
        }
    }

    /// Restrict the result region to an intersection of the
    /// given region.  Assuming the region given is not a superset
    /// this will also change the initial region.
    pub fn restrict_result_region(&self, regx: &SomeRegion) -> Self {
        //println!("restricting result region of {} to {}", &self, &regx);

        let rslt_reg = self.result_region();

        if rslt_reg.intersects(&regx) == false {
            panic!(
                "{} does not intersect rule result region {}",
                regx, rslt_reg
            );
        }

        if regx.is_superset_of(&rslt_reg) {
            return self.clone();
            //            panic!(
            //                "{} is a superset of rule result region {}",
            //                regx,
            //                rslt_reg
            //            );
        }

        let reg_int = regx.intersection(&rslt_reg);

        let zeros = reg_int.low_mask();
        let ones = reg_int.high_mask();

        let rc_rul = Self {
            b00: self.b00.m_and(&zeros),
            b01: self.b01.m_and(&ones),
            b11: self.b11.m_and(&ones),
            b10: self.b10.m_and(&zeros),
        };

        //println!(" giving {} with intial {}", rc_rul, rc_rul.initial_region());
        rc_rul
    }

    /// Return the expected length of a string representation of SomeRule.
    pub fn formatted_string_length(&self) -> usize {
        (NUM_BITS_PER_INT * self.b00.num_ints() * 3) - 1
    }

    /// Return a string representation of SomeRule.
    pub fn formatted_string(&self) -> String {
        let mut strrc = String::with_capacity(self.formatted_string_length());

        let num_ints = self.b00.num_ints();
        let num_bits = num_ints * NUM_BITS_PER_INT;

        for i in (0..num_bits).rev() {
            let b00: bool = self.b00.is_bit_set(i);
            let b01: bool = self.b01.is_bit_set(i);
            let b11: bool = self.b11.is_bit_set(i);
            let b10: bool = self.b10.is_bit_set(i);

            if i != (num_bits - 1) {
                if (i + 1) % NUM_BITS_PER_INT == 0 {
                    strrc.push('_');
                } else {
                    strrc.push('/');
                }
            }

            if b00 && b01 == false && b11 && b10 == false {
                strrc.push_str("XX");
            } else if b00 && b01 == false && b11 == false && b10 {
                strrc.push_str("X0");
            } else if b00 == false && b01 && b11 && b10 == false {
                strrc.push_str("X1");
            } else if b00 == false && b01 && b11 == false && b10 {
                strrc.push_str("Xx");
            } else if b00 && b01 == false && b11 == false && b10 == false {
                strrc.push_str("00");
            } else if b00 == false && b01 == false && b11 && b10 == false {
                strrc.push_str("11");
            } else if b00 == false && b01 == false && b11 == false && b10 {
                strrc.push_str("10");
            } else if b00 == false && b01 && b11 == false && b10 == false {
                strrc.push_str("01");
            } else if b00 && b01 && b11 == false && b10 == false {
                strrc.push_str("0X");
            } else if b00 == false && b01 == false && b11 && b10 {
                strrc.push_str("1X");
            } else if b00 == false && b01 == false && b11 == false && b10 == false {
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
    /// The arguments b01 and b10 are masks of changes that are sought.
    ///
    /// It is expected that b01 & b10 would be all zeros, e.g. you do not need
    /// 0->1 and 1->0 in the same bit position.
    ///
    /// It is expected that b01 and b10 are not both all zeros, otherwise
    /// no change would be sought.
    ///
    /// For changes that are sought, for bit positions that are X->1, X->0, or X->x,
    /// the X value can be changed to focus on the desired change.
    //
    /// X->1 is 1->1 and 0->1, the X can be changed to 0.
    /// X->0 is 0->0 and 1->0, the X can be changed to 1.
    /// X->x is 1->0 and 0->1, the X can be changed to 1 or 0, depending on the change sought.
    ///
    pub fn parse_for_changes(&self, change_needed: &SomeChange) -> Option<Self> {

        let cng_int = self.change().c_and(change_needed);

        if cng_int.is_low() {
            // No change, or no change is needed
            return None;
        }

        // Get rule initial region and x mask.
        let mut i_reg = self.initial_region();
        let i_reg_xes = i_reg.x_mask();

        // Figure region bit positions to change from X to 1
        let to_ones = i_reg_xes.m_and(&cng_int.b01);

        if to_ones.is_not_low() {
            i_reg = i_reg.set_to_zeros(&to_ones);
        }

        // Figure region bit positions to change from X to 0
        let to_zeros = i_reg_xes.m_and(&cng_int.b10);

        if to_zeros.is_not_low() {
            i_reg = i_reg.set_to_ones(&to_zeros);
        }

        // Return a restricted rule
        Some(self.restrict_initial_region(&i_reg))
    }

    /// Return a SomeChange struct instance
    pub fn change(&self) -> SomeChange {
        SomeChange::new(&self.b01, &self.b10)
    }

    /// Return true if two rules are mutually exclusive.
    /// Both rules lose wanted changes, running them in any order.
    pub fn mutually_exclusive(&self, other: &SomeRule, wanted: &SomeChange) -> bool {
        if self.order_bad(other, wanted) {
            if other.order_bad(self, wanted) {
                return true;
            }
        }
        false
    }

    /// Return true if the target rule, run before the second rule, will lose all desired changes.
    ///
    ///    A change can be lost by:
    ///        A wanted 0->1 change in rule1 (self) corresponds with a 0 in the initial-region of step2.
    ///        A wanted 1->0 change in rule1 (self) corresponds with a 1 in the initial region of step2.
    pub fn order_ok(&self, other: &SomeRule, wanted: &SomeChange) -> bool {
        !self.order_bad(other, wanted)
    }
    pub fn order_bad(&self, other: &SomeRule, wanted: &SomeChange) -> bool {
        // println!("order_bad: {} to {} change wanted {}", &self.formatted_string(), &step2.formatted_string(), &wnated.formatted_string());

        // Calc aggregate rule.
        let rulx = self.then_to(&other);

        // Get a mask of the wanted changes in this rule.
        let s_wanted = self.change().c_and(wanted);

        // Get a mask of the wanted changes after running both rules.
        let a_wanted = rulx.change().c_and(wanted);

        // Get a mask of wanted changes in this rule that remain after running the second rule.
        let rslt = s_wanted.c_and(&a_wanted);

        // Return true, the oreder is bad, if no wanted changes remain.
        rslt.is_low()
    }

    /// Combine two rules in sequence.
    /// The result region of the first rule may not intersect the initial region of the second rule.
    pub fn then_to(&self, other: &SomeRule) -> Self {
        if self.result_region().intersects(&other.initial_region()) {
            return self.then_to2(&other);
        }
        let rul_between = self.result_region().rule_to_region(&other.initial_region()).unwrap();
        self.then_to2(&rul_between).then_to2(&other)
    }
    /// Combine two rules in sequence.
    /// The result region of the first rule must intersect the initial region of the second rule.
    fn then_to2(&self, other: &SomeRule) -> Self {
        assert!(self.result_region().intersects(&other.initial_region()));

        Self {
            b00: self.b00.m_and(&other.b00).m_or(&self.b01.m_and(&other.b10)),
            b01: self.b01.m_and(&other.b11).m_or(&self.b00.m_and(&other.b01)),
            b11: self.b11.m_and(&other.b11).m_or(&self.b10.m_and(&other.b01)),
            b10: self.b10.m_and(&other.b00).m_or(&self.b11.m_and(&other.b10)),
        }
    }
    
    /// Return the number of one-bit changes required to go from a given region, to another, via a rule.
    /// This gives a general idea of the effect of adding a rule to a possible rule-path,
    /// assuming other rules may be required to finish the path.
    /// In other words, how far out of your way do you have to go to make a rule part of the solution?
    pub fn from_to_number_changes(&self, from_reg: &SomeRegion, to_reg: &SomeRegion) -> usize {
        let mut rul2 = self.clone();
        let mut num_changes = 0;

        // Calc from_reg to rule initial region number changes.
        let initial = rul2.initial_region();
        if initial.is_superset_of(from_reg) {
            rul2 = rul2.restrict_initial_region(&initial);
        } else {
            let rul1 = from_reg.rule_to_region(&initial).unwrap();
            num_changes = rul1.change().number_changes();
            rul2 = rul2.restrict_initial_region(&rul1.result_region());
        }

        // Add self rule number changes.
        num_changes += rul2.change().number_changes();

        // Add self rule result to_reg number changes.
        let result = rul2.result_region();
        if to_reg.is_superset_of(&result) {
        } else {
            let rul3 = result.rule_to_region(to_reg).unwrap();
            num_changes += rul3.change().number_changes();
        }

        num_changes
    }
} // end impl SomeRule

impl Clone for SomeRule {
    fn clone(&self) -> Self {
        Self {
            b00: self.b00.clone(),
            b01: self.b01.clone(),
            b11: self.b11.clone(),
            b10: self.b10.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::mask::SomeMask;
    use crate::state::SomeState;
    use crate::region::SomeRegion;
    use crate::rule::SomeRule;
    use crate::change::SomeChange;

    #[test]
    fn test_from_to_number_changes() -> Result<(), String> {
        let reg4 = SomeRegion::new_from_string(1, "s0100").unwrap();
        let regd = SomeRegion::new_from_string(1, "s1101").unwrap();
        let regf = SomeRegion::new_from_string(1, "s1111").unwrap();
        let reg6 = SomeRegion::new_from_string(1, "s0110").unwrap();

        // Tets no intersection of rule and from or to regions.
        let rul1 = regd.rule_to_region(&regf).unwrap();
        let num_chg = rul1.from_to_number_changes(&reg4, &reg6);
        println!("from {} using {} to {}, number changes = {}", &reg4, &rul1, &reg6, &num_chg);
        assert!(num_chg == 5);

        // Test intersection of rule initial region and from region.
        let rul1 = reg4.rule_to_region(&regf).unwrap();
        let num_chg = rul1.from_to_number_changes(&reg4, &reg6);
        println!("from {} using {} to {}, number changes = {}", &reg4, &rul1, &reg6, &num_chg);
        assert!(num_chg == 5);

        // Test intersection of rule result and to region.
        let rul1 = regd.rule_to_region(&reg6).unwrap();
        let num_chg = rul1.from_to_number_changes(&reg4, &reg6);
        println!("from {} using {} to {}, number changes = {}", &reg4, &rul1, &reg6, &num_chg);
        assert!(num_chg == 5);

        // Test intersection of both initial and result of rule.
        let rul1 = reg4.rule_to_region(&reg6).unwrap();
        let num_chg = rul1.from_to_number_changes(&reg4, &reg6);
        println!("from {} using {} to {}, number changes = {}", &reg4, &rul1, &reg6, &num_chg);
        assert!(num_chg == 1);

        Ok(())
    }

    #[test]
    fn test_initial_region() -> Result<(), String> {
        let sta = SomeState::new_from_string(1, "s1010").unwrap();
        let st6 = SomeState::new_from_string(1, "s0110").unwrap();
        let regx = SomeRule::new(&sta, &SomeState::new_from_string(1, "s1001").unwrap());
        let regy = SomeRule::new(&st6, &SomeState::new_from_string(1, "s0101").unwrap());

        if regx.initial_region() != SomeRegion::new(&sta, &sta) {
            return Err(format!("test_initial_region 1 initial region not r1010?"));
        }

        let regz = regx.union(&regy);
        if regz.initial_region() != SomeRegion::new_from_string(1, "rXX10").unwrap() {
            return Err(format!("test_initial_region 2 initial region not rXX10?"));
        }

        Ok(())
    }

    #[test]
    fn test_intersection() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(4, "01/01/01/00/00/00/11/11/11/10/10/10/XX/XX/XX/XX/XX/Xx/Xx/Xx/Xx/Xx/X0/X0/X0/X0/X0/X1/X1/X1/X1/X1").unwrap();
        let rul2 = SomeRule::new_from_string(4, "01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx").unwrap();
        let rul3 = SomeRule::new_from_string(4, "01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11").unwrap();

        if rul1.intersection(&rul2) != rul3 {
            return Err(format!("test_intersection rule {} ne {}?", rul1.intersection(&rul2), &rul3));
        }

        Ok(())
    }

    #[test]
    fn test_is_subset_of() -> Result<(), String> {
        let rul2 = SomeRule::new_from_string(4, "01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx").unwrap();
        let rul3 = SomeRule::new_from_string(4, "01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11").unwrap();

        if rul3.is_subset_of(&rul2) == false {
            return Err(format!("test_is_subset_of: not a subset?"));
        }

        Ok(())
    }

    #[test]
    fn test_is_valid_intersection() -> Result<(), String> {

        let rul1 = SomeRule::new_from_string(1, "XX").unwrap();
        let rul2 = SomeRule::new_from_string(1, "X1").unwrap();
        let rul3 = SomeRule::new_from_string(1, "00").unwrap();

        let int12 = rul1.intersection(&rul2);
        if int12.is_valid_intersection() == false {
            return Err(format!("test_is_valid_intersection 1 returned False?"));
        }

        let int23 = rul2.intersection(&rul3);
        if int23.is_valid_intersection() {
            return Err(format!("test_is_valid_intersection 2 returned True?"));
        }

        Ok(())
    }

    #[test]
    fn test_is_valid_union() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "00").unwrap();
        let rul2 = SomeRule::new_from_string(1, "01").unwrap();
        let rul3 = rul1.union(&rul2);
        if rul3.is_valid_union() {
            return Err(format!("test_is_valid_union 1 True?"));
        }

        let rul1 = SomeRule::new_from_string(1, "11").unwrap();
        let rul2 = SomeRule::new_from_string(1, "10").unwrap();
        let rul3 = rul1.union(&rul2);
        if rul3.is_valid_union() {
            return Err(format!("test_is_valid_union 2 True?"));
        }

        let rul1 = SomeRule::new_from_string(1, "11").unwrap();
        let rul2 = SomeRule::new_from_string(1, "01").unwrap();
        let rul3 = rul1.union(&rul2);
        if rul3.is_valid_union() == false {
            return Err(format!("test_is_valid_union 3 False?"));
        }

        let rul1 = SomeRule::new_from_string(1, "x1").unwrap();
        let rul2 = SomeRule::new_from_string(1, "00").unwrap();
        let rul3 = rul1.union(&rul2);
        if rul3.is_valid_union() {
            return Err(format!("test_is_valid_union 4 True?"));
        }

        Ok(())
    }

    #[test]
    fn test_mutually_exclusive() -> Result<(), String> {
        // The results of rules, (10 vs 01) do not intersect the initial regions (00, 00).
        // Running either rule precludes running the other.
        let rul1 = SomeRule::new_from_string(1, "01/00").unwrap();
        let rul2 = SomeRule::new_from_string(1, "00/01").unwrap();
        let chg1 = SomeChange::new(&SomeMask::new_from_string(1, "m11").unwrap(), &SomeMask::new_low(1));
        if rul1.mutually_exclusive(&rul2, &chg1) == false {
            return Err(format!("test_mutually_exclusive 1 False?"));
        }

        // The results of rules (10, 01) intersect one of the initial regions (00, 10),
        // so one rul1 should be run before rul2.
        let rul1 = SomeRule::new_from_string(1, "01/00").unwrap();
        let rul2 = SomeRule::new_from_string(1, "10/01").unwrap();
        let chg1 = SomeChange::new(&SomeMask::new_from_string(1, "m11").unwrap(), &SomeMask::new_low(1));
        if rul1.mutually_exclusive(&rul2, &chg1) == false {
            return Err(format!("test_mutually_exclusive 1 False?"));
        }

        // The results of rules (1X, X0) intersects both of the initial regions (XX, XX),
        // so either rule can be run before the other.
        let rul1 = SomeRule::new_from_string(1, "x1/xx").unwrap();
        let rul2 = SomeRule::new_from_string(1, "xx/x0").unwrap();
        let chg1 = SomeChange::new(&SomeMask::new_from_string(1, "m11").unwrap(), &SomeMask::new_low(1));
        if rul1.mutually_exclusive(&rul2, &chg1)  {
            return Err(format!("test_mutually_exclusive 1 True?"));
        }

        Ok(())
    }

    #[test]
    fn test_order_bad() -> Result<(), String> {
        // The results of rules (10, 01) intersect one of the initial regions (00, 10),
        // so one rul1 should be run before rul2.
        let rul1 = SomeRule::new_from_string(1, "01/00").unwrap();
        let rul2 = SomeRule::new_from_string(1, "10/01").unwrap();
        let rul3 = SomeRule::new_from_string(1, "xx/xx").unwrap();
        let chg1 = SomeChange::new(&SomeMask::new_from_string(1, "m11").unwrap(), &SomeMask::new_low(1));

        println!("1->2 {}", rul1.order_bad(&rul2, &chg1));
        println!("2->1 {}", rul2.order_bad(&rul1, &chg1));
        if rul1.order_bad(&rul2, &chg1) == false {
            return Err(format!("test_order_bad 1 False?"));
        }
        if rul2.order_bad(&rul1, &chg1) == false {
            return Err(format!("test_order_bad 2 False?"));
        }
        if rul1.order_bad(&rul3, &chg1) {
            return Err(format!("test_order_bad 3 True?"));
        }
        if rul2.order_bad(&rul3, &chg1) {
            return Err(format!("test_order_bad 4 True?"));
        }
        
        Ok(())
    }

    #[test]
    fn test_parse_for_changes() -> Result<(), String> {

        let rul1 = SomeRule::new_from_string(1, "X1/X0/Xx/Xx").unwrap();
        let chg1 = SomeChange::new(&SomeMask::new_from_string(1, "m1010").unwrap(), &SomeMask::new_from_string(1, "m101").unwrap());
        let rul2 = rul1.parse_for_changes(&chg1).unwrap();

        println!("rul2 {}", &rul2);

        if rul2 != SomeRule::new_from_string(1, "/01/10/01/10").unwrap() {
            return Err(format!("test_parse_for_changes rul2 not /01/10/01/10?"));
        }

        Ok(())
    }

    #[test]
    fn test_restrict_initial_region() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "X1/X0/Xx/Xx").unwrap();
        let rul2 = rul1.restrict_initial_region(&SomeRegion::new_from_string(1, "r10X1").unwrap());
        
        println!("rul2 = {}", rul2);
        if rul2 != SomeRule::new_from_string(1, "11/00/Xx/10").unwrap() {
            return Err(format!("test_restrict_initial_region rul2 not 11/00/Xx/10?"));
        }

        Ok(())
    }

    #[test]
    fn test_restrict_result_region() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "Xx/Xx/XX/XX").unwrap();
        let rul2 = rul1.restrict_result_region(&SomeRegion::new_from_string(1, "r1010").unwrap());
        
        println!("rul = {}", rul2);
        if rul2 != SomeRule::new_from_string(1, "01/10/11/00").unwrap() {
            return Err(format!("test_restrict_result_region rul2 not 01/10/11/00?"));
        }

        Ok(())
    }

    // Also tests result_region
    #[test]
    fn test_result_from_initial_region() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "Xx/XX/x1/x0/xx/xx").unwrap();
        let reg1 = SomeRegion::new_from_string(1, "rx00110").unwrap();
        
        let reg2 = rul1.result_from_initial_region(&reg1);
        println!("reg2 {}", reg2);

        if reg2 != SomeRegion::new_from_string(1, "rX01010").unwrap() {
            return Err(format!("test_result_from_initial_region reg2 not X01010?"));
        }

        Ok(())
    }

    #[test]
    fn test_result_from_initial_state() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "Xx/XX/x1/x0/xx/xx").unwrap();
        let sta1 = SomeState::new_from_string(1, "s000110").unwrap();
        
        let sta2 = rul1.result_from_initial_state(&sta1);
        println!("rul1 {}", &rul1);
        println!("sta2 {}", &sta2);

        if sta2 != SomeState::new_from_string(1, "s101010").unwrap() {
            return Err(format!("test_result_from_initial_state sta2 not 001010?"));
        }

        Ok(())
    }

    #[test]
    fn test_union() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string(1, "00/01/00/01/xx").unwrap();
        let rul2 = SomeRule::new_from_string(1, "00/01/10/10/11").unwrap();
        let rul3 = rul1.union(&rul2);

        println!("rul3 = {}", &rul3);
        if rul3 != SomeRule::new_from_string(1, "00/01/x0/Xx/xx").unwrap() {
            return Err(format!("test_union rul3 not 00/01/x0/Xx/xx?"));
        }
        Ok(())
    }

    /// Take an invalid rule, see if a valid subset exists.
    ///
    /// 00 + X1 = 11, 0X, return 11.
    /// 00 + Xx = 10, 0X, return 10.
    /// 01 + X0 = 10, 0X, return 10.
    /// 01 + XX = 11, 0X, return 11.
    /// 11 + X0 = 00, 1X, return 00.
    /// 11 + Xx = 01, 1X, return 01.
    /// 10 + X1 = 01, 1X, return 01.
    /// 10 + XX = 00, 1X, return 00.
    #[test]
    fn test_valid_subset() -> Result<(), String> {
        // Form a rule of X1/Xx/X0/XX/X0/Xx/X1/XX
        let rul1 = SomeRule {
            b00: SomeMask::new_from_string(1, "m00111001").unwrap(),
            b01: SomeMask::new_from_string(1, "m11000110").unwrap(),
            b11: SomeMask::new_from_string(1, "m10010011").unwrap(),
            b10: SomeMask::new_from_string(1, "m01101100").unwrap(),
        };

        // Form a rule of 00/00/01/01/11/11/10/10
        let rul2 = SomeRule {
            b00: SomeMask::new_from_string(1, "m11000000").unwrap(),
            b01: SomeMask::new_from_string(1, "m00110000").unwrap(),
            b11: SomeMask::new_from_string(1, "m00001100").unwrap(),
            b10: SomeMask::new_from_string(1, "m00000011").unwrap(),
        };

        // Form an invalid rule with the union of:
        // X1/Xx/X0/XX/X0/Xx/X1/XX
        // 00/00/01/01/11/11/10/10
        let rul3 = rul1.union(&rul2);

        let rul4 = rul3.valid_subset().unwrap();

        // Form a rule of 11/10/10/11/00/01/01/00
        let rul5 = SomeRule {
            b00: SomeMask::new_from_string(1, "m00001001").unwrap(),
            b01: SomeMask::new_from_string(1, "m00000110").unwrap(),
            b11: SomeMask::new_from_string(1, "m10010000").unwrap(),
            b10: SomeMask::new_from_string(1, "m01100000").unwrap(),
        };

        // From the invalid union of:
        // X1/Xx/X0/XX/X0/Xx/X1/XX
        // 00/00/01/01/11/11/10/10
        // Should get the following valid part.
        // 11/10/10/11/00/01/01/00
        if rul4 != rul5 {
            return Err(format!("test_valid_subset subset is? {}", rul4.formatted_string()));
        }

        Ok(())
    }

} // end tests
