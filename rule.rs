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

#[readonly::make]
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

    /// Return true if a rule is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        let tmprul = self.intersection(other);

        *self == tmprul
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

        let toggle = self.b01.to_state().s_and(&sta.s_not());
        toggle.s_or(&self.b10.to_state().s_and(sta))
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

    /// Return true if two rules are mutually exclusive
    pub fn mutually_exclusive(&self, other: &SomeRule, wanted: &SomeChange) -> bool {
        if self.order_bad(other, wanted) {
            if other.order_bad(self, wanted) {
                return true;
            }
        }
        false
    }

    /// Return true if the target rule should not be run before the second.
    ///
    ///    If a wanted 0->1 change in rule1 (self) corresponds with a 0 in the initial-region of step2.
    ///
    ///    If a wanted 1->0 change in rule1 (self) corresponds with a 1 in the initial region of step2.
    ///
    pub fn order_bad(&self, other: &SomeRule, wanted: &SomeChange) -> bool {

        // println!("order_bad: {} to {} change wanted {}", &self.formatted_string(), &step2.formatted_string(), &wnated.formatted_string());

        let sb01 = self.b01.m_and(&wanted.b01);
        if sb01.is_not_low() {
            // println!("order_bad: sb01 {} step2 zero pos {}", &sb01, &step2.initial.zero_bit_positions());
            if sb01.m_and(&other.b10.m_or(&other.b11).m_not()).is_not_low() {
                // println!("order_bad: returning true");
                return true;
            }
        }

        let sb10 = self.b10.m_and(&wanted.b10);
        if sb10.is_not_low() {

            if sb10.m_and(&other.b01.m_or(&other.b00).m_not()).is_not_low() {
                // println!("order_bad: returning true");
                return true;
            }
        }
        // println!("order_bad: returning false");
        false
    }

} // end SomeRule

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
