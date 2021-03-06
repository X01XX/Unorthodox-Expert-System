//! Rule struct for, representing a change of bits between aninitial and result state.
//!
//! Rules for two squares can be combined to create a rule for a SomeGroup struct.
//!
//! If the initial region of two groups intersect, and intersection of their rules can be taken,
//! which may be valid, invalid, or partially valid.
//!
//! In combining rules, 0->X and 1->X bit positions are disallowed, since
//! there has to be some limit on combination that leaves a rule with predictive power.
//!
//! The rule can be used in a way that is like "forward chaining" (result_from_initial) and
//! "backward chaining" (intitial_from_result).

use crate::bits::NUM_BITS_PER_INT;
use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;
use serde::{Deserialize, Serialize};
use std::fmt;

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
            b00: SomeMask::new(initial.bts.b_not().b_and(&result.bts.b_not())),
            b01: SomeMask::new(initial.bts.b_not().b_and(&result.bts)),
            b11: SomeMask::new(initial.bts.b_and(&result.bts)),
            b10: SomeMask::new(initial.bts.b_and(&result.bts.b_not())),
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

    // Return true if a invalid union rule can be pruned
    //    pub fn can_be_pruned(&self) -> bool {
    //        let reg_x = self.initial_region().x_mask();
    //
    //        let onex = self.b10.m_and(&self.b11);
    //
    //        let zerox = self.b00.m_and(&self.b01);
    //
    //        if onex.m_and(&zerox).is_not_low() {
    //            return false;
    //        }
    //
    //        if onex.m_and(&reg_x) != onex {
    //            return false;
    //        }
    //
    //        if zerox.m_and(&reg_x) != zerox {
    //            return false;
    //        }
    //        true
    //    }

    /// Return a logical AND of two rules.  The result may be invalid.
    pub fn intersection(&self, other: &Self) -> Self {
        Self {
            b00: self.b00.m_and(&other.b00),
            b01: self.b01.m_and(&other.b01),
            b11: self.b11.m_and(&other.b11),
            b10: self.b10.m_and(&other.b10),
        }
    }

    // Return the expected result of running a rule on a state
    // Assumes the state is a subset of the rule initial region
    //    pub fn result_of_state(&self, sta: &SomeState) -> SomeState {
    //        let sta_msk = SomeMask::new(sta.bts);
    //
    //        let b01 = self.b01.m_and(&sta_msk.m_not());
    //        let b10 = self.b10.m_and(&sta_msk);
    //
    //        SomeState::new(sta_msk.m_xor(&b01).m_xor(&b10).bts)
    //    }

    /// Return the initial region of a rule.
    pub fn initial_region(&self) -> SomeRegion {
        let st_high = SomeState::new(self.b11.bts.b_or(&self.b10.bts));
        let st_low = SomeState::new(self.b00.bts.b_or(&self.b01.bts).b_not());

        SomeRegion::new(&st_high, &st_low)
    }

    /// Return the result region of a rule.
    pub fn result_region(&self) -> SomeRegion {
        let st_high = SomeState::new(self.b11.bts.b_or(&self.b01.bts));
        let st_low = SomeState::new(self.b00.bts.b_or(&self.b10.bts).b_not());

        SomeRegion::new(&st_high, &st_low)
    }

    // Return a mask of zero-bit changes for a rule.
    // For an aggregate rule (SomeState -> SomeRegion), the
    // 1->X bits are don't cares.
    //    pub fn zero_change_mask(&self) -> SomeMask {
    //        self.b01.m_xor(&self.b01.m_and(&self.b10))
    //    }

    // Return a mask of one-bit changes for a rule.
    // For an aggregate rule (SomeState -> SomeRegion), the
    // 1->X bits are don't cares.
    //    pub fn one_change_mask(&self) -> SomeMask {
    //        self.b10.m_xor(&self.b01.m_and(&self.b10))
    //    }

    /// Return the result region after applying an initial region to a rule.
    /// This could be called "forward chaining".
    pub fn result_from_initial(&self, reg: &SomeRegion) -> SomeRegion {
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

        let mut toggle = self.b01.bts.b_and(&sta.bts.b_not());
        toggle = toggle.b_or(&self.b10.bts.b_and(&sta.bts));
        SomeState {
            bts: sta.bts.b_xor(&toggle),
        }
    }

    // Return the initial region after applying a result region to a rule.
    // This could be called "backward chaining".
    //    pub fn initial_from_result(&self, reg: &SomeRegion) -> SomeRegion {
    //        if reg.intersects(&self.result_region()) == false {
    //            panic!("initial_from_result: given region does not intersect the ruls result region");
    //        }
    //
    //        self.restrict_result_region(reg).initial_region()
    //    }

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

    // Return true if the rule does not change anything
    //    pub fn no_change(&self) -> bool {
    //        self.b01.is_low() && self.b10.is_low()
    //    }

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

    // Return true if the change masks of two rules are equal
    //    pub fn changes_equal(&self, other: &Self) -> bool {
    //        if self.b01 == other.b01 && self.b10 == other.b10 {
    //            //println!("{} eq {} and {} eq {} = true", self.b01, other.b01, self.b10, other.b10);
    //            return true;
    //        }
    //        //println!("{} eq {} and {} eq {} = false", self.b01, other.b01, self.b10, other.b10);
    //        false
    //    }

    // Return true if two rule changes exactly cancel each other.
    // If there are no changes in one rule, this function should
    // probably not be called, but the rules would not actively cancel
    // each other.
    //    pub fn changes_cancel(&self, other: &Self) -> bool {
    //        if self.b01.is_low() && self.b10.is_low() {
    //            return false;
    //        }
    //
    //        // At least one of the elements will be not-low
    //        if self.b01 == other.b10 && self.b10 == other.b01 {
    //            return true;
    //        }
    //
    //        false
    //    }

    // Set initial region X bits to one
    //    pub fn set_initial_to_ones(&self, msk: &SomeMask) -> Self {
    //        let nmsk = msk.m_not();
    //
    //        Self {
    //            b00: self.b00.m_and(&nmsk),
    //            b01: self.b01.m_and(&nmsk),
    //            b11: self.b11.clone(),
    //            b10: self.b10.clone(),
    //        }
    //    }

    // Set initial region X bits to zero
    //    pub fn set_initial_to_zeros(&self, msk: &SomeMask) -> Self {
    //        let nmsk = msk.m_not();
    //
    //        Self {
    //            b00: self.b00.clone(),
    //            b01: self.b01.clone(),
    //            b11: self.b11.m_and(&nmsk),
    //            b10: self.b10.m_and(&nmsk),
    //        }
    //    }

    /// Return the expected length of a string representation of SomeRule.
    pub fn formatted_string_length(&self) -> usize {
        (NUM_BITS_PER_INT * self.b00.bts.len() * 3) - 1
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
    /// focus in on desired changes, and avoid undesired changes.
    ///
    /// The arguments b01 and b10 are masks of changes that are sought.
    ///
    /// It is expected that b01 & b10 would be all zeros, e.g. you do not need
    /// 0->1 and 1->0 in the same bit position.
    ///
    /// It is expected that b01 and b10 are not both all zeros, otherwise
    /// no change would be sought.
    ///
    /// Any other change in the rule is to be avoided, if possible.
    ///
    /// For changes that are sought, for bit positions that are X->1, X->0, or X->x,
    /// the X value can be changed to focus on the desired change.
    //
    /// X->1 is 1->1 and 0->1, the X can be changed to 0.
    /// X->0 is 0->0 and 1->0, the X can be changed to 1.
    /// X->x is 1->0 and 0->1, the X can be changed to 1 or 0, depending on the change sought.
    ///
    /// For changes that are not sought,
    ///
    /// X->1, the X bit position can be changed to 1, to avoid 0->1.
    /// X->0, the X bit position can be changed to 0, to avoid 1->0.
    ///
    /// Changing the X bit positions of a rules' initial region increases the
    /// non-X bit position requirements that the current state must match in
    /// order to use the rule.
    pub fn parse_for_changes(&self, b01: &SomeMask, b10: &SomeMask) -> Option<Self> {
        let ones = self.b10.m_and(&b10);
        let zeros = self.b01.m_and(&b01);

        if ones.is_low() && zeros.is_low() {
            return None;
        }

        // Get unwanted changes, except when there are two
        // in the same position, that is 1->0 and 0->1.
        let mut ones_not = self.b10.m_xor(&ones);
        let mut zeros_not = self.b01.m_xor(&zeros);

        // Filter out two-change bit positions.
        let both_not = ones_not.m_and(&zeros_not);
        ones_not = ones_not.m_xor(&both_not);
        zeros_not = zeros_not.m_xor(&both_not);

        // Get rule initial region and x mask.
        let mut i_reg = self.initial_region();
        let i_reg_xes = i_reg.x_mask();

        // Figure region bit positions to change from X to 1
        let to_ones = ones.m_or(&zeros_not).m_and(&i_reg_xes);

        if to_ones.is_not_low() {
            i_reg = i_reg.set_to_ones(&to_ones);
        }

        // Figure region bit positions to change from X to 0
        let to_zeros = zeros.m_or(&ones_not).m_and(&i_reg_xes);

        if to_zeros.is_not_low() {
            i_reg = i_reg.set_to_zeros(&to_zeros);
        }

        // Return a restricted rule
        Some(self.restrict_initial_region(&i_reg))
    }

    // Return a SomeChange struct instance
    pub fn change(&self) -> SomeChange {
        SomeChange {
            b01: self.b01.clone(),
            b10: self.b10.clone(),
        }
    }

    /// Return true if it is OK to run the target rule before rule2.
    ///
    /// To approve a given order,
    ///
    ///    A wanted 0->1 change in rule1 (self) should not correspond with a 0->.. in rule2.
    ///
    ///    A wanted 1->0 change in rule1 (self) should not correspond with a 1->.. in rule2.
    ///
    /// Run twice with different order, to determine
    ///
    ///    No order required.
    ///
    ///    Mutually exclusive rules.
    ///
    ///    An order is required.
    ///
    pub fn order_ok(&self, rule2: &SomeRule, wanted: &SomeChange) -> bool {
        let reg2 = rule2.initial_region();

        let sb01 = self.b01.m_and(&wanted.b01);
        if sb01.is_not_low() {
            let reg2_0 = SomeMask {
                bts: reg2.state1.s_or(&reg2.state2).bts.b_not(),
            };
            let b01_0 = sb01.m_and(&reg2_0);
            if b01_0.is_not_low() {
                return false;
            }
        }

        let sb10 = self.b10.m_and(&wanted.b10);
        if sb10.is_not_low() {
            let reg2_1 = SomeMask {
                bts: reg2.state1.s_and(&reg2.state2).bts,
            };
            let b10_1 = sb10.m_and(&reg2_1);
            if b10_1.is_not_low() {
                return false;
            }
        }
        true
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
