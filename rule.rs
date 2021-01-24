// Rule struct for an Unorthodox Expert System

use crate::bits::NUM_BITS_PER_INT;
use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct SomeRule {
    pub b00: SomeMask,
    pub b01: SomeMask,
    pub b11: SomeMask,
    pub b10: SomeMask,
}

impl fmt::Display for SomeRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeRule {
    // Return a new rule struct instance
    pub fn new(sta1: &SomeState, sta2: &SomeState) -> Self {
        Self {
            b00: SomeMask::new(sta1.bts.b_not().b_and(&sta2.bts.b_not())),
            b01: SomeMask::new(sta1.bts.b_not().b_and(&sta2.bts)),
            b11: SomeMask::new(sta1.bts.b_and(&sta2.bts)),
            b10: SomeMask::new(sta1.bts.b_and(&sta2.bts.b_not())),
        }
    }

    // Return a mask of changes b01 or b10
    pub fn change_mask(&self) -> SomeMask {
        self.b01.m_or(&self.b10)
    }

    // Return true if a rule is a subset of another
    pub fn is_subset_of(&self, other: &Self) -> bool {
        let tmprul = self.intersection(other);

        *self == tmprul
    }

    // Return true if a rule is valid after a union (no 1X or 0X bits)
    pub fn is_valid_union(&self) -> bool {
        if self.b00.m_and(&self.b01).is_low() {
            if self.b11.m_and(&self.b10).is_low() {
                return true;
            }
        }
        false
    }

    // Return true if a rule is valid after an intersection (and),
    // no bit positions are zero in all 4 masks.
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

    // Return a logical OR of two rules
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

    // Return a logical AND of two rules
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

    pub fn initial_region(&self) -> SomeRegion {
        let st_high = SomeState::new(self.b11.bts.b_or(&self.b10.bts));
        let st_low = SomeState::new(self.b00.bts.b_or(&self.b01.bts).b_not());

        SomeRegion::new(&st_high, &st_low)
    }

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

    // Return the result region after applying an initial region to a rule
    pub fn result_from_initial(&self, reg: &SomeRegion) -> SomeRegion {
        if reg.intersects(&self.initial_region()) == false {
            panic!("result_from_initial: given region does not intersect the ruls initial region");
        }

        self.restrict_initial_region(reg).result_region()
    }

    // Return the result region after applying an initial region to a rule
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

    // Return the initial region after applying a result region to a rule
    pub fn initial_from_result(&self, reg: &SomeRegion) -> SomeRegion {
        if reg.intersects(&self.result_region()) == false {
            panic!("initial_from_result: given region does not intersect the ruls result region");
        }

        self.restrict_result_region(reg).initial_region()
    }
    // Restrict the intitial region to an intersection of the
    // given region.  Assuming the region given is not a superset
    // this will also change the result region.
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
    pub fn no_change(&self) -> bool {
        self.b01.is_low() && self.b10.is_low()
    }
    // Restrict the result region to an intersection of the
    // given region.  Assuming the region given is not a superset
    // this will also change the initial region.
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

    //    pub fn dont_care_mask(&self) -> SomeMask {
    //        self.b00
    //            .m_or(&self.b10)
    //            .m_or(&self.b11)
    //            .m_or(&self.b10)
    //            .m_not()
    //    }

    // Set initial region X bits to one
    pub fn set_initial_to_ones(&self, msk: &SomeMask) -> Self {
        let nmsk = msk.m_not();

        Self {
            b00: self.b00.m_and(&nmsk),
            b01: self.b01.m_and(&nmsk),
            b11: self.b11.clone(),
            b10: self.b10.clone(),
        }
    }

    // Set initial region X bits to zero
    pub fn set_initial_to_zeros(&self, msk: &SomeMask) -> Self {
        let nmsk = msk.m_not();

        Self {
            b00: self.b00.clone(),
            b01: self.b01.clone(),
            b11: self.b11.m_and(&nmsk),
            b10: self.b10.m_and(&nmsk),
        }
    }

    pub fn formatted_string_length(&self) -> usize {
        (NUM_BITS_PER_INT * self.b00.bts.len() * 3) - 1
    }

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
                strrc.push('/');
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
} // end SomeRule

// Create an rule that represents a change from one region to equal, or subset, another.
// X->X, 1->X and 0->X bit positions will have zero in all rule bit positions, signfying "don't care".
// Such an all-zeros column would cause the is_valid_intersection function to return false.
// For later operations with a second rule:
//   "don't care" + "do care" = "do care" (a column with one or two bits set)
//   "don't care" & "do care" = "don't care"
//   "don't care" ^ "do care" = "do care"
pub fn region_to_region(from: &SomeRegion, to: &SomeRegion) -> SomeRule {
    let f_ones = SomeMask::new(from.state1.bts.b_and(&from.state2.bts));
    let f_zeros = SomeMask::new(from.state1.bts.b_not().b_and(&from.state2.bts.b_not()));
    let f_xes = SomeMask::new(from.state1.bts.b_xor(&from.state2.bts));

    let t_ones = SomeMask::new(to.state1.bts.b_and(&to.state2.bts));
    let t_zeros = SomeMask::new(to.state1.bts.b_not().b_and(&to.state2.bts.b_not()));
    let t_xes = SomeMask::new(to.state1.bts.b_xor(&to.state2.bts));
    
    let bx1 = f_xes.m_and(&t_ones);
    let bx0 = f_xes.m_and(&t_zeros);
    
    let b1x = f_ones.m_and(&t_xes);
    let b0x = f_zeros.m_and(&t_xes);
    let bxx = f_xes.m_and(&t_xes);

    // Set the bits desired, the undesired bits end up as zeros
    let nb00 = f_zeros.m_and(&t_zeros).m_or(&bx0).m_or(&b0x).m_or(&bxx);

    let nb01 = f_zeros.m_and(&t_ones).m_or(&bx1).m_or(&b0x);

    let nb11 = f_ones.m_and(&t_ones).m_or(&bx1).m_or(&b1x).m_or(&bxx);

    let nb10 = f_ones.m_and(&t_zeros).m_or(&bx0).m_or(&b1x);

    // println!("from {} to {} = b01: {}  b10: {}", &from, &to, &nb01, &nb10);

    SomeRule {
        b00: nb00,
        b01: nb01,
        b11: nb11,
        b10: nb10,
    }
}

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
