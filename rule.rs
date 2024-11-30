//! Rule struct for, representing a change of bits between an initial and result state.
//!
//! If the initial regions of two rules intersect, that intersection can be taken,
//! which may be valid, invalid, or partially valid.
//!
//! In combining rules, the results 0->X and 1->X for a bit position are disallowed, since
//! there has to be some limit on combination that leaves a rule with predictive power.
//!
//! The rule can be used in a way that is like "forward chaining" (result_from_initial) and
//! "backward chaining" (intitial_from_result).

use crate::bits::NumBits;
use crate::change::{AccessChanges, SomeChange};
use crate::mask::SomeMask;
use crate::region::{AccessStates, SomeRegion};
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::tools::StrLen;

use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
/// A struct of four masks with the same number of bits.
/// They represent a sample, before/after.
pub struct SomeRule {
    /// A mask for bit change 0->0
    pub m00: SomeMask,
    /// A mask for bit change 0->1
    pub m01: SomeMask,
    /// A mask for bit change 1->1
    pub m11: SomeMask,
    /// A mask for bit change 1->0
    pub m10: SomeMask,
}

impl fmt::Display for SomeRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

impl SomeRule {
    /// Return a new SomeRule instance given an initial state and the corresponding result state.
    pub fn new(smpl: &SomeSample) -> Self {
        Self {
            m00: smpl
                .initial
                .bitwise_not()
                .bitwise_and_not(&smpl.result)
                .convert_to_mask(),
            m01: smpl.result.bitwise_and_not(&smpl.initial).convert_to_mask(),
            m11: smpl.initial.bitwise_and(&smpl.result).convert_to_mask(),
            m10: smpl.initial.bitwise_and_not(&smpl.result).convert_to_mask(),
        }
    }

    /// Generate a rule from a string.
    /// Al bit positions must be specified.
    /// like SomeRule::from_str("00/01/11/10/XX/xx/Xx/xX/X0/X1")
    pub fn from_str(str_in: &str) -> Result<Self, String> {
        let rep = str_in.trim();

        if rep.is_empty() {
            return Err("SomeRule::from_str: Empty string?".to_string());
        }

        // Initialize new mask strings.
        let mut m00 = String::from("m");
        let mut m01 = String::from("m");
        let mut m11 = String::from("m");
        let mut m10 = String::from("m");

        // Gather bit position tokens as pairs of valid characters.
        let mut token = String::with_capacity(2);

        for bt in rep.graphemes(true) {
            if bt == "/" || bt == "_" || bt == " " {
                continue;
            }
            // Add character to token.
            token.push_str(bt);

            // Process finished token.
            if token.len() == 2 {
                if token == "00" {
                    m00.push('1');
                    m01.push('0');
                    m11.push('0');
                    m10.push('0');
                } else if token == "01" {
                    m00.push('0');
                    m01.push('1');
                    m11.push('0');
                    m10.push('0');
                } else if token == "11" {
                    m00.push('0');
                    m01.push('0');
                    m11.push('1');
                    m10.push('0');
                } else if token == "10" {
                    m00.push('0');
                    m01.push('0');
                    m11.push('0');
                    m10.push('1');
                } else if token == "XX" || token == "xx" {
                    m00.push('1');
                    m01.push('0');
                    m11.push('1');
                    m10.push('0');
                } else if token == "Xx" || token == "xX" {
                    m00.push('0');
                    m01.push('1');
                    m11.push('0');
                    m10.push('1');
                } else if token == "X0" || token == "x0" {
                    m00.push('1');
                    m01.push('0');
                    m11.push('0');
                    m10.push('1');
                } else if token == "X1" || token == "x1" {
                    m00.push('0');
                    m01.push('1');
                    m11.push('1');
                    m10.push('0');
                } else if token == "1X" || token == "1x" {
                    m00.push('0');
                    m01.push('0');
                    m11.push('1');
                    m10.push('1');
                } else if token == "0X" || token == "0x" {
                    m00.push('1');
                    m01.push('1');
                    m11.push('0');
                    m10.push('0');
                } else {
                    return Err(format!("SomeRule::from_str: Unrecognized token {token}"));
                }
                token.clear();
            }
        }
        // Check for unfinished token.
        if token.is_empty() {
        } else {
            return Err(format!(
                "SomeRule::from_str: Did not understand token {token}"
            ));
        }

        // Get mask instances from bit strings.
        let m00 = SomeMask::from_str(&m00)?;
        let m01 = SomeMask::from_str(&m01)?;
        let m11 = SomeMask::from_str(&m11)?;
        let m10 = SomeMask::from_str(&m10)?;

        Ok(Self { m00, m01, m11, m10 })
    }

    /// Return true if a rule is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if let Some(tmprul) = self.intersection(other) {
            *self == tmprul
        } else {
            false
        }
    }

    /// Return true if a rule is a superset of another.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        if let Some(tmprul) = self.intersection(other) {
            *other == tmprul
        } else {
            false
        }
    }

    /// Return true if a rule is valid after a union (no 1X or 0X bits)
    pub fn is_valid_union(&self) -> bool {
        self.m00.bitwise_and(&self.m01).is_low() && self.m11.bitwise_and(&self.m10).is_low()
    }

    /// Return true if a rule is valid after an intersection,
    /// that is no bit positions are zero in all 4 masks.
    pub fn is_valid_intersection(&self) -> bool {
        self.m00
            .bitwise_or(&self.m01.bitwise_or(&self.m11.bitwise_or(&self.m10)))
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
        debug_assert_eq!(self.num_bits(), other.num_bits());

        let rule2 = Self {
            m00: self.m00.bitwise_or(&other.m00),
            m01: self.m01.bitwise_or(&other.m01),
            m11: self.m11.bitwise_or(&other.m11),
            m10: self.m10.bitwise_or(&other.m10),
        };
        if rule2.is_valid_union() {
            return Some(rule2);
        }

        // Pare down rule.
        let not_zero_x_mask = rule2.m00.bitwise_and(&rule2.m01).bitwise_not();
        let not_one_x_mask = rule2.m11.bitwise_and(&rule2.m10).bitwise_not();

        let rule3 = Self {
            m00: rule2.m00.bitwise_and(&not_zero_x_mask),
            m01: rule2.m01.bitwise_and(&not_zero_x_mask),
            m11: rule2.m11.bitwise_and(&not_one_x_mask),
            m10: rule2.m10.bitwise_and(&not_one_x_mask),
        };
        // Check for any null bit positions.
        if rule3.is_valid_intersection() {
            return Some(rule3);
        }
        None
    }

    /// Return a logical OR of two rules. The result may be invalid.
    pub fn union(&self, other: &Self) -> Option<Self> {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        let ret_rule = Self {
            m00: self.m00.bitwise_or(&other.m00),
            m01: self.m01.bitwise_or(&other.m01),
            m11: self.m11.bitwise_or(&other.m11),
            m10: self.m10.bitwise_or(&other.m10),
        };
        if ret_rule.is_valid_union() {
            return Some(ret_rule);
        }
        None
    }

    /// Return a logical AND of two rules.  The result may be invalid.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        let ret_rule = Self {
            m00: self.m00.bitwise_and(&other.m00),
            m01: self.m01.bitwise_and(&other.m01),
            m11: self.m11.bitwise_and(&other.m11),
            m10: self.m10.bitwise_and(&other.m10),
        };
        if ret_rule.is_valid_intersection() {
            return Some(ret_rule);
        }
        None
    }

    /// Return the initial region of a rule.
    pub fn initial_region(&self) -> SomeRegion {
        let st_high = self.m11.bitwise_or(&self.m10).convert_to_state();
        let st_low = self
            .m00
            .bitwise_or(&self.m01)
            .bitwise_not()
            .convert_to_state();

        SomeRegion::new(vec![st_high, st_low])
    }

    /// Return the result region of a rule.
    pub fn result_region(&self) -> SomeRegion {
        let st_high = self.m11.bitwise_or(&self.m01).convert_to_state();
        let st_low = self
            .m00
            .bitwise_or(&self.m10)
            .bitwise_not()
            .convert_to_state();

        // Change X->x positions to indicate a change, instead of appearing as X->X.
        let x_xnot_mask = self.m01.bitwise_and(&self.m10);

        SomeRegion::new(vec![
            st_high.bitwise_and_not(&x_xnot_mask),
            st_low.bitwise_or(&x_xnot_mask),
        ])
    }

    /// Return result from an input region.
    pub fn result_from_initial_region(&self, regx: &SomeRegion) -> SomeRegion {
        debug_assert_eq!(self.num_bits(), regx.num_bits());

        assert!(self.initial_region().intersects(regx));

        self.restrict_initial_region(regx).result_region()
    }

    /// Return the result region after applying an initial state to a rule.
    /// This could be called "forward chaining".
    pub fn result_from_initial_state(&self, sta: &SomeState) -> SomeState {
        debug_assert_eq!(self.num_bits(), sta.num_bits());

        if !self.initial_region().is_superset_of(sta) {
            panic!(
                "result_from_initial_state: given state is not a subset of the ruls initial region"
            );
        };
        let toggle: SomeMask = self
            .m01
            .bitwise_and_not(sta)
            .bitwise_or(&self.m10.bitwise_and(sta));
        sta.bitwise_xor(&toggle)
    }

    /// Restrict the initial region to an intersection of the
    /// given region.  Assuming the region given is not a superset
    /// this will also change the result region.
    pub fn restrict_initial_region(&self, other: &impl AccessStates) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        let init_reg = self.initial_region();

        if let Some(reg_int) = init_reg.intersection(other) {
            let zeros = reg_int.low_state().bitwise_not().convert_to_mask();
            let ones = reg_int.high_state().convert_to_mask();

            Self {
                m00: self.m00.bitwise_and(&zeros),
                m01: self.m01.bitwise_and(&zeros),
                m11: self.m11.bitwise_and(&ones),
                m10: self.m10.bitwise_and(&ones),
            }
        } else {
            panic!("other does not intersect rule initial region {init_reg}");
        }
    }

    /// Restrict the result region to an intersection of the
    /// given region.  Assuming the region given is not a superset
    /// this will also change the initial region.
    pub fn restrict_result_region(&self, regx: &SomeRegion) -> Self {
        debug_assert_eq!(self.num_bits(), regx.num_bits());

        //println!("restricting result region of {} to {}", self, regx);

        let rslt_reg = self.result_region();

        if let Some(reg_int) = regx.intersection(&rslt_reg) {
            let zeros = reg_int.low_state().bitwise_not().convert_to_mask();
            let ones = reg_int.high_state().convert_to_mask();

            Self {
                m00: self.m00.bitwise_and(&zeros),
                m01: self.m01.bitwise_and(&ones),
                m11: self.m11.bitwise_and(&ones),
                m10: self.m10.bitwise_and(&zeros),
            }
        } else {
            panic!("{regx} does not intersect rule result region {rslt_reg}");
        }
    }

    /// Return a string representation of SomeRule.
    fn formatted_str(&self) -> String {
        let mut strrc = String::with_capacity(self.strlen());

        let m00 = format!("{}", self.m00);
        let m01 = format!("{}", self.m01);
        let m11 = format!("{}", self.m11);
        let m10 = format!("{}", self.m10);

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

            // Change bit positions into a number, 0 to 15.
            let mut msk = 0;

            if c00 == "1" {
                msk += 1;
            }
            if c01 == "1" {
                msk += 2;
            }
            if c11 == "1" {
                msk += 4;
            }
            if c10 == "1" {
                msk += 8;
            }

            if msk == 0 {
                strrc.push_str("../"); // Will fail is_valid_intersection.
            } else if msk == 1 {
                strrc.push_str("00/");
            } else if msk == 2 {
                strrc.push_str("01/");
            } else if msk == 3 {
                strrc.push_str("0X/"); // Will fail is_valid_union.
            } else if msk == 4 {
                strrc.push_str("11/");
            } else if msk == 5 {
                strrc.push_str("XX/");
            } else if msk == 6 {
                strrc.push_str("X1/");
            } else if msk == 7 {
                strrc.push_str("11,0X?/"); // Will fail is_valid_union, parsed_union can remove 0X.
            } else if msk == 8 {
                strrc.push_str("10/");
            } else if msk == 9 {
                strrc.push_str("X0/");
            } else if msk == 10 {
                strrc.push_str("Xx/");
            } else if msk == 11 {
                strrc.push_str("10,0X?/"); // Will fail is_valid_union, parsed_union can remove 0X.
            } else if msk == 12 {
                strrc.push_str("1X/"); // Will fail is_valid_union.
            } else if msk == 13 {
                strrc.push_str("00,1X?/"); // Will fail is_valid_union, parsed_union can remove 1X.
            } else if msk == 14 {
                strrc.push_str("01,1X?/"); // Will fail is_valid_union, parsed_union can remove  1X.
            } else if msk == 15 {
                strrc.push_str("0X?,1X?/"); // Will fail is_valid_union.
            }
        } // next i

        strrc.pop();
        strrc
    }

    /// Of the wanted changes in a rule, the rule initial region may be manipulated to
    /// focus in on the desired changes.
    ///
    /// The change argument contains masks of changes (m01, m10) that are sought.
    ///
    /// For rule bit positions that are X->x, X->0 or X->1,
    /// the X value can be changed to focus on the desired change,
    /// or to mask out an unwanted change.
    pub fn restrict_for_changes(&self, rule_to_goal: &SomeRule) -> Option<SomeRule> {
        debug_assert_eq!(self.num_bits(), rule_to_goal.num_bits());

        let wanted_changes = rule_to_goal.wanted_changes();

        debug_assert!(wanted_changes
            .m01
            .bitwise_or(&wanted_changes.m10)
            .is_not_low()); // Some changes are needed.

        // Check if any rule changes are needed.
        if self.m01.bitwise_and(&wanted_changes.m01).is_low()
            && self.m10.bitwise_and(&wanted_changes.m10).is_low()
        {
            return None;
        }

        let mut rule_tmp = self.clone();

        // Get rule X->x mask.
        let x_not_x = rule_tmp.m01.bitwise_and(&rule_tmp.m10);

        // Mask out unneeded 1->0 changes in X->x bit positions.
        let not_10 = wanted_changes.m01.bitwise_and(&x_not_x);

        // Change selected X->x bit positions to 0->1.
        if not_10.is_not_low() {
            rule_tmp = rule_tmp.mask_out_ones(&not_10);
        }

        // Mask out unneeded 0->1 changes in X->x bit positions.
        let not_01 = wanted_changes.m10.bitwise_and(&x_not_x);

        // Change selected X->x bit positions to 1->0.
        if not_01.is_not_low() {
            rule_tmp = rule_tmp.mask_out_zeros(&not_01);
        }

        Some(rule_tmp)
    }

    /// Return true if two rules are mutually exclusive.
    /// Both rules lose wanted changes, running them in any order.
    ///
    /// For a change to pass from one rule through a second rule:
    ///    A wanted 0->1 change in first rule should correspond to a 1->1 in the second rule.
    ///    A wanted 1->0 change in first rule should correspond to a 0->0 in the second rule.
    pub fn mutually_exclusive(&self, other: &Self, wanted: &SomeChange) -> bool {
        // println!("starting, self {self} other {other} wanted {wanted}");
        debug_assert!(self.num_bits() == other.num_bits());
        debug_assert!(self.num_bits() == wanted.num_bits());

        self.sequence_blocks_changes(other, wanted) && other.sequence_blocks_changes(self, wanted)
    }

    /// Return true if all wanted changes in rule are blocked by running a second rule.
    ///
    /// The result region of the first rule may not intersect the initial region of the second rule.
    ///
    /// For a change to pass from one rule through a second rule:
    ///    A wanted 0->1 change in first rule should correspond to a 1 result in the second rule.
    ///    A wanted 1->0 change in first rule should correspond to a 0 result in the second rule.
    pub fn sequence_blocks_changes(&self, other: &Self, wanted: &SomeChange) -> bool {
        //println!("sequence_blocks_change: {} to {} change wanted {}", self, other, wanted);
        debug_assert!(self.num_bits() == other.num_bits());
        debug_assert!(self.num_bits() == wanted.num_bits());
        debug_assert!(wanted.is_not_low());

        // Get a mask of wanted 0->1 changes in target rule.
        let msk01 = self.m01.bitwise_and(&wanted.m01);

        // Get a mask of wanted 1->0 changes in target rule.
        let msk10 = self.m10.bitwise_and(&wanted.m10);

        debug_assert!(msk01.is_not_low() || msk10.is_not_low()); // Target rule must have at least one wanted change.

        // Combine rules, to take into account the possibility that changes may be implied
        // in moving from the result region of the target rule to the initial region of the second rule.
        let rule2 = self.combine_sequence(other);
        //format!("rule2 {rule2}");

        // Get a mask of wanted 0->1 changes in combined rule.
        let msk01 = rule2.m01.bitwise_and(&wanted.m01);

        // Get a mask of wanted 1->0 changes in combined rule.
        let msk10 = rule2.m10.bitwise_and(&wanted.m10);

        // Check if any 0->1 changes passed through.
        if rule2.m01.bitwise_and(&msk01).is_not_low() {
            return false;
        }
        // Check if any 1->0 changes passed through.
        if rule2.m10.bitwise_and(&msk10).is_not_low() {
            return false;
        }
        true
    }

    /// Return a SomeChange instance.
    pub fn to_change(&self) -> SomeChange {
        SomeChange::new(self.m01.clone(), self.m10.clone())
    }

    /// Return a rule for translating from a region to another region.
    /// The result may have a 0->X, or 1->X, bit position, in which case it will not pass a is-valid-union-test.
    pub fn new_region_to_region(from: &SomeRegion, to: &SomeRegion) -> SomeRule {
        debug_assert_eq!(from.num_bits(), to.num_bits());

        let from_x = from.x_mask();
        let from_1 = from.edge_ones_mask();
        let from_0 = from.edge_zeros_mask();

        let to_x = to.x_mask();
        let to_1 = to.edge_ones_mask();
        let to_0 = to.edge_zeros_mask();

        let x_to_0 = from_x.bitwise_and(&to_0);
        let x_to_1 = from_x.bitwise_and(&to_1);
        let x_to_x = from_x.bitwise_and(&to_x).bitwise_and(
            &from
                .first_state()
                .bitwise_xor(&to.first_state().bitwise_not()),
        );
        let x_to_xnot = from_x
            .bitwise_and(&to_x)
            .bitwise_and(&from.first_state().bitwise_xor(to.first_state()));

        // Incorporate usually dissallowed bit changes, interpreted as "change don't care".
        let zero_to_x = from_0.bitwise_and(&to_x);
        let one_to_x = from_1.bitwise_and(&to_x);

        SomeRule {
            m00: from_0
                .bitwise_and(&to_0)
                .bitwise_or(&x_to_0)
                .bitwise_or(&x_to_x)
                .bitwise_or(&zero_to_x),
            m01: from_0
                .bitwise_and(&to_1)
                .bitwise_or(&x_to_1)
                .bitwise_or(&zero_to_x)
                .bitwise_or(&x_to_xnot),
            m11: from_1
                .bitwise_and(&to_1)
                .bitwise_or(&x_to_1)
                .bitwise_or(&x_to_x)
                .bitwise_or(&one_to_x),
            m10: from_1
                .bitwise_and(&to_0)
                .bitwise_or(&x_to_0)
                .bitwise_or(&one_to_x)
                .bitwise_or(&x_to_xnot),
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
    pub fn _new_state_to_region(from: &SomeState, to: &SomeRegion) -> SomeRule {
        debug_assert_eq!(from.num_bits(), to.num_bits());

        let from_0 = from.bitwise_not();

        let to_x = to.x_mask();
        let to_1 = to.edge_ones_mask();
        let to_0 = to.edge_zeros_mask();

        let zero_to_x = to_x.bitwise_and(&from_0);
        let one_to_x = to_x.bitwise_and(from);

        SomeRule {
            m00: to_0.bitwise_and(&from_0).bitwise_or(&zero_to_x),
            m01: to_1.bitwise_and(&from_0),
            m11: to_1.bitwise_and(from).bitwise_or(&one_to_x),
            m10: to_0.bitwise_and(from),
        }
    }

    /// Return the number of bits changed in a rule.
    pub fn num_bits_changed(&self) -> usize {
        self.m01.bitwise_or(&self.m10).num_one_bits()
    }

    /// Return true if a rule causes predictable change.
    pub fn causes_predictable_change(&self) -> bool {
        self.m10.is_not_low() || self.m01.is_not_low()
    }

    /// Combine two rules into a single, aggregate, rule.
    pub fn combine_sequence(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        // Check if one rule naturally leads to the next.
        if self.result_region().intersects(&other.initial_region()) {
            return self.combine_sequence2(other);
        }

        // Get a rule that will connect the twe rules.
        let rule_between =
            Self::new_region_to_region(&self.result_region(), &other.initial_region());

        // Combine the three rules.
        self.combine_sequence2(&rule_between)
            .combine_sequence2(other)
    }

    /// Combine two rules.
    /// The result region of the first rule must intersect the initial region of the second rule.
    /// Changes in the first rule may be reversed in the second rule.
    fn combine_sequence2(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        assert!(self.result_region().intersects(&other.initial_region()));

        Self {
            m00: self
                .m00
                .bitwise_and(&other.m00)
                .bitwise_or(&self.m01.bitwise_and(&other.m10)),
            m01: self
                .m01
                .bitwise_and(&other.m11)
                .bitwise_or(&self.m00.bitwise_and(&other.m01)),
            m11: self
                .m11
                .bitwise_and(&other.m11)
                .bitwise_or(&self.m10.bitwise_and(&other.m01)),
            m10: self
                .m10
                .bitwise_and(&other.m00)
                .bitwise_or(&self.m11.bitwise_and(&other.m10)),
        }
    }

    /// Return the number bits used in a rule's masks.
    pub fn num_bits(&self) -> usize {
        self.m00.num_bits()
    }

    /// Return a rule with selected 1 bit positions masked off.
    fn mask_out_ones(&self, msk_out: &SomeMask) -> Self {
        let msk_in = msk_out.bitwise_not();
        let ret = Self {
            m00: self.m00.clone(),
            m01: self.m01.clone(),
            m11: self.m11.bitwise_and(&msk_in),
            m10: self.m10.bitwise_and(&msk_in),
        };
        assert!(ret.is_valid_intersection()); // Check for at least one bit set in each position.
        ret
    }

    /// Return a rule with selected 0 bit positions masked off.
    fn mask_out_zeros(&self, msk_out: &SomeMask) -> Self {
        let msk_in = msk_out.bitwise_not();
        let ret = Self {
            m00: self.m00.bitwise_and(&msk_in),
            m01: self.m01.bitwise_and(&msk_in),
            m11: self.m11.clone(),
            m10: self.m10.clone(),
        };
        assert!(ret.is_valid_intersection()); // Check for at least one bit set in each position.
        ret
    }

    /// Return a mask of "change care" bit positions.
    fn change_care_mask(&self) -> SomeMask {
        self.result_region().edge_mask()
    }

    /// Return a change containing wanted changes to achieve the rule goal.
    pub fn wanted_changes(&self) -> SomeChange {
        self.to_change().bitwise_and(&self.change_care_mask())
    }

    /// Return a change containing unwanted changes to achieve the rule goal.
    /// Unwanted changes are not fatal, but lead off the "glide path" straight from the current
    /// state to the goal (current_state.union(goal)).
    /// An unwanted change of 0->1 in a bit position becomes
    /// wanted 1->0 change in the next step, canceling the unwanted change.
    pub fn unwanted_changes(&self) -> SomeChange {
        let care_mask = self.change_care_mask();

        let m00 = self.m00.bitwise_and(&care_mask);
        let m11 = self.m11.bitwise_and(&care_mask);

        SomeChange::new(m00, m11)
    }
} // end impl SomeRule

/// Implement the trait StrLen for SomeRule.
impl StrLen for SomeRule {
    fn strlen(&self) -> usize {
        (3 * self.num_bits()) - 1
    }
}

impl AccessChanges for SomeRule {
    fn m01(&self) -> &SomeMask {
        &self.m01
    }
    fn m10(&self) -> &SomeMask {
        &self.m10
    }
}

/// Implement the NumBits trait for SomeRule.
impl NumBits for SomeRule {
    fn num_bits(&self) -> usize {
        self.num_bits()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsed_union() -> Result<(), String> {
        let rul1 = SomeRule::from_str("XX/XX/Xx/Xx/X0/X0")?;
        let rul2 = SomeRule::from_str("X0/X1/X0/X1/01/11")?;
        if let Some(rint) = rul1.parsed_union(&rul2) {
            println!("{} int {} = {}", rul1, rul2, rint);
            assert!(rint == SomeRule::from_str("00/11/10/01/10/00")?);
        } else {
            return Err(format!("{} int {} is None?", rul1, rul2));
        }

        let rul1 = SomeRule::from_str("X1/X1/XX/XX/Xx/Xx")?;
        let rul2 = SomeRule::from_str("00/10/01/10/00/11")?;
        if let Some(rint) = rul1.parsed_union(&rul2) {
            println!("{} int {} = {}", rul1, rul2, rint);
            assert!(rint == SomeRule::from_str("11/01/11/00/10/01")?);
        } else {
            return Err(format!("{} int {} is None?", rul1, rul2));
        }

        Ok(())
    }

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_rul = SomeRule::new(&SomeSample::from_str("s0000_0000->s0000_0000")?);

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_rul = SomeRule::new(&SomeSample::from_str(
            "s0000_0000_0000_0000->s0000_0000_0000_0000",
        )?); //tmp_sta.clone(), tmp_sta.clone()));

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_rul = SomeRule::new(&SomeSample::from_str("s0_0000->s0_0000")?);

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_rul = SomeRule::new(&SomeSample::from_str("s0000->s0000")?);

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn new() -> Result<(), String> {
        let rule_from_sample = SomeRule::new(&SomeSample::from_str("s0101->s0011")?);

        let rule_from_masks = SomeRule {
            m00: SomeMask::from_str("m0111")?.bitwise_not(),
            m01: SomeMask::from_str("m0010")?,
            m11: SomeMask::from_str("m0001")?,
            m10: SomeMask::from_str("m0100")?,
        };

        let rule_from_string = SomeRule::from_str("00/10/01/11")?;

        println!("rule_from_sample: {rule_from_sample} rule_from_masks: {rule_from_masks}");
        assert!(rule_from_sample == rule_from_masks);

        println!("rule_from_sample: {rule_from_sample} rule_from_string: {rule_from_string}");
        assert!(rule_from_sample == rule_from_string);

        Ok(())
    }

    #[test]
    fn initial_region() -> Result<(), String> {
        let rulx = SomeRule::from_str("XX/Xx/X0/X1_00/01/11/10")?;

        println!("initial region {}", rulx.initial_region());
        assert!(rulx.initial_region() == SomeRegion::from_str("rXXXX_0011")?);

        println!("result region  {}", rulx.result_region());
        // Check result formatted string, to differentiate between X->X and X->x.
        assert!(format!("{}", rulx.result_region()) == "rXx01_0110");

        Ok(())
    }

    #[test]
    fn result_region() -> Result<(), String> {
        let rulx = SomeRule::from_str("XX/Xx/X0/X1_00/01/11/10")?;
        println!("result region {}", rulx.result_region());

        assert!(format!("{}", rulx.result_region()) == "rXx01_0110");

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let rul1 = SomeRule::from_str("01/01/01/00/00/00/11/11/11/10/10/10/XX/XX/XX/XX/XX/Xx/Xx/Xx/Xx/Xx/X0/X0/X0/X0/X0/X1/X1/X1/X1/X1")?;
        let rul2 = SomeRule::from_str("01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx")?;
        let rul3 = SomeRule::from_str("01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11")?;
        let Some(rul_int) = rul1.intersection(&rul2) else {
            panic!("this should work!");
        };

        println!("rul1: {rul1} rul2: {rul2} rul_int: {rul_int}");
        assert!(rul_int == rul3);

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let rul1 = SomeRule::from_str("01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx")?;
        let rul2 = SomeRule::from_str("01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11")?;

        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul2.is_subset_of(&rul1));
        assert!(!rul1.is_subset_of(&rul2));

        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let rul1 = SomeRule::from_str("01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx")?;
        let rul2 = SomeRule::from_str("01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11")?;

        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.is_superset_of(&rul2));
        assert!(!rul2.is_superset_of(&rul1));

        Ok(())
    }

    #[test]
    fn is_valid_intersection() -> Result<(), String> {
        let rul1 = SomeRule::from_str("XX")?;
        let rul2 = SomeRule::from_str("X1")?;
        let rul3 = SomeRule::from_str("00")?;

        println!("rul1: {rul1} rul2: {rul2} rul3: {rul3}");
        assert!(rul1.intersection(&rul2).is_some());
        assert!(rul2.intersection(&rul3).is_none());

        Ok(())
    }

    #[test]
    fn is_valid_union() -> Result<(), String> {
        let rul1 = SomeRule::from_str("00")?;
        let rul2 = SomeRule::from_str("01")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_none());

        let rul1 = SomeRule::from_str("11")?;
        let rul2 = SomeRule::from_str("10")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_none());

        let rul1 = SomeRule::from_str("11")?;
        let rul2 = SomeRule::from_str("01")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_some());

        let rul1 = SomeRule::from_str("x1")?;
        let rul2 = SomeRule::from_str("00")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_none());

        Ok(())
    }

    #[test]
    fn sequence_blocks_changes() -> Result<(), String> {
        // All possible change pass-through conditions can be tested at once.
        let rul1 = SomeRule::from_str("01/01/01/10/10/10")?;
        let rul2 = SomeRule::from_str("11/X1/XX/00/X0/XX")?;
        let chg1 = SomeChange::from_str("01/01/01/10/10/10")?;
        println!("rul1 {rul1}\nrul2 {rul2}\nchg1 {chg1}");
        assert!(!rul1.sequence_blocks_changes(&rul2, &chg1));

        // Change non pass-through conditions must be tested one-by-one.

        // Test 0->1 non pass-through conditions.
        let rul1 = SomeRule::from_str("01")?;
        let rul2 = SomeRule::from_str("10")?;
        let chg1 = SomeChange::from_str("01")?;
        println!("rul1 {rul1}\nrul2 {rul2}\nchg1 {chg1}");
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::from_str("01")?;
        println!("rul1 {rul1}\nrul2 {rul2}\nchg1 {chg1}");
        assert!(!rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::from_str("X0")?;
        println!("rul1 {rul1}\nrul2 {rul2}\nchg1 {chg1}");
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::from_str("Xx")?;
        println!("rul1 {rul1}\nrul2 {rul2}\nchg1 {chg1}");
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        // Test 1->0 non pass-through conditions.
        let rul1 = SomeRule::from_str("10")?;
        let rul2 = SomeRule::from_str("01")?;
        let chg1 = SomeChange::from_str("10")?;
        println!("rul1 {rul1}\nrul2 {rul2}\nchg1 {chg1}");
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::from_str("10")?;
        println!("rul1 {rul1}\nrul2 {rul2}\nchg1 {chg1}");
        assert!(!rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::from_str("X1")?;
        println!("rul1 {rul1}\nrul2 {rul2}\nchg1 {chg1}");
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::from_str("Xx")?;
        println!("rul1 {rul1}\nrul2 {rul2}\nchg1 {chg1}");
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        Ok(())
    }

    #[test]
    fn restrict_for_changes() -> Result<(), String> {
        let rul1 = SomeRule::from_str("X0/X1_Xx/10/Xx/Xx_X0/X1/X0/X1/X0/X1")?;

        let rule_to_goal = SomeRule::new_region_to_region(
            &SomeRegion::from_str("10_0001_100011")?,
            &SomeRegion::from_str("XX_X010_010011")?,
        );

        println!("rule_to_goal {rule_to_goal}");
        println!("rul1         {rul1}");

        let rul3 = SomeRule::from_str("X0/X1/Xx/10_01/10/X0/X1_X0/X1/X0/X1")?;

        if let Some(rul2) = rul1.restrict_for_changes(&rule_to_goal) {
            println!("rul2         {rul2}");
            println!("rul3         {rul3}");
            assert!(rul2 == rul3);
        } else {
            panic!("rul2 restriction should succeed");
        }

        Ok(())
    }

    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        let rul1 = SomeRule::from_str("X1/X0/Xx/Xx")?;
        let rul2 = rul1.restrict_initial_region(&SomeRegion::from_str("r10X1")?);

        println!("rul2: {rul2}");
        assert!(rul2 == SomeRule::from_str("11/00/Xx/10")?);

        Ok(())
    }

    #[test]
    fn restrict_result_region() -> Result<(), String> {
        let rul1 = SomeRule::from_str("Xx/Xx/XX/XX")?;
        let rul2 = rul1.restrict_result_region(&SomeRegion::from_str("r1010")?);

        println!("rul2: {rul2}");
        assert!(rul2 == SomeRule::from_str("01/10/11/00")?);

        Ok(())
    }

    #[test]
    fn result_from_initial_state() -> Result<(), String> {
        let rul1 = SomeRule::from_str("Xx/XX/x1/x0/xx/xx")?;
        let sta1 = SomeState::from_str("s000110")?;

        let sta2 = rul1.result_from_initial_state(&sta1);
        println!("rul1: {rul1} sta1: {sta1} sta2: {sta2}");

        assert!(sta2 == SomeState::from_str("s101010")?);

        Ok(())
    }

    #[test]
    fn result_from_initial_region() -> Result<(), String> {
        let rul1 = SomeRule::from_str("XX/01/XX/10/x1/x1/x0/x0/Xx/Xx/Xx/XX/XX/XX")?;
        let reg1 = SomeRegion::from_str("r00_11_01_01_x01_x01")?;

        let reg2 = rul1.result_from_initial_region(&reg1);
        println!("rul1: {rul1} reg1: {reg1} result: {reg2}");

        assert!(reg2 == SomeRegion::from_str("r01_10_11_00_x10_x01")?);

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let rul1 = SomeRule::from_str("00/01/00/01/xx")?;
        let rul2 = SomeRule::from_str("00/01/10/10/11")?;

        let Some(rul3) = rul1.union(&rul2) else {
            panic!("This should work!");
        };
        println!("rul3 = {rul3}");
        assert!(rul3 == SomeRule::from_str("00/01/x0/Xx/xx")?);

        Ok(())
    }

    #[test]
    fn combine_sequence() -> Result<(), String> {
        // Test 0->0
        let rul1 = SomeRule::from_str("00/00/00/00/00/00")?;
        let rul2 = SomeRule::from_str("00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("00/01/00/01/00/01")?;
        assert!(rul3 == rul4);

        // Test 0->1
        let rul1 = SomeRule::from_str("01/01/01/01/01/01")?;
        let rul2 = SomeRule::from_str("11/10/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("01/00/00/01/01/00")?;
        assert!(rul3 == rul4);

        // Test 1->1
        let rul1 = SomeRule::from_str("11/11/11/11/11/11")?;
        let rul2 = SomeRule::from_str("11/10/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("11/10/10/11/11/10")?;
        assert!(rul3 == rul4);

        // Test 1->0
        let rul1 = SomeRule::from_str("10/10/10/10/10/10")?;
        let rul2 = SomeRule::from_str("00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("10/11/10/11/10/11")?;
        assert!(rul3 == rul4);

        // Test X->0
        let rul1 = SomeRule::from_str("X0/X0/X0/X0/X0/X0")?;
        let rul2 = SomeRule::from_str("00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("X0/X1/X0/X1/X0/X1")?;
        assert!(rul3 == rul4);

        // Test X->1
        let rul1 = SomeRule::from_str("X1/X1/X1/X1/X1/X1")?;
        let rul2 = SomeRule::from_str("11/10/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("X1/X0/X0/X1/X1/X0")?;
        assert!(rul3 == rul4);

        // Test X->X
        let rul1 = SomeRule::from_str("XX/XX/XX/XX/XX/XX/XX/XX")?;
        let rul2 = SomeRule::from_str("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("11/10/00/01/X0/X1/XX/Xx")?;
        assert!(rul3 == rul4);

        // Test X->X
        let rul1 = SomeRule::from_str("XX/XX/XX/XX/XX/XX/XX/XX")?;
        let rul2 = SomeRule::from_str("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("11/10/00/01/X0/X1/XX/Xx")?;
        assert!(rul3 == rul4);

        // Test X->X
        let rul1 = SomeRule::from_str("XX/XX/XX/XX/XX/XX/XX/XX")?;
        let rul2 = SomeRule::from_str("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("11/10/00/01/X0/X1/XX/Xx")?;
        assert!(rul3 == rul4);

        // Test X->x
        let rul1 = SomeRule::from_str("Xx/Xx/Xx/Xx/Xx/Xx/Xx/Xx")?;
        let rul2 = SomeRule::from_str("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::from_str("01/00/10/11/X0/X1/Xx/XX")?;
        assert!(rul3 == rul4);

        // Test 1->X. This is normally invalid in a rule, it is interpreted as "change don't care"
        let rul1 = SomeRule::new_region_to_region(
            &SomeRegion::from_str("1111_1111")?,
            &SomeRegion::from_str("XXXX_XXXX")?,
        );

        let rul2 = SomeRule::from_str("11/10/00/01_X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::new_region_to_region(
            &SomeRegion::from_str("1111_1111")?,
            &SomeRegion::from_str("1001_01XX")?,
        );

        assert!(rul3 == rul4);

        // Test 0->X. This is normally invalid in a rule, it is interpreted as "change don't care"
        let rul1 = SomeRule::new_region_to_region(
            &SomeRegion::from_str("0000_0000")?,
            &SomeRegion::from_str("XXXX_XXXX")?,
        );

        let rul2 = SomeRule::from_str("11/10/00/01_X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul3 {}", rul3.to_string());

        let rul4 = SomeRule::new_region_to_region(
            &SomeRegion::from_str("0000_0000")?,
            &SomeRegion::from_str("1001_01XX")?,
        );

        assert!(rul3 == rul4);

        Ok(())
    }

    #[test]
    fn new_region_to_region() -> Result<(), String> {
        let reg1 = SomeRegion::from_str("r000_111_XXX_Xx")?;
        let reg2 = SomeRegion::from_str("r01X_01X_01X_xX")?;
        let rul1 = SomeRule::new_region_to_region(&reg1, &reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1: {rul1}");

        assert!(rul1 == SomeRule::from_str("00/01/0X_10/11/1X/X0_X1/XX_Xx/Xx")?);

        Ok(())
    }

    #[test]
    fn mutually_exclusive() -> Result<(), String> {
        let chg1 = SomeChange::from_str("Xx/XX")?;

        let rul1 = SomeRule::from_str("01/01")?;
        let rul2 = SomeRule::from_str("11/10")?;
        let rslt = rul1.mutually_exclusive(&rul2, &chg1);
        println!("{rul1} mutually exclusive {rul2} is {rslt}");
        assert!(!rslt);

        let rul1 = SomeRule::from_str("01/01")?;
        let rul2 = SomeRule::from_str("10/11")?;
        let rslt = rul1.mutually_exclusive(&rul2, &chg1);
        println!("{rul1} mutually exclusive {rul2} is {rslt}");
        assert!(rslt);

        Ok(())
    }

    #[test]
    fn change_care_mask() -> Result<(), String> {
        let rul1 = SomeRule::new_region_to_region(
            &SomeRegion::from_str("0010_01XX")?,
            &SomeRegion::from_str("1001_XXXX")?,
        );
        let msk = rul1.change_care_mask();
        println!("care {msk}");

        assert!(msk == SomeMask::from_str("1111_0000")?);

        Ok(())
    }

    #[test]
    fn wanted_changes() -> Result<(), String> {
        let rul1 = SomeRule::new_region_to_region(
            &SomeRegion::from_str("XX_0101_01XX")?,
            &SomeRegion::from_str("01_0110_XXXX")?,
        );
        let cng = rul1.wanted_changes();
        println!("wanted {cng}");

        assert!(cng.m01 == SomeMask::from_str("01_0010_0000")?);
        assert!(cng.m10 == SomeMask::from_str("10_0001_0000")?);

        Ok(())
    }

    #[test]
    fn unwanted_changes() -> Result<(), String> {
        let rul1 = SomeRule::new_region_to_region(
            &SomeRegion::from_str("XX_0101_01XX")?,
            &SomeRegion::from_str("01_0110_XXXX")?,
        );
        let cng = rul1.unwanted_changes();
        println!("unwanted {cng}");

        assert!(cng.m01 == SomeMask::from_str("10_1000_0000")?);
        assert!(cng.m10 == SomeMask::from_str("01_0100_0000")?);

        Ok(())
    }
} // end tests
