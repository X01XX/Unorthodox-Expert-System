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

extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
/// A struct of four masks with the same number of bits.
/// They represent a sample, before/after.
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
                .bitwise_and_not(&smpl.result)
                .convert_to_mask(),
            b01: smpl.result.bitwise_and_not(&smpl.initial).convert_to_mask(),
            b11: smpl.initial.bitwise_and(&smpl.result).convert_to_mask(),
            b10: smpl.initial.bitwise_and_not(&smpl.result).convert_to_mask(),
        }
    }

    /// Generate a rule from a string.
    /// Al bit positions must be specified.
    /// like SomeRule::new_from_string("00/01/11/10/XX/xx/Xx/xX/X0/X1")
    pub fn new_from_string(rep: &str) -> Result<Self, String> {
        // Check for empty string.
        if rep.is_empty() {
            return Err(format!(
                "SomeRule::new_from_string: No bit positions specified in {}",
                &rep
            ));
        }

        // Initialize new mask strings.
        let mut b00 = String::from("m0b");
        let mut b01 = String::from("m0b");
        let mut b11 = String::from("m0b");
        let mut b10 = String::from("m0b");

        // Gather bit position tokens as pairs of valid characters.
        let mut token = String::with_capacity(2);

        for bt in rep.graphemes(true) {
            if bt == "/" || bt == "_" {
                continue;
            }
            // Add character to token.
            token.push_str(bt);

            // Process finished token.
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
                } else if token == "1X" || token == "1x" {
                    b00.push('0');
                    b01.push('0');
                    b11.push('1'); // Assume minimum change.
                    b10.push('0');
                } else if token == "0X" || token == "0x" {
                    b00.push('1'); // Assume minimum change.
                    b01.push('0');
                    b11.push('0');
                    b10.push('0');
                } else {
                    return Err(format!(
                        "SomeRule::new_from_string: Unrecognized token {}",
                        &token
                    ));
                }
                token.clear();
            }
        }
        // Check for unfinished token.
        if token.is_empty() {
        } else {
            return Err(format!(
                "SomeRule::new_from_string: Did not understand token {}",
                &token
            ));
        }

        // Get mask instances from bit strings.
        let b00 = SomeMask::new_from_string(&b00)?;
        let b01 = SomeMask::new_from_string(&b01)?;
        let b11 = SomeMask::new_from_string(&b11)?;
        let b10 = SomeMask::new_from_string(&b10)?;

        Ok(Self { b00, b01, b11, b10 })
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
        debug_assert_eq!(self.num_bits(), other.num_bits());

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
        let not_zero_x_mask = rule2.b00.bitwise_and(&rule2.b01).bitwise_not();
        let not_one_x_mask = rule2.b11.bitwise_and(&rule2.b10).bitwise_not();

        let rule3 = Self {
            b00: rule2.b00.bitwise_and(&not_zero_x_mask),
            b01: rule2.b01.bitwise_and(&not_zero_x_mask),
            b11: rule2.b11.bitwise_and(&not_one_x_mask),
            b10: rule2.b10.bitwise_and(&not_one_x_mask),
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
        debug_assert_eq!(self.num_bits(), other.num_bits());

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
        let st_high = self.b11.bitwise_or(&self.b10).convert_to_state();
        let st_low = self
            .b00
            .bitwise_or(&self.b01)
            .bitwise_not()
            .convert_to_state();

        SomeRegion::new(vec![st_high, st_low])
    }

    /// Return the result region of a rule.
    pub fn result_region(&self) -> SomeRegion {
        let st_high = self.b11.bitwise_or(&self.b01).convert_to_state();
        let st_low = self
            .b00
            .bitwise_or(&self.b10)
            .bitwise_not()
            .convert_to_state();

        // Change X->x positions to indicate a change, instead of appearing as X->X.
        let x_xnot_mask = self.b01.bitwise_and(&self.b10);

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
            .b01
            .bitwise_and_not(sta)
            .bitwise_or(&self.b10.bitwise_and(sta));
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
                b00: self.b00.bitwise_and(&zeros),
                b01: self.b01.bitwise_and(&zeros),
                b11: self.b11.bitwise_and(&ones),
                b10: self.b10.bitwise_and(&ones),
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
                b00: self.b00.bitwise_and(&zeros),
                b01: self.b01.bitwise_and(&ones),
                b11: self.b11.bitwise_and(&ones),
                b10: self.b10.bitwise_and(&zeros),
            }
        } else {
            panic!("{regx} does not intersect rule result region {rslt_reg}");
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
                strrc.push_str("0X?/"); // Will fail is_valid_union.
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
                strrc.push_str("1X?/"); // Will fail is_valid_union.
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
    pub fn restrict_for_changes(&self, change_needed: &SomeChange) -> Vec<SomeRule> {
        debug_assert_eq!(self.num_bits(), change_needed.num_bits());

        // Restrict change to what applies to the rule.
        let net_change_needed = change_needed.restrict_to(&self.initial_region());

        debug_assert!(net_change_needed.is_not_low()); // Some change is required.
        debug_assert!(net_change_needed
            .b01
            .bitwise_and(&net_change_needed.b10)
            .is_low()); // Changes should not cancel.

        // Init return RuleStore.
        let mut ret_rules = Vec::<SomeRule>::new();

        // Get rule initial region.
        let init_reg = self.initial_region();
        let init_reg_zeros = init_reg.edge_zeros_mask();
        let init_reg_ones = init_reg.edge_ones_mask();
        let init_reg_xs = init_reg.x_mask();

        if net_change_needed.b01.is_not_low() {
            // Process individual 0->1 changes needed.
            let change_bits = net_change_needed.b01.split();

            for m01x in change_bits.iter() {
                // Check if the rule applies to the needed change.
                if m01x.bitwise_and(&self.b01).is_low() {
                    continue;
                }

                // Restrict (if needed) and store rule.
                if m01x.bitwise_and(&init_reg_zeros).is_not_low() {
                    if ret_rules.contains(self) {
                    } else {
                        ret_rules.push(self.clone());
                    }
                } else if m01x.bitwise_and(&init_reg_xs).is_not_low() {
                    let ireg = init_reg.set_to_zeros(m01x);
                    let ruley = self.restrict_initial_region(&ireg);
                    ret_rules.push(ruley);
                }
            }
        }

        if net_change_needed.b10.is_not_low() {
            // Process individual 1->0 changes needed.
            let change_bits = net_change_needed.b10.split();

            for m10x in change_bits.iter() {
                // Check if the rule applies to the needed change.
                if m10x.bitwise_and(&self.b10).is_low() {
                    continue;
                }

                // Restrict (if needed) and store rule.
                if m10x.bitwise_and(&init_reg_ones).is_not_low() {
                    if ret_rules.contains(self) {
                    } else {
                        ret_rules.push(self.clone());
                    }
                } else if m10x.bitwise_and(&init_reg_xs).is_not_low() {
                    let ireg = init_reg.set_to_ones(m10x);
                    let ruley = self.restrict_initial_region(&ireg);
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
        debug_assert!(self.num_bits() == other.num_bits());
        debug_assert!(self.num_bits() == wanted.num_bits());

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
        debug_assert!(self.num_bits() == other.num_bits());
        debug_assert!(self.num_bits() == wanted.num_bits());

        debug_assert!(wanted.is_not_low());

        let msk01 = self.b01.bitwise_and(&wanted.b01);

        let msk01_11 = msk01.bitwise_and(&other.b11); // Matches 1->1, X->1 and X->X.

        if msk01_11.is_not_low() {
            return false;
        }

        let msk10 = self.b10.bitwise_and(&wanted.b10);

        let msk10_00 = msk10.bitwise_and(&other.b00); // Matches 0->0, X->0 and X->X.

        if msk10_00.is_not_low() {
            return false;
        }

        debug_assert!(msk01.is_not_low() || msk10.is_not_low()); // Target rule must have at least one wanted change.

        true
    }

    /// Return a SomeChange instance.
    pub fn to_change(&self) -> SomeChange {
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
    pub fn _new_state_to_region(from: &SomeState, to: &SomeRegion) -> SomeRule {
        debug_assert_eq!(from.num_bits(), to.num_bits());

        let from_0 = from.bitwise_not();

        let to_x = to.x_mask();
        let to_1 = to.edge_ones_mask();
        let to_0 = to.edge_zeros_mask();

        let zero_to_x = to_x.bitwise_and(&from_0);
        let one_to_x = to_x.bitwise_and(from);

        SomeRule {
            b00: to_0.bitwise_and(&from_0).bitwise_or(&zero_to_x),
            b01: to_1.bitwise_and(&from_0),
            b11: to_1.bitwise_and(from).bitwise_or(&one_to_x),
            b10: to_0.bitwise_and(from),
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

    /// Combine two rules.
    /// The result region of the first rule must intersect the initial region of the second rule.
    /// Changes in the first rule may be reversed in the second rule.
    pub fn combine_pair_sequence(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());

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

    /// Return the number bits used in a rule's masks.
    pub fn num_bits(&self) -> usize {
        self.b00.num_bits()
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

/// Implement the NumBits trait for SomeRule.
impl NumBits for SomeRule {
    fn num_bits(&self) -> usize {
        self.num_bits()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools;

    #[test]
    fn parsed_union() -> Result<(), String> {
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
    fn strlen() -> Result<(), String> {
        let tmp_rul = SomeRule::new(&SomeSample::new_from_string("0b0000_0000->0b0000_0000")?); //(tmp_sta.clone(), tmp_sta.clone()));

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_rul = SomeRule::new(&SomeSample::new_from_string(
            "0b0000_0000_0000_0000->0b0000_0000_0000_0000",
        )?); //tmp_sta.clone(), tmp_sta.clone()));

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_rul = SomeRule::new(&SomeSample::new_from_string("0b0_0000->0b0_0000")?); //(tmp_sta.clone(), tmp_sta.clone()));

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        let tmp_rul = SomeRule::new(&SomeSample::new_from_string("0b0000->0b0000")?); //(tmp_sta.clone(), tmp_sta.clone()));

        let strrep = format!("{tmp_rul}");
        let len = strrep.len();
        let calc_len = tmp_rul.strlen();
        println!("str {tmp_rul} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }

    #[test]
    fn new() -> Result<(), String> {
        let rule_from_sample = SomeRule::new(&SomeSample::new_from_string("0b0101->0b0011")?); //(

        let rule_from_masks = SomeRule {
            b00: SomeMask::new_from_string("0x7")?.bitwise_not(),
            b01: SomeMask::new_from_string("0b0010")?,
            b11: SomeMask::new_from_string("0b0001")?,
            b10: SomeMask::new_from_string("0b0100")?,
        };

        let rule_from_string = SomeRule::new_from_string("00/10/01/11")?;

        println!("rule_from_sample: {rule_from_sample} rule_from_masks: {rule_from_masks}");
        assert!(rule_from_sample == rule_from_masks);

        println!("rule_from_sample: {rule_from_sample} rule_from_string: {rule_from_string}");
        assert!(rule_from_sample == rule_from_string);

        Ok(())
    }

    #[test]
    fn initial_region() -> Result<(), String> {
        let rulx = SomeRule::new_from_string("XX/Xx/X0/X1_00/01/11/10")?;

        println!("initial region {}", rulx.initial_region());
        assert!(rulx.initial_region() == SomeRegion::new_from_string("rXXXX_0011")?);

        println!("result region {}", rulx.result_region());
        // Check result formatted string, to differentiate between X->X and X->x.
        assert!(format!("{}", rulx.result_region()) == "rXx01_0110");

        Ok(())
    }

    #[test]
    fn result_region() -> Result<(), String> {
        let rulx = SomeRule::new_from_string("XX/Xx/X0/X1_00/01/11/10")?;
        println!("result region {}", rulx.result_region());

        assert!(format!("{}", rulx.result_region()) == "rXx01_0110");

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
        assert!(!rul1.is_subset_of(&rul2));

        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("01/X1/Xx/00/xx/x0/11/x1/xx/10/Xx/x0/xx/11/00/X0/X1/Xx/10/01/X0/X1/X0/00/10/Xx/XX/X1/11/01/Xx/xx")?;
        let rul2 = SomeRule::new_from_string("01/01/01/00/00/00/11/11/11/10/10/10/xx/11/00/00/11/Xx/10/01/10/01/X0/00/10/10/00/x1/11/01/01/11")?;

        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.is_superset_of(&rul2));
        assert!(!rul2.is_superset_of(&rul1));

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
    fn sequence_blocks_changes() -> Result<(), String> {
        // All possible change pass-through conditions can be tested at once.
        let rul1 = SomeRule::new_from_string("01/01/01/10/10/10")?;
        let rul2 = SomeRule::new_from_string("11/X1/XX/00/X0/XX")?;
        let chg1 = SomeChange::new_from_string("Xx/Xx/Xx/Xx/Xx/Xx")?;
        assert!(!rul1.sequence_blocks_changes(&rul2, &chg1));

        // Change non pass-through conditions must be tested one-by-one.

        // Test 0->1 non pass-through conditions.
        let rul1 = SomeRule::new_from_string("01")?;
        let rul2 = SomeRule::new_from_string("10")?;
        let chg1 = SomeChange::new_from_string("Xx")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::new_from_string("00")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::new_from_string("01")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::new_from_string("X0")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::new_from_string("Xx")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        // Test 1->0 non pass-through conditions.
        let rul1 = SomeRule::new_from_string("10")?;
        let rul2 = SomeRule::new_from_string("01")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::new_from_string("11")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::new_from_string("10")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::new_from_string("X1")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

        let rul2 = SomeRule::new_from_string("Xx")?;
        assert!(rul1.sequence_blocks_changes(&rul2, &chg1));

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
            SomeMask::new_from_string("0b1000")?,
            SomeMask::new_from_string("0b0010")?,
        );

        let rul2 = rul1.restrict_for_changes(&chg1);
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

        // Change X->x to 1->0.
        // Change X->x to 0->1.
        // Leave one X->x unaffected.
        let rul1 = SomeRule::new_from_string("00/Xx/Xx/Xx")?;
        let chg1 = SomeChange::new(
            SomeMask::new_from_string("0b0010")?,
            SomeMask::new_from_string("0b0100")?,
        );

        let rul4 = rul1.restrict_for_changes(&chg1);
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
        let sta1 = SomeState::new_from_string("0b000110")?;

        let sta2 = rul1.result_from_initial_state(&sta1);
        println!("rul1: {rul1} sta1: {sta1} sta2: {sta2}");

        assert!(sta2 == SomeState::new_from_string("0b101010")?);

        Ok(())
    }

    #[test]
    fn result_from_initial_region() -> Result<(), String> {
        let rul1 = SomeRule::new_from_string("XX/01/XX/10/x1/x1/x0/x0/Xx/Xx/Xx/XX/XX/XX")?;
        let reg1 = SomeRegion::new_from_string("r00_11_01_01_x01_x01")?;

        let reg2 = rul1.result_from_initial_region(&reg1);
        println!("rul1: {rul1} reg1: {reg1} result: {reg2}");

        assert!(reg2 == SomeRegion::new_from_string("r01_10_11_00_x10_x01")?);

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

    #[test]
    fn combine_pair_sequence() -> Result<(), String> {
        // Test 0->0
        let rul1 = SomeRule::new_from_string("00/00/00/00/00/00")?;
        let rul2 = SomeRule::new_from_string("00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("00/01/00/01/00/01")?;
        assert!(rul3 == rul4);

        // Test 0->1
        let rul1 = SomeRule::new_from_string("01/01/01/01/01/01")?;
        let rul2 = SomeRule::new_from_string("11/10/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("01/00/00/01/01/00")?;
        assert!(rul3 == rul4);

        // Test 1->1
        let rul1 = SomeRule::new_from_string("11/11/11/11/11/11")?;
        let rul2 = SomeRule::new_from_string("11/10/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("11/10/10/11/11/10")?;
        assert!(rul3 == rul4);

        // Test 1->0
        let rul1 = SomeRule::new_from_string("10/10/10/10/10/10")?;
        let rul2 = SomeRule::new_from_string("00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("10/11/10/11/10/11")?;
        assert!(rul3 == rul4);

        // Test X->0
        let rul1 = SomeRule::new_from_string("X0/X0/X0/X0/X0/X0")?;
        let rul2 = SomeRule::new_from_string("00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("X0/X1/X0/X1/X0/X1")?;
        assert!(rul3 == rul4);

        // Test X->1
        let rul1 = SomeRule::new_from_string("X1/X1/X1/X1/X1/X1")?;
        let rul2 = SomeRule::new_from_string("11/10/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("X1/X0/X0/X1/X1/X0")?;
        assert!(rul3 == rul4);

        // Test X->X
        let rul1 = SomeRule::new_from_string("XX/XX/XX/XX/XX/XX/XX/XX")?;
        let rul2 = SomeRule::new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        assert!(rul3 == rul4);

        // Test X->X
        let rul1 = SomeRule::new_from_string("XX/XX/XX/XX/XX/XX/XX/XX")?;
        let rul2 = SomeRule::new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        assert!(rul3 == rul4);

        // Test X->X
        let rul1 = SomeRule::new_from_string("XX/XX/XX/XX/XX/XX/XX/XX")?;
        let rul2 = SomeRule::new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        assert!(rul3 == rul4);

        // Test X->x
        let rul1 = SomeRule::new_from_string("Xx/Xx/Xx/Xx/Xx/Xx/Xx/Xx")?;
        let rul2 = SomeRule::new_from_string("11/10/00/01/X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_pair_sequence(&rul2);
        println!("rul3 {}", rul3.formatted_string());

        let rul4 = SomeRule::new_from_string("01/00/10/11/X0/X1/Xx/XX")?;
        assert!(rul3 == rul4);

        Ok(())
    }

    #[test]
    fn new_region_to_region() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string("r000")?;
        let reg2 = SomeRegion::new_from_string("r01X")?;
        let rul1 = SomeRule::new_region_to_region(&reg1, &reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1: {rul1}");
        let rul2 = SomeRule::new_from_string("00/01/00")?;
        assert!(rul1 == rul2);

        let reg1 = SomeRegion::new_from_string("r111")?;
        let reg2 = SomeRegion::new_from_string("r01X")?;
        let rul1 = SomeRule::new_region_to_region(&reg1, &reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1: {rul1}");
        let rul2 = SomeRule::new_from_string("10/11/11")?;
        assert!(rul1 == rul2);

        let reg1 = SomeRegion::new_from_string("rXXX")?;
        let reg2 = SomeRegion::new_from_string("r01X")?;
        let rul1 = SomeRule::new_region_to_region(&reg1, &reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1: {rul1}");
        let rul2 = SomeRule::new_from_string("X0/X1/XX")?;
        assert!(rul1 == rul2);

        // Test proper subset region.
        let reg1 = SomeRegion::new_from_string("r0011")?;
        let reg2 = SomeRegion::new_from_string("rx01x")?;
        let rul1 = SomeRule::new_region_to_region(&reg1, &reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1 is {rul1}");
        let rul2 = SomeRule::new_from_string("00/00/11/11")?;
        assert!(rul1 == rul2);

        // Test intersecting regions.
        let reg1 = SomeRegion::new_from_string("r010x")?;
        let reg2 = SomeRegion::new_from_string("rx1x1")?;
        let rul1 = SomeRule::new_region_to_region(&reg1, &reg2);
        println!("reg1: {reg1} reg2: {reg2} rul1 is {rul1}");
        let rul2 = SomeRule::new_from_string("00/11/00/X1")?;
        assert!(rul1 == rul2);

        Ok(())
    }

    #[test]
    fn mutually_exclusive() -> Result<(), String> {
        let chg1 = SomeChange::new_from_string("Xx/XX")?;

        let rul1 = SomeRule::new_from_string("01/01")?;
        let rul2 = SomeRule::new_from_string("11/10")?;
        let rslt = rul1.mutually_exclusive(&rul2, &chg1);
        println!("{rul1} mutually exclusive {rul2} is {rslt}");
        assert!(!rslt);

        let rul1 = SomeRule::new_from_string("01/01")?;
        let rul2 = SomeRule::new_from_string("10/11")?;
        let rslt = rul1.mutually_exclusive(&rul2, &chg1);
        println!("{rul1} mutually exclusive {rul2} is {rslt}");
        assert!(rslt);

        Ok(())
    }
} // end tests
