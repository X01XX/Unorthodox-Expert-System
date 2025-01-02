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
use crate::region::SomeRegion;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::tools::{self, StrLen};

use unicode_segmentation::UnicodeSegmentation;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

//#[readonly::make]
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
            m00: smpl.initial.bitwise_not().bitwise_and_not(&smpl.result),
            m01: smpl.result.bitwise_and_not(&smpl.initial),
            m11: smpl.initial.bitwise_and(&smpl.result),
            m10: smpl.initial.bitwise_and_not(&smpl.result),
        }
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

    /// Return a valid union, or None.
    pub fn valid_union(&self, other: &Self) -> Option<Self> {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        let unx = self.union(other);

        if unx.is_valid_union() {
            Some(unx)
        } else {
            None
        }
    }

    /// Return a logical OR of two rules. The result may not pass is_valid_union.
    pub fn union(&self, other: &Self) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        Self {
            m00: self.m00.bitwise_or(&other.m00),
            m01: self.m01.bitwise_or(&other.m01),
            m11: self.m11.bitwise_or(&other.m11),
            m10: self.m10.bitwise_or(&other.m10),
        }
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
        let st_high = self.m11.bitwise_or(&self.m10).as_state();
        let st_low = self.m00.bitwise_or(&self.m01).bitwise_not().as_state();

        SomeRegion::new(vec![st_high, st_low])
    }

    /// Return the result region of a rule.
    pub fn result_region(&self) -> SomeRegion {
        let st_high = self.m11.bitwise_or(&self.m01).as_state();
        let st_low = self.m00.bitwise_or(&self.m10).bitwise_not().as_state();

        // Change X->x positions to indicate a change, instead of appearing as X->X.
        let x_xnot_mask = self.m01.bitwise_and(&self.m10);

        SomeRegion::new(vec![
            st_high.bitwise_and_not(&x_xnot_mask).as_state(),
            st_low.bitwise_or(&x_xnot_mask).as_state(),
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
        sta.bitwise_xor(&toggle).as_state()
    }

    /// Restrict the initial region to an intersection of the
    /// given region.  Assuming the region given is not a superset
    /// this will also change the result region.
    pub fn restrict_initial_region(&self, other: &impl tools::AccessStates) -> Self {
        debug_assert_eq!(self.num_bits(), other.num_bits());

        let init_reg = self.initial_region();

        if let Some(reg_int) = init_reg.intersection(other) {
            let zeros = reg_int.low_state().bitwise_not();
            let ones = reg_int.high_state().as_mask();

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
            let zeros = reg_int.low_state().bitwise_not();
            let ones = reg_int.high_state().as_mask();

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
    pub fn formatted_str(&self) -> String {
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
                strrc.push_str("00,01,11?/"); // Will fail is_valid_union.
            } else if msk == 8 {
                strrc.push_str("10/");
            } else if msk == 9 {
                strrc.push_str("X0/");
            } else if msk == 10 {
                strrc.push_str("Xx/");
            } else if msk == 11 {
                strrc.push_str("00,01,10?/"); // Will fail is_valid_union.
            } else if msk == 12 {
                strrc.push_str("1X/"); // Will fail is_valid_union.
            } else if msk == 13 {
                strrc.push_str("00,11,10?/"); // Will fail is_valid_union.
            } else if msk == 14 {
                strrc.push_str("01,11,10?/"); // Will fail is_valid_union.
            } else if msk == 15 {
                strrc.push_str("00,01,11,10?/"); // Will fail is_valid_union.
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
    pub fn restrict_for_changes(&self, wanted_changes: &SomeChange) -> Option<SomeRule> {
        debug_assert_eq!(self.num_bits(), wanted_changes.num_bits());

        // Check if any rule changes are needed.
        if self.as_change().intersection(wanted_changes).is_low() {
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
        //println!("SomeRule::mutually_exclusive, self {self} other {other} wanted {wanted}");
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

        let changes_care_01 = self.m01.bitwise_and(&wanted.m01);

        let changes_care_10 = self.m10.bitwise_and(&wanted.m10);

        if changes_care_01.is_low() && changes_care_10.is_low() {
            return false;
        }

        let seq = self.combine_sequence(other);

        let changes_care_01_ok = changes_care_01.bitwise_and(&seq.m01);

        let changes_care_10_ok = changes_care_10.bitwise_and(&seq.m10);

        !(changes_care_01.is_subset_ones_of(&changes_care_01_ok)
            && changes_care_10.is_subset_ones_of(&changes_care_10_ok))
    }

    /// Return a SomeChange instance.
    pub fn as_change(&self) -> SomeChange {
        SomeChange::new(self.m01.clone(), self.m10.clone())
    }

    /// Return the minimum-change rule to change a region into a subset of a second region.
    /// The result will never contain X->x positions.
    /// 1->X positions will be translated to 1->1.
    /// 0->X positions will be translated to 0->0.
    pub fn new_region_to_region_min(from: &SomeRegion, to: &SomeRegion) -> SomeRule {
        debug_assert_eq!(from.num_bits(), to.num_bits());

        let f0 = from.edge_zeros_mask();
        let f1 = from.edge_ones_mask();
        let fx = from.x_mask();

        let t0 = to.edge_zeros_mask();
        let t1 = to.edge_ones_mask();
        let tx = to.x_mask();

        Self {
            m00: f0
                .bitwise_and(&t0) // 0->0
                .bitwise_or(&fx.bitwise_and(&t0)) // X->0
                .bitwise_or(&f0.bitwise_and(&tx)) // 0->X
                .bitwise_or(&fx.bitwise_and(&tx)), // X->X
            m01: f0.bitwise_and(&t1).bitwise_or(&fx.bitwise_and(&t1)), // 0->1, X->1
            m11: f1
                .bitwise_and(&t1) // 1->1
                .bitwise_or(&fx.bitwise_and(&t1)) // X->1
                .bitwise_or(&f1.bitwise_and(&tx)) // 1->X
                .bitwise_or(&fx.bitwise_and(&tx)), // X->X
            m10: f1.bitwise_and(&t0).bitwise_or(&fx.bitwise_and(&t0)), // 1->0, X->0
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

        // Get a rule that will connect the two rules.
        let rule_between =
            Self::new_region_to_region_min(&self.result_region(), &other.initial_region());

        // Combine the three rules.
        self.combine_sequence2(&rule_between)
            .combine_sequence2(other)
    }

    /// Combine two rules.
    /// The result region of the first rule must intersect the initial region of the second rule.
    /// Changes in the first rule may be reversed in the second rule.
    fn combine_sequence2(&self, other: &Self) -> Self {
        //println!("SomeRule::combine_sequence2: {self} {other}");
        debug_assert_eq!(self.num_bits(), other.num_bits());

        assert!(self.result_region().intersects(&other.initial_region()));

        let self_x = self.initial_region().x_mask();
        let other_0 = other.result_region().edge_zeros_mask();
        let other_1 = other.result_region().edge_ones_mask();

        let x0 = self_x.bitwise_and(&other_0);
        let x1 = self_x.bitwise_and(&other_1);

        let other_tmp = other.restrict_initial_region(&self.result_region());

        Self {
            m00: self
                .m00
                .bitwise_and(&other_tmp.m00)
                .bitwise_or(&self.m01.bitwise_and(&other_tmp.m10))
                .bitwise_or(&x0),
            m01: self
                .m01
                .bitwise_and(&other_tmp.m11)
                .bitwise_or(&self.m00.bitwise_and(&other_tmp.m01))
                .bitwise_or(&x1),
            m11: self
                .m11
                .bitwise_and(&other_tmp.m11)
                .bitwise_or(&self.m10.bitwise_and(&other_tmp.m01))
                .bitwise_or(&x1),
            m10: self
                .m10
                .bitwise_and(&other_tmp.m00)
                .bitwise_or(&self.m11.bitwise_and(&other_tmp.m10))
                .bitwise_or(&x0),
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
} // end impl SomeRule

/// Implement the trait StrLen for SomeRule.
impl tools::StrLen for SomeRule {
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

impl FromStr for SomeRule {
    type Err = String;

    /// Generate a rule from a string.
    /// All bit positions must be specified.
    /// like SomeRule::from_str("00/01/11/10/XX/xx/Xx/xX/X0/X1")
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("SomeRule::from_str: {str_in}");
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_region_to_region_min() -> Result<(), String> {
        let reg1 = SomeRegion::from_str("000_111_XXX")?;
        let reg2 = SomeRegion::from_str("01X_01X_01X")?;
        let rul1 = SomeRule::new_region_to_region_min(&reg1, &reg2);
        println!("reg1: {reg1}");
        println!("reg2: {reg2}");
        println!("rul1: {rul1}");

        assert!(rul1 == SomeRule::from_str("00/01/00_10/11/11_X0/X1/XX")?);

        //assert!(1 == 2);
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
        assert!(!rul1.union(&rul2).is_valid_union());

        let rul1 = SomeRule::from_str("11")?;
        let rul2 = SomeRule::from_str("10")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(!rul1.union(&rul2).is_valid_union());

        let rul1 = SomeRule::from_str("11")?;
        let rul2 = SomeRule::from_str("01")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(rul1.union(&rul2).is_valid_union());

        let rul1 = SomeRule::from_str("x1")?;
        let rul2 = SomeRule::from_str("00")?;
        println!("rul1: {rul1} rul2: {rul2}");
        assert!(!rul1.union(&rul2).is_valid_union());

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

        let rule_to_goal = SomeRule::new_region_to_region_min(
            &SomeRegion::from_str("10_0001_100011")?,
            &SomeRegion::from_str("XX_X010_010011")?,
        );

        println!("rule_to_goal {rule_to_goal}");
        println!("rul1         {rul1}");

        let rul3 = SomeRule::from_str("X0/X1/Xx/10_01/10/X0/X1_X0/X1/X0/X1")?;

        if let Some(rul2) = rul1.restrict_for_changes(&rule_to_goal.as_change()) {
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
        let rul1 = SomeRule::from_str("00/00/00/00_00/00/00/00")?;
        let rul2 = SomeRule::from_str("00/01/11/10_X1/X0/XX/Xx")?;
        let rul3 = rul1.union(&rul2);
        println!("rul3 = {rul3}");
        assert!(format!("{rul3}") == "00/0X/XX/X0_00,01,11?/X0/XX/00,01,10?");

        Ok(())
    }

    #[test]
    fn combine_sequence() -> Result<(), String> {
        println!("Test 00 ->");
        let rul1 = SomeRule::from_str("00/00/00/00_00/00/00/00")?;
        let rul2 = SomeRule::from_str("00/01/11/10_X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul1 {}", rul1.to_string());
        println!("rul2 {}", rul2.to_string());
        println!("rul3 {}", rul3.to_string());
        assert!(rul3 == SomeRule::from_str("00/01/01/00_00/01/00/01")?);

        println!("\nTest 01 ->");
        let rul1 = SomeRule::from_str("01/01/01/01_01/01/01/01")?;
        let rul2 = SomeRule::from_str("00/01/11/10_X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul1 {}", rul1.to_string());
        println!("rul2 {}", rul2.to_string());
        println!("rul3 {}", rul3.to_string());
        assert!(rul3 == SomeRule::from_str("00/01/01/00_00/01/01/00")?);

        println!("\nTest 11 ->");
        let rul1 = SomeRule::from_str("11/11/11/11_11/11/11/11")?;
        let rul2 = SomeRule::from_str("00/01/11/10_X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul1 {}", rul1.to_string());
        println!("rul2 {}", rul2.to_string());
        println!("rul3 {}", rul3.to_string());
        assert!(rul3 == SomeRule::from_str("10/11/11/10_10/11/11/10")?);

        println!("\nTest X0 ->");
        let rul1 = SomeRule::from_str("X0/X0/X0/X0_X0/X0/X0/X0")?;
        let rul2 = SomeRule::from_str("00/01/11/10_X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul1 {}", rul1.to_string());
        println!("rul2 {}", rul2.to_string());
        println!("rul3 {}", rul3.to_string());
        assert!(rul3 == SomeRule::from_str("X0/X1/X1/X0_X0/X1/X0/X1")?);

        println!("\nTest X1 ->");
        let rul1 = SomeRule::from_str("X1/X1/X1/X1_X1/X1/X1/X1")?;
        let rul2 = SomeRule::from_str("00/01/11/10_X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul1 {}", rul1.to_string());
        println!("rul2 {}", rul2.to_string());
        println!("rul3 {}", rul3.to_string());
        assert!(rul3 == SomeRule::from_str("X0/X1/X1/X0_X0/X1/X1/X0")?);

        println!("\nTest Xx ->");
        let rul1 = SomeRule::from_str("Xx/Xx/Xx/Xx_Xx/Xx/Xx/Xx")?;
        let rul2 = SomeRule::from_str("00/01/11/10_X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul1 {}", rul1.to_string());
        println!("rul2 {}", rul2.to_string());
        println!("rul3 {}", rul3.to_string());
        assert!(rul3 == SomeRule::from_str("X0/X1/X1/X0_X0/X1/Xx/XX")?);

        println!("\nTest XX ->");
        let rul1 = SomeRule::from_str("XX/XX/XX/XX_XX/XX/XX/XX")?;
        let rul2 = SomeRule::from_str("00/01/11/10_X0/X1/XX/Xx")?;
        let rul3 = rul1.combine_sequence(&rul2);
        println!("rul1 {}", rul1.to_string());
        println!("rul2 {}", rul2.to_string());
        println!("rul3 {}", rul3.to_string());
        assert!(rul3 == SomeRule::from_str("X0/X1/X1/X0_X0/X1/XX/Xx")?);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn mutually_exclusive() -> Result<(), String> {
        let chg1 = SomeChange::from_str("Xx/..")?;

        let rul1 = SomeRule::from_str("01/01")?;
        let rul2 = SomeRule::from_str("11/10")?;
        let rslt = rul1.mutually_exclusive(&rul2, &chg1);
        println!("{rul1} mutually exclusive {rul2} is {rslt}");
        //assert!(!rslt);

        let chg2 = SomeChange::from_str("Xx/Xx/Xx/Xx")?;
        let rul1 = SomeRule::from_str("XX/XX/XX/10")?;
        let rul2 = SomeRule::from_str("XX/XX/10/XX")?;
        let rslt = rul1.mutually_exclusive(&rul2, &chg2);
        println!("\n{rul1} mutually exclusive {rul2} is {rslt}");
        //assert!(rslt);

        let rul1 = SomeRule::from_str("01/01")?;
        let rul2 = SomeRule::from_str("10/11")?;
        let rslt = rul1.mutually_exclusive(&rul2, &chg1);
        println!("\n{rul1} mutually exclusive {rul2} is {rslt}");
        //assert!(rslt);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn wanted_changes() -> Result<(), String> {
        let rul1 = SomeRule::new_region_to_region_min(
            &SomeRegion::from_str("XX_0101_01XX")?,
            &SomeRegion::from_str("01_0110_XXXX")?,
        );
        let cng = rul1.as_change();
        println!("wanted {cng}");

        assert!(cng.m01 == SomeMask::from_str("01_0010_0000")?);
        assert!(cng.m10 == SomeMask::from_str("10_0001_0000")?);

        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let rul1_str = "01/01/Xx/XX_X0/X1/00/11";
        let rul1 = SomeRule::from_str(rul1_str)?;
        println!("rul1 {rul1}");
        assert!(format!("{rul1}") == rul1_str);

        Ok(())
    }
} // end tests
