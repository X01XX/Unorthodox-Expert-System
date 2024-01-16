//! The SomeSquare struct. This represents a state/square in a pseudo Karnaugh Map, and result states from excuting an action.

use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::AccessStates;
use crate::resultstore::ResultStore;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::tools::{not, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeSquare {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl PartialEq for SomeSquare {
    fn eq(&self, other: &Self) -> bool {
        self.state == other.state
    }
}
impl Eq for SomeSquare {}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug)]

/// A state, with the most recent results on a given action.
pub struct SomeSquare {
    pub state: SomeState,         // State that an action was taken on.
    pub results: ResultStore,     // Circular list of most recent results.
    pub rules: Option<RuleStore>, // Rules, None, 1 or 2 rules depending on pn
    pub pn: Pn,                   // Square Pattern number.
    pub pnc: bool,                // Pattern number confirmed.
}

impl SomeSquare {
    /// Return a SomeSquare instance, from a sample.
    pub fn new(asample: &SomeSample) -> Self {
        Self {
            state: asample.initial.clone(),
            results: ResultStore::new(vec![asample.result.clone()]),
            rules: Some(RuleStore::new(vec![SomeRule::new(asample)])),
            pn: Pn::One,
            pnc: false,
        }
    }

    /// Return a rate for a square.
    /// Squares that are pnc are equal to the minimum number of samples needed to reach pnc.
    /// Otherwise, the rate is the number of results stored.
    pub fn rate(&self) -> usize {
        if self.pnc {
            match self.pn {
                Pn::One => 2,
                Pn::Two => 4,
                Pn::Unpredictable => 3,
            }
        } else {
            self.len_results()
        }
    }

    /// Add a new sample to a square (4-item circular buffer).
    /// Return true if the addition changed the square, either the
    /// pn or pnc changed.  If there is a change, update the rules.
    pub fn add_sample(&mut self, asample: &SomeSample) -> bool {
        assert!(self.state == asample.initial);

        let mut str_info = String::from(&format!(
            "\n  Square {} adding result {} {}",
            self.state,
            self.results.num_results() + 1,
            &asample.result
        ));

        let sav_pn = self.pn;
        let sav_pnc = self.pnc;

        self.pn = self.results.add_result(asample.result.clone());
        self.pnc = self.calc_pnc();

        let mut changed = false;
        if self.pn != sav_pn || self.pnc != sav_pnc {
            changed = true;

            match self.pn {
                Pn::One => {
                    self.rules = Some(RuleStore::new(vec![SomeRule::new(asample)]));
                }
                Pn::Two => {
                    self.rules = Some(RuleStore::new(vec![
                        SomeRule::new(&SomeSample::new(
                            self.state.clone(),
                            self.results.first().clone(),
                        )),
                        SomeRule::new(&SomeSample::new(
                            self.state.clone(),
                            self.results.second().clone(),
                        )),
                    ]));
                }
                Pn::Unpredictable => {
                    self.rules = None;
                }
            }
        }

        if self.pn != sav_pn {
            str_info.push_str(&format!(", pn changed from {} to {}", &sav_pn, &self.pn));
        } else {
            str_info.push_str(&format!(", pn {}", &self.pn));
        }

        if self.pnc != sav_pnc {
            str_info.push_str(&format!(", pnc changed from {} to {}", &sav_pnc, &self.pnc));
        } else {
            str_info.push_str(&format!(", pnc {}", &self.pnc));
        }

        println!("{}", str_info);
        changed
    }

    /// Return the number of results stored for the square.
    pub fn len_results(&self) -> usize {
        self.results.len()
    }

    /// Return the first result for the square.
    pub fn first_result(&self) -> &SomeState {
        self.results.first()
    }

    pub fn num_results(&self) -> usize {
        self.results.num_results()
    }

    /// Return true if two squares are adjacent, that is they differ by exactly one bit.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        self.state.is_adjacent(&other.state)
    }

    /// Return the most recent results
    pub fn most_recent_result(&self) -> &SomeState {
        self.results.most_recent_result()
    }

    /// Check if squares rules are, or could be, compatible.
    pub fn rules_compatible(&self, other: &Self) -> bool {
        if self.pn == Pn::Unpredictable {
            return true;
        }

        if self.pn > other.pn {
            if let Some(rules1) = &self.rules {
                if let Some(rules2) = &other.rules {
                    rules1.subcompatible(rules2)
                } else {
                    panic!("SNH");
                }
            } else {
                panic!("SNH");
            }
        } else if let Some(rules1) = &self.rules {
            if let Some(rules2) = &other.rules {
                rules1.compatible(rules2)
            } else {
                panic!("SNH");
            }
        } else {
            panic!("SNH");
        }
    }

    /// Return indication of compatibility for two squares to
    /// form a union, with the same number of rules as the first square.
    /// Some(true) = true.
    /// Some(false) = false.
    /// None = More samples needed.
    pub fn compatible(&self, other: &Self) -> Option<bool> {
        assert!(self.state != other.state);

        // Check the suitability of the first square.
        if not(self.pn == Pn::One || self.pnc) {
            return None;
        }

        // Check that the first square pn value is >= than the second.
        if self.pn < other.pn {
            return Some(false);
        }

        // Check for subcompatibility.
        if self.pn > other.pn {
            // Check if other pnc = true.
            if other.pnc {
                return Some(false);
            }
            // Check if the first square in Pn::Unpredictable.
            if self.pn == Pn::Unpredictable {
                return None;
            }
            // Compare rules.
            if let Some(rules1) = &self.rules {
                if let Some(rules2) = &other.rules {
                    if rules1.subcompatible(rules2) {
                        return None;
                    } else {
                        return Some(false);
                    }
                } else {
                    panic!("SNH");
                }
            } else {
                panic!("SNH");
            }
        }
        // Must have the same number of rules.

        // Check for pn == Pn::Unpredicable.
        if self.pn == Pn::Unpredictable {
            return Some(true);
        }
        // Check if rules can form a union.
        if let Some(rules1) = &self.rules {
            if let Some(rules2) = &other.rules {
                if rules1.compatible(rules2) {
                    if other.pn == Pn::Two {
                        if other.pnc {
                            Some(true)
                        } else {
                            None
                        }
                    } else {
                        Some(true)
                    }
                } else {
                    Some(false)
                }
            } else {
                panic!("SNH");
            }
        } else {
            panic!("SNH");
        }
    }

    /// Return the Pattern Number Confirmed (pnc) value.
    fn calc_pnc(&self) -> bool {
        if self.pn == Pn::Unpredictable {
            return true;
        }

        if self.pn == Pn::One && self.results.len() > 1 {
            return true;
        }

        if self.pn == Pn::Two && self.results.len() > 3 {
            return true;
        }
        false
    }

    /// Return true if a square is between two given squares, exclusive.
    pub fn is_between(&self, sqr1: &Self, sqr2: &Self) -> bool {
        self.state.is_between(&sqr1.state, &sqr2.state)
    }

    /// Return true is a square state is a subset of another group/square/regian/state.
    pub fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        match other.one_state() {
            true => self.state == *other.first_state(),
            false => other
                .edge_mask()
                .bitwise_and(&self.state.bitwise_xor(other.first_state()))
                .is_low(),
        }
    }

    /// Return a String representation of a square.
    fn formatted_string(&self) -> String {
        let mut rc_str = String::from("S[");
        rc_str.push_str(&self.state.to_string());
        rc_str.push_str(&format!(", pn: {}", &self.pn));
        if self.pnc {
            rc_str.push_str(", pnc: t");
        } else {
            rc_str.push_str(", pnc: f");
        }

        if let Some(ruls_str) = &self.rules {
            rc_str.push(' ');
            rc_str.push_str(&ruls_str.to_string());
        }

        rc_str.push(']');
        rc_str
    }
} // end impl SomeSquare

/// Implement the trait StrLen for SomeSquare.
impl StrLen for SomeSquare {
    fn strlen(&self) -> usize {
        40
    }
}

/// Implement the trait AccessStates for SomeSquare.
impl AccessStates for SomeSquare {
    fn one_state(&self) -> bool {
        true
    }
    fn first_state(&self) -> &SomeState {
        &self.state
    }
    fn x_mask(&self) -> SomeMask {
        self.state.new_low().to_mask()
    }
    fn edge_mask(&self) -> SomeMask {
        self.state.new_high().to_mask()
    }
    fn high_state(&self) -> SomeState {
        self.state.clone()
    }
    fn low_state(&self) -> SomeState {
        self.state.clone()
    }
    fn diff_mask(&self, other: &impl AccessStates) -> SomeMask {
        self.state.diff_mask(other)
    }
    fn intersects(&self, other: &impl AccessStates) -> bool {
        self.state.intersects(other)
    }
    fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        self.is_subset_of(other)
    }
    fn is_superset_of(&self, other: &impl AccessStates) -> bool {
        self.state.is_superset_of(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;

    // Test multiple additions to a square, cycle through all pn and pnc values.
    #[test]
    fn cycle_through_pn_pnc_values() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));

        let mut sqrx = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        ));
        assert_eq!(sqrx.pn, Pn::One);
        assert!(!sqrx.pnc);

        // Second result, same as the first.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::One);
        assert!(sqrx.pnc);

        // Third result, same as the first two.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::One);
        assert!(sqrx.pnc);

        // Fourth result, different from the first three, square becomes Unpredictable.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Fifth result, same as the first, square remains Unpredictable.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Sixth result, same as the second most recent, square becomes Pn::Two.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::Two);
        assert!(sqrx.pnc);

        // Seventh result, same as the second most recent, square stays Pn::Two.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::Two);
        assert!(sqrx.pnc);

        // Eighth result, same as the most recent, square becomes Pn::Unpredictable.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Nineth result, same as the most recent, square remains Pn::Unpredictable.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Tenth result, same as the most recent, square becomes Pn::One.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::One);
        assert!(sqrx.pnc);

        Ok(())
    }

    // Test can_combine, cannot compare the same square.
    #[test]
    #[should_panic(expected = "self.state != other.state")]
    fn compatible_not_same() {
        let tmp_sta = SomeState::new(SomeBits::new(8));

        let sqr1 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101").unwrap(),
            tmp_sta.new_from_string("s0b0101").unwrap(),
        ));

        sqr1.compatible(&sqr1);
        ()
    }

    // Test compatible Pn == One
    #[test]
    fn compatible_pn_one_one() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));

        // Test two squares with only one sample each.
        // Allow a true result for bootstrapping.
        let sqr1 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        ));

        let mut sqr2 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        ));
        println!("sqr1: {sqr1}");
        println!("sqr2: {sqr2}");

        // Try 1 vs 2 order.
        let rslt = sqr1.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        // Try reverse order.
        let rslt = sqr2.compatible(&sqr1);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        // Add a sample to a square.
        sqr2.add_sample(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        ));

        // Try 1 vs 2 order.
        let rslt = sqr1.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        // Try reverse order.
        let rslt = sqr2.compatible(&sqr1);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        // Create a square incompatible to sqr1 to produce M result.
        let mut sqr3 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1100")?,
        ));
        println!("sqr3: {sqr3}");

        let rslt = sqr1.compatible(&sqr3);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Add to sqr3 to make it pnc.
        sqr3.add_sample(&SomeSample::new(
            sqr3.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));
        println!("sqr3: {sqr3}");

        let rslt = sqr1.compatible(&sqr3);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        let rslt = sqr3.compatible(&sqr1);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        Ok(())
    }

    // Test compatible Pn == One and Pn == two
    #[test]
    fn compatible_pn_one_two() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));

        // Create sqr1 pn 1 pnc f.
        let sqr1 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        ));

        // Create sqr2 pn 2 pnc f, one result compatible to sqr1.
        let mut sqr2 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        ));

        // Make sqr2 Pn::Two.
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));

        // Make sqr2 pnc = true.
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b1101")?,
        ));

        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));

        // Create sqr3 pn 1 pnc f, incompatible to any result in sqr2.
        let sqr3 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        ));

        println!("sqr1: {sqr1}");
        println!("sqr2: {sqr2}");
        println!("sqr3: {sqr3}");

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc f.
        let rslt = sqr2.compatible(&sqr1);
        println!("result {:?}", rslt);
        assert!(rslt == None);

        // Test sqr3 pn 1 pnc f, sqr2 pn 2 pnc f.
        let rslt = sqr2.compatible(&sqr3);
        println!("result {:?}", rslt);
        assert!(rslt == Some(false));

        // Create sqr4 pn 1 pnc t, compatible with one result in sqr2.
        let mut sqr4 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        ));

        sqr4.add_sample(&SomeSample::new(
            sqr4.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        println!("sqr4: {sqr4}");
        println!("sqr2: {sqr2}");

        // Test sqr4 pn 1 pnc t, sqr2 pn 2 pnc f.
        let rslt = sqr4.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Exersize other branch of code.
        let rslt = sqr2.compatible(&sqr4);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Add to sqr2 to make it pnc.
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b1101")?,
        ));
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));

        println!("sqr1 {sqr1}");
        println!("sqr2 {sqr2}");

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc t.
        let rslt = sqr2.compatible(&sqr1);
        println!("rslt {:?}", rslt);
        assert!(rslt == None);

        // Test sqr3 pn 1 pnc f, sqr2 pn 2 pnc t.
        println!("sqr2 {sqr2}");
        println!("sqr3 {sqr3}");

        let rslt = sqr2.compatible(&sqr3);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Exersize other branch of code.
        let rslt = sqr3.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        Ok(())
    }

    // Test compatible Pn == two
    #[test]
    fn compatible_pn_two_two() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));

        // Create sqr1 pn 2 pnc t.
        let mut sqr1 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        ));

        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));

        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1101")?,
        ));

        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));

        // Create sqr2 pn 2 pnc f.
        let mut sqr2 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        ));

        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        ));
        println!("sqr1 {sqr1}");
        println!("sqr2 {sqr2}");

        let rslt = sqr1.compatible(&sqr2);
        println!("rslt {:?}", rslt);

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc f.
        assert!(rslt == None);

        // Create sqr3 pn 2 pnc f, not compatible with sqr1.
        let mut sqr3 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        ));

        sqr3.add_sample(&SomeSample::new(
            sqr3.state.clone(),
            tmp_sta.new_from_string("s0b1101")?,
        ));
        println!("sqr1 {sqr1}");
        println!("sqr3 {sqr3}");
        let rslt = sqr1.compatible(&sqr3);
        println!("rslt {:?}", rslt);

        // Test sqr1 pn 1 pnc f, sqr3 pn 2 pnc f.
        assert!(rslt == Some(false));

        // Make sqr1 pnc.
        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1101")?,
        ));
        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));

        // Test sqr1 pn 1 pnc t, sqr2 pn 2 pnc f.
        if sqr1.compatible(&sqr2) == Some(true) {
            return Err(String::from("Test 3 failed?"));
        }

        // Add to sqr2 to make it pnc.
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        ));

        println!("sqr1 {sqr1}");
        println!("sqr2 {sqr2}");

        // Test sqr1 pn 2 pnc t, sqr2 pn 2 pnc t.
        let rslt = sqr1.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        // Test sqr1 pn 2 pnc t, sqr3 pn 2 pnc f.
        let rslt = sqr1.compatible(&sqr3);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Create sqr4, cause X0/X1 or Xx/XX combination error with sqr1.
        let mut sqr4 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0100")?,
            tmp_sta.new_from_string("s0b0100")?,
        ));

        sqr4.add_sample(&SomeSample::new(
            sqr4.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        sqr4.add_sample(&SomeSample::new(
            sqr4.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        ));
        sqr4.add_sample(&SomeSample::new(
            sqr4.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        println!("sqr1 {sqr1}");
        println!("sqr4 {sqr4}");

        // Test sqr1 pn 2 pnc t, sqr4 pn 2 pnc t.
        let rslt = sqr1.compatible(&sqr4);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Create sqr5, combinable with sqr1, but results in reverse order.
        let mut sqr5 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0100")?,
        ));

        sqr5.add_sample(&SomeSample::new(
            sqr5.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        sqr5.add_sample(&SomeSample::new(
            sqr5.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        ));
        sqr5.add_sample(&SomeSample::new(
            sqr5.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        println!("sqr1 {sqr1}");
        println!("sqr5 {sqr5}");

        // Test sqr1 pn 2 pnc t, sqr5 pn 2 pnc t.
        let rslt = sqr1.compatible(&sqr5);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        Ok(())
    }

    // Test compatible Pn == Unpredictable, Pn == 1
    #[test]
    fn compatible_pn_u_one() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));

        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        ));

        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));
        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1000")?,
        ));

        // Create sqr2 pn 1 pnc f.
        let mut sqr2 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        ));
        println!("sqr1 {sqr1}");
        println!("sqr2 {sqr2}");

        // Test sqr1 pn U pnc t, sqr2 pn 1 pnc f.
        let rslt = sqr1.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == None);

        // Make sqr2 pnc == true.
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        println!("sqr2 {sqr2}");

        // Test sqr1 pn U pnc t, sqr2 pn 1 pnc t.
        let rslt = sqr1.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        Ok(())
    }

    // Test compatible Pn == Unpredictable, Pn == 2
    #[test]
    fn compatible_pn_u_two() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));

        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        ));

        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));
        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1000")?,
        ));

        // Create sqr2 pn 2 pnc f.
        let mut sqr2 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0101")?,
            tmp_sta.new_from_string("s0b0101")?,
        ));
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        ));
        println!("sqr1 {sqr1}");
        println!("sqr2 {sqr2}");

        // Test sqr1 pn U pnc t, sqr2 pn 2 pnc f.
        let rslt = sqr1.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == None);

        // Make sqr2 pnc == true.
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        ));
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        ));
        println!("sqr1 {sqr1}");
        println!("sqr2 {sqr2}");

        // Test sqr1 pn U pnc t, sqr2 pn 2 pnc t.
        let rslt = sqr1.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        Ok(())
    }

    // Test compatible Pn == Unpredictable, Pn == Unpredictable
    #[test]
    fn compatible_pn_u_u() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(8));

        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1101")?,
        ));

        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1100")?,
        ));
        sqr1.add_sample(&SomeSample::new(
            sqr1.state.clone(),
            tmp_sta.new_from_string("s0b1000")?,
        ));

        // Create sqr2 pn U pnc T.
        let mut sqr2 = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b0001")?,
            tmp_sta.new_from_string("s0b0001")?,
        ));

        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b0010")?,
        ));
        sqr2.add_sample(&SomeSample::new(
            sqr2.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        ));
        println!("sqr1 {sqr1}");
        println!("sqr2 {sqr2}");

        // Test sqr1 pn U pnc t, sqr2 pn U pnc t.
        let rslt = sqr1.compatible(&sqr2);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        Ok(())
    }
}
