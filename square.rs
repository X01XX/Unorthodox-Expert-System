//! The SomeSquare struct. This represents a state/square in a pseudo Karnaugh Map, and result states from excuting an action.

use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::AccessStates;
use crate::resultstore::ResultStore;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::tools::StrLen;

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
                Pn::One => 3,
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

        if self.num_results() > 100 {
            panic!("Square: {self}, too many results {}?", self.num_results());
        }
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

    /// Check if squares rules are, are not, or could be, compatible.
    pub fn compatible(&self, other: &Self) -> Option<bool> {
        assert!(self.state != other.state);

        // Check for unpredictable results.
        if self.pn == Pn::Unpredictable && other.pn == Pn::Unpredictable {
            return Some(true);
        }

        if self.pn == Pn::Unpredictable {
            if other.pnc {
                return Some(false);
            }
            return None;
        }

        if other.pn == Pn::Unpredictable {
            if self.pnc {
                return Some(false);
            }
            return None;
        }

        // Get rules refs.
        let s_rules = self.rules.as_ref().expect("SNH");
        let o_rules = other.rules.as_ref().expect("SNH");

        if self.pn > other.pn {
            if !other.pnc && s_rules.subcompatible(o_rules).is_some() {
                None
            } else {
                Some(false)
            }
        } else if other.pn > self.pn {
            if !self.pnc && o_rules.subcompatible(s_rules).is_some() {
                None
            } else {
                Some(false)
            }
        } else {
            Some(s_rules.compatible(o_rules))
        }
    }

    /// Return the Pattern Number Confirmed (pnc) value.
    fn calc_pnc(&self) -> bool {
        if self.pn == Pn::Unpredictable {
            return true;
        }

        if self.pn == Pn::One && self.results.len() > 2 {
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
    fn diff_edge_mask(&self, other: &impl AccessStates) -> SomeMask {
        self.state.diff_edge_mask(other)
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

    // Test multiple additions to a square, cycle through all pn and pnc values.
    #[test]
    fn cycle_through_pn_pnc_values() -> Result<(), String> {
        let mut sqrx = SomeSquare::new(&SomeSample::new(
            SomeState::new_from_string("s0b0101")?,
            SomeState::new_from_string("s0b0101")?,
        ));
        assert_eq!(sqrx.pn, Pn::One);
        assert!(!sqrx.pnc);

        // Second result, same as the first.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0101")?,
        ));
        assert!(!changed);
        // Third result, same as the first.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0101")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::One);
        assert!(sqrx.pnc);

        // Third result, same as the first two.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0101")?,
        ));
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::One);
        assert!(sqrx.pnc);

        // Fourth result, different from the first three, square becomes Unpredictable.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0100")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Fifth result, same as the first, square remains Unpredictable.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0101")?,
        ));
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Sixth result, same as the second most recent, square becomes Pn::Two.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0100")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::Two);
        assert!(sqrx.pnc);

        // Seventh result, same as the second most recent, square stays Pn::Two.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0101")?,
        ));
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::Two);
        assert!(sqrx.pnc);

        // Eighth result, same as the most recent, square becomes Pn::Unpredictable.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0101")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Nineth result, same as the most recent, square remains Pn::Unpredictable.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0101")?,
        ));
        assert!(!changed);
        assert_eq!(sqrx.pn, Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Tenth result, same as the most recent, square becomes Pn::One.
        let changed = sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0101")?,
        ));
        assert!(changed);
        assert_eq!(sqrx.pn, Pn::One);
        assert!(sqrx.pnc);

        Ok(())
    }

    // Test compatible: Pn both One
    #[test]
    fn compatible_pn_1_1() -> Result<(), String> {
        // Test two squares with only one sample each.
        // Allow a true result for bootstrapping.

        // Create square 5.
        let sta_5 = SomeState::new_from_string("s0b0101")?;

        let sqr_5 = SomeSquare::new(&SomeSample::new(sta_5.clone(), sta_5.clone()));
        println!("sqr_5: {sqr_5}");

        // Create square d, compatible to square 5.
        let sta_d = SomeState::new_from_string("s0b1101")?;

        let sqr_d = SomeSquare::new(&SomeSample::new(sta_d.clone(), sta_d.clone()));
        println!("sqr_d: {sqr_d}");

        // Test compatible.
        let rslt = sqr_5.compatible(&sqr_d);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        // Create square d, not compatible to square 5.
        let sta_9 = SomeState::new_from_string("s0b1001")?;
        let sta_d = SomeState::new_from_string("s0b1101")?;

        let sqr_d = SomeSquare::new(&SomeSample::new(sta_d.clone(), sta_9.clone()));
        println!("sqr_d: {sqr_d}");

        // Test compatible.
        let rslt = sqr_5.compatible(&sqr_d);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        Ok(())
    }

    // Test compatible both Pn == two
    #[test]
    fn compatible_pn_2_2() -> Result<(), String> {
        // Create sqr_d pn 2.
        let sta_c = SomeState::new_from_string("s0b1100")?;
        let sta_d = SomeState::new_from_string("s0b1101")?;

        let mut sqr_d = SomeSquare::new(&SomeSample::new(sta_d.clone(), sta_d.clone()));

        sqr_d.add_sample(&SomeSample::new(sta_d.clone(), sta_c.clone()));
        println!("sqr_d {sqr_d}");

        // Create sqr_5 pn 2.
        let sta_4 = SomeState::new_from_string("s0b0100")?;
        let sta_5 = SomeState::new_from_string("s0b0101")?;

        let mut sqr_5 = SomeSquare::new(&SomeSample::new(sta_5.clone(), sta_5.clone()));

        sqr_5.add_sample(&SomeSample::new(sta_5.clone(), sta_4.clone()));
        println!("sqr_5 {sqr_5}");

        let rslt = sqr_d.compatible(&sqr_5);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        // Create sqr_5 pn 2, reverse order of results.
        let sta_4 = SomeState::new_from_string("s0b0100")?;
        let sta_5 = SomeState::new_from_string("s0b0101")?;

        let mut sqr_5 = SomeSquare::new(&SomeSample::new(sta_5.clone(), sta_4.clone()));

        sqr_5.add_sample(&SomeSample::new(sta_5.clone(), sta_5.clone()));
        println!("sqr_5 {sqr_5}");

        let rslt = sqr_d.compatible(&sqr_5);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        // Create sqr_5 pn 2, not compatible to sqr_d.
        let sta_6 = SomeState::new_from_string("s0b0110")?;
        let sta_5 = SomeState::new_from_string("s0b0101")?;

        let mut sqr_5 = SomeSquare::new(&SomeSample::new(sta_5.clone(), sta_6.clone()));

        sqr_5.add_sample(&SomeSample::new(sta_5.clone(), sta_5.clone()));
        println!("sqr_5 {sqr_5}");

        let rslt = sqr_d.compatible(&sqr_5);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        Ok(())
    }

    // Test compatible: if self.pn  == Pn::Unpredictable
    // Test compatible: if other.pn == Pn::Unpredictable
    #[test]
    fn compatible_only_one_pn_u() -> Result<(), String> {
        let sta_d = SomeState::new_from_string("s0b1101")?;

        // Create sqr_d, pn U.
        let mut sqr_d = SomeSquare::new(&SomeSample::new(sta_d.clone(), sta_d.clone()));

        sqr_d.add_sample(&SomeSample::new(
            sta_d.clone(),
            SomeState::new_from_string("s0b1100")?,
        ));

        sqr_d.add_sample(&SomeSample::new(
            sta_d.clone(),
            SomeState::new_from_string("s0b1000")?,
        ));

        // Create sqr_5 pn 1 pnc f.
        let sta_5 = SomeState::new_from_string("s0b0101")?;

        let mut sqr_5 = SomeSquare::new(&SomeSample::new(sta_5.clone(), sta_5.clone()));
        println!("sqr_d {sqr_d}");
        println!("sqr_5 {sqr_5}");

        // Test sqr_d pn U pnc t, sqr_5 pn 1 pnc f.
        let rslt = sqr_d.compatible(&sqr_5);
        println!("rslt {:?}", rslt);
        assert!(rslt == None);

        // Test sqr_5 pn 1 pnc f, sqr_d pn U pnc t.
        let rslt = sqr_5.compatible(&sqr_d);
        println!("rslt {:?}", rslt);
        assert!(rslt == None);

        // Update sqr_5, to pn 1, pnc t.
        sqr_5.add_sample(&SomeSample::new(sta_5.clone(), sta_5.clone()));
        sqr_5.add_sample(&SomeSample::new(sta_5.clone(), sta_5.clone()));
        println!("sqr_d {sqr_d}");
        println!("sqr_5 {sqr_5}");

        // Test sqr_d pn U pnc t, sqr_5 pn 1 pnc t.
        let rslt = sqr_d.compatible(&sqr_5);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Test sqr_5 pn 1 pnc t, sqr_d pn U pnc t.
        let rslt = sqr_5.compatible(&sqr_d);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        Ok(())
    }

    // Test compatible: if self.pn == Pn::Unpredictable && other.pn == Pn::Unpredictable
    #[test]
    fn compatible_pn_u_u() -> Result<(), String> {
        // Create sqr_d pn U pnc T.
        let sta_d = SomeState::new_from_string("s0b1101")?;

        let mut sqr_d = SomeSquare::new(&SomeSample::new(sta_d.clone(), sta_d.clone()));

        sqr_d.add_sample(&SomeSample::new(
            sqr_d.state.clone(),
            SomeState::new_from_string("s0b1100")?,
        ));
        sqr_d.add_sample(&SomeSample::new(
            sqr_d.state.clone(),
            SomeState::new_from_string("s0b1000")?,
        ));
        println!("sqr_d {sqr_d}");

        // Create sqr_1 pn U pnc T.
        let sta_1 = SomeState::new_from_string("s0b0001")?;
        let mut sqr_1 = SomeSquare::new(&SomeSample::new(sta_1.clone(), sta_1.clone()));

        sqr_1.add_sample(&SomeSample::new(
            sqr_1.state.clone(),
            SomeState::new_from_string("s0b0010")?,
        ));
        sqr_1.add_sample(&SomeSample::new(
            sqr_1.state.clone(),
            SomeState::new_from_string("s0b0100")?,
        ));
        println!("sqr_1 {sqr_1}");

        // Test sqr_d pn U pnc t, sqr_1 pn U pnc t.
        let rslt = sqr_d.compatible(&sqr_1);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(true));

        Ok(())
    }

    // Test if self.pn (Pn::Two) > other.pn (Pn::One)
    // Test if other.pn (Pn::Two) > self.pn (Pn::One)
    #[test]
    fn compatible_pn_2_1() -> Result<(), String> {
        // Create sqr_d pn 2 pnc f.
        let sta_d = SomeState::new_from_string("s0b1101")?;

        let mut sqr_d = SomeSquare::new(&SomeSample::new(sta_d.clone(), sta_d.clone()));

        sqr_d.add_sample(&SomeSample::new(
            sqr_d.state.clone(),
            SomeState::new_from_string("s0b1100")?,
        ));
        println!("sqr_d {sqr_d}");

        // Create sqr_1 pn 1, not compatible with sqr_d, pnc f.
        let sta_1 = SomeState::new_from_string("s0b0001")?;
        let sqr_1 = SomeSquare::new(&SomeSample::new(
            sta_1.clone(),
            SomeState::new_from_string("s0b0011")?,
        ));
        println!("sqr_1 {sqr_1}");

        // Try Pn::Two, Pn::One.
        let rslt = sqr_d.compatible(&sqr_1);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Try Pn::One, Pn::Two.
        let rslt = sqr_1.compatible(&sqr_d);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Create sqr_1 pn 1, compatible with sqr_d, pnc f.
        let sta_1 = SomeState::new_from_string("s0b0001")?;
        let mut sqr_1 = SomeSquare::new(&SomeSample::new(sta_1.clone(), sta_1.clone()));
        println!("sqr_1 {sqr_1}");

        // Try Pn::Two, Pn::One.
        let rslt = sqr_d.compatible(&sqr_1);
        println!("rslt {:?}", rslt);
        assert!(rslt == None);

        // Try Pn::One, Pn::Two.
        let rslt = sqr_1.compatible(&sqr_d);
        println!("rslt {:?}", rslt);
        assert!(rslt == None);

        // Update sqr_1 to pnc t.
        sqr_1.add_sample(&SomeSample::new(sta_1.clone(), sta_1.clone()));
        sqr_1.add_sample(&SomeSample::new(sta_1.clone(), sta_1.clone()));
        println!("sqr_1 {sqr_1}");

        // Try Pn::Two, Pn::One.
        let rslt = sqr_d.compatible(&sqr_1);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        // Try Pn::One, Pn::Two.
        let rslt = sqr_1.compatible(&sqr_d);
        println!("rslt {:?}", rslt);
        assert!(rslt == Some(false));

        Ok(())
    }
}
