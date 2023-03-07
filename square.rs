//! The SomeSquare struct. This represents a state/square in a pseudo Karnaugh Map, and result states from excuting an action.

use crate::pn::Pn;
use crate::resultstore::ResultStore;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing

impl fmt::Display for SomeSquare {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("S[");
        let _ = write!(rc_str, "{}", &self.state);
        let _ = write!(rc_str, ", pn: {}", &self.pn);
        if self.pnc {
            rc_str.push_str(", pnc: t");
        } else {
            rc_str.push_str(", pnc: f");
        }

        let _ = write!(rc_str, ", {}", self.rules);
        //let _ = write!(rc_str, ", rslts: {}", &self.results);

        rc_str.push(']');

        write!(f, "{rc_str}")
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
    pub state: SomeState,     // State that an action was taken on.
    pub results: ResultStore, // Circular list of most recent results.
    pub rules: RuleStore,     // Rules, 0, 1 or 2 rules depending on pn
    pub pn: Pn,               // Square Pattern number.
    pub pnc: bool,            // Pattern number confirmed.
}

impl SomeSquare {
    /// Return a new Square instance.
    pub fn new(state: SomeState, result_state: SomeState) -> Self {
        let mut rules = RuleStore::new();
        rules.push(SomeRule::new(&state, &result_state));

        Self {
            state,
            results: ResultStore::new(result_state),
            rules,
            pn: Pn::One,
            pnc: false,
        }
    }

    /// Return a rate for a square, to determine an anchor weight.
    /// Squares that are pnc are equal weight.
    /// Other squares are the weight of their number of results stored,
    /// presumably LT 4.
    pub fn rate(&self) -> usize {
        if self.pnc {
            return 4;
        }

        self.len_results()
    }

    /// Return a string representing a square.
    pub fn str_terse(&self) -> String {
        self.state.formatted_string()
    }

    /// Add a result to a square (4-item circular buffer).
    /// Return true if the addition changed the square, either the
    /// pn or pnc changed.  If there is a change, update the rules.
    pub fn add_result(&mut self, st: SomeState) -> bool {
        let mut str_info = String::from(&format!(
            "\n  Square {} adding result {} {}",
            self.str_terse(),
            self.results.len() + 1,
            &st
        ));

        let sav_pn = self.pn;
        let sav_pnc = self.pnc;

        self.pn = self.results.add_result(st);
        self.pnc = self.calc_pnc();

        let mut changed = false;
        if self.pn != sav_pn || self.pnc != sav_pnc {
            changed = true;

            match self.pn {
                Pn::One => {
                    if self.rules.len() != 1 {
                        self.rules = RuleStore::new();
                        self.rules
                            .push(SomeRule::new(&self.state, self.results.first()));
                    }
                }
                Pn::Two => {
                    if self.rules.len() != 2 {
                        self.rules = RuleStore::new();
                        self.rules
                            .push(SomeRule::new(&self.state, self.results.first()));
                        self.rules
                            .push(SomeRule::new(&self.state, self.results.second()));
                    }
                }
                Pn::Unpredictable => {
                    if self.rules.is_not_empty() {
                        self.rules = RuleStore::new();
                    }
                }
            }
        }

        if self.pn != sav_pn {
            let _ = write!(str_info, ", pn changed from {} to {}", &sav_pn, &self.pn);
        } else {
            let _ = write!(str_info, ", pn {}", &self.pn);
        }

        if self.pnc != sav_pnc {
            let _ = write!(str_info, ", pnc changed from {} to {}", &sav_pnc, &self.pnc);
        } else {
            let _ = write!(str_info, ", pnc {}", &self.pnc);
        }

        println!("{}", &str_info);
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

    /// Return true if two squares are adjacent, that is they differ by exactly one bit.
    pub fn is_adjacent(&self, other: &SomeSquare) -> bool {
        self.state.is_adjacent(&other.state)
    }

    /// Return the most recent results
    pub fn most_recent_result(&self) -> &SomeState {
        self.results.most_recent_result()
    }

    /// Return a String representing a SomeSquare instance.
    pub fn formatted_string2(&self) -> String {
        let mut rc_str = String::from("S[");
        let _ = write!(rc_str, "{}", &self.state);
        let _ = write!(rc_str, ", pn: {}", &self.pn);
        let _ = write!(rc_str, ", pnc: {}", &self.pnc);
        let _ = write!(rc_str, ", {}", self.rules);

        rc_str.push(']');
        rc_str
    }

    /// Check if two squares can be combined.
    /// This does not check squares that may be between them.
    pub fn can_combine(&self, sqrx: &SomeSquare) -> Option<bool> {
        assert!(self.state != sqrx.state); // Combining a square with itself is probably an error in logic.

        // Predictability established for both squares.
        if self.pnc && sqrx.pnc {
            if self.pn != sqrx.pn {
                return Some(false);
            }
            if self.pn == Pn::Unpredictable {
                return Some(true);
            }

            return self.rules.can_form_union(&sqrx.rules);
        }

        // Check for bootstrap compatible.
        // Rules have to be formed to allow going back and resampling.
        // The rules may have a significant failure rate.
        if self.pn == Pn::One && sqrx.pn == Pn::One {
            if Some(true) == self.rules.can_form_union(&sqrx.rules) {
                return Some(true);
            }
            if self.pnc || sqrx.pnc {
                return Some(false);
            }
            return None;
        }

        if self.pnc {
            // so sqrx.pnc == false
            if sqrx.pn > self.pn {
                return Some(false);
            }
            if self.pn != Pn::Unpredictable && self.rules.can_form_union(&sqrx.rules) == Some(false)
            {
                return Some(false);
            }
        } else if sqrx.pnc {
            // so self.pnc == false
            if self.pn > sqrx.pn {
                return Some(false);
            }
            if sqrx.pn != Pn::Unpredictable && self.rules.can_form_union(&sqrx.rules) == Some(false)
            {
                return Some(false);
            }
        }
        // Need more samples
        None
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
    pub fn is_between(&self, sqr1: &SomeSquare, sqr2: &SomeSquare) -> bool {
        if self.state == sqr1.state {
            return false;
        }
        if self.state == sqr2.state {
            return false;
        }

        self.state
            .bitwise_or(&sqr1.state)
            .bitwise_and(&self.state.bitwise_or(&sqr2.state))
            .to_mask()
            .is_low()
    }

    /// Return the distance (number of bits different) between two squares.
    pub fn distance(&self, other: &SomeSquare) -> usize {
        self.state.distance(&other.state)
    }
} // end impl SomeSquare

#[cfg(test)]
mod tests {
    use super::*;

    // Test multiple additions to a square, cycle through all pn and pnc values.
    #[test]
    fn cycle_through_pn_pnc_values() -> Result<(), String> {
        let mut sqrx = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );
        assert!(sqrx.pn == Pn::One);
        assert!(!sqrx.pnc);

        // Second result, same as the first.
        let changed = sqrx.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        assert!(changed);
        assert!(sqrx.pn == Pn::One);
        assert!(sqrx.pnc);

        // Third result, same as the first two.
        let changed = sqrx.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        assert!(!changed);
        assert!(sqrx.pn == Pn::One);
        assert!(sqrx.pnc);

        // Fourth result, different from the first three, square becomes Unpredictable.
        let changed = sqrx.add_result(SomeState::new_from_string(1, "s0b0100").unwrap());
        assert!(changed);
        assert!(sqrx.pn == Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Fifth result, same as the first, square remains Unpredictable.
        let changed = sqrx.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        assert!(!changed);
        assert!(sqrx.pn == Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Sixth result, same as the second most recent, square becomes Pn::Two.
        let changed = sqrx.add_result(SomeState::new_from_string(1, "s0b0100").unwrap());
        assert!(changed);
        assert!(sqrx.pn == Pn::Two);
        assert!(sqrx.pnc);

        // Seventh result, same as the second most recent, square stays Pn::Two.
        let changed = sqrx.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        assert!(!changed);
        assert!(sqrx.pn == Pn::Two);
        assert!(sqrx.pnc);

        // Eighth result, same as the most recent, square becomes Pn::Unpredictable.
        let changed = sqrx.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        assert!(changed);
        assert!(sqrx.pn == Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Nineth result, same as the most recent, square remains Pn::Unpredictable.
        let changed = sqrx.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        assert!(!changed);
        assert!(sqrx.pn == Pn::Unpredictable);
        assert!(sqrx.pnc);

        // Tenth result, same as the most recent, square becomes Pn::One.
        let changed = sqrx.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        assert!(changed);
        assert!(sqrx.pn == Pn::One);
        assert!(sqrx.pnc);

        Ok(())
    }

    // Test can_combine cannot compare the same square.
    #[test]
    #[should_panic]
    fn can_combine_not_same() {
        let sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );

        let sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );

        sqr1.can_combine(&sqr2);

        ()
    }

    // Test can_combine Pn == One
    #[test]
    fn can_combine_pn_one_one() -> Result<(), String> {
        // Test two squares with only one sample each.
        // Allow a true result for bootstrapping.
        let sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );

        let sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b1101").unwrap(),
            SomeState::new_from_string(1, "s0b1101").unwrap(),
        );

        if sqr1.can_combine(&sqr2) != Some(true) {
            return Err(String::from("Test 1 failed?"));
        }

        // Create a square incompatible to sqr1 to produce M result.
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b1101").unwrap(),
            SomeState::new_from_string(1, "s0b1100").unwrap(),
        );

        if sqr1.can_combine(&sqr3).is_some() {
            return Err(String::from("Test 2 failed?"));
        }

        // Add to sqr3 to make it pnc.
        sqr3.add_result(SomeState::new_from_string(1, "s0b1100").unwrap());

        if sqr1.can_combine(&sqr3) != Some(false) {
            return Err(String::from("Test 3 failed?"));
        }

        if sqr3.can_combine(&sqr1) != Some(false) {
            return Err(String::from("Test 4 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == One and Pn == two
    #[test]
    fn can_combine_pn_one_two() -> Result<(), String> {
        // Create sqr1 pn 1 pnc f.
        let sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );

        // Create sqr2 pn 2 pnc f, one result compatible to sqr1.
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b1101").unwrap(),
            SomeState::new_from_string(1, "s0b1101").unwrap(),
        );

        sqr2.add_result(SomeState::new_from_string(1, "s0b1100").unwrap());

        // Create sqr3 pn 1 pnc f, incompatible to any result in sqr2.
        let sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b1100").unwrap(),
            SomeState::new_from_string(1, "s0b0100").unwrap(),
        );

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc f.
        if sqr1.can_combine(&sqr2).is_some() {
            return Err(String::from("Test 1 failed?"));
        }

        // Test sqr3 pn 1 pnc f, sqr2 pn 2 pnc f.
        if sqr3.can_combine(&sqr2).is_some() {
            return Err(String::from("Test 2 failed?"));
        }

        // Create sqr4 pn 1 pnc t, compatible with one result in sqr2.
        let mut sqr4 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );

        sqr4.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());

        // Test sqr4 pn 1 pnc t, sqr2 pn 2 pnc f.
        if sqr4.can_combine(&sqr2) != Some(false) {
            return Err(String::from("Test 3 failed?"));
        }

        // Exersize other branch of code.
        if sqr2.can_combine(&sqr4) != Some(false) {
            return Err(String::from("Test 4 failed?"));
        }

        // Add to sqr2 to make it pnc.
        sqr2.add_result(SomeState::new_from_string(1, "s0b1101").unwrap());
        sqr2.add_result(SomeState::new_from_string(1, "s0b1100").unwrap());

        println!("sqr1 {}", sqr1);
        println!("sqr2 {}", sqr2);
        println!("can: {:?}", sqr1.can_combine(&sqr2));

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc t.
        if sqr1.can_combine(&sqr2).is_some() {
            return Err(String::from("Test 5 failed?"));
        }

        // Exersize other branch of code.
        if sqr2.can_combine(&sqr1).is_some() {
            return Err(String::from("Test 6 failed?"));
        }

        // Test sqr3 pn 1 pnc f, sqr2 pn 2 pnc t.
        if sqr2.can_combine(&sqr3) != Some(false) {
            return Err(String::from("Test 7 failed?"));
        }

        // Exersize other branch of code.
        if sqr3.can_combine(&sqr2) != Some(false) {
            return Err(String::from("Test 8 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == two
    #[test]
    fn can_combine_pn_two_two() -> Result<(), String> {
        // Create sqr1 pn 2 pnc f.
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b1101").unwrap(),
            SomeState::new_from_string(1, "s0b1101").unwrap(),
        );

        sqr1.add_result(SomeState::new_from_string(1, "s0b1100").unwrap());

        // Create sqr2 pn 2 pnc f.
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );

        sqr2.add_result(SomeState::new_from_string(1, "s0b0100").unwrap());

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc f.
        if sqr1.can_combine(&sqr2).is_some() {
            return Err(String::from("Test 1 failed?"));
        }

        // Create sqr3 pn 2 pnc f, not compatible with sqr1.
        let mut sqr3 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );

        sqr3.add_result(SomeState::new_from_string(1, "s0b1101").unwrap());

        // Test sqr1 pn 1 pnc f, sqr3 pn 2 pnc f.
        if sqr1.can_combine(&sqr3).is_some() {
            return Err(String::from("Test 2 failed?"));
        }

        // Make sqr1 pnc.
        sqr1.add_result(SomeState::new_from_string(1, "s0b1101").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0b1100").unwrap());

        // Test sqr1 pn 1 pnc t, sqr2 pn 2 pnc f.
        if sqr1.can_combine(&sqr2).is_some() {
            return Err(String::from("Test 3 failed?"));
        }

        // Exersize other branch of code.
        if sqr2.can_combine(&sqr1).is_some() {
            return Err(String::from("Test 4 failed?"));
        }

        // Add to sqr2 to make it pnc.
        sqr2.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        sqr2.add_result(SomeState::new_from_string(1, "s0b0100").unwrap());

        // Test sqr1 pn 2 pnc t, sqr2 pn 2 pnc t.
        if sqr1.can_combine(&sqr2) != Some(true) {
            return Err(String::from("Test 5 failed?"));
        }

        // Test sqr1 pn 2 pnc t, sqr3 pn 2 pnc f.
        if sqr1.can_combine(&sqr3) != Some(false) {
            return Err(String::from("Test 6 failed?"));
        }

        // Create sqr4, cause X0/X1 or Xx/XX combination error with sqr1.
        let mut sqr4 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0100").unwrap(),
            SomeState::new_from_string(1, "s0b0100").unwrap(),
        );

        sqr4.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        sqr4.add_result(SomeState::new_from_string(1, "s0b0100").unwrap());
        sqr4.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());

        // Test sqr1 pn 2 pnc t, sqr4 pn 2 pnc t.
        if sqr1.can_combine(&sqr4) != Some(false) {
            return Err(String::from("Test 7 failed?"));
        }

        // Create sqr5, combinable with sqr1, but results in reverse order.
        let mut sqr5 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0100").unwrap(),
        );

        sqr5.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        sqr5.add_result(SomeState::new_from_string(1, "s0b0100").unwrap());
        sqr5.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());

        // Test sqr1 pn 2 pnc t, sqr5 pn 2 pnc t.
        if sqr1.can_combine(&sqr5) != Some(true) {
            return Err(String::from("Test 8 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == Unpredictable, Pn == 1
    #[test]
    fn can_combine_pn_u_one() -> Result<(), String> {
        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b1101").unwrap(),
            SomeState::new_from_string(1, "s0b1101").unwrap(),
        );

        sqr1.add_result(SomeState::new_from_string(1, "s0b1100").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0b1000").unwrap());

        // Create sqr2 pn 1 pnc f.
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );

        // Test sqr1 pn U pnc t, sqr2 pn 1 pnc f.
        if sqr1.can_combine(&sqr2).is_some() {
            return Err(String::from("Test 1 failed?"));
        }

        // Exorsize other code branch.
        if sqr2.can_combine(&sqr1).is_some() {
            return Err(String::from("Test 2 failed?"));
        }

        // Make sqr2 pnc == true.
        sqr2.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());

        // Test sqr1 pn U pnc t, sqr2 pn 1 pnc t.
        if sqr1.can_combine(&sqr2) != Some(false) {
            return Err(String::from("Test 3 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == Unpredictable, Pn == 2
    #[test]
    fn can_combine_pn_u_two() -> Result<(), String> {
        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b1101").unwrap(),
            SomeState::new_from_string(1, "s0b1101").unwrap(),
        );

        sqr1.add_result(SomeState::new_from_string(1, "s0b1100").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0b1000").unwrap());

        // Create sqr2 pn 2 pnc f.
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0101").unwrap(),
            SomeState::new_from_string(1, "s0b0101").unwrap(),
        );
        sqr2.add_result(SomeState::new_from_string(1, "s0b0100").unwrap());

        // Test sqr1 pn U pnc t, sqr2 pn 2 pnc f.
        if sqr1.can_combine(&sqr2).is_some() {
            return Err(String::from("Test 1 failed?"));
        }

        // Exersize other code branch.
        if sqr2.can_combine(&sqr1).is_some() {
            return Err(String::from("Test 2 failed?"));
        }

        // Make sqr2 pnc == true.
        sqr2.add_result(SomeState::new_from_string(1, "s0b0101").unwrap());
        sqr2.add_result(SomeState::new_from_string(1, "s0b0100").unwrap());

        // Test sqr1 pn U pnc t, sqr2 pn 2 pnc t.
        if sqr1.can_combine(&sqr2) != Some(false) {
            return Err(String::from("Test 3 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == Unpredictable, Pn == Unpredictable
    #[test]
    fn can_combine_pn_u_u() -> Result<(), String> {
        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b1101").unwrap(),
            SomeState::new_from_string(1, "s0b1101").unwrap(),
        );

        sqr1.add_result(SomeState::new_from_string(1, "s0b1100").unwrap());
        sqr1.add_result(SomeState::new_from_string(1, "s0b1000").unwrap());

        // Create sqr2 pn U pnc T.
        let mut sqr2 = SomeSquare::new(
            SomeState::new_from_string(1, "s0b0001").unwrap(),
            SomeState::new_from_string(1, "s0b0001").unwrap(),
        );

        sqr2.add_result(SomeState::new_from_string(1, "s0b0010").unwrap());
        sqr2.add_result(SomeState::new_from_string(1, "s0b0100").unwrap());

        // Test sqr1 pn U pnc t, sqr2 pn U pnc t.
        if sqr1.can_combine(&sqr2) != Some(true) {
            return Err(String::from("Test 1 failed?"));
        }

        Ok(())
    }
}
