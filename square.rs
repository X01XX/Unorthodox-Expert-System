//! The SomeSquare struct. This represents a state/square in a pseudo Karnaugh Map, and result states from excuting an action.

use crate::pn::Pn;
use crate::resultstore::ResultStore;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::state::SomeState;
use crate::truth::Truth;

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeSquare {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("S[");
        rc_str.push_str(&format!("{}", &self.state));
        rc_str.push_str(&format!(", pn: {}", &self.results.pn));
        rc_str.push_str(&format!(", pnc: {}", &self.results.pnc));
        rc_str.push_str(&format!(", ch: {}", &self.changed()));
        rc_str.push_str(&format!(", rslts: {}", &self.results));

        rc_str.push_str(&format!(", {}", self.rules));

        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

impl PartialEq for SomeSquare {
    fn eq(&self, other: &Self) -> bool {
        self.state == other.state
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug)]

/// A state, with the most recent results on a given action.
pub struct SomeSquare {
    pub state: SomeState,     // State that an action was taken on.
    pub results: ResultStore, // Circular list of most recent results.
    pub rules: RuleStore,     // Rules, 0, 1 or 2 rules depending on pn
}

impl SomeSquare {
    /// Return a new Square instance.
    pub fn new(state: SomeState, result_state: SomeState) -> Self {
        let mut rcsqr = Self {
            state: state.clone(),
            results: ResultStore::new(result_state.clone()),
            rules: RuleStore::new(),
        };

        rcsqr.rules.push(SomeRule::new(&state, &result_state));
        //println!("New square {}", rcsqr);
        rcsqr
    }

    /// Return true if the most recent sample changed some interpretation of a square.
    pub fn changed(&self) -> bool {
        self.results.changed
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
            "\n  Square {} adding result{}{}",
            self.str_terse(),
            format!(" {} ", self.results.num_results + 1),
            &st
        ));

        let sav_pn = self.results.pn;
        let sav_pnc = self.results.pnc;

        let rc = self.results.add_result(st);

        if rc {
            match self.results.pn {
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
                    if self.rules.len() != 0 {
                        self.rules = RuleStore::new();
                    }
                }
            }
        }

        if sav_pn != self.results.pn {
            str_info.push_str(&format!(
                ", pn changed from {} to {}",
                &sav_pn, &self.results.pn
            ));
        } else {
            str_info.push_str(&format!(", pn {}", &self.results.pn));
        }

        if sav_pnc != self.results.pnc {
            str_info.push_str(&format!(
                ", pnc changed from {} to {}",
                &sav_pnc, &self.results.pnc
            ));
        } else {
            str_info.push_str(&format!(", pnc {}", &self.results.pnc));
        }

        println!("{}", &str_info);
        rc
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

    pub fn formatted_string2(&self) -> String {
        let mut rc_str = String::from("S[");
        rc_str.push_str(&format!("{}", &self.state));
        rc_str.push_str(&format!(", pn: {}", &self.results.pn));
        rc_str.push_str(&format!(", pnc: {}", &self.results.pnc));
        rc_str.push_str(&format!(", ch: {}", &self.changed()));
        rc_str.push_str(&format!(", {}", self.rules));

        rc_str.push(']');
        rc_str
    }

    // Check if two squares can be combined.
    pub fn can_combine(&self, sqrx: &SomeSquare) -> Truth {
        assert!(*self != *sqrx);

        if self.results.pnc && sqrx.results.pnc {
            if self.results.pn != sqrx.results.pn {
                return Truth::F;
            }
            if self.results.pn == Pn::Unpredictable {
                return Truth::T;
            }

            return self.rules.can_form_union(&sqrx.rules);
        }

        // Check for bootstrap compatible
        if self.results.pn == Pn::One && sqrx.results.pn == Pn::One {
            if self.rules.can_form_union(&sqrx.rules) == Truth::T {
                return Truth::T;
            }
        }

        if self.results.pnc {
            // so sqrx.results.pnc == false
            if sqrx.results.pn > self.results.pn {
                return Truth::F;
            }
            if self.results.pn == Pn::Unpredictable {
                return Truth::M;
            }
            if self.rules.can_form_union(&sqrx.rules) != Truth::F {
                return Truth::M;
            }
            return Truth::F;
        }

        if sqrx.results.pnc {
            // so self.results.pnc == false
            if self.results.pn > sqrx.results.pn {
                return Truth::F;
            }
            if sqrx.results.pn == Pn::Unpredictable {
                return Truth::M;
            }
            if self.rules.can_form_union(&sqrx.rules) != Truth::F {
                return Truth::M;
            }
            return Truth::F;
        }

        Truth::M
    }
} // end impl SomeSquare

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;

    // Test can_combine cannot compare the same square.
    #[test]
    #[should_panic]
    fn can_combine_not_same() {

        let sqr1 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                   SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        let sqr2 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                   SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        sqr1.can_combine(&sqr2);

        ()
    }

    // Test can_combine Pn == One
    #[test]
    fn can_combine_pn_one_one() -> Result<(), String> {

        // Test two squares with only one sample each.
        // Allow a true result for bootstrapping.
        let sqr1 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                   SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        let sqr2 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()),
                                   SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()));

        if sqr1.can_combine(&sqr2) != Truth::T {
            return Err(String::from("Test 1 failed?"));
        }

        // Create a square incompatible to sqr1 to produce M result.
        let mut sqr3 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()));

        if sqr1.can_combine(&sqr3) != Truth::M {
            return Err(String::from("Test 2 failed?"));
        }

        // Add to sqr3 to make it pnc.
        sqr3.add_result(SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()));

        if sqr1.can_combine(&sqr3) != Truth::F {
            return Err(String::from("Test 3 failed?"));
        }

        if sqr3.can_combine(&sqr1) != Truth::F {
            return Err(String::from("Test 4 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == One and Pn == two
    #[test]
    fn can_combine_pn_one_two() -> Result<(), String> {

        // Create sqr1 pn 1 pnc f.
        let sqr1 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                   SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        // Create sqr2 pn 2 pnc f, one result compatible to sqr1.
        let mut sqr2 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()));

        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()));

        // Create sqr3 pn 1 pnc f, incompatible to any result in sqr2.
        let sqr3 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()),
                                   SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc f.
        if sqr1.can_combine(&sqr2) != Truth::M {
            return Err(String::from("Test 1 failed?"));
        }

        // Test sqr3 pn 1 pnc f, sqr2 pn 2 pnc f.
        if sqr3.can_combine(&sqr2) != Truth::M {
            return Err(String::from("Test 2 failed?"));
        }

        // Create sqr4 pn 1 pnc t, compatible with one result in sqr2.
        let mut sqr4 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        sqr4.add_result(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        // Test sqr4 pn 1 pnc t, sqr2 pn 2 pnc f.
        if sqr4.can_combine(&sqr2) != Truth::F {
            return Err(String::from("Test 3 failed?"));
        }

        // Exersize other branch of code. 
        if sqr2.can_combine(&sqr4) != Truth::F {
            return Err(String::from("Test 4 failed?"));
        }

        // Add to sqr2 to make it pnc.
        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()));
        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()));

        println!("sqr1 {}", sqr1);
        println!("sqr2 {}", sqr2);
        println!("can: {}", sqr1.can_combine(&sqr2));

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc t.
        if sqr1.can_combine(&sqr2) != Truth::M {
            return Err(String::from("Test 5 failed?"));
        }

        // Exersize other branch of code.
        if sqr2.can_combine(&sqr1) != Truth::M {
            return Err(String::from("Test 6 failed?"));
        }

        // Test sqr3 pn 1 pnc f, sqr2 pn 2 pnc t.
        if sqr2.can_combine(&sqr3) != Truth::F {
            return Err(String::from("Test 7 failed?"));
        }

        // Exersize other branch of code.
        if sqr3.can_combine(&sqr2) != Truth::F {
            return Err(String::from("Test 8 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == two
    #[test]
    fn can_combine_pn_two_two() -> Result<(), String> {

        // Create sqr1 pn 2 pnc f.
        let mut sqr1 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()));

        sqr1.add_result(SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()));

        // Create sqr2 pn 2 pnc f.
        let mut sqr2 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));

        // Test sqr1 pn 1 pnc f, sqr2 pn 2 pnc f.
        if sqr1.can_combine(&sqr2) != Truth::M {
            return Err(String::from("Test 1 failed?"));
        }

        // Create sqr3 pn 2 pnc f, not compatible with sqr1.
        let mut sqr3 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        sqr3.add_result(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()));

        // Test sqr1 pn 1 pnc f, sqr3 pn 2 pnc f.
        if sqr1.can_combine(&sqr3) != Truth::M {
            return Err(String::from("Test 2 failed?"));
        }

        // Make sqr1 pnc.
        sqr1.add_result(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()));
        sqr1.add_result(SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()));

        // Test sqr1 pn 1 pnc t, sqr2 pn 2 pnc f.
        if sqr1.can_combine(&sqr2) != Truth::M {
            return Err(String::from("Test 3 failed?"));
        }
        
        // Exersize other branch of code.
        if sqr2.can_combine(&sqr1) != Truth::M {
            return Err(String::from("Test 4 failed?"));
        }

        // Add to sqr2 to make it pnc.
        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));
        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));

        // Test sqr1 pn 2 pnc t, sqr2 pn 2 pnc t.
        if sqr1.can_combine(&sqr2) != Truth::T {
            return Err(String::from("Test 5 failed?"));
        }

        // Test sqr1 pn 2 pnc t, sqr3 pn 2 pnc f.
        if sqr1.can_combine(&sqr3) != Truth::F {
            return Err(String::from("Test 6 failed?"));
        }

        // Create sqr4, cause X0/X1 or Xx/XX combination error with sqr1.
        let mut sqr4 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));

        sqr4.add_result(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));
        sqr4.add_result(SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));
        sqr4.add_result(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        // Test sqr1 pn 2 pnc t, sqr4 pn 2 pnc t.
        if sqr1.can_combine(&sqr4) != Truth::F {
            return Err(String::from("Test 7 failed?"));
        }

        // Create sqr5, combinable with sqr1, but results in reverse order.
        let mut sqr5 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));

        sqr5.add_result(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));
        sqr5.add_result(SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));
        sqr5.add_result(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        // Test sqr1 pn 2 pnc t, sqr5 pn 2 pnc t.
        if sqr1.can_combine(&sqr5) != Truth::T {
            return Err(String::from("Test 8 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == Unpredictable, Pn == 1
    #[test]
    fn can_combine_pn_u_one() -> Result<(), String> {

        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()));

        sqr1.add_result(SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()));
        sqr1.add_result(SomeState::new(SomeBits::new_from_string(1, "1000").unwrap()));

        // Create sqr2 pn 1 pnc f.
        let mut sqr2 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        // Test sqr1 pn U pnc t, sqr2 pn 1 pnc f.
        if sqr1.can_combine(&sqr2) != Truth::M {
            return Err(String::from("Test 1 failed?"));
        }

        // Exorsize other code branch.
        if sqr2.can_combine(&sqr1) != Truth::M {
            return Err(String::from("Test 2 failed?"));
        }

        // Make sqr2 pnc == true.
        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));

        // Test sqr1 pn U pnc t, sqr2 pn 1 pnc t.
        if sqr1.can_combine(&sqr2) != Truth::F {
            return Err(String::from("Test 3 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == Unpredictable, Pn == 2
    #[test]
    fn can_combine_pn_u_two() -> Result<(), String> {

        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()));

        sqr1.add_result(SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()));
        sqr1.add_result(SomeState::new(SomeBits::new_from_string(1, "1000").unwrap()));

        // Create sqr2 pn 2 pnc f.
        let mut sqr2 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));
         sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));

        // Test sqr1 pn U pnc t, sqr2 pn 2 pnc f.
        if sqr1.can_combine(&sqr2) != Truth::M {
            return Err(String::from("Test 1 failed?"));
        }

        // Exersize other code branch.
        if sqr2.can_combine(&sqr1) != Truth::M {
            return Err(String::from("Test 2 failed?"));
        }

        // Make sqr2 pnc == true.
        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "0101").unwrap()));
        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));

        // Test sqr1 pn U pnc t, sqr2 pn 2 pnc t.
        if sqr1.can_combine(&sqr2) != Truth::F {
            return Err(String::from("Test 3 failed?"));
        }

        Ok(())
    }

    // Test can_combine Pn == Unpredictable, Pn == Unpredictable
    #[test]
    fn can_combine_pn_u_u() -> Result<(), String> {

        // Create sqr1 pn U pnc T.
        let mut sqr1 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "1101").unwrap()));

        sqr1.add_result(SomeState::new(SomeBits::new_from_string(1, "1100").unwrap()));
        sqr1.add_result(SomeState::new(SomeBits::new_from_string(1, "1000").unwrap()));

        // Create sqr2 pn U pnc T.
        let mut sqr2 = SomeSquare::new(SomeState::new(SomeBits::new_from_string(1, "0001").unwrap()),
                                       SomeState::new(SomeBits::new_from_string(1, "0001").unwrap()));

        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "0010").unwrap()));
        sqr2.add_result(SomeState::new(SomeBits::new_from_string(1, "0100").unwrap()));

        // Test sqr1 pn U pnc t, sqr2 pn U pnc t.
        if sqr1.can_combine(&sqr2) != Truth::T {
            return Err(String::from("Test 1 failed?"));
        }

        Ok(())
    }
}
