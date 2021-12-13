//! The SomeSquare struct. This represents a state/square in a pseudo Karnaugh Map, and result states from excuting an action.

use crate::pn::Pn;
use crate::resultstore::{ResultStore, MAX_RESULTS};
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::state::SomeState;
//use crate::truth::Truth;
//use crate::combine::can_combine;
use crate::compare::Compare;

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

//    /// Return the Pn value for a square.
//    pub fn get_pn(&self) -> Pn {
//        self.results.pn
//    }

//    /// Return the pnc (Pattern Number Confirmed) value for a square.
//    pub fn get_pnc(&self) -> bool {
//        self.results.pnc
//    }

    /// Return true if the most recent sample changed some interpretation of a square.
    pub fn changed(&self) -> bool {
        self.results.changed
    }

    /// Return a string representing a square.
    pub fn str_terse(&self) -> String {
        self.state.formatted_string()
    }


//    pub fn can_combine(&self, other: &Self) -> Truth {
//        can_combine(self, other)
//    } // end can_combine

    /// Add a result to a square (4-item circular buffer).
    /// Return true if the addition changed the square, either the
    /// pn or pnc changed.  If there is a change, update the rules.
    pub fn add_result(&mut self, st: SomeState) -> bool {
        let mut add_str = String::from(" ");
        if self.len_results() < MAX_RESULTS {
            add_str = format!(" {} ", self.len_results() + 1);
        }

        let mut str_info = String::from(&format!(
            "\n  Square {} adding result{}{}",
            self.str_terse(),
            add_str,
            &st
        ));

        let sav_pn = self.results.pn;
        let sav_pnc = self.results.pnc;

        let rc = self.results.push_back(st);

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

        if sav_pn != self.results.pn {
            str_info.push_str(&format!(", pn changed from {} to {}", &sav_pn, &self.results.pn));
        } else {
            str_info.push_str(&format!(", pn {}", &self.results.pn));
        }

        if sav_pnc != self.results.pnc {
            str_info.push_str(&format!(
                ", pnc changed from {} to {}",
                &sav_pnc,
                &self.results.pnc
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

} // end impl SomeSquare

impl Compare for SomeSquare {
    fn get_pn_ref(&self) -> &Pn {
        &self.results.pn
    }
    fn get_pnc(&self) -> bool {
        self.results.pnc
    }
    fn get_rules_ref(&self) -> &RuleStore {
        &self.rules
    }
    fn get_region(&self) -> SomeRegion {
        SomeRegion::new(&self.state, &self.state)
    }
}

