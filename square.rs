//! The SomeSquare struct. This represents a state/square in a pseudo Karnaugh Map, and result states from excuting an action.

use crate::pn::Pn;
use crate::resultstore::{ResultStore, MAX_RESULTS};
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
        rc_str.push_str(&format!(", pn: {}", &self.get_pn()));
        rc_str.push_str(&format!(", pnc: {}", &self.get_pnc()));
        rc_str.push_str(&format!(", ch: {}", &self.changed()));
        rc_str.push_str(&format!(", rslts: {}", &self.results));

        rc_str.push_str(&format!(", {}", self.rules));

        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct SomeSquare {
    state: SomeState,     // State that an action was taken on.
    results: ResultStore, // Circular list of results.
    rules: RuleStore,     // Rules, 0, 1 or 2 rules depending on pn
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

    /// Accessor, return a read-only reference to the state field.
    pub fn get_state(&self) -> &SomeState {
        &self.state
    }

    /// Accessor, return a read-only reference to the rules field.
    pub fn get_rules(&self) -> &RuleStore {
        &self.rules
    }

    /// Return the Pn value for a square.
    pub fn get_pn(&self) -> Pn {
        self.results.pn
    }

    /// Return the pnc (Pattern Number Confirmed) value for a square.
    pub fn get_pnc(&self) -> bool {
        self.results.pnc
    }

    /// Return true if the most recent sample changed some interpretation of a square.
    pub fn changed(&self) -> bool {
        self.results.changed
    }

    /// Return a string representing a square.
    pub fn str_terse(&self) -> String {
        self.state.get_bts().formatted_string('S')
    }

    /// Can two squares be combined?
    ///
    /// This is one of the most tricky, and crucial, functions.
    ///
    /// At program start, you want a few easy wins by combining squares
    /// with only one sample each, even if a few combinations turn out to be invalid.
    ///
    /// Combinations allow plans to be formed to get desired samples,
    /// to improve understanding of the logic.
    ///
    /// Combinations will be invalidated if they produce an unexpected result
    /// when used in a plan.
    ///
    /// No more than the last four samples are considered.
    ///
    /// The pattern number (pn) and the number of samples are
    /// deciding factors.
    ///
    /// With continuing samples, a square might cycle through different
    /// patterns, although that should be rare.
    ///
    /// This returns three possible results: True, False or MoreSamplesNeeded.
    ///
    pub fn can_combine(&self, other: &Self) -> Truth {
        match self.get_pn() {
            Pn::One => {
                match other.get_pn() {
                    Pn::One => {
                        // self.pn == One, other.pn == One
                        // If the rules can be combined, the squares can be combined.
                        if let Some(_runx) = self.rules.union(&other.rules) {
                            return Truth::T;
                        }
                        // else
                        return Truth::F;
                    }

                    Pn::Two => {
                        // self.pn == One, other.pn == Two
                        // If the pn==One square has GT one sample, the squares cannot be combined.
                        if self.len_results() > 1 {
                            return Truth::F;
                        }

                        // If the pn==One square rule is combinable with one of the
                        // pn==Two square rules, more samples are needed.
                        if self.rules[0].union(&other.rules[0]).is_valid_union() {
                            return Truth::M;
                        }

                        if self.rules[0].union(&other.rules[1]).is_valid_union() {
                            return Truth::M;
                        }

                        // else
                        return Truth::F;
                    }

                    Pn::Unpredictable => {
                        // self.pn == One, other.pn == Unpredictable
                        // If the pn==One square is pnc, the squares cannot be combined.
                        if self.get_pnc() {
                            return Truth::F;
                        }

                        // The pn==One square needs more samples.
                        return Truth::M;
                    }
                } // end match other.pn
            }
            Pn::Two => {
                match other.get_pn() {
                    Pn::One => {
                        // self.pn == Two, other.pn == One
                        // If the pn==One square is has GT 1 sample, the squares cannot be combined.
                        if other.len_results() > 1 {
                            return Truth::F;
                        }

                        // If the pn==one square has one sample, and
                        // its rule is combinable with one of the pn==Two square rules,
                        // more samples are needed.
                        if other.rules[0].union(&self.rules[0]).is_valid_union() {
                            return Truth::M;
                        }

                        if other.rules[0].union(&self.rules[1]).is_valid_union() {
                            return Truth::M;
                        }

                        // else
                        return Truth::F;
                    }
                    Pn::Two => {
                        // self.pn == Two, other.pn == Two
                        // The pn values match, if the rules can be combined,
                        // the squares can be combined.
                        if let Some(_runx) = self.rules.union(&other.rules) {
                            return Truth::T;
                        }
                        // else
                        return Truth::F;
                    }
                    Pn::Unpredictable => {
                        // self.pn == Two, other = Unpredictable
                        // If the pn==Two square is not pnc, more samples needed.
                        if self.get_pnc() == false {
                            return Truth::M;
                        }
                        // else
                        return Truth::F;
                    }
                } // end match other.pn
            }
            Pn::Unpredictable => {
                match other.get_pn() {
                    // self.pn == Unpredictable, other.pn == One
                    Pn::One => {
                        // self.pn == Unpredictable
                        // If the pn==One square is pnc,
                        // the squares cannot be combined.
                        if other.get_pnc() {
                            return Truth::F;
                        }

                        // The pn==One square needs more samples.
                        return Truth::M;
                    }
                    Pn::Two => {
                        // self.pn == Unpredictable, other.pn == Two
                        // If the pn==Two square is pnc, the squares cannot be combined.
                        if other.get_pnc() {
                            return Truth::F;
                        }

                        // The smaller pn==Two square needs more samples.
                        return Truth::M;
                    }
                    Pn::Unpredictable => {
                        // self.pn == Unpredictable, other.pn == Unpredictable
                        // The pn values match, no rules exist to be checked,
                        // the squares can be combined.
                        return Truth::T;
                    }
                } // end match other.pn
            }
        } // end match self.pn
    } // end can_combine

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

        let sav_pn = self.get_pn();
        let sav_pnc = self.get_pnc();

        let rc = self.results.push_wrap(st);

        match self.results.pn {
            Pn::One => {
                if self.rules.len() != 1 {
                    self.rules = RuleStore::new();
                    self.rules
                        .push(SomeRule::new(&self.state, self.results.first()));
                }
                if self.len_results() == 2 {
                    str_info.push_str(&format!(
                        ", result the same as first, so not subset of any pn==2, since order matters"
                    ));
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
        if sav_pn != self.get_pn() {
            str_info.push_str(&format!(", pn changed from {} to {}", &sav_pn, &self.get_pn()));
        } else {
            str_info.push_str(&format!(", pn {}", &self.get_pn()));
        }
        if sav_pnc != self.get_pnc() {
            str_info.push_str(&format!(
                ", pnc changed from {} to {}",
                &sav_pnc,
                &self.get_pnc()
            ));
        } else {
            str_info.push_str(&format!(", pnc {}", &self.get_pnc()));
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

    /// Return the second to last result for the square.
    pub fn last_result(&self) -> &SomeState {
        self.results.last_result()
    }

    /// Return true if two squares are adjacent, that is they differ by exactly one bit.
    pub fn is_adjacent(&self, other: &SomeSquare) -> bool {
        self.state.is_adjacent(&other.state)
    }
} // end impl SomeSquare
