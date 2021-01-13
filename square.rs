// Implement a square struct,
// which represents a state in a K-Map and one or more result states
// from excuting an action.

use crate::combinable::Combinable;
use crate::pn::Pn;
use crate::resultstore::ResultStore;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeSquare {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("S[");
        rc_str.push_str(&format!("{}", &self.state));
        rc_str.push_str(&format!(", pn: {}", &self.pn()));
        rc_str.push_str(&format!(", pnc: {}", &self.pnc()));
        rc_str.push_str(&format!(", ch: {}", &self.changed()));
        rc_str.push_str(&format!(", rslts: {}", &self.results));

        rc_str.push_str(&format!(", {}", self.rules));

        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct SomeSquare {
    pub state: SomeState,     // State that an action was taken on.
    pub results: ResultStore, // Circular list of results.
    pub rules: RuleStore,     // Rules, 0, 1 or 2 rules depending on pn
}

impl SomeSquare {
    // Return a new Square instance
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

    pub fn pn(&self) -> Pn {
        self.results.pn
    }

    pub fn pnc(&self) -> bool {
        self.results.pnc
    }

    pub fn changed(&self) -> bool {
        self.results.changed
    }

    //    pub fn is_new(&self) -> bool {
    //        1 == self.results.len()
    //    }

    pub fn str_terse(&self) -> String {
        self.state.bts.formatted_string('S')
    }

    // Can two squares be combined?
    //
    // This is one of the most tricky, and crucial, functions.
    //
    // At program start, you want a few easy wins by combining squares
    // with only one sample each, even if a few combinations turn out to be invalid.
    //
    // Combinations allow plans to be formed to get desired samples,
    // to improve understanding of the logic.
    //
    // Combinations will be invalidated if they produce an unexpected result
    // when used in a plan.
    //
    // No more than the last four samples are considered.
    //
    // The pattern number (pn) and the number of samples are
    // deciding factors.
    //
    // With continuing samples, a square might cycle through different
    // patterns, although that should be rare.
    //
    // This returns three possible results: True, False or MoreSamplesNeeded.
    //
    pub fn can_combine(&self, other: &Self) -> Combinable {
        match self.pn() {
            Pn::One => {
                match other.pn() {
                    Pn::One => {
                        // self.pn == One, other.pn == One
                        // If the rules can be combined, the squares can be combined.
                        if let Some(_runx) = self.rules.union(&other.rules) {
                            return Combinable::True;
                        }
                        // else
                        return Combinable::False;
                    }

                    Pn::Two => {
                        // self.pn == One, other.pn == Two
                        // If the pn==One square has GT one sample, the squares cannot be combined.
                        if self.len_results() > 1 {
                            return Combinable::False;
                        }

                        // If the pn==One square rule is combinable with one of the
                        // pn==Two square rules, more samples are needed.
                        if self.rules[0].union(&other.rules[0]).is_valid_union() {
                            return Combinable::MoreSamplesNeeded;
                        }

                        if self.rules[0].union(&other.rules[1]).is_valid_union() {
                            return Combinable::MoreSamplesNeeded;
                        }

                        // else
                        return Combinable::False;
                    }

                    Pn::Unpredictable => {
                        // self.pn == One, other.pn == Unpredictable
                        // If the pn==One square is confirmed, the squares cannot be combined.
                        if self.pnc() {
                            return Combinable::False;
                        }

                        // The pn==One square needs more samples until it is confirmed.
                        return Combinable::MoreSamplesNeeded;
                    }
                } // end match other.pn
            }
            Pn::Two => {
                match other.pn() {
                    Pn::One => {
                        // self.pn == Two, other.pn == One
                        // If the pn==One square is has GT 1 sample, the squares cannot be combined.
                        if other.len_results() > 1 {
                            return Combinable::False;
                        }

                        // If the pn==one square has one sample, and
                        // its rule is combinable with one of the pn==Two square rules,
                        // more samples are needed.
                        if other.rules[0].union(&self.rules[0]).is_valid_union() {
                            return Combinable::MoreSamplesNeeded;
                        }

                        if other.rules[0].union(&self.rules[1]).is_valid_union() {
                            return Combinable::MoreSamplesNeeded;
                        }

                        // else
                        return Combinable::False;
                    }
                    Pn::Two => {
                        // self.pn == Two, other.pn == Two
                        // The pn values match, if the rules can be combined,
                        // the squares can be combined.
                        if let Some(_runx) = self.rules.union(&other.rules) {
                            return Combinable::True;
                        }
                        // else
                        return Combinable::False;
                    }
                    Pn::Unpredictable => {
                        // self.pn == Two, other = Unpredictable
                        // If the pn==Two square is not confirmed, more samples needed.
                        if self.pnc() == false {
                            return Combinable::MoreSamplesNeeded;
                        }
                        // else
                        return Combinable::False;
                    }
                } // end match other.pn
            }
            Pn::Unpredictable => {
                match other.pn() {
                    // self.pn == Unpredictable, other.pn == One
                    Pn::One => {
                        // self.pn == Unpredictable
                        // If the pn==One square is confirmed,
                        // the squares cannot be combined.
                        if other.pnc() {
                            return Combinable::False;
                        }

                        // The pn==One square needs more samples until it is confirmed.
                        return Combinable::MoreSamplesNeeded;
                    }
                    Pn::Two => {
                        // self.pn == Unpredictable, other.pn == Two
                        // If the pn==Two square is confirmed, the squares cannot be combined.
                        if other.pnc() {
                            return Combinable::False;
                        }

                        // The smaller pn==Two square needs more samples until it is confirmed.
                        return Combinable::MoreSamplesNeeded;
                    }
                    Pn::Unpredictable => {
                        // self.pn == Unpredictable, other.pn == Unpredictable
                        // The pn values match, no rules exist to be checked,
                        // the squares can be combined.
                        return Combinable::True;
                    }
                } // end match other.pn
            }
        } // end match self.pn
    } // end can_combine

    // Add a result to a square (4-item circular buffer).
    // Return true if the addition changed the square, either the
    // pn or pnc changed.  If there is a change, update the rules.
    pub fn add_result(&mut self, st: SomeState) -> bool {
        //println!("Adding result {} to square {}", st, self.str_terse());

        let rc = self.results.push_wrap(st);

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
        rc
    }

    pub fn len_results(&self) -> usize {
        self.results.len()
    }

    pub fn last_result(&self) -> SomeState {
        self.results.last_result()
    }

    pub fn second_last_result(&self) -> SomeState {
        self.results.second_last_result()
    }

    pub fn is_adjacent(&self, other: &SomeSquare) -> bool {
        self.state.is_adjacent(&other.state)
    }

    pub fn distance(&self, other: &SomeSquare) -> usize {
        self.state.distance(&other.state)
    }

    // Given a 2-rule RuleStore, and a square within it,
    // return the expected next result for the square
    pub fn next_result(&self, ruls: &RuleStore) -> SomeState {
        assert!(ruls.avec.len() == 2);

        assert!(ruls[0].initial_region().is_superset_of_state(&self.state));

        if self.len_results() > 1 {
            return self.second_last_result();
        }

        let result = self.last_result();

        let rulx = SomeRule::new(&self.state, &result);

        if rulx.is_subset_of(&ruls.avec[0]) {
            return ruls.avec[1].result_from_initial_state(&self.state);
        }

        ruls.avec[0].result_from_initial_state(&self.state)
    }
} // end impl SomeSquare
