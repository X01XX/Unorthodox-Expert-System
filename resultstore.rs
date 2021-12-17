//! The ResultStore struct.  Saves up to 4 results of an action for a single state.
//!
//! Uses a VecDeque to allow removal of the oldest sample when adding a new sample when 4 are already loaded.

use crate::pn::Pn;
use crate::state::SomeState;

pub const MAX_RESULTS: usize = 4; // Results for a two-result square can be seen twice, changing pnc to true.
// If three-result squares are to be supported, a one-result square would need three results before pnc = true, instead of two.
// So you could tell the difference between (1, 1, 1) and (1, 1, 2, 1, 1, 2).
// Some assumptions for one-result squares would need to be changed in other code.
// Better not to go there unless there is a really good reason.

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for ResultStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ResultStore {
    /// A VecDeque to store sample results for one domain/action/state, that is a square.
    pub astore: Vec<SomeState>,
    /// Number results seen so far.
    num_results: usize,
    /// Pattern number, One, Two or Unpredicatble, trips change indicator when changed.
    pub pn: Pn,
    /// When first sample, pn or pnc has changed.
    pub changed: bool,
    /// Pattern Number Confirmed.
    pub pnc: bool,
}

impl ResultStore {
    /// Return a new ResultStore, with an initial result.
    pub fn new(st: SomeState) -> Self {
        let mut ret = Self {
            astore: Vec::<SomeState>::with_capacity(MAX_RESULTS),
            num_results: 0,
            pn: Pn::One,
            changed: true,
            pnc: false,
        };
        ret.add_result(st);
        ret.changed = true;
        ret
    }

    /// Return the number of results in the store.
    pub fn len(&self) -> usize {
        self.astore.len()
    }

    /// Add a result to a circular buffer.
    /// Return true if the pattern number or pnc changed
    /// or Pn::One with two results, which indicates "cannot be Pn::Two"
    pub fn add_result(&mut self, st: SomeState) -> bool {
        self.changed = false;

        if self.astore.len() < MAX_RESULTS {
            self.astore.push(st);
        } else {
            self.astore[self.num_results % 4] = st;
        }
        self.num_results += 1;

        let pnx = self.calc_pn();

        // Check if pn changed
        if self.pn != pnx {
            self.pn = pnx;
            self.changed = true;
            self.pnc = false;
        }

        // calc, or recalc, pnc
        if self.pnc == false {
            if self.astore.len() == MAX_RESULTS || self.pn == Pn::Unpredictable {
                self.changed = true;
                self.pnc = true;
            }
        }

        // For pn =1, a second sample proves its not pn = 2
        // so it could invalidate a pn=2 group
        if self.pn == Pn::One {
            if self.len() > 1 {
                self.changed = true;
                self.pnc = true;
            }
        }

        // First sample is always a change
        if self.len() == 1 {
            self.changed = true;
        }

        self.changed
    }

    /// Return the first result.
    pub fn first(&self) -> &SomeState {
        &self.astore.get(0).unwrap()
    }

    /// Return the second result.
    pub fn second(&self) -> &SomeState {
        &self.astore.get(1).unwrap()
    }

    /// Return the most recent result.
    pub fn most_recent_result(&self) -> &SomeState {
        &self.astore[(self.num_results - 1) % MAX_RESULTS]
    }

    /// Calculate the Pattern Number.
    pub fn calc_pn(&self) -> Pn {
        let len = self.astore.len();

        // Test pn == 1
        if len == 1 {
            //println!("calc_pn returning pn 1a");
            return Pn::One;
        }

        // Check if all entries == first entry
        let mut flg = true;
        for inx in 1..len {
            if self.astore[inx] != self.astore[0] {
                flg = false;
                break;
            }
        }
        if flg {
            //println!("calc_pn returning pn 1b");
            return Pn::One;
        }

        // pn != 1, Check for pn == 2
        if len > 2 {
            if self.astore[0] == self.astore[2] {
                if len > 3 {
                    if self.astore[1] == self.astore[3] {
                        //println!("calc_pn returning pn 2a");
                        return Pn::Two;
                    }
                } else {
                    //println!("calc_pn returning pn 2b");
                    return Pn::Two;
                }
            }
        } else {
            //println!("calc_pn returning pn 2c");
            return Pn::Two;
        }

        //println!("calc_pn returning pn Unp");
        Pn::Unpredictable
    }

    /// Return the expected length of a string to represent a ResultStore.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        if self.astore.len() > 0 {
            rc_len += self.astore.len() * self.astore[0].formatted_string_length();
            if self.astore.len() > 1 {
                rc_len += (self.astore.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string to represent a ResultStore.
    pub fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::with_capacity(self.formatted_string_length());
        rc_str.push('[');

        for rsltx in self.astore.iter() {

            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &rsltx));
            flg = 1;
        }

        rc_str.push_str(&format!("]"));

        rc_str
    }

} // end impl ResultStore
