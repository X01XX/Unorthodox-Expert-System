//! The ResultStore struct.  Saves up to 4 results of an action for a single state.
//!
//! Uses a VecDeque to allow removal of the oldest sample when adding a new sample when 4 are already loaded.

use crate::pn::Pn;
use crate::state::SomeState;

pub const MAX_RESULTS: usize = 4; // Seems like the best. It certaily should be even, for two-result squares.

use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fmt;

impl fmt::Display for ResultStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ResultStore {
    /// A VecDeque to store sample results for one domain/action/state, that is a square.
    pub astore: VecDeque<SomeState>,
    /// Pattern number, One, Two or Unpredicatble, trips change indicator when changed.
    pub pn: Pn,
    /// When first sample, pn or pnc has changed.
    pub changed: bool,
    /// Pattern Number Confirmed, i.e num results stored has reached MAX_RESULTS.
    pub pnc: bool,
}

impl ResultStore {
    /// Return a new ResultStore, with an initial result.
    pub fn new(st: SomeState) -> Self {
        let mut ret = Self {
            astore: VecDeque::<SomeState>::with_capacity(MAX_RESULTS),
            pn: Pn::One,
            changed: true,
            pnc: false,
        };
        ret.push_wrap(st);
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
    pub fn push_wrap(&mut self, st: SomeState) -> bool {
        self.changed = false;

        if self.astore.len() >= MAX_RESULTS {
            self.astore.pop_front();
        }

        self.astore.push_back(st);

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
        if self.pn == Pn::One && self.len() == 2 {
            self.changed = true;
        }

        // First sample is always a change
        if self.len() == 1 {
            self.changed = true;
        }

        self.changed
    }

    /// Return the first result.
    pub fn first(&self) -> &SomeState {
        &self.astore[0]
    }

    /// Return the second result.
    pub fn second(&self) -> &SomeState {
        &self.astore[1]
    }

    /// Return the last result.
    pub fn last_result(&self) -> &SomeState {
        &self.astore[self.astore.len() - 1]
    }

    /// Calculate the Pattern Number.
    pub fn calc_pn(&self) -> Pn {
        let num = self.astore.len();

        // Test pn == 1
        if num == 1 {
            //println!("calc_pn returning pn 1a");
            return Pn::One;
        }

        // Check if all entries == first entry
        let mut flg = true;
        for inx in 1..num {
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
        if num > 2 {
            if self.astore[0] == self.astore[2] {
                if num > 3 {
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

        //rc_len += format!("{}", self.num_results).len();

        rc_len
    }

    /// Return a string to represent a ResultStore.
    pub fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::with_capacity(self.formatted_string_length());
        rc_str.push('[');

        for i in 0..self.astore.len() {
            let rsltx = self.astore[i].clone();
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
