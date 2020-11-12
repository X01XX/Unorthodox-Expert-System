// Implement a VecDeque for result states for a square

use crate::pn::Pn;
use crate::state::SomeState;

const MAX_RESULTS: usize = 4;

use std::collections::VecDeque;
use std::fmt;

impl fmt::Display for ResultStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::new();

        for i in 0..self.astore.len() {
            let rsltx = self.astore[i].clone();
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &rsltx));
            flg = 1;
        }

        rc_str.push_str(&format!(", num_rslts: {}", self.num_results));

        write!(f, "{}", rc_str)
    }
}

pub struct ResultStore {
    astore: VecDeque<SomeState>,
    pub pn: Pn, // Pattern number, 1, 2, 3 (unpredicatble), trips change indicatore when changed.
    pub changed: bool, // First sample, pn or pnc has changed
    pub pnc: bool, // Pattern Number Confirmed, i.e num results stored has reached 4
    pub num_results: usize, // Total number of results
}

impl ResultStore {
    pub fn new(st: SomeState) -> Self {
        let mut ret = Self {
            astore: VecDeque::<SomeState>::with_capacity(MAX_RESULTS),
            pn: Pn::One,
            changed: true,
            pnc: false,
            num_results: 0,
        };
        ret.push_wrap(st);
        ret.changed = true;
        ret
    }

    // Add a result to a circular buffer
    // Return true if the pattern number or pnc changed
    // or pn=1 with two results, signals "cannot be pn=2"
    pub fn push_wrap(&mut self, st: SomeState) -> bool {
        self.changed = false;

        if self.astore.len() >= MAX_RESULTS {
            self.astore.pop_front();
        }

        self.astore.push_back(st);

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
            if self.pn == Pn::One || self.pn == Pn::Unpredictable {
                if self.astore.len() >= 3 {
                    self.changed = true;
                    self.pnc = true;
                }
            } else if self.pn == Pn::Two {
                if self.astore.len() >= 4 {
                    self.changed = true;
                    self.pnc = true;
                }
            }
        }

        // For pn =1, a second sample proves its not pn = 2
        // so it could invalidate a pn=2 group
        if self.pn == Pn::One && self.num_results == 2 {
            self.changed = true;
        }

        // First sample is always a change
        if self.num_results == 1 {
            self.changed = true;
        }

        self.changed
    }

    pub fn first(&self) -> &SomeState {
        &self.astore[0]
    }

    pub fn second(&self) -> &SomeState {
        &self.astore[1]
    }

    pub fn len(&self) -> usize {
        self.astore.len()
    }

    // Calculate the Pattern Number
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

        // pn != 2
        //println!("calc_pn returning pn Unp");
        Pn::Unpredictable
    }
}
