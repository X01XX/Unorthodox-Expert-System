//! The StepStore struct.  A vector of SomeStep structs.

use crate::change::SomeChange;
use crate::step::SomeStep;

use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for StepStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string(""))
    }
}

#[derive(Debug)]
pub struct StepStore {
    /// A vector for steps.
    pub avec: Vec<SomeStep>,
}

impl StepStore {
    /// Return a new StepStore, empty.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeStep>::with_capacity(5),
        }
    }

    /// Return a new StepStore, empty, with an expected capacity.
    pub fn new_with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeStep>::with_capacity(num),
        }
    }

    /// Return the number of steps in a StepStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a step to a StepStore.
    pub fn push(&mut self, val: SomeStep) {
        self.avec.push(val);
    }

    /// Append a StepStore to a StepStore.
    pub fn append(&mut self, mut val: StepStore) {
        self.avec.append(&mut val.avec); // empties val.avec
    }

    /// Return an immutable iterator for a StepStore.
    pub fn iter(&self) -> Iter<SomeStep> {
        self.avec.iter()
    }

    //    pub fn reverse(&self) -> Self {
    //		let mut rc_steps = StepStore { avec:  Vec::<SomeStep>::with_capacity(self.len()) };
    //
    //        for inx in (0..self.len()).rev() {
    //			rc_steps.add(self.avec[inx]);
    //		}
    //
    //		rc_steps
    //	}

    /// Reverse the order of steps in a StepStore.
    pub fn reverse(&mut self) {
        self.avec.reverse();
    }

    /// Return a vector with indices of two steps with the same initial region,
    /// else return an empty vector.
    pub fn same_intitial(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::new();

        let mut x = 0;
        for stpx in self.avec.iter() {
            for y in (x + 1)..self.len() {
                if stpx.initial == self.avec[y].initial {
                    ret_vec.push(x);
                    ret_vec.push(y);
                    return ret_vec;
                }
            }

            x = x + 1;
        }

        ret_vec
    }

    /// Return the expected length of a string representing a StepStore.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        if self.avec.len() > 0 {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string representing a StepStore.
    pub fn formatted_string(&self, prefix: &str) -> String {
        let mut flg = 0;
        let mut rc_str = String::with_capacity(prefix.len() + self.formatted_string_length());
        rc_str.push_str(prefix);
        rc_str.push('[');

        for stpx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &stpx));
            flg = 1;
        }
        rc_str.push(']');

        rc_str
    }

    /// Return a list of indicies representing change-step vectors that must be used after others.
    /// Those change-steps can be ignored for the question of "Whats the next step?"
    /// This can be less complex, and more definitive, when there is only one step in a change-step vector.
    /// A step that makes more than one needed change can appear in two change-step vectors.  In that case
    /// the two change-step vectors cannot require a order.
    pub fn order_next_changes(
        &self,
        steps_by_change: &Vec<Vec<usize>>,
        wanted: &SomeChange,
    ) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::new();

        for x in 0..steps_by_change.len() {
            let changex = &steps_by_change[x];

            for y in (x + 1)..steps_by_change.len() {
                let changey = &steps_by_change[y];

                let mut ord_ex1 = true;
                let mut ord_ex2 = true;

                for stepx in changex.iter() {
                    for stepy in changey.iter() {
                        if stepx == stepy {
                            ord_ex1 = false;
                            ord_ex2 = false;
                        } else {
                            if self[*stepx].rule.order_ok(&self[*stepy].rule, wanted) {
                                ord_ex1 = false;
                            }
                            if self[*stepy].rule.order_ok(&self[*stepx].rule, wanted) {
                                ord_ex2 = false;
                            }
                        }
                    } // next step y
                } // next stepx

                if ord_ex1 && ord_ex2 == false {
                    if ret_vec.contains(&x) == false {
                        ret_vec.push(x);
                    }
                }
                if ord_ex2 && ord_ex1 == false {
                    if ret_vec.contains(&y) == false {
                        ret_vec.push(y);
                    }
                }
            } // next y
        } // next x

        //println!("*** order_next_changes: returns {:?}", ret_vec);
        ret_vec
    }
} // end impl StepStore

impl Index<usize> for StepStore {
    type Output = SomeStep;
    fn index<'a>(&'a self, i: usize) -> &'a SomeStep {
        &self.avec[i]
    }
}

impl Clone for StepStore {
    fn clone(&self) -> Self {
        let mut rcstp = Self::new_with_capacity(self.len());
        for stpx in self.avec.iter() {
            rcstp.push(stpx.clone());
        }
        rcstp
    }
}
