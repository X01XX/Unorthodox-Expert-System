//! The StepStore struct.  A vector of SomeStep structs.

use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::removeunordered;
use crate::step::SomeStep;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for StepStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string(""))
    }
}

//#[readonly::make]
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct StepStore {
    /// A vector for steps.
    pub avec: Vec<SomeStep>,
}

impl StepStore {
    /// Return a new, empty, StepStore.
    pub fn new(avec: Vec<SomeStep>) -> Self {
        Self { avec }
    }

    /// Return a new, empty, StepStore, with an expected capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeStep>::with_capacity(num),
        }
    }

    /// Return the number of steps in a StepStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Add a step to a StepStore.
    pub fn push(&mut self, val: SomeStep) {
        self.avec.push(val);
    }

    /// Append a StepStore to a StepStore.
    pub fn append(&mut self, other: &mut StepStore) {
        self.avec.append(&mut other.avec); // empties other.avec
    }

    /// Return an immutable iterator for a StepStore.
    pub fn iter(&self) -> Iter<SomeStep> {
        self.avec.iter()
    }

    /// Reverse the order of steps in a StepStore.
    pub fn reverse(&mut self) {
        self.avec.reverse();
    }

    /// Return the expected length of a string representing a StepStore.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        if self.is_not_empty() {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string representing a StepStore.
    pub fn formatted_string(&self, prefix: &str) -> String {
        let mut rc_str = String::with_capacity(prefix.len() + self.formatted_string_length());
        rc_str.push_str(prefix);
        rc_str.push('[');

        for (inx, stpx) in self.avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &stpx));
        }
        rc_str.push(']');

        rc_str
    }

    /// Given a number of steps, and a required change, return a vector of vectors
    /// where the sub-vectors indicate a single bit change that is required.
    pub fn split_steps_by_bit_change(&self, required_change: &SomeChange) -> Vec<Vec<&SomeStep>> {
        let mut b01 = Vec::<SomeMask>::new();

        if required_change.b01.is_not_low() {
            b01 = required_change.b01.split();
        }

        let mut b10 = Vec::<SomeMask>::new();

        if required_change.b10.is_not_low() {
            b10 = required_change.b10.split();
        }

        let b01_len = b01.len();
        let b10_len = b10.len();
        let tot_len = b01_len + b10_len;

        let mut ret_vec = Vec::<Vec<&SomeStep>>::with_capacity(tot_len);

        // Populate ret-vec with empty vectors
        // Some will end up with only one number, indicating a rule that must be used.
        // Some will end up with more than one number, indicating a range from witch one must be picked.
        // Note that a rule might change more than one bit, so it may appear in more than one ret-vec sub-vectors.
        for _ in 0..tot_len {
            ret_vec.push(Vec::<&SomeStep>::new());
        }

        // Add step index numbers to the return vector.
        for stepx in &self.avec {
            // Check for matching b01 changes
            for (inx, b01x) in b01.iter().enumerate() {
                if stepx.rule.b01.bitwise_and(b01x).is_not_low() {
                    ret_vec[inx].push(stepx);
                }
            } // next b01x

            // Check for matching b10 changes
            for (inx, b10x) in b10.iter().enumerate() {
                if stepx.rule.b10.bitwise_and(b10x).is_not_low() {
                    ret_vec[inx + b01_len].push(stepx);
                }
            } // next b01x
        } // next stepx

        ret_vec
    } // end steps_by_change_bit

    /// Return aggregate changes
    pub fn aggregate_changes(&self) -> Option<SomeChange> {
        if self.is_empty() {
            return None;
        }

        let mut schg = SomeChange::new_low(self.avec[0].num_ints());
        for stpx in &self.avec {
            schg = schg.bitwise_or_rule(&stpx.rule);
        }

        Some(schg)
    }

    /// Given steps are split up into a vector of step vectors, each step vector has steps that change a
    /// particular needed-change bit.  Often, there will only be one step in a step vector.
    ///
    /// Steps that make more than one needed bit change will be in multiple step vectors.
    ///
    /// All possible step vector pairs are checked for being mutually exclusive, that is the needed change
    /// needs to be backed off to run any step in the other step vector.  If any such pair is found, return None.
    ///
    /// Any pair of vectors that have the same step, due to multiple bit changes, will not be construed to be
    /// mutually exclusive.
    ///
    pub fn get_steps_by_bit_change(
        &self,
        required_change: &SomeChange,
    ) -> Option<Vec<Vec<&SomeStep>>> {
        // Sort the steps by each needed bit change. (some actions may change more than one bit, so will be in more than one subvector)
        let mut steps_by_change_vov: Vec<Vec<&SomeStep>> =
            self.split_steps_by_bit_change(required_change);

        // These may be low-level rules, but at least they have some sense of where they are going!

        // Check if any pair of single-bit change, all steps in vectors, are mutually exclusive.
        if any_mutually_exclusive_changes(&steps_by_change_vov, required_change) {
            //println!("get_steps_by_bit_change: mutually exclusive change rules found");
            return None;
        }

        // Check for step vectors where all steps should be done after all steps in at least one other step vector,
        // to void backtracking/loops.
        if steps_by_change_vov.len() > 1 {
            let inxs: Vec<usize> = do_later_changes(&steps_by_change_vov, required_change);
            if inxs.len() == steps_by_change_vov.len() {
                return None;
            }
            for inx in inxs.iter() {
                removeunordered::remove_unordered(&mut steps_by_change_vov, *inx);
            }
        }

        Some(steps_by_change_vov)
    }
} // end impl StepStore

/// Return true if any single-bit change step vector pairs are all mutually exclusive
fn any_mutually_exclusive_changes(by_change: &Vec<Vec<&SomeStep>>, wanted: &SomeChange) -> bool {
    for inx in 0..(by_change.len() - 1) {
        for iny in (inx + 1)..by_change.len() {
            //println!("any_mutually_exclusive_changes checking {:?} and {:?}", &by_change[inx], &by_change[iny]);
            if all_mutually_exclusive_changes(&by_change[inx], &by_change[iny], wanted) {
                return true;
            }
        } // next iny
    } // next inx

    false
}

/// Return true if all combinations of steps, in two step vectors, are mutually exclusive.
fn all_mutually_exclusive_changes(
    vec_x: &[&SomeStep],
    vec_y: &[&SomeStep],
    wanted: &SomeChange,
) -> bool {
    for refx in vec_x.iter() {
        for refy in vec_y.iter() {
            if std::ptr::eq(refx, refy) {
                return false;
            }
            if refx.mutually_exclusive(refy, wanted) {
                //println!("all_mutually_exclusive_changes step {} mutually exclusive to step {}", refx, refy);
            } else {
                return false;
            }
        } //next refy
    } // next refx
    true
}

/// Return a vector of descending indices, of step vectors that should be done later
/// than at least one other step vector.
fn do_later_changes(by_change: &Vec<Vec<&SomeStep>>, wanted: &SomeChange) -> Vec<usize> {
    let mut inxs = Vec::<usize>::new();

    // Generate a vector of indices of changes that should be done later.
    for inx in 0..by_change.len() {
        for iny in 0..by_change.len() {
            if iny == inx {
                continue;
            }
            if step_vecs_order_bad(&by_change[inx], &by_change[iny], wanted) && !inxs.contains(&inx)
            {
                inxs.push(inx);
            }
            if step_vecs_order_bad(&by_change[iny], &by_change[inx], wanted) && !inxs.contains(&iny)
            {
                inxs.push(iny);
            }
        } // next iny
    } //next inx

    // Return a vector of indices in descending order.
    if inxs.len() > 1 {
        inxs.sort_by(|a, b| b.cmp(a));
    }
    inxs
}

/// Return true if the order of all steps in step vector arg one will reuire a
/// wanted bit change to be reversed in order to do any step in step vector arg two.
fn step_vecs_order_bad(
    vec_x: &Vec<&SomeStep>,
    vec_y: &Vec<&SomeStep>,
    wanted: &SomeChange,
) -> bool {
    assert!(!vec_x.is_empty());
    assert!(!vec_y.is_empty());
    for refx in vec_x.iter() {
        for refy in vec_y.iter() {
            if std::ptr::eq(refx, refy) {
                return false;
            }
            if !refx.rule.order_bad(&refy.rule, wanted) {
                return false;
            }
        } //next refy
    } // next refx

    true
}

impl Index<usize> for StepStore {
    type Output = SomeStep;
    fn index(&self, i: usize) -> &SomeStep {
        &self.avec[i]
    }
}
