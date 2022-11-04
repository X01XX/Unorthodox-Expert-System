//! The StepStore struct.  A vector of SomeStep structs.

use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::step::SomeStep;

use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for StepStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string(""))
    }
}

#[readonly::make]
#[derive(Debug, Clone)]
pub struct StepStore {
    /// A vector for steps.
    pub avec: Vec<SomeStep>,
}

impl Default for StepStore {
    fn default() -> Self {
        Self::new()
    }
}

impl StepStore {
    /// Return a new StepStore, empty.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeStep>::with_capacity(5),
        }
    }

    /// Return a new StepStore with a step
    pub fn new_with_step(astep: SomeStep) -> Self {
        let mut vecx = Vec::<SomeStep>::with_capacity(2);
        vecx.push(astep);
        Self { avec: vecx }
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

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.len() == 0
    }

    /// Add a step to a StepStore.
    pub fn push(&mut self, val: SomeStep) {
        self.avec.push(val);
    }

    /// Append a StepStore to a StepStore.
    pub fn append(&mut self, val: &mut StepStore) {
        self.avec.append(&mut val.avec); // empties val.avec
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

        if !self.avec.is_empty() {
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
            let _ = write!(rc_str, "{}", &stpx);
            flg = 1;
        }
        rc_str.push(']');

        rc_str
    }

    /// Given a number of steps, and a required change, return a vector of vectors
    /// where the sub-vectors indicate a single bit change that is required.
    /// Note that a step that changes more than one bit may end up in more than one sub-vector.
    pub fn steps_by_change_bit(&self, required_change: &SomeChange) -> Vec<Vec<&SomeStep>> {
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
        for stepx in self.avec.iter() {
            // Check for matching b01 changes
            for (inx, b01x) in b01.iter().enumerate() {
                if stepx.rule.b01.bits_and(b01x).is_not_low() {
                    ret_vec[inx].push(stepx);
                }
            } // next b01x

            // Check for matching b10 changes
            for (inx, b10x) in b10.iter().enumerate() {
                if stepx.rule.b10.bits_and(b10x).is_not_low() {
                    ret_vec[inx + b01_len].push(stepx);
                }
            } // next b01x
        } // next stepx

        ret_vec
    } // end steps_by_change_bit

    // Return num_ints used in stepstore
    fn num_ints(&self) -> Option<usize> {
        if self.is_empty() {
            return None;
        }
        Some(self.avec[0].num_ints())
    }

    // Return aggregate changes
    pub fn aggregate_changes(&self) -> Option<SomeChange> {
        if let Some(num_ints) = self.num_ints() {
            let mut schg = SomeChange::new_low(num_ints);
            for stpx in &self.avec {
                schg = schg.c_or(&stpx.rule.change());
            }
            return Some(schg);
        }
        None
    }
} // end impl StepStore

impl Index<usize> for StepStore {
    type Output = SomeStep;
    fn index(&self, i: usize) -> &SomeStep {
        &self.avec[i]
    }
}
