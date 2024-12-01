//! The StepStore struct.  A vector of SomeStep structs.

use crate::bits::vec_same_num_bits;
use crate::change::SomeChange;
use crate::step::SomeStep;
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::{Iter, IterMut};

impl fmt::Display for StepStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str(""))
    }
}

#[readonly::make]
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
pub struct StepStore {
    /// A vector for steps.
    pub items: Vec<SomeStep>,
}

impl StepStore {
    /// Return a new, empty, StepStore.
    pub fn new(items: Vec<SomeStep>) -> Self {
        debug_assert!(vec_same_num_bits(&items));

        Self { items }
    }

    /// Return a new, empty, StepStore, with an expected capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SomeStep>::with_capacity(num),
        }
    }

    /// Return the number of steps in a StepStore.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Add a step to a StepStore.
    pub fn push(&mut self, val: SomeStep) {
        debug_assert!(if let Some(num_bits) = self.num_bits() {
            num_bits == val.num_bits()
        } else {
            true
        });

        self.items.push(val);
    }

    /// Extend a StepStore by emptying another StepStore.
    pub fn append(&mut self, mut other: Self) {
        debug_assert!(self.is_empty() || other.is_empty() || self.num_bits() == other.num_bits());

        self.items.append(&mut other.items);
    }

    /// Return an immutable iterator for a StepStore.
    pub fn iter(&self) -> Iter<SomeStep> {
        self.items.iter()
    }

    /// Return an immutable iterator for a StepStore.
    pub fn iter_mut(&mut self) -> IterMut<SomeStep> {
        self.items.iter_mut()
    }

    /// Reverse the order of steps in a StepStore.
    pub fn reverse_order(&mut self) {
        self.items.reverse();
    }

    /// Return a string representing a StepStore.
    fn formatted_str(&self, prefix: &str) -> String {
        let mut rc_str = String::with_capacity(prefix.len() + self.strlen());
        rc_str.push_str(prefix);
        rc_str.push('[');

        for (inx, stpx) in self.items.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&stpx.to_string());
        }
        rc_str.push(']');

        rc_str
    }

    /// Given a number of steps, and a required change, return a vector of vectors
    /// where the sub-vectors indicate a single bit change that is required.
    pub fn split_steps_by_bit_change(&self, required_change: &SomeChange) -> Vec<Vec<&SomeStep>> {
        //println!("stepstore::split_steps_by_bit_change: steps {self} change {required_change}");
        debug_assert!(if let Some(num_bits) = self.num_bits() {
            num_bits == required_change.num_bits()
        } else {
            true
        });

        let m01 = required_change.m01.split();

        let m10 = required_change.m10.split();

        let m01_len = m01.len();
        let m10_len = m10.len();
        let tot_len = m01_len + m10_len;

        let mut ret_vec = Vec::<Vec<&SomeStep>>::with_capacity(tot_len);

        // Populate ret-vec with empty vectors
        // Some will end up with only one number, indicating a rule that must be used.
        // Some will end up with more than one number, indicating a range from witch one must be picked.
        // Note that a rule might change more than one bit, so it may appear in more than one ret-vec sub-vectors.
        for _ in 0..tot_len {
            ret_vec.push(Vec::<&SomeStep>::new());
        }

        // Add step index numbers to the return vector.
        for stepx in &self.items {
            let edge_mask = stepx.initial.edge_mask();

            // Check for matching m01 changes
            for (inx, m01x) in m01.iter().enumerate() {
                if stepx.rule.m01.bitwise_and(m01x).is_not_low()
                    && m01x.bitwise_and(&edge_mask).is_not_low()
                {
                    ret_vec[inx].push(stepx);
                }
            } // next m01x

            // Check for matching m10 changes
            for (inx, m10x) in m10.iter().enumerate() {
                if stepx.rule.m10.bitwise_and(m10x).is_not_low()
                    && m10x.bitwise_and(&edge_mask).is_not_low()
                {
                    ret_vec[inx + m01_len].push(stepx);
                }
            } // next m01x
        } // next stepx

        ret_vec
    } // end steps_by_change_bit

    /// Return aggregate changes
    pub fn aggregate_changes(&self) -> Option<SomeChange> {
        if self.is_empty() {
            return None;
        }

        let tmp_mask = self[0].initial.first_state().new_low().as_mask();
        let mut schg = SomeChange::new(tmp_mask.clone(), tmp_mask);
        for stpx in &self.items {
            schg = schg.union(&stpx.rule);
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
    ) -> Result<Vec<Vec<&SomeStep>>, String> {
        //println!("StepStore::get_steps_by_bit_change: steps {self} change {required_change}");
        debug_assert!(if let Some(num_bits) = self.num_bits() {
            num_bits == required_change.num_bits()
        } else {
            true
        });

        // Sort the steps by each needed bit change. (some actions may change more than one bit, so will be in more than one subvector)
        let mut steps_by_change_vov: Vec<Vec<&SomeStep>> =
            self.split_steps_by_bit_change(required_change);

        for avec in steps_by_change_vov.iter() {
            if avec.is_empty() {
                return Err(format!(
                    "stepstore::get_steps_by_bit_change: No steps found for {required_change}"
                ));
            }
        }

        // These may be low-level rules, but at least they have some sense of where they are going!

        // Check if any pair of single-bit change, all steps in vectors, are mutually exclusive.
        // So one change can be made, but not the other.
        if any_mutually_exclusive_changes(&steps_by_change_vov, required_change) {
            return Err(
                "stepstore::get_steps_by_bit_change: Mutually exclusive steps found".to_string(),
            );
        }

        // Check for step vectors where all steps should be done after all steps in at least one other step vector,
        // to void backtracking/loops.
        if steps_by_change_vov.len() > 1 {
            let inxs: Vec<usize> = do_later_changes(&steps_by_change_vov, required_change);
            if inxs.len() == steps_by_change_vov.len() {
                return Err("stepstore::get_steps_by_bit_change: ?".to_string());
            }
            for inx in inxs.iter() {
                tools::remove_unordered(&mut steps_by_change_vov, *inx);
            }
        }

        Ok(steps_by_change_vov)
    }

    /// Return the number of bits used be steps in the StepStore.
    pub fn num_bits(&self) -> Option<usize> {
        if self.is_empty() {
            return None;
        }
        Some(self.items[0].num_bits())
    }
} // end impl StepStore

/// Return true if any single-bit change step vector pairs are all mutually exclusive
fn any_mutually_exclusive_changes(by_change: &[Vec<&SomeStep>], wanted: &SomeChange) -> bool {
    debug_assert!(wanted.is_not_low());
    for inx in 0..(by_change.len() - 1) {
        for iny in (inx + 1)..by_change.len() {
            //println!("any_mutually_exclusive_changes checking {:?} and {:?}", by_change[inx], by_change[iny]);
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
    debug_assert!(!vec_x.is_empty() && !vec_y.is_empty());
    for refx in vec_x.iter() {
        for refy in vec_y.iter() {
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
fn do_later_changes(by_change: &[Vec<&SomeStep>], wanted: &SomeChange) -> Vec<usize> {
    let mut inxs = Vec::<usize>::new();

    // Generate a vector of indices of changes that should be done later.
    for inx in 0..by_change.len() {
        for iny in 0..by_change.len() {
            if iny == inx {
                continue;
            }
            if step_vecs_sequence_blocks_changes(&by_change[inx], &by_change[iny], wanted)
                && !inxs.contains(&inx)
            {
                inxs.push(inx);
            }
            if step_vecs_sequence_blocks_changes(&by_change[iny], &by_change[inx], wanted)
                && !inxs.contains(&iny)
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

/// Return true if the order of all steps in step vector arg one will
/// fail to pass through all wanted changes.
fn step_vecs_sequence_blocks_changes(
    vec_x: &[&SomeStep],
    vec_y: &[&SomeStep],
    wanted: &SomeChange,
) -> bool {
    assert!(!vec_x.is_empty());
    assert!(!vec_y.is_empty());
    for refx in vec_x.iter() {
        for refy in vec_y.iter() {
            if !refx.sequence_blocks_changes(refy, wanted) {
                return false;
            }
        } //next refy
    } // next refx

    true
}

impl Index<usize> for StepStore {
    type Output = SomeStep;
    fn index(&self, i: usize) -> &SomeStep {
        &self.items[i]
    }
}

impl IntoIterator for StepStore {
    type Item = SomeStep;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

/// Implement the trait StrLen for StepStore.
impl StrLen for StepStore {
    fn strlen(&self) -> usize {
        let mut rc_len = 2;

        for (inx, itemx) in self.items.iter().enumerate() {
            if inx > 0 {
                rc_len += 2; // for ", "
            }
            rc_len += itemx.strlen();
        }

        rc_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rule::SomeRule;
    use crate::sample::SomeSample;
    use crate::step::{AltRuleHint, SomeStep};

    #[test]
    fn strlen() -> Result<(), String> {
        let tmp_rul = SomeRule::new(&SomeSample::from_str("s0000_0000->s0000_0000")?);
        let tmp_stp = SomeStep::new(0, tmp_rul, AltRuleHint::NoAlt {});

        let mut tmp_stpst = StepStore::new(vec![tmp_stp.clone()]);

        let strrep = format!("{tmp_stpst}");
        let len = strrep.len();
        let calc_len = tmp_stpst.strlen();
        println!("str {tmp_stpst} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        tmp_stpst.push(tmp_stp);
        let strrep = format!("{tmp_stpst}");
        let len = strrep.len();
        let calc_len = tmp_stpst.strlen();
        println!("str {tmp_stpst} len {len} calculated len {calc_len}");
        assert!(len == calc_len);

        Ok(())
    }
}
