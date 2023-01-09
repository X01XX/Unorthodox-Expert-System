//! The TargetStore struct, a vector of SomeTarget structs.

use crate::state::SomeState;
use crate::target::SomeTarget;

use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing
use std::ops::Index; // IndexMut
use std::slice::Iter;

impl fmt::Display for TargetStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n[");

        for targx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n ");
            }
            let _ = write!(rc_str, "{}", &targx);
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
#[readonly::make]
pub struct TargetStore {
    /// A vector of SomeTarget instances.
    avec: Vec<SomeTarget>,
}

impl Default for TargetStore {
    fn default() -> Self {
        Self::new()
    }
}

impl TargetStore {
    /// Return a new TargetStore instance.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeTarget>::new(),
        }
    }

    /// Return a new TargetStore instance.
    pub fn new_with_target(target: SomeTarget) -> Self {
        Self { avec: vec![target] }
    }

    /// Return the length of the SomeTarget vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a target to the vector.
    pub fn push(&mut self, val: SomeTarget) {
        self.avec.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeTarget> {
        self.avec.iter()
    }

    /// Return true is a TargetStore is a superset of a StateStore.
    pub fn is_superset_of_states(&self, states: &Vec<&SomeState>) -> bool {
        assert!(self.len() == states.len());

        for (inx, targx) in self.avec.iter().enumerate() {
            if targx.is_superset_of_state(states[inx]) {
                continue;
            }
            return false;
        }
        true
    }

    /// Return true if a one regions TargetStore is a superset of a given state.
    pub fn is_superset_of_state(&self, state: &SomeState) -> bool {
        assert!(self.len() == 1);
        self.avec[0].is_superset_of_state(state)
    }
} // end impl TargetStore

impl Index<usize> for TargetStore {
    type Output = SomeTarget;
    fn index(&self, i: usize) -> &SomeTarget {
        &self.avec[i]
    }
}
