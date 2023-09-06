//! The StateStore struct. A vector of SomeState structs.

use crate::state::SomeState;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;

use std::fmt;

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Deserialize, Serialize, Debug, Clone, Default)]
pub struct StateStore {
    /// A vector of states.
    avec: Vec<SomeState>,
}

impl StateStore {
    /// Return a new, empty, StateStore instance.
    pub fn new(avec: Vec<SomeState>) -> Self {
        Self { avec }
    }

    /// Return a new, empty, StateStore instance, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeState>::with_capacity(num),
        }
    }

    /// Return the number of states in a StateStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a state to a StateStore.
    pub fn push(&mut self, val: SomeState) {
        self.avec.push(val);
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Return an immuable iterator.
    pub fn iter(&self) -> Iter<SomeState> {
        self.avec.iter()
    }

    /// Return true if a StateStore contains a given state.
    pub fn contains(&self, stax: &SomeState) -> bool {
        self.avec.contains(stax)
    }

    /// Return the expected length of a string representing a StateStore.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        if self.is_not_empty() {
            rc_len += self.avec.len() * self.avec[0].strlen();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string representing a StateStore.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::with_capacity(self.formatted_string_length());
        rc_str.push('[');

        for (inx, stax) in self.avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &stax));
        }

        rc_str.push(']');

        rc_str
    }
} // end impl StateStore

impl Index<usize> for StateStore {
    type Output = SomeState;
    fn index(&self, i: usize) -> &SomeState {
        &self.avec[i]
    }
}
