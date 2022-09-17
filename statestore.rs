//! The StateStore struct. A vector of SomeState structs.

use crate::state::SomeState;

use std::ops::Index;
use std::slice::Iter;

use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl PartialEq for StateStore {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for stax in self.iter() {
            if !other.contains(stax) {
                return false;
            }
        }
        true
    }
}
impl Eq for StateStore {}

#[derive(Debug, Clone)]
pub struct StateStore {
    /// A vector of states.
    pub avec: Vec<SomeState>,
}

impl Default for StateStore {
    fn default() -> Self {
        Self::new()
    }
}

impl StateStore {
    /// Return a new StateStore instance, empty.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeState>::new(),
        }
    }

    /// Return a new StateStore instance, empty, with a specified capacity.
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
        self.avec.len() == 0
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

        if !self.avec.is_empty() {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string representing a StateStore.
    pub fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::with_capacity(self.formatted_string_length());
        rc_str.push('[');

        for stax in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            let _ = write!(rc_str, "{}", &stax);
            flg = 1;
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
