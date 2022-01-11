//! The StateStore struct. A vector of SomeState structs.

use crate::state::SomeState;
//use crate::region::SomeRegion;
//use crate::removeunordered::remove_unordered;

use std::ops::Index;
use std::slice::Iter;

use std::fmt;

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
            if other.contains(stax) == false {
                return false;
            }
        }
        true
    }
}
impl Eq for StateStore {}

#[derive(Debug)]
pub struct StateStore {
    /// A vector of states.
    avec: Vec<SomeState>,
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

        if self.avec.len() > 0 {
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
            rc_str.push_str(&format!("{}", &stax));
            flg = 1;
        }

        rc_str.push(']');

        rc_str
    }

    // Return a region representing the union of states in a StateStore.
//    pub fn region(&self) -> Option<SomeRegion> {
//        if self.len() == 0 {
//            return None;
//        }
//        let mut ret_reg = SomeRegion::new(&self[0], &self[0]);
//
//        for stax in &self.avec {
//            if ret_reg.is_superset_of_state(stax) {
//            } else {
//                ret_reg = ret_reg.union_state(stax);
//            }
//        }
//
//        Some(ret_reg)
//    }

    // Remove a state from a StateStore
//    pub fn remove_unordered(&mut self, other: &SomeState) {
//        let mut inx = 0;
//        for stax in self.avec.iter() {
//            if stax == other {
//                break;
//            }
//            inx += 1;
//        }
//        remove_unordered(&mut self.avec, inx);
//    }

} // end impl StateStore

impl Index<usize> for StateStore {
    type Output = SomeState;
    fn index<'a>(&'a self, i: usize) -> &'a SomeState {
        &self.avec[i]
    }
}

impl Clone for StateStore {
    fn clone(&self) -> Self {
        let mut rcstp = Self::with_capacity(self.len());
        for stax in self.avec.iter() {
            rcstp.push(stax.clone());
        }
        rcstp
    }
}
