//! The ChangeStore struct, a vector of SomeChange structs.
//!
use crate::change::SomeChange;
use crate::tools::StrLen;

use std::fmt;
use std::slice::Iter;

#[readonly::make]
#[derive(Debug, Clone, Default)]
pub struct ChangeStore {
    items: Vec<SomeChange>,
}

impl fmt::Display for ChangeStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

/// ChangeStores are equal if they contain the same changes, order does not matter.
impl PartialEq for ChangeStore {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for cngx in self.iter() {
            if !other.contains(cngx) {
                return false;
            }
        }

        true
    }
}

impl Eq for ChangeStore {}

impl ChangeStore {
    /// Return a new, empty, ChangeStore.
    #[allow(dead_code)]
    pub fn new(items: Vec<SomeChange>) -> Self {
        Self { items }
    }

    /// Return a new ChangeStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SomeChange>::with_capacity(num),
        }
    }

    /// Return true if a ChangeStore contains a change.
    pub fn contains(&self, cng: &SomeChange) -> bool {
        self.items.contains(cng)
    }

    /// Return the length of a ChangeStore.
    /// Should be 0, 1 or 2.
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

    /// Add a change to a ChangeStore.
    /// For changes of a group or square, there will be up to two changes. If two changes, they will have the same initial region.
    /// For a ChangesCorr instance, there will be changes corresponding to the domains in a DomainStore.
    pub fn push(&mut self, val: SomeChange) {
        self.items.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeChange> {
        self.items.iter()
    }

    /// Return a string representing a ChangeStore.
    fn formatted_str(&self) -> String {
        let mut rc_str = String::with_capacity(self.strlen());
        rc_str.push('[');

        let mut first = true;
        for cngx in self.items.iter() {
            if first {
                first = false;
            } else {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&cngx.to_string());
        }
        rc_str.push(']');

        rc_str
    }
}

/// Implement the trait StrLen for ChangeStore.
impl StrLen for ChangeStore {
    fn strlen(&self) -> usize {
        let mut ret = 2; // [..]
        let mut first = true;
        for cngx in self.items.iter() {
            if first {
                first = false;
            } else {
                ret += 2; // for ", "
            }
            ret += cngx.strlen();
        }
        ret
    }
}
