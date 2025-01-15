//! The ChangesCorr struct, a store of SomeChanges, correspanding in order, to domains in a DomainStore instance.
//!
//! Each change will have a number of bits equal to the bits used by the corresponding
//! domain, not necessarily the same as other changes in the vector.

use crate::change::SomeChange;
use crate::changestore::ChangeStore;

use std::slice::Iter;
use std::fmt;

impl fmt::Display for ChangesCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "CC[]")
        } else {
            write!(f, "CC{}", self.changes)
        }
    }   
}

#[readonly::make]
#[derive(Debug, Clone)]
/// A vector of changes, corresponding to domains in a vector.
pub struct ChangesCorr {
    pub changes: ChangeStore,
}

impl ChangesCorr {
    /// Return a new ChangesCorr instance, empty, with a specified capacity.
    pub fn with_capacity(cap: usize) -> Self {
        debug_assert!(cap > 0);
        Self {
            changes: ChangeStore::with_capacity(cap),
        }
    }

    /// Return the number of changes.
    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.changes.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.changes.is_empty()
    }

    /// Return true if the store is not empty.
    #[allow(dead_code)]
    pub fn is_not_empty(&self) -> bool {
        !self.changes.is_empty()
    }

    /// Add a change to the change store.
    pub fn push(&mut self, rulx: SomeChange) {
        self.changes.push(rulx);
    }

    /// Return a vector iterator.
    #[allow(dead_code)]
    pub fn iter(&self) -> Iter<SomeChange> {
        self.changes.iter()
    }

}

