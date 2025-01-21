//! The ChangesCorr struct, a store of SomeChanges, corresponding in order, to domains in a DomainStore instance.
//!
//! Each change will have a number of bits equal to the bits used by the corresponding
//! domain, not necessarily the same as other changes in the vector.

use crate::change::SomeChange;
use crate::changestore::ChangeStore;
use crate::tools::StrLen;

use std::fmt;
use std::slice::Iter;

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
    pub fn len(&self) -> usize {
        self.changes.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.changes.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.changes.is_empty()
    }

    /// Add a change to the change store.
    pub fn push(&mut self, rulx: SomeChange) {
        self.changes.push(rulx);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeChange> {
        self.changes.iter()
    }

    /// Return true if all changes are low.
    pub fn is_low(&self) -> bool {
        for cngx in self.changes.iter() {
            if cngx.is_not_low() {
                return false;
            }
        }
        true
    }

    /// Return true if any changes are not low.
    pub fn is_not_low(&self) -> bool {
        !self.is_low()
    }

    /// Return the intersection of two ChangesCorr.
    pub fn intersection(&self, other: &Self) -> Self {
        let mut ret = Self::with_capacity(self.len());
        for (cngx, cngy) in self.iter().zip(other.iter()) {
            ret.push(cngx.intersection(cngy));
        }
        ret
    }
}

/// Implement the trait StrLen for Changescorr.
impl StrLen for ChangesCorr {
    fn strlen(&self) -> usize {
        let mut rc_len = 2; // for "CC"

        rc_len += self.changes.strlen();

        rc_len
    }
}
