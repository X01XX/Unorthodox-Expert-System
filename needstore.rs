//! The NeedStore struct, a vector of SomeNeed structs.

use crate::need::SomeNeed;
use crate::target::ATarget;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::{Iter, IterMut};

impl fmt::Display for NeedStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

#[readonly::make]
#[derive(Debug, Serialize, Deserialize)]
/// A vector of SomeNeed structs, and SomeNeed-specific funtions.
pub struct NeedStore {
    /// A vector of SomeNeed struct instances.
    pub items: Vec<SomeNeed>,
}

impl NeedStore {
    /// Return a new NeedStore instance.
    pub fn new(items: Vec<SomeNeed>) -> Self {
        Self { items }
    }

    /// Return a new NeedStore with a given capacity.
    pub fn with_capacity(size: usize) -> Self {
        Self {
            items: Vec::<SomeNeed>::with_capacity(size),
        }
    }

    /// Return the length of the SomeNeed vector.
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

    /// Add a need to the vector.
    pub fn push(&mut self, val: SomeNeed) {
        self.items.push(val);
    }

    /// Extend a NeedStore by emptying another NeedStore..
    pub fn append(&mut self, mut other: Self) {
        self.items.append(&mut other.items);
    }

    /// Return a mutable iterator.
    pub fn iter_mut(&mut self) -> IterMut<SomeNeed> {
        self.items.iter_mut()
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeNeed> {
        self.items.iter()
    }

    /// Sort needs by priority.
    pub fn sort_by_priority(&mut self) {
        self.items.sort_by_key(|ndx| ndx.priority());
    }

    /// Return a String representation of a NeedStore.
    fn formatted_str(&self) -> String {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n[");

        for needx in &self.items {
            if flg == 1 {
                rc_str.push_str(",\n ");
            }
            rc_str.push_str(&needx.to_string());
            flg = 1;
        }
        rc_str.push(']');
        rc_str
    }

    /// Return true if a kind of state is in the NeedStore.
    pub fn kind_is_in(&self, name: &str) -> bool {
        for needx in &self.items {
            if needx.name() == name {
                return true;
            }
        }
        false
    }

    /// Remove a need by index.
    pub fn remove(&mut self, inx: usize) {
        self.items.remove(inx);
    }

    /// Return true if a need with a given type and target is in a NeedStore.
    pub fn contains_similar_need(&self, name: &str, target: &ATarget) -> bool {
        //println!("NeedStore::contains_similar_need: name {name} target {target}");
        for nedx in self.iter() {
            if nedx.name() != name {
                return false;
            }
            if match (nedx.target(), target) {
                (ATarget::State { state: state1 }, ATarget::State { state: state2 }) => {
                    state1 == state2
                }
                (ATarget::Region { region: region1 }, ATarget::Region { region: region2 }) => {
                    region1 == region2
                }
                (
                    ATarget::DomainRegions { regions: region1 },
                    ATarget::DomainRegions { regions: region2 },
                ) => region1 == region2,
                (
                    ATarget::SelectRegions { select: sel1 },
                    ATarget::SelectRegions { select: sel2 },
                ) => sel1.regions == sel2.regions,
                _ => false,
            } {
                return true;
            }
        }
        false
    }
} // end impl NeedStore

impl Index<usize> for NeedStore {
    type Output = SomeNeed;
    fn index(&self, i: usize) -> &SomeNeed {
        &self.items[i]
    }
}
