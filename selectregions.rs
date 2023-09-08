//! Implement a struct of Select RegionStores.

use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;

impl fmt::Display for SelectRegions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = self.regions.to_string();
        str.push_str(&format!(", value: {:+}", self.value));
        str.push_str(&format!(", times visited {}", self.times_visited));
        write!(f, "{}", str)
    }
}

#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct SelectRegions {
    /// Regions, in domain order, describing the requirements for an select state.
    /// If the regions are all X, except for one, then it affects only one domain.
    /// Otherwise, it affects a combination of two, or more, domains.
    pub regions: RegionStore,
    /// A value for being in the select state.
    /// A Positive value is, so far, given to a goal state.
    /// A negative value is, so far, given to a plan that passes through the regions,
    /// not counting the beginning and end state.
    pub value: isize,
    /// A cond of the number of time a SelectRegion has been visited due to satisfying a need.
    pub times_visited: usize,
}

impl Index<usize> for SelectRegions {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.regions[i]
    }
}

impl SelectRegions {
    /// Return a new SelectRegions instance.
    pub fn new(regions: RegionStore, value: isize) -> Self {
        Self {
            regions,
            value,
            times_visited: 0,
        }
    }

    /// Increment times visited.
    pub fn inc_times_visited(&mut self) {
        self.times_visited += 1;
    }

    /// Return true if there is an intersection of corresponding regions, of two SelectRegions.
    pub fn intersects(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        self.regions.intersects_corr(&other.regions)
    }

    /// Return true if at least one corresponding pair in two SelectRegions is adjacent,
    /// while other corresponding pairs are adjacent or intersect.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        self.regions.is_adjacent_corr(&other.regions)
    }

    /// Return the adjacent part of two SelectRegions.
    /// Presumably, at least one pair of corresponding regions will be adjacent, calc the adjacent part.
    /// If a pair of corresponding regions intersect, calc the intersection.
    pub fn adjacent_part(&self, other: &Self) -> Self {
        assert!(self.is_adjacent(other));

        Self::new(self.regions.adjacent_part_corr(&other.regions), 0)
    }

    /// Calculate the distance between a SelectRegions and the current state.
    pub fn distance_states(&self, stas: &[&SomeState]) -> usize {
        debug_assert!(self.len() == stas.len());

        self.regions.distance_states_corr(stas)
    }

    /// Return the length of an instance.
    pub fn len(&self) -> usize {
        self.regions.len()
    }

    /// Add a Region.
    pub fn push(&mut self, regx: SomeRegion) {
        self.regions.push(regx);
    }
}
