//! Implement a struct of Select RegionStores.

use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;

impl fmt::Display for SelectRegions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = self.regions.formatted_string();
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

    /// Return a string representation for a vector of SelectRegions references.
    pub fn vec_ref_string(avec: &[&Self]) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in avec.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&format!("{}", orx));
        }
        ret_str.push(']');
        ret_str
    }
    /// Increment times visited.
    pub fn inc_times_visited(&mut self) {
        self.times_visited += 1;
    }

    /// Return true if there is an intersection of corresponding regions, of two SelectRegions.
    pub fn intersects(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.regions.iter().zip(other.regions.iter()) {
            if !x.intersects(y) {
                return false;
            }
        }
        true
    }

    /// Return true if at least one corresponding pair in two SelectReagions is adjacent,
    /// while other corresponding pairs are adjacent or intersect.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        let mut num_dif = 0;
        for (regx, regy) in self.regions.iter().zip(other.regions.iter()) {
            let dif = regx.diff_mask(regy).num_one_bits();

            if dif > 1 {
                return false;
            }
            num_dif += dif;
        }
        num_dif > 0
    }

    /// Return the adjacent part of two SelectRegions.
    /// Presumably, at least one pair of corresponding regions will be adjacent, calc the adjacent part.
    /// If a pair of corresponding regions intersect, calc the intersection.
    pub fn adjacent_part(&self, other: &Self) -> Self {
        assert!(self.is_adjacent(other));

        let mut ret_select = Self::new(
            RegionStore::new(Vec::<SomeRegion>::with_capacity(self.len())),
            0,
        );

        for (reg_s, reg_o) in self.regions.iter().zip(other.regions.iter()) {
            if reg_s.is_adjacent(reg_o) {
                ret_select.regions.push(reg_s.adjacent_part(reg_o));
            } else if let Some(reg_int) = reg_s.intersection(reg_o) {
                ret_select.regions.push(reg_int);
            } else {
                panic!("SNH");
            }
        }

        ret_select
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
