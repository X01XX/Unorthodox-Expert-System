//! Implement a struct of Select RegionStores.
//! This struct contains regions in domain order, the regions will have a size matching the corresponding domain, not other
//! regions in the vector.
//!
//! The regions have a boolean And relationship.
//! If only one region is non-maximum, that singles out that domain.

use crate::region::SomeRegion;
use crate::regionstorecorr::RegionStoreCorr;
use crate::statestorecorr::StateStoreCorr;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;

impl fmt::Display for SelectRegions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = self.regions.to_string();
        if self.pos > 0 || self.neg > 0 {
            str.push_str(&format!(", positive: {}, negative: {}", self.pos, self.neg));
        }
        if self.times_visited > 0 {
            str.push_str(&format!(", times visited {}", self.times_visited));
        }
        write!(f, "{}", str)
    }
}

#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct SelectRegions {
    /// Regions, in domain order, describing the requirements for an select state.
    /// If the regions are all X, except for one, then it affects only one domain.
    /// Otherwise, it affects a combination of two, or more, domains.
    pub regions: RegionStoreCorr,
    /// A value for being in the select state.
    /// A Positive value is, so far, given to a goal state.
    pub pos: usize,
    /// A negative value is, so far, given to a plan that passes through the regions,
    /// not counting the beginning and end state.
    pub neg: usize,
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
    pub fn new(regions: RegionStoreCorr, pos: usize, neg: usize) -> Self {
        Self {
            regions,
            pos,
            neg,
            times_visited: 0,
        }
    }

    /// Return the aggregate value of a n instance.
    pub fn value(&self) -> isize {
        self.pos as isize - self.neg as isize
    }

    /// Increment times visited.
    pub fn inc_times_visited(&mut self) {
        self.times_visited += 1;
    }

    /// Return the intersection of two SelectRegions.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        debug_assert!(self.is_similar_to(other));

        self.regions
            .intersection_corr(&other.regions)
            .map(|regs| Self::new(regs, self.pos + other.pos, self.neg + other.neg))
    }

    /// Calculate the distance between a SelectRegions and a vector of states.
    pub fn distance_states(&self, stas: &StateStoreCorr) -> usize {
        debug_assert!(self.len() == stas.len());

        self.regions.distance_states_corr(stas)
    }

    /// Return the number of regions in a SelectRegions instance.
    pub fn len(&self) -> usize {
        self.regions.len()
    }

    /// Return true if two SelectRegions are similar.
    pub fn is_similar_to(&self, other: &Self) -> bool {
        if self.regions.len() != other.regions.len() {
            return false;
        }
        for (regx, regy) in self.regions.iter().zip(other.regions.iter()) {
            if regx.num_bits() != regy.num_bits() {
                return false;
            }
        }
        true
    }

    /// Add a Region.
    pub fn push(&mut self, regx: SomeRegion) {
        self.regions.push(regx);
    }

    /// Return true if a SelectRegions is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.regions.is_subset_of_corr(&other.regions)
    }

    /// Return true if a SelectRegions is a superset of a vector of state refs.
    pub fn is_superset_of_states(&self, stas: &StateStoreCorr) -> bool {
        self.regions.is_superset_states_corr(stas)
    }
}

/// Implement the trait StrLen for SomeRegion.
impl StrLen for SelectRegions {
    fn strlen(&self) -> usize {
        // Regions
        let mut ret = self.regions.strlen();
        if self.pos > 0 || self.neg > 0 {
            ret += 24 + format!("{}", self.pos).len() + format!("{}", self.neg).len();
        }
        if self.times_visited > 0 {
            ret += 16 + format!("{}", self.times_visited).len();
        }
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::state::SomeState;

    #[test]
    fn test_strlen() -> Result<(), String> {
        let ur_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(8))]);

        let srs = SelectRegions::new(
            RegionStoreCorr::new(vec![
                ur_reg.new_from_string("r0xx1").expect("SNH"),
                ur_reg.new_from_string("r0x1x").expect("SNH"),
            ]),
            0,
            0,
        );

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());

        assert!(rslt.len() == srs.strlen());

        let srs = SelectRegions::new(
            RegionStoreCorr::new(vec![
                ur_reg.new_from_string("r0xx1").expect("SNH"),
                ur_reg.new_from_string("r0x1x").expect("SNH"),
            ]),
            1,
            20,
        );

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        let mut srs = SelectRegions::new(
            RegionStoreCorr::new(vec![
                ur_reg.new_from_string("r0xx1").expect("SNH"),
                ur_reg.new_from_string("r0x1x").expect("SNH"),
            ]),
            0,
            0,
        );
        srs.times_visited = 1;

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        let mut srs = SelectRegions::new(
            RegionStoreCorr::new(vec![
                ur_reg.new_from_string("r0xx1").expect("SNH"),
                ur_reg.new_from_string("r0x1x").expect("SNH"),
            ]),
            0,
            5,
        );
        srs.times_visited = 11;

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        //assert!(1 == 2);
        Ok(())
    }
}
