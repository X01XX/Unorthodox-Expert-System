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
        if self.value != 0 {
            str.push_str(&format!(", value: {:+}", self.value));
        }
        if self.times_visited > 0 {
            str.push_str(&format!(", times visited {}", self.times_visited));
        }
        write!(f, "{}", str)
    }
}

impl PartialEq for SelectRegions {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (regx, regy) in self.regions.iter().zip(other.regions.iter()) {
            if regx != regy {
                return false;
            }
        }
        true
    }
}
impl Eq for SelectRegions {}

#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct SelectRegions {
    /// Regions, in domain order, describing the requirements for an select state.
    /// If the regions are all X, except for one, then it affects only one domain.
    /// Otherwise, it affects a combination of domains where the corsponding region is not all X.
    pub regions: RegionStoreCorr,
    /// A value for being in the select state.
    pub value: isize,
    /// The number of times a SelectRegion has been visited due to satisfying a SelectRegion need.
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
    pub fn new(regions: RegionStoreCorr, value: isize) -> Self {
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

    /// Return the intersection of two SelectRegions.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        self.regions
            .intersection(&other.regions)
            .map(|regs| Self::new(regs, self.value + other.value))
    }

    /// Calculate the distance between a SelectRegions and a vector of states.
    pub fn distance_states(&self, stas: &StateStoreCorr) -> usize {
        debug_assert!(self.len() == stas.len());

        self.regions.distance_states(stas)
    }

    /// Return the number of regions in a SelectRegions instance.
    pub fn len(&self) -> usize {
        self.regions.len()
    }

    /// Add a Region.
    pub fn push(&mut self, regx: SomeRegion) {
        self.regions.push(regx);
    }

    /// Return true if a SelectRegions is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.regions.is_subset_of(&other.regions)
    }

    /// Return true if a SelectRegions is a superset of a vector of state refs.
    pub fn is_superset_of_states(&self, stas: &StateStoreCorr) -> bool {
        self.regions.is_superset_states(stas)
    }

    /// Return true if a SelectRegions is a superset of a vector of state refs.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        self.regions.is_superset_of(&other.regions)
    }

    /// Return true if there is an intersection of corresponding regions, of two SelectRegions.
    pub fn intersects(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        self.regions.intersects(&other.regions)
    }

    /// Set the positive value.
    pub fn set_value(&mut self, val: isize) {
        self.value = val;
    }

    /// Subtract a SelectRegions from another.
    /// Fragments, it any, retain the same value.
    pub fn subtract(&self, other: &Self) -> Vec<Self> {
        // println!("subtract {other} from {self}");

        let mut ret_vec = vec![];

        if other.is_superset_of(self) {
            return ret_vec;
        } else if other.intersects(self) {
            let regs = self.regions.subtract(&other.regions);
            for regz in regs {
                ret_vec.push(SelectRegions::new(regz, self.value));
            }
        } else {
            return vec![self.clone()];
        }

        ret_vec
    }

    /// Return the number of squares encompassed by a SelectRegions.
    pub fn extent(&self) -> usize {
        self.regions.extent()
    }
}

/// Implement the trait StrLen for SomeRegion.
impl StrLen for SelectRegions {
    fn strlen(&self) -> usize {
        // Regions
        let mut ret = self.regions.strlen();
        if self.value != 0 {
            ret += 9 + format!("{}", self.value).len();
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

    #[test]
    fn test_strlen() -> Result<(), String> {
        let srs = SelectRegions::new(
            RegionStoreCorr::new(vec![
                SomeRegion::new_from_string("r0xx1").expect("SNH"),
                SomeRegion::new_from_string("r0x1x").expect("SNH"),
            ]),
            0,
        );

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());

        assert!(rslt.len() == srs.strlen());

        let srs = SelectRegions::new(
            RegionStoreCorr::new(vec![
                SomeRegion::new_from_string("r0xx1").expect("SNH"),
                SomeRegion::new_from_string("r0x1x").expect("SNH"),
            ]),
            -19,
        );

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        let mut srs = SelectRegions::new(
            RegionStoreCorr::new(vec![
                SomeRegion::new_from_string("r0xx1").expect("SNH"),
                SomeRegion::new_from_string("r0x1x").expect("SNH"),
            ]),
            0,
        );
        srs.times_visited = 1;

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        let mut srs = SelectRegions::new(
            RegionStoreCorr::new(vec![
                SomeRegion::new_from_string("r0xx1").expect("SNH"),
                SomeRegion::new_from_string("r0x1x").expect("SNH"),
            ]),
            -5,
        );
        srs.times_visited = 11;

        let rslt = format!("{}", srs);
        println!("str {rslt} len {} calced {}", rslt.len(), srs.strlen());
        assert!(rslt.len() == srs.strlen());

        Ok(())
    }
}
