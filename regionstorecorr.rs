//! The RegionStoreCorr truct, a vector of SomeRegions, corresponding to domains in a vector.
//! Each region will have a number of bits equal to the bits used by the corresponding
//! domain, not necessarily the same as other regions in the vector.

use crate::region::SomeRegion;
use crate::statestorecorr::StateStoreCorr;
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

impl fmt::Display for RegionStoreCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.avec))
    }
}
#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct RegionStoreCorr {
    /// A vector of regions.
    pub avec: Vec<SomeRegion>,
}

impl PartialEq for RegionStoreCorr {
    fn eq(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (regx, regy) in self.avec.iter().zip(other.iter()) {
            if regx != regy {
                return false;
            }
        }
        true
    }
}
impl Eq for RegionStoreCorr {}

impl RegionStoreCorr {
    /// Return a new, RegionStore.
    pub fn new(avec: Vec<SomeRegion>) -> Self {
        Self { avec }
    }

    /// Return a new RegionStoreCorr instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeRegion>::with_capacity(num),
        }
    }

    /// Return the number of regions.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Add a region to the vector.
    pub fn push(&mut self, val: SomeRegion) {
        self.avec.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRegion> {
        self.avec.iter()
    }

    /// Return True if a RegionStore is a superset of all corresponding states in a SomeState vector.
    /// Used in optimal regionstore calculations.
    pub fn is_superset_states_corr(&self, stas: &StateStoreCorr) -> bool {
        debug_assert!(self.len() == stas.len());

        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return true if RegionStore is a subset of another RegionStore.
    /// Used in optimal regionstore calculations.
    pub fn is_subset_of_corr(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_subset_of(y) {
            } else {
                return false;
            }
        }
        true
    }

    /// Return True if a RegionStore is a superset of another RSC.
    pub fn is_superset_of_corr(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_superset_of(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return the intersection, if any, of two RegionStores.
    pub fn intersection_corr(&self, other: &Self) -> Option<Self> {
        debug_assert!(self.len() == other.len());

        let mut ret = Self::with_capacity(self.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x.intersects(y) {
                if let Some(reg_int) = x.intersection(y) {
                    ret.push(reg_int);
                }
            } else {
                return None;
            }
        }

        Some(ret)
    }

    /// Calculate the distance between a RegionStore and the current state.
    pub fn distance_states_corr(&self, stas: &StateStoreCorr) -> usize {
        debug_assert!(self.len() == stas.len());

        let mut dist = 0;
        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of(y) {
            } else {
                dist += x.distance(y);
            }
        }

        dist
    }

    /// Add a RegionStore, removing subset (and equal) RegionStores.
    /// Return true if the item was added.
    pub fn vec_push_nosubs_corr(avec: &mut Vec<RegionStoreCorr>, item: RegionStoreCorr) -> bool {
        // Check for supersets.
        for itemx in avec.iter() {
            if itemx.is_superset_of_corr(&item) {
                return false;
            }
        }

        // Identify supersets
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in avec.iter().enumerate() {
            if regx.is_subset_of_corr(&item) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in reverse (highest index) order
        for inx in rmvec.iter().rev() {
            tools::remove_unordered(avec, *inx);
        }

        avec.push(item);

        true
    }
} // End impl RegionStoreCorr.

impl Index<usize> for RegionStoreCorr {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.avec[i]
    }
}

impl IndexMut<usize> for RegionStoreCorr {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}

/// Implement the trait StrLen for RegionStoreCorr.
impl StrLen for RegionStoreCorr {
    fn strlen(&self) -> usize {
        let mut rc_len = 2;

        if self.is_not_empty() {
            rc_len += self.avec.len() * self.avec[0].strlen();
            rc_len += (self.avec.len() - 1) * 2;
        }

        rc_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::state::SomeState;

    #[test]
    fn test_is_superset_states_corr() -> Result<(), String> {
        let ur_bits = SomeBits::new(8);
        let tmp_sta0 = SomeState::new(ur_bits.clone());
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x00").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x1x").expect("SNH"));

        let sta1 = SomeState::new(ur_bits.new_from_string("0x1")?);
        let sta8 = SomeState::new(ur_bits.new_from_string("0x8")?);
        let stas = StateStoreCorr::new(vec![sta1, sta8]);

        println!("regstr1 {}", regstr1);
        println!("stas    {}", stas);

        assert!(!regstr1.is_superset_states_corr(&stas));

        let sta4 = SomeState::new(ur_bits.new_from_string("0x4")?);
        let sta10 = SomeState::new(ur_bits.new_from_string("0xa")?);
        let stas2 = StateStoreCorr::new(vec![sta4, sta10]);

        println!("regstr1 {}", regstr1);
        println!("stas2   {}", stas2);

        assert!(regstr1.is_superset_states_corr(&stas2));

        Ok(())
    }

    #[test]
    fn test_distance_states_corr() -> Result<(), String> {
        let ur_bits = SomeBits::new(8);
        let tmp_sta0 = SomeState::new(ur_bits.clone());
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x00").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x1x").expect("SNH"));

        let sta1 = SomeState::new(ur_bits.new_from_string("0x1")?);
        let sta8 = SomeState::new(ur_bits.new_from_string("0x8")?);
        let stas = StateStoreCorr::new(vec![sta1, sta8]);

        let dist = regstr1.distance_states_corr(&stas);
        println!("Distance = {dist}");

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn test_is_superset_subset_of_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(8));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x0x").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x0x").expect("SNH"));

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(tmp_reg0.new_from_string("r0101").expect("SNH"));
        regstr2.push(tmp_reg0.new_from_string("r1x01").expect("SNH"));

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);

        assert!(regstr1.is_superset_of_corr(&regstr2));
        assert!(!regstr2.is_superset_of_corr(&regstr1));

        assert!(!regstr1.is_subset_of_corr(&regstr2));
        assert!(regstr2.is_subset_of_corr(&regstr1));

        Ok(())
    }

    #[test]
    fn test_intersection_corr() -> Result<(), String> {
        let tmp_sta0 = SomeState::new(SomeBits::new(8));
        let tmp_reg0 = SomeRegion::new(vec![tmp_sta0.clone()]);

        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(tmp_reg0.new_from_string("r0x0x").expect("SNH"));
        regstr1.push(tmp_reg0.new_from_string("r1x0x").expect("SNH"));

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(tmp_reg0.new_from_string("rx1x1").expect("SNH"));
        regstr2.push(tmp_reg0.new_from_string("r1xx1").expect("SNH"));

        let intreg = regstr1.intersection_corr(&regstr2).expect("SNH");
        println!("int part {}", intreg);

        assert!(
            intreg
                == RegionStoreCorr::new(vec![
                    tmp_reg0.new_from_string("r0101").expect("SNH"),
                    tmp_reg0.new_from_string("r1x01").expect("SNH")
                ])
        );
        Ok(())
    }
}
