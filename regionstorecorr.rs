//! The RegionStoreCorr struct, a vector of SomeRegions, corresponding to domains in a vector.
//! Each region will have a number of bits equal to the bits used by the corresponding
//! domain, not necessarily the same as other regions in the vector.

use crate::bits::NumBits;
use crate::region::SomeRegion;
use crate::statestorecorr::StateStoreCorr;
use crate::tools::{self, corresponding_num_bits, AvecRef, StrLen};

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
/// A vector of regions, corresponding to domains.
pub struct RegionStoreCorr {
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

    /// Return true if a RegionStoreCorr is between two others.
    pub fn is_between(&self, other1: &Self, other2: &Self) -> bool {
        debug_assert_eq!(self.len(), other1.len());
        debug_assert_eq!(self.len(), other2.len());
        debug_assert!(corresponding_num_bits(self, other1));
        debug_assert!(corresponding_num_bits(self, other2));

        for (rcx, (rcy, rcz)) in self.iter().zip(other1.iter().zip(other2.iter())) {
            if rcx
                .diff_edge_mask(rcy)
                .bitwise_and(&rcx.diff_edge_mask(rcz))
                .is_not_low()
            {
                return false;
            }
        }
        true
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
    pub fn is_superset_states(&self, stas: &StateStoreCorr) -> bool {
        debug_assert!(self.len() == stas.len());
        debug_assert!(corresponding_num_bits(self, stas));

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
    pub fn is_subset_of(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());
        debug_assert!(corresponding_num_bits(self, other));

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_subset_of(y) {
            } else {
                return false;
            }
        }
        true
    }

    /// Return True if a RegionStore is a superset of another RSC.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());
        debug_assert!(corresponding_num_bits(self, other));

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_superset_of(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return the intersection, if any, of two RegionStores.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        debug_assert!(self.len() == other.len());
        debug_assert!(corresponding_num_bits(self, other));

        let mut ret = Self::with_capacity(self.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if let Some(regz) = x.intersection(y) {
                ret.push(regz);
            } else {
                return None;
            }
        }

        Some(ret)
    }

    /// Calculate the distance between a RegionStore and the current state.
    pub fn distance_states(&self, stas: &StateStoreCorr) -> usize {
        debug_assert!(self.len() == stas.len());
        debug_assert!(corresponding_num_bits(self, stas));

        let mut dist = 0;
        for (x, y) in self.iter().zip(stas.iter()) {
            if x.intersects(y) {
            } else {
                dist += x.distance(y);
            }
        }

        dist
    }

    /// Calculate the distance between two RegionStoreCorrs.
    pub fn distance(&self, other: &Self) -> usize {
        debug_assert!(self.len() == other.len());
        debug_assert!(corresponding_num_bits(self, other));

        let mut dist = 0;
        for (x, y) in self.iter().zip(other.iter()) {
            if x.intersects(y) {
            } else {
                dist += x.distance(y);
            }
        }

        dist
    }

    /// Return self minus a given RegionStoreCorr.
    pub fn subtract(&self, subtrahend: &Self) -> Vec<Self> {
        debug_assert!(self.len() == subtrahend.len());
        debug_assert!(corresponding_num_bits(self, subtrahend));

        let mut ret = Vec::<Self>::new();

        // Check for no change.
        if !self.intersects(subtrahend) {
            ret.push(self.clone());
            return ret;
        }
        // Check for superest.
        if subtrahend.is_superset_of(self) {
            return ret;
        }

        for (inx, (regx, regy)) in self.iter().zip(subtrahend.iter()).enumerate() {
            let xb_msk = regx.x_mask().bitwise_and(&regy.edge_mask());
            if xb_msk.is_low() {
                continue;
            }
            // At least one X over non-X bit found.

            // Isolate each X over non-X bit.
            let single_bits = xb_msk.split();

            // Generate a new RegionStore for each isolated bit.
            for sbitx in single_bits.iter() {
                // Alter one X bit in self/regx to the opposite of the corresponding non-X bit in subtrahend/regy.
                let regz = if sbitx.bitwise_and(regy.first_state()).is_low() {
                    // Other/regy bit is zero, in regy.first_state (and regy.state2, since its non-X).
                    regx.set_to_ones(sbitx)
                } else {
                    regx.set_to_zeros(sbitx)
                };

                // Copy self, except for one region with one bit changed.
                let mut one_result = Self::with_capacity(self.len());
                for (iny, regm) in self.iter().enumerate() {
                    if iny == inx {
                        one_result.push(regz.clone());
                    } else {
                        one_result.push(regm.clone());
                    }
                }
                // Save fragment to return.
                ret.push(one_result);
            }
        }
        ret
    }

    /// Return true if there is an intersection of corresponding regions.
    pub fn intersects(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());
        debug_assert!(corresponding_num_bits(self, other));

        for (x, y) in self.iter().zip(other.iter()) {
            if !x.intersects(y) {
                return false;
            }
        }
        true
    }

    /// Translate regions to within another.
    pub fn translate_to(&self, other: &Self) -> Self {
        debug_assert!(self.len() == other.len());
        debug_assert!(corresponding_num_bits(self, other));

        let mut ret_regs = Self::new(vec![]);

        for (regx, regy) in self.iter().zip(other.iter()) {
            ret_regs.push(regx.translate_to(regy));
        }
        ret_regs
    }

    /// Return the number of bits different between two RegionStoreCorr.
    pub fn num_different_bits(&self, other: &Self) -> usize {
        debug_assert!(self.len() == other.len());
        debug_assert!(corresponding_num_bits(self, other));

        let mut ret_num = 0;
        for (regx, regy) in self.iter().zip(other.iter()) {
            ret_num += regx.diff_edge_mask(regy).num_one_bits();
        }
        ret_num
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

impl AvecRef for RegionStoreCorr {
    fn avec_ref(&self) -> &Vec<impl NumBits> {
        &self.avec
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::state::SomeState;

    #[test]
    fn test_is_superset_states_corr() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x00").expect("SNH"));
        regstr1.push(SomeRegion::new_from_string("r1x1x").expect("SNH"));

        let sta1 = SomeState::new(SomeBits::new_from_string("0x1")?);
        let sta8 = SomeState::new(SomeBits::new_from_string("0x8")?);
        let stas = StateStoreCorr::new(vec![sta1, sta8]);

        println!("regstr1 {}", regstr1);
        println!("stas    {}", stas);

        assert!(!regstr1.is_superset_states(&stas));

        let sta4 = SomeState::new(SomeBits::new_from_string("0x4")?);
        let sta10 = SomeState::new(SomeBits::new_from_string("0xa")?);
        let stas2 = StateStoreCorr::new(vec![sta4, sta10]);

        println!("regstr1 {}", regstr1);
        println!("stas2   {}", stas2);

        assert!(regstr1.is_superset_states(&stas2));

        Ok(())
    }

    #[test]
    fn distance_states() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x00").expect("SNH"));
        regstr1.push(SomeRegion::new_from_string("r1x1x").expect("SNH"));

        let sta1 = SomeState::new(SomeBits::new_from_string("0x1")?);
        let sta8 = SomeState::new(SomeBits::new_from_string("0x8")?);
        let stas = StateStoreCorr::new(vec![sta1, sta8]);

        let dist = regstr1.distance_states(&stas);
        println!("Distance = {dist}");

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn is_superset_subset_of() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x0x").expect("SNH"));
        regstr1.push(SomeRegion::new_from_string("r1x0x").expect("SNH"));

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("r0101").expect("SNH"));
        regstr2.push(SomeRegion::new_from_string("r1x01").expect("SNH"));

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);

        assert!(regstr1.is_superset_of(&regstr2));
        assert!(!regstr2.is_superset_of(&regstr1));

        assert!(!regstr1.is_subset_of(&regstr2));
        assert!(regstr2.is_subset_of(&regstr1));

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x0x").expect("SNH"));
        regstr1.push(SomeRegion::new_from_string("r1x0x").expect("SNH"));

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("rx1x1").expect("SNH"));
        regstr2.push(SomeRegion::new_from_string("r1xx1").expect("SNH"));

        let intreg = regstr1.intersection(&regstr2).expect("SNH");
        println!("int part {}", intreg);

        assert!(
            intreg
                == RegionStoreCorr::new(vec![
                    SomeRegion::new_from_string("r0101").expect("SNH"),
                    SomeRegion::new_from_string("r1x01").expect("SNH")
                ])
        );
        Ok(())
    }
}
