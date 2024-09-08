//! The RegionStoreCorr struct, a vector of SomeRegions, corresponding to domains in a vector.
//! Each region will have a number of bits equal to the bits used by the corresponding
//! domain, not necessarily the same as other regions in the vector.

use crate::bits::NumBits;
use crate::region::SomeRegion;
use crate::statestorecorr::StateStoreCorr;
use crate::tools::{self, AvecRef, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

impl fmt::Display for RegionStoreCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.items))
    }
}
#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
/// A vector of regions, corresponding to domains in a vector.
pub struct RegionStoreCorr {
    pub items: Vec<SomeRegion>,
}

impl PartialEq for RegionStoreCorr {
    fn eq(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (regx, regy) in self.items.iter().zip(other.iter()) {
            if regx != regy {
                return false;
            }
        }
        true
    }
}
impl Eq for RegionStoreCorr {}

impl RegionStoreCorr {
    /// Return a new, RegionStoreCorr.
    pub fn new(items: Vec<SomeRegion>) -> Self {
        Self { items }
    }

    /// Return a new RegionStoreCorr instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SomeRegion>::with_capacity(num),
        }
    }

    /// Return the number of regions.
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

    /// Add a region to the region vector.
    pub fn push(&mut self, val: SomeRegion) {
        self.items.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRegion> {
        self.items.iter()
    }

    /// Return True if a RegionStoreCorr is a superset of all corresponding states in.
    pub fn is_superset_states(&self, stas: &StateStoreCorr) -> bool {
        debug_assert!(self.len() == stas.len());
        debug_assert!(self.corresponding_num_bits(stas));

        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return true if RegionStoreCorr is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());
        debug_assert!(self.corresponding_num_bits(other));

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_subset_of(y) {
            } else {
                return false;
            }
        }
        true
    }

    /// Return true if RegionStoreCorr is adjacent to another.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());
        debug_assert!(self.corresponding_num_bits(other));

        self.distance(other) == 1
    }

    /// Return true if RegionStoreCorr is adjacent to another.
    pub fn bridge(&self, other: &Self) -> Self {
        debug_assert!(self.len() == other.len());
        debug_assert!(self.corresponding_num_bits(other));
        debug_assert!(self.distance(other) == 1);

        let mut ret_regs = Self::new(vec![]);

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_adjacent(y) {
                if let Some(xy) = x.bridge(y) {
                    ret_regs.push(xy);
                } else {
                    panic!("SNH");
                }
            } else if let Some(xy) = x.intersection(y) {
                ret_regs.push(xy);
            } else {
                panic!("SNH");
            }
        }
        ret_regs
    }

    /// Return True if a RegionStoreCorr is a superset of another.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());
        debug_assert!(self.corresponding_num_bits(other));

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_superset_of(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return the intersection, if any, of two RegionStoresCorrs.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        debug_assert!(self.len() == other.len());
        debug_assert!(self.corresponding_num_bits(other));

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

    /// Calculate the distance between a RegionStoreCorr and States in a StateStoreCorr.
    pub fn distance_states(&self, stas: &StateStoreCorr) -> usize {
        debug_assert!(self.len() == stas.len());
        debug_assert!(self.corresponding_num_bits(stas));

        let mut dist = 0;
        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of(y) {
            } else {
                dist += x.distance(y);
            }
        }

        dist
    }

    /// Calculate the distance between two RegionStoreCorrs.
    pub fn distance(&self, other: &Self) -> usize {
        debug_assert!(self.len() == other.len());
        debug_assert!(self.corresponding_num_bits(other));

        let mut dist = 0;
        for (x, y) in self.iter().zip(other.iter()) {
            if x.intersects(y) {
            } else {
                dist += x.distance(y);
            }
        }

        dist
    }

    /// Return RegionStoreCorr minus another.
    pub fn subtract(&self, subtrahend: &Self) -> Vec<Self> {
        debug_assert!(self.len() == subtrahend.len());
        debug_assert!(self.corresponding_num_bits(subtrahend));

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
            let remainders = regx.subtract(regy);

            for reg_rem in &remainders {
                // Copy self, except for one region with one bit changed.
                let mut one_result = Self::with_capacity(self.len());
                for (iny, regm) in self.iter().enumerate() {
                    if iny == inx {
                        one_result.push(reg_rem.clone());
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

    /// Return true if there is an intersection of two RegionStorCorrs.
    pub fn intersects(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());
        debug_assert!(self.corresponding_num_bits(other));

        for (x, y) in self.iter().zip(other.iter()) {
            if !x.intersects(y) {
                return false;
            }
        }
        true
    }

    /// Make minimum changes to a RegionStoreCorr so that it will be a subset of another.
    pub fn translate_to(&self, other: &Self) -> Self {
        debug_assert!(self.len() == other.len());
        debug_assert!(self.corresponding_num_bits(other));

        let mut ret_regs = Self::new(vec![]);

        for (regx, regy) in self.iter().zip(other.iter()) {
            ret_regs.push(regx.translate_to(regy));
        }
        ret_regs
    }

    /// Return true if corresponding items in two vectors have the same number of bits.
    pub fn corresponding_num_bits(&self, other: &impl AvecRef) -> bool {
        for (item1, item2) in self.avec_ref().iter().zip(other.avec_ref().iter()) {
            if item1.num_bits() != item2.num_bits() {
                return false;
            }
        }
        true
    }
} // End impl RegionStoreCorr.

impl Index<usize> for RegionStoreCorr {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.items[i]
    }
}

impl IndexMut<usize> for RegionStoreCorr {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}

/// Implement the trait StrLen for RegionStoreCorr.
impl StrLen for RegionStoreCorr {
    fn strlen(&self) -> usize {
        let mut rc_len = 2;

        if self.is_not_empty() {
            rc_len += self.items.len() * self.items[0].strlen();
            rc_len += (self.items.len() - 1) * 2;
        }

        rc_len
    }
}

impl AvecRef for RegionStoreCorr {
    fn avec_ref(&self) -> &Vec<impl NumBits> {
        &self.items
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::state::SomeState;

    #[test]
    fn is_superset_states() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x00")?);
        regstr1.push(SomeRegion::new_from_string("r1x1x")?);

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
        regstr1.push(SomeRegion::new_from_string("r0x00")?);
        regstr1.push(SomeRegion::new_from_string("r1x1x")?);

        let sta1 = SomeState::new(SomeBits::new_from_string("0x1")?);
        let sta8 = SomeState::new(SomeBits::new_from_string("0x8")?);
        let stas = StateStoreCorr::new(vec![sta1, sta8]);

        let dist = regstr1.distance_states(&stas);
        println!("Distance = {dist}");

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x0x")?);
        regstr1.push(SomeRegion::new_from_string("r1x0x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("r0101")?);
        regstr2.push(SomeRegion::new_from_string("r1x01")?);

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);

        assert!(!regstr1.is_subset_of(&regstr2));
        assert!(regstr2.is_subset_of(&regstr1));

        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x0x")?);
        regstr1.push(SomeRegion::new_from_string("r1x0x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("r0101")?);
        regstr2.push(SomeRegion::new_from_string("r1x01")?);

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);

        assert!(regstr1.is_superset_of(&regstr2));
        assert!(!regstr2.is_superset_of(&regstr1));

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x0x")?);
        regstr1.push(SomeRegion::new_from_string("r1x0x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("rx1x1")?);
        regstr2.push(SomeRegion::new_from_string("r1xx1")?);

        let intreg = regstr1.intersection(&regstr2).expect("SNH");
        println!("int part {}", intreg);

        assert!(
            intreg
                == RegionStoreCorr::new(vec![
                    SomeRegion::new_from_string("r0101")?,
                    SomeRegion::new_from_string("r1x01")?
                ])
        );
        Ok(())
    }

    #[test]
    fn distance() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x0x")?);
        regstr1.push(SomeRegion::new_from_string("r1x00")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("r11x1")?);
        regstr2.push(SomeRegion::new_from_string("r1xx1")?);

        let dist = regstr1.distance(&regstr2);
        println!("dist {}", dist);

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x0x")?);
        regstr1.push(SomeRegion::new_from_string("r1x00")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("r11x1")?);
        regstr2.push(SomeRegion::new_from_string("r1xx1")?);

        let intb = regstr1.intersects(&regstr2);
        println!("{regstr1} intersects {regstr2} is {intb}");

        assert!(!intb);

        let mut regstr3 = RegionStoreCorr::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string("r010x")?);
        regstr3.push(SomeRegion::new_from_string("rx10x")?);

        let intb = regstr1.intersects(&regstr3);
        println!("{regstr1} intersects {regstr3} is {intb}");

        assert!(intb);

        Ok(())
    }

    #[test]
    fn translate_to() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r0x0x")?);
        regstr1.push(SomeRegion::new_from_string("r1x00").expect("SNH"));

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("r11x1")?);
        regstr2.push(SomeRegion::new_from_string("r1xx1")?);

        let regstr3 = regstr1.translate_to(&regstr2);
        println!("{regstr1} transate_to {regstr2} is {regstr3}");

        let mut regstrtmp = RegionStoreCorr::with_capacity(2);
        regstrtmp.push(SomeRegion::new_from_string("r1101")?);
        regstrtmp.push(SomeRegion::new_from_string("r1x01")?);

        assert!(regstr3 == regstrtmp);

        let mut regstr4 = RegionStoreCorr::with_capacity(2);
        regstr4.push(SomeRegion::new_from_string("r010x")?);
        regstr4.push(SomeRegion::new_from_string("rx10x")?);

        let regstr5 = regstr1.translate_to(&regstr4);
        println!("{regstr1} transate_to {regstr4} is {regstr5}");

        let mut regstrtmp = RegionStoreCorr::with_capacity(2);
        regstrtmp.push(SomeRegion::new_from_string("r010x")?);
        regstrtmp.push(SomeRegion::new_from_string("r1100")?);

        assert!(regstr5 == regstrtmp);

        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("rx_01xx")?);
        regstr1.push(SomeRegion::new_from_string("rx_01xx")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("rx_0101")?);
        regstr2.push(SomeRegion::new_from_string("r1_xxx0")?);

        let regstrvec = regstr1.subtract(&regstr2);
        println!("{regstr1} subtract {regstr2} is: ");
        for rscx in regstrvec.iter() {
            println!("  {rscx}");
        }

        assert!(regstrvec.len() == 4);
        assert!(regstrvec.contains(&RegionStoreCorr::new(vec![
            SomeRegion::new_from_string("rx_01x0")?,
            SomeRegion::new_from_string("rx_01xx")?
        ])));
        assert!(regstrvec.contains(&RegionStoreCorr::new(vec![
            SomeRegion::new_from_string("rx_011x")?,
            SomeRegion::new_from_string("rx_01xx")?
        ])));
        assert!(regstrvec.contains(&RegionStoreCorr::new(vec![
            SomeRegion::new_from_string("rx_01xx")?,
            SomeRegion::new_from_string("rx_01x1")?
        ])));
        assert!(regstrvec.contains(&RegionStoreCorr::new(vec![
            SomeRegion::new_from_string("rx_01xx")?,
            SomeRegion::new_from_string("r0_01xx")?
        ])));

        Ok(())
    }
}
