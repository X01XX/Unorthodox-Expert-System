//! The RegionsCorr struct, a store of SomeRegions, corresponding in order, to domains in a DomainStore instance.
//!
//! Each region will have a number of bits equal to the bits used by the corresponding
//! domain, not necessarily the same as other regions in the vector.

use crate::bits::NumBits;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::statescorr::StatesCorr;
use crate::tools::{AvecRef, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

use unicode_segmentation::UnicodeSegmentation;

impl fmt::Display for RegionsCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RC{}", self.regions)
    }
}
#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
/// A vector of regions, corresponding to domains in a vector.
pub struct RegionsCorr {
    pub regions: RegionStore,
}

impl PartialEq for RegionsCorr {
    fn eq(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (regx, regy) in self.regions.iter().zip(other.iter()) {
            if regx != regy {
                return false;
            }
        }
        true
    }
}
impl Eq for RegionsCorr {}

impl RegionsCorr {
    /// Return a new, RegionsCorr.
    pub fn new(regions: Vec<SomeRegion>) -> Self {
        //debug_assert!(!regions.is_empty());
        Self {
            regions: RegionStore::new(regions),
        }
    }

    /// Return a new RegionsCorr instance, empty, with a specified capacity.
    pub fn with_capacity(cap: usize) -> Self {
        debug_assert!(cap > 0);
        Self {
            regions: RegionStore::with_capacity(cap),
        }
    }

    /// Return the number of regions.
    pub fn len(&self) -> usize {
        self.regions.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.regions.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.regions.is_empty()
    }

    /// Add a region to the region vector.
    pub fn push(&mut self, val: SomeRegion) {
        self.regions.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRegion> {
        self.regions.iter()
    }

    /// Return True if a RegionsCorr is a superset of all corresponding states in.
    pub fn is_superset_states(&self, stas: &StatesCorr) -> bool {
        debug_assert!(self.is_congruent(stas));

        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return true if RegionsCorr is a subset of another.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        debug_assert!(self.is_congruent(other));

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_subset_of(y) {
            } else {
                return false;
            }
        }
        true
    }

    /// Return true if RegionsCorr is adjacent to another.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        debug_assert!(self.is_congruent(other));

        self.distance(other) == 1
    }

    /// Return True if a RegionsCorr is a superset of another.
    pub fn is_superset_of(&self, other: &Self) -> bool {
        debug_assert!(self.is_congruent(other));

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
        debug_assert!(self.is_congruent(other));

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

    /// Return the union, of two RegionStoresCorrs.
    pub fn union(&self, other: &Self) -> Self {
        debug_assert!(self.is_congruent(other));

        let mut ret = Self::with_capacity(self.len());

        for (x, y) in self.iter().zip(other.iter()) {
            ret.push(x.union(y));
        }

        ret
    }

    /// Calculate the distance between a RegionsCorr and States in a StatesCorr.
    pub fn distance_states(&self, stas: &StatesCorr) -> usize {
        debug_assert!(self.is_congruent(stas));

        let mut dist = 0;
        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of(y) {
            } else {
                dist += x.distance(y);
            }
        }

        dist
    }

    /// Calculate the distance between two RegionsCorrs.
    pub fn distance(&self, other: &Self) -> usize {
        debug_assert!(self.is_congruent(other));

        let mut dist = 0;
        for (x, y) in self.iter().zip(other.iter()) {
            dist += x.distance(y);
        }

        dist
    }

    /// Return RegionsCorr minus another.
    pub fn subtract(&self, subtrahend: &Self) -> Vec<Self> {
        debug_assert!(self.is_congruent(subtrahend));

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
        debug_assert!(self.is_congruent(other));

        for (x, y) in self.iter().zip(other.iter()) {
            if !x.intersects(y) {
                return false;
            }
        }
        true
    }

    /// Make minimum changes to a RegionsCorr so that it will be a subset of another.
    pub fn translate_to(&self, other: &Self) -> Self {
        debug_assert!(self.is_congruent(other));

        let mut ret_regs = Self::with_capacity(self.len());

        for (regx, regy) in self.iter().zip(other.iter()) {
            ret_regs.push(regx.translate_to(regy));
        }
        ret_regs
    }

    /// Return true if corresponding regions in two vectors have the same number of bits.
    pub fn is_congruent(&self, other: &impl AvecRef) -> bool {
        if self.len() != other.avec_ref().len() {
            return false;
        }
        for (item1, item2) in self.avec_ref().iter().zip(other.avec_ref().iter()) {
            if item1.num_bits() != item2.num_bits() {
                return false;
            }
        }
        true
    }

    /// Return a RegionsCorr instance, given a string representation.
    /// Like RC[], RC[r1010], or RC[r101, r1000].
    pub fn from(rc_str: &str) -> Result<Self, String> {
        //println!("regionscorr::from: {rc_str}");

        let mut rc_str2 = String::new();
        let mut last_chr = false;

        for (inx, chr) in rc_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "R" {
                    continue;
                } else {
                    return Err(format!(
                        "RegionsCorr::from: Invalid string, {rc_str} should start with RC["
                    ));
                }
            }
            if inx == 1 {
                if chr == "C" {
                    continue;
                } else {
                    return Err(format!(
                        "RegionsCorr::from: Invalid string, {rc_str} should start with RC["
                    ));
                }
            }
            if chr == "]" {
                last_chr = true;
                rc_str2.push_str(chr);
                continue;
            }

            if last_chr {
                return Err(format!(
                    "RegionsCorr::from: Invalid string, {rc_str} should end with ]"
                ));
            }
            rc_str2.push_str(chr);
        }
        if !last_chr {
            return Err(format!(
                "RegionsCorr::from: Invalid string, {rc_str} should end with ]"
            ));
        }

        let regions = RegionStore::from(&rc_str2)?;

        Ok(Self { regions })
    }

    /// Return a vector of corresponding num_bits.
    pub fn num_bits_vec(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::with_capacity(self.len());
        for regx in self.iter() {
            ret_vec.push(regx.num_bits());
        }
        ret_vec
    }
} // End impl RegionsCorr.

impl Index<usize> for RegionsCorr {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.regions[i]
    }
}

impl IndexMut<usize> for RegionsCorr {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.regions[i]
    }
}

/// Implement the trait StrLen for RegionsCorr.
impl StrLen for RegionsCorr {
    fn strlen(&self) -> usize {
        let mut rc_len = 4;

        if self.is_not_empty() {
            rc_len += self.regions.len() * self.regions[0].strlen();
            rc_len += (self.regions.len() - 1) * 2;
        }

        rc_len
    }
}

impl AvecRef for RegionsCorr {
    fn avec_ref(&self) -> &Vec<impl NumBits> {
        self.regions.avec_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strlen() -> Result<(), String> {
        let regs1_str = "RC[r0x00, r1x1x]";
        let regs1 = RegionsCorr::from(&regs1_str)?;
        let len = regs1.strlen();
        println!("len of {regs1_str} = {len}");

        assert!(len == regs1_str.len());

        Ok(())
    }

    #[test]
    fn is_superset_states() -> Result<(), String> {
        let regstr1 = RegionsCorr::from("RC[r0x00, r1x1x]")?;

        let stas = StatesCorr::from("SC[0x1, 0x8]")?;

        println!("regstr1 {}", regstr1);
        println!("stas    {}", stas);

        assert!(!regstr1.is_superset_states(&stas));

        let stas2 = StatesCorr::from("SC[0x4, 0xa]")?;

        println!("regstr1 {}", regstr1);
        println!("stas2   {}", stas2);

        assert!(regstr1.is_superset_states(&stas2));

        Ok(())
    }

    #[test]
    fn distance_states() -> Result<(), String> {
        let regstr1 = RegionsCorr::from("RC[r0x00, r1x1x]")?;

        let stas = StatesCorr::from("SC[0x1, 0x8]")?;

        let dist = regstr1.distance_states(&stas);
        println!("Distance = {dist}");

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let regstr1 = RegionsCorr::from("RC[r0x0x, r1x0x]")?;

        let regstr2 = RegionsCorr::from("RC[r0101, r1x01]")?;

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);

        assert!(!regstr1.is_subset_of(&regstr2));
        assert!(regstr2.is_subset_of(&regstr1));

        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let regstr1 = RegionsCorr::from("RC[r0x0x, r1x0x]")?;

        let regstr2 = RegionsCorr::from("RC[r0101, r1x01]")?;

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);

        assert!(regstr1.is_superset_of(&regstr2));
        assert!(!regstr2.is_superset_of(&regstr1));

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let regstr1 = RegionsCorr::from("RC[r0x0x, r1x0x]")?;

        let regstr2 = RegionsCorr::from("RC[rx1x1, r1xx1]")?;

        let intreg = regstr1.intersection(&regstr2).expect("SNH");
        println!("int part {}", intreg);

        assert!(intreg == RegionsCorr::from("RC[r0101, r1x01]")?);
        Ok(())
    }

    #[test]
    fn distance() -> Result<(), String> {
        let regstr1 = RegionsCorr::from("RC[r0x0x, r1x00]")?;

        let regstr2 = RegionsCorr::from("RC[r11x1, r1xx1]")?;

        let dist = regstr1.distance(&regstr2);
        println!("dist {}", dist);

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let regstr1 = RegionsCorr::from("RC[r0x0x, r1x00]")?;

        let regstr2 = RegionsCorr::from("RC[r11x1, r1xx1]")?;

        let intb = regstr1.intersects(&regstr2);
        println!("{regstr1} intersects {regstr2} is {intb}");

        assert!(!intb);

        let regstr3 = RegionsCorr::from("RC[r010x, rx10x]")?;

        let intb = regstr1.intersects(&regstr3);
        println!("{regstr1} intersects {regstr3} is {intb}");

        assert!(intb);

        Ok(())
    }

    #[test]
    fn translate_to() -> Result<(), String> {
        let regstr1 = RegionsCorr::from("RC[r0x0x, r1x00]")?;

        let regstr2 = RegionsCorr::from("RC[r11x1, r1xx1]")?;

        let regstr3 = regstr1.translate_to(&regstr2);
        println!("{regstr1} transate_to {regstr2} is {regstr3}");

        let mut regstrtmp = RegionsCorr::with_capacity(2);
        regstrtmp.push(SomeRegion::from("r1101")?);
        regstrtmp.push(SomeRegion::from("r1x01")?);

        assert!(regstr3 == regstrtmp);

        let regstr4 = RegionsCorr::from("RC[r010x, rx10x]")?;

        let regstr5 = regstr1.translate_to(&regstr4);
        println!("{regstr1} transate_to {regstr4} is {regstr5}");

        let regstrtmp = RegionsCorr::from("RC[r010x, r1100]")?;

        assert!(regstr5 == regstrtmp);

        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let regstr1 = RegionsCorr::from("RC[rx_01xx, rx_01xx]")?;
        let regstr2 = RegionsCorr::from("RC[rx_0101, r1_xxx0]")?;

        let regstrvec = regstr1.subtract(&regstr2);
        println!("{regstr1} subtract");
        println!("{regstr2} is: ");
        for rscx in regstrvec.iter() {
            println!("{rscx}");
        }

        assert!(regstrvec.len() == 4);

        assert!(regstrvec.contains(&RegionsCorr::from("RC[rx_01x0, rx_01xx]")?));
        assert!(regstrvec.contains(&RegionsCorr::from("RC[rx_011x, rx_01xx]")?));
        assert!(regstrvec.contains(&RegionsCorr::from("RC[rx_01xx, rx_01x1]")?));
        assert!(regstrvec.contains(&RegionsCorr::from("RC[rx_01xx, r0_01xx]")?));

        // Test the equivalent one-region subtraction.
        let regstr1 = RegionsCorr::from("RC[rx0_1xxx_01xx]")?;
        let regstr2 = RegionsCorr::from("RC[rx0_1011_xxx0]")?;

        let regstrvec = regstr1.subtract(&regstr2);
        println!("{regstr1} subtract");
        println!("{regstr2} is: ");
        for rscx in regstrvec.iter() {
            println!("{rscx}");
        }
        assert!(regstrvec.contains(&RegionsCorr::from("RC[rx_01x0_x_01xx]")?));
        assert!(regstrvec.contains(&RegionsCorr::from("RC[rx_011x_x_01xx]")?));
        assert!(regstrvec.contains(&RegionsCorr::from("RC[rx_01xx_x_01x1]")?));
        assert!(regstrvec.contains(&RegionsCorr::from("RC[rx_01xx_0_01xx]")?));

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn from() -> Result<(), String> {
        let regst1 = RegionsCorr::from("RC[]")?;
        println!("regst1 {regst1}");
        assert!(format!("{regst1}") == "RC[]");

        let regst2 = RegionsCorr::from("RC[r1010]")?;
        println!("regst2 {regst2}");
        assert!(format!("{regst2}") == "RC[r1010]");

        let regst3 = RegionsCorr::from("RC[r1010, r1111]")?;
        println!("regst3 {regst3}");
        assert!(format!("{regst3}") == "RC[r1010, r1111]");

        //assert!(1 == 2);
        Ok(())
    }
}
