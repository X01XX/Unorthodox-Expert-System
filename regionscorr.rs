//! The RegionsCorr struct, a store of SomeRegions, corresponding in order, to domains in a DomainStore instance.
//!
//! Each region will have a number of bits equal to the bits used by the corresponding
//! domain, not necessarily the same as other regions in the vector.

use crate::bits::NumBits;
use crate::maskscorr::MasksCorr;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::statescorr::StatesCorr;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use std::str::FromStr;

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

    /// Return the intersection, if any, of two RegionsCorrs.
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

    /// Return the union, of two RegionsCorrs.
    pub fn union(&self, other: &Self) -> Self {
        debug_assert!(self.is_congruent(other));

        let mut ret = Self::with_capacity(self.len());

        for (x, y) in self.iter().zip(other.iter()) {
            ret.push(x.union(y));
        }

        ret
    }

    /// Return the x-mask of a RegionsCorr.
    pub fn x_mask(&self) -> MasksCorr {
        let mut ret = MasksCorr::with_capacity(self.len());

        for regx in self.iter() {
            ret.push(regx.x_mask());
        }

        ret
    }

    /// Return the edge-mask of a RegionsCorr.
    pub fn edge_mask(&self) -> MasksCorr {
        let mut ret = MasksCorr::with_capacity(self.len());

        for regx in self.iter() {
            ret.push(regx.edge_mask());
        }

        ret
    }

    /// Calculate the distance between a RegionsCorr and States in a StatesCorr.
    pub fn distance_states(&self, stas: &StatesCorr) -> usize {
        debug_assert!(self.is_congruent(stas));

        self.diff_edge_mask_states(stas).num_one_bits()
    }

    /// Calculate the distance between two RegionsCorrs.
    pub fn distance(&self, other: &Self) -> usize {
        debug_assert!(self.is_congruent(other));

        self.diff_edge_mask(other).num_one_bits()
    }

    /// Return a mask of one edge bit positions of a RegionsCorr.
    fn ones_edges(&self) -> MasksCorr {
        let mut ret_mask = MasksCorr::with_capacity(self.len());
        for regx in self.iter() {
            ret_mask.push(regx.ones_edges());
        }
        ret_mask
    }

    /// Return RegionsCorr minus another.
    pub fn subtract(&self, subtrahend: &Self) -> Vec<Self> {
        debug_assert!(self.is_congruent(subtrahend));
        //println!("regionscorr::subtract {self} - {subtrahend}");

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

        let sub_mask = self.x_mask().bitwise_and(&subtrahend.edge_mask());

        let sub_masks = sub_mask.split();

        let ones_edges = subtrahend.ones_edges();

        for smskx in sub_masks.iter() {
            if ones_edges.bitwise_and(smskx).is_low() {
                ret.push(self.set_to_ones(smskx));
            } else {
                ret.push(self.set_to_zeros(smskx));
            }
        }
        ret
    }

    /// Return true if there is an intersection of two RegionStorCorrs.
    pub fn intersects(&self, other: &Self) -> bool {
        debug_assert!(self.is_congruent(other));

        self.distance(other) == 0
    }

    /// Return true if corresponding regions in two vectors have the same number of bits.
    pub fn is_congruent(&self, other: &impl tools::CorrespondingItems) -> bool {
        self.num_bits_vec() == other.num_bits_vec()
    }

    /// Return a vector of corresponding num_bits.
    pub fn num_bits_vec(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::with_capacity(self.len());
        for regx in self.iter() {
            ret_vec.push(regx.num_bits());
        }
        ret_vec
    }

    /// Return a single-region RegionsCorr from a multi-region RegionsCorr,
    /// where the sum of the regions num_bits is LE a single Bitint.
    pub fn combine(&self) -> Self {
        assert!(self.len() > 1);

        let mut regx = self.regions[0].clone();
        for regy in self.regions.iter().skip(1) {
            regx = regx.combine(regy);
        }

        let mut ret_regs = Self::with_capacity(1);
        ret_regs.push(regx);
        ret_regs
    }

    /// Set region bit positions to X, given a mask.
    fn set_to_x(&self, masks: &MasksCorr) -> Self {
        let mut ret_regs = Self::with_capacity(self.len());
        for (regx, mskx) in self.iter().zip(masks.iter()) {
            ret_regs.push(regx.set_to_x(mskx));
        }
        ret_regs
    }

    /// Set region bit positions to 0, given a mask.
    fn set_to_zeros(&self, masks: &MasksCorr) -> Self {
        let mut ret_regs = Self::with_capacity(self.len());
        for (regx, mskx) in self.iter().zip(masks.iter()) {
            ret_regs.push(regx.set_to_zeros(mskx));
        }
        ret_regs
    }

    /// Set region bit positions to 1, given a mask.
    fn set_to_ones(&self, masks: &MasksCorr) -> Self {
        let mut ret_regs = Self::with_capacity(self.len());
        for (regx, mskx) in self.iter().zip(masks.iter()) {
            ret_regs.push(regx.set_to_ones(mskx));
        }
        ret_regs
    }

    /// Return symmetrical_overlapping_regions for two adjacent RegionsCorrs.
    pub fn symmetrical_overlapping_regions(&self, other: &Self) -> Self {
        debug_assert!(self.is_congruent(other));

        let dif_mask = self.diff_edge_mask(other);
        assert!(dif_mask.num_one_bits() == 1);

        self.set_to_x(&dif_mask)
            .intersection(&other.set_to_x(&dif_mask))
            .expect("SNH")
    }

    /// Return a difference edge mask for two RegionsCorrs.
    pub fn diff_edge_mask(&self, other: &Self) -> MasksCorr {
        assert!(self.is_congruent(other));

        let mut ret_mc = MasksCorr::with_capacity(self.len());

        for (regx, regy) in self.iter().zip(other.iter()) {
            ret_mc.push(regx.diff_edge_mask(regy));
        }
        ret_mc
    }

    /// Return a difference edge mask for a RegionsCorrs and a StatesCorr.
    pub fn diff_edge_mask_states(&self, other: &StatesCorr) -> MasksCorr {
        assert!(self.is_congruent(other));

        let mut ret_mc = MasksCorr::with_capacity(self.len());

        for (regx, stay) in self.iter().zip(other.iter()) {
            ret_mc.push(regx.diff_edge_mask(stay));
        }
        ret_mc
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
impl tools::StrLen for RegionsCorr {
    fn strlen(&self) -> usize {
        2 + self.regions.strlen()
    }
}

impl tools::AvecRef for RegionsCorr {
    fn avec_ref(&self) -> &Vec<impl NumBits> {
        self.regions.avec_ref()
    }
}

impl tools::CorrespondingItems for RegionsCorr {
    fn num_bits_vec(&self) -> Vec<usize> {
        self.num_bits_vec()
    }
}

impl FromStr for RegionsCorr {
    type Err = String;
    /// Return a RegionsCorr instance, given a string representation.
    /// Like RC[], RC[r1010], or RC[r101, r1000].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("regionscorr::from_str: {str_in}");
        let rc_str = str_in.trim();

        if rc_str.is_empty() {
            return Err("RegionsCorr::from_str: Empty string?".to_string());
        }

        let mut rc_str2 = String::new();
        let mut last_chr = false;

        for (inx, chr) in rc_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "R" {
                    continue;
                } else {
                    return Err(format!(
                        "RegionsCorr::from_str: Invalid string, {rc_str} should start with RC["
                    ));
                }
            }
            if inx == 1 {
                if chr == "C" {
                    continue;
                } else {
                    return Err(format!(
                        "RegionsCorr::from_str: Invalid string, {rc_str} should start with RC["
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
                    "RegionsCorr::from_str: Invalid string, {rc_str} should end with ]"
                ));
            }
            rc_str2.push_str(chr);
        }
        if !last_chr {
            return Err(format!(
                "RegionsCorr::from_str: Invalid string, {rc_str} should end with ]"
            ));
        }

        let regions = RegionStore::from_str(&rc_str2)?;

        Ok(Self { regions })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tools::StrLen;

    #[test]
    fn strlen() -> Result<(), String> {
        let regs1_str = "RC[r0x00, r1x1x]";
        let regs1 = RegionsCorr::from_str(&regs1_str)?;
        let len = regs1.strlen();
        println!("len of {regs1_str} = {len}");

        assert!(len == regs1_str.len());

        Ok(())
    }

    #[test]
    fn is_superset_states() -> Result<(), String> {
        let regstr1 = RegionsCorr::from_str("RC[r0x00, r1x1x]")?;

        let stas = StatesCorr::from_str("SC[s0001, s1000]")?;

        println!("regstr1 {}", regstr1);
        println!("stas    {}", stas);

        assert!(!regstr1.is_superset_states(&stas));

        let stas2 = StatesCorr::from_str("SC[s0100, s1010]")?;

        println!("regstr1 {}", regstr1);
        println!("stas2   {}", stas2);

        assert!(regstr1.is_superset_states(&stas2));

        Ok(())
    }

    #[test]
    fn distance_states() -> Result<(), String> {
        let regstr1 = RegionsCorr::from_str("RC[r0x00, r1x1x]")?;

        let stas = StatesCorr::from_str("SC[s0001, s1000]")?;

        let dist = regstr1.distance_states(&stas);
        println!("Distance = {dist}");

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let regstr1 = RegionsCorr::from_str("RC[r0x0x, r1x0x]")?;

        let regstr2 = RegionsCorr::from_str("RC[r0101, r1x01]")?;

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);

        assert!(!regstr1.is_subset_of(&regstr2));
        assert!(regstr2.is_subset_of(&regstr1));

        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let regstr1 = RegionsCorr::from_str("RC[r0x0x, r1x0x]")?;

        let regstr2 = RegionsCorr::from_str("RC[r0101, r1x01]")?;

        println!("regstr1 {}", regstr1);
        println!("regstr2 {}", regstr2);

        assert!(regstr1.is_superset_of(&regstr2));
        assert!(!regstr2.is_superset_of(&regstr1));

        Ok(())
    }

    #[test]
    fn distance() -> Result<(), String> {
        let regstr1 = RegionsCorr::from_str("RC[r0x0x, r1x00]")?;

        let regstr2 = RegionsCorr::from_str("RC[r11x1, r1xx1]")?;

        let dist = regstr1.distance(&regstr2);
        println!("dist {}", dist);

        assert!(dist == 2);
        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let regstr1 = RegionsCorr::from_str("RC[r0x0x, r1x00]")?;

        let regstr2 = RegionsCorr::from_str("RC[r11x1, r1xx1]")?;

        let intb = regstr1.intersects(&regstr2);
        println!("{regstr1} intersects {regstr2} is {intb}");

        assert!(!intb);

        let regstr3 = RegionsCorr::from_str("RC[r010x, rx10x]")?;

        let intb = regstr1.intersects(&regstr3);
        println!("{regstr1} intersects {regstr3} is {intb}");

        assert!(intb);

        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let regstr1 = RegionsCorr::from_str("RC[rx_01xx, rx_01xx]")?;
        let regstr2 = RegionsCorr::from_str("RC[rx_0101, r1_xxx0]")?;

        let regstrvec = regstr1.subtract(&regstr2);
        println!("{regstr1} subtract");
        println!("{regstr2} is: ");
        for rscx in regstrvec.iter() {
            println!("{rscx}");
        }

        assert!(regstrvec.len() == 4);

        assert!(regstrvec.contains(&RegionsCorr::from_str("RC[rx_01x0, rx_01xx]")?));
        assert!(regstrvec.contains(&RegionsCorr::from_str("RC[rx_011x, rx_01xx]")?));
        assert!(regstrvec.contains(&RegionsCorr::from_str("RC[rx_01xx, rx_01x1]")?));
        assert!(regstrvec.contains(&RegionsCorr::from_str("RC[rx_01xx, r0_01xx]")?));

        // Test the equivalent one-region subtraction.
        let regstr1 = RegionsCorr::from_str("RC[rx0_1xxx_01xx]")?;
        let regstr2 = RegionsCorr::from_str("RC[rx0_1011_xxx0]")?;

        let regstrvec = regstr1.subtract(&regstr2);
        println!("{regstr1} subtract");
        println!("{regstr2} is: ");
        for rscx in regstrvec.iter() {
            println!("{rscx}");
        }
        assert!(regstrvec.contains(&RegionsCorr::from_str("RC[rx_01x0_x_01xx]")?));
        assert!(regstrvec.contains(&RegionsCorr::from_str("RC[rx_011x_x_01xx]")?));
        assert!(regstrvec.contains(&RegionsCorr::from_str("RC[rx_01xx_x_01x1]")?));
        assert!(regstrvec.contains(&RegionsCorr::from_str("RC[rx_01xx_0_01xx]")?));

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let regst1 = RegionsCorr::from_str("RC[]")?;
        println!("regst1 {regst1}");
        assert!(format!("{regst1}") == "RC[]");

        let regst2 = RegionsCorr::from_str("RC[r1010]")?;
        println!("regst2 {regst2}");
        assert!(format!("{regst2}") == "RC[r1010]");

        let regst3_str = "RC[r1010, r1111]";
        let regst3 = RegionsCorr::from_str(&regst3_str)?;
        println!("regst3 {regst3}");
        assert!(format!("{regst3}") == regst3_str);

        Ok(())
    }

    #[test]
    fn combine() -> Result<(), String> {
        let regst1 = RegionsCorr::from_str("RC[r10, r11, r01]")?;
        let regst2 = regst1.combine();
        println!("{regst1} combined = {regst2}");
        assert!(regst2 == RegionsCorr::from_str("RC[r10_1101]")?);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn symmetrical_overlapping_regions() -> Result<(), String> {
        let regsc1 = RegionsCorr::from_str("RC[rX10X]")?;
        let regsc2 = RegionsCorr::from_str("RC[r1X1X]")?;

        let regsc3 = regsc1.symmetrical_overlapping_regions(&regsc2);
        println!("{regsc1} symmetrical_overlapping_regions {regsc2} = {regsc3}");
        assert!(regsc3 == RegionsCorr::from_str("RC[r11XX]")?);

        Ok(())
    }

    // Test intersection, and equivalence to region intersection.
    #[test]
    fn eqv_intersection() -> Result<(), String> {
        let rc1 = RegionsCorr::from_str("RC[rX1, r0X]")?;
        let rc2 = RegionsCorr::from_str("RC[r1X, rX0]")?;
        let rc3 = rc1.intersection(&rc2).unwrap();
        println!("{rc1} intersection {rc2} = {rc3}");
        let rc4 = rc3.combine();
        let mut rc5 = RegionsCorr::with_capacity(1);
        rc5.push(
            SomeRegion::from_str("rX10X")?
                .intersection(&SomeRegion::from_str("r1XX0")?)
                .unwrap(),
        );
        println!("{rc4} should be eq {rc5}");
        assert!(rc4 == rc5);

        Ok(())
    }

    // Test union, and equivalence to region union.
    #[test]
    fn eqv_union() -> Result<(), String> {
        let rc1 = RegionsCorr::from_str("RC[rX1, r0X]")?;
        let rc2 = RegionsCorr::from_str("RC[r1X, rX0]")?;
        let rc3 = rc1.union(&rc2);
        println!("{rc1} union {rc2} = {rc3}");
        let rc4 = rc3.combine();
        let mut rc5 = RegionsCorr::with_capacity(1);
        rc5.push(SomeRegion::from_str("rX10X")?.union(&SomeRegion::from_str("r1XX0")?));
        println!("{rc4} should be eq {rc5}");
        assert!(rc4 == rc5);

        Ok(())
    }
}
