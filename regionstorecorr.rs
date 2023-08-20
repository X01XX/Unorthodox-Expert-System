//! The RegionStoreCorr, a vector of SomeRegion structs corresponding to domains.
//!
//! The regions in a RegionStoreCorr will have the same number of bits used by the corresponding domain,
//! and different domains can use different numbers of bits.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::removeunordered;
use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

impl fmt::Display for RegionStoreCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
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
    /// Return a new, empty, RegionStoreCorr.
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

    /// Return the expected length of a string representing a RegionStoreCorr.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        let alen = self.avec.len();

        if alen > 0 {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string representing a RegionStoreCorr.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::with_capacity(self.formatted_string_length());
        rc_str.push('[');

        for (inx, regx) in self.avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &regx));
        }

        rc_str.push(']');

        rc_str
    }

    /// Return True if a RegionStoreCorr is a superset of all corresponding states in a SomeState vector.
    /// Used in optimal regionstore calculations.
    pub fn is_superset_states(&self, stas: &[&SomeState]) -> bool {
        debug_assert!(self.len() == stas.len());

        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of_state(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Calculate the distance between a RegionStoreCorr and the current state.
    pub fn distance_states(&self, stas: &[&SomeState]) -> usize {
        debug_assert!(self.len() == stas.len());

        let mut dist = 0;
        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of_state(y) {
            } else {
                dist += x.distance_state(y);
            }
        }

        dist
    }

    /// Calculate the distance between a RegionStoreCorr and another.
    pub fn distance(&self, regs: &Self) -> usize {
        debug_assert!(self.len() == regs.len());

        let mut dist = 0;
        for (x, y) in self.iter().zip(regs.iter()) {
            if x.is_superset_of(y) {
            } else {
                dist += x.distance(y);
            }
        }

        dist
    }

    /// Return True if a RegionStoreCorr is a superset of another RSC.
    pub fn is_superset_of(&self, other: &RegionStoreCorr) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_superset_of(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return a vector of difference masks for two RegionStoreCorrs.
    pub fn diff_masks(&self, other: &RegionStoreCorr) -> Vec<SomeMask> {
        debug_assert!(self.len() == other.len());

        let mut ret = Vec::<SomeMask>::with_capacity(self.len());

        for (x, y) in self.iter().zip(other.iter()) {
            ret.push(x.diff_mask(y));
        }

        ret
    }

    /// Return a RegionStoreCorr with certain bit positions set to X.
    pub fn set_to_x(&self, other: &Vec<SomeMask>) -> Self {
        debug_assert!(self.len() == other.len());

        let mut ret = Self::new(Vec::<SomeRegion>::with_capacity(self.len()));

        for (x, y) in self.iter().zip(other.iter()) {
            ret.push(x.set_to_x(y));
        }

        ret
    }

    /// Return the intersection, if any, of two RegionStoreCorrs.
    pub fn intersection(&self, other: &RegionStoreCorr) -> Option<RegionStoreCorr> {
        debug_assert!(self.len() == other.len());

        let mut ret = RegionStoreCorr::with_capacity(self.len());

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

    /// Return the union, of two RegionStoreCorrs.
    pub fn union(&self, other: &RegionStoreCorr) -> RegionStoreCorr {
        debug_assert!(self.len() == other.len());

        let mut ret = RegionStoreCorr::with_capacity(self.len());

        for (x, y) in self.iter().zip(other.iter()) {
            ret.push(x.union(y));
        }

        ret
    }

    /// Return true if there is an intersection of corresponding regions, of two RegionStoreCorrs.
    pub fn intersects(&self, other: &RegionStoreCorr) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if !x.intersects(y) {
                return false;
            }
        }
        true
    }

    /// Return true if RegionStoreCorr is a subset of another RegionStoreCorr.
    /// Used in optimal regionstore calculations.
    pub fn is_subset_of(&self, other: &RegionStoreCorr) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_subset_of(y) {
            } else {
                return false;
            }
        }
        true
    }

    /// Return true if at least one corresponding pair in two ReagionStoreCorrs is adjacent,
    /// while other corresponding pairs are adjacent or intersect.
    pub fn is_adjacent(&self, other: &RegionStoreCorr) -> bool {
        debug_assert!(self.len() == other.len());

        let mut num_dif = 0;
        for (regx, regy) in self.iter().zip(other.iter()) {
            let dif = regx.diff_mask(regy).num_one_bits();

            if dif > 1 {
                return false;
            }
            num_dif += dif;
        }
        num_dif > 0
    }

    /// Return a RegionStoreCorr states ref vector.
    pub fn states(&self) -> Vec<&SomeState> {
        let mut stas = Vec::<&SomeState>::with_capacity(self.len());
        for regx in self.iter() {
            stas.push(regx.state1());
        }
        stas
    }

    /// Return a string representing a vector of RegionStoreCorrs.
    pub fn vec_string(avec: &[RegionStoreCorr]) -> String {
        let mut rc_str = String::new();
        rc_str.push('[');

        for (inx, regx) in avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", regx));
        }

        rc_str.push(']');

        rc_str
    }

    /// Return a string representing a vector of RegionStoreCorr references.
    pub fn vec_ref_string(avec: &[&RegionStoreCorr]) -> String {
        let mut rc_str = String::new();
        rc_str.push('[');

        for (inx, regx) in avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", regx));
        }

        rc_str.push(']');

        rc_str
    }

    /// Split corresponding regions by intersections, producing a result where each region is a subset
    /// of any intersecting original regions. All parts of the original regions should be represented in the
    /// result.
    pub fn vec_ref_split_to_subsets(rs_vec: &[&RegionStoreCorr]) -> Vec<RegionStoreCorr> {
        assert!(!rs_vec.is_empty());

        if rs_vec.len() == 1 {
            return vec![rs_vec[0].clone()];
        }

        // Init return vector of regions, subuset of all intersecting original regions.
        let mut ret_vec = Vec::<RegionStoreCorr>::new();

        let mut each_reg_set = Vec::<RegionStore>::with_capacity(rs_vec.len());

        for inx in 0..rs_vec[0].len() {
            let mut tmp_vec = RegionStore::with_capacity(rs_vec.len());
            for rsx in rs_vec.iter() {
                tmp_vec.push(rsx[inx].clone());
            }

            each_reg_set.push(tmp_vec);
        }

        // Get subsets for each domain.
        let mut each_set_subs = Vec::<RegionStore>::with_capacity(rs_vec.len());
        for setx in each_reg_set.iter() {
            //println!("set: {}", SomeRegion::vec_ref_string(setx));
            each_set_subs.push(setx.split_to_subsets());
        }

        // Assemble refs for each domain subsets.
        let mut refs = Vec::<Vec<&SomeRegion>>::with_capacity(rs_vec.len());
        for setx in each_set_subs.iter() {
            let mut refs_tmp = Vec::<&SomeRegion>::with_capacity(setx.len());
            for regx in setx.iter() {
                refs_tmp.push(regx);
            }
            refs.push(refs_tmp);
        }

        let options = tools::any_one_of_each(&refs);
        //println!("num options returned: {}", options.len());

        for opx in options.iter() {
            let mut regs = Vec::<SomeRegion>::with_capacity(opx.len());
            for refx in opx.iter() {
                regs.push((*refx).clone());
            }
            ret_vec.push(RegionStoreCorr::new(regs));
        }
        //println!("num RegionStoreCorr is {}", ret_vec.len());
        ret_vec
    }

    /// Subtract a RegionStoreCorr vector from another RegionStoreCorr vec.
    pub fn vec_subtract(
        minuend: &Vec<RegionStoreCorr>,
        subtrahend: &[&RegionStoreCorr],
    ) -> Vec<RegionStoreCorr> {
        let mut ret_str = Vec::<RegionStoreCorr>::with_capacity(minuend.len());
        for regsx in minuend.iter() {
            ret_str.push(regsx.clone());
        }

        for regx in subtrahend.iter() {
            if RegionStoreCorr::vec_any_intersection(&ret_str, regx) {
                ret_str = RegionStoreCorr::vec_subtract_regionstorecorr(&ret_str, regx);
            }
        }
        ret_str
    }

    /// Return true if any region is a superset, or equal, to a RegionStoreCorr.
    pub fn vec_any_superset_of(avec: &[RegionStoreCorr], regcr: &RegionStoreCorr) -> bool {
        tools::vec_contains(avec, RegionStoreCorr::is_superset_of, regcr)
    }

    /// Add a regionstorecorr, removing subset regionstorecorr.
    pub fn vec_push_nosubs(avec: &mut Vec<RegionStoreCorr>, reg: RegionStoreCorr) -> bool {
        // Check for supersets, which probably is an error
        if RegionStoreCorr::vec_any_superset_of(avec, &reg) {
            //println!("skipped adding region {}, a superset exists in {}", reg, self);
            return false;
        }

        // Identify subsets.
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in avec.iter().enumerate() {
            if regx.is_subset_of(&reg) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in descending index order.
        for inx in rmvec.iter().rev() {
            removeunordered::remove_unordered(avec, *inx);
        }

        avec.push(reg);

        true
    }

    /// Return true if any regionstorecorr vector intersects a given regionstorcorr.
    pub fn vec_any_intersection(avec: &[RegionStoreCorr], reg: &RegionStoreCorr) -> bool {
        tools::vec_contains(avec, RegionStoreCorr::intersects, reg)
    }

    /// Subtract a RegionStoreCorr from a RegionStoreCorr vector.
    pub fn vec_subtract_regionstorecorr(
        minuend: &[RegionStoreCorr],
        subtrahend: &RegionStoreCorr,
    ) -> Vec<RegionStoreCorr> {
        let mut ret_str = Vec::<RegionStoreCorr>::new();

        for regy in minuend.iter() {
            if subtrahend.intersects(regy) {
                for regz in regy.subtract(subtrahend) {
                    RegionStoreCorr::vec_push_nosubs(&mut ret_str, regz);
                }
            } else {
                RegionStoreCorr::vec_push_nosubs(&mut ret_str, regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Return self - a given RegionStoreCorr.
    pub fn subtract(&self, subtrahend: &RegionStoreCorr) -> Vec<RegionStoreCorr> {
        let mut ret = Vec::<RegionStoreCorr>::new();

        if self.is_subset_of(subtrahend) {
            return ret;
        }

        if !self.intersects(subtrahend) {
            ret.push(self.clone());
            return ret;
        }

        for (inx, (regx, regy)) in self.iter().zip(subtrahend.iter()).enumerate() {
            let xb_msk = regx.x_mask().bitwise_and(&regy.non_x_mask());
            if xb_msk.is_low() {
                continue;
            }
            // At least one X over non-X bit found.

            // Isolate each X over non-X bit.
            let single_bits = xb_msk.split();

            // Generate a new RegionStoreCorr for each isolated bit.
            for sbitx in single_bits.iter() {
                // Alter one X bit in self/regx to the opposite of the corresponding non-X bit in subtrahend/regy.
                let regz = if sbitx.bitwise_and(regy.state1()).is_low() {
                    // Other/regy bit is zero, in regy.state1 (and regy.state2, since its non-X).
                    regx.set_to_ones(sbitx)
                } else {
                    regx.set_to_zeros(sbitx)
                };

                // Copy self, except for one region with one bit changed.
                let mut one_result = RegionStoreCorr::with_capacity(self.len());
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

    /// Return the adjacent part of two RegionStoreCorr.
    /// Presumably, at least one pair of corresponding regions will be adjacent, calc the adjacent part.
    /// If a pair of corresponding regions intersect, calc the intersection.
    pub fn adjacent_part(&self, other: &Self) -> Self {
        assert!(self.is_adjacent(other));

        let mut ret_corr = RegionStoreCorr::new(Vec::<SomeRegion>::with_capacity(self.len()));

        for (reg_s, reg_o) in self.iter().zip(other.iter()) {
            if reg_s.is_adjacent(reg_o) {
                ret_corr.push(reg_s.adjacent_part(reg_o));
            } else if let Some(reg_int) = reg_s.intersection(reg_o) {
                ret_corr.push(reg_int);
            } else {
                panic!("SNH");
            }
        }

        ret_corr
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;

    #[test]
    fn is_adjacent() -> Result<(), String> {
        let tmp_sta1 = SomeState::new(SomeBits::new(1));
        let tmp_reg1 = SomeRegion::new(vec![tmp_sta1.clone()]);

        let tmp_sta2 = SomeState::new(SomeBits::new(2));
        let tmp_reg2 = SomeRegion::new(vec![tmp_sta2.clone()]);

        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(tmp_reg1.new_from_string("r0000_x10x")?);
        regstr1.push(tmp_reg2.new_from_string("r0000_000x_0000_000x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(tmp_reg1.new_from_string("r0000_x11x")?);
        regstr2.push(tmp_reg2.new_from_string("r0000_0000_0000_000x")?);

        assert!(regstr1.is_adjacent(&regstr2));
        assert!(!regstr1.is_adjacent(&regstr1));

        Ok(())
    }

    #[test]
    fn intersects() -> Result<(), String> {
        let tmp_sta1 = SomeState::new(SomeBits::new(1));
        let tmp_reg1 = SomeRegion::new(vec![tmp_sta1.clone()]);

        let tmp_sta2 = SomeState::new(SomeBits::new(2));
        let tmp_reg2 = SomeRegion::new(vec![tmp_sta2.clone()]);

        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(tmp_reg1.new_from_string("r0000_x10x")?);
        regstr1.push(tmp_reg2.new_from_string("r0000_000x_0000_000x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(tmp_reg1.new_from_string("r0000_1x01")?);
        regstr2.push(tmp_reg2.new_from_string("r0000_0000_0000_00x1")?);

        assert!(regstr1.intersects(&regstr2));

        let mut regstr3 = RegionStoreCorr::with_capacity(2);
        regstr3.push(tmp_reg1.new_from_string("r0000_x11x")?);
        regstr3.push(tmp_reg2.new_from_string("r0000_0000_0000_000x")?);

        assert!(!regstr1.intersects(&regstr3));

        Ok(())
    }

    #[test]
    fn test_subtract() -> Result<(), String> {
        let tmp_sta1 = SomeState::new(SomeBits::new(1));
        let tmp_reg1 = SomeRegion::new(vec![tmp_sta1.clone()]);

        let tmp_sta2 = SomeState::new(SomeBits::new(2));
        let tmp_reg2 = SomeRegion::new(vec![tmp_sta2.clone()]);

        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(tmp_reg1.new_from_string("r0000_x10x")?);
        regstr1.push(tmp_reg2.new_from_string("r0000_000x_0000_000x")?);

        // Test subtracting a superset.
        let result1 = regstr1.subtract(&regstr1);
        assert!(result1.is_empty());

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(tmp_reg1.new_from_string("r0000_0xx1")?);
        regstr2.push(tmp_reg2.new_from_string("r0000_00x0_0000_00x1")?);

        // Test subtracting an intersection.
        let result2 = regstr1.subtract(&regstr2);
        println!("{} minus \n{}\n", regstr1, regstr2);
        for corrx in result2.iter() {
            println!("{corrx}");
        }
        assert!(result2.len() == 4);
        assert!(tools::vec_contains(
            &result2,
            RegionStoreCorr::eq,
            &RegionStoreCorr::new(vec!(
                tmp_reg1.new_from_string("r0000_110x")?,
                tmp_reg2.new_from_string("r0000_000x_0000_000x")?
            )),
        ));
        assert!(tools::vec_contains(
            &result2,
            RegionStoreCorr::eq,
            &RegionStoreCorr::new(vec!(
                tmp_reg1.new_from_string("r0000_x100")?,
                tmp_reg2.new_from_string("r0000_000x_0000_000x")?
            )),
        ));
        assert!(tools::vec_contains(
            &result2,
            RegionStoreCorr::eq,
            &RegionStoreCorr::new(vec!(
                tmp_reg1.new_from_string("r0000_x10x")?,
                tmp_reg2.new_from_string("r0000_0001_0000_000x")?
            )),
        ));
        assert!(tools::vec_contains(
            &result2,
            RegionStoreCorr::eq,
            &RegionStoreCorr::new(vec!(
                tmp_reg1.new_from_string("r0000_x10x")?,
                tmp_reg2.new_from_string("r0000_000x_0000_0000")?
            )),
        ));

        Ok(())
    }

    #[test]
    fn test_vec_subtract() -> Result<(), String> {
        let tmp_sta1 = SomeState::new(SomeBits::new(1));
        let tmp_reg1 = SomeRegion::new(vec![tmp_sta1.clone()]);

        let tmp_sta2 = SomeState::new(SomeBits::new(2));
        let tmp_reg2 = SomeRegion::new(vec![tmp_sta2.clone()]);

        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(tmp_reg1.new_from_string("r0000_x10x")?);
        regstr1.push(tmp_reg2.new_from_string("r0000_000x_0000_000x")?);

        let mut vec1 = vec![regstr1.clone()];

        // Test subtracting a superset.
        let result1 = RegionStoreCorr::vec_subtract_regionstorecorr(&vec1, &regstr1);
        assert!(result1.is_empty());

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(tmp_reg1.new_from_string("r0000_0xx1")?);
        regstr2.push(tmp_reg2.new_from_string("r0000_00x0_0000_00x1")?);

        // Test subtracting an intersection.
        let result2 = RegionStoreCorr::vec_subtract_regionstorecorr(&vec1, &regstr2);

        println!(
            "{} minus \n{}\n",
            RegionStoreCorr::vec_string(&vec1),
            regstr2
        );
        for corrx in result2.iter() {
            println!("{corrx}");
        }
        assert!(result2.len() == 4);
        assert!(tools::vec_contains(
            &result2,
            RegionStoreCorr::eq,
            &RegionStoreCorr::new(vec!(
                tmp_reg1.new_from_string("r0000_110x")?,
                tmp_reg2.new_from_string("r0000_000x_0000_000x")?
            )),
        ));
        assert!(tools::vec_contains(
            &result2,
            RegionStoreCorr::eq,
            &RegionStoreCorr::new(vec!(
                tmp_reg1.new_from_string("r0000_x100")?,
                tmp_reg2.new_from_string("r0000_000x_0000_000x")?
            )),
        ));
        assert!(tools::vec_contains(
            &result2,
            RegionStoreCorr::eq,
            &RegionStoreCorr::new(vec!(
                tmp_reg1.new_from_string("r0000_x10x")?,
                tmp_reg2.new_from_string("r0000_0001_0000_000x")?
            )),
        ));
        assert!(tools::vec_contains(
            &result2,
            RegionStoreCorr::eq,
            &RegionStoreCorr::new(vec!(
                tmp_reg1.new_from_string("r0000_x10x")?,
                tmp_reg2.new_from_string("r0000_000x_0000_0000")?
            )),
        ));

        vec1.push(RegionStoreCorr::new(vec![
            tmp_reg1.new_from_string("r0000_1110")?,
            tmp_reg2.new_from_string("r0000_000x_0000_0000")?,
        ]));

        // Test subtracting an intersection, while non-intersection passes through.
        let result3 = RegionStoreCorr::vec_subtract_regionstorecorr(&vec1, &regstr2);

        println!(
            "\n{} minus \n{}\n",
            RegionStoreCorr::vec_string(&vec1),
            regstr2
        );
        for corrx in result3.iter() {
            println!("{corrx}");
        }
        assert!(result3.len() == 5);
        assert!(tools::vec_contains(
            &result3,
            RegionStoreCorr::eq,
            &RegionStoreCorr::new(vec!(
                tmp_reg1.new_from_string("r0000_1110")?,
                tmp_reg2.new_from_string("r0000_000x_0000_0000")?
            )),
        ));

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn test_vec_split_to_subsets() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        let mut rs_vec = Vec::<&RegionStoreCorr>::with_capacity(1);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(tmp_reg.new_from_string("rx10x")?);
        rs_vec.push(&regstr1);

        println!("Initial1: {}", RegionStoreCorr::vec_ref_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_ref_split_to_subsets(&rs_vec);

        println!("Result1: {}", RegionStoreCorr::vec_string(&rslt));

        assert!(rslt.len() == 1);
        assert!(rslt.contains(&regstr1));

        let mut rs_vec = Vec::<&RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(tmp_reg.new_from_string("rx10x")?);
        rs_vec.push(&regstr1);
        rs_vec.push(&regstr1);

        println!("Initial2: {}", RegionStoreCorr::vec_ref_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_ref_split_to_subsets(&rs_vec);

        println!("Result2: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 1);
        assert!(rslt.contains(&regstr1));

        let mut rs_vec = Vec::<&RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(tmp_reg.new_from_string("rx10x")?);
        rs_vec.push(&regstr1);

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(tmp_reg.new_from_string("r010x")?);
        rs_vec.push(&regstr2);

        println!("Initial3: {}", RegionStoreCorr::vec_ref_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_ref_split_to_subsets(&rs_vec);

        println!("Result3: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 2);
        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r110x")?
        ])));
        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r010x")?
        ])));

        let mut rs_vec = Vec::<&RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(tmp_reg.new_from_string("rx10x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(tmp_reg.new_from_string("r010x")?);

        rs_vec.push(&regstr2);
        rs_vec.push(&regstr1);

        println!("Initial4: {}", RegionStoreCorr::vec_ref_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_ref_split_to_subsets(&rs_vec);

        println!("Result4: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 2);
        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r110x")?
        ])));
        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r010x")?
        ])));

        let mut rs_vec = Vec::<&RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(tmp_reg.new_from_string("rx10x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(tmp_reg.new_from_string("r1xx1")?);

        rs_vec.push(&regstr2);
        rs_vec.push(&regstr1);

        println!("Initial5: {}", RegionStoreCorr::vec_ref_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_ref_split_to_subsets(&rs_vec);

        println!("Result5: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 5);
        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("rx100")?
        ])));
        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r010x")?
        ])));
        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r1101")?
        ])));
        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r10x1")?
        ])));
        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r1101")?
        ])));

        let mut rs_vec = Vec::<&RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(tmp_reg.new_from_string("rx10x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(tmp_reg.new_from_string("r01xx")?);

        let mut regstr3 = RegionStoreCorr::with_capacity(1);
        regstr3.push(tmp_reg.new_from_string("rxxx1")?);

        rs_vec.push(&regstr2);
        rs_vec.push(&regstr1);
        rs_vec.push(&regstr3);

        println!("Initial6: {}", RegionStoreCorr::vec_ref_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_ref_split_to_subsets(&rs_vec);

        println!("Result6: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 8);

        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("rx0x1")?
        ])));

        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r1100")?
        ])));

        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r1101")?
        ])));

        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r0110")?
        ])));

        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r0111")?
        ])));

        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r0100")?
        ])));

        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r0101")?
        ])));

        assert!(rslt.contains(&RegionStoreCorr::new(vec![
            tmp_reg.new_from_string("r1x11")?
        ])));
        Ok(())
    }
}
