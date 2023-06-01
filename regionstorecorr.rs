//! The RegionStoreCorr, a vector of SomeRegion structs corresponding to domains.
//!
//! The regions in a RegionStoreCorr will have the same number of bits used by the corresponding domain,
//! and different domains can use different numbers of bits.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::state::SomeState;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
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

    /// Calculate the distance between the cuurent state anb a select region.
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

    /// Return a vector of masks representing edge bit positions.
    pub fn edge_masks(&self) -> Vec<SomeMask> {
        let mut ret_msks = Vec::<SomeMask>::with_capacity(self.len());

        for regx in self.iter() {
            ret_msks.push(regx.edge_mask());
        }

        ret_msks
    }

    /// Return a string representing a vector of regions.
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

    /// Split corresponding regions by intersections, producing a result where each region is a subset
    /// of any intersecting original regions. All parts of the original regions should be represented in the
    /// result.
    pub fn vec_split_to_subsets(rs_vec: &[RegionStoreCorr]) -> Vec<RegionStoreCorr> {
        assert!(!rs_vec.is_empty());

        if rs_vec.len() == 1 {
            return vec![rs_vec[0].clone()];
        }

        // Init return vector of regions, subuset of all intersecting original regions.
        let mut ret_vec = Vec::<RegionStoreCorr>::new();

        let mut each_reg_set = Vec::<Vec<&SomeRegion>>::with_capacity(rs_vec.len());

        for inx in 0..rs_vec[0].len() {
            let mut tmp_vec = Vec::<&SomeRegion>::with_capacity(rs_vec.len());
            for rsx in rs_vec.iter() {
                tmp_vec.push(&rsx[inx]);
            }

            each_reg_set.push(tmp_vec);
        }

        // Get subsets for each domain.
        let mut each_set_subs = Vec::<Vec<SomeRegion>>::with_capacity(rs_vec.len());
        for setx in each_reg_set.iter() {
            //println!("set: {}", SomeRegion::vec_ref_string(setx));
            each_set_subs.push(SomeRegion::vec_ref_split_to_subsets(setx));
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
} // End impl RegionStoreCorr.

impl Index<usize> for RegionStoreCorr {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.avec[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_split_to_subsets() -> Result<(), String> {
        let mut rs_vec = Vec::<RegionStoreCorr>::with_capacity(1);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);
        rs_vec.push(regstr1.clone());

        println!("Initial1: {}", RegionStoreCorr::vec_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_split_to_subsets(&rs_vec);

        println!("Result1: {}", RegionStoreCorr::vec_string(&rslt));

        assert!(rslt.len() == 1);
        assert!(rslt.contains(&regstr1));

        let mut rs_vec = Vec::<RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);
        rs_vec.push(regstr1.clone());
        rs_vec.push(regstr1.clone());

        println!("Initial2: {}", RegionStoreCorr::vec_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_split_to_subsets(&rs_vec);

        println!("Result2: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 1);
        assert!(rslt.contains(&regstr1));

        let mut rs_vec = Vec::<RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);
        rs_vec.push(regstr1.clone());

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string(1, "r010x")?);
        rs_vec.push(regstr2.clone());

        println!("Initial3: {}", RegionStoreCorr::vec_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_split_to_subsets(&rs_vec);

        println!("Result3: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 2);
        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r110x"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r010x"
            )?]))
        );

        let mut rs_vec = Vec::<RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string(1, "r010x")?);

        rs_vec.push(regstr2.clone());
        rs_vec.push(regstr1.clone());

        println!("Initial4: {}", RegionStoreCorr::vec_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_split_to_subsets(&rs_vec);

        println!("Result4: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 2);
        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r110x"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r010x"
            )?]))
        );

        let mut rs_vec = Vec::<RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string(1, "r1xx1")?);

        rs_vec.push(regstr2.clone());
        rs_vec.push(regstr1.clone());

        println!("Initial5: {}", RegionStoreCorr::vec_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_split_to_subsets(&rs_vec);

        println!("Result5: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 5);
        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "rx100"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r010x"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r1101"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r10x1"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r1101"
            )?]))
        );

        let mut rs_vec = Vec::<RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string(1, "r01xx")?);

        let mut regstr3 = RegionStoreCorr::with_capacity(1);
        regstr3.push(SomeRegion::new_from_string(1, "rxxx1")?);

        rs_vec.push(regstr2.clone());
        rs_vec.push(regstr1.clone());
        rs_vec.push(regstr3.clone());

        println!("Initial6: {}", RegionStoreCorr::vec_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_split_to_subsets(&rs_vec);

        println!("Result6: {}", RegionStoreCorr::vec_string(&rslt));
        assert!(rslt.len() == 8);

        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "rx0x1"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r1100"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r1101"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r0110"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r0111"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r0100"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r0101"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStoreCorr::new(vec![SomeRegion::new_from_string(
                1, "r1x11"
            )?]))
        );
        Ok(())
    }
}
