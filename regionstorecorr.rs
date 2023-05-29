//! The RegionStoreCorr, a vector of SomeRegion structs corresponding to domains.
//!
//! The regions in a RegionStoreCorr will have the same number of bits used by the corresponding domain,
//! and different domains can use different numbers of bits.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::removeunordered;
use crate::state::SomeState;

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
        if self.len() != other.len() {
            return false;
        }
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

    /// Return the sum of distances between corresponding regions and states.
    /// Used in optimal regionstore calculations.
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

    /// Return a vector of masks representing X/non-x bit positions of self / other.
    pub fn x_masks(&self, other: &RegionStoreCorr) -> Vec<SomeMask> {
        debug_assert!(self.len() == other.len());

        let mut ret_msks = Vec::<SomeMask>::with_capacity(self.len());

        for (x, b) in self.iter().zip(other.iter()) {
            ret_msks.push(x.x_mask().bitwise_and(&b.x_mask().bitwise_not()));
        }

        ret_msks
    }

    /// Return a vector of masks representing edge bit positions.
    pub fn edge_masks(&self) -> Vec<SomeMask> {
        let mut ret_msks = Vec::<SomeMask>::with_capacity(self.len());

        for regx in self.iter() {
            ret_msks.push(regx.edge_mask());
        }

        ret_msks
    }

    /// Subtract RegionStoreCorr from another.
    pub fn subtract(&self, other: &RegionStoreCorr) -> Vec<RegionStoreCorr> {
        debug_assert!(self.len() == other.len());

        let mut ret_str = Vec::<RegionStoreCorr>::new();

        let xb_msks = self.x_masks(other);

        for inx in 0..self.len() {
            if xb_msks[inx].is_low() {
                continue;
            }
            let single_bits = xb_msks[inx].split();

            for bitx in single_bits.iter() {
                let mut new_store = RegionStoreCorr::with_capacity(self.len());

                for iny in 0..self.len() {
                    if iny == inx {
                        match bitx.bitwise_and(&other[inx].state1).is_low() {
                            true =>
                            // process x/0
                            {
                                new_store.push(self[inx].set_to_ones(bitx))
                            }
                            false =>
                            // process x/1
                            {
                                new_store.push(self[inx].set_to_zeros(bitx))
                            }
                        };
                    } else {
                        new_store.push(self[iny].clone());
                    }
                }
                ret_str.push(new_store);
            }
        }
        ret_str
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

    /// Return true if a vector of RegionStoreCorrs contains a given regionstorecorr.
    #[allow(dead_code)]
    fn vec_contains(avec: &[RegionStoreCorr], ars: &RegionStoreCorr) -> bool {
        for rsx in avec {
            if rsx == ars {
                return true;
            }
        }
        false
    }

    /// Return true if any RegionStoreCorr is a superset, or equal, to a given RSC.
    pub fn vec_any_superset(rs_vec: &[RegionStoreCorr], reg_str: &RegionStoreCorr) -> bool {
        for regx in rs_vec {
            if regx.is_superset_of(reg_str) {
                return true;
            }
        }
        false
    }

    /// Add a RegionStoreCorr to a corresponding RSC vector.
    /// Skip add if there is any superset.
    /// Otherwise, remove any subset RegionStoreCorrs.
    pub fn vec_push_nosubs(rs_vec: &mut Vec<RegionStoreCorr>, reg_str: RegionStoreCorr) -> bool {
        // Check for supersets, which probably is an error
        if Self::vec_any_superset(rs_vec, &reg_str) {
            return false;
        }

        // Identify subsets.
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in rs_vec.iter().enumerate() {
            if regx.is_subset_of(&reg_str) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in descending index order.
        for inx in rmvec.iter().rev() {
            removeunordered::remove_unordered(rs_vec, *inx);
        }

        rs_vec.push(reg_str);

        true
    }

    /// Split corresponding RegionStoreCorrs by intersections, producing a result where each RSC is a subset
    /// of any intersecting original RSCs. All parts of the original RegionStores are accounted for in the
    /// result.
    pub fn vec_split_to_subsets(rs_vec: &[RegionStoreCorr]) -> Vec<RegionStoreCorr> {
        // Init return vector of RegionStores.
        let mut ret_vec = Vec::<RegionStoreCorr>::new();

        // Init temp vector, for RegionStore fragments.
        let mut tmp_vec = Vec::<RegionStoreCorr>::new();

        // Init vector to note indices of RegionStores that are split.
        // There may be a few duplicates, but that will not affect the use of the vector.
        let mut int_vec = Vec::<usize>::new();

        // Get initial fragments, from any two non-equal intersecting RegionStores.
        for inx in 0..(rs_vec.len() - 1) {
            for iny in (inx + 1)..rs_vec.len() {
                // Skip two equal regions.  If no other intersections, one will make it into the return vector.
                if rs_vec[inx] == rs_vec[iny] {
                    continue;
                }

                // If there is an intersection, split the RegionStores into fragments.
                if let Some(int) = rs_vec[inx].intersection(&rs_vec[iny]) {
                    int_vec.push(inx);
                    int_vec.push(iny);

                    for rsz in rs_vec[inx].subtract(&int).into_iter() {
                        Self::vec_push_nosubs(&mut tmp_vec, rsz);
                    }

                    for rsz in rs_vec[iny].subtract(&int).into_iter() {
                        Self::vec_push_nosubs(&mut tmp_vec, rsz);
                    }
                    Self::vec_push_nosubs(&mut tmp_vec, int);
                }
            }
        }
        // For RegionStores with no intersections, add to the return vector.
        for (iny, rsy) in rs_vec.iter().enumerate() {
            if !int_vec.contains(&iny) {
                Self::vec_push_nosubs(&mut ret_vec, rsy.clone());
            }
        }

        // Look for additional non-subset intersections.
        loop {
            // Init vector for next pass.
            let mut next_pass = Vec::<RegionStoreCorr>::new();

            // Check remaining fragments for additional intersections.
            // If no intersections are found, add the fragment to the return vector,
            // else add fragments of fragments to the next_pass vector.
            for rsy in tmp_vec.into_iter() {
                let mut split = false;

                // Check for intersecting RegionStore from original argument.
                for rsx in rs_vec.iter() {
                    if let Some(int) = rsy.intersection(rsx) {
                        // Skip if rsy if it is a subset, thats the end we are looking for.
                        if int == rsy {
                            continue;
                        }
                        // Split intersection into fragments.
                        // Add fragments to the next_pass vector.
                        for rsz in rsy.subtract(&int).into_iter() {
                            Self::vec_push_nosubs(&mut next_pass, rsz);
                        }
                        // Add the intersection to the next_pass vector.
                        Self::vec_push_nosubs(&mut next_pass, int);
                        split = true;
                    }
                } // next rsx
                  // If no intersectiosn, add the fragment to the return vector.
                if !split {
                    Self::vec_push_nosubs(&mut ret_vec, rsy);
                }
            } // next rsy

            // If no more fragments to check, return.
            if next_pass.is_empty() {
                return ret_vec;
            }
            // Set up next fragments to check.
            tmp_vec = next_pass;
        } // End loop
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

        println!("Result1: {}", RegionStoreCorr::vec_string(&rs_vec));

        assert!(rslt.len() == 1);
        assert!(rslt.contains(&regstr1));

        let mut rs_vec = Vec::<RegionStoreCorr>::with_capacity(2);

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);
        rs_vec.push(regstr1.clone());
        rs_vec.push(regstr1.clone());

        println!("Initial2: {}", RegionStoreCorr::vec_string(&rs_vec));

        let rslt = RegionStoreCorr::vec_split_to_subsets(&rs_vec);

        println!("Result2: {}", RegionStoreCorr::vec_string(&rs_vec));
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

        println!("Result3: {}", RegionStoreCorr::vec_string(&rs_vec));
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

        println!("Result4: {}", RegionStoreCorr::vec_string(&rs_vec));
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

        println!("Result5: {}", RegionStoreCorr::vec_string(&rs_vec));
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

        println!("Result6: {}", RegionStoreCorr::vec_string(&rs_vec));
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

    // for [r0000_x0x1, r0000_0000_0000_0x1x]
    //   - [r0000_1001, r0000_0000_0000_0110]
    // 0   [r0000_x011, r0000_0000_0000_0x1x]
    // 1   [r0000_00x1, r0000_0000_0000_0x1x]
    // 2   [r0000_x0x1, r0000_0000_0000_0x11]
    // 3   [r0000_x0x1, r0000_0000_0000_001x]
    #[test]
    fn subtract() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string(1, "rx0x1")?);
        regstr1.push(SomeRegion::new_from_string(2, "r0x1x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string(1, "r1001")?);
        regstr2.push(SomeRegion::new_from_string(2, "r0110")?);

        let subt = regstr1.subtract(&regstr2);

        println!("for {}", regstr1);
        println!("  - {}", regstr2);

        for (inx, rsx) in subt.iter().enumerate() {
            println!("{}   {}", inx, rsx);
        }
        assert!(subt.len() == 4);

        let mut regstr3 = RegionStoreCorr::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string(1, "r0000_x011")?);
        regstr3.push(SomeRegion::new_from_string(2, "r0000_0000_0000_0x1x")?);
        assert!(RegionStoreCorr::vec_contains(&subt, &regstr3));

        let mut regstr3 = RegionStoreCorr::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string(1, "r0000_00x1")?);
        regstr3.push(SomeRegion::new_from_string(2, "r0000_0000_0000_0x1x")?);
        assert!(RegionStoreCorr::vec_contains(&subt, &regstr3));

        let mut regstr3 = RegionStoreCorr::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string(1, "r0000_x0x1")?);
        regstr3.push(SomeRegion::new_from_string(2, "r0000_0000_0000_0x11")?);
        assert!(RegionStoreCorr::vec_contains(&subt, &regstr3));

        let mut regstr3 = RegionStoreCorr::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string(1, "r0000_x0x1")?);
        regstr3.push(SomeRegion::new_from_string(2, "r0000_0000_0000_001x")?);
        assert!(RegionStoreCorr::vec_contains(&subt, &regstr3));

        Ok(())
    }

    #[test]
    fn x_masks() -> Result<(), String> {
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string(1, "rx0x1")?);
        regstr1.push(SomeRegion::new_from_string(2, "r0x1x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string(1, "r1001")?);
        regstr2.push(SomeRegion::new_from_string(2, "r0110")?);

        let xb_msks = regstr1.x_masks(&regstr2);

        println!("msk0 = {}, msk1 = {}", xb_msks[0], xb_msks[1]);

        assert!(xb_msks[0] == SomeMask::new_from_string(1, "m0b1010")?);
        assert!(xb_msks[1] == SomeMask::new_from_string(2, "m0b0101")?);

        Ok(())
    }
}
