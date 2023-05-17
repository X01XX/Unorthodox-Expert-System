//! The RegionStore, a vector of SomeRegion structs.
//!
//! In the case of Optimal Regions, per domain, the regions in a RegionStore
//! may not have the same number of integers, hence the *_each functions.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::removeunordered;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for RegionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct RegionStore {
    /// A vector of regions.
    pub avec: Vec<SomeRegion>,
}

impl PartialEq for RegionStore {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for regx in &self.avec {
            if !other.contains(regx) {
                return false;
            }
        }
        true
    }
}
impl Eq for RegionStore {}

impl RegionStore {
    /// Return a new, empty, RegionStore.
    pub fn new(avec: Vec<SomeRegion>) -> Self {
        Self { avec }
    }

    /// Return a new RegionStore instance, empty, with a specified capacity.
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

    /// Return true if any region is a superset, or equal, to a region.
    pub fn any_superset_of(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if regx.is_superset_of(reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region is a subset, or equal, to a region.
    pub fn any_subset_of(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if regx.is_subset_of(reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region intersects a given region.
    pub fn any_intersection(&self, reg: &SomeRegion) -> bool {
        for regx in &self.avec {
            if regx.intersects(reg) {
                return true;
            }
        }
        false
    }

    /// Return true if any region is a superset of a state.
    pub fn any_superset_of_state(&self, sta: &SomeState) -> bool {
        for regx in &self.avec {
            if regx.is_superset_of_state(sta) {
                return true;
            }
        }
        false
    }

    /// Return vector of regions that are a superset of a given region.
    pub fn supersets_of(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        let mut ret_vec = Vec::<&SomeRegion>::new();
        for regx in &self.avec {
            if regx.is_superset_of(reg) {
                ret_vec.push(regx);
            }
        }
        ret_vec
    }

    /// Return vector of regions that intersect a given region.
    pub fn intersects_of(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        let mut ret_vec = Vec::<&SomeRegion>::new();
        for regx in &self.avec {
            if regx.intersects(reg) {
                ret_vec.push(regx);
            }
        }
        ret_vec
    }

    /// Return vector of regions that are a adjacent to a given region.
    pub fn adjacent_to(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        let mut ret_vec = Vec::<&SomeRegion>::new();
        for regx in &self.avec {
            if regx.is_adjacent(reg) {
                ret_vec.push(regx);
            }
        }
        ret_vec
    }

    /// Return vector of regions that are closest to a given region.
    pub fn closest_to(&self, reg: &SomeRegion) -> Vec<&SomeRegion> {
        let mut ret_vec = Vec::<&SomeRegion>::new();
        let mut min_distance = usize::MAX;
        for regx in &self.avec {
            let dist = regx.distance(reg);
            if dist < min_distance {
                min_distance = dist;
            }
        }
        for regx in &self.avec {
            if regx.distance(reg) == min_distance {
                ret_vec.push(regx);
            }
        }
        ret_vec
    }

    /// Return vector of regions that are a superset of a given state.
    pub fn supersets_of_state(&self, sta: &SomeState) -> Vec<&SomeRegion> {
        let mut ret_vec = Vec::<&SomeRegion>::new();
        for regx in &self.avec {
            if regx.is_superset_of_state(sta) {
                ret_vec.push(regx);
            }
        }
        ret_vec
    }

    /// Return the number of supersets of a state.
    pub fn number_supersets_of_state(&self, sta: &SomeState) -> usize {
        self.avec
            .iter()
            .map(|regx| usize::from(regx.is_superset_of_state(sta)))
            .sum()
    }

    /// Return true if a RegionStore contains a region.
    /// Regions may be equal, without matching states.
    /// A region formed by 0 and 5 will equal a region formed by 4 and 1.
    pub fn contains(&self, reg: &SomeRegion) -> bool {
        self.avec.contains(reg)
    }

    /// Return true if a given state is only in one region.
    pub fn state_in_1_region(&self, sta: &SomeState) -> bool {
        self.avec
            .iter()
            .map(|regx| usize::from(regx.is_superset_of_state(sta)))
            .sum::<usize>()
            == 1
    }

    /// Find and remove a given region.
    pub fn remove_region(&mut self, reg: &SomeRegion) -> bool {
        // Find a matching region
        let mut fnd = false;
        let mut inx = 0;

        for regx in &self.avec {
            if regx == reg {
                fnd = true;
                break;
            }
            inx += 1;
        }

        // Remove the region
        if fnd {
            removeunordered::remove_unordered(&mut self.avec, inx);
        }

        fnd
    }

    // Add a region, removing subset (and equal) regions.
    //  pub fn push_no_dup(&mut self, reg: SomeRegion) -> bool {
    //      if self.contains(&reg) {
    //          return false;
    //      }
    //
    //      self.avec.push(reg);
    //
    //      true
    //  }

    /// Add a region, removing subset regions.
    pub fn push_nosubs(&mut self, reg: SomeRegion) -> bool {
        // Check for supersets, which probably is an error
        if self.any_superset_of(&reg) {
            //println!("skipped adding region {}, a superset exists in {}", reg, self);
            return false;
        }

        // Identify subsets.
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in self.avec.iter().enumerate() {
            if regx.is_subset_of(&reg) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in descending index order.
        for inx in rmvec.iter().rev() {
            removeunordered::remove_unordered(&mut self.avec, *inx);
        }

        self.avec.push(reg);

        true
    }

    /// Add a region, removing superset (and equal) regions.
    pub fn push_nosups(&mut self, reg: SomeRegion) -> bool {
        // Check for subsets, which probably is an error
        if self.any_subset_of(&reg) {
            // println!("skipped adding region {}, a superset exists", reg.str());
            return false;
        }

        // Identify supersets
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in self.avec.iter().enumerate() {
            if regx.is_superset_of(&reg) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in reverse (highest index) order
        for inx in rmvec.iter().rev() {
            removeunordered::remove_unordered(&mut self.avec, *inx);
        }

        self.avec.push(reg);

        true
    }

    /// Return the expected length of a string representing a RegionStore.
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

    /// Return a string representing a RegionStore.
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

    /// Return a string representing a vector of regions.
    pub fn vec_ref_string(avec: &[&RegionStore]) -> String {
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

    // Return the result of intersection of two region stores
    //  pub fn intersection(&self, other: &RegionStore) -> Self {
    //
    //      let mut ret_store = Self::new();
    //
    //      for regx in self.iter() {
    //
    //          for regy in other.iter() {
    //
    //              if regx.intersects(&regy) {
    //                  ret_store.push_nosubs(regx.intersection(&regy));
    //              }
    //          }
    //      }
    //      ret_store
    //  }

    /// Subtract a region from a RegionStore
    pub fn subtract_region(&self, regx: &SomeRegion) -> Self {
        let mut ret_str = RegionStore::new(vec![]);

        for regy in &self.avec {
            if regx.intersects(regy) {
                let avec = regy.subtract(regx);
                for regz in &avec {
                    ret_str.push_nosubs(regz.clone());
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Subtract a RegionStore from a RegionStore
    pub fn subtract(&self, other: &RegionStore) -> RegionStore {
        let mut ret_str = self.clone();

        for regx in other.iter() {
            if ret_str.any_intersection(regx) {
                ret_str = ret_str.subtract_region(regx);
            }
        }
        ret_str
    }

    /// Return True if a RegionStore is a superset of all corresponding states in a StateStore.
    /// Used in optimal regionstore calculations.
    pub fn is_superset_corr_states(&self, stas: &[&SomeState]) -> bool {
        debug_assert!(self.len() == stas.len());

        for (x, y) in self.iter().zip(stas.iter()) {
            if x.is_superset_of_state(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return True if a RegionStore is equal of all corresponding states in another RegionStore.
    /// Used in optimal regionstore calculations.
    pub fn eq_corr(&self, other: &RegionStore) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x == y {
            } else {
                return false;
            }
        }
        true
    }

    /// Return the sum of distances between corresponding regions and states.
    /// Used in optimal regionstore calculations.
    pub fn distance_corr_states(&self, stas: &[&SomeState]) -> usize {
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

    /// Return True if a RegionStore is a superset of corresponding regions in a given RegionStore.
    /// Used in optimal regionstore calculations.
    pub fn is_superset_corr(&self, other: &RegionStore) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x.is_superset_of(y) {
            } else {
                return false;
            }
        }

        true
    }

    /// Return true if corresponding regions in two RegionStores are equal.
    /// Used in optimal regionstore calculations.
    pub fn equal_corr(&self, other: &RegionStore) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if x == y {
            } else {
                return false;
            }
        }
        true
    }

    /// Return an intersection of corresponding regions, of two RegionStores.
    /// Used in optimal regionstore calculations.
    pub fn intersection_corr(&self, other: &RegionStore) -> Option<RegionStore> {
        debug_assert!(self.len() == other.len());

        let mut ret = RegionStore::with_capacity(self.len());

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

    /// Return true if there is an intersection of corresponding regions, of two RegionStores.
    pub fn intersects_corr(&self, other: &RegionStore) -> bool {
        debug_assert!(self.len() == other.len());

        for (x, y) in self.iter().zip(other.iter()) {
            if !x.intersects(y) {
                return false;
            }
        }
        true
    }

    /// Return true if each region in a RegionStore is a subset, in order, of two RegionStores.
    /// Used in optimal regionstore calculations.
    pub fn subset_corr(&self, other: &RegionStore) -> bool {
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
    pub fn xb_mask_corr(&self, other: &RegionStore) -> Vec<SomeMask> {
        debug_assert!(self.len() == other.len());

        let mut ret_msks = Vec::<SomeMask>::with_capacity(self.len());

        for (x, b) in self.iter().zip(other.iter()) {
            ret_msks.push(x.x_mask().bitwise_and(&b.x_mask().bitwise_not()));
        }

        ret_msks
    }

    /// Return a vector of masks representing edge bit positions.
    pub fn edge_mask_corr(&self) -> Vec<SomeMask> {
        let mut ret_msks = Vec::<SomeMask>::with_capacity(self.len());

        for regx in self.iter() {
            ret_msks.push(regx.edge_mask());
        }

        ret_msks
    }

    /// Subtract corresponding regions from a RegionStore
    pub fn subtract_corr(&self, other: &RegionStore) -> Vec<RegionStore> {
        debug_assert!(self.len() == other.len());

        let mut ret_str = Vec::<RegionStore>::new();

        let xb_msks = self.xb_mask_corr(other);

        for inx in 0..self.len() {
            if xb_msks[inx].is_low() {
                continue;
            }
            let single_bits = xb_msks[inx].split();

            for bitx in single_bits.iter() {
                let mut new_store = RegionStore::with_capacity(self.len());

                for iny in 0..self.len() {
                    if iny == inx {
                        new_store.push(match bitx.bitwise_and(&other[inx].state1).is_low() {
                            true =>
                            // process x/0
                            {
                                self[inx].set_to_ones(bitx)
                            }
                            false =>
                            // process x/1
                            {
                                self[inx].set_to_zeros(bitx)
                            }
                        });
                    } else {
                        new_store.push(self[iny].clone());
                    }
                }
                ret_str.push(new_store);
            }
        }
        ret_str
    }

    /// Subtract corresponding regions from a RegionStore,
    /// Limit regions considered to those containing any region in a given vector.
    pub fn subtract_corr_containing(
        &self,
        other: &RegionStore,
        containing: &[&RegionStore],
    ) -> Vec<RegionStore> {
        debug_assert!(self.len() == other.len());

        let mut ret_str = Vec::<RegionStore>::new();

        let xb_msks = self.xb_mask_corr(other);

        for inx in 0..self.len() {
            if xb_msks[inx].is_low() {
                continue;
            }
            let single_bits = xb_msks[inx].split();

            for bitx in single_bits.iter() {
                let mut new_store = RegionStore::with_capacity(self.len());

                for iny in 0..self.len() {
                    if iny == inx {
                        new_store.push(match bitx.bitwise_and(&other[inx].state1).is_low() {
                            true =>
                            // process x/0
                            {
                                self[inx].set_to_ones(bitx)
                            }
                            false =>
                            // process x/1
                            {
                                self[inx].set_to_zeros(bitx)
                            }
                        });
                    } else {
                        new_store.push(self[iny].clone());
                    }
                } // Next iny.
                let mut contains = false;
                for regsx in containing.iter() {
                    if new_store.is_superset_corr(regsx) {
                        contains = true;
                        break;
                    }
                }
                if contains {
                    ret_str.push(new_store);
                }
            }
        }
        ret_str
    }
} // End impl RegionStore.

/// Return true if a vector of regionstores contains a given regionstore,
/// comparing corresponding regions.
#[allow(dead_code)]
fn vec_rs_corr_contains(avec: &Vec<RegionStore>, ars: &RegionStore) -> bool {
    for rsx in avec {
        if rsx.equal_corr(ars) {
            return true;
        }
    }
    false
}

/// Return true if any region is a superset, or equal, to a region.
pub fn vec_rs_any_superset_corr(rs_vec: &[RegionStore], reg_str: &RegionStore) -> bool {
    for regx in rs_vec {
        if regx.is_superset_corr(reg_str) {
            return true;
        }
    }
    false
}

/// Add a corresponding RegionStore to a corresponding RegionStore vector.
/// Skip add if there is any superset.
/// Otherwise, remove any subset RegionStores.
pub fn vec_rs_push_nosubs_corr(rs_vec: &mut Vec<RegionStore>, reg_str: RegionStore) -> bool {
    // Check for supersets, which probably is an error
    if vec_rs_any_superset_corr(rs_vec, &reg_str) {
        return false;
    }

    // Identify subsets.
    let mut rmvec = Vec::<usize>::new();

    for (inx, regx) in rs_vec.iter().enumerate() {
        if regx.subset_corr(&reg_str) {
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

/// Split corresponding RegionStores by intersections, producing a result where each RegionStore is a subset
/// of any intersecting original RegionStores. All parts of the original RegionStores are accounted for in the
/// result.
pub fn vec_rs_corr_split_to_subsets(rs_vec: &Vec<RegionStore>) -> Vec<RegionStore> {
    // Init return vector of RegionStores.
    let mut ret_vec = Vec::<RegionStore>::new();

    // Init temp vector, for RegionStore fragments.
    let mut tmp_vec = Vec::<RegionStore>::new();

    // Init vector to note indices of RegionStores that are split.
    // There may be a few duplicates, but that will not affect the use of the vector.
    let mut int_vec = Vec::<usize>::new();

    // Get initial fragments, from any two non-equal intersecting RegionStores.
    for inx in 0..(rs_vec.len() - 1) {
        for iny in (inx + 1)..rs_vec.len() {
            // Skip two equal regions.  If no other intersections, one will make it into the return vector.
            if rs_vec[inx].eq_corr(&rs_vec[iny]) {
                continue;
            }

            // If there is an intersection, split the RegionStores into fragments.
            if let Some(int) = rs_vec[inx].intersection_corr(&rs_vec[iny]) {
                int_vec.push(inx);
                int_vec.push(iny);

                for rsz in rs_vec[inx].subtract_corr(&int).into_iter() {
                    vec_rs_push_nosubs_corr(&mut tmp_vec, rsz);
                }

                for rsz in rs_vec[iny].subtract_corr(&int).into_iter() {
                    vec_rs_push_nosubs_corr(&mut tmp_vec, rsz);
                }
                vec_rs_push_nosubs_corr(&mut tmp_vec, int);
            }
        }
    }
    // For RegionStores with no intersections, add to the return vector.
    for (iny, rsy) in rs_vec.iter().enumerate() {
        if !int_vec.contains(&iny) {
            vec_rs_push_nosubs_corr(&mut ret_vec, rsy.clone());
        }
    }

    // Look for additional non-subset intersections.
    loop {
        // Init vector for next pass.
        let mut next_pass = Vec::<RegionStore>::new();

        // Check remaining fragments for additional intersections.
        // If no intersections are found, add the fragment to the return vector,
        // else add fragments of fragments to the next_pass vector.
        for rsy in tmp_vec.into_iter() {
            let mut split = false;

            // Check for intersecting RegionStore from original argument.
            for rsx in rs_vec.iter() {
                if let Some(int) = rsy.intersection_corr(rsx) {
                    // Skip if rsy if it is a subset, thats the end we are looking for.
                    if int.eq_corr(&rsy) {
                        continue;
                    }
                    // Split intersection into fragments.
                    // Add fragments to the next_pass vector.
                    for rsz in rsy.subtract_corr(&int).into_iter() {
                        vec_rs_push_nosubs_corr(&mut next_pass, rsz);
                    }
                    // Add the intersection to the next_pass vector.
                    vec_rs_push_nosubs_corr(&mut next_pass, int);
                    split = true;
                }
            } // next rsx
            // If no intersectiosn, add the fragment to the return vector.
            if !split {
                vec_rs_push_nosubs_corr(&mut ret_vec, rsy);
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

impl Index<usize> for RegionStore {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.avec[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_rs_corr_split_by_partial_intersection() -> Result<(), String> {
        let mut rs_vec = Vec::<RegionStore>::with_capacity(1);

        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);
        rs_vec.push(regstr1.clone());

        println!("Initial1:");
        for rsx in &rs_vec {
            println!("  {}", rsx);
        }

        let rslt = vec_rs_corr_split_to_subsets(&rs_vec);

        println!("Result1:");
        for rsx in &rslt {
            println!("  {}", rsx);
        }
        assert!(rslt.len() == 1);
        assert!(rslt.contains(&regstr1));

        let mut rs_vec = Vec::<RegionStore>::with_capacity(2);

        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);
        rs_vec.push(regstr1.clone());
        rs_vec.push(regstr1.clone());

        println!("Initial2:");
        for rsx in &rs_vec {
            println!("  {}", rsx);
        }

        let rslt = vec_rs_corr_split_to_subsets(&rs_vec);

        println!("Result2:");
        for rsx in &rslt {
            println!("  {}", rsx);
        }
        assert!(rslt.len() == 1);
        assert!(rslt.contains(&regstr1));

        let mut rs_vec = Vec::<RegionStore>::with_capacity(2);

        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);
        rs_vec.push(regstr1.clone());

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string(1, "r010x")?);
        rs_vec.push(regstr2.clone());

        println!("Initial3:");
        for rsx in &rs_vec {
            println!("  {}", rsx);
        }

        let rslt = vec_rs_corr_split_to_subsets(&rs_vec);

        println!("Result3:");
        for rsx in &rslt {
            println!("  {}", rsx);
        }
        assert!(rslt.len() == 2);
        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r110x"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r010x"
            )?]))
        );

        let mut rs_vec = Vec::<RegionStore>::with_capacity(2);

        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string(1, "r010x")?);

        rs_vec.push(regstr2.clone());
        rs_vec.push(regstr1.clone());

        println!("Initial4:");
        for rsx in &rs_vec {
            println!("  {}", rsx);
        }

        let rslt = vec_rs_corr_split_to_subsets(&rs_vec);

        println!("Result4:");
        for rsx in &rslt {
            println!("  {}", rsx);
        }
        assert!(rslt.len() == 2);
        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r110x"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r010x"
            )?]))
        );

        let mut rs_vec = Vec::<RegionStore>::with_capacity(2);

        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string(1, "r1xx1")?);

        rs_vec.push(regstr2.clone());
        rs_vec.push(regstr1.clone());

        println!("Initial5:");
        for rsx in &rs_vec {
            println!("  {}", rsx);
        }

        let rslt = vec_rs_corr_split_to_subsets(&rs_vec);

        println!("Result5:");
        for rsx in &rslt {
            println!("  {}", rsx);
        }
        assert!(rslt.len() == 5);
        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "rx100"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r010x"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r1101"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r10x1"
            )?]))
        );
        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r1101"
            )?]))
        );

        let mut rs_vec = Vec::<RegionStore>::with_capacity(2);

        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string(1, "rx10x")?);

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string(1, "r01xx")?);

        let mut regstr3 = RegionStore::with_capacity(1);
        regstr3.push(SomeRegion::new_from_string(1, "rxxx1")?);

        rs_vec.push(regstr2.clone());
        rs_vec.push(regstr1.clone());
        rs_vec.push(regstr3.clone());

        println!("Initial6:");
        for rsx in &rs_vec {
            println!("  {}", rsx);
        }

        let rslt = vec_rs_corr_split_to_subsets(&rs_vec);

        println!("Result6:");
        for rsx in &rslt {
            println!("  {}", rsx);
        }
        assert!(rslt.len() == 8);

        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "rx0x1"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r1100"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r1101"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r0110"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r0111"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r0100"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
                1, "r0101"
            )?]))
        );

        assert!(
            rslt.contains(&RegionStore::new(vec![SomeRegion::new_from_string(
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
    fn subtract_corr() -> Result<(), String> {
        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string(1, "rx0x1")?);
        regstr1.push(SomeRegion::new_from_string(2, "r0x1x")?);

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string(1, "r1001")?);
        regstr2.push(SomeRegion::new_from_string(2, "r0110")?);

        let subt = regstr1.subtract_corr(&regstr2);

        println!("for {}", regstr1);
        println!("  - {}", regstr2);

        for (inx, rsx) in subt.iter().enumerate() {
            println!("{}   {}", inx, rsx);
        }
        assert!(subt.len() == 4);

        let mut regstr3 = RegionStore::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string(1, "r0000_x011")?);
        regstr3.push(SomeRegion::new_from_string(2, "r0000_0000_0000_0x1x")?);
        assert!(vec_rs_corr_contains(&subt, &regstr3));

        let mut regstr3 = RegionStore::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string(1, "r0000_00x1")?);
        regstr3.push(SomeRegion::new_from_string(2, "r0000_0000_0000_0x1x")?);
        assert!(vec_rs_corr_contains(&subt, &regstr3));

        let mut regstr3 = RegionStore::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string(1, "r0000_x0x1")?);
        regstr3.push(SomeRegion::new_from_string(2, "r0000_0000_0000_0x11")?);
        assert!(vec_rs_corr_contains(&subt, &regstr3));

        let mut regstr3 = RegionStore::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string(1, "r0000_x0x1")?);
        regstr3.push(SomeRegion::new_from_string(2, "r0000_0000_0000_001x")?);
        assert!(vec_rs_corr_contains(&subt, &regstr3));

        Ok(())
    }

    #[test]
    fn xb_mask_corr() -> Result<(), String> {
        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string(1, "rx0x1")?);
        regstr1.push(SomeRegion::new_from_string(2, "r0x1x")?);

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string(1, "r1001")?);
        regstr2.push(SomeRegion::new_from_string(2, "r0110")?);

        let xb_msks = regstr1.xb_mask_corr(&regstr2);

        println!("msk0 = {}, msk1 = {}", xb_msks[0], xb_msks[1]);

        assert!(xb_msks[0] == SomeMask::new_from_string(1, "m0b1010")?);
        assert!(xb_msks[1] == SomeMask::new_from_string(2, "m0b0101")?);

        Ok(())
    }

    #[test]
    fn remove_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(3);

        let reg1 = SomeRegion::new_from_string(1, "r0x0x")?;
        let reg2 = SomeRegion::new_from_string(1, "r0xx1")?;
        let reg3 = SomeRegion::new_from_string(1, "rx1x1")?;

        regstr.push(reg1.clone());
        regstr.push(reg2.clone());

        assert!(!regstr.remove_region(&reg3));
        assert!(regstr.remove_region(&reg2));
        assert!(regstr.len() == 1);
        assert!(regstr.contains(&reg1));
        Ok(())
    }

    #[test]
    fn subtract_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let regx = SomeRegion::new_from_string(1, "r0101")?;

        let reg_rslt = regstr.subtract_region(&regx);
        println!("results {}", reg_rslt);

        assert!(reg_rslt.len() == 7);
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "r0x11")?));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "r00x1")?));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "rx111")?));
        assert!(reg_rslt.contains(&SomeRegion::new_from_string(1, "r11x1")?));
        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx11x")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string(1, "r01x1")?);
        regstr2.push(SomeRegion::new_from_string(1, "r0x01")?);

        let regstr3 = regstr.subtract(&regstr2);
        println!("regstr3: {}", &regstr3);

        assert!(regstr3.len() == 5);
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r0x00")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r0011")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r11x1")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "rx110")?));
        assert!(regstr3.contains(&SomeRegion::new_from_string(1, "r111x")?));
        Ok(())
    }

    #[test]
    fn push_nosups() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosups(SomeRegion::new_from_string(1, "r0111")?);
        println!("results {}", &regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r0x0x")?));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r1110")?));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r0111")?));
        Ok(())
    }

    #[test]
    fn push_nosubs() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosubs(SomeRegion::new_from_string(1, "rxxx1")?);
        println!("results {}", &regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r0x0x")?));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "r1110")?));
        assert!(regstr.contains(&SomeRegion::new_from_string(1, "rxxx1")?));
        Ok(())
    }

    #[test]
    fn state_in_1_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.state_in_1_region(&SomeState::new_from_string(1, "s0b0100")?));
        assert!(!regstr.state_in_1_region(&SomeState::new_from_string(1, "s0b0111")?));
        Ok(())
    }

    #[test]
    fn any_superset_of_state() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of_state(&SomeState::new_from_string(1, "s0b0111")?));
        assert!(!regstr.any_superset_of_state(&SomeState::new_from_string(1, "s0b1011")?));
        Ok(())
    }

    #[test]
    fn any_intersection() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_intersection(&SomeRegion::new_from_string(1, "r1xx1")?));
        assert!(!regstr.any_intersection(&SomeRegion::new_from_string(1, "r10x1")?));
        Ok(())
    }

    #[test]
    fn any_subset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_subset_of(&SomeRegion::new_from_string(1, "rx11x")?));
        assert!(!regstr.any_subset_of(&SomeRegion::new_from_string(1, "r1xx1")?));
        Ok(())
    }

    #[test]
    fn any_superset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::new_from_string(1, "r0x0x")?);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1")?);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1")?);
        regstr.push(SomeRegion::new_from_string(1, "r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of(&SomeRegion::new_from_string(1, "r01x1")?));
        assert!(!regstr.any_superset_of(&SomeRegion::new_from_string(1, "r1xx1")?));
        Ok(())
    }
}
