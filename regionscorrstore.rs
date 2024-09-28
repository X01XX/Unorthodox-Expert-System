//! The RegionsCorrStore, a vector of RegionsCorr structs.

use crate::regionscorr::RegionsCorr;
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

impl fmt::Display for RegionsCorrStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.items))
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct RegionsCorrStore {
    /// A vector of regions.
    pub items: Vec<RegionsCorr>,
}

impl PartialEq for RegionsCorrStore {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for regx in &self.items {
            if !other.contains(regx) {
                return false;
            }
        }
        true
    }
}
impl Eq for RegionsCorrStore {}

impl RegionsCorrStore {
    /// Return a new, RegionsCorrStore.
    pub fn new(items: Vec<RegionsCorr>) -> Self {
        Self { items }
    }

    /// Return the number of regions.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return true if the store is empty.
    pub fn _is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Add a region to the vector.
    pub fn push(&mut self, val: RegionsCorr) {
        self.items.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<RegionsCorr> {
        self.items.iter()
    }

    /// Return true if a RegionsCorrStore contains a region.
    /// Regions may be equal, without matching states.
    /// A region formed by 0 and 5 will equal a region formed by 4 and 1.
    pub fn contains(&self, reg: &RegionsCorr) -> bool {
        self.items.contains(reg)
    }

    /// Return true if there is any superset of a given RegionsCorr.
    fn any_superset_of(&self, rcx: &RegionsCorr) -> bool {
        for rcy in self.iter() {
            if rcy.is_superset_of(rcx) {
                return true;
            }
        }
        false
    }

    /// Return true if there is any intersection of a given RegionsCorr.
    fn any_intersection_of(&self, rcx: &RegionsCorr) -> bool {
        for rcy in self.iter() {
            if rcy.intersects(rcx) {
                return true;
            }
        }
        false
    }

    /// Delete subsets of a given RegionsCorr.
    fn delete_subsets_of(&mut self, rcx: &RegionsCorr) {
        let mut del = Vec::<usize>::new();

        for (inx, rcy) in self.iter().enumerate() {
            if rcx.is_superset_of(rcy) {
                del.push(inx);
            }
        }
        // Remove items, if any, highest index first.
        for inx in del.iter().rev() {
            self.items.remove(*inx);
        }
    }

    /// Add a region to the vector, deleting subsets.
    pub fn push_nosubs(&mut self, val: RegionsCorr) {
        if self.any_superset_of(&val) {
            return;
        }
        self.delete_subsets_of(&val);
        self.items.push(val);
    }

    /// Subtract a RegionsCorr.
    fn subtract_regionscorr(&self, rcx: &RegionsCorr) -> Self {
        let mut ret_store = Self::new(vec![]);

        for rcy in self.iter() {
            if rcy.intersects(rcx) {
                let remaining = rcy.subtract(rcx);
                for rcz in remaining {
                    ret_store.push_nosubs(rcz);
                }
            } else {
                ret_store.push_nosubs(rcy.clone());
            }
        }
        ret_store
    }

    /// Subtract a RegionsCorrStore.
    fn subtract(&self, other: &Self) -> Self {
        let mut ret = self.clone();
        for rcx in other.iter() {
            if self.any_intersection_of(rcx) {
                ret = ret.subtract_regionscorr(rcx);
            }
        }
        ret
    }

    /// Return self fragmented by intersections.
    /// Successively, subtract intersections, collect remainders.
    /// Each fragment returned will be a subset of one, or more, items in
    /// the original, but not otherwise intersect any item in the original.
    pub fn split_by_intersections(&self) -> Self {
        if self.len() < 2 {
            // Nothing to intersect.
            return self.clone();
        }

        let mut ret_remainders = Self::new(vec![]); // Store to collect successive remainders.

        let mut cur_left = self.clone(); // Store for current non-remainders.

        while cur_left.is_not_empty() {
            // Find the remainders of each SelectRegions minus others.
            let mut cycle_remainders = Self::new(vec![]);
            let mut cycle_left = Self::new(vec![]);

            for (inx, regsx) in cur_left.iter().enumerate() {
                // Init regsx remainders store.
                let mut regsx_remainders = Self::new(vec![regsx.clone()]);

                // Subtract anything that intersects regsx, except for regsx.
                for (iny, regsy) in cur_left.iter().enumerate() {
                    if iny != inx && regsx_remainders.any_intersection_of(regsy) {
                        regsx_remainders = regsx_remainders.subtract_regionscorr(regsy);
                    }
                }

                // Add to cycle_remainders.
                if regsx_remainders.is_not_empty() {
                    // Calc whats left of regsx, without remainders.
                    let regsx_left = Self::new(vec![regsx.clone()]).subtract(&regsx_remainders);

                    // Add regsx remainders to cycle remainders.
                    for regsz in regsx_remainders.into_iter() {
                        cycle_remainders.push_nosubs(regsz);
                    }

                    // Add to regsx left to cycle left.
                    for regsz in regsx_left.into_iter() {
                        cycle_left.push_nosubs(regsz);
                    }
                }
            } // next inx, regsx

            // Add cycle remainders to return store.
            for regsz in cycle_remainders.into_iter() {
                ret_remainders.push_nosubs(regsz);
            }

            // Set up next cycle.
            cur_left = cycle_left;
        } // end loop

        ret_remainders
    }
} // end impl RegionsCorrStore.

impl Index<usize> for RegionsCorrStore {
    type Output = RegionsCorr;
    fn index(&self, i: usize) -> &RegionsCorr {
        &self.items[i]
    }
}

impl IndexMut<usize> for RegionsCorrStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}

impl IntoIterator for RegionsCorrStore {
    type Item = RegionsCorr;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

/// Implement the trait StrLen for RegionsCorrStore.
impl StrLen for RegionsCorrStore {
    fn strlen(&self) -> usize {
        let mut rc_len = 2;

        if self.is_not_empty() {
            rc_len += self.items.len() * self.items[0].strlen();
            rc_len += (self.items.len() - 1) * 2;
        }

        rc_len
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::region::SomeRegion;

    #[test]
    fn new() -> Result<(), String> {
        let regs = vec![
            RegionsCorr::new(vec![SomeRegion::new_from_string("rx101")?]),
            RegionsCorr::new(vec![SomeRegion::new_from_string("rx101")?]),
        ];
        let regstr1 = RegionsCorrStore::new(regs);
        println!("regstr1 {regstr1}");

        Ok(())
    }
}
