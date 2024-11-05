//! The RegionsCorrStore, a vector of RegionsCorr structs.

use crate::regionscorr::RegionsCorr;
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use unicode_segmentation::UnicodeSegmentation;

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

    /// Return a new RegionsCorrStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<RegionsCorr>::with_capacity(num),
        }
    }

    /// Return the number of regions.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return true if the store is empty.
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Add a region to the vector.
    pub fn push(&mut self, val: RegionsCorr) {
        debug_assert!(self.is_empty() || self[0].num_bits_vec() == val.num_bits_vec());

        self.items.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<RegionsCorr> {
        self.items.iter()
    }

    /// Return true if a RegionsCorrStore contains a region.
    /// Regions may be equal, without matching states.
    /// A region formed by 0 and 5 will equal a region formed by 4 and 1.
    pub fn contains(&self, rcx: &RegionsCorr) -> bool {
        debug_assert!(self.is_empty() || self[0].num_bits_vec() == rcx.num_bits_vec());

        self.items.contains(rcx)
    }

    /// Return true if there is any superset of a given RegionsCorr.
    fn any_superset_of(&self, rcx: &RegionsCorr) -> bool {
        debug_assert!(self.is_empty() || self[0].num_bits_vec() == rcx.num_bits_vec());

        for rcy in self.iter() {
            if rcy.is_superset_of(rcx) {
                return true;
            }
        }
        false
    }

    /// Return true if there is any intersection of a given RegionsCorr.
    pub fn any_intersection_of(&self, rcx: &RegionsCorr) -> bool {
        debug_assert!(self.is_empty() || self[0].num_bits_vec() == rcx.num_bits_vec());

        for rcy in self.iter() {
            if rcy.intersects(rcx) {
                return true;
            }
        }
        false
    }

    /// Delete subsets of a given RegionsCorr.
    fn delete_subsets_of(&mut self, rcx: &RegionsCorr) {
        debug_assert!(self.is_empty() || self[0].num_bits_vec() == rcx.num_bits_vec());

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
    pub fn push_nosubs(&mut self, rcx: RegionsCorr) {
        debug_assert!(self.is_empty() || self[0].num_bits_vec() == rcx.num_bits_vec());

        if self.any_superset_of(&rcx) {
            return;
        }
        self.delete_subsets_of(&rcx);
        self.items.push(rcx);
    }

    /// Subtract a RegionsCorr.
    pub fn subtract_regionscorr(&self, rcx: &RegionsCorr) -> Self {
        debug_assert!(self.is_empty() || self[0].num_bits_vec() == rcx.num_bits_vec());

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
        debug_assert!(self.is_empty() || self[0].num_bits_vec() == other[0].num_bits_vec());

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

    /// Return true if a RegionsCorr is a superset of a RegionsCorrStore.
    #[allow(dead_code)]
    pub fn is_superset_of(&self, other: &Self) -> bool {
        debug_assert!(self[0].num_bits_vec() == other[0].num_bits_vec());

        other.subtract(self).is_empty()
    }

    /// Return a RegionsCorrStore instance, given a string representation.
    /// Like RCS[], RCS[RC[r0010]] or RCS[RC[r1010], RC[r1111]].
    pub fn from(rcs_str: &str) -> Result<Self, String> {
        //println!("regionscorrstore::from: {rcs_str}");

        // Unwrap RCS[], check that brackets are balanced overall.
        let mut rcs_str2 = String::new();
        let mut num_left = 0;
        let mut num_right = 0;
        let mut last_chr = String::new();

        for (inx, chr) in rcs_str.graphemes(true).enumerate() {
            if chr == " " {
                continue;
            }
            if inx == 0 {
                if chr == "R" {
                    continue;
                } else {
                    return Err(format!(
                        "RegionsCorrStore::from: Invalid string, {rcs_str} should start with RCS["
                    ));
                }
            }
            if inx == 1 {
                if chr == "C" {
                    continue;
                } else {
                    return Err(format!(
                        "RegionsCorrStore::from: Invalid string, {rcs_str} should start with RCS["
                    ));
                }
            }
            if inx == 2 {
                if chr == "S" {
                    continue;
                } else {
                    return Err(format!(
                        "RegionsCorrStore::from: Invalid string, {rcs_str} should start with RCS["
                    ));
                }
            }
            if inx == 3 {
                if chr == "[" {
                    num_left += 1;
                    continue;
                } else {
                    return Err(format!(
                        "RegionsCorrStore::from: Invalid string, {rcs_str} should start with RCS["
                    ));
                }
            }
            if chr == "[" {
                num_left += 1;
            }
            if chr == "]" {
                num_right += 1;
            }
            if num_right > num_left {
                return Err(format!(
                    "RegionsCorrStore::from: Invalid string, {rcs_str}, brackets are not balanced."
                ));
            }
            last_chr = chr.to_string();
            rcs_str2.push_str(chr);
        }
        if num_right != num_left {
            return Err(format!(
                "RegionsCorrStore::from: Invalid string, {rcs_str}, brackets are not balanced."
            ));
        }
        if last_chr != "]" {
            return Err(format!(
                "RegionsCorrStore::from: Invalid string, {rcs_str} should end with ]"
            ));
        }
        // Remove last right-bracket, balancing RCS[.
        rcs_str2.remove(rcs_str2.len() - 1);
        //println!("rcs_str2 {rcs_str2}");

        // Process contents of RCS[], if any.
        let mut rcs = RegionsCorrStore::new(vec![]);

        let mut pc_str = String::new();
        let mut num_left = 0;
        let mut num_right = 0;

        for chr in rcs_str2.graphemes(true) {
            if chr == "[" {
                num_left += 1;
            }

            if chr == "]" {
                num_right += 1;
            }

            if chr == "," && num_left == num_right {
                //println!("pc_str {pc_str}");
                match RegionsCorr::from(&pc_str) {
                    Ok(pcx) => rcs.push(pcx),
                    Err(errstr) => return Err(format!("RegionsCorrStore::from: {errstr}")),
                }
                pc_str = String::new();
                continue;
            }

            pc_str.push_str(chr);
        }

        if pc_str.is_empty() {
        } else {
            match RegionsCorr::from(&pc_str) {
                Ok(pcx) => rcs.push(pcx),
                Err(errstr) => return Err(format!("RegionsCorrStore::from: {errstr}")),
            }
        }

        Ok(rcs)
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

    #[test]
    fn new() -> Result<(), String> {
        let regstr1 = RegionsCorrStore::new(vec![]);
        println!("regstr1 {regstr1}");

        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let regstr1 = RegionsCorrStore::from("RCS[RC[rx1X1], RC[r1xx1]]")?;
        println!("regstr1 {regstr1}");

        // Test single RegionsCorr superset.
        let sub1 = RegionsCorrStore::from("RCS[RC[r11x1]]")?;
        println!("sub1 {sub1}");

        if regstr1.is_superset_of(&sub1) {
            println!("test 1 OK");
        } else {
            return Err("test1 failed".to_string());
        }

        // Test intersections that add up to a superset.
        let sub2 = RegionsCorrStore::from("RCS[RC[r1x01]]")?;
        println!("sub2 {sub2}");

        if regstr1.is_superset_of(&sub2) {
            println!("test 2 OK");
        } else {
            return Err("test2 failed".to_string());
        }

        // Test intersections that do not add up to a superset.
        let sub3 = RegionsCorrStore::from("RCS[RC[r100x]]")?;
        println!("sub3 {sub3}");

        if regstr1.is_superset_of(&sub3) {
            return Err("test3 failed".to_string());
        } else {
            println!("test 3 OK");
        }

        // Test no intersections.
        let sub4 = RegionsCorrStore::from("RCS[RC[r1x00]]")?;
        println!("sub4 {sub4}");

        if regstr1.is_superset_of(&sub4) {
            return Err("test4 failed".to_string());
        } else {
            println!("test 4 OK");
        }
        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn from() -> Result<(), String> {
        let rcs1 = RegionsCorrStore::from("RCS[]")?;
        println!("rcs1 {rcs1}");

        let rcs2 = RegionsCorrStore::from("RCS[RC[r0X10, r100]]")?;
        println!("rcs2 {rcs2}");
        assert!(rcs2.len() == 1);

        let rcs3 = RegionsCorrStore::from("RCS[RC[r0X10, r100], RC[r0X11, r101]]")?;
        println!("rcs3 {rcs3}");
        assert!(rcs3.len() == 2);

        Ok(())
    }
}
