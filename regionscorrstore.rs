//! The RegionsCorrStore, a vector of RegionsCorr structs.

use crate::regionscorr::RegionsCorr;
use crate::regionstore::RegionStore;
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;
use std::str::FromStr;

impl fmt::Display for RegionsCorrStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RCS{}", tools::vec_string(&self.items))
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct RegionsCorrStore {
    /// A vector of regions.
    pub items: Vec<RegionsCorr>,
}

impl RegionsCorrStore {
    /// Return a new, RegionsCorrStore.
    pub fn new(items: Vec<RegionsCorr>) -> Self {
        if items.len() > 1 {
            let zero_vec = items[0].num_bits_vec();
            for rcx in items.iter().skip(1) {
                assert!(rcx.num_bits_vec() == zero_vec);
            }
        }
        Self { items }
    }

    /// Return a new RegionsCorrStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<RegionsCorr>::with_capacity(num),
        }
    }

    /// For an instance containing RegionsCorr items with more than one region,
    /// return a RegionsCorrStore with the RegionsCorr items combined.
    pub fn combine(&self) -> Self {
        let mut ret = Self::with_capacity(self.len());
        for rcx in self.iter() {
            ret.push(rcx.combine());
        }
        ret
    }

    /// Return a RegionStore from a  RCS.
    pub fn to_regionstore(&self) -> RegionStore {
        let mut ret = RegionStore::with_capacity(self.len());
        for rcx in self.iter() {
            if rcx.len() == 1 {
                ret.push(rcx.regions[0].clone());
            } else {
                ret.push(rcx.combine().regions[0].clone());
            }
        }
        ret
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
        debug_assert!(self.is_empty() || self[0].is_congruent(&val));

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
        debug_assert!(self.is_empty() || self[0].is_congruent(rcx));

        self.items.contains(rcx)
    }

    /// Return true if there is any superset of a given RegionsCorr.
    fn any_superset_of(&self, rcx: &RegionsCorr) -> bool {
        debug_assert!(self.is_empty() || self[0].is_congruent(rcx));

        for rcy in self.iter() {
            if rcy.is_superset_of(rcx) {
                return true;
            }
        }
        false
    }

    /// Return true if there is any intersection of a given RegionsCorr.
    pub fn any_intersection_of(&self, rcx: &RegionsCorr) -> bool {
        debug_assert!(self.is_empty() || self[0].is_congruent(rcx));

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
            if rcy.is_subset_of(rcx) {
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
        debug_assert!(
            self.is_empty() || rcx.is_empty() || self[0].num_bits_vec() == rcx.num_bits_vec()
        );

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
    pub fn subtract(&self, other: &Self) -> Self {
        debug_assert!(
            self.is_empty()
                || other.is_empty()
                || self[0].num_bits_vec() == other[0].num_bits_vec()
        );

        let mut ret = self.clone();
        for rcx in other.iter() {
            if ret.any_intersection_of(rcx) {
                ret = ret.subtract_regionscorr(rcx);
            }
        }
        ret
    }

    /// Return self fragmented by intersections.
    /// Each fragment returned will be a subset of any item
    /// it intersects in the original.
    /// All fragments returned will account for all parts of all items
    /// in the original.
    pub fn split_by_intersections(&self) -> Self {
        //println!("regionstorecorr::split_by_intersections: split {self}");
        // Remove duplicates, if any.
        let mut remaining = Self::new(vec![]);
        for rscx in self.iter() {
            if !remaining.contains(rscx) {
                remaining.push(rscx.clone());
            }
        }

        if remaining.len() < 2 {
            // Nothing to intersect.
            return remaining;
        }

        let mut ret = Self::new(vec![]);
        loop {
            // Find intersections and not-intersections.
            let mut ints = Self::new(vec![]);

            // Subtract intersections from each region.
            for inx in 0..remaining.len() {
                let mut left_over = Self::new(vec![remaining[inx].clone()]);

                for iny in 0..remaining.len() {
                    if iny == inx {
                        // don't subtract remaining[inx] from itself.
                        continue;
                    }
                    if let Some(reg_int) = remaining[inx].intersection(&remaining[iny]) {
                        left_over = left_over.subtract_regionscorr(&reg_int);
                        if !ints.contains(&reg_int) {
                            ints.push(reg_int);
                        }
                    }
                }
                ret.append(left_over);
            }
            if ints.is_empty() {
                return ret;
            }
            remaining = ints;
        }
    }

    /// Return true if a RegionsCorr is a superset of a RegionsCorrStore.
    #[allow(dead_code)]
    pub fn is_superset_of(&self, other: &Self) -> bool {
        debug_assert!(
            self.is_empty()
                || other.is_empty()
                || self[0].num_bits_vec() == other[0].num_bits_vec()
        );

        other.subtract(self).is_empty()
    }

    /// Extend a RegionsCorrStore by emptying another RegionsCorrStore.
    pub fn append(&mut self, mut other: Self) {
        self.items.append(&mut other.items);
    }

    /// Return a reference to the last item in an instance.
    pub fn last(&self) -> Option<&RegionsCorr> {
        self.items.last()
    }

    /// Return indicies of first intersection found between two instances.
    pub fn intersecting_pair(&self, other: &Self) -> Option<(usize, usize)> {
        for (inx, rcx) in self.iter().enumerate() {
            for (iny, rcy) in other.iter().enumerate() {
                if rcx.intersects(rcy) {
                    return Some((inx, iny));
                }
            }
        }
        None
    }

    /// Return indicies of first intersection found between two instances.
    pub fn adjacent_pair(&self, other: &Self) -> Option<(usize, usize)> {
        for (inx, rcx) in self.iter().enumerate() {
            for (iny, rcy) in other.iter().enumerate() {
                if rcx.is_adjacent(rcy) {
                    return Some((inx, iny));
                }
            }
        }
        None
    }

    /// Truncate after the first X items of an instance.
    pub fn truncate(&mut self, limit: usize) {
        self.items.truncate(limit);
    }

    /// Reverse the order of items in an instance.
    pub fn reverse(&mut self) {
        self.items.reverse();
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

impl FromStr for RegionsCorrStore {
    type Err = String;
    /// Return a RegionsCorrStore instance, given a string representation.
    /// Like RCS[], RCS[RC[r0010]] or RCS[RC[r1010], RC[r1111]].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("regionscorrstore::from_str: {str_in}");
        let str_in2 = str_in.trim();

        // Strip off surrounding id and brackets.
        if str_in2.len() < 5 {
            return Err(
                "regionscorrstore::from_str: string should be at least = RCS[<one RegionsCorr>]"
                    .to_string(),
            );
        }

        if str_in2[0..4] != *"RCS[" {
            return Err("regionscorrstore::from_str: string should begin with RCS[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("regionscorrstore::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[4..(str_in2.len() - 1)];

        // Split string into RegionsCorr tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("regionscorrstore::from_str: {errstr}")),
        };
        //println!("tokens {:?}", tokens);

        let mut rcs = RegionsCorrStore::new(Vec::<RegionsCorr>::with_capacity(tokens.len()));

        for tokx in tokens.iter() {
            match RegionsCorr::from_str(tokx) {
                Ok(pcx) => rcs.push(pcx),
                Err(errstr) => return Err(format!("regionscorrstore::from_str: {errstr}")),
            }
        }

        Ok(rcs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Check fragments of a RS for various error conditions.
    fn check_fragments(srs1: &RegionsCorrStore, frags: &RegionsCorrStore) -> Result<(), String> {
        // Check for non-subset intersections.
        for regx in frags.iter() {
            for regy in srs1.iter() {
                if regx.intersects(regy) && !regx.is_subset_of(regy) {
                    return Err(format!("{regx} intersects {regy} ?"));
                }
            }
        }

        // Check fragments cover whole area.
        let dif = srs1.subtract(&frags);
        println!("dif {}", dif);
        if dif.is_not_empty() {
            return Err(format!("diff is {dif} ?"));
        }

        // Check no fragments are supersets (or equal) any other.
        for inx in 0..(frags.len() - 1) {
            for iny in (inx + 1)..frags.len() {
                if frags[inx].is_superset_of(&frags[iny]) {
                    return Err(format!("{} is a superset of {}", frags[inx], frags[iny]));
                }
            }
        }
        Ok(())
    }

    #[test]
    fn new() -> Result<(), String> {
        let regstr1 = RegionsCorrStore::new(vec![]);
        println!("regstr1 {regstr1}");

        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let regstr1 = RegionsCorrStore::from_str("RCS[RC[rx1X1], RC[r1xx1]]")?;
        println!("regstr1 {regstr1}");

        // Test single RegionsCorr superset.
        let sub1 = RegionsCorrStore::from_str("RCS[RC[r11x1]]")?;
        println!("sub1 {sub1}");

        if regstr1.is_superset_of(&sub1) {
            println!("test 1 OK");
        } else {
            return Err("test1 failed".to_string());
        }

        // Test intersections that add up to a superset.
        let sub2 = RegionsCorrStore::from_str("RCS[RC[r1x01]]")?;
        println!("sub2 {sub2}");

        if regstr1.is_superset_of(&sub2) {
            println!("test 2 OK");
        } else {
            return Err("test2 failed".to_string());
        }

        // Test intersections that do not add up to a superset.
        let sub3 = RegionsCorrStore::from_str("RCS[RC[r100x]]")?;
        println!("sub3 {sub3}");

        if regstr1.is_superset_of(&sub3) {
            return Err("test3 failed".to_string());
        } else {
            println!("test 3 OK");
        }

        // Test no intersections.
        let sub4 = RegionsCorrStore::from_str("RCS[RC[r1x00]]")?;
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
    fn from_str() -> Result<(), String> {
        let rcs1_str = "RCS[]";
        let rcs1 = RegionsCorrStore::from_str(&rcs1_str)?;
        println!("rcs1 {rcs1}");
        assert!(format!("{rcs1}") == rcs1_str);

        let rcs2_str = "RCS[RC[r0010]]";
        let rcs2 = RegionsCorrStore::from_str(&rcs2_str)?;
        println!("rcs2 {rcs2}");
        assert!(format!("{rcs2}") == rcs2_str);

        let rcs3_str = "RCS[RC[r1010], RC[r1111]]";
        let rcs3 = RegionsCorrStore::from_str(&rcs3_str)?;
        println!("rcs3 {rcs3}");
        assert!(format!("{rcs3}") == rcs3_str);

        Ok(())
    }

    #[test]
    fn split_by_intersections() -> Result<(), String> {
        // Test empty RCS.
        let rcs1 = RegionsCorrStore::from_str("RCS[]")?;
        let fragments1 = rcs1.split_by_intersections();
        println!("fragments1 {fragments1}");
        assert!(fragments1.len() == 0);

        // Test single RCSs.
        let rcs2 = RegionsCorrStore::from_str("RCS[RC[r0x11]]")?;
        let fragments2 = rcs2.split_by_intersections();
        println!("fragments2 {fragments2}");
        assert!(fragments2.len() == 1);

        // Test two non-intersecting RCSs.
        let rcs3 = RegionsCorrStore::from_str("RCS[RC[r0011], RC[r1xx1]]")?;
        let fragments3 = rcs3.split_by_intersections();
        println!("fragments3 {fragments3}");
        assert!(fragments3.len() == 2);

        // Test two intersecting RCSs.
        let rcs4 = RegionsCorrStore::from_str("RCS[RC[rX011], RC[r1xx1]]")?;
        let fragments4 = rcs4.split_by_intersections();
        println!("fragments4 {fragments4}");
        assert!(fragments4.len() == 4);
        assert!(check_fragments(&rcs4, &fragments4) == Ok(()));

        // Test two intersecting, multi-region, RCSs.
        let rcs5 = RegionsCorrStore::from_str("RCS[RC[rX10X, X1X1], RC[rX1X1, X10X]]")?;
        let fragments5 = rcs5.split_by_intersections();
        println!("fragments5 {fragments5}");
        assert!(fragments5.len() == 5);
        assert!(check_fragments(&rcs5, &fragments5) == Ok(()));

        // Test two intersecting, one-region, RCSs, *equivalent* to the above RCSs.
        let rcs6 = RegionsCorrStore::from_str("RCS[RC[rX10X_X1X1], RC[rX1X1_X10X]]")?;
        let fragments6 = rcs6.split_by_intersections();
        println!("fragments6 {fragments6}");
        assert!(fragments6.len() == 5);
        assert!(check_fragments(&rcs6, &fragments6) == Ok(()));

        // Test four one-region, RCSs.
        let rcs7 =
            RegionsCorrStore::from_str("RCS[RC[r0_X10X], RC[r0_X1X1], RC[r1_X10X], RC[r1_X1X1]]")?;
        let fragments7 = rcs7.split_by_intersections();
        println!("fragments7 {fragments7}");
        assert!(fragments7.len() == 6);
        assert!(check_fragments(&rcs7, &fragments7) == Ok(()));

        // Test four one-region, RCSs.
        let rcs8 = RegionsCorrStore::from_str("RCS[RC[rX10X], RC[r0XX1], RC[rX11X], RC[r0X1X]]")?;
        let fragments8 = rcs8.split_by_intersections();
        println!("fragments8 {fragments8}");
        assert!(fragments8.len() == 9);
        assert!(check_fragments(&rcs8, &fragments8) == Ok(()));

        Ok(())
    }

    #[test]
    fn to_regionstore() -> Result<(), String> {
        let rcs1 = RegionsCorrStore::from_str("RCS[RC[r0011], RC[r1xx1]]")?;
        let rs1 = rcs1.to_regionstore();
        println!("{rcs1} to {rs1}");
        assert!(rs1 == RegionStore::from_str("[r0011, r1xx1]")?);

        let rcs1 = RegionsCorrStore::from_str("RCS[RC[r00, r11], RC[r1x, rx1]]")?;
        let rs1 = rcs1.to_regionstore();
        println!("{rcs1} to {rs1}");
        assert!(rs1 == RegionStore::from_str("[r0011, r1xx1]")?);

        Ok(())
    }

    // Test subtract, and equivalence to regionstore subtraction.
    #[test]
    fn eqv_subtract() -> Result<(), String> {
        let rcs1 = RegionsCorrStore::from_str("RCS[RC[rX1, r0X]]")?;
        let rcs2 = RegionsCorrStore::from_str("RCS[RC[r1X, rX1]]")?;
        let rcs3 = rcs1.subtract(&rcs2);
        println!("{rcs1} - {rcs2} = {rcs3}");
        let rs1 = rcs3.to_regionstore();
        let rs2 = RegionStore::from_str("[rX10X]")?.subtract(&RegionStore::from_str("[1XX1]")?);
        println!("{rs1} should be eq {rs2}");
        assert!(rs1 == rs2);

        Ok(())
    }

    // Test split_by_intersections, and equivalence to regionstore split_by_intersections.
    #[test]
    fn eqv_split() -> Result<(), String> {
        let rcs1 = RegionsCorrStore::from_str("RCS[RC[rX1, r0X], RC[r1X, rX1]]")?;
        let rcs2 = rcs1.split_by_intersections();
        println!("{rcs1} split = {rcs2}");
        let rs1 = rcs2.to_regionstore();
        let rs2 = RegionStore::from_str("[rX10X, r1XX1]")?.split_by_intersections();
        println!("{rs1} should be eq {rs2}");
        assert!(rs1 == rs2);

        Ok(())
    }
}
