//! The RegionStore, a vector of SomeRegion structs.

use crate::bits::NumBits;
use crate::region::{AccessStates, SomeRegion};
use crate::state::SomeState;
use crate::tools::{self, AvecRef, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

use unicode_segmentation::UnicodeSegmentation;

impl fmt::Display for RegionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.items))
    }
}
#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct RegionStore {
    /// A vector of regions.
    pub items: Vec<SomeRegion>,
}

impl PartialEq for RegionStore {
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
impl Eq for RegionStore {}

impl RegionStore {
    /// Return a new, RegionStore.
    pub fn new(items: Vec<SomeRegion>) -> Self {
        Self { items }
    }

    /// Return a new RegionStore instance, empty, with a specified capacity.
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

    /// Add a region to the vector.
    pub fn push(&mut self, val: SomeRegion) {
        self.items.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRegion> {
        self.items.iter()
    }

    /// Return true if any region is a superset, or equal, to a region.
    pub fn any_superset_of(&self, reg: &SomeRegion) -> bool {
        debug_assert!(self.is_empty() || reg.num_bits() == self.num_bits().unwrap());

        tools::vec_contains(&self.items, SomeRegion::is_superset_of, reg)
    }

    /// Return true if any region is a subset, or equal, to a region.
    pub fn any_subset_of(&self, reg: &SomeRegion) -> bool {
        debug_assert!(self.is_empty() || reg.num_bits() == self.num_bits().unwrap());

        tools::vec_contains(&self.items, SomeRegion::is_subset_of, reg)
    }

    /// Return true if any region intersects a given region.
    pub fn any_intersection_of(&self, reg: &SomeRegion) -> bool {
        debug_assert!(self.is_empty() || reg.num_bits() == self.num_bits().unwrap());

        tools::vec_contains(&self.items, SomeRegion::intersects, reg)
    }

    /// Return true if any region is a superset of a state.
    pub fn any_superset_of_state(&self, sta: &SomeState) -> bool {
        debug_assert!(self.is_empty() || sta.num_bits() == self.num_bits().unwrap());

        tools::vec_contains(&self.items, SomeRegion::is_superset_of, sta)
    }

    /// Return vector of regions that are a superset of a given item.
    pub fn supersets_of(&self, itmx: &impl AccessStates) -> Self {
        debug_assert!(self.is_empty() || itmx.num_bits() == self.num_bits().unwrap());

        let regs = self
            .items
            .iter()
            .filter_map(|regx| {
                if regx.is_superset_of(itmx) {
                    Some(regx.clone())
                } else {
                    None
                }
            })
            .collect::<Vec<SomeRegion>>();

        Self::new(regs)
    }

    /// Return true if a RegionStore contains a region.
    /// Regions may be equal, without matching states.
    /// A region formed by 0 and 5 will equal a region formed by 4 and 1.
    pub fn contains(&self, reg: &SomeRegion) -> bool {
        debug_assert!(self.is_empty() || reg.num_bits() == self.num_bits().unwrap());

        self.items.contains(reg)
    }

    /// Add a region, removing subset regions.
    pub fn push_nosubs(&mut self, reg: SomeRegion) -> bool {
        debug_assert!(self.is_empty() || reg.num_bits() == self.num_bits().unwrap());

        // Check for supersets.
        if self.any_superset_of(&reg) {
            //println!("skipped adding region {}, a superset exists in {}", reg, self);
            return false;
        }

        // Identify subsets.
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in self.items.iter().enumerate() {
            if regx.is_subset_of(&reg) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in descending index order.
        for inx in rmvec.iter().rev() {
            tools::remove_unordered(&mut self.items, *inx);
        }

        self.items.push(reg);

        true
    }

    /// Add a region, removing superset (and equal) regions.
    pub fn push_nosups(&mut self, reg: SomeRegion) -> bool {
        debug_assert!(self.is_empty() || reg.num_bits() == self.items[0].num_bits());

        // Check for subsets.
        if self.any_subset_of(&reg) {
            // println!("skipped adding region {}, a superset exists", reg.str());
            return false;
        }

        // Identify supersets
        let mut rmvec = Vec::<usize>::new();

        for (inx, regx) in self.items.iter().enumerate() {
            if regx.is_superset_of(&reg) {
                rmvec.push(inx);
            }
        }

        // Remove identified regions, in reverse (highest index) order
        for inx in rmvec.iter().rev() {
            tools::remove_unordered(&mut self.items, *inx);
        }

        self.items.push(reg);

        true
    }

    /// Subtract a region/state from a RegionStore.
    pub fn subtract_item(&self, itmx: &impl AccessStates) -> Self {
        debug_assert!(self.is_empty() || itmx.num_bits() == self.items[0].num_bits());

        let mut ret_str = Self::new(vec![]);

        for regy in &self.items {
            if itmx.intersects(regy) {
                for regz in regy.subtract(itmx) {
                    ret_str.push_nosubs(regz);
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Subtract a RegionStore from a RegionStore
    pub fn subtract(&self, subtrahend: &Self) -> Self {
        debug_assert!(
            self.is_empty() || subtrahend.is_empty() || subtrahend.num_bits() == self.num_bits()
        );

        let mut ret_str = self.clone();

        for regx in subtrahend.iter() {
            if ret_str.any_intersection_of(regx) {
                ret_str = ret_str.subtract_item(regx);
            }
        }
        ret_str
    }

    /// Subtract a state from a RegionStore, with results being supersets of a second state.
    /// Assumes all regions are supersets of the second state before doing the subtraction.
    pub fn subtract_state_to_supersets_of(&self, substa: &SomeState, supsta: &SomeState) -> Self {
        debug_assert!(self.is_empty() || substa.num_bits() == self.items[0].num_bits());
        debug_assert!(self.is_empty() || supsta.num_bits() == self.items[0].num_bits());

        assert!(self.any_superset_of_state(substa));

        let mut ret_str = Self::new(vec![]);

        for regy in &self.items {
            if regy.is_superset_of(substa) {
                for regz in regy.subtract_state_to_supersets_of(substa, supsta) {
                    ret_str.push_nosubs(regz);
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    // Return the union of two RegionStores.
    pub fn union(&self, other: &Self) -> Self {
        debug_assert!(self.is_empty() || other.is_empty() || self.num_bits() == other.num_bits());

        let mut ret = self.clone();
        for regx in other.items.iter() {
            ret.push_nosubs(regx.clone());
        }
        ret
    }

    /// Return the intersection of two RegionStores.
    /// Regions overlapping adjacent regions, in the result, are not found.
    /// If that is wanted, it would be max-reg.subtract(&max-reg.subtract(ret))
    pub fn intersection(&self, other: &Self) -> Self {
        debug_assert!(
            self.is_empty()
                || other.is_empty()
                || self.items[0].num_bits() == other.items[0].num_bits()
        );

        let mut ret = Self::new(vec![]);
        for regx in self.items.iter() {
            for regy in other.iter() {
                if let Some(regz) = regx.intersection(regy) {
                    ret.push_nosubs(regz);
                }
            }
        }
        ret
    }

    /// Using two-state regions, where each pair of states represent incompatible squares.
    /// Superset regions are not fatal, but are inefficient.
    /// See the addendum "Calculating possible regions using dissimilar pairs of squares", in theory.html.
    pub fn possible_regions_by_negative_inference(&self) -> Self {
        assert!(!self.is_empty());

        // Calc the maximum possible region.
        let max_poss_reg = SomeRegion::new(vec![
            self[0].first_state().new_high(),
            self[0].first_state().new_low(),
        ]);

        // Init list for holding possible regions.
        let mut poss_regs = RegionStore::new(vec![max_poss_reg.clone()]);

        for ex_regx in self.iter() {
            assert!(ex_regx.len() == 2);
            poss_regs = poss_regs.intersection(&poss_regs.possible_regions_by_negative_inference2(
                &max_poss_reg,
                ex_regx.first_state(),
                &ex_regx.far_state(),
            ));
        }

        poss_regs
    }

    /// Return the possible regions implied by a maximum region and two dissimilar states.
    fn possible_regions_by_negative_inference2(
        &self,
        max_poss_reg: &SomeRegion,
        state1: &SomeState,
        state2: &SomeState,
    ) -> Self {
        debug_assert!(self.is_empty() || max_poss_reg.num_bits() == self.num_bits().unwrap());
        debug_assert!(max_poss_reg.num_bits() == state1.num_bits());
        debug_assert!(max_poss_reg.num_bits() == state2.num_bits());

        assert!(state1 != state2);

        let not_first_state = Self::new(max_poss_reg.subtract(state1));

        let not_second_state = Self::new(max_poss_reg.subtract(state2));

        not_first_state.union(&not_second_state)
    }

    /// Return the number of bits used in a RegionStore.
    pub fn num_bits(&self) -> Option<usize> {
        if self.is_not_empty() {
            Some(self.items[0].num_bits())
        } else {
            None
        }
    }

    /// Return true if a RegionStore is a superset of a Region.
    #[allow(dead_code)]
    pub fn is_superset_of(&self, other: &Self) -> bool {
        other.subtract(self).is_empty()
    }

    /// Return a regionstore, given a string representation.
    /// Like [], [r1010] or [r1010, r0101].
    pub fn from(rs_str: &str) -> Result<Self, String> {
        //println!("regionstore::from: {rs_str}");

        let mut rs_str2 = String::new();
        let mut last_chr = false;

        for (inx, chr) in rs_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "[" {
                    continue;
                } else {
                    return Err(format!(
                        "RegionStore::from: Invalid string, {rs_str} should start with ["
                    ));
                }
            }
            if chr == "]" {
                last_chr = true;
                continue;
            }

            if last_chr {
                return Err(format!(
                    "RegionStore::from: Invalid string, {rs_str} should end with ]"
                ));
            }
            rs_str2.push_str(chr);
        }
        if !last_chr {
            return Err(format!(
                "RegionStore::from: Invalid string, {rs_str} should end with ]"
            ));
        }

        if rs_str2.is_empty() {
            return Ok(RegionStore::new(vec![]));
        }

        // Split string into <region> tokens.
        let mut token = String::new();
        let mut token_list = Vec::<String>::new();

        for chr in rs_str2.graphemes(true) {
            if chr == "," || chr == " " {
                if token.is_empty() {
                } else {
                    token_list.push(token);
                    token = String::new();
                }
            } else {
                token.push_str(chr);
            }
        }
        if token.is_empty() {
        } else {
            token_list.push(token);
        }
        //println!("token_list {:?}", token_list);

        // println!("token_list2 {:?}", token_list2);

        // Tally up tokens.
        let mut regions = Vec::<SomeRegion>::new();

        for tokenx in token_list.into_iter() {
            match SomeRegion::from(&tokenx) {
                Ok(regx) => regions.push(regx),
                Err(errstr) => return Err(format!("RegionStore::from: {errstr}")),
            }
        }
        let ret_regionstore = RegionStore::new(regions);

        Ok(ret_regionstore)
    }

    /// Extend a RegionStore by emptying another RegionStore.
    pub fn append(&mut self, mut other: Self) {
        self.items.append(&mut other.items);
    }

    /// Return the largest intersections of items.
    pub fn largest_intersections(&self) -> Self {
        let mut intersections = Self::new(vec![]);

        // Check each possible pair.
        for inx in 0..(self.len() - 1) {
            for iny in (inx + 1)..self.len() {
                if let Some(reg_int) = self[inx].intersection(&self[iny]) {
                    intersections.push_nosubs(reg_int);
                }
            }
        }
        intersections
    }

    /// Return self fragmented by intersections.
    /// Each fragment returned will be a subset of any item
    /// it intersects in the original.
    /// All fragments returned will account for all parts of all items
    /// in the original.
    pub fn split_by_intersections(&self) -> Self {
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

        let mut fragments = Self::new(vec![]); // Store to collect successive fragments.

        while !remaining.is_empty() {
            // Get largest intersections.
            let intersections = remaining.largest_intersections();

            // Subtract intersections from regions remaining.
            remaining = remaining.subtract(&intersections);

            // Extract remaining regions, like a cookie cutter.
            let mut next_regions = Self::new(vec![]);
            for regx in self.iter() {
                for intx in intersections.iter() {
                    if let Some(regy) = regx.intersection(intx) {
                        if !next_regions.contains(&regy) {
                            next_regions.push(regy);
                        }
                    }
                }
            }

            // Save non-intersecting fragments.
            fragments.append(remaining);

            // Set up next cycle.
            remaining = next_regions;
        }
        fragments
    }

    /// Return the number of squares used by regions in a RegionStore, without double-counting squares in overlaps.
    #[allow(dead_code)]
    fn number_squares(&self) -> usize {
        //println!("regionstore::number_squares: {self}");
        let mut fragments = self.split_by_intersections();
        // Fragments may overlap.
        // Get fragments of fragments, as needed.
        let mut fragments2 = fragments.split_by_intersections();

        while fragments2.len() != fragments.len() {
            fragments = fragments2;
            fragments2 = fragments.split_by_intersections();
        }

        let mut count = 0;
        for regx in fragments.iter() {
            count += regx.number_squares();
        }
        count
    }
} // end impl RegionStore.

impl Index<usize> for RegionStore {
    type Output = SomeRegion;
    fn index(&self, i: usize) -> &SomeRegion {
        &self.items[i]
    }
}

impl IndexMut<usize> for RegionStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}

impl IntoIterator for RegionStore {
    type Item = SomeRegion;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl AvecRef for RegionStore {
    fn avec_ref(&self) -> &Vec<impl NumBits> {
        &self.items
    }
}

/// Implement the trait StrLen for RegionStore.
impl StrLen for RegionStore {
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
    fn possible_regions_by_negative_inference() -> Result<(), String> {
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::from("r01x1")?); // (5, 7)
        regstr1.push(SomeRegion::from("rx101")?); // (5, F)
        regstr1.push(SomeRegion::from("r0x11")?); // (3, 7)

        let poss_regs1 = regstr1.possible_regions_by_negative_inference();
        println!("poss_regs1: {}", poss_regs1);

        assert!(poss_regs1.len() == 5);
        assert!(poss_regs1.contains(&SomeRegion::from("r0X0X")?));
        assert!(poss_regs1.contains(&SomeRegion::from("rX11X")?));

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(SomeRegion::from("rx10x")?);

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(SomeRegion::from("r01x1")?);
        regstr2.push(SomeRegion::from("r11x1")?);

        // Without additional processing, in RegionStore::intersection, the result would be [0101, 1101].
        let regstr3 = regstr1.intersection(&regstr2);
        println!("results {}", regstr3);
        assert!(regstr3.len() == 2);
        assert!(regstr3.contains(&SomeRegion::from("r0101")?));
        assert!(regstr3.contains(&SomeRegion::from("r1101")?));

        Ok(())
    }

    #[test]
    fn subtract_region() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::from("r0x0x")?);
        regstr.push(SomeRegion::from("r0xx1")?);
        regstr.push(SomeRegion::from("rx1x1")?);
        regstr.push(SomeRegion::from("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let regx = SomeRegion::from("r0101")?;

        let reg_rslt = regstr.subtract_item(&regx);
        println!("results {}", reg_rslt);

        assert!(reg_rslt.len() == 7);
        assert!(reg_rslt.contains(&SomeRegion::from("r0x11")?));
        assert!(reg_rslt.contains(&SomeRegion::from("r00x1")?));
        assert!(reg_rslt.contains(&SomeRegion::from("rx111")?));
        assert!(reg_rslt.contains(&SomeRegion::from("r11x1")?));
        Ok(())
    }

    #[test]
    fn subtract() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);
        regstr.push(SomeRegion::from("r0x0x")?);
        regstr.push(SomeRegion::from("r0xx1")?);
        regstr.push(SomeRegion::from("rx1x1")?);
        regstr.push(SomeRegion::from("rx11x")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(SomeRegion::from("r01x1")?);
        regstr2.push(SomeRegion::from("r0x01")?);

        let regstr3 = regstr.subtract(&regstr2);
        println!("regstr3: {}", regstr3);

        assert!(regstr3.len() == 5);
        assert!(regstr3.contains(&SomeRegion::from("r0x00")?));
        assert!(regstr3.contains(&SomeRegion::from("r0011")?));
        assert!(regstr3.contains(&SomeRegion::from("r11x1")?));
        assert!(regstr3.contains(&SomeRegion::from("rx110")?));
        assert!(regstr3.contains(&SomeRegion::from("r111x")?));
        Ok(())
    }

    #[test]
    fn push_nosups() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::from("r0x0x")?);
        regstr.push(SomeRegion::from("r0xx1")?);
        regstr.push(SomeRegion::from("rx1x1")?);
        regstr.push(SomeRegion::from("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosups(SomeRegion::from("r0111")?);
        println!("results {}", regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::from("r0x0x")?));
        assert!(regstr.contains(&SomeRegion::from("r1110")?));
        assert!(regstr.contains(&SomeRegion::from("r0111")?));
        Ok(())
    }

    #[test]
    fn push_nosubs() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::from("r0x0x")?);
        regstr.push(SomeRegion::from("r0xx1")?);
        regstr.push(SomeRegion::from("rx1x1")?);
        regstr.push(SomeRegion::from("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        regstr.push_nosubs(SomeRegion::from("rxxx1")?);
        println!("results {}", regstr);

        assert!(regstr.len() == 3);
        assert!(regstr.contains(&SomeRegion::from("r0x0x")?));
        assert!(regstr.contains(&SomeRegion::from("r1110")?));
        assert!(regstr.contains(&SomeRegion::from("rxxx1")?));
        Ok(())
    }

    #[test]
    fn any_superset_of_state() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::from("r0x0x")?);
        regstr.push(SomeRegion::from("r0xx1")?);
        regstr.push(SomeRegion::from("rx1x1")?);
        regstr.push(SomeRegion::from("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of_state(&SomeState::from("0b0111")?));
        assert!(!regstr.any_superset_of_state(&SomeState::from("0b1011")?));
        Ok(())
    }

    #[test]
    fn any_intersection() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::from("r0x0x")?);
        regstr.push(SomeRegion::from("r0xx1")?);
        regstr.push(SomeRegion::from("rx1x1")?);
        regstr.push(SomeRegion::from("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_intersection_of(&SomeRegion::from("r1xx1")?));
        assert!(!regstr.any_intersection_of(&SomeRegion::from("r10x1")?));
        Ok(())
    }

    #[test]
    fn any_subset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::from("r0x0x")?);
        regstr.push(SomeRegion::from("r0xx1")?);
        regstr.push(SomeRegion::from("rx1x1")?);
        regstr.push(SomeRegion::from("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_subset_of(&SomeRegion::from("rx11x")?));
        assert!(!regstr.any_subset_of(&SomeRegion::from("r1xx1")?));
        Ok(())
    }

    #[test]
    fn any_superset_of() -> Result<(), String> {
        let mut regstr = RegionStore::with_capacity(4);

        regstr.push(SomeRegion::from("r0x0x")?);
        regstr.push(SomeRegion::from("r0xx1")?);
        regstr.push(SomeRegion::from("rx1x1")?);
        regstr.push(SomeRegion::from("r1110")?);
        // Intersections, 0x01, 01x1.
        // Intersections of intersections, 0101.

        assert!(regstr.any_superset_of(&SomeRegion::from("r01x1")?));
        assert!(!regstr.any_superset_of(&SomeRegion::from("r1xx1")?));
        Ok(())
    }

    // Test calculation of two dissimilar pairs of states effect on possible regions.
    #[test]
    fn two_dissimilar_pairs() -> Result<(), String> {
        let max_reg = SomeRegion::from("rXXXX")?;

        let rslt = RegionStore::new(vec![max_reg.clone()]);

        let state_6 = SomeState::from("0b0110")?;
        let state_a = SomeState::from("0b1010")?;

        let not_state_6 = RegionStore::new(max_reg.subtract(&state_6));
        let not_state_a = RegionStore::new(max_reg.subtract(&state_a));
        let rslt = rslt.intersection(&not_state_6.union(&not_state_a));

        let state_4 = SomeState::from("0b0100")?;
        let state_d = SomeState::from("0b1101")?;

        let not_state_4 = RegionStore::new(max_reg.subtract(&state_4));
        let not_state_d = RegionStore::new(max_reg.subtract(&state_d));

        let rslt = rslt.intersection(&not_state_4.union(&not_state_d));

        println!("result regions {}", rslt);

        assert!(rslt.len() == 7);

        // Test all possible 4-bit regions, with at least one X bit position.
        // 3^4 - 2^4 = 65 regions.
        for x in 0..16 {
            for y in 0..16 {
                if y == x {
                    continue;
                }

                let regx = SomeRegion::new(vec![
                    SomeState::from(&format!("0x{:x}", x))?,
                    SomeState::from(&format!("0x{:x}", y))?,
                ]);

                if (regx.is_superset_of(&state_6) && regx.is_superset_of(&state_a))
                    || (regx.is_superset_of(&state_4) && regx.is_superset_of(&state_d))
                {
                    // Check there is no superset region in the result.
                    if rslt.any_superset_of(&regx) {
                        return Err(format!("Superset of {} found in {}", regx, rslt));
                    }
                } else {
                    // Check there is a superset in the result.
                    if !rslt.any_superset_of(&regx) {
                        return Err(format!("Superset of {} NOT found in {}", regx, rslt));
                    }
                }
            } // next y
        } // next x

        Ok(())
    }

    #[test]
    fn is_superset_of() -> Result<(), String> {
        let regs = vec![SomeRegion::from("rx1X1")?, SomeRegion::from("r1xx1")?];
        let regstr1 = RegionStore::new(regs);
        println!("regstr1 {regstr1}");

        // Test single SomeRegion superset.
        let sub1 = RegionStore::from("[r11x1]")?;
        println!("sub1 {sub1}");

        if regstr1.is_superset_of(&sub1) {
            println!("test 1 OK");
        } else {
            return Err("test1 failed".to_string());
        }

        // Test intersections that add up to a superset.
        let sub2 = RegionStore::from("[r1x01]")?;
        println!("sub2 {sub2}");

        if regstr1.is_superset_of(&sub2) {
            println!("test 2 OK");
        } else {
            return Err("test2 failed".to_string());
        }

        // Test intersections that do not add up to a superset.
        let sub3 = RegionStore::from("[r100x]")?;
        println!("sub3 {sub3}");

        if regstr1.is_superset_of(&sub3) {
            return Err("test3 failed".to_string());
        } else {
            println!("test 3 OK");
        }

        // Test no intersections.
        let sub4 = RegionStore::from("[r1x00]")?;
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
        let regst1 = RegionStore::from("[]")?;
        println!("regst1 {regst1}");
        assert!(format!("{regst1}") == "[]");

        let regst2 = RegionStore::from("[r1010]")?;
        println!("regst2 {regst2}");
        assert!(format!("{regst2}") == "[r1010]");

        let regst3 = RegionStore::from("[r1010, r1111]")?;
        println!("regst3 {regst3}");
        assert!(format!("{regst3}") == "[r1010, r1111]");

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn split_by_intersections() -> Result<(), String> {
        // Test empty RS.
        let regst1 = RegionStore::from("[]")?;
        let fragments1 = regst1.split_by_intersections();
        println!("fragments1 {fragments1}");
        assert!(fragments1.len() == 0);

        // Test single region RS.
        let regst2 = RegionStore::from("[r0x11]")?;
        let fragments2 = regst2.split_by_intersections();
        println!("fragments2 {fragments2}");
        assert!(fragments2.len() == 1);

        // Test two non-intersecting regions RS.
        let regst3 = RegionStore::from("[r0011, r1xx1]")?;
        let fragments3 = regst3.split_by_intersections();
        println!("fragments3 {fragments3}");
        assert!(fragments3.len() == 2);

        let regst4 = RegionStore::from("[r01x1, r011x]")?;
        let fragments4 = regst4.split_by_intersections();
        println!("fragments4 {fragments4}");
        assert!(fragments4.len() == 3);
        assert!(fragments4.contains(&SomeRegion::from("r0101")?));
        assert!(fragments4.contains(&SomeRegion::from("r0110")?));
        assert!(fragments4.contains(&SomeRegion::from("r0111")?));

        let regst5 = RegionStore::from("[r01x1, r011x, r0x11]")?;
        let fragments5 = regst5.split_by_intersections();
        println!("fragments5 {fragments5}");
        assert!(fragments5.len() == 4);
        assert!(fragments5.contains(&SomeRegion::from("r0101")?));
        assert!(fragments5.contains(&SomeRegion::from("r0110")?));
        assert!(fragments5.contains(&SomeRegion::from("r0011")?));
        assert!(fragments5.contains(&SomeRegion::from("r0111")?));

        let regst6 = RegionStore::from("[r01x1, r011x, rxx11]")?;
        let fragments6 = regst6.split_by_intersections();
        println!("fragments6 {fragments6}");
        assert!(fragments6.len() == 5);
        assert!(fragments6.contains(&SomeRegion::from("r0101")?));
        assert!(fragments6.contains(&SomeRegion::from("r0110")?));
        assert!(fragments6.contains(&SomeRegion::from("rx011")?));
        assert!(fragments6.contains(&SomeRegion::from("r1x11")?));
        assert!(fragments6.contains(&SomeRegion::from("r0111")?));

        let regst7 = RegionStore::from("[rx10x, rx1x1]")?;
        let fragments7 = regst7.split_by_intersections();
        println!("fragments7 {fragments7}");
        assert!(fragments7.len() == 3);
        assert!(fragments7.contains(&SomeRegion::from("rX100")?));
        assert!(fragments7.contains(&SomeRegion::from("rX111")?));
        assert!(fragments7.contains(&SomeRegion::from("rX101")?));

        let regst8 = RegionStore::from("[rxXXX, rx1x1, r01x1, rx111]")?;
        let fragments8 = regst8.split_by_intersections();
        println!("fragments8 {fragments8}");
        assert!(fragments8.len() == 6);
        assert!(fragments8.contains(&SomeRegion::from("rXXX0")?));
        assert!(fragments8.contains(&SomeRegion::from("rX0XX")?));
        assert!(fragments8.contains(&SomeRegion::from("r1101")?));
        assert!(fragments8.contains(&SomeRegion::from("r0101")?));
        assert!(fragments8.contains(&SomeRegion::from("r1111")?));
        assert!(fragments8.contains(&SomeRegion::from("r0111")?));

        let regst9 = RegionStore::from("[rxXXX, rx1x1, r01x1]")?;
        let fragments9 = regst9.split_by_intersections();
        println!("fragments9 {fragments9}");
        assert!(fragments9.len() == 4);
        assert!(fragments9.contains(&SomeRegion::from("rXXX0")?));
        assert!(fragments9.contains(&SomeRegion::from("rX0XX")?));
        assert!(fragments9.contains(&SomeRegion::from("r11X1")?));
        assert!(fragments9.contains(&SomeRegion::from("r01X1")?));

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn number_squares() -> Result<(), String> {
        // Test empty RS.
        let regst1 = RegionStore::from("[]")?;
        let num_sqrs1 = regst1.number_squares();
        println!("num_sqrs1 {regst1} = {num_sqrs1}");
        assert!(num_sqrs1 == 0);

        // Test single region RS.
        let regst2 = RegionStore::from("[r0x11]")?;
        let num_sqrs2 = regst2.number_squares();
        println!("num_sqrs2 {regst2} = {num_sqrs2}");
        assert!(num_sqrs2 == 2);

        // Test two non-intersecting regions RS.
        let regst3 = RegionStore::from("[r0011, r1xx1]")?;
        let num_sqrs3 = regst3.number_squares();
        println!("num_sqrs3 {regst3} = {num_sqrs3}");
        assert!(num_sqrs3 == 5);

        // Two intersecting regions, 7 is in two regions.
        let regst4 = RegionStore::from("[r01x1, r011x]")?;
        let num_sqrs4 = regst4.number_squares();
        println!("num_sqrs4 {regst4} = {num_sqrs4}");
        assert!(num_sqrs4 == 3);

        // Three intersecting regions, 7 in two regions, 6 in two regions.
        let regst5 = RegionStore::from("[r01x1, r011x, rx110]")?;
        let num_sqrs5 = regst5.number_squares();
        println!("num_sqrs5 {regst5} = {num_sqrs5}");
        assert!(num_sqrs5 == 4);

        // Three regions, 5 in two regions, 011X in two regions.
        let regst6 = RegionStore::from("[r01xx, rxx01, x11x]")?;
        let num_sqrs6 = regst6.number_squares();
        println!("num_sqrs6 {regst6} = {num_sqrs6}");
        assert!(num_sqrs6 == 9);

        // Two regions, X101 in two regions.
        let regst7 = RegionStore::from("[rx10x, rx1x1]")?;
        let num_sqrs7 = regst7.number_squares();
        println!("num_sqrs7 {regst7} = {num_sqrs7}");
        assert!(num_sqrs7 == 6);

        // Four regions, 5 in four regions.
        let regst8 = RegionStore::from("[r0x01, rx101, r010x, 01x1]")?;
        let num_sqrs8 = regst8.number_squares();
        println!("num_sqrs8 {regst8} = {num_sqrs8}");
        assert!(num_sqrs8 == 5);

        // Four regions, 5, 7, D, F, all of the squares, each in two regions.
        let regst9 = RegionStore::from("[r01x1, rx111, r11x1, x101]")?;
        let num_sqrs9 = regst9.number_squares();
        println!("num_sqrs9 {regst9} = {num_sqrs9}");
        assert!(num_sqrs9 == 4);

        // Test subset region.
        let regst10 = RegionStore::from("[rxxx1, r0x01]")?;
        let num_sqrs10 = regst10.number_squares();
        println!("num_sqrs10 {regst10} = {num_sqrs10}");
        assert!(num_sqrs10 == 8);

        let regst11 = RegionStore::from("[rxXXX, rx1x1, r01x1, rx111]")?;
        let num_sqrs11 = regst11.number_squares();
        println!("num_sqrs11 {regst11} = {num_sqrs11}");
        assert!(num_sqrs11 == 16);

        //assert!(1 == 2);
        Ok(())
    }
}
