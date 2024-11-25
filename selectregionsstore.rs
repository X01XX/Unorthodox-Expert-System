//! Implement a struct of SelectRegionStore.

use crate::regionscorr::RegionsCorr;
use crate::regionscorrstore::RegionsCorrStore;
use crate::selectregions::SelectRegions;
use crate::statescorr::StatesCorr;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::{Iter, IterMut};

impl fmt::Display for SelectRegionsStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl PartialEq for SelectRegionsStore {
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
impl Eq for SelectRegionsStore {}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
/// A struct of SelectRegions.
pub struct SelectRegionsStore {
    pub items: Vec<SelectRegions>,
}

impl SelectRegionsStore {
    /// Return a new SelectRegionsStores instance.
    /// Duplicate RegionsCorr not allowed.
    pub fn new(items: Vec<SelectRegions>) -> Self {
        debug_assert!(
            items.len() < 2 || {
                let mut ret = true;
                for itemx in items.iter().skip(1) {
                    if itemx.num_bits_vec() != items[0].num_bits_vec() {
                        ret = false;
                        break;
                    }
                }
                ret
            }
        );
        let mut ret_srs = Self { items: vec![] };

        // Test for duplicates.
        for selx in items {
            ret_srs.push(selx);
        }
        ret_srs
    }

    /// Return a new SelectRegions instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SelectRegions>::with_capacity(num),
        }
    }

    /// Add a SelectRegionsStore.
    /// Duplicate RegionsCorr not allowed.
    pub fn push(&mut self, select: SelectRegions) {
        //print!("{} push {}", self, select);
        debug_assert!(self.is_empty() || select.num_bits_vec() == self.items[0].num_bits_vec());

        assert!(!self.contains(&select));

        self.items.push(select);
    }

    /// Add a SelectRegionsStore, deleting subsets.
    pub fn push_nosubs(&mut self, select: SelectRegions) {
        debug_assert!(self.is_empty() || select.num_bits_vec() == self.items[0].num_bits_vec());

        // Don't add a subset.
        if self.any_supersets_of(&select) {
            return;
        }
        // Identify subsets by index.
        let mut del = self
            .items
            .iter()
            .enumerate()
            .filter_map(|(inx, regstx)| {
                if regstx.regions.is_subset_of(&select.regions) {
                    Some(inx)
                } else {
                    None
                }
            })
            .collect::<Vec<usize>>();

        // Remove subsets, highest indicies first.
        del.sort();
        for inx in del.iter().rev() {
            tools::remove_unordered(&mut self.items, *inx);
        }
        // Add new select instance.
        self.items.push(select);
    }

    /// Add a SelectRegionsStore, deleting supersets.
    pub fn push_nosups(&mut self, select: SelectRegions) {
        debug_assert!(self.is_empty() || select.num_bits_vec() == self.items[0].num_bits_vec());

        //print!("{} push {}", self, select);
        // Don't add a superset.
        if self.any_subsets_of(&select) {
            return;
        }
        // Identify supersets by index.
        let mut del = self
            .items
            .iter()
            .enumerate()
            .filter_map(|(inx, regstx)| {
                if regstx.regions.is_superset_of(&select.regions) {
                    Some(inx)
                } else {
                    None
                }
            })
            .collect::<Vec<usize>>();

        // Remove subsets, highest indicies first.
        del.sort();
        for inx in del.iter().rev() {
            tools::remove_unordered(&mut self.items, *inx);
        }
        // Add new select instance.
        self.items.push(select);
    }

    /// Return the length of an instance.
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

    /// Return an iterator
    pub fn iter(&self) -> Iter<SelectRegions> {
        self.items.iter()
    }

    /// Return an mut iterator
    pub fn iter_mut(&mut self) -> IterMut<SelectRegions> {
        self.items.iter_mut()
    }

    /// Return a Vector of SelectRegions not supersets of a given RegionsCorr.
    pub fn _not_supersets_of(&self, regs: &RegionsCorr) -> Vec<&SelectRegions> {
        debug_assert!(self.is_empty() || regs.num_bits_vec() == self.items[0].num_bits_vec());

        self.items
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_of(regs))
            .collect()
    }

    /// Return true if any SelectRegions is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &StatesCorr) -> bool {
        debug_assert!(self.is_empty() || stas.num_bits_vec() == self.items[0].num_bits_vec());

        for regsx in &self.items {
            if regsx.regions.is_superset_states(stas) {
                return true;
            }
        }
        false
    }

    /// Return the sum of values of SelectRegions that are superset of a given RegionsCorr.
    pub fn rate_regions(&self, regs: &RegionsCorr) -> isize {
        debug_assert!(self.is_empty() || regs.num_bits_vec() == self.items[0].num_bits_vec());

        self.items
            .iter()
            .filter_map(|selx| {
                if selx.regions.is_superset_of(regs) {
                    Some(selx.net_value)
                } else {
                    None
                }
            })
            .sum()
    }

    /// Return true if a RegionsCorr instance is a subset af any negative SelectRegions.
    pub fn in_negative_regions(&self, regs: &RegionsCorr) -> bool {
        debug_assert!(self.is_empty() || regs.num_bits_vec() == self.items[0].num_bits_vec());

        for selx in self.items.iter() {
            if selx.net_value < 0 && selx.regions.is_superset_of(regs) {
                return true;
            }
        }
        false
    }

    /// Return true if any SelectRegion is a region superset of another..
    fn any_supersets_of(&self, slrx: &SelectRegions) -> bool {
        debug_assert!(self.is_empty() || slrx.num_bits_vec() == self.items[0].num_bits_vec());

        for regsx in &self.items {
            if regsx.regions.is_superset_of(&slrx.regions) {
                return true;
            }
        }
        false
    }

    /// Return true if any SelectRegion is a region subset of another.
    fn any_subsets_of(&self, slrx: &SelectRegions) -> bool {
        debug_assert!(self.is_empty() || slrx.num_bits_vec() == self.items[0].num_bits_vec());

        for regsx in &self.items {
            if regsx.regions.is_subset_of(&slrx.regions) {
                return true;
            }
        }
        false
    }

    /// Return the intersection of a SelectRegionStore and a Selectregions instance.
    pub fn _intersection_item(&self, slrx: &SelectRegions) -> Option<Self> {
        debug_assert!(self.is_empty() || slrx.num_bits_vec() == self.items[0].num_bits_vec());

        let mut ret = Self::new(vec![]);
        for inx in 0..self.len() {
            if let Some(regs) = self[inx].intersection(slrx) {
                ret.push_nosubs(regs);
            }
        }
        if ret.is_empty() {
            return None;
        }
        Some(ret)
    }

    /// Return the intersection of a SelectRegionStore and a Selectregions instance.
    pub fn intersection(&self, other: &SelectRegionsStore) -> Option<Self> {
        debug_assert!(self.is_empty() || self[0].num_bits_vec() == other[0].num_bits_vec());

        let mut ret = Self::new(vec![]);
        for inx in 0..(self.len() - 1) {
            for iny in (inx + 1)..self.len() {
                if let Some(regs) = self[inx].intersection(&other[iny]) {
                    ret.push_nosubs(regs);
                }
            }
        }
        if ret.is_empty() {
            return None;
        }
        Some(ret)
    }

    /// Return true if any item intersects a given SelectRegion.
    pub fn any_intersection_of(&self, slrx: &SelectRegions) -> bool {
        debug_assert!(self.is_empty() || slrx.num_bits_vec() == self.items[0].num_bits_vec());

        for sely in self.items.iter() {
            if sely.intersects(slrx) {
                return true;
            }
        }
        false
    }

    /// Return list of select regions that are superset of a State vector.
    pub fn supersets_of_states(&self, stas: &StatesCorr) -> Vec<&SelectRegions> {
        debug_assert!(self.is_empty() || stas.num_bits_vec() == self.items[0].num_bits_vec());

        self.items
            .iter()
            .filter(|regsx| regsx.regions.is_superset_states(stas))
            .collect()
    }

    /// Return a string represeting an SelectRegionsStore.
    fn formatted_string(&self) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in self.items.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&orx.to_string());
        }
        ret_str.push(']');
        ret_str
    }

    /// Return true if an equal RegionsCorr is already in the SelectRegionsStore.
    pub fn contains(&self, slrx: &SelectRegions) -> bool {
        //println!("selectregionsstore::contains: store: {self} arg: {slrx}");
        debug_assert!(self.is_empty() || slrx.num_bits_vec() == self.items[0].num_bits_vec());

        for regstrx in &self.items {
            if regstrx.regions == slrx.regions {
                return true;
            }
        }
        false
    }

    /// Append from another store.
    pub fn append(&mut self, mut other: Self) {
        debug_assert!(
            self.is_empty()
                || other.is_empty()
                || other.items[0].num_bits_vec() == self.items[0].num_bits_vec()
        );

        self.items.append(&mut other.items);
    }

    /// Pop the last item.
    pub fn pop(&mut self) -> Option<SelectRegions> {
        self.items.pop()
    }

    /// Subtract a SelectRegions.
    pub fn subtract_selectregions(&self, subtrahend: &SelectRegions) -> Self {
        // println!("subtract {subtrahend} from {self}");
        debug_assert!(self.is_empty() || subtrahend.num_bits_vec() == self.items[0].num_bits_vec());

        let mut ret_str = Self::new(vec![]);

        for regy in self.iter() {
            if subtrahend.intersects(regy) {
                for regsz in regy.subtract(subtrahend) {
                    ret_str.push_nosubs(regsz);
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Subtract a selectregionstore from another.
    pub fn subtract(&self, other: &Self) -> Self {
        debug_assert!(
            self.is_empty()
                || other.is_empty()
                || other.items[0].num_bits_vec() == self.items[0].num_bits_vec()
        );

        let mut ret = self.clone();

        for selx in other.items.iter() {
            if ret.any_intersection_of(selx) {
                ret = ret.subtract_selectregions(selx);
            }
        }
        ret
    }

    /// Return self fragmented by intersections.
    pub fn split_by_intersections(&self) -> Self {
        //println!("selectregionsstore::split_by_intersections: {self}");
        if self.len() < 2 {
            return self.clone();
        }

        // Create RegionsCorrStore from SelectRegionsStore.
        let mut fragments = RegionsCorrStore::new(vec![]);
        for selx in self.items.iter() {
            fragments.push(selx.regions.clone());
        }

        // Calc intersections.
        fragments = fragments.split_by_intersections();

        //println!("fragments: {fragments}");

        let mut sel_fragments = Vec::<SelectRegions>::with_capacity(fragments.len());
        for regsx in fragments {
            // Calc positive and negative value for RegionsCorr.
            let mut pos_value = 0;
            let mut neg_value = 0;
            for sely in self.iter() {
                if sely.regions.is_superset_of(&regsx) {
                    pos_value += sely.pos_value;
                    neg_value += sely.neg_value;
                }
            }
            // Create a SelectRegions for the RegionsCorr.
            let mut selx = SelectRegions::new(regsx, 1);
            selx.set_values(pos_value, neg_value);

            // Save SelectRegions to vector.
            sel_fragments.push(selx);
        }
        Self::new(sel_fragments)
    }
} // End impl SelectRegionsStore

impl Index<usize> for SelectRegionsStore {
    type Output = SelectRegions;
    fn index(&self, i: usize) -> &SelectRegions {
        &self.items[i]
    }
}

impl IntoIterator for SelectRegionsStore {
    type Item = SelectRegions;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_by_intersections1() -> Result<(), String> {
        // Try with empty RegionStore.
        let srs1 = SelectRegionsStore::new(vec![]);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} is {frags}");
        assert!(frags == srs1);

        // Try with no intersections
        let mut srs1 = SelectRegionsStore::new(vec![]);

        let regstr1 = SelectRegions::from("SR[RC[r0xx1, r1x1x], -1]")?;
        srs1.push(regstr1);

        let regstr2 = SelectRegions::from("SR[RC[r1x0x, rx1x1], 2]")?;
        srs1.push(regstr2);

        let frags = srs1.split_by_intersections();
        println!("Fragments of:");
        for srsx in srs1.iter() {
            println!("    {srsx}");
        }
        println!("Are:");
        for srsx in frags.iter() {
            println!("    {srsx}");
        }

        assert!(frags == srs1);
        Ok(())
    }

    #[test]
    fn split_by_intersections2() -> Result<(), String> {
        // Try one level of intersection.
        let mut srs1 = SelectRegionsStore::new(vec![]);

        let regstr1 = SelectRegions::from("SR[RC[r0x0x, r0x0x], 1]")?;
        srs1.push(regstr1);

        let regstr2 = SelectRegions::from("SR[RC[rx1x1, rx1x1], 2]")?;
        srs1.push(regstr2);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 9);

        // Check for non-subset intersections.
        for selx in frags.iter() {
            for sely in srs1.iter() {
                if selx.intersects(sely) && !selx.is_subset_of(sely) {
                    return Err(format!("{selx} intersects {sely} ?"));
                }
            }
        }

        // Check fragments cover whole area.
        let dif = srs1.subtract(&frags);
        println!("dif {}", dif);
        assert!(dif.is_empty());

        // Check fragment values.
        for selx in frags.iter() {
            let mut pos_val = 0;
            let mut neg_val = 0;
            // Gather values from start SelectRegionsStore.
            for sely in srs1.iter() {
                if selx.is_subset_of(sely) {
                    pos_val += sely.pos_value;
                    neg_val += sely.neg_value;
                }
            }
            assert!(pos_val == selx.pos_value);
            assert!(neg_val == selx.neg_value);
            assert!(pos_val + neg_val == selx.net_value);
        }

        // Check no fragments are supersets (or equal) any other, or intersect.
        // Check each possible pair.
        for inx in 0..(frags.len() - 1) {
            for iny in (inx + 1)..frags.len() {
                if frags[inx].is_superset_of(&frags[iny]) {
                    return Err(format!("{} is a superset of {}", frags[inx], frags[iny]));
                }
                if frags[iny].is_superset_of(&frags[inx]) {
                    return Err(format!("{} is a superset of {}", frags[iny], frags[inx]));
                }
            }
        }
        Ok(())
    }

    #[test]
    fn split_by_intersections3() -> Result<(), String> {
        // Try two levels of intersection.
        let mut srs1 = SelectRegionsStore::new(vec![]);

        let regstr1 = SelectRegions::from("SR[RC[r0x0x], 1]")?;
        srs1.push(regstr1);

        let regstr2 = SelectRegions::from("SR[RC[rx1x1], 2]")?;
        srs1.push(regstr2);

        let regstr3 = SelectRegions::from("SR[RC[rx10x], 3]")?;
        srs1.push(regstr3);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 6);

        // Check for non-subset intersections.
        for selx in frags.iter() {
            for sely in srs1.iter() {
                if selx.intersects(sely) && !selx.is_subset_of(sely) {
                    return Err(format!("{selx} intersects {sely} ?"));
                }
            }
        }

        // Check fragments cover whole area.
        let dif = srs1.subtract(&frags);
        println!("dif {}", dif);
        assert!(dif.is_empty());

        // Check fragment values.
        for selx in frags.iter() {
            let mut pos_val = 0;
            let mut neg_val = 0;
            // Gather values from start SelectRegionsStore.
            for sely in srs1.iter() {
                if selx.is_subset_of(sely) {
                    pos_val += sely.pos_value;
                    neg_val += sely.neg_value;
                }
            }
            assert!(pos_val == selx.pos_value);
            assert!(neg_val == selx.neg_value);
            assert!(pos_val + neg_val == selx.net_value);
        }

        // Check no fragments are supersets (or equal) any other.
        // Check each possible pair.
        for inx in 0..(frags.len() - 1) {
            for iny in (inx + 1)..frags.len() {
                if frags[inx].is_superset_of(&frags[iny]) {
                    return Err(format!("{} is a superset of {}", frags[inx], frags[iny]));
                }
                if frags[iny].is_superset_of(&frags[inx]) {
                    return Err(format!("{} is a superset of {}", frags[iny], frags[inx]));
                }
            }
        }

        Ok(())
    }

    #[test]
    fn split_by_intersections4() -> Result<(), String> {
        // Try one level of intersection, two region select regions..
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::from("SR[RC[r0xx1, r1x1x], -1]")?;
        srs1.push(regstr1);

        let regstr2 = SelectRegions::from("SR[RC[rxx0x, rx1x1], 2]")?;
        srs1.push(regstr2);

        let regstr3 = SelectRegions::from("SR[RC[rxxxx, rxxxx], 1]")?;
        srs1.push(regstr3);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 20);

        // Check for non-subset intersections.
        for selx in frags.iter() {
            for sely in srs1.iter() {
                if selx.intersects(sely) && !selx.is_subset_of(sely) {
                    return Err(format!("{selx} intersects {sely} ?"));
                }
            }
        }

        // Check fragments cover whole area.
        let dif = srs1.subtract(&frags);
        println!("dif {}", dif);
        assert!(dif.is_empty());

        // Check fragment values.
        for selx in frags.iter() {
            let mut pos_val = 0;
            let mut neg_val = 0;
            // Gather values from start SelectRegionsStore.
            for sely in srs1.iter() {
                if selx.is_subset_of(sely) {
                    pos_val += sely.pos_value;
                    neg_val += sely.neg_value;
                }
            }
            assert!(pos_val == selx.pos_value);
            assert!(neg_val == selx.neg_value);
            assert!(pos_val + neg_val == selx.net_value);
        }

        // Check no fragments are supersets (or equal) any other.
        // Check each possible pair.
        for inx in 0..(frags.len() - 1) {
            for iny in (inx + 1)..frags.len() {
                if frags[inx].is_superset_of(&frags[iny]) {
                    return Err(format!("{} is a superset of {}", frags[inx], frags[iny]));
                }
                if frags[iny].is_superset_of(&frags[inx]) {
                    return Err(format!("{} is a superset of {}", frags[iny], frags[inx]));
                }
            }
        }
        Ok(())
    }

    #[test]
    fn split_by_intersections5() -> Result<(), String> {
        // Try three levels of intersection.
        let mut srs1 = SelectRegionsStore::new(vec![]);

        let regstr1 = SelectRegions::from("SR[RC[r0xxx], 4]")?;
        srs1.push(regstr1);

        let regstr2 = SelectRegions::from("SR[RC[rxx1x], 3]")?;
        srs1.push(regstr2);

        let regstr3 = SelectRegions::from("SR[RC[rx1x1], 2]")?;
        srs1.push(regstr3);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 10);

        // Check 3-level intersection.
        let selx = SelectRegions::from("SR[RC[r0111], 9]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check 2-level intersection (1 of 4).
        let selx = SelectRegions::from("SR[RC[r0101], 6]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check 2-level intersection (2 of 4).
        let selx = SelectRegions::from("SR[RC[r1111], 5]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check 2-level intersection (3 of 4).
        let selx = SelectRegions::from("SR[RC[r001x], 7]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check 2-level intersection (4 of 4).
        let selx = SelectRegions::from("SR[RC[r0x10], 7]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (1 of 5).
        let selx = SelectRegions::from("SR[RC[r1101], 2]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (2 of 5).
        let selx = SelectRegions::from("SR[RC[r000x], 4]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (3 of 5).
        let selx = SelectRegions::from("SR[RC[r0x00], 4]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (4 of 5).
        let selx = SelectRegions::from("SR[RC[r101x], 3]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (5 of 5).
        let selx = SelectRegions::from("SR[RC[r1x10], 3]")?;
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check for non-subset intersections.
        for selx in frags.iter() {
            for sely in srs1.iter() {
                if selx.intersects(sely) && !selx.is_subset_of(sely) {
                    return Err(format!("{selx} intersects {sely} ?"));
                }
            }
        }

        // Check fragments cover whole area.
        let dif = srs1.subtract(&frags);
        println!("dif {}", dif);
        assert!(dif.is_empty());

        // Check fragment values.
        for selx in frags.iter() {
            let mut pos_val = 0;
            let mut neg_val = 0;
            // Gather values from start SelectRegionsStore.
            for sely in srs1.iter() {
                if selx.is_subset_of(sely) {
                    pos_val += sely.pos_value;
                    neg_val += sely.neg_value;
                }
            }
            assert!(pos_val == selx.pos_value);
            assert!(neg_val == selx.neg_value);
            assert!(pos_val + neg_val == selx.net_value);
        }

        // Check no fragments are supersets (or equal) any other.
        // Check each possible pair.
        for inx in 0..(frags.len() - 1) {
            for iny in (inx + 1)..frags.len() {
                if frags[inx].is_superset_of(&frags[iny]) {
                    return Err(format!("{} is a superset of {}", frags[inx], frags[iny]));
                }
                if frags[iny].is_superset_of(&frags[inx]) {
                    return Err(format!("{} is a superset of {}", frags[iny], frags[inx]));
                }
            }
        }

        Ok(())
    }

    #[test]
    fn subtract_selectregions1() -> Result<(), String> {
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::from("SR[RC[rxx0x], 1]")?;
        srs1.push(regstr1.clone());

        let mut srs2 = SelectRegionsStore::new(vec![]);
        let regstr2 = SelectRegions::from("SR[RC[rx1x1], 2]")?;
        srs2.push(regstr2.clone());

        // Try simple subtract_selectregions.
        let srs3 = srs1.subtract_selectregions(&regstr2);
        if srs3.is_empty() {
            return Err(format!("{} - {} = None ?", srs1, srs2));
        } else {
            println!("{} - {} = {}", srs1, regstr2, srs3);
            if srs3.len() == 2 {
            } else {
                return Err(format!("{} - {} = {} ?", srs1, regstr2, srs3));
            }
            let srsexp = SelectRegions::from("SR[RC[rxx00], 1]")?;
            if srs3.contains(&srsexp) {
            } else {
                return Err(format!(
                    "{srs1} - {regstr2} = {srs3} does not contain {srsexp}?"
                ));
            }

            let srsexp = SelectRegions::from("SR[RC[rx00x], 1]")?;
            if srs3.contains(&srsexp) {
            } else {
                return Err(format!(
                    "{srs1} - {regstr2} = {srs3} does not contain {srsexp}?"
                ));
            }
        }

        // Try the reverse subtract.
        let srs3 = srs2.subtract_selectregions(&regstr1);
        if srs3.is_empty() {
            return Err(format!("{} - {} = None ?", srs2, regstr1));
        } else {
            println!("{} - {} = {}", srs2, regstr1, srs3);
            if srs3.len() == 1 {
            } else {
                return Err(format!("{} - {} = {} ?", srs2, regstr1, srs3));
            }
            let srsexp = SelectRegions::from("SR[RC[rx111], 2]")?;
            if srs3.contains(&srsexp) {
            } else {
                return Err(format!(
                    "{srs2} - {regstr1} = {srs3} does not contain {srsexp}?"
                ));
            }
        }

        // Try subtract non-intersecting selectregions.
        let mut srs3 = SelectRegionsStore::new(vec![]);
        let regstr3 = SelectRegions::from("SR[RC[rx101], 1]")?;
        srs3.push(regstr3.clone());
        let srs4 = srs3.subtract_selectregions(&regstr1);
        if srs4.is_empty() {
            println!("{} - {} = None", srs3, regstr1);
        } else {
            return Err(format!("{} - {} = {} ?", srs3, regstr1, srs4));
        }

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn subtract_selectregions2() -> Result<(), String> {
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::from("SR[RC[rxx0x, r01xx], 1]")?;
        srs1.push(regstr1.clone());

        let mut srs2 = SelectRegionsStore::new(vec![]);
        let regstr2 = SelectRegions::from("SR[RC[rx1x1, rxx11], 2]")?;
        srs2.push(regstr2.clone());

        // Try simple subtract_selectregions.
        let srs3 = srs1.subtract_selectregions(&regstr2);
        if srs3.is_empty() {
            return Err(format!("{} - {} = None ?", srs1, regstr2));
        } else {
            println!("{} - {} = {}", srs1, regstr2, srs3);
            if srs3.len() == 4 {
            } else {
                return Err(format!("{} - {} = {} ?", srs1, regstr2, srs3));
            }

            let sr_tmp1 = SelectRegions::from("SR[RC[rxx00, r01xx], 1]")?;

            if srs3.contains(&sr_tmp1) {
            } else {
                return Err(format!(
                    "1 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp1}?"
                ));
            }

            let sr_tmp2 = SelectRegions::from("SR[RC[rx00x, r01xx], 1]")?;

            if srs3.contains(&sr_tmp2) {
            } else {
                return Err(format!(
                    "2 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp2}?"
                ));
            }

            let sr_tmp3 = SelectRegions::from("SR[RC[rxx0x, r01x0], 1]")?;

            if srs3.contains(&sr_tmp3) {
            } else {
                return Err(format!(
                    "3 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp3}?"
                ));
            }

            let sr_tmp4 = SelectRegions::from("SR[RC[rxx0x, r010x], 1]")?;

            if srs3.contains(&sr_tmp4) {
            } else {
                return Err(format!(
                    "4 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp4}?"
                ));
            }
        }

        Ok(())
    }

    #[test]
    fn split_by_intersections6() -> Result<(), String> {
        // Try one level of intersection.
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::from("SR[RC[r0x0x], 1]")?;
        srs1.push(regstr1);

        let regstr2 = SelectRegions::from("SR[RC[r0x1x], 2]")?;
        srs1.push(regstr2);

        let regstr3 = SelectRegions::from("SR[RC[r01x1], -1]")?;
        srs1.push(regstr3);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 6);

        let selx = SelectRegions::from("SR[RC[r0101], 1]")?;
        let sely = SelectRegions::from("SR[RC[r0101], -1]")?;
        if let Some(selz) = sely.intersection(&selx) {
            if !frags.contains(&selz) {
                return Err(format!("fragments {frags} does not contain {selz}"));
            }
        }
        let selx = SelectRegions::from("SR[RC[r0111], 2]")?;
        let sely = SelectRegions::from("SR[RC[r0111], -1]")?;
        if let Some(selz) = sely.intersection(&selx) {
            if !frags.contains(&selz) {
                return Err(format!("fragments {frags} does not contain {selz}"));
            }
        }

        // Check for non-subset intersections.
        for selx in frags.iter() {
            for sely in srs1.iter() {
                if selx.intersects(sely) && !selx.is_subset_of(sely) {
                    return Err(format!("{selx} intersects {sely} ?"));
                }
            }
        }

        // Check fragments cover whole area.
        let dif = srs1.subtract(&frags);
        println!("dif {}", dif);
        assert!(dif.is_empty());

        // Check fragment values.
        for selx in frags.iter() {
            let mut pos_val = 0;
            let mut neg_val = 0;
            // Gather values from start SelectRegionsStore.
            for sely in srs1.iter() {
                if selx.is_subset_of(sely) {
                    pos_val += sely.pos_value;
                    neg_val += sely.neg_value;
                }
            }
            assert!(pos_val == selx.pos_value);
            assert!(neg_val == selx.neg_value);
            assert!(pos_val + neg_val == selx.net_value);
        }

        // Check no fragments are supersets (or equal) any other.
        // Check each possible pair.
        for inx in 0..(frags.len() - 1) {
            for iny in (inx + 1)..frags.len() {
                if frags[inx].is_superset_of(&frags[iny]) {
                    return Err(format!("{} is a superset of {}", frags[inx], frags[iny]));
                }
                if frags[iny].is_superset_of(&frags[inx]) {
                    return Err(format!("{} is a superset of {}", frags[iny], frags[inx]));
                }
            }
        }
        Ok(())
    }

    #[test]
    fn split_by_intersections7() -> Result<(), String> {
        // Try one level of intersection.
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::from("SR[RC[rxx0x], 1]")?;
        srs1.push(regstr1);

        let regstr2 = SelectRegions::from("SR[RC[rxx1x], 2]")?;
        srs1.push(regstr2);

        let regstr3 = SelectRegions::from("SR[RC[r11x1], -1]")?;
        srs1.push(regstr3);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 8);

        let selx = SelectRegions::from("SR[RC[r1111], 2]")?;
        let sely = SelectRegions::from("SR[RC[r1111], -1]")?;
        if let Some(selz) = sely.intersection(&selx) {
            if !frags.contains(&selz) {
                return Err(format!("fragments {frags} does not contain {selz}"));
            }
        }

        let selx = SelectRegions::from("SR[RC[r1101], 1]")?;
        let sely = SelectRegions::from("SR[RC[r1101], -1]")?;
        if let Some(selz) = sely.intersection(&selx) {
            if !frags.contains(&selz) {
                return Err(format!("fragments {frags} does not contain {selz}"));
            }
        }

        let selz = SelectRegions::from("SR[RC[rxx00], 1]")?;
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::from("SR[RC[rx00x], 1]")?;
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::from("SR[RC[r0x0x], 1]")?;
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::from("SR[RC[rxx10], 2]")?;
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::from("SR[RC[rx01x], 2]")?;
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::from("SR[RC[r0x1x], 2]")?;
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        // Check for non-subset intersections.
        for selx in frags.iter() {
            for sely in srs1.iter() {
                if selx.intersects(sely) && !selx.is_subset_of(sely) {
                    return Err(format!("{selx} intersects {sely} ?"));
                }
            }
        }

        // Check fragments cover whole area.
        let dif = srs1.subtract(&frags);
        println!("dif {}", dif);
        assert!(dif.is_empty());

        // Check fragment values.
        for selx in frags.iter() {
            let mut pos_val = 0;
            let mut neg_val = 0;
            // Gather values from start SelectRegionsStore.
            for sely in srs1.iter() {
                if selx.is_subset_of(sely) {
                    pos_val += sely.pos_value;
                    neg_val += sely.neg_value;
                }
            }
            assert!(pos_val == selx.pos_value);
            assert!(neg_val == selx.neg_value);
            assert!(pos_val + neg_val == selx.net_value);
        }

        // Check no fragments are supersets (or equal) any other.
        // Check each possible pair.
        for inx in 0..(frags.len() - 1) {
            for iny in (inx + 1)..frags.len() {
                if frags[inx].is_superset_of(&frags[iny]) {
                    return Err(format!("{} is a superset of {}", frags[inx], frags[iny]));
                }
                if frags[iny].is_superset_of(&frags[inx]) {
                    return Err(format!("{} is a superset of {}", frags[iny], frags[inx]));
                }
            }
        }

        Ok(())
    }
}
