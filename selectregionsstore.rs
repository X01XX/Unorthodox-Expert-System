//! Implement a struct of SelectRegionStore.

use crate::regionstorecorr::RegionStoreCorr;
use crate::selectregions::SelectRegions;
use crate::statestorecorr::StateStoreCorr;
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
    pub fn new(items: Vec<SelectRegions>) -> Self {
        Self { items }
    }

    /// Return a new SelectRegions instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SelectRegions>::with_capacity(num),
        }
    }

    /// Add a SelectRegionsStore.
    pub fn push(&mut self, select: SelectRegions) {
        //print!("{} push {}", self, select);
        debug_assert!(self.is_empty() || select.len() == self.items[0].len());

        if !self.contains(&select) {
            self.items.push(select);
        }
    }

    /// Add a SelectRegionsStore, deleting subsets.
    pub fn push_nosubs(&mut self, select: SelectRegions) {
        debug_assert!(self.is_empty() || select.len() == self.items[0].len());

        // Don't add a subset.
        if self.any_supersets_of(&select) {
            return;
        }
        // Identify subsets.
        let mut del = Vec::<usize>::new();
        for (inx, regstx) in self.items.iter().enumerate() {
            if regstx.regions.is_subset_of(&select.regions) {
                del.push(inx);
            }
        }
        // Remove subsets, highest indicies first.
        for inx in del.iter().rev() {
            tools::remove_unordered(&mut self.items, *inx);
        }
        // Add new select instance.
        self.items.push(select);
    }

    /// Add a SelectRegionsStore, deleting supersets.
    pub fn push_nosups(&mut self, select: SelectRegions) {
        debug_assert!(self.is_empty() || select.len() == self.items[0].len());

        //print!("{} push {}", self, select);
        // Don't add a superset.
        if self.any_subsets_of(&select) {
            return;
        }
        // Identify supersets.
        let mut del = Vec::<usize>::new();
        for (inx, regstx) in self.items.iter().enumerate() {
            if regstx.regions.is_superset_of(&select.regions) {
                del.push(inx);
            }
        }
        // Remove subsets, highest indicies first.
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

    /// Return a Vector of SelectRegions not supersets of a given RegionStoreCorr.
    pub fn _not_supersets_of(&self, regs: &RegionStoreCorr) -> Vec<&SelectRegions> {
        debug_assert!(self.is_empty() || regs.len() == self.items[0].len());

        self.items
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_of(regs))
            .collect()
    }

    /// Return true if any SelectRegions is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &StateStoreCorr) -> bool {
        debug_assert!(self.is_empty() || stas.len() == self.items[0].len());

        for regsx in &self.items {
            if regsx.regions.is_superset_states(stas) {
                return true;
            }
        }
        false
    }

    /// Return the sum of values of SelectRegions that are superset of a given RegionStoreCorr.
    pub fn rate_regions(&self, regs: &RegionStoreCorr) -> isize {
        debug_assert!(self.is_empty() || regs.len() == self.items[0].len());

        let mut net_value: isize = 0;

        for regsx in self.items.iter() {
            if regsx.regions.is_superset_of(regs) {
                net_value += regsx.net_value;
            }
        }
        net_value
    }

    /// Return true if any SelectRegion is a region superset of another..
    fn any_supersets_of(&self, slrx: &SelectRegions) -> bool {
        debug_assert!(self.is_empty() || slrx.len() == self.items[0].len());

        for regsx in &self.items {
            if regsx.regions.is_superset_of(&slrx.regions) {
                return true;
            }
        }
        false
    }

    /// Return true if any SelectRegion is a region subset of another.
    fn any_subsets_of(&self, slrx: &SelectRegions) -> bool {
        debug_assert!(self.is_empty() || slrx.len() == self.items[0].len());

        for regsx in &self.items {
            if regsx.regions.is_subset_of(&slrx.regions) {
                return true;
            }
        }
        false
    }

    /// Return the intersection of a SelectRegionStore and a Selectregions instance.
    pub fn _intersection_item(&self, slrx: &SelectRegions) -> Option<Self> {
        debug_assert!(self.is_empty() || slrx.len() == self.items[0].len());

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
        debug_assert!(self.is_empty());

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
        debug_assert!(self.is_empty() || slrx.len() == self.items[0].len());

        for sely in self.items.iter() {
            if sely.intersects(slrx) {
                return true;
            }
        }
        false
    }

    /// Return list of select regions that are superset of a State vector.
    pub fn supersets_of_states(&self, stas: &StateStoreCorr) -> Vec<&SelectRegions> {
        debug_assert!(self.is_empty() || stas.len() == self.items[0].len());

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

    /// Return true if an equal RegionStoreCorr is already in the SelectRegionsStore.
    pub fn contains(&self, slrx: &SelectRegions) -> bool {
        debug_assert!(self.is_empty() || slrx.len() == self.items[0].len());

        for regstrx in &self.items {
            if regstrx == slrx {
                return true;
            }
        }
        false
    }

    /// Append from another store.
    pub fn append(&mut self, mut other: Self) {
        debug_assert!(
            self.is_empty() || other.is_empty() || other.items[0].len() == self.items[0].len()
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
        debug_assert!(self.is_empty() || subtrahend.len() == self.items[0].len());

        let mut ret_str = Self::new(vec![]);

        for regy in self.iter() {
            if subtrahend.intersects(regy) {
                if subtrahend.regions == regy.regions {
                    continue;
                }
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
            self.is_empty() || other.is_empty() || other.items[0].len() == self.items[0].len()
        );

        let mut ret = self.clone();
        for selx in other.items.iter() {
            if ret.any_intersection_of(selx) {
                let tmp = ret.subtract_selectregions(selx);
                if tmp.is_empty() {
                    return Self::new(vec![]);
                }
                ret = tmp;
            }
        }
        ret
    }

    /// Return self fragmented by intersections.
    pub fn split_by_intersections(&self) -> Self {
        if self.len() < 2 {
            return self.clone();
        }

        let mut final_remainders = Self::new(vec![]);
        let mut cur_remainders = self.clone();

        loop {
            // Find the remainders of each SelectRegions minus others.
            let mut cycle_remainders = Self::new(vec![]);
            let mut cycle_ints = Self::new(vec![]);

            for (inx, selx) in cur_remainders.iter().enumerate() {
                // Init selx remainders store.
                let mut remx = Self::new(vec![selx.clone()]);
                //println!("subtracting ints from {selx}");

                // Subtract anything that intersects selx.
                for (iny, sely) in cur_remainders.iter().enumerate() {
                    if iny != inx && remx.any_intersection_of(sely) {
                        //print!("    {remx} - {sely} = ");
                        remx = remx.subtract_selectregions(sely);
                        //print!("remx after subtraction {remx}");
                        //println!(" ");
                    }
                }
                //println!("Remainders of {selx} are {remx}");

                // Add to cycle_remainders.
                if !remx.is_empty() {
                    // Aggregate intersections is selx subtract remainders.
                    let ints = Self::new(vec![selx.clone()]).subtract(&remx);
                    //println!("{} minus {} = {}", selx.clone(), remx, ints);

                    for selz in remx.into_iter() {
                        cycle_remainders.push_nosubs(selz);
                    }

                    // Add to cycle_ints.

                    for selz in ints.into_iter() {
                        cycle_ints.push_nosubs(selz);
                    }
                }
            } // next inx, selx

            //println!("cycle remainders: {}", cycle_remainders);
            //println!("cycle_ints: {}", cycle_ints);
            //assert!(1 == 2);

            for selz in cycle_remainders.into_iter() {
                final_remainders.push_nosubs(selz);
            }
            //println!("final remainders: {}", final_remainders);
            if cycle_ints.is_empty() {
                break;
            }

            // Set up next cycle.
            cur_remainders = cycle_ints;
            //println!("Next remainders = {cur_remainders}");
        } // end loop

        //println!("final remainders: {}", final_remainders);
        for selx in final_remainders.iter_mut() {
            let mut pos_value = 0;
            let mut neg_value = 0;
            for sely in self.iter() {
                if sely.is_superset_of(selx) {
                    pos_value += sely.pos_value;
                    neg_value += sely.neg_value;
                }
            }
            selx.set_values(pos_value, neg_value);
        }
        final_remainders
    }

    /// Return the sum of values of negative SelectRegions that are superset of a given RegionStoreCorr.
    pub fn rate_by_negative_regions(&self, regs: &RegionStoreCorr) -> isize {
        debug_assert!(self.is_empty() || regs.len() == self.items[0].len());

        let mut value: isize = 0;
        for regsx in self.items.iter() {
            if regsx.neg_value < 0 && regsx.regions.is_superset_of(regs) {
                value += regsx.neg_value;
            }
        }
        value
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
    use crate::region::SomeRegion;

    #[test]
    fn split_by_intersections1() -> Result<(), String> {
        // Try with empty RegionStore.
        let srs1 = SelectRegionsStore::new(vec![]);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} is {frags}");
        assert!(frags == srs1);

        // Try with no intersections
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let reg1a = SomeRegion::new_from_string("r0xx1").expect("SNH");
        let reg1b = SomeRegion::new_from_string("r1x1x").expect("SNH");
        let regstr1 = SelectRegions::new(RegionStoreCorr::new(vec![reg1a, reg1b]), -1);
        srs1.push(regstr1);

        let reg2a = SomeRegion::new_from_string("r1x0x").expect("SNH");
        let reg2b = SomeRegion::new_from_string("rx1x1").expect("SNH");
        let regstr2 = SelectRegions::new(RegionStoreCorr::new(vec![reg2a, reg2b]), 2);
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
        let reg1a = SomeRegion::new_from_string("r0x0x").expect("SNH");
        let regstr1 =
            SelectRegions::new(RegionStoreCorr::new(vec![reg1a.clone(), reg1a.clone()]), 1);
        srs1.push(regstr1);

        let reg2a = SomeRegion::new_from_string("rx1x1").expect("SNH");
        let regstr2 =
            SelectRegions::new(RegionStoreCorr::new(vec![reg2a.clone(), reg2a.clone()]), 2);
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
    fn split_by_intersections3() -> Result<(), String> {
        // Try two levels of intersection.
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let reg1a = SomeRegion::new_from_string("r0x0x").expect("SNH");
        let regstr1 = SelectRegions::new(RegionStoreCorr::new(vec![reg1a.clone()]), 1);
        srs1.push(regstr1);

        let reg2a = SomeRegion::new_from_string("rx1x1").expect("SNH");
        let regstr2 = SelectRegions::new(RegionStoreCorr::new(vec![reg2a.clone()]), 2);
        srs1.push(regstr2);

        let reg3a = SomeRegion::new_from_string("rx10x").expect("SNH");
        let regstr3 = SelectRegions::new(RegionStoreCorr::new(vec![reg3a.clone()]), 3);
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
        let regstr1 = SelectRegions::new(
            RegionStoreCorr::new(vec![
                SomeRegion::new_from_string("r0xx1")?,
                SomeRegion::new_from_string("r1x1x")?,
            ]),
            -1,
        );
        srs1.push(regstr1);

        let regstr2 = SelectRegions::new(
            RegionStoreCorr::new(vec![
                SomeRegion::new_from_string("rxx0x")?,
                SomeRegion::new_from_string("rx1x1")?,
            ]),
            2,
        );
        srs1.push(regstr2);

        let regstr3 = SelectRegions::new(
            RegionStoreCorr::new(vec![
                SomeRegion::new_from_string("rxxxx")?,
                SomeRegion::new_from_string("rxxxx")?,
            ]),
            0,
        );
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
        let reg1a = SomeRegion::new_from_string("r0xxx").expect("SNH");
        let regstr1 = SelectRegions::new(RegionStoreCorr::new(vec![reg1a.clone()]), 4);
        srs1.push(regstr1);

        let reg2a = SomeRegion::new_from_string("rxx1x").expect("SNH");
        let regstr2 = SelectRegions::new(RegionStoreCorr::new(vec![reg2a.clone()]), 3);
        srs1.push(regstr2);

        let reg3a = SomeRegion::new_from_string("rx1x1").expect("SNH");
        let regstr3 = SelectRegions::new(RegionStoreCorr::new(vec![reg3a.clone()]), 2);
        srs1.push(regstr3);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 10);

        // Check 3-level intersection.
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0111").expect("SNH")]),
            9,
        );
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check 2-level intersection (1 of 4).
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0101").expect("SNH")]),
            6,
        );
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check 2-level intersection (2 of 4).
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r1111").expect("SNH")]),
            5,
        );
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check 2-level intersection (3 of 4).
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r001x").expect("SNH")]),
            7,
        );
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check 2-level intersection (4 of 4).
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0x10").expect("SNH")]),
            7,
        );
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (1 of 5).
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r1101").expect("SNH")]),
            2,
        );
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (2 of 5).
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r000x").expect("SNH")]),
            4,
        );
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (3 of 5).
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0x00").expect("SNH")]),
            4,
        );
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (4 of 5).
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r101x").expect("SNH")]),
            3,
        );
        if !frags.contains(&selx) {
            return Err(format!("fragments {frags} does not contain {selx}"));
        }

        // Check no intersection (5 of 5).
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r1x10").expect("SNH")]),
            3,
        );
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
        let regstr1 = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("rxx0x").expect("SNH")]),
            1,
        );
        srs1.push(regstr1.clone());

        let mut srs2 = SelectRegionsStore::new(vec![]);
        let regstr2 = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("rx1x1").expect("SNH")]),
            2,
        );
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
            let srsexp = SelectRegions::new(
                RegionStoreCorr::new(vec![SomeRegion::new_from_string("rxx00").expect("SNH")]),
                1,
            );
            if srs3.contains(&srsexp) {
            } else {
                return Err(format!(
                    "{srs1} - {regstr2} = {srs3} does not contain {srsexp}?"
                ));
            }

            let srsexp = SelectRegions::new(
                RegionStoreCorr::new(vec![SomeRegion::new_from_string("rx00x").expect("SNH")]),
                1,
            );
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
            let srsexp = SelectRegions::new(
                RegionStoreCorr::new(vec![SomeRegion::new_from_string("rx111").expect("SNH")]),
                2,
            );
            if srs3.contains(&srsexp) {
            } else {
                return Err(format!(
                    "{srs2} - {regstr1} = {srs3} does not contain {srsexp}?"
                ));
            }
        }

        // Try subtract non-intersecting selectregions.
        let mut srs3 = SelectRegionsStore::new(vec![]);
        let regstr3 = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("rx101").expect("SNH")]),
            0,
        );
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
        let regstr1 = SelectRegions::new(
            RegionStoreCorr::new(vec![
                SomeRegion::new_from_string("rxx0x").expect("SNH"),
                SomeRegion::new_from_string("r01xx").expect("SNH"),
            ]),
            1,
        );
        srs1.push(regstr1.clone());

        let mut srs2 = SelectRegionsStore::new(vec![]);
        let regstr2 = SelectRegions::new(
            RegionStoreCorr::new(vec![
                SomeRegion::new_from_string("rx1x1").expect("SNH"),
                SomeRegion::new_from_string("rxx11").expect("SNH"),
            ]),
            2,
        );
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

            let sr_tmp1 = SelectRegions::new(
                RegionStoreCorr::new(vec![
                    SomeRegion::new_from_string("rxx00").expect("SNH"),
                    SomeRegion::new_from_string("r01xx").expect("SNH"),
                ]),
                1,
            );

            if srs3.contains(&sr_tmp1) {
            } else {
                return Err(format!(
                    "1 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp1}?"
                ));
            }

            let sr_tmp2 = SelectRegions::new(
                RegionStoreCorr::new(vec![
                    SomeRegion::new_from_string("rx00x").expect("SNH"),
                    SomeRegion::new_from_string("r01xx").expect("SNH"),
                ]),
                1,
            );

            if srs3.contains(&sr_tmp2) {
            } else {
                return Err(format!(
                    "2 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp2}?"
                ));
            }

            let sr_tmp3 = SelectRegions::new(
                RegionStoreCorr::new(vec![
                    SomeRegion::new_from_string("rxx0x").expect("SNH"),
                    SomeRegion::new_from_string("r01x0").expect("SNH"),
                ]),
                1,
            );

            if srs3.contains(&sr_tmp3) {
            } else {
                return Err(format!(
                    "3 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp3}?"
                ));
            }

            let sr_tmp4 = SelectRegions::new(
                RegionStoreCorr::new(vec![
                    SomeRegion::new_from_string("rxx0x").expect("SNH"),
                    SomeRegion::new_from_string("r010x").expect("SNH"),
                ]),
                1,
            );

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
        let regstr1 = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0x0x").expect("SNH")]),
            1,
        );
        srs1.push(regstr1);

        let regstr2 = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0x1x").expect("SNH")]),
            2,
        );
        srs1.push(regstr2);

        let regstr3 = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r01x1").expect("SNH")]),
            -1,
        );
        srs1.push(regstr3);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 6);

        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0101").expect("SNH")]),
            1,
        );
        let sely = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0101").expect("SNH")]),
            -1,
        );
        if let Some(selz) = sely.intersection(&selx) {
            if !frags.contains(&selz) {
                return Err(format!("fragments {frags} does not contain {selz}"));
            }
        }
        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0111").expect("SNH")]),
            2,
        );
        let sely = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0111").expect("SNH")]),
            -1,
        );
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
        let regstr1 = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("rxx0x").expect("SNH")]),
            1,
        );
        srs1.push(regstr1);

        let regstr2 = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("rxx1x").expect("SNH")]),
            2,
        );
        srs1.push(regstr2);

        let regstr3 = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r11x1").expect("SNH")]),
            -1,
        );
        srs1.push(regstr3);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 8);

        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r1111").expect("SNH")]),
            2,
        );
        let sely = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r1111").expect("SNH")]),
            -1,
        );
        if let Some(selz) = sely.intersection(&selx) {
            if !frags.contains(&selz) {
                return Err(format!("fragments {frags} does not contain {selz}"));
            }
        }

        let selx = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r1101").expect("SNH")]),
            1,
        );
        let sely = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r1101").expect("SNH")]),
            -1,
        );
        if let Some(selz) = sely.intersection(&selx) {
            if !frags.contains(&selz) {
                return Err(format!("fragments {frags} does not contain {selz}"));
            }
        }

        let selz = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("rxx00").expect("SNH")]),
            1,
        );
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("rx00x").expect("SNH")]),
            1,
        );
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0x0x").expect("SNH")]),
            1,
        );
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("rxx10").expect("SNH")]),
            2,
        );
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("rx01x").expect("SNH")]),
            2,
        );
        if !frags.contains(&selz) {
            return Err(format!("fragments {frags} does not contain {selz}"));
        }

        let selz = SelectRegions::new(
            RegionStoreCorr::new(vec![SomeRegion::new_from_string("r0x1x").expect("SNH")]),
            2,
        );
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
