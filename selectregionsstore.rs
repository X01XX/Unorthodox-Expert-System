//! Implement a struct of SelectRegionStore.

use crate::plan::SomePlan;
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
        for regx in &self.regionstores {
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
    pub regionstores: Vec<SelectRegions>,
}

impl SelectRegionsStore {
    /// Return a new SelectRegionsStores instance.
    pub fn new(regionstores: Vec<SelectRegions>) -> Self {
        Self { regionstores }
    }

    /// Return a new SelectRegions instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            regionstores: Vec::<SelectRegions>::with_capacity(num),
        }
    }

    /// Add a SelectRegionsStore.
    pub fn push(&mut self, select: SelectRegions) {
        if !self.contains(&select) {
            self.regionstores.push(select);
        }
    }

    /// Add a SelectRegionsStore, deleting subsets.
    pub fn push_nosubs(&mut self, select: SelectRegions) {
        // Don't add a subset.
        if self.any_supersets_of(&select) {
            return;
        }
        // Identify subsets.
        let mut del = Vec::<usize>::new();
        for (inx, regstx) in self.regionstores.iter().enumerate() {
            if regstx.regions.is_subset_of(&select.regions) {
                del.push(inx);
            }
        }
        // Remove subsets, highest indicies first.
        for inx in del.iter().rev() {
            tools::remove_unordered(&mut self.regionstores, *inx);
        }
        // Add new select instance.
        self.regionstores.push(select);
    }

    /// Add a SelectRegionsStore, deleting supersets.
    pub fn push_nosups(&mut self, select: SelectRegions) {
        // Don't add a superset.
        if self.any_subsets_of(&select) {
            return;
        }
        // Identify supersets.
        let mut del = Vec::<usize>::new();
        for (inx, regstx) in self.regionstores.iter().enumerate() {
            if regstx.regions.is_superset_of(&select.regions) {
                del.push(inx);
            }
        }
        // Remove subsets, highest indicies first.
        for inx in del.iter().rev() {
            tools::remove_unordered(&mut self.regionstores, *inx);
        }
        // Add new select instance.
        self.regionstores.push(select);
    }

    /// Return the length of an instance.
    pub fn len(&self) -> usize {
        self.regionstores.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.regionstores.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.regionstores.is_empty()
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<SelectRegions> {
        self.regionstores.iter()
    }

    /// Return an mut iterator
    pub fn iter_mut(&mut self) -> IterMut<SelectRegions> {
        self.regionstores.iter_mut()
    }

    /// Return the sum of values and times visited of Select Regions thaot are superset of a given RegionStoreCorr.
    pub fn rate_regions(&self, regs: &RegionStoreCorr) -> (isize, usize) {
        let mut times_visited: usize = 0;
        let mut value: isize = 0;
        for regsx in self.regionstores.iter() {
            if regsx.regions.is_superset_of(regs) {
                value += regsx.value();
                times_visited += regsx.times_visited;
            }
        }
        (value, times_visited)
    }

    /// Return a Vector of SelectRegions not supersets of a given StateStore.
    pub fn not_supersets_of_states(&self, stas: &StateStoreCorr) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_states(stas))
            .collect()
    }

    /// Return true if any SelectRegion is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &StateStoreCorr) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_states(stas) {
                return true;
            }
        }
        false
    }

    /// Return the aggregate value of negative select regions the current states are in.
    pub fn value_supersets_of_states(&self, stas: &StateStoreCorr) -> isize {
        let mut val: isize = 0;
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_states(stas) && regsx.value() < 0 {
                val += regsx.value();
            }
        }
        val
    }

    /// Return true if any SelectRegion is a region superset of another..
    pub fn any_supersets_of(&self, other: &SelectRegions) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_of(&other.regions) {
                return true;
            }
        }
        false
    }

    /// Return true if any SelectRegion is a region subset of another.
    pub fn any_subsets_of(&self, other: &SelectRegions) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_subset_of(&other.regions) {
                return true;
            }
        }
        false
    }

    /// Return true if any SelectRegion has regions equal to another.
    pub fn any_equal_regions(&self, other: &SelectRegions) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions == other.regions {
                return true;
            }
        }
        false
    }

    /// Return the intersection of a SelectRegionStore and a Selectregions instance.
    pub fn intersection(&self, slrx: &SelectRegions) -> Option<Self> {
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

    /// Return true if any item intersects a given SelectRegion.
    pub fn any_intersection_of(&self, selx: &SelectRegions) -> bool {
        for sely in self.regionstores.iter() {
            if sely.intersects(selx) {
                return true;
            }
        }
        false
    }

    /// Return list of select regions that are superset of a State vector.
    pub fn supersets_of_states(&self, stas: &StateStoreCorr) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| regsx.regions.is_superset_states(stas))
            .collect()
    }

    /// Return a string represeting an SelectRegionsStore.
    fn formatted_string(&self) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in self.regionstores.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&orx.to_string());
        }
        ret_str.push(']');
        ret_str
    }

    /// Return true if an equal RegionStoreCorr is already in the SelectRegionsStore.
    fn contains(&self, selx: &SelectRegions) -> bool {
        for regstrx in &self.regionstores {
            if regstrx.regions == selx.regions {
                return true;
            }
        }
        false
    }

    /// Return the sum of all negative select regions values a plan goes through.
    /// This ignores the select regions a plan starts, or end, in.
    pub fn rate_plan(&self, aplan: &SomePlan, current_states: &StateStoreCorr) -> isize {
        // Create a mutable state ref vector.
        let mut all_states = current_states.clone();

        let dom_id = aplan.dom_id;

        // Store rate for each step.
        let mut rates = Vec::<isize>::with_capacity(aplan.len());

        for stepx in aplan.iter() {
            let valx = self.value_supersets_of_states(&all_states);
            // Print violations.
            //for selx in self.regionstores.iter() {
            //    if selx.regions.is_superset_states(&all_states) {
            //        println!("step {} of {} violates {} at {}", stepx, aplan, selx, SomeState::vec_ref_string(&all_states));
            //    }
            //}
            rates.push(valx);
            all_states[dom_id] = stepx.initial.state1().clone();
        }
        rates.push(self.value_supersets_of_states(&all_states));

        if rates.is_empty() {
            return 0;
        }
        rates.iter().sum()
    }

    /// Append from another store.
    pub fn append(&mut self, mut val: Self) {
        self.regionstores.append(&mut val.regionstores);
    }

    /// Pop the last item.
    pub fn pop(&mut self) -> Option<SelectRegions> {
        self.regionstores.pop()
    }

    /// Subtract a SelectRegions.
    pub fn subtract_selectregions(&self, subtrahend: &SelectRegions) -> Self {
        // println!("subtract {subtrahend} from {self}");

        let mut ret_str = Self::new(vec![]);

        for regy in self.iter() {
            if subtrahend.intersects(regy) {
                if subtrahend.regions == regy.regions {
                    continue;
                }
                for regz in regy.regions.subtract(&subtrahend.regions) {
                    ret_str.push_nosubs(SelectRegions::new(regz, regy.pos, regy.neg));
                }
            } else {
                ret_str.push_nosubs(regy.clone());
            }
        } // next regy

        ret_str
    }

    /// Subtract a selectregionstore from another.
    pub fn subtract(&self, other: &Self) -> Self {
        let mut ret = self.clone();
        for selx in other.regionstores.iter() {
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

        // Get first level ints.
        let mut tmp_ints = Self::new(vec![]);

        // Check each possible pair.
        for inx in 0..(self.len() - 1) {
            for iny in (inx + 1)..self.len() {
                assert!(self[inx] != self[iny]); // Check for dups.

                if let Some(regx) = self[inx].intersection(&self[iny]) {
                    tmp_ints.push_nosubs(regx);
                }
            }
        }

        if tmp_ints.is_empty() {
            return self.clone();
        }
        let mut remainder = self.subtract(&tmp_ints);

        loop {
            // Get first level ints.
            let mut next_ints = Self::new(vec![]);

            // Check each possible pair.
            for inx in 0..(tmp_ints.len() - 1) {
                for iny in (inx + 1)..tmp_ints.len() {
                    if let Some(regx) = tmp_ints[inx].intersection(&tmp_ints[iny]) {
                        next_ints.push_nosups(regx);
                    }
                } // next iny
            } // next inx

            if next_ints.is_empty() {
                remainder.append(tmp_ints);
                // Set values.
                for selx in remainder.iter_mut() {
                    selx.set_pos(0);
                    selx.set_neg(0);
                    for origx in self.iter() {
                        if selx.is_subset_of(origx) {
                            selx.set_pos(selx.pos + origx.pos);
                            selx.set_neg(selx.neg + origx.neg);
                        }
                    }
                }
                return remainder;
            }

            let remain2 = tmp_ints.subtract(&next_ints);
            remainder.append(remain2);
            tmp_ints = next_ints;
        } // end loop
    }
} // End impl SelectRegionsStore

impl Index<usize> for SelectRegionsStore {
    type Output = SelectRegions;
    fn index(&self, i: usize) -> &SelectRegions {
        &self.regionstores[i]
    }
}

impl IntoIterator for SelectRegionsStore {
    type Item = SelectRegions;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.regionstores.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::region::SomeRegion;
    use crate::state::SomeState;

    #[test]
    fn split_by_intersections() -> Result<(), String> {
        let ur_reg = SomeRegion::new(vec![SomeState::new(SomeBits::new(4))]);

        // Try with empty RegionStore.
        let srs1 = SelectRegionsStore::new(vec![]);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} is {frags}");
        assert!(frags == srs1);

        // Try with no intersections
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let reg1a = ur_reg.new_from_string("r0xx1").expect("SNH");
        let reg1b = ur_reg.new_from_string("r1x1x").expect("SNH");
        let regstr1 = SelectRegions::new(RegionStoreCorr::new(vec![reg1a, reg1b]), 0, 0);
        srs1.push(regstr1);

        let reg2a = ur_reg.new_from_string("r1x0x").expect("SNH");
        let reg2b = ur_reg.new_from_string("r0x0x").expect("SNH");
        let regstr2 = SelectRegions::new(RegionStoreCorr::new(vec![reg2a, reg2b]), 0, 0);
        srs1.push(regstr2);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} is {frags}");
        assert!(frags == srs1);

        // Try one level of intersection.
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let reg1a = ur_reg.new_from_string("r0x0x").expect("SNH");
        let regstr1 = SelectRegions::new(
            RegionStoreCorr::new(vec![reg1a.clone(), reg1a.clone()]),
            0,
            0,
        );
        srs1.push(regstr1);

        let reg2a = ur_reg.new_from_string("rx1x1").expect("SNH");
        let regstr2 = SelectRegions::new(
            RegionStoreCorr::new(vec![reg2a.clone(), reg2a.clone()]),
            0,
            0,
        );
        srs1.push(regstr2);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags}");
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

        // Try two levels of intersection.
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let reg1a = ur_reg.new_from_string("r0x0x").expect("SNH");
        let regstr1 = SelectRegions::new(RegionStoreCorr::new(vec![reg1a.clone()]), 0, 0);
        srs1.push(regstr1);

        let reg2a = ur_reg.new_from_string("rx1x1").expect("SNH");
        let regstr2 = SelectRegions::new(RegionStoreCorr::new(vec![reg2a.clone()]), 0, 0);
        srs1.push(regstr2);

        let reg3a = ur_reg.new_from_string("rx10x").expect("SNH");
        let regstr3 = SelectRegions::new(RegionStoreCorr::new(vec![reg3a.clone()]), 0, 0);
        srs1.push(regstr3);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags}");
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

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn test_subtract_selectregions1() -> Result<(), String> {
        let ur_reg1 = SomeRegion::new(vec![SomeState::new(SomeBits::new(4))]);

        let mut srs1 = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::new(
            RegionStoreCorr::new(vec![ur_reg1.new_from_string("rxx0x").expect("SNH")]),
            0,
            0,
        );
        srs1.push(regstr1.clone());

        let mut srs2 = SelectRegionsStore::new(vec![]);
        let regstr2 = SelectRegions::new(
            RegionStoreCorr::new(vec![ur_reg1.new_from_string("rx1x1").expect("SNH")]),
            0,
            0,
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
            if srs3.contains(&SelectRegions::new(
                RegionStoreCorr::new(vec![ur_reg1.new_from_string("rxx00").expect("SNH")]),
                0,
                0,
            )) {
            } else {
                return Err(format!("{} - {} = {} ?", srs1, regstr2, srs3));
            }
            if srs3.contains(&SelectRegions::new(
                RegionStoreCorr::new(vec![ur_reg1.new_from_string("rx00x").expect("SNH")]),
                0,
                0,
            )) {
            } else {
                return Err(format!("{} - {} = {} ?", srs1, regstr2, srs3));
            }
        }

        // Try the reverse subtract_selectregions.
        let srs3 = srs2.subtract_selectregions(&regstr1);
        if srs3.is_empty() {
            return Err(format!("{} - {} = None ?", srs2, regstr1));
        } else {
            println!("{} - {} = {}", srs2, regstr1, srs3);
            if srs3.len() == 1 {
            } else {
                return Err(format!("{} - {} = {} ?", srs2, regstr1, srs3));
            }
            if srs3.contains(&SelectRegions::new(
                RegionStoreCorr::new(vec![ur_reg1.new_from_string("rx111").expect("SNH")]),
                0,
                0,
            )) {
            } else {
                return Err(format!("{} - {} = {} ?", srs2, regstr1, srs3));
            }
        }

        // Try subtract_selectregions superset from subset.
        let mut srs3 = SelectRegionsStore::new(vec![]);
        let regstr3 = SelectRegions::new(
            RegionStoreCorr::new(vec![ur_reg1.new_from_string("rx101").expect("SNH")]),
            0,
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
    fn test_subtract_selectregions2() -> Result<(), String> {
        let ur_reg1 = SomeRegion::new(vec![SomeState::new(SomeBits::new(4))]);

        let mut srs1 = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::new(
            RegionStoreCorr::new(vec![
                ur_reg1.new_from_string("rxx0x").expect("SNH"),
                ur_reg1.new_from_string("r01xx").expect("SNH"),
            ]),
            0,
            0,
        );
        srs1.push(regstr1.clone());

        let mut srs2 = SelectRegionsStore::new(vec![]);
        let regstr2 = SelectRegions::new(
            RegionStoreCorr::new(vec![
                ur_reg1.new_from_string("rx1x1").expect("SNH"),
                ur_reg1.new_from_string("rxx11").expect("SNH"),
            ]),
            0,
            0,
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
                    ur_reg1.new_from_string("rxx00").expect("SNH"),
                    ur_reg1.new_from_string("r01xx").expect("SNH"),
                ]),
                0,
                0,
            );

            if srs3.contains(&sr_tmp1) {
            } else {
                return Err(format!("{} - {} = {} ?", srs1, regstr2, srs3));
            }

            let sr_tmp2 = SelectRegions::new(
                RegionStoreCorr::new(vec![
                    ur_reg1.new_from_string("rx00x").expect("SNH"),
                    ur_reg1.new_from_string("r01xx").expect("SNH"),
                ]),
                0,
                0,
            );

            if srs3.contains(&sr_tmp2) {
            } else {
                return Err(format!("{} - {} = {} ?", srs1, regstr2, srs3));
            }

            let sr_tmp3 = SelectRegions::new(
                RegionStoreCorr::new(vec![
                    ur_reg1.new_from_string("rxx0x").expect("SNH"),
                    ur_reg1.new_from_string("r01x0").expect("SNH"),
                ]),
                0,
                0,
            );

            if srs3.contains(&sr_tmp3) {
            } else {
                return Err(format!("{} - {} = {} ?", srs1, regstr2, srs3));
            }

            let sr_tmp4 = SelectRegions::new(
                RegionStoreCorr::new(vec![
                    ur_reg1.new_from_string("rxx0x").expect("SNH"),
                    ur_reg1.new_from_string("r010x").expect("SNH"),
                ]),
                0,
                0,
            );

            if srs3.contains(&sr_tmp4) {
            } else {
                return Err(format!("{} - {} = {} ?", srs1, regstr2, srs3));
            }
        }

        //assert!(1 == 2);
        Ok(())
    }
}
