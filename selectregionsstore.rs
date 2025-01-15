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
use std::str::FromStr;

impl fmt::Display for SelectRegionsStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

/// Implement the PartialEq trait, for a
/// A quick comparison of definitions.
impl PartialEq for SelectRegionsStore {
    fn eq(&self, other: &Self) -> bool {
        if self.items.len() != other.items.len() {
            return false;
        }
        self.is_not_empty() && self[0].is_congruent(&other[0])
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
        let mut ret_srs = Self { items: vec![] };

        // Avoid region-duplicates.
        for selx in items {
            ret_srs.push(selx);
        }

        ret_srs
    }

    /// Add a SelectRegionsStore.
    /// Duplicate RegionsCorr are skipped.
    pub fn push(&mut self, select: SelectRegions) {
        //print!("{} push {}", self, select);
        debug_assert!(self.is_empty() || self.items[0].regions.is_congruent(&select));

        if !self.contains_regionscorr(&select.regions) {
            self.items.push(select);
        }
    }

    /// Add a SelectRegionsStore, deleting subsets.
    pub fn push_nosubs(&mut self, select: SelectRegions) {
        debug_assert!(self.is_empty() || self.items[0].regions.is_congruent(&select));

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

    /// Return true if any SelectRegions is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &StatesCorr) -> bool {
        debug_assert!(self.is_empty() || self.items[0].regions.is_congruent(stas));

        for regsx in &self.items {
            if regsx.regions.is_superset_states(stas) {
                return true;
            }
        }
        false
    }

    /// Return the sum of values of SelectRegions that are superset of a given RegionsCorr.
    pub fn supersets_sum(&self, regs: &RegionsCorr) -> isize {
        debug_assert!(self.is_empty() || self.items[0].regions.is_congruent(regs));

        self.items
            .iter()
            .filter_map(|selx| {
                if selx.regions.is_superset_of(regs) {
                    Some(selx.value)
                } else {
                    None
                }
            })
            .sum()
    }

    /// Return true if a RegionsCorr instance is a subset af any negative SelectRegions.
    pub fn in_negative_regions(&self, regs: &RegionsCorr) -> bool {
        debug_assert!(self.is_empty() || self.items[0].regions.is_congruent(regs));

        for selx in self.items.iter() {
            if selx.value < 0 && selx.regions.is_superset_of(regs) {
                return true;
            }
        }
        false
    }

    /// Return true if any SelectRegion is a region superset of another..
    fn any_supersets_of(&self, slrx: &SelectRegions) -> bool {
        debug_assert!(self.is_empty() || self.items[0].is_congruent(slrx));

        for regsx in &self.items {
            if regsx.regions.is_superset_of(&slrx.regions) {
                return true;
            }
        }
        false
    }

    /// Return true if any item intersects a given SelectRegion.
    pub fn any_intersection_of(&self, slrx: &SelectRegions) -> bool {
        debug_assert!(self.is_empty() || self.items[0].is_congruent(slrx));

        for sely in self.items.iter() {
            if sely.intersects(slrx) {
                return true;
            }
        }
        false
    }

    /// Return list of select regions that are superset of a State vector.
    pub fn supersets_of_states(&self, stas: &StatesCorr) -> Vec<&SelectRegions> {
        debug_assert!(self.is_empty() || self.items[0].regions.is_congruent(stas));

        self.items
            .iter()
            .filter(|regsx| regsx.regions.is_superset_states(stas))
            .collect()
    }

    /// Return a string represeting an SelectRegionsStore.
    fn formatted_str(&self) -> String {
        let mut ret_str = String::from("[");

        let mut first = true;
        for orx in self.items.iter() {
            if first {
                first = false;
            } else {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&orx.to_string());
        }
        ret_str.push(']');
        ret_str
    }

    /// Return true if a SelectRegionsStore contains a given SelectRegions.
    pub fn contains(&self, slrx: &SelectRegions) -> bool {
        //println!("selectregionsstore::contains: store: {self} arg: {slrx}");
        debug_assert!(self.is_empty() || self.items[0].is_congruent(slrx));

        self.items.contains(slrx)
    }

    /// Return true if an equal RegionsCorr is already in the SelectRegionsStore.
    pub fn contains_regionscorr(&self, rcx: &RegionsCorr) -> bool {
        //println!("selectregionsstore::contains_regionscorr: store: {self} arg: {rcx}");
        debug_assert!(self.is_empty() || self.items[0].regions.is_congruent(rcx));

        for regstrx in &self.items {
            if regstrx.regions == *rcx {
                return true;
            }
        }
        false
    }

    /// Subtract a SelectRegions.
    pub fn subtract_selectregions(&self, subtrahend: &SelectRegions) -> Self {
        // println!("subtract {subtrahend} from {self}");
        debug_assert!(self.is_empty() || self.items[0].is_congruent(subtrahend));

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

    /// Subtract a SelectRegionStore from another.
    pub fn subtract(&self, other: &Self) -> Self {
        debug_assert!(self.is_empty() || other.is_empty() || self.items[0].is_congruent(&other[0]));

        let mut ret = self.clone();

        for selx in other.items.iter() {
            if ret.any_intersection_of(selx) {
                ret = ret.subtract_selectregions(selx);
            }
        }
        for selx in ret.iter_mut() {
            selx.set_value(self.rate_regionscorr(&selx.regions));
        }
        ret
    }

    /// Return the sum of SelectRegion values that a given SelectRegion is a subset of.
    pub fn rate_regionscorr(&self, rcx: &RegionsCorr) -> isize {
        let mut val = 0;
        for sely in self.items.iter() {
            if sely.regions.is_superset_of(rcx) {
                val += sely.value;
            }
        }
        val
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

        let mut sel_fragments = Vec::<SelectRegions>::with_capacity(fragments.len());
        for regsx in fragments {
            // Calc value for RegionsCorr.
            let value = self.rate_regionscorr(&regsx);

            if value != 0 {
                // Create a SelectRegions for the RegionsCorr.
                let selx = SelectRegions::new(regsx, value);

                // Save SelectRegions to vector.
                sel_fragments.push(selx);
            }
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

impl FromStr for SelectRegionsStore {
    type Err = String;
    /// Return a SelectRegionsStore instance, given a string representation.
    /// Like [], [SR[RC[r1010], 1]], or [SR[RC[r101, r100], -1], SR[RC[r111, r101], 0]].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("selectregionsstore::from_str: {str_in}");
        let str_in2 = str_in.trim();
        if str_in2.len() < 2 {
            return Err("selectregionstore::from_str: string should be at least = []".to_string());
        }

        if str_in2 == "[]" {
            return Ok(Self::new(vec![]));
        }

        if str_in2[0..1] != *"[" {
            return Err("selectregionsstore::from_str: string should begin with [".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("selectregionsstore::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[1..(str_in2.len() - 1)];
        println!("token_str: {token_str}");

        // Split string into SelectRegion tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("selectregionsstore::from_str: {errstr}")),
        };

        println!("tokens {:?}", tokens);

        // Tally up tokens.
        let mut sregions = Vec::<SelectRegions>::new();

        for tokenx in tokens.into_iter() {
            match SelectRegions::from_str(&tokenx) {
                Ok(regx) => sregions.push(regx),
                Err(errstr) => return Err(format!("selectregionsstore::from_str: {errstr}")),
            }
        }

        Ok(SelectRegionsStore::new(sregions))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    /// Check fragments of a SRS for various error conditions.
    fn check_fragments(
        srs1: &SelectRegionsStore,
        frags: &SelectRegionsStore,
    ) -> Result<(), String> {
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
        if dif.is_not_empty() {
            for difx in dif.iter() {
                if difx.value != 0 {
                    return Err(format!("diff is {dif} ?"));
                }
            }
        }

        // Check fragment values.
        for selx in frags.iter() {
            let val = srs1.rate_regionscorr(&selx.regions);
            if val != selx.value {
                return Err(format!(
                    "selx {selx} val {val} NE selx.value {} ?",
                    selx.value
                ));
            }
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
    fn test_the_checker() -> Result<(), String> {
        // Check for non-subset intersections.
        let srs1 = SelectRegionsStore::from_str("[SR[RC[r1X0X], -1], SR[RC[rX1X1], 2]]")?;
        let frags = SelectRegionsStore::from_str("[SR[RC[r1x01], -1]]")?;
        match check_fragments(&srs1, &frags) {
            Ok(()) => return Err("Failed check for non-subset intersections.".to_string()),
            Err(errstr) => {
                println!("{}", errstr);
                assert!(errstr == "SR[RC[r1x01], -1] intersects SR[RC[rX1X1], +2] ?")
            }
        }

        // Check fragments cover whole area.
        let srs1 = SelectRegionsStore::from_str("[SR[RC[r1X0X], -1], SR[RC[rX1X1], 2]]")?;
        let frags = SelectRegionsStore::from_str(
            "[SR[RC[r1101], 1], SR[RC[r1X00], -1], SR[RC[r100X], -1]]",
        )?;
        match check_fragments(&srs1, &frags) {
            Ok(()) => return Err("Failed Check fragments cover whole area..".to_string()),
            Err(errstr) => {
                println!("{}", errstr);
                assert!(errstr == "diff is [SR[RC[rX111], +2], SR[RC[r01X1], +2]] ?")
            }
        }

        // Check fragment values.
        let srs1 = SelectRegionsStore::from_str("[SR[RC[r1X0X], -1], SR[RC[rX1X1], 2]]")?;
        // Check bad value.
        let frags =
            SelectRegionsStore::from_str("[SR[RC[r1X00], -1], SR[RC[r100X], -1], SR[RC[r1101], -1], SR[RC[r01X1], 2], SR[RC[rX111], 2]]")?;
        match check_fragments(&srs1, &frags) {
            Ok(()) => return Err("Failed Check fragments values.".to_string()),
            Err(errstr) => {
                println!("{}", errstr);
                assert!(errstr == "selx SR[RC[r1101], -1] val 1 NE selx.value -1 ?")
            }
        }

        // Check no fragments are supersets (or equal) any other.
        let srs1 = SelectRegionsStore::from_str("[SR[RC[r1X0X], -1], SR[RC[rX1X1], 2]]")?;
        let mut frags = srs1.split_by_intersections();
        frags.push(SelectRegions::from_str("SR[RC[r0111], 2]")?);
        match check_fragments(&srs1, &frags) {
            Ok(()) => return Err("Failed Check fragments are supersets.".to_string()),
            Err(errstr) => {
                println!("{}", errstr);
                assert!(errstr == "SR[RC[rX111], +2] is a superset of SR[RC[r0111], +2]");
            }
        }
        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn split_by_intersections1() -> Result<(), String> {
        // Try with empty RegionStore.
        let srs1 = SelectRegionsStore::new(vec![]);

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} is {frags}");
        assert!(frags.is_empty());

        // Try with no intersections
        let srs1 =
            SelectRegionsStore::from_str("[SR[RC[r0xx1, r1x1x], -1], SR[RC[r1x0x, rx1x1], 2]]")?;

        let frags = srs1.split_by_intersections();
        println!("Fragments of:");
        for srsx in srs1.iter() {
            println!("    {srsx}");
        }
        println!("Are:");
        for srsx in frags.iter() {
            println!("    {srsx}");
        }

        if frags.len() == 2 {
            Ok(())
        } else {
            Err(format!("{frags} NE srs1 {srs1} ?"))
        }
    }

    #[test]
    fn split_by_intersections2() -> Result<(), String> {
        // Try one level of intersection.
        let srs1 =
            SelectRegionsStore::from_str("[SR[RC[r0x0x, r0x0x], 1], SR[RC[rx1x1, rx1x1], 2]]")?;

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 9);

        check_fragments(&srs1, &frags)
    }

    #[test]
    fn split_by_intersections3() -> Result<(), String> {
        // Try two levels of intersection.
        let srs1 =
            SelectRegionsStore::from_str("[SR[RC[r0x0x], 1], SR[RC[rx1x1], 2], SR[RC[rx10x], 3]]")?;

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 6);

        check_fragments(&srs1, &frags)
    }

    #[test]
    fn split_by_intersections4() -> Result<(), String> {
        // Try one level of intersection, two region select regions..
        let srs1 = SelectRegionsStore::from_str(
            "[SR[RC[r0xx1, r1x1x], -1], SR[RC[rxx0x, rx1x1], 2], SR[RC[rxxxx, rxxxx], 1]]",
        )?;

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 17);

        check_fragments(&srs1, &frags)
    }

    #[test]
    fn split_by_intersections5() -> Result<(), String> {
        // Try three levels of intersection.
        let srs1 =
            SelectRegionsStore::from_str("[SR[RC[r0xxx], 4], SR[RC[rxx1x], 3], SR[RC[rx1x1], 2]]")?;

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 10);

        check_fragments(&srs1, &frags)
    }

    #[test]
    fn subtract_selectregions1() -> Result<(), String> {
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::from_str("SR[RC[rxx0x], 1]")?;
        srs1.push(regstr1.clone());

        let mut srs2 = SelectRegionsStore::new(vec![]);
        let regstr2 = SelectRegions::from_str("SR[RC[rx1x1], 2]")?;
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
            let srsexp = SelectRegions::from_str("SR[RC[rxx00], 1]")?;
            if srs3.contains(&srsexp) {
            } else {
                return Err(format!(
                    "{srs1} - {regstr2} = {srs3} does not contain {srsexp}?"
                ));
            }

            let srsexp = SelectRegions::from_str("SR[RC[rx00x], 1]")?;
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
            let srsexp = SelectRegions::from_str("SR[RC[rx111], 2]")?;
            if srs3.contains(&srsexp) {
            } else {
                return Err(format!(
                    "{srs2} - {regstr1} = {srs3} does not contain {srsexp}?"
                ));
            }
        }

        // Try subtract non-intersecting selectregions.
        let mut srs3 = SelectRegionsStore::new(vec![]);
        let regstr3 = SelectRegions::from_str("SR[RC[rx101], 1]")?;
        srs3.push(regstr3.clone());
        let srs4 = srs3.subtract_selectregions(&regstr1);
        if srs4.is_empty() {
            println!("{} - {} = None", srs3, regstr1);
        } else {
            return Err(format!("{} - {} = {} ?", srs3, regstr1, srs4));
        }
        Ok(())
    }

    #[test]
    fn subtract_selectregions2() -> Result<(), String> {
        let mut srs1 = SelectRegionsStore::new(vec![]);
        let regstr1 = SelectRegions::from_str("SR[RC[rxx0x, r01xx], 1]")?;
        srs1.push(regstr1.clone());

        let mut srs2 = SelectRegionsStore::new(vec![]);
        let regstr2 = SelectRegions::from_str("SR[RC[rx1x1, rxx11], 2]")?;
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

            let sr_tmp1 = SelectRegions::from_str("SR[RC[rxx00, r01xx], 1]")?;

            if srs3.contains(&sr_tmp1) {
            } else {
                return Err(format!(
                    "1 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp1}?"
                ));
            }

            let sr_tmp2 = SelectRegions::from_str("SR[RC[rx00x, r01xx], 1]")?;

            if srs3.contains(&sr_tmp2) {
            } else {
                return Err(format!(
                    "2 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp2}?"
                ));
            }

            let sr_tmp3 = SelectRegions::from_str("SR[RC[rxx0x, r01x0], 1]")?;

            if srs3.contains(&sr_tmp3) {
            } else {
                return Err(format!(
                    "3 {srs1} - {regstr2} = {srs3} does not contain {sr_tmp3}?"
                ));
            }

            let sr_tmp4 = SelectRegions::from_str("SR[RC[rxx0x, r010x], 1]")?;

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
        let srs1 = SelectRegionsStore::from_str(
            "[SR[RC[r0x0x], 1], SR[RC[r0x1x], 2], SR[RC[r01x1], -1]]",
        )?;

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 5);

        check_fragments(&srs1, &frags)
    }

    #[test]
    fn split_by_intersections7() -> Result<(), String> {
        // Try one level of intersection.
        let srs1 = SelectRegionsStore::from_str(
            "[SR[RC[rxx0x], 1], SR[RC[rxx1x], 2], SR[RC[r11x1], -1]]",
        )?;

        let frags = srs1.split_by_intersections();
        println!("fragments of {srs1} are {frags} len {}", frags.len());
        assert!(frags.len() == 7);

        check_fragments(&srs1, &frags)
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let srs1 = SelectRegionsStore::from_str("[]")?;
        println!("srs1 {srs1}");
        assert!(format!("{srs1}") == "[]");

        let srs2 = SelectRegionsStore::from_str("[SR[RC[r1010], +1]]")?;
        println!("srs2 {srs2}");
        assert!(format!("{srs2}") == "[SR[RC[r1010], +1]]");

        let srs3 = SelectRegionsStore::from_str("[SR[RC[r101, r100], -1], SR[RC[r111, r101], 0]]")?;
        println!("srs3 {srs3}");
        assert!(format!("{srs3}") == "[SR[RC[r101, r100], -1], SR[RC[r111, r101], +0]]");

        //assert!(1 == 2);
        Ok(())
    }
}
