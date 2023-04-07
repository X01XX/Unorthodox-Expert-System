//! Implement a struct of Select RegionStores.

use crate::regionstore::RegionStore;

use crate::state::SomeState;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for SelectRegions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = self.regions.formatted_string();
        str.push_str(&format!("/{}", self.value));
        write!(f, "{}", str)
    }
}

//#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct SelectRegions {
    /// Regions, in domain order, describing the requirements for an select state.
    pub regions: RegionStore,
    /// A value for being in the select state.
    pub value: isize,
}

impl fmt::Display for SelectRegionsStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SelectRegions {
    pub fn vec_ref_string(avec: &[&SelectRegions]) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in avec.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&format!("{}", orx));
        }
        ret_str.push(']');
        ret_str
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
/// A struct of select RegionStores, where each RegionStore contains a list
/// of domain select regions, in domain number order.
pub struct SelectRegionsStore {
    pub regions: Vec<SelectRegions>,
}

impl SelectRegionsStore {
    /// Return a new SelectRegionsStores instance.
    pub fn new(regions: Vec<SelectRegions>) -> Self {
        Self { regions }
    }

    /// Return a new SelectRegionStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            regions: Vec::<SelectRegions>::with_capacity(num),
        }
    }

    /// Add a RegionsStore.
    pub fn push(&mut self, regions: RegionStore, value: isize) {
        if !self.contains(&regions) {
            self.regions.push(SelectRegions { regions, value });
        }
    }

    /// Return the length of an instance.
    pub fn len(&self) -> usize {
        self.regions.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.regions.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.regions.is_empty()
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<SelectRegions> {
        self.regions.iter()
    }

    /// Return the number of supersets of a StateStore
    pub fn number_supersets_of_states(&self, stas: &[&SomeState]) -> usize {
        self.regions
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_corr_states(stas)))
            .sum()
    }

    /// Return a Vector of RegionStores not supersets of a given StateStore.
    pub fn not_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regions
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_corr_states(stas))
            .collect()
    }

    /// Return true if any RegionStore is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &[&SomeState]) -> bool {
        for regsx in &self.regions {
            if regsx.regions.is_superset_corr_states(stas) {
                return true;
            }
        }
        false
    }

    // Return the aggregate value of select regions the current states are in.
    pub fn value_supersets_of_states(&self, stas: &[&SomeState]) -> isize {
        let mut val: isize = 0;
        for regsx in &self.regions {
            if regsx.regions.is_superset_corr_states(stas) {
                val += regsx.value;
            }
        }
        val
    }

    /// Return true if any RegionStore is a superset of a given RegionStore.
    pub fn any_supersets_of(&self, regs: &RegionStore) -> bool {
        for regsx in &self.regions {
            if regsx.regions.is_superset_corr(regs) {
                return true;
            }
        }
        false
    }

    /// Return list of select regions that are superset of a StateStore.
    pub fn supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regions
            .iter()
            .filter(|regsx| regsx.regions.is_superset_corr_states(stas))
            .collect()
    }

    /// Return a string represeting an SelectRegionsStore.
    pub fn formatted_string(&self) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in self.regions.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&format!("{}", orx));
        }
        ret_str.push(']');
        ret_str
    }

    /// Return a copy of the select RegionStores, and intersections.
    pub fn and_intersections(&self) -> Self {
        //println!("and_intersections of {}", &self.formatted_string());

        let mut select_and_ints = SelectRegionsStore::with_capacity(self.len());

        for regstrx in &self.regions {
            select_and_ints.regions.push(regstrx.clone());
        }

        let mut changed = true;
        while changed {
            changed = false;
            let lenx = select_and_ints.len();
            for inx in 0..(lenx - 1) {
                for iny in (inx + 1)..lenx {
                    if let Some(an_int) = select_and_ints[inx]
                        .regions
                        .intersect_corr(&select_and_ints[iny].regions)
                    {
                        if !select_and_ints.contains(&an_int) {
                            select_and_ints.push(
                                an_int,
                                select_and_ints[inx].value + select_and_ints[iny].value,
                            );
                            changed = true;
                        }
                    }
                }
            }
        }

        select_and_ints
    }

    /// Return true if an equal RegionStore is already in the SelectRegionsStore.
    fn contains(&self, regstr: &RegionStore) -> bool {
        for regstrx in &self.regions {
            if regstrx.regions.equal_corr(regstr) {
                return true;
            }
        }
        false
    }
}

impl Index<usize> for SelectRegionsStore {
    type Output = SelectRegions;
    fn index(&self, i: usize) -> &SelectRegions {
        &self.regions[i]
    }
}
