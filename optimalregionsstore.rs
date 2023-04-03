//! Implement a struct of optimal RegionStores.

use crate::regionstore::RegionStore;

use crate::state::SomeState;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for OptimalRegions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = self.regions.formatted_string();
        str.push_str(&format!("/{}", self.value));
        write!(f, "{}", str)
    }
}

//#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct OptimalRegions {
    /// Regions, in domain order, describing the requirements for an optimal state.
    pub regions: RegionStore,
    /// A value for being in the optimal state.
    pub value: usize,
}

impl fmt::Display for OptimalRegionsStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl OptimalRegions {
    pub fn vec_ref_string(avec: &[&OptimalRegions]) -> String {
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
/// A struct of optimal RegionStores, where each RegionStore contains a list
/// of domain optimal regions, in domain number order.
pub struct OptimalRegionsStore {
    pub optimal: Vec<OptimalRegions>,
}

impl OptimalRegionsStore {
    /// Return a new OptimalRegionsStores instance.
    pub fn new(optimal: Vec<OptimalRegions>) -> Self {
        Self { optimal }
    }

    /// Return a new OptimalRegionStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            optimal: Vec::<OptimalRegions>::with_capacity(num),
        }
    }

    /// Add a RegionsStore.
    pub fn push(&mut self, regions: RegionStore, value: usize) {
        if !self.contains(&regions) {
            self.optimal.push(OptimalRegions { regions, value });
        }
    }

    /// Return the length of an instance.
    pub fn len(&self) -> usize {
        self.optimal.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.optimal.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.optimal.is_empty()
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<OptimalRegions> {
        self.optimal.iter()
    }

    /// Return the number of supersets of a StateStore
    pub fn number_supersets_of_states(&self, stas: &[&SomeState]) -> usize {
        self.optimal
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_corr_states(stas)))
            .sum()
    }

    /// Return a Vector of RegionStores not supersets of a given StateStore.
    pub fn not_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&OptimalRegions> {
        self.optimal
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_corr_states(stas))
            .collect()
    }

    /// Return true if any RegionStore is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &[&SomeState]) -> bool {
        for regsx in &self.optimal {
            if regsx.regions.is_superset_corr_states(stas) {
                return true;
            }
        }
        false
    }

    /// Return true if any RegionStore is a superset of a given RegionStore.
    pub fn any_supersets_of(&self, regs: &RegionStore) -> bool {
        for regsx in &self.optimal {
            if regsx.regions.is_superset_corr(regs) {
                return true;
            }
        }
        false
    }

    /// Return list of optimal regions that are superset of a StateStore.
    pub fn supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&OptimalRegions> {
        self.optimal
            .iter()
            .filter(|regsx| regsx.regions.is_superset_corr_states(stas))
            .collect()
    }

    /// Return a string represeting an OptimalRegionsStore.
    pub fn formatted_string(&self) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in self.optimal.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&format!("{}", orx));
        }
        ret_str.push(']');
        ret_str
    }

    /// Return a copy of the optimal RegionStores, and intersections.
    pub fn and_intersections(&self) -> Self {
        //println!("and_intersections of {}", &self.formatted_string());

        let mut optimal_and_ints = OptimalRegionsStore::with_capacity(self.len());

        for regstrx in &self.optimal {
            optimal_and_ints.optimal.push(regstrx.clone());
        }

        let mut changed = true;
        while changed {
            changed = false;
            let lenx = optimal_and_ints.len();
            for inx in 0..(lenx - 1) {
                for iny in (inx + 1)..lenx {
                    if let Some(an_int) = optimal_and_ints[inx]
                        .regions
                        .intersect_corr(&optimal_and_ints[iny].regions)
                    {
                        if !optimal_and_ints.contains(&an_int) {
                            optimal_and_ints.push(
                                an_int,
                                optimal_and_ints[inx].value + optimal_and_ints[iny].value,
                            );
                            changed = true;
                        }
                    }
                }
            }
        }

        optimal_and_ints
    }

    /// Return true if an equal RegionStore is already in the OptimalRegionsStore.
    fn contains(&self, regstr: &RegionStore) -> bool {
        for regstrx in &self.optimal {
            if regstrx.regions.equal_corr(regstr) {
                return true;
            }
        }
        false
    }
}

impl Index<usize> for OptimalRegionsStore {
    type Output = OptimalRegions;
    fn index(&self, i: usize) -> &OptimalRegions {
        &self.optimal[i]
    }
}
