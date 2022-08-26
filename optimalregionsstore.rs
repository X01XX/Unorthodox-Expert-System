/// Implement a struct for Optimal RegionsStores.

use crate::statestore::StateStore;
use crate::regionstore::RegionStore;

use serde::{Deserialize, Serialize};
use std::ops::{Index};
use std::slice::{Iter};

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct OptimalRegionsStore {
    pub optimal: Vec<RegionStore>,
}

impl OptimalRegionsStore {
    /// Return a new OptimalRegionsStores instance.
    pub fn new() -> Self {
        Self {
            optimal: Vec::<RegionStore>::new(),
        }
    }

    /// Add a RegionsStore.
    pub fn push(&mut self, rsx: RegionStore) {
        self.optimal.push(rsx);
    }

    /// Return the length of an instance.
    pub fn len(&self) -> usize {
        self.optimal.len()
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<RegionStore> {
        self.optimal.iter()
    }

    /// Return the number of supersets of a StateStore
    pub fn number_supersets_of_states(&self, stas: &StateStore) -> usize {
        let mut ret = 0;

        for regsx in self.optimal.iter() {
            if regsx.is_superset_of_states(stas) {
                ret += 1;
            }
        }
        ret
    }

    /// Return a Vector of RegionStores not supersets of a given StateStore.
    pub fn not_supersets_of_states(&self, stas: &StateStore) -> Self {
        let mut ret_store = Self::new();

        for regsx in self.optimal.iter() {
            if regsx.is_superset_of_states(stas) {
            } else {
                ret_store.push(regsx.clone());
            }
        }
        ret_store
    }

    /// Return true if any RegionStore is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &StateStore) -> bool {

        for regsx in self.optimal.iter() {
            if regsx.is_superset_of_states(&stas) {
                return true;
            }
        }
        false
    }

    /// Return list of optimal regions that are superset of a StateStore.
    pub fn supersets_of_states(&self, stas: &StateStore) -> Self {
        let mut ret = Self::new();
        for regsx in self.optimal.iter() {
            if regsx.is_superset_of_states(&stas) {
                ret.push(regsx.clone());
            }
        }
        ret
    }

    /// Return a copy of the optimal RegionStores, and intersections.
    pub fn and_intersections(&self) -> Self {

        let mut optimal_and_ints = OptimalRegionsStore::new();

        for regstrx in self.optimal.iter() {
            optimal_and_ints.push(regstrx.clone());
        }

        for inx in 0..(self.len() - 1) {

            for iny in (inx + 1)..self.len() {

                if let Some(an_int) = self[inx].intersect_each(&self[iny]) {
                    optimal_and_ints.push(an_int);
                }
            }
        }

        optimal_and_ints
    }
}

impl Index<usize> for OptimalRegionsStore {
    type Output = RegionStore;
    fn index<'a>(&'a self, i: usize) -> &'a RegionStore {
        &self.optimal[i]
    }
}
