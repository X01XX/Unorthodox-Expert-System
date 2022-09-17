use crate::regionstore::RegionStore;
/// Implement a struct for Optimal RegionsStores.
use crate::statestore::StateStore;

use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct OptimalRegionsStore {
    pub optimal: Vec<RegionStore>,
}

impl Default for OptimalRegionsStore {
    fn default() -> Self {
        Self::new()
    }
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

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.optimal.len() == 0
    }

    /// Return an iterator
    pub fn iter(&self) -> Iter<RegionStore> {
        self.optimal.iter()
    }

    /// Return the number of supersets of a StateStore
    pub fn number_supersets_of_states(&self, stas: &StateStore) -> usize {
        self.optimal.iter().map(|regsx| if regsx.is_superset_of_states(stas) { 1 } else { 0 }).sum()
    }

    /// Return a Vector of RegionStores not supersets of a given StateStore.
    pub fn not_supersets_of_states(&self, stas: &StateStore) -> Self {
        Self {
            optimal: self.optimal.iter().filter_map(|regsx| if regsx.is_superset_of_states(stas) { None } else { Some(regsx.clone()) }).collect(),
        }
    }

    /// Return true if any RegionStore is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &StateStore) -> bool {
        for regsx in self.optimal.iter() {
            if regsx.is_superset_of_states(stas) {
                return true;
            }
        }
        false
    }

    /// Return list of optimal regions that are superset of a StateStore.
    pub fn supersets_of_states(&self, stas: &StateStore) -> Self {
        Self {
            optimal: self.optimal.iter().filter_map(|regsx| if regsx.is_superset_of_states(stas) { Some(regsx.clone()) } else { None }).collect(),
        }
    }

    /// Return a string represeting an OptimalRegionsStore.
    pub fn formatted_string(&self) -> String {
        let mut ret_str = String::from("[");
        let mut not_first = false;
        for regstrx in self.optimal.iter() {
            if not_first {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&regstrx.formatted_string());
            not_first = true;
        }
        ret_str.push(']');
        ret_str
    }

    /// Return a copy of the optimal RegionStores, and intersections.
    pub fn and_intersections(&self) -> Self {
        //println!("and_intersections of {}", &self.formatted_string());

        let mut optimal_and_ints = OptimalRegionsStore::new();

        for regstrx in self.optimal.iter() {
            optimal_and_ints.push(regstrx.clone());
        }

        let mut changed = true;
        while changed {
            changed = false;
            let lenx = optimal_and_ints.len();
            for inx in 0..(lenx - 1) {
                for iny in (inx + 1)..lenx {
                    if let Some(an_int) =
                        optimal_and_ints[inx].intersect_each(&optimal_and_ints[iny])
                    {
                        if !optimal_and_ints.contains(&an_int) {
                            optimal_and_ints.push(an_int);
                            changed = true;
                        }
                    }
                }
            }
        }

        optimal_and_ints
    }

    fn contains(&self, regstr: &RegionStore) -> bool {
        for regstrx in self.optimal.iter() {
            if regstrx.equal_each(regstr) {
                return true;
            }
        }
        false
    }
}

impl Index<usize> for OptimalRegionsStore {
    type Output = RegionStore;
    fn index(&self, i: usize) -> &RegionStore {
        &self.optimal[i]
    }
}
