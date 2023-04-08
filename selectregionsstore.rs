//! Implement a struct of Select RegionStores.

use crate::regionstore::{vec_rs_corr_split_by_partial_intersection, RegionStore};

use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::randompick::RandomPick;

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
    pub regionstores: Vec<SelectRegions>,
}

impl SelectRegionsStore {
    /// Return a new SelectRegionsStores instance.
    pub fn new(regionstores: Vec<SelectRegions>) -> Self {
        Self { regionstores }
    }

    /// Return a new SelectRegionStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            regionstores: Vec::<SelectRegions>::with_capacity(num),
        }
    }

    /// Add a RegionsStore.
    pub fn push(&mut self, regions: RegionStore, value: isize) {
        if value != 0 && !self.contains(&regions) {
            self.regionstores.push(SelectRegions { regions, value });
        }
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

    /// Return the number of supersets of a StateStore
    pub fn number_supersets_of_states(&self, stas: &[&SomeState]) -> usize {
        self.regionstores
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_corr_states(stas)))
            .sum()
    }

    /// Return a Vector of RegionStores not supersets of a given StateStore.
    pub fn not_supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| !regsx.regions.is_superset_corr_states(stas))
            .collect()
    }

    /// Return true if any RegionStore is a superset of a StateStore.
    pub fn any_supersets_of_states(&self, stas: &[&SomeState]) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_corr_states(stas) {
                return true;
            }
        }
        false
    }

    // Return the aggregate value of select regions the current states are in.
    pub fn value_supersets_of_states(&self, stas: &[&SomeState]) -> isize {
        let mut val: isize = 0;
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_corr_states(stas) {
                val += regsx.value;
            }
        }
        val
    }

    /// Return true if any RegionStore is a superset of a given RegionStore.
    pub fn any_supersets_of(&self, regs: &RegionStore) -> bool {
        for regsx in &self.regionstores {
            if regsx.regions.is_superset_corr(regs) {
                return true;
            }
        }
        false
    }

    /// Return list of select regions that are superset of a StateStore.
    pub fn supersets_of_states(&self, stas: &[&SomeState]) -> Vec<&SelectRegions> {
        self.regionstores
            .iter()
            .filter(|regsx| regsx.regions.is_superset_corr_states(stas))
            .collect()
    }

    /// Return a string represeting an SelectRegionsStore.
    pub fn formatted_string(&self) -> String {
        let mut ret_str = String::from("[");
        for (inx, orx) in self.regionstores.iter().enumerate() {
            if inx > 0 {
                ret_str.push_str(", ");
            }
            ret_str.push_str(&format!("{}", orx));
        }
        ret_str.push(']');
        ret_str
    }

    /// Return a list of regionstores, split by partial intersection, until
    /// none have a partial intersection with the original regionstores.
    /// Some result regionstores may overlap each other.
    /// The original regionstores minus the result regionstores should be null.
    /// Each result regionstore will be a subset of one, or more, of the original regionstores.
    pub fn split_by_partial_intersections(&self) -> Self {
        let mut rs = Vec::<RegionStore>::with_capacity(self.len());
        for reg_valx in &self.regionstores {
            rs.push(reg_valx.regions.clone());
        }

        let mut ret = Self::new(vec![]);
        for reg_strx in vec_rs_corr_split_by_partial_intersection(&rs) {
            let mut val = 0;
            for reg_valx in &self.regionstores {
                if reg_valx.regions.is_superset_corr(&reg_strx) {
                    val += reg_valx.value;
                }
            }
            ret.push(reg_strx, val);
        }
        ret
    }

    /// Return true if an equal RegionStore is already in the SelectRegionsStore.
    fn contains(&self, regstr: &RegionStore) -> bool {
        for regstrx in &self.regionstores {
            if regstrx.regions.equal_corr(regstr) {
                return true;
            }
        }
        false
    }

    /// Return needs to exit negative select regions.
    pub fn choose_select_exit_needs(&self, all_states: &[&SomeState]) -> Option<NeedStore> {
        let mut ret_nds = NeedStore::new(vec![]);

        let mut sup_store = Vec::<&SelectRegions>::new();

        for selx in self.regionstores.iter() {
            if selx.value < 0 && selx.regions.is_superset_corr_states(all_states) {
                sup_store.push(selx);
            }
        }

        for selx in sup_store.iter() {
            let edges = selx.regions.edge_mask_corr();
            let mut rp1 = RandomPick::new(edges.len());
            while let Some(inx) = rp1.pick() {
                if edges[inx].is_low() { continue; }
                
                let edge_mask_per_bit = edges[inx].split();
                let mut rp2 = RandomPick::new(edge_mask_per_bit.len());

                while let Some(iny) = rp2.pick() {
                    let edgex = &edge_mask_per_bit[iny];
                    // Assemble altered vector of state refs.
                    let new_state = all_states[inx].bitwise_xor(edgex);
                    let mut statesx = Vec::<&SomeState>::with_capacity(all_states.len());
                    for (iny, stax) in all_states.iter().enumerate() {
                        if iny == inx {
                            statesx.push(&new_state);
                        } else {
                            statesx.push(stax);
                        }
                    }
                    if self.value_supersets_of_states(&statesx) >= 0 {
                        // Assemble states for need.
                        let mut statesy = StateStore::with_capacity(all_states.len());
                        for stax in statesx.into_iter() {
                            statesy.push(stax.clone());
                        }
                        ret_nds.push(SomeNeed::FromSelectRegion {
                            target_states: statesy,
                        });
                        if ret_nds.len() > 4 {
                            return Some(ret_nds);
                        }
                    }
                }
            }
        }

        if ret_nds.is_empty() {
            None
        } else {
            Some(ret_nds)
        }
    }
} // End impl SelectRegionsStore

impl Index<usize> for SelectRegionsStore {
    type Output = SelectRegions;
    fn index(&self, i: usize) -> &SelectRegions {
        &self.regionstores[i]
    }
}
