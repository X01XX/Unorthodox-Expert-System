//! The SquareStore struct.  A HashMap of SomeSquare structs.

use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::pn::Pn;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

impl fmt::Display for SquareStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::new();

        for (_key, sqrx) in &self.ahash {
            if flg == 1 {
                rc_str.push_str(",\n");
            }
            rc_str.push_str(&format!("{}", sqrx));
            flg = 1;
        }

        write!(f, "{}", rc_str)
    }
}

#[derive(Serialize, Deserialize)]
pub struct SquareStore {
    pub ahash: HashMap<SomeState, SomeSquare>,
}

impl SquareStore {
    /// Return a new, empty, SquareStore.
    pub fn new() -> Self {
        Self {
            ahash: HashMap::new(),
        }
    }

    /// Return a list of squares in a given region.
    pub fn stas_in_reg(&self, areg: &SomeRegion) -> StateStore {
        let mut rc_store = StateStore::new();

        for (key, _) in &self.ahash {
            if areg.is_superset_of_state(key) {
                rc_store.push(key.clone());
            }
        }
        rc_store
    }

    /// Return a list of squares in a given region.
    pub fn squares_in_reg(&self, areg: &SomeRegion) -> Vec::<&SomeSquare> {
        let mut ret_vec = Vec::<&SomeSquare>::new();

        for (key, sqrx) in &self.ahash {
            if areg.is_superset_of_state(key) {
                ret_vec.push(sqrx);
            }
        }
        ret_vec
    }

    /// Return an Option mutable reference for a square given a state,
    /// or None if not found.
    pub fn find_mut(&mut self, val: &SomeState) -> Option<&mut SomeSquare> {
        self.ahash.get_mut(val)
    }

    /// Return an Option immutable reference for a square given a state,
    /// or None if not found.
    pub fn find(&self, val: &SomeState) -> Option<&SomeSquare> {
        self.ahash.get(val)
    }

    /// Add a square that is not currently in the store.
    pub fn insert(&mut self, sqrx: SomeSquare, dom: usize, act: usize) {
        println!(
            "\nDom {} Act {} Adding square {} -> {}",
            dom,
            act,
            sqrx.state,
            &sqrx.first_result()
        );
        self.ahash.insert(sqrx.state.clone(), sqrx);
    }

    /// Return a list of square states not in a list of regions.
    pub fn not_in_regions(&self, regs: &RegionStore) -> StateStore {
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            if regs.any_superset_of_state(key) == false {
                states.push(key.clone());
            }
        }

        states
    }

    /// Return a list of squares with pn GT Pn:One, not yet pnc.
    pub fn pn_gt1_no_pnc(&self) -> StateStore {
        let mut states = StateStore::new();

        for (key, sqry) in &self.ahash {
            if sqry.results.pn != Pn::One &&
               sqry.results.pnc == false {
                   states.push(key.clone());
            }
        }

        states
    }

    /// Return a list of square states that are only in one region of a list of regions.
    pub fn states_in_1_region(&self, regs: &RegionStore) -> StateStore {
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            if regs.state_in_1_region(key) {
                states.push(key.clone());
            }
        }

        states
    }

    /// Return a list of square states that are adjacent to a given region.
    pub fn stas_adj_reg(&self, regx: &SomeRegion) -> StateStore {
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            if regx.is_adjacent_state(key) {
                states.push(key.clone());
            }
        }

        states
    }

} // end impl SquareStore
