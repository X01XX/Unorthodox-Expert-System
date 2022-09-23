//! The SquareStore struct.  A HashMap of SomeSquare structs.

use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::statestore::StateStore;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing

impl fmt::Display for SquareStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::new();

        for sqrx in self.ahash.values() {
            if flg == 1 {
                rc_str.push_str(",\n");
            }
            let _ = write!(rc_str, "{}", sqrx);
            flg = 1;
        }

        write!(f, "{}", rc_str)
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct SquareStore {
    pub ahash: HashMap<SomeState, SomeSquare>,
}

impl Default for SquareStore {
    fn default() -> Self {
        Self::new()
    }
}

impl SquareStore {
    /// Return a new, empty, SquareStore.
    pub fn new() -> Self {
        Self {
            ahash: HashMap::new(),
        }
    }

    /// Remove a square
    pub fn remove(&mut self, stax: &SomeState) {
        self.ahash.remove(stax);
    }

    /// Return the number of squares stored.
    pub fn len(&self) -> usize {
        self.ahash.len()
    }

    /// Return the states for all squares.
    pub fn all_square_states(&self) -> StateStore {
        StateStore {
            avec: self.ahash.keys().cloned().collect()
        }
    }

    /// Return a list of squares in a given region.
    pub fn stas_in_reg(&self, areg: &SomeRegion) -> StateStore {
        StateStore {
            avec: self.ahash.keys().filter_map(|keyx| if areg.is_superset_of_state(keyx) { Some(keyx.clone()) } else { None }).collect()
        }
    }

    /// Return a list of squares in a given region.
    pub fn squares_in_reg(&self, areg: &SomeRegion) -> Vec<&SomeSquare> {
        self.ahash.values().filter(|sqrx| areg.is_superset_of_state(&sqrx.state)).collect()
    }

    /// Return a list of squares between two given squares.
    pub fn squares_between(&self, sqr1: &SomeSquare, sqr2: &SomeSquare) -> Vec<&SomeSquare> {
        let mut ret = Vec::<&SomeSquare>::new();

        for sqrx in self.ahash.values() {
            if sqrx.is_between(sqr1, sqr2) {
                ret.push(sqrx);
            }
        }
        ret
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
        assert!(self.find(&sqrx.state).is_none());
        self.ahash.insert(sqrx.state.clone(), sqrx);
    }

    /// Return a list of square states not in a list of regions.
    pub fn not_in_regions(&self, regs: &RegionStore) -> StateStore {
        StateStore {
            avec: self.ahash.keys().filter_map(|keyx| if !regs.any_superset_of_state(keyx) { Some(keyx.clone()) } else { None }).collect()
        }
    }

    /// Return a list of squares with pn GT Pn:One, not yet pnc.
    pub fn pn_gt1_no_pnc(&self) -> StateStore {
        StateStore {
            avec: self.ahash.values().filter_map(|sqrx| if sqrx.pn != Pn::One && !sqrx.pnc { Some(sqrx.state.clone()) } else { None }).collect()
        }
    }

    /// Return a list of square states that are only in one region of a list of regions.
    pub fn states_in_1_region(&self, regs: &RegionStore) -> StateStore {
        StateStore {
            avec: self.ahash.keys().filter_map(|keyx| if regs.state_in_1_region(keyx) { Some(keyx.clone()) } else { None }).collect()
        }
    }

    /// Return a list of square states that are adjacent to a given region.
    pub fn stas_adj_reg(&self, regx: &SomeRegion) -> StateStore {
        StateStore {
            avec: self.ahash.keys().filter_map(|keyx| if regx.is_adjacent_state(keyx) { Some(keyx.clone()) } else { None }).collect()
        }

    }
} // end impl SquareStore
