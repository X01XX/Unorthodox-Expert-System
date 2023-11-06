//! The SquareStore struct.  A HashMap of SomeSquare structs.

use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::statestore::StateStore;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

impl fmt::Display for SquareStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Default)]
pub struct SquareStore {
    pub ahash: HashMap<SomeState, SomeSquare>,
}

impl SquareStore {
    /// Return a new, empty, SquareStore.
    pub fn new(ahash: HashMap<SomeState, SomeSquare>) -> Self {
        Self { ahash }
    }

    /// Remove a square.
    pub fn remove(&mut self, key: &SomeState) -> Option<SomeSquare> {
        self.ahash.remove(key)
    }

    /// Return the number of squares stored.
    pub fn len(&self) -> usize {
        self.ahash.len()
    }

    /// Return a list of squares in a given region.
    pub fn stas_in_reg(&self, areg: &SomeRegion) -> StateStore {
        let mut ret_keys = StateStore::new(vec![]);

        let sel_keys: Vec<&SomeState> = self
            .ahash
            .keys()
            .filter(|keyx| areg.is_superset_of(*keyx))
            .collect();

        for keyx in sel_keys.iter() {
            ret_keys.push((*keyx).clone());
        }
        ret_keys
    }

    /// Return a list of squares in a given region.
    pub fn squares_in_reg(&self, areg: &SomeRegion) -> Vec<&SomeSquare> {
        self.ahash
            .values()
            .filter(|sqrx| areg.is_superset_of(*sqrx))
            .collect()
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

    /// Return a StateStore of squares with pn GT Pn:One, not yet pnc.
    pub fn pn_gt1_no_pnc(&self) -> StateStore {
        let mut ret_keys = StateStore::new(vec![]);

        let sel_keys: Vec<&SomeState> = self
            .ahash
            .values()
            .filter_map(|sqrx| {
                if sqrx.pn != Pn::One && !sqrx.pnc {
                    Some(&sqrx.state)
                } else {
                    None
                }
            })
            .collect();

        for keyx in sel_keys.iter() {
            ret_keys.push((*keyx).clone());
        }
        ret_keys
    }

    /// Return a StateStore of square states that are adjacent to a given region.
    pub fn stas_adj_reg(&self, regx: &SomeRegion) -> StateStore {
        let mut ret_keys = StateStore::new(vec![]);

        let sel_keys: Vec<&SomeState> = self
            .ahash
            .keys()
            .filter(|keyx| regx.is_adjacent(*keyx))
            .collect();

        for keyx in sel_keys.iter() {
            ret_keys.push((*keyx).clone());
        }
        ret_keys
    }

    /// Return a String representation of a SquareStore.
    fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::new();

        for sqrx in self.ahash.values() {
            if flg == 1 {
                rc_str.push_str(",\n");
            }
            rc_str.push_str(&format!("{sqrx}"));
            flg = 1;
        }

        rc_str
    }
} // end impl SquareStore
