//! The SquareStore struct.  A HashMap of SomeSquare structs.
//!

//use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::statestore::StateStore;

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
            rc_str.push_str(&format!("{}", &sqrx));
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
    pub fn new() -> Self {
        Self {
            ahash: HashMap::new(),
        }
    }

    // Return a string for squares in a given region.
    pub fn stas_in_reg(&self, areg: &SomeRegion) -> StateStore {
        let mut rc_store = StateStore::new();

        for (key, _) in &self.ahash {
            if areg.is_superset_of_state(&key) {
                rc_store.push(key.clone());
            }
        }
        rc_store
    }

    pub fn find_mut(&mut self, val: &SomeState) -> Option<&mut SomeSquare> {
        self.ahash.get_mut(val)
    }

    pub fn find(&self, val: &SomeState) -> Option<&SomeSquare> {
        self.ahash.get(val)
    }

    // Add a square that is not currently in the store.
    pub fn insert(&mut self, sqrx: SomeSquare) {
        self.ahash.insert(sqrx.state.clone(), sqrx);
    }

    pub fn not_in_regions(&self, regs: &RegionStore) -> StateStore {
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            if regs.any_superset_of_state(&key) == false {
                states.push(key.clone());
            }
        }

        states
    }

    pub fn states_in_1_region(&self, regs: &RegionStore) -> StateStore {
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            if regs.state_in_1_region(&key) {
                states.push(key.clone());
            }
        }

        states
    }

    // Return states that are adjacent to a given region.
    pub fn stas_adj_reg(&self, regx: &SomeRegion) -> StateStore {
        let mut states = StateStore::new();

        for (key, _sqry) in &self.ahash {
            if regx.is_adjacent_state(&key) {
                states.push(key.clone());
            }
        }

        states
    }

    // Return a StateStore of states of squares in a given regions
    //    pub fn stas_in_regs(&self, regsx: &RegionStore) -> StateStore {
    //        let mut states = StateStore::new();
    //
    //        for (key, _sqry) in &self.ahash {
    //            for regx in regsx.iter() {
    //                if regx.is_superset_of_state(&key) {
    //                    states.push(key.clone());
    //                    break;
    //                }
    //            }
    //        }
    //        states
    //    }

    // Return the maximum Pn value of a set of squares,
    // identified by a list of their keys.
    //    pub fn max_pn(&self, keys: &StateStore) -> Pn {
    //        assert!(keys.len() > 0);
    //
    //        let mut max_pn = Pn::One;
    //
    //        for keyx in keys.iter() {
    //            let sqrx = self.find(&keyx).unwrap();
    //            if sqrx.pn() > max_pn {
    //                max_pn = sqrx.pn();
    //            }
    //        }
    //        max_pn
    //    } // end max_pn

    // Return the minimum Pn value of a set pnc squares,
    // identified by a list of their keys.
    //    pub fn min_pnc(&self, keys: &StateStore) -> Pn {
    //        assert!(keys.len() > 0);
    //
    //        let mut min_pnc = Pn::Unpredictable;
    //        let mut not_found = true;
    //
    //        for keyx in keys.iter() {
    //            let sqrx = self.find(&keyx).unwrap();
    //            if sqrx.pnc() {
    //                not_found = false;
    //                if sqrx.pn() < min_pnc {
    //                    min_pnc = sqrx.pn();
    //                }
    //            }
    //        }
    //
    //        if not_found {
    //            panic!("No pnc square found in list!");
    //        }
    //        min_pnc
    //    } // end min_pnc

    // Return true if any key value in a StateStore corresponds with
    // a square that has pnc set to true.
    //    pub fn any_pnc(&self, keys: &StateStore) -> bool {
    //        for keyx in keys.iter() {
    //            let sqrx = self.find(&keyx).unwrap();
    //            if sqrx.pnc() {
    //                return true;
    //            }
    //        }
    //
    //        false
    //    }
} // end impl SquareStore
