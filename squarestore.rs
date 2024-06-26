//! The SquareStore struct.  A HashMap of SomeSquare structs, the key in the state field.

use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::statestore::StateStore;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

impl fmt::Display for SquareStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

/// Error enum for pick_a_square_in (for more samples)
pub enum PickError {
    NoSquares,
    PncSquare,
}

#[readonly::make]
#[derive(Serialize, Deserialize, Default)]
pub struct SquareStore {
    pub ahash: HashMap<SomeState, SomeSquare>,
    num_bits: Option<usize>,
}

impl SquareStore {
    /// Return a new, empty, SquareStore.
    pub fn new(ahash: HashMap<SomeState, SomeSquare>) -> Self {
        Self { ahash, num_bits: None }
    }

    /// Remove a square.
    pub fn remove(&mut self, key: &SomeState) -> Option<SomeSquare> {
        debug_assert!(self.num_bits.is_none() || key.num_bits() == self.num_bits.unwrap());

        self.ahash.remove(key)
    }

    /// Return the number of squares stored.
    pub fn len(&self) -> usize {
        self.ahash.len()
    }

    /// Return a list of squares in a given region.
    pub fn stas_in_reg(&self, areg: &SomeRegion) -> StateStore {
        debug_assert!(self.num_bits.is_none() || areg.num_bits() == self.num_bits.unwrap());

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
        debug_assert!(self.num_bits.is_none() || areg.num_bits() == self.num_bits.unwrap());

        self.ahash
            .values()
            .filter(|sqrx| areg.is_superset_of(*sqrx))
            .collect()
    }

    /// Return an Option mutable reference for a square given a state,
    /// or None if not found.
    pub fn find_mut(&mut self, val: &SomeState) -> Option<&mut SomeSquare> {
        debug_assert!(self.num_bits.is_none() || val.num_bits() == self.num_bits.unwrap());

        self.ahash.get_mut(val)
    }

    /// Find a square that is expected to exist.
    pub fn find_mut_must(&mut self, val: &SomeState) -> &mut SomeSquare {
        debug_assert!(self.num_bits.is_none() || val.num_bits() == self.num_bits.unwrap());

        if let Some(sqrx) = self.ahash.get_mut(val) {
            return sqrx;
        }
        panic!("Square expected to be found is not found");
    }

    /// Return an Option immutable reference for a square given a state,
    /// or None if not found.
    pub fn find(&self, val: &SomeState) -> Option<&SomeSquare> {
        debug_assert!(self.num_bits.is_none() || val.num_bits() == self.num_bits.unwrap());

        self.ahash.get(val)
    }

    /// Find a square that is expected to exist.
    pub fn find_must(&self, val: &SomeState) -> &SomeSquare {
        debug_assert!(self.num_bits.is_none() || val.num_bits() == self.num_bits.unwrap());

        if let Some(sqrx) = self.ahash.get(val) {
            return sqrx;
        }
        panic!("Square expected to be found is not found");
    }

    /// Add a square that is not currently in the store.
    pub fn insert(&mut self, sqrx: SomeSquare, dom_id: usize, act_id: usize) {
        if self.num_bits.is_none() {
            self.num_bits = Some(sqrx.num_bits());
        }
        debug_assert!(sqrx.num_bits() == self.num_bits.unwrap());

        println!(
            "\nDom {} Adding square {} -{}-> {}",
            dom_id,
            sqrx.state,
            act_id,
            sqrx.first_result()
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
        debug_assert!(self.num_bits.is_none() || regx.num_bits() == self.num_bits.unwrap());

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

    /// Pick a square in a region, for seeking more samples.
    /// Return a square with the maximum number of samples.
    pub fn pick_a_square_in(&self, target_reg: &SomeRegion) -> Result<&SomeSquare, PickError> {
        debug_assert!(self.num_bits.is_none() || target_reg.num_bits() == self.num_bits.unwrap());

        let sqrs = self.squares_in_reg(target_reg);

        if sqrs.is_empty() {
            return Err(PickError::NoSquares);
        }

        // Check for pnc square in target reg.
        for sqrx in sqrs.iter() {
            if sqrx.pnc {
                println!("Problem: sqr {}", sqrx);
                return Err(PickError::PncSquare);
            }
        }

        // Get squares with the maximum number of samples.
        let mut max_rated = Vec::<&SomeSquare>::new();
        let mut max_rate = 0;

        for sqrx in sqrs.iter() {
            let rt = sqrx.rate();
            if rt > max_rate {
                max_rated = Vec::<&SomeSquare>::new();
                max_rate = rt;
            }
            if rt == max_rate {
                max_rated.push(sqrx);
            }
        }
        // Choose a maximum rated square.
        Ok(max_rated[rand::thread_rng().gen_range(0..max_rated.len())])
    }
} // end impl SquareStore
