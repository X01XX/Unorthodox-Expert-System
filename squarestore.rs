//! The SquareStore struct.  A HashMap of SomeSquare structs, the key is the square state field.

use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::sample::SomeSample;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::statestore::StateStore;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;

/// Maximum number of recent squares/samples to keep in a circular buffer.
const MAX_MEMORY: usize = 20;

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
    num_bits: usize,
    /// Memory for squares that are no longer needed.
    pub memory: VecDeque<SomeSquare>,
}

impl SquareStore {
    /// Return a new, empty, SquareStore.
    pub fn new(ahash: HashMap<SomeState, SomeSquare>, num_bits: usize) -> Self {
        Self {
            ahash,
            num_bits,
            memory: VecDeque::<SomeSquare>::with_capacity(MAX_MEMORY),
        }
    }

    /// Remove a square.
    pub fn remove(&mut self, key: &SomeState) -> Option<SomeSquare> {
        debug_assert!(key.num_bits() == self.num_bits);

        self.ahash.remove(key)
    }

    /// Return the number of squares stored.
    pub fn len(&self) -> usize {
        self.ahash.len()
    }

    /// Return a list of squares in a given region.
    pub fn stas_in_reg(&self, areg: &SomeRegion) -> StateStore {
        debug_assert!(areg.num_bits() == self.num_bits);

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
        debug_assert!(areg.num_bits() == self.num_bits);

        self.ahash
            .values()
            .filter(|sqrx| areg.is_superset_of(*sqrx))
            .collect()
    }

    /// Return a list of memory squares in a given region.
    pub fn memory_squares_in_reg(&self, areg: &SomeRegion) -> Vec<&SomeSquare> {
        debug_assert!(areg.num_bits() == self.num_bits);

        let mut ret_sqrs = Vec::<&SomeSquare>::new();

        for sqrx in self.memory.iter() {
            if areg.is_superset_of(&sqrx.state) {
                ret_sqrs.push(sqrx);
            }
        }
        ret_sqrs
    }

    /// Return an Option mutable reference for a square given a state,
    /// or None if not found.
    pub fn find_mut(&mut self, key: &SomeState) -> Option<&mut SomeSquare> {
        debug_assert!(key.num_bits() == self.num_bits);

        self.ahash.get_mut(key)
    }

    /// Return an Option mutable reference for a memory square given a state,
    /// or None if not found.
    pub fn memory_find_mut(&mut self, key: &SomeState) -> Option<&mut SomeSquare> {
        debug_assert!(key.num_bits() == self.num_bits);

        self.memory.iter_mut().find(|sqrx| sqrx.state == *key)
    }

    /// Find a square that is expected to exist.
    pub fn find_mut_must(&mut self, val: &SomeState) -> &mut SomeSquare {
        debug_assert!(val.num_bits() == self.num_bits);

        if let Some(sqrx) = self.ahash.get_mut(val) {
            return sqrx;
        }
        panic!("Square expected to be found is not found");
    }

    /// Return an Option immutable reference for a square given a state,
    /// or None if not found.
    pub fn find(&self, val: &SomeState) -> Option<&SomeSquare> {
        debug_assert!(val.num_bits() == self.num_bits);

        self.ahash.get(val)
    }

    /// Return true if there is a square with a given state in memory.
    pub fn memory_is_in(&self, key: &SomeState) -> bool {
        debug_assert_eq!(key.num_bits(), self.num_bits);

        for sqrx in self.memory.iter() {
            if sqrx.state == *key {
                return true;
            }
        }
        false
    }

    /// Return list of indicies of squares in memory that are in a given region.
    /// Indicies are in decscending order.
    pub fn memory_stas_in_reg(&self, regx: &SomeRegion) -> StateStore {
        debug_assert_eq!(regx.num_bits(), self.num_bits);

        let mut ret_stas = StateStore::new(vec![]);

        for sqrx in self.memory.iter() {
            if regx.is_superset_of(&sqrx.state) {
                ret_stas.push(sqrx.state.clone());
            }
        }
        ret_stas
    }

    /// Find a square that is expected to exist.
    pub fn find_must(&self, val: &SomeState) -> &SomeSquare {
        debug_assert!(val.num_bits() == self.num_bits);

        if let Some(sqrx) = self.ahash.get(val) {
            return sqrx;
        }
        panic!("Square expected to be found is not found");
    }

    /// Add a square that is not currently in the store.
    pub fn insert(&mut self, sqrx: SomeSquare) {
        debug_assert!(sqrx.num_bits() == self.num_bits);

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
        debug_assert!(regx.num_bits() == self.num_bits);

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
        debug_assert!(target_reg.num_bits() == self.num_bits);

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

    /// Add a square to memory, oldest first.
    pub fn add_square_to_memory(&mut self, sqrx: SomeSquare) {
        debug_assert_eq!(sqrx.num_bits(), self.num_bits);

        if self.find(&sqrx.state).is_some() {
            panic!(
                "add_square_to_memory: square for {} exists in hash",
                sqrx.state
            );
        }
        if self.memory.len() >= MAX_MEMORY {
            self.memory.pop_front();
        }
        self.memory.push_back(sqrx);
    }

    /// Update memory, if needed.
    pub fn update_memory(&mut self, asample: &SomeSample) {
        debug_assert_eq!(asample.num_bits(), self.num_bits);

        for sqrx in self.memory.iter_mut() {
            if sqrx.state == asample.initial {
                sqrx.add_sample(asample);
                return;
            }
        }
    }

    // Remove a square from memory.
    pub fn memory_remove(&mut self, key: &SomeState) -> Option<SomeSquare> {
        debug_assert_eq!(key.num_bits(), self.num_bits);

        let mut index: Option<usize> = None;
        for (inx, sqrx) in self.memory.iter_mut().enumerate() {
            if sqrx.state == *key {
                index = Some(inx);
                break;
            }
        }
        if let Some(inx) = index {
            self.memory.remove(inx)
        } else {
            None
        }
    }

    /// Move a square from memory to HashMap.
    pub fn remember(&mut self, key: &SomeState) {
        debug_assert_eq!(key.num_bits(), self.num_bits);

        if let Some(sqrx) = self.memory_remove(key) {
            self.insert(sqrx);
        } else {
            panic!("square {key} not found in memory");
        }
    }
} // end impl SquareStore
