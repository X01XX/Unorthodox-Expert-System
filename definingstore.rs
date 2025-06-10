#![allow(dead_code)]

use crate::state::SomeState;
use crate::defining::Defining;
use crate::tools;

use std::fmt;
use serde::{Deserialize, Serialize};
use std::slice::Iter;

impl fmt::Display for DefiningStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.items))
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct DefiningStore {
    /// A vector of regions.
    pub items: Vec<Defining>,
}

impl DefiningStore {
    /// Return a new, RegionStore.
    pub fn new(items: Vec<Defining>) -> Self {
        Self { items }
    }

    /// Return the number of regions.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<Defining> {
        self.items.iter()
    }

    /// Return true if a state is needed in any defining instance in the store.
    pub fn state_needed(&self, stax: &SomeState) -> bool {
        for vtx in self.items.iter() {
            if vtx.state_needed(stax) {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::SomeState;
    use crate::statestore::StateStore;
    use crate::vertex::SomeVertex;
    use crate::region::SomeRegion;
    use std::str::FromStr;

    #[test]
    fn test_new() -> Result<(), String> {
        // Create an empty store.
        let defstr = DefiningStore::new(vec![]);
        assert!(defstr.is_empty());
        println!("{}", defstr);

        // Create a vertex with two edges.
        let pinnacle = SomeState::from_str("s0101")?;
        let sta1 = SomeState::from_str("s0111")?;
        let sta2 = SomeState::from_str("s1101")?;
        let edges = StateStore::new(vec![sta1, sta2]);
        let vtx1 = SomeVertex::new(pinnacle, edges);
        let defx = Defining::new(SomeRegion::new(vec![SomeState::from_str("s0000")?, SomeState::from_str("s0101")?]), vtx1);

        // Create a store with one item.
        let defstr = DefiningStore::new(vec![defx]);
        assert!(defstr.is_not_empty());
        println!("{}", defstr);

        //assert!(1 == 2);
        Ok(())
    }
}

