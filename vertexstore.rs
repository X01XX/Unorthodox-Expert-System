#![allow(dead_code)]
//! The VertexStore struct, a vector of SomeVertex structs.
//!
use crate::vertex::SomeVertex;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::statestore::StateStore;
use crate::tools::StrLen;

use std::fmt;
use std::slice::Iter;

#[readonly::make]
#[derive(Debug, Clone, Default)]
pub struct VertexStore {
    items: Vec<SomeVertex>,
}

impl fmt::Display for VertexStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

impl VertexStore {
    /// Return a new, empty, VertexStore.
    pub fn new(items: Vec<SomeVertex>) -> Self {
        Self { items }
    }

    /// Return a new VertexStore instance, empty, with a specified capacity.
    pub fn with_capacity(num: usize) -> Self {
        Self {
            items: Vec::<SomeVertex>::with_capacity(num),
        }
    }

    /// Return the length of a VertexStore.
    /// Should be 0, 1 or 2.
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

    /// Add a vertex to a VertexStore.
    /// For vertexs of a group or square, there will be up to two vertexs. If two vertexs, they will have the same initial region.
    /// For a VertexsCorr instance, there will be vertexs corresponding to the domains in a DomainStore.
    pub fn push(&mut self, val: SomeVertex) {
        self.items.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeVertex> {
        self.items.iter()
    }

    /// Return a string representing a VertexStore.
    fn formatted_str(&self) -> String {
        let mut rc_str = String::with_capacity(self.strlen());
        rc_str.push('[');

        let mut first = true;
        for vrtx in self.items.iter() {
            if first {
                first = false;
            } else {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&vrtx.to_string());
        }
        rc_str.push(']');

        rc_str
    }

    /// Return the structure implied by a vertexstore.
    pub fn structure_implied(&self) -> RegionStore {
        assert!(self.is_not_empty());

        let mut ret = RegionStore::new(vec![SomeRegion::new(vec![self.items[0].pinnacle.new_high(), self.items[0].pinnacle.new_low()])]);

        for vrtx in self.items.iter() {
            ret = ret.intersection(&vrtx.structure_implied());
        }
        ret
    }

    /// Return a store of states used in a vertexstore.
    pub fn states(&self) -> StateStore {
        let mut ret = StateStore::new(vec![]);
        for vtx in self.items.iter() {
            if ret.contains(&vtx.pinnacle) {
            } else {
                ret.push(vtx.pinnacle.clone());
            }
            for stax in vtx.edges.iter() {
                if ret.contains(stax) {
                } else {
                    ret.push(stax.clone());
                }
            }
        }

        ret
    }
}

/// Implement the trait StrLen for VertexStore.
impl StrLen for VertexStore {
    fn strlen(&self) -> usize {
        let mut ret = 2; // [..]
        let mut first = true;
        for vrtx in self.items.iter() {
            if first {
                first = false;
            } else {
                ret += 2; // for ", "
            }
            ret += vrtx.strlen();
        }
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::SomeState;
    use std::str::FromStr;

    // Test structure_implied, with vertexstore new, display, strlen.
    #[test]
    fn structure_implied() -> Result<(), String> {
        // Create a vertex with two edges.
        let pinnacle = SomeState::from_str("s0101")?;
        let sta1 = SomeState::from_str("s0111")?;
        let sta2 = SomeState::from_str("s1101")?;
        let edges = StateStore::new(vec![sta1, sta2]);
        let vtx1 = SomeVertex::new(pinnacle, edges);

        // Create another vertex with two edges.
        let pinnacle = SomeState::from_str("s1111")?;
        let sta1 = SomeState::from_str("s0111")?;
        let sta2 = SomeState::from_str("s1101")?;
        let edges = StateStore::new(vec![sta1, sta2]);
        let vtx2 = SomeVertex::new(pinnacle, edges);

        // Create a vertexstore.
        let storex = VertexStore::new(vec![vtx1, vtx2]);

        // Calc structure.
        let structure = storex.structure_implied();
        println!("{}", structure);
        assert!(structure.len() == 6);
        assert!(structure.contains(&SomeRegion::from_str("r0X0X")?));
        assert!(structure.contains(&SomeRegion::from_str("r0X1X")?));
        assert!(structure.contains(&SomeRegion::from_str("r1X0X")?));
        assert!(structure.contains(&SomeRegion::from_str("r1X1X")?));
        assert!(structure.contains(&SomeRegion::from_str("rXXX0")?));
        assert!(structure.contains(&SomeRegion::from_str("rX0XX")?));

        assert!(format!("{}", structure).len() == structure.strlen());

        //assert!(1 == 2);
        Ok(())
    }
}
