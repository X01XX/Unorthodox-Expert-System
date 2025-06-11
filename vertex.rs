#![allow(dead_code)]
//! The vertex struct, a state in only one region (external dependency), with adjacent dissimilar states.

use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;

use serde::{Deserialize, Serialize};
use std::fmt;
use crate::StrLen;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]

pub struct SomeVertex {
    pub pinnacle: SomeState,
    pub edges:    StateStore
}

/// Implement the fmt::Display Trait for a SomeVertex instance.
impl fmt::Display for SomeVertex {                                                                                                                
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }   
}

impl SomeVertex {
    /// Return a new SomeVertex instance.
    pub fn new (pinnacle: SomeState, edges: StateStore) -> Self {
        // Check args
        //assert!(edges.is_not_empty());
        for stax in edges.iter() {
            assert!(stax.is_adjacent(&pinnacle))
        }

        // Return result.
        Self { pinnacle, edges }
    }

    /// Return a string used to represent a vertex.
    fn formatted_str(&self) -> String {
        format!("({} - {})", self.pinnacle, self.edges)
    }

    /// Return the structure implied by a vertex.
    pub fn structure_implied(&self) -> RegionStore {

        let max_reg = SomeRegion::new(vec![self.pinnacle.new_high(), self.pinnacle.new_low()]);

        let mut and_complements_adjacent = RegionStore::new(vec![max_reg.clone()]);

        let complement_pin = max_reg.subtract(&self.pinnacle);

        for stax in self.edges.iter() {
            and_complements_adjacent = and_complements_adjacent.intersection(&max_reg.subtract(stax));
        }
        complement_pin.union(&and_complements_adjacent)
    }

    /// Return region a vertex defines.
    pub fn defines(&self) -> SomeRegion {

        let mut ret = SomeRegion::new(vec![self.pinnacle.new_high(), self.pinnacle.new_low()]);

        for stax in self.edges.iter() {
            let regx = SomeRegion::new(vec![self.pinnacle.clone(), self.pinnacle.bitwise_xor(&self.pinnacle.bitwise_xor(stax).bitwise_not()).as_state()]);
            ret = ret.intersection(&regx).unwrap();
        }

        ret
    }

    /// Return all states used in a vertex.
    pub fn states(&self) -> StateStore {
        self.edges.union(&StateStore::new(vec![self.pinnacle.clone()]))
    }

    /// Return true if a state is in a vertex.
    pub fn state_in(&self, stax: &SomeState) -> bool {
        if self.pinnacle == *stax {
            return true;
        }
        self.edges.contains(stax)
    }
}

/// Implement the trait StrLen for Vertex.
impl StrLen for SomeVertex {
    fn strlen(&self) -> usize {
        let mut ret = 2; // [..]
        ret += self.pinnacle.strlen();
        ret += 3; // " - "
        ret += self.edges.strlen();

        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_new_display_strlen() -> Result<(), String> {
        let pinnacle = SomeState::from_str("s0000")?;

        // Create a two-state store.
        let sta1 = SomeState::from_str("s0001")?;
        let sta2 = SomeState::from_str("s0010")?;
        let edges = StateStore::new(vec![sta1, sta2]);

        // Create a vertex.
        let vtx = SomeVertex::new(pinnacle, edges);
        assert!(format!("{}", vtx) == "(s0000 - [s0001, s0010])");
        assert!(format!("{}", vtx).len() == vtx.strlen());

        Ok(())
    }

    #[test]
    fn structure_implied() -> Result<(), String> {

        // Create a two-state store.
        let sta1 = SomeState::from_str("s0001")?;
        let sta2 = SomeState::from_str("s0010")?;
        let edges = StateStore::new(vec![sta1, sta2]);

        // Create a vertex.
        let pinnacle = SomeState::from_str("s0000")?;
        let vtx = SomeVertex::new(pinnacle, edges);

        // Find structue implied by vertex.
        let structure = vtx.structure_implied();
        println!("{}", structure);

        assert!(structure.len() == 5);
        assert!(structure.contains(&SomeRegion::from_str("r1XXX")?));
        assert!(structure.contains(&SomeRegion::from_str("rX1XX")?));
        assert!(structure.contains(&SomeRegion::from_str("rXX1X")?));
        assert!(structure.contains(&SomeRegion::from_str("rXXX1")?));
        assert!(structure.contains(&SomeRegion::from_str("rXX00")?));

        Ok(())
    }

     #[test]
    fn defines() -> Result<(), String> {

        // Create a two-state vertex.
        let pinnacle = SomeState::from_str("s0101")?;
        let sta1 = SomeState::from_str("s0111")?;
        let sta2 = SomeState::from_str("s1101")?;
        let edges = StateStore::new(vec![sta1, sta2]);
        let vtx = SomeVertex::new(pinnacle, edges);

        let regx = vtx.defines();
        println!("regx: {}", regx);
        assert!(regx == SomeRegion::from_str("r0X0X")?);

        Ok(())
    }
}
