#![allow(dead_code)]
//! A struct for a defining region.
use crate::state::SomeState;
use crate::region::SomeRegion;
use crate::vertex::SomeVertex;

use std::fmt;
use serde::{Deserialize, Serialize};
use crate::StrLen;

/// Implement the fmt::Display Trait for a Defining instance.
impl fmt::Display for Defining {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }   
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Defining {
    pub region: SomeRegion,
    pub vertex: SomeVertex
}

impl Defining {
    // Return a Defining instance.
    pub fn new (region: SomeRegion, vertex: SomeVertex) -> Self {
      assert!(region == vertex.defines());

      Self { region, vertex }
    }

    /// Return a string used to represent a Defining instance.
    fn formatted_str(&self) -> String {
        format!("({} - {})", self.region, self.vertex)
    }

    /// Return true if a sampled state is used in a defining instance.
    pub fn state_needed(&self, stax: &SomeState) -> bool {
        if stax == &self.vertex.pinnacle {
            return true;
        }
        self.vertex.edges.contains(stax)
    }
}

/// Implement the trait StrLen for Defining.
impl StrLen for Defining {
    fn strlen(&self) -> usize {
        let mut ret = 2; // [..]
        ret += self.region.strlen();
        ret += 3; // " - "
        ret += self.vertex.strlen();

        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::SomeState;
    use crate::statestore::StateStore;
    use std::str::FromStr;

    #[test]
    fn test_stuff()  -> Result<(), String> {
        // Create a vertex with two edges.
        let pinnacle = SomeState::from_str("s0101")?;
        let sta1 = SomeState::from_str("s0111")?;
        let sta2 = SomeState::from_str("s1101")?;
        let edges = StateStore::new(vec![sta1, sta2]);
        let vtx1 = SomeVertex::new(pinnacle, edges);

        let defx = Defining::new(SomeRegion::new(vec![SomeState::from_str("s0000")?, SomeState::from_str("s0101")?]), vtx1);
        println!("{}", defx);
        //assert!(1 == 2);
        Ok(())
    }
}
