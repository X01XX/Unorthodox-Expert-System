use crate::region::AccessStates;
/// Implement a struct to indicate a desired region for a given domain.
use crate::region::SomeRegion;

use std::fmt;

impl fmt::Display for SomeTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "T:{}{}", &self.dom_id, &self.region)
    }
}

#[readonly::make]
#[derive(Debug, Clone)]
pub struct SomeTarget {
    /// Domain indicator
    pub dom_id: usize,
    /// A region to seek.
    pub region: SomeRegion,
}

impl SomeTarget {
    /// Return a new target.
    pub fn new(dom_id: usize, regx: SomeRegion) -> Self {
        Self {
            dom_id,
            region: regx,
        }
    }

    pub fn is_superset_of(&self, stax: &impl AccessStates) -> bool {
        self.region.is_superset_of(stax)
    }
}
