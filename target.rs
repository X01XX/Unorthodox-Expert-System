use crate::region::AccessStates;
/// Implement a struct to indicate a desired region for a given domain.
use crate::region::SomeRegion;

use std::fmt;

impl fmt::Display for SomeTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "T:{}{}", &self.dom_num, &self.region)
    }
}

#[readonly::make]
#[derive(Debug, Clone)]
pub struct SomeTarget {
    /// Domain indicator
    pub dom_num: usize,
    /// A region to seek.
    pub region: SomeRegion,
}

impl SomeTarget {
    /// Return a new target.
    pub fn new(dom_num: usize, regx: SomeRegion) -> Self {
        Self {
            dom_num,
            region: regx,
        }
    }

    pub fn is_superset_of(&self, stax: &impl AccessStates) -> bool {
        self.region.is_superset_of(stax)
    }
}
