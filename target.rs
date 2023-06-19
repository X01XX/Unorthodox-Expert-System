/// Implement a struct to indicate a desired region for a given domain.
use crate::region::SomeRegion;
use crate::state::SomeState;

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

    pub fn is_superset_of_state(&self, stax: &SomeState) -> bool {
        self.region.is_superset_of_state(stax)
    }

    /// Return a string representing a vector of SomeTarget references.
    pub fn vec_ref_string(avec: &[&SomeTarget]) -> String {
        let mut rc_str = String::new();
        rc_str.push('[');

        for (inx, targx) in avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", targx));
        }

        rc_str.push(']');

        rc_str
    }
}
