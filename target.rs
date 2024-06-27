//! Implement an enum to indicate a desired target of state, region, or domain regions.

use crate::region::SomeRegion;
use crate::regionstorecorr::RegionStoreCorr;
use crate::state::SomeState;

use std::fmt;

impl fmt::Display for ATarget<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Target :{}", &self.formatted_string())
    }
}

pub enum ATarget<'a> {
    State { state: &'a SomeState },
    Region { region: &'a SomeRegion },
    DomainRegions { regions: &'a RegionStoreCorr },
}

impl ATarget<'_> {
    /// Return a String representation.
    fn formatted_string(&self) -> String {
        match self {
            Self::State { state } => format!("{}", state),
            Self::Region { region } => format!("{}", region),
            Self::DomainRegions { regions } => format!("{}", regions),
        }
    }
}
