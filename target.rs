//! Implement an enum to indicate a desired target of state, region, or domain regions.

use crate::region::SomeRegion;
use crate::regionscorr::RegionsCorr;
use crate::selectregions::SelectRegions;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for ATarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.formatted_str())
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum ATarget {
    State { state: SomeState },
    Region { region: SomeRegion },
    DomainRegions { regions: RegionsCorr },
    SelectRegions { select: SelectRegions },
}

impl ATarget {
    /// Return a String representation.
    fn formatted_str(&self) -> String {
        match self {
            Self::State { state } => format!("{state}"),
            Self::Region { region } => format!("{region}"),
            Self::DomainRegions { regions } => format!("{regions}"),
            Self::SelectRegions { select } => format!("{select}"),
        }
    }
}
