//! The StepsCorr struct, a store of SomeSteps, corresponding in order, to domains in a DomainStore instance.
//!
//! Each step will have a number of bits equal to the bits used by the corresponding
//! domain, not necessarily the same as other steps in the vector.

use crate::changescorr::ChangesCorr;
use crate::regionscorr::RegionsCorr;
use crate::rulescorr::RulesCorr;
use crate::step::SomeStep;
use crate::stepstore::StepStore;
use crate::tools::StrLen;

use std::fmt;
use std::slice::Iter;

impl fmt::Display for StepsCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "STC[]")
        } else {
            write!(f, "STC{}", self.steps)
        }
    }
}
impl PartialEq for StepsCorr {
    fn eq(&self, other: &Self) -> bool {
        debug_assert!(self.len() == other.len());

        for (stpx, stpy) in self.iter().zip(other.iter()) {
            if stpx.act_id != stpy.act_id {
                return false;
            }
            if stpx.initial != stpy.initial {
                return false;
            }
        }
        true
    }
}
impl Eq for StepsCorr {}

#[readonly::make]
#[derive(Debug, Clone)]
/// A vector of steps, corresponding to domains in a vector.
pub struct StepsCorr {
    pub steps: StepStore,
}

impl StepsCorr {
    /// Return a new StepsCorr instance, empty, with a specified capacity.
    pub fn with_capacity(cap: usize) -> Self {
        debug_assert!(cap > 0);
        Self {
            steps: StepStore::with_capacity(cap),
        }
    }

    /// Return the number of steps.
    pub fn len(&self) -> usize {
        self.steps.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.steps.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.steps.is_empty()
    }

    /// Add a step to the step store.
    pub fn push(&mut self, rulx: SomeStep) {
        self.steps.push(rulx);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeStep> {
        self.steps.iter()
    }

    /// Return a StepsCorrs' rules initial regions.
    pub fn initial_regions(&self) -> RegionsCorr {
        let mut ret = RegionsCorr::with_capacity(self.len());

        for stpx in self.iter() {
            ret.push(stpx.initial.clone());
        }
        ret
    }

    /// Return a StepsCorrs' rules result regions.
    pub fn result_regions(&self) -> RegionsCorr {
        let mut ret = RegionsCorr::with_capacity(self.len());

        for stpx in self.iter() {
            ret.push(stpx.result.clone());
        }
        ret
    }

    /// Return a StepsCorr with restricted initial regions.
    pub fn restrict_initial_regions(&self, regions: &RegionsCorr) -> Self {
        let mut ret = Self::with_capacity(self.len());

        for (stpx, regy) in self.iter().zip(regions.iter()) {
            ret.push(stpx.restrict_initial_region(regy));
        }
        ret
    }

    /// Return a StepsCorr with restricted result regions.
    pub fn restrict_result_regions(&self, regions: &RegionsCorr) -> Self {
        let mut ret = Self::with_capacity(self.len());

        for (stpx, regy) in self.iter().zip(regions.iter()) {
            ret.push(stpx.restrict_result_region(regy));
        }
        ret
    }

    /// Return changes made by a StepsCorr.
    pub fn changes(&self) -> ChangesCorr {
        let mut ret = ChangesCorr::with_capacity(self.len());

        for stpx in self.iter() {
            ret.push(stpx.rule.as_change());
        }
        ret
    }

    /// Return rules used by StepsCorr.
    pub fn rules(&self) -> RulesCorr {
        let mut ret = RulesCorr::with_capacity(self.len());

        for stpx in self.iter() {
            ret.push(stpx.rule.clone());
        }
        ret
    }

    /// Return true if a StepsCorr initial regions intersect a given regionsCorr.
    pub fn initial_regions_intersect(&self, rcx: &RegionsCorr) -> bool {
        for (stpx, regx) in self.iter().zip(rcx.iter()) {
            if stpx.initial.intersects(regx) {
            } else {
                return false;
            }
        }
        true
    }

    /// Return true if a StepsCorr result regions intersect a given regionsCorr.
    pub fn result_regions_intersect(&self, rcx: &RegionsCorr) -> bool {
        for (stpx, regx) in self.iter().zip(rcx.iter()) {
            if stpx.result.intersects(regx) {
            } else {
                return false;
            }
        }
        true
    }
}

impl StrLen for StepsCorr {
    fn strlen(&self) -> usize {
        let mut rc_len = 3; // for "STC"

        rc_len += self.steps.strlen();

        rc_len
    }
}
