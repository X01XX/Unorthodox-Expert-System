//! The SomeNeed enum, representing needs for samples, or housekeeping tasks.
//!
//! Something is logically needed, like:
//! More samples of a square(or state, or bit pattern),
//! A sample in a region that has contradictory predictions.
//! Samples to limit a group.
//! Housekeeping needs, like adding a group.

use crate::region::SomeRegion;
use crate::regionstorecorr::RegionStoreCorr;
use crate::rulestore::RuleStore;
use crate::state::SomeState;
use crate::target::SomeTarget;
use crate::targetstore::TargetStore;

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeNeed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[derive(Debug, Serialize, Deserialize)]
/// Enums that represent a number of different needs.
pub enum SomeNeed {
    /// Seek a sample in a region that contains no samples.
    SampleInRegion {
        dom_id: usize,
        act_id: usize,
        target_region: SomeRegion,
        priority: usize,
    },
    /// Seek a sample in a region that other groups do not cover.
    StateInRemainder {
        dom_id: usize,
        act_id: usize,
        target_region: SomeRegion,
        priority: usize,
    },
    /// Sample a state that is not in a group.
    StateNotInGroup {
        dom_id: usize,
        act_id: usize,
        target_state: SomeState,
        priority: usize,
    },
    /// Get an additional sample of a state, to confirm a group.
    ConfirmGroup {
        dom_id: usize,
        act_id: usize,
        target_state: SomeState,
        priority: usize,

        grp_reg: SomeRegion,
    },
    /// Sample a state to resolve a contradictory intersection of two groups.
    ContradictoryIntersection {
        dom_id: usize,
        act_id: usize,
        target_region: SomeRegion,
        priority: usize,

        group1: SomeRegion,
        ruls1: Option<RuleStore>,
        group2: SomeRegion,
        ruls2: Option<RuleStore>,
    },
    /// Sample a state to limit a group.
    LimitGroup {
        dom_id: usize,
        act_id: usize,
        target_state: SomeState,
        priority: usize,

        for_group: SomeRegion,
        anchor: SomeState,
    },
    /// Sample a external state adjacent to a group anchor, to limit the group.
    LimitGroupAdj {
        dom_id: usize,
        act_id: usize,
        target_state: SomeState,
        priority: usize,
        for_group: SomeRegion,
        anchor: SomeState,
    },
    /// Sample a internal state adjacent to a group anchor, to confirm the group.
    ConfirmGroupAdj {
        dom_id: usize,
        act_id: usize,
        target_state: SomeState,
        priority: usize,
        for_group: SomeRegion,
        anchor: SomeState,
    },
    /// Move all current domain states from the corresponding regions of a SelectRegion.
    ExitSelectRegion {
        target_regions: RegionStoreCorr,
        priority: usize,
    },
    /// Move all current domain states to the corresponding regions of an SelectRegion.
    ToSelectRegion {
        target_regions: RegionStoreCorr,
        priority: usize,
        times_visited: usize,
        value: usize,
    },
    /// Housekeeping, add a group.
    AddGroup {
        group_region: SomeRegion,
        rules: Option<RuleStore>,
    },
}

impl SomeNeed {
    /// Return a need name string.
    pub fn name(&self) -> &str {
        match self {
            Self::AddGroup { .. } => "AddGroup",
            Self::ConfirmGroup { .. } => "ConfirmGroup",
            Self::ContradictoryIntersection { .. } => "ContradictoryIntersection",
            Self::LimitGroup { .. } => "LimitGroup",
            Self::LimitGroupAdj { .. } => "LimitGroupAdj",
            Self::ConfirmGroupAdj { .. } => "ConfirmGroupAdj",
            Self::StateInRemainder { .. } => "StateInRemainder",
            Self::StateNotInGroup { .. } => "StateNotInGroup",
            Self::SampleInRegion { .. } => "SampleInRegion",
            Self::ToSelectRegion { .. } => "ToSelectRegion",
            Self::ExitSelectRegion { .. } => "ExitSelectRegion",
        }
    }

    /// Set priority number for a need.  Lower is more important.
    /// Don't use number zero!
    pub fn set_priority(&mut self) {
        match self {
            // By ascending priority number.
            Self::ContradictoryIntersection { priority, .. } => *priority += 200,
            Self::ExitSelectRegion { priority, .. } => *priority += 300,
            Self::ConfirmGroup { priority, .. } => *priority += 400,
            Self::LimitGroup { priority, .. } => *priority += 500,
            Self::LimitGroupAdj { priority, .. } => *priority += 600,
            Self::ConfirmGroupAdj { priority, .. } => *priority += 700,
            Self::StateNotInGroup { priority, .. } => *priority += 800,
            Self::SampleInRegion { priority, .. } => *priority += 850,
            Self::ToSelectRegion { priority, .. } => *priority += 900,
            // Some needs should have a higher priority number compared to ToSelectRegion.
            Self::StateInRemainder { priority, .. } => *priority = 1000,
            _ => panic!(
                "SomeNeed::priority should not be called for the {} need.",
                self.name()
            ),
        } // end match ndx
    } // end caLc_priority

    /// Return a need priority.
    pub fn priority(&self) -> usize {
        match self {
            // By ascending priority number.
            Self::ContradictoryIntersection { priority, .. } => *priority,
            Self::ExitSelectRegion { priority, .. } => *priority,
            Self::ConfirmGroup { priority, .. } => *priority,
            Self::LimitGroup { priority, .. } => *priority,
            Self::LimitGroupAdj { priority, .. } => *priority,
            Self::ConfirmGroupAdj { priority, .. } => *priority,
            Self::StateNotInGroup { priority, .. } => *priority,
            Self::SampleInRegion { priority, .. } => *priority,
            Self::ToSelectRegion { priority, .. } => *priority,
            // Some needs should have a higher priority number compared to ToSelectRegion.
            Self::StateInRemainder { priority, .. } => *priority,
            _ => panic!(
                "SomeNeed::priority should not be called for the {} need.",
                self.name()
            ),
        } // end match ndx
    } // end priority

    /// Return true if a state satisfies a need.
    pub fn satisfied_by(&self, cur_state: &SomeState) -> bool {
        match self {
            Self::ConfirmGroup { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            Self::ContradictoryIntersection { target_region, .. } => {
                if target_region.is_superset_of(cur_state) {
                    return true;
                }
            }
            Self::LimitGroup { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            Self::LimitGroupAdj { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            Self::ConfirmGroupAdj { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            Self::StateInRemainder { target_region, .. } => {
                if target_region.is_superset_of(cur_state) {
                    return true;
                }
            }
            Self::StateNotInGroup { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            Self::SampleInRegion { target_region, .. } => {
                if target_region.is_superset_of(cur_state) {
                    return true;
                }
            }
            _ => panic!(
                "SomeNeed::satisfied_by should not be called for the {} need.",
                self.name()
            ),
        } //end match self
        false
    } // end satisfied_by

    /// Return need action number.
    pub fn act_id(&self) -> usize {
        match self {
            Self::ConfirmGroup { act_id, .. } => *act_id,
            Self::ContradictoryIntersection { act_id, .. } => *act_id,
            Self::LimitGroup { act_id, .. } => *act_id,
            Self::LimitGroupAdj { act_id, .. } => *act_id,
            Self::ConfirmGroupAdj { act_id, .. } => *act_id,
            Self::StateNotInGroup { act_id, .. } => *act_id,
            Self::SampleInRegion { act_id, .. } => *act_id,
            Self::StateInRemainder { act_id, .. } => *act_id,
            _ => panic!(
                "SomeNeed::act_id should not be called for the {} need.",
                self.name()
            ),
        } //end match self
    } // end act_id

    /// Return need domain number.
    pub fn dom_id(&self) -> usize {
        match self {
            Self::ConfirmGroup { dom_id, .. } => *dom_id,
            Self::ContradictoryIntersection { dom_id, .. } => *dom_id,
            Self::LimitGroup { dom_id, .. } => *dom_id,
            Self::LimitGroupAdj { dom_id, .. } => *dom_id,
            Self::ConfirmGroupAdj { dom_id, .. } => *dom_id,
            Self::StateInRemainder { dom_id, .. } => *dom_id,
            Self::StateNotInGroup { dom_id, .. } => *dom_id,
            Self::SampleInRegion { dom_id, .. } => *dom_id,
            _ => panic!(
                "SomeNeed::dom_id should not be called for the {} need.",
                self.name()
            ),
        } //end match self
    } // end dom_id

    /// Return a region for a need target.
    pub fn target(&self) -> TargetStore {
        match self {
            Self::ConfirmGroup {
                dom_id,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_id,
                SomeRegion::new(vec![target_state.clone()]),
            )]),
            Self::ContradictoryIntersection {
                dom_id,
                target_region,
                ..
            } => TargetStore::new(vec![SomeTarget::new(*dom_id, target_region.clone())]),
            Self::LimitGroup {
                dom_id,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_id,
                SomeRegion::new(vec![target_state.clone()]),
            )]),
            Self::LimitGroupAdj {
                dom_id,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_id,
                SomeRegion::new(vec![target_state.clone()]),
            )]),
            Self::ConfirmGroupAdj {
                dom_id,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_id,
                SomeRegion::new(vec![target_state.clone()]),
            )]),
            Self::StateInRemainder {
                dom_id,
                target_region,
                ..
            } => TargetStore::new(vec![SomeTarget::new(*dom_id, target_region.clone())]),
            Self::StateNotInGroup {
                dom_id,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_id,
                SomeRegion::new(vec![target_state.clone()]),
            )]),
            Self::SampleInRegion {
                dom_id,
                target_region,
                ..
            } => TargetStore::new(vec![SomeTarget::new(*dom_id, target_region.clone())]),
            Self::ToSelectRegion { target_regions, .. } => {
                let mut targ = TargetStore::with_capacity(target_regions.len());
                for (dom_idx, targx) in target_regions.iter().enumerate() {
                    targ.push(SomeTarget::new(dom_idx, targx.clone()));
                }
                targ
            }
            Self::ExitSelectRegion { target_regions, .. } => {
                let mut targ = TargetStore::with_capacity(target_regions.len());
                for (dom_idx, targx) in target_regions.iter().enumerate() {
                    targ.push(SomeTarget::new(dom_idx, targx.clone()));
                }
                targ
            }
            _ => panic!(
                "SomeNeed::target should not be called for the {} need.",
                self.name()
            ),
        }
    } // end target

    /// Return a String representation of SomeNeed.
    fn formatted_string(&self) -> String {
        match self {
            Self::AddGroup {
                group_region,
                rules,
            } => {
                if let Some(xrules) = rules {
                    format!("N(Create group from {group_region} {xrules})")
                } else {
                    format!("N(Create group from {group_region} No rules)")
                }
            }
            Self::ConfirmGroup {
                dom_id,
                act_id,
                target_state,
                grp_reg,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Get additional sample of state {target_state} to confirm group {grp_reg})")
            }
            Self::ContradictoryIntersection {
                dom_id,
                act_id,
                target_region,
                group1,
                ruls1,
                group2,
                ruls2,
                priority,
            } => {
                let ruls1_str = if let Some(rules) = ruls1 {
                    format!("{rules}")
                } else {
                    String::from("None")
                };
                let ruls2_str = if let Some(rules) = ruls2 {
                    format!("{rules}")
                } else {
                    String::from("None")
                };
                format!(
                "N(Dom {dom_id} Act {act_id} Pri {priority} Sample Region {target_region} intersection of {group1} {ruls1_str} and {group2} {ruls2_str})")
            }
            Self::LimitGroup {
                dom_id,
                act_id,
                target_state,
                for_group,
                anchor,
                priority,
                ..
            } => {
                if target_state == anchor {
                    format!(
                        "N(Dom {dom_id} Act {act_id} Pri {priority} Sample anchor State {anchor}, to limit group {for_group})")
                } else {
                    format!(
                        "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target_state}, far from anchor {anchor} to define group {for_group})")
                }
            }
            Self::LimitGroupAdj {
                dom_id,
                act_id,
                target_state,
                for_group,
                anchor,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target_state}, adj to {anchor} to limit group {for_group})")
            }
            Self::ConfirmGroupAdj {
                dom_id,
                act_id,
                target_state,
                for_group,
                anchor,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target_state}, adj to {anchor} to confirm group {for_group})")
            }
            Self::StateInRemainder {
                dom_id,
                act_id,
                target_region,
                priority,
            } => {
                format!(
                "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target_region} in remainder)")
            }
            Self::StateNotInGroup {
                dom_id,
                act_id,
                target_state,
                priority,
            } => {
                format!(
                "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target_state} not in a group)")
            }
            Self::SampleInRegion {
                dom_id,
                act_id,
                target_region,
                priority,
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State in {target_region})"
                )
            }
            Self::ToSelectRegion {
                target_regions,
                priority,
                times_visited,
                value,
            } => {
                format!("N(Pri {priority} To Select Regions {target_regions}, value {value}, times visited: {times_visited})")
            }
            Self::ExitSelectRegion {
                target_regions,
                priority,
            } => {
                format!("N(Pri {priority} Exit Select Regions to {target_regions})")
            }
        } // end match
    }
} // End impl SomeNeed
