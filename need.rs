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
    /// Housekeeping, add a group.
    AddGroup {
        group_region: SomeRegion,
        rules: Option<RuleStore>,
    },
    /// Get an additional sample of a state.
    ConfirmGroup {
        dom_id: usize,
        act_id: usize,
        target_state: SomeState,
        grp_reg: SomeRegion,
        priority: usize,
    },
    /// Sample a state to resolve a contradictory intersection of two groups.
    ContradictoryIntersection {
        dom_id: usize,
        act_id: usize,
        target_region: SomeRegion,
        group1: SomeRegion,
        ruls1: Option<RuleStore>,
        group2: SomeRegion,
        ruls2: Option<RuleStore>,
        priority: usize,
    },
    /// Sample a state to limit a group.
    LimitGroup {
        dom_id: usize,
        act_id: usize,
        target_state: SomeState,
        for_group: SomeRegion,
        anchor: SomeState,
        priority: usize,
    },
    /// Sample an adjacent state to limit a group.
    LimitGroupAdj {
        dom_id: usize,
        act_id: usize,
        target_state: SomeState,
        for_group: SomeRegion,
        anchor: SomeState,
        priority: usize,
    },
    /// Housekeeping, Remove group anchor.
    RemoveGroupAnchor { group_region: SomeRegion },
    /// Housekeeping, set the anchor for a group.
    SetGroupAnchor {
        group_region: SomeRegion,
        anchor: SomeState,
    },
    /// Housekeeping, set a group to limited, using a state that is only in that group,
    /// and has adjacent, external, dissimilar squares on all group region edges.
    SetGroupLimited {
        group_region: SomeRegion,
        num_adj: usize,
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
    /// Move all current domain states to the corresponding regions of an SelectRegion.
    ToSelectRegion {
        target_regions: RegionStoreCorr,
        priority: usize,
    },
    /// Move all current domain states from the corresponding regions of an OptmalRegion.
    ExitSelectRegion {
        dom_id: usize,
        target_region: SomeRegion,
        priority: usize,
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
            Self::RemoveGroupAnchor { .. } => "RemoveGroupAnchor",
            Self::SetGroupAnchor { .. } => "SetGroupAnchor",
            Self::SetGroupLimited { .. } => "SetGroupLimited",
            Self::StateInRemainder { .. } => "StateInRemainder",
            Self::StateNotInGroup { .. } => "StateNotInGroup",
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
            Self::ConfirmGroup { priority, .. } => *priority += 500,
            Self::LimitGroup { priority, .. } => *priority += 600,
            Self::LimitGroupAdj { priority, .. } => *priority += 700,
            Self::StateNotInGroup { priority, .. } => *priority += 800,
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
            Self::StateNotInGroup { priority, .. } => *priority,
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
            Self::StateNotInGroup { act_id, .. } => *act_id,
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
            Self::StateInRemainder { dom_id, .. } => *dom_id,
            Self::StateNotInGroup { dom_id, .. } => *dom_id,
            Self::ExitSelectRegion { dom_id, .. } => *dom_id,
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
            Self::ToSelectRegion { target_regions, .. } => {
                let mut targ = TargetStore::with_capacity(target_regions.len());
                for (dom_idx, targx) in target_regions.iter().enumerate() {
                    targ.push(SomeTarget::new(dom_idx, targx.clone()));
                }
                targ
            }
            Self::ExitSelectRegion {
                dom_id,
                target_region,
                ..
            } => {
                let mut targ = TargetStore::with_capacity(1);
                targ.push(SomeTarget::new(*dom_id, target_region.clone()));
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
                    format!("N(Create group from {group_region} None)")
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
            Self::RemoveGroupAnchor { group_region } => {
                format!("N(Remove anchor for group {group_region})")
            }
            Self::SetGroupAnchor {
                group_region,
                anchor,
            } => format!("N(set group {group_region} anchor {anchor})"),
            Self::SetGroupLimited {
                group_region,
                num_adj,
            } => {
                format!("N(set group {group_region} limited, num adj {num_adj})")
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
            Self::ToSelectRegion {
                target_regions,
                priority,
            } => {
                format!("N(Pri {priority} To Select Regions {target_regions})")
            }
            Self::ExitSelectRegion {
                dom_id,
                target_region,
                priority,
            } => {
                format!("N(Pri {priority} Exit Select Regions to domain {dom_id} {target_region})")
            }
        } // end match
    }
} // End impl SomeNeed
