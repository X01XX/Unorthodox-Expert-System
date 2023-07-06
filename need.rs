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
        let rc_str = match self {
            SomeNeed::AddGroup {
                group_region,
                rules,
            } => format!("N(Create group from {group_region} {rules})"),
            SomeNeed::AddSeekEdge { reg: regx } => format!("N(Add SeekEdge region: {})", &regx),
            SomeNeed::AStateMakeGroup {
                dom_num,
                act_num,
                target_state,
                for_reg,
                far,
                priority,
            } => {
                format!(
                "N(Dom {dom_num} Act {act_num} Pri {priority} Sample State {target_state} to, with {far}, make group {for_reg})")
            }
            SomeNeed::ConfirmGroup {
                dom_num,
                act_num,
                target_state,
                grp_reg,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_num} Act {act_num} Pri {priority} Get additional sample of state {target_state} to confirm group {grp_reg})")
            }
            SomeNeed::ContradictoryIntersection {
                dom_num,
                act_num,
                target_region,
                group1,
                ruls1,
                group2,
                ruls2,
                priority,
            } => {
                format!(
                "N(Dom {dom_num} Act {act_num} Pri {priority} Sample Region {target_region} intersection of {group1} {ruls1} and {group2} {ruls2})")
            }
            SomeNeed::InactivateSeekEdge { reg: regx } => {
                format!("N(Inactivate SeekEdge region: {})", &regx)
            }
            SomeNeed::LimitGroup {
                dom_num,
                act_num,
                target_state,
                for_group,
                anchor,
                priority,
                ..
            } => {
                if target_state == anchor {
                    format!(
                        "N(Dom {dom_num} Act {act_num} Pri {priority} Sample anchor State {anchor}, to limit group {for_group})")
                } else {
                    format!(
                        "N(Dom {dom_num} Act {act_num} Pri {priority} Sample State {target_state}, far from anchor {anchor} to define group {for_group})")
                }
            }
            SomeNeed::LimitGroupAdj {
                dom_num,
                act_num,
                target_state,
                for_group,
                anchor,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_num} Act {act_num} Pri {priority} Sample State {target_state}, adj to {anchor} to limit group {for_group})")
            }
            SomeNeed::RemoveGroupAnchor { group_region } => {
                format!("N(Remove anchor for group {group_region})")
            }
            SomeNeed::SeekEdge {
                dom_num,
                act_num,
                target_state,
                in_group,
                priority,
            } => {
                format!(
                    "N(Dom {} Act {} Pri {} Sample State {}, between {} and {} to seek edge)",
                    dom_num,
                    act_num,
                    priority,
                    target_state,
                    in_group.state1(),
                    in_group.state2()
                )
            }
            SomeNeed::SetGroupAnchor {
                group_region,
                anchor,
            } => format!("N(set group {group_region} anchor {anchor})"),
            SomeNeed::SetGroupLimited {
                group_region,
                num_adj,
            } => {
                format!("N(set group {group_region} limited, num adj {num_adj})")
            }
            SomeNeed::StateInRemainder {
                dom_num,
                act_num,
                target_region,
                priority,
            } => {
                format!(
                "N(Dom {dom_num} Act {act_num} Pri {priority} Sample State {target_region} in remainder)")
            }
            SomeNeed::StateNotInGroup {
                dom_num,
                act_num,
                target_state,
                priority,
            } => {
                format!(
                "N(Dom {dom_num} Act {act_num} Pri {priority} Sample State {target_state} not in a group)")
            }
            SomeNeed::ToSelectRegion {
                target_regions,
                priority,
            } => {
                format!("N(Pri {priority} To Select Regions {target_regions})")
            }
            SomeNeed::ExitSelectRegion {
                target_regions,
                priority,
            } => {
                format!("N(Pri {priority} Exit Select Regions to {target_regions})")
            }
        }; // end match

        write!(f, "{rc_str}")
    }
}

#[derive(Debug, Serialize, Deserialize)]
/// Enums that represent a number of different needs.
pub enum SomeNeed {
    /// Housekeeping, add a group.
    AddGroup {
        group_region: SomeRegion,
        rules: RuleStore,
    },
    /// Housekeeping, add a region to the seek_edge vector.
    AddSeekEdge { reg: SomeRegion },
    /// Sample a state as part of making a new group.
    AStateMakeGroup {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        for_reg: SomeRegion,
        far: SomeState,
        priority: usize,
    },
    /// Get an additional sample of a state.
    ConfirmGroup {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        grp_reg: SomeRegion,
        priority: usize,
    },
    /// Sample a state to resolve a contradictory intersection of two groups.
    ContradictoryIntersection {
        dom_num: usize,
        act_num: usize,
        target_region: SomeRegion,
        group1: SomeRegion,
        ruls1: RuleStore,
        group2: SomeRegion,
        ruls2: RuleStore,
        priority: usize,
    },
    /// Housekeeping, inactivate a region in the seek_edge vector.
    InactivateSeekEdge { reg: SomeRegion },
    /// Sample a state to limit a group.
    LimitGroup {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        for_group: SomeRegion,
        anchor: SomeState,
        priority: usize,
    },
    /// Sample an adjacent state to limit a group.
    LimitGroupAdj {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        for_group: SomeRegion,
        anchor: SomeState,
        priority: usize,
    },
    /// Housekeeping, Remove group anchor.
    RemoveGroupAnchor { group_region: SomeRegion },
    /// Sample a state to find a new edge in the total solution.
    SeekEdge {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        in_group: SomeRegion,
        priority: usize,
    },
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
        dom_num: usize,
        act_num: usize,
        target_region: SomeRegion,
        priority: usize,
    },
    /// Sample a state that is not in a group.
    StateNotInGroup {
        dom_num: usize,
        act_num: usize,
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
        target_regions: RegionStoreCorr,
        priority: usize,
    },
}

impl SomeNeed {
    /// Return a need name string.
    pub fn name(&self) -> &str {
        match self {
            SomeNeed::AddGroup { .. } => "AddGroup",
            SomeNeed::AddSeekEdge { .. } => "AddSeekEdge",
            SomeNeed::AStateMakeGroup { .. } => "AStateMakeGroup",
            SomeNeed::ConfirmGroup { .. } => "ConfirmGroup",
            SomeNeed::ContradictoryIntersection { .. } => "ContradictoryIntersection",
            SomeNeed::InactivateSeekEdge { .. } => "InactivateSeekEdge",
            SomeNeed::LimitGroup { .. } => "LimitGroup",
            SomeNeed::LimitGroupAdj { .. } => "LimitGroupAdj",
            SomeNeed::RemoveGroupAnchor { .. } => "RemoveGroupAnchor",
            SomeNeed::SeekEdge { .. } => "SeekEdge",
            SomeNeed::SetGroupAnchor { .. } => "SetGroupAnchor",
            SomeNeed::SetGroupLimited { .. } => "SetGroupLimited",
            SomeNeed::StateInRemainder { .. } => "StateInRemainder",
            SomeNeed::StateNotInGroup { .. } => "StateNotInGroup",
            SomeNeed::ToSelectRegion { .. } => "ToSelectRegion",
            SomeNeed::ExitSelectRegion { .. } => "ExitSelectRegion",
        }
    }

    /// Set priority number for a need.  Lower is more important.
    /// Don't use number zero!
    pub fn set_priority(&mut self) {
        match self {
            // By ascending priority number.
            SomeNeed::SeekEdge { priority, .. } => *priority += 100,
            SomeNeed::ContradictoryIntersection { priority, .. } => *priority += 200,
            SomeNeed::ExitSelectRegion { priority, .. } => *priority += 300,
            SomeNeed::AStateMakeGroup { priority, .. } => *priority += 400,
            SomeNeed::ConfirmGroup { priority, .. } => *priority += 500,
            SomeNeed::LimitGroup { priority, .. } => *priority += 500,
            SomeNeed::LimitGroupAdj { priority, .. } => *priority += 500,
            SomeNeed::StateNotInGroup { priority, .. } => *priority += 600,
            SomeNeed::ToSelectRegion { priority, .. } => *priority += 700,
            // Some needs should have a higher priority number compared to ToSelectRegion.
            SomeNeed::StateInRemainder { priority, .. } => *priority = 10000,
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
            SomeNeed::SeekEdge { priority, .. } => *priority,
            SomeNeed::ContradictoryIntersection { priority, .. } => *priority,
            SomeNeed::ExitSelectRegion { priority, .. } => *priority,
            SomeNeed::AStateMakeGroup { priority, .. } => *priority,
            SomeNeed::ConfirmGroup { priority, .. } => *priority,
            SomeNeed::LimitGroup { priority, .. } => *priority,
            SomeNeed::LimitGroupAdj { priority, .. } => *priority,
            SomeNeed::StateNotInGroup { priority, .. } => *priority,
            SomeNeed::ToSelectRegion { priority, .. } => *priority,
            // Some needs should have a higher priority number compared to ToSelectRegion.
            SomeNeed::StateInRemainder { priority, .. } => *priority,
            _ => panic!(
                "SomeNeed::priority should not be called for the {} need.",
                self.name()
            ),
        } // end match ndx
    } // end priority

    /// Return true if a state satisfies a need.
    pub fn satisfied_by(&self, cur_state: &SomeState) -> bool {
        match self {
            SomeNeed::AStateMakeGroup { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            SomeNeed::ConfirmGroup { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            SomeNeed::ContradictoryIntersection { target_region, .. } => {
                if target_region.is_superset_of_state(cur_state) {
                    return true;
                }
            }
            SomeNeed::LimitGroup { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            SomeNeed::LimitGroupAdj { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            SomeNeed::SeekEdge { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            SomeNeed::StateInRemainder { target_region, .. } => {
                if target_region.is_superset_of_state(cur_state) {
                    return true;
                }
            }
            SomeNeed::StateNotInGroup { target_state, .. } => {
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
    pub fn act_num(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup { act_num, .. } => *act_num,
            SomeNeed::ConfirmGroup { act_num, .. } => *act_num,
            SomeNeed::ContradictoryIntersection { act_num, .. } => *act_num,
            SomeNeed::LimitGroup { act_num, .. } => *act_num,
            SomeNeed::LimitGroupAdj { act_num, .. } => *act_num,
            SomeNeed::SeekEdge { act_num, .. } => *act_num,
            SomeNeed::StateNotInGroup { act_num, .. } => *act_num,
            SomeNeed::StateInRemainder { act_num, .. } => *act_num,
            _ => panic!(
                "SomeNeed::act_num should not be called for the {} need.",
                self.name()
            ),
        } //end match self
    } // end act_num

    /// Return need domain number.
    pub fn dom_num(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup { dom_num, .. } => *dom_num,
            SomeNeed::ConfirmGroup { dom_num, .. } => *dom_num,
            SomeNeed::ContradictoryIntersection { dom_num, .. } => *dom_num,
            SomeNeed::LimitGroup { dom_num, .. } => *dom_num,
            SomeNeed::LimitGroupAdj { dom_num, .. } => *dom_num,
            SomeNeed::SeekEdge { dom_num, .. } => *dom_num,
            SomeNeed::StateInRemainder { dom_num, .. } => *dom_num,
            SomeNeed::StateNotInGroup { dom_num, .. } => *dom_num,
            _ => panic!(
                "SomeNeed::dom_num should not be called for the {} need.",
                self.name()
            ),
        } //end match self
    } // end dom_num

    /// Return a region for a need target.
    pub fn target(&self) -> TargetStore {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )]),
            SomeNeed::ConfirmGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )]),
            SomeNeed::ContradictoryIntersection {
                dom_num,
                target_region,
                ..
            } => TargetStore::new(vec![SomeTarget::new(*dom_num, target_region.clone())]),
            SomeNeed::LimitGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )]),
            SomeNeed::LimitGroupAdj {
                dom_num,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )]),
            SomeNeed::SeekEdge {
                dom_num,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )]),
            SomeNeed::StateInRemainder {
                dom_num,
                target_region,
                ..
            } => TargetStore::new(vec![SomeTarget::new(*dom_num, target_region.clone())]),
            SomeNeed::StateNotInGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new(vec![SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )]),
            SomeNeed::ToSelectRegion { target_regions, .. } => {
                let mut targ = TargetStore::with_capacity(target_regions.len());
                for (dom_numx, targx) in target_regions.iter().enumerate() {
                    targ.push(SomeTarget::new(dom_numx, targx.clone()));
                }
                targ
            }
            SomeNeed::ExitSelectRegion { target_regions, .. } => {
                let mut targ = TargetStore::with_capacity(1);
                for (dom_numx, regx) in target_regions.iter().enumerate() {
                    targ.push(SomeTarget::new(dom_numx, regx.clone()));
                }
                targ
            }
            _ => panic!(
                "SomeNeed::target should not be called for the {} need.",
                self.name()
            ),
        }
    } // end target
}
