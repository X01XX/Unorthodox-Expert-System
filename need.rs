//! The SomeNeed enum, representing needs for samples, or housekeeping tasks.
//!
//! Something is logically needed, like:
//! More samples of a square(or state, or bit pattern),
//! A sample in a region that has contradictory predictions.
//! Samples to limit a group.
//! Housekeeping needs, like adding a group.

use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::rulestore::RuleStore;
use crate::state::SomeState;
use crate::target::SomeTarget;
use crate::targetstore::TargetStore;

use std::fmt;

impl fmt::Display for SomeNeed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pri = self.priority();
        let rc_str = match self {
            SomeNeed::AStateMakeGroup {
                dom_num,
                act_num,
                target_state,
                for_reg,
                far,
                num_x,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample State {}, far from {}, to make group {} nx: {})",
                dom_num, act_num, pri, target_state, far, for_reg, num_x
            ),
            SomeNeed::StateNotInGroup {
                dom_num,
                act_num,
                target_state,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample State {} not in a group)",
                dom_num, act_num, pri, target_state
            ),
            SomeNeed::ContradictoryIntersection {
                dom_num,
                act_num,
                target_region,
                group1,
                ruls1,
                group2,
                ruls2,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample Region {} intersection of {} {} and {} {})",
                dom_num, act_num, pri, target_region, group1, ruls1, group2, ruls2
            ),
            SomeNeed::ToRegion {
                dom_num,
                target_region,
                ..
            } => format!(
                "N(Dom {} Pri {} To Optimal Region {})",
                dom_num, pri, target_region,
            ),
            SomeNeed::ToOptimalRegion { target_regions } => {
                format!("N(Pri {} To Optimal Regions {})", pri, target_regions,)
            }
            SomeNeed::LimitGroup {
                dom_num,
                act_num,
                target_state,
                for_group,
                anchor,
                group_num,
            } => {
                if target_state == anchor {
                    format!(
                        "N(Dom {} Act {} Pri {} Sample anchor State {}, to limit group {} gn {})",
                        dom_num, act_num, pri, anchor, for_group, group_num,
                    )
                } else {
                    format!(
                        "N(Dom {} Act {} Pri {} Sample State {}, far from {} to limit group {} gn {})",
                        dom_num, act_num, pri, target_state, anchor, for_group, group_num,
                    )
                }
            }
            SomeNeed::LimitGroupAdj {
                dom_num,
                act_num,
                target_state,
                for_group,
                anchor,
                group_num,
            } => {
                format!(
                    "N(Dom {} Act {} Pri {} Sample State {}, adj to {} to limit group {} gn {})",
                    dom_num, act_num, pri, target_state, anchor, for_group, group_num,
                )
            }
            SomeNeed::ConfirmGroup {
                dom_num,
                act_num,
                target_state,
                grp_reg,
                group_num,
            } => {
                format!(
                    "N(Dom {} Act {} Pri {} Get additional sample of state {} to confirm group {} gn {})",
                    dom_num, act_num, pri, target_state, grp_reg, group_num,
                )
            }

            SomeNeed::SeekEdge {
                dom_num,
                act_num,
                target_state,
                in_group,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample State {}, between {} and {} to seek edge)",
                dom_num, act_num, pri, target_state, in_group.state1, in_group.state2
            ),
            SomeNeed::AddGroup {
                group_region,
                rules,
            } => format!("N(Create group from {} {})", group_region, rules),
            SomeNeed::RemoveGroupAnchor { group_region } => {
                format!("N(Remove anchor for group {})", group_region)
            }
            SomeNeed::SetGroupLimited { group_region } => {
                format!("N(set group {} limited)", group_region)
            }
            SomeNeed::SetGroupAnchor {
                group_region,
                anchor,
            } => format!("N(set group {} anchor {})", group_region, anchor),
            SomeNeed::InactivateSeekEdge { reg: regx } => {
                format!("N(Inactivate SeekEdge region: {})", &regx)
            }
            SomeNeed::AddSeekEdge { reg: regx } => format!("N(Add SeekEdge region: {})", &regx),
        }; // end match

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
pub enum SomeNeed {
    /// Sample a state as part of making a new group.
    AStateMakeGroup {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        for_reg: SomeRegion,
        far: SomeState,
        num_x: usize,
    },
    /// Sample a state that is not in a group.
    StateNotInGroup {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
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
    },
    /// Sample a state to limit a group.
    LimitGroup {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        for_group: SomeRegion,
        anchor: SomeState,
        group_num: usize,
    },
    /// Sample an adjacent state to limit a group.
    LimitGroupAdj {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        for_group: SomeRegion,
        anchor: SomeState,
        group_num: usize,
    },
    /// Get an additional sample of a state.
    ConfirmGroup {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        grp_reg: SomeRegion,
        group_num: usize,
    },
    /// Sample a state to find a new edge in the total solution.
    SeekEdge {
        dom_num: usize,
        act_num: usize,
        target_state: SomeState,
        in_group: SomeRegion,
    },
    /// Move current state to a given region.
    ToRegion {
        dom_num: usize,
        act_num: usize,
        target_region: SomeRegion,
    },
    /// Move current state to given regions.
    ToOptimalRegion { target_regions: RegionStore },
    /// Housekeeping, add a group.
    AddGroup {
        group_region: SomeRegion,
        rules: RuleStore,
    },
    /// Housekeeping, Remove group anchor.
    RemoveGroupAnchor { group_region: SomeRegion },
    /// Housekeeping, set a group to limited, using a state
    /// that is only in that group, has adjacent, external, dissimilar squares.
    SetGroupLimited { group_region: SomeRegion },
    SetGroupAnchor {
        group_region: SomeRegion,
        anchor: SomeState,
    },
    /// Housekeeping, inactivate a region in the seek_edge vector.
    InactivateSeekEdge { reg: SomeRegion },
    /// Housekeeping, add a region to the seek_edge vector.
    AddSeekEdge { reg: SomeRegion },
}

impl PartialEq for SomeNeed {
    fn eq(&self, other: &Self) -> bool {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num,
                act_num,
                target_state,
                ..
            } => {
                if let SomeNeed::AStateMakeGroup {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    target_state: target_state_2,
                    ..
                } = other
                {
                    return dom_num == dom_num_2
                        && act_num == act_num_2
                        && target_state == target_state_2;
                }
            }
            SomeNeed::StateNotInGroup {
                dom_num,
                act_num,
                target_state,
            } => {
                if let SomeNeed::StateNotInGroup {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    target_state: target_state_2,
                } = other
                {
                    return dom_num == dom_num_2
                        && act_num == act_num_2
                        && target_state == target_state_2;
                }
            }
            SomeNeed::ContradictoryIntersection {
                dom_num,
                act_num,
                target_region,
                ..
            } => {
                if let SomeNeed::ContradictoryIntersection {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    target_region: target_region_2,
                    ..
                } = other
                {
                    return dom_num == dom_num_2
                        && act_num == act_num_2
                        && target_region == target_region_2;
                }
            }
            SomeNeed::ToRegion {
                dom_num,
                target_region,
                ..
            } => {
                if let SomeNeed::ToRegion {
                    dom_num: dom_num_2,
                    target_region: target_region_2,
                    ..
                } = other
                {
                    return dom_num == dom_num_2 && target_region == target_region_2;
                }
            }
            SomeNeed::ToOptimalRegion { target_regions } => {
                if let SomeNeed::ToOptimalRegion {
                    target_regions: targ_regs_2,
                } = other
                {
                    return target_regions == targ_regs_2;
                }
            }
            SomeNeed::LimitGroup {
                dom_num,
                act_num,
                target_state,
                anchor,
                ..
            } => {
                if let SomeNeed::LimitGroup {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    target_state: target_state_2,
                    anchor: anchor_2,
                    ..
                } = other
                {
                    return dom_num == dom_num_2
                        && act_num == act_num_2
                        && target_state == target_state_2
                        && anchor == anchor_2;
                }
            }
            SomeNeed::LimitGroupAdj {
                dom_num,
                act_num,
                target_state,
                anchor,
                ..
            } => {
                if let SomeNeed::LimitGroupAdj {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    target_state: target_state_2,
                    anchor: anchor_2,
                    ..
                } = other
                {
                    return dom_num == dom_num_2
                        && act_num == act_num_2
                        && target_state == target_state_2
                        && anchor == anchor_2;
                }
            }
            SomeNeed::ConfirmGroup {
                dom_num,
                act_num,
                target_state,
                ..
            } => {
                if let SomeNeed::ConfirmGroup {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    target_state: target_state_2,
                    ..
                } = other
                {
                    return dom_num == dom_num_2
                        && act_num == act_num_2
                        && target_state == target_state_2;
                }
            }
            SomeNeed::AddGroup { group_region, .. } => {
                if let SomeNeed::AddGroup {
                    group_region: group_region_2,
                    ..
                } = other
                {
                    return group_region == group_region_2;
                }
            }
            SomeNeed::RemoveGroupAnchor { group_region } => {
                if let SomeNeed::RemoveGroupAnchor {
                    group_region: group_region_2,
                } = other
                {
                    return group_region == group_region_2;
                }
            }
            SomeNeed::SetGroupLimited { group_region } => {
                if let SomeNeed::SetGroupLimited {
                    group_region: group_region_2,
                } = other
                {
                    return group_region == group_region_2;
                }
            }
            SomeNeed::SetGroupAnchor {
                group_region,
                anchor,
            } => {
                if let SomeNeed::SetGroupAnchor {
                    group_region: group_region_2,
                    anchor: anchor_2,
                } = other
                {
                    return group_region == group_region_2 && anchor == anchor_2;
                }
            }
            SomeNeed::SeekEdge {
                dom_num,
                act_num,
                target_state,
                in_group,
            } => {
                if let SomeNeed::SeekEdge {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    target_state: target_state_2,
                    in_group: in_group_2,
                } = other
                {
                    return dom_num == dom_num_2
                        && act_num == act_num_2
                        && in_group == in_group_2
                        && target_state == target_state_2;
                }
            }
            SomeNeed::InactivateSeekEdge { reg } => {
                if let SomeNeed::InactivateSeekEdge { reg: reg_2 } = other {
                    return reg == reg_2;
                }
            }
            SomeNeed::AddSeekEdge { reg } => {
                if let SomeNeed::AddSeekEdge { reg: reg_2 } = other {
                    return reg == reg_2;
                }
            } // Add new needs here
        };
        false
    }
}

impl Eq for SomeNeed {}

impl SomeNeed {
    // Return a type string
    pub fn type_string(&self) -> String {
        match self {
            SomeNeed::AStateMakeGroup { .. } => "AStateMakeGroup".to_string(),
            SomeNeed::StateNotInGroup { .. } => "StateNotInGroup".to_string(),
            SomeNeed::ContradictoryIntersection { .. } => "ContradictoryIntersection".to_string(),
            SomeNeed::ToRegion { .. } => "ToRegion".to_string(),
            SomeNeed::ToOptimalRegion { .. } => "ToOptimalRegion".to_string(),
            SomeNeed::LimitGroup { .. } => "LimitGroup".to_string(),
            SomeNeed::LimitGroupAdj { .. } => "LimitGroupAdj".to_string(),
            SomeNeed::ConfirmGroup { .. } => "ConfirmGroup".to_string(),
            SomeNeed::SeekEdge { .. } => "SeekEdge".to_string(),
            SomeNeed::AddGroup { .. } => "AddGroup".to_string(),
            SomeNeed::RemoveGroupAnchor { .. } => "RemoveGroupAnchor".to_string(),
            SomeNeed::SetGroupLimited { .. } => "SetGroupLimited".to_string(),
            SomeNeed::SetGroupAnchor { .. } => "SetGroupAnchor".to_string(),
            SomeNeed::InactivateSeekEdge { reg: _ } => "InactivateSeekEdge".to_string(),
            SomeNeed::AddSeekEdge { reg: _ } => "AddSeekEdge".to_string(),
        } // end match
    } // end type_string

    /// Return a priority number for a need.  Lower is more important.
    //  Don't use number zero!
    pub fn priority(&self) -> usize {
        match self {
            SomeNeed::SeekEdge { .. } => 100,
            SomeNeed::ContradictoryIntersection { .. } => 200,
            SomeNeed::AStateMakeGroup { num_x, .. } => 399 - num_x,
            SomeNeed::ConfirmGroup { group_num, .. } => 400 + group_num,
            SomeNeed::LimitGroupAdj { group_num, .. } => 400 + group_num,
            SomeNeed::LimitGroup { group_num, .. } => 400 + group_num,
            SomeNeed::StateNotInGroup { .. } => 500,
            SomeNeed::ToOptimalRegion { .. } => 600,
            SomeNeed::ToRegion { .. } => 700,
            _ => usize::MAX,
        } // end match ndx
    } // end priority

    /// Return a group number, the order in the group store list.
    pub fn group_num(&self) -> usize {
        match self {
            SomeNeed::LimitGroup { group_num, .. } => *group_num,
            SomeNeed::LimitGroupAdj { group_num, .. } => *group_num,
            SomeNeed::ConfirmGroup { group_num, .. } => *group_num,
            _ => usize::MAX,
        }
    }

    /// Return true if a state satisfies a need.
    pub fn satisfied_by(&self, cur_state: &SomeState) -> bool {
        match self {
            SomeNeed::AStateMakeGroup { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            SomeNeed::StateNotInGroup { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            SomeNeed::ContradictoryIntersection { target_region, .. } => {
                if target_region.is_superset_of_state(cur_state) {
                    return true;
                }
            }
            SomeNeed::ToRegion { target_region, .. } => {
                if target_region.is_superset_of_state(cur_state) {
                    return true;
                }
            }
            SomeNeed::ConfirmGroup { target_state, .. } => {
                if cur_state == target_state {
                    return true;
                }
            }
            SomeNeed::SeekEdge { target_state, .. } => {
                if cur_state == target_state {
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
            _ => panic!(
                "satisfied_by: should not be called on this need {}",
                self.type_string()
            ),
        } //end match self
        false
    } // end satisfied_by

    /// Return need action number.
    pub fn act_num(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup { act_num, .. } => *act_num,
            SomeNeed::StateNotInGroup { act_num, .. } => *act_num,
            SomeNeed::ContradictoryIntersection { act_num, .. } => *act_num,
            SomeNeed::ToRegion { act_num, .. } => *act_num,
            SomeNeed::ConfirmGroup { act_num, .. } => *act_num,
            SomeNeed::SeekEdge { act_num, .. } => *act_num,
            SomeNeed::LimitGroup { act_num, .. } => *act_num,
            SomeNeed::LimitGroupAdj { act_num, .. } => *act_num,
            _ => panic!("act_num: not known for need {}", self.type_string()),
        } //end match self
    } // end act_num

    /// Return need domain number.
    pub fn dom_num(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup { dom_num, .. } => *dom_num,
            SomeNeed::StateNotInGroup { dom_num, .. } => *dom_num,
            SomeNeed::ContradictoryIntersection { dom_num, .. } => *dom_num,
            SomeNeed::ToRegion { dom_num, .. } => *dom_num,
            SomeNeed::ConfirmGroup { dom_num, .. } => *dom_num,
            SomeNeed::SeekEdge { dom_num, .. } => *dom_num,
            SomeNeed::LimitGroup { dom_num, .. } => *dom_num,
            SomeNeed::LimitGroupAdj { dom_num, .. } => *dom_num,
            _ => panic!("dom_num: not known for need {}", self),
        } //end match self
    } // end dom_num

    /// Return a region for a need target.
    pub fn target(&self) -> TargetStore {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state, target_state),
            )),
            SomeNeed::StateNotInGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state, target_state),
            )),
            SomeNeed::ContradictoryIntersection {
                dom_num,
                target_region,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(*dom_num, target_region.clone())),
            SomeNeed::ToRegion {
                dom_num,
                target_region,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(*dom_num, target_region.clone())),
            SomeNeed::ConfirmGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state, target_state),
            )),
            SomeNeed::SeekEdge {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state, target_state),
            )),
            SomeNeed::LimitGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state, target_state),
            )),
            SomeNeed::LimitGroupAdj {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state, target_state),
            )),
            SomeNeed::ToOptimalRegion { target_regions, .. } => {
                let mut targ = TargetStore::new();
                for (dom_numx, targx) in target_regions.iter().enumerate() {
                    targ.push(SomeTarget::new(dom_numx, targx.clone()));
                }
                targ
            }
            _ => panic!(
                "target: should not be called for this need {}",
                self.type_string()
            ),
        }
    } // end target

    /// Set the Domain number for a need.
    pub fn set_dom(&mut self, num: usize) {
        match self {
            SomeNeed::AStateMakeGroup { dom_num, .. } => *dom_num = num,
            SomeNeed::StateNotInGroup { dom_num, .. } => *dom_num = num,
            SomeNeed::ContradictoryIntersection { dom_num, .. } => *dom_num = num,
            SomeNeed::ToRegion { dom_num, .. } => *dom_num = num,
            SomeNeed::ConfirmGroup { dom_num, .. } => *dom_num = num,
            SomeNeed::SeekEdge { dom_num, .. } => *dom_num = num,
            SomeNeed::LimitGroup { dom_num, .. } => *dom_num = num,
            SomeNeed::LimitGroupAdj { dom_num, .. } => *dom_num = num,
            _ => {
                panic!(
                    "set_dom: should not be called for this need {}",
                    self.type_string()
                );
            }
        };
    } // end set_dom
}
