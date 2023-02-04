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
        let rc_str = match self {
            SomeNeed::AStateMakeGroup {
                dom_num,
                act_num,
                target_state,
                for_reg,
                far,
                num_x,
            } => {
                let pri = self.priority();
                format!(
                "N(Dom {dom_num} Act {act_num} Pri {pri} Sample State {target_state}, far from {far}, to make group {for_reg} nx: {num_x})")
            }
            SomeNeed::StateNotInGroup {
                dom_num,
                act_num,
                target_state,
            } => {
                let pri = self.priority();
                format!(
                "N(Dom {dom_num} Act {act_num} Pri {pri} Sample State {target_state} not in a group)")
            }
            SomeNeed::ContradictoryIntersection {
                dom_num,
                act_num,
                target_region,
                group1,
                ruls1,
                group2,
                ruls2,
            } => {
                let pri = self.priority();
                format!(
                "N(Dom {dom_num} Act {act_num} Pri {pri} Sample Region {target_region} intersection of {group1} {ruls1} and {group2} {ruls2})")
            }
            SomeNeed::ToOptimalRegion { target_regions } => {
                let pri = self.priority();
                format!("N(Pri {pri} To Optimal Regions {target_regions})")
            }
            SomeNeed::LimitGroup {
                dom_num,
                act_num,
                target_state,
                for_group,
                anchor,
                ..
            } => {
                let pri = self.priority();
                if target_state == anchor {
                    format!(
                        "N(Dom {dom_num} Act {act_num} Pri {pri} Sample anchor State {anchor}, to limit group {for_group})")
                } else {
                    format!(
                        "N(Dom {dom_num} Act {act_num} Pri {pri} Sample State {target_state}, far from {anchor} to limit group {for_group})")
                }
            }
            SomeNeed::LimitGroupAdj {
                dom_num,
                act_num,
                target_state,
                for_group,
                anchor,
                ..
            } => {
                let pri = self.priority();
                format!(
                    "N(Dom {dom_num} Act {act_num} Pri {pri} Sample State {target_state}, adj to {anchor} to limit group {for_group})")
            }
            SomeNeed::ConfirmGroup {
                dom_num,
                act_num,
                target_state,
                grp_reg,
                ..
            } => {
                let pri = self.priority();
                format!(
                    "N(Dom {dom_num} Act {act_num} Pri {pri} Get additional sample of state {target_state} to confirm group {grp_reg})")
            }
            SomeNeed::SeekEdge {
                dom_num,
                act_num,
                target_state,
                in_group,
            } => {
                let pri = self.priority();
                format!(
                    "N(Dom {} Act {} Pri {} Sample State {}, between {} and {} to seek edge)",
                    dom_num, act_num, pri, target_state, in_group.state1, in_group.state2
                )
            }
            SomeNeed::AddGroup {
                group_region,
                rules,
            } => format!("N(Create group from {group_region} {rules})"),
            SomeNeed::RemoveGroupAnchor { group_region } => {
                format!("N(Remove anchor for group {group_region})")
            }
            SomeNeed::SetGroupLimited { group_region } => {
                format!("N(set group {group_region} limited)")
            }
            SomeNeed::SetGroupAnchor {
                group_region,
                anchor,
            } => format!("N(set group {group_region} anchor {anchor})"),
            SomeNeed::InactivateSeekEdge { reg: regx } => {
                format!("N(Inactivate SeekEdge region: {})", &regx)
            }
            SomeNeed::AddSeekEdge { reg: regx } => format!("N(Add SeekEdge region: {})", &regx),
        }; // end match

        write!(f, "{rc_str}")
    }
}

#[derive(Debug)]
/// Enums that represent a number of different needs.
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
    /// Return a need name string.
    /// Mostly used in testing.
    /// Ugly, slow, but it works.
    pub fn name(&self) -> String {
        let str = format!("{self:?}");
        let first_space = str.find(char::is_whitespace).unwrap();
        str[0..first_space].to_string()
    }

    /// Return a priority number for a need.  Lower is more important.
    ///  Don't use number zero!
    /// "- num_x" gives priority to larger regions.
    /// "+ group_num" gives priority to groups near the beginning of a group list.
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
            _ => panic!(
                "SomeNeed::priority should not be called for the {} need.",
                self.name()
            ),
            //_ => usize::MAX,
        } // end match ndx
    } // end priority

    /// Return a group number, the order in the group store list.
    pub fn group_num(&self) -> usize {
        match self {
            SomeNeed::LimitGroup { group_num, .. } => *group_num,
            SomeNeed::LimitGroupAdj { group_num, .. } => *group_num,
            SomeNeed::ConfirmGroup { group_num, .. } => *group_num,
            _ => panic!(
                "SomeNeed::group_num should not be called for the {} need.",
                self.name()
            ),
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
            SomeNeed::StateNotInGroup { act_num, .. } => *act_num,
            SomeNeed::ContradictoryIntersection { act_num, .. } => *act_num,
            SomeNeed::ConfirmGroup { act_num, .. } => *act_num,
            SomeNeed::SeekEdge { act_num, .. } => *act_num,
            SomeNeed::LimitGroup { act_num, .. } => *act_num,
            SomeNeed::LimitGroupAdj { act_num, .. } => *act_num,
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
            SomeNeed::StateNotInGroup { dom_num, .. } => *dom_num,
            SomeNeed::ContradictoryIntersection { dom_num, .. } => *dom_num,
            SomeNeed::ConfirmGroup { dom_num, .. } => *dom_num,
            SomeNeed::SeekEdge { dom_num, .. } => *dom_num,
            SomeNeed::LimitGroup { dom_num, .. } => *dom_num,
            SomeNeed::LimitGroupAdj { dom_num, .. } => *dom_num,
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
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )),
            SomeNeed::StateNotInGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )),
            SomeNeed::ContradictoryIntersection {
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
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )),
            SomeNeed::SeekEdge {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )),
            SomeNeed::LimitGroup {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )),
            SomeNeed::LimitGroupAdj {
                dom_num,
                target_state,
                ..
            } => TargetStore::new_with_target(SomeTarget::new(
                *dom_num,
                SomeRegion::new(target_state.clone(), target_state.clone()),
            )),
            SomeNeed::ToOptimalRegion { target_regions, .. } => {
                let mut targ = TargetStore::new();
                for (dom_numx, targx) in target_regions.iter().enumerate() {
                    targ.push(SomeTarget::new(dom_numx, targx.clone()));
                }
                targ
            }
            _ => panic!(
                "SomeNeed::target should not be called for the {} need.",
                self.name()
            ),
        }
    } // end target

    /// Set the Domain number for a need.
    pub fn set_dom(&mut self, num: usize) {
        match self {
            SomeNeed::AStateMakeGroup { dom_num, .. } => *dom_num = num,
            SomeNeed::StateNotInGroup { dom_num, .. } => *dom_num = num,
            SomeNeed::ContradictoryIntersection { dom_num, .. } => *dom_num = num,
            SomeNeed::ConfirmGroup { dom_num, .. } => *dom_num = num,
            SomeNeed::SeekEdge { dom_num, .. } => *dom_num = num,
            SomeNeed::LimitGroup { dom_num, .. } => *dom_num = num,
            SomeNeed::LimitGroupAdj { dom_num, .. } => *dom_num = num,
            _ => {
                panic!(
                    "SomeNeed::set_dom should not be called for the {} need.",
                    self.name()
                );
            }
        };
    } // end set_dom
}
