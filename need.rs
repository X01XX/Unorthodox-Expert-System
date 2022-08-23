//! The SomeNeed enum, representing needs for samples, or housekeeping tasks.
//!
//! Something is logically needed, like:
//! More samples of a square(or state, or bit pattern),
//! A sample in a region that has contradictory predictions.
//! Samples to limit a group.
//! Housekeeping needs, like limiting a group.

use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::rulestore::RuleStore;
use crate::state::SomeState;

use std::fmt;

impl fmt::Display for SomeNeed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pri = self.priority();
        let rc_str = match self {
            SomeNeed::AStateMakeGroup {
                dom_num,
                act_num,
                targ_state,
                for_reg,
                far,
                num_x,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample State {}, far from {}, to make group {} nx: {})",
                dom_num, act_num, pri, targ_state, far, for_reg, num_x
            ),
            SomeNeed::StateNotInGroup {
                dom_num,
                act_num,
                targ_state,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample State {}, Not in a group)",
                dom_num, act_num, pri, targ_state
            ),
            SomeNeed::ContradictoryIntersection {
                dom_num,
                act_num,
                goal_reg,
                group1,
                ruls1,
                group2,
                ruls2,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample Region {} intersection of {} {} and {} {})",
                dom_num, act_num, pri, goal_reg, group1, ruls1, group2, ruls2
            ),
            SomeNeed::ToRegion {
                dom_num, goal_reg, ..
            } => format!(
                "N(Dom {} Pri {} To Optimal Region {})",
                dom_num, pri, goal_reg,
            ),
            SomeNeed::ToRegion2 {
                goal_regs,
            } => format!(
                "N(Pri {} To Optimal Regions {})",
                pri, goal_regs,
            ),
            SomeNeed::LimitGroup {
                dom_num,
                act_num,
                targ_state,
                for_group,
                anchor,
            } => {
                if for_group.is_superset_of_state(&targ_state) {
                    if targ_state == anchor {
                        format!(
                            "N(Dom {} Act {} Pri {} Sample anchor State {}, to limit group {})",
                            dom_num, act_num, pri, anchor, for_group,
                        )
                    } else {
                        format!(
                            "N(Dom {} Act {} Pri {} Sample State {}, far from {} to limit group {})",
                            dom_num, act_num, pri, targ_state, anchor, for_group,
                        )
                    }
                } else {
                    format!(
                        "N(Dom {} Act {} Pri {} Sample State {}, adj to {} to limit group {})",
                        dom_num, act_num, pri, targ_state, anchor, for_group,
                    )
                }
            }
            SomeNeed::ConfirmGroup {
                dom_num,
                act_num,
                targ_state,
                grp_reg,
            } => {
                format!(
                    "N(Dom {} Act {} Pri {} Get additional sample of state {} to confirm group {})",
                    dom_num, act_num, pri, targ_state, grp_reg
                )
            }

            SomeNeed::SeekEdge {
                dom_num,
                act_num,
                targ_state,
                in_group,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample State {}, between {} and {} to seek edge)",
                dom_num, act_num, pri, targ_state, in_group.state1, in_group.state2
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
        targ_state: SomeState,
        for_reg: SomeRegion,
        far: SomeState,
        num_x: usize,
    },
    /// Sample a state that is not in a group.
    StateNotInGroup {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
    },
    /// Sample a state to resolve a contradictory intersection of two groups.
    ContradictoryIntersection {
        dom_num: usize,
        act_num: usize,
        goal_reg: SomeRegion,
        group1: SomeRegion,
        ruls1: RuleStore,
        group2: SomeRegion,
        ruls2: RuleStore,
    },
    /// Sample a state to limit a group.
    LimitGroup {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
        for_group: SomeRegion,
        anchor: SomeState,
    },
    /// Get an additional sample of a state.
    ConfirmGroup {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
        grp_reg: SomeRegion,
    },
    /// Sample a state to find a new edge in the total solution.
    SeekEdge {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
        in_group: SomeRegion,
    },
    /// Move current state to a given region.
    ToRegion {
        dom_num: usize,
        act_num: usize,
        goal_reg: SomeRegion,
    },
    /// Move current state to given regions.
    ToRegion2 {
        goal_regs: RegionStore,
    },
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
                targ_state,
                ..
            } => match other {
                SomeNeed::AStateMakeGroup {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    targ_state: targ_state_2,
                    ..
                } => {
                    if dom_num == dom_num_2 && act_num == act_num_2 && targ_state == targ_state_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::StateNotInGroup {
                dom_num,
                act_num,
                targ_state,
            } => match other {
                SomeNeed::StateNotInGroup {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    targ_state: targ_state_2,
                } => {
                    if dom_num == dom_num_2 && act_num == act_num_2 && targ_state == targ_state_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::ContradictoryIntersection {
                dom_num,
                act_num,
                goal_reg,
                ..
            } => match other {
                SomeNeed::ContradictoryIntersection {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    goal_reg: goal_reg_2,
                    ..
                } => {
                    if dom_num == dom_num_2 && act_num == act_num_2 && goal_reg == goal_reg_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::ToRegion {
                dom_num, goal_reg, ..
            } => match other {
                SomeNeed::ToRegion {
                    dom_num: dom_num_2,
                    goal_reg: goal_reg_2,
                    ..
                } => {
                    if dom_num == dom_num_2 && goal_reg == goal_reg_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::ToRegion2 {
                goal_regs,
            } => match other {
                SomeNeed::ToRegion2 {
                    goal_regs: goal_regs_2,
                } => {
                    if goal_regs == goal_regs_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::LimitGroup {
                dom_num,
                act_num,
                targ_state,
                anchor,
                ..
            } => match other {
                SomeNeed::LimitGroup {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    targ_state: targ_state_2,
                    anchor: anchor_2,
                    ..
                } => {
                    if dom_num == dom_num_2
                        && act_num == act_num_2
                        && targ_state == targ_state_2
                        && anchor == anchor_2
                    {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::ConfirmGroup {
                dom_num,
                act_num,
                targ_state,
                ..
            } => match other {
                SomeNeed::ConfirmGroup {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    targ_state: targ_state_2,
                    ..
                } => {
                    if dom_num == dom_num_2 && act_num == act_num_2 && targ_state == targ_state_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::AddGroup { group_region, .. } => match other {
                SomeNeed::AddGroup {
                    group_region: group_region_2,
                    ..
                } => {
                    if group_region == group_region_2 {
                        return true;
                    }
                }

                _ => (),
            },
            SomeNeed::RemoveGroupAnchor { group_region } => match other {
                SomeNeed::RemoveGroupAnchor {
                    group_region: group_region_2,
                } => {
                    if group_region == group_region_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::SetGroupLimited { group_region } => match other {
                SomeNeed::SetGroupLimited {
                    group_region: group_region_2,
                } => {
                    if group_region == group_region_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::SetGroupAnchor {
                group_region,
                anchor,
            } => match other {
                SomeNeed::SetGroupAnchor {
                    group_region: group_region_2,
                    anchor: anchor_2,
                } => {
                    if group_region == group_region_2 && anchor == anchor_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::SeekEdge {
                dom_num,
                act_num,
                targ_state,
                in_group,
            } => match other {
                SomeNeed::SeekEdge {
                    dom_num: dom_num_2,
                    act_num: act_num_2,
                    targ_state: targ_state_2,
                    in_group: in_group_2,
                } => {
                    if dom_num == dom_num_2
                        && act_num == act_num_2
                        && in_group == in_group_2
                        && targ_state == targ_state_2
                    {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::InactivateSeekEdge { reg } => match other {
                SomeNeed::InactivateSeekEdge { reg: reg_2 } => {
                    if reg == reg_2 {
                        return true;
                    }
                }
                _ => (),
            },
            SomeNeed::AddSeekEdge { reg } => match other {
                SomeNeed::AddSeekEdge { reg: reg_2 } => {
                    if reg == reg_2 {
                        return true;
                    }
                }
                _ => (),
            },
            // Add new needs here
        };
        false
    }
}

impl Eq for SomeNeed {}

impl SomeNeed {
    // Return a type string
    pub fn type_string(&self) -> String {
        let ret_str = match self {
            SomeNeed::AStateMakeGroup { .. } => format!("AStateMakeGroup"),
            SomeNeed::StateNotInGroup { .. } => format!("StateNotInGroup"),
            SomeNeed::ContradictoryIntersection { .. } => format!("ContradictoryIntersection"),
            SomeNeed::ToRegion { .. } => format!("ToRegion"),
            SomeNeed::ToRegion2 { .. } => format!("ToRegion2"),
            SomeNeed::LimitGroup { .. } => format!("LimitGroup"),
            SomeNeed::ConfirmGroup { .. } => format!("ConfirmGroup"),
            SomeNeed::SeekEdge { .. } => format!("SeekEdge"),
            SomeNeed::AddGroup { .. } => format!("AddGroup"),
            SomeNeed::RemoveGroupAnchor { .. } => format!("RemoveGroupAnchor"),
            SomeNeed::SetGroupLimited { .. } => format!("SetGroupLimited"),
            SomeNeed::SetGroupAnchor { .. } => format!("SetGroupAnchor"),
            SomeNeed::InactivateSeekEdge { reg: _ } => format!("InactivateSeekEdge"),
            SomeNeed::AddSeekEdge { reg: _ } => format!("AddSeekEdge"),
        }; // end match
        ret_str
    } // end type_string

    /// Return a priority number for a need.  Lower is more important.
    //  Don't use number zero!
    pub fn priority(&self) -> usize {
        match self {
            SomeNeed::ContradictoryIntersection { .. } => return 1,
            SomeNeed::SeekEdge { .. } => return 2,
            SomeNeed::AStateMakeGroup { .. } => return 3,
            SomeNeed::StateNotInGroup { .. } => return 4,
            SomeNeed::ConfirmGroup { .. } => return 5,
            SomeNeed::LimitGroup { .. } => return 6,
            SomeNeed::ToRegion { .. } => return 8,
            SomeNeed::ToRegion2 { .. } => return 7,
            _ => return 9999,
        } // end match ndx
    } // end priority

    /// Return true if a state satisfies a need.
    pub fn satisfied_by(&self, cur_state: &SomeState) -> bool {
        match self {
            SomeNeed::AStateMakeGroup { targ_state, .. } => {
                if cur_state == targ_state {
                    return true;
                }
            }
            SomeNeed::StateNotInGroup { targ_state, .. } => {
                if cur_state == targ_state {
                    return true;
                }
            }
            SomeNeed::ContradictoryIntersection { goal_reg, .. } => {
                if goal_reg.is_superset_of_state(&cur_state) {
                    return true;
                }
            }
            SomeNeed::ToRegion { goal_reg, .. } => {
                if goal_reg.is_superset_of_state(&cur_state) {
                    return true;
                }
            }
            SomeNeed::ConfirmGroup { targ_state, .. } => {
                if cur_state == targ_state {
                    return true;
                }
            }
            SomeNeed::SeekEdge { targ_state, .. } => {
                if cur_state == targ_state {
                    return true;
                }
            }
            SomeNeed::LimitGroup { targ_state, .. } => {
                if cur_state == targ_state {
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
            SomeNeed::AStateMakeGroup { act_num, .. } => return *act_num,
            SomeNeed::StateNotInGroup { act_num, .. } => return *act_num,
            SomeNeed::ContradictoryIntersection { act_num, .. } => return *act_num,
            SomeNeed::ToRegion { act_num, .. } => return *act_num,
            SomeNeed::ConfirmGroup { act_num, .. } => return *act_num,
            SomeNeed::SeekEdge { act_num, .. } => return *act_num,
            SomeNeed::LimitGroup { act_num, .. } => return *act_num,
            _ => panic!("act_num: not known for need {}", self.type_string()),
        } //end match self
    } // end act_num

    /// Return need domain number.
    pub fn dom_num(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup { dom_num, .. } => return *dom_num,
            SomeNeed::StateNotInGroup { dom_num, .. } => return *dom_num,
            SomeNeed::ContradictoryIntersection { dom_num, .. } => return *dom_num,
            SomeNeed::ToRegion { dom_num, .. } => return *dom_num,
            SomeNeed::ConfirmGroup { dom_num, .. } => return *dom_num,
            SomeNeed::SeekEdge { dom_num, .. } => return *dom_num,
            SomeNeed::LimitGroup { dom_num, .. } => return *dom_num,
            _ => panic!("dom_num: not known for need {}", self),
        } //end match self
    } // end dom_num

    /// Return a region for a need target.
    pub fn target(&self) -> SomeRegion {
        match self {
            SomeNeed::AStateMakeGroup { targ_state, .. } => {
                return SomeRegion::new(targ_state, targ_state)
            }
            SomeNeed::StateNotInGroup { targ_state, .. } => {
                return SomeRegion::new(targ_state, targ_state)
            }
            SomeNeed::ContradictoryIntersection { goal_reg, .. } => return goal_reg.clone(),
            SomeNeed::ToRegion { goal_reg, .. } => return goal_reg.clone(),
            SomeNeed::ConfirmGroup { targ_state, .. } => {
                return SomeRegion::new(targ_state, targ_state)
            }
            SomeNeed::SeekEdge { targ_state, .. } => {
                return SomeRegion::new(targ_state, targ_state)
            }
            SomeNeed::LimitGroup { targ_state, .. } => {
                return SomeRegion::new(targ_state, targ_state)
            }
            SomeNeed::AddSeekEdge { reg, .. } => return reg.clone(),
            SomeNeed::SetGroupLimited { group_region, .. } => return group_region.clone(),
            SomeNeed::SetGroupAnchor { group_region, .. } => return group_region.clone(),
            _ => panic!(
                "target: should not be called for this need {}",
                self.type_string()
            ),
        };
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
            _ => {
                panic!(
                    "set_dom: should not be called for this need {}",
                    self.type_string()
                );
            }
        };
    } // end set_dom
}
