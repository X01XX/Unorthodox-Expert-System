//! The SomeNeed enum, representing needs for samples, or housekeeping tasks.
//!
//! Something is logically needed, like:
//! More samples of a square(or state, or bit pattern),
//! A sample in a region that has contradictory predictions.
//! Samples to limit a group.
//! Housekeeping needs, like limiting a group.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::rulestore::RuleStore;
use crate::state::SomeState;
use crate::statestore::StateStore;

use std::fmt;

impl fmt::Display for SomeNeed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pri = self.priority();
        let rc_str = match self {
            SomeNeed::AStateMakeGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                for_reg: freg,
                far,
                num_x: nx,
            } =>  format!(
                "N(Dom {} Act {} Pri {} Sample State {}, far from {}, to make group {} nx: {})",
                dm, an, pri, sta, far, freg, nx
            ),
            SomeNeed::StateNotInGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample State {}, Not in a group)",
                dm, an, pri, sta
            ),
            SomeNeed::ContradictoryIntersection {
                dom_num: dm,
                act_num: an,
                goal_reg: g_reg,
                group1: grp1,
                ruls1,
                group2: grp2,
                ruls2,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample Region {} intersection of {} {} and {} {}",
                dm, an, pri, &g_reg, &grp1, &ruls1, &grp2, &ruls2
            ),
            SomeNeed::ToRegion {
                dom_num: dm,
                goal_reg: g_reg,
                ..
            } => format!(
                "N(Dom {} Pri {} To Region {}",
                dm, pri, &g_reg,
            ),
            SomeNeed::SampleRegion {
                dom_num: dm,
                act_num: an,
                goal_reg: g_reg,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample Region {}",
                dm, an, pri, &g_reg,
            ),
            SomeNeed::LimitGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                for_group: greg,
                anchor: anc_sta,
            } => {
                if greg.is_superset_of_state(&sta) {
                    if sta == anc_sta {
                        format!(
                            "N(Dom {} Act {} Pri {} Sample anchor State {}, to limit group {})",
                            dm, an, pri, anc_sta, greg,
                        )
                    } else {
                        format!(
                            "N(Dom {} Act {} Pri {} Sample State {}, far from {} to limit group {})",
                            dm, an, pri, sta, anc_sta, greg,
                        )
                    }
                } else {
                    format!(
                        "N(Dom {} Act {} Pri {} Sample State {}, adj to {} to limit group {})",
                        dm, an, pri, sta, anc_sta, greg,
                    )
                }
            }
            SomeNeed::StateAdditionalSample {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                grp_reg: greg,
                far: farx,
            } => 
                if farx == sta {
                    format!(
                        "N(Dom {} Act {} Pri {} Get additional sample of state {} for region {})",
                        dm, an, pri, &sta, &greg
                    )
                } else {
                    format!(
                        "N(Dom {} Act {} Pri {} Get additional sample of state {} for region {} far {})",
                        dm, an, pri, &sta, &greg, &farx
                    )
                },

            SomeNeed::SeekEdge {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                in_group: greg,
            } => format!(
                "N(Dom {} Act {} Pri {} Sample State {}, between {} and {})",
                dm, an, pri, &sta, &greg.state1, &greg.state2
            ),
            SomeNeed::AddGroup { group_region: greg } => format!("N(Create group {})", greg),
            SomeNeed::AddGroup2 { group_region, states } => format!("N(Create group from {} {})", group_region, states),
            SomeNeed::RemoveGroupAnchor { group_region: greg } => format!("N(Remove anchor for group {})", greg),
            SomeNeed::SetGroupLimited {
                group_region: greg,
                cstate: sta1,
            } => format!("N(set group {} limited by {})", greg, sta1),
            SomeNeed::SetEdgeExpand {
                group_region: greg,
                edge_mask: mbitx,
            } => format!("N(group {} set edge expand {})", greg, mbitx),

            SomeNeed::InactivateSeekEdge { reg: regx } => {
                format!("N(Inactivate SeekEdge region: {}", &regx)
            }
            SomeNeed::AddSeekEdge { reg: regx } => format!("N(Add SeekEdge region: {}", &regx),
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
    StateAdditionalSample {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
        grp_reg: SomeRegion,
        far: SomeState,
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
    /// Sample a given region, to get a sample in only one region.
    SampleRegion {
        dom_num: usize,
        act_num: usize,
        goal_reg: SomeRegion,
    },
    /// Housekeeping, add a group.
    AddGroup { group_region: SomeRegion },
        /// Housekeeping, add a group.
    AddGroup2 { group_region: SomeRegion, states: StateStore },
    /// Housekeeping, Remove group anchor.
    RemoveGroupAnchor { group_region: SomeRegion },
    /// Housekeeping, set a group to limited, using a state
    /// that is only in that group, has adjacent, external, dissimilar squares.
    SetGroupLimited {
        group_region: SomeRegion,
        cstate: SomeState,
    },
    /// Housekeeping, set an edge expand mask.
    SetEdgeExpand {
        group_region: SomeRegion,
        edge_mask: SomeMask,
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
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                ..
            } => match other {
                SomeNeed::AStateMakeGroup {
                    dom_num: dmx,
                    act_num: anx,
                    targ_state: stax,
                    ..
                } => 
                    if dm == dmx && an == anx && sta == stax {
                        return true;
                    },
                _ => {}
            },
            SomeNeed::StateNotInGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
            } => match other {
                SomeNeed::StateNotInGroup {
                    dom_num: dmx,
                    act_num: anx,
                    targ_state: stax,
                } => 
                    if dm == dmx && an == anx && sta == stax {
                        return true;
                    },
                _ => {}
            },
            SomeNeed::ContradictoryIntersection {
                dom_num: dm,
                act_num: an,
                goal_reg: g_reg,
                ..
            } => match other {
                SomeNeed::ContradictoryIntersection {
                    dom_num: dmx,
                    act_num: anx,
                    goal_reg: g_regx,
                    ..
                } => 
                    if dm == dmx && an == anx && *g_reg == *g_regx {
                        return true;
                    },
                _ => {}
            },
            SomeNeed::ToRegion {
                dom_num: dm,
                goal_reg: g_reg,
                ..
            } => match other {
                SomeNeed::ToRegion {
                    dom_num: dmx,
                    goal_reg: g_regx,
                    ..
                } =>
                    if dm == dmx && *g_reg == *g_regx {
                        return true;
                    },
                _ => {}
            },
            SomeNeed::SampleRegion {
                dom_num: dm,
                act_num: an,
                goal_reg: g_reg,
            } => match other {
                SomeNeed::SampleRegion {
                    dom_num: dmx,
                    act_num: anx,
                    goal_reg: g_regx,
                } =>
                    if dm == dmx && an == anx && *g_reg == *g_regx {
                        return true;
                    },
                _ => {}
            },
            SomeNeed::LimitGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                anchor: anc_sta,
                ..
            } => match other {
                SomeNeed::LimitGroup {
                    dom_num: dmx,
                    act_num: anx,
                    targ_state: stax,
                    anchor: anc_stax,
                    ..
                } =>
                    if dm == dmx && an == anx && sta == stax && anc_sta == anc_stax {
                        return true;
                    },
                _ => {}
            },
            SomeNeed::StateAdditionalSample {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                ..
            } => match other {
                SomeNeed::StateAdditionalSample {
                    dom_num: dmx,
                    act_num: anx,
                    targ_state: stax,
                    ..
                } => 
                    if dm == dmx && an == anx && sta == stax {
                        return true;
                    },
                _ => {}
            },
            SomeNeed::AddGroup { group_region: greg } => match other {
                SomeNeed::AddGroup {
                    group_region: gregx,
                } => 
                    if *greg == *gregx {
                        return true;
                    },
                _ => {}
            },
            SomeNeed::AddGroup2 { group_region, .. } => match other {
                SomeNeed::AddGroup2 {
                    group_region: group_regionx, ..
                } => 
                    if group_region == group_regionx {
                        return true;
                    },
                    
                _ => {}
            },
            SomeNeed::RemoveGroupAnchor { group_region: greg } => match other {
                SomeNeed::RemoveGroupAnchor {
                    group_region: gregx,
                } =>
                    if *greg == *gregx {
                        return true;
                    }
                _ => {}
            },
            SomeNeed::SetGroupLimited {
                group_region: greg,
                cstate: sta1,
            } => match other {
                SomeNeed::SetGroupLimited {
                    group_region: gregx,
                    cstate: sta1x,
                } =>
                    if greg == gregx && sta1 == sta1x {
                        return true;
                    }
                _ => {}
            },
            SomeNeed::SetEdgeExpand {
                group_region: greg,
                edge_mask: mbitx,
            } => match other {
                SomeNeed::SetEdgeExpand {
                    group_region: gregx,
                    edge_mask: mbity,
                } =>
                    if greg == gregx && mbitx == mbity {
                        return true;
                    }
                _ => {}
            },
            SomeNeed::SeekEdge {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                in_group: greg,
            } => match other {
                SomeNeed::SeekEdge {
                    dom_num: dmx,
                    act_num: anx,
                    targ_state: stax,
                    in_group: gregx,
                } =>
                    if dm == dmx && an == anx && greg == gregx && sta == stax {
                        return true;
                    }
                _ => {}
            },
            SomeNeed::InactivateSeekEdge { reg: regx } => match other {
                SomeNeed::InactivateSeekEdge { reg: regy } => {
                    if *regx == *regy {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::AddSeekEdge { reg: regx } => match other {
                SomeNeed::AddSeekEdge { reg: regy } =>
                    if *regx == *regy {
                        return true;
                    },
                _ => { }
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
            SomeNeed::AStateMakeGroup {
                ..
            } => format!("AStateMakeGroup"),
            SomeNeed::StateNotInGroup {
                ..
            } => format!("StateNotInGroup"),
            SomeNeed::ContradictoryIntersection {
                ..
            } => format!("ContradictoryIntersection"),
            SomeNeed::ToRegion {
                ..
            } => format!("ToRegion"),
            SomeNeed::SampleRegion {
                ..
            } => format!("SampleRegion"),
            SomeNeed::LimitGroup {
                ..
            } => format!("LimitGroup"),
            SomeNeed::StateAdditionalSample {
                ..
            } => format!("StateAdditionalSample"),
            SomeNeed::SeekEdge {
                ..
            } => format!("SeekEdge"),
            SomeNeed::AddGroup { .. } => format!("AddGroup"),
            SomeNeed::AddGroup2 { .. } => format!("AddGroup2"),
            SomeNeed::RemoveGroupAnchor { .. } => format!("RemoveGroupAnchor"),
            SomeNeed::SetGroupLimited {
                ..
            } => format!("SetGroupLimited"),
            SomeNeed::SetEdgeExpand {
                ..
            } => format!("SetEdgeExpand"),

            SomeNeed::InactivateSeekEdge { reg: _ } => {
                format!("InactivateSeekEdge")
            }
            SomeNeed::AddSeekEdge { reg: _ } => format!("AddSeekEdge"),
        }; // end match
        ret_str
    } // end type_string

    /// Return a priority number for a need.  Lower is more important.
    //  Don't use number zero!
    pub fn priority(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup { .. } => return 3,
            SomeNeed::StateNotInGroup { .. } => return 4,
            SomeNeed::ContradictoryIntersection { .. } => return 2,
            SomeNeed::ToRegion { .. } => return 9,
            SomeNeed::SampleRegion { .. } => return 8,
            SomeNeed::LimitGroup { .. } => return 8,
            SomeNeed::StateAdditionalSample { .. } => return 5,
            SomeNeed::SeekEdge { .. } => return 1,
            _ => return 9999,
        } // end match ndx
    } // end priority

    /// Return true if a state satisfies a need.
    pub fn satisfied_by(&self, cur_state: &SomeState) -> bool {
        match self {
            SomeNeed::AStateMakeGroup {
                targ_state: sta, ..
            } =>
                if cur_state == sta {
                    return true;
                }
            SomeNeed::StateNotInGroup {
                targ_state: sta, ..
            } =>
                if cur_state == sta {
                    return true;
                }
            SomeNeed::ContradictoryIntersection {
                goal_reg: g_reg, ..
            } =>
                if g_reg.is_superset_of_state(&cur_state) {
                    return true;
                }
            SomeNeed::ToRegion {
                goal_reg: g_reg, ..
            } =>
                if g_reg.is_superset_of_state(&cur_state) {
                    return true;
                }
            SomeNeed::SampleRegion {
                goal_reg: g_reg, ..
            } =>
                if g_reg.is_superset_of_state(&cur_state) {
                    return true;
                }
            SomeNeed::StateAdditionalSample {
                targ_state: sta, ..
            } =>
                if cur_state == sta {
                    return true;
                }
            SomeNeed::SeekEdge {
                targ_state: sta, ..
            } =>
                if cur_state == sta {
                    return true;
                }
            SomeNeed::LimitGroup {
                targ_state: sta, ..
            } => if cur_state == sta { return true; }
            _ => panic!("satisfied_by: should not be called on this need {}", self.type_string()),
        } //end match self
        false
    } // end satisfied_by

    /// Return need action number.
    pub fn act_num(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup {
                act_num: an, ..
            } => return *an,
            SomeNeed::StateNotInGroup {
                act_num: an, ..
            } => return *an,
            SomeNeed::ContradictoryIntersection {
                act_num: an, ..
            } => return *an,
            SomeNeed::ToRegion {
                act_num: an, ..
            } => return *an,
            SomeNeed::SampleRegion {
                act_num: an, ..
            } => return *an,
            SomeNeed::StateAdditionalSample {
                act_num: an, ..
            } => return *an,
            SomeNeed::SeekEdge {
                act_num: an, ..
            } => return *an,
            SomeNeed::LimitGroup {
                act_num: an, ..
            } => return *an,
            _ => panic!("act_num: not known for need {}", self.type_string()),
        } //end match self
    } // end act_num

    /// Return need domain number.
    pub fn dom_num(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num: dm, ..
            } => return *dm,
            SomeNeed::StateNotInGroup {
                dom_num: dm, ..
            } => return *dm,
            SomeNeed::ContradictoryIntersection {
                dom_num: dm, ..
            } => return *dm,
            SomeNeed::ToRegion {
                dom_num: dm, ..
            } => return *dm,
            SomeNeed::SampleRegion {
                dom_num: dm, ..
            } => return *dm,
            SomeNeed::StateAdditionalSample {
                dom_num: dm, ..
            } => return *dm,
            SomeNeed::SeekEdge {
                dom_num: dm, ..
            } => return *dm,
            SomeNeed::LimitGroup {
                dom_num: dm, ..
            } => return *dm,
            _ => panic!("dom_num: not known for need {}", self.type_string()),
        } //end match self
    } // end dom_num

    /// Return a region for a need target.
    pub fn target(&self) -> SomeRegion {
        match self {
            SomeNeed::AStateMakeGroup {
                targ_state: sta, ..
            } => return SomeRegion::new(&sta, &sta),
            SomeNeed::StateNotInGroup {
                targ_state: sta, ..
            } => return SomeRegion::new(&sta, &sta),
            SomeNeed::ContradictoryIntersection {
                goal_reg: g_reg, ..
            } => return g_reg.clone(),
            SomeNeed::ToRegion {
                goal_reg: g_reg, ..
            } => return g_reg.clone(),
            SomeNeed::SampleRegion {
                goal_reg: g_reg, ..
            } => return g_reg.clone(),
            SomeNeed::StateAdditionalSample {
                targ_state: sta, ..
            } => return SomeRegion::new(&sta, &sta),
            SomeNeed::SeekEdge {
                targ_state: sta, ..
            } => return SomeRegion::new(&sta, &sta),
            SomeNeed::LimitGroup {
                targ_state: sta, ..
            } => return SomeRegion::new(&sta, &sta),
            _ => panic!("target: should not be called for this need {}", self.type_string()),
        };
    } // end target

    /// Set the Domain number for a need.
    pub fn set_dom(&mut self, num: usize) {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num: dm, ..
            } => *dm = num,

            SomeNeed::StateNotInGroup {
                dom_num: dm, ..
            } => *dm = num,

            SomeNeed::ContradictoryIntersection {
                dom_num: dm, ..
            } => *dm = num,

            SomeNeed::ToRegion {
                dom_num: dm, ..
            } => *dm = num,

            SomeNeed::SampleRegion {
                dom_num: dm, ..
            } => *dm = num,

            SomeNeed::StateAdditionalSample {
                dom_num: dm, ..
            } => *dm = num,

            SomeNeed::SeekEdge {
                dom_num: dm, ..
            } => *dm = num,

            SomeNeed::LimitGroup {
                dom_num: dm, ..
            } => *dm = num,

            _ => {
                panic!("set_dom: should not be called for this need {}", self.type_string());
            }
        };
    } // end set_dom
}
