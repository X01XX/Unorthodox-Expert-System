//! The SomeNeed enum, representing needs for samples, or housekeeping tasks.
//!
//! Something is logically needed, like:
//! More samples of a square(or state, or bit pattern),
//! A sample in a region that has contradictory predictions.
//! Samples to confirm a group.
//! Housekeeping needs, like confirming a group.

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::rulestore::RuleStore;
use crate::state::SomeState;

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
                act_num: _,
                goal_reg: g_reg,
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
            SomeNeed::ConfirmGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                for_group: greg,
                anchor: anc_sta,
            } => {
                if greg.is_superset_of_state(&sta) {
                    if sta == anc_sta {
                        format!(
                            "N(Dom {} Act {} Pri {} Sample anchor State {}, to confirm group {})",
                            dm, an, pri, anc_sta, greg,
                        )
                    } else {
                        format!(
                            "N(Dom {} Act {} Pri {} Sample State {}, far from {} to confirm group {})",
                            dm, an, pri, sta, anc_sta, greg,
                        )
                    }
                } else {
                    format!(
                        "N(Dom {} Act {} Pri {} Sample State {}, adj to {} to confirm group {})",
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
            } => {
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
                }
            }
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
            SomeNeed::SetGroupPnc { group_region: greg } => format!("N(Set group pnc {})", greg),
            SomeNeed::RemoveGroupAnchor { group_region: greg } => format!("N(Remove anchor for group {})", greg),
            SomeNeed::SetGroupConfirmed {
                group_region: greg,
                cstate: sta1,
            } => format!("N(set group {} confirmed by {})", greg, sta1),
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
    /// Sample a state to confirm a group.
    ConfirmGroup {
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
    /// Housekeeping, Set group pnc.
    SetGroupPnc { group_region: SomeRegion },
    /// Housekeeping, Remove group anchor.
    RemoveGroupAnchor { group_region: SomeRegion },
    /// Housekeeping, set a group to confirmed, using a state
    /// that is only in that group, has adjacent, external, dissimilar squares.
    SetGroupConfirmed {
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
                for_reg: _,
                far: _,
                num_x: _,
            } => match other {
                SomeNeed::AStateMakeGroup {
                    dom_num: dmx,
                    act_num: anx,
                    targ_state: stax,
                    for_reg: _,
                    far: _,
                    num_x: _,
                } => {
                    if dm == dmx && an == anx && sta == stax {
                        return true;
                    }
                }
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
                } => {
                    if dm == dmx && an == anx && sta == stax {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::ContradictoryIntersection {
                dom_num: dm,
                act_num: an,
                goal_reg: g_reg,
                group1: _,
                ruls1: _,
                group2: _,
                ruls2: _,
            } => match other {
                SomeNeed::ContradictoryIntersection {
                    dom_num: dmx,
                    act_num: anx,
                    goal_reg: g_regx,
                    group1: _,
                    ruls1: _,
                    group2: _,
                    ruls2: _,
                } => {
                    if dm == dmx && an == anx && *g_reg == *g_regx {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::ToRegion {
                dom_num: dm,
                act_num: _,
                goal_reg: g_reg,
            } => match other {
                SomeNeed::ToRegion {
                    dom_num: dmx,
                    act_num: _,
                    goal_reg: g_regx,
                } => {
                    if dm == dmx && *g_reg == *g_regx {
                        return true;
                    }
                }
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
                } => {
                    if dm == dmx && an == anx && *g_reg == *g_regx {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::ConfirmGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                for_group: _,
                anchor: anc_sta,
            } => match other {
                SomeNeed::ConfirmGroup {
                    dom_num: dmx,
                    act_num: anx,
                    targ_state: stax,
                    for_group: _,
                    anchor: anc_stax,
                } => {
                    if dm == dmx && an == anx && sta == stax && anc_sta == anc_stax {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::StateAdditionalSample {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                grp_reg: _,
                far: _,
            } => match other {
                SomeNeed::StateAdditionalSample {
                    dom_num: dmx,
                    act_num: anx,
                    targ_state: stax,
                    grp_reg: _,
                    far: _,
                } => {
                    if dm == dmx && an == anx && sta == stax {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::AddGroup { group_region: greg } => match other {
                SomeNeed::AddGroup {
                    group_region: gregx,
                } => {
                    if *greg == *gregx {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::SetGroupPnc { group_region: greg } => match other {
                SomeNeed::SetGroupPnc {
                    group_region: gregx,
                } => {
                    if *greg == *gregx {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::RemoveGroupAnchor { group_region: greg } => match other {
                SomeNeed::RemoveGroupAnchor {
                    group_region: gregx,
                } => {
                    if *greg == *gregx {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::SetGroupConfirmed {
                group_region: greg,
                cstate: sta1,
            } => match other {
                SomeNeed::SetGroupConfirmed {
                    group_region: gregx,
                    cstate: sta1x,
                } => {
                    if greg == gregx && sta1 == sta1x {
                        return true;
                    }
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
                } => {
                    if greg == gregx && mbitx == mbity {
                        return true;
                    }
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
                } => {
                    if dm == dmx && an == anx && greg == gregx && sta == stax {
                        return true;
                    }
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
                SomeNeed::AddSeekEdge { reg: regy } => {
                    if *regx == *regy {
                        return true;
                    }
                }
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
                dom_num: _,
                act_num: _,
                targ_state: _,
                for_reg: _,
                far: _,
                num_x: _,
            } => format!("AStateMakeGroup"),
            SomeNeed::StateNotInGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
            } => format!("StateNotInGroup"),
            SomeNeed::ContradictoryIntersection {
                dom_num: _,
                act_num: _,
                goal_reg: _,
                group1: _,
                ruls1: _,
                group2: _,
                ruls2: _,
            } => format!("ContradictoryIntersection"),
            SomeNeed::ToRegion {
                dom_num: _,
                act_num: _,
                goal_reg: _,
            } => format!("ToRegion"),
            SomeNeed::SampleRegion {
                dom_num: _,
                act_num: _,
                goal_reg: _,
            } => format!("SampleRegion"),
            SomeNeed::ConfirmGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
                for_group: _,
                anchor: _,
            } => format!("ConfirmGroup"),
            SomeNeed::StateAdditionalSample {
                dom_num: _,
                act_num: _,
                targ_state: _,
                grp_reg: _,
                far: _,
            } => format!("StateAdditionalSample"),
            SomeNeed::SeekEdge {
                dom_num: _,
                act_num: _,
                targ_state: _,
                in_group: _,
            } => format!("SeekEdge"),
            SomeNeed::AddGroup { group_region: _, } => format!("AddGroup"),
            SomeNeed::SetGroupPnc { group_region: _, } => format!("SetGroupPnc"),
            SomeNeed::RemoveGroupAnchor { group_region: _, } => format!("RemoveGroupAnchor"),
            SomeNeed::SetGroupConfirmed {
                group_region: _,
                cstate: _,
            } => format!("SetGroupConfirmed"),
            SomeNeed::SetEdgeExpand {
                group_region: _,
                edge_mask: _,
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
            SomeNeed::AStateMakeGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
                for_reg: _,
                far: _,
                num_x: _,
            } => {
                return 3;
            } // end process for AStateMakeGroup

            SomeNeed::StateNotInGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
            } => {
                return 4;
            } // end process for StateNotInGroup

            SomeNeed::ContradictoryIntersection {
                dom_num: _,
                act_num: _,
                goal_reg: _,
                group1: _,
                ruls1: _,
                group2: _,
                ruls2: _,
            } => {
                return 2;
            } // end process for ContradictoryIntersection

            SomeNeed::ToRegion {
                dom_num: _,
                act_num: _,
                goal_reg: _,
            } => {
                return 9;
            } // end process for ToRegion

            SomeNeed::SampleRegion {
                dom_num: _,
                act_num: _,
                goal_reg: _,
            } => {
                return 8;
            } // end process for SampleRegion

            SomeNeed::ConfirmGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
                for_group: _,
                anchor: _,
            } => {
                return 8;
            } // end process for ConfirmGroup

            SomeNeed::StateAdditionalSample {
                dom_num: _,
                act_num: _,
                targ_state: _,
                grp_reg: _,
                far: _,
            } => {
                return 5;
            } // end process for StateAdditionalSample
            SomeNeed::SeekEdge {
                dom_num: _,
                act_num: _,
                targ_state: _,
                in_group: _,
            } => {
                return 1;
            }
            _ => {
                return 9999;
            }
        } // end match ndx
    } // end priority

    /// Return true if a state satisfies a need.
    pub fn satisfied_by(&self, cur_state: &SomeState) -> bool {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                for_reg: _,
                far: _,
                num_x: _,
            } => {
                if cur_state == sta {
                    return true;
                }
                return false;
            } // end process a AStateMakeGroup need
            SomeNeed::StateNotInGroup {
                dom_num: _,
                act_num: _,
                targ_state: sta,
            } => {
                if cur_state == sta {
                    return true;
                }
                return false;
            } // end process a StateNotInGroup need
            SomeNeed::ContradictoryIntersection {
                dom_num: _,
                act_num: _,
                goal_reg: g_reg,
                group1: _,
                ruls1: _,
                group2: _,
                ruls2: _,
            } => {
                if g_reg.is_superset_of_state(&cur_state) {
                    return true;
                }
                return false;
            } // end process ContradictoryIntersection
            SomeNeed::ToRegion {
                dom_num: _,
                act_num: _,
                goal_reg: g_reg,
            } => {
                if g_reg.is_superset_of_state(&cur_state) {
                    return true;
                }
                return false;
            } // end process ToRegion
            SomeNeed::SampleRegion {
                dom_num: _,
                act_num: _,
                goal_reg: g_reg,
            } => {
                if g_reg.is_superset_of_state(&cur_state) {
                    return true;
                }
                return false;
            } // end process SampleRegion
            SomeNeed::StateAdditionalSample {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                grp_reg: _,
                far: _,
            } => {
                if cur_state == sta {
                    return true;
                }
                return false;
            } // end process a StateAdditionalSample need
            SomeNeed::SeekEdge {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                in_group: _,
            } => {
                if cur_state == sta {
                    return true;
                }
                return false;
            }
            SomeNeed::ConfirmGroup {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                for_group: _,
                anchor: _,
            } => {
                if cur_state == sta {
                    return true;
                }
                return false;
            } // end process a ConfirmGroup need
            _ => panic!("satisfied_by: should not be called on this need {}", self.type_string()),
        } //end match self
    } // end satisfied_by

    /// Return need action number.
    pub fn act_num(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num: _,
                act_num: an,
                targ_state: _,
                for_reg: _,
                far: _,
                num_x: _,
            } => {
                return *an;
            } // end process a AStateMakeGroup need
            SomeNeed::StateNotInGroup {
                dom_num: _,
                act_num: an,
                targ_state: _,
            } => {
                return *an;
            } // end process a StateNotInGroup need
            SomeNeed::ContradictoryIntersection {
                dom_num: _,
                act_num: an,
                goal_reg: _,
                group1: _,
                ruls1: _,
                group2: _,
                ruls2: _,
            } => {
                return *an;
            } // end process ContradictoryIntersection
            SomeNeed::ToRegion {
                dom_num: _,
                act_num: an,
                goal_reg: _,
            } => {
                return *an;
            } // end process ToRegion
            SomeNeed::SampleRegion {
                dom_num: _,
                act_num: an,
                goal_reg: _,
            } => {
                return *an;
            } // end process SampleRegion
            SomeNeed::StateAdditionalSample {
                dom_num: _,
                act_num: an,
                targ_state: _,
                grp_reg: _,
                far: _,
            } => {
                return *an;
            }
            SomeNeed::SeekEdge {
                dom_num: _,
                act_num: an,
                targ_state: _,
                in_group: _,
            } => {
                return *an;
            }
            SomeNeed::ConfirmGroup {
                dom_num: _,
                act_num: an,
                targ_state: _,
                for_group: _,
                anchor: _,
            } => {
                return *an;
            } // end process a ConfirmGroup need
            _ => panic!("act_num: not known for need {}", self.type_string()),
        } //end match self
    } // end act_num

    /// Return need domain number.
    pub fn dom_num(&self) -> usize {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                for_reg: _,
                far: _,
                num_x: _,
            } => {
                return *dm;
            } // end process a AStateMakeGroup need
            SomeNeed::StateNotInGroup {
                dom_num: dm,
                act_num: _,
                targ_state: _,
            } => {
                return *dm;
            } // end process a StateNotInGroup need
            SomeNeed::ContradictoryIntersection {
                dom_num: dm,
                act_num: _,
                goal_reg: _,
                group1: _,
                ruls1: _,
                group2: _,
                ruls2: _,
            } => {
                return *dm;
            } // end process ContradictoryIntersection
            SomeNeed::ToRegion {
                dom_num: dm,
                act_num: _,
                goal_reg: _,
            } => {
                return *dm;
            } // end process ToRegion
            SomeNeed::SampleRegion {
                dom_num: dm,
                act_num: _,
                goal_reg: _,
            } => {
                return *dm;
            } // end process SampleRegion
            SomeNeed::StateAdditionalSample {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                grp_reg: _,
                far: _,
            } => {
                return *dm;
            } // end process a StateAdditionalSample need
            SomeNeed::SeekEdge {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                in_group: _,
            } => {
                return *dm;
            }
            SomeNeed::ConfirmGroup {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                for_group: _,
                anchor: _,
            } => {
                return *dm;
            } // end process a ConfirmGroup need
            _ => panic!("dom_num: not known for need {}", self.type_string()),
        } //end match self
    } // end dom_num

    /// Return a region for a need target.
    pub fn target(&self) -> SomeRegion {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                for_reg: _,
                far: _,
                num_x: _,
            } => {
                return SomeRegion::new(&sta, &sta);
            }
            SomeNeed::StateNotInGroup {
                dom_num: _,
                act_num: _,
                targ_state: sta,
            } => {
                return SomeRegion::new(&sta, &sta);
            }
            SomeNeed::ContradictoryIntersection {
                dom_num: _,
                act_num: _,
                goal_reg: g_reg,
                group1: _,
                ruls1: _,
                group2: _,
                ruls2: _,
            } => {
                return g_reg.clone();
            }
            SomeNeed::ToRegion {
                dom_num: _,
                act_num: _,
                goal_reg: g_reg,
            } => {
                return g_reg.clone();
            }
            SomeNeed::SampleRegion {
                dom_num: _,
                act_num: _,
                goal_reg: g_reg,
            } => {
                return g_reg.clone();
            }
            SomeNeed::StateAdditionalSample {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                grp_reg: _,
                far: _,
            } => {
                return SomeRegion::new(&sta, &sta);
            }
            SomeNeed::SeekEdge {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                in_group: _,
            } => {
                return SomeRegion::new(&sta, &sta);
            }
            SomeNeed::ConfirmGroup {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                for_group: _,
                anchor: _,
            } => {
                return SomeRegion::new(&sta, &sta);
            } // end process a ConfirmGroup need
            _ => panic!("target: should not be called for this need {}", self.type_string()),
        };
    } // end target

    /// Set the Domain number for a need.
    pub fn set_dom(&mut self, num: usize) {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                for_reg: _,
                far: _,
                num_x: _,
            } => *dm = num,
            SomeNeed::StateNotInGroup {
                dom_num: dm,
                act_num: _,
                targ_state: _,
            } => {
                *dm = num;
            }
            SomeNeed::ContradictoryIntersection {
                dom_num: dm,
                act_num: _,
                goal_reg: _,
                group1: _,
                ruls1: _,
                group2: _,
                ruls2: _,
            } => {
                *dm = num;
            }
            SomeNeed::ToRegion {
                dom_num: dm,
                act_num: _,
                goal_reg: _,
            } => {
                *dm = num;
            }
            SomeNeed::SampleRegion {
                dom_num: dm,
                act_num: _,
                goal_reg: _,
            } => {
                *dm = num;
            }
            SomeNeed::StateAdditionalSample {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                grp_reg: _,
                far: _,
            } => {
                *dm = num;
            }
            SomeNeed::SeekEdge {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                in_group: _,
            } => {
                *dm = num;
            }
            SomeNeed::ConfirmGroup {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                for_group: _,
                anchor: _,
            } => {
                *dm = num;
            }
            _ => {
                panic!("set_dom: should not be called for this need {}", self.type_string());
            }
        };
    } // end set_dom
}
