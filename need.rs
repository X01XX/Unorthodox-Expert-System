// Need type

use crate::mask::SomeMask;
use crate::region::SomeRegion;
use crate::rulestore::RuleStore;
use crate::state::SomeState;

use std::fmt;

impl fmt::Display for SomeNeed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rc_str = match self {
            SomeNeed::AStateMakeGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                for_reg: freg,
                far,
                num_x: nx,
            } => format!(
                "N(Dom {} Act {} Sample State {}, far from {}, to make group {} nx: {})",
                dm, an, sta, far, freg, nx
            ),
            SomeNeed::StateNotInGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
            } => format!(
                "N(Dom {} Act {} Sample State {}, Not in a group)",
                dm, an, sta,
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
                "N(Dom {} Act {} Sample Region {} intersection of {} {} and {} {}",
                dm, an, &g_reg, &grp1, &ruls1, &grp2, &ruls2
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
                            "N(Dom {} Act {} Sample anchor State {}, to confirm group {})",
                            dm, an, anc_sta, greg,
                        )
                    } else {
                        format!(
                            "N(Dom {} Act {} Sample State {}, far from {} to confirm group {})",
                            dm, an, sta, anc_sta, greg,
                        )
                    }
                } else {
                    format!(
                        "N(Dom {} Act {} Sample State {}, adj to {} to confirm group {})",
                        dm, an, sta, anc_sta, greg,
                    )
                }
            }
            SomeNeed::StateAdditionalSample {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                grp_reg: greg,
                far: farx,
            } => format!(
                "N(Dom {} Act {} Get additional sample of state {} for group {} far {})",
                dm, an, &sta, &greg, &farx
            ),
            SomeNeed::AddGroup {
                act_num: an,
                group_region: greg,
            } => format!("N(Act: {} Create group {})", an, greg,),
            SomeNeed::SetGroupConfirmed {
                act_num: an,
                group_region: greg,
                cstate: sta1,
            } => format!("N(Act: {} set group {} confirmed by {})", an, greg, sta1,),
            SomeNeed::AStateExpandGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                base_group: greg,
            } => format!(
                "N(Dom {} Act {} Sample State {}, to expand group {})",
                dm, an, sta, greg,
            ),
            SomeNeed::ClearGroupCheckBit {
                act_num: an,
                group_region: greg,
                mbit: mbitx,
            } => format!("N(Act: {} group {} clear check bit {})", an, greg, mbitx,),
            SomeNeed::InBetween {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                in_group: greg,
            } => format!(
                "N(Dom {} Act {} Sample State {}, between {} and {})",
                dm, an, &sta, &greg.state1, &greg.state2
            ),
        }; // end match

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
pub enum SomeNeed {
    AStateMakeGroup {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
        for_reg: SomeRegion,
        far: SomeState,
        num_x: usize,
    },
    StateNotInGroup {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
    },
    ContradictoryIntersection {
        dom_num: usize,
        act_num: usize,
        goal_reg: SomeRegion,
        group1: SomeRegion,
        ruls1: RuleStore,
        group2: SomeRegion,
        ruls2: RuleStore,
    },
    ConfirmGroup {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
        for_group: SomeRegion,
        anchor: SomeState,
    },
    StateAdditionalSample {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
        grp_reg: SomeRegion,
        far: SomeState,
    },
    AddGroup {
        act_num: usize,
        group_region: SomeRegion,
    },
    SetGroupConfirmed {
        act_num: usize,
        group_region: SomeRegion,
        cstate: SomeState,
    },
    AStateExpandGroup {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
        base_group: SomeRegion,
    },
    ClearGroupCheckBit {
        act_num: usize,
        group_region: SomeRegion,
        mbit: SomeMask,
    },
    InBetween {
        dom_num: usize,
        act_num: usize,
        targ_state: SomeState,
        in_group: SomeRegion,
    },
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
            SomeNeed::AddGroup {
                act_num: an,
                group_region: greg,
            } => match other {
                SomeNeed::AddGroup {
                    act_num: anx,
                    group_region: gregx,
                } => {
                    if an == anx && *greg == *gregx {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::SetGroupConfirmed {
                act_num: an,
                group_region: greg,
                cstate: sta1,
            } => match other {
                SomeNeed::SetGroupConfirmed {
                    act_num: anx,
                    group_region: gregx,
                    cstate: sta1x,
                } => {
                    if an == anx && greg == gregx && sta1 == sta1x {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::AStateExpandGroup {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                base_group: greg,
            } => match other {
                SomeNeed::AStateExpandGroup {
                    dom_num: dmx,
                    act_num: anx,
                    targ_state: stax,
                    base_group: gregx,
                } => {
                    if dm == dmx && an == anx && greg == gregx && sta == stax {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::ClearGroupCheckBit {
                act_num: an,
                group_region: greg,
                mbit: mbitx,
            } => match other {
                SomeNeed::ClearGroupCheckBit {
                    act_num: anx,
                    group_region: gregx,
                    mbit: mbity,
                } => {
                    if an == anx && greg == gregx && mbitx == mbity {
                        return true;
                    }
                }
                _ => {}
            },
            SomeNeed::InBetween {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                in_group: greg,
            } => match other {
                SomeNeed::InBetween {
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
            // Add new needs here
        };
        false
    }
}

impl Eq for SomeNeed {}

impl SomeNeed {
    // Return a priority number for a need
    pub fn priority(&self) -> Option<usize> {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
                for_reg: _,
                far: _,
                num_x: _,
            } => {
                return Some(2);
            } // end process for AStateMakeGroup

            SomeNeed::StateNotInGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
            } => {
                return Some(3);
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
                return Some(1);
            } // end process for ContradictoryIntersection

            SomeNeed::ConfirmGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
                for_group: _,
                anchor: _,
            } => {
                return Some(6);
            } // end process for ConfirmGroup

            SomeNeed::StateAdditionalSample {
                dom_num: _,
                act_num: _,
                targ_state: _,
                grp_reg: _,
                far: _,
            } => {
                return Some(4);
            } // end process for StateAdditionalSample

            // Previously handled, but not removed from list
            SomeNeed::AddGroup {
                act_num: _,
                group_region: _,
            } => {
                return None;
            }

            // Previously handled, but not removed from list
            SomeNeed::SetGroupConfirmed {
                act_num: _,
                group_region: _,
                cstate: _,
            } => {
                return None;
            }

            SomeNeed::AStateExpandGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
                base_group: _,
            } => {
                return Some(5);
            }

            // Previously handled, but not removed from list
            SomeNeed::ClearGroupCheckBit {
                act_num: _,
                group_region: _,
                mbit: _,
            } => {
                return None;
            }
            SomeNeed::InBetween {
                dom_num: _,
                act_num: _,
                targ_state: _,
                in_group: _,
            } => {
                return Some(0);
            }
        } // end match ndx
    } // end priority

    // Return true if a state satisfies a need
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

            SomeNeed::AStateExpandGroup {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                base_group: _,
            } => {
                if cur_state == sta {
                    return true;
                }
                return false;
            }

            SomeNeed::InBetween {
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

            _ => panic!("satisfied_by should not be called on this need"),
        } //end match self
    } // end satisfied_by

    // Return need act_num
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

            SomeNeed::ConfirmGroup {
                dom_num: _,
                act_num: an,
                targ_state: _,
                for_group: _,
                anchor: _,
            } => {
                return *an;
            } // end process a ConfirmGroup need

            SomeNeed::StateAdditionalSample {
                dom_num: _,
                act_num: an,
                targ_state: _,
                grp_reg: _,
                far: _,
            } => {
                return *an;
            } // end process a StateAdditionalSample need

            SomeNeed::AddGroup {
                act_num: an,
                group_region: _,
            } => {
                return *an;
            }

            SomeNeed::SetGroupConfirmed {
                act_num: an,
                group_region: _,
                cstate: _,
            } => {
                return *an;
            }

            SomeNeed::AStateExpandGroup {
                dom_num: _,
                act_num: an,
                targ_state: _,
                base_group: _,
            } => {
                return *an;
            }

            SomeNeed::ClearGroupCheckBit {
                act_num: an,
                group_region: _,
                mbit: _,
            } => {
                return *an;
            }

            SomeNeed::InBetween {
                dom_num: _,
                act_num: an,
                targ_state: _,
                in_group: _,
            } => {
                return *an;
            }
        } //end match self
    } // end act_num

    // Return need dom_num
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

            SomeNeed::StateAdditionalSample {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                grp_reg: _,
                far: _,
            } => {
                return *dm;
            } // end process a StateAdditionalSample need

            SomeNeed::AStateExpandGroup {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                base_group: _,
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
            SomeNeed::InBetween {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                in_group: _,
            } => {
                return *dm;
            }
            _ => panic!("dom_num should not be called for this need"),
        } //end match self
    } // end dom_num

    // Return a region for a need target
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
            SomeNeed::StateAdditionalSample {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                grp_reg: _,
                far: _,
            } => {
                return SomeRegion::new(&sta, &sta);
            }
            SomeNeed::AStateExpandGroup {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                base_group: _,
            } => {
                return SomeRegion::new(&sta, &sta);
            }
            SomeNeed::InBetween {
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

            _ => panic!("target should not be called for this need"),
        };
    } // end target

    pub fn clone(&self) -> SomeNeed {
        match self {
            SomeNeed::AStateMakeGroup {
                dom_num: dm,
                act_num: an,
                targ_state: tstate,
                for_reg: freg,
                far: far_state,
                num_x: nx,
            } => {
                return SomeNeed::AStateMakeGroup {
                    dom_num: *dm,
                    act_num: *an,
                    targ_state: tstate.clone(),
                    for_reg: freg.clone(),
                    far: far_state.clone(),
                    num_x: *nx,
                };
            } // end process for AStateMakeGroup

            SomeNeed::StateNotInGroup {
                dom_num: dm,
                act_num: an,
                targ_state: tstate,
            } => {
                return SomeNeed::StateNotInGroup {
                    dom_num: *dm,
                    act_num: *an,
                    targ_state: tstate.clone(),
                };
            } // end process for StateNotInGroup

            SomeNeed::ContradictoryIntersection {
                dom_num: dm,
                act_num: an,
                goal_reg: greg,
                group1: g1reg,
                group2: g2reg,
                ruls1: r1,
                ruls2: r2,
            } => {
                return SomeNeed::ContradictoryIntersection {
                    dom_num: *dm,
                    act_num: *an,
                    goal_reg: greg.clone(),
                    group1: g1reg.clone(),
                    group2: g2reg.clone(),
                    ruls1: r1.clone(),
                    ruls2: r2.clone(),
                };
            } // end process for ContradictoryIntersection

            SomeNeed::ConfirmGroup {
                dom_num: dm,
                act_num: an,
                targ_state: tstate,
                for_group: fgrp,
                anchor: anch,
            } => {
                return SomeNeed::ConfirmGroup {
                    dom_num: *dm,
                    act_num: *an,
                    targ_state: tstate.clone(),
                    for_group: fgrp.clone(),
                    anchor: anch.clone(),
                };
            } // end process for ConfirmGroup

            SomeNeed::StateAdditionalSample {
                dom_num: dm,
                act_num: an,
                targ_state: tstate,
                far: fx,
                grp_reg: gx,
            } => {
                return SomeNeed::StateAdditionalSample {
                    dom_num: *dm,
                    act_num: *an,
                    targ_state: tstate.clone(),
                    far: fx.clone(),
                    grp_reg: gx.clone(),
                };
            } // end process for StateAdditionalSample

            // Previously handled, but not removed from list
            SomeNeed::AddGroup {
                act_num: an,
                group_region: greg,
            } => {
                return SomeNeed::AddGroup {
                    act_num: *an,
                    group_region: greg.clone(),
                };
            }

            // Previously handled, but not removed from list
            SomeNeed::SetGroupConfirmed {
                act_num: an,
                group_region: greg,
                cstate: cst,
            } => {
                return SomeNeed::SetGroupConfirmed {
                    act_num: *an,
                    group_region: greg.clone(),
                    cstate: cst.clone(),
                };
            }

            // Previously handled, but not removed from list
            SomeNeed::AStateExpandGroup {
                dom_num: dm,
                act_num: an,
                targ_state: tstate,
                base_group: bgrp,
            } => {
                return SomeNeed::AStateExpandGroup {
                    dom_num: *dm,
                    act_num: *an,
                    targ_state: tstate.clone(),
                    base_group: bgrp.clone(),
                };
            }

            // Previously handled, but not removed from list
            SomeNeed::ClearGroupCheckBit {
                act_num: an,
                group_region: greg,
                mbit: abit,
            } => {
                return SomeNeed::ClearGroupCheckBit {
                    act_num: *an,
                    group_region: greg.clone(),
                    mbit: abit.clone(),
                };
            }

            SomeNeed::InBetween {
                dom_num: dm,
                act_num: an,
                targ_state: sta,
                in_group: greg,
            } => {
                return SomeNeed::InBetween {
                    dom_num: *dm,
                    act_num: *an,
                    targ_state: sta.clone(),
                    in_group: greg.clone(),
                };
            }
        } // end match ndx
    } // end clone

    // Set the Domain num for a need
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
            SomeNeed::StateAdditionalSample {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                grp_reg: _,
                far: _,
            } => {
                *dm = num;
            }
            SomeNeed::AStateExpandGroup {
                dom_num: dm,
                act_num: _,
                targ_state: _,
                base_group: _,
            } => {
                *dm = num;
            }
            SomeNeed::InBetween {
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
                println!("need ??: {}", self);
                panic!("set_dm should not be called for this need");
            }
        };
    } // end set_dom
}
