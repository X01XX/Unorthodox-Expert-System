//! The SomeNeed enum, representing needs for samples, or housekeeping tasks.
//!
//! Something is logically needed, like:
//! More samples of a square(or state, or bit pattern),
//! A sample in a region that has contradictory predictions.
//! Samples to limit a group.
//! Housekeeping needs, like adding a group.

use crate::region::SomeRegion;
use crate::rulestore::RuleStore;
use crate::state::SomeState;
use crate::statescorr::StatesCorr;
use crate::target::ATarget;

use std::fmt;

impl fmt::Display for SomeNeed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

#[derive(Debug)]
/// Enums that represent a number of different needs.
pub enum SomeNeed {
    /// Seek a sample in a region that other groups do not cover.
    StateInRemainder {
        dom_id: usize,
        act_id: usize,
        target: ATarget,
        priority: usize,
    },
    /// Sample a state that is not in a group.
    StateNotInGroup {
        dom_id: usize,
        act_id: usize,
        target: ATarget,
        priority: usize,
    },
    /// Get an additional sample of a state, to confirm a group.
    ConfirmGroup {
        dom_id: usize,
        act_id: usize,
        target: ATarget,
        priority: usize,

        grp_reg: SomeRegion,
    },
    /// Sample a state to resolve a contradictory intersection of two groups.
    ContradictoryIntersection {
        dom_id: usize,
        act_id: usize,
        target: ATarget,
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
        target: ATarget,
        priority: usize,

        for_group: SomeRegion,
        anchor: SomeState,
    },
    /// Sample a external state adjacent to a group anchor, to limit the group.
    LimitGroupAdj {
        dom_id: usize,
        act_id: usize,
        target: ATarget,
        priority: usize,
        for_group: SomeRegion,
        anchor: SomeState,
    },
    /// Sample a internal state adjacent to a group anchor, to confirm the group.
    ConfirmGroupAdj {
        dom_id: usize,
        act_id: usize,
        target: ATarget,
        priority: usize,
        for_group: SomeRegion,
        anchor: SomeState,
    },
    /// Move all current domain states out of a SelectRegion, to non-negative regions.
    ExitSelectRegions { target: ATarget, priority: usize },
    /// Move all current domain states to a SelectRegion.
    ToSelectRegions {
        target: ATarget,
        priority: usize,
        times_visited: usize,
    },
    /// Housekeeping, add a group.
    AddGroup {
        group_region: SomeRegion,
        rules: Option<RuleStore>,
        pnc: bool,
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
            Self::ToSelectRegions { .. } => "ToSelectRegions",
            Self::ExitSelectRegions { .. } => "ExitSelectRegions",
        }
    }

    /// Set priority number for a need.  Lower is more important.
    /// Don't use number zero!
    pub fn add_priority_base(&mut self) {
        match self {
            // By ascending priority number.
            Self::ContradictoryIntersection { priority, .. } => *priority += 200,
            Self::ExitSelectRegions { priority, .. } => *priority += 300,
            Self::ConfirmGroup { priority, .. } => *priority += 400,
            Self::LimitGroup { priority, .. } => *priority += 500,
            Self::LimitGroupAdj { priority, .. } => *priority += 600,
            Self::ConfirmGroupAdj { priority, .. } => *priority += 700,
            Self::StateNotInGroup { priority, .. } => *priority += 800,
            Self::ToSelectRegions { priority, .. } => *priority += 900,
            Self::StateInRemainder { priority, .. } => *priority += 1000,
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
            Self::ExitSelectRegions { priority, .. } => *priority,
            Self::ConfirmGroup { priority, .. } => *priority,
            Self::LimitGroup { priority, .. } => *priority,
            Self::LimitGroupAdj { priority, .. } => *priority,
            Self::ConfirmGroupAdj { priority, .. } => *priority,
            Self::StateNotInGroup { priority, .. } => *priority,
            Self::ToSelectRegions { priority, .. } => *priority,
            // Some needs should have a higher priority number compared to ToSelectRegions.
            Self::StateInRemainder { priority, .. } => *priority,
            _ => panic!(
                "SomeNeed::priority should not be called for the {} need.",
                self.name()
            ),
        } // end match ndx
    } // end priority

    /// Return true if a state satisfies a need.
    pub fn satisfied_by(&self, states: &StatesCorr) -> bool {
        match self.target() {
            ATarget::State { state: stax } => states[self.dom_id().expect("SNH")] == *stax,
            ATarget::Region { region: regx } => {
                regx.is_superset_of(&states[self.dom_id().expect("SNH")])
            }
            ATarget::DomainRegions { regions: regsx } => regsx.is_superset_states(states),
            ATarget::SelectRegions { select: selx } => selx.regions.is_superset_states(states),
        }
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
            Self::StateInRemainder { act_id, .. } => *act_id,
            _ => panic!(
                "SomeNeed::act_id should not be called for the {} need.",
                self.name()
            ),
        } //end match self
    } // end act_id

    /// Return need domain number.
    /// Two needs may cover GT 1 domain, so return None.
    pub fn dom_id(&self) -> Option<usize> {
        match self {
            Self::ConfirmGroup { dom_id, .. } => Some(*dom_id),
            Self::ContradictoryIntersection { dom_id, .. } => Some(*dom_id),
            Self::LimitGroup { dom_id, .. } => Some(*dom_id),
            Self::LimitGroupAdj { dom_id, .. } => Some(*dom_id),
            Self::ConfirmGroupAdj { dom_id, .. } => Some(*dom_id),
            Self::StateInRemainder { dom_id, .. } => Some(*dom_id),
            Self::StateNotInGroup { dom_id, .. } => Some(*dom_id),
            _ => None,
        } //end match self
    } // end dom_id

    /// Return a String representation of SomeNeed.
    fn formatted_str(&self) -> String {
        match self {
            Self::AddGroup {
                group_region,
                rules,
                pnc,
            } => {
                if let Some(xrules) = rules {
                    format!("N(Create group from {group_region} {xrules} pnc {pnc})")
                } else {
                    format!("N(Create group from {group_region} No rules pnc {pnc})")
                }
            }
            Self::ConfirmGroup {
                dom_id,
                act_id,
                target,
                grp_reg,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Get additional sample of state {target} to confirm group {grp_reg})")
            }
            Self::ContradictoryIntersection {
                dom_id,
                act_id,
                target,
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
                "N(Dom {dom_id} Act {act_id} Pri {priority} Sample Region {target} intersection of {group1} {ruls1_str} and {group2} {ruls2_str})")
            }
            Self::LimitGroup {
                dom_id,
                act_id,
                target,
                for_group,
                anchor,
                priority,
                ..
            } => match target {
                ATarget::State { state: stax } => {
                    if stax == anchor {
                        format!(
                            "N(Dom {dom_id} Act {act_id} Pri {priority} Sample anchor State {anchor}, to limit group {for_group})")
                    } else {
                        format!(
                            "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target}, far from anchor {anchor} to define group {for_group})")
                    }
                }
                _ => panic!("Should not happen"),
            },
            Self::LimitGroupAdj {
                dom_id,
                act_id,
                target,
                for_group,
                anchor,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target}, adj to {anchor} to limit group {for_group})")
            }
            Self::ConfirmGroupAdj {
                dom_id,
                act_id,
                target,
                for_group,
                anchor,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target}, adj to {anchor} to confirm group {for_group})")
            }
            Self::StateInRemainder {
                dom_id,
                act_id,
                target,
                priority,
            } => {
                format!(
                "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target} in remainder)")
            }
            Self::StateNotInGroup {
                dom_id,
                act_id,
                target,
                priority,
            } => {
                format!(
                "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target} not in a group)")
            }
            Self::ToSelectRegions {
                target,
                priority,
                times_visited,
            } => {
                format!(
                    "N(Pri {priority} To Select Regions {target}, times visited: {times_visited})"
                )
            }
            Self::ExitSelectRegions { target, priority } => {
                format!("N(Pri {priority} Exit Select Regions to {target})")
            }
        } // end match
    }

    /// Return the distance between a need target and the current state.
    pub fn distance(&self, states: &StatesCorr) -> usize {
        match self.target() {
            ATarget::State { state: stax } => states[self.dom_id().expect("SNH")].distance(stax),
            ATarget::Region { region: regx } => regx.distance(&states[self.dom_id().expect("SNH")]),
            ATarget::DomainRegions { regions: regsx } => regsx.distance_states(states),
            ATarget::SelectRegions { select: selx } => selx.regions.distance_states(states),
        }
    } // end distance

    /// Return target of A NEED.
    pub fn target(&self) -> &ATarget {
        match self {
            Self::ConfirmGroup { target, .. } => target,
            Self::ContradictoryIntersection { target, .. } => target,
            Self::LimitGroup { target, .. } => target,
            Self::LimitGroupAdj { target, .. } => target,
            Self::ConfirmGroupAdj { target, .. } => target,
            Self::StateInRemainder { target, .. } => target,
            Self::StateNotInGroup { target, .. } => target,
            Self::ExitSelectRegions { target, .. } => target,
            Self::ToSelectRegions { target, .. } => target,
            _ => panic!("SomeNeed::distance: Unrecognized need {}", self),
        } //end match self
    } // end target
} // End impl SomeNeed
