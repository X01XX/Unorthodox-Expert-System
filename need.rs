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

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeNeed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

#[derive(Debug, Serialize, Deserialize)]
/// Enums that represent a number of different needs.
pub enum SomeNeed {
    /// Sample a state that is not in a group, often the current state.
    ///
    /// Used to bootstrap the formation of single-result groups, defined by squares with a low number of samples.
    ///
    /// Some single-result low-number-samples-per-defining-state groups, will be invalidated upon use,
    /// because further sampling would reveal a more complicated rule sructure, or the defining states may become a two-result state,
    /// or an unpredictable state.
    ///
    /// Those groups that work will allow changing the current state to sample, and resample,
    /// states as needed.
    ///
    /// This is also used to resample states that produce more than one result, to establish predictability, or unpredictability,
    /// before creating a group defined by such states.
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
    /// Move all current domain states out of a SelectRegion, to non-negative regions.
    ExitSelectRegions { target: ATarget, priority: usize },
    /// Move all current domain states to a SelectRegion.
    ToSelectRegions {
        target: ATarget,
        priority: usize,
        times_visited: usize,
    },
    /// For a non-adjacent incompatible pair, find a closer pair.
    CloserIP {
        dom_id: usize,
        act_id: usize,
        target: ATarget,
        priority: usize,
        unknown_region: SomeRegion,
    },
    /// Get sample of a state to fill a region, where a square in only one region has been identified.
    FindSimilarityTo {
        dom_id: usize,
        act_id: usize,
        target: ATarget,
        priority: usize,
        to_state: SomeState,
        in_reg: SomeRegion,
    },
}

impl SomeNeed {
    /// Return a need name string.
    pub fn name(&self) -> &str {
        match self {
            Self::ConfirmGroup { .. } => "ConfirmGroup",
            Self::CloserIP { .. } => "CloserIP",
            Self::FindSimilarityTo { .. } => "FindSimilarityTo",
            Self::ContradictoryIntersection { .. } => "ContradictoryIntersection",
            Self::LimitGroup { .. } => "LimitGroup",
            Self::LimitGroupAdj { .. } => "LimitGroupAdj",
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
            Self::CloserIP { priority, .. } => *priority += 100,
            Self::FindSimilarityTo { priority, .. } => *priority += 125,
            Self::ContradictoryIntersection { priority, .. } => *priority += 200,
            Self::StateNotInGroup { priority, .. } => *priority += 250,
            Self::ExitSelectRegions { priority, .. } => *priority += 300,
            Self::ConfirmGroup { priority, .. } => *priority += 400,
            Self::LimitGroup { priority, .. } => *priority += 500,
            Self::LimitGroupAdj { priority, .. } => *priority += 600,
            Self::ToSelectRegions { priority, .. } => *priority += 900,
        } // end match ndx
    } // end caLc_priority

    /// Return a need priority.
    pub fn priority(&self) -> usize {
        match self {
            // By ascending priority number.
            Self::ContradictoryIntersection { priority, .. } => *priority,
            Self::ExitSelectRegions { priority, .. } => *priority,
            Self::CloserIP { priority, .. } => *priority,
            Self::FindSimilarityTo { priority, .. } => *priority,
            Self::ConfirmGroup { priority, .. } => *priority,
            Self::LimitGroup { priority, .. } => *priority,
            Self::LimitGroupAdj { priority, .. } => *priority,
            Self::StateNotInGroup { priority, .. } => *priority,
            Self::ToSelectRegions { priority, .. } => *priority,
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
            Self::CloserIP { act_id, .. } => *act_id,
            Self::FindSimilarityTo { act_id, .. } => *act_id,
            Self::ContradictoryIntersection { act_id, .. } => *act_id,
            Self::LimitGroup { act_id, .. } => *act_id,
            Self::LimitGroupAdj { act_id, .. } => *act_id,
            Self::StateNotInGroup { act_id, .. } => *act_id,
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
            Self::CloserIP { dom_id, .. } => Some(*dom_id),
            Self::FindSimilarityTo { dom_id, .. } => Some(*dom_id),
            Self::ContradictoryIntersection { dom_id, .. } => Some(*dom_id),
            Self::LimitGroup { dom_id, .. } => Some(*dom_id),
            Self::LimitGroupAdj { dom_id, .. } => Some(*dom_id),
            Self::StateNotInGroup { dom_id, .. } => Some(*dom_id),
            _ => None,
        } //end match self
    } // end dom_id

    /// Return a String representation of SomeNeed.
    fn formatted_str(&self) -> String {
        match self {
            Self::ConfirmGroup {
                dom_id,
                act_id,
                target,
                grp_reg,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Get additional sample of state {target} to confirm group {grp_reg})"
                )
            }
            Self::CloserIP {
                dom_id,
                act_id,
                target,
                unknown_region,
                priority,
                ..
            } => match target {
                ATarget::State { state } => format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Get additional sample of state {state} to find closer incompatible pair within {unknown_region})"
                ),
                ATarget::Region { region } => format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Get sample in {region} to find closer incompatible pair within {unknown_region})"
                ),
                _ => panic!("SNH"),
            },
            Self::FindSimilarityTo {
                dom_id,
                act_id,
                target,
                to_state,
                in_reg,
                priority,
                ..
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Get sample of state {target} to find similarity to {to_state} in region {in_reg})"
                )
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
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Sample Region {target} intersection of {group1} {ruls1_str} and {group2} {ruls2_str})"
                )
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
                            "N(Dom {dom_id} Act {act_id} Pri {priority} Sample anchor State {anchor}, to limit group {for_group})"
                        )
                    } else {
                        format!(
                            "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target}, far from anchor {anchor} to define group {for_group})"
                        )
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
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target}, adj to {anchor} to limit group {for_group})"
                )
            }
            Self::StateNotInGroup {
                dom_id,
                act_id,
                target,
                priority,
            } => {
                format!(
                    "N(Dom {dom_id} Act {act_id} Pri {priority} Sample State {target} not in a group)"
                )
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
            Self::CloserIP { target, .. } => target,
            Self::FindSimilarityTo { target, .. } => target,
            Self::ContradictoryIntersection { target, .. } => target,
            Self::LimitGroup { target, .. } => target,
            Self::LimitGroupAdj { target, .. } => target,
            Self::StateNotInGroup { target, .. } => target,
            Self::ExitSelectRegions { target, .. } => target,
            Self::ToSelectRegions { target, .. } => target,
        } //end match self
    } // end target
} // End impl SomeNeed
