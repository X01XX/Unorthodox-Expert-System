//! The SomeGroup struct, which represents a group of two compatible Squares.
//!
//! This represents a group of two squares, that are
//! mutually compatible, as are any squares between them.

use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::square::SomeSquare;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct SomeGroup {
    /// Region the group covers.  Formed by two Pn equal squares.
    /// All squares sampled in between are compatable.
    /// <SomeRegion>.state1 and .state2 are keys to the squares that formed the region.
    region: SomeRegion,
    /// Pattern Number enum One, Two or Unpredictable, shared by the two defining squares.
    pn: Pn,
    /// Rules formed by two squares.
    rules: RuleStore,
    /// Set to false to "delete" the group from the parent DomainStore vector.
    /// To minimize vector copying.
    active: bool,
    /// Set to true when a state only in the group has all adjacent states checked    
    confirmed: bool,
    /// The state, in only one (this) group, used to confirm the group.
    anchor: Option<SomeState>,
    /// Mask of non-x bits checked for expansion dur to available rules.
    edge_expand: SomeMask,
    /// Flag used to check for other groups that are close.
    /// So a new group is checked against all others, until no
    /// more needs are generated.
    pair_needs: bool,
}

impl SomeGroup {
    /// Return a new group, using two states, representing two squares, and
    /// their combined rules.  The RuleStore will be empty for Pn::Unpredictable squares.
    pub fn new(sta1: &SomeState, sta2: &SomeState, ruls: RuleStore) -> Self {
        //        println!(
        //            "adding group {}",
        //            SomeRegion::new(&sta1, &sta2)
        //        );
        let mut pnx = Pn::One;
        if ruls.len() == 0 {
            pnx = Pn::Unpredictable;
        } else if ruls.len() == 2 {
            pnx = Pn::Two;
        }

        Self {
            region: SomeRegion::new(&sta1, &sta2), // Region the group covers, and the states sampled that are joined
            pn: pnx,
            rules: ruls,
            active: true,
            confirmed: false,
            anchor: None,
            edge_expand: SomeMask::new_low(sta1.num_ints()),
            pair_needs: true,
        }
    }

    /// Accessor, return a read-only reference to the region field.
    pub fn get_region(&self) -> &SomeRegion {
        &self.region
    }
    
    /// Accessor, return a read-only reference to the pn field.
    pub fn get_pn(&self) -> &Pn {
        &self.pn
    }

    /// Accessor, return a read-only reference to the rules field.
    pub fn get_rules(&self) -> &RuleStore {
        &self.rules
    }

    /// Accessor, return the value of the active field.
    pub fn get_active(&self) -> bool {
        self.active
    }

    /// Accessor, return the value of the confirmed field.
    pub fn get_confirmed(&self) -> bool {
        self.confirmed
    }

    /// Accessor, return a read-only reference to the anchor field.
    pub fn get_anchor(&self) -> &Option<SomeState> {
        &self.anchor
    }

    /// Accessor, return a read-only reference to the edge_expand field.
    pub fn get_edge_expand(&self) -> &SomeMask {
        &self.edge_expand
    }

    /// Accessor, set the edge_expand field.
    pub fn set_edge_expand(&mut self, amask: &SomeMask) {
        self.edge_expand = amask.clone();
    }

    /// Set a one bit in edge_expand to zero.  The group cannot
    /// expand on that edge.
//    pub fn check_off_expand_bit(&mut self, boff: &SomeMask) {
//        //println!("*** group {} checking off expand bit {}", &self.region, &boff);
//        self.edge_expand = self.edge_expand.m_and(&boff.m_not());
//    }

    /// Return a string representing a group.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::from("G(");
        rc_str.push_str(&format!("{}", self.region.formatted_string()));

        //        rc_str.push_str(&format!(
        //            " 1: {} 2: {}",
        //            self.region.state1, self.region.state2
        //        ));

        rc_str.push_str(&format!(", pn: {}", self.pn));

        match self.pn {
            Pn::One => {
                rc_str.push_str(&format!(", {}", self.rules));
            }
            Pn::Two => {
                rc_str.push_str(&format!(", {}", self.rules));
            }
            Pn::Unpredictable => {
                rc_str.push_str(", [Unpredictable]");
            }
        }

        if self.active == false {
            rc_str.push_str(", INactive");
        } else {
            match &self.anchor {
                Some(sta1) => {
                    if self.confirmed {
                        rc_str.push_str(&format!(", confirmed using {}", sta1));
                    } else {
                        rc_str.push_str(&format!(", confirming using {}", sta1));
                    }
                }
                None => {}
            }
        }

        //        rc_str.push_str(&format!(
        //            " nxe: {}",
        //            &self.edge_expand
        //        ));

        //rc_str.push_str(&format!("){}", self.active));
        rc_str.push_str(&format!(")"));
        rc_str
    }

    /// Inactivate a group, rather than deleting it from a vector.
    /// It may be replaced by a new, active, group.
    pub fn inactivate(&mut self, dom: usize, act: usize) -> bool {
        println!(
            "\nDom {} Act {} Deleting group {}",
            dom,
            act,
            self.region.formatted_string()
        );
        self.active = false;
        true
    }

    /// Return true if a square is compatible with a group.
    pub fn square_is_ok(&self, sqrx: &SomeSquare) -> bool {
        //println!("square_is_ok grp: {} sqr: {}", &self.region, &sqrx.state);
        match self.pn {
            Pn::One => match sqrx.get_pn() {
                Pn::One => {
                    //println!("square_is_ok at One One");
                    return sqrx.get_rules().is_subset_of(&self.rules);
                }
                _ => {
                    //println!("square_is_ok at One Other");
                    return false;
                }
            },
            Pn::Two => match sqrx.get_pn() {
                Pn::One => {
                    //println!("square_is_ok at Two One len = {}", sqrx.len_results());
                    if sqrx.len_results() > 1 {
                        return false;
                    }
                    return sqrx.get_rules().is_subset_of(self.get_rules());
                }
                Pn::Two => {
                    //println!("square_is_ok at Two Two");
                    return sqrx.get_rules().is_subset_of(&self.rules);
                }
                _ => {
                    //println!("square_is_ok at Two Other");
                    return false;
                }
            },
            Pn::Unpredictable => match sqrx.get_pn() {
                Pn::Unpredictable => {
                    //println!("square_is_ok at U U");
                    return true;
                }
                _ => {
                    //println!("square_is_ok at U Other");
                    if sqrx.get_pnc() {
                        return false;
                    } else {
                        return true;
                    }
                }
            },
        }
    }

    /// Return true if a sample is compatible with a group.
    pub fn sample_is_ok(&self, init: &SomeState, rslt: &SomeState) -> bool {
        let tmp_rul = SomeRule::new(&init, &rslt);

        match self.pn {
            Pn::One => {
                return self.rules.is_superset_of_rule(&tmp_rul);
            }
            Pn::Two => {
                return self.rules.is_superset_of_rule(&tmp_rul);
            }
            Pn::Unpredictable => {
                return true;
            }
        }
    }

    /// Clear the anchor, it is no longer only in one group,
    /// or is superceeded by a higher rated anchor.
    pub fn set_anchor_off(&mut self) {
        self.anchor = None;
        self.confirmed = false;
    }

    /// Set the anchor state, representing a square that is only in this group,
    /// all adjacent, external squares have been tested and found to be
    /// incompatible, and the square farthest from the anchor has been sampled.
    pub fn set_anchor(&mut self, astate: SomeState) {
        self.anchor = Some(astate.clone());
        self.confirmed = true;
        let state2 = self.region.far_state(&astate);
        self.region = SomeRegion::new(&astate, &state2);
    }
}
