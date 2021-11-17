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

#[readonly::make]
#[derive(Serialize, Deserialize, Debug)]
pub struct SomeGroup {
    /// Region the group covers.  Formed by two Pn equal squares.
    /// All squares sampled in between are compatable.
    /// <SomeRegion>.state1 and .state2 are keys to the squares that formed the region.
    pub region: SomeRegion,
    /// Pattern Number enum One, Two or Unpredictable, shared by the two defining squares.
    pub pn: Pn,
    /// Pnc indicator
    pub pnc: bool,
    /// Rules formed by two squares.
    pub rules: RuleStore,
    /// Set to true when a state only in the group has all adjacent states checked    
    pub confirmed: bool,
    /// The state, in only one (this) group, used to confirm the group.
    pub anchor: Option<SomeState>,
    /// Mask of non-x bits checked for expansion based on available rules capability to change bits.
    pub edge_expand: SomeMask,
    /// Flag used to check for other groups that are close.
    /// So a new group is checked against all others, until no
    /// more needs are generated.
    pub pair_needs: bool,
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
            pnc: false,
            rules: ruls,
            confirmed: false,
            anchor: None,
            edge_expand: SomeMask::new_low(sta1.num_ints()),
            pair_needs: true,
        }
    }

    /// Accessor, set the edge_expand field.
    pub fn set_edge_expand(&mut self, amask: &SomeMask) {
        self.edge_expand = amask.clone();
    }

    /// Accessor set the pnc field to true
    pub fn set_pnc(&mut self) {
        self.pnc = true;
    }

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


        //        rc_str.push_str(&format!(
        //            " nxe: {}",
        //            &self.edge_expand
        //        ));

        rc_str.push_str(&format!(")"));
        rc_str
    }

    /// Return true if a square is compatible with a group.
    pub fn square_is_ok(&self, sqrx: &SomeSquare) -> bool {

        //println!("square_is_ok grp: {} sqr: {}", &self.region, &sqrx.state);
        match self.pn {
            Pn::One => match sqrx.get_pn() {
                Pn::One => {
                    return sqrx.rules.is_subset_of(&self.rules);
                }
                _ => {
                    return false;
                }
            },
            Pn::Two => match sqrx.get_pn() {
                Pn::One => {
                    if sqrx.len_results() > 1 {
                        return false;
                    }
                    return sqrx.rules.is_subset_of(&self.rules);
                }
                Pn::Two => {
                    return sqrx.rules.is_subset_of(&self.rules);
                }
                _ => {
                    return false;
                }
            },
            Pn::Unpredictable => match sqrx.get_pn() {
                Pn::Unpredictable => {
                    return true;
                }
                _ => {
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
