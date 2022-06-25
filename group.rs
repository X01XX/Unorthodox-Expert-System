//! The SomeGroup struct, which represents a group of two compatible Squares.
//!
//! This represents a group of two squares, that are
//! mutually compatible, as are any squares between them.

use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::square::SomeSquare;
use crate::squarestore::SquareStore;
use crate::state::SomeState;
//use crate::truth::Truth;

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
    pub limited: bool,
    /// The state, in only one (this) group, used to limit the group.
    pub anchor: Option<SomeState>,
    /// Flag used to check for other groups that are close.
    /// So a new group is checked against all others, until no
    /// more needs are generated.
    pub pair_needs: bool,
}

impl SomeGroup {
    /// Return a new group, given a region, RuleStore, pnc values.
    /// The RuleStore will be empty for Pn::Unpredictable squares.
    pub fn new(regionx: SomeRegion, ruls: RuleStore, pnc: bool) -> Self {
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
            region: regionx,
            pn: pnx,
            pnc: pnc,
            rules: ruls,
            limited: false,
            anchor: None,
            pair_needs: true,
        }
    }

    /// Accessor set the pnc field to true
    pub fn set_pnc(&mut self) {
        self.pnc = true;
    }

    /// Return a string representing a group.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::from("G(");
        rc_str.push_str(&format!("{}", self.region.formatted_string()));

        rc_str.push_str(&format!(", pn: {}", self.pn));

        if self.pnc {
            rc_str.push_str(&format!(", pnc: t,"));
        } else {
            rc_str.push_str(&format!(", pnc: f,"));
        }
//        if self.limited {
//            rc_str.push_str(&format!(", limited"));
//        }

        match self.pn {
            Pn::One => {
                rc_str.push_str(&format!(" {}", self.rules));
            }
            Pn::Two => {
                rc_str.push_str(&format!(" {}", self.rules));
            }
            Pn::Unpredictable => {
                rc_str.push_str(" R[Unpredictable]");
            }
        }

        match &self.anchor {
            Some(sta1) => {
                if self.limited {
                    rc_str.push_str(&format!(", limited using {}", sta1));
                } else {
                    rc_str.push_str(&format!(", limiting using {}", sta1));
                }
            }
            None => (),
        }

        rc_str.push_str(&format!(")"));
        rc_str
    }

    /// Return true if a subset square is compatible with a group.
    /// Set group pnc if needed.
    pub fn check_square(&mut self, sqrx: &SomeSquare, squares: &SquareStore) -> bool {
        //        println!(
        //            "group:check_square grp {} sqr {}",
        //            &self.region, &sqrx.state
        //        );

        // Check group definition.
        if sqrx.state == self.region.state1 || sqrx.state == self.region.state2 {
            if self.region.state1 == self.region.state2 {
                // Allow change to one-state group
                if self.pn != sqrx.results.pn {
                    if sqrx.results.pn > Pn::One && sqrx.results.pnc == false {
                        return false;
                    }
                    self.pn = sqrx.results.pn;
                    self.rules = sqrx.rules.clone();
                }
                self.pnc = sqrx.results.pnc;
                return true;
            } else if sqrx.state == self.region.state1 {
                let sqry = squares.find(&self.region.state2).unwrap();
                if sqrx.results.pn != sqry.results.pn {
                    return false;
                }
                if sqrx.results.pn == Pn::Unpredictable {
                    return true;
                }
                if self.pnc == false {
                    if sqrx.results.pnc && sqry.results.pnc {
                        self.pnc = true;
                    }
                }
                return true;
            } else { // must == state2
                let sqry = squares.find(&self.region.state1).unwrap();
                if sqrx.results.pn != sqry.results.pn {
                    return false;
                }
                if sqrx.results.pn == Pn::Unpredictable {
                    return true;
                }
                if self.pnc == false {
                    if sqrx.results.pnc && sqry.results.pnc {
                        self.pnc = true;
                    }
                }
                return true;
            }
        }

        // Check if square is compatible with group.
        if sqrx.results.pn > self.pn {
            return false;
        }

        if sqrx.results.pn != self.pn && sqrx.results.pnc {
            return false;
        }

        if self.pn == Pn::Unpredictable {
            return true;
        }

        sqrx.rules.is_subset_of(&self.rules)
    }

    /// Return true if a sample is compatible with a group.
    pub fn check_sample(&self, init: &SomeState, rslt: &SomeState) -> bool {
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
        self.limited = false;
    }

    /// Set the anchor strel231.txtate, representing a square that is only in this group,
    /// all adjacent, external squares have been tested and found to be
    /// incompatible, and the square farthest from the anchor has been sampled.
    pub fn set_anchor(&mut self, astate: SomeState) {
        self.anchor = Some(astate.clone());
        self.limited = true;
        let state2 = self.region.far_state(&astate);
        self.region = SomeRegion::new(&astate, &state2);
    }
} // end impl SomeGroup

//#[cfg(test)]
//mod tests {
//    use crate::group::SomeGroup;
//}
