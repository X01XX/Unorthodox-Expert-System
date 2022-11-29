//! The SomeGroup struct, which represents a group of two compatible Squares.
//!
//! This represents a group of two squares, that are
//! mutually compatible, as are any squares between them.

use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::bits::{bits_and, bits_not};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing

impl fmt::Display for SomeGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug)]
pub struct SomeGroup {
    /// Region the group covers.  Formed by two Pn-equal squares.
    /// All squares sampled in between should be compatable.
    /// <SomeRegion>.state1 and .state2 are keys to the squares that formed the region.
    /// <SomeRegion>.state1 and .state2 may be equal, that is only one square makes the region.
    /// If the squares are Pn::One, they may need more samples.
    pub region: SomeRegion,
    /// Pattern Number enum One, Two or Unpredictable, shared by the two defining squares.
    pub pn: Pn,
    /// Pnc indicator, the boolean And of the two squares pnc values.
    pub pnc: bool,
    /// Rules formed by two squares.  Will be an empty RuleStore for Pn::Unpredictable.
    pub rules: RuleStore,
    /// Set to true when a state only in the group has all adjacent states outside
    /// of the group region checked.
    /// When some external adjacent states are checked, but some are unreachable, hard to reach,
    /// or no longer reachable, the group may represent something speculative.
    pub limited: bool,
    /// The state, in only one (this) group, used to limit the group.
    pub anchor: Option<SomeState>,
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
        if ruls.is_empty() {
            pnx = Pn::Unpredictable;
        } else if ruls.len() == 2 {
            pnx = Pn::Two;
        }

        Self {
            region: regionx,
            pn: pnx,
            pnc,
            rules: ruls,
            limited: false,
            anchor: None,
        }
    }

    /// Accessor set the pnc field to true
    pub fn set_pnc(&mut self) {
        self.pnc = true;
    }

    /// Return a string representing a group.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::from("G(");
        rc_str.push_str(&self.region.formatted_string());

        let _ = write!(rc_str, ", pn: {}", self.pn);

        if self.pnc {
            rc_str.push_str(", pnc: t, ");
        } else {
            rc_str.push_str(", pnc: f, ");
        }
        //        if self.limited {
        //            rc_str.push_str(&format!(", limited"));
        //        }

        match self.pn {
            Pn::One => {
                rc_str.push_str(&self.rules.formatted_string());
            }
            Pn::Two => {
                rc_str.push_str(&self.rules.formatted_string());
            }
            Pn::Unpredictable => {
                rc_str.push_str("R[Unpredictable]");
            }
        }

        match &self.anchor {
            Some(sta1) => {
                if self.limited {
                    let _ = write!(rc_str, ", limited using {}", sta1);
                } else {
                    let _ = write!(rc_str, ", limiting using {}", sta1);
                }
            }
            None => (),
        }

        rc_str.push(')');
        rc_str
    }

    /// Return true if a subset square is compatible with a group.
    pub fn check_square(&mut self, sqrx: &SomeSquare) -> bool {
        //        println!(
        //            "group:check_square grp {} sqr {}",
        //            &self.region, &sqrx.state
        //        );

        // Check group definition.
        if sqrx.state == self.region.state1 || sqrx.state == self.region.state2 {
            if self.region.state1 == self.region.state2 {
                // Allow change of Pn::One to Pn::Unpredictable.
                if self.pn != sqrx.pn {
                    if sqrx.pn > Pn::One && !sqrx.pnc {
                        return false;
                    }
                    self.pn = sqrx.pn;
                    self.rules = sqrx.rules.clone();
                }
                self.pnc = sqrx.pnc;
                return true;
            } else {
                return sqrx.pn == self.pn;
            }
        }

        // Check if square is compatible with group.
        if sqrx.pn > self.pn {
            return false;
        }

        if sqrx.pn != self.pn && sqrx.pnc {
            return false;
        }

        if self.pn == Pn::Unpredictable {
            return true;
        }

        sqrx.rules.is_subset_of(&self.rules)
    }

    /// Return true if a sample is compatible with a group.
    pub fn check_sample(&self, init: &SomeState, rslt: &SomeState) -> bool {
        let tmp_rul = SomeRule::new(init, rslt);

        match self.pn {
            Pn::One => self.rules.is_superset_of_rule(&tmp_rul),
            Pn::Two => self.rules.is_superset_of_rule(&tmp_rul),
            Pn::Unpredictable => true,
        }
    }

    /// Clear the anchor, it is no longer only in one group,
    /// or is superceeded by a higher rated anchor.
    pub fn set_anchor_off(&mut self) {
        assert!(self.anchor.is_some());

        self.anchor = None;
        self.limited = false;
    }

    /// Set limited to false.
    pub fn set_limited_off(&mut self) {
        //        assert!(self.limited);
        self.limited = false;
    }

    /// Set limited to true.
    pub fn set_limited(&mut self) {
        //        assert!(!self.limited);

        self.limited = true;

        if let Some(astate) = &self.anchor {
            if self.region.state1 == self.region.state2 {
                if self.region.state1 != *astate {
                    self.region = SomeRegion::new(astate, astate);
                }
            } else if self.region.state1 != *astate && self.region.state2 != *astate {
                let state2 = self.region.far_state(astate);
                self.region = SomeRegion::new(astate, &state2);
            }
        }
    }

    /// Set the anchor strel231.txtate, representing a square that is only in this group,
    /// all adjacent, external squares have been tested and found to be
    /// incompatible, and the square farthest from the anchor has been sampled.
    pub fn set_anchor(&mut self, astate: &SomeState) {
        self.limited = false;
        self.anchor = Some(astate.clone());
    }

    /// Check limited setting in groups due to new bit that can change.
    pub fn check_limited(&mut self, new_chgs: &SomeChange) {
        assert!(self.limited);

        //let nonx = self.region.same_bits();
        //let positions = nonx.bits_and(new_chgs);
        
        let same_bits = self.region.same_bits();
        let one_bits = SomeMask::new(bits_and(&same_bits, &bits_and(&self.region.state1, &new_chgs.b10)));
        let zero_bits = SomeMask::new(bits_and(&same_bits, &bits_and(&bits_not(&self.region.state1), &new_chgs.b01)));
        let positions = one_bits.bits_or(&zero_bits);
        
        if !positions.is_low() {
            self.limited = false;
            //println!("resetting limit flag!");
        }
    }
} // end impl SomeGroup

//#[cfg(test)]
//mod tests {
//    use crate::group::SomeGroup;
//}
