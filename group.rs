//! The SomeGroup struct, which represents a group of two compatible Squares.
//!
//! This represents a group of two squares, that are
//! mutually compatible, as are any squares between them.

use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
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
/// A group formed by two squares.
/// Squares in between are compatible or not sampled.
pub struct SomeGroup {
    /// Region the group covers.  Formed by two Pn-equal squares.
    /// All squares sampled in between should be compatable.
    /// [SomeRegion].state1 and .state2 are keys to the squares that formed the region.
    /// [SomeRegion].state1 and .state2 may be equal, that is only one square makes the region.
    /// If the squares are Pn::One, they may need more samples.
    pub region: SomeRegion,
    /// Pattern Number enum One, Two or Unpredictable, shared by the two defining squares.
    pub pn: Pn,
    /// Pnc indicator, the boolean And of the two squares pnc values.
    pub pnc: bool,
    /// Rules formed by two squares.  Will be None for Pn::Unpredictable.
    pub rules: Option<RuleStore>,
    /// Set to true when a state only in the group has all adjacent states outside
    /// of the group region checked.
    /// When some external adjacent states are checked, but some are unreachable, hard to reach,
    /// or no longer reachable, the group may represent something speculative.
    pub limited: bool,
    /// The state, in only one (this) group, used to limit the group.
    pub anchor: Option<SomeState>,
    /// Number adjacent squares used to limit a group.
    pub anchor_num: usize,
}

impl SomeGroup {
    /// Return a new group, given a region, RuleStore, pnc values.
    /// The RuleStore will be empty for Pn::Unpredictable squares.
    pub fn new(regionx: SomeRegion, ruls: Option<RuleStore>, pnc: bool) -> Self {
        //println!(
        //  "adding group {}",
        //  SomeRegion::newif additions.is_not_low() {
        //);
        let mut pnx = Pn::One;
        if ruls.is_none() {
            pnx = Pn::Unpredictable;
        } else if let Some(xruls) = ruls.as_ref() {
            assert!(!xruls.is_empty() && xruls.len() < 3);
            if xruls.len() == 2 {
                pnx = Pn::Two;
            }
        }

        Self {
            region: regionx,
            pn: pnx,
            pnc,
            rules: ruls,
            limited: false,
            anchor: None,
            anchor_num: 0,
        }
    }

    /// Accessor set the pnc field to true.
    /// Reform region, if needed.
    pub fn set_pnc(&mut self) {
        if self.pnc {
            println!("Group {} pnc already true", self.region);
            return;
        }

        self.pnc = true;
        if self.region.states.len() > 2 {
            self.region = SomeRegion::new(vec![
                self.region.state1().clone(),
                self.region.state2().clone(),
            ]);
        }
        println!("  Setting group {} pnc to true", self.region);
    }

    /// Return a string representing a group.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::from("G(");
        rc_str.push_str(&self.region.formatted_string());

        rc_str.push_str(&format!(", pn: {}", self.pn));

        rc_str.push_str(if self.pnc { ", pnc: t, " } else { ", pnc: f, " });

        match self.pn {
            Pn::One => {
                rc_str.push_str(&self.rules.as_ref().expect("SNH").formatted_string());
            }
            Pn::Two => {
                rc_str.push_str(&self.rules.as_ref().expect("SNH").formatted_string());
            }
            Pn::Unpredictable => {
                rc_str.push_str("R[Unpredictable]");
            }
        }

        match &self.anchor {
            Some(sta1) => {
                if self.limited {
                    rc_str.push_str(&format!(
                        ", limited using {sta1} num adj {}",
                        self.anchor_num
                    ));
                } else {
                    rc_str.push_str(&format!(", limiting using {sta1}"));
                }
            }
            None => (),
        }

        rc_str.push(')');
        rc_str
    }

    /// Return true if a subset square is compatible with a group.
    pub fn check_subset_square(&self, sqrx: &SomeSquare) -> bool {
        assert!(self.region.is_superset_of_state(&sqrx.state));
        //println!(
        //  "group:check_square grp {} sqr {}",
        //  &self.region, &sqrx.state
        //);

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

        self.rules
            .as_ref()
            .expect("SNH")
            .is_superset_of(sqrx.rules.as_ref().expect("SNH"))
    }

    /// Return true if a sample is compatible with a group.
    pub fn check_subset_sample(&self, smpl: &SomeSample) -> bool {
        assert!(self.region.is_superset_of_state(&smpl.initial));

        let tmp_rul = smpl.rule();

        match self.pn {
            Pn::One => self
                .rules
                .as_ref()
                .expect("SNH")
                .is_superset_of_rule(&tmp_rul),
            Pn::Two => self
                .rules
                .as_ref()
                .expect("SNH")
                .is_superset_of_rule(&tmp_rul),
            Pn::Unpredictable => true,
        }
    }

    /// Clear the anchor, it is no longer only in one group,
    /// or is superceeded by a higher rated anchor.
    pub fn set_anchor_off(&mut self) {
        assert!(self.anchor.is_some());

        self.anchor = None;

        self.limited = false;
        self.anchor_num = 0;
    }

    /// Set limited to false.
    pub fn set_limited_off(&mut self) {
        //        assert!(self.limited);
        self.limited = false;
        self.anchor_num = 0;
    }

    /// Set limited to true.
    pub fn set_limited(&mut self, num: usize) {
        //        assert!(!self.limited);

        self.limited = true;
        self.anchor_num = num;

        if let Some(astate) = &self.anchor {
            if self.region.state1() != astate && self.region.state2() != astate {
                self.region =
                    SomeRegion::new(vec![astate.clone(), self.region.state_far_from(astate)]);
            }
        }
    }

    /// Set the anchor state, representing a square that is only in this group,
    /// all adjacent, external squares have been tested and found to be
    /// incompatible, and the square farthest from the anchor has been sampled.
    pub fn set_anchor(&mut self, astate: &SomeState) {
        self.anchor = Some(astate.clone());

        self.limited = false;
        self.anchor_num = 0;
    }

    /// Check limited setting in groups due to new bit that can change.
    /// If the group region has a non-X bit position matching a new bit position that can change,
    /// set the limited indicator to false.
    pub fn check_limited(&mut self, change_mask: &SomeMask) {
        assert!(self.limited);

        let edge_mask = self.region.edge_mask();

        let positions = edge_mask.bitwise_and(change_mask);

        //        println!(
        //            "Check limited setting for {} with {} positions {}",
        //            self.region, new_chgs, positions
        //        );

        if !positions.is_low() {
            self.limited = false;
            self.anchor_num = 0;
            //println!("resetting limit flag!");
        }
    }

    // Return true if a group region is a superset of a given state.
    pub fn is_superset_of_state(&self, stax: &SomeState) -> bool {
        self.region.is_superset_of_state(stax)
    }

    // Return true if a group region is a superset of a given region.
    pub fn is_superset_of(&self, regx: &SomeRegion) -> bool {
        self.region.is_superset_of(regx)
    }
} // end impl SomeGroup

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::rule::SomeRule;

    #[test]
    fn check_subset_sample() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);
        let tmp_rul = SomeRule::new(&tmp_sta, &tmp_sta);

        let rules = RuleStore::new(vec![tmp_rul.new_from_string("10/x1/x0/00")?]);
        let regx = tmp_reg.new_from_string("r1xx0")?;

        let grpx = SomeGroup::new(regx, Some(rules), true);

        let initial = tmp_sta.new_from_string("s0b1100")?;
        let result = tmp_sta.new_from_string("s0b0100")?;

        if !grpx.check_subset_sample(&SomeSample::new(initial, 0, result)) {
            return Err(format!("check_subset_sample: test 1 failed!"));
        }

        Ok(())
    }

    #[test]
    fn check_subset_square() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(1));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);
        let tmp_rul = SomeRule::new(&tmp_sta, &tmp_sta);

        // Test if sqrx.pn > self.pn
        let rules = RuleStore::new(vec![tmp_rul.new_from_string("10/x1/x0/00")?]);
        let regx = tmp_reg.new_from_string("r1xx0")?;

        let grpx = SomeGroup::new(regx, Some(rules), true); // Pn::One, pnc == true.

        let mut sqrx = SomeSquare::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        );
        sqrx.add_result(tmp_sta.new_from_string("s0b0101")?); // Pn::Two, pnc == false.

        if grpx.check_subset_square(&sqrx) {
            return Err(format!("check_subset_square: test 1 failed!"));
        }

        // Test if sqrx.pn != self.pn && sqrx.pnc
        let rules = RuleStore::new(vec![
            tmp_rul.new_from_string("10/x1/x0/00")?,
            tmp_rul.new_from_string("10/x1/x0/01")?,
        ]);

        let regx = tmp_reg.new_from_string("r1xx0")?;

        let grpx = SomeGroup::new(regx, Some(rules), true); // Pn::Two, pnc == true.

        let mut sqrx = SomeSquare::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        );
        sqrx.add_result(tmp_sta.new_from_string("s0b0100")?); // pn = Pn::One, pnc = true.

        if grpx.check_subset_square(&sqrx) {
            return Err(format!("check_subset_square: test 2 failed!"));
        }

        // Test if self.pn == Pn::Unpredictable
        let rules = None;

        let regx = tmp_reg.new_from_string("r1xx0")?;

        let grpx = SomeGroup::new(regx, rules, true);

        let sqrx = SomeSquare::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        );

        if !grpx.check_subset_square(&sqrx) {
            return Err(format!("check_subset_square: test 3 failed!"));
        }

        // Test self.rules.is_superset_of(&sqrx.rules)
        let rules = RuleStore::new(vec![tmp_rul.new_from_string("10/x1/x0/00")?]);

        let regx = tmp_reg.new_from_string("r1xx0")?;

        let grpx = SomeGroup::new(regx, Some(rules), true);

        let sqrx = SomeSquare::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        );

        if !grpx.check_subset_square(&sqrx) {
            return Err(format!("check_subset_square: test 4a failed!"));
        }

        let sqrx = SomeSquare::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0101")?,
        );

        if grpx.check_subset_square(&sqrx) {
            return Err(format!("check_subset_square: test 4b failed!"));
        }
        Ok(())
    }
} // end tests
