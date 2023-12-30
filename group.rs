//! The SomeGroup struct, which represents a group of two compatible Squares.
//!
//! This represents a group of two squares, that are
//! mutually compatible, as are any squares between them.

use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::AccessStates;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
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
    pub expand: Option<SomeRegion>,
}

impl SomeGroup {
    /// Return a new group, given a region, RuleStore, pnc values.
    /// The RuleStore will be empty for Pn::Unpredictable squares.
    pub fn new(
        regionx: SomeRegion,
        ruls: Option<RuleStore>,
        pnc: bool,
        expand: Option<SomeRegion>,
    ) -> Self {
        //println!(
        //  "creating group {}",
        //   regionx
        //);
        let mut pnx = Pn::One;
        if ruls.is_none() {
            pnx = Pn::Unpredictable;
        } else if let Some(xruls) = ruls.as_ref() {
            assert!(!xruls.is_empty() && xruls.is_valid());
            assert!(regionx == xruls.initial_region());
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
            expand,
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
        println!("  Group  {} confirmed.", self.region);
    }

    /// Return a string representing a group.
    fn formatted_string(&self) -> String {
        let mut rc_str = String::from("G(");
        rc_str.push_str(&self.region.to_string());

        rc_str.push_str(&format!(", pn: {}", self.pn));

        rc_str.push_str(if self.pnc { ", pnc: t, " } else { ", pnc: f, " });

        match self.pn {
            Pn::One => {
                rc_str.push_str(&self.rules.as_ref().expect("SNH").to_string());
            }
            Pn::Two => {
                rc_str.push_str(&self.rules.as_ref().expect("SNH").to_string());
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

        match &self.expand {
            Some(expreg) => rc_str.push_str(&format!(", expand {}", expreg)),
            None => (),
        }

        rc_str.push(')');
        rc_str
    }

    /// Return false if a subset square is incompatible with a group.
    /// Delete expand region if needed.
    pub fn check_square(&mut self, sqrx: &SomeSquare) -> bool {
        if !self.region.is_superset_of(sqrx) {
            if let Some(expreg) = &self.expand {
                if expreg.is_superset_of(sqrx) {
                    if sqrx.pn > self.pn {
                        //println!("deleting expand for sqr");
                        self.expand = None;
                        return true;
                    }
                    if self.pn == Pn::Unpredictable {
                    } else if let Some(sqr_rules) = &sqrx.rules {
                        if self.pn == Pn::Two {
                            if let Some(rules) = &self.rules {
                                if sqrx.pn == Pn::Two {
                                    if rules.compatible(sqr_rules) {
                                    } else {
                                        //println!("deleting expand for sqr");
                                        self.expand = None;
                                    }
                                } else if rules.subcompatible(sqr_rules) {
                                } else {
                                    //println!("deleting expand for sqr");
                                    self.expand = None;
                                }
                            }
                        } else if let Some(rules) = &self.rules {
                            if rules.compatible(sqr_rules) {
                            } else {
                                //println!("deleting expand for sqr");
                                self.expand = None;
                            }
                        }
                    }
                }
            }
            return true;
        }
        //println!(
        //  "group:check_square grp {} sqr {}",
        //  self.region, sqrx.state
        //);

        if self.pn == Pn::Unpredictable {
            if sqrx.pn == self.pn || !sqrx.pnc {
                return true;
            }
            return false;
        }

        // Check if square is compatible with group.
        if sqrx.pn > self.pn {
            return false;
        }

        if sqrx.pn != self.pn && sqrx.pnc {
            return false;
        }

        self.rules
            .as_ref()
            .expect("SNH")
            .is_superset_of(sqrx.rules.as_ref().expect("SNH"))
    }

    /// Return false if a sample is incompatible with a group.
    /// Delete expand region if needed.
    pub fn check_sample(&mut self, smpl: &SomeSample) -> bool {
        if !self.region.is_superset_of(&smpl.initial) {
            if let Some(expreg) = &self.expand {
                if expreg.is_superset_of(&smpl.initial) {
                    let samp_rules = RuleStore::new(vec![SomeRule::new(smpl)]);
                    if self.pn == Pn::Unpredictable {
                    } else if self.pn == Pn::Two {
                        if let Some(rules) = &self.rules {
                            if rules.subcompatible(&samp_rules) {
                            } else {
                                //println!("deleting expand");
                                self.expand = None;
                            }
                        }
                    } else if let Some(rules) = &self.rules {
                        if rules.compatible(&samp_rules) {
                        } else {
                            //println!("deleting expand");
                            self.expand = None;
                        }
                    }
                }
            }
            return true;
        }

        if self.pn == Pn::Unpredictable {
            return true;
        }

        let tmp_rul = smpl.rule();

        self.rules
            .as_ref()
            .expect("SNH")
            .is_superset_of_rule(&tmp_rul)
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

    // Return true if a group region is a superset of a given group/region/square/state.
    pub fn is_superset_of(&self, other: &impl AccessStates) -> bool {
        self.region.is_superset_of(other)
    }

    // Return true if a group region is a subset of a given group/region/square/state.
    pub fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        self.region.is_subset_of(other)
    }

    // Return true if a group region intersects another group/region/square/state.
    pub fn intersects(&self, other: &impl AccessStates) -> bool {
        self.region.intersects(other)
    }

    // Return the number of edges in a group region.
    pub fn num_edges(&self) -> usize {
        self.region.num_edges()
    }

    // Clear the expand field.
    pub fn clear_expand(&mut self) {
        //println!("clearing expand");
        self.expand = None;
    }
} // end impl SomeGroup

/// Implement the trait AccessStates for SomeGroup.
impl AccessStates for SomeGroup {
    fn one_state(&self) -> bool {
        1 == self.region.states.len()
    }
    fn first_state(&self) -> &SomeState {
        self.region.states.first().expect("SNH")
    }
    fn x_mask(&self) -> SomeMask {
        self.region.x_mask()
    }
    fn edge_mask(&self) -> SomeMask {
        self.region.edge_mask()
    }
    fn high_state(&self) -> SomeState {
        self.region.high_state()
    }
    fn low_state(&self) -> SomeState {
        self.region.low_state()
    }
    fn diff_mask(&self, other: &impl AccessStates) -> SomeMask {
        self.region.diff_mask(other)
    }
    fn intersects(&self, other: &impl AccessStates) -> bool {
        self.region.intersects(other)
    }
    fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        self.region.is_subset_of(other)
    }
    fn is_superset_of(&self, other: &impl AccessStates) -> bool {
        self.region.is_superset_of(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::rule::SomeRule;

    #[test]
    fn check_subset_sample() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        let rules = RuleStore::new(vec![tmp_rul.new_from_string("10/x1/x0/00")?]);
        let regx = tmp_reg.new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules), true, None);

        let initial = tmp_sta.new_from_string("s0b1100")?;
        let result = tmp_sta.new_from_string("s0b0100")?;

        if !grpx.check_sample(&SomeSample::new(initial, result)) {
            return Err(format!("check_subset_sample: test 1 failed!"));
        }

        Ok(())
    }

    #[test]
    fn check_subset_square() -> Result<(), String> {
        let tmp_sta = SomeState::new(SomeBits::new(vec![0]));
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);
        let tmp_rul = SomeRule::new(&SomeSample::new(tmp_sta.clone(), tmp_sta.clone()));

        // Test if sqrx.pn > self.pn
        let rules = RuleStore::new(vec![tmp_rul.new_from_string("10/x1/x0/00")?]);
        let regx = tmp_reg.new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules), true, None); // Pn::One, pnc == true.

        let mut sqrx = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        ));
        sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0101")?,
        )); // Pn::Two, pnc == false.

        if grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 1 failed!"));
        }

        // Test if sqrx.pn != self.pn && sqrx.pnc
        let rules = RuleStore::new(vec![
            tmp_rul.new_from_string("10/x1/x0/00")?,
            tmp_rul.new_from_string("10/x1/x0/01")?,
        ]);

        let regx = tmp_reg.new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules), true, None); // Pn::Two, pnc == true.

        let mut sqrx = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        ));
        sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            tmp_sta.new_from_string("s0b0100")?,
        )); // pn = Pn::One, pnc = true.

        if grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 2 failed!"));
        }

        // Test if self.pn == Pn::Unpredictable
        let rules = None;

        let regx = tmp_reg.new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, rules, true, None);

        let sqrx = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        ));

        if !grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 3 failed!"));
        }

        // Test self.rules.is_superset_of(&sqrx.rules)
        let rules = RuleStore::new(vec![tmp_rul.new_from_string("10/x1/x0/00")?]);

        let regx = tmp_reg.new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules), true, None);

        let sqrx = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0100")?,
        ));

        if !grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 4a failed!"));
        }

        let sqrx = SomeSquare::new(&SomeSample::new(
            tmp_sta.new_from_string("s0b1100")?,
            tmp_sta.new_from_string("s0b0101")?,
        ));

        if grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 4b failed!"));
        }
        Ok(())
    }
} // end tests
