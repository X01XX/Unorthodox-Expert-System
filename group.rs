//! The SomeGroup struct.
//!
//! This represents a group of one, or more, squares, that are
//! mutually compatible, as (presumably) are any squares between them.

use crate::bits::{BitsRef, NumBits, SomeBits};
use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::AccessStates;
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
    /// Region the group covers.  Formed by one, or more, Pn-equal squares.
    /// All squares sampled in between should be compatable.
    /// The first square has been sampled.
    pub region: SomeRegion,
    /// Pattern Number enum One, Two or Unpredictable, shared by the defining squares.
    pub pn: Pn,
    /// Pnc indicator, the boolean And of the two squares pnc values.
    /// If true, each square forming the region has been sampled enough to
    /// become confident that the changes seen are predictable, or unpredictable.
    pub pnc: bool,
    /// Rules formed by the squares.  Will be None for Pn::Unpredictable.
    pub rules: Option<RuleStore>,
    /// Set to true when a state only in the group has all adjacent states outside
    /// of the group region checked.
    pub limited: bool,
    /// The state, in only one (this) group, used to limit the group.
    pub anchor: Option<SomeState>,
    /// Maskof  adjacent squares used to limit a group.
    /// This will be the region edge mask, limited by the bit changes of available rules.
    pub anchor_mask: Option<SomeMask>,
}

impl SomeGroup {
    /// Return a new group, given a region, and rules (if any).
    /// The RuleStore will be empty for Pn::Unpredictable squares.
    pub fn new(regionx: SomeRegion, ruls: Option<RuleStore>) -> Self {
        //println!(
        //  "creating group {}",
        //   regionx
        //);
        debug_assert!(if let Some(rulstr) = &ruls {
            rulstr.is_not_empty()
                && rulstr.len() < 3
                && rulstr.num_bits().unwrap() == regionx.num_bits()
                && rulstr.rules_initial_region_eq(&regionx)
        } else {
            true
        });

        // Determine Pn.
        let mut pnx = Pn::One;
        if ruls.is_none() {
            pnx = Pn::Unpredictable;
        } else if let Some(xruls) = ruls.as_ref() {
            if xruls.len() == 2 {
                pnx = Pn::Two;
            }
        }

        Self {
            region: regionx,
            pn: pnx,
            pnc: false,
            rules: ruls,
            limited: false,
            anchor: None,
            anchor_mask: None,
        }
    }

    /// Accessor set the pnc field to true.
    /// Reform region, if needed.
    pub fn set_pnc(&mut self, dom_id: usize, act_id: usize) {
        if self.pnc {
            println!(
                "Dom {} Act {} Group {} already confirmed",
                dom_id, act_id, self.region
            );
            return;
        }

        self.pnc = true;
        if self.region.states.len() > 2 {
            self.region = SomeRegion::new(vec![
                self.region.first_state().clone(),
                self.region.far_state().clone(),
            ]);
        }
        println!(
            "Dom {} Act {} Group {} confirmed",
            dom_id, act_id, self.region
        );
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
                    rc_str.push_str(&format!(", limited using {sta1}"));
                    if let Some(anchor_mask) = &self.anchor_mask {
                        if anchor_mask.is_not_low() {
                            rc_str.push_str(&format!(" adj mask {}", anchor_mask));
                        }
                    }
                } else {
                    rc_str.push_str(&format!(", limiting using {sta1}"));
                }
            }
            None => (),
        }

        rc_str.push(')');
        rc_str
    }

    /// Return false if a subset square is incompatible with a group.
    /// Delete expand region if needed.
    pub fn check_square(&mut self, sqrx: &SomeSquare) -> bool {
        //println!("SomeGroup:check_square {}", sqrx.state);
        if !self.region.is_superset_of(sqrx) {
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
        self.anchor_mask = None;
    }

    /// Set limited to false.
    pub fn set_limited_off(&mut self, dom_id: usize, act_id: usize) {
        println!(
            "Dom {} Act {} Group {} set limited off",
            dom_id, act_id, self.region
        );
        self.limited = false;
        self.anchor_mask = None;
    }

    /// Set limited to true.
    pub fn set_limited(&mut self, anchor_mask: SomeMask, dom_id: usize, act_id: usize) {
        println!(
            "Dom {} Act {} Group {} set limited on, adj mask {}",
            dom_id, act_id, self.region, anchor_mask
        );
        self.limited = true;
        self.anchor_mask = Some(anchor_mask);

        if let Some(astate) = &self.anchor {
            if self.region.first_state() != astate && self.region.far_state() != *astate {
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
        self.anchor_mask = None;
    }

    /// Check limited setting in groups due to new bit that can change.
    /// If the group region has a non-X bit position matching a new bit position that can change,
    /// set the limited indicator to false.
    pub fn check_limited(&mut self, max_reg: &SomeRegion) {
        assert!(self.limited);

        let anchor = self.anchor.as_ref().expect("SNH");
        if max_reg.is_superset_of(anchor) {
        } else {
            return;
        };

        let positions = self.region.edge_mask().bitwise_and(&max_reg.x_mask());

        //        println!(
        //            "Check limited setting for {} with {} positions {}",
        //            self.region, new_chgs, positions
        //        );

        if !positions.is_low() {
            self.limited = false;
            self.anchor_mask = None;
            //println!("resetting limit flag!");
        }
    }

    /// Return true if a group region is a superset of a given group/region/square/state.
    pub fn is_superset_of(&self, other: &impl AccessStates) -> bool {
        self.region.is_superset_of(other)
    }

    /// Return true if a group region is a subset of a given group/region/square/state.
    pub fn is_subset_of(&self, other: &impl AccessStates) -> bool {
        self.region.is_subset_of(other)
    }

    /// Return true if a group region intersects another group/region/square/state.
    pub fn intersects(&self, other: &impl AccessStates) -> bool {
        self.region.intersects(other)
    }

    /// Return the number of edges in a group region.
    pub fn num_edges(&self) -> usize {
        self.region.num_edges()
    }

    /// Return true if a group's rules cause predictable change.
    pub fn causes_predictable_change(&self) -> bool {
        match &self.rules {
            Some(rules) => rules.causes_predictable_change(),
            None => false,
        }
    }

    /// Return true if two groups are adjacent.
    pub fn is_adjacent(&self, other: &Self) -> bool {
        self.region.is_adjacent(other)
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
    fn diff_edge_mask(&self, other: &impl AccessStates) -> SomeMask {
        self.region.diff_edge_mask(other)
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
    fn num_bits(&self) -> usize {
        self.region.num_bits()
    }
}

/// Implement the trait BitsRef for SomeGroup.
impl BitsRef for SomeGroup {
    fn bitsref(&self) -> &SomeBits {
        self.region.first_state().bitsref()
    }
}

/// Implement the NumBits trait for SomeRegion.
impl NumBits for SomeGroup {
    fn num_bits(&self) -> usize {
        self.region.num_bits()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rule::SomeRule;

    #[test]
    fn check_subset_sample() -> Result<(), String> {
        let rules = RuleStore::new(vec![SomeRule::new_from_string("10/x1/x0/00")?]);
        let regx = SomeRegion::new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules));

        let initial = SomeState::new_from_string("s0b1100")?;
        let result = SomeState::new_from_string("s0b0100")?;

        if !grpx.check_sample(&SomeSample::new(initial, result)) {
            return Err(format!("check_subset_sample: test 1 failed!"));
        }

        Ok(())
    }

    #[test]
    fn check_subset_square() -> Result<(), String> {
        // Test if sqrx.pn > self.pn
        let rules = RuleStore::new(vec![SomeRule::new_from_string("10/x1/x0/00")?]);
        let regx = SomeRegion::new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules)); // Pn::One

        let mut sqrx = SomeSquare::new(&SomeSample::new(
            SomeState::new_from_string("s0b1100")?,
            SomeState::new_from_string("s0b0100")?,
        ));
        sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0101")?,
        )); // Pn::Two, pnc == false.

        if grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 1 failed!"));
        }

        // Test if sqrx.pn != self.pn && sqrx.pnc
        let rules = RuleStore::new(vec![
            SomeRule::new_from_string("10/x1/x0/00")?,
            SomeRule::new_from_string("10/x1/x0/01")?,
        ]);

        let regx = SomeRegion::new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules)); // Pn::Two

        let mut sqrx = SomeSquare::new(&SomeSample::new(
            SomeState::new_from_string("s0b1100")?,
            SomeState::new_from_string("s0b0100")?,
        ));
        sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0100")?,
        )); // pn = Pn::One, pnc = true.

        sqrx.add_sample(&SomeSample::new(
            sqrx.state.clone(),
            SomeState::new_from_string("s0b0100")?,
        )); // pn = Pn::One, pnc = true.

        if grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 2 failed!"));
        }

        // Test if self.pn == Pn::Unpredictable
        let rules = None;

        let regx = SomeRegion::new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, rules);

        let sqrx = SomeSquare::new(&SomeSample::new(
            SomeState::new_from_string("s0b1100")?,
            SomeState::new_from_string("s0b0100")?,
        ));

        if !grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 3 failed!"));
        }

        // Test self.rules.is_superset_of(&sqrx.rules)
        let rules = RuleStore::new(vec![SomeRule::new_from_string("10/x1/x0/00")?]);

        let regx = SomeRegion::new_from_string("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules));

        let sqrx = SomeSquare::new(&SomeSample::new(
            SomeState::new_from_string("s0b1100")?,
            SomeState::new_from_string("s0b0100")?,
        ));

        if !grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 4a failed!"));
        }

        let sqrx = SomeSquare::new(&SomeSample::new(
            SomeState::new_from_string("s0b1100")?,
            SomeState::new_from_string("s0b0101")?,
        ));

        if grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 4b failed!"));
        }
        Ok(())
    }
} // end tests
