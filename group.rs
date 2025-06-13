//! The SomeGroup struct.
//!
//! This represents a group of one, or more, squares, that are
//! mutually compatible, as (presumably) are any squares between them.

use crate::bits::{BitsRef, NumBits, SomeBits};
use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::square::SomeSquare;
use crate::state::SomeState;
use crate::tools;
use crate::vertex::SomeVertex;

use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
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
    /// The vertex, anchored on a state, in only one (this) group, used to limit the group,
    /// and define the logical structure of the action.
    pub anchor: Option<SomeVertex>,
}

impl SomeGroup {
    /// Return a new group, given a region, and rules (if any).
    /// The RuleStore will be empty for Pn::Unpredictable squares.
    pub fn new(regionx: SomeRegion, ruls: Option<RuleStore>, pnc: bool) -> Self {
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
            pnc,
            rules: ruls,
            limited: false,
            anchor: None,
        }
    }

    /// Accessor set the pnc field to true.
    /// Reform region, if needed.
    pub fn set_pnc(&mut self) -> bool {
        if self.pnc {
            return false;
        }

        self.pnc = true;
        if self.region.states.len() > 2 {
            self.region = SomeRegion::new(vec![
                self.region.first_state().clone(),
                self.region.far_state().clone(),
            ]);
        }
        true
    }

    /// Return a string representing a group.
    fn formatted_str(&self) -> String {
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
                rc_str.push_str("[Unpredictable]");
            }
        }

        if let Some(anchor) = &self.anchor {
            if self.limited {
                rc_str.push_str(&format!(", limited using {anchor}"));
            } else {
                rc_str.push_str(&format!(", limiting using {anchor}"));
            }
        }

        rc_str.push(')');
        rc_str
    }

    /// Return a string representing a group.
    pub fn formatted_str_adjusted(&self, adjust: usize) -> String {
        let mut rc_str = String::from("G(");
        rc_str.push_str(&self.region.to_string());

        if self.region.len() > 2 {
            rc_str.push_str(&format!(", pn: {}", self.pn));
        } else {
            rc_str.push_str(&format!(",  pn: {}", self.pn));
        }

        rc_str.push_str(if self.pnc { ", pnc: t, " } else { ", pnc: f, " });

        let ruls_len = match self.pn {
            Pn::One => {
                let ruls = self.rules.as_ref().expect("SNH").to_string();
                rc_str.push_str(&ruls);
                ruls.len()
            }
            Pn::Two => {
                let ruls = self.rules.as_ref().expect("SNH").to_string();
                rc_str.push_str(&ruls);
                ruls.len()
            }
            Pn::Unpredictable => {
                rc_str.push_str("[Unpredictable]");
                15
            }
        };

        if let Some(anchor) = &self.anchor {
            rc_str.push(',');
            if ruls_len < adjust {
                rc_str.push_str(&" ".repeat(adjust - ruls_len));
            }
            if self.limited {
                if anchor.edge_mask.is_not_low() {
                    rc_str.push_str(&format!(" limited using {anchor}"));
                }
            } else {
                rc_str.push_str(&format!(" limiting using {anchor}"));
            }
            rc_str.push(')');
        } else {
            rc_str.push(')');
            if ruls_len < adjust {
                rc_str.push_str(&" ".repeat(adjust - ruls_len));
            }
        }
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

        if self.pn == sqrx.pn {
            self.rules
                .as_ref()
                .expect("SNH")
                .is_superset_of(sqrx.rules.as_ref().expect("SNH"))
        } else {
            self.rules
                .as_ref()
                .expect("SNH")
                .subcompatible_index(sqrx.rules.as_ref().expect("SNH"))
                .is_some()
        }
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
        //println!("group {} set_anchor_off", self.region);
        assert!(self.anchor.is_some());

        self.anchor = None;

        self.limited = false;
    }

    /// Set limited flag to false.
    pub fn set_limited_off(&mut self) -> bool {
        if self.limited {
            self.limited = false;
            true
        } else {
            false
        }
    }

    /// Set limited to true.
    pub fn set_limited(&mut self) {
        self.limited = true;

        if let Some(anchor) = &self.anchor {
            if self.region.first_state() != &anchor.pinnacle && self.region.far_state() != anchor.pinnacle {
                self.region = SomeRegion::new(vec![anchor.pinnacle.clone(), self.region.far_from(&anchor.pinnacle)]);
            }
        } else {
            panic!("Problem: set limited, with no anchor?");
        }
    }

    /// Set the anchor state, representing a square that is only in this group,
    /// all adjacent, external squares have been tested and found to be
    /// incompatible, and the square farthest from the anchor has been sampled.
    pub fn set_anchor(&mut self, anchor: SomeVertex) {
        self.anchor = Some(anchor);

        self.limited = false;
    }

    /// Check limited setting in groups due to new bit that can change.
    /// If the group region has a non-X bit position matching a new bit position that can change,
    /// set the limited indicator to false.
    pub fn check_limited(&mut self, max_reg: &SomeRegion) {
        if !self.limited {
            return;
        }

        let anchor = self.anchor.as_ref().expect("SNH");
        if max_reg.is_superset_of(&anchor.pinnacle) {
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
            //println!("resetting limit flag!");
        }
    }

    /// Return true if a group region is a superset of a given group/region/square/state.
    pub fn is_superset_of(&self, other: &impl tools::AccessStates) -> bool {
        self.region.is_superset_of(other)
    }

    /// Return true if a group region is a subset of a given group/region/square/state.
    pub fn is_subset_of(&self, other: &impl tools::AccessStates) -> bool {
        self.region.is_subset_of(other)
    }

    /// Return true if a group region intersects another group/region/square/state.
    pub fn intersects(&self, other: &impl tools::AccessStates) -> bool {
        self.region.intersects(other)
    }

    /// Return the number of edges in a group region.
    pub fn num_edges(&self) -> usize {
        self.region.num_edges()
    }

    /// Return true if a group's rules cause predictable change.
    /// Pn::Unpredictable groups will return false.
    /// Some Pn::One groups can cause no change.
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
impl tools::AccessStates for SomeGroup {
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
    fn diff_edge_mask(&self, other: &impl tools::AccessStates) -> SomeMask {
        self.region.diff_edge_mask(other)
    }
    fn intersects(&self, other: &impl tools::AccessStates) -> bool {
        self.region.intersects(other)
    }
    fn is_subset_of(&self, other: &impl tools::AccessStates) -> bool {
        self.region.is_subset_of(other)
    }
    fn is_superset_of(&self, other: &impl tools::AccessStates) -> bool {
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
    use std::str::FromStr;

    #[test]
    fn check_subset_sample() -> Result<(), String> {
        let rules = RuleStore::from_str("[10/x1/x0/00]")?;
        let regx = SomeRegion::from_str("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules), false);

        if !grpx.check_sample(&SomeSample::from_str("s1100->s0100")?) {
            return Err(format!("check_subset_sample: test 1 failed!"));
        }

        Ok(())
    }

    #[test]
    fn check_subset_square() -> Result<(), String> {
        // Test if sqrx.pn > self.pn
        let rules = RuleStore::from_str("[10/x1/x0/00]")?;
        let regx = SomeRegion::from_str("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules), false); // Pn::One

        let mut sqrx = SomeSquare::new(&SomeSample::from_str("s1100->s0100")?);

        sqrx.add_sample(&SomeSample::from_str("s1100->s0101")?); // Pn::Two, pnc == false.

        if grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 1 failed!"));
        }

        // Test if sqrx.pn != self.pn && sqrx.pnc
        let rules = RuleStore::from_str("[10/x1/x0/00, 10/x1/x0/01]")?;

        let regx = SomeRegion::from_str("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules), false); // Pn::Two

        let mut sqrx = SomeSquare::new(&SomeSample::from_str("s1100->s0100")?);

        sqrx.add_sample(&SomeSample::from_str("s1100->s0100")?); // pn = Pn::One, pnc = true.

        sqrx.add_sample(&SomeSample::from_str("s1100->s0100")?); // pn = Pn::One, pnc = true.

        if grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 2 failed!"));
        }

        // Test if self.pn == Pn::Unpredictable
        let rules = None;

        let regx = SomeRegion::from_str("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, rules, false);

        let sqrx = SomeSquare::new(&SomeSample::from_str("s1100->s0100")?);

        if !grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 3 failed!"));
        }

        // Test self.rules.is_superset_of(&sqrx.rules)
        let rules = RuleStore::from_str("[10/x1/x0/00]")?;

        let regx = SomeRegion::from_str("r1xx0")?;

        let mut grpx = SomeGroup::new(regx, Some(rules), false);

        let sqrx = SomeSquare::new(&SomeSample::from_str("s1100->s0100")?);

        if !grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 4a failed!"));
        }

        let sqrx = SomeSquare::new(&SomeSample::from_str("s1100->s0101")?);

        if grpx.check_square(&sqrx) {
            return Err(format!("check_subset_square: test 4b failed!"));
        }
        Ok(())
    }
} // end tests
