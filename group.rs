// Implement a Group struct, for an Unorthodox Expert System.
// This represents a group of two or more Squares, that are
// mutually compatible.

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
    pub region: SomeRegion, // Region the group covers, and the states sampled that are joined
    pub pn: Pn,             // Pattern Number enum One, Two or Unpredictable
    pub rules: RuleStore,   // Rules that all squares of the group are a subset of
    pub active: bool,       // Set to false to "delete" the group from a vector
    pub act_num: usize,     // The nAct umber of the action that the group belongs to
    pub confirmed: bool, // Set to true when a state only in the group has all adjacent states checked
    pub anchor: Option<SomeState>, // The state in only the group used to confirm
    pub not_x_confirm: SomeMask, // Mask of non-x bits to check for confirmation.
    pub not_x_expand: SomeMask, // Mask of non-x bits to check for expansion.
}

impl SomeGroup {
    pub fn new(sta1: &SomeState, sta2: &SomeState, ruls: RuleStore, act_num: usize) -> Self {
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

        let not_x = SomeMask::new(sta1.bts.b_xor(&sta2.bts).b_not());

        Self {
            region: SomeRegion::new(&sta1, &sta2), // Region the group covers, and the states sampled that are joined
            pn: pnx,
            rules: ruls,
            active: true,
            act_num,
            confirmed: false,
            anchor: None,
            not_x_confirm: not_x.clone(),
            not_x_expand: not_x,
        }
    }

    pub fn check_off_confirm_bit(&mut self, boff: &SomeMask) {
        //println!("*** group {} checking off confirm bit {}", &self.region, &boff);
        self.not_x_confirm = self.not_x_confirm.m_and(&boff.m_not());
    }

    pub fn check_off_expand_bit(&mut self, boff: &SomeMask) {
        //println!("*** group {} checking off expand bit {}", &self.region, &boff);
        self.not_x_expand = self.not_x_expand.m_and(&boff.m_not());
    }

    // Return true if a not_x_confirm bit is set
    pub fn not_x_confirm_bit_set(&self, bmsk: &SomeMask) -> bool {
        return !self.not_x_confirm.m_and(&bmsk).is_low();
    }

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
                rc_str.push_str(", []");
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
        //            " nxe: {} nxc: {}",
        //            &self.not_x_expand, &self.not_x_confirm
        //        ));

        rc_str.push_str(")");
        rc_str
    }

    pub fn str_region(&self) -> String {
        format!("{}", self.region.formatted_string())
    }

    pub fn inactivate(&mut self) -> bool {
        println!("Deleting group {}", self.str_region());
        self.active = false;
        true
    }

    pub fn square_is_ok(&self, sqrx: &SomeSquare) -> bool {
        //println!("square_is_ok grp: {} sqr: {}", &self.region, &sqrx.state);
        match self.pn {
            Pn::One => match sqrx.pn() {
                Pn::One => {
                    //println!("square_is_ok at One One");
                    return sqrx.rules.is_subset_of(&self.rules);
                }
                _ => {
                    //println!("square_is_ok at One Other");
                    return false;
                }
            },
            Pn::Two => match sqrx.pn() {
                Pn::One => {
                    //println!("square_is_ok at Two One");
                    if sqrx.len_results() > 1 {
                        return false;
                    }
                    return sqrx.rules.is_subset_of(&self.rules);
                }
                Pn::Two => {
                    //println!("square_is_ok at Two Two");
                    return sqrx.rules.is_subset_of(&self.rules);
                }
                _ => {
                    //println!("square_is_ok at Two Other");
                    return false;
                }
            },
            Pn::Unpredictable => match sqrx.pn() {
                Pn::Unpredictable => {
                    //println!("square_is_ok at U U");
                    return true;
                }
                _ => {
                    //println!("square_is_ok at U Other");
                    if sqrx.pnc() {
                        return false;
                    } else {
                        return true;
                    }
                }
            },
        }
    }

    // Return true if a sample is OK with a group
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

    pub fn set_anchor_off(&mut self) {
        self.anchor = None;
        self.confirmed = false;
    }

    pub fn set_anchor(&mut self, astate: SomeState) {
        self.anchor = Some(astate.clone());
        self.confirmed = true;
        let state2 = self.region.far_state(&astate);
        self.region = SomeRegion::new(&astate, &state2);
    }
}
