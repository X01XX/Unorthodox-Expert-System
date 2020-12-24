// Implement a Group struct,
// which represents a group of two or more Squares, that are
// mutually compatible.

//use crate::bits::SomeBits;
use crate::mask::SomeMask;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::square::SomeSquare;
use crate::state::SomeState;
//use crate::statestore::StateStore;
use std::fmt;

impl fmt::Display for SomeGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rc_str = self.str2();

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
pub struct SomeGroup {
    pub region: SomeRegion, // Region the group covers, and the states sampled that are joined
    pub pn: Pn,             // Pattern Number enum One, Two or Unpredictable
    pub rules: RuleStore,   // Rules that all squares of the group are a subset of
    pub active: bool,       // Set to false to "delete" the group from a vector
    pub act_num: usize,     // The nAct umber of the action that the group belongs to
    pub confirmed: bool, // Set to true when a state only in the group has all adjacent states checked
    pub anchor: Option<SomeState>, // The state in only the group used to confirm
    pub not_x_check: SomeMask, // Mask of non-x bits to check or rule out.
}

impl SomeGroup {
    pub fn new(
        sta1: &SomeState,
        sta2: &SomeState,
        ruls: RuleStore,
        act_num: usize,
        max_x: &SomeMask,
    ) -> Self {
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
            act_num,
            confirmed: false,
            anchor: None,
            not_x_check: max_x.m_and(&SomeMask::new(sta1.bts.b_xor(&sta2.bts).b_not())),
        }
    }

    pub fn check_off(&mut self, boff: &SomeMask) {
        //println!("*** group {} checking off bit {}", &self.region, &boff);
        self.not_x_check = self.not_x_check.m_and(&boff.m_not());
    }

    // Return true if a not_x_check bit is set
    pub fn not_x_bit_set(&self, bmsk: &SomeMask) -> bool {
        return !self.not_x_check.m_and(&bmsk).is_low();
    }

    pub fn str2(&self) -> String {
        let mut rc_str = String::from("G(");
        rc_str.push_str(&format!("{}", self.region.str_terse()));

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

        rc_str.push_str(")");
        rc_str
    }

    pub fn str_terse(&self) -> String {
        format!("G{}", self.region.str_terse())
    }

    pub fn inactivate(&mut self) -> bool {
        println!("Deleting group {}", self.str_terse());
        self.active = false;
        true
    }

    pub fn square_is_ok(&self, sqrx: &SomeSquare) -> bool {
        //println!("square_is_ok grp: {} sqr: {}", &self.region, &sqrx.state);
        match self.pn {
            Pn::One => match sqrx.pn() {
                Pn::One => {
                    return sqrx.rules.is_subset_of(&self.rules);
                }
                _ => {
                    return false;
                }
            },
            Pn::Two => match sqrx.pn() {
                Pn::One => {
                    if sqrx.num_results() > 1 {
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
            Pn::Unpredictable => match sqrx.pn() {
                Pn::Unpredictable => {
                    return true;
                }
                _ => {
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

    // Process new X bits mask in max_region
    pub fn new_x_bits(&mut self, bitsx: &SomeMask) {
        let bitsy = bitsx.m_and(&self.region.x_mask().m_not());
        self.not_x_check = self.not_x_check.m_or(&bitsy);
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
