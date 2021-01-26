// Implement an Action struct,
// which represents the best guess of the expected responses
// of executing an action, based on the current state.
//
use crate::bits::SomeBits;
//use crate::bitsstore::BitsStore;
use crate::change::SomeChange;
use crate::combinable::Combinable;
use crate::group::SomeGroup;
use crate::groupstore::GroupStore;
use crate::mask::SomeMask;
use crate::maskstore::MaskStore;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
//use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::square::SomeSquare;
use crate::squarestore::SquareStore;
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use std::fmt;
extern crate rand;
use rand::Rng;
use serde::{Deserialize, Serialize};

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("A(ID: ");

        rc_str.push_str(&self.num.to_string());

        if self.seek_edge.any_active() {
            rc_str.push_str(&format!(" clsr: {}", self.seek_edge));
        }

        rc_str.push_str(&format!(
            " Changes {} recent cng: {}",
            self.pos_bit_cngs, self.pos_bit_changed
        ));

        let regs = self.groups.regions();

        let mut fil = String::from(",\n       Grps: (");
        for grpx in self.groups.iter() {
            if grpx.active {
                let stas_in = self.squares.stas_in_reg(&grpx.region);

                // Count the number of states in a group that are also in only one region
                let mut cnt = 0;
                for stax in stas_in.iter() {
                    if regs.state_in_1_region(&stax) {
                        cnt += 1;
                    }
                }

                rc_str.push_str(&format!(
                    "{}{} num Sqrs: {} in1: {}",
                    &fil,
                    &grpx.formatted_string(),
                    &stas_in.len(),
                    &cnt,
                ));

                if grpx.not_x_confirm.is_low() == false {
                    rc_str.push_str(&format!(" chk: {}", &grpx.not_x_confirm));
                }

                fil = String::from(",\n              ");
            }
        }

        //rc_str.push_str(&format!("{}", self.groups));

        //        rc_str.push_str("),\n       Sqrs: (");

        //        rc_str.push_str(&format!("{}", &self.squares));

        rc_str.push_str("))");

        write!(f, "{}", rc_str)
    }
}

#[derive(Serialize, Deserialize)]
pub struct SomeAction {
    // Action number, index/key into ActionStore vector.
    pub num: usize,
    // Groups of compatible-change squares
    pub groups: GroupStore,
    // Squares hold results of multiple samples of the same state
    pub squares: SquareStore,
    // Regions of invalidated groups hide a new edge of the solution
    // Closer and closer dissimilar squares are sought, producing smaller and smaller
    // regions, until a pair of adjacent, dissimilar, squares are found.
    pub seek_edge: RegionStore,
    // The results of all actions of a Domain indicate bit positions that may be 0 or 1.
    x_mask: SomeMask,
    // The number of ints the action uses to represent a bit pattern
    num_ints: usize,
    // Aggregation of possible bit changes
    pub pos_bit_cngs: SomeChange,
    // Flag for indicating a recent change in the pos_bit_cngs rule
    pos_bit_changed: bool,
}

impl SomeAction {
    pub fn new(num_ints: usize, num: usize) -> Self {
        //let st_low = SomeState::new(SomeBits::new_low(num_ints));
        SomeAction {
            num,
            groups: GroupStore::new(),
            squares: SquareStore::new(),
            seek_edge: RegionStore::new(),
            x_mask: SomeMask::new(SomeBits::new_low(num_ints)),
            num_ints,
            pos_bit_cngs: SomeChange::new(num_ints),
            pos_bit_changed: false,
        }
    }

    // Return Combinable enum for any two squares with the same Pn value
    // Check squares inbetween for compatibility.
    pub fn can_combine(&self, sqrx: &SomeSquare, sqry: &SomeSquare) -> Combinable {
        assert!(sqrx.pn() == sqry.pn());

        // Check the two squares
        let cmbx = sqrx.can_combine(&sqry);
        if cmbx == Combinable::False {
            return cmbx;
        }

        // Get square keys for all squares in the region formed by the
        // two given squares.
        let stas = self
            .squares
            .stas_in_reg(&SomeRegion::new(&sqrx.state, &sqry.state));

        // If there are no squares inbetween, done.
        if stas.len() == 2 {
            return cmbx;
        }

        // Check squares between

        // Get square references from the states between sqrx and sqry.
        let mut sqrs_ref = Vec::<&SomeSquare>::with_capacity(stas.len());

        for stax in stas.iter() {
            if *stax == sqrx.state || *stax == sqry.state {
            } else {
                sqrs_ref.push(self.squares.find(&stax).unwrap());
            }
        }

        // Check each square for compatibility to the two defining squares.
        //
        // Two squares, with one sample each, Pn::One, could have rules that are mutually incompatible,
        // but both rules could be valid subsets of a Pn::Two pair of squares.
        for sqrz in &sqrs_ref {
            // If an inbetween square had a Pn value greater than the defining pair,
            // assuming the lower Pn square used is not <SomeSquare>.pnc() == true,
            // the SomeSquare::can_combine function would return
            // Combinable::MoreSamplesNeeded (for the lower Pn square).
            if sqrz.pn() > sqrx.pn() {
                return Combinable::False;
            }

            if sqrz.can_combine(&sqrx) == Combinable::False {
                return Combinable::False;
            }
            if sqrz.can_combine(&sqry) == Combinable::False {
                return Combinable::False;
            }
        }

        cmbx
    }

    pub fn take_action_need(&mut self, cur: &SomeState, ndx: &SomeNeed, new_state: &SomeState) {
        self.groups.changed = false;

        self.take_action_need2(cur, ndx, new_state);

        if self.groups.changed {
            self.pos_bit_cngs = self.possible_bit_changes();
        }
    }

    // Significant effort is used to generate needs, so take in the
    // state that will satisfy the need, and the need itself for clues in
    // processing the result.
    pub fn take_action_need2(&mut self, cur: &SomeState, ndx: &SomeNeed, new_state: &SomeState) {
        // println!("take_action_need {}", &ndx);
        // Get the result, the sample is cur -> new_state

        // Process each kind of need
        match ndx {
            SomeNeed::AStateMakeGroup {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                for_reg,
                far,
                num_x: _,
            } => {
                self.store_sample(&cur, &new_state);

                // Form the rules, make the group
                // If the squares are incompatible, or need more samples, skip action.
                let sqrx = self.squares.find(&sta).unwrap();
                if let Some(sqry) = self.squares.find(&far) {
                    if sqrx.pn() == sqry.pn() && self.can_combine(&sqrx, &sqry) == Combinable::True
                    {
                        if sqrx.pn() == Pn::Unpredictable {
                            if self.groups.any_superset_of(&for_reg) == false {
                                self.groups.push(SomeGroup::new(
                                    &sqrx.state,
                                    &sqry.state,
                                    RuleStore::new(),
                                    self.num,
                                    //   &self.x_mask,
                                ));
                            } else {
                                panic!(
                                    "Supersets found for new group (1) {} in {}",
                                    for_reg,
                                    SomeRegion::new(&sqrx.state, &sqry.state)
                                );
                            }
                        } else {
                            let rulsxy = sqrx.rules.union(&sqry.rules).unwrap();

                            println!("Adding group: {}", &rulsxy[0].initial_region());
                            if self.groups.any_superset_of(&rulsxy[0].initial_region()) {
                                //println!(
                                //    "Supersets found for new group {} in {}",
                                //    rulsxy.initial_region(),
                                //    self.groups.supersets_of(&rulsxy[0].initial_region())
                                //);
                            } else {
                                self.groups.push(SomeGroup::new(
                                    &sqrx.state,
                                    &sqry.state,
                                    rulsxy,
                                    self.num,
                                    //    &self.x_mask,
                                ));
                            }
                        } // end if Unpredictable
                    } // end if can combine
                } // end find far
            } // end process AStateMakeGroup Need

            SomeNeed::SeekEdge {
                dom_num: _,
                act_num: _,
                targ_state: sta,
                in_group: greg,
            } => {
                self.store_sample(&cur, &new_state);
                self.check_square_new_sample(&cur);

                // Form the rules, make the group
                let sqr1 = self.squares.find(&greg.state1).unwrap();
                let sqr2 = self.squares.find(&greg.state2).unwrap();
                let sqr3 = self.squares.find(&sta).unwrap();

                // Process next sample of square in-between for new square and state1 square.
                // Should be different from state1 square or state2 square.
                // It may be different from both state1 and state2.
                // If it needs more samples, skip, next need will increment the number samples.
                let cnb1 = sqr3.can_combine(&sqr1);
                match cnb1 {
                    Combinable::False => {
                        let dist = sqr1.distance(&sqr3);
                        if dist == 1 {
                            self.seek_edge.inactivate(greg);
                        } else {
                            let even_closer_reg = SomeRegion::new(&sqr1.state, &sqr3.state);
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }
                    _ => {}
                } // end match cnb1

                // Process next sample of square in-between for new square and state2 square
                // Should be different from state1 square or state2 square
                let cnb2 = sqr3.can_combine(&sqr2);
                match cnb2 {
                    Combinable::False => {
                        let dist = sqr2.distance(&sqr3);
                        if dist == 1 {
                            self.seek_edge.inactivate(greg);
                        } else {
                            let even_closer_reg = SomeRegion::new(&sqr2.state, &sqr3.state);
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }
                    _ => {}
                } // end match cnb2
            } // end match SeekEdgeNeed
            _ => {
                // default, store sample, if not in a group, make one
                self.store_sample(&cur, &new_state);
                self.check_square_new_sample(&cur);
            }
        } // end match ndx
    } // End take_action_need2

    // Add a sample by user command
    pub fn take_action_arbitrary(&mut self, init_state: &SomeState, rslt_state: &SomeState) {
        println!(
            "take_action_arbitrary for state {} result {}",
            init_state, rslt_state
        );

        self.groups.changed = false;

        self.store_sample(&init_state, &rslt_state);
        self.check_square_new_sample(&init_state);

        if self.groups.changed {
            self.pos_bit_cngs = self.possible_bit_changes();
        }
    }

    pub fn take_action_step(&mut self, cur: &SomeState, new_state: &SomeState) {
        self.groups.changed = false;

        self.take_action_step2(cur, new_state);

        if self.groups.changed {
            self.pos_bit_cngs = self.possible_bit_changes();
        }
    }

    // Evaluate a sample produced by a step in a plan.
    // If there is an existing square,
    //     update it. A sample may be OK for a two-result group, but not if it comes out-of-order for
    //     an existing square.
    //     check if it breaks anything
    // else
    //     if it brakes anything
    //         add it as a square
    //
    pub fn take_action_step2(&mut self, cur: &SomeState, new_state: &SomeState) {
        // If square exists, update it, check square, return
        if let Some(sqrx) = self.squares.find_mut(cur) {
            // println!("about to add result to sqr {}", cur.str());
            sqrx.add_result(new_state.clone());

            if sqrx.changed() {
                self.check_square_new_sample(cur);
                return;
            }

            return;
        }

        // Get num groups that might be invalidated
        let num_grps_invalidated = self.groups.check_sample(cur, new_state);

        // Get num active groups in
        let num_grps_in = self.groups.num_groups_state_in(cur);
        if num_grps_invalidated > 0 {
            println!(
                "sqr {} in {} groups, invalidated {}",
                cur, num_grps_in, num_grps_invalidated
            );
        }

        if num_grps_invalidated > 0 || num_grps_in == 0 {
            self.store_sample(cur, new_state);
            self.check_square_new_sample(cur);
        }
    }

    // Evaluate a sample produced for a need.
    // It is expected that the sample needs to be stored,
    fn store_sample(&mut self, cur: &SomeState, new_state: &SomeState) {
        // Get an existing square and update, or create a new square.
        //println!("store_sample");
        if let Some(sqrx) = self.squares.find_mut(cur) {
            // println!("about to add result to sqr {}", cur.str());
            sqrx.add_result(new_state.clone());
        } else {
            // println!("No square found for state {}", cur.str());
            self.squares
                .insert(SomeSquare::new(cur.clone(), new_state.clone()));
        }
    }

    // Check a square, referenced by state, against valid groups.
    // The square may invalidate some groups.
    // Add a group for the square if the square is in no valid group.
    // If any groups were invalidated, check other squares
    // that are in no groups.
    fn check_square_new_sample(&mut self, key: &SomeState) {
        //println!("check_square_new_sample");

        self.groups.changed = false;

        // Get number of groups invalidated, which may orphan some squares.
        let regs_invalid = self.validate_groups_new_sample(&key);

        let mut min_dist = 99999;
        let mut close_dif = StateStore::new();

        let sqrx = self.squares.find(&key).unwrap();

        // Process groups invalidated to see if a close dissimilar square
        // is not adjacent, so save a region to later generate inbetween needs
        // until there is an adjacent, dissimilar pair.  e.g. finding
        // a new edge in the solution.
        if regs_invalid.len() > 0 {
            let stas_in = self.squares.stas_in_regs(&regs_invalid);

            for stax in stas_in.iter() {
                if stax == key {
                    continue;
                }

                let sqry = self.squares.find(&stax).unwrap();

                if sqrx.can_combine(&sqry) == Combinable::False {
                    let num_dif = key.distance(&stax);

                    if num_dif < min_dist {
                        min_dist = num_dif;
                        close_dif = StateStore::new();
                    }

                    if num_dif == min_dist {
                        close_dif.push(stax.clone());
                    }
                }
            } // next stax

            //println!(
            //    "for sqr {}, closest diff sqrs are {}, dist {}",
            //    &key, &close_dif, &min_dist
            //);

            // Find adjacent close squares
            if close_dif.len() > 0 && min_dist > 1 {
                // any squares between but need more samples?

                // just store need info in action struct?
                self.seek_edge
                    .push_nosups(SomeRegion::new(&key, &close_dif[0]));
            }
        } // end if rgs_invalid.len() > 0

        // Create a group for square if needed
        if regs_invalid.len() == 0 {
            if self.groups.num_groups_state_in(&key) == 0 {
                self.create_groups_given_sample(&key);
            }
        } else {
            // Check squares that may not be in a group

            if regs_invalid.len() == 1 {
                println!("\n1 group invalidated, checking for any square in no groups");
            } else {
                println!(
                    "\n{} groups invalidated, checking for any square in no groups",
                    regs_invalid
                );
            }

            let regs = self.groups.regions();

            let keys = self.squares.not_in_regions(&regs);

            for keyx in keys.iter() {
                // A later square may be in a group created by an earlier square
                if self.groups.num_groups_state_in(&keyx) == 0 {
                    self.create_groups_given_sample(&keyx);
                }
            }
        }

        if self.groups.changed {
            self.pos_bit_cngs = self.possible_bit_changes();
        }
    }

    // Check groups an updated square is in to see if they are still valid.
    // Return a RegionStore of group regions that have been invalidated.
    fn validate_groups_new_sample(&mut self, key: &SomeState) -> RegionStore {
        //println!("validate_groups_new_sample");

        // Square should exist
        let sqrx = self.squares.find(&key).unwrap();

        // Get num groups inactivated
        let regs_invalid = self.groups.check_square(&sqrx);
        if regs_invalid.len() > 0 {
            println!(
                "Square {} invalidated groups {}",
                sqrx.str_terse(),
                regs_invalid
            );
        }

        return regs_invalid;
    }

    // Check groups due to a new sample
    // Create a group with the square, if needed.
    // Return the number of groups invalidated
    fn create_groups_given_sample(&mut self, key: &SomeState) {
        // Square should exist
        let sqrx = self.squares.find(&key).unwrap();

        // Get num active groups in
        let num_grps_in = self.groups.num_groups_state_in(&sqrx.state);
        println!(
            "Square {} in {} groups",
            sqrx.str_terse(),
            num_grps_in,
            //num_grps_invalidated
        );

        if num_grps_in == 0 {
            //println!("Checking Square {} for new groups", &sqrx.str_terse());

            let rsx = self.possible_group_regions(sqrx);

            if rsx.len() > 0 {
                // println!("Regions for new groups {}", rsx.str());
                for regx in rsx.iter() {
                    if self.groups.any_superset_of(regx) == false {
                        if sqrx.pn() == Pn::Unpredictable {
                            self.groups.push(SomeGroup::new(
                                &regx.state1,
                                &regx.state2,
                                RuleStore::new(),
                                self.num,
                                //  &self.x_mask,
                            ));
                        } else {
                            let sqr_1 = self.squares.find(&regx.state1).unwrap();
                            let sqr_2 = self.squares.find(&regx.state2).unwrap();

                            //println!("Squares with states {}, {} produce ruls {}", &regx.state1.str(), &regx.state2.str(), ruls.str());

                            self.groups.push(SomeGroup::new(
                                &regx.state1,
                                &regx.state2,
                                sqr_1.rules.union(&sqr_2.rules).unwrap(),
                                self.num,
                                //     &self.x_mask,
                            ));
                        }
                    }
                } // end for regx
            } else {
                // Make a single-square group
                self.groups.push(SomeGroup::new(
                    &sqrx.state,
                    &sqrx.state,
                    sqrx.rules.clone(),
                    self.num,
                    //   &self.x_mask,
                ));
            }
        }
    }

    // Get needs for an Action, to improve understanding of the reaction pattern(s).
    // When most needs are satisfied, needs for group confirmation are generated.
    // If housekeeping needes are grenerated, they are processed and needs
    // are checked again.
    pub fn get_needs(&mut self, cur_state: &SomeState, x_mask: &SomeMask) -> NeedStore {
        //println!("Running Action {}::get_needs {}", self.num, cur_state);

        self.x_mask = x_mask.clone();

        // loop until no housekeeping need is returned.
        let mut nds = NeedStore::new();

        let mut try_again = false;

        loop {
            // Check if current state is in any groups
            let mut in_grp = false;
            for grpx in self.groups.iter() {
                if grpx.active && grpx.region.is_superset_of_state(cur_state) {
                    in_grp = true;
                    break;
                }
            }

            // If not, generate need
            if in_grp == false {
                nds.push(SomeNeed::StateNotInGroup {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: cur_state.clone(),
                });
            }

            // Look for needs to find a new edge in an invalidated group
            let mut ndx = self.seek_edge_needs1();
            if ndx.len() > 0 {
                nds.append(&mut ndx);
            } else {
                let mut ndx = self.seek_edge_needs2(&cur_state);
                if ndx.len() > 0 {
                    nds.append(&mut ndx);
                }
            }

            // Check for additional samples for group states needs
            let mut ndx = self.additional_group_state_samples();

            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Check any two groups for:
            // Overlapping regions that may be combined.
            // Overlapping groups that form a contradictory intersection.
            // Adjacent groups that may be combined, or generate a subgroup that overlaps both.
            let mut ndx = self.group_pair_needs();

            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Check for squares in-one-group needs
            if nds.len() == 0 {
                let mut ndx = self.confirm_groups_needs();

                if ndx.len() > 0 {
                    nds.append(&mut ndx);
                }
            }

            if nds.len() == 0 {
                let mut ndx = self.expand_needs();

                if ndx.len() > 0 {
                    nds.append(&mut ndx);
                }
            }

            //println!("needs: {}", nds);

            // Process a few specific housekeeping needs related to changing the Action or groups.
            for ndx in nds.iter_mut() {
                match ndx {
                    SomeNeed::AddGroup {
                        act_num: _,
                        group_region: greg,
                    } => {
                        // Add a new group
                        if self.groups.any_superset_of(&greg) {
                            println!(
                                "**** Supersets found for new group {} in {}",
                                &greg,
                                self.groups.supersets_of(&greg)
                            );

                            continue;
                        }

                        println!(
                            "AddGroup {} using {} and {}",
                            &greg, &greg.state1, &greg.state2
                        );

                        let sqrx = self.squares.find(&greg.state1).unwrap();
                        let sqry = self.squares.find(&greg.state2).unwrap();

                        assert!(sqrx.pn() == sqry.pn());

                        if sqrx.pn() == Pn::Unpredictable {
                            self.groups.push(SomeGroup::new(
                                &greg.state1,
                                &greg.state2,
                                RuleStore::new(),
                                self.num,
                            ));
                            try_again = true;
                        } else {
                            self.groups.push(SomeGroup::new(
                                &greg.state1,
                                &greg.state2,
                                sqrx.rules.union(&sqry.rules).unwrap(),
                                self.num,
                            ));
                            try_again = true;
                        }
                    }
                    SomeNeed::SetGroupConfirmed {
                        group_region: greg,
                        cstate: sta1,
                    } => {
                        try_again = true;

                        let grpx = self.groups.find_mut(&greg).unwrap();
                        println!("Act {} Group {} confirmed using {}", self.num, greg, sta1);
                        grpx.set_anchor(sta1.clone());
                    }
                    SomeNeed::ClearGroupCheckBit {
                        group_region: greg,
                        mbit: mbitx,
                    } => {
                        try_again = true;

                        let grpx = self.groups.find_mut(&greg).unwrap();
                        grpx.check_off_confirm_bit(&mbitx);
                    }
                    SomeNeed::ClearGroupExpandBit {
                        group_region: greg,
                        mbit: mbitx,
                    } => {
                        try_again = true;

                        let grpx = self.groups.find_mut(&greg).unwrap();
                        grpx.check_off_expand_bit(&mbitx);
                    }
                    SomeNeed::InactivateSeekEdge { reg: regx } => {
                        self.seek_edge.inactivate(&regx);
                        try_again = true;
                    }
                    SomeNeed::AddSeekEdge { reg: regx } => {
                        self.seek_edge.push_nosups(regx.clone());
                        try_again = true;
                    }
                    _ => {}
                } // end match
            } // next ndx

            if try_again == false {
                return nds;
            }

            nds = NeedStore::new();

            try_again = false;
        } // end loop
    } // end get_needs

    // Pre-check and clean-up, scan action seek_edge RegionStore.
    //
    // If there is an existing square between the region state-squares, that is
    // incompatible with any one of the squares, inactivate the region.
    //
    // If the two incompatible squares are not adjacent, add a new region to
    // seek_edge, built from the incompatible pair.
    //
    // Process all closer_reg regions until all are inactivated, have no squares between
    // the region defining states, or have one square that needs more samples between
    // the region defining states.
    pub fn seek_edge_needs1(&self) -> NeedStore {
        let mut ret_nds = NeedStore::new();

        let mut new_regs = RegionStore::new();
        for regx in self.seek_edge.iter() {
            if regx.active == false {
                continue;
            }

            // Get the squares represented by the states that form the region
            let sqr1 = self.squares.find(&regx.state1).unwrap();
            let sqr2 = self.squares.find(&regx.state2).unwrap();

            let stas_in = self.squares.stas_in_reg(&regx);

            // If no squares squares inbetween, skip for now
            if stas_in.len() == 2 {
                continue;
            }

            for stax in stas_in.iter() {
                // Ignore the states in the StateStore that form the region
                if *stax == regx.state1 || *stax == regx.state2 {
                    continue;
                }

                // Process stax that is inbetween
                let sqr3 = self.squares.find(&stax).unwrap();
                let cnb1 = sqr3.can_combine(&sqr1);
                match cnb1 {
                    Combinable::False => {
                        //  println!(
                        //      "sqr {} is not combinable with {}",
                        //      &stax, &sqr1.state
                        //  );

                        if sqr1.is_adjacent(&sqr3) {
                            if sqr1.pnc() && sqr3.pnc() {
                                ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                            }
                        } else {
                            new_regs.push_nosups(SomeRegion::new(&sqr1.state, &sqr3.state));
                        }
                    }
                    _ => {}
                } // end match cnb1

                let cnb2 = sqr3.can_combine(&sqr2);
                match cnb2 {
                    Combinable::False => {
                        //println!(
                        //    "*** problem?, sqr {} is not combinable with {}",
                        //    &stax, &sqr2.state
                        //);

                        if sqr2.is_adjacent(&sqr3) {
                            if sqr2.pnc() && sqr3.pnc() {
                                ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                            }
                        } else {
                            new_regs.push_nosups(SomeRegion::new(&sqr2.state, &sqr3.state));
                        }
                    }
                    _ => {}
                } // end match cnb2
            } // next stax (for a square in-between)
        } // next regx closer_reg

        // Apply new regions, set flag to try again
        for regx in new_regs.iter() {
            if regx.active {
                ret_nds.push(SomeNeed::AddSeekEdge { reg: regx.clone() });
            }
        }

        return ret_nds;
    } // end seek_edge_needs1

    // Return SeekEdge needs.
    //
    // When a group is invalidated by a new sample, something was wrong with that group.
    // Possible replacement groups will be greatly decreased in number if the
    // new sample can be used to find an adjacent, dissimilar pair of squares.
    //
    // For Group(s) invalidated by a new sample, there is at least one
    // dissimilar square pair in the group.
    //
    // If a dissimilar pair is not adjacent, the region formed by the pair
    // is added to the action seek_edge RegionStore.
    pub fn seek_edge_needs2(&self, cur_state: &SomeState) -> NeedStore {
        let mut ret_nds = NeedStore::new();

        // Get seek edge needs, scan the action seek_edge RegionStore.
        for regx in self.seek_edge.iter() {
            if regx.active == false {
                continue;
            }

            let stas_in = self.squares.stas_in_reg(&regx);

            if stas_in.len() == 2 {
                // No square between region.state1 and region.state2, seek one

                // Get list of one-bit masks representing the bits different of the
                // two regions states.
                let dif_bits = MaskStore {
                    avec: regx.x_mask().split(),
                };

                // Select a random set of single-bit masks, up to one-half of the number of differences.
                // So if the region states are 10 or 11 bits different, a state 5 bits different from
                // one of the two states will be sought.  So the number of bit differences should go down
                // 50% on each cycle.
                let indicies = self.random_x_of_n(dif_bits.len() / 2, dif_bits.len());

                //let mut dif_msk = SomeMask::new(cur_state.bts.new_low());
                let mut dif_msk = SomeMask::new(SomeBits::new_low(cur_state.num_ints()));

                let mut inx = 0;
                for mskx in dif_bits.iter() {
                    if indicies.contains(&inx) {
                        dif_msk = dif_msk.m_or(mskx);
                    }
                    inx += 1;
                }

                // Randomly choose which state to use to calculate the target state from
                let mut statex = &regx.state2;
                let choice = rand::thread_rng().gen_range(0, 2);
                if choice == 0 {
                    statex = &regx.state1;
                }

                // Calculate the target inbetween
                let seek_state = SomeState::new(statex.bts.b_xor(&dif_msk.bts));

                // Make need for seek_state
                ret_nds.push(SomeNeed::SeekEdge {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: seek_state,
                    in_group: regx.clone(),
                });
                continue;
            }

            // At least one square was found inbetween, possibly from a previous
            // SeekEdge need being satisfied.

            // Get squares represented by the states that form the region
            let sqr1 = self.squares.find(&regx.state1).unwrap();
            let sqr2 = self.squares.find(&regx.state2).unwrap();

            // Find the squares that need more samples to determine combinability
            // with a preferebce for squares that already have the most samples.
            for stax in stas_in.iter() {
                // Ignore states in the StateStore that form the region
                if *stax == regx.state1 || *stax == regx.state2 {
                    continue;
                }

                let sqr3 = self.squares.find(&stax).unwrap();

                let cnb1 = sqr3.can_combine(&sqr1);
                match cnb1 {
                    Combinable::False => {
                        if sqr1.is_adjacent(&sqr3) {
                            if sqr1.pnc() == false {
                                ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: sqr1.state.clone(),
                                    in_group: regx.clone(),
                                });
                            }

                            if sqr3.pnc() == false {
                                ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: sqr3.state.clone(),
                                    in_group: regx.clone(),
                                });
                            }
                        }
                    }
                    Combinable::True => {
                        //  println!(
                        //      "*** sqr {} is combinable with {}",
                        //      &stax, &sqr1.state
                        //  );
                    }
                    Combinable::MoreSamplesNeeded => {
                        if sqr3.pn() < sqr1.pn() {
                            ret_nds.push(SomeNeed::SeekEdge {
                                dom_num: 0, // set this in domain get_needs
                                act_num: self.num,
                                targ_state: sqr3.state.clone(),
                                in_group: regx.clone(),
                            });
                        }
                        if sqr1.pn() < sqr3.pn() {
                            ret_nds.push(SomeNeed::SeekEdge {
                                dom_num: 0, // set this in domain get_needs
                                act_num: self.num,
                                targ_state: sqr1.state.clone(),
                                in_group: regx.clone(),
                            });
                        }
                    }
                } // end match cnb1

                let cnb2 = sqr3.can_combine(&sqr2);
                match cnb2 {
                    Combinable::False => {
                        if sqr2.is_adjacent(&sqr3) {
                            if sqr2.pnc() == false {
                                ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: sqr2.state.clone(),
                                    in_group: regx.clone(),
                                });
                            }

                            if sqr3.pnc() == false {
                                ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: sqr3.state.clone(),
                                    in_group: regx.clone(),
                                });
                            }
                        }
                    }
                    Combinable::True => {
                        //  println!(
                        //      "*** sqr {} is combinable with {}",
                        //      &stax, &sqr2.state
                        //  );
                    }
                    Combinable::MoreSamplesNeeded => {
                        if sqr3.pn() < sqr2.pn() {
                            ret_nds.push(SomeNeed::SeekEdge {
                                dom_num: 0, // set this in domain get_needs
                                act_num: self.num,
                                targ_state: sqr3.state.clone(),
                                in_group: regx.clone(),
                            });
                        }
                        if sqr2.pn() < sqr3.pn() {
                            ret_nds.push(SomeNeed::SeekEdge {
                                dom_num: 0, // set this in domain get_needs
                                act_num: self.num,
                                targ_state: sqr2.state.clone(),
                                in_group: regx.clone(),
                            });
                        }
                    }
                } // end match cnb2
            } // next stax (for a square in-between)
        } // next regx

        ret_nds
    } // end seek_edge_needs2

    // Get additional sample needs for the states that form a group
    fn additional_group_state_samples(&self) -> NeedStore {
        //println!("additional_group_state_sample");
        let mut ret_nds = NeedStore::new();

        for grpx in self.groups.iter() {
            if grpx.active == false {
                continue;
            }

            let sqrx = self.squares.find(&grpx.region.state1).unwrap();

            if sqrx.state != grpx.region.state1 {
                panic!("{} ne {}", sqrx.state, grpx.region.state1);
            }
            if sqrx.pnc() == false {
                ret_nds.push(SomeNeed::StateAdditionalSample {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sqrx.state.clone(),
                    grp_reg: grpx.region.clone(),
                    far: grpx.region.state2.clone(),
                });
            }

            let sqrx = self.squares.find(&grpx.region.state2).unwrap();
            if sqrx.state != grpx.region.state2 {
                panic!("{} ne {}", sqrx.state, grpx.region.state2);
            }
            if sqrx.pnc() == false {
                ret_nds.push(SomeNeed::StateAdditionalSample {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sqrx.state.clone(),
                    grp_reg: grpx.region.clone(),
                    far: grpx.region.state1.clone(),
                });
            }
        } // next grpx

        ret_nds
    } // end additional_group_state_samples

    // Return expand needs for groups.
    fn expand_needs(&mut self) -> NeedStore {
        //println!("expand_needs");
        let mut ret_nds = NeedStore::new();

        //        let regs = self.groups.regions();
        //        let states_in_1 = self.squares.states_in_1_region(&regs);

        for grpx in self.groups.iter() {
            if grpx.active == false {
                continue;
            }

            let not_x = grpx.not_x_expand.m_and(&self.x_mask);

            if not_x.is_low() {
                continue;
            }

            // Get a vector of one-bit masks
            let check_bits = MaskStore {
                avec: not_x.split(),
            };

            // Check for expansion, or single-bit adjacent external
            for bitx in check_bits.iter() {
                let reg_exp = grpx.region.toggle_bits(bitx);

                // At least two squares should be in the group, so in the expanded region
                //let sqr_stas = self.squares.stas_in_reg(&reg_exp);

                let reg_both = reg_exp.union(&grpx.region); // one bit so double the "area"

                let mut ndsx = self.possible_group_needs(&reg_both, 1);

                // println!(
                //     "expand_needs, act {}, check reg {} needs {} for expansion of group {}",
                //     &self.num, &reg_both, &ndsx, &grpx.region
                // );

                if ndsx.len() == 0 {
                    ret_nds.push(SomeNeed::ClearGroupExpandBit {
                        group_region: grpx.region.clone(),
                        mbit: bitx.clone(),
                    });
                } else {
                    ret_nds.append(&mut ndsx);
                }
            } // next bitx
        } // next grpx

        ret_nds
    }

    // Given a region look for far needs
    //
    // Return needs for first or more samples to reach pn EQ.
    fn far_needs(&self, aregion: &SomeRegion, stas_in_reg: &StateStore) -> NeedStore {
        //println!("far_needs");
        let mut ret_nds = NeedStore::new();

        assert!(stas_in_reg.len() > 0);

        // Get states representing squares with the highest number of results
        let mut max_results = 0;
        let mut stas_max_results = StateStore::new();
        for stax in stas_in_reg.iter() {
            let sqrx = self.squares.find(&stax).unwrap();

            let mut rslts_cnt = sqrx.len_results();

            let sta_far = aregion.far_state(&sqrx.state);
            if let Some(sqry) = self.squares.find(&sta_far) {
                rslts_cnt += sqry.len_results();
            }

            if rslts_cnt > max_results {
                max_results = rslts_cnt;
                stas_max_results = StateStore::new();
            }

            if rslts_cnt == max_results {
                stas_max_results.push(stax.clone());
            }
        } // next stax

        for stax in stas_max_results.iter() {
            let sta_far = aregion.far_state(&stax);

            ret_nds.push(SomeNeed::AStateMakeGroup {
                dom_num: 0, // set this in domain get_needs
                act_num: self.num,
                targ_state: sta_far.clone(),
                for_reg: aregion.clone(),
                far: stax.clone(),
                num_x: aregion.num_x(),
            });
        }

        ret_nds
    } // end far_needs

    // Check for squares-in-one-group (anchor) needs.
    //
    // Seek adjacent samples outside the group, and the far square
    // in the group.
    //
    // If an external, adjacent, square is combinable with the anchor,
    // return a need to make a group of the pair.
    //
    // Recheck the rating of the current anchor, and other possible anchors,
    // in case the anchor should be changed.
    fn confirm_groups_needs(&mut self) -> NeedStore {
        //println!("confirm_groups_needs");
        let mut ret_nds = NeedStore::new();

        let regs = self.groups.regions();

        let states1: StateStore = self.squares.states_in_1_region(&regs);

        //println!("Act {}, states in one region {}", self.num, states1);

        // Find squares in one group for each group, that may be an anchor
        for greg in regs.iter() {
            let grpx = self.groups.find_mut(greg).unwrap();

            let stsin = greg.states_in(&states1); // The states in greg and no other group

            // println!("Act {} Group {} States {}", self.num, greg, stsin);

            if let Some(stax) = &grpx.anchor {
                if stsin.contains(&stax) == false {
                    grpx.set_anchor_off();
                }
            }

            if stsin.len() == 0 {
                continue;
            }

            let non_x_mask = grpx.not_x_confirm.m_and(&self.x_mask);

            // Get the bit masks on non-X bit-positions in greg
            let non_x_msks = non_x_mask.split();

            // For each state, sta1, only in the group region, greg:
            //
            //  Calculate each state, sta_adj, adjacent to sta1, outside of greg.
            //
            //  Calculate a rate for each sta1 option, based on the number of adjacent states
            //  in only one group.
            let mut max_num = 0;

            // Create a StateStore composed of anchor, far, adjacent-external
            let mut cfmv_max = Vec::<StateStore>::new();

            for sta1 in stsin.iter() {
                let mut cfmx = StateStore::new();

                cfmx.push(sta1.clone());

                let mut cnt = 1;

                // Rate anchor
                if let Some(sqrx) = self.squares.find(&sta1) {
                    if sqrx.pnc() {
                        cnt += 5;
                    } else {
                        cnt += sqrx.len_results();
                    }
                }

                cfmx.push(greg.far_state(&sta1));

                // Rate far state
                if let Some(sqrx) = self.squares.find(&cfmx[1]) {
                    if sqrx.pnc() {
                        cnt += 5;
                    } else {
                        cnt += sqrx.len_results();
                    }
                }

                // Rate adjacent external states
                for non_x_bit in non_x_msks.iter() {
                    let sta_adj = SomeState::new(sta1.bts.b_xor(&non_x_bit.bts));
                    //println!(
                    //    "checking {} adjacent to {} external to {}",
                    //    &sta_adj, &sta1, &greg
                    // );

                    if states1.contains(&sta_adj) {
                        //println!("{} is in only one group", &sta_adj);
                        cnt += 100;
                    }

                    cfmx.push(sta_adj);

                    if let Some(sqrx) = self.squares.find(&sta1) {
                        if sqrx.pnc() {
                            cnt += 5;
                        } else {
                            cnt += sqrx.len_results();
                        }
                    }
                } // next non_x_bit

                //println!("group {} anchor {} rating {}", &greg, &cfmx[0], cnt);

                // Accumulate highest rated anchors
                if cnt > max_num {
                    max_num = cnt;
                    cfmv_max = Vec::<StateStore>::new();
                }

                if cnt == max_num {
                    cfmv_max.push(cfmx);
                }
            } // next sta1

            // Check if a confirmed group anchor is still rated the best
            // If so, skip further processing.
            if grpx.confirmed {
                if let Some(anchor) = &grpx.anchor {
                    // handle the rare case of anchors with the same rating
                    let mut in_flag = false;
                    for crfx in cfmv_max.iter() {
                        if crfx[0] == *anchor {
                            in_flag = true;
                            break;
                        }
                    }

                    if in_flag {
                        // println!(
                        //     "group {} anchor {} rating confirmed at {}",
                        //     &greg, &anchor, max_num
                        //  );
                        continue;
                    }
                }
            }

            // Select an anchor
            let mut cfm_max = &cfmv_max[0];
            if cfmv_max.len() > 1 {
                cfm_max = &cfmv_max[rand::thread_rng().gen_range(0, cfmv_max.len())];
            }

            // If any external adjacent states have not been sampled, or not enough,
            // return needs for that.
            //
            // If the group far state has not been sampled, or not enough, return a need for that.
            //
            // Else confirm the group.
            let anchor_sta = &cfm_max[0];

            let anchor_sqr = self.squares.find(anchor_sta).unwrap();
            if anchor_sqr.pnc() {
                // println!("group {} anchor {} pnc", &greg, &anchor_sta);
            } else {
                // Get additional samples of the anchor
                ret_nds.push(SomeNeed::ConfirmGroup {
                    dom_num: 0, // will be set in domain code
                    act_num: self.num,
                    anchor: anchor_sta.clone(),
                    targ_state: anchor_sta.clone(),
                    for_group: greg.clone(),
                });
                continue; // next greg
            }

            // Check each adjacent external state
            let mut nds_grp = NeedStore::new(); // needs for more samples
            let mut nds_grp_add = NeedStore::new(); // needs for added group
            let mut grp_clear_bit = NeedStore::new(); // clear bit need for group

            for inx in 2..cfm_max.len() {
                let adj_sta = &cfm_max[inx];

                let a_bit_msk = SomeMask::new(adj_sta.bts.b_xor(&anchor_sta.bts));

                if grpx.not_x_confirm_bit_set(&a_bit_msk) == false {
                    continue;
                }

                //println!("*** for group {} checking adj sqr {}", &greg, &adj_sta);

                if let Some(adj_sqr) = self.squares.find(adj_sta) {
                    if adj_sqr.pnc() {
                        let anchor_sqr = self.squares.find(anchor_sta).unwrap();

                        if anchor_sqr.can_combine(&adj_sqr) == Combinable::True {
                            nds_grp_add.push(SomeNeed::AddGroup {
                                act_num: self.num,
                                group_region: SomeRegion::new(&anchor_sta, &adj_sta),
                            });
                        } else {
                            // Set that bit off in the check mask
                            grp_clear_bit.push(SomeNeed::ClearGroupCheckBit {
                                group_region: greg.clone(),
                                mbit: SomeMask::new(anchor_sta.bts.b_xor(&adj_sta.bts)),
                            });
                        }
                    } else {
                        nds_grp.push(SomeNeed::ConfirmGroup {
                            dom_num: 0, // will be set in domain code
                            act_num: self.num,
                            anchor: anchor_sta.clone(),
                            targ_state: adj_sta.clone(),
                            for_group: greg.clone(),
                        });
                    }
                } else {
                    nds_grp.push(SomeNeed::ConfirmGroup {
                        dom_num: 0, // will be set in domain code
                        act_num: self.num,
                        anchor: anchor_sta.clone(),
                        targ_state: adj_sta.clone(),
                        for_group: greg.clone(),
                    });
                }
            } // next inx in cfm_max

            if nds_grp_add.len() > 0 {
                // println!("*** nds_grp_add {}", &nds_grp_add);
                ret_nds.append(&mut nds_grp_add);
                continue;
            }

            if grp_clear_bit.len() > 0 {
                //  println!("*** nds_grp_clear_bit {}", &grp_clear_bit);
                ret_nds.append(&mut grp_clear_bit);
            }

            if nds_grp.len() > 0 {
                //  println!("*** nds_grp {}", &nds_grp);
                ret_nds.append(&mut nds_grp);
                continue;
            }

            //   println!("grp {} check far", &greg);

            // Process far state, after the anchor and adjacent, external, checks have been made.
            // If its dissimilar to the anchor, the group will be invalidated.
            // Fairly cheap, quick, somewhat accurate, method of establishing a region,
            // instead of checking all states adjacent-internal to the anchor.
            if let Some(sqrf) = self.squares.find(&cfm_max[1]) {
                if sqrf.pnc() {
                    // Set the group confirmed
                    ret_nds.push(SomeNeed::SetGroupConfirmed {
                        group_region: greg.clone(),
                        cstate: anchor_sta.clone(),
                    });
                } else {
                    // Get additional samples of the far state
                    ret_nds.push(SomeNeed::ConfirmGroup {
                        dom_num: 0, // will be set in domain code
                        act_num: self.num,
                        anchor: anchor_sta.clone(),
                        targ_state: cfm_max[1].clone(),
                        for_group: greg.clone(),
                    });
                }
            } else {
                // Get the first sample of the far state
                ret_nds.push(SomeNeed::ConfirmGroup {
                    dom_num: 0, // will be set in domain code
                    act_num: self.num,
                    anchor: anchor_sta.clone(),
                    targ_state: cfm_max[1].clone(),
                    for_group: greg.clone(),
                });
            }
        } // next greg
        ret_nds
    } // end confirm_group_needs

    // Check overlapping intersection for combinations
    fn group_pair_needs(&self) -> NeedStore {
        //println!("group_pair_needs");
        let mut nds = NeedStore::new();

        // Check every pair of groups
        for inx in 0..self.groups.len() {
            let grpx = &self.groups[inx];

            if grpx.active == false {
                continue;
            }

            // Pair grpx with every group after it in the GroupStore
            for iny in (inx + 1)..self.groups.len() {
                let grpy = &self.groups[iny];

                if grpy.active == false {
                    continue;
                }

                if grpx.region.intersects(&grpy.region) {
                    let mut ndx = self.groups_intersection_needs(&grpx, &grpy);
                    if ndx.len() > 0 {
                        nds.append(&mut ndx);
                    }
                } else if grpx.region.is_adjacent(&grpy.region) {
                    if grpx.pn == grpy.pn {
                        let mut ndx = self.groups_adjacent_needs(&grpx, &grpy);
                        if ndx.len() > 0 {
                            nds.append(&mut ndx);
                        }
                    } // end if pn ==
                } // end if regions adjacent
            } // next iny
        } // next inx

        nds
    } // end group_pair_needs

    // Check for needs, for making a given region into a group.
    // A need may be to take more samples, or just add the group.
    fn possible_group_needs(&self, reg_grp: &SomeRegion, _from: usize) -> NeedStore {
        //println!("possible_group_needs from {}", from);
        let mut nds = NeedStore::new();

        // Get squares in the region.
        let stas_in_reg = self.squares.stas_in_reg(&reg_grp);

        // If no squares, seek the highest and lowest squares
        if stas_in_reg.len() == 0 {
            //println!("possible_group_needs 1");
            let sta1 = reg_grp.state1.s_or(&reg_grp.state2); // highest square key
            let sta2 = reg_grp.state1.s_and(&reg_grp.state2); // lowest square key

            nds.push(SomeNeed::AStateMakeGroup {
                // nds should be empty so far
                dom_num: 0, // set this in domain get_needs
                act_num: self.num,
                targ_state: sta1.clone(),
                for_reg: reg_grp.clone(),
                far: sta2.clone(),
                num_x: reg_grp.num_x(),
            });

            nds.push(SomeNeed::AStateMakeGroup {
                dom_num: 0, // set this in domain get_needs
                act_num: self.num,
                targ_state: sta1,
                for_reg: reg_grp.clone(),
                far: sta2,
                num_x: reg_grp.num_x(),
            });

            return nds;
        }

        // At this point, at least one existing square is in the region
        if stas_in_reg.len() == 1 {
            let sta_far = reg_grp.far_state(&stas_in_reg[0]);

            nds.push(SomeNeed::AStateMakeGroup {
                dom_num: 0, // will be set later
                act_num: self.num,
                targ_state: sta_far,
                for_reg: reg_grp.clone(),
                far: stas_in_reg[0].clone(),
                num_x: reg_grp.num_x(),
            });

            return nds;
        }

        // See if a pair of squares defines the region
        let max_pn = self.squares.max_pn(&stas_in_reg);
        let pair_stas = reg_grp.defining_pairs(&stas_in_reg);

        if pair_stas.len() == 0 {
            return self.far_needs(&reg_grp, &stas_in_reg);
        }

        let mut inx = 0;
        while inx < pair_stas.len() {
            let sqrx = self.squares.find(&pair_stas[inx]).unwrap();
            let sqry = self.squares.find(&pair_stas[inx + 1]).unwrap();

            if sqrx.pn() != sqry.pn() {
                inx += 2;
                continue;
            }

            let cmbl = self.can_combine(&sqrx, &sqry);

            if cmbl == Combinable::MoreSamplesNeeded {
                if sqrx.pn() < max_pn {
                    if sqrx.pnc() {
                        return NeedStore::new();
                    } else {
                        nds.push(SomeNeed::StateAdditionalSample {
                            dom_num: 0, // set this in domain get_needs
                            act_num: self.num,
                            targ_state: sqrx.state.clone(),
                            grp_reg: reg_grp.clone(),
                            far: sqry.state.clone(),
                        });
                    }
                    inx += 2;
                    continue;
                }

                if sqry.pn() < max_pn {
                    if sqry.pnc() {
                        return NeedStore::new();
                    } else {
                        nds.push(SomeNeed::StateAdditionalSample {
                            dom_num: 0, // set this in domain get_needs
                            act_num: self.num,
                            targ_state: sqry.state.clone(),
                            grp_reg: reg_grp.clone(),
                            far: sqrx.state.clone(),
                        });
                    }
                    inx += 2;
                    continue;
                }
            }

            if cmbl == Combinable::True {
                nds = NeedStore::new();
                nds.push(SomeNeed::AddGroup {
                    act_num: self.num,
                    group_region: SomeRegion::new(&sqrx.state, &sqry.state),
                });
                return nds;
            } else if cmbl == Combinable::False {
                return NeedStore::new();
            }
            inx += 2;
        } // next inx
        nds
    } // end possible_group_needs

    // Check two intersecting groups for needs.
    // Possibly combining to groups.
    // Possibly checking for a contradictatory intersection.
    fn groups_intersection_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //println!("groups_intersection_needs");

        let reg_both = grpx.region.union(&grpy.region);

        if grpx.pn == Pn::Unpredictable && grpy.pn == Pn::Unpredictable {
            return self.possible_group_needs(&reg_both, 2);
        }

        let reg_int = grpx.region.intersection(&grpy.region);

        if grpx.pn != grpy.pn {
            let mut nds = NeedStore::new();
            nds.push(self.cont_int_region_needs(&reg_int, &grpx, &grpy));
            return nds;
        }

        let rulsx = grpx.rules.restrict_initial_region(&reg_int);
        let rulsy = grpy.rules.restrict_initial_region(&reg_int);

        // If contradictory, return needs to resolve
        if rulsx != rulsy {
            let mut nds = NeedStore::new();

            // Check if a valid sub-region of the intersection exists
            if let Some(rulsxy) = rulsx.intersection(&rulsy) {
                // A valid sub-union exists, seek a sample in intersection that is not in rulsxy.initial_region
                let ok_reg = rulsxy.initial_region();

                let regy = reg_int.far_reg(&ok_reg); // to avoid subtraction, use the far sub-region

                nds.push(self.cont_int_region_needs(&regy, &grpx, &grpy));
            } else {
                nds.push(self.cont_int_region_needs(&reg_int, &grpx, &grpy));
            }
            return nds;
        }

        // Rules are the same, see if the two groups can be combined
        return self.possible_group_needs(&reg_both, 3);
    } // end groups_intersection_needs

    // Get needs for two adjacent groups, with the same pn rating.
    fn groups_adjacent_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //println!("groups_adjacent_needs");
        let nds = NeedStore::new();

        if grpx.pn != grpy.pn {
            return nds;
        }

        match grpx.pn {
            Pn::Unpredictable => {
                // If the regions have the same X-bit pattern, they can be combined
                if grpx.region.x_mask() == grpy.region.x_mask() {
                    let regx = grpx.region.union(&grpy.region);
                    return self.possible_group_needs(&regx, 4);
                }

                let regz = grpx.region.overlapping_part(&grpy.region);

                if self.groups.any_superset_of(&regz) {
                    return nds;
                }

                //println!("groups_adjacent_needs: {} and {}", &grpx.region, &grpy.region);
                return self.possible_group_needs(&regz, 5);
            }
            _ => {
                // If the regions have the same X-bit pattern, the overlapping part may be a superset
                // of both.
                if grpx.region.x_mask() == grpy.region.x_mask() {
                    if let Some(_) = grpx.rules.union(&grpy.rules) {
                        let regx = grpx.region.union(&grpy.region);
                        return self.possible_group_needs(&regx, 6);
                    }
                }

                // Check if any valid rules can be found in the overlapping region.
                let reg_ov = grpx.region.overlapping_part(&grpy.region);

                if let Some(rulesx) = grpx
                    .rules
                    .restrict_initial_region(&reg_ov)
                    .union_prune(&grpy.rules.restrict_initial_region(&reg_ov))
                {
                    let regz = rulesx.initial_region();

                    if regz.intersects(&grpx.region) && regz.intersects(&grpy.region) {
                        if self.groups.any_superset_of(&regz) {
                            return nds;
                        }

                        println!(
                            "overlap for act {} grp {} grp {} region {} rules {}",
                            self.num, grpx.region, grpy.region, regz, rulesx
                        );

                        return self.possible_group_needs(&regz, 7);
                    }
                } // end if let Some
            } // end match pn default
        } // end match pn for adjacent regions
        nds
    } // end groups_adjacent_needs

    // For a contradictory intersection, return a need for more samples.
    // If no prior samples in the intersection, seek one.
    // If a prior sampled square is pnc, panic.
    // If prior samples found, seek additional samples.
    fn cont_int_region_needs(
        &self,
        regx: &SomeRegion,
        grpx: &SomeGroup,
        grpy: &SomeGroup,
    ) -> SomeNeed {
        //println!("cont_int_region_needs");
        // Check for any squares in the region
        let stas_in = self.squares.stas_in_reg(regx);

        if stas_in.len() == 0 {
            return SomeNeed::ContradictoryIntersection {
                dom_num: 0, // set this in domain get_needs
                act_num: self.num,
                goal_reg: regx.clone(),
                group1: grpx.region.clone(),
                ruls1: grpx.rules.restrict_initial_region(regx),
                group2: grpy.region.clone(),
                ruls2: grpy.rules.restrict_initial_region(regx),
            };
        }

        // Some samples have been taken in the region

        // Find a square with the highest number of samples
        // If any are pnc, panic.
        let mut max_rslts = 0;
        let mut stas_check = StateStore::new();
        for stax in stas_in.iter() {
            let sqrz = self.squares.find(stax).unwrap();
            if sqrz.pnc() {
                panic!(
                    "Square {} is pnc in contradictory intersection {}\n  of group {}\n and group {}",
                    &sqrz, &regx, &grpx, &grpy
                );
            }

            if sqrz.len_results() > max_rslts {
                max_rslts = sqrz.len_results();
                stas_check = StateStore::new();
            }

            if sqrz.len_results() == max_rslts {
                stas_check.push(sqrz.state.clone());
            }
        }

        let mut inx = 0;
        if stas_check.len() > 1 {
            inx = rand::thread_rng().gen_range(0, stas_check.len());
        }

        SomeNeed::ContradictoryIntersection {
            dom_num: 0, // set this in domain get_needs
            act_num: self.num,
            goal_reg: SomeRegion::new(&stas_check[inx], &stas_check[inx]),
            group1: grpx.region.clone(),
            ruls1: grpx.rules.restrict_initial_region(regx),
            group2: grpy.region.clone(),
            ruls2: grpy.rules.restrict_initial_region(regx),
        }
    } // end cont_int_region_needs

    // Get possible steps that can be used to make at least one
    // change in a given rule.
    pub fn get_steps(&self, achange: &SomeChange) -> StepStore {
        let mut stps = StepStore::new();

        for grpx in &self.groups.avec {
            if grpx.active == false {
                continue;
            }

            match grpx.pn {
                Pn::One => {
                    // Find bit changes that are desired
                    let ones = grpx.rules[0].b10.m_and(&achange.b10);
                    let zeros = grpx.rules[0].b01.m_and(&achange.b01);

                    if ones.is_not_low() || zeros.is_not_low() {
                        let mut xrule = grpx.rules[0].clone();

                        if ones.is_not_low() {
                            xrule = xrule.set_initial_to_ones(&ones);
                        }

                        if zeros.is_not_low() {
                            xrule = xrule.set_initial_to_zeros(&zeros);
                        }

                        let regx = xrule.initial_region();
                        stps.push(SomeStep::new(self.num, xrule, false, regx));
                    }
                }
                Pn::Two => {
                    for rulx in &grpx.rules.avec {
                        let ones = rulx.b10.m_and(&achange.b10);
                        let zeros = rulx.b01.m_and(&achange.b01);

                        if ones.is_not_low() || zeros.is_not_low() {
                            let mut xrule = rulx.clone();

                            if ones.is_not_low() {
                                xrule = xrule.set_initial_to_ones(&ones);
                            }

                            if zeros.is_not_low() {
                                xrule = xrule.set_initial_to_zeros(&zeros);
                            }

                            let regx = xrule.initial_region();

                            let stas = self.squares.stas_in_reg(&regx);

                            let mut found = false;
                            for stax in stas.iter() {
                                let sqrx = self.squares.find(&stax).unwrap();

                                let next_result = sqrx.next_result(&grpx.rules);

                                let expected_result = rulx.result_from_initial_state(stax);

                                if next_result == expected_result {
                                    let stpx = SomeStep::new(
                                        self.num,
                                        xrule.restrict_initial_region(&SomeRegion::new(
                                            &stax, &stax,
                                        )),
                                        false,
                                        SomeRegion::new(&next_result, &next_result),
                                    );
                                    println!(
                                        "pn2 rul {} sqr {} next rslt {} found. making step {}",
                                        &rulx, &stax, &next_result, &stpx
                                    );
                                    stps.push(stpx);
                                    found = true;
                                } // end if
                            } // next stax

                            if found == false {
                                let stpx = SomeStep::new(self.num, xrule, true, regx);
                                println!("pn2 rul {} making generic step {}", &rulx, &stpx);
                                stps.push(stpx);
                            }
                        } // next low or low
                    } // next rulx
                } // end match Two
                Pn::Unpredictable => {}
            } // end match pn
        } // next grpx

        stps
    } // end get_steps

    //    pub fn new_x_bits(&mut self, bitsx: &SomeMask) {
    //        self.x_mask = self.x_mask.m_or(&bitsx);
    //        self.groups.new_x_bits(&bitsx);
    //    }

    // Return true if a group exists and is active
    pub fn group_exists_and_active(&self, group_reg: &SomeRegion) -> bool {
        if let Some(grpx) = self.groups.find(group_reg) {
            if grpx.active {
                return true;
            }
        }
        return false;
    }

    // Get a random choice of a number of unique numbers (num_results) to a
    // given number of positions, 0, 1 .. -> the_len (exclusive).
    // random 2 of 5 -> [0, 3]
    pub fn random_x_of_n(&self, num_results: usize, the_len: usize) -> Vec<usize> {
        if num_results < 1 || num_results >= the_len {
            panic!(
                "random_x_of_n: Number results {} is not right for length {}",
                &num_results, &the_len
            );
        }

        let mut yvec = Vec::<usize>::new();

        // Get a random number in each itoration.
        // Some duplicates may happen so the number of iterations will
        // be GE the number of desired results.
        while yvec.len() < num_results {
            let inx = rand::thread_rng().gen_range(0, &the_len);
            //println!("inx = {}", inx);
            if yvec.contains(&inx) {
            } else {
                yvec.push(inx);
            }
        }
        yvec
    } // end random_indicies

    // Find squares whose rules can be combined with a given squares rules.
    // Check if any included squares invalidate a combination.
    // Remove subset combinations.
    // Return the regions resulting from successful combinations.
    pub fn possible_group_regions(&self, sqrx: &SomeSquare) -> RegionStore {
        let mut rsx = RegionStore::new();

        for (key, sqry) in &self.squares.ahash {
            if *key == sqrx.state {
                continue;
            }

            if sqrx.pn() != sqry.pn() {
                continue;
            }

            let regx = SomeRegion::new(&sqrx.state, &sqry.state);

            if rsx.any_superset_of(&regx) {
                continue;
            }

            if self.can_combine(&sqrx, &sqry) == Combinable::True {
                rsx.push_nosubs(regx);
            }
        } // end for

        for regx in rsx.iter() {
            let sqry = self.squares.find(&regx.state2).unwrap();
            println!(
                "\nSquare {} can combine with\nSquare {}\ngiving {}\n",
                sqrx, sqry, regx,
            );
        }

        //println!("regions for new groups {}", rsx.str());
        rsx
    } // end possible_group_regions

    // Aggregate all predictable bit changes that are possible.
    // Repeated sampling of squares will tend to increase the number
    // of possible bits changes.  Some changes, like going from
    // predictable to unpredictable, could decrease them.
    pub fn possible_bit_changes(&self) -> SomeChange {
        let mut ret_cng = SomeChange::new(self.num_ints);

        for grpx in &self.groups.avec {
            if grpx.active == false {
                continue;
            }

            match grpx.pn {
                Pn::One => {
                    // Find bit changes that are desired
                    ret_cng.b10 = ret_cng.b01.m_or(&grpx.rules[0].b10);
                    ret_cng.b01 = ret_cng.b01.m_or(&grpx.rules[0].b01);
                }
                Pn::Two => {
                    for rulx in &grpx.rules.avec {
                        ret_cng.b10 = ret_cng.b01.m_or(&rulx.b10);
                        ret_cng.b01 = ret_cng.b01.m_or(&rulx.b01);
                    } // next rulx
                } // end match Two
                Pn::Unpredictable => {}
            } // end match pn
        } // next grpx

        ret_cng
    }
}
