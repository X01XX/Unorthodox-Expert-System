// Implement an Action struct,
// which represents the best guess of the expected responses
// of executing an action.
//
//use crate::bits::SomeBits;
use crate::bitsstore::BitsStore;
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
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::square::SomeSquare;
use crate::squarestore::SquareStore;
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::stepstore::StepStore;

use std::fmt;
extern crate rand;
use rand::Rng;

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("A(ID: ");

        rc_str.push_str(&self.num.to_string());

        if self.closer_regs.any_active() {
            rc_str.push_str(&format!(" clsr: {}", self.closer_regs));
        }

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
                    &grpx,
                    &stas_in.len(),
                    &cnt,
                ));

                if grpx.not_x_check.is_low() == false {
                    rc_str.push_str(&format!(" chk: {}", &grpx.not_x_check));
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

pub struct SomeAction {
    pub num: usize,
    pub groups: GroupStore,
    pub squares: SquareStore,
    to_run: fn(&SomeState, usize) -> SomeState,
    pub closer_regs: RegionStore,
}

impl SomeAction {
    pub fn new(num: usize, fx: fn(&SomeState, usize) -> SomeState) -> Self {
        SomeAction {
            num,
            groups: GroupStore::new(),
            squares: SquareStore::new(),
            to_run: fx,
            closer_regs: RegionStore::new(),
        }
    }

    // Significant effort is used to generate needs, so take in the
    // state that will satisfy the need, and the need itself for clues in
    // processing the result.
    pub fn take_action_need(
        &mut self,
        cur: &SomeState,
        ndx: &SomeNeed,
        max_region: &SomeRegion,
        hv: usize,
    ) -> SomeState {
        //println!("take_action_need {}", &ndx);
        // Get the result, the sample is cur -> new_state

        let new_state = (self.to_run)(cur, hv);
        //self.store_sample(&cur, &new_state, &max_region);

        // Process each kind of need
        match ndx {
            SomeNeed::AStateMakeGroup {
                act_num: _an,
                targ_state: sta,
                for_reg,
                far,
                num_x: _,
            } => {
                self.store_sample(&cur, &new_state);

                // Form the rules, make the group
                if let Some(sqrx) = self.squares.find(&sta) {
                    if let Some(sqry) = self.squares.find(&far) {
                        if sqrx.can_combine(&sqry) == Combinable::True {
                            if sqrx.pn() == Pn::Unpredictable {
                                if self.groups.any_superset_of(&for_reg) == false {
                                    self.groups.push(SomeGroup::new(
                                        &sqrx.state,
                                        &sqry.state,
                                        RuleStore::new(),
                                        self.num,
                                        &max_region,
                                    ));
                                } else {
                                    panic!(
                                        "Supersets found for new group (1) {} in {}",
                                        for_reg,
                                        SomeRegion::new(&sqrx.state, &sqry.state)
                                    );
                                }
                            } else {
                                if let Some(rulsxy) = sqrx.rules.union(&sqry.rules) {
                                    if self.squares.verify_combination(
                                        &SomeRegion::new(&sqrx.state, &sqry.state),
                                        &rulsxy,
                                        &sqrx.pn(),
                                    ) {
                                        println!("Adding group: {}", &rulsxy[0].initial_region());
                                        if self.groups.any_superset_of(&rulsxy[0].initial_region())
                                        {
                                            panic!(
                                                "Supersets found for new group {} in {}",
                                                rulsxy.initial_region(),
                                                self.groups
                                                    .supersets_of(&rulsxy[0].initial_region())
                                            );
                                        } else {
                                            self.groups.push(SomeGroup::new(
                                                &sqrx.state,
                                                &sqry.state,
                                                rulsxy,
                                                self.num,
                                                &max_region,
                                            ));
                                        }
                                    } else {
                                        println!(
                                            "verify_combination = sqr {} sqr {} = false",
                                            sqrx.state, sqry.state
                                        );
                                    } // end if verify_combination
                                } // end if union
                            }
                        } else {
                            println!(
                                "can_combine = sqr {} sqr {} = {}",
                                sqrx.state,
                                sqry.state,
                                sqrx.can_combine(&sqry)
                            );
                        } // end if can combine
                    } else {
                        println!("cannot find sqr {}", &far);
                    } // end if sqry
                } else {
                    println!("cannot find sqr {}", &sta);
                } // endif sqrx

                self.check_square_new_sample(&cur, &max_region);
            } // end process AStateMakeGroup Need

            SomeNeed::InBetween {
                act_num: _,
                targ_state: sta,
                in_group: greg,
            } => {
                self.store_sample(&cur, &new_state);
                self.check_square_new_sample(&cur, &max_region);

                // Form the rules, make the group
                if let Some(sqr1) = self.squares.find(&greg.state1) {
                    if let Some(sqr2) = self.squares.find(&greg.state2) {
                        if let Some(sqr3) = self.squares.find(&sta) {
                            // Process next sample of square in-between for new square and state1 square
                            // Should be different from state1 square or state2 square
                            let cnb1 = sqr3.can_combine(&sqr1);
                            match cnb1 {
                                Combinable::False => {
                                    //println!(
                                    //    "*** good, sqr {} is not combinable with {}",
                                    //    &sqr3.state, &sqr1.state
                                    //);
                                    // check distance, if 1, invalidate region, else push new region
                                    let dist = sqr1.state.s_xor(&sqr3.state).num_one_bits();
                                    if dist == 1 {
                                        //  println!(
                                        //      "Act {} InBetween need, inactivating closer_reg {}",
                                        //      &self.num, &greg
                                        //  );
                                        self.closer_regs.inactivate(greg);
                                    } else {
                                        let even_closer_reg =
                                            SomeRegion::new(&sqr1.state, &sqr3.state);
                                        // println!("Act {} InBetween need, replacing closer_reg {} with {}", &self.num, &greg, &even_closer_reg);
                                        self.closer_regs.push_nosups(even_closer_reg);
                                    }
                                }
                                Combinable::True => {
                                    //println!(
                                    //    "*** sqr {} is combinable with {}",
                                    //    &sqr3.state, &sqr1.state
                                    //);
                                }
                                Combinable::MoreSamplesNeeded => {}
                            } // end match cnb1

                            // Process next sample of square in-between for new square and state2 square
                            // Should be different from state1 square or state2 square
                            let cnb2 = sqr3.can_combine(&sqr2);
                            match cnb2 {
                                Combinable::False => {
                                    // println!(
                                    //     "*** good, sqr {} is not combinable with {}",
                                    //     &sqr3.state, &sqr2.state
                                    // );
                                    // check distance, if 1, invalidate region, else push new region
                                    let dist = sqr2.state.s_xor(&sqr3.state).num_one_bits();
                                    if dist == 1 {
                                        //     println!(
                                        //         "Act: {} InBetween need, inactivating closer_reg {}",
                                        //         &self.num, &greg
                                        //     );
                                        self.closer_regs.inactivate(greg);
                                    } else {
                                        let even_closer_reg =
                                            SomeRegion::new(&sqr2.state, &sqr3.state);
                                        //     println!("Act: {} InBetween need, replacing closer_reg {} with {}", &self.num, &greg, &even_closer_reg);
                                        self.closer_regs.push_nosups(even_closer_reg);
                                    }
                                }
                                Combinable::True => {
                                    // println!(
                                    //     "*** sqr {} is combinable with {}",
                                    //     &sqr3.state, &sqr2.state
                                    // );
                                }
                                Combinable::MoreSamplesNeeded => {}
                            } // end match cnb2
                        } else {
                            panic!("square not found");
                        }
                    } else {
                        panic!("square not found");
                    }
                } else {
                    panic!("square not found");
                }
            }
            _ => {
                // default, store sample, if not in a group, make one
                self.store_sample(&cur, &new_state);
                self.check_square_new_sample(&cur, &max_region);
            }
        } // end match SomeNeed ndx

        new_state
    }

    // Add a sample by user command
    pub fn take_action_arbitrary(
        &mut self,
        init_state: &SomeState,
        rslt_state: &SomeState,
        max_region: &SomeRegion,
    ) {
        //println!("take_action_arbitrary for state {}", init_state);
        self.store_sample(&init_state, &rslt_state);
        self.check_square_new_sample(&init_state, &max_region);
    }

    pub fn take_action_step(
        &mut self,
        cur: &SomeState,
        max_region: &SomeRegion,
        hv: usize,
    ) -> SomeState {
        //println!("take_action_step");

        let new_state = (self.to_run)(cur, hv);

        self.eval_sample_step(cur, &new_state, &max_region);

        new_state
    }

    // Evaluate a sample produced by a step in a plan.
    // If there is an existing square,
    //     update it.
    //     check if it breaks anything
    // else
    //     if it brakes anything
    //         add it as a square
    //
    pub fn eval_sample_step(
        &mut self,
        cur: &SomeState,
        new_state: &SomeState,
        max_region: &SomeRegion,
    ) -> bool {
        // If square exists, update it
        // Use a block to hide mutable borrow from later code
        {
            let t_sqrx = self.squares.find_mut(cur); // see if square exists

            match t_sqrx {
                Some(sqrx) => {
                    // println!("about to add result to sqr {}", cur.str());
                    sqrx.add_result(new_state.clone());

                    if sqrx.changed() {
                        self.check_square_new_sample(cur, &max_region);
                        return true;
                    }

                    return false;
                }
                None => {
                    //println!("No square found for state {}", cur);
                }
            }
        }

        // Get num groups that might be inactivated
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
            self.check_square_new_sample(cur, &max_region);
        }

        true
    }

    // Evaluate a sample produced for a need.
    // It is expected that the sample needs to be stored,
    fn store_sample(&mut self, cur: &SomeState, new_state: &SomeState) -> bool {
        // Get an existing square and update, or create a new square.
        //println!("store_sample");
        let t_sqrx = self.squares.find_mut(cur); // see if square exists

        match t_sqrx {
            Some(sqrx) => {
                // println!("about to add result to sqr {}", cur.str());
                sqrx.add_result(new_state.clone());

                if sqrx.changed() == false {
                    return false;
                }
            }
            None => {
                // println!("No square found for state {}", cur.str());
                self.squares
                    .add(SomeSquare::new(cur.clone(), new_state.clone()));
            }
        }

        //self.check_square_new_sample(&cur, &max_region);

        true
    }

    // Check a square, referenced by state, against valid groups.
    // The square may invalidate some groups.
    // Add a group for the square if the square is in no valid group.
    // If any groups were invalidated, check other squares
    // that are in no groups.
    fn check_square_new_sample(&mut self, key: &SomeState, max_region: &SomeRegion) -> bool {
        //println!("check_square_new_sample");

        // Get number of groups invalidated, which may orphan some squares.
        let regs_invalid = self.validate_groups_new_sample(&key);

        let mut min_dist = 99999;
        let mut close_dif = StateStore::new();

        if let Some(sqrx) = self.squares.find(&key) {
            // Process groups invalidated
            if regs_invalid.len() > 0 {
                let stas_in = self.squares.stas_in_regs(&regs_invalid);

                for stax in stas_in.iter() {
                    if stax == key {
                        continue;
                    }

                    if let Some(sqry) = self.squares.find(&stax) {
                        if sqrx.can_combine(&sqry) == Combinable::False {
                            let num_dif = key.s_xor(&stax).num_one_bits();

                            if num_dif < min_dist {
                                min_dist = num_dif;
                                close_dif = StateStore::new();
                            }

                            if num_dif == min_dist {
                                close_dif.push(stax.clone());
                            }
                        }
                    } else {
                        panic!("cant find square?");
                    } // end if let Some(sqry)
                } // next stax

                println!(
                    "for sqr {}, closest diff sqrs are {}, dist {}",
                    &key, &close_dif, &min_dist
                );

                // Find adjacent close squares
                if close_dif.len() > 0 && min_dist > 1 {
                    // any squares between but need more samples?

                    // just store need info in action struct?
                    self.closer_regs
                        .push_nosups(SomeRegion::new(&key, &close_dif[0]));
                }
            } // end if rgs_invalid.len() > 0
        } else {
            panic!("cant find square?");
        } // end if let Some(sqrx)

        // Create a group for square if needed
        if regs_invalid.len() == 0 {
            if self.groups.num_groups_state_in(&key) == 0 {
                self.create_groups_given_sample(&key, &max_region);
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
                    self.create_groups_given_sample(&keyx, &max_region);
                }
            }
        }

        true
    }

    // Check groups a new sample is in to see if they are still valid.
    // Return a RegionStore of group regions that have been invalidated.
    fn validate_groups_new_sample(&mut self, key: &SomeState) -> RegionStore {
        //println!("validate_groups_new_sample");

        // Square should exist
        let t_sqrx = self.squares.find(&key);

        match t_sqrx {
            Some(sqrx) => {
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
            None => {
                panic!("square should have been found!");
            }
        }
    }

    // Check groups due to a new sample
    // Create a group with the square, if needed.
    // Return the number of groups invalidated
    fn create_groups_given_sample(&mut self, key: &SomeState, max_region: &SomeRegion) {
        //let mut num_grps_invalidated = 0;

        // Square should exist
        let t_sqrx = self.squares.find(&key);

        match t_sqrx {
            Some(sqrx) => {
                // Get num groups inactivated
                //num_grps_invalidated = self.groups.check_square(&sqrx);

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

                    let rsx = self.squares.possible_group_regions(sqrx);

                    if rsx.len() > 0 {
                        // println!("Regions for new groups {}", rsx.str());
                        for regx in rsx.iter() {
                            if self.groups.any_superset_of(regx) == false {
                                if sqrx.pn() == Pn::Unpredictable {
                                    if self.groups.push(SomeGroup::new(
                                        &regx.state1,
                                        &regx.state2,
                                        RuleStore::new(),
                                        self.num,
                                        &max_region,
                                    )) {
                                    } else {
                                        panic!("groups add should have worked!");
                                    }
                                } else {
                                    if let Some(ruls) =
                                        self.squares.rules(&regx.state1, &regx.state2)
                                    {
                                        //println!("Squares with states {}, {} produce ruls {}", &regx.state1.str(), &regx.state2.str(), ruls.str());

                                        if self.groups.push(SomeGroup::new(
                                            &regx.state1,
                                            &regx.state2,
                                            ruls,
                                            self.num,
                                            &max_region,
                                        )) {
                                        } else {
                                            panic!("groups add should have worked!");
                                        }
                                    } else {
                                        panic!("expected this to work");
                                    }
                                }
                            }
                        } // end for regx
                    } else {
                        // Make a single-square group
                        if self.groups.push(SomeGroup::new(
                            &sqrx.state,
                            &sqrx.state,
                            sqrx.rules.clone(),
                            self.num,
                            &max_region,
                        )) {
                            //self.reconfirm = true;
                        }
                    }
                }
            }
            None => {
                panic!("square should have been found!");
            }
        }
        //num_grps_invalidated
    }

    // Get needs for an Action, to improve understanding of the reaction pattern(s).
    // When most needs are satisfied, needs for group confirmation are generated.
    // If a square in-one-group has an adjacent, similar, square, outside
    // of a group, a new group needs to be added, get needs will be rerun.
    pub fn get_needs(&mut self, cur_state: &SomeState, max_region: &SomeRegion) -> NeedStore {
        // println!("Running Action {}::get_needs {}", self.num, cur_state);

        // loop through get_needs until no bookkeeping need is returned.
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

            if in_grp == false {
                nds.push(SomeNeed::StateNotInGroup {
                    act_num: self.num,
                    targ_state: cur_state.clone(),
                });
            }

            let mut ndx = self.in_between_needs(&cur_state);
            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Check for additional samples for group states needs
            let mut ndx = self.additional_group_state_samples();

            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Check for overlapping regions that may be combined

            let mut ndx = self.group_pair_needs();

            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Check for squares in-one-group needs
            //if nds.len() == 0 && self.reconfirm
            if nds.len() == 0 {
                let mut ndx = self.confirm_groups_needs(&max_region);

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

            // Process a few specific needs related to changing the Action or groups.
            for ndx in nds.iter_mut() {
                match ndx {
                    SomeNeed::AddGroup {
                        act_num: _,
                        group_region: greg,
                    } => {
                        try_again = true;

                        // Add a new group
                        if self.groups.any_superset_of(&greg) {
                            if 1 == 2 / 2 {
                                panic!(
                                    "**** Supersets found for new group {} in {}",
                                    &greg,
                                    self.groups.supersets_of(&greg)
                                );
                            }
                            continue;
                        }

                        if let Some(sqrx) = self.squares.find(&greg.state1) {
                            if let Some(sqry) = self.squares.find(&greg.state2) {
                                if sqrx.pn() == Pn::Unpredictable && sqry.pn() == Pn::Unpredictable
                                {
                                    self.groups.push(SomeGroup::new(
                                        &greg.state1,
                                        &greg.state2,
                                        RuleStore::new(),
                                        self.num,
                                        &max_region,
                                    ));

                                    return self.get_needs(cur_state, &max_region);
                                } else {
                                    if let Some(ruls) =
                                        self.squares.rules(&greg.state1, &greg.state2)
                                    {
                                        self.groups.push(SomeGroup::new(
                                            &greg.state1,
                                            &greg.state2,
                                            ruls,
                                            self.num,
                                            &max_region,
                                        ));

                                        return self.get_needs(cur_state, &max_region);
                                    } else {
                                        panic!("expected this to work");
                                    }
                                }
                            } else {
                                panic!("should have found square {}", &greg.state1);
                            }
                        } else {
                            panic!("should have found square {}", &greg.state2);
                        }
                    }
                    SomeNeed::SetGroupConfirmed {
                        act_num: _,
                        group_region: greg,
                        cstate: sta1,
                    } => {
                        try_again = true;

                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            println!("Act {} Group {} confirmed using {}", self.num, greg, sta1);
                            grpx.set_anchor(sta1.clone());
                        } else {
                            panic!("group {} not found for SetGroupConfirnmed", greg);
                        }
                    }
                    SomeNeed::ClearGroupCheckBit {
                        act_num: _,
                        group_region: greg,
                        mbit: mbitx,
                    } => {
                        try_again = true;

                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            grpx.check_off(&mbitx);
                        } else {
                            println!("did not find group {} ?", &greg);
                        }
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

    // Return in-between needs.
    //
    // When a group is invalidated by a new sample, something was wrong with that group.
    // Possible replacement groups will be greatly decreased in number if the
    // new sample can be used to find an adjacent, dissimilar pair of squares.
    //
    // For Group(s) invalidated by a new sample, there is at least one
    // dissimilar square pair in the group.
    //
    // If a dissimilar pair is not adjacent, the region formed by the pair
    // is added to the action closer_regs RegionStore.
    pub fn in_between_needs(&mut self, cur_state: &SomeState) -> NeedStore {
        let mut ret_nds = NeedStore::new();

        {
            // Pre-check and clean-up, scan action closer_regs RegionStore.
            //
            // If there is an existing square between the region state-squares, that is
            // incompatible with any one of the squares, inactivate the region.
            //
            // If the two incompatible squares are not adjacent, add a new region to
            // closer_regs, built from the incompatible pair.
            let mut do_again = true;

            while do_again {
                do_again = false;

                let mut new_regs = RegionStore::new();
                for regx in self.closer_regs.iter_mut() {
                    if regx.active == false {
                        continue;
                    }

                    let stas_in = self.squares.stas_in_reg(&regx);

                    // If no more than the two squares that form the region, ignore this region
                    if stas_in.len() == 2 {
                        continue;
                    }

                    if let Some(sqr1) = self.squares.find(&regx.state1) {
                        // Get the squares represented by the states that form the region
                        if let Some(sqr2) = self.squares.find(&regx.state2) {
                            for stax in stas_in.iter() {
                                // Ignore the states in the StateStore that form the region
                                if *stax == regx.state1 || *stax == regx.state2 {
                                    continue;
                                }

                                // Process stax that is inbetween
                                if let Some(sqr3) = self.squares.find(&stax) {
                                    let cnb1 = sqr3.can_combine(&sqr1);
                                    match cnb1 {
                                        Combinable::False => {
                                            //  println!(
                                            //      "sqr {} is not combinable with {}",
                                            //      &stax, &sqr1.state
                                            //  );
                                            if sqr1.state.s_xor(&sqr3.state).num_one_bits() == 1 {
                                                regx.inactivate();
                                            } else {
                                                new_regs.push_nosups(SomeRegion::new(
                                                    &sqr1.state,
                                                    &sqr3.state,
                                                ));
                                            }
                                        }
                                        _ => {}
                                    } // end match cnb1

                                    let cnb2 = sqr3.can_combine(&sqr2);
                                    match cnb2 {
                                        Combinable::False => {
                                            println!(
                                                "*** problem?, sqr {} is not combinable with {}",
                                                &stax, &sqr2.state
                                            );
                                            if sqr2.state.s_xor(&sqr3.state).num_one_bits() == 1 {
                                                regx.inactivate();
                                            } else {
                                                new_regs.push_nosups(SomeRegion::new(
                                                    &sqr2.state,
                                                    &sqr3.state,
                                                ));
                                            }
                                        }
                                        _ => {}
                                    } // end match cnb2
                                } else {
                                    panic!("square not found");
                                }
                            } // next stax (for a square in-between)
                        } else {
                            panic!("square not found");
                        } // end if let sqr2
                    } else {
                        panic!("square not found");
                    } // end if let sqr1
                } // next regx closer_reg

                // Apply new regions, set flag to try again
                for regx in new_regs.iter() {
                    if regx.active {
                        do_again = true;
                        self.closer_regs.push_nosups(regx.clone());
                    }
                }
            } // end while do-again
        } // end pre-check and clean-up block

        // Get inbetween needs, scan the action closer_regs RegionStore.
        for regx in self.closer_regs.iter() {
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

                let mut dif_msk = SomeMask::new(cur_state.bts.new_low());

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
                ret_nds.push(SomeNeed::InBetween {
                    act_num: self.num,
                    targ_state: seek_state,
                    in_group: regx.clone(),
                });
            } else {
                // At least one square was found inbetween, possibly from a previous
                // inbetween need being satisfied.
                let mut max_rslts = 0;
                let mut max_stas = StateStore::new();

                if let Some(sqr1) = self.squares.find(&regx.state1) {
                    // Get squares represented by the states that form the region
                    if let Some(sqr2) = self.squares.find(&regx.state2) {
                        // Find the squares that need more samples to determine combinability
                        // with a preferebce for squares that already have the most samples.
                        for stax in stas_in.iter() {
                            // Ignore states in the StateStore that form the region
                            if *stax == regx.state1 || *stax == regx.state2 {
                                continue;
                            }

                            if let Some(sqr3) = self.squares.find(&stax) {
                                let cnb1 = sqr3.can_combine(&sqr1);
                                match cnb1 {
                                    Combinable::False => {
                                        println!(
                                            "*** problem?, sqr {} is not combinable with {}",
                                            &stax, &sqr1.state
                                        );
                                    }
                                    Combinable::True => {
                                        //  println!(
                                        //      "*** sqr {} is combinable with {}",
                                        //      &stax, &sqr1.state
                                        //  );
                                    }
                                    Combinable::MoreSamplesNeeded => {
                                        if sqr3.num_results() > max_rslts {
                                            max_rslts = sqr3.num_results();
                                            max_stas = StateStore::new();
                                        }
                                        if sqr3.num_results() == max_rslts {
                                            max_stas.push(stax.clone());
                                        }
                                    }
                                } // end match cnb1

                                let cnb2 = sqr3.can_combine(&sqr2);
                                match cnb2 {
                                    Combinable::False => {
                                        println!(
                                            "*** problem?, sqr {} is not combinable with {}",
                                            &stax, &sqr2.state
                                        );
                                    }
                                    Combinable::True => {
                                        //  println!(
                                        //      "*** sqr {} is combinable with {}",
                                        //      &stax, &sqr2.state
                                        //  );
                                    }
                                    Combinable::MoreSamplesNeeded => {
                                        if sqr3.num_results() > max_rslts {
                                            max_rslts = sqr3.num_results();
                                            max_stas = StateStore::new();
                                        }
                                        if sqr3.num_results() == max_rslts {
                                            max_stas.push(stax.clone());
                                        }
                                    }
                                } // end match cnb2
                            } else {
                                panic!("square not found");
                            }
                        } // next stax (for a square in-between)
                    } else {
                        panic!("square not found");
                    } // end if let state2
                } else {
                    panic!("square not found");
                } // end if let state1

                // Choose sta index
                if max_stas.len() == 0 {
                    println!("problem, no stas?");
                    return NeedStore::new();
                }

                let mut inx = 0;
                if max_stas.len() > 1 {
                    inx = rand::thread_rng().gen_range(0, &max_stas.len());
                }

                let seek_sta = max_stas[inx].clone();

                ret_nds.push(SomeNeed::InBetween {
                    act_num: self.num,
                    targ_state: seek_sta,
                    in_group: regx.clone(),
                });
            } // end if stas_in.len() == 2, else
        } // next regx

        ret_nds
    }
    // Get additional sample needs for the states that form a group
    fn additional_group_state_samples(&self) -> NeedStore {
        //println!("additional_group_state_sample");
        let mut ret_nds = NeedStore::new();

        for grpx in self.groups.iter() {
            if grpx.active == false {
                continue;
            }

            let sqr_opt = self.squares.find(&grpx.region.state1);
            match sqr_opt {
                Some(sqrx) => {
                    if sqrx.state != grpx.region.state1 {
                        panic!("{} ne {}", sqrx.state, grpx.region.state1);
                    }
                    if sqrx.pnc() == false {
                        ret_nds.push(SomeNeed::StateAdditionalSample {
                            act_num: self.num,
                            targ_state: sqrx.state.clone(),
                            grp_reg: grpx.region.clone(),
                            far: grpx.region.state2.clone(),
                        });
                    }
                }
                None => {
                    panic!("group region square not found");
                }
            } // end match sqr_opt

            if let Some(sqrx) = self.squares.find(&grpx.region.state2) {
                if sqrx.state != grpx.region.state2 {
                    panic!("{} ne {}", sqrx.state, grpx.region.state2);
                }
                if sqrx.pnc() == false {
                    ret_nds.push(SomeNeed::StateAdditionalSample {
                        act_num: self.num,
                        targ_state: sqrx.state.clone(),
                        grp_reg: grpx.region.clone(),
                        far: grpx.region.state1.clone(),
                    });
                }
            } else {
                panic!("group region square not found");
            }
        }

        ret_nds
    } // end additional_group_state_samples

    // Return expand needs for groups.
    fn expand_needs(&self) -> NeedStore {
        //println!("expand_needs");
        let mut ret_nds = NeedStore::new();

        //        let regs = self.groups.regions();
        //        let states_in_1 = self.squares.states_in_1_region(&regs);

        for grpx in self.groups.iter() {
            if grpx.active == false || grpx.not_x_check.is_low() {
                continue;
            }

            // Get a vector of one-bit masks
            let check_bits = MaskStore {
                avec: grpx.not_x_check.split(),
            };

            // Check for expansion, or single-bit adjacent external
            for bitx in check_bits.iter() {
                let regb = grpx.region.toggle_bits(bitx);
                //println!("toggle {} with {} returns {}", &grpx.region, bitx, regb);
                let sqr_stas = self.squares.stas_in_reg(&regb);

                let reg_both = regb.union(&grpx.region); // one bit so double the "area"

                // println!(
                //     "expand_needs, act {}, check reg {} sqrs {} for expansion of group {}",
                //     &self.num, &regb, &sqr_stas, &grpx.region
                // );

                if sqr_stas.len() > 0 {
                    if self.squares.no_incompatible_pairs(&sqr_stas) {
                        if let Some(pnc) = self.squares.first_pnc_val(&sqr_stas) {
                            if pnc != grpx.pn {
                                //  println!("*** pnc {} != grpc.pn {}", &pnc, &grpx.pn);
                                ret_nds.push(SomeNeed::ClearGroupCheckBit {
                                    act_num: self.num,
                                    group_region: grpx.region.clone(),
                                    mbit: bitx.clone(),
                                });
                            } else {
                                // Get rules, see if a union with group os OK
                                if let Some(rulsx) = self.squares.rules_union(&sqr_stas, pnc) {
                                    if let Some(_ruls_both) = rulsx.union(&grpx.rules) {
                                        ret_nds
                                            .append(&mut self.far_needs(&reg_both, &grpx.region));
                                    } else {
                                        // println!(
                                        //    "*** rulesx {} union with grp rules {} is None",
                                        //     rulsx, &grpx.rules
                                        // );
                                        ret_nds.push(SomeNeed::ClearGroupCheckBit {
                                            act_num: self.num,
                                            group_region: grpx.region.clone(),
                                            mbit: bitx.clone(),
                                        });
                                    }
                                } else {
                                    // println!(
                                    //     "*** no rules union for squares {} for pnc {}",
                                    //      &sqr_stas, &pnc
                                    //  );
                                    ret_nds.push(SomeNeed::ClearGroupCheckBit {
                                        act_num: self.num,
                                        group_region: grpx.region.clone(),
                                        mbit: bitx.clone(),
                                    });
                                    //   println!(
                                    //       "Act {} rules_union fails for {} ?",
                                    //       &self.num, &sqr_stas
                                    //   );
                                }
                            }
                        } else {
                            // no pnc square

                            let pnx = self.squares.max_pn(&sqr_stas);

                            if pnx == grpx.pn {
                                if let Some(rulsx) = self.squares.rules_union(&sqr_stas, pnx) {
                                    if let Some(_rulsy) = rulsx.union(&grpx.rules) {
                                        ret_nds
                                            .append(&mut self.far_needs(&reg_both, &grpx.region));
                                    } else {
                                        // extra region rules do not form a union with the group rules
                                        // Set non-x_bit off
                                        //  println!(
                                        //     "*** rulsx {} union grp ruls {} is None",
                                        //      &rulsx, &grpx.rules
                                        //  );
                                        ret_nds.push(SomeNeed::ClearGroupCheckBit {
                                            act_num: self.num,
                                            group_region: grpx.region.clone(),
                                            mbit: bitx.clone(),
                                        });
                                    }
                                } else {
                                    // squares in extra region cannot form rules
                                    //    println!(
                                    //        "*** sqrs {} union under pn {} returned None",
                                    //       &sqr_stas, &pnx
                                    //    );
                                    ret_nds.push(SomeNeed::ClearGroupCheckBit {
                                        act_num: self.num,
                                        group_region: grpx.region.clone(),
                                        mbit: bitx.clone(),
                                    });
                                }
                            } else if pnx > grpx.pn {
                                // Set non-x_bit off
                                //        println!("*** pnx {} != grpx.pn {}", &pnx, &grpx.pn);
                                ret_nds.push(SomeNeed::ClearGroupCheckBit {
                                    act_num: self.num,
                                    group_region: grpx.region.clone(),
                                    mbit: bitx.clone(),
                                });
                            } else {
                                // Seek more samples in region
                                ret_nds.append(&mut self.far_needs(&reg_both, &grpx.region));
                            }
                        } // end if pnc
                    } else {
                        // incompatible squares found
                        // println!("*** incompatible squares in {}", &sqr_stas);
                        ret_nds.push(SomeNeed::ClearGroupCheckBit {
                            act_num: self.num,
                            group_region: grpx.region.clone(),
                            mbit: bitx.clone(),
                        });
                    }
                } else {
                    // no squares found in extra region, seek a sample in the region
                    // Get samples of a far square

                    let state1x = reg_both.far_state(&grpx.region.state1);
                    ret_nds.push(SomeNeed::AStateExpandGroup {
                        act_num: self.num,
                        targ_state: state1x,
                        base_group: grpx.region.clone(),
                    });

                    let state2x = reg_both.far_state(&grpx.region.state2);
                    ret_nds.push(SomeNeed::AStateExpandGroup {
                        act_num: self.num,
                        targ_state: state2x,
                        base_group: grpx.region.clone(),
                    });
                }
            } // next bitx
        } // next grpx

        ret_nds
    }

    // Give a region and a group-region that is a sub-region, return needs for states far from the given states.
    // If any state has more samples that the other, just return a need for that state.
    // Return an AddGroup Need if one of the far squares is pnc.
    fn far_needs(&self, aregion: &SomeRegion, greg: &SomeRegion) -> NeedStore {
        //println!("far_needs");
        let mut ret_nds = NeedStore::new();

        assert!(aregion.is_superset_of(&greg));
        assert!(aregion != greg);

        let state1_far = aregion.far_state(&greg.state1);

        let state2_far = aregion.far_state(&greg.state2);

        if let Some(sqr1x) = self.squares.find(&state1_far) {
            if let Some(sqr2x) = self.squares.find(&state2_far) {
                // both found
                if sqr1x.pnc() {
                    ret_nds.push(SomeNeed::AddGroup {
                        act_num: self.num,
                        group_region: SomeRegion::new(&state1_far, &greg.state1),
                    });
                } else if sqr2x.pnc() {
                    ret_nds.push(SomeNeed::AddGroup {
                        act_num: self.num,
                        group_region: SomeRegion::new(&state2_far, &greg.state2),
                    });
                } else {
                    if sqr1x.num_results() > sqr2x.num_results() {
                        ret_nds.push(SomeNeed::AStateExpandGroup {
                            act_num: self.num,
                            targ_state: state1_far,
                            base_group: greg.clone(),
                        });
                    } else {
                        ret_nds.push(SomeNeed::AStateExpandGroup {
                            act_num: self.num,
                            targ_state: state2_far,
                            base_group: greg.clone(),
                        });
                    }
                }
            } else {
                // sqr2x not found
                if sqr1x.pnc() {
                    ret_nds.push(SomeNeed::AddGroup {
                        act_num: self.num,
                        group_region: SomeRegion::new(&state1_far, &greg.state1),
                    });
                } else {
                    ret_nds.push(SomeNeed::AStateExpandGroup {
                        act_num: self.num,
                        targ_state: state1_far,
                        base_group: greg.clone(),
                    });
                }
            }
        } else {
            // sqr1x not found

            if let Some(sqr2x) = self.squares.find(&state2_far) {
                if sqr2x.pnc() {
                    ret_nds.push(SomeNeed::AddGroup {
                        act_num: self.num,
                        group_region: SomeRegion::new(&state2_far, &greg.state2),
                    });
                } else {
                    ret_nds.push(SomeNeed::AStateExpandGroup {
                        act_num: self.num,
                        targ_state: state2_far,
                        base_group: greg.clone(),
                    });
                }
            } else {
                // Neither far squares found
                ret_nds.push(SomeNeed::AStateExpandGroup {
                    act_num: self.num,
                    targ_state: state1_far,
                    base_group: greg.clone(),
                });
                ret_nds.push(SomeNeed::AStateExpandGroup {
                    act_num: self.num,
                    targ_state: state2_far,
                    base_group: greg.clone(),
                });
            }
        }
        ret_nds
    }

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
    fn confirm_groups_needs(&mut self, max_region: &SomeRegion) -> NeedStore {
        //println!("confirm_groups_needs");
        let mut ret_nds = NeedStore::new();

        let regs = self.groups.regions();

        let states1 = self.squares.states_in_1_region(&regs);

        let anchors = self.groups.anchors();
        //println!("anchors {}", &anchors);

        //println!("Act {}, states in one region {}", self.num, states1);

        // Find squares in one group for each group, that may be an anchor
        for greg in regs.iter() {
            let mut stsin = greg.states_in(&states1); // The states in greg and no other group

            // println!("Act {} Group {} States {}", self.num, greg, stsin);

            // Look for states in greg that have not been sampled and are adjacent to an
            // external square that is in only one group.
            for stax in states1.iter() {
                if greg.is_adjacent_state(&stax) {
                    let adj_sta = greg.overlapping_part_state(&stax);

                    if self.groups.num_groups_state_in(&adj_sta) == 1 {
                        if states1.contains(&adj_sta) {
                        } else {
                            //println!("adding state {} for group {} to state1", &adj_sta, &greg);
                            stsin.push(adj_sta);
                        }
                    } // end if self.groups
                } // end if greg.adjecent
            } // next stax

            if stsin.len() == 0 {
                // Check if previously confirmed group no longer has
                // squares in one group.
                if let Some(grpx) = self.groups.find_mut(greg) {
                    if let Some(_) = grpx.anchor {
                        grpx.set_anchor_off();
                    }
                } else {
                    panic!("can't find gorup {}", &greg);
                }
                continue;
            }

            // Get the bit masks on non-X bit-positions in greg, not counting
            // bit positions that have never been X for all groups in all actions.
            let non_x_msks = BitsStore {
                avec: greg.not_x_mask().m_and(&max_region.x_mask()).bts.split(),
            };

            // For each state, sta1, only in the group region, greg:
            //
            //  Calculate each state, sta_adj, adjacent to sta1, outside of greg.
            //
            //  Calculate a rate for each sta1 option, based on the number of adjacent states
            //  in only one group and and/or being used as an anchor in another group.
            let mut max_num = 0;
            let mut cfmv_max = Vec::<StateStore>::new();

            for sta1 in stsin.iter() {
                let mut cfmx = StateStore::new();

                cfmx.push(sta1.clone());

                let mut cnt = 1;

                if let Some(sqrx) = self.squares.find(&sta1) {
                    if sqrx.pnc() {
                        cnt += 5;
                    } else {
                        cnt += sqrx.len_results();
                    }
                }

                // Process adjacent external states
                for non_x_bit in non_x_msks.iter() {
                    let sta_adj = SomeState::new(sta1.bts.b_xor(non_x_bit));
                    //println!(
                    //    "checking {} adjacent to {} external to {}",
                    //    &sta_adj, &sta1, &greg
                    // );
                    if anchors.contains(&sta_adj) {
                        //println!("{} is an anchor", &sta_adj);
                        cnt += 1000;
                    } else if states1.contains(&sta_adj) {
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

                // check far state
                let far_sta = greg.far_state(&sta1);
                if let Some(sqrx) = self.squares.find(&far_sta) {
                    if sqrx.pnc() {
                        cnt += 5;
                    } else {
                        cnt += sqrx.len_results();
                    }
                }

                //println!("group {} anchor {} rating {}", &greg, &cfmx[0], cnt);

                if cnt > max_num {
                    max_num = cnt;
                    cfmv_max = Vec::<StateStore>::new();
                }

                if cnt == max_num {
                    cfmv_max.push(cfmx);
                }
            } // next sta1

            // Check if a confirmed group anchor is still rated the best
            if let Some(grpx) = self.groups.find(&greg) {
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
            } else {
                panic!("cant find region?");
            }

            let cfm_max = &cfmv_max[0];

            // If the anchor has not been sampled, enough, return a need for that.
            //
            // if group external adjacent states have not been sampled, enough, return needs for that.
            //
            // If the group far state has not been sampled, enough, return a need for that.
            //
            // else confirm the group.
            let anchor_sta = &cfm_max[0];
            if let Some(anchor_sqr) = self.squares.find(anchor_sta) {
                if anchor_sqr.pnc() {
                    // println!("group {} anchor {} pnc", &greg, &anchor_sta);
                } else {
                    // Get additional samples of the anchor
                    ret_nds.push(SomeNeed::ConfirmGroup {
                        act_num: self.num,
                        anchor: anchor_sta.clone(),
                        targ_state: anchor_sta.clone(),
                        for_group: greg.clone(),
                    });
                    continue; // next greg
                }
            } else {
                // Get the first sample of the anchor
                ret_nds.push(SomeNeed::ConfirmGroup {
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

            if let Some(grpx) = self.groups.find(&greg) {
                for inx in 1..cfm_max.len() {
                    let adj_sta = &cfm_max[inx];

                    let a_bit_msk = SomeMask::new(adj_sta.bts.b_xor(&anchor_sta.bts));

                    if grpx.not_x_bit_set(&a_bit_msk) == false {
                        continue;
                    }

                    //println!("*** for group {} checking adj sqr {}", &greg, &adj_sta);

                    if let Some(adj_sqr) = self.squares.find(adj_sta) {
                        if adj_sqr.pnc() {
                            if let Some(anchor_sqr) = self.squares.find(anchor_sta) {
                                if anchor_sqr.can_combine(&adj_sqr) == Combinable::True {
                                    nds_grp_add.push(SomeNeed::AddGroup {
                                        act_num: self.num,
                                        group_region: SomeRegion::new(&anchor_sta, &adj_sta),
                                    });
                                } else {
                                    // Set that bit off in the check mask
                                    grp_clear_bit.push(SomeNeed::ClearGroupCheckBit {
                                        act_num: self.num,
                                        group_region: greg.clone(),
                                        mbit: SomeMask::new(anchor_sta.bts.b_xor(&adj_sta.bts)),
                                    });
                                }
                            } else {
                                panic!("should have found anchor state");
                            }
                        } else {
                            nds_grp.push(SomeNeed::ConfirmGroup {
                                act_num: self.num,
                                anchor: anchor_sta.clone(),
                                targ_state: adj_sta.clone(),
                                for_group: greg.clone(),
                            });
                        }
                    } else {
                        nds_grp.push(SomeNeed::ConfirmGroup {
                            act_num: self.num,
                            anchor: anchor_sta.clone(),
                            targ_state: adj_sta.clone(),
                            for_group: greg.clone(),
                        });
                    }
                } // next inx in cfm_max
            } else {
                panic!("group not found?");
            } // end if let group find

            if nds_grp_add.len() > 0 {
                // println!("*** nds_grp_add {}", &nds_grp_add);
                ret_nds.append(&mut nds_grp_add);
                continue; // next greg
            }

            if nds_grp.len() > 0 {
                //  println!("*** nds_grp {}", &nds_grp);
                ret_nds.append(&mut nds_grp);
                continue; // next greg
            }

            if grp_clear_bit.len() > 0 {
                //  println!("*** nds_grp_clear_bit {}", &grp_clear_bit);
                ret_nds.append(&mut grp_clear_bit);
                // pass-through to check far square
            }

            //   println!("grp {} check far", &greg);

            // Process far state, after the anchor and adjaent, external, tests have been made.
            // If its dissimilar to the anchor, the group will be invalidated.
            // Fairly cheap, quick, somewhat accurate, method of establishing a region,
            // instead of checking all states adjacent-internal.
            let sta_far = SomeState::new(anchor_sta.bts.b_xor(&greg.x_mask().bts));
            let sqrf_opt = self.squares.find(&sta_far);
            match sqrf_opt {
                Some(sqrf) => {
                    if sqrf.pnc() {
                        // Set the group confirmed
                        ret_nds.push(SomeNeed::SetGroupConfirmed {
                            act_num: self.num,
                            group_region: greg.clone(),
                            cstate: anchor_sta.clone(),
                        });
                        continue;
                    } else {
                        // Get additional samples of the far state
                        ret_nds.push(SomeNeed::ConfirmGroup {
                            act_num: self.num,
                            anchor: anchor_sta.clone(),
                            targ_state: sta_far.clone(),
                            for_group: greg.clone(),
                        });
                    }
                }
                None => {
                    // Get the first sample of the far state
                    ret_nds.push(SomeNeed::ConfirmGroup {
                        act_num: self.num,
                        anchor: anchor_sta.clone(),
                        targ_state: sta_far.clone(),
                        for_group: greg.clone(),
                    });
                }
            } // end match sqrx_opt
        } // next greg

        ret_nds
    } // end confirm_groups

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
                        if grpx.region.is_adjacent(&grpy.region) {
                            let mut ndx = self.groups_adjacent_needs(&grpx, &grpy);
                            if ndx.len() > 0 {
                                nds.append(&mut ndx);
                            }
                        } // end if adjacent
                    } // end if pn ==
                } // end if regions adjacent
            } // next iny
        } // next inx

        nds
    } // end group_pair_needs

    // Check for needs, for making a given region into a group.
    // A need may be to take more samples, or just add the group.
    fn possible_group_needs(&self, reg_grp: &SomeRegion, from: usize) -> NeedStore {
        //println!("possible_group_needs");
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
                act_num: self.num,
                targ_state: sta1.clone(),
                for_reg: reg_grp.clone(),
                far: sta2.clone(),
                num_x: reg_grp.num_x(),
            });

            nds.push(SomeNeed::AStateMakeGroup {
                act_num: self.num,
                targ_state: sta1,
                for_reg: reg_grp.clone(),
                far: sta2,
                num_x: reg_grp.num_x(),
            });

            return nds;
        }

        // At this point, at least one existing square is in the region

        // Check pn and pnc failure modes
        if stas_in_reg.len() > 1 && self.squares.any_pnc(&stas_in_reg) {
            //println!("possible_group_needs 2");
            let max_pn = self.squares.max_pn(&stas_in_reg);

            let max_pnc = self.squares.max_pnc(&stas_in_reg);

            let min_pnc = self.squares.min_pnc(&stas_in_reg);

            if max_pnc != min_pnc {
                return nds;
            }

            if max_pn > max_pnc {
                return nds;
            }
        }

        // If an existing pair can make a group, calc and return needs
        // Otherwise store first pair with the greatest number of stored results.
        let mut max_samples = 0;
        let mut sta_pair = StateStore::new();

        if let Some(poss_pairs) = self.squares.possible_pairs_to_region(reg_grp) {
            //println!("possible_group_needs 3");
            let mut inx1 = 0;

            loop {
                let inx2 = inx1 + 1;

                if let Some(sqrx) = self.squares.find(&poss_pairs[inx1]) {
                    if let Some(sqry) = self.squares.find(&poss_pairs[inx2]) {
                        // If both squares are pnc, add the group, done.
                        if sqrx.pnc() && sqry.pnc() {
                            println!(
                                "possible_group_needs: add group 1 {} reg_grp {} from {}",
                                SomeRegion::new(&poss_pairs[inx1], &poss_pairs[inx2]),
                                &reg_grp,
                                &from
                            );
                            nds.push(SomeNeed::AddGroup {
                                // nds should be empty so far
                                act_num: self.num,
                                group_region: SomeRegion::new(&poss_pairs[inx1], &poss_pairs[inx2]),
                            });
                            return nds;
                        }

                        // Accumulate pair ratings
                        let mut num_samples = sqrx.len_results();
                        num_samples += sqry.len_results();

                        if num_samples > max_samples {
                            max_samples = num_samples;
                            sta_pair = StateStore::new();
                            sta_pair.push(sqrx.state.clone());
                            sta_pair.push(sqry.state.clone());
                        }
                    } else {
                        panic!("Square not found?");
                    }
                } else {
                    panic!("Square not found?");
                } // endif Some(sqrx)

                inx1 += 2;
                if inx1 == poss_pairs.len() {
                    break;
                }
            } // end loop

            // Return needs for pair with max samples
            if sta_pair.len() > 0 {
                if let Some(sqrx) = self.squares.find(&sta_pair[0]) {
                    if sqrx.pnc() {
                    } else {
                        nds.push(SomeNeed::AStateMakeGroup {
                            // nds should be empty so far
                            act_num: self.num,
                            targ_state: sta_pair[0].clone(),
                            for_reg: reg_grp.clone(),
                            far: sta_pair[1].clone(),
                            num_x: reg_grp.num_x(),
                        });
                    }
                } else {
                    panic!("Square not found?");
                }

                if let Some(sqry) = self.squares.find(&sta_pair[1]) {
                    if sqry.pnc() {
                    } else {
                        nds.push(SomeNeed::AStateMakeGroup {
                            // nds should be empty so far
                            act_num: self.num,
                            targ_state: sta_pair[1].clone(),
                            for_reg: reg_grp.clone(),
                            far: sta_pair[0].clone(),
                            num_x: reg_grp.num_x(),
                        });
                    }
                } else {
                    panic!("Square not found?");
                }

                return nds;
            }
        } // end if poss_pairs

        // At this point, no pairs of squares make the region

        // Find a square, and a square far state, to get more samples
        //println!("possible_group_needs 4");
        let mut max_samples = 0;
        let mut sta_pairs = Vec::<(SomeState, SomeState)>::new();

        for stax in stas_in_reg.iter() {
            if let Some(sqrx) = self.squares.find(&stax) {
                // start with sqrx num stored samples
                let num_samples = sqrx.len_results();

                // Get possible far square
                let stay = reg_grp.far_state(&stax);

                if num_samples > max_samples {
                    max_samples = num_samples;
                    sta_pairs = Vec::<(SomeState, SomeState)>::new();
                }

                if num_samples == max_samples {
                    sta_pairs.push((stax.clone(), stay));
                }
            } else {
                panic!("square not found?");
            } // end Some(sqrx)
        } // next stax

        assert!(max_samples > 0);

        let sta_pair = &sta_pairs[rand::thread_rng().gen_range(0, sta_pairs.len())];

        let sta1 = &sta_pair.0;
        let sta2 = &sta_pair.1;

        if let Some(sqrx) = self.squares.find(&sta1) {
            if sqrx.pnc() {
            } else {
                nds.push(SomeNeed::AStateMakeGroup {
                    act_num: self.num,
                    targ_state: sta2.clone(),
                    for_reg: reg_grp.clone(),
                    far: sta1.clone(),
                    num_x: reg_grp.num_x(),
                });
            }
        } else {
            panic!("Square not found?");
        }

        if let Some(sqry) = self.squares.find(&sta2) {
            if sqry.pnc() {
            } else {
                nds.push(SomeNeed::AStateMakeGroup {
                    act_num: self.num,
                    targ_state: sta2.clone(),
                    for_reg: reg_grp.clone(),
                    far: sta1.clone(),
                    num_x: reg_grp.num_x(),
                });
            }
        } else {
            // far sta not found in squarestore
            nds.push(SomeNeed::AStateMakeGroup {
                act_num: self.num,
                targ_state: sta2.clone(),
                for_reg: reg_grp.clone(),
                far: sta1.clone(),
                num_x: reg_grp.num_x(),
            });
        }
        nds
    }

    // Check two intersecting groups for needs.
    // Possibly combining to groups.
    // Possibly checking for a contradictatory intersection.
    fn groups_intersection_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //println!("groups_intersection_needs");

        if grpx.pn != grpy.pn {
            return self.group_pair_cont_int_needs(&grpx, &grpy);
        }

        // So grpx.pn == grpy.pn
        match grpx.pn {
            Pn::Unpredictable => {
                // The intersection is not contradictory, check if combination is possible
                let regx = grpx.region.union(&grpy.region);

                let stas_in = self.squares.stas_in_reg(&regx);

                if self.squares.any_pnc(&stas_in) {
                    let min_pnc = self.squares.min_pnc(&stas_in);

                    if min_pnc == Pn::Unpredictable {
                        return self.possible_group_needs(&regx, 1);
                    }
                }

                return NeedStore::new();
            }
            _ => {
                // Get rules union, if any
                if let Some(rulesx) = grpx.rules.union(&grpy.rules) {
                    // Check all squares in the combined group regions
                    let regx = grpx.region.union(&grpy.region);

                    if self.squares.verify_combination(&regx, &rulesx, &grpx.pn) {
                        //println!(
                        //    "combination verified for act {} grp {} grp {} region {} rules {}",
                        //    self.num, grpx.region, grpy.region, regx, rulesx
                        //);

                        return self.possible_group_needs(&regx, 2);
                    } // end if verify_combinaton

                    return self.group_pair_cont_int_needs(&grpx, &grpy);
                } else {
                    // The groups cannot be combined, check for contradictory intersection
                    return self.group_pair_cont_int_needs(&grpx, &grpy);
                } // end if rules union is OK
            } // end match variant _
        } // end match pn
    } // end groups_intersection_needs

    // Get needs for two adjacent groups, with the same pn rating.
    fn groups_adjacent_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //println!("groups_adjacent_needs");
        let nds = NeedStore::new();

        match grpx.pn {
            Pn::Unpredictable => {
                // If the regions have the same X-bit pattern, they may be combined
                if grpx.region.x_mask() == grpy.region.x_mask() {
                    let regx = grpx.region.union(&grpy.region);
                    return self.possible_group_needs(&regx, 3);
                }

                let regz = grpx.region.overlapping_part(&grpy.region);

                if regz.intersects(&grpx.region) && regz.intersects(&grpy.region) {
                    if self.groups.any_superset_of(&regz) {
                        return nds;
                    }

                    //println!("groups_adjacent_needs: {} and {}", &grpx.region, &grpy.region);
                    return self.possible_group_needs(&regz, 4);
                }
            }
            _ => {
                // If the regions have the same X-bit pattern, they may be combined
                if grpx.region.x_mask() == grpy.region.x_mask() {
                    let regx = grpx.region.union(&grpy.region);
                    return self.possible_group_needs(&regx, 5);
                }

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

                        return self.possible_group_needs(&regz, 6);
                    }
                } // end if let Some
            } // end match pn default
        } // end match pn for adjacent regions
        nds
    } // end groups_adjacent_needs

    // Return the contradictory intersection needs, if any, for two intersecting groups.
    fn group_pair_cont_int_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //println!("group_pair_cont_int_needs");
        let mut nds = NeedStore::new();

        // Calculate the intersection
        let reg_int = grpx.region.intersection(&grpy.region);

        if grpx.pn != grpy.pn {
            // Pns not equal, the whole region is contradictory
            nds.push(self.cont_int_region_needs(&reg_int, &grpx, &grpy));
            return nds;
        }

        if grpx.pn == Pn::Unpredictable {
            println!("Pn::Unpredictable in group_pair_cont_int_need ? this should not happen.");
            return nds;
        }

        let rulsx = grpx.rules.restrict_initial_region(&reg_int);

        let rulsy = grpy.rules.restrict_initial_region(&reg_int);

        if rulsx.is_equal(&rulsy) {
            return nds;
        } // A valid union of the whole intersection region exists

        // Check if a valid sub-region of the intersection exists
        if let Some(rulsxy) = rulsx.intersection(&rulsy) {
            // A valid sub-union exists, seek a sample in intersection that is not in rulsxy.initial_region
            let ok_reg = rulsxy.initial_region();

            let regy = reg_int.far_reg(&ok_reg); // to avoid subtraction, use the far sub-region

            nds.push(self.cont_int_region_needs(&regy, &grpx, &grpy));
        } else {
            nds.push(self.cont_int_region_needs(&reg_int, &grpx, &grpy));
        }

        nds
    } // end groups_pair_cont_int_needs

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
            if let Some(sqrz) = self.squares.find(stax) {
                if sqrz.pnc() {
                    panic!("Square {} is pnc in contradictory intersection {} of group {} and group {}", stax, regx, &grpx.region, &grpy.region);
                }

                if sqrz.num_results() > max_rslts {
                    max_rslts = sqrz.num_results();
                    stas_check.push(sqrz.state.clone());
                }
            } else {
                panic!("Square {} not found", stax);
            }
        }

        SomeNeed::ContradictoryIntersection {
            act_num: self.num,
            goal_reg: SomeRegion::new(&stas_check[0], &stas_check[0]),
            group1: grpx.region.clone(),
            ruls1: grpx.rules.restrict_initial_region(regx),
            group2: grpy.region.clone(),
            ruls2: grpy.rules.restrict_initial_region(regx),
        }
    } // end cont_int_region_needs

    // Get possible steps that can be used to make at least one
    // change in a given rule.
    pub fn get_steps(&self, arule: &SomeRule) -> StepStore {
        self.groups.get_steps(&arule, self.num)
    }

    pub fn new_x_bits(&mut self, bitsx: &SomeMask) {
        self.groups.new_x_bits(&bitsx);
    }

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
}
