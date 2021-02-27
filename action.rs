//! The SomeAction struct.  It stores, and analyzes, the results of an action.
//!
//! This stores initial->result samples, generates needs for more samples, and
//! represents the current best-guess rules of the expected responses
//! of executing an action for a given region or state.
//!
use crate::bits::SomeBits;
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
            rc_str.push_str(&format!(" seek_edge: {}", self.seek_edge));
        }

        //rc_str.push_str(&format!(" Predictable changes {}", self.predictable_bit_changes));

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

                //                if grpx.not_x_expand.is_low() == false {
                //                    rc_str.push_str(&format!(" exp: {}", &grpx.not_x_expand));
                //                }

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
    /// Action number, index/key into parent ActionStore vector.
    pub num: usize,
    /// Groups of compatible-change squares.
    pub groups: GroupStore,
    /// A store of squares sampled for an action.
    pub squares: SquareStore,
    /// Regions of invalidated groups indicate a new edge of the solution.
    /// Closer and closer dissimilar squares are sought, producing smaller and smaller
    /// regions, until a pair of adjacent, dissimilar, squares are found.
    seek_edge: RegionStore,
    /// The number of integers the Domain/Action uses to represent a bit pattern
    num_ints: usize,
    /// Aggregation of possible bit changes.
    /// For group expansion and confirmation.
    pub predictable_bit_changes: SomeChange,
}

impl SomeAction {
    /// Return a new SomeAction struct, given the number integers used in the SomeBits struct,
    /// and the Action number, an index into SomeDomain::ActionStore which contains it.
    pub fn new(num_ints: usize) -> Self {
        assert!(num_ints > 0);

        SomeAction {
            num: 0,
            groups: GroupStore::new(),
            squares: SquareStore::new(),
            seek_edge: RegionStore::new(),
            num_ints: num_ints,
            predictable_bit_changes: SomeChange::new_low(num_ints),
        }
    }

    /// Return Combinable enum for any two squares with the same Pn value.
    /// Check squares inbetween for compatibility.
    pub fn can_combine(&self, sqrx: &SomeSquare, sqry: &SomeSquare) -> Combinable {
        assert!(sqrx.pn() == sqry.pn());

        // Check the two squares
        let cmbx = sqrx.can_combine(&sqry);
        if cmbx == Combinable::False {
            return cmbx;
        }

        // Get keys for all squares in the region formed by the
        // two given squares.
        let stas = self
            .squares
            .stas_in_reg(&SomeRegion::new(&sqrx.state, &sqry.state));

        // If there are no squares inbetween, done.
        if stas.len() == 2 {
            return cmbx;
        }

        // Handle Pn::Unpredictable squares
        if sqrx.pn() == Pn::Unpredictable {
            // Check each inbetween square
            for stax in stas.iter() {
                if *stax == sqrx.state || *stax == sqry.state {
                    continue;
                }

                let sqrz = self.squares.find(&stax).unwrap();
                if sqrz.pn() == Pn::Unpredictable {
                } else {
                    if sqrz.pnc() {
                        return Combinable::False;
                    }
                }
            }
            return Combinable::True;
        }

        // Get rules
        let rulsx = sqrx.rules.union(&sqry.rules).unwrap();

        // Check squares between for compatibility to the rules.
        for stax in stas.iter() {
            if *stax == sqrx.state || *stax == sqry.state {
                continue;
            }

            let sqrz = self.squares.find(&stax).unwrap();

            if sqrz.pn() == Pn::Unpredictable {
                // sqrx.pn() cannot be Unpredictable at this point, so this invalidates
                return Combinable::False;
            }

            if sqrz.pn() != sqrx.pn() {
                if sqrz.pnc() {
                    return Combinable::False;
                }
                if sqrz.pn() > sqrx.pn() {
                    return Combinable::False;
                }
                if sqrx.pn() == Pn::Two && sqrz.pn() == Pn::One && sqrz.len_results() > 1 {
                    return Combinable::False;
                }
            }

            if sqrz.rules.is_subset_of(&rulsx) == false {
                return Combinable::False;
            }
        } // next stax

        cmbx
    }

    /// Evaluate a sample taken to satisfy a need.
    ///
    /// If the GroupStore has changed, recalcualte the predictable change mask.
    pub fn eval_need_sample(
        &mut self,
        initial: &SomeState,
        ndx: &SomeNeed,
        result: &SomeState,
        dom: usize,
    ) {
        self.groups.changed = false;

        self.eval_need_sample2(initial, ndx, result, dom);
    }

    pub fn eval_need_sample2(
        &mut self,
        initial: &SomeState,
        ndx: &SomeNeed,
        result: &SomeState,
        dom: usize,
    ) {
        // println!("take_action_need2 {}", &ndx);

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
                self.store_sample(&initial, &result, dom);

                // Form the rules, make the group
                // If the squares are incompatible, or need more samples, skip action.
                let sqrx = self.squares.find(&sta).unwrap();
                if let Some(sqry) = self.squares.find(&far) {
                    if sqrx.pn() == sqry.pn() && self.can_combine(&sqrx, &sqry) == Combinable::True
                    {
                        if sqrx.pn() == Pn::Unpredictable {
                            if self.groups.any_superset_of(&for_reg) == false {
                                self.groups.push(
                                    SomeGroup::new(&sqrx.state, &sqry.state, RuleStore::new()),
                                    dom,
                                    self.num,
                                );
                            } else {
                                let regs = self.groups.supersets_of(&for_reg);
                                println!(
                                    "Dom {} Act {} Supersets found for new group (1) {} in {}",
                                    &dom, &self.num, for_reg, &regs
                                );
                            }
                        } else {
                            let rulsxy = sqrx.rules.union(&sqry.rules).unwrap();

                            //println!("Adding group   {}", &rulsxy[0].initial_region());
                            if self.groups.any_superset_of(&rulsxy[0].initial_region()) {
                                //println!(
                                //    "Supersets found for new group {} in {}",
                                //    rulsxy.initial_region(),
                                //    self.groups.supersets_of(&rulsxy[0].initial_region())
                                //);
                            } else {
                                self.groups.push(
                                    SomeGroup::new(&sqrx.state, &sqry.state, rulsxy),
                                    dom,
                                    self.num,
                                );
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
                self.store_sample(&initial, &result, dom);
                self.check_square_new_sample(&initial, dom);

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
                        if sqr1.is_adjacent(&sqr3) {
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
                        if sqr2.is_adjacent(&sqr3) {
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
                // Store sample.
                self.store_sample(&initial, &result, dom);
                // Check to invalidate old groups, create new groups.
                self.check_square_new_sample(&initial, dom);
            }
        } // end match ndx
    } // End take_action_need2

    /// Add a sample by user command.
    /// Its best to start a session and proceed with:
    ///   all user-specified samples, or
    ///   no user-specified samples.
    ///
    /// If the GroupStore has changed, recalcualte the predictable change mask.
    pub fn eval_arbitrary_sample(
        &mut self,
        init_state: &SomeState,
        rslt_state: &SomeState,
        dom: usize,
    ) {
        println!(
            "take_action_arbitrary for state {} result {}",
            init_state, rslt_state
        );

        self.groups.changed = false;

        self.store_sample(&init_state, &rslt_state, dom);
        self.check_square_new_sample(&init_state, dom);

        // Update predictable bit change masks, for group expansion and confirmation.
        if self.groups.changed {
            self.predictable_bit_changes = self.possible_bit_changes();
        }
    }

    /// Evaluate the sample taken for a step in a plan.
    ///
    /// If the GroupStore has changed, recalcualte the predictable change mask.
    pub fn eval_step_sample(&mut self, cur: &SomeState, new_state: &SomeState, dom: usize) {
        self.groups.changed = false;

        self.eval_step_sample2(cur, new_state, dom);

        // Update predictable bit change masks, for group expansion and confirmation.
        if self.groups.changed {
            self.predictable_bit_changes = self.possible_bit_changes();
        }
    }

    pub fn eval_step_sample2(&mut self, cur: &SomeState, new_state: &SomeState, dom: usize) {
        // If square exists, update it, check square, return
        if let Some(sqrx) = self.squares.find_mut(cur) {
            // println!("about to add result to sqr {}", cur.str());
            sqrx.add_result(new_state.clone());

            if sqrx.changed() {
                self.check_square_new_sample(cur, dom);
                return;
            }

            return;
        }

        // Get num groups that might be invalidated
        let num_grps_invalidated = self.groups.check_sample(cur, new_state, dom, self.num);

        // Get num active groups in
        let num_grps_in = self.groups.num_groups_state_in(cur);
        if num_grps_invalidated > 0 {
            println!(
                "Dom {} Act {} sqr {} in {} groups, invalidated {}",
                dom, self.num, cur, num_grps_in, num_grps_invalidated
            );
        }

        if num_grps_invalidated > 0 || num_grps_in == 0 {
            self.store_sample(cur, new_state, dom);
            self.check_square_new_sample(cur, dom);
        }
    }

    /// Store a sample.
    /// Update an existing square or create a new square.
    fn store_sample(&mut self, cur: &SomeState, new_state: &SomeState, dom: usize) {
        // Get an existing square and update, or create a new square.
        //println!("store_sample");
        if let Some(sqrx) = self.squares.find_mut(cur) {
            // println!("about to add result to sqr {}", cur.str());
            sqrx.add_result(new_state.clone());
        } else {
            // println!("No square found for state {}", cur.str());
            self.squares.insert(
                SomeSquare::new(cur.clone(), new_state.clone()),
                dom,
                self.num,
            );
        }
    }

    /// Check a square, referenced by state, against valid groups.
    /// The square may invalidate some groups.
    /// Add a group for the square if the square is in no valid group.
    /// If any groups were invalidated, check for any other squares
    /// that are in no groups.
    fn check_square_new_sample(&mut self, key: &SomeState, dom: usize) {
        //println!("check_square_new_sample");

        let sqrx = self.squares.find(&key).unwrap();

        // Get groups invalidated, which may orphan some squares.
        //let regs_invalid = self.validate_groups_new_sample(&key);
        let regs_invalid = self.groups.check_square(&sqrx, dom, self.num);

        // Save regions invalidated to seek new edges.
        for regx in regs_invalid.iter() {
            if regx.x_mask().num_one_bits() > 1 {
                self.seek_edge.push_nosubs(regx.clone());
            }
        }

        // Create a group for square if needed
        if regs_invalid.len() == 0 {
            if self.groups.num_groups_state_in(&key) == 0 {
                self.create_groups_from_square(&key, dom);
            }
            return;
        }

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
                self.create_groups_from_square(&keyx, dom);
            }
        }
    } // end check_square_new_sample

    /// Check groups due to a new, or updated, square.
    /// Create a group with the square, if needed.
    fn create_groups_from_square(&mut self, key: &SomeState, dom: usize) {
        //println!("create_groups_from_square {}", &key);

        // Square should exist
        let sqrx = self.squares.find(&key).unwrap();

        // Get num active groups in
        let num_grps_in = self.groups.num_groups_state_in(&sqrx.state);
        println!(
            "\nDom {} Act {} Square {} in {} groups",
            dom,
            self.num,
            sqrx.str_terse(),
            num_grps_in,
        );

        if num_grps_in > 0 {
            return;
        }

        //println!("Checking Square {} for new groups", &sqrx.str_terse());

        // Get possible regions, sqrx.state will be <region>.state1
        let rsx: RegionStore = self.possible_regions_from_square(sqrx);

        if rsx.len() == 0 {
            // Make a single-square group
            self.groups.push(
                SomeGroup::new(&sqrx.state, &sqrx.state, sqrx.rules.clone()),
                dom,
                self.num,
            );
            return;
        }

        // println!("Regions for new groups {}", rsx.str());
        for regx in rsx.iter() {
            if regx.active {
            } else {
                continue;
            }
            if sqrx.pn() == Pn::Unpredictable {
                self.groups.push(
                    SomeGroup::new(&sqrx.state, &regx.state2, RuleStore::new()),
                    dom,
                    self.num,
                );
            } else {
                let sqry = self.squares.find(&regx.state2).unwrap();
                let ruls = sqrx.rules.union(&sqry.rules).unwrap();
                //println!("Squares with states {}, {} produce ruls {}", &sqrx.state, &sqry.state, ruls);

                self.groups.push(
                    SomeGroup::new(&sqrx.state, &sqry.state, ruls),
                    dom,
                    self.num,
                );
            }
        } // end for regx
    } // end create_groups_from_square

    /// Get needs for an Action, to improve understanding of the result pattern(s).
    /// When most needs are satisfied, needs for group confirmation are generated.
    /// If housekeeping needs are generated, they are processed and needs
    /// are checked again.
    pub fn get_needs(&mut self, cur_state: &SomeState, x_mask: &SomeMask, dom: usize) -> NeedStore {
        //println!("Running Action {}::get_needs {}", self.num, cur_state);

        // loop until no housekeeping need is returned.
        let mut nds = NeedStore::new();
        let mut cnt = 0;
        loop {
            cnt += 1;

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
            //println!("Ran seek_edge_needs1");
            if ndx.len() > 0 {
                nds.append(&mut ndx);
            } else {
                let mut ndx = self.seek_edge_needs2();
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
            //println!("Ran group_pair_needs");
            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Check for squares in-one-group needs
            if nds.len() == 0 {
                let mut ndx = self.confirm_groups_needs(&x_mask);

                if ndx.len() > 0 {
                    nds.append(&mut ndx);
                }
            }

            if nds.len() == 0 {
                let mut ndx = self.expand_needs(&x_mask);

                if ndx.len() > 0 {
                    nds.append(&mut ndx);
                }
            }

            //println!("needs: {}", nds);
            if cnt > 20 {
                println!("needs: {}", &nds);
                panic!("loop count GT 20!");
            }

            // Process a few specific housekeeping needs related to changing the Action or groups.
            let mut try_again = false;
            for ndx in nds.iter_mut() {
                match ndx {
                    SomeNeed::AddGroup { group_region: greg } => {
                        try_again = true;
                        // Add a new group
                        if self.groups.any_superset_of(&greg) {
                            let sups = self.groups.supersets_of(&greg);
                            if sups.len() == 1 && sups.contains(&greg) {
                            } else {
                                println!(
                                    "**** Supersets found for new group {} in {}",
                                    &greg,
                                    self.groups.supersets_of(&greg)
                                );
                            }
                            continue;
                        }

                        println!(
                            "Dom {} Act {} AddGroup {} using {} and {}",
                            dom, self.num, &greg, &greg.state1, &greg.state2
                        );

                        let sqrx = self.squares.find(&greg.state1).unwrap();
                        let sqry = self.squares.find(&greg.state2).unwrap();

                        assert!(sqrx.pn() == sqry.pn());

                        if sqrx.pn() == Pn::Unpredictable {
                            self.groups.push(
                                SomeGroup::new(&greg.state1, &greg.state2, RuleStore::new()),
                                dom,
                                self.num,
                            );
                        } else {
                            self.groups.push(
                                SomeGroup::new(
                                    &greg.state1,
                                    &greg.state2,
                                    sqrx.rules.union(&sqry.rules).unwrap(),
                                ),
                                dom,
                                self.num,
                            );
                        }
                    }
                    SomeNeed::SetGroupConfirmed {
                        group_region: greg,
                        cstate: sta1,
                    } => {
                        try_again = true;
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            println!("\nAct {} Group {} confirmed using {}", self.num, greg, sta1);
                            grpx.set_anchor(sta1.clone());
                        }
                    }
                    SomeNeed::ClearEdgeExpandBit {
                        group_region: greg,
                        mbit: mbitx,
                    } => {
                        try_again = true;
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            grpx.check_off_expand_bit(&mbitx);
                        }
                    }
                    SomeNeed::InactivateSeekEdge { reg: regx } => {
                        try_again = true;
                        self.seek_edge.inactivate(&regx);
                    }
                    SomeNeed::AddSeekEdge { reg: regx } => {
                        try_again = true;
                        if self.seek_edge.push_nosups(regx.clone()) {
                        } else {
                            println!("need {} failed to add to\n{}", &ndx, &self.seek_edge);
                            panic!("done");
                        }
                    }
                    SomeNeed::ClearGroupPairNeeds { group_region: greg } => {
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            try_again = true;
                            grpx.pair_needs = false;
                        }
                    }
                    _ => {}
                } // end match
            } // next ndx

            if try_again == false {
                // Update predictable bit change masks, for group expansion and confirmation.
                if self.groups.changed {
                    self.predictable_bit_changes = self.possible_bit_changes();
                }

                //println!("Act: {} get_needs: returning: {}", &self.num, &nds);
                return nds;
            }

            nds = NeedStore::new();
        } // end loop
    } // end get_needs

    /// When a group is invalidated by a new sample, something was wrong with that group.
    ///
    /// When the group has more than one X-bit position (the region states are not adjacent),
    /// the number of possible replacement groups will be greatly decreased if the
    /// new sample can be used to find an adjacent, dissimilar pair of squares
    /// within the invalidated group.
    ///
    pub fn seek_edge_needs1(&self) -> NeedStore {
        //println!("seek_edge_needs1");
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

            // Check squares that define the region.

            let cnb1 = sqr1.can_combine(&sqr2);

            match cnb1 {
                // A group of Pn:One may be invalidated by more samples, leading to
                // the same group at Pn:+
                Combinable::True => {
                    ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                    continue;
                }
                Combinable::MoreSamplesNeeded => {
                    if sqr1.pn() < sqr2.pn() {
                        ret_nds.push(SomeNeed::SeekEdge {
                            dom_num: 0, // set this in domain get_needs
                            act_num: self.num,
                            targ_state: sqr1.state.clone(),
                            in_group: regx.clone(),
                        });
                        continue;
                    } else if sqr2.pn() < sqr1.pn() {
                        ret_nds.push(SomeNeed::SeekEdge {
                            dom_num: 0, // set this in domain get_needs
                            act_num: self.num,
                            targ_state: sqr2.state.clone(),
                            in_group: regx.clone(),
                        });
                        continue;
                    } else {
                        panic!(
                            "sqrpn {} {} == sqr pn {} {}",
                            &sqr1.state,
                            sqr1.pn(),
                            &sqr2.state,
                            sqr2.pn()
                        );
                    }
                }
                Combinable::False => {
                    if sqr1.is_adjacent(&sqr2) {
                        if sqr1.pnc() == false {
                            ret_nds.push(SomeNeed::SeekEdge {
                                dom_num: 0, // set this in domain get_needs
                                act_num: self.num,
                                targ_state: sqr1.state.clone(),
                                in_group: regx.clone(),
                            });
                            continue;
                        } else if sqr2.pnc() == false {
                            ret_nds.push(SomeNeed::SeekEdge {
                                dom_num: 0, // set this in domain get_needs
                                act_num: self.num,
                                targ_state: sqr2.state.clone(),
                                in_group: regx.clone(),
                            });
                            continue;
                        } else {
                            ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                            continue;
                        }
                    } // else seek_edge_need2 will look for states between
                }
            } // end match cnb1

            if stas_in.len() == 2 {
                continue;
            }

            // Check squares any pair of squares in the region that are not
            // combinable, except both the region defining squares.
            let mut found = false;
            for inx in 0..stas_in.len() {
                let stax = &stas_in[inx];

                let sqrx = self.squares.find(&stax).unwrap();

                for iny in (inx + 1)..stas_in.len() {
                    let stay = &stas_in[iny];

                    let regy = SomeRegion::new(&stax, &stay);

                    if &regy == regx {
                        continue;
                    }

                    let sqry = self.squares.find(&stay).unwrap();

                    match sqrx.can_combine(&sqry) {
                        Combinable::False => {
                            //  println!(
                            //      "sqr {} is not combinable with {}",
                            //      &stax, &sqry
                            //  );

                            new_regs.push_nosups(regy);
                            found = true;
                        }
                        _ => {}
                    } // end match sqrx, sqry
                } // next iny
            } // next inx

            if found {
                continue;
            }

            // Look for pairs that may need more samples, except both the region defining squares.
            for inx in 0..stas_in.len() {
                let sqrx = self.squares.find(&stas_in[inx]).unwrap();

                for iny in (inx + 1)..stas_in.len() {
                    let regy = SomeRegion::new(&sqrx.state, &stas_in[iny]);

                    if &regy == regx {
                        continue;
                    }

                    let sqry = self.squares.find(&stas_in[iny]).unwrap();

                    match sqrx.can_combine(&sqry) {
                        Combinable::MoreSamplesNeeded => {
                            //  println!(
                            //      "sqr {} msn with {}",
                            //      &stax, &sqry.state
                            //  );
                            if sqrx.pn() < sqry.pn() {
                                ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: sqrx.state.clone(),
                                    in_group: regx.clone(),
                                });
                                found = true;
                            } else if sqry.pn() < sqrx.pn() {
                                ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: sqry.state.clone(),
                                    in_group: regx.clone(),
                                });
                                found = true;
                            }
                        }
                        _ => {}
                    } // end match sqrx+y
                } // next iny
            } // next inx

            // Group might be invalidate due to a Pn increase, then be OK
            // after more samples.
            if found == false {
                ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
            }
        } // next regx closer_reg

        // Apply new seek edge regions
        if new_regs.any_active() {
            //ret_nds = NeedStore::new();
            for regx in new_regs.iter() {
                if regx.active {
                    ret_nds.push(SomeNeed::AddSeekEdge { reg: regx.clone() });
                }
            }
        }

        return ret_nds;
    } // end seek_edge_needs1

    /// For seek_edge regions that have more than one X bit position
    /// (the region states are not adjacent) and have no squares between them,
    /// return needs to seek a sample between them.
    pub fn seek_edge_needs2(&self) -> NeedStore {
        //println!("seek_edge_needs2");
        let mut ret_nds = NeedStore::new();

        // Get seek edge needs, scan the action seek_edge RegionStore.
        for regx in self.seek_edge.iter() {
            if regx.active == false {
                continue;
            }

            if regx.state1.is_adjacent(&regx.state2) {
                panic!(
                    "region states {} {} are adjacent",
                    &regx.state1, &regx.state2
                );
            }

            if regx.state1 == regx.state2 {
                panic!("region states {} {} are equal", &regx.state1, &regx.state2);
            }

            let stas_in = self.squares.stas_in_reg(&regx);

            //println!("seek edge in {} stas {}", &regx, &stas_in);

            if stas_in.len() == 2 {
                // No unsatisfied needs from seek_edge_needs1
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

                let mut dif_msk = SomeMask::new(SomeBits::new_low(self.num_ints));

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
        } // next regx

        ret_nds
    } // end seek_edge_needs2

    /// Get additional sample needs for the states that form a group
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

    /// Return expand needs for groups.
    fn expand_needs(&mut self, x_mask: &SomeMask) -> NeedStore {
        //println!("expand_needs");
        let mut ret_nds = NeedStore::new();

        //        let regs = self.groups.regions();
        //        let states_in_1 = self.squares.states_in_1_region(&regs);

        for grpx in self.groups.iter() {
            if grpx.active == false {
                continue;
            }

            // Check for edge bits under mask of all possible X bits.
            let edge_expand = grpx.edge_expand.m_and(&x_mask);

            if edge_expand.is_low() {
                continue;
            }

            // Get a vector of one-bit masks
            let edge_msks = edge_expand.split();

            // Check for expansion, or single-bit adjacent external
            for edge_msk in edge_msks.iter() {
                let reg_exp = grpx.region.toggle_bits(&edge_msk);

                // At least two squares should be in the group, so in the expanded region
                //let sqr_stas = self.squares.stas_in_reg(&reg_exp);

                let reg_both = reg_exp.union(&grpx.region); // one bit so double the "area"

                let mut ndsx = self.possible_group_needs(&reg_both, 1);

                // println!(
                //     "expand_needs, act {}, check reg {} needs {} for expansion of group {}",
                //     &self.num, &reg_both, &ndsx, &grpx.region
                // );

                if ndsx.len() == 0 {
                    ret_nds.push(SomeNeed::ClearEdgeExpandBit {
                        group_region: grpx.region.clone(),
                        mbit: edge_msk.clone(),
                    });
                } else {
                    ret_nds.append(&mut ndsx);
                }
            } // next bitx
        } // next grpx

        ret_nds
    }

    /// Given a region look for far needs.
    ///
    /// Return needs for first or more samples to reach pn EQ.
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
                if sqrx.pn() == sqry.pn() {
                    return ret_nds;
                }
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

            let sqrx = self.squares.find(&stax).unwrap();

            if let Some(sqry) = self.squares.find(&sta_far) {
                if sqry.pn() < sqrx.pn() {
                    if sqrx.pnc() {
                        return NeedStore::new();
                    }
                    ret_nds.push(SomeNeed::AStateMakeGroup {
                        dom_num: 0, // set this in domain get_needs
                        act_num: self.num,
                        targ_state: sta_far.clone(),
                        for_reg: aregion.clone(),
                        far: stax.clone(),
                        num_x: aregion.num_x(),
                    });
                }
            } else {
                // far square not found
                ret_nds.push(SomeNeed::AStateMakeGroup {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sta_far.clone(),
                    for_reg: aregion.clone(),
                    far: stax.clone(),
                    num_x: aregion.num_x(),
                });
            }
        }
        //println!("Act {} far_needs for {} returning {}", &self.num, &aregion, &ret_nds);
        ret_nds
    } // end far_needs

    /// Check for squares-in-one-group (anchor) needs.
    ///
    /// Seek adjacent samples outside the group, and the far square
    /// in the group.
    ///
    /// If an external, adjacent, square is combinable with the anchor,
    /// return a need to make a group of the pair.
    ///
    /// Recheck the rating of the current anchor, and other possible anchors,
    /// in case the anchor should be changed.
    fn confirm_groups_needs(&mut self, x_mask: &SomeMask) -> NeedStore {
        //println!("confirm_groups_needs");
        let mut ret_nds = NeedStore::new();

        let regs = self.groups.regions();

        let mut new_group_regs = RegionStore::new();

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

            // Get mask of edge bits to use to confirm.
            let edge_confirm = grpx.region.x_mask().m_not().m_and(&x_mask);

            // Get the bit masks on non-X bit-positions in greg
            let edge_msks = edge_confirm.split();

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
                for edge_bit in edge_msks.iter() {
                    let sta_adj = SomeState::new(sta1.bts.b_xor(&edge_bit.bts));
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
                } // next sta1

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
                    } else {
                        grpx.set_anchor_off();
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

                //println!("*** for group {} checking adj sqr {}", &greg, &adj_sta);

                if let Some(adj_sqr) = self.squares.find(adj_sta) {
                    if adj_sqr.pnc() {
                        let new_reg = SomeRegion::new(&anchor_sta, &adj_sta);

                        if new_group_regs.any_superset_of(&new_reg)
                            || self.groups.any_superset_of(&new_reg)
                        {
                            continue;
                        }

                        new_group_regs.push_nosubs(new_reg.clone());

                        let anchor_sqr = self.squares.find(anchor_sta).unwrap();

                        //println!(
                        //    "confirm_group_needs AddGroup {} using {} and {}",
                        //    &new_reg, &anchor_sta, &adj_sta
                        //);

                        if anchor_sqr.can_combine(&adj_sqr) == Combinable::True {
                            nds_grp_add.push(SomeNeed::AddGroup {
                                group_region: new_reg,
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

    /// Check needs for adjacent and intersecting groups.
    fn group_pair_needs(&self) -> NeedStore {
        //println!("group_pair_needs");
        let mut nds = NeedStore::new();

        let mut regs_done = RegionStore::new();

        // Check every pair of groups
        for inx in 0..self.groups.len() {
            let grpx = &self.groups[inx];

            if grpx.active == false || grpx.pair_needs == false {
                continue;
            }

            let mut need_flag = false;

            // Pair grpx with every group after it in the GroupStore
            for iny in 0..self.groups.len() {
                if iny == inx {
                    continue;
                }

                let grpy = &self.groups[iny];

                if grpy.active == false {
                    continue;
                }

                if grpx.region.intersects(&grpy.region) {
                    let regx = grpx.region.union(&grpy.region);
                    if regs_done.contains(&regx) {
                    } else {
                        regs_done.push(regx);
                        let mut ndx = self.group_pair_intersection_needs(&grpx, &grpy);
                        if ndx.len() > 0 {
                            need_flag = true;
                            nds.append(&mut ndx);
                        }
                    }
                } else if grpx.region.is_adjacent(&grpy.region) {
                    if grpx.pn == grpy.pn {
                        let mut ndx = self.group_pair_adjacent_needs(&grpx, &grpy);
                        if ndx.len() > 0 {
                            need_flag = true;
                            nds.append(&mut ndx);
                        }
                    } // end if pn ==
                } // end if regions adjacent
            } // next iny

            if need_flag == false {
                nds.push(SomeNeed::ClearGroupPairNeeds {
                    group_region: grpx.region.clone(),
                });
            }
        } // next inx

        nds
    } // end group_pair_needs

    /// Check for needs, for making a given region into a group.
    /// A need may be to take more samples, or just add the group.
    fn possible_group_needs(&self, reg_grp: &SomeRegion, _from: usize) -> NeedStore {
        //println!("possible_group_needs for {} from {}", &reg_grp, from);
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
                targ_state: sta2,
                for_reg: reg_grp.clone(),
                far: sta1,
                num_x: reg_grp.num_x(),
            });

            return nds;
        }

        // If one square in the region, return a need for the far square.
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
        //let max_pn = self.squares.max_pn(&stas_in_reg);

        let pair_stas = reg_grp.defining_pairs(&stas_in_reg);

        if pair_stas.len() == 0 {
            //println!("no defining pairs in {}", &pair_stas);
            return self.far_needs(&reg_grp, &stas_in_reg);
        }

        let mut nds2 = NeedStore::new();

        let mut inx = 0;
        while inx < pair_stas.len() {
            let sqrx = self.squares.find(&pair_stas[inx]).unwrap();
            let sqry = self.squares.find(&pair_stas[inx + 1]).unwrap();

            if sqrx.pn() != sqry.pn() {
                if sqrx.pn() < sqry.pn() {
                    if sqrx.pnc() {
                        return NeedStore::new();
                    }
                    nds2.push(SomeNeed::StateAdditionalSample {
                        dom_num: 0, // set this in domain get_needs
                        act_num: self.num,
                        targ_state: sqrx.state.clone(),
                        grp_reg: reg_grp.clone(),
                        far: sqry.state.clone(),
                    });
                }

                if sqry.pn() < sqrx.pn() {
                    if sqry.pnc() {
                        return NeedStore::new();
                    }
                    nds2.push(SomeNeed::StateAdditionalSample {
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

            let regz = SomeRegion::new(&sqrx.state, &sqry.state);
            if regz != *reg_grp {
                panic!("defining pair reg {} ne grp_reg {}??", &regz, &reg_grp);
            }

            let cmbl = self.can_combine(&sqrx, &sqry);
            //println!("can {} combine with {}, {}", &sqrx, &sqry, &cmbl);

            if cmbl == Combinable::MoreSamplesNeeded {
                if sqrx.pn() < sqry.pn() {
                    nds.push(SomeNeed::StateAdditionalSample {
                        dom_num: 0, // set this in domain get_needs
                        act_num: self.num,
                        targ_state: sqrx.state.clone(),
                        grp_reg: reg_grp.clone(),
                        far: sqry.state.clone(),
                    });
                }

                if sqry.pn() < sqrx.pn() {
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

            if cmbl == Combinable::True {
                nds = NeedStore::new();
                let new_reg = SomeRegion::new(&sqrx.state, &sqry.state);

                if self.groups.any_superset_of(&new_reg) {
                } else {
                    nds = NeedStore::new();

                    //println!(
                    //     "possible_group_needs AddGroup {} using {} and {}",
                    //       &SomeRegion::new(&sqrx.state, &sqry.state), &sqrx.state, &sqry.state
                    //);
                    nds.push(SomeNeed::AddGroup {
                        group_region: SomeRegion::new(&sqrx.state, &sqry.state),
                    });
                    return nds;
                }
            } else if cmbl == Combinable::False {
                return NeedStore::new();
            }
            inx += 2;
        } // next inx

        if nds.len() == 0 {
            return nds2;
        }
        nds
    } // end possible_group_needs

    /// Check two intersecting groups for needs.
    /// Possibly combining to groups.
    /// Possibly checking for a contradictatory intersection.
    fn group_pair_intersection_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //println!(
        //    "groups_intersection_needs {} and {}",
        //    &grpx.region, &grpy.region
        //);

        let mut nds = NeedStore::new();

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
            // Check if a valid sub-region of the intersection exists
            if let Some(rulsxy) = rulsx.intersection(&rulsy) {
                // A valid sub-union exists, seek a sample in intersection that is not in rulsxy.initial_region
                let ok_reg = rulsxy.initial_region();

                if ok_reg == reg_int {
                    nds.push(self.cont_int_region_needs(&reg_int, &grpx, &grpy));
                } else {
                    // to avoid subtraction, use the far sub-region
                    let regy = reg_int.far_reg(&ok_reg);
                    nds.push(self.cont_int_region_needs(&regy, &grpx, &grpy));
                }
            } else {
                nds.push(self.cont_int_region_needs(&reg_int, &grpx, &grpy));
            }
            return nds;
        }

        // Rules are the same, see if the two groups can be combined
        self.possible_group_needs(&reg_both, 3)
    } // end group_pair_intersection_needs

    /// Get needs for two adjacent groups, with the same pn rating.
    fn group_pair_adjacent_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //println!(
        //    "group_pair_adjacent_needs {} and {}",
        //    &grpx.region, &grpy.region
        //);
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

                //println!("group_pair_adjacent_needs: {} and {}", &grpx.region, &grpy.region);
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

                        //println!(
                        //    "overlap for act {} grp {} grp {} region {} rules {}",
                        //    self.num, grpx.region, grpy.region, regz, rulesx
                        //);

                        let xx = self.possible_group_needs(&regz, 7);
                        //println!("poss grp needs: {}", &xx);
                        if xx.len() > 0 {
                            println!(
                                "overlap for act {} grp {} grp {} region {} rules {}",
                                self.num, grpx.region, grpy.region, regz, rulesx
                            );
                        }
                        return xx;
                    }
                } // end if let Some
            } // end match pn default
        } // end match pn for adjacent regions
        nds
    } // end group_pair_adjacent_needs

    /// For a contradictory intersection, return a need for more samples.
    /// If no prior samples in the intersection, seek one.
    /// If a prior sampled square is pnc, panic.
    /// If prior samples found, seek additional samples.
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

    /// Get possible steps that can be used to make at least part of a
    /// given change.
    ///
    /// For each rule, prune the rule X bit positions to favor desired changes and aoiv undesired changes.
    ///
    /// For a two-result group, see if there is an existing square that is expected to
    /// produce the desired change.
    pub fn get_steps(&self, achange: &SomeChange) -> StepStore {
        let mut stps = StepStore::new();

        for grpx in &self.groups.avec {
            if grpx.active == false {
                continue;
            }

            match grpx.pn {
                Pn::One => {
                    // Find bit changes that are desired
                    if let Some(rulx) = grpx.rules[0].parse_for_changes(&achange.b01, &achange.b10)
                    {
                        stps.push(SomeStep::new(self.num, rulx, false, grpx.region.clone()));
                    }
                }
                Pn::Two => {
                    for ruly in &grpx.rules.avec {
                        if let Some(rulx) = ruly.parse_for_changes(&achange.b01, &achange.b10) {
                            // See if an existing square is ready to produce the desired result
                            let i_reg = rulx.initial_region();
                            let stas = self.squares.stas_in_reg(&i_reg);

                            let mut found = false;
                            for stax in stas.iter() {
                                let sqrx = self.squares.find(&stax).unwrap();

                                // Will include at least one bit change desired, but maybe others.
                                let expected_result = rulx.result_from_initial_state(&stax);

                                // If a Pn::Two squares last result is not equal to what is wanted,
                                // the next result should be.
                                if sqrx.last_result() != &expected_result {
                                    let stpx = SomeStep::new(
                                        self.num,
                                        rulx.restrict_initial_region(&SomeRegion::new(
                                            &stax, &stax,
                                        )),
                                        false,
                                        grpx.region.clone(),
                                    );
                                    stps.push(stpx);
                                    found = true;
                                } // end if
                            } // next stax

                            if found == false {
                                stps.push(SomeStep::new(self.num, rulx, true, grpx.region.clone()));
                            }
                        } // endif Some(rulx)
                    } // next ruly
                } // end match Two
                Pn::Unpredictable => {}
            } // end match grpx.pn
        } // next grpx

        stps
    } // end get_steps

    /// Get a random choice of a number of unique numbers (num_results) to a
    /// given number of positions, 0, 1 .. -> the_len (exclusive).
    /// random 2 of 5 -> [0, 3]
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

    /// Find squares whose rules can be combined with a given squares rules.
    /// Check if any included squares invalidate a combination.
    /// Remove subset combinations.
    /// Return the regions resulting from successful combinations.
    pub fn possible_regions_from_square(&self, sqrx: &SomeSquare) -> RegionStore {
        //println!("possible_group_regions from sqr {}", &sqrx.state);

        let mut rsx = RegionStore::new();

        // Collect possible region, deleting subset regions
        for (key, sqry) in &self.squares.ahash {
            if *key == sqrx.state {
                continue;
            }

            if sqrx.pn() != sqry.pn() {
                continue;
            }

            // Create region, sqrx.state becomes regx.state1
            let regx = SomeRegion::new(&sqrx.state, &sqry.state);

            if self.groups.any_superset_of(&regx) {
                continue;
            }

            if rsx.any_superset_of(&regx) {
                continue;
            }

            if self.can_combine(&sqrx, &sqry) == Combinable::True {
                rsx.push_nosubs(regx);
            }
        } // end for

        // Print possible regions
        for regx in rsx.iter() {
            if regx.active == false {
                continue;
            }

            let sqry = self.squares.find(&regx.state2).unwrap();
            if sqry.pn() == Pn::Unpredictable {
                println!(
                    "\n  Square {} [Unpredictable] can combine with\n  Square {} [Unpredictable]\n  giving {} [Unpredictable]",
                    &sqrx.state, &sqry.state, regx
                );
            } else {
                println!(
                    "\n  Square {} {} can combine with\n  Square {} {}\n  giving {} {}",
                    &sqrx.state,
                    &sqrx.rules,
                    &sqry.state,
                    &sqry.rules,
                    regx,
                    &sqrx.rules.union(&sqry.rules).unwrap()
                );
            }
        }

        //println!("regions for new groups {}", &rsx);
        rsx
    } // end possible_regions_from_square

    /// Aggregate all predictable bit changes that are possible.
    /// Repeated sampling of squares will tend to increase the number
    /// of possible bits changes.  Some changes, like going from
    /// predictable to unpredictable, could decrease them.
    pub fn possible_bit_changes(&self) -> SomeChange {
        let mut ret_cng = SomeChange::new_low(self.num_ints);

        for grpx in &self.groups.avec {
            if grpx.active == false {
                continue;
            }

            match grpx.pn {
                Pn::One => {
                    // Find bit changes that can happen
                    ret_cng.b10 = ret_cng.b10.m_or(&grpx.rules[0].b10);
                    ret_cng.b01 = ret_cng.b01.m_or(&grpx.rules[0].b01);
                }
                Pn::Two => {
                    for rulx in &grpx.rules.avec {
                        ret_cng.b10 = ret_cng.b10.m_or(&rulx.b10);
                        ret_cng.b01 = ret_cng.b01.m_or(&rulx.b01);
                    } // next rulx
                } // end match Two
                Pn::Unpredictable => {}
            } // end match pn
        } // next grpx

        //println!("Act {} pbc: b01: {} b10: {}", &self.num, &ret_cng.b01, &ret_cng.b10);
        ret_cng
    }
}
