//! The SomeAction struct.  It stores, and analyzes, the results of an action.
//!
//! This stores initial->result samples, generates needs for more samples, and
//! represents the current best-guess rules of the expected responses
//! of executing an action for a given state.

//use crate::bits::SomeBits;
use crate::change::SomeChange;
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
use crate::truth::Truth;
//use crate::randompick::RandomPick;
use crate::actions::take_action;
use crate::combine::{can_combine, can_combine_check_between};
//use crate::compare::Compare;

//use rayon::prelude::*;
use std::fmt;
extern crate rand;
use rand::Rng;
use serde::{Deserialize, Serialize};

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("A(ID: ");

        rc_str.push_str(&self.num.to_string());

        if self.seek_edge.len() > 0 {
            rc_str.push_str(&format!(" seek_edge within: {}", self.seek_edge));
        }

        //rc_str.push_str(&format!(" Predictable changes {}", self.predictable_bit_changes));

        let regs = self.groups.regions();

        let mut fil = String::from(",\n       Grps: ");
        for grpx in self.groups.iter() {

            let stas_in = self.squares.stas_in_reg(&grpx.region);

            // Count the number of states in a group that are also in only one region
            let mut cnt = 0;
            for stax in stas_in.iter() {
                if regs.state_in_1_region(stax) {
                    cnt += 1;
                }
            }

            rc_str.push_str(&format!(
                "{}{} num Sqrs: {} in1: {})",
                &fil,
                &grpx.formatted_string(),
                &stas_in.len(),
                &cnt,
            ));

            fil = String::from(",\n             ");
        }

        rc_str.push_str(")");

        write!(f, "{}", rc_str)
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct SomeAction {
    /// Action number, index/key into parent ActionStore vector.
    pub num: usize,
    /// Store for groups of compatible-change squares.
    pub groups: GroupStore,
    /// A store of squares sampled for an action.
    pub squares: SquareStore,
    /// Regions of invalidated groups indicate a new edge of the solution.
    /// Closer and closer dissimilar squares are sought, producing smaller and smaller
    /// regions, until a pair of adjacent, dissimilar, squares are found.
    pub seek_edge: RegionStore,
    /// Changes that the group rules can do. Generally this will expand at first, then plateau, but might regress if something gets stuck.
    pub aggregate_changes: SomeChange,
}

impl SomeAction {
    /// Return a new SomeAction struct, given the number integers used in the SomeBits struct.
    /// The action number, an index into the ActionStore that will contina it, is set to zero and
    /// changed later.
    pub fn new(inx: usize, num_ints: usize) -> Self {

        SomeAction {
            num: inx,
            groups: GroupStore::new(),
            squares: SquareStore::new(),
            seek_edge: RegionStore::new(),
            aggregate_changes: SomeChange::new_low(num_ints),
        }
    }

    /// Set the action number
    pub fn set_num(&mut self, anum: usize) {
        self.num = anum;
    }

    /// Return the number of integers used in the bits instances
    pub fn num_ints(&self) -> usize {
        self.aggregate_changes.b01.num_ints()
    }

    /// Return Truth enum for the combination of any two squares,
    /// and the squares between them.
    pub fn can_combine(&self, sqrx: &SomeSquare, sqry: &SomeSquare) -> Truth {
        assert!(sqrx.state != sqry.state);

        let cmbx = can_combine(sqrx, sqry);
        if cmbx == Truth::F {
            return cmbx;
        }

        let cmbx2 = can_combine_check_between(sqrx, sqry, &self.squares);

        // If cmbx is Truth::M and cmbx2 is Truth::F, return Truth::F
        if cmbx2 < cmbx {
            return cmbx2;
        }
        cmbx
    }

    /// Evaluate a sample taken to satisfy a need.
    pub fn eval_sample(
        &mut self,
        initial: &SomeState,
        result: &SomeState,
        dom: usize,
    ) {
        self.store_sample(&initial, &result, dom);
        self.check_square_new_sample(&initial, dom);
      }

    /// Evaluate a sample taken to satisfy a need.
    pub fn eval_need_sample(
        &mut self,
        initial: &SomeState,
        ndx: &SomeNeed,
        result: &SomeState,
        dom: usize,
    ) {

        // Processing for all needs
        self.eval_sample(initial, result, dom);

        // Additional processing for selected kinds of need
        match ndx {
            SomeNeed::AStateMakeGroup {
                targ_state: sta,
                for_reg,
                far,
                ..
            } => {
                if self.groups.any_superset_of(&for_reg) {
                    return;
                }

                // Form the rules, make the group
                // If the squares are incompatible, or need more samples, skip action.
                let sqrx = self.squares.find(&sta).unwrap();

                //println!("AStateMakeGroup: sqr {} sampled, pn {} pnc {}", &sqrx, sqrx.results.pn, sqrx.results.pnc);
                if let Some(sqry) = self.squares.find(&far) {

                    if sqrx.state == sqry.state || self.can_combine(&sqrx, &sqry) == Truth::T {

                        if sqrx.results.pn == Pn::Unpredictable {

                            self.groups.push(
                                SomeGroup::new(&sqrx.state, &sqry.state, RuleStore::new(), sqrx.results.pnc && sqrx.results.pnc),
                                dom,
                                self.num,
                            );
                        } else {
                            if let Some(rulsxy) = sqrx.rules.union(&sqry.rules) {

                                self.groups.push(
                                    SomeGroup::new(&sqrx.state, &sqry.state, rulsxy, sqrx.results.pnc && sqry.results.pnc),
                                    dom,
                                    self.num,
                                );
                            }
                        } // end if Unpredictable
                    } // end if can combine
                } // end find far
            } // end process AStateMakeGroup Need

            SomeNeed::SeekEdge {
                targ_state: sta,
                in_group: greg,
                ..
            } => {
                // Form the rules, make the group
                let sqr1 = self.squares.find(&greg.state1).unwrap();
                let sqr2 = self.squares.find(&greg.state2).unwrap();
                let sqr3 = self.squares.find(&sta).unwrap();

                // Process next sample of square in-between for new square and state1 square.
                // Should be different from state1 square or state2 square.
                // It may be different from both state1 and state2.
                // If it needs more samples, skip, next need will increment the number samples.
                let cnb1 = can_combine(sqr3, sqr1);
                match cnb1 {
                    Truth::F => {
                        if sqr1.is_adjacent(&sqr3) {
                            println!("Dom {} Act {} new edge found between {} and {} removing seek edge {}", dom, self.num, &sqr1.state, &sqr3.state, &greg); 
                            self.seek_edge.remove_region(greg);
                        } else {
                            let even_closer_reg = SomeRegion::new(&sqr1.state, &sqr3.state);
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }
                    _ => {}
                } // end match cnb1


                // Process next sample of square in-between for new square and state2 square
                // Should be different from state1 square or state2 square
                let cnb2 = can_combine(sqr3, sqr2);
                match cnb2 {
                    Truth::F => {
                        if sqr2.is_adjacent(&sqr3) {
                            println!("Dom {} Act {} new edge found between {} and {} removing seek edge {}", dom, self.num, &sqr2.state, &sqr3.state, &greg); 
                            self.seek_edge.remove_region(greg);
                        } else {
                            let even_closer_reg = SomeRegion::new(&sqr2.state, &sqr3.state);
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }
                    _ => {}
                } // end match cnb2
            } // end match SeekEdgeNeed
            _ => {}
        } // end match ndx

    } // end eval_need_sample

    /// Add a sample by user command.
    /// Its best to start a session and proceed with:
    ///   all user-specified samples, or
    ///   no user-specified samples.
    pub fn eval_arbitrary_sample(
        &mut self,
        initial: &SomeState,
        result: &SomeState,
        dom: usize,
    ) {
        println!(
            "take_action_arbitrary for state {} result {}",
            initial, result
        );

        self.eval_sample(initial, result, dom);
    }

    /// Evaluate the sample taken for a step in a plan.
    pub fn eval_step_sample(&mut self, cur: &SomeState, new_state: &SomeState, dom: usize) {

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
            self.eval_sample(cur, new_state, dom);
        }
    } // end eval_step_sample

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
    /// Update aggregate_changes, if any groups are added or deleted.
    fn check_square_new_sample(&mut self, key: &SomeState, dom: usize) {
        //println!("check_square_new_sample for {}", key);

        let sqrx = self.squares.find(&key).unwrap();

        // Get groups invalidated, which may orphan some squares.
        //let regs_invalid = self.validate_groups_new_sample(&key);
        let regs_invalid: RegionStore = self.groups.check_square(&sqrx, dom, self.num, &self.squares);

        // Save regions invalidated to seek new edges.
        for regx in regs_invalid.iter() {

            if regx.x_mask().num_one_bits() > 2 && *key != regx.state1 && *key != regx.state2 {

                let sqr1 = self.squares.find(&regx.state1).unwrap();
                let sqr2 = self.squares.find(&regx.state2).unwrap();

                if sqr1.results.pnc && sqr2.results.pnc {
                    if sqrx.state.is_adjacent(&sqr1.state) == false && can_combine(sqrx, sqr1) == Truth::F {
                        println!("\nDom {} Act {} Seek edge between {} and {}", &dom, &self.num, &sqrx.state, &sqr1.state);
                        self.seek_edge.push_nosubs(SomeRegion::new(&sqrx.state, &sqr1.state));
                    }
                    if sqrx.state.is_adjacent(&sqr2.state) == false && can_combine(sqrx, sqr2) == Truth::F {
                        println!("\nDom {} Act {} Seek edge between {} and {}", &dom, &self.num, &sqrx.state, &sqr2.state);
                        self.seek_edge.push_nosubs(SomeRegion::new(&sqrx.state, &sqr2.state));
                    }
                }
            }
        }

        // Create a group for square if needed
        if regs_invalid.len() == 0 {
            if self.groups.num_groups_state_in(&key) == 0 {
                self.create_groups_from_square(&key, dom);
                self.aggregate_changes = self.calc_aggregate_changes();
            }
            return;
        }

        // Check squares that may not be in a group

        let regs = self.groups.regions();

        let keys = self.squares.not_in_regions(&regs);

        for keyx in keys.iter() {
            // A later square may be in a group created by an earlier square
            if self.groups.num_groups_state_in(keyx) == 0 {
                self.create_groups_from_square(keyx, dom);
            }
        }

        self.aggregate_changes = self.calc_aggregate_changes();
    } // end check_square_new_sample

    /// Check groups due to a new, or updated, square.
    /// Create a group with the square, if needed.
    fn create_groups_from_square(&mut self, key: &SomeState, dom: usize) {
        //println!("create_groups_from_square {}", &key);

        // Square should exist
        let sqrx = self.squares.find(&key).unwrap();

        // Check if square can be used to create groups
        // Allowing a square to make a group with a single sample is needed
        // for group bootstrapping.
        if sqrx.results.pn == Pn::One || sqrx.results.pnc {
        } else {
            return;
        }

        // Get num groups the state is in
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
                SomeGroup::new(&sqrx.state, &sqrx.state, sqrx.rules.clone(), sqrx.results.pnc),
                dom,
                self.num,
            );
            return;
        }

        // println!("Regions for new groups {}", rsx.str());
        for regx in rsx.iter() {

            if sqrx.results.pn == Pn::Unpredictable {
                self.groups.push(
                    SomeGroup::new(&sqrx.state, &regx.state2, RuleStore::new(), true),
                    dom,
                    self.num,
                );
            } else {
                let sqry = self.squares.find(&regx.state2).unwrap();
                let ruls = sqrx.rules.union(&sqry.rules).unwrap();
                //println!("Squares with states {}, {} produce ruls {}", &sqrx.state, &sqry.state, ruls);

                self.groups.push(
                    SomeGroup::new(&sqrx.state, &sqry.state, ruls, sqrx.results.pnc && sqry.results.pnc),
                    dom,
                    self.num,
                );
            }
        } // next regx
    } // end create_groups_from_square

    /// Return the aggregate changes for an action
    fn calc_aggregate_changes(&self) -> SomeChange {
        let mut ret_cngs = SomeChange::new_low(self.num_ints());

        for grpx in self.groups.iter() {

            if grpx.pn != Pn::Unpredictable {
                let rules = &grpx.rules;

                for rulex in rules.iter() {
                    ret_cngs = ret_cngs.c_or(&rulex.change());
                }
            }
        }

        ret_cngs
    }

    /// Return needs for states that are not in a group.
    /// The Domain current state for which there are no samples.
    /// A pn > 1 state that needs more samples.
    pub fn state_not_in_group_needs(&self, cur_state: &SomeState) -> NeedStore {
        let mut nds = NeedStore::new();
        
        // Check if current state is in any groups
        let mut in_grp = false;
        for grpx in self.groups.iter() {
            if grpx.region.is_superset_of_state(cur_state) {
                in_grp = true;
                break;
            }
        }

        // If not, generate need
        if in_grp == false {
            nds.push(SomeNeed::StateNotInGroup {
                dom_num: 0, // will be set later
                act_num: self.num,
                targ_state: cur_state.clone(),
            });
        }

        // Look for a pn > 1, pnc == false, not in group squares
        // Extra samples are needed to gain pnc, then the first group.
        let sqrs_pngt1 = self.squares.pn_gt1_no_pnc();

        for stax in sqrs_pngt1.iter() {
            if self.groups.any_superset_of_state(&stax) {
                continue;
            }

            if in_grp || stax != cur_state {
                nds.push(SomeNeed::StateNotInGroup {
                    dom_num: 0, // will be set later
                    act_num: self.num,
                    targ_state: stax.clone()
                });
            }
        }

        nds
    }

    /// Get needs for an Action, to improve understanding of the result pattern(s).
    /// When most needs are satisfied, needs for group limitation are generated.
    /// If housekeeping needs are generated, they are processed and needs
    /// are checked again.
    pub fn get_needs(&mut self, cur_state: &SomeState, agg_chgs: &SomeChange, dom: usize) -> NeedStore {
        //println!("Running Action {}::get_needs {}", self.num, cur_state);

        // loop until no housekeeping need is returned.
        let mut nds = NeedStore::new();
        let mut cnt = 0;
        loop {
            cnt += 1;

            // Look for needs for states not in groups
            let mut ndx = self.state_not_in_group_needs(cur_state);
            if ndx.len() > 0 {
                nds.append(&mut ndx);
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

            // Check for expand needs
            let mut ndx = self.expand_needs(agg_chgs);

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
                let mut ndx = self.limit_groups_needs(agg_chgs);

                if ndx.len() > 0 {
                    nds.append(&mut ndx);
                }
            }

            // Check for repeating housekeeping needs loop
            if cnt > 20 {
                println!("needs: {}", &nds);
                panic!("Dom {} Act {} loop count GT 20!", dom, self.num);
            }

            // Process a few specific housekeeping needs related to changing the Action or groups.
            let mut try_again = false;
            for ndx in nds.iter_mut() {
                match ndx {
                    SomeNeed::AddGroup { group_region: greg } => {

                        // Check for supersets
                        if self.groups.any_superset_of(&greg) {
                            let sups = self.groups.supersets_of(&greg);
                            if sups.len() == 1 && sups.contains(&greg) {
                            } else {
                                println!(
                                    "\nDom {} Act {} **** Supersets found for new group {} in {}",
                                    dom, self.num, &greg,
                                    self.groups.supersets_of(&greg)
                                );
                            }
                            continue;
                        }

                        try_again = true;
                        // Add the new group
                        println!(
                            "Dom {} Act {} AddGroup {} using {} and {}",
                            dom, self.num, &greg, greg.state1, greg.state2
                        );

                        let sqrx = self.squares.find(&greg.state1).unwrap();
                        let sqry = self.squares.find(&greg.state2).unwrap();

                        assert!(sqrx.results.pn == sqry.results.pn);

                        if sqrx.results.pn == Pn::Unpredictable {
                            self.groups.push(
                                SomeGroup::new(&greg.state1, &greg.state2, RuleStore::new(), true),
                                dom,
                                self.num,
                            );
                        } else {
                            self.groups.push(
                                SomeGroup::new(
                                    &greg.state1,
                                    &greg.state2,
                                    sqrx.rules.union(&sqry.rules).unwrap(),
                                    sqrx.results.pnc && sqry.results.pnc,
                                ),
                                dom,
                                self.num,
                            );
                        }
                    }
                    SomeNeed::SetGroupLimited {
                        group_region: greg,
                        cstate: sta1,
                    } => {
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            println!("\nDom {} Act {} Group {} limited using {}", dom, self.num, greg, sta1);
                            grpx.set_anchor(sta1.clone());
                        }
                    }
                    SomeNeed::RemoveGroupAnchor {
                        group_region: greg,
                    } => {
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            try_again = true;
                            grpx.set_anchor_off();
                        }
                    }
                    SomeNeed::SetEdgeExpand {
                        group_region: greg,
                        edge_mask: mbitx,
                    } => {
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            try_again = true;
                            grpx.set_edge_expand(&mbitx);
                        }
                    }
                    SomeNeed::InactivateSeekEdge { reg: regx } => {
                        self.seek_edge.remove_region(&regx);
                    }
                    SomeNeed::AddSeekEdge { reg: regx } => {
                        try_again = true;
                        if self.seek_edge.push_nosups(regx.clone()) {
                        } else {
                            println!("Dom {} Act {} need {} failed to add to\n{}", dom, self.num, &ndx, &self.seek_edge);
                            panic!("done");
                        }
                    }
                    _ => {}
                } // end match
            } // next ndx

            if try_again == false {
                //println!("Act: {} get_needs: returning: {}", &self.num, &nds);
                if nds.len() == 0 {
                    return self.left_over_needs();
                }

                // Filter out housekeeping needs, if any.
                let mut inxs = Vec::<usize>::with_capacity(nds.len());
                let mut inx = 0;
                for ndx in nds.iter() {
                    match ndx  {
                        SomeNeed::AddGroup { group_region: _ } => { inxs.push(inx); }
                        SomeNeed::SetGroupLimited {
                            ..
                        } => { inxs.push(inx); }
                        SomeNeed::RemoveGroupAnchor {
                            ..
                        } => { inxs.push(inx); }
                        SomeNeed::SetEdgeExpand {
                            ..
                        } => { inxs.push(inx); }
                        SomeNeed::InactivateSeekEdge { reg: _ } => { inxs.push(inx); } 
                        SomeNeed::AddSeekEdge { reg: _ } => { inxs.push(inx); }
                        _ => { }
                    }
                    inx += 1;
                }

                for inx in inxs.iter().rev() {
                    nds.remove_unordered(*inx);
                }
                return nds;
            }

            nds = NeedStore::new();
        } // end loop
    } // end get_needs

    /// Get left-over needs, from regions not covered by groups,
    /// and no current samples.
    pub fn left_over_needs(&self) -> NeedStore {
        let mut nds = NeedStore::new();
        
        let regs = self.left_overs();

        for regx in regs.iter() {
            nds.push(SomeNeed::StateNotInGroup {
                        dom_num: 0, // set this in domain get_needs
                        act_num: self.num,
                        targ_state: regx.state1.clone() });
            nds.push(SomeNeed::StateNotInGroup {
                        dom_num: 0, // set this in domain get_needs
                        act_num: self.num,
                        targ_state: regx.state2.clone() });
        } // next regx

        nds
    }

    /// When a group is invalidated by a new sample, something was wrong within that group.
    ///
    /// When the group has more than one X-bit positions (the region states are not adjacent),
    /// the number of possible replacement groups will be greatly decreased if the
    /// new sample can be used to find an adjacent, dissimilar pair of squares (an edge)
    /// within the invalidated group.
    pub fn seek_edge_needs1(&self) -> NeedStore {
        //println!("seek_edge_needs1");
        let mut ret_nds = NeedStore::new();

        let mut new_regs = RegionStore::new();

        for regx in self.seek_edge.iter() {
            //print!("seek_edge_needs1: checking reg {} ", &regx);
            // Get the squares represented by the states that form the region
            let sqr1 = self.squares.find(&regx.state1).unwrap();
            let sqr2 = self.squares.find(&regx.state2).unwrap();

            // Check squares that define the region.

            let cnb1 = can_combine(sqr1, sqr2);
            //print!("state 1 {} can combine state 2 {} is {} ", &sqr1.state, &sqr2.state, cnb1); 

            let mut needs_found = false;
            match cnb1 {
                // A group of Pn:One may be invalidated by more samples, leading to
                // the same group at Pn:+
                Truth::T => {
                    //print!(" inactivating {} ", &regx);
                    ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                    needs_found = true;
                }
                Truth::M => {
                    if sqr1.results.pnc == false {
                        //print!("get more samples of square {} ", &sqr1.state);
                        ret_nds.push(SomeNeed::SeekEdge {
                            dom_num: 0, // set this in domain get_needs
                            act_num: self.num,
                            targ_state: sqr1.state.clone(),
                            in_group: regx.clone(),
                        });
                        needs_found = true;
                    } else if sqr2.results.pnc == false {
                        //print!("get more samples of square {} ", &sqr2.state);
                        ret_nds.push(SomeNeed::SeekEdge {
                            dom_num: 0, // set this in domain get_needs
                            act_num: self.num,
                            targ_state: sqr2.state.clone(),
                            in_group: regx.clone(),
                        });
                        needs_found = true;
                    } else {
                        panic!(
                            "sqrpn {} {} == sqr pn {} {}",
                            sqr1.state,
                            sqr1.results.pn,
                            sqr2.state,
                            sqr2.results.pn
                        );
                    }
                }
                Truth::F => {
                    if sqr1.is_adjacent(&sqr2) {
                        //print!(" square {} is adjacent to {}, inactivating seek edge {} ", &sqr1.state, &sqr2.state, &regx);
                        ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                        needs_found = true;
                    } else {  // seek_edge_need2 will look for states between
                        //print!(" sqrs are not adjacent, needs2 will handle ");
                    }
                }
            } // end match cnb1

            if needs_found {
                continue;
            }

            let stas_in = self.squares.stas_in_reg(&regx);

            if stas_in.len() == 2 {
                continue;
            }

            // Check for squares in the region that are dissimilar to the region defining squares
            let mut fs_found = false;
            for stax in stas_in.iter() {

                if *stax == regx.state1 || *stax == regx.state2 {
                    continue;
                }

                let sqrx = self.squares.find(&stax).unwrap();

                match can_combine(sqrx, sqr1) {
                    Truth::F => {
                        fs_found = true;
                        //  println!(
                        //      "sqr {} is not combinable with {}",
                        //      &stax, &sqry
                        //  );

                        new_regs.push_nosups(SomeRegion::new(&stax, &sqr1.state));
                    }
                    _ => {}
                }
                match can_combine(sqrx, sqr2) {
                    Truth::F => {
                        fs_found = true;
                        //  println!(
                        //      "sqr {} is not combinable with {}",
                        //      &stax, &sqry
                        //  );

                        new_regs.push_nosups(SomeRegion::new(&stax, &sqr2.state));
                    }
                    _ => {}
                }
            } // next stax

            if fs_found {
                continue;
            }

            // Check for squares in the region that need more samples
            for stax in stas_in.iter() {
                if *stax == regx.state1 || *stax == regx.state2 {
                    continue;
                }
                let sqrx = self.squares.find(&stax).unwrap();
                match can_combine(sqrx, sqr1) {
                    Truth::M => {
                        //  println!(
                        //      "sqr {} is not combinable with {}",
                        //      &stax, &sqry
                        //  );

                        if sqrx.results.pnc {
                            if sqr1.results.pnc {
                                panic!("sqrx {} sqr1 {} both pnc?", &sqrx.state, &sqr1.state);
                            } else {
                                ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: sqr1.state.clone(),
                                    in_group: regx.clone(),
                                });
                            }
                        } else {
                            //print!("get more samples of square {} ", &stax);
                            ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: stax.clone(),
                                    in_group: regx.clone(),
                                });
                        }
                    }
                    _ => {}
                }
                match can_combine(sqrx, sqr2) {
                    Truth::M => {
                        //  println!(
                        //      "sqr {} is not combinable with {}",
                        //      &stax, &sqry
                        //  );

                        if sqrx.results.pnc {
                            if sqr2.results.pnc {
                                panic!("sqrx {} sqr2 {} both pnc?", &sqrx.state, &sqr2.state);
                            } else {
                                ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: sqr2.state.clone(),
                                    in_group: regx.clone(),
                                });
                            }
                        } else {
                            //print!("get more samples of square {} ", &stax);
                            ret_nds.push(SomeNeed::SeekEdge {
                                    dom_num: 0, // set this in domain get_needs
                                    act_num: self.num,
                                    targ_state: stax.clone(),
                                    in_group: regx.clone(),
                                });
                        }
                    }
                    _ => {}
                }
            } // next stax
            println!(" ");
        } // next regx

        // Apply new seek edge regions
        // After get_needs does the housekeeping, it will run this again
        if new_regs.len() > 0 {
            ret_nds = NeedStore::new();
            for regx in new_regs.iter() {
                ret_nds.push(SomeNeed::AddSeekEdge { reg: regx.clone() });
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
            //print!("seek_edge_needs2: checking reg {} ", &regx);

            if regx.state1.is_adjacent(&regx.state2) {
                panic!(
                    "region states {} {} are adjacent",
                    regx.state1, regx.state2
                );
            }

            if regx.state1 == regx.state2 {
                panic!("region states {} {} are equal ", regx.state1, regx.state2);
            }

            let stas_in = self.squares.stas_in_reg(&regx);

            //println!("seek edge in {} stas {}", &regx, &stas_in);

            if stas_in.len() != 2 {
                //println!(" stas in = 2");
                continue;
            }
            //print!(" stas in = {} ", stas_in.len());

            // No square between region.state1 and region.state2, seek one

            // Select a random set of single-bit masks, up to one-half of the number of differences.
            // So if the region states are 10 or 11 bits different, a state 5 bits different from
            // one of the two states will be sought.  So the number of bit differences should go down
            // 50% on each cycle.
            let dif_msk = regx.x_mask().half_mask();

            // Randomly choose which state to use to calculate the target state from
            let mut statex = regx.state2.clone();
            let choice = rand::thread_rng().gen_range(0, 2);
            if choice == 0 {
                statex = regx.state1.clone();
            }

            // Calculate the target inbetween
            let seek_state = statex.s_xor(&dif_msk.to_state());

            // Make need for seek_state
            //print!("get first sample of square {}", &seek_state);
            ret_nds.push(SomeNeed::SeekEdge {
                dom_num: 0, // set this in domain get_needs
                act_num: self.num,
                targ_state: seek_state,
                in_group: regx.clone(),
            });
            println!(" ");
        } // next regx

        ret_nds
    } // end seek_edge_needs2

    /// Get additional sample needs for the states that form a group.
    /// Should only affect groups with Pn::One
    pub fn additional_group_state_samples(&self) -> NeedStore {
        //println!("additional_group_state_sample");
        let mut ret_nds = NeedStore::new();

        for grpx in self.groups.iter() {

            if grpx.pnc {
                continue;
            }

            let sqrx = self.squares.find(&grpx.region.state1).unwrap();

            if sqrx.results.pnc == false {
                ret_nds.push(SomeNeed::StateAdditionalSample {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sqrx.state.clone(),
                    grp_reg: grpx.region.clone(),
                    far: grpx.region.state2.clone(),
                });
            }

            let sqry = self.squares.find(&grpx.region.state2).unwrap();

            if sqry.results.pnc == false {
                ret_nds.push(SomeNeed::StateAdditionalSample {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sqry.state.clone(),
                    grp_reg: grpx.region.clone(),
                    far: grpx.region.state1.clone(),
                });
            }

//            if sqrx.results.pnc && sqry.results.pnc {
//                ret_nds.push(SomeNeed::SetGroupPnc {
//                    group_region: grpx.region.clone(),
//                });
//            }

        } // next grpx

        ret_nds
    } // end additional_group_state_samples

    /// Return expand needs for groups.
    /// Edges of each region under bit positions that can be changed,
    /// from the point of view of the Domain (the x_mask), need to be checked.
    fn expand_needs(&self, agg_chgs: &SomeChange) -> NeedStore {

        //println!("expand_needs");
        let mut ret_nds = NeedStore::new();

        for grpx in self.groups.iter() {
            if grpx.pnc == false && grpx.pn > Pn::One {
                continue;
            }
            let mut nds_tmp = self.expand_needs_group(grpx, agg_chgs);
            ret_nds.append(&mut nds_tmp);
        }

        ret_nds

        // Run a get_needs thread for each action
//        let mut vecx: Vec<NeedStore> = self
//            .groups.avec
//            .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
//            .map(|grpx| self.expand_needs_group(grpx, agg_chgs))
//            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
//        let mut nds_agg = NeedStore::new();

//        for mut nst in vecx.iter_mut() {
//            nds_agg.append(&mut nst);
//        }

//        nds_agg
    }

    /// Return the expand need for a gorup
    pub fn expand_needs_group(&self, grpx: &SomeGroup, agg_chgs: &SomeChange) -> NeedStore {

        let mut ret_nds = NeedStore::new();

        if grpx.limited || grpx.pnc == false {
            return ret_nds;
        }

        // Calc possible change mask for group region non-X bits.
        let chg_mask = grpx.region.ones_mask().m_and(&agg_chgs.b10);
        let chg_mask = chg_mask.m_or(&grpx.region.zeros_mask().m_and(&agg_chgs.b01));

        if chg_mask == grpx.edge_expand {
            return ret_nds;
        }

        let regs_new: RegionStore = self.possible_regions_for_group(&grpx, &chg_mask);
        //println!("test for group {} possible regs: {}", greg, &regs_new);

        if regs_new.len() == 1 && regs_new[0] == grpx.region {
            ret_nds.push(SomeNeed::SetEdgeExpand { group_region: grpx.region.clone(), edge_mask: chg_mask });
            return ret_nds;
        }

        for regx in regs_new.iter() {

            if *regx == grpx.region {
                continue;
            }

            if let Some(gnds) = self.possible_group_needs(regx, 33) {
                return gnds;
            }
        }

        ret_nds
    }

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
    pub fn limit_groups_needs(&self, agg_chgs: &SomeChange) -> NeedStore {
        //println!("limit_groups_needs chg {}", agg_chgs);

        let mut ret_nds = NeedStore::new();

        let regs = self.groups.regions();

        let states1: StateStore = self.squares.states_in_1_region(&regs);

        //println!("Act {}, states in one region {}", self.num, states1);

        // Find squares in one group for each group, that may be an anchor
        for grpx in self.groups.iter() {

            if grpx.pnc == false {
                continue;
            }

            let greg = grpx.region.clone();

            let stsin = greg.states_in(&states1); // The states in greg and no other group

            //println!("Act {} Group {} States {}", self.num, greg, stsin);

            // If no states only in a group region, see if there is any non-overlapped part.
            if stsin.len() == 0 {
                // Get a copy of the regions without the target region.
                let mut all_regs = regs.clone();
                all_regs.remove_region(&greg);

                // Start a RegionStore conainting the target region.
                let mut regs_left = RegionStore::new();
                regs_left.push(greg.clone());

                // Subtract overlapping regions.
                regs_left = regs_left.subtract(&all_regs);

                // Generate needs for any non-overlapped part.
                for regx in regs_left.iter() {
                    let ndx = SomeNeed::SampleRegion {
                                    dom_num: 0, // will be filled later
                                    act_num: self.num,
                                    goal_reg: regx.clone(),
                                    };
                    //println!("****Need: {}", &ndx);
                    ret_nds.push(ndx);
                }
                continue;
            }

            // Check if a current anchor is still in only one region.
            if let Some(stax) = &grpx.anchor {
                if stsin.contains(&stax) == false {
                    ret_nds.push(SomeNeed::RemoveGroupAnchor {
                        group_region: grpx.region.clone(),
                        });
                    //grpx.set_anchor_off();
                }
            }

            // Get mask of edge bits to use to limit.
            let mut edge_limit = grpx.region.ones_mask().m_and(&agg_chgs.b10);
            edge_limit = edge_limit.m_or(&grpx.region.zeros_mask().m_and(&agg_chgs.b01));
            //println!("edge limit: {}", edge_limit);

            // Get the single-bit masks of edges in the group region.
            let edge_msks = edge_limit.split();

            // For each state, sta1, only in the group region, greg:
            //
            //  Calculate each state, sta_adj, adjacent to sta1, outside of greg.
            //
            //  Calculate a rate for each sta1 option, based on the number of adjacent states
            //  in only one group.
            let mut max_rate = 0;

            // Create a StateStore composed of anchor, far, and adjacent-external states.
            let mut cfmv_max = Vec::<StateStore>::new();

            for sta1 in stsin.iter() {
                let mut cfmx = StateStore::new();

                cfmx.push(sta1.clone());

                let mut sta_rate = 1;

                // Rate anchor
                if let Some(sqrx) = self.squares.find(sta1) {
                    if sqrx.results.pnc {
                        sta_rate += 5;
                    } else {
                        sta_rate += sqrx.len_results();
                    }
                }

                cfmx.push(greg.far_state(&sta1));

                // Rate far state
                if let Some(sqrx) = self.squares.find(&cfmx[1]) {
                    if sqrx.results.pnc {
                        sta_rate += 4; // LT anchor value, so (pnc anchor, non-pnc far) NE (non-pnc anchor, pnc far)
                    } else {
                        sta_rate += sqrx.len_results();
                    }
                }

                // Rate adjacent external states
                for edge_bit in edge_msks.iter() {
                    let sta_adj = sta1.s_xor(&edge_bit.to_state());
                    //println!(
                    //    "checking {} adjacent to {} external to {}",
                    //    &sta_adj, &sta1, &greg
                    //);

                    if states1.contains(&sta_adj) {
                        //println!("{} is in only one group", &sta_adj);
                        sta_rate += 100;
                    }

                    cfmx.push(sta_adj.clone());

                    if let Some(sqrx) = self.squares.find(&sta_adj) {
                        if sqrx.results.pnc {
                            sta_rate += 5;
                        } else {
                            sta_rate += sqrx.len_results();
                        }
                    } else {
                        if regs.state_in_1_region(&sta_adj) {
                            sta_rate += 90;
                        }
                    }
                } // next sta1

                //println!("group {} anchor {} rating {}", &greg, &cfmx[0], cnt);

                // Accumulate highest rated anchors
                if sta_rate > max_rate {
                    max_rate = sta_rate;
                    cfmv_max = Vec::<StateStore>::new();
                }
                //println!("rate {} is {}", cfmx[0], sta_rate);
                if sta_rate == max_rate {
                    cfmv_max.push(cfmx);
                }
            } // next sta1

            // Check if a limited group anchor is still rated the best
            // If so, skip further processing.
            if grpx.limited {
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
                        //     "group {} anchor {} rating limited at {}",
                        //     &greg, &anchor, max_num
                        //  );
                        continue;
                    } else {
                        ret_nds.push(SomeNeed::RemoveGroupAnchor {
                            group_region: grpx.region.clone(),
                            });
                        //grpx.set_anchor_off();
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
            // Else limit the group.
            let anchor_sta = &cfm_max[0];

            let anchor_sqr = self.squares.find(anchor_sta).unwrap();
            if anchor_sqr.results.pnc {
                // println!("group {} anchor {} pnc", &greg, &anchor_sta);
            } else {
                // Get additional samples of the anchor
                ret_nds.push(SomeNeed::LimitGroup {
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

            //print!("cfm_mask ");
            //for stax in cfm_max.iter() {
            //    print!(" {}", &stax);
            //}
            //println!(" ");

            for inx in 2..cfm_max.len() {
                //println!("cfm_max num {} ", inx);
                
                let adj_sta = &cfm_max[inx];

                //println!("*** for group {} checking adj sqr {}", &greg, &adj_sta);

                if let Some(adj_sqr) = self.squares.find(adj_sta) {
                    if adj_sqr.results.pnc {

                        if can_combine(anchor_sqr, adj_sqr) == Truth::T {
                            nds_grp_add.push(SomeNeed::AddGroup {
                                group_region: SomeRegion::new(&anchor_sta, &adj_sta),
                            });
                        }
                    } else {
                        nds_grp.push(SomeNeed::LimitGroup {
                            dom_num: 0, // will be set in domain code
                            act_num: self.num,
                            anchor: anchor_sta.clone(),
                            targ_state: adj_sta.clone(),
                            for_group: greg.clone(),
                        });
                    }
                } else {
                    nds_grp.push(SomeNeed::LimitGroup {
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
                if sqrf.results.pnc {
                    // Set the group limited
                    ret_nds.push(SomeNeed::SetGroupLimited {
                        group_region: greg.clone(),
                        cstate: anchor_sta.clone(),
                    });
                } else {
                    // Get additional samples of the far state
                    ret_nds.push(SomeNeed::LimitGroup {
                        dom_num: 0, // will be set in domain code
                        act_num: self.num,
                        anchor: anchor_sta.clone(),
                        targ_state: cfm_max[1].clone(),
                        for_group: greg.clone(),
                    });
                }
            } else {
                // Get the first sample of the far state
                ret_nds.push(SomeNeed::LimitGroup {
                    dom_num: 0, // will be set in domain code
                    act_num: self.num,
                    anchor: anchor_sta.clone(),
                    targ_state: cfm_max[1].clone(),
                    for_group: greg.clone(),
                });
            }
        } // next grpx
        //println!("limit_group_needs: returning {}", &ret_nds);
        ret_nds
    } // end limit_group_needs

    /// Check needs for adjacent and intersecting groups.
    pub fn group_pair_needs(&self) -> NeedStore {
        //println!("group_pair_needs");
        let mut nds = NeedStore::new();

        if self.groups.len() < 2 {
            return nds;
        }

        // Check every pair of groups
        for inx in 0..(self.groups.len() - 1) {

            let grpx = &self.groups[inx];

            // Pair grpx with every group after it in the GroupStore
            for iny in (inx + 1)..self.groups.len() {

                let grpy = &self.groups[iny];

                if grpx.region.intersects(&grpy.region) {
                    let mut ndx = self.group_pair_intersection_needs(&grpx, &grpy);
                    if ndx.len() > 0 {
                        nds.append(&mut ndx);
                    }
                } else if grpx.region.is_adjacent(&grpy.region) {
                    if grpx.pn == grpy.pn {
                        let mut ndx = self.group_pair_adjacent_needs(&grpx, &grpy);
                        if ndx.len() > 0 {
                            nds.append(&mut ndx);
                        }
                    } // end if pn ==
                } // end if regions adjacent
            } // next iny
        } // next inx

        nds
    } // end group_pair_needs

    /// Given a calculated region return a region based on at least one sampled state.
    /// The result region could use a pair of the states, or a state and a calculated far state.
    /// Pnc-releated validation of the region is done.
    fn best_states_to_define_region(&self, reg_grp: &SomeRegion) -> Option<SomeRegion> {

        // Check for edge cases, no choice in states.
        if reg_grp.num_x() < 2 {
            return Some(reg_grp.clone());
        }

        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(reg_grp);

        // If no squares, return clone, your guess is as good as mine.
        if sqrs_in_reg.len() == 0 {
            return Some(reg_grp.clone());
        }

        // Still little choice...
        if sqrs_in_reg.len() == 1 {
            return Some(SomeRegion::new(&sqrs_in_reg[0].state, &reg_grp.far_state(&sqrs_in_reg[0].state)));
        }

        // More than one square found in the region ...

        // Get the maximum Pn value.
        let mut max_pn = Pn::One;
        for sqrx in sqrs_in_reg.iter() {
            if sqrx.results.pn > max_pn {
                max_pn = sqrx.results.pn;
            }
        }

        // Check for pnc problem
        for sqrx in sqrs_in_reg.iter() {
            if sqrx.results.pn < max_pn {
                if sqrx.results.pnc {
                    return None;
                }
            }
        }

        // Get min number samples to get to pnc for max_pn.
        let pnc_min = max_pn.num_samples_needed();

        // Get pairs of square-pn states and far states.
        // Avoid duplicate pairs, where the current square is a far state
        // for a previous square.
        let mut far_stas = Vec::<SomeState>::new();

        // Pairs where the first item 0 has at least one sample, item 1 may not have been sampled yet.
        // Item 2 is the pnc number sample deficit, zero is the best, using two squares that have acheived the max_pnc.
        let mut pairs = Vec::<(SomeState, SomeState, usize)>::new();

        for sqrx in sqrs_in_reg.iter() {

            // Skip square that is a far square to a square already processed.
            if far_stas.contains(&sqrx.state) {
                continue;
            }

            let mut pnc_deficit = 0;
            if sqrx.results.pnc {
            } else {
                pnc_deficit = pnc_min - sqrx.len_results();
            }

            // Calc far state.
            let stay = reg_grp.far_state(&sqrx.state);

            // Increment pnc_deficit.
            if let Some(sqry) = self.squares.find(&stay) {
                if sqry.results.pnc {
                } else {
                    pnc_deficit = pnc_min - sqry.len_results();
                }
            } else {
                pnc_deficit += pnc_min;
            }

            // Save pair, far state.
            pairs.push((sqrx.state.clone(), stay.clone(), pnc_deficit));
            far_stas.push(stay);
        }

        // Find lowest pnc_deficit, and indexs to those pairs.
        let mut min_def = pnc_min * 2;
        let mut pair_min_inxs = Vec::<usize>::new();
        //let mut num_new_states = 0;

        let mut inx = 0;
        for pairx in pairs.iter() {
            if pairx.2 < min_def {
                min_def = pairx.2;
                pair_min_inxs = Vec::<usize>::new();
                //num_new_states = 0;
            }
            if pairx.2 == min_def {
                pair_min_inxs.push(inx);
                if let Some(_) = self.squares.find(&pairx.1) {
                } else {
                    //num_new_states += 1;
                }
            }
            inx += 1;
        }
        //println!("AddGroup2 mindef {} number pairs {} num new second states {}", &min_def, &pair_min_inxs.len(), &num_new_states);

        // Return region
        let mut inx = 0;
        if pair_min_inxs.len() > 1 {
            inx = rand::thread_rng().gen_range(0, pair_min_inxs.len());
        }
        let pair = &pairs[pair_min_inxs[inx]];
        Some(SomeRegion::new(&pair.0, &pair.1))
    }

    /// Return None to indicate an invalid region,
    /// a NeedStore of AddGroup2 needs,
    /// an empty NeedStore to indicate a valid region.
    fn possible_group_needs(&self, reg_grp: &SomeRegion, _from: usize) -> Option<NeedStore> {
        //  println!("possible_group_needs for {}", &reg_grp);

        // Init return store.
        let mut ret_nds = NeedStore::new();

        // Check if only one state can fit in the region.
        if reg_grp.num_x() == 0 {
            if let Some(sqrx) = self.squares.find(&reg_grp.state1) {
                if sqrx.results.pnc {
                    ret_nds.push(
                        SomeNeed::AddGroup {
                            group_region: reg_grp.clone(),
                        }
                    );
                    return Some(ret_nds);
                }
            }
            ret_nds.push(
                SomeNeed::AStateMakeGroup {
                    dom_num: 0,
                    act_num: self.num,
                    targ_state: reg_grp.state1.clone(),
                    for_reg: reg_grp.clone(),
                    far: reg_grp.state1.clone(),
                    num_x: 0,
                }
            );
            return Some(ret_nds);
        }

        // So, at least one X in region.

        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(reg_grp);

        // If no squares, return needs to sample states.
        if sqrs_in_reg.len() == 0 {
            ret_nds.push(
                    SomeNeed::AStateMakeGroup {
                        dom_num: 0, // Will be set later.
                        act_num: self.num,
                        targ_state: reg_grp.state1.clone(),
                        for_reg: reg_grp.clone(),
                        far: reg_grp.state2.clone(),
                        num_x: reg_grp.num_x(),
                    }
                );
            ret_nds.push(
                    SomeNeed::AStateMakeGroup {
                        dom_num: 0, // Will be set later.
                        act_num: self.num,
                        targ_state: reg_grp.state2.clone(),
                        for_reg: reg_grp.clone(),
                        far: reg_grp.state1.clone(),
                        num_x: reg_grp.num_x(),
                    }
                );
            return Some(ret_nds);
        }

        // If one square in region, return need to sample the far square.
        // No need to check for more samples needed of the existing square,
        // since the first sample of the far square might be incompatible.
        if sqrs_in_reg.len() == 1 {
            ret_nds.push(
                SomeNeed::AStateMakeGroup {
                    dom_num: 0, // Will be set later.
                    act_num: self.num,
                    targ_state: reg_grp.far_state(&sqrs_in_reg[0].state),
                    for_reg: reg_grp.clone(),
                    far: sqrs_in_reg[0].state.clone(),
                    num_x: reg_grp.num_x(),
                }
            );
            return Some(ret_nds);
        }

        // So, number of squares in the region is GT 1

        // Get max pn value.
        let mut max_pn = sqrs_in_reg[0].results.pn;
        let mut max_pn_num = 0;

        for sqrx in sqrs_in_reg.iter() {
            if sqrx.results.pn > max_pn {
                max_pn = sqrx.results.pn;
                max_pn_num = 1;
            } else if sqrx.results.pn == max_pn {
                max_pn_num += 1;
            }
        } // next stax

        // Get list of max_pn squares, non-max_pn squares.
        let mut pn_sqrs = Vec::<&SomeSquare>::with_capacity(max_pn_num);
        let mut pn_not_sqrs = Vec::<&SomeSquare>::with_capacity(sqrs_in_reg.len() - max_pn_num + 1);
        for sqrx in sqrs_in_reg.iter() {
            if sqrx.results.pn == max_pn {
                pn_sqrs.push(sqrx);
            } else {
                pn_not_sqrs.push(sqrx);
            }
        }

        // Check for dissimilar squares
        for sqrx in pn_not_sqrs.iter() {
            if sqrx.results.pnc {
                return None;
            }
        } // next sqrx

        // Set representative max square.
        let max_sqr = pn_sqrs[0];

        // Init aggregate region for pn squares.
        let mut reg_max = SomeRegion::new(&max_sqr.state, &max_sqr.state);

        for sqrx in pn_sqrs.iter() {
            if reg_max.is_superset_of_state(&sqrx.state) {
            } else {
                reg_max = reg_max.union_state(&sqrx.state);
            }
        } // next sqrx

        // Check if there is enough pn == max_pn squares to fill the region.
        if reg_max != *reg_grp {
            ret_nds.push(
                SomeNeed::SampleRegion {
                    dom_num: 0, // Will be set later.
                    act_num: self.num,
                    goal_reg: reg_grp.far_reg(&reg_max),
                }
            );
            return Some(ret_nds);
        }

        // max_pn squares fill the region

        if max_pn == Pn::Unpredictable {

            //let mut strcc = format!("Act {} AddGroup2 A {} using sqrs ", &self.num, &reg_grp);
            //for sqrx in pn_sqrs.iter() {
           //     strcc.push_str(&format!(" {}", sqrx.state));
            //}
           // println!("{}", strcc);

            let mut sta1_found = false;
            let mut sta1_pnc = false;
            let mut sta2_found = false;
            let mut sta2_pnc = false;
            if let Some(regx) = self.best_states_to_define_region(reg_grp) {
                //println!("AddGroup2 A use pn state {} and far {}", &regx.state1, &regx.state2);
                if let Some(sqrx) = self.squares.find(&regx.state1) {
                    sta1_found = true;
                    sta1_pnc = sqrx.results.pnc;
                }
                if let Some(sqrx) = self.squares.find(&regx.state2) {
                    sta2_found = true;
                    sta2_pnc = sqrx.results.pnc;
                }
                if sta1_pnc && sta2_pnc {
                    ret_nds.push(SomeNeed::AddGroup { group_region: regx });
                    return Some(ret_nds);
                }
                if sta2_found == false || sta2_pnc == false {
                    ret_nds.push(
                        SomeNeed::AStateMakeGroup {
                            dom_num: 0, // Will be set later.
                            act_num: self.num,
                            targ_state: regx.state2.clone(),
                            for_reg: regx.clone(),
                            far: regx.state1.clone(),
                            num_x: reg_grp.num_x(),
                        }
                    );
                }
                if sta1_found == false || sta1_pnc == false {
                    ret_nds.push(
                        SomeNeed::AStateMakeGroup {
                            dom_num: 0, // Will be set later.
                            act_num: self.num,
                            targ_state: regx.state1.clone(),
                            for_reg: regx.clone(),
                            far: regx.state2.clone(),
                            num_x: reg_grp.num_x(),
                        }
                    );
                }
            }
            return Some(ret_nds);
        }

        // Get aggregate rules for all squares with == pn
        let mut agg_rules = max_sqr.rules.clone();
        for sqrx in pn_sqrs.iter() {
            if agg_rules.initial_region().is_superset_of_state(&sqrx.state) {
                if sqrx.rules.is_subset_of(&agg_rules) {
                    continue;
                } else {
                    return None;
                }
            }

            if let Some(rules_tmp) = agg_rules.union(&sqrx.rules) {
                agg_rules = rules_tmp;
            } else {
                return None;
            }
        } // next sqrx

        // Check if other rules are compatible
        for sqrx in pn_not_sqrs.iter() {
            if sqrx.rules.is_subset_of(&agg_rules) == false {
                return None;
            }
        } // next sqrx

        // Form group
       // let mut strcc = format!("Act {} AddGroup2 B {} using sqrs ", &self.num, &reg_grp);
        //for sqrx in pn_sqrs.iter() {
        //    strcc.push_str(&format!(" {}", sqrx.state));
       // }
       // println!("{}", strcc);

        let mut sta1_found = false;
        let mut sta1_pnc = false;
        let mut sta2_found = false;
        let mut sta2_pnc = false;
        if let Some(regx) = self.best_states_to_define_region(reg_grp) {
            //println!("AddGroup2 B use pn state {} and far {}", &regx.state1, &regx.state2);
            if let Some(sqrx) = self.squares.find(&regx.state1) {
                sta1_found = true;
                sta1_pnc = sqrx.results.pnc;
            }
            if let Some(sqrx) = self.squares.find(&regx.state2) {
                sta2_found = true;
                sta2_pnc = sqrx.results.pnc;
            }
            if sta1_pnc && sta2_pnc {
                ret_nds.push(SomeNeed::AddGroup { group_region: regx });
                return Some(ret_nds);
            } else if sta2_found == false || sta2_pnc == false {
                ret_nds.push(
                    SomeNeed::AStateMakeGroup {
                        dom_num: 0, // Will be set later.
                        act_num: self.num,
                        targ_state: regx.state2.clone(),
                        for_reg: regx.clone(),
                        far: regx.state1.clone(),
                        num_x: reg_grp.num_x(),
                    }
                );
            } else if sta1_found == false || sta1_pnc == false {
                ret_nds.push(
                    SomeNeed::AStateMakeGroup {
                        dom_num: 0, // Will be set later.
                        act_num: self.num,
                        targ_state: regx.state1.clone(),
                        for_reg: regx.clone(),
                        far: regx.state2.clone(),
                        num_x: reg_grp.num_x(),
                    }
                );
            }
        }

        return Some(ret_nds);
    } // end possible_group_needs

    /// Check two intersecting groups for needs.
    /// Possibly combining to groups.
    /// Possibly checking for a contradictatory intersection.
    fn group_pair_intersection_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
//        println!(
//            "groups_pair_intersection_needs {} {} and {} {}",
//            &grpx.region, &grpx.pn, &grpy.region, grpy.pn
//        );

        let mut nds = NeedStore::new();

        let reg_both = grpx.region.union(&grpy.region);

        if grpx.pn == Pn::Unpredictable && grpy.pn == Pn::Unpredictable {
            if let Some(nds) = self.possible_group_needs(&reg_both, 2) {
                return nds;
            }
            return NeedStore::new();
        }

        let reg_int = grpx.region.intersection(&grpy.region);

        if grpx.pn != grpy.pn {
            //println!("pn != !");
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

                // to avoid subtraction, use the far sub-region
                let regy = reg_int.far_reg(&ok_reg);
                
                //println!("pn2 intersection is {} far reg is {}", rulsxy.formatted_string(), &regy);

                nds.push(self.cont_int_region_needs(&regy, &grpx, &grpy));
            } else {
                //println!("pn2 whole intersection is bad");
                nds.push(self.cont_int_region_needs(&reg_int, &grpx, &grpy));
            }

            return nds;
        } else {
            if let Some(nds) = self.possible_group_needs(&reg_both, 2) {
                return nds;
            }
            return NeedStore::new();
        }
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

        // An overlapping pert will not affect the choice of a square
        // only in one region,for limiting the groups.
        if grpx.limited && grpy.limited {
            return nds;
        }
//        if grpx.region.x_mask() == grpy.region.x_mask() {
//            return nds;
//        }

        let regz = grpx.region.overlapping_part(&grpy.region);
        //println!("overlapping part = {}", &regz);
        if self.groups.any_superset_of(&regz) {
            return nds;
        }

        if grpx.pn == Pn::Unpredictable {
            //println!("group_pair_adjacent_needs: {} and {}", &grpx.region, &grpy.region);
            if let Some(nds) = self.possible_group_needs(&regz, 5) {
                return nds;
            }
            return NeedStore::new();
        }

        if let Some(rulsxy) = grpx.rules.restrict_initial_region(&regz).union_subset(&grpy.rules.restrict_initial_region(&regz)) {
            //println!("group_pair_adjacent_needs: {} and {} rules {}", &grpx.region, &grpy.region, &rulesxy);
            if let Some(nds) = self.possible_group_needs(&rulsxy.initial_region(), 6) {
                return nds;
            }
        } else {
            //println!("rules union failed");
        }

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
        //println!("cont_int_region_needs {} for grp {} and grp {} ", &regx, &grpx.region, &grpy.region);
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
            if sqrz.results.pnc {
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
    /// For each rule, prune the rule X bit positions to favor desired changes.
    ///
    /// For a two-result group, see if there is an existing square that is expected to
    /// produce the desired change.
    pub fn get_steps(&self, achange: &SomeChange) -> StepStore {
        let mut stps = StepStore::new();

        for grpx in self.groups.iter() {

            match grpx.pn {
                Pn::One => {
                    // Find bit changes that are desired
                    if let Some(rulx) = grpx.rules[0].parse_for_changes(&achange)
                    {
                        stps.push(SomeStep::new(self.num, rulx, false, grpx.region.clone()));
                    }
                }
                Pn::Two => {
                    for ruly in grpx.rules.iter() {
                        if let Some(rulx) = ruly.parse_for_changes(&achange) {
                            // See if an existing square is ready to produce the desired result
                            let i_reg = rulx.initial_region();
                            let stas = self.squares.stas_in_reg(&i_reg);

                            let mut found = false;
                            for stax in stas.iter() {
                                let sqrx = self.squares.find(stax).unwrap();

                                // Will include at least one bit change desired, but maybe others.
                                let expected_result = rulx.result_from_initial_state(stax);

                                // If a Pn::Two squares last result is not equal to what is wanted,
                                // the next result should be.
                                if sqrx.most_recent_result() != &expected_result {
                                    let stpx = SomeStep::new(
                                        self.num,
                                        rulx.restrict_initial_region(&SomeRegion::new(stax, stax)),
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

        // println!("Steps: {}", &stps);
        stps
    } // end get_steps

    /// Get possible steps that can be used to make a given change.
    ///
    /// For each rule, prune the rule X bit positions to favor desired changes.
    ///
    /// For a two-result group, see if there is an existing square that is expected to
    /// produce the desired change.
    pub fn get_steps_exact(&self, achange: &SomeChange) -> StepStore {
        let mut stps = StepStore::new();

        for grpx in self.groups.iter() {

            match grpx.pn {
                Pn::One => {
                    // Find bit changes that are desired
                    if let Some(rulx) = grpx.rules[0].parse_for_changes(achange)
                    {
                        if rulx.change() == *achange {
                            stps.push(SomeStep::new(self.num, rulx, false, grpx.region.clone()));
                        }
                    }
                }
                Pn::Two => {
                    for ruly in grpx.rules.iter() {
                        if let Some(rulx) = ruly.parse_for_changes(achange) {
                            
                            if rulx.change() == *achange {
                                // See if an existing square is ready to produce the desired result
                                let i_reg = rulx.initial_region();
                                let stas = self.squares.stas_in_reg(&i_reg);

                                let mut found = false;
                                for stax in stas.iter() {
                                    let sqrx = self.squares.find(stax).unwrap();

                                    // Will include at least one bit change desired, but maybe others.
                                    let expected_result = rulx.result_from_initial_state(stax);

                                    // If a Pn::Two squares last result is not equal to what is wanted,
                                    // the next result should be.
                                    if sqrx.most_recent_result() != &expected_result {
                                        let stpx = SomeStep::new(
                                            self.num,
                                            rulx.restrict_initial_region(&SomeRegion::new(stax, stax)),
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
                        } // endif == achange
                    } // next ruly
                } // end match Two
                Pn::Unpredictable => {}
            } // end match grpx.pn
        } // next grpx

        // println!("Steps: {}", &stps);
        stps
    } // end get_steps_exact

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

            if sqrx.results.pn != sqry.results.pn {
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

            if self.can_combine(&sqrx, &sqry) == Truth::T {
                rsx.push_nosubs(regx);
            }
        } // end for

        // Print possible regions
        for regx in rsx.iter() {

            let sqry = self.squares.find(&regx.state2).unwrap();
            if sqry.results.pn == Pn::Unpredictable {
                println!(
                    "\n  Square {} [Unpredictable] can combine with\n  Square {} [Unpredictable]\n  giving {} [Unpredictable]",
                    sqrx.state, sqry.state, regx
                );
            } else {
                println!(
                    "\n  Square {} {} can combine with\n  Square {} {}\n  giving {} {}",
                    sqrx.state,
                    sqrx.rules,
                    sqry.state,
                    sqry.rules,
                    regx,
                    &sqrx.rules.union(&sqry.rules).unwrap()
                );
            }
        }

        //println!("regions for new groups {}", &rsx);
        rsx
    } // end possible_regions_from_square

    /// Given a group, calculate the possible regions it may expand to by applying 
    /// dissimilar, non-intersecting squares, within the limits of the changes roughly allowed
    /// by the current rules, given by a mask.
    pub fn possible_regions_for_group(&self, grpx: &SomeGroup, chg_mask: &SomeMask) -> RegionStore {
        //println!("possible_regions_for_group {}", &grpx.region);
        let mut regs = RegionStore::new();

        // Check if any changes possible
        if chg_mask.is_subset_of(&grpx.region.x_mask()) {
            regs.push(grpx.region.clone());
            return regs;
        }

        // Get masks for closest dissimilar squares.
        let mut close_dis = MaskStore::new(Vec::<SomeMask>::new());
        for (key, sqry) in &self.squares.ahash {

            let diff_mask = grpx.region.diff_mask_state(&key);

            // Skip squares in the group
            if diff_mask.is_low() {
                continue;
            }

            let trh = can_combine(grpx, sqry);
            if trh ==  Truth::F {
                close_dis.push_nosups(diff_mask);
            }
        }

        // Add in closer squares that fail combination due to intervening squares
        for (key, sqry) in &self.squares.ahash {

            let diff_mask = grpx.region.diff_mask_state(&key);

            // Skip squares in the group
            if diff_mask.is_low() {
                continue;
            }

            // Skip if any close dissimilar square is between
            if close_dis.any_subset(&diff_mask) {
                continue;
            }
                
            let trh = can_combine_check_between(grpx, sqry, &self.squares);
            if trh ==  Truth::F {
                close_dis.push_nosups(diff_mask);
            }
        }

        // No dissimilar squares, return the maximum possible expansion.
        if close_dis.len() == 0 {
            regs.push(grpx.region.set_to_x(&chg_mask));
            return regs;
        }

        //println!("close_dis {}", &close_dis);

        // Get farthest similar squares.
        let mut far_sim = MaskStore::new(Vec::<SomeMask>::new());

        for (key, _sqry) in &self.squares.ahash {

            let diff_mask = grpx.region.diff_mask_state(&key);

            // Skip squares in the group
            if diff_mask.is_low() {
                continue;
            }

            // Skip if any close dissimilar square is between
            if close_dis.any_subset(&diff_mask) {
                continue;
            }

            let diff_mask = diff_mask.m_and(&chg_mask);
            if diff_mask.is_not_low() {
                far_sim.push_nosubs(diff_mask);
            }

        } // next key, sqry

        //println!("far_sim {}", &far_sim);

        // Return expanded regions, if any, limited by the given change mask.
        if far_sim.len() > 0 {
            for mskx in far_sim.iter() {
                regs.push(grpx.region.set_to_x(mskx));
            }
            return regs;
        }

        // No useful far similar squares, some close dissimilar squares.
        // Seek samples between those that differ by GT 1 bit.
        let mut aggr_dis = SomeMask::new_low(self.num_ints());
        for cls_dis in close_dis.iter() { 
            if cls_dis.just_one_bit() {
                aggr_dis = aggr_dis.m_or(cls_dis);
                continue;
            }

            // cls_dis GT 1 bit diff, extend grpx region half as far.
            let hlf_msk = cls_dis.half_mask();
            regs.push(grpx.region.set_to_x(&hlf_msk));
        }

        // Check if at least one close dissimilar square is not adjacent.
        if regs.len() > 0 {
            return regs;
        }

        // Maybe not all edges are accounted for by adjacent dissimilar squares.
        let possible_new_x = chg_mask.m_and(&aggr_dis.m_not());
        let net_x = possible_new_x.m_and(&grpx.region.x_mask().m_not());
        
        if net_x.is_low() {
            regs.push(grpx.region.clone());
        } else if net_x.just_one_bit() {
            regs.push(grpx.region.set_to_x(&net_x));
        } else {
            regs.push(grpx.region.set_to_x(&net_x.half_mask()));
        }

        regs
    } // end possible_regions_for_group

    /// Find and print verticies
    pub fn vertices(&self) {

        let mut lefts = Vec::<RegionStore>::with_capacity(self.groups.len());

        for grpx in self.groups.iter() {
            let mut left = RegionStore::new();
            left.push(grpx.region.clone());
            for grpy in self.groups.iter() {
                if grpy.region != grpx.region {
                    left = left.subtract_region(&grpy.region);
                }
            }
            //println!("grp {} left {}", &grpx.region, &left);
            lefts.push(left);
        }

        for inx in 0..lefts.len() {

            for regx in lefts[inx].iter() {
                let mut ovlp1 = RegionStore::new();

                for iny in 0..lefts.len() {
                    if inx == iny {
                        continue;
                    }

                    for regy in lefts[iny].iter() {
                        if regy.is_adjacent(regx) {
                            let regovp = regx.overlapping_part(regy);
                            //print!(" {} adj {} ovlp {}", regx, regy, regx.overlapping_part(regy));
                            ovlp1.push(regovp);
                        }
                    } // next regy

                    
                } // next iny
                if ovlp1.len() > 0 {
                    print!("grp {} in 1 {} ovlps [", self.groups[inx].region, regx);
                    for tx in ovlp1.iter() {
                        print!("{} ", tx);
                    }
                    println!("]");
                }
            } // next regx
            //println!(" ");
        } // next inx
    } // end vertices

    /// Find left-over regions not currently coverd
    /// by groups.
    pub fn left_overs(&self) -> RegionStore {
        if self.groups.len() < 2 {
            return RegionStore::new();
        }

        let tot_reg = self.groups.regions().union().unwrap();

        let mut left = RegionStore::new();
        left.push(tot_reg);

        for grpx in self.groups.iter() {
            left = left.subtract_region(&grpx.region);
        }

        //println!("left-overs: {}", &left);
        left
    }

    /// Take an action with the current state.
    pub fn take_action(&mut self, dom: usize, cur_state: &SomeState) -> SomeState {

        let asquare = self.squares.find(cur_state);

        let astate = take_action(dom, self.num, cur_state, asquare);

        astate
    }

} // end impl SomeAction


