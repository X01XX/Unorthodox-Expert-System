//! The SomeAction struct.  It stores, and analyzes, the results of an action.
//!
//! This stores initial->result samples, generates needs for more samples, and
//! represents the current best-guess rules of the expected responses
//! of executing an action for a given state.

use crate::actioninterface::ActionInterface;
use crate::change::SomeChange;
use crate::group::SomeGroup;
use crate::groupstore::GroupStore;
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

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("A(ID: ");

        rc_str += &self.num.to_string();

        if self.seek_edge.len() > 0 {
            rc_str += &format!(" seek_edge within: {}", self.seek_edge);
        }

        let regs = self.groups.regions();

        let mut fil = String::from(",\n       Grps: ");
        for grpx in self.groups.iter() {
            let stas_in = self.squares.stas_in_reg(&grpx.region);

            // Count the number of states in a group that are also in only one region
            //            let mut cnt = 0;
            //            for stax in stas_in.iter() {
            //                if regs.state_in_1_region(stax) {
            //                    cnt += 1;
            //                }
            //            }

            let cnt: usize = stas_in
                .iter()
                .map(|x| if regs.state_in_1_region(x) { 1 } else { 0 })
                .sum();

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
    /// Interface to an action that does something.
    do_something: ActionInterface,
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
            do_something: ActionInterface::new(),
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

        let cmbx = sqrx.can_combine(&sqry);
        if cmbx != Truth::T {
            return cmbx;
        }

        if self.can_combine_check_between(sqrx, sqry) == false {
            return Truth::F;
        }

        cmbx
    }

    /// Evaluate a sample.
    pub fn eval_sample(&mut self, initial: &SomeState, result: &SomeState, dom: usize) {
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
        // Processing for all samples.
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

                if sqrx.results.pn == Pn::One || sqrx.results.pnc {
                    //println!("AStateMakeGroup: sqr {} sampled, pn {} pnc {}", &sqrx, sqrx.results.pn, sqrx.results.pnc);
                    if let Some(sqry) = self.squares.find(&far) {
                        if sqry.results.pn == Pn::One || sqry.results.pnc {
                            if sqrx.state == sqry.state
                                || self.can_combine(&sqrx, &sqry) == Truth::T
                            {
                                let regz = SomeRegion::new(&sqrx.state, &sqry.state);
                                assert!(self.check_region_for_group(&regz));

                                if sqrx.results.pn == Pn::Unpredictable {
                                    self.groups.push(
                                        SomeGroup::new(
                                            regz,
                                            RuleStore::new(),
                                            sqrx.results.pnc && sqrx.results.pnc,
                                        ),
                                        dom,
                                        self.num,
                                    );
                                } else {
                                    if let Some(rulsxy) = sqrx.rules.union(&sqry.rules) {
                                        self.groups.push(
                                            SomeGroup::new(
                                                regz,
                                                rulsxy,
                                                sqrx.results.pnc && sqry.results.pnc,
                                            ),
                                            dom,
                                            self.num,
                                        );
                                    }
                                } // end if Unpredictable
                            } // end if can combine
                        } // end if sqry.results.pn ==
                    } // end find far
                } // end if sqrx.results.pn ==
            } // end process AStateMakeGroup Need

            SomeNeed::SeekEdge {
                targ_state: sta,
                in_group: greg,
                ..
            } => {
                let mut make_groups_from = Vec::<SomeState>::new();
                if *sta != greg.state1 && *sta != greg.state2 {
                    // Find the squares
                    let sqr1 = self.squares.find(&greg.state1).unwrap();
                    let sqr2 = self.squares.find(&greg.state2).unwrap();
                    let sqr3 = self.squares.find(&sta).unwrap();

                    // Process next sample of square in-between for new square and state1 square.
                    // Should be different from state1 square or state2 square.
                    // It may be different from both state1 and state2.
                    // If it needs more samples, skip, next need will increment the number samples.
                    //println!("can combine 1 greg {} sta {}", greg, sta);

                    if sqr3.can_combine(&sqr1) == Truth::F {
                        if sqr1.is_adjacent(&sqr3) && sqr1.results.pnc && sqr3.results.pnc {
                            println!("Dom {} Act {} new edge found between {} and {} removing seek edge {}", dom, self.num, &sqr1.state, &sqr3.state, &greg);
                            self.seek_edge.remove_region(greg);
                            make_groups_from.push(sqr1.state.clone());
                            make_groups_from.push(sqr3.state.clone());
                        } else {
                            let even_closer_reg = SomeRegion::new(&sqr1.state, &sqr3.state);
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }

                    // Process next sample of square in-between for new square and state2 square
                    // Should be different from state1 square or state2 square
                    //println!("can combine 2");

                    if sqr3.can_combine(&sqr2) == Truth::F {
                        if sqr2.is_adjacent(&sqr3) && sqr2.results.pnc && sqr3.results.pnc {
                            println!("Dom {} Act {} new edge found between {} and {} removing seek edge {}", dom, self.num, &sqr2.state, &sqr3.state, &greg);
                            self.seek_edge.remove_region(greg);
                            make_groups_from.push(sqr2.state.clone());
                            if make_groups_from.contains(&sqr3.state) == false {
                                make_groups_from.push(sqr3.state.clone());
                            }
                        } else {
                            let even_closer_reg = SomeRegion::new(&sqr2.state, &sqr3.state);
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }
                }

                // Create a group for square if needed
                if make_groups_from.len() > 0 {
                    for stax in make_groups_from {
                        self.create_groups_from_square(&stax, dom);
                    }
                    self.aggregate_changes = self.calc_aggregate_changes();
                }
            } // end match SeekEdgeNeed
            _ => (),
        } // end match ndx
    } // end eval_need_sample

    /// Add a sample by user command.
    /// Its best to start a session and proceed with:
    ///   all user-specified samples, or
    ///   no user-specified samples.
    pub fn eval_arbitrary_sample(&mut self, initial: &SomeState, result: &SomeState, dom: usize) {
        //println!(
        //    "take_action_arbitrary for state {} result {}",
        //    initial, result
        //);

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
            }
            return;
        }

        // Get num groups that might be invalidated
        let any_grps_invalidated = self.groups.any_groups_invalidated(cur, new_state);

        // Get num active groups in
        let num_grps_in = self.groups.num_groups_state_in(cur);

        if any_grps_invalidated || num_grps_in == 0 {
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

    /// Check if the states defining a region allow it to be used to define a group.
    fn check_region_for_group(&self, regx: &SomeRegion) -> bool {
        let sqrx = self.squares.find(&regx.state1).unwrap();
        if sqrx.results.pn == Pn::One {
        } else if sqrx.results.pnc {
        } else {
            //println!("square not pn == One or pnc == true {}", sqrx);
            return false;
        }

        let sqry = self.squares.find(&regx.state2).unwrap();
        if sqry.results.pn == Pn::One {
        } else if sqry.results.pnc {
        } else {
            //println!("square not pn == One or pnc == true {}", sqry);
            return false;
        }
        true
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
        let regs_invalid: RegionStore =
            self.groups
                .check_square(&sqrx, dom, self.num, &self.squares);

        // Save regions invalidated to seek new edges.
        for regx in regs_invalid.iter() {
            if regx.x_mask().num_one_bits() > 2 && *key != regx.state1 && *key != regx.state2 {
                if let Some(sqr1) = self.squares.find(&regx.state1) {
                    if let Some(sqr2) = self.squares.find(&regx.state2) {
                        if sqr1.results.pnc && sqr2.results.pnc {
                            if sqrx.state.is_adjacent(&sqr1.state) == false
                                && sqrx.can_combine(&sqr1) == Truth::F
                            {
                                println!(
                                    "\nDom {} Act {} Seek edge between {} and {}",
                                    &dom, &self.num, &sqrx.state, &sqr1.state
                                );
                                self.seek_edge
                                    .push_nosubs(SomeRegion::new(&sqrx.state, &sqr1.state));
                            }
                            if sqrx.state.is_adjacent(&sqr2.state) == false
                                && sqrx.can_combine(&sqr2) == Truth::F
                            {
                                println!(
                                    "\nDom {} Act {} Seek edge between {} and {}",
                                    &dom, &self.num, &sqrx.state, &sqr2.state
                                );
                                self.seek_edge
                                    .push_nosubs(SomeRegion::new(&sqrx.state, &sqr2.state));
                            }
                        }
                    }
                }
            }
        }

        // Create a group for square if needed
        let grps_in = self.groups.groups_state_in(&key);
        if grps_in.len() == 0 || (grps_in.len() == 1 && (grps_in[0].x_mask().is_low())) {
            self.create_groups_from_square(&key, dom);
            self.aggregate_changes = self.calc_aggregate_changes();
            if regs_invalid.len() == 0 {
                return;
            }
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

        //        if num_grps_in > 0 {
        //            return;
        //        }

        //println!("Checking Square {} for new groups", &sqrx.str_terse());

        // Get possible regions, sqrx.state will be <region>.state1
        let rsx: RegionStore = self.possible_regions_from_square(sqrx);

        let mut group_added = false;
        for regx in rsx.iter() {
            if let Some(sqrx) = self.squares.find(&regx.state1) {
                if sqrx.results.pn == Pn::One || sqrx.results.pnc {
                    if let Some(sqry) = self.squares.find(&regx.state2) {
                        if sqry.results.pn == Pn::One || sqry.results.pnc {
                            let mut ruls = RuleStore::new();
                            if sqrx.results.pn != Pn::Unpredictable {
                                ruls = sqrx.rules.union(&sqry.rules).unwrap();
                            }

                            let regy = SomeRegion::new(&sqrx.state, &regx.state2);
                            assert!(self.check_region_for_group(&regy));
                            self.groups.push(
                                SomeGroup::new(regy, ruls, sqrx.results.pnc && sqry.results.pnc),
                                dom,
                                self.num,
                            );
                            group_added = true;
                        }
                    }
                }
            }
        } // next regx

        if group_added || num_grps_in > 0 {
            return;
        }

        // Make a single-square group
        let regz = SomeRegion::new(&sqrx.state, &sqrx.state);
        assert!(self.check_region_for_group(&regz));
        self.groups.push(
            SomeGroup::new(regz, sqrx.rules.clone(), sqrx.results.pnc),
            dom,
            self.num,
        );

        return;
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
                    targ_state: stax.clone(),
                });
            }
        }

        nds
    }

    /// Get needs for an Action, to improve understanding of the result pattern(s).
    /// When most needs are satisfied, needs for group limitation are generated.
    /// If housekeeping needs are generated, they are processed and needs
    /// are checked again.
    pub fn get_needs(
        &mut self,
        cur_state: &SomeState,
        agg_chgs: &SomeChange,
        dom: usize,
    ) -> NeedStore {
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
            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Look for needs to find a sample between in an invalidated group
            let mut ndx = self.seek_edge_needs2();
            if ndx.len() > 0 {
                nds.append(&mut ndx);
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
            let mut ndx = self.limit_groups_needs(agg_chgs);

            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Check for repeating housekeeping needs loop
            if cnt > 20 {
                println!("needs: {}", &nds);
                panic!("Dom {} Act {} loop count GT 20!", dom, self.num);
            }

            // Edit out subset group adds.
            let mut new_grp_regs = RegionStore::new();
            for ndx in nds.iter_mut() {
                match ndx {
                    SomeNeed::AddGroup { group_region, .. } => {
                        new_grp_regs.push_nosubs(group_region.clone());
                    }
                    _ => (),
                };
            } // Next ndx

            // Process a few specific housekeeping needs related to changing the Action or groups.
            let mut try_again = false;
            for ndx in nds.iter_mut() {
                match ndx {
                    SomeNeed::AddGroup {
                        group_region,
                        rules,
                    } => {
                        if new_grp_regs.contains(&group_region) {
                            // Check for supersets
                            if self.groups.any_superset_of(&group_region) {
                                let sups = self.groups.supersets_of(&group_region);
                                if sups.len() == 1 && sups.contains(&group_region) {
                                } else {
                                    println!(
                                        "\nDom {} Act {} **** Supersets found for new group {} in {}",
                                        dom, self.num, &group_region,
                                        self.groups.supersets_of(&group_region)
                                    );
                                }
                                continue;
                            }

                            // Calc pnc
                            let mut pnc = false;
                            if let Some(sqrx) = self.squares.find(&group_region.state1) {
                                if group_region.state2 == group_region.state1 {
                                    pnc = sqrx.results.pnc;
                                } else {
                                    if let Some(sqry) = self.squares.find(&group_region.state2) {
                                        pnc = sqrx.results.pnc && sqry.results.pnc;
                                    }
                                }
                            }

                            // Get stas used to calc new group, for display.
                            let sqrs_in_reg = self.squares.squares_in_reg(&group_region);
                            let mut max_pn = Pn::One;
                            for sqrx in sqrs_in_reg.iter() {
                                if sqrx.results.pn > max_pn {
                                    max_pn = sqrx.results.pn;
                                }
                            }
                            let mut stas_max_pn = StateStore::new();
                            for sqrx in sqrs_in_reg.iter() {
                                if sqrx.results.pn == max_pn {
                                    stas_max_pn.push(sqrx.state.clone());
                                }
                            }

                            let mut stas_min = StateStore::new();

                            if stas_max_pn.contains(&group_region.state1) {
                                stas_min.push(group_region.state1.clone());
                                if stas_max_pn.contains(&group_region.state2) {
                                    stas_min.push(group_region.state2.clone());
                                    println!("\nDom {} Act {} Group {} is an additional calculation, based on squares {}", dom, self.num, &group_region, &stas_min);
                                } else {
                                    println!("\nDom {} Act {} Group {} is an additional calculation, based on square {} and {} others, {} not yet sampled.",
                                              dom, self.num, &group_region, &stas_min, stas_max_pn.len() - 1, &group_region.state2);
                                }
                            } else if stas_max_pn.contains(&group_region.state2) {
                                stas_min.push(group_region.state2.clone());
                                println!("\nDom {} Act {} Group {} is an additional calculation, based on square {} and {} others, {} not yet sampled.",
                                              dom, self.num, &group_region, &stas_min, stas_max_pn.len() - 1, &group_region.state1);
                            } else {
                                println!("\nDom {} Act {} Group {} is an additional calculation, {} and {} not sampled yet",
                                              dom, self.num, &group_region, &group_region.state1, &group_region.state2);
                            }
                            assert!(self.check_region_for_group(&group_region));
                            self.groups.push(
                                SomeGroup::new(group_region.clone(), rules.clone(), pnc),
                                dom,
                                self.num,
                            );
                        }
                    }
                    SomeNeed::SetGroupLimited {
                        group_region: greg,
                        anchor: sta1,
                    } => {
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            println!(
                                "\nDom {} Act {} Group {} limited using {}",
                                dom, self.num, greg, sta1
                            );
                            grpx.set_anchor(sta1.clone());
                        }
                    }
                    SomeNeed::RemoveGroupAnchor { group_region: greg } => {
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            try_again = true;
                            grpx.set_anchor_off();
                        }
                    }
                    SomeNeed::InactivateSeekEdge { reg: regx } => {
                        self.seek_edge.remove_region(&regx);
                    }
                    SomeNeed::AddSeekEdge { reg: regx } => {
                        try_again = true;
                        if self.seek_edge.push_nosups(regx.clone()) {
                        } else {
                            println!(
                                "Dom {} Act {} need {} failed to add to\n{}",
                                dom, self.num, &ndx, &self.seek_edge
                            );
                            panic!("done");
                        }
                    }
                    _ => (),
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
                    match ndx {
                        SomeNeed::AddGroup { .. } => {
                            inxs.push(inx);
                        }
                        SomeNeed::SetGroupLimited { .. } => {
                            inxs.push(inx);
                        }
                        SomeNeed::RemoveGroupAnchor { .. } => {
                            inxs.push(inx);
                        }
                        SomeNeed::InactivateSeekEdge { .. } => {
                            inxs.push(inx);
                        }
                        SomeNeed::AddSeekEdge { .. } => {
                            inxs.push(inx);
                        }
                        _ => (),
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
                targ_state: regx.state1.clone(),
            });
            nds.push(SomeNeed::StateNotInGroup {
                dom_num: 0, // set this in domain get_needs
                act_num: self.num,
                targ_state: regx.state2.clone(),
            });
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
            if sqr1.results.pnc == false {
                //print!("get more samples of square {} ", &sqr1.state);
                ret_nds.push(SomeNeed::SeekEdge {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sqr1.state.clone(),
                    in_group: regx.clone(),
                });
                continue;
            }
            if sqr2.results.pnc == false {
                //print!("get more samples of square {} ", &sqr2.state);
                ret_nds.push(SomeNeed::SeekEdge {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sqr2.state.clone(),
                    in_group: regx.clone(),
                });
                continue;
            }

            // Get squares between
            let sqrs_in = self.squares.squares_in_reg(regx);
            if sqrs_in.len() == 2 {
                continue; // no squares between
            }

            let mut sqrs_in2 = Vec::<&SomeSquare>::with_capacity(sqrs_in.len() - 2);
            for sqrx in sqrs_in.iter() {
                if sqrx.state != sqr1.state && sqrx.state != sqr2.state {
                    sqrs_in2.push(sqrx);
                }
            }

            // Look for a square with pnc == true.
            let mut found_pnc = false;
            for sqrx in sqrs_in2.iter() {
                if sqrx.results.pnc {
                    found_pnc = true;

                    let cnb1 = sqrx.can_combine(&sqr1);
                    let cnb2 = sqrx.can_combine(&sqr2);

                    if cnb1 == Truth::T && cnb2 == Truth::T {
                        ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                    } else {
                        if cnb1 == Truth::F {
                            new_regs.push_nosups(SomeRegion::new(&sqrx.state, &sqr1.state));
                        }
                        if cnb2 == Truth::F {
                            new_regs.push_nosups(SomeRegion::new(&sqrx.state, &sqr2.state));
                        }
                    }
                    break;
                }
            }
            if found_pnc {
                continue;
            }

            // Process squares with pnc == false
            for sqrx in sqrs_in2.iter() {
                ret_nds.push(SomeNeed::SeekEdge {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sqrx.state.clone(),
                    in_group: regx.clone(),
                });
            }

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
                ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                continue;
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
            if rand::random::<bool>() {
                statex = regx.state1.clone();
            }

            // Calculate the target inbetween
            let seek_state = SomeState::new(statex.bts.b_xor(&dif_msk.bts));

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
    pub fn additional_group_state_samples(&mut self) -> NeedStore {
        //println!("additional_group_state_sample");
        let mut ret_nds = NeedStore::new();

        for grpx in self.groups.iter_mut() {
            if grpx.pnc {
                continue;
            }

            let mut set_pnc = true;

            if let Some(sqrx) = self.squares.find(&grpx.region.state1) {
                if sqrx.results.pnc == false {
                    ret_nds.push(SomeNeed::StateAdditionalSample {
                        dom_num: 0, // set this in domain get_needs
                        act_num: self.num,
                        targ_state: grpx.region.state1.clone(),
                        grp_reg: grpx.region.clone(),
                        far: grpx.region.state2.clone(),
                    });
                    set_pnc = false;
                }
            }

            if let Some(sqry) = self.squares.find(&grpx.region.state2) {
                if sqry.results.pnc == false {
                    ret_nds.push(SomeNeed::StateAdditionalSample {
                        dom_num: 0, // set this in domain get_needs
                        act_num: self.num,
                        targ_state: grpx.region.state2.clone(),
                        grp_reg: grpx.region.clone(),
                        far: grpx.region.state1.clone(),
                    });
                    set_pnc = false;
                }
            }

            if set_pnc {
                grpx.set_pnc();
            }
        } // next grpx

        ret_nds
    } // end additional_group_state_samples

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
                //println!("no squares in only group {}", &greg);
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
                    let sta_adj = SomeState::new(sta1.bts.b_xor(&edge_bit.bts));
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

                //println!("group {} anchor {} rating {}", &greg, &cfmx[0], sta_rate);

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
                cfm_max = &cfmv_max[rand::thread_rng().gen_range(0..cfmv_max.len())];
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
                        if anchor_sqr.can_combine(&adj_sqr) == Truth::T {
                            let mut sta_str = StateStore::with_capacity(2);
                            sta_str.push(anchor_sta.clone());
                            sta_str.push(adj_sta.clone());

                            let regz = SomeRegion::new(&anchor_sta, &adj_sta);
                            assert!(self.check_region_for_group(&regz));

                            let mut ruls = RuleStore::new();
                            if anchor_sqr.results.pn != Pn::Unpredictable {
                                ruls = anchor_sqr.rules.union(&adj_sqr.rules).unwrap();
                            }
                            nds_grp_add.push(SomeNeed::AddGroup {
                                group_region: regz,
                                rules: ruls,
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
                //println!("*** nds_grp_add {}", &nds_grp_add);
                ret_nds.append(&mut nds_grp_add);
                continue;
            }

            if nds_grp.len() > 0 {
                //println!("*** nds_grp {}", &nds_grp);
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
                        anchor: anchor_sta.clone(),
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

    /// Check needs for intersecting groups.
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
                }
            } // next iny
        } // next inx

        nds
    } // end group_pair_needs

    /// Return true if there is no invalid pair of squares in a region.
    fn no_incompatible_square_pair_in_region(&self, regx: &SomeRegion) -> bool {
        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(regx);

        // Check if any square pairs are incompatible
        for inx in 0..(sqrs_in_reg.len() - 1) {
            for iny in (inx + 1)..sqrs_in_reg.len() {
                if sqrs_in_reg[inx].can_combine(&sqrs_in_reg[iny]) == Truth::F {
                    return false;
                }
            }
        }
        true
    }

    /// Return true if there is no incompatible squares inbetween two compatible squares.
    fn can_combine_check_between(&self, sqr1: &SomeSquare, sqr2: &SomeSquare) -> bool {
        // println!("action:can_combine_check_between sqr {} and sqr {}", sqr1.state, sqr2.state);
        assert!(sqr1.state != sqr2.state);

        // Calc region formed by the two squares
        let regx = SomeRegion::new(&sqr1.state, &sqr2.state);

        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(&regx);

        if sqr1.results.pn == Pn::Unpredictable {
            for sqrx in sqrs_in_reg {
                if sqrx.results.pnc && sqrx.results.pn != Pn::Unpredictable {
                    return false;
                }
            }
            return true;
        }

        // Check if any square pairs are incompatible
        for sqrx in sqrs_in_reg {
            if sqrx.state != sqr1.state {
                if sqrx.results.pn > sqr1.results.pn {
                    return false;
                }
                if sqrx.can_combine(sqr1) == Truth::F {
                    return false;
                }
            }
            if sqrx.state != sqr2.state {
                if sqrx.results.pn > sqr2.results.pn {
                    return false;
                }
                if sqrx.can_combine(sqr2) == Truth::F {
                    return false;
                }
            }
        }
        true
    }

    /// Return needs to define a region.
    /// This assumes there are no incompatible square pairs in the region.
    pub fn region_defining_needs(&self, regx: &SomeRegion) -> NeedStore {
        //println!("region_defining_needs for {}", regx);

        let mut nds = NeedStore::new();

        let sqrs_in = self.squares.squares_in_reg(regx);

        // Find max pn
        let mut max_pn = Pn::One;
        for sqrx in sqrs_in.iter() {
            if sqrx.results.pn > max_pn {
                max_pn = sqrx.results.pn;
            }
        }
        //println!("max_pn: {}", max_pn);

        // Get all pairs that encompass the whole region
        let mut pairs = Vec::<(&SomeSquare, &SomeSquare)>::new();

        for inx in 0..(sqrs_in.len() - 1) {
            let sqrx = &sqrs_in[inx];

            if sqrx.results.pn != max_pn {
                continue;
            }

            for iny in (inx + 1)..sqrs_in.len() {
                let sqry = &sqrs_in[iny];

                if sqry.results.pn != max_pn {
                    continue;
                }

                let regy = SomeRegion::new(&sqrx.state, &sqry.state);

                if regy == *regx && sqrx.can_combine(sqry) == Truth::T {
                    if (sqrx.results.pnc || sqrx.results.pn == Pn::One)
                        && (sqry.results.pnc || sqry.results.pn == Pn::One)
                    {
                        if sqrx.results.pn == Pn::Unpredictable {
                            nds.push(SomeNeed::AddGroup {
                                group_region: regy,
                                rules: RuleStore::new(),
                            });
                        } else {
                            if sqrx.results.pn == Pn::Unpredictable {
                                nds.push(SomeNeed::AddGroup {
                                    group_region: regy,
                                    rules: RuleStore::new(),
                                });
                            } else {
                                nds.push(SomeNeed::AddGroup {
                                    group_region: regy,
                                    rules: sqrx.rules.union(&sqry.rules).unwrap(),
                                });
                            }
                        }
                        return nds;
                    }
                    pairs.push((sqrx, sqry));
                }
            }
        }

        // Check for pairs with the greatest number samples
        if pairs.len() > 0 {
            let mut pairs2 = Vec::<(&SomeSquare, &SomeSquare)>::new();
            let mut max_samples = 0;
            for (sqrx, sqry) in pairs.iter() {
                let mut num_samples = 0;
                if sqrx.results.pnc {
                    num_samples += 5;
                } else {
                    num_samples += sqrx.results.len();
                }
                if sqry.results.pnc {
                    num_samples += 5;
                } else {
                    num_samples += sqry.results.len();
                }
                if num_samples > max_samples {
                    max_samples = num_samples;
                    pairs2 = Vec::<(&SomeSquare, &SomeSquare)>::new();
                }
                if num_samples == max_samples {
                    pairs2.push((sqrx, sqry));
                }
            }

            // Make needs for the best pairs
            let mut choice = 0;
            if pairs2.len() > 1 {
                choice = rand::thread_rng().gen_range(0..pairs2.len());
            }
            let sqrx = pairs2[choice].0;
            let sqry = pairs2[choice].1;

            if sqrx.results.pnc == false {
                nds.push(SomeNeed::AStateMakeGroup {
                    dom_num: 0, // Will be set later.
                    act_num: self.num,
                    targ_state: sqrx.state.clone(),
                    for_reg: regx.clone(),
                    far: sqry.state.clone(),
                    num_x: regx.num_x(),
                });
            }
            if sqry.results.pnc == false {
                nds.push(SomeNeed::AStateMakeGroup {
                    dom_num: 0, // Will be set later.
                    act_num: self.num,
                    targ_state: sqry.state.clone(),
                    for_reg: regx.clone(),
                    far: sqrx.state.clone(),
                    num_x: regx.num_x(),
                });
            }

            return nds;
        }

        // No pairs found

        // Find max pn value
        let mut max_pn = Pn::One;
        for sqrx in sqrs_in.iter() {
            if sqrx.results.pn > max_pn {
                max_pn = sqrx.results.pn;
            }
        }

        // Look for pnc squares
        for sqrx in sqrs_in.iter() {
            if sqrx.results.pnc || max_pn == Pn::One {
                let far_sta = regx.far_state(&sqrx.state);
                nds.push(SomeNeed::AStateMakeGroup {
                    dom_num: 0, // Will be set later.
                    act_num: self.num,
                    targ_state: far_sta,
                    for_reg: regx.clone(),
                    far: sqrx.state.clone(),
                    num_x: regx.num_x(),
                });
            }
        }
        nds
    }

    /// Check two intersecting groups for needs.
    /// Possibly combining two groups.
    /// Possibly checking for a contradictatory intersection.
    pub fn group_pair_intersection_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //                println!(
        //                    "groups_pair_intersection_needs {} {} and {} {}",
        //                    &grpx.region, &grpx.pn, &grpy.region, grpy.pn
        //                );

        let mut nds = NeedStore::new();

        let reg_both = grpx.region.union(&grpy.region);

        if grpx.pn == Pn::Unpredictable && grpy.pn == Pn::Unpredictable {
            if self.no_incompatible_square_pair_in_region(&reg_both) {
                return self.region_defining_needs(&reg_both);
            }
            return nds;
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

                // to avoid subtraction, use the far sub-region
                let regy = reg_int.far_reg(&ok_reg);

                //println!("pn2 intersection is {} far reg is {}", rulsxy.formatted_string(), &regy);

                nds.push(self.cont_int_region_needs(&regy, &grpx, &grpy));
            } else {
                //println!("pn2 whole intersection is bad");
                nds.push(self.cont_int_region_needs(&reg_int, &grpx, &grpy));
            }

            return nds;
        }

        // rulsx == rulsy
        if self.no_incompatible_square_pair_in_region(&reg_both) {
            return self.region_defining_needs(&reg_both);
        }

        nds
    } // end group_pair_intersection_needs

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
            inx = rand::thread_rng().gen_range(0..stas_check.len());
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
                    if let Some(rulx) = grpx.rules[0].parse_for_changes(&achange) {
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
                Pn::Unpredictable => (),
            } // end match grpx.pn
        } // next grpx

        // println!("Steps: {}", &stps);
        stps
    } // end get_steps

    /// Find squares whose rules can be combined with a given squares rules.
    /// Check if any included squares invalidate a combination.
    /// Remove subset combinations.
    /// Return the regions resulting from successful combinations.
    pub fn possible_regions_from_square(&self, sqrx: &SomeSquare) -> RegionStore {
        //println!("possible_group_regions from sqr {}", &sqrx.state);

        let mut rsx = RegionStore::new();

        // Collect possible region, deleting subset regions
        for (_key, sqry) in &self.squares.ahash {
            if sqry.state == sqrx.state {
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
        self.do_something.take_action(dom, self.num, cur_state)
    }
} // end impl SomeAction
