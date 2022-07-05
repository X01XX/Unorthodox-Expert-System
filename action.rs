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
use std::collections::VecDeque;
use std::fmt;

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("A(ID: ");

        rc_str += &self.num.to_string();

        rc_str += &String::from(", number squares: ");
        rc_str += &self.squares.len().to_string();

        if self.seek_edge.len() > 0 {
            rc_str += &format!(", seek_edge within: {}", self.seek_edge);
        }

        let regs = self.groups.regions();

        let mut fil = String::from(",\n       Grps: ");
        for grpx in self.groups.iter() {
            let stas_in = self.squares.stas_in_reg(&grpx.region);

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
    /// Trigger cleanup logic after a new sample.
    cleanup_trigger: usize,
}

impl SomeAction {
    /// Return a new SomeAction struct, given the number integers used in the SomeBits struct.
    /// The action number, an index into the ActionStore that will contain it, is set to zero and
    /// changed later.
    pub fn new(inx: usize, num_ints: usize) -> Self {
        SomeAction {
            num: inx,
            groups: GroupStore::new(),
            squares: SquareStore::new(),
            seek_edge: RegionStore::new(),
            aggregate_changes: SomeChange::new_low(num_ints),
            do_something: ActionInterface::new(),
            cleanup_trigger: 20,
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
        if cmbx == Truth::F {
            return cmbx;
        }

        // Check for Pn::Unpredictable region, cmbx may be Truth::T or Truth::M.
        if sqrx.pn == Pn::Unpredictable || sqry.pn == Pn::Unpredictable {
            if self.no_incompatible_pn_square_in_region(
                &SomeRegion::new(&sqrx.state, &sqry.state),
                Pn::Unpredictable,
            ) == false
            {
                return Truth::F;
            } else {
                return cmbx;
            }
        }

        // Check for Pn values not equal, but more samples may allow the combination.
        if sqrx.pn != sqry.pn {
            if self
                .no_incompatible_square_pair_in_region(&SomeRegion::new(&sqrx.state, &sqry.state))
                == false
            {
                return Truth::F;
            } else {
                return cmbx;
            }
        }

        // Check for cmbx == Truth::T or Truth::M, while Pn values are equal, One or Two.
        if self.check_subsets_between(sqrx, sqry) == false {
            return Truth::F;
        }

        cmbx
    }

    /// Evaluate a sample.
    pub fn eval_sample(&mut self, initial: &SomeState, result: &SomeState, dom: usize) {
        if self.cleanup_trigger > 0 {
            self.cleanup_trigger -= 1;
        }
        if self.store_sample(&initial, &result, dom) {
            self.check_square_new_sample(&initial, dom);
        }
    }

    /// Evaluate a sample taken to satisfy a need.
    pub fn eval_need_sample(
        &mut self,
        initial: &SomeState,
        ndx: &SomeNeed,
        result: &SomeState,
        dom: usize,
    ) {
        if self.cleanup_trigger > 0 {
            self.cleanup_trigger -= 1;
        }

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

                if sqrx.pn == Pn::One || sqrx.pnc {
                    //println!("AStateMakeGroup: sqr {} sampled, pn {} pnc {}", &sqrx, sqrx.pn, sqrx.pnc);
                    if let Some(sqry) = self.squares.find(&far) {
                        if sqry.pn == Pn::One || sqry.pnc {
                            if sqrx.state == sqry.state
                                || self.can_combine(&sqrx, &sqry) == Truth::T
                            {
                                let regz = SomeRegion::new(&sqrx.state, &sqry.state);

                                let rulsxy;
                                if sqrx.pn == Pn::Unpredictable {
                                    rulsxy = RuleStore::new();
                                } else {
                                    rulsxy = sqrx.rules.union(&sqry.rules).unwrap();
                                }

                                self.groups.push(
                                    SomeGroup::new(regz, rulsxy, sqrx.pnc && sqry.pnc),
                                    dom,
                                    self.num,
                                );
                            } // end if can combine
                        } // end if sqry.pn ==
                    } // end find far
                } // end if sqrx.pn ==
            } // end process AStateMakeGroup Need

            SomeNeed::SeekEdge {
                targ_state: sta,
                in_group: greg,
                ..
            } => {
                assert!(greg.state1 != greg.state2);

                if *sta != greg.state1 && *sta != greg.state2 {
                    let mut make_groups_from = Vec::<SomeState>::new();

                    // Find the squares
                    let sqr1 = self.squares.find(&greg.state1).unwrap();
                    let sqr2 = self.squares.find(&greg.state2).unwrap();
                    let sqr3 = self.squares.find(&sta).unwrap();

                    // Process next sample of square in-between for new square and state1 square.
                    // Should be different from state1 square or state2 square.
                    // It may be different from both state1 and state2.
                    // If it needs more samples, skip, next need will increment the number samples.

                    if sqr3.can_combine(&sqr1) == Truth::F {
                        if sqr1.pnc && sqr3.pnc {
                            if sqr1.is_adjacent(&sqr3) {
                                println!("Dom {} Act {} new edge found between {} and {} removing seek edge {}", dom, self.num, &sqr1.state, &sqr3.state, &greg);
                                self.seek_edge.remove_region(greg);
                                make_groups_from.push(sqr1.state.clone());
                                make_groups_from.push(sqr3.state.clone());
                            } else {
                                let even_closer_reg = SomeRegion::new(&sqr1.state, &sqr3.state);
                                self.seek_edge.push_nosups(even_closer_reg);
                            }
                        }
                    }

                    if sqr3.can_combine(&sqr2) == Truth::F {
                        if sqr2.pnc && sqr3.pnc {
                            if sqr2.is_adjacent(&sqr3) {
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
                }
            } // end match SeekEdgeNeed
            _ => (),
        } // end match ndx
    } // end eval_need_sample

    /// Evaluate the sample taken for a step in a plan.
    pub fn eval_step_sample(&mut self, cur: &SomeState, new_state: &SomeState, dom: usize) {
        if self.cleanup_trigger > 0 {
            self.cleanup_trigger -= 1;
        }

        // If a square exists, update it.
        if let Some(sqrx) = self.squares.find_mut(cur) {
            // println!("about to add result to sqr {}", cur.str());
            sqrx.add_result(new_state.clone());

            if sqrx.changed {
                self.check_square_new_sample(cur, dom);
            }
            return;
        }

        // Check if any groups are invalidated
        if self.groups.any_groups_invalidated(cur, new_state) {
            self.eval_sample(cur, new_state, dom);
        }
    } // end eval_step_sample

    /// Store a sample.
    /// Update an existing square or create a new square.
    fn store_sample(&mut self, cur: &SomeState, new_state: &SomeState, dom: usize) -> bool {
        // Get an existing square and update, or create a new square.
        //println!("store_sample");
        if let Some(sqrx) = self.squares.find_mut(cur) {
            // println!("about to add result to sqr {}", cur.str());
            return sqrx.add_result(new_state.clone());
        }

        // println!("No square found for state {}", cur.str());
        self.squares.insert(
            SomeSquare::new(cur.clone(), new_state.clone()),
            dom,
            self.num,
        );
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

        if self.groups.num_groups_state_in(key) == 1 {
            let grps_in: RegionStore = self.groups.groups_state_in(key);
            let grpx = self.groups.find(&grps_in[0]).unwrap();
            let anchor_rate = self.group_anchor_rate(&grpx, key, &self.groups.anchor_states());
            if anchor_rate > grpx.anchor_rate {
                if (0, 0, 0) != grpx.anchor_rate {
                    if anchor_rate > grpx.anchor_rate {
                        println!(
                            "Changing group {} anchor from {:?} to {:?}",
                            grpx.region, grpx.anchor_rate, anchor_rate
                        );
                        self.groups.set_anchor(&grps_in[0], key, anchor_rate);
                    }
                }
            }
        }

        // Get groups invalidated, which may orphan some squares.
        //let regs_invalid = self.validate_groups_new_sample(&key);
        let regs_invalid: RegionStore = self.groups.check_square(&sqrx, dom, self.num);

        // Save regions invalidated to seek new edges.
        for regx in regs_invalid.iter() {
            if regx.x_mask().num_one_bits() > 1 && *key != regx.state1 && *key != regx.state2 {
                if let Some(sqr1) = self.squares.find(&regx.state1) {
                    if let Some(sqr2) = self.squares.find(&regx.state2) {
                        if sqr1.pnc && sqr2.pnc {
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
            if regs_invalid.len() == 0 {
                self.aggregate_changes = self.calc_aggregate_changes();
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
        if sqrx.pn == Pn::One || sqrx.pnc {
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
                if sqrx.pn == Pn::One || sqrx.pnc {
                    if let Some(sqry) = self.squares.find(&regx.state2) {
                        if sqry.pn == Pn::One || sqry.pnc {
                            let ruls;
                            if sqrx.pn == Pn::Unpredictable {
                                ruls = RuleStore::new();
                            } else {
                                ruls = sqrx.rules.union(&sqry.rules).unwrap();
                            }

                            self.groups.push(
                                SomeGroup::new(regx.clone(), ruls, sqrx.pnc && sqry.pnc),
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
        self.groups.push(
            SomeGroup::new(regz, sqrx.rules.clone(), sqrx.pnc),
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
    pub fn state_not_in_group_needs(
        &self,
        cur_state: &SomeState,
        memory: &VecDeque<SomeState>,
    ) -> NeedStore {
        let mut nds = NeedStore::new();

        // Check if current state is in any groups
        if self.groups.any_superset_of_state(cur_state) == false {
            nds.push(SomeNeed::StateNotInGroup {
                dom_num: 0, // will be set later
                act_num: self.num,
                targ_state: cur_state.clone(),
            });
        }

        // Check memory
        for stax in memory.iter() {
            if stax == cur_state {
                continue;
            }
            if self.groups.any_superset_of_state(stax) == false {
                nds.push(SomeNeed::StateNotInGroup {
                    dom_num: 0, // will be set later
                    act_num: self.num,
                    targ_state: stax.clone(),
                });
            }
        } // next stax

        // Look for a pn > 1, pnc == false, not in group squares
        // Extra samples are needed to gain pnc, then the first group.
        let sqrs_pngt1 = self.squares.pn_gt1_no_pnc();

        for stax in sqrs_pngt1.iter() {
            if stax == cur_state || self.groups.any_superset_of_state(&stax) {
                continue;
            }

            nds.push(SomeNeed::StateNotInGroup {
                dom_num: 0, // will be set later
                act_num: self.num,
                targ_state: stax.clone(),
            });
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
        dom: usize,
        memory: &VecDeque<SomeState>,
    ) -> NeedStore {
        //println!("Running Action {}::get_needs {}", self.num, cur_state);

        // loop until no housekeeping need is returned.
        let mut nds = NeedStore::new();
        let mut cnt = 0;
        loop {
            cnt += 1;

            // Look for needs for states not in groups
            let mut ndx = self.state_not_in_group_needs(cur_state, memory);
            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Look for needs to find a new edge in an invalidated group
            let mut ndx = self.seek_edge_needs();
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
            let mut ndx = self.group_pair_needs();
            //println!("Ran group_pair_needs");
            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Check for squares in-one-group needs
            let mut ndx = self.limit_groups_needs();
            if ndx.len() > 0 {
                nds.append(&mut ndx);
            }

            // Check for repeating housekeeping needs loop
            if cnt > 20 {
                println!("needs: {}", &nds);
                panic!("Dom {} Act {} loop count GT 20!", dom, self.num);
            }

            // Edit out subset group adds, dups may still exist.
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
                        if new_grp_regs.contains(&group_region) == false {
                            continue;
                        }

                        // Check for supersets
                        if self.groups.any_superset_of(&group_region) {
                            if let Some(_) = self.groups.find(&group_region) {
                            } else {
                                println!(
                                    "\nDom {} Act {} **** Supersets found for new group {} in {}",
                                    dom,
                                    self.num,
                                    &group_region,
                                    self.groups.supersets_of(&group_region)
                                );
                            }
                            continue;
                        }

                        // Calc pnc
                        let pnc;
                        let sqrx = self.squares.find(&group_region.state1).unwrap();
                        if group_region.state2 == group_region.state1 {
                            pnc = sqrx.pnc;
                        } else {
                            let sqry = self.squares.find(&group_region.state2).unwrap();
                            pnc = sqrx.pnc && sqry.pnc;
                        }

                        self.groups.push(
                            SomeGroup::new(group_region.clone(), rules.clone(), pnc),
                            dom,
                            self.num,
                        );
                        try_again = true;
                    }
                    SomeNeed::SetGroupLimited {
                        group_region: greg,
                        // anchor: sta1,
                    } => {
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            println!("\nDom {} Act {} Group {} limited", dom, self.num, greg);
                            grpx.set_limited();
                        }
                    }
                    SomeNeed::SetGroupAnchor {
                        group_region: greg,
                        anchor: sta1,
                        rate,
                    } => {
                        if let Some(grpx) = self.groups.find_mut(&greg) {
                            println!(
                                "\nDom {} Act {} Group {} setting anchor to {}",
                                dom, self.num, greg, sta1
                            );
                            grpx.set_anchor(&sta1, *rate);
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
                //                if nds.len() == 0 {
                //                    return self.left_over_needs();
                //                }

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
                        SomeNeed::SetGroupAnchor { .. } => {
                            inxs.push(inx);
                        }
                        SomeNeed::InactivateSeekEdge { .. } => {
                            inxs.push(inx);
                        }
                        _ => (),
                    }
                    inx += 1;
                }

                // Remove houskeeping needs, from highest to lowest index.
                for inx in inxs.iter().rev() {
                    nds.remove_unordered(*inx);
                }

                // Do cleanup
                if self.cleanup_trigger == 0 || nds.len() == 0 {
                    self.cleanup(&nds);
                    self.cleanup_trigger = 10;
                }

                return nds;
            }

            nds = NeedStore::new();
        } // end loop
    } // end get_needs

    /// Cleanup, that is delete unneeded squares
    fn cleanup(&mut self, needs: &NeedStore) {
        let stas = self.squares.all_square_states();

        let mut sqr_del = StateStore::new();

        for stax in stas.iter() {
            // Check needs
            let mut in_needs = false;
            for ndx in needs.iter() {
                if ndx.target().is_superset_of_state(stax) {
                    in_needs = true;
                    break;
                }
            }
            if in_needs {
                continue;
            }

            let mut in_groups = false;
            for grpx in self.groups.iter() {
                if grpx.region.state1 == *stax || grpx.region.state2 == *stax {
                    in_groups = true;
                    break;
                }
                if let Some(stay) = &grpx.anchor {
                    if stay == stax {
                        in_groups = true;
                        break;
                    }
                }
            }
            if in_groups {
                continue;
            }

            let mut adj_anchor = false;
            for grpx in self.groups.iter() {
                if let Some(stay) = &grpx.anchor {
                    if stax.is_adjacent(&stay) {
                        adj_anchor = true;
                        break;
                    }
                }
            }
            if adj_anchor {
                continue;
            }

            let mut in_seek = false;
            for regx in self.seek_edge.iter() {
                if regx.is_superset_of_state(stax) {
                    in_seek = true;
                    break;
                }
            }
            if in_seek {
                continue;
            }

            if self.groups.num_groups_state_in(stax) == 0 {
                continue;
            }

            // println!("sqr {} unneeded", stax);
            sqr_del.push(stax.clone());
            self.squares.remove(stax);
        } // next stax
        if sqr_del.len() > 0 {
            println!("Act {} deleted unneeded squares {}", self.num, sqr_del);
        }
    } // end cleanup

    /// When a group is invalidated by a new sample, something was wrong within that group.
    ///
    /// When the group has more than one X-bit positions (the region states are not adjacent or equal),
    /// the number of possible replacement groups will be greatly decreased if the
    /// new sample can be used to find an adjacent, dissimilar pair of squares (an edge)
    /// within the invalidated group.
    pub fn seek_edge_needs(&self) -> NeedStore {
        //println!("seek_edge_needs");
        let mut ret_nds = NeedStore::new();

        let mut new_regs = RegionStore::new();

        for regx in self.seek_edge.iter() {
            //print!("seek_edge_needs: checking reg {} ", &regx);
            // Get the squares represented by the states that form the region
            let sqr1 = self.squares.find(&regx.state1).unwrap();
            let sqr2 = self.squares.find(&regx.state2).unwrap();

            // Check that squares that define the region are pnc.
            if sqr1.pnc == false {
                //print!("get more samples of square {} ", &sqr1.state);
                ret_nds.push(SomeNeed::SeekEdge {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sqr1.state.clone(),
                    in_group: regx.clone(),
                });
                continue;
            }
            if sqr2.pnc == false {
                //print!("get more samples of square {} ", &sqr2.state);
                ret_nds.push(SomeNeed::SeekEdge {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: sqr2.state.clone(),
                    in_group: regx.clone(),
                });
                continue;
            }

            // Get squares between defining states.
            let sqrs_in = self.squares.squares_in_reg(regx);

            // No squares between
            if sqrs_in.len() == 2 {
                let mut ndx = self.seek_edge_needs2(regx);
                if ndx.len() > 0 {
                    ret_nds.append(&mut ndx);
                }
                continue;
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
                if sqrx.pnc {
                    found_pnc = true;

                    let cnb1 = sqrx.can_combine(&sqr1);
                    let cnb2 = sqrx.can_combine(&sqr2);

                    if cnb1 != Truth::F && cnb2 != Truth::F {
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

            // Generate need for squares with pnc == false
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
    } // end seek_edge_needs

    /// Generate needs for seek_edge regions that have no squares between the region defining states.
    pub fn seek_edge_needs2(&self, regx: &SomeRegion) -> NeedStore {
        //println!("seek_edge_needs2");
        let mut ret_nds = NeedStore::new();

        if regx.state1.is_adjacent(&regx.state2) {
            ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
            return ret_nds;
        }

        // Select a random set of single-bit masks, up to one-half of the number of differences.
        // So if the region states are 10 or 11 bits different, a state 5 bits different from
        // one of the two states will be sought.  So the number of bit differences should go down
        // 50% on each cycle.
        let dif_msk = regx.x_mask().half_mask();

        // Randomly choose which state to use to calculate the target state from
        let seek_state;
        if rand::random::<bool>() {
            seek_state = SomeState::new(regx.state1.bts.b_xor(&dif_msk.bts));
        } else {
            seek_state = SomeState::new(regx.state2.bts.b_xor(&dif_msk.bts));
        }

        // Make need for seek_state
        //print!("get first sample of square {}", &seek_state);
        ret_nds.push(SomeNeed::SeekEdge {
            dom_num: 0, // set this in domain get_needs
            act_num: self.num,
            targ_state: seek_state,
            in_group: regx.clone(),
        });
        //println!(" ");

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

            let sqrx = self.squares.find(&grpx.region.state1).unwrap();
            if sqrx.pnc == false {
                ret_nds.push(SomeNeed::StateAdditionalSample {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: grpx.region.state1.clone(),
                    grp_reg: grpx.region.clone(),
                    far: grpx.region.state2.clone(),
                });
            }

            let sqry = self.squares.find(&grpx.region.state2).unwrap();
            if sqry.pnc == false {
                ret_nds.push(SomeNeed::StateAdditionalSample {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    targ_state: grpx.region.state2.clone(),
                    grp_reg: grpx.region.clone(),
                    far: grpx.region.state1.clone(),
                });
            }

            if sqrx.pnc && sqry.pnc {
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
    pub fn limit_groups_needs(&self) -> NeedStore {
        //println!("limit_groups_needs chg {}", agg_chgs);

        let mut ret_nds = NeedStore::new();

        let anchors = self.groups.anchor_states();

        // Check groups current anchors are still in only one region,
        for grpx in self.groups.iter() {
            if let Some(stax) = &grpx.anchor {
                if self.groups.num_groups_state_in(&stax) != 1 {
                    ret_nds.push(SomeNeed::RemoveGroupAnchor {
                        group_region: grpx.region.clone(),
                    });
                }
            }
        }
        if ret_nds.len() > 0 {
            return ret_nds;
        }

        // Find squares in one group for each group, that may be an anchor
        for grpx in self.groups.iter() {
            if grpx.pnc == false || grpx.limited {
                continue;
            }

            let mut ndx = self.limit_group_needs(grpx, &anchors);
            if ndx.len() > 0 {
                ret_nds.append(&mut ndx);
            }
        }
        ret_nds
    } // end limit_groups_needs

    /// Return a rate for a possible anchor for a group.
    /// The rate will be a tuple containing:
    ///     The number of adjacent states that are anchors of other groups,
    ///     The number adjacent states that are in only one group,
    ///     The number of samples taken for the adjacent states.
    pub fn group_anchor_rate(
        &self,
        grpx: &SomeGroup,
        stax: &SomeState,
        anchors: &StateStore,
    ) -> (usize, usize, usize) {
        assert!(self.groups.num_groups_state_in(&stax) == 1);

        let mut sta_rate = (0, 0, 0);

        if let Some(sqrx) = self.squares.find(stax) {
            if sqrx.pnc {
                sta_rate.2 += 5;
            } else {
                sta_rate.2 += sqrx.len_results();
            }
        }

        // Rate far state
        let sta_far = grpx.region.far_state(&stax);
        if let Some(sqrx) = self.squares.find(&sta_far) {
            if sqrx.pnc {
                sta_rate.2 += 4; // LT anchor value, so (pnc anchor, non-pnc far) NE (non-pnc anchor, pnc far)
            } else {
                sta_rate.2 += sqrx.len_results();
            }
        }

        // Get masks of edge bits to use to limit group.
        let edge_msks = grpx.region.x_mask().m_not().split();

        // Rate adjacent external states
        for edge_bit in edge_msks.iter() {
            let sta_adj = SomeState::new(stax.bts.b_xor(&edge_bit.bts));
            //println!(
            //    "checking {} adjacent to {} external to {}",
            //    &sta_adj, &stax, &greg
            //);

            if anchors.contains(&sta_adj) {
                sta_rate.0 += 1;
                sta_rate.1 += 1;
            } else if self.groups.num_groups_state_in(&sta_adj) == 1 {
                //println!("{} is in only one group", &sta_adj);
                sta_rate.1 += 1;
            }

            if let Some(sqrx) = self.squares.find(&sta_adj) {
                if sqrx.pnc {
                    sta_rate.2 += 5;
                } else {
                    sta_rate.2 += sqrx.len_results();
                }
            }
        } // next edge_bit

        sta_rate
    }

    /// Return the limiting need for a group.
    pub fn limit_group_needs(&self, grpx: &SomeGroup, anchors: &StateStore) -> NeedStore {
        let mut ret_nds = NeedStore::new();

        if grpx.limited && grpx.anchor.is_some() {
            println!("limit group needs for {}?", grpx.region);
            return ret_nds;
        }

        if grpx.anchor.is_none() {
            let stas_in: StateStore = self.squares.stas_in_reg(&grpx.region);

            // For each state, sta1, only in the group region, greg:
            //
            //  Calculate each state, sta_adj, adjacent to sta1, outside of greg.
            //
            //  Calculate a rate for each sta1 option, based on the number of adjacent states
            //  in only one group.
            let mut max_rate = (0, 0, 0);

            // Create a StateStore composed of anchor, far, and adjacent-external states.
            let mut cfmv_max = Vec::<SomeState>::new();

            for stax in stas_in.iter() {
                if self.groups.num_groups_state_in(&stax) != 1 {
                    continue;
                }

                let sta_rate = self.group_anchor_rate(&grpx, stax, anchors);

                //println!("group {} anchor {} rating {}", &greg, &cfmx[0], sta_rate);

                // Accumulate highest rated anchors
                if sta_rate > max_rate {
                    max_rate = sta_rate;
                    cfmv_max = Vec::<SomeState>::new();
                }
                //println!("rate {} is {}", cfmx[0], sta_rate);
                if sta_rate == max_rate {
                    cfmv_max.push(stax.clone());
                }
            } // next stax

            if cfmv_max.len() == 0 {
                return ret_nds;
            }

            // Select an anchor
            let mut cfm_max = &cfmv_max[0];
            if cfmv_max.len() > 1 {
                cfm_max = &cfmv_max[rand::thread_rng().gen_range(0..cfmv_max.len())];
            }
            ret_nds.push(SomeNeed::SetGroupAnchor {
                group_region: grpx.region.clone(),
                anchor: cfm_max.clone(),
                rate: max_rate,
            });
            return ret_nds;
        }
        // If any external adjacent states have not been sampled, or not enough,
        // return needs for that.
        //
        // If the group far state has not been sampled, or not enough, return a need for that.
        //
        // Else limit the group.
        let anchor_sta;
        if let Some(stax) = &grpx.anchor {
            anchor_sta = stax.clone();
        } else {
            println!("No anchor for group {}?", grpx);
            panic!("Done");
        }

        let anchor_sqr = self.squares.find(&anchor_sta).unwrap();
        if anchor_sqr.pnc {
            // println!("group {} anchor {} pnc", &greg, &anchor_sta);
        } else {
            // Get additional samples of the anchor
            ret_nds.push(SomeNeed::LimitGroup {
                dom_num: 0, // will be set in domain code
                act_num: self.num,
                anchor: anchor_sta.clone(),
                targ_state: anchor_sta.clone(),
                for_group: grpx.region.clone(),
            });
            return ret_nds;
        }

        // Check each adjacent external state
        let mut nds_grp = NeedStore::new(); // needs for more samples
        let mut nds_grp_add = NeedStore::new(); // needs for added group

        // Get masks of edge bits to use to limit group.
        let edge_msks = grpx.region.x_mask().m_not().split();

        //let stax = anchor_sta.clone();

        for mskx in edge_msks.iter() {
            let adj_sta = SomeState::new(anchor_sta.bts.b_xor(&mskx.bts));

            //println!("*** for group {} checking adj sqr {}", &greg, &adj_sta);

            if let Some(adj_sqr) = self.squares.find(&adj_sta) {
                if adj_sqr.pnc {
                    if anchor_sqr.can_combine(&adj_sqr) == Truth::T {
                        let regz = SomeRegion::new(&anchor_sta, &adj_sta);

                        let ruls;
                        if anchor_sqr.pn == Pn::Unpredictable {
                            ruls = RuleStore::new();
                        } else {
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
                        for_group: grpx.region.clone(),
                    });
                }
            } else {
                nds_grp.push(SomeNeed::LimitGroup {
                    dom_num: 0, // will be set in domain code
                    act_num: self.num,
                    anchor: anchor_sta.clone(),
                    targ_state: adj_sta.clone(),
                    for_group: grpx.region.clone(),
                });
            }
        } // next inx in cfm_max

        if nds_grp_add.len() > 0 {
            //println!("*** nds_grp_add {}", &nds_grp_add);
            ret_nds.append(&mut nds_grp_add);
            return ret_nds;
        }

        if nds_grp.len() > 0 {
            //println!("*** nds_grp {}", &nds_grp);
            ret_nds.append(&mut nds_grp);
            return ret_nds;
        }

        //   println!("grp {} check far", &greg);

        // Process far state, after the anchor and adjacent, external, checks have been made.
        // Instead of checking every adjacent square internal to the group.
        if grpx.region.state1 != grpx.region.state2 {
            let sta_far = grpx.region.far_state(&anchor_sta);
            if let Some(sqrf) = self.squares.find(&sta_far) {
                if sqrf.pnc {
                    // Set the group limited
                    ret_nds.push(SomeNeed::SetGroupLimited {
                        group_region: grpx.region.clone(),
                    });
                } else {
                    // Get additional samples of the far state
                    ret_nds.push(SomeNeed::LimitGroup {
                        dom_num: 0, // will be set in domain code
                        act_num: self.num,
                        anchor: anchor_sta.clone(),
                        targ_state: sta_far.clone(),
                        for_group: grpx.region.clone(),
                    });
                }
            } else {
                // Get the first sample of the far state
                ret_nds.push(SomeNeed::LimitGroup {
                    dom_num: 0, // will be set in domain code
                    act_num: self.num,
                    anchor: anchor_sta.clone(),
                    targ_state: sta_far.clone(),
                    for_group: grpx.region.clone(),
                });
            }
        }
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

    /// Return true if there is no square with an incompatible Pn value.
    fn no_incompatible_pn_square_in_region(&self, regx: &SomeRegion, pnx: Pn) -> bool {
        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(regx);

        for sqrx in sqrs_in_reg.iter() {
            if sqrx.pn != pnx {
                if sqrx.pnc || sqrx.pn > pnx {
                    return false;
                }
            }
        }
        true
    }

    /// Return true if there is no incompatible squares between two compatible squares.
    fn check_subsets_between(&self, sqr1: &SomeSquare, sqr2: &SomeSquare) -> bool {
        // println!("action:can_combine_check_between sqr {} and sqr {}", sqr1.state, sqr2.state);
        if sqr1.state == sqr2.state {
            return true;
        }
        if sqr1.can_combine(sqr2) != Truth::T {
            return false;
        }

        // Calc region formed by the two squares
        let regx = SomeRegion::new(&sqr1.state, &sqr2.state);

        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(&regx);

        if sqr1.pn == Pn::Unpredictable {
            for sqrx in sqrs_in_reg {
                if sqrx.pnc && sqrx.pn != Pn::Unpredictable {
                    return false;
                }
            }
            return true;
        }

        let rules = sqr1.rules.union(&sqr2.rules).unwrap();

        for sqrx in sqrs_in_reg {
            if sqrx == sqr1 || sqrx == sqr2 {
                continue;
            }

            if sqrx.pnc && sqrx.pn != sqr1.pn {
                return false;
            }

            if rules.is_superset_of(&sqrx.rules) == false {
                return false;
            }
        }

        true
    }

    /// Return needs to define a region, from the combination of two smaller regions.
    /// This assumes there are no incompatible square pairs in the region.
    pub fn region_defining_needs(
        &self,
        regx: &SomeRegion,
        reg1: &SomeRegion,
        reg2: &SomeRegion,
    ) -> NeedStore {
        //println!("region_defining_needs for {}", regx);

        let mut nds = NeedStore::new();

        // Gather the states from the regions, they may share one state.
        let mut anchor_stas = StateStore::new();
        anchor_stas.push(reg1.state1.clone());
        anchor_stas.push(reg1.state2.clone());
        if anchor_stas.contains(&reg2.state1) {
        } else {
            anchor_stas.push(reg2.state1.clone());
        }
        if anchor_stas.contains(&reg2.state2) {
        } else {
            anchor_stas.push(reg2.state2.clone());
        }

        // Gather anchor squares
        let mut anchor_sqrs = Vec::<&SomeSquare>::with_capacity(anchor_stas.len());
        for stax in anchor_stas.iter() {
            anchor_sqrs.push(self.squares.find(stax).unwrap());
        }

        // Check the far states from the anchors, form pairs.
        let mut num_samples = 0;
        let mut pnc = false;
        let mut pairs = Vec::<(SomeState, &SomeSquare)>::new();
        for sqrx in anchor_sqrs.iter() {
            let sta2 = regx.far_state(&sqrx.state);
            if let Some(sqr2) = self.squares.find(&sta2) {
                if sqr2.pnc {
                    pnc = true;
                }
                if sqr2.len_results() > num_samples {
                    pairs = Vec::<(SomeState, &SomeSquare)>::new();
                    num_samples = sqr2.len_results();
                }
                if sqr2.len_results() == num_samples {
                    pairs.push((sta2, sqrx));
                }
            } else {
                if num_samples == 0 {
                    pairs.push((sta2, sqrx));
                }
            }
        }

        // Check for defining squares
        if pnc {
            for pairx in pairs.iter() {
                let sqr2 = self.squares.find(&pairx.0).unwrap();
                if sqr2.pnc {
                    let rules;
                    if sqr2.pn == Pn::Unpredictable {
                        rules = RuleStore::new();
                    } else {
                        rules = sqr2.rules.union(&pairx.1.rules).unwrap();
                    }
                    nds.push(SomeNeed::AddGroup {
                        group_region: SomeRegion::new(&sqr2.state, &pairx.1.state),
                        rules,
                    });
                    return nds;
                }
            }
        }

        // Get more samples
        for pairx in pairs.iter() {
            if pairx.1.pnc == false {
                nds.push(SomeNeed::AStateMakeGroup {
                    dom_num: 0, // Will be set later.
                    act_num: self.num,
                    targ_state: pairx.1.state.clone(),
                    for_reg: regx.clone(),
                    far: pairx.0.clone(),
                    num_x: regx.num_x(),
                });
            }
            if let Some(sqr2) = self.squares.find(&pairx.0) {
                if sqr2.pnc == false {
                    nds.push(SomeNeed::AStateMakeGroup {
                        dom_num: 0, // Will be set later.
                        act_num: self.num,
                        targ_state: pairx.0.clone(),
                        for_reg: regx.clone(),
                        far: pairx.1.state.clone(),
                        num_x: regx.num_x(),
                    });
                }
            } else {
                nds.push(SomeNeed::AStateMakeGroup {
                    dom_num: 0, // Will be set later.
                    act_num: self.num,
                    targ_state: pairx.0.clone(),
                    for_reg: regx.clone(),
                    far: pairx.1.state.clone(),
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
            if self.no_incompatible_pn_square_in_region(&reg_both, Pn::Unpredictable) {
                return self.region_defining_needs(&reg_both, &grpx.region, &grpy.region);
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
        if self.no_incompatible_pn_square_in_region(&reg_both, grpx.pn) {
            if self.no_incompatible_square_pair_in_region(&reg_both) {
                return self.region_defining_needs(&reg_both, &grpx.region, &grpy.region);
            }
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
        //println!("cont_int_region_needs {} for grp {} {} and grp {} {}", &regx, &grpx.region, &grpx.rules, &grpy.region, &grpy.rules);
        // Check for any squares in the region
        let stas_in = self.squares.stas_in_reg(regx);

        if stas_in.len() == 0 {
            let ruls1;
            if grpx.rules.len() == 0 {
                ruls1 = RuleStore::new();
            } else {
                ruls1 = grpx.rules.restrict_initial_region(regx);
            }

            let ruls2;
            if grpy.rules.len() == 0 {
                ruls2 = RuleStore::new();
            } else {
                ruls2 = grpy.rules.restrict_initial_region(regx);
            }

            return SomeNeed::ContradictoryIntersection {
                dom_num: 0, // set this in domain get_needs
                act_num: self.num,
                goal_reg: regx.clone(),
                group1: grpx.region.clone(),
                ruls1,
                group2: grpy.region.clone(),
                ruls2,
            };
        }

        // Some samples have been taken in the region

        // Find a square with the highest number of samples
        // If any are pnc, panic.
        let mut max_rslts = 0;
        let mut stas_check = StateStore::new();
        for stax in stas_in.iter() {
            let sqrz = self.squares.find(stax).unwrap();
            if sqrz.pnc {
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

        let ruls1;
        if grpx.rules.len() == 0 {
            ruls1 = RuleStore::new();
        } else {
            ruls1 = grpx.rules.restrict_initial_region(regx);
        }

        let ruls2;
        if grpy.rules.len() == 0 {
            ruls2 = RuleStore::new();
        } else {
            ruls2 = grpy.rules.restrict_initial_region(regx);
        }

        SomeNeed::ContradictoryIntersection {
            dom_num: 0, // set this in domain get_needs
            act_num: self.num,
            goal_reg: SomeRegion::new(&stas_check[inx], &stas_check[inx]),
            group1: grpx.region.clone(),
            ruls1,
            group2: grpy.region.clone(),
            ruls2,
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

            if sqrx.pn != sqry.pn {
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

            if self.can_combine(&sqrx, &sqry) != Truth::T {
                continue;
            }

            rsx.push_nosubs(regx);
        } // end for

        // Print possible regions
        for regx in rsx.iter() {
            let sqry = self.squares.find(&regx.state2).unwrap();
            if sqry.pn == Pn::Unpredictable {
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

    /// Take an action with the current state.
    pub fn take_action(&mut self, dom: usize, cur_state: &SomeState) -> SomeState {
        self.do_something.take_action(dom, self.num, cur_state)
    }
} // end impl SomeAction
