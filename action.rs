//! The SomeAction struct.
//!
//! Take an action, and store the (domain current state) initial->result sample.
//! Generate the current best-guess rules of the expected responses
//! of executing an action for a given state.
//!
//! Return needs for more samples, to improve the understanding of the action.
//!
//! Return a list of steps that will make any one, of any number (> 0), of needed bit changes.
//! For making a plan (series of actions) to change the domain current state to a different, desired, value.
//!

use crate::actioninterface::ActionInterface;
use crate::change::SomeChange;
use crate::group::SomeGroup;
use crate::groupstore::GroupStore;
use crate::mask::SomeMask;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::square::SomeSquare;
use crate::squarestore::SquareStore;
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing

/// Number of new squares added before a cleanup check is run.
const CLEANUP: usize = 5;

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("A(ID: ");

        rc_str += &self.num.to_string();

        rc_str += ", number squares: ";
        rc_str += &self.squares.len().to_string();

        if !self.seek_edge.is_empty() {
            let _ = write!(rc_str, ", seek_edge within: {}", self.seek_edge);
        }

        let regs = self.groups.regions();

        let mut fil = ",\n       Grps: ";
        for grpx in self.groups.iter() {
            let stas_in = self.squares.stas_in_reg(&grpx.region);

            let cnt: usize = stas_in
                .iter()
                .map(|x| usize::from(regs.state_in_1_region(x)))
                .sum();

            let _ = write!(
                rc_str,
                "{}{} num Sqrs: {} in1: {})",
                &fil,
                &grpx.formatted_string(),
                &stas_in.len(),
                &cnt,
            );

            fil = ",\n             ";
        }

        rc_str.push(')');

        write!(f, "{rc_str}")
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
/// The SomeAction struct, aggregate the best current guess at what an action
/// will do for any state.
pub struct SomeAction {
    /// Action number, index/key into parent ActionStore vector.
    pub num: usize,
    /// Parent Domain number.
    pub dom_num: usize,
    /// Store for groups of compatible-change squares.
    pub groups: GroupStore,
    /// A store of squares sampled for an action.
    pub squares: SquareStore,
    /// Regions of invalidated groups indicate a new edge of the solution.
    /// Closer and closer dissimilar squares are sought, producing smaller and smaller
    /// regions, until a pair of adjacent, dissimilar, squares are found.
    pub seek_edge: RegionStore,
    /// Interface to an action that does something.
    /// Storing a function pointer runs into problems with the parallel crate
    /// and the serialization crate.
    do_something: ActionInterface,
    /// Trigger cleanup logic after a number of new squares.
    cleanup_trigger: usize,
}

impl SomeAction {
    /// Return a new SomeAction struct, given the number integers used in the SomeBits struct.
    /// The action number, an index into the ActionStore that will contain it, is set to zero and
    /// changed later.
    pub fn new(dom_num: usize, act_num: usize, num_ints: usize) -> Self {
        assert!(num_ints > 0);
        SomeAction {
            num: act_num,
            dom_num,
            groups: GroupStore::new(num_ints),
            squares: SquareStore::new(),
            seek_edge: RegionStore::new(),
            do_something: ActionInterface::new(dom_num, act_num),
            cleanup_trigger: CLEANUP,
        }
    }

    /// Return the truth value for the combination of any two different squares,
    /// and the squares between them.
    fn can_combine(&self, sqrx: &SomeSquare, sqry: &SomeSquare) -> Option<bool> {
        assert!(sqrx.state != sqry.state);
        //println!("can_combine: {} and {}?", sqrx.state, sqry.state);

        let cmbx = sqrx.can_combine(sqry);

        if Some(false) == cmbx {
            return cmbx;
        }

        // Check if Pn::One squares cannot combine as-is, for bootstrapping.
        if sqrx.pn == Pn::One && sqry.pn == Pn::One && cmbx.is_none() {
            return Some(false);
        }

        if sqrx.pn == sqry.pn {
            if !self.no_incompatible_pn_square_in_region(
                &SomeRegion::new(sqrx.state.clone(), sqry.state.clone()),
                sqrx.pn,
            ) {
                return Some(false);
            }
            if sqrx.pn == Pn::Unpredictable {
                return Some(true);
            }

            let Some(rules) = sqrx.rules.union(&sqry.rules) else { return Some(false); };

            if !self.all_subset_rules_in_region(
                &SomeRegion::new(sqrx.state.clone(), sqry.state.clone()),
                &rules,
            ) {
                return Some(false);
            }
        } else if !self.no_incompatible_square_combination_in_region(&SomeRegion::new(
            sqrx.state.clone(),
            sqry.state.clone(),
        )) {
            return Some(false);
        }

        cmbx
    }

    /// Store and evaluate a sample if it forms a new square,
    /// or causes a change to an existing square (change in pn or pnc).
    pub fn eval_sample(&mut self, smpl: &SomeSample) {
        if self.store_sample(smpl) {
            self.check_square_new_sample(smpl);
            self.eval_sample_check_anchor(smpl);
        }
    }

    /// Check for anchor change due to a square change.
    /// A new square (in group X) may have a higher anchor rating
    /// than the current (group X) anchor.
    /// An square must exist to set an anchor.
    fn eval_sample_check_anchor(&mut self, smpl: &SomeSample) {
        if self.groups.num_groups_state_in(&smpl.initial) != 1 {
            return;
        }

        let Some(sqrx) = self.squares.find(&smpl.initial) else { println!("eval_sample_check_anchor: 1: This should not happen"); return; };

        if sqrx.results.len() != 1 {
            return;
        }

        let grps_in: RegionStore = self.groups.groups_state_in(&smpl.initial);

        let Some(grpx) = self.groups.find(&grps_in[0]) else { println!("eval_sample_check_anchor: 2: This should not happen"); return; };

        let Some(anchor) = &grpx.anchor else { return; };

        if anchor == &smpl.initial {
            return;
        }

        let anchor_states: Vec<&SomeState> = self.groups.anchor_states();

        // The current anchor rate may have changed, so recalculate it.
        let anchor_rate = self.group_anchor_rate(grpx, anchor, &anchor_states);

        let sqr_rate = self.group_anchor_rate(grpx, &smpl.initial, &anchor_states);

        if sqr_rate > anchor_rate {
            println!(
                "Changing group {} anchor from {} {:?} to {} {:?}",
                grpx.region, anchor, anchor_rate, smpl.initial, sqr_rate
            );

            self.groups.set_anchor(&grps_in[0], &smpl.initial);
        }
    } // end eval_sample_check_anchors

    /// Evaluate a sample taken to satisfy a need.
    pub fn eval_need_sample(&mut self, ndx: &SomeNeed, dom: usize, smpl: &SomeSample) {
        // Processing for all samples that will be stored.
        self.eval_sample(smpl);

        // Additional processing for selected kinds of need
        match ndx {
            SomeNeed::AStateMakeGroup {
                target_state: sta,
                for_reg,
                far,
                ..
            } => {
                if self.groups.any_superset_of(for_reg) {
                    return;
                }

                // Form the rules, make the group
                // If the squares are incompatible, or need more samples, skip action.
                let sqrx = self.squares.find(sta).unwrap();

                if sqrx.pn == Pn::One || sqrx.pnc {
                    //println!("AStateMakeGroup: sqr {} sampled, pn {} pnc {}", &sqrx, sqrx.pn, sqrx.pnc);
                    let Some(sqry) = self.squares.find(far) else { return; };

                    if (sqry.pn == Pn::One || sqry.pnc)
                        && (sqrx.state == sqry.state || self.can_combine(sqrx, sqry) == Some(true))
                    {
                        let regz = SomeRegion::new(sqrx.state.clone(), sqry.state.clone());

                        // Generate group rules.
                        let rulsxy = if sqrx.pn == Pn::Unpredictable {
                            RuleStore::new()
                        } else {
                            sqrx.rules.union(&sqry.rules).unwrap()
                        };

                        self.groups.push(
                            SomeGroup::new(regz, rulsxy, sqrx.pnc && sqry.pnc),
                            dom,
                            self.num,
                        );
                    } // end if sqry.pn ==
                } // end if sqrx.pn ==
            } // end process AStateMakeGroup Need

            SomeNeed::SeekEdge {
                target_state: sta,
                in_group: greg,
                ..
            } => {
                assert!(greg.state1 != greg.state2);

                if *sta != greg.state1 && *sta != greg.state2 {
                    let mut make_groups_from = Vec::<SomeState>::new();

                    // Find the squares
                    let sqr1 = self.squares.find(&greg.state1).unwrap();
                    let sqr2 = self.squares.find(&greg.state2).unwrap();
                    let sqr3 = self.squares.find(sta).unwrap();

                    // Process next sample of square in-between for new square and state1 square.
                    // Should be different from state1 square or state2 square.
                    // It may be different from both state1 and state2.
                    // If it needs more samples, skip, next need will increment the number samples.

                    if sqr3.can_combine(sqr1) == Some(false) && sqr1.pnc && sqr3.pnc {
                        if sqr1.is_adjacent(sqr3) {
                            println!("\nDom {} Act {} new edge found between {} and {} removing seek edge {}", dom, self.num, &sqr1.state, &sqr3.state, &greg);
                            self.seek_edge.remove_region(greg);
                            make_groups_from.push(sqr1.state.clone());
                            make_groups_from.push(sqr3.state.clone());
                        } else {
                            let even_closer_reg =
                                SomeRegion::new(sqr1.state.clone(), sqr3.state.clone());
                            println!(
                                "\nDom {} Act {} replace seek edge {} with {}",
                                dom, self.num, &greg, &even_closer_reg
                            );
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }

                    if sqr3.can_combine(sqr2) == Some(false) && sqr2.pnc && sqr3.pnc {
                        if sqr2.is_adjacent(sqr3) {
                            println!("\nDom {} Act {} new edge found between {} and {} removing seek edge {}", dom, self.num, &sqr2.state, &sqr3.state, &greg);
                            self.seek_edge.remove_region(greg);
                            make_groups_from.push(sqr2.state.clone());
                            if !make_groups_from.contains(&sqr3.state) {
                                make_groups_from.push(sqr3.state.clone());
                            }
                        } else {
                            let even_closer_reg =
                                SomeRegion::new(sqr2.state.clone(), sqr3.state.clone());
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }

                    // Create a group for square if needed
                    if !make_groups_from.is_empty() {
                        for stax in make_groups_from {
                            self.create_groups_from_square(&stax);
                        }
                    }
                }
            } // end match SeekEdgeNeed

            SomeNeed::ConfirmGroup { grp_reg, .. } => {
                let Some(sqr1) = self.squares.find(&grp_reg.state1) else { return; };
                let Some(sqr2) = self.squares.find(&grp_reg.state2) else { return; };

                if sqr1.pnc && sqr2.pnc {
                    self.set_group_pnc(grp_reg);
                }
            }

            _ => (),
        } // end match ndx
    } // end eval_need_sample

    /// Check a group for pnc, set it if needed
    pub fn set_group_pnc(&mut self, grp_reg: &SomeRegion) {
        let Some(grpx) = self.groups.find_mut(grp_reg) else { println!("ConfirmGroup {grp_reg} group not found?"); return; };

        if grpx.pnc {
            println!(
                "ConfirmGroup {} already pnc and ConfirmGroup need?",
                grpx.region
            );
            return;
        }

        let Some(sqr1) = self.squares.find(&grpx.region.state1) else { panic!(
                "ConfirmGroup {} state1 {} square not found?",
                grpx.region, grpx.region.state1
            ); };

        if !sqr1.pnc {
            return;
        }

        if grpx.region.state1 == grpx.region.state2 {
            grpx.set_pnc();
            return;
        }

        let Some(sqr2) = self.squares.find(&grpx.region.state2) else {
            panic!(
                "ConfirmGroup {} state2 {} square not found?",
                grpx.region, grpx.region.state2
            ); };

        if sqr2.pnc {
            grpx.set_pnc();
        }
    }

    /// Set a group anchor.
    pub fn set_group_anchor(&mut self, grp_reg: &SomeRegion, anchor: &SomeState) {
        let Some(grpx) = self.groups.find_mut(grp_reg) else {
                println!("set_group_anchor {grp_reg} group not found?");
                return;
            };

        if grpx.pnc {
            grpx.set_anchor(anchor);
        } else {
            println!("set_group_anchor {} not pnc?", grpx.region);
        }
    }

    /// Evaluate the sample taken for a step in a plan.
    /// If a square is currently stored for the sample, update the square.
    /// If the square invalidates any group, store a new square.
    /// In most cases, if the sample is as expected, no new square will be stored.
    fn eval_step_sample(&mut self, smpl: &SomeSample) {
        // If a square exists, update it.
        if let Some(sqrx) = self.squares.find_mut(&smpl.initial) {
            // println!("about to add result to sqr {}", cur.str());
            if sqrx.add_result(smpl.result.clone()) {
                self.check_square_new_sample(smpl);
            }
            return;
        }

        // Check if any groups are invalidated
        if self.groups.any_groups_invalidated(smpl) {
            self.eval_sample(smpl);
        }
    } // end eval_step_sample

    /// Store a sample.
    /// Update an existing square or create a new square.
    /// Return true if something changed, like pn or pnc.
    fn store_sample(&mut self, smpl: &SomeSample) -> bool {
        // Get an existing square and update, or create a new square.
        //println!("store_sample");
        if let Some(sqrx) = self.squares.find_mut(&smpl.initial) {
            // println!("about to add result to sqr {}", cur.str());
            return sqrx.add_result(smpl.result.clone());
        }

        // println!("No square found for state {}", cur.str());
        if self.cleanup_trigger > 0 {
            self.cleanup_trigger -= 1;
        }
        self.squares.insert(
            SomeSquare::new(smpl.initial.clone(), smpl.result.clone()),
            self.dom_num,
            self.num,
        );
        true
    }

    /// Check a square, referenced by state, against valid groups.
    /// The square may invalidate some groups.
    /// Add a group for the square if the square is in no valid group.
    /// If any groups were invalidated, check for any other squares
    /// that are in no groups.
    fn check_square_new_sample(&mut self, smpl: &SomeSample) {
        //println!("check_square_new_sample for {}", key);

        let sqrx = self.squares.find(&smpl.initial).unwrap();

        // Get groups invalidated, which may orphan some squares.
        //let regs_invalid = self.validate_groups_new_sample(&key);
        let regs_invalid: RegionStore = self.groups.check_square(sqrx, self.dom_num, self.num);

        // Save regions invalidated to seek new edges.
        for regx in regs_invalid.iter() {
            if smpl.initial != regx.state1 && smpl.initial != regx.state2 {
                let Some(sqr1) = self.squares.find(&regx.state1) else {
                    panic!("Can't find group defining square?");
                };
                let Some(sqr2) = self.squares.find(&regx.state2) else {
                    panic!("Can't find group defining square?");
                };

                if sqr1.pnc && sqr2.pnc {
                    if !sqrx.state.is_adjacent(&sqr1.state) && sqrx.can_combine(sqr1) == Some(false)
                    {
                        println!(
                            "\nDom {} Act {} Seek edge between {} and {}",
                            &self.dom_num, &self.num, &sqrx.state, &sqr1.state
                        );
                        self.seek_edge
                            .push_nosubs(SomeRegion::new(sqrx.state.clone(), sqr1.state.clone()));
                    }

                    if !sqrx.state.is_adjacent(&sqr2.state) && sqrx.can_combine(sqr2) == Some(false)
                    {
                        println!(
                            "\nDom {} Act {} Seek edge between {} and {}",
                            &self.dom_num, &self.num, &sqrx.state, &sqr2.state
                        );
                        self.seek_edge
                            .push_nosubs(SomeRegion::new(sqrx.state.clone(), sqr2.state.clone()));
                    }
                }
            }
        }

        // Check for any effects on group limited setting
        for grpx in self.groups.iter_mut() {
            if !grpx.limited {
                continue;
            }

            let Some(anchor_state) = &grpx.anchor else { continue; };

            if !anchor_state.is_adjacent(&sqrx.state) {
                continue;
            };

            let Some(anchor_sqr) = self.squares.find(anchor_state) else {
                panic!("Anchor state not found?"); };

            if anchor_sqr.can_combine(sqrx) == Some(true) {
                grpx.set_limited_off();
            }
        }

        // Create a group for square if needed
        let grps_in = self.groups.groups_state_in(&smpl.initial);
        if grps_in.is_empty() || (grps_in.len() == 1 && (grps_in[0].x_mask().is_low())) {
            self.create_groups_from_square(&smpl.initial);
        }

        if regs_invalid.is_empty() {
            return;
        }

        // Check squares that may not be in a group

        let regs = self.groups.regions();

        let keys = self.squares.not_in_regions(&regs);

        for keyx in keys.iter() {
            // A later square may be in a group created by an earlier square
            if self.groups.num_groups_state_in(keyx) == 0 {
                self.create_groups_from_square(keyx);
            }
        }
    } // end check_square_new_sample

    /// Check groups due to a new, or updated, square.
    /// Create a group with the square, if needed.
    fn create_groups_from_square(&mut self, key: &SomeState) {
        //println!("create_groups_from_square {}", &key);

        // Square should exist
        let sqrx = self.squares.find(key).unwrap();

        // Check if square can be used to create groups
        // Allowing a square to make a group with a single sample is needed
        // for group bootstrapping.
        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return;
        }

        // Get num groups the state is in
        let num_grps_in = self.groups.num_groups_state_in(key);
        println!(
            "\nDom {} Act {} Square {} in {} groups",
            self.dom_num,
            self.num,
            sqrx.str_terse(),
            num_grps_in,
        );

        //println!("Checking Square {} for new groups", &sqrx.str_terse());

        // Get possible regions, sqrx.state will be <region>.state1
        let rsx: RegionStore = self.possible_regions_from_square(sqrx);

        let mut group_added = false;
        for regx in rsx.iter() {
            let Some(sqrx) = self.squares.find(&regx.state1) else { panic!("Region state square not found?"); };

            let Some(sqry) = self.squares.find(&regx.state2) else { panic!("Region state square not found?"); };

            let ruls = if sqrx.pn == Pn::Unpredictable {
                RuleStore::new()
            } else {
                sqrx.rules.union(&sqry.rules).unwrap()
            };

            self.groups.push(
                SomeGroup::new(regx.clone(), ruls, sqrx.pnc && sqry.pnc),
                self.dom_num,
                self.num,
            );
            group_added = true;
        } // next regx

        if group_added || num_grps_in > 0 {
            return;
        }

        // Make a single-square group
        let regz = SomeRegion::new(sqrx.state.clone(), sqrx.state.clone());
        self.groups.push(
            SomeGroup::new(regz, sqrx.rules.clone(), sqrx.pnc),
            self.dom_num,
            self.num,
        );
    } // end create_groups_from_square

    /// Return needs for states that are not in a group.
    /// The Domain current state for which there are no samples.
    /// A pn > 1 state that needs more samples.
    pub fn state_not_in_group_needs(
        &self,
        cur_state: &SomeState,
        memory: &VecDeque<SomeSample>,
    ) -> NeedStore {
        let mut nds = NeedStore::new();

        // Check if current state is in any groups
        if !self.groups.any_superset_of_state(cur_state) {
            nds.push(SomeNeed::StateNotInGroup {
                dom_num: 0, // will be set later
                act_num: self.num,
                target_state: cur_state.clone(),
            });
        }

        // Check memory
        for stax in memory.iter() {
            if &stax.initial == cur_state {
                continue;
            }
            if !self.groups.any_superset_of_state(&stax.initial) {
                nds.push(SomeNeed::StateNotInGroup {
                    dom_num: 0, // will be set later
                    act_num: self.num,
                    target_state: stax.initial.clone(),
                });
            }
        } // next stax

        // Look for a pn > 1, pnc == false, not in group squares
        // Extra samples are needed to gain pnc, then the first group.
        let sqrs_pngt1 = self.squares.pn_gt1_no_pnc();

        for stax in sqrs_pngt1.iter() {
            if stax == cur_state || self.groups.any_superset_of_state(stax) {
                continue;
            }

            nds.push(SomeNeed::StateNotInGroup {
                dom_num: 0, // will be set later
                act_num: self.num,
                target_state: stax.clone(),
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
        memory: &VecDeque<SomeSample>,
        agg_changes: &SomeChange,
    ) -> NeedStore {
        //println!("Running Action {}::get_needs {}", self.num, cur_state);

        // loop until no housekeeping need is returned.
        let mut nds = NeedStore::new();
        let mut cnt = 0;
        loop {
            cnt += 1;

            // Look for needs to find a new edge in an invalidated group
            let mut ndx = self.seek_edge_needs();
            if !ndx.is_empty() {
                nds.append(&mut ndx);
            }

            // Check for additional samples for group states needs
            let mut ndx = self.confirm_group_needs();
            if !ndx.is_empty() {
                nds.append(&mut ndx);
            }

            // Check any two groups for:
            // Overlapping regions that may be combined.
            // Overlapping groups that form a contradictory intersection.
            let mut ndx = self.group_pair_needs();
            //println!("Ran group_pair_needs");
            if !ndx.is_empty() {
                nds.append(&mut ndx);
            }

            // Check for squares in-one-group needs
            let mut ndx = self.limit_groups_needs(agg_changes);
            if !ndx.is_empty() {
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
                        new_grp_regs.push_nosubs(group_region.clone())
                    }
                    _ => continue,
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
                        if !new_grp_regs.contains(group_region) {
                            continue;
                        }

                        // Check for supersets
                        if self.groups.any_superset_of(group_region) {
                            if self.groups.find(group_region).is_some() {
                            } else {
                                println!(
                                    "\nDom {} Act {} **** Supersets found for new group {} in {}",
                                    dom,
                                    self.num,
                                    &group_region,
                                    self.groups.supersets_of(group_region)
                                );
                            }
                            continue;
                        }

                        // Calc pnc
                        let sqrx = self.squares.find(&group_region.state1).unwrap();
                        let pnc = if group_region.state2 == group_region.state1 {
                            sqrx.pnc
                        } else {
                            let sqry = self.squares.find(&group_region.state2).unwrap();
                            sqrx.pnc && sqry.pnc
                        };

                        self.groups.push(
                            SomeGroup::new(group_region.clone(), rules.clone(), pnc),
                            dom,
                            self.num,
                        );
                        try_again = true;
                    }
                    SomeNeed::SetGroupLimited { group_region: greg } => {
                        if let Some(grpx) = self.groups.find_mut(greg) {
                            println!("\nDom {} Act {} Group {} limited", dom, self.num, greg);
                            grpx.set_limited();
                        }
                    }
                    SomeNeed::SetGroupAnchor {
                        group_region: greg,
                        anchor: sta1,
                    } => {
                        let Some(grpx) = self.groups.find_mut(greg) else { continue; };

                        if let Some(anchor) = &grpx.anchor {
                            println!(
                                "\nDom {} Act {} Group {} setting anchor from {} to {}",
                                dom, self.num, greg, anchor, sta1
                            );
                        } else {
                            println!(
                                "\nDom {} Act {} Group {} setting anchor to {}",
                                dom, self.num, greg, sta1
                            );
                        }
                        grpx.set_anchor(sta1);
                        try_again = true;
                        break; // Only set one anchor for any group of needs to avoid loop with two changes at a time.
                    }
                    SomeNeed::RemoveGroupAnchor { group_region: greg } => {
                        let Some(grpx) = self.groups.find_mut(greg) else { continue; };

                        println!(
                            "\nDom {} Act {} Group {} remove anchor",
                            dom, self.num, greg
                        );
                        try_again = true;
                        grpx.set_anchor_off();
                    }
                    SomeNeed::InactivateSeekEdge { reg: regx } => {
                        println!("\nDom {} Act {} remove seek edge {}", dom, self.num, regx);
                        self.seek_edge.remove_region(regx);
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

            if !try_again {
                //println!("Act: {} get_needs: returning: {}", &self.num, &nds);
                //                if nds.len() == 0 {
                //                    return self.left_over_needs();
                //                }

                // Filter out housekeeping needs, if any.
                let mut inxs = Vec::<usize>::with_capacity(nds.len());

                for (inx, ndx) in nds.iter().enumerate() {
                    match ndx {
                        SomeNeed::AddGroup { .. } => {
                            inxs.push(inx);
                        }
                        SomeNeed::SetGroupLimited { .. } => {
                            inxs.push(inx);
                        }
                        SomeNeed::InactivateSeekEdge { .. } => {
                            inxs.push(inx);
                        }
                        _ => (),
                    }
                }

                // Remove houskeeping needs, from highest to lowest index.
                for inx in inxs.iter().rev() {
                    nds.remove_unordered(*inx);
                }

                // Do cleanup
                if self.cleanup_trigger == 0 {
                    self.cleanup(&nds);
                    self.cleanup_trigger = CLEANUP;
                }

                // Checks that will not return housekeeping needs

                // Look for needs for states not in groups
                let mut ndx = self.state_not_in_group_needs(cur_state, memory);
                if !ndx.is_empty() {
                    nds.append(&mut ndx);
                }

                return nds;
            }

            nds = NeedStore::new();
        } // end loop
    } // end get_needs

    /// Cleanup, that is delete unneeded squares
    fn cleanup(&mut self, needs: &NeedStore) {
        let mut first_del = true;

        // Don't delete squares currently in needs.
        'next_stax: for stax in self.squares.all_square_keys() {
            // Check needs
            for ndx in needs.iter() {
                for targx in ndx.target().iter() {
                    if targx.is_superset_of_state(&stax) {
                        continue 'next_stax;
                    }
                }
            }

            // Don't delete squares in groups.
            // let mut in_groups = false;
            for grpx in self.groups.iter() {
                if grpx.region.is_superset_of_state(&stax) {
                    if grpx.region.state1 == stax || grpx.region.state2 == stax {
                        continue 'next_stax;
                    }

                    if let Some(stay) = &grpx.anchor {
                        if *stay == stax {
                            continue 'next_stax;
                        }
                        if stax == grpx.region.far_state(stay) {
                            continue 'next_stax;
                        }
                    }
                } else if let Some(stay) = &grpx.anchor {
                    if stax.is_adjacent(stay) {
                        continue 'next_stax;
                    }
                }
            }

            // Don't delete squares in seek edge regions.
            for regx in self.seek_edge.iter() {
                if regx.is_superset_of_state(&stax) {
                    continue 'next_stax;
                }
            }

            // Don't delete squares that are not in a group.
            // That is, squares with Pn: > One that need more samples.
            if self.groups.num_groups_state_in(&stax) == 0 {
                continue;
            }

            // Display and remove.
            if first_del {
                first_del = false;
                print!(
                    "\nDom {} Act {} deleted unneeded squares: ",
                    self.dom_num, self.num
                );
            }

            print!(" {stax}");
            self.squares.remove(&stax);
        } // next stax

        if !first_del {
            println!(" ");
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

        'next_regx: for regx in self.seek_edge.iter() {
            //print!("seek_edge_needs: checking reg {} ", &regx);
            // Get the squares represented by the states that form the region
            let sqr1 = self.squares.find(&regx.state1).unwrap();
            let sqr2 = self.squares.find(&regx.state2).unwrap();

            // Check that squares that define the region are pnc.
            if !sqr1.pnc {
                //print!("get more samples of square {} ", &sqr1.state);
                ret_nds.push(SomeNeed::SeekEdge {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    target_state: sqr1.state.clone(),
                    in_group: regx.clone(),
                });
                continue;
            }
            if !sqr2.pnc {
                //print!("get more samples of square {} ", &sqr2.state);
                ret_nds.push(SomeNeed::SeekEdge {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    target_state: sqr2.state.clone(),
                    in_group: regx.clone(),
                });
                continue;
            }

            // Get squares between defining states.
            let sqrs_in = self.squares.squares_in_reg(regx);

            // No squares between
            if sqrs_in.len() == 2 {
                let mut ndx = self.seek_edge_needs2(regx);
                if !ndx.is_empty() {
                    ret_nds.append(&mut ndx);
                }
                continue;
            }

            let mut sqrs_in2 = Vec::<&SomeSquare>::with_capacity(sqrs_in.len() - 2);
            for sqrx in &sqrs_in {
                if sqrx.state != sqr1.state && sqrx.state != sqr2.state {
                    sqrs_in2.push(sqrx);
                }
            }

            // Look for a square with pnc == true.
            for sqrx in &sqrs_in2 {
                if sqrx.pnc {
                    let cnb1 = sqrx.can_combine(sqr1);
                    let cnb2 = sqrx.can_combine(sqr2);

                    if cnb1 == Some(true) && cnb2 == Some(true) {
                        ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                    } else {
                        if cnb1 == Some(false) {
                            new_regs.push_nosups(SomeRegion::new(
                                sqrx.state.clone(),
                                sqr1.state.clone(),
                            ));
                        }
                        if cnb2 == Some(false) {
                            new_regs.push_nosups(SomeRegion::new(
                                sqrx.state.clone(),
                                sqr2.state.clone(),
                            ));
                        }
                    }
                    continue 'next_regx;
                }
            }

            // Get max samples for any square.
            let mut max_len = 0;
            for sqrx in &sqrs_in2 {
                if sqrx.len_results() > max_len {
                    max_len = sqrx.len_results();
                }
            }

            // Generate need for squares with pnc != true, max samples.
            for sqrx in &sqrs_in2 {
                if sqrx.len_results() == max_len {
                    ret_nds.push(SomeNeed::SeekEdge {
                        dom_num: 0, // set this in domain get_needs
                        act_num: self.num,
                        target_state: sqrx.state.clone(),
                        in_group: regx.clone(),
                    });
                }
            }

            println!(" ");
        } // next regx

        // Apply new seek edge regions
        // After get_needs does the housekeeping, it will run this again
        if !new_regs.is_empty() {
            ret_nds = NeedStore::new();
            for regx in new_regs.iter() {
                ret_nds.push(SomeNeed::AddSeekEdge { reg: regx.clone() });
            }
        }

        ret_nds
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
        let seek_state = if rand::random::<bool>() {
            regx.state1.bitwise_xor(&dif_msk)
        } else {
            regx.state2.bitwise_xor(&dif_msk)
        };

        // Make need for seek_state
        //print!("get first sample of square {}", &seek_state);
        ret_nds.push(SomeNeed::SeekEdge {
            dom_num: 0, // set this in domain get_needs
            act_num: self.num,
            target_state: seek_state,
            in_group: regx.clone(),
        });
        //println!(" ");

        ret_nds
    } // end seek_edge_needs2

    /// Get additional sample needs for the states that form a group.
    /// Should only affect groups with Pn::One.
    /// Groups closer to the beginning of the group will have priority due to lower group number.
    pub fn confirm_group_needs(&mut self) -> NeedStore {
        //println!("confirm_group_needs");
        let mut ret_nds = NeedStore::new();

        for (group_num, grpx) in self.groups.iter_mut().enumerate() {
            if grpx.pnc {
                continue;
            }

            let sqrx = self.squares.find(&grpx.region.state1).unwrap();
            if !sqrx.pnc {
                ret_nds.push(SomeNeed::ConfirmGroup {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    target_state: grpx.region.state1.clone(),
                    grp_reg: grpx.region.clone(),
                    group_num,
                });
            }

            // If this is a one-state group ..
            if grpx.region.state1 == grpx.region.state2 {
                if sqrx.pnc {
                    grpx.set_pnc();
                }
                continue;
            }

            let sqry = self.squares.find(&grpx.region.state2).unwrap();
            if !sqry.pnc {
                ret_nds.push(SomeNeed::ConfirmGroup {
                    dom_num: 0, // set this in domain get_needs
                    act_num: self.num,
                    target_state: grpx.region.state2.clone(),
                    grp_reg: grpx.region.clone(),
                    group_num,
                });
            }

            // Group may have become pnc due to a sample from running a plan, rather than a specific
            // need, which is handled in eval_need_sample.
            if sqrx.pnc && sqry.pnc {
                grpx.set_pnc();
            }
        } // next grpx

        ret_nds
    } // end confirm_group_needs

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
    pub fn limit_groups_needs(&self, agg_changes: &SomeChange) -> NeedStore {
        //println!("limit_groups_needs chg {}", changes_mask);

        let mut ret_nds = NeedStore::new();

        let mut anchors = Vec::<&SomeState>::new();

        // Check groups current anchors are still in only one region,
        for grpx in self.groups.iter() {
            let Some(stax) = &grpx.anchor else { continue; };

            anchors.push(stax);

            if self.groups.num_groups_state_in(stax) != 1 {
                ret_nds.push(SomeNeed::RemoveGroupAnchor {
                    group_region: grpx.region.clone(),
                });
            }
        }
        if !ret_nds.is_empty() {
            return ret_nds;
        }

        // Get anchor needs.
        for (group_num, grpx) in self.groups.iter().enumerate() {
            if !grpx.pnc {
                continue;
            }

            let mut ndx = self.limit_group_anchor_needs(grpx, &anchors, group_num);

            if ndx.is_empty() {
                if let Some(anchor) = &grpx.anchor {
                    if !grpx.limited {
                        ndx = self.limit_group_adj_needs(grpx, anchor, agg_changes, group_num);
                    }
                }
            }
            if !ndx.is_empty() {
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
    fn group_anchor_rate(
        &self,
        grpx: &SomeGroup,
        stax: &SomeState,
        anchors: &[&SomeState],
    ) -> (usize, usize, usize) {
        assert!(self.groups.num_groups_state_in(stax) == 1);

        let mut sta_rate = (0, 0, 0);

        if let Some(sqrx) = self.squares.find(stax) {
            sta_rate.2 += sqrx.rate();
        }

        // Rate far state
        let sta_far = grpx.region.far_state(stax);
        if let Some(sqrx) = self.squares.find(&sta_far) {
            sta_rate.2 += sqrx.rate();
        }

        // Get masks of edge bits to use to limit group.
        let edge_msks = grpx.region.same_bits().split();

        // Rate adjacent external states
        for edge_bit in &edge_msks {
            let sta_adj = stax.bitwise_xor(edge_bit);
            //println!(
            //    "checking {} adjacent to {} external to {}",
            //    &sta_adj, &stax, &greg
            //);

            if anchors.contains(&&sta_adj) {
                sta_rate.0 += 1;
                sta_rate.1 += 1;
            } else if self.groups.num_groups_state_in(&sta_adj) == 1 {
                //println!("{} is in only one group", &sta_adj);
                sta_rate.1 += 1;
            }

            if let Some(sqrx) = self.squares.find(&sta_adj) {
                sta_rate.2 += sqrx.rate();
            }
        } // next edge_bit

        sta_rate
    }

    /// Return the limiting anchor needs for a group.
    /// If no state in the group is in only one group, return an empty NeedStore.
    /// If an existing anchor has the same, or better, rating than other possible states,
    /// return an empty NeedStore.
    pub fn limit_group_anchor_needs(
        &self,
        grpx: &SomeGroup,
        anchors: &[&SomeState],
        group_num: usize,
    ) -> NeedStore {
        let mut ret_nds = NeedStore::new();

        // Find (other group) anchors that are adjacent to the group.
        let mut adj_anchors = Vec::<&SomeState>::new();
        for ancx in anchors.iter() {
            if grpx.region.is_adjacent_state(ancx) {
                adj_anchors.push(ancx);
            }
        }

        // For adjacent (other group) anchors,
        // store corresponding state in group region,
        // which may not have been sampled yet.
        let mut stas_in: Vec<&SomeState> = self.squares.stas_in_reg(&grpx.region);
        let mut additional_stas = StateStore::new();
        for ancx in &adj_anchors {
            // Calc state in group that corresponds to an adjacent anchor.
            let stay = ancx.bitwise_xor(&grpx.region.diff_mask_state(ancx));

            if !stas_in.contains(&&stay) {
                // The state may have been sampled already.
                if !additional_stas.contains(&stay) {
                    // The state may be adjacent to more than one anchor.
                    additional_stas.push(stay);
                }
            }
        }
        for stax in additional_stas.iter() {
            stas_in.push(stax);
        }

        // For each state, sta1, only in the group region, greg:
        //
        //  Calculate each state, sta_adj, adjacent to sta1, outside of greg.
        //
        //  Calculate a rate for each sta1 option, based on the number of adjacent states
        //  in only one group.
        let mut max_rate = (0, 0, 0);

        // Create a StateStore composed of anchor, far, and adjacent-external states.
        let mut cfmv_max = Vec::<&SomeState>::new();

        for stax in &stas_in {
            // Potential new anchor must be in only one group.
            if self.groups.num_groups_state_in(stax) != 1 {
                continue;
            }

            let sta_rate = self.group_anchor_rate(grpx, stax, &adj_anchors);

            //println!("group {} possible anchor {} rating {} {} {}", &grpx.region, &stax, sta_rate.0, sta_rate.1, sta_rate.2);

            // Accumulate highest rated anchors
            if sta_rate > max_rate {
                max_rate = sta_rate;
                cfmv_max = Vec::<&SomeState>::new();
            }
            //println!("rate {} is {}", cfmx[0], sta_rate);
            if sta_rate == max_rate {
                cfmv_max.push(stax);
            }
        } // next stax

        // Check current anchor, if any
        if let Some(anchor) = &grpx.anchor {
            //println!("anchor {} cfmv_max {}", anchor, cfmv_max);
            if cfmv_max.contains(&anchor) {
                //println!("group {} anchor {} still good, cfmv_max", grpx.region, anchor);
                let anchor_sqr = self.squares.find(anchor).unwrap();
                if anchor_sqr.pnc {
                    // println!("group {} anchor {} pnc", &greg, &anchor_sta);
                } else {
                    // Get additional samples of the anchor
                    ret_nds.push(SomeNeed::LimitGroup {
                        dom_num: 0, // will be set in domain code
                        act_num: self.num,
                        anchor: anchor.clone(),
                        target_state: anchor.clone(),
                        for_group: grpx.region.clone(),
                        group_num,
                    });
                }
                return ret_nds;
            } else {
                //println!("anchor changing for group {} from {}", grpx.region, anchor);
            }
        }

        if cfmv_max.is_empty() {
            //println!("group {} cfmv_max empty", grpx.region);
            return ret_nds;
        }

        // Select an anchor
        let mut cfm_max = cfmv_max[0];
        if cfmv_max.len() > 1 {
            cfm_max = cfmv_max[rand::thread_rng().gen_range(0..cfmv_max.len())];
        }
        if let Some(_sqrx) = self.squares.find(cfm_max) {
            ret_nds.push(SomeNeed::SetGroupAnchor {
                group_region: grpx.region.clone(),
                anchor: cfm_max.clone(),
            });
        } else {
            // Potential anchor not sampled yet.
            ret_nds.push(SomeNeed::LimitGroup {
                dom_num: 0, // will be set in domain code
                act_num: self.num,
                anchor: cfm_max.clone(),
                target_state: cfm_max.clone(),
                for_group: grpx.region.clone(),
                group_num,
            });
        }
        ret_nds
    } // end limit_group_anchor_needs

    /// Return the limiting needs for a group with an anchor chosen.
    pub fn limit_group_adj_needs(
        &self,
        grpx: &SomeGroup,
        anchor_sta: &SomeState,
        agg_changes: &SomeChange,
        group_num: usize,
    ) -> NeedStore {
        // If any external adjacent states have not been sampled, or not enough,
        // return needs for that.
        //
        // If the group far state has not been sampled, or not enough, return a need for that.
        //
        // Else limit the group.
        let mut ret_nds = NeedStore::new();

        let anchor_sqr = self.squares.find(anchor_sta).unwrap();

        // Check each adjacent external state
        let mut nds_grp = NeedStore::new(); // needs for more samples
        let mut nds_grp_add = NeedStore::new(); // needs for added group

        // Get masks of edge bits to use to limit group.
        // Ignore bits that cannot be changed by any action.
        let same_bits = grpx.region.same_bits();

        let one_bits = same_bits.bitwise_and(&agg_changes.b10.bitwise_and(&grpx.region.state1));

        let zero_bits = same_bits
            .bitwise_and(&agg_changes.b01)
            .bitwise_and(&grpx.region.state1.bitwise_not());

        let edge_msks: Vec<SomeMask> = one_bits.bitwise_or(&zero_bits).split();

        for mskx in &edge_msks {
            let adj_sta = anchor_sta.bitwise_xor(mskx);

            //println!("*** for group {} checking adj sqr {}", &greg, &adj_sta);

            if let Some(adj_sqr) = self.squares.find(&adj_sta) {
                if adj_sqr.pnc {
                    // Create new group, if an adjacent square can combine with the anchor.
                    // Current anchor will then be in two regions,
                    // the next run of limit_group_anchor_needs will deal with it.
                    if anchor_sqr.can_combine(adj_sqr) == Some(true) {
                        let regz = SomeRegion::new(anchor_sta.clone(), adj_sta);

                        let ruls = if anchor_sqr.pn == Pn::Unpredictable {
                            RuleStore::new()
                        } else {
                            anchor_sqr.rules.union(&adj_sqr.rules).unwrap()
                        };
                        nds_grp_add.push(SomeNeed::AddGroup {
                            group_region: regz,
                            rules: ruls,
                        });
                    }
                } else {
                    // Get another sample of adjacent square.
                    nds_grp.push(SomeNeed::LimitGroupAdj {
                        dom_num: 0, // will be set in domain code
                        act_num: self.num,
                        anchor: anchor_sta.clone(),
                        target_state: adj_sta,
                        for_group: grpx.region.clone(),
                        group_num,
                    });
                }
            } else {
                nds_grp.push(SomeNeed::LimitGroupAdj {
                    dom_num: 0, // will be set in domain code
                    act_num: self.num,
                    anchor: anchor_sta.clone(),
                    target_state: adj_sta,
                    for_group: grpx.region.clone(),
                    group_num,
                });
            }
        } // next inx in cfm_max

        if !nds_grp_add.is_empty() {
            //println!("*** nds_grp_add {}", &nds_grp_add);
            return nds_grp_add;
        }

        if !nds_grp.is_empty() {
            //println!("*** nds_grp {}", &nds_grp);
            return nds_grp;
        }

        // Process far state, after the anchor and adjacent, external, checks have been made.
        // Instead of checking every adjacent square internal to the group.

        // Group is non-X, so no far state
        if grpx.region.state1 == grpx.region.state2 {
            ret_nds.push(SomeNeed::SetGroupLimited {
                group_region: grpx.region.clone(),
            });
            return ret_nds;
        }

        let sta_far = grpx.region.far_state(anchor_sta);

        if let Some(sqrf) = self.squares.find(&sta_far) {
            if sqrf.pnc {
                // Set the group limited
                ret_nds.push(SomeNeed::SetGroupLimited {
                    group_region: grpx.region.clone(),
                });
            } else {
                // Get additional samples of the far state.
                ret_nds.push(SomeNeed::LimitGroup {
                    dom_num: 0, // will be set in domain code
                    act_num: self.num,
                    anchor: anchor_sta.clone(),
                    target_state: sta_far.clone(),
                    for_group: grpx.region.clone(),
                    group_num,
                });
            }
        } else {
            // Get the first sample of the far state.
            ret_nds.push(SomeNeed::LimitGroup {
                dom_num: 0, // will be set in domain code
                act_num: self.num,
                anchor: anchor_sta.clone(),
                target_state: sta_far.clone(),
                for_group: grpx.region.clone(),
                group_num,
            });
        }

        //println!("limit_group_needs: returning {}", &ret_nds);
        ret_nds
    } // end limit_group_adj_needs

    /// Check group pairs for an intersection.
    pub fn group_pair_needs(&self) -> NeedStore {
        //println!("group_pair_needs");
        let mut nds = NeedStore::new();

        if self.groups.len() < 2 {
            return nds;
        }

        // Check every pair of groups
        for inx in 0..(self.groups.len() - 1) {
            // Skip the last group, as there is no subsequent group.

            let grpx = &self.groups[inx];

            if grpx.pn != Pn::One && !grpx.pnc {
                continue;
            }

            // Pair grpx with every group after it in the GroupStore
            for iny in (inx + 1)..self.groups.len() {
                let grpy = &self.groups[iny];

                if grpy.pn != Pn::One && !grpy.pnc {
                    continue;
                }

                if grpx.region.intersects(&grpy.region) {
                    let mut ndx = self.group_pair_intersection_needs(grpx, grpy);
                    if !ndx.is_empty() {
                        nds.append(&mut ndx);
                    }
                } else if grpx.region.is_adjacent(&grpy.region) && grpx.pn == grpy.pn {
                    let mut ndx = self.group_pair_combine_needs(grpx, grpy);
                    if !ndx.is_empty() {
                        //println!("group combine adjacent needs: {}", ndx);
                        nds.append(&mut ndx);
                    }
                }
            } // next iny
        } // next inx

        nds
    } // end group_pair_needs

    /// Return true if there is no invalid combination of squares in a region.
    fn no_incompatible_square_combination_in_region(&self, regx: &SomeRegion) -> bool {
        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(regx);

        if sqrs_in_reg.len() < 2 {
            return true;
        }

        // Get pn min and max values.
        let mut max_pn = Pn::One;

        let mut any_pnc = false;
        let mut max_pnc = Pn::One;
        let mut min_pnc = Pn::Unpredictable;

        for sqrx in &sqrs_in_reg {
            if sqrx.pn > max_pn {
                max_pn = sqrx.pn;
            }
            if sqrx.pnc {
                any_pnc = true;
                if sqrx.pn > max_pnc {
                    max_pnc = sqrx.pn;
                }
                if sqrx.pn < min_pnc {
                    min_pnc = sqrx.pn;
                }
            }
        }

        // Check pn min and max values.
        if any_pnc {
            if max_pnc != min_pnc {
                return false;
            }

            if max_pn > max_pnc {
                return false;
            }

            if max_pnc == Pn::Unpredictable {
                return true;
            }
        }

        if max_pn == Pn::One {
            // Check by accumulation.
            let mut rulx = sqrs_in_reg[0].rules[0].clone();
            for sqrx in &sqrs_in_reg {
                for ruly in sqrx.rules.iter() {
                    if let Some(rulz) = rulx.union(ruly) {
                        rulx = rulz;
                    } else {
                        return false;
                    }
                }
            }

            return true;
        }

        // Get the indices for squares with Pn::Two.
        let mut pn_two = Vec::<usize>::new();
        for (inx, sqrx) in sqrs_in_reg.iter().enumerate() {
            if sqrx.pn == Pn::Two {
                pn_two.push(inx);
            }
        }

        assert!(!pn_two.is_empty());

        // Handle the situation of one square with Pn::Two.
        if pn_two.len() == 1 {
            let rulesx = &sqrs_in_reg[pn_two[0]].rules;
            for sqrx in &sqrs_in_reg {
                if sqrx.pn == Pn::One
                    && sqrx.rules[0].union(&rulesx[0]).is_none()
                    && sqrx.rules[0].union(&rulesx[1]).is_none()
                {
                    return false;
                }
            }
            return true;
        }

        // There is more than one square with Pn::Two,
        // get/check the union of all Pn::Two squares.
        let Some(mut rulesx) = sqrs_in_reg[pn_two[0]].rules.union(&sqrs_in_reg[pn_two[1]].rules) else { return false; };

        for inx in 2..pn_two.len() {
            if let Some(rulesy) = rulesx.union(&sqrs_in_reg[pn_two[inx]].rules) {
                rulesx = rulesy;
            } else {
                return false;
            }
        }

        // Get region formed by Pn::Two squares.
        let regy = rulesx.initial_region();

        // Simple case, all Pn::One rules should be a subset of Pn::Two rules.
        if regy == *regx {
            for sqrx in &sqrs_in_reg {
                if sqrx.pn == Pn::One && !sqrx.rules.is_subset_of(&rulesx) {
                    return false;
                }
            }
            return true;
        }

        // Not simple case, some squares checked for subset, some for union.
        for sqrx in &sqrs_in_reg {
            if sqrx.pn == Pn::One {
                if regy.is_superset_of_state(&sqrx.state) {
                    if !sqrx.rules.is_subset_of(&rulesx) {
                        return false;
                    }
                } else if sqrx.rules[0].union(&rulesx[0]).is_none()
                    && sqrx.rules[0].union(&rulesx[1]).is_none()
                {
                    return false;
                }
            }
        }

        true
    }

    /// Return true if all square rules in a region are a subset of given rules.
    fn all_subset_rules_in_region(&self, regx: &SomeRegion, rules: &RuleStore) -> bool {
        assert!(!rules.is_empty());

        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(regx);

        // Check if any square pairs are incompatible
        for sqrx in sqrs_in_reg {
            if sqrx.pn == Pn::Unpredictable {
                return false;
            }
            if !sqrx.rules.is_subset_of(rules) {
                return false;
            }
        }
        true
    }

    /// Return true if there is no square with an incompatible Pn value.
    fn no_incompatible_pn_square_in_region(&self, regx: &SomeRegion, pnx: Pn) -> bool {
        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(regx);

        for sqrx in &sqrs_in_reg {
            if sqrx.pn != pnx && (sqrx.pnc || sqrx.pn > pnx) {
                return false;
            }
        }
        true
    }

    /// Return needs to define a region, from the combination of two smaller regions.
    /// This assumes there are no incompatible squares in the region.
    fn region_defining_needs(
        &self,
        regx: &SomeRegion,
        reg1: &SomeRegion,
        reg2: &SomeRegion,
    ) -> NeedStore {
        //println!("region_defining_needs for {}", regx);

        let mut nds = NeedStore::new();

        // Gather the states from the regions, they may share one defining state.
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
        let mut pairs = Vec::<(SomeState, &SomeSquare)>::new();
        for sqrx in &anchor_sqrs {
            let sta2 = regx.far_state(&sqrx.state);

            if let Some(sqr2) = self.squares.find(&sta2) {
                if sqr2.pnc && sqrx.pnc || sqrx.pn == Pn::One && sqr2.pn == Pn::One {
                    let rules = if sqrx.pn == Pn::Unpredictable {
                        RuleStore::new()
                    } else {
                        sqrx.rules.union(&sqr2.rules).unwrap()
                    };
                    nds.push(SomeNeed::AddGroup {
                        group_region: SomeRegion::new(sqr2.state.clone(), sqrx.state.clone()),
                        rules,
                    });
                    return nds;
                }
                if sqr2.len_results() > num_samples {
                    pairs = Vec::<(SomeState, &SomeSquare)>::new();
                    num_samples = sqr2.len_results();
                }
                if sqr2.len_results() == num_samples {
                    pairs.push((sta2, sqrx));
                }
            } else if num_samples == 0 {
                pairs.push((sta2, sqrx));
            }
        }

        // Get more samples
        for pairx in &pairs {
            if !pairx.1.pnc {
                nds.push(SomeNeed::AStateMakeGroup {
                    dom_num: 0, // Will be set later.
                    act_num: self.num,
                    target_state: pairx.1.state.clone(),
                    for_reg: regx.clone(),
                    far: pairx.0.clone(),
                    num_x: regx.num_x(),
                });
            }
            if let Some(sqr2) = self.squares.find(&pairx.0) {
                if !sqr2.pnc {
                    nds.push(SomeNeed::AStateMakeGroup {
                        dom_num: 0, // Will be set later.
                        act_num: self.num,
                        target_state: pairx.0.clone(),
                        for_reg: regx.clone(),
                        far: pairx.1.state.clone(),
                        num_x: regx.num_x(),
                    });
                }
            } else {
                nds.push(SomeNeed::AStateMakeGroup {
                    dom_num: 0, // Will be set later.
                    act_num: self.num,
                    target_state: pairx.0.clone(),
                    for_reg: regx.clone(),
                    far: pairx.1.state.clone(),
                    num_x: regx.num_x(),
                });
            }
        }

        nds
    }

    /// Check two groups for combining needs.
    pub fn group_pair_combine_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        let nds = NeedStore::new();

        let reg_both = grpx.region.union(&grpy.region);

        if grpx.pn == Pn::Unpredictable {
            if self.no_incompatible_pn_square_in_region(&reg_both, Pn::Unpredictable) {
                return self.region_defining_needs(&reg_both, &grpx.region, &grpy.region);
            }
            return nds;
        }

        if let Some(rulsxy) = grpx.rules.union(&grpy.rules) {
            if (grpx.pn == Pn::One || (grpx.pnc && grpy.pnc))
                && self.all_subset_rules_in_region(&reg_both, &rulsxy)
            {
                return self.region_defining_needs(&reg_both, &grpx.region, &grpy.region);
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

        let Some(reg_int) = grpx.region.intersection(&grpy.region) else { return nds; };

        if grpx.pn != grpy.pn {
            let mut nds = NeedStore::new();
            nds.push(self.cont_int_region_need(&reg_int, grpx, grpy));
            return nds;
        }

        let reg_both = grpx.region.union(&grpy.region);

        if grpx.pn == Pn::Unpredictable {
            if self.no_incompatible_pn_square_in_region(&reg_both, Pn::Unpredictable) {
                return self.region_defining_needs(&reg_both, &grpx.region, &grpy.region);
            }
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

                nds.push(self.cont_int_region_need(&regy, grpx, grpy));
            } else {
                //println!("pn2 whole intersection is bad");
                nds.push(self.cont_int_region_need(&reg_int, grpx, grpy));
            }

            return nds;
        }

        if let Some(rulsxy) = grpx.rules.union(&grpy.rules) {
            if (grpx.pn == Pn::One || (grpx.pnc && grpy.pnc))
                && self.all_subset_rules_in_region(&reg_both, &rulsxy)
            {
                return self.region_defining_needs(&reg_both, &grpx.region, &grpy.region);
            }
        }

        nds
    } // end group_pair_intersection_needs

    /// For a contradictory intersection, return a need for more samples.
    /// If no prior samples in the intersection, seek one.
    /// If a prior sampled square is pnc, panic.
    /// If prior samples found, seek additional samples.
    fn cont_int_region_need(
        &self,
        regx: &SomeRegion,
        grpx: &SomeGroup,
        grpy: &SomeGroup,
    ) -> SomeNeed {
        //println!("cont_int_region_needs {} for grp {} {} and grp {} {}", &regx, &grpx.region, &grpx.rules, &grpy.region, &grpy.rules);
        // Check for any squares in the region
        let stas_in = self.squares.stas_in_reg(regx);

        if stas_in.is_empty() {
            let ruls1 = if grpx.rules.is_empty() {
                RuleStore::new()
            } else {
                grpx.rules.restrict_initial_region(regx)
            };

            let ruls2 = if grpy.rules.is_empty() {
                RuleStore::new()
            } else {
                grpy.rules.restrict_initial_region(regx)
            };

            return SomeNeed::ContradictoryIntersection {
                dom_num: 0, // set this in domain get_needs
                act_num: self.num,
                target_region: regx.clone(),
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
        // change start
        let mut stas_check = Vec::<&SomeState>::new();
        for stax in &stas_in {
            let sqrz = self.squares.find(stax).unwrap();
            if sqrz.pnc {
                panic!(
                    "Square {} is pnc in contradictory intersection {}\n  of group {}\n and group {}",
                    &sqrz, &regx, &grpx, &grpy
                );
            }

            if sqrz.len_results() > max_rslts {
                max_rslts = sqrz.len_results();
                stas_check = Vec::<&SomeState>::new();
            }

            if sqrz.len_results() == max_rslts {
                stas_check.push(&sqrz.state);
            }
        }

        let mut inx = 0;
        if stas_check.len() > 1 {
            inx = rand::thread_rng().gen_range(0..stas_check.len());
        }

        let ruls1 = if grpx.rules.is_empty() {
            RuleStore::new()
        } else {
            grpx.rules.restrict_initial_region(regx)
        };

        let ruls2 = if grpy.rules.is_empty() {
            RuleStore::new()
        } else {
            grpy.rules.restrict_initial_region(regx)
        };

        SomeNeed::ContradictoryIntersection {
            dom_num: 0, // set this in domain get_needs
            act_num: self.num,
            target_region: SomeRegion::new(stas_check[inx].clone(), stas_check[inx].clone()),
            group1: grpx.region.clone(),
            ruls1,
            group2: grpy.region.clone(),
            ruls2,
        }
    } // end cont_int_region_need

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
                    if let Some(rulx) = grpx.rules[0].parse_for_changes(achange) {
                        stps.push(SomeStep::new(self.num, rulx, false, grpx.region.clone()));
                    }
                }
                Pn::Two => {
                    for ruly in grpx.rules.iter() {
                        let Some(rulx) = ruly.parse_for_changes(achange) else { continue; };

                        // See if an existing square is ready to produce the desired result
                        let i_reg = rulx.initial_region();
                        let stas = self.squares.stas_in_reg(&i_reg);

                        let mut found = false;
                        for stax in &stas {
                            let sqrx = self.squares.find(stax).unwrap();

                            // Will include at least one bit change desired, but maybe others.
                            let expected_result = rulx.result_from_initial_state(stax);

                            // If a Pn::Two squares last result is not equal to what is wanted,
                            // the next result should be.
                            if sqrx.most_recent_result() != &expected_result {
                                let stpx = SomeStep::new(
                                    self.num,
                                    rulx.restrict_initial_region(&SomeRegion::new(
                                        sqrx.state.clone(),
                                        sqrx.state.clone(),
                                    )),
                                    false,
                                    grpx.region.clone(),
                                );
                                stps.push(stpx);
                                found = true;
                            } // end if
                        } // next stax

                        if !found {
                            stps.push(SomeStep::new(self.num, rulx, true, grpx.region.clone()));
                        }
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
    fn possible_regions_from_square(&self, sqrx: &SomeSquare) -> RegionStore {
        //println!("possible_group_regions from sqr {}", &sqrx.state);

        let mut rsx = RegionStore::new();

        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return rsx;
        }

        // Collect possible region, deleting subset regions
        for sqry in self.squares.ahash.values() {
            if sqry.state == sqrx.state {
                continue;
            }

            if sqrx.pn != sqry.pn {
                continue;
            }

            if sqrx.pn == Pn::One || sqry.pnc {
            } else {
                continue;
            }

            // Create region, sqrx.state becomes regx.state1
            let regx = SomeRegion::new(sqrx.state.clone(), sqry.state.clone());

            if self.groups.any_superset_of(&regx) {
                continue;
            }

            if rsx.any_superset_of(&regx) {
                continue;
            }

            if self.can_combine(sqrx, sqry) != Some(true) {
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

    /// Take an action for a need.
    pub fn take_action_need(
        &mut self,
        dom: usize,
        cur_state: &SomeState,
        ndx: &SomeNeed,
    ) -> SomeSample {
        let astate = self.do_something.take_action(cur_state);
        let asample = SomeSample::new(cur_state.clone(), self.num, astate);
        self.eval_need_sample(ndx, dom, &asample);
        asample
    }

    /// Take an action with the current state.
    pub fn take_action_step(&mut self, cur_state: &SomeState) -> SomeSample {
        let astate = self.do_something.take_action(cur_state);
        let asample = SomeSample::new(cur_state.clone(), self.num, astate);
        self.eval_step_sample(&asample);
        asample
    }

    /// Take an action with a given state.
    pub fn take_action_arbitrary(&mut self, cur_state: &SomeState) -> SomeSample {
        //println!("action {} take_action_arbitrary", self.num);
        let astate = self.do_something.take_action(cur_state);
        let asample = SomeSample::new(cur_state.clone(), self.num, astate);

        if self.groups.any_superset_of_state(cur_state) {
            self.eval_step_sample(&asample);
        } else {
            self.eval_sample(&asample);
        }
        asample
    }

    /// Return a change with all changes that can be made for the action.
    pub fn aggregate_changes(&self) -> &SomeChange {
        &self.groups.aggregate_changes
    }

    /// Return the GroupStore flag indicating the update status of its aggregate changes value.
    pub fn agg_chgs_updated(&self) -> bool {
        self.groups.agg_chgs_updated
    }

    /// Reset to GroupStore agg_chgs_updated flag, after the incorporation
    /// of its aggregate changes into the parent ActionStore struct.
    pub fn reset_agg_chgs_updated(&mut self) {
        self.groups.reset_agg_chgs_updated();
    }
} // end impl SomeAction

// Some action tests are made from the domain level.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn groups_formed_1() -> Result<(), String> {
        // Init action
        let mut act0 = SomeAction::new(0, 0, 1);
        //let mem = VecDeque::<SomeState>::new();
        //let chg = SomeChange::new_low(1);

        // Eval sample that other samples will be incompatible with.
        let s7 = SomeState::new_from_string(1, "s0b0111").unwrap();
        act0.eval_sample(&SomeSample::new(s7.clone(), 0, s7.clone()));

        // Process three similar samples.
        act0.eval_sample(&SomeSample::new(
            SomeState::new_from_string(1, "s0b1011").unwrap(),
            0,
            SomeState::new_from_string(1, "s0b1010").unwrap(),
        ));

        act0.eval_sample(&SomeSample::new(
            SomeState::new_from_string(1, "s0b1101").unwrap(),
            0,
            SomeState::new_from_string(1, "s0b1100").unwrap(),
        ));

        act0.eval_sample(&SomeSample::new(
            SomeState::new_from_string(1, "s0b0001").unwrap(),
            0,
            SomeState::new_from_string(1, "s0b0000").unwrap(),
        ));

        if act0.groups.len() != 4 {
            println!("Groups {}", act0.groups);
            return Err(format!("Four groups not formed?"));
        }
        if let Some(_) = act0
            .groups
            .find(&SomeRegion::new_from_string(1, "r0111").unwrap())
        {
        } else {
            return Err("Region 0111 not found!".to_string());
        }
        if let Some(_) = act0
            .groups
            .find(&SomeRegion::new_from_string(1, "r1xx1").unwrap())
        {
        } else {
            return Err("Region 1XX1 not found!".to_string());
        }
        if let Some(_) = act0
            .groups
            .find(&SomeRegion::new_from_string(1, "rxx01").unwrap())
        {
        } else {
            return Err("Region XX01 not found!".to_string());
        }
        if let Some(_) = act0
            .groups
            .find(&SomeRegion::new_from_string(1, "rx0x1").unwrap())
        {
        } else {
            return Err("Region X0X1 not found!".to_string());
        }
        Ok(())
    }

    #[test]
    fn no_incompatible_square_combination_in_region() -> Result<(), String> {
        // Init states.
        let sta_f = SomeState::new_from_string(1, "s0b1111").unwrap();
        let sta_e = SomeState::new_from_string(1, "s0b1110").unwrap();
        let sta_d = SomeState::new_from_string(1, "s0b1101").unwrap();
        let sta_c = SomeState::new_from_string(1, "s0b1100").unwrap();
        let sta_b = SomeState::new_from_string(1, "s0b1011").unwrap();
        let sta_a = SomeState::new_from_string(1, "s0b1010").unwrap();
        let sta_7 = SomeState::new_from_string(1, "s0b0111").unwrap();
        let sta_5 = SomeState::new_from_string(1, "s0b0101").unwrap();
        let sta_4 = SomeState::new_from_string(1, "s0b0100").unwrap();
        let sta_3 = SomeState::new_from_string(1, "s0b0011").unwrap();
        let sta_1 = SomeState::new_from_string(1, "s0b0001").unwrap();
        let sta_0 = SomeState::new_from_string(1, "s0b0000").unwrap();

        // Init action
        let mut act0 = SomeAction::new(0, 0, 1);

        // Set up region XXX1
        let regx = SomeRegion::new(sta_1.clone(), sta_f.clone());

        // Make square F, Pn::Two. LSB 1->1 and 1->0.
        act0.eval_sample(&SomeSample::new(sta_f.clone(), 0, sta_f.clone()));
        act0.eval_sample(&SomeSample::new(sta_f.clone(), 0, sta_e.clone()));
        act0.eval_sample(&SomeSample::new(sta_f.clone(), 0, sta_f.clone()));
        act0.eval_sample(&SomeSample::new(sta_f.clone(), 0, sta_e.clone()));

        // Should be OK so far.
        if !act0.no_incompatible_square_combination_in_region(&regx) {
            return Err("Result 1 is false?".to_owned());
        } else {
            println!("Result 1 as expected");
        }

        // Set up square 1. Pn::One. LSB 1->0. Inside XXX1, outside X1X1.
        act0.eval_sample(&SomeSample::new(sta_1.clone(), 0, sta_0.clone()));

        // Should be OK so far.
        if !act0.no_incompatible_square_combination_in_region(&regx) {
            return Err("Result 2 is false?".to_owned());
        } else {
            println!("Result 2 as expected");
        }

        // Make square 5, Pn::Two. LSB 1->0 and 1->1.
        act0.eval_sample(&SomeSample::new(sta_5.clone(), 0, sta_4.clone()));
        act0.eval_sample(&SomeSample::new(sta_5.clone(), 0, sta_5.clone()));
        act0.eval_sample(&SomeSample::new(sta_5.clone(), 0, sta_4.clone()));
        act0.eval_sample(&SomeSample::new(sta_5.clone(), 0, sta_5.clone()));

        // Squares F and 5 make a region, X1X1, a subset of XXX1.

        // Should be OK so far.
        if !act0.no_incompatible_square_combination_in_region(&regx) {
            return Err("Result 3 is false?".to_owned());
        } else {
            println!("Result 3 as expected");
        }

        // Set up square 7. Pn::One. LSB 1->1. Inside X1X1.
        act0.eval_sample(&SomeSample::new(sta_7.clone(), 0, sta_7.clone()));

        // Set up square D. Pn::One. LSB 1->0. Inside X1X1.
        act0.eval_sample(&SomeSample::new(sta_d.clone(), 0, sta_c.clone()));

        // Set up square B. Pn::One. LSB 1->0. Outside X1X1.
        act0.eval_sample(&SomeSample::new(sta_b.clone(), 0, sta_a.clone()));

        // Should be OK so far.
        if !act0.no_incompatible_square_combination_in_region(&regx) {
            return Err("Result 4 is false?".to_owned());
        } else {
            println!("Result 4 as expected");
        }

        // Try a bad square 3.  2 LSB 11->01.
        act0.eval_sample(&SomeSample::new(sta_3.clone(), 0, sta_1.clone()));

        // Should fail.
        if act0.no_incompatible_square_combination_in_region(&regx) {
            return Err("Result 5 is true?".to_owned());
        } else {
            println!("Result 5 as expected");
        }

        // Remove square 3.
        act0.squares.remove(&sta_3);

        // Add new square 3.  LSB 1->1.
        act0.eval_sample(&SomeSample::new(sta_3.clone(), 0, sta_3.clone()));

        // Should be OK so far.
        if !act0.no_incompatible_square_combination_in_region(&regx) {
            return Err("Result 6 is false?".to_owned());
        } else {
            println!("Result 6 as expected");
        }

        // Square 3 to pnc, Pn::One.
        act0.eval_sample(&SomeSample::new(sta_3.clone(), 0, sta_3.clone()));

        // Should fail.
        if act0.no_incompatible_square_combination_in_region(&regx) {
            return Err("Result 7 is true?".to_owned());
        } else {
            println!("Result 7 as expected");
        }

        Ok(())
    }

    // Test making a group from two Pn::Two squares.
    #[test]
    fn possible_region() -> Result<(), String> {
        // Set up 2-result square sf.
        let sf = SomeState::new_from_string(1, "s0b1111").unwrap();
        let se = SomeState::new_from_string(1, "s0b1110").unwrap();

        let mut act0 = SomeAction::new(0, 0, 1);

        act0.eval_sample(&SomeSample::new(sf.clone(), 0, sf.clone()));
        act0.eval_sample(&SomeSample::new(sf.clone(), 0, se.clone()));
        act0.eval_sample(&SomeSample::new(sf.clone(), 0, sf.clone()));
        act0.eval_sample(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up 2-result square s1.
        let s1 = SomeState::new_from_string(1, "s0b0001").unwrap();
        let s0 = SomeState::new_from_string(1, "s0b0000").unwrap();
        act0.eval_sample(&SomeSample::new(s1.clone(), 0, s1.clone()));
        act0.eval_sample(&SomeSample::new(s1.clone(), 0, s0.clone()));
        act0.eval_sample(&SomeSample::new(s1.clone(), 0, s1.clone()));
        act0.eval_sample(&SomeSample::new(s1.clone(), 0, s0.clone()));

        let memory = VecDeque::<SomeSample>::new();
        let nds = act0.get_needs(
            &s1,
            0,
            &memory,
            &SomeChange::new(
                SomeMask::new_from_string(1, "m0b1111").unwrap(),
                SomeMask::new_from_string(1, "m0b1111").unwrap(),
            ),
        );
        println!("Act: {}", &act0);
        println!("needs: {}", nds);

        if nds.len() != 1 {
            return Err(String::from("Unexpected needs?"));
        }
        if act0.groups.len() != 1 {
            return Err(String::from("Unexpected groups?"));
        }

        Ok(())
    }
}
