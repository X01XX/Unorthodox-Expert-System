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
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;

/// Number of new squares added before a cleanup check is run.
const CLEANUP: usize = 5;

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("A(ID: ");

        rc_str += &self.num.to_string();

        rc_str += ", number squares: ";
        rc_str += &self.squares.len().to_string();

        if self.seek_edge.is_not_empty() {
            rc_str.push_str(&format!(", seek_edge within: {}", self.seek_edge));
        }

        if let Some(aregion) = &self.remainder_check_region {
            rc_str.push_str(&format!(", remainder: {}", aregion));
        }

        let mut fil = ",\n       Grps: ";
        for grpx in self.groups.iter() {
            let stas_in = self.squares.stas_in_reg(&grpx.region);

            let cnt: usize = stas_in
                .iter()
                .map(|stax| usize::from(self.groups.num_state_in(stax) == 1))
                .sum();

            rc_str.push_str(&format!(
                "{}{} num Sqrs: {} in1: {})",
                &fil,
                &grpx.formatted_string(),
                &stas_in.len(),
                &cnt,
            ));

            fil = ",\n             ";
        }

        rc_str.push(')');

        write!(f, "{rc_str}")
    }
}

//#[readonly::make]
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
    /// When the action has no needs, check for any missed regions.
    remainder_checked: bool,
    remainder_check_region: Option<SomeRegion>,
}

impl SomeAction {
    /// Return a new SomeAction struct, given the number integers used in the SomeBits struct.
    /// The action number, an index into the ActionStore that will contain it, is set to zero and
    /// changed later.
    pub fn new(dom_num: usize, act_num: usize, init_mask: SomeMask) -> Self {
        SomeAction {
            num: act_num,
            dom_num,
            groups: GroupStore::new(
                vec![],
                SomeChange::new(init_mask.new_low(), init_mask.new_low()),
            ),
            squares: SquareStore::new(HashMap::new()),
            seek_edge: RegionStore::new(vec![]),
            do_something: ActionInterface::new(),
            cleanup_trigger: CLEANUP,
            remainder_checked: false,
            remainder_check_region: None,
        }
    }

    /// Return the truth value for the combination of any two different squares,
    /// and the squares between them.
    fn can_combine(&self, sqrx: &SomeSquare, sqry: &SomeSquare) -> bool {
        assert_ne!(sqrx.state, sqry.state);
        //println!("can_combine: {} and {}?", sqrx.state, sqry.state);

        if !sqrx.can_combine_now(sqry) {
            return false;
        }

        if sqrx.pn == Pn::Unpredictable {
            return !self.any_incompatible_pn_square_in_region(&SomeRegion::new(vec![
                sqrx.state.clone(),
                sqry.state.clone(),
            ]));
        }

        let Some(rules) = sqrx.rules.as_ref().expect("SNH").union(sqry.rules.as_ref().expect("SNH")) else { return false; };

        self.all_subset_rules_in_region(
            &SomeRegion::new(vec![sqrx.state.clone(), sqry.state.clone()]),
            &rules,
        )
    }

    /// Add a new square from a sample.
    /// Check the consequenses of adding the square.
    pub fn add_new_square(&mut self, smpl: &SomeSample) {
        if self.cleanup_trigger > 0 {
            self.cleanup_trigger -= 1;
        }
        self.squares.insert(
            SomeSquare::new(smpl.initial.clone(), smpl.result.clone()),
            self.dom_num,
            self.num,
        );
        self.check_square_new_sample(&smpl.initial);
        self.eval_sample_check_anchor(smpl);
    }

    /// Store and evaluate a sample if it forms a new square,
    /// or causes a change to an existing square (change in pn or pnc).
    pub fn eval_sample(&mut self, smpl: &SomeSample) {
        // Check if sample is for an existing square.
        if let Some(sqrx) = self.squares.find_mut(&smpl.initial) {
            if sqrx.add_result(smpl.result.clone()) {
                self.check_square_new_sample(&smpl.initial);
            }
        } else {
            self.add_new_square(smpl);
        }
    }

    /// Check for anchor change due to a new sample.
    /// A square implied by the sample, in a group, may have a higher anchor rating
    /// than the current group anchor.
    fn eval_sample_check_anchor(&mut self, smpl: &SomeSample) {
        let Some(grpx) = self.groups.state_in_1_group(&smpl.initial) else { return };

        let Some(anchor) = &grpx.anchor else { return; };

        if anchor == &smpl.initial {
            return;
        }

        // The current anchor rate may have changed, so recalculate it.
        let anchor_rate = self.group_anchor_rate(grpx, anchor);

        let sqr_rate = self.group_anchor_rate(grpx, &smpl.initial);

        if sqr_rate <= anchor_rate {
            return;
        }

        let grp_reg = grpx.region.clone();

        // Get a mutable reference.
        let Some(grpx) = self.groups.find_mut(&grp_reg) else { panic!("Should work") };

        println!(
            "Changing group {} anchor from {} {:?} to {} {:?}",
            grpx.region,
            grpx.anchor.as_ref().unwrap(),
            anchor_rate,
            smpl.initial,
            sqr_rate
        );

        // Create a new square, if none exists already.
        if self.squares.find(&smpl.initial).is_none() {
            self.squares.insert(
                SomeSquare::new(smpl.initial.clone(), smpl.result.clone()),
                self.dom_num,
                self.num,
            );
        }

        grpx.set_anchor(&smpl.initial);
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
                let sqrx = self
                    .squares
                    .find(sta)
                    .expect("Square not found, state given by need should be an existing square");

                if sqrx.pn == Pn::One || sqrx.pnc {
                    //println!("AStateMakeGroup: sqr {} sampled, pn {} pnc {}", &sqrx, sqrx.pn, sqrx.pnc);
                    let Some(sqry) = self.squares.find(far) else { return; };

                    if (sqry.pn == Pn::One || sqry.pnc)
                        && (sqrx.state == sqry.state || self.can_combine(sqrx, sqry))
                    {
                        let regz = SomeRegion::new(vec![sqrx.state.clone(), sqry.state.clone()]);

                        // Generate group rules.
                        let rulsxy = if sqrx.pn == Pn::Unpredictable {
                            None
                        } else {
                            Some(sqrx.rules.as_ref().expect("SNH").union(sqry.rules.as_ref().expect("SNH")).expect("Need should not be generated unless the squares rules are compatible"))
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
                assert_ne!(greg.state1(), greg.state2());

                if sta != greg.state1() && sta != greg.state2() {
                    let mut make_groups_from = Vec::<SomeState>::new();

                    // Find the squares
                    let sqr1 = self
                        .squares
                        .find(greg.state1())
                        .expect("Group region states should refer to existing squares");
                    let sqr2 = self
                        .squares
                        .find(greg.state2())
                        .expect("Group region states should refer to existing squares");
                    let sqr3 = self
                        .squares
                        .find(sta)
                        .expect("Need should only apply to an existing square");

                    // Process next sample of square in-between for new square and state1 square.
                    // Should be different from state1 square or state2 square.
                    // It may be different from both state1 and state2.
                    // If it needs more samples, skip, next need will increment the number samples.

                    if !sqr3.can_combine_now(sqr1) && sqr1.pnc && sqr3.pnc {
                        if sqr1.is_adjacent(sqr3) {
                            println!("\nDom {} Act {} new edge found between {} and {} removing seek edge {}", dom, self.num, &sqr1.state, &sqr3.state, &greg);
                            self.seek_edge.remove_region(greg);
                            make_groups_from.push(sqr1.state.clone());
                            make_groups_from.push(sqr3.state.clone());
                        } else {
                            let even_closer_reg =
                                SomeRegion::new(vec![sqr1.state.clone(), sqr3.state.clone()]);
                            println!(
                                "\nDom {} Act {} replace seek edge {} with {}",
                                dom, self.num, &greg, &even_closer_reg
                            );
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }

                    if !sqr3.can_combine_now(sqr2) && sqr2.pnc && sqr3.pnc {
                        if sqr2.is_adjacent(sqr3) {
                            println!("\nDom {} Act {} new edge found between {} and {} removing seek edge {}", dom, self.num, &sqr2.state, &sqr3.state, &greg);
                            self.seek_edge.remove_region(greg);
                            make_groups_from.push(sqr2.state.clone());
                            if !make_groups_from.contains(&sqr3.state) {
                                make_groups_from.push(sqr3.state.clone());
                            }
                        } else {
                            let even_closer_reg =
                                SomeRegion::new(vec![sqr2.state.clone(), sqr3.state.clone()]);
                            self.seek_edge.push_nosups(even_closer_reg);
                        }
                    }

                    // Create a group for square if needed
                    for stax in make_groups_from {
                        self.create_groups_from_square(&stax);
                    }
                }
            } // end match SeekEdgeNeed
            // Check if group can be confirmed.
            SomeNeed::ConfirmGroup { grp_reg, .. } => {
                let Some(sqr1) = self.squares.find(grp_reg.state1()) else { return; };
                let Some(sqr2) = self.squares.find(grp_reg.state2()) else { return; };

                if sqr1.pnc && sqr2.pnc {
                    self.set_group_pnc(grp_reg);
                }
            }
            // Reset remainder check fields.
            SomeNeed::StateInRemainder { .. } => {
                self.remainder_checked = false;
                self.remainder_check_region = None;
            }
            _ => (),
        } // end match ndx
    } // end eval_need_sample

    /// Check a group for pnc, set it if needed
    pub fn set_group_pnc(&mut self, grp_reg: &SomeRegion) {
        let Some(grpx) = self.groups.find_mut(grp_reg) else { println!("ConfirmGroup {grp_reg} group not found?"); return; };

        if grpx.region.states.len() > 2 {
            return;
        }

        if grpx.pnc {
            println!(
                "ConfirmGroup {} already pnc and ConfirmGroup need?",
                grpx.region
            );
            return;
        }

        let Some(sqr1) = self.squares.find(grpx.region.state1()) else { panic!(
                "ConfirmGroup {} state1 {} square not found?",
                grpx.region, grpx.region.state1()
            ); };

        if !sqr1.pnc {
            return;
        }

        if grpx.region.state1() == grpx.region.state2() {
            grpx.set_pnc();
            return;
        }

        let Some(sqr2) = self.squares.find(grpx.region.state2()) else {
            panic!(
                "ConfirmGroup {} state2 {} square not found?",
                grpx.region, grpx.region.state2()
            ); };

        if sqr2.pnc {
            grpx.set_pnc();
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
                self.check_square_new_sample(&smpl.initial);
            }
            return;
        }

        // Check if any groups are invalidated
        if self.groups.any_groups_invalidated(smpl) {
            self.eval_sample(smpl);
        } else {
            self.eval_sample_check_anchor(smpl);
        }
    } // end eval_step_sample

    /// Check a square, referenced by state, against valid groups.
    /// The square may invalidate some groups.
    /// Add a group for the square if the square is in no valid group.
    /// If any groups were invalidated, check for any other squares
    /// that are in no groups.
    fn check_square_new_sample(&mut self, key: &SomeState) {
        //println!("check_square_new_sample for {}", key);

        let sqrx = self
            .squares
            .find(key)
            .expect("key should always refer to an existing square");

        // Get groups invalidated, which may orphan some squares.
        //let regs_invalid = self.validate_groups_new_sample(&key);
        let regs_invalid: RegionStore = self.groups.check_square(sqrx, self.dom_num, self.num);

        // Save regions invalidated to seek new edges.
        for regx in regs_invalid.iter() {
            if key != regx.state1() && key != regx.state2() && regx.states.len() > 1 {
                let Some(sqr1) = self.squares.find(regx.state1()) else {
                    panic!("Can't find group defining square?");
                };
                if !sqr1.pnc {
                    continue;
                }
                let Some(sqr2) = self.squares.find(regx.state2()) else {
                    continue;
                };

                if !sqr2.pnc {
                    continue;
                }

                if !sqrx.state.is_adjacent(&sqr1.state) && !sqrx.can_combine_now(sqr1) {
                    println!(
                        "\nDom {} Act {} Seek edge between {} and {}",
                        &self.dom_num, &self.num, &sqrx.state, &sqr1.state
                    );
                    self.seek_edge.push_nosubs(SomeRegion::new(vec![
                        sqrx.state.clone(),
                        sqr1.state.clone(),
                    ]));
                }

                if !sqrx.state.is_adjacent(&sqr2.state) && !sqrx.can_combine_now(sqr2) {
                    println!(
                        "\nDom {} Act {} Seek edge between {} and {}",
                        &self.dom_num, &self.num, &sqrx.state, &sqr2.state
                    );
                    self.seek_edge.push_nosubs(SomeRegion::new(vec![
                        sqrx.state.clone(),
                        sqr2.state.clone(),
                    ]));
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

            if anchor_sqr.can_combine_now(sqrx) {
                grpx.set_limited_off();
            }
        }

        // Create a group for square if needed
        let grps_in = self.groups.groups_state_in(key);
        if grps_in.is_empty() || (grps_in.len() == 1 && (grps_in[0].x_mask().is_low())) {
            self.create_groups_from_square(key);
        }

        if regs_invalid.is_empty() {
            return;
        }

        // Check squares from invalidated groups that may not be in any group.
        let mut stas_to_check = Vec::<SomeState>::new();

        for regx in regs_invalid.iter() {
            let stas = self.squares.stas_in_reg(regx);
            //println!("stas to check {}", SomeState::vec_ref_string(&stas));

            for stax in stas.iter() {
                if self.groups.num_groups_state_in(stax) == 0 && !stas_to_check.contains(stax) {
                    stas_to_check.push((*stax).clone());
                }
            }
        }
        for stax in stas_to_check.iter() {
            self.create_groups_from_square(stax);
        }
    } // end check_square_new_sample

    /// Check groups due to a new, or updated, square.
    /// Create a group with the square, if needed.
    fn create_groups_from_square(&mut self, key: &SomeState) {
        //println!("create_groups_from_square {}", &key);

        // Get groups the state is in
        let grps_in = self.groups.groups_state_in(key);

        let sqrx = self
            .squares
            .find(key)
            .expect("key should refer to an existing square");

        // Check if square can be used to create groups.
        // Allowing a square to make a group with a single sample is needed
        // for group bootstrapping.
        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return;
        }

        let groups_in = grps_in.len();
        let mut plural = "";
        if groups_in != 1 {
            plural = "s";
        }
        println!(
            "\nDom {} Act {} Square {} in {} group{}",
            self.dom_num,
            self.num,
            sqrx.str_terse(),
            grps_in.len(),
            plural,
        );

        //println!("Checking Square {} for new groups", &sqrx.str_terse());

        // Get possible regions, sqrx.state will be <region>.state1
        // Duplicate group regions are possible, but at least one should be
        // a valid new group.
        let grps: Vec<SomeGroup> = self.possible_groups_from_square(sqrx);

        if !grps.is_empty() {
            for grpx in grps {
                if !self.groups.any_superset_of(&grpx.region) {
                    self.groups.push(grpx, self.dom_num, self.num);
                }
            } // next regx
            return;
        }

        // Make a single-square group
        if groups_in == 0 {
            let regz = SomeRegion::new(vec![sqrx.state.clone()]);
            self.groups.push(
                SomeGroup::new(regz, sqrx.rules.clone(), sqrx.pnc),
                self.dom_num,
                self.num,
            );
        }
    } // end create_groups_from_square

    /// Return needs for states that are not in a group.
    /// The Domain current state for which there are no samples.
    /// A pn > 1 state that needs more samples.
    pub fn state_not_in_group_needs(
        &self,
        cur_state: &SomeState,
        memory: &VecDeque<SomeSample>,
    ) -> NeedStore {
        let mut nds = NeedStore::new(vec![]);

        // Check if current state is in any groups
        if !self.groups.any_superset_of_state(cur_state) {
            if let Some(sqrx) = self.squares.find(cur_state) {
                if sqrx.pn == Pn::One || sqrx.pnc {
                    println!(
                        "problem?: Dom {} Act {} square {} not in group?",
                        self.dom_num, self.num, sqrx
                    );
                } else {
                    let mut needx = SomeNeed::StateNotInGroup {
                        dom_num: self.dom_num,
                        act_num: self.num,
                        target_state: cur_state.clone(),
                        priority: 0,
                    };
                    needx.set_priority();
                    nds.push(needx);
                    return nds;
                }
            } else {
                let mut needx = SomeNeed::StateNotInGroup {
                    dom_num: self.dom_num,
                    act_num: self.num,
                    target_state: cur_state.clone(),
                    priority: 0,
                };
                needx.set_priority();
                nds.push(needx);
                return nds;
            }
        }

        // Look for a pn > 1, pnc == false, not in group squares
        // Extra samples are needed to gain pnc, then the first group.
        let mut target: Option<&SomeState> = None;
        let mut dist = usize::MAX;

        let sqrs_pngt1 = self.squares.pn_gt1_no_pnc();

        for stax in sqrs_pngt1.iter() {
            if *stax == cur_state || self.groups.any_superset_of_state(stax) {
                continue;
            }

            if cur_state.distance(stax) < dist {
                target = Some(stax);
                dist = cur_state.distance(stax);
            }
        }
        if let Some(stax) = target {
            let mut needx = SomeNeed::StateNotInGroup {
                dom_num: self.dom_num,
                act_num: self.num,
                target_state: stax.clone(),
                priority: dist,
            };
            needx.set_priority();
            nds.push(needx);
            return nds;
        }

        // Check memory
        let mut target: Option<&SomeSample> = None;
        let mut dist = usize::MAX;

        for smpx in memory.iter() {
            if &smpx.initial == cur_state || self.groups.any_superset_of_state(&smpx.initial) {
                continue;
            }
            if cur_state.distance(&smpx.initial) < dist {
                target = Some(smpx);
                dist = cur_state.distance(&smpx.initial);
            }
        } // next stax

        if let Some(smpx) = target {
            let mut needx = SomeNeed::StateNotInGroup {
                dom_num: self.dom_num,
                act_num: self.num,
                target_state: smpx.initial.clone(),
                priority: dist,
            };
            needx.set_priority();
            nds.push(needx);
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
        let mut nds = NeedStore::new(vec![]);
        let mut cnt = 0;
        loop {
            cnt += 1;

            // Look for needs to find a new edge in an invalidated group
            nds.append(&mut self.seek_edge_needs());

            // Check for additional samples for group states needs
            nds.append(&mut self.confirm_group_needs());

            // Check any two groups for:
            // Overlapping regions that may be combined.
            // Overlapping groups that form a contradictory intersection.
            nds.append(&mut self.group_pair_needs());

            // Check for squares in-one-group needs
            if let Some(mut ndx) = self.limit_groups_needs(&agg_changes.bits_change_mask()) {
                nds.append(&mut ndx);
            }

            // Check for repeating housekeeping needs loop
            if cnt > 20 {
                println!("needs: {}", &nds);
                panic!("Dom {} Act {} loop count GT 20!", dom, self.num);
            }

            // Edit out subset/eq group adds.
            let mut new_grp_regs = RegionStore::new(vec![]);
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
                                    SomeRegion::vec_ref_string(
                                        &self.groups.supersets_of(group_region)
                                    )
                                );
                            }
                            continue;
                        }

                        // Calc pnc
                        let sqrx = self
                            .squares
                            .find(group_region.state1())
                            .expect("Group region states should refer to existing squares");
                        let pnc = if group_region.state2() == group_region.state1() {
                            sqrx.pnc
                        } else if let Some(sqry) = self.squares.find(group_region.state2()) {
                            sqrx.pnc && sqry.pnc
                        } else {
                            false
                        };

                        let ruls = rules.as_mut().map(|ruls| ruls.clone());
                        self.groups.push(
                            SomeGroup::new(group_region.clone(), ruls, pnc),
                            dom,
                            self.num,
                        );
                        try_again = true;
                    }
                    SomeNeed::SetGroupLimited {
                        group_region: greg,
                        num_adj,
                    } => {
                        if let Some(grpx) = self.groups.find_mut(greg) {
                            println!(
                                "\nDom {} Act {} Group {} limited num adj {}",
                                dom, self.num, greg, num_adj
                            );
                            grpx.set_limited(*num_adj);
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
                        if self.seek_edge.push_nosups(regx.clone()) {
                            try_again = true;
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
                //if nds.is_empty() {
                //  return self.left_over_needs();
                //}

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
                nds.append(&mut self.state_not_in_group_needs(cur_state, memory));

                if nds.is_empty() {
                    // Do remainder check.
                    if self.remainder_checked {
                        if let Some(aregion) = &self.remainder_check_region {
                            //println!("dom {} act {} remainder need 2 added for {}", self.dom_num, self.num, astate);
                            let mut needx = SomeNeed::StateInRemainder {
                                dom_num: self.dom_num,
                                act_num: self.num,
                                target_region: aregion.clone(),
                                priority: 0,
                            };
                            needx.set_priority();
                            nds.push(needx);
                        }
                    } else {
                        self.remainder_checked = true;

                        let max_reg = SomeRegion::new(vec![
                            cur_state.clone(),
                            cur_state.bitwise_xor(&agg_changes.b01.bitwise_and(&agg_changes.b10)),
                        ]);

                        self.remainder_check_region = self.remainder_check_region(max_reg);

                        if let Some(aregion) = &self.remainder_check_region {
                            //match self.display_anchor_info() {
                            //    _ => ()
                            //}
                            //println!("dom {} act {} remainder need 1 added for {}", self.dom_num, self.num, astate);
                            let mut needx = SomeNeed::StateInRemainder {
                                dom_num: self.dom_num,
                                act_num: self.num,
                                target_region: aregion.clone(),
                                priority: 0,
                            };
                            needx.set_priority();
                            nds.push(needx);
                        }
                    }
                } else {
                    self.remainder_checked = false;
                    self.remainder_check_region = None;
                }

                return nds;
            }

            nds = NeedStore::new(vec![]);
        } // end loop
    } // end get_needs

    /// Check for needs in a region not covered by current groups.
    fn remainder_check_region(&self, max_region: SomeRegion) -> Option<SomeRegion> {
        let mut remainder_regs = RegionStore::new(vec![max_region.clone()]);

        for grpx in self.groups.iter() {
            remainder_regs = remainder_regs.subtract_region(&grpx.region);
        }

        if remainder_regs.is_not_empty() {
            println!(
                "dom {} act {} max_region {} remainder is {}",
                self.dom_num, self.num, max_region, remainder_regs
            );
        }

        if remainder_regs.is_not_empty() {
            //println!("Checking null check state, returning {}", remainder_regs[0].state1);
            let mut inx = 0;
            if remainder_regs.len() > 1 {
                inx = rand::thread_rng().gen_range(0..remainder_regs.len());
            }
            return Some(remainder_regs[inx].clone());
        }

        //println!("Checking null check state, returning None");
        None
    }

    /// Cleanup unneeded squares.
    fn cleanup(&mut self, needs: &NeedStore) {
        // Store for keys of squares to delete.
        let mut to_del = StateStore::new(vec![]);

        'next_keyx: for keyx in self.squares.ahash.keys() {
            // Check needs
            for ndx in needs.iter() {
                for targx in ndx.target().iter() {
                    if targx.is_superset_of_state(keyx) {
                        continue 'next_keyx;
                    }
                }
            }

            // Don't delete squares in groups.
            // let mut in_groups = false;
            for grpx in self.groups.iter() {
                if grpx.region.is_superset_of_state(keyx) {
                    for stax in grpx.region.states.iter() {
                        if stax == keyx {
                            continue 'next_keyx;
                        }
                    }

                    if let Some(stay) = &grpx.anchor {
                        if stay == keyx {
                            continue 'next_keyx;
                        }
                        if *keyx == grpx.region.state_far_from(stay) {
                            continue 'next_keyx;
                        }
                    }
                } else if let Some(stay) = &grpx.anchor {
                    if keyx.is_adjacent(stay) {
                        continue 'next_keyx;
                    }
                }
            }

            // Don't delete squares in seek edge regions.
            for regx in self.seek_edge.iter() {
                if regx.is_superset_of_state(keyx) {
                    continue 'next_keyx;
                }
            }

            // Don't delete squares that are not in a group.
            // That is, squares with Pn: > One that need more samples.
            if self.groups.num_groups_state_in(keyx) == 0 {
                continue;
            }

            to_del.push(keyx.clone());
        }

        if !to_del.is_empty() {
            println!(
                "\nDom {} Act {} deleted unneeded squares: {}",
                self.dom_num, self.num, to_del
            );
            self.squares.del_squares(&to_del);
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
        let mut ret_nds = NeedStore::new(vec![]);

        let mut new_regs = RegionStore::new(vec![]);

        'next_regx: for regx in self.seek_edge.iter() {
            //print!("seek_edge_needs: checking reg {} ", &regx);
            // Get the squares represented by the states that form the region
            let sqr1 = self
                .squares
                .find(regx.state1())
                .expect("Group region states should refer to existing squares");
            let sqr2 = self
                .squares
                .find(regx.state2())
                .expect("Group region states should refer to existing squares");

            // Check that squares that define the region are pnc.
            if !sqr1.pnc {
                //print!("get more samples of square {} ", &sqr1.state);
                let mut needx = SomeNeed::SeekEdge {
                    dom_num: self.dom_num,
                    act_num: self.num,
                    target_state: sqr1.state.clone(),
                    in_group: regx.clone(),
                    priority: 0,
                };
                needx.set_priority();
                ret_nds.push(needx);
                continue;
            }
            if !sqr2.pnc {
                //print!("get more samples of square {} ", &sqr2.state);
                let mut needx = SomeNeed::SeekEdge {
                    dom_num: self.dom_num,
                    act_num: self.num,
                    target_state: sqr2.state.clone(),
                    in_group: regx.clone(),
                    priority: 0,
                };
                needx.set_priority();
                ret_nds.push(needx);
                continue;
            }

            // Get squares between defining states.
            let sqrs_in = self.squares.squares_in_reg(regx);

            // No squares between
            if sqrs_in.len() == 2 {
                ret_nds.append(&mut self.seek_edge_needs2(regx));
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
                    let cnb1 = sqrx.can_combine_now(sqr1);
                    let cnb2 = sqrx.can_combine_now(sqr2);

                    if cnb1 && cnb2 {
                        ret_nds.push(SomeNeed::InactivateSeekEdge { reg: regx.clone() });
                    } else {
                        if !cnb1 {
                            new_regs.push_nosups(SomeRegion::new(vec![
                                sqrx.state.clone(),
                                sqr1.state.clone(),
                            ]));
                        }
                        if !cnb2 {
                            new_regs.push_nosups(SomeRegion::new(vec![
                                sqrx.state.clone(),
                                sqr2.state.clone(),
                            ]));
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
                    let mut needx = SomeNeed::SeekEdge {
                        dom_num: self.dom_num,
                        act_num: self.num,
                        target_state: sqrx.state.clone(),
                        in_group: regx.clone(),
                        priority: 0,
                    };
                    needx.set_priority();
                    ret_nds.push(needx);
                }
            }

            //println!(" ");
        } // next regx

        // Apply new seek edge regions
        // After get_needs does the housekeeping, it will run this again
        if new_regs.is_not_empty() {
            ret_nds = NeedStore::with_capacity(new_regs.len());
            for regx in new_regs.iter() {
                ret_nds.push(SomeNeed::AddSeekEdge { reg: regx.clone() });
            }
        }

        ret_nds
    } // end seek_edge_needs

    /// Generate needs for seek_edge regions that have no squares between the region defining states.
    pub fn seek_edge_needs2(&self, regx: &SomeRegion) -> NeedStore {
        //println!("seek_edge_needs2");
        let mut ret_nds = NeedStore::new(vec![]);

        if regx.state1().is_adjacent(regx.state2()) {
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
            regx.state1().bitwise_xor(&dif_msk)
        } else {
            regx.state2().bitwise_xor(&dif_msk)
        };

        // Make need for seek_state
        //print!("get first sample of square {}", &seek_state);
        let mut needx = SomeNeed::SeekEdge {
            dom_num: self.dom_num,
            act_num: self.num,
            target_state: seek_state,
            in_group: regx.clone(),
            priority: 0,
        };
        needx.set_priority();
        ret_nds.push(needx);
        //println!(" ");

        ret_nds
    } // end seek_edge_needs2

    /// Get additional sample needs for the states that form a group.
    /// Should only affect groups with Pn::One.
    /// Groups closer to the beginning of the group will have priority due to lower group number.
    pub fn confirm_group_needs(&mut self) -> NeedStore {
        //println!("confirm_group_needs");
        let mut ret_nds = NeedStore::new(vec![]);

        for (group_num, grpx) in self.groups.iter_mut().enumerate() {
            if grpx.pnc {
                continue;
            }

            let sqrx = self
                .squares
                .find(grpx.region.state1())
                .expect("Group region states should refer to existing squares");
            if !sqrx.pnc {
                let mut needx = SomeNeed::ConfirmGroup {
                    dom_num: self.dom_num,
                    act_num: self.num,
                    target_state: grpx.region.state1().clone(),
                    grp_reg: grpx.region.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.set_priority();
                ret_nds.push(needx);
            }

            // If this is a one-state group ..
            if grpx.region.state1() == grpx.region.state2() {
                if sqrx.pnc {
                    grpx.set_pnc();
                }
                continue;
            }

            if let Some(sqry) = self.squares.find(grpx.region.state2()) {
                if sqry.pnc {
                    if sqrx.pnc {
                        grpx.set_pnc();
                    }
                    continue;
                }
            }

            let mut needx = SomeNeed::ConfirmGroup {
                dom_num: self.dom_num,
                act_num: self.num,
                target_state: grpx.region.state2().clone(),
                grp_reg: grpx.region.clone(),
                priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
            };
            needx.set_priority();
            ret_nds.push(needx);
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
    pub fn limit_groups_needs(&self, change_mask: &SomeMask) -> Option<NeedStore> {
        //println!("limit_groups_needs chg {}", change_mask);

        let mut ret_nds = NeedStore::new(vec![]);

        // Check groups current anchors are still in only one region,
        for grpx in self.groups.iter() {
            let Some(stax) = &grpx.anchor else { continue; };

            if self.groups.num_groups_state_in(stax) != 1 {
                ret_nds.push(SomeNeed::RemoveGroupAnchor {
                    group_region: grpx.region.clone(),
                });
            }
        }
        if ret_nds.is_not_empty() {
            return Some(ret_nds);
        }

        // Get anchor needs.
        for (group_num, grpx) in self.groups.iter().enumerate() {
            if !grpx.pnc {
                continue;
            }

            if let Some(mut ndx) = self.limit_group_anchor_needs(grpx, group_num) {
                ret_nds.append(&mut ndx);
            } else if let Some(anchor) = &grpx.anchor {
                if !grpx.limited {
                    // Get masks of edge bits to use to limit group.
                    // Ignore bits that cannot be changed by any action.
                    let change_bits = grpx.region.edge_mask().bitwise_and(change_mask);
                    let edge_msks: Vec<SomeMask> = change_bits.split();

                    if let Some(mut ndx) =
                        self.limit_group_adj_needs(grpx, anchor, &edge_msks, group_num)
                    {
                        ret_nds.append(&mut ndx);
                    } else {
                        ret_nds.push(SomeNeed::SetGroupLimited {
                            group_region: grpx.region.clone(),
                            num_adj: edge_msks.len(),
                        });
                    }
                }
            }
        }
        if ret_nds.is_not_empty() {
            return Some(ret_nds);
        }
        None
    } // end limit_groups_needs

    /// Return a rate for a possible anchor for a group.
    /// The rate will be a tuple containing:
    ///     The number of adjacent states that are anchors of other groups,
    ///     The number adjacent states that are in only one group,
    ///     The number of samples taken for the adjacent states.
    /// To set an anchor, a square with at least one sample is required, but ..
    /// To rate a possible anchor state, no sample of the state is requried.
    /// When comparing tuples, Rust compares item pairs in order until there is a difference.
    pub fn group_anchor_rate(&self, grpx: &SomeGroup, stax: &SomeState) -> (usize, usize, usize) {
        //assert_eq!(self.groups.num_groups_state_in(stax), 1);
        if self.groups.num_groups_state_in(stax) != 1 {
            return (0, 0, 0);
        }

        let mut anchors = 0;
        let mut in_1_group = 0;
        let mut sqr_samples = 0;

        // Get masks of edge bits to use to limit group.
        let edge_msks = grpx.region.edge_mask().split();

        // Rate adjacent external states
        for edge_bit in &edge_msks {
            let sta_adj = stax.bitwise_xor(edge_bit);
            //println!(
            //    "checking {} adjacent to {} external to {}",
            //    &sta_adj, &stax, &greg
            //);

            let stats = self.groups.in_one_anchor(&sta_adj);

            if stats == Some(true) {
                anchors += 1;
            } else if stats == Some(false) {
                //println!("{} is in only one group", &sta_adj);
                in_1_group += 1;
            }

            if let Some(sqrx) = self.squares.find(&sta_adj) {
                sqr_samples += sqrx.rate();
            }
        } // next edge_bit

        (anchors, in_1_group, sqr_samples)
    }

    /// Return the limiting anchor needs for a group.
    /// If no state in the group is in only one group, return None.
    /// If an existing anchor has the same, or better, rating than other possible states,
    /// return None.
    pub fn limit_group_anchor_needs(
        &self,
        grpx: &SomeGroup,
        group_num: usize,
    ) -> Option<NeedStore> {
        let mut ret_nds = NeedStore::new(vec![]);

        let adj_squares = self.squares.stas_adj_reg(&grpx.region);

        // For adjacent (other group) anchors,
        // store corresponding state in group region,
        // which has not have been sampled yet.
        let mut stas_in: Vec<&SomeState> = self.squares.stas_in_reg(&grpx.region);

        // Home for additional states, that have not been sampled yet, so their
        // reference can be pushed to the stas_in vector.
        let mut additional_stas = Vec::<SomeState>::new();

        for ancx in adj_squares.iter() {
            // Calc state in group that corresponds to an adjacent anchor.
            let stay = ancx.bitwise_xor(&grpx.region.diff_mask_state(ancx));

            // Check if the state has not been sampled already.
            if !stas_in.contains(&&stay) {
                // The state may be in the vertor already, due to being
                // adjacent to more than one external regions' anchor.
                if !additional_stas.contains(&stay) {
                    additional_stas.push(stay);
                }
            }
        }
        // Add additional state references to stas_in vector.
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

            let sta_rate = self.group_anchor_rate(grpx, stax);

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

        if cfmv_max.is_empty() {
            //println!("group {} cfmv_max empty", grpx.region);
            return None;
        }

        // Check current anchor, if any
        if let Some(anchor) = &grpx.anchor {
            //println!("anchor {} cfmv_max {}", anchor, cfmv_max);
            if cfmv_max.contains(&anchor) {
                //println!("group {} anchor {} still good, cfmv_max", grpx.region, anchor);
                let anchor_sqr = self
                    .squares
                    .find(anchor)
                    .expect("Group anchor state should refer to an existing square");

                if anchor_sqr.pnc {
                    return None;
                    // println!("group {} anchor {} pnc", &greg, &anchor_sta);
                }

                // Get additional samples of the anchor
                let mut needx = SomeNeed::LimitGroup {
                    dom_num: self.dom_num,
                    act_num: self.num,
                    anchor: anchor.clone(),
                    target_state: anchor.clone(),
                    for_group: grpx.region.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.set_priority();
                ret_nds.push(needx);

                return Some(ret_nds);
            }
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
            let mut needx = SomeNeed::LimitGroup {
                dom_num: self.dom_num,
                act_num: self.num,
                anchor: cfm_max.clone(),
                target_state: cfm_max.clone(),
                for_group: grpx.region.clone(),
                priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
            };
            needx.set_priority();
            ret_nds.push(needx);
        }
        Some(ret_nds)
    } // end limit_group_anchor_needs

    /// Return the limiting needs for a group with an anchor chosen, but not yet set to limited.
    pub fn limit_group_adj_needs(
        &self,
        grpx: &SomeGroup,
        anchor_sta: &SomeState,
        edge_msks: &[SomeMask],
        group_num: usize,
    ) -> Option<NeedStore> {
        // If any external adjacent states have not been sampled, or not enough,
        // return needs for that.
        //
        // If the group far state has not been sampled, or not enough, return a need for that.
        //
        // Else limit the group.
        let mut ret_nds = NeedStore::new(vec![]);

        let anchor_sqr = self
            .squares
            .find(anchor_sta)
            .expect("Group region anchor should refer to an existing square");

        // Check each adjacent external state
        let mut nds_grp = NeedStore::new(vec![]); // needs for more samples
        let mut nds_grp_add = NeedStore::new(vec![]); // needs for added group

        for mskx in edge_msks {
            let adj_sta = anchor_sta.bitwise_xor(mskx);

            //println!("*** for group {} checking adj sqr {}", &greg, &adj_sta);

            if let Some(adj_sqr) = self.squares.find(&adj_sta) {
                if adj_sqr.pnc {
                    // Create new group, if an adjacent square can combine with the anchor.
                    // Current anchor will then be in two regions,
                    // the next run of limit_group_anchor_needs will deal with it.
                    if anchor_sqr.can_combine_now(adj_sqr) {
                        let regz = SomeRegion::new(vec![anchor_sta.clone(), adj_sta]);

                        let ruls: Option<RuleStore> = if anchor_sqr.pn == Pn::Unpredictable {
                            None
                        } else {
                            anchor_sqr
                                .rules
                                .as_ref()
                                .expect("SNH")
                                .union(adj_sqr.rules.as_ref().expect("SNH"))
                        };
                        nds_grp_add.push(SomeNeed::AddGroup {
                            group_region: regz,
                            rules: ruls,
                        });
                    }
                } else {
                    // Get another sample of adjacent square.
                    let mut needx = SomeNeed::LimitGroupAdj {
                        dom_num: self.dom_num,
                        act_num: self.num,
                        anchor: anchor_sta.clone(),
                        target_state: adj_sta,
                        for_group: grpx.region.clone(),
                        priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                    };
                    needx.set_priority();
                    nds_grp.push(needx);
                }
            } else {
                // Get first sample of adjacent square.
                let mut needx = SomeNeed::LimitGroupAdj {
                    dom_num: self.dom_num,
                    act_num: self.num,
                    anchor: anchor_sta.clone(),
                    target_state: adj_sta,
                    for_group: grpx.region.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.set_priority();
                nds_grp.push(needx);
            }
        } // next inx in cfm_max

        if nds_grp_add.is_not_empty() {
            //println!("*** nds_grp_add {}", &nds_grp_add);
            return Some(nds_grp_add);
        }

        if nds_grp.is_not_empty() {
            //println!("*** nds_grp {}", &nds_grp);
            return Some(nds_grp);
        }

        // Process far state, after the anchor and adjacent, external, checks have been made.
        // Instead of checking every adjacent square internal to the group.

        // Group is non-X, so no far state
        if grpx.region.state1() == grpx.region.state2() {
            return None;
        }

        let sta_far = grpx.region.state_far_from(anchor_sta);

        if let Some(sqrf) = self.squares.find(&sta_far) {
            if sqrf.pnc {
                return None;
            } else {
                // Get additional samples of the far state.
                let mut needx = SomeNeed::LimitGroup {
                    dom_num: self.dom_num,
                    act_num: self.num,
                    anchor: anchor_sta.clone(),
                    target_state: sta_far,
                    for_group: grpx.region.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.set_priority();
                ret_nds.push(needx);
            }
        } else {
            // Get the first sample of the far state.
            let mut needx = SomeNeed::LimitGroup {
                dom_num: self.dom_num,
                act_num: self.num,
                anchor: anchor_sta.clone(),
                target_state: sta_far,
                for_group: grpx.region.clone(),
                priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
            };
            needx.set_priority();
            ret_nds.push(needx);
        }

        //println!("limit_group_needs: returning {}", &ret_nds);
        if ret_nds.is_empty() {
            None
        } else {
            Some(ret_nds)
        }
    } // end limit_group_adj_needs

    /// Check group pairs for an intersection.
    pub fn group_pair_needs(&self) -> NeedStore {
        //println!("group_pair_needs");
        let mut nds = NeedStore::new(vec![]);

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
                    nds.append(&mut self.group_pair_intersection_needs(grpx, grpy));
                } else if grpx.region.is_adjacent(&grpy.region) && grpx.pn == grpy.pn {
                    nds.append(&mut self.group_pair_combine_needs(grpx, grpy));
                }
            } // next iny
        } // next inx

        nds
    } // end group_pair_needs

    /// Return true if all square rules in a region are a subset of given rules.
    fn all_subset_rules_in_region(&self, regx: &SomeRegion, rules: &RuleStore) -> bool {
        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(regx);

        // Check if any square pairs are incompatible
        for sqrx in sqrs_in_reg {
            if sqrx.pn == Pn::Unpredictable {
                return false;
            }
            if !sqrx.rules.as_ref().expect("SNH").is_subset_of(rules) {
                return false;
            }
        }
        true
    }

    /// Return true if there is no square with an incompatible Pn/pnc value with Pn::Unpredictable.
    fn any_incompatible_pn_square_in_region(&self, regx: &SomeRegion) -> bool {
        // Get squares in the region.
        let sqrs_in_reg = self.squares.squares_in_reg(regx);

        for sqrx in &sqrs_in_reg {
            if sqrx.pn != Pn::Unpredictable && sqrx.pnc {
                return true;
            }
        }
        false
    }

    /// Return needs to define a group region, from the combination of two smaller group regions, if possible.
    /// Assumes the combined region has been checked for validity.
    fn region_combine_needs(
        &self,
        regx: &SomeRegion,
        reg1: &SomeRegion,
        reg2: &SomeRegion,
    ) -> NeedStore {
        //println!("region_defining_needs for {}", regx);

        let mut nds = NeedStore::new(vec![]);

        // Gather the states from the regions, they may share one defining state.
        let mut defining_stas = reg1.states.clone();

        for stax in reg2.states.iter() {
            if defining_stas.contains(stax) {
            } else {
                defining_stas.push(stax.clone());
            }
        }

        // Gather defining squares
        let mut defining_sqrs = Vec::<&SomeSquare>::with_capacity(defining_stas.len());
        for stax in defining_stas.iter() {
            if let Some(sqrx) = self.squares.find(stax) {
                // Last square may not be found.
                defining_sqrs.push(sqrx);
            }
        }

        // Check the far states from possible defining squares, form pairs.
        // Return a need to form a group, if possible.
        // Otherwise choose a pairs that minimizes the additional samples needed to form the region,
        // return needs for more samples of the pairs.
        let mut num_samples = 0;
        let mut pairs = Vec::<(SomeState, &SomeSquare)>::new();
        for sqrx in &defining_sqrs {
            let sta2 = regx.state_far_from(&sqrx.state);

            let rate = if let Some(sqr2) = self.squares.find(&sta2) {
                // Check if a group can be formed.
                if sqr2.pnc && sqrx.pnc || sqrx.pn == Pn::One && sqr2.pn == Pn::One {
                    let rules: Option<RuleStore> = if sqrx.pn == Pn::Unpredictable {
                        None
                    } else {
                        sqrx.rules
                            .as_ref()
                            .expect("SNH")
                            .union(sqr2.rules.as_ref().expect("SNH"))
                    };
                    nds.push(SomeNeed::AddGroup {
                        group_region: SomeRegion::new(vec![sqr2.state.clone(), sqrx.state.clone()]),
                        rules,
                    });
                    return nds;
                }

                // Return pair rate
                sqr2.rate() + sqrx.rate()
            } else {
                // Return sqrx rate.
                sqrx.rate()
            };

            if rate > num_samples {
                pairs = Vec::<(SomeState, &SomeSquare)>::new();
                num_samples = rate;
            }
            if rate == num_samples {
                pairs.push((sta2, sqrx));
            }
        } // next sqrx

        // Get more samples
        for pairx in &pairs {
            if !pairx.1.pnc {
                let mut needx = SomeNeed::AStateMakeGroup {
                    dom_num: self.dom_num,
                    act_num: self.num,
                    target_state: pairx.1.state.clone(),
                    for_reg: regx.clone(),
                    far: pairx.0.clone(),
                    priority: pairx.1.state.num_bits() - regx.num_x(), // Adjust priority to service larger possible groups first.
                };
                needx.set_priority();
                nds.push(needx);
            }
            if let Some(sqr2) = self.squares.find(&pairx.0) {
                if !sqr2.pnc {
                    let mut needx = SomeNeed::AStateMakeGroup {
                        dom_num: self.dom_num,
                        act_num: self.num,
                        target_state: pairx.0.clone(),
                        for_reg: regx.clone(),
                        far: pairx.1.state.clone(),
                        priority: pairx.1.state.num_bits() - regx.num_x(), // Adjust priority to service larger possible groups first.
                    };
                    needx.set_priority();
                    nds.push(needx);
                }
            } else {
                let mut needx = SomeNeed::AStateMakeGroup {
                    dom_num: self.dom_num,
                    act_num: self.num,
                    target_state: pairx.0.clone(),
                    for_reg: regx.clone(),
                    far: pairx.1.state.clone(),
                    priority: pairx.1.state.num_bits() - regx.num_x(), // Adjust priority to service larger possible groups first.
                };
                needx.set_priority();
                nds.push(needx);
            }
        }

        nds
    }

    /// Check two groups for combining needs.
    pub fn group_pair_combine_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        let nds = NeedStore::new(vec![]);

        if grpx.limited || grpy.limited || !grpx.pnc || !grpy.pnc {
            return nds;
        }

        let reg_both = grpx.region.union(&grpy.region);

        if grpx.pn == Pn::Unpredictable {
            if self.any_incompatible_pn_square_in_region(&reg_both) {
                return nds;
            }
            return self.region_combine_needs(&reg_both, &grpx.region, &grpy.region);
        }
        if let Some(rulsxy) = grpx
            .rules
            .as_ref()
            .expect("SNH")
            .union(grpy.rules.as_ref().expect("SNH"))
        {
            if (grpx.pn == Pn::One || (grpx.pnc && grpy.pnc))
                && self.all_subset_rules_in_region(&reg_both, &rulsxy)
            {
                return self.region_combine_needs(&reg_both, &grpx.region, &grpy.region);
            }
        }
        nds
    }

    /// Check two intersecting groups for needs.
    /// Possibly combining two groups.
    /// Possibly checking for a contradictatory intersection.
    pub fn group_pair_intersection_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //println!(
        //  "groups_pair_intersection_needs {} {} and {} {}",
        //  &grpx.region, &grpx.pn, &grpy.region, grpy.pn
        //);

        let mut nds = NeedStore::new(vec![]);

        let Some(reg_int) = grpx.region.intersection(&grpy.region) else { return nds; };

        if grpx.pn != grpy.pn {
            return NeedStore::new(vec![self.cont_int_region_need(&reg_int, grpx, grpy)]);
        }

        if grpx.pn == Pn::Unpredictable {
            return self.group_pair_combine_needs(grpx, grpy);
        }

        let rulsx = grpx
            .rules
            .as_ref()
            .expect("SNH")
            .restrict_initial_region(&reg_int);

        let rulsy = grpy
            .rules
            .as_ref()
            .expect("SNH")
            .restrict_initial_region(&reg_int);

        // If rules are the same, check if a combination could be made.
        if rulsx == rulsy {
            return self.group_pair_combine_needs(grpx, grpy);
        }

        // If contradictory, return needs to resolve

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
        let sqrs_in = self.squares.squares_in_reg(regx);

        if sqrs_in.is_empty() {
            let ruls1 = if grpx.rules.is_none() {
                None
            } else {
                Some(
                    grpx.rules
                        .clone()
                        .expect("SNH")
                        .restrict_initial_region(regx),
                )
            };

            let ruls2 = if grpy.rules.is_none() {
                None
            } else {
                Some(
                    grpy.rules
                        .clone()
                        .expect("SNH")
                        .restrict_initial_region(regx),
                )
            };

            let mut needx = SomeNeed::ContradictoryIntersection {
                dom_num: self.dom_num,
                act_num: self.num,
                target_region: regx.clone(),
                group1: grpx.region.clone(),
                ruls1,
                group2: grpy.region.clone(),
                ruls2,
                priority: 0,
            };
            needx.set_priority();
            return needx;
        }

        // Some samples have been taken in the region

        // Find a square with the highest number of samples
        // If any are pnc, panic.
        let mut max_rslts = 0;
        // change start
        let mut stas_check = Vec::<&SomeState>::new();
        for sqrz in &sqrs_in {
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

        let ruls1 = if grpx.rules.is_none() {
            None
        } else {
            Some(
                grpx.rules
                    .clone()
                    .expect("SNH")
                    .restrict_initial_region(regx),
            )
        };

        let ruls2 = if grpy.rules.is_none() {
            None
        } else {
            Some(
                grpy.rules
                    .clone()
                    .expect("SNH")
                    .restrict_initial_region(regx),
            )
        };

        let mut needx = SomeNeed::ContradictoryIntersection {
            dom_num: self.dom_num,
            act_num: self.num,
            target_region: SomeRegion::new(vec![stas_check[inx].clone()]),
            group1: grpx.region.clone(),
            ruls1,
            group2: grpy.region.clone(),
            ruls2,
            priority: 0,
        };
        needx.set_priority();
        needx
    } // end cont_int_region_need

    /// Get possible steps that can be used to make at least part of a
    /// given change.
    ///
    /// For each rule, prune the rule X bit positions to favor desired changes.
    ///
    /// For a two-result group, see if there is an existing square that is expected to
    /// produce the desired change.
    pub fn get_steps(&self, achange: &SomeChange) -> StepStore {
        debug_assert!(achange.b01.bitwise_and(&achange.b10).is_low()); // No X->x change wanted.

        let mut stps = StepStore::new(vec![]);

        for grpx in self.groups.iter() {
            if grpx.pn == Pn::Unpredictable {
                continue;
            }

            // Check if group rules cause at least one change that is needed.
            let mut skip = true;
            for rulx in grpx.rules.as_ref().expect("SNH").iter() {
                if achange.bitwise_and_rule(rulx).is_not_low() {
                    skip = false;
                    break;
                }
            }
            if skip {
                continue;
            }

            if grpx.pn == Pn::One {
                // Find bit changes that are desired
                if let Some(rulx) = grpx.rules.as_ref().expect("SNH")[0].parse_for_changes(achange)
                {
                    stps.push(SomeStep::new(self.num, rulx, false, grpx.region.clone()));
                }
                continue;
            }

            if grpx.pn == Pn::Two {
                // Get restricted region for needed changes.
                let mut parsed_region: Option<SomeRegion> = None;
                for ruly in grpx.rules.as_ref().expect("SNH").iter() {
                    let Some(rulx) = ruly.parse_for_changes(achange) else { continue; };
                    parsed_region = Some(rulx.initial_region());
                }
                // Check if any rule has no changes, so a state could be sampled once, or twice, to get the desired change.
                let mut one_no_change = false;
                if let Some(regx) = parsed_region {
                    for ruly in grpx.rules.as_ref().expect("SNH").iter() {
                        let rulx = ruly.restrict_initial_region(&regx);
                        if rulx.b01.is_low() && rulx.b10.is_low() {
                            one_no_change = true;
                            break;
                        }
                    }
                }

                for ruly in grpx.rules.as_ref().expect("SNH").iter() {
                    let Some(rulx) = ruly.parse_for_changes(achange) else { continue; };

                    // See if an existing square is ready to produce the desired result
                    let i_reg = rulx.initial_region();
                    let sqrs = self.squares.squares_in_reg(&i_reg);

                    let mut found = false;
                    for sqrx in &sqrs {
                        // Will include at least one bit change desired, but maybe others.
                        let expected_result = rulx.result_from_initial_state(&sqrx.state);

                        // If a Pn::Two squares last result is not equal to what is wanted,
                        // the next result should be.
                        if *sqrx.most_recent_result() != expected_result {
                            let stpx = SomeStep::new(
                                self.num,
                                rulx.restrict_initial_region(&SomeRegion::new(vec![sqrx
                                    .state
                                    .clone()])),
                                false,
                                grpx.region.clone(),
                            );
                            stps.push(stpx);
                            found = true;
                        } // end if
                    } // next sqrx

                    if !found && one_no_change {
                        stps.push(SomeStep::new(self.num, rulx, true, grpx.region.clone()));
                    }
                } // next ruly
            } // end Pn::Two
        } // next grpx

        // println!("Steps: {}", &stps);
        stps
    } // end get_steps

    /// Find groups that can be formed by a given square, and other similar squares, that is not currently in a group.
    fn possible_groups_from_square(&self, sqrx: &SomeSquare) -> Vec<SomeGroup> {
        //println!("possible_groups_from_square {}", &sqrx.state);

        let mut ret_grps = Vec::<SomeGroup>::new();

        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return ret_grps;
        }

        // Collect similar squares.
        let mut sim_sqrs = Vec::<&SomeSquare>::new();

        // Collect possible regions.

        // Init a list containing the maximum region.
        let mut poss_regs = RegionStore::new(vec![SomeRegion::new(vec![
            sqrx.state.new_high(),
            sqrx.state.new_low(),
        ])]);

        for sqry in self.squares.ahash.values() {
            if sqry.state == sqrx.state {
                continue;
            }

            // Previous subtractions may put some squares out of reach.
            if poss_regs.any_superset_of_state(&sqry.state) {
                if sqrx.can_combine_now(sqry) {
                    sim_sqrs.push(sqry);
                } else if sqrx.may_combine_later_to(sqry) {
                    // More samples of sqry are needed to tell if it is simmilar or dissimmilar.
                } else {
                    poss_regs = poss_regs.subtract_state_to_supersets_of(&sqry.state, &sqrx.state);
                }
            }
        }

        if sim_sqrs.is_empty() {
            return ret_grps;
        }

        // Collect similar squares that have a superset in possible regions.
        // Subtractions after finding a similar square may have put it out of reach.
        let mut sim_sqrs2 = Vec::<&SomeSquare>::with_capacity(sim_sqrs.len());

        for sqry in sim_sqrs {
            if poss_regs.any_superset_of_state(&sqry.state) {
                sim_sqrs2.push(sqry);
            }
        }

        if sim_sqrs2.is_empty() {
            return ret_grps;
        }

        //println!("Similar squares:");
        //for sqrz in sim_sqrs2.iter() {
        //    println!("  {}", sqrz);
        //}
        //println!("Disimilar squares:");
        //for sqrz in dissim_sqrs.iter() {
        //    println!("  {}", sqrz);
        //}

        // Calc excluded regions formed by pairs of similar squares
        // that cannot be combined.
        // If the target square has a 1->0 bit,
        // it may combine with a square having a corresponding 0->1, forming X->x,
        // and one with 0->0, forming X->0,
        // but 0->0 and 0->1 cannot combine.
        if sim_sqrs2.len() > 1 {
            let mut excluded_regs = RegionStore::new(vec![]);

            for iny in 0..(sim_sqrs2.len() - 1) {
                for inz in (iny + 1)..sim_sqrs2.len() {
                    if !sim_sqrs2[iny].can_combine_now(sim_sqrs2[inz]) {
                        excluded_regs.push(SomeRegion::new(vec![
                            sim_sqrs2[iny].state.clone(),
                            sim_sqrs2[inz].state.clone(),
                        ]));
                    }
                }
            }
            //println!("excluded regions {excluded_regs}");

            // Subtract excluded region state pairs.
            // So regions can contain any one state, but not both.
            for regy in excluded_regs.iter() {
                if poss_regs.any_superset_of(regy) {
                    let mut tmp_regs = RegionStore::new(vec![]);
                    for regx in &poss_regs.avec {
                        if regx.is_superset_of(regy) {
                            let subregs1 =
                                regx.subtract_state_to_supersets_of(regy.state1(), &sqrx.state);
                            for sreg in subregs1 {
                                tmp_regs.push_nosubs(sreg);
                            }
                            let subregs2 =
                                regx.subtract_state_to_supersets_of(regy.state2(), &sqrx.state);
                            for sreg in subregs2 {
                                tmp_regs.push_nosubs(sreg);
                            }
                        } else {
                            tmp_regs.push_nosubs(regx.clone());
                        }
                    } // next regx
                    poss_regs = tmp_regs;
                }
            } // next regy
        }

        //println!("poss regions: {poss_regs}");

        // Validate possible regions.
        ret_grps = poss_regs
            .avec
            .par_iter() // par_iter for parallel processing, iter for sequential diagnostic messages.
            .filter_map(|regx| self.validate_possible_group(sqrx, regx, &sim_sqrs2))
            .collect::<Vec<SomeGroup>>();

        ret_grps
    } // end possible_regions_from_square

    /// Validate a region that may be made from a given square, in combination with similar squares.
    fn validate_possible_group(
        &self,
        sqrx: &SomeSquare,
        regx: &SomeRegion,
        sim_sqrs: &[&SomeSquare],
    ) -> Option<SomeGroup> {
        // Basic checks.
        if !regx.is_superset_of_state(&sqrx.state) {
            return None;
        }

        // Find squares in the given region, and calc region built from simmilar squares.
        let mut regy = SomeRegion::new(vec![sqrx.state.clone()]);
        let mut sqrs_in = Vec::<&SomeSquare>::new();
        for sqry in sim_sqrs.iter() {
            if regx.is_superset_of_state(&sqry.state) {
                sqrs_in.push(sqry);
                if !regy.is_superset_of_state(&sqry.state) {
                    regy = regy.union_state(&sqry.state);
                }
            }
        }
        if self.groups.any_superset_of(&regy) {
            return None;
        }

        // Don't make a single-state region.
        if sqrs_in.is_empty() {
            return None;
        }

        let mut rules: Option<RuleStore> = None;
        if let Some(rulesx) = &sqrx.rules {
            // Check all squares rules can form a union.
            let mut rulesz = rulesx.clone();
            for sqry in sqrs_in.iter() {
                if let Some(rulesy) = &sqry.rules {
                    let rules_new = rulesz.union(rulesy)?;
                    rulesz = rules_new;
                } else {
                    return None;
                }
            }
            rules = Some(rulesz);
        }

        // Calc pnc.
        let mut pnc = false;
        let sta_far = regy.state_far_from(&sqrx.state);
        if sqrx.pnc {
            for sqrx in &sqrs_in {
                if sqrx.state == sta_far {
                    pnc = sqrx.pnc;
                }
            }
        }

        // Return a group, keep sqrx.state as first state in group.region.
        let regz = SomeRegion::new(vec![sqrx.state.clone(), sta_far]);
        Some(SomeGroup::new(regz, rules, pnc))
    } // end validate_combination

    /// Take an action for a need.
    pub fn take_action_need(
        &mut self,
        dom: usize,
        cur_state: &SomeState,
        ndx: &SomeNeed,
    ) -> SomeSample {
        let astate = self
            .do_something
            .take_action(cur_state, self.dom_num, self.num);

        let asample = SomeSample::new(cur_state.clone(), self.num, astate);
        self.eval_need_sample(ndx, dom, &asample);
        asample
    }

    /// Take an action with the current state.
    pub fn take_action_step(&mut self, cur_state: &SomeState) -> SomeSample {
        let astate = self
            .do_something
            .take_action(cur_state, self.dom_num, self.num);

        let asample = SomeSample::new(cur_state.clone(), self.num, astate);
        self.eval_step_sample(&asample);
        asample
    }

    /// Take an action with a given state.
    pub fn take_action_arbitrary(&mut self, cur_state: &SomeState) -> SomeSample {
        //println!("action {} take_action_arbitrary", self.num);
        let astate = self
            .do_something
            .take_action(cur_state, self.dom_num, self.num);

        let asample = SomeSample::new(cur_state.clone(), self.num, astate);

        self.eval_sample(&asample);

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

    /// Display anchor rates, like (number adjacent anchors, number other adjacent squares only in one region, samples)
    pub fn display_anchor_info(&self) -> Result<(), String> {
        println!("Act: {} group anchor rates", self.num);
        for grpx in self.groups.iter() {
            if grpx.anchor.is_some() {
                self.display_group_anchor_info2(grpx)?
            }
        } // next grpx
        Ok(())
    }

    /// Display a groups' achor info, given the groups' region.
    pub fn display_group_anchor_info(&self, aregion: &SomeRegion) -> Result<(), String> {
        if let Some(grpx) = self.groups.find(aregion) {
            self.display_group_anchor_info2(grpx)
        } else {
            Err("Group with region {aregion} not found".to_string())
        }
    }

    /// Display a groups' achor info.
    pub fn display_group_anchor_info2(&self, grpx: &SomeGroup) -> Result<(), String> {
        if let Some(anchor) = &grpx.anchor {
            println!("\nGroup:   {}", grpx.region);
            let sqrx = self
                .squares
                .find(anchor)
                .expect("Group anchor should refer to an existing square");
            let rate = self.group_anchor_rate(grpx, anchor);
            println!("anchor {sqrx} rate {:?}", rate);
            let stas_adj = self.squares.stas_adj_reg(&grpx.region);
            for stax in stas_adj.iter() {
                if stax.is_adjacent(anchor) {
                    let sqrx = self.squares.find(stax).expect(
                        "Call to stas_adj_reg should return states that refer to existing squares",
                    );
                    let grps = self.groups.groups_state_in(stax);
                    if grps.len() == 1 {
                        if let Some(grpy) = self.groups.find(grps[0]) {
                            if let Some(anchory) = &grpy.anchor {
                                if *stax == anchory {
                                    println!("adj    {sqrx} in one group {} is anchor", grps[0]);
                                } else {
                                    println!("adj    {sqrx} in one group {}", grps[0]);
                                }
                            } else {
                                println!("adj    {sqrx} in one group {}", grps[0]);
                            }
                        } else {
                            return Err(format!("group {} not found?", grps[0]));
                        }
                    } else {
                        println!(
                            "adj    {sqrx} in groups {}",
                            SomeRegion::vec_ref_string(&grps)
                        );
                    }
                }
            } // next stax
        } else {
            println!("Group {} does not have an anchor defined", grpx.region);
        }
        Ok(())
    }

    pub fn check_limited(&mut self, change_mask: &SomeMask) {
        self.groups.check_limited(change_mask);
    }
} // end impl SomeAction

// Some action tests are made from the domain level.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;

    #[test]
    fn two_result_group() -> Result<(), String> {
        // Init action
        let tmp_bts = SomeBits::new(1);
        let mut act0 = SomeAction::new(0, 0, SomeMask::new(tmp_bts.clone()));
        let tmp_sta = SomeState::new(tmp_bts.clone());

        // Put in two incompatible one-result squares, but both subset of the
        // later two-result squares.
        // 0->1 and 0->1, in the fourth bit.
        let s1 = tmp_sta.new_from_string("s0b0001").unwrap();
        let s9 = tmp_sta.new_from_string("s0b1001").unwrap();
        act0.eval_sample(&SomeSample::new(s1.clone(), 0, s9.clone()));
        let s5 = tmp_sta.new_from_string("s0b0101").unwrap();
        act0.eval_sample(&SomeSample::new(s5.clone(), 0, s5.clone()));

        // Set up first two_result square.
        let s0 = tmp_sta.new_from_string("s0b0000").unwrap();
        let s8 = tmp_sta.new_from_string("s0b1000").unwrap();
        act0.eval_sample(&SomeSample::new(s0.clone(), 0, s0.clone()));
        act0.eval_sample(&SomeSample::new(s0.clone(), 0, s8.clone()));
        act0.eval_sample(&SomeSample::new(s0.clone(), 0, s0.clone()));
        act0.eval_sample(&SomeSample::new(s0.clone(), 0, s8.clone()));

        // Set up second two_result square.
        let s7 = tmp_sta.new_from_string("s0b0111").unwrap();
        let sf = tmp_sta.new_from_string("s0b1111").unwrap();
        act0.eval_sample(&SomeSample::new(s7.clone(), 0, sf.clone()));
        act0.eval_sample(&SomeSample::new(s7.clone(), 0, s7.clone()));
        act0.eval_sample(&SomeSample::new(s7.clone(), 0, sf.clone()));
        act0.eval_sample(&SomeSample::new(s7.clone(), 0, s7.clone()));

        println!("Groups {}", act0.groups);
        assert!(act0.groups.len() == 1);

        Ok(())
    }

    #[test]
    fn groups_formed_1() -> Result<(), String> {
        // Init action
        let tmp_bts = SomeBits::new(1);
        let mut act0 = SomeAction::new(0, 0, SomeMask::new(tmp_bts.clone()));
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        // Eval sample that other samples will be incompatible with.
        let s7 = tmp_sta.new_from_string("s0b0111").unwrap();
        act0.eval_sample(&SomeSample::new(s7.clone(), 0, s7.clone()));

        // Process three similar samples.
        act0.eval_sample(&SomeSample::new(
            tmp_sta.new_from_string("s0b1011")?,
            0,
            tmp_sta.new_from_string("s0b1010")?,
        ));

        act0.eval_sample(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            0,
            tmp_sta.new_from_string("s0b1100")?,
        ));

        act0.eval_sample(&SomeSample::new(
            tmp_sta.new_from_string("s0b0001")?,
            0,
            tmp_sta.new_from_string("s0b0000")?,
        ));

        println!("Groups {}", act0.groups);
        assert!(act0.groups.len() == 4);

        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("r0111")?)
            .is_some());
        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("r1xx1")?)
            .is_some());
        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("rxx01")?)
            .is_some());
        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("rx0x1")?)
            .is_some());
        Ok(())
    }

    // Test making a group from two Pn::Two squares.
    #[test]
    fn possible_region() -> Result<(), String> {
        let tmp_bts = SomeBits::new(1);
        let mut act0 = SomeAction::new(0, 0, SomeMask::new(tmp_bts.clone()));
        let tmp_sta = SomeState::new(tmp_bts.clone());

        // Set up 2-result square sf.
        let sf = tmp_sta.new_from_string("s0b1111")?;
        let se = tmp_sta.new_from_string("s0b1110")?;

        act0.eval_sample(&SomeSample::new(sf.clone(), 0, sf.clone()));
        act0.eval_sample(&SomeSample::new(sf.clone(), 0, se.clone()));
        act0.eval_sample(&SomeSample::new(sf.clone(), 0, sf.clone()));
        act0.eval_sample(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up 2-result square s1.
        let s1 = tmp_sta.new_from_string("s0b0001")?;
        let s0 = tmp_sta.new_from_string("s0b0000")?;
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
                tmp_sta.to_mask().new_from_string("m0b1111")?,
                tmp_sta.to_mask().new_from_string("m0b1111")?,
            ),
        );
        println!("Act: {}", &act0);
        println!("needs: {}", nds);

        assert!(nds.len() == 1);
        assert!(act0.groups.len() == 1);

        Ok(())
    }

    // Test making a region from three samples.
    #[test]
    fn three_sample_region1() -> Result<(), String> {
        // Init action.
        let tmp_bts = SomeBits::new(1);
        let mut act0 = SomeAction::new(0, 0, SomeMask::new(tmp_bts.clone()));
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        // Set up square 0.
        let s0 = tmp_sta.new_from_string("s0b0000")?;
        act0.eval_sample(&SomeSample::new(s0.clone(), 0, s0.clone()));

        // Set up square 3.
        let s3 = tmp_sta.new_from_string("s0b0011")?;
        act0.eval_sample(&SomeSample::new(s3.clone(), 0, s3.clone()));

        // Set up square 5.
        let s5 = tmp_sta.new_from_string("s0b0101")?;
        act0.eval_sample(&SomeSample::new(s5.clone(), 0, s5.clone()));

        println!("Act: {}", &act0);

        assert!(act0.groups.len() == 1);
        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("r0xxx")?)
            .is_some());

        Ok(())
    }
    // Test making a region from three samples, with one dissimilar sample.
    #[test]
    fn three_sample_region2() -> Result<(), String> {
        // Init action.
        let tmp_bts = SomeBits::new(1);
        let mut act0 = SomeAction::new(0, 0, SomeMask::new(tmp_bts.clone()));
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        // Set up square 0.
        let s0 = tmp_sta.new_from_string("s0b0000")?;
        act0.eval_sample(&SomeSample::new(s0.clone(), 0, s0.clone()));

        // Set up square 3.
        let s3 = tmp_sta.new_from_string("s0b0011")?;
        act0.eval_sample(&SomeSample::new(s3.clone(), 0, s3.clone()));

        // Set up square 4, dissimilar to s5 by third bit being 1->0.
        let s4 = tmp_sta.new_from_string("s0b0100")?;
        act0.eval_sample(&SomeSample::new(s4.clone(), 0, s0.clone()));

        // Set up square 5.
        let s5 = tmp_sta.new_from_string("s0b0101")?;
        act0.eval_sample(&SomeSample::new(s5.clone(), 0, s5.clone()));

        println!("Act: {}", &act0);

        assert!(act0.groups.len() == 3);
        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("r0x00")?)
            .is_some());
        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("r00xx")?)
            .is_some());
        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("r0xx1")?)
            .is_some());

        Ok(())
    }

    // Test making a region from three samples, with two similar samples that cannot be combined,
    // due to the second bits being 1->0 and 1->1.
    #[test]
    fn three_sample_region3() -> Result<(), String> {
        // Init action.
        let tmp_bts = SomeBits::new(1);
        let mut act0 = SomeAction::new(0, 0, SomeMask::new(tmp_bts.clone()));
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);

        // Set up square 2.
        let s2 = tmp_sta.new_from_string("s0b0010")?;
        let s0 = tmp_sta.new_from_string("s0b0000")?;
        act0.eval_sample(&SomeSample::new(s2.clone(), 0, s0.clone()));

        // Set up square b.
        let sb = tmp_sta.new_from_string("s0b1011")?;
        act0.eval_sample(&SomeSample::new(sb.clone(), 0, sb.clone()));

        // Set up square 5.
        let s5 = tmp_sta.new_from_string("s0b0101")?;
        act0.eval_sample(&SomeSample::new(s5.clone(), 0, s5.clone()));

        println!("Act: {}", &act0);

        assert!(act0.groups.len() == 2);
        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("r0xxx")?)
            .is_some());
        assert!(act0
            .groups
            .find(&tmp_reg.new_from_string("rxxx1")?)
            .is_some());

        Ok(())
    }
}
