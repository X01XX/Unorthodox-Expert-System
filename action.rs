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
use crate::region::{AccessStates, SomeRegion};
use crate::regionstore::RegionStore;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::square::SomeSquare;
use crate::squarestore::{PickError, SquareStore};
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::step::{AltRuleHint, SomeStep};
use crate::stepstore::StepStore;
use crate::tools::{self, not};

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
        write!(f, "{}", self.formatted_string())
    }
}

const MAX_MEMORY: usize = 20; // Max number of recent squares/samples to keep in a circular buffer.

#[readonly::make]
#[derive(Serialize, Deserialize)]
/// The SomeAction struct, aggregate the best current guess at what an action
/// will do for any state.
pub struct SomeAction {
    /// Action number, index/key into parent ActionStore vector.
    pub id: usize,
    /// Parent Domain number.
    pub dom_id: usize,
    /// Store for groups of compatible-change squares.
    pub groups: GroupStore,
    /// A store of squares sampled for an action.
    pub squares: SquareStore,
    /// Regions of invalidated groups indicate a new edge of the solution.
    /// Closer and closer dissimilar squares are sought, producing smaller and smaller
    /// regions, until a pair of adjacent, dissimilar, squares are found.
    do_something: ActionInterface,
    /// Trigger cleanup logic after a number of new squares.
    cleanup_trigger: usize,
    /// When the actions groups change check for any missed regions.
    check_remainder: bool,
    remainder_check_regions: RegionStore,
    /// Memory for past samples that were not stored in a square, or squares removed from the cleanup function, oldest first.
    pub memory: VecDeque<SomeSquare>,
}

impl SomeAction {
    /// Return a new SomeAction struct, given the number integers used in the SomeBits struct.
    /// The action number, an index into the ActionStore that will contain it, is set to zero and
    /// changed later.
    pub fn new(act_id: usize, dom_id: usize, rules: Vec<RuleStore>) -> Self {
        SomeAction {
            id: act_id,
            dom_id,
            groups: GroupStore::new(vec![]),
            squares: SquareStore::new(HashMap::new()),
            do_something: ActionInterface::new(rules),
            cleanup_trigger: CLEANUP,
            check_remainder: false,
            remainder_check_regions: RegionStore::new(vec![]),
            memory: VecDeque::<SomeSquare>::with_capacity(MAX_MEMORY),
        }
    }

    /// Return the index in memory of a square, given a key.
    fn memory_index(&self, key: &SomeState) -> Option<usize> {
        for (inx, sqrx) in self.memory.iter().enumerate() {
            if sqrx.state == *key {
                return Some(inx);
            }
        }
        None
    }

    /// Add a new square from a sample.
    /// Check the consequenses of adding the square.
    pub fn add_new_sample(&mut self, smpl: &SomeSample) -> &SomeSquare {
        if let Some(inx) = self.memory_index(&smpl.initial) {
            if let Some(mut sqrx) = self.memory.remove(inx) {
                sqrx.add_sample(smpl);
                return self.add_new_square(sqrx);
            }
        }

        self.add_new_square(SomeSquare::new(smpl))
    }

    /// Add a new square.
    fn add_new_square(&mut self, sqrx: SomeSquare) -> &SomeSquare {
        if self.cleanup_trigger > 0 {
            self.cleanup_trigger -= 1;
        }
        let key = sqrx.state.clone();
        self.squares.insert(sqrx, self.dom_id, self.id);
        self.squares.find(&key).expect("SNH")
    }

    /// Evaluate a new or changed square.
    fn eval_changed_square(&mut self, key: &SomeState) {
        //println!("SomeAction:eval_changed_square {key}");
        if let Some(sqrx) = self.squares.find_mut(key) {
            // Check if it invalidates any groups.
            let regs_invalid: RegionStore = self.groups.check_square(sqrx, self.dom_id, self.id);

            let pnc = sqrx.pnc;

            if regs_invalid.is_not_empty() {
                self.check_remainder = true;
                self.process_invalid_regions(&regs_invalid);
            }

            if !self.groups.any_superset_of(key) {
                self.create_groups_from_squares(&[key.clone()]);
            }
            if not(pnc) {
                return;
            }
        } else {
            return;
        }

        // Square is pnc, check if a group can be confirmed.
        for grpx in self.groups.iter_mut() {
            if grpx.pnc || !grpx.region.is_superset_of(key) {
                continue;
            }

            if grpx.region.len() == 1 {
                grpx.set_pnc(self.dom_id, self.id);
            } else if grpx.region.len() == 2 {
                if key == grpx.region.state1() {
                    if let Some(sqr2) = self.squares.find(grpx.region.state2()) {
                        if sqr2.pnc {
                            grpx.set_pnc(self.dom_id, self.id);
                        }
                    }
                } else if key == grpx.region.state2() {
                    if let Some(sqr1) = self.squares.find(grpx.region.state1()) {
                        if sqr1.pnc {
                            grpx.set_pnc(self.dom_id, self.id);
                        }
                    }
                }
            }
        }
    }

    /// Do basic functions for any new sample.
    /// Return true if a matching square exists.
    pub fn eval_sample(&mut self, smpl: &SomeSample) -> bool {
        // println!("eval_sample: {}", smpl);
        // Check if a square already exists.
        if let Some(sqrx) = self.squares.find_mut(&smpl.initial) {
            if sqrx.add_sample(smpl) {
                self.eval_changed_square(&smpl.initial);
            }
            return true;
        }

        // Check if the sample invalidates any groups.
        if self.groups.any_groups_invalidated(smpl) {
            // Add new square.
            self.add_new_sample(smpl);
            self.eval_changed_square(&smpl.initial);

            return true;
        }

        // Create group from square.
        if !self.groups.any_superset_of(&smpl.initial) {
            self.add_new_sample(smpl);
            self.create_groups_from_squares(&[smpl.initial.clone()]);
            return true;
        }
        false
    }

    /// Evaluate an arbitrary sample, creating a square if needed.
    pub fn eval_sample_arbitrary(&mut self, smpl: &SomeSample) {
        if !self.eval_sample(smpl) {
            self.add_new_sample(smpl);
        }
    }

    /// Check invalid regions for orphaneh squares, create new regions.
    fn process_invalid_regions(&mut self, invalid_regs: &RegionStore) {
        // Load states from squares in the invalidated regions.
        let mut stas_in_regs = StateStore::new(vec![]);
        for regx in invalid_regs.iter() {
            let stas_in = self.squares.stas_in_reg(regx);
            for stax in stas_in.iter() {
                if !stas_in_regs.contains(stax) {
                    // Skip duplicates caused by overlapping regions.
                    stas_in_regs.push(stax.clone());
                }
            }
        }
        // Store states not in any groups.
        let mut orphaned_stas = Vec::<SomeState>::new();
        for stax in stas_in_regs.iter() {
            if !self.groups.any_superset_of(stax) {
                orphaned_stas.push((*stax).clone());
            }
        }
        // Try creating groups from each square.
        if orphaned_stas.is_empty() {
        } else {
            self.create_groups_from_squares(&orphaned_stas);
        }
    }

    /// Create possible groups from one, or more, states.
    fn create_groups_from_squares(&mut self, keys: &[SomeState]) {
        assert!(!keys.is_empty());
        self.check_remainder = true;

        // Collect possible groups.
        let groups: Vec<SomeGroup> = if keys.len() == 1 {
            self.create_groups_from_squares2(&keys[0])
        } else {
            keys.par_iter() // par_iter for parallel processing, iter for sequential diagnostic messages.
                .map(|keyx| self.create_groups_from_squares2(keyx))
                .flatten()
                .collect::<Vec<SomeGroup>>()
        };

        // Store possible groups.
        for grpx in groups {
            if !self.groups.any_superset_of(&grpx.region) {
                self.groups.push_nosubs(grpx, self.dom_id, self.id);
            }
        }
    }

    /// Check groups due to a new, or updated, square.
    fn create_groups_from_squares2(&self, key: &SomeState) -> Vec<SomeGroup> {
        //println!("create_groups_from_squares2 {}", key);
        debug_assert!(!self.groups.any_superset_of(key));

        // Lookup square.
        let sqrx = self
            .squares
            .find(key)
            .expect("key should refer to an existing square");

        // Check if square can be used to create groups.
        // Allowing a square to make a group with a single sample is needed
        // for group bootstrapping.
        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return vec![];
        }

        //println!("Checking Square {} for new groups", &sqrx.str_terse());

        // Get possible regions, sqrx.state will be <region>.state1
        // Duplicate group regions are possible.
        self.possible_groups_from_square(sqrx)
    } // end create_groups_from_squares2

    /// Return needs for states that are not in a group.
    /// The Domain current state for which there are no samples.
    /// A pn > 1 state that needs more samples.
    pub fn state_not_in_group_needs(&self, cur_state: &SomeState) -> NeedStore {
        let mut nds = NeedStore::new(vec![]);

        // Check if current state is in any groups
        if !self.groups.any_superset_of(cur_state) {
            if let Some(sqrx) = self.squares.find(cur_state) {
                if sqrx.pn == Pn::One || sqrx.pnc {
                    println!(
                        "problem?: Dom {} Act {} square {} not in group?",
                        self.dom_id, self.id, sqrx
                    );
                } else {
                    let mut needx = SomeNeed::StateNotInGroup {
                        dom_id: self.dom_id,
                        act_id: self.id,
                        target_state: cur_state.clone(),
                        priority: 0,
                    };
                    needx.set_priority();
                    nds.push(needx);
                    return nds;
                }
            } else {
                let mut needx = SomeNeed::StateNotInGroup {
                    dom_id: self.dom_id,
                    act_id: self.id,
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
        let sqrs_pngt1 = self.squares.pn_gt1_no_pnc();

        for stax in sqrs_pngt1.iter() {
            if stax == cur_state || self.groups.any_superset_of(stax) {
                continue;
            }

            let mut needx = SomeNeed::StateNotInGroup {
                dom_id: self.dom_id,
                act_id: self.id,
                target_state: (*stax).clone(),
                priority: cur_state.distance(stax),
            };
            needx.set_priority();
            nds.push(needx);
        } // next stax

        nds
    }
    /// Get needs for an Action, to improve understanding of the result pattern(s).
    /// When most needs are satisfied, needs for group limitation are generated.
    /// If housekeeping needs are generated, they are processed and needs
    /// are checked again.
    pub fn get_needs(
        &mut self,
        cur_state: &SomeState,
        dom_id: usize,
        max_reg: &SomeRegion,
    ) -> NeedStore {
        let mut ret = NeedStore::new(vec![]);

        let mut found = true;
        while found {
            found = false;

            ret = self.get_needs2(cur_state, dom_id, max_reg);

            // Check memory.

            // For each need
            for ndx in ret.iter() {
                let mut inx: Option<usize> = None;

                // For each memory item, oldest first.
                // If more than one sample is needed, and exists, sequence will be preserved.
                let targx = ndx.target();
                for (iny, sqrx) in self.memory.iter().enumerate() {
                    if targx[0].is_superset_of(sqrx) {
                        //println!("Memory square {} found for need {}", sqrx, ndx);
                        inx = Some(iny);
                        break;
                    }
                } // next sqrx

                // If a square is found, remove it, use it.
                if let Some(iny) = inx {
                    if let Some(sqrx) = self.memory.remove(iny) {
                        // Process square as a series of samples.
                        let key = sqrx.state.clone();
                        self.squares.insert(sqrx, self.dom_id, self.id);
                        self.eval_changed_square(&key);
                        found = true;
                        break;
                    } else {
                        panic!("SNH");
                    }
                }
            } // next ndx
        } // end while
        ret
    }

    /// Get needs from eagh action.
    pub fn get_needs2(
        &mut self,
        cur_state: &SomeState,
        dom_id: usize,
        max_reg: &SomeRegion,
    ) -> NeedStore {
        //println!("Running Action {}::get_needs {}", self.num, cur_state);

        // loop until no housekeeping need is returned.
        let mut nds = NeedStore::new(vec![]);
        let mut cnt = 0;
        loop {
            cnt += 1;

            // Look for needs to find a new edge in an invalidated group
            //nds.append(self.seek_edge_needs());

            // Check for additional samples for group states needs
            nds.append(self.confirm_group_needs());

            // Check any two groups for:
            // Overlapping groups that form a contradictory intersection.
            nds.append(self.group_pair_needs());

            // Check for expand needs.
            if nds.is_empty() {
                nds.append(self.expand_groups_needs(max_reg));
            }

            // Check for expand needs.
            //nds.append(self.expand_groups_needs());

            // Check for squares in-one-group needs
            if let Some(ndx) = self.limit_groups_needs(max_reg) {
                nds.append(ndx);
            }

            // Check for repeating housekeeping needs loop
            if cnt > 20 {
                println!("needs: {}", &nds);
                panic!("Dom {} Act {} loop count GT 20!", dom_id, self.id);
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
                if let SomeNeed::AddGroup {
                    group_region,
                    rules,
                } = ndx
                {
                    if !new_grp_regs.contains(group_region) {
                        continue;
                    }

                    // Check for supersets
                    if self.groups.any_superset_of(group_region) {
                        if self.groups.find(group_region).is_some() {
                        } else {
                            println!(
                                "\nDom {} Act {} **** Supersets found for new group {} in {}",
                                dom_id,
                                self.id,
                                &group_region,
                                tools::vec_ref_string(&self.groups.supersets_of(group_region))
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

                    self.groups.push_nosubs(
                        SomeGroup::new(group_region.clone(), rules.clone(), pnc),
                        dom_id,
                        self.id,
                    );
                    try_again = true;
                }
            } // next ndx

            if !try_again {
                //println!("Act: {} get_needs: returning: {}", &self.num, &nds);
                //if nds.is_empty() {
                //  return self.left_over_needs();
                //}

                // Filter out housekeeping needs, if any.
                let mut inxs = Vec::<usize>::with_capacity(nds.len());

                for (inx, ndx) in nds.iter().enumerate() {
                    if let SomeNeed::AddGroup { .. } = ndx {
                        inxs.push(inx);
                    }
                }

                // Remove houskeeping needs, from highest to lowest index.
                for inx in inxs.iter().rev() {
                    nds.remove_unordered(*inx);
                }

                // Do cleanup
                if self.cleanup_trigger == 0 {
                    self.cleanup(&nds, dom_id);
                    self.cleanup_trigger = CLEANUP;
                }

                // Checks that will not return housekeeping needs

                // Look for needs for states not in groups
                nds.append(self.state_not_in_group_needs(cur_state));

                if nds.kind_is_in("StateNotInGroup") {
                } else {
                    if self.check_remainder {
                        self.remainder_check_regions = self.remainder_check_region(max_reg);
                        self.check_remainder = false;
                    }
                    for regx in self.remainder_check_regions.iter() {
                        let mut needx = SomeNeed::StateInRemainder {
                            dom_id: self.dom_id,
                            act_id: self.id,
                            target_region: regx.clone(),
                            priority: 0,
                        };
                        needx.set_priority();
                        nds.push(needx);
                    }
                }

                return nds;
            }

            nds = NeedStore::new(vec![]);
        } // end loop
    } // end get_needs

    /// Check for needs in a region not covered by current groups.
    fn remainder_check_region(&self, max_reg: &SomeRegion) -> RegionStore {
        let mut remainder_regs = RegionStore::new(vec![max_reg.clone()]);

        for grpx in self.groups.iter() {
            remainder_regs = remainder_regs.subtract_item(&grpx.region);
        }

        //if remainder_regs.is_not_empty() {
        //    println!(
        //        "Dom {} Act {} remainder is {}",
        //        self.dom_id, self.id, remainder_regs
        //    );
        //}

        remainder_regs
    }

    /// Cleanup unneeded squares.
    fn cleanup(&mut self, needs: &NeedStore, dom_id: usize) {
        // Store for keys of squares to delete.
        let mut to_del = StateStore::new(vec![]);

        'next_sqr: for keyx in self.squares.ahash.keys() {
            // Check needs
            for ndx in needs.iter() {
                for targx in ndx.target().iter() {
                    if targx.is_superset_of(keyx) {
                        continue 'next_sqr;
                    }
                }
            }

            // Identify shared symmetric groups.
            let mut shared_regions = RegionStore::new(vec![]);
            for inx in 0..(self.groups.len() - 1) {
                let grpx = &self.groups[inx];

                for iny in (inx + 1)..self.groups.len() {
                    let grpy = &self.groups[iny];

                    if grpy.pn != grpx.pn {
                        continue;
                    }
                    if let Some(shared) = grpy.region.shared_symmetric_region(&grpx.region) {
                        if grpx.pn == Pn::Unpredictable {
                            shared_regions.push(shared);
                        } else if let Some(ruls) = &grpx.rules {
                            let rulsx = ruls.restrict_initial_region(&shared);
                            if let Some(ruls) = &grpy.rules {
                                let rulsy = ruls.restrict_initial_region(&shared);

                                if rulsx.union(&rulsy).is_some() {
                                    //println!("shared region : {} {} {shared}", grpx.region, grpy.region);
                                    shared_regions.push(shared);
                                }
                            }
                        }
                    }
                } // next iny
            } // next inx
              // Delete shared symmetric groups.
            for regx in shared_regions.iter() {
                self.groups.remove_subsets_of(regx, dom_id, self.id);
            }

            // Don't delete squares in groups.
            // let mut in_groups = false;
            for grpx in self.groups.iter() {
                if grpx.is_superset_of(keyx) {
                    for stax in grpx.region.states.iter() {
                        if stax == keyx {
                            continue 'next_sqr;
                        }
                    }
                    if grpx.region.states.len() > 2 && grpx.region.state2() == keyx {
                        continue 'next_sqr;
                    }

                    if let Some(stay) = &grpx.anchor {
                        if stay == keyx {
                            continue 'next_sqr;
                        }
                        if *keyx == grpx.region.state_far_from(stay) {
                            continue 'next_sqr;
                        }
                    }
                } else if let Some(stay) = &grpx.anchor {
                    if keyx.is_adjacent(stay) {
                        continue 'next_sqr;
                    }
                }
            }

            // Don't delete squares that are not in a group.
            // That is, squares with Pn: > One that need more samples.
            if self.groups.num_groups_in(keyx) == 0 {
                continue;
            }

            // Add square key to delete vector.
            to_del.push(keyx.clone());
        }

        // Delete squares.
        if to_del.is_empty() {
        } else {
            println!(
                "\nDom {} Act {} deleted unneeded squares: {}",
                self.dom_id, self.id, to_del
            );
            for keyx in to_del.iter() {
                if let Some(sqrx) = self.squares.remove(keyx) {
                    self.add_square_to_memory(sqrx);
                }
            }
        }
    } // end cleanup

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
                    dom_id: self.dom_id,
                    act_id: self.id,
                    target_state: grpx.region.state1().clone(),
                    grp_reg: grpx.region.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.set_priority();
                ret_nds.push(needx);
            }

            // If this is a one-state group ..
            if grpx.one_state() {
                //if sqrx.pnc {
                //    grpx.set_pnc(self.dom_id, self.id, 5);
                //}
                continue;
            }

            if let Some(sqry) = self.squares.find(grpx.region.state2()) {
                if sqry.pnc {
                    //if sqrx.pnc {
                    //    grpx.set_pnc(self.dom_id, self.id, 6);
                    //}
                    continue;
                }
            }

            let mut needx = SomeNeed::ConfirmGroup {
                dom_id: self.dom_id,
                act_id: self.id,
                target_state: grpx.region.state2().clone(),
                grp_reg: grpx.region.clone(),
                priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
            };
            needx.set_priority();
            ret_nds.push(needx);
        } // next grpx

        ret_nds
    } // end confirm_group_needs

    /// Return group expansion needs.
    /// Based on dissimilar groups.
    pub fn expand_groups_needs(&mut self, max_reg: &SomeRegion) -> NeedStore {
        let mut ret_nds = NeedStore::new(vec![]);

        if self.groups.len() < 2 {
            return ret_nds;
        }

        // Storage for non-intersecting, dissimilar group regions.
        let mut pairs = Vec::<(&SomeRegion, &SomeRegion)>::new();

        for inx in 0..(self.groups.len() - 1) {
            let grpx = &self.groups[inx];
            if grpx.region.intersects(max_reg) {
            } else {
                continue;
            }

            for iny in (inx + 1)..self.groups.len() {
                let grpy = &self.groups[iny];

                if grpy.region.intersects(max_reg) {
                } else {
                    continue;
                }

                if grpx.region.intersects(&grpy.region) {
                    continue;
                }

                if grpy.pn != grpx.pn {
                    pairs.push((&grpx.region, &grpy.region));
                } else if grpy.pn == Pn::Unpredictable {
                    continue;
                } else if let Some(xrules) = &grpx.rules {
                    if let Some(yrules) = &grpy.rules {
                        if xrules.union(yrules).is_none() {
                            pairs.push((&grpx.region, &grpy.region));
                        }
                    }
                }
            }
        }

        // Init initial possible regions.
        let mut poss_regs = RegionStore::new(vec![max_reg.clone()]);

        // Add each pair compliment.
        for pairx in pairs.iter() {
            poss_regs = poss_regs.intersection(
                &pairx
                    .0
                    .complement_in(max_reg)
                    .union(&pairx.1.complement_in(max_reg)),
            );
        }

        //println!("Possible regions: {}", poss_regs);

        // Analyze results.

        // Check each group.
        for grpx in self.groups.iter() {
            for regx in poss_regs.iter() {
                if !regx.is_superset_of(&grpx.region) || grpx.region == *regx {
                    continue;
                }

                if let Some(regx_int) = regx.intersection(max_reg) {
                    let target_reg = regx_int.far_reg(&grpx.region);

                    match self.squares.pick_a_square_in(&target_reg) {
                        Ok(sqrx) => {
                            let mut ndx = SomeNeed::ExpandGroup {
                                dom_id: self.dom_id,
                                act_id: self.id,
                                group_region: grpx.region.clone(),
                                expand_region: regx_int.clone(),
                                target_region: SomeRegion::new(vec![sqrx.state.clone()]),
                                priority: 0,
                            };
                            ndx.set_priority();
                            ret_nds.push(ndx);
                        }
                        Err(PickError::NoSquares) => {
                            let mut ndx = SomeNeed::ExpandGroup {
                                dom_id: self.dom_id,
                                act_id: self.id,
                                group_region: grpx.region.clone(),
                                expand_region: regx_int.clone(),
                                target_region: regx_int.far_reg(&grpx.region),
                                priority: 0,
                            };
                            ndx.set_priority();
                            ret_nds.push(ndx);
                        }
                        Err(PickError::PncSquare) => (),
                    }
                }
            } // next regx
        } // next grpx

        // Check each region.
        'next_regx: for regx in poss_regs.iter() {
            for grpx in self.groups.iter() {
                if regx.intersects(&grpx.region) {
                    continue 'next_regx;
                }
            }
            // No groups in regx.
            match self.squares.pick_a_square_in(regx) {
                Ok(sqrx) => {
                    // Found square that needs more samples.
                    let mut needx = SomeNeed::StateNotInGroup {
                        dom_id: self.dom_id,
                        act_id: self.id,
                        target_state: sqrx.state.clone(),
                        priority: 0,
                    };
                    needx.set_priority();
                    ret_nds.push(needx);
                }
                Err(PickError::NoSquares) => {
                    let mut needx = SomeNeed::SampleInRegion {
                        dom_id: self.dom_id,
                        act_id: self.id,
                        target_region: regx.clone(),
                        priority: 0,
                    };
                    needx.set_priority();
                    ret_nds.push(needx);
                }
                Err(PickError::PncSquare) => {
                    println!("Pnc square found in {regx} ?");
                }
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
    pub fn limit_groups_needs(&mut self, max_reg: &SomeRegion) -> Option<NeedStore> {
        //println!("limit_groups_needs chg {}", change_mask);

        //let mut ret_nds = NeedStore::new(vec![]);

        // Check if groups current anchors are still in only one region.
        let mut remove_anchor = Vec::<SomeRegion>::new();
        for grpx in self.groups.iter() {
            let Some(stax) = &grpx.anchor else {
                continue;
            };

            if !self.groups.in_1_group(stax) {
                remove_anchor.push(grpx.region.clone());
            }
        }
        // Remove anchors, as needed.
        for regx in remove_anchor.iter() {
            if let Some(grpx) = self.groups.find_mut(regx) {
                //println!("set {} anchor off", grpx.region);
                grpx.set_anchor_off();
            }
        }

        let mut ret_nds = NeedStore::new(vec![]);

        // Reset limited indicator to recheck.
        for grpx in self.groups.iter_mut() {
            if not(grpx.pnc) {
                continue;
            }
            if !grpx.limited {
                continue;
            }
            if let Some(anchor) = &grpx.anchor {
                if max_reg.is_superset_of(anchor) {
                    if let Some(anchor_mask) = &grpx.anchor_mask {
                        let max_mask = max_reg.x_mask().bitwise_and(&grpx.region.edge_mask());
                        if *anchor_mask != max_mask {
                            grpx.set_limited_off(self.dom_id, self.id);
                        }
                    }
                }
            }
        }

        // Get anchor needs.
        let mut try_again = true;
        while try_again {
            try_again = false;

            // Gather group anchor adjacent needs.
            // Possibly decide to turn the limited flag on.
            let mut set_on = Vec::<(SomeRegion, SomeMask)>::new();

            for group_num in 0..self.groups.len() {
                let regx = self.groups[group_num].region.clone();
                {
                    let grpx = self.groups.find(&regx).expect("SNH");

                    if grpx.limited {
                        continue;
                    }
                    if not(grpx.pnc) {
                        continue;
                    }
                }
                let ndsx = self.limit_group_anchor_needs(&regx, group_num, max_reg);
                if ndsx.is_not_empty() {
                    ret_nds.append(ndsx);
                    continue;
                }

                let grpx_anchor = self
                    .groups
                    .find(&regx)
                    .as_ref()
                    .expect("SNH")
                    .anchor
                    .clone();
                if let Some(anchor) = &grpx_anchor {
                    if max_reg.is_superset_of(anchor) {
                        if let Some(ndx) =
                            self.limit_group_adj_needs(&regx, anchor, max_reg, group_num)
                        {
                            ret_nds.append(ndx);
                            continue;
                        } else {
                            let edges = regx.edge_mask().bitwise_and(&max_reg.x_mask());
                            set_on.push((self.groups[group_num].region.clone(), edges));
                        }
                    }
                }
            }

            // Set limited on for selected groups.
            for (grp_reg, edges) in set_on {
                //let grpx = self.groups.find_mut(&grp_reg).expect("SNH");
                //grpx.set_limited(edges, self.dom_id, self.id);
                self.set_group_limited(&grp_reg, edges);
                try_again = true;
            }
        } // end try_again

        if ret_nds.is_not_empty() {
            return Some(ret_nds);
        }
        None
    } // end limit_groups_needs

    /// Set the limited flag on for a given group.
    /// For other groups, with the limited flag on, if the number of edges
    /// used to limit the group is less than the number used to limit
    /// the given group, set their limit flag of, to recalculate the best
    /// anchor.
    fn set_group_limited(&mut self, grp_reg: &SomeRegion, edges: SomeMask) {
        let num_edges = edges.num_one_bits();

        let grpx = self.groups.find_mut(grp_reg).expect("SNH");
        grpx.set_limited(edges, self.dom_id, self.id);

        for grpy in self.groups.iter_mut() {
            if grpy.limited {
            } else {
                continue;
            }
            if grpy.region == *grp_reg {
                continue;
            }
            if let Some(edgesy) = &grpy.anchor_mask {
                if edgesy.num_one_bits() < num_edges {
                    grpy.set_limited_off(self.dom_id, self.id);
                }
            }
        } // next grpy
    }

    /// Return a rate for a possible anchor for a group.
    /// The rate will be a tuple containing:
    ///     The number of adjacent states that are anchors of other groups,
    ///     The number adjacent states that are in only one group,
    ///     The number of samples taken for the adjacent states.
    /// To set an anchor, a square with at least one sample is required, but ..
    /// To rate a possible anchor state, no sample of the state is requried.
    /// When comparing tuples, Rust compares item pairs in order until there is a difference.
    pub fn group_anchor_rate(&self, regx: &SomeRegion, stax: &SomeState) -> (usize, usize, usize) {
        //assert_eq!(self.groups.num_groups_state_in(stax), 1);
        if !self.groups.in_1_group(stax) {
            return (0, 0, 0);
        }

        let mut anchors = 0;
        let mut in_1_group = 0;
        let mut sqr_samples = 0;

        // Get masks of edge bits to use to limit group.
        let edge_msks = regx.edge_mask().split();

        // Rate adjacent external states
        for edge_bit in &edge_msks {
            let sta_adj = stax.bitwise_xor(edge_bit);
            //println!(
            //    "checking {} adjacent to {} external to {}",
            //    sta_adj, stax, greg
            //);

            let stats = self.groups.in_one_anchor(&sta_adj);

            if stats == Some(true) {
                anchors += 1;
            } else if stats == Some(false) {
                //println!("{} is in only one group", sta_adj);
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
        &mut self,
        regx: &SomeRegion,
        group_num: usize,
        max_reg: &SomeRegion,
    ) -> NeedStore {
        //let grpx = self.groups.find_mut(&regx).expect("SNH");

        let mut ret_nds = NeedStore::new(vec![]);

        let adj_squares = self.squares.stas_adj_reg(regx);

        // For adjacent (other group) anchors,
        // store corresponding state in group region,
        // which has not have been sampled yet.
        let mut stas_in: StateStore = self.squares.stas_in_reg(regx);

        // Home for additional states, that have not been sampled yet, so their
        // reference can be pushed to the stas_in vector.
        let mut additional_stas = Vec::<SomeState>::new();

        for ancx in adj_squares.iter() {
            // Calc state in group that corresponds to an adjacent anchor.
            let stay = ancx.bitwise_xor(&regx.diff_mask(ancx));

            // Check if the state has not been sampled already.
            if !stas_in.contains(&stay) {
                // The state may be in the vertor already, due to being
                // adjacent to more than one external regions' anchor.
                if !additional_stas.contains(&stay) {
                    additional_stas.push(stay);
                }
            }
        }
        // Add additional state references to stas_in vector.
        for stax in additional_stas.iter() {
            stas_in.push(stax.clone());
        }

        let grpx_pn = self.groups.find(regx).as_ref().expect("SNH").pn;
        let grpx_rules = self.groups.find(regx).as_ref().expect("SNH").rules.clone();

        // Calculate shared symmetric regions.
        let mut shared_regions = RegionStore::new(vec![]);
        for (inx, grpy) in self.groups.iter().enumerate() {
            if inx == group_num {
                continue;
            }
            if grpy.pn != grpx_pn {
                continue;
            }
            if let Some(shared) = grpy.region.shared_symmetric_region(regx) {
                if grpx_pn == Pn::Unpredictable {
                    shared_regions.push(shared);
                } else if let Some(ruls) = &grpx_rules {
                    let rulsx = ruls.restrict_initial_region(&shared);
                    if let Some(ruls) = &grpy.rules {
                        let rulsy = ruls.restrict_initial_region(&shared);

                        if rulsx.union(&rulsy).is_some() {
                            //println!("shared region : {} {} {shared}", grpx.region, grpy.region);
                            shared_regions.push(shared);
                        }
                    }
                }
            }
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

        for stax in stas_in.iter() {
            // Potential new anchor must be in only one group.
            if !self.groups.in_1_group(stax)
                || shared_regions.any_superset_of_state(stax)
                || !max_reg.is_superset_of(stax)
            {
                //println!("stax for anchor {stax} in group {} skipped", grpx.region);
                continue;
            }

            //println!("stax for anchor {stax} in group {}", grpx.region);
            let sta_rate = self.group_anchor_rate(regx, stax);

            //println!("group {} possible anchor {} rating {} {} {}", grpx.region, stax, sta_rate.0, sta_rate.1, sta_rate.2);

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
            return NeedStore::new(vec![]);
        }

        // Check current anchor, if any
        let grpx_anchor = self.groups.find(regx).as_ref().expect("SNH").anchor.clone();
        if let Some(anchor) = &grpx_anchor {
            //println!("anchor {} cfmv_max {}", anchor, cfmv_max);
            if cfmv_max.contains(&anchor) {
                //println!("group {} anchor {} still good, cfmv_max", grpx.region, anchor);
                let anchor_sqr = self
                    .squares
                    .find(anchor)
                    .expect("Group anchor state should refer to an existing square");

                if anchor_sqr.pnc {
                    return NeedStore::new(vec![]);
                    // println!("group {} anchor {} pnc", greg, anchor_sta);
                }

                // Get additional samples of the anchor
                let mut needx = SomeNeed::LimitGroup {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    anchor: anchor.clone(),
                    target_state: anchor.clone(),
                    for_group: regx.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.set_priority();
                ret_nds.push(needx);

                return ret_nds;
            }
        }

        // Select an anchor
        let mut cfm_max = cfmv_max[0];

        if cfmv_max.len() > 1 {
            cfm_max = cfmv_max[rand::thread_rng().gen_range(0..cfmv_max.len())];
        }

        if let Some(_sqrx) = self.squares.find(cfm_max) {
            self.groups.find_mut(regx).expect("SNH").set_anchor(cfm_max);
            return NeedStore::new(vec![]);
        } else {
            // Potential anchor not sampled yet.
            let mut needx = SomeNeed::LimitGroup {
                dom_id: self.dom_id,
                act_id: self.id,
                anchor: cfm_max.clone(),
                target_state: cfm_max.clone(),
                for_group: regx.clone(),
                priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
            };
            needx.set_priority();
            ret_nds.push(needx);
        }
        ret_nds
    } // end limit_group_anchor_needs

    /// Return the limiting needs for a group with an anchor chosen, but not yet set to limited.
    pub fn limit_group_adj_needs(
        &mut self,
        regx: &SomeRegion,
        anchor_sta: &SomeState,
        max_reg: &SomeRegion,
        group_num: usize,
    ) -> Option<NeedStore> {
        // let grpx = self.groups.find_mut(&regx).expect("SNH");
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

        // Get masks of edge bits to use to limit group.
        // Ignore bits that cannot be changed by any action.
        if max_reg.is_superset_of(anchor_sta) {
        } else {
            return None;
        }

        let change_bits = regx.edge_mask().bitwise_and(&max_reg.x_mask());

        let edge_msks: Vec<SomeMask> = change_bits.split();

        for mskx in edge_msks {
            let adj_sta = anchor_sta.bitwise_xor(&mskx);

            //println!("*** for group {} checking adj sqr {}", greg, adj_sta);

            if let Some(adj_sqr) = self.squares.find(&adj_sta) {
                if adj_sqr.pnc {
                    // Create new group, if an adjacent square can combine with the anchor.
                    // Current anchor will then be in two regions,
                    // the next run of limit_group_anchor_needs will deal with it.
                    if anchor_sqr.compatible(adj_sqr) == Some(true) {
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
                        dom_id: self.dom_id,
                        act_id: self.id,
                        anchor: anchor_sta.clone(),
                        target_state: adj_sta,
                        for_group: regx.clone(),
                        priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                    };
                    needx.set_priority();
                    nds_grp.push(needx);
                }
            } else {
                // Get first sample of adjacent square.
                let mut needx = SomeNeed::LimitGroupAdj {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    anchor: anchor_sta.clone(),
                    target_state: adj_sta,
                    for_group: regx.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.set_priority();
                nds_grp.push(needx);
            }
        } // next inx in cfm_max

        if nds_grp_add.is_not_empty() {
            //println!("*** nds_grp_add {}", nds_grp_add);
            return Some(nds_grp_add);
        }

        if nds_grp.is_not_empty() {
            //println!("*** nds_grp {}", nds_grp);
            return Some(nds_grp);
        }

        // Process far state, after the anchor and adjacent, external, checks have been made.
        // Instead of checking every adjacent square internal to the group.

        // Group is non-X, so no far state
        if regx.len() == 1 {
            return None;
        }

        let sta_far = regx.state_far_from(anchor_sta);

        if let Some(sqrf) = self.squares.find(&sta_far) {
            if sqrf.pnc {
                return None;
            } else {
                // Get additional samples of the far state.
                let mut needx = SomeNeed::LimitGroup {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    anchor: anchor_sta.clone(),
                    target_state: sta_far,
                    for_group: regx.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.set_priority();
                ret_nds.push(needx);
            }
        } else {
            // Get the first sample of the far state.
            let mut needx = SomeNeed::LimitGroup {
                dom_id: self.dom_id,
                act_id: self.id,
                anchor: anchor_sta.clone(),
                target_state: sta_far,
                for_group: regx.clone(),
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

        // Check for no pairs.
        if self.groups.len() < 2 {
            return nds;
        }

        // Check every pair of one sample, or confirmed, groups.
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

                if grpx.intersects(grpy) {
                    nds.append(self.group_pair_intersection_needs(grpx, grpy, inx));
                }
            } // next iny
        } // next inx

        nds
    } // end group_pair_needs

    /// Check two intersecting groups for needs.
    /// Possibly combining two groups.
    /// Possibly checking for a contradictatory intersection.
    pub fn group_pair_intersection_needs(
        &self,
        grpx: &SomeGroup,
        grpy: &SomeGroup,
        group_num: usize,
    ) -> NeedStore {
        //println!(
        //  "groups_pair_intersection_needs {} {} and {} {}",
        //  &grpx.region, grpx.pn, grpy.region, grpy.pn
        //);

        let mut nds = NeedStore::new(vec![]);

        // Check if Pn indicates the whole intersection region is OK.
        if grpx.pn == Pn::Unpredictable && grpy.pn == Pn::Unpredictable {
            return nds;
        }

        // Get group intersection region.
        let reg_int = grpx.region.intersection(&grpy.region).unwrap();

        // Check if the whole intersection region is a contradiction.
        if grpx.pn != grpy.pn {
            // Get grpx rules in the intersection region.
            let rulsx = grpx
                .rules
                .as_ref()
                .map(|rulsx| rulsx.restrict_initial_region(&reg_int));

            // Get grpy rules in the intersection region.
            let rulsy = grpy
                .rules
                .as_ref()
                .map(|rulsy| rulsy.restrict_initial_region(&reg_int));

            return NeedStore::new(vec![
                self.cont_int_region_need(&reg_int, grpx, grpy, group_num, rulsx, rulsy)
            ]);
        }

        // At this point, Pn values are both Pn::One, or both Pn::Two.

        // Get grpx rules within the intersection region.
        let rulsx = grpx
            .rules
            .as_ref()
            .unwrap()
            .restrict_initial_region(&reg_int);

        // Get grpy rules within the intersection region.
        let rulsy = grpy
            .rules
            .as_ref()
            .unwrap()
            .restrict_initial_region(&reg_int);

        // If the rules are the same, done.
        if rulsx == rulsy {
            return nds;
        }

        // If contradictory, return needs to resolve

        // Check if a valid sub-region of the intersection exists
        if let Some(rulsxy) = rulsx.intersection(&rulsy) {
            // A valid sub-union exists, seek a sample in intersection that is not in rulsxy.initial_region
            let ok_reg = rulsxy.initial_region();

            // To avoid subtraction, and maybe having a number of regions, use the far sub-region
            let far_reg = reg_int.far_reg(&ok_reg);

            // Calc rules for far region.
            let rulsx = rulsx.restrict_initial_region(&far_reg);
            let rulsy = rulsy.restrict_initial_region(&far_reg);

            //println!("pn2 intersection is {} far reg is {}", rulsxy, regy);

            nds.push(self.cont_int_region_need(
                &far_reg,
                grpx,
                grpy,
                group_num,
                Some(rulsx),
                Some(rulsy),
            ));
        } else {
            //println!("pn2 whole intersection is bad");
            nds.push(self.cont_int_region_need(
                &reg_int,
                grpx,
                grpy,
                group_num,
                Some(rulsx),
                Some(rulsy),
            ));
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
        group_num: usize,
        rulsx: Option<RuleStore>,
        rulsy: Option<RuleStore>,
    ) -> SomeNeed {
        //println!("cont_int_region_needs {} for grp {} {} and grp {} {}", regx, grpx.region, grpx.rules, grpy.region, grpy.rules);
        // Check for any squares in the region
        match self.squares.pick_a_square_in(regx) {
            Ok(sqrx) => {
                let mut needx = SomeNeed::ContradictoryIntersection {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    target_region: SomeRegion::new(vec![sqrx.state.clone()]),
                    group1: grpx.region.clone(),
                    ruls1: rulsx,
                    group2: grpy.region.clone(),
                    ruls2: rulsy,
                    priority: group_num,
                };
                needx.set_priority();
                needx
            }
            Err(PickError::NoSquares) => {
                let mut needx = SomeNeed::ContradictoryIntersection {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    target_region: regx.clone(),
                    group1: grpx.region.clone(),
                    ruls1: rulsx,
                    group2: grpy.region.clone(),
                    ruls2: rulsy,
                    priority: group_num,
                };
                needx.set_priority();
                needx
            }
            Err(PickError::PncSquare) => {
                panic!("Pnc square found in {regx} ?");
            }
        }
    } // end cont_int_region_need

    /// Get possible steps that can be used to make at least part of a
    /// given change.
    ///
    /// For each rule, prune the rule X bit positions to favor desired changes.
    ///
    /// For a two-result group, see if there is an existing square that is expected to
    /// produce the desired change.
    pub fn get_steps(&self, achange: &SomeChange, within: Option<&SomeRegion>) -> StepStore {
        debug_assert!(achange.b01.bitwise_and(&achange.b10).is_low()); // No X->x change wanted.

        let mut stps = StepStore::new(vec![]);

        for grpx in self.groups.iter() {
            if grpx.pn == Pn::Unpredictable {
                continue;
            }

            if let Some(limit_reg) = within {
                if !grpx.intersects(limit_reg) {
                    continue;
                }
            }

            // Check if group rules cause at least one change that is needed.
            let mut skip = true;
            for rulx in grpx.rules.as_ref().expect("SNH").iter() {
                if achange.intersection(rulx).is_not_low() {
                    skip = false;
                    break;
                }
            }
            if skip {
                continue;
            }

            if grpx.pn == Pn::One {
                // Find bit changes that are desired
                if let Some(rulx) =
                    grpx.rules.as_ref().expect("SNH")[0].restrict_for_changes(achange, within)
                {
                    stps.push(SomeStep::new(
                        self.id,
                        rulx,
                        AltRuleHint::NoAlt {},
                        grpx.region.clone(),
                    ));
                }
                continue;
            }

            if grpx.pn == Pn::Two {
                // Get restricted region for needed changes.
                let mut parsed_region: Option<SomeRegion> = None;
                for ruly in grpx.rules.as_ref().expect("SNH").iter() {
                    let Some(rulx) = ruly.restrict_for_changes(achange, within) else {
                        continue;
                    };
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

                if let Some(rules) = &grpx.rules {
                    for (inx, ruly) in rules.iter().enumerate() {
                        let Some(rulx) = ruly.restrict_for_changes(achange, within) else {
                            continue;
                        };

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
                                    self.id,
                                    rulx.restrict_initial_region(&SomeRegion::new(vec![sqrx
                                        .state
                                        .clone()])),
                                    AltRuleHint::NoAlt {},
                                    grpx.region.clone(),
                                );
                                stps.push(stpx);
                                found = true;
                            } // end if
                        } // next sqrx

                        if !found {
                            if one_no_change {
                                stps.push(SomeStep::new(
                                    self.id,
                                    rulx,
                                    AltRuleHint::AltNoChange {},
                                    grpx.region.clone(),
                                ));
                            } else {
                                let alt_rule = if inx == 0 {
                                    rules[1].clone()
                                } else {
                                    rules[0].clone()
                                };
                                stps.push(SomeStep::new(
                                    self.id,
                                    rulx,
                                    AltRuleHint::AltRule { rule: alt_rule },
                                    grpx.region.clone(),
                                ));
                            }
                        }
                    } // next ruly
                }
            } // end Pn::Two
        } // next grpx

        // println!("Steps: {}", stps);
        stps
    } // end get_steps

    /// Find groups that can be formed by a given square, and other similar squares, that is not currently in a group.
    fn possible_groups_from_square(&self, sqrx: &SomeSquare) -> Vec<SomeGroup> {
        //println!("possible_groups_from_square {}", sqrx.state);

        let mut ret_grps = Vec::<SomeGroup>::new();

        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return ret_grps;
        }

        // Init a list containing the maximum possible region.
        let max_poss_reg = SomeRegion::new(vec![sqrx.state.new_high(), sqrx.state.new_low()]);

        // Init list for holding possible regions.
        let mut poss_regs = RegionStore::new(vec![max_poss_reg.clone()]);

        for sqry in self.squares.ahash.values() {
            if sqry.state == sqrx.state {
                continue;
            }

            // Previous subtractions may put some squares out of reach.
            if poss_regs.any_superset_of_state(&sqry.state) && sqrx.compatible(sqry) == Some(false)
            {
                poss_regs = poss_regs.subtract_state_to_supersets_of(&sqry.state, &sqrx.state);
            }
        }

        // Check memory for additional dissimilar squares.
        for sqrm in self.memory.iter() {
            if poss_regs.any_superset_of_state(&sqrm.state) && sqrx.compatible(sqrm) == Some(false)
            {
                poss_regs = poss_regs.subtract_state_to_supersets_of(&sqrm.state, &sqrx.state);
            }
        }

        // Collect similar squares that are within the possible regions.
        let mut sim_sqrs = Vec::<&SomeSquare>::new();

        for sqry in self.squares.ahash.values() {
            if sqry.state == sqrx.state {
                continue;
            }

            if poss_regs.any_superset_of_state(&sqry.state) && sqrx.compatible(sqry) == Some(true) {
                sim_sqrs.push(sqry);
            }
        }

        // Check for similar squares from memory, that are within the possible regions.
        for sqrm in self.memory.iter() {
            if poss_regs.any_superset_of_state(&sqrm.state) && sqrx.compatible(sqrm) == Some(true) {
                sim_sqrs.push(sqrm);
            }
        }

        // println!("Similar squares:");
        // for sqrz in sim_sqrs2.iter() {
        //    println!("  {}", sqrz);
        //}

        // Calc excluded regions formed by pairs of similar squares
        // that cannot be combined.
        // If the target square has a 1->0 bit,
        // it may combine with a square having a corresponding 0->1, forming X->x,
        // and one with 0->0, forming X->0,
        // but 0->0 and 0->1 cannot combine.
        if sim_sqrs.len() > 1 {
            let mut excluded_regs = RegionStore::new(vec![]);

            for iny in 0..(sim_sqrs.len() - 1) {
                for inz in (iny + 1)..sim_sqrs.len() {
                    let rslt = if sim_sqrs[iny].pn > sim_sqrs[inz].pn {
                        sim_sqrs[iny].rules_compatible(sim_sqrs[inz])
                    } else {
                        sim_sqrs[inz].rules_compatible(sim_sqrs[iny])
                    };

                    if not(rslt) {
                        excluded_regs.push_nosups(SomeRegion::new(vec![
                            sim_sqrs[iny].state.clone(),
                            sim_sqrs[inz].state.clone(),
                        ]));
                    }
                }
            }
            //println!("excluded regions {excluded_regs}");

            // Subtract excluded region state pairs.
            // So regions can contain either state, but not both.
            //
            // There is a tendency to produce contradictory intersections.
            // e.g. Given 4->4, 1->0 and D->D, that results in 0X0X and X10X, intersecting at 010X.
            // Within 010X, 5 is contradictory.
            //
            // Maximum region minus state = complement of state.
            for ex_regx in excluded_regs.iter() {
                if poss_regs.any_superset_of(ex_regx) {
                    let not_state1 = RegionStore::new(
                        max_poss_reg.subtract_state_to_supersets_of(ex_regx.state1(), &sqrx.state),
                    );
                    let not_state2 = RegionStore::new(
                        max_poss_reg.subtract_state_to_supersets_of(ex_regx.state2(), &sqrx.state),
                    );
                    let not_states = not_state1.union(&not_state2);
                    poss_regs = poss_regs.intersection(&not_states);
                    // println!("sqrx {} bad reg {}", sqrx.state, ex_regx);
                }
            }
        }

        // Validate possible regions.
        ret_grps = poss_regs
            .avec
            .par_iter() // par_iter for parallel processing, iter for sequential diagnostic messages.
            .filter_map(|regx| self.validate_possible_group(sqrx, regx, &sim_sqrs))
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
        // println!("validate_possible_group: state {} num sim {} reg {}", sqrx.state, sim_sqrs.len(), regx);
        debug_assert!(regx.is_superset_of(sqrx));

        // Find squares in the given region, and calc region built from simmilar squares.
        let mut regy = SomeRegion::new(vec![sqrx.state.clone()]); // sqrx.state probably will not still be the first state after unions.
        let mut sqrs_in = Vec::<&SomeSquare>::new();
        for sqry in sim_sqrs.iter() {
            if regx.is_superset_of(*sqry) {
                sqrs_in.push(sqry);
                if !regy.is_superset_of(*sqry) {
                    regy = regy.union(*sqry);
                }
            }
        }
        if self.groups.any_superset_of(&regy) {
            return None;
        }

        let mut stas_in = Vec::<SomeState>::with_capacity(sqrs_in.len() + 1);

        stas_in.push(sqrx.state.clone());

        for sqry in sqrs_in.iter() {
            stas_in.push(sqry.state.clone());
        }

        // Create region, cleanup squares between, etc.
        let grp_reg = SomeRegion::new(stas_in);

        // If sqrx is not Pn::Unpredictable, aggregate the rules.
        let mut rules: Option<RuleStore> = None;
        if let Some(rulesx) = &sqrx.rules {
            let mut rulesz = rulesx.clone();

            for stay in grp_reg.states.iter().skip(1) {
                // Far state may not be found, for a GT two state region.
                if let Some(sqry) = sqrs_in.iter().find(|&sqry| &sqry.state == stay) {
                    rulesz = rulesz.union(sqry.rules.as_ref()?)?;
                }
            }
            rules = Some(rulesz);
        }

        // Get far_state_pnc.
        let mut far_pnc = false;
        if grp_reg.len() == 1 {
            far_pnc = sqrx.pnc;
        } else if grp_reg.len() == 2 {
            let far_state = grp_reg.state2();
            if let Some(sqry) = sqrs_in.iter().find(|&sqry| &sqry.state == far_state) {
                far_pnc = sqry.pnc;
            }
        }

        if self.groups.any_superset_of(&grp_reg) {
            return None;
        }

        // Return a group, keeping sqrx.state as first state in the group region.
        Some(SomeGroup::new(grp_reg, rules, sqrx.pnc && far_pnc))
    } // end validate_combination

    /// Take an action for a need.
    pub fn take_action_need(&mut self, cur_state: &SomeState, ndx: &SomeNeed) -> SomeSample {
        let asample = self.take_action_arbitrary(cur_state);

        // Additional processing for selected kinds of need
        if let SomeNeed::ConfirmGroup { grp_reg, .. } = ndx {
            // The sample could have invalidated the group.
            if let Some(grpx) = self.groups.find_mut(grp_reg) {
                if !grpx.pnc {
                    let sqr1 = self.squares.find(grp_reg.state1()).expect("SNH");
                    if grp_reg.len() == 1 {
                        if sqr1.pnc {
                            grpx.set_pnc(self.dom_id, self.id);
                        }
                    } else if let Some(sqr2) = self.squares.find(grp_reg.state2()) {
                        if sqr1.pnc && sqr2.pnc {
                            grpx.set_pnc(self.dom_id, self.id);
                        }
                    }
                }
            }
        }

        asample
    }

    /// Take an action with the current state, add the sample teo squarestore.
    /// Return a sample.
    pub fn take_action_arbitrary(&mut self, cur_state: &SomeState) -> SomeSample {
        let astate = self
            .do_something
            .take_action(cur_state, self.dom_id, self.id);

        let asample = SomeSample::new(cur_state.clone(), astate);

        self.eval_sample_arbitrary(&asample);
        asample
    }

    /// Take an action with the current state, for a step.
    /// Assume the result is as expected.
    /// Return a sample.
    pub fn take_action_step(&mut self, cur_state: &SomeState) -> SomeSample {
        let astate = self
            .do_something
            .take_action(cur_state, self.dom_id, self.id);

        let asample = SomeSample::new(cur_state.clone(), astate);

        self.update_square(&asample);

        asample
    }

    /// Update a square with a new sample, if it exists.
    pub fn update_square(&mut self, asample: &SomeSample) {
        // If a square exists, update it.
        if let Some(sqrx) = self.squares.find_mut(&asample.initial) {
            sqrx.add_sample(asample);
        } else {
            for sqrx in self.memory.iter_mut() {
                if sqrx.state == asample.initial {
                    sqrx.add_sample(asample);
                    break;
                }
            }
        }
    }

    /// Check an unexpected step result.
    /// If there is an existitg square, it has already been updated.
    pub fn eval_unexpected_result(&mut self, asample: &SomeSample) {
        // Check active squares.
        if self.squares.find_mut(&asample.initial).is_some() {
            self.eval_changed_square(&asample.initial);
            return;
        }
        // Check memory.
        if let Some(inx) = self.memory_index(&asample.initial) {
            if let Some(sqrx) = self.memory.remove(inx) {
                self.add_new_square(sqrx);
                self.eval_changed_square(&asample.initial);
            }
            return;
        }
        // Add square.
        self.add_new_square(SomeSquare::new(asample));
        self.eval_changed_square(&asample.initial);
    }

    /// Return a change with all changes that can be made for the action.
    pub fn aggregate_changes(&self) -> Option<&SomeChange> {
        if let Some(changes) = &self.groups.aggregate_changes {
            Some(changes)
        } else {
            None
        }
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
        println!("Act: {} group anchor rates", self.id);
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
            let rate = self.group_anchor_rate(&grpx.region, anchor);
            println!("anchor {sqrx} rate {:?}", rate);
            let stas_adj = self.squares.stas_adj_reg(&grpx.region);
            for stax in stas_adj.iter() {
                if stax.is_adjacent(anchor) {
                    let sqrx = self.squares.find(stax).expect(
                        "Call to stas_adj_reg should return states that refer to existing squares",
                    );
                    let grps = self.groups.groups_in(stax);
                    if grps.len() == 1 {
                        if let Some(grpy) = self.groups.find(grps[0]) {
                            if let Some(anchory) = &grpy.anchor {
                                if stax == anchory {
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
                        println!("adj    {sqrx} in groups {}", tools::vec_ref_string(&grps));
                    }
                }
            } // next stax
        } else {
            println!("Group {} does not have an anchor defined", grpx.region);
        }
        Ok(())
    }

    /// Check group limited setting.
    pub fn check_limited(&mut self, max_reg: &SomeRegion) {
        self.groups.check_limited(max_reg);
    }

    /// Return the total number of groups in the action.
    pub fn number_groups(&self) -> usize {
        self.groups.len()
    }

    /// Return the number of groups expected in the action.
    pub fn number_groups_expected(&self) -> usize {
        self.do_something.len()
    }

    /// Return a String representation of SomeAction.
    fn formatted_string(&self) -> String {
        let mut rc_str = String::from("A(ID: ");

        rc_str += &self.id.to_string();

        rc_str += ", number squares: ";
        rc_str += &self.squares.len().to_string();

        if self.remainder_check_regions.is_not_empty() {
            rc_str.push_str(&format!(", remainder: {}", self.remainder_check_regions));
        }

        let mut fil = ",\n       Grps: ";
        for grpx in self.groups.iter() {
            let stas_in = self.squares.stas_in_reg(&grpx.region);

            let cnt: usize = stas_in
                .iter()
                .map(|stax| usize::from(self.groups.num_groups_in(stax) == 1))
                .sum();

            rc_str.push_str(&format!(
                "{}{} num Sqrs: {} in1: {})",
                fil,
                grpx,
                stas_in.len(),
                cnt,
            ));

            fil = ",\n             ";
        }

        rc_str.push(')');
        rc_str
    }

    /// Save a new sample to memory, oldest first.
    pub fn add_sample_to_memory(&mut self, asample: SomeSample) {
        if self.squares.find(&asample.initial).is_some() {
            panic!(
                "add_sample_to_memory: square for {} exists in hash",
                asample.initial
            );
        }
        // Add to an existing square, if possible.
        for sqrx in self.memory.iter_mut() {
            if sqrx.state == asample.initial {
                sqrx.add_sample(&asample);
                return;
            }
        }

        self.add_square_to_memory(SomeSquare::new(&asample));
    }

    /// Add a square to memory, oldest first.
    fn add_square_to_memory(&mut self, sqrx: SomeSquare) {
        if self.squares.find(&sqrx.state).is_some() {
            panic!(
                "add_square_to_memory: square for {} exists in hash",
                sqrx.state
            );
        }
        if self.memory.len() >= MAX_MEMORY {
            self.memory.pop_front();
        }
        self.memory.push_back(sqrx);
    }

    /// Return a vector of rules for the "rx" command.
    pub fn all_rules(&self) -> Vec<(usize, &SomeRule)> {
        let mut ret = Vec::<(usize, &SomeRule)>::new();
        for rulx in self.groups.all_rules() {
            ret.push((self.id, rulx));
        }
        ret
    }
} // end impl SomeAction

// Some action tests are made from the domain level.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;

    impl SomeAction {
        /// Set a group anchor, given a group region and anchor state.
        /// Used in domain tests.
        pub fn set_group_anchor(&mut self, grp_reg: &SomeRegion, anchor: &SomeState) {
            let Some(grpx) = self.groups.find_mut(grp_reg) else {
                panic!("Group not found?");
            };
            grpx.set_anchor(anchor);
        }
    }

    #[test]
    fn shared_symmetric_region() -> Result<(), String> {
        // Init action
        let tmp_bts = SomeBits::new(8);
        let mut act0 = SomeAction::new(0, 0, vec![]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.new_low(), tmp_sta.new_high()]);

        let s3 = tmp_sta.new_from_string("s0b0011")?;
        let s4 = tmp_sta.new_from_string("s0b0100")?;
        let s5 = tmp_sta.new_from_string("s0b0101")?;
        let s7 = tmp_sta.new_from_string("s0b0111")?;
        let s8 = tmp_sta.new_from_string("s0b1000")?;
        let s9 = tmp_sta.new_from_string("s0b1001")?;
        let sa = tmp_sta.new_from_string("s0b1010")?;
        let sb = tmp_sta.new_from_string("s0b1011")?;
        let sc = tmp_sta.new_from_string("s0b1100")?;
        let se = tmp_sta.new_from_string("s0b1110")?;
        let sf = tmp_sta.new_from_string("s0b1111")?;

        // Set up XX01 group.
        act0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), s5.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s9.clone(), s9.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), s5.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s9.clone(), s9.clone()));

        // Set up divider squares.
        act0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), s8.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sb.clone(), s4.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sc.clone(), s3.clone()));

        // Set up 111X group.
        // The shared symetric group will be 11X1.
        act0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), sf.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(se.clone(), se.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), sf.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(se.clone(), se.clone()));

        let nds = act0.get_needs(&sa, 0, &tmp_reg);

        println!("Groups {}", act0.groups);
        println!("needs {nds}");

        if let Some(grpx) = act0
            .groups
            .find(&tmp_reg.new_from_string("r111x").expect("SNH"))
        {
            if let Some(ancx) = &grpx.anchor {
                if *ancx == se {
                } else {
                    return Err(format!("group 111X anchor not 1110"));
                }
            } else {
                return Err(format!("group 111X expected anchor not found"));
            }
        } else {
            return Err(format!("group 111X not found"));
        }

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn delete_shared_symmetric_region() -> Result<(), String> {
        // Init action
        let tmp_bts = SomeBits::new(8);
        let mut act0 = SomeAction::new(0, 0, vec![]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.new_low(), tmp_sta.new_high()]);

        let s4 = tmp_sta.new_from_string("s0b0100")?;
        let s6 = tmp_sta.new_from_string("s0b0110")?;
        let s7 = tmp_sta.new_from_string("s0b0111")?;
        let s8 = tmp_sta.new_from_string("s0b1000")?;
        let s9 = tmp_sta.new_from_string("s0b1001")?;
        let sa = tmp_sta.new_from_string("s0b1010")?;
        let sd = tmp_sta.new_from_string("s0b1101")?;
        let sf = tmp_sta.new_from_string("s0b1111")?;

        // Set up X10X group.
        act0.eval_sample_arbitrary(&SomeSample::new(s4.clone(), s4.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sd.clone(), sd.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s4.clone(), s4.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sd.clone(), sd.clone()));

        // Set up divider squares.
        act0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), s8.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s8.clone(), s7.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s9.clone(), s6.clone()));

        // Set up 1X1X group.
        // The shared symetric group will be 11X1.
        act0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), sf.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sa.clone(), sa.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), sf.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sa.clone(), sa.clone()));

        println!("Groups {}", act0.groups);

        if let Some(_) = act0
            .groups
            .find(&tmp_reg.new_from_string("r11x1").expect("SNH"))
        {
            act0.cleanup(&NeedStore::new(vec![]), 0);
            if let Some(_) = act0
                .groups
                .find(&tmp_reg.new_from_string("r11x1").expect("SNH"))
            {
                return Err(format!("group 11X1 not deleted"));
            }
        } else {
            return Err(format!("group 11X1 not found"));
        }
        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn two_result_group() -> Result<(), String> {
        // Init action
        let tmp_bts = SomeBits::new(8);
        let mut act0 = SomeAction::new(0, 0, vec![]);
        let tmp_sta = SomeState::new(tmp_bts.clone());

        // Put in two incompatible one-result squares, but both subset of the
        // later two-result squares.
        // 0->1 and 0->1, in the fourth bit.
        //let s1 = SomeState::new(SomeBits::new(vec![0x1]));
        let s1 = tmp_sta.new_from_string("s0b0001")?;
        let s9 = tmp_sta.new_from_string("s0b1001")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s1.clone(), s9.clone()));
        let s5 = tmp_sta.new_from_string("s0b0101")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), s5.clone()));

        // Set up first two_result square.
        let s0 = tmp_sta.new_from_string("s0b0000")?;
        let s8 = tmp_sta.new_from_string("s0b1000")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), s0.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), s8.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), s0.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), s8.clone()));

        // Set up second two_result square.
        let s7 = tmp_sta.new_from_string("s0b0111")?;
        let sf = tmp_sta.new_from_string("s0b1111")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), sf.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), s7.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), sf.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), s7.clone()));

        println!("Groups {}", act0.groups);
        assert!(act0.groups.len() == 1);

        Ok(())
    }

    // Test making groups from a few single-sample states.
    #[test]
    fn groups_formed_1() -> Result<(), String> {
        // Init action
        let tmp_bts = SomeBits::new(8);
        let mut act0 = SomeAction::new(0, 0, vec![]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.new_low(), tmp_sta.new_high()]);

        // Eval sample that other samples will be incompatible with.
        let s7 = tmp_sta.new_from_string("s0b0111")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), s7.clone()));

        // Process three similar samples.
        act0.eval_sample_arbitrary(&SomeSample::new(
            tmp_sta.new_from_string("s0b1011")?,
            tmp_sta.new_from_string("s0b1010")?,
        ));

        act0.eval_sample_arbitrary(&SomeSample::new(
            tmp_sta.new_from_string("s0b1101")?,
            tmp_sta.new_from_string("s0b1100")?,
        ));

        act0.eval_sample_arbitrary(&SomeSample::new(
            tmp_sta.new_from_string("s0b0001")?,
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
        let tmp_bts = SomeBits::new(8);
        let mut act0 = SomeAction::new(0, 0, vec![]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.new_low(), tmp_sta.new_high()]);
        let max_reg = tmp_reg.new_from_string("rXXXX")?;

        // Set up 2-result square sf.
        let sf = tmp_sta.new_from_string("s0b1111")?;
        let se = tmp_sta.new_from_string("s0b1110")?;

        act0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), sf.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), se.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), sf.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), se.clone()));

        // Set up 2-result square s1.
        let s1 = tmp_sta.new_from_string("s0b0001")?;
        let s0 = tmp_sta.new_from_string("s0b0000")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s1.clone(), s1.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s1.clone(), s0.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s1.clone(), s1.clone()));
        act0.eval_sample_arbitrary(&SomeSample::new(s1.clone(), s0.clone()));

        let nds = act0.get_needs(&s1, 0, &max_reg);
        println!("Act: {}", act0);
        println!("needs: {}", nds);

        assert!(nds.kind_is_in("LimitGroupAdj"));
        assert!(act0.groups.len() == 1);

        Ok(())
    }

    // Test making a region from three similar-change samples.
    #[test]
    fn three_sample_region1() -> Result<(), String> {
        // Init action.
        let tmp_bts = SomeBits::new(8);
        let mut act0 = SomeAction::new(0, 0, vec![]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.new_low(), tmp_sta.new_high()]);

        // Set up square 0.
        let s0 = tmp_sta.new_from_string("s0b0000")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), s0.clone()));

        // Set up square 3.
        let s3 = tmp_sta.new_from_string("s0b0011")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s3.clone(), s3.clone()));

        // Set up square 5.
        let s5 = tmp_sta.new_from_string("s0b0101")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), s5.clone()));

        println!("Act: {}", act0);

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
        let tmp_bts = SomeBits::new(8);
        let mut act0 = SomeAction::new(0, 0, vec![]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.new_low(), tmp_sta.new_high()]);

        // Set up square 0.
        let s0 = tmp_sta.new_from_string("s0b0000")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), s0.clone()));

        // Set up square 3.
        let s3 = tmp_sta.new_from_string("s0b0011")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s3.clone(), s3.clone()));

        // Set up square 4, dissimilar to s5 by third bit being 1->0.
        let s4 = tmp_sta.new_from_string("s0b0100")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s4.clone(), s0.clone()));

        // Set up square 5.
        let s5 = tmp_sta.new_from_string("s0b0101")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), s5.clone()));

        println!("Act: {}", act0);

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
        let tmp_bts = SomeBits::new(8);
        let mut act0 = SomeAction::new(0, 0, vec![]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.new_low(), tmp_sta.new_high()]);

        // Set up square 2.
        let s2 = tmp_sta.new_from_string("s0b0010")?;
        let s0 = tmp_sta.new_from_string("s0b0000")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s2.clone(), s0.clone()));

        // Set up square b.
        let sb = tmp_sta.new_from_string("s0b1011")?;
        act0.eval_sample_arbitrary(&SomeSample::new(sb.clone(), sb.clone()));

        // Set up square 5.
        let s5 = tmp_sta.new_from_string("s0b0101")?;
        act0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), s5.clone()));

        println!("Act: {}", act0);

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
