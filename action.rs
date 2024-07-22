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
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::square::{Compatibility, SomeSquare};
use crate::squarestore::{PickError, SquareStore};
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::step::{AltRuleHint, SomeStep};
use crate::stepstore::StepStore;
use crate::target::ATarget;
use crate::tools;

use rand::Rng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

/// Number of new squares added before a cleanup check is run.
const CLEANUP: usize = 5;

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
/// The SomeAction struct, aggregate the best current guess at what an action
/// will do for any parent domain state.
pub struct SomeAction {
    /// Action number, index/key into parent ActionStore vector.
    pub id: usize,
    /// Parent Domain number.
    pub dom_id: usize,
    /// Number bits expected, for argument checking.
    num_bits: usize,
    /// Store for groups of compatible-change squares.
    pub groups: GroupStore,
    /// A store of squares sampled for an action.
    pub squares: SquareStore,
    /// Connection to a process that takes the current state, and returns a, possibly changed, state.
    do_something: ActionInterface,
    /// Trigger cleanup logic after a number of new squares.
    cleanup_trigger: usize,
    /// When the actions groups change check for any missed regions.
    check_remainder: bool,
    /// Regions currently not covered by groups, when tere are no more needs to sample squares.
    remainder_check_regions: RegionStore,
}

impl SomeAction {
    /// Return a new SomeAction struct.
    pub fn new(act_id: usize, dom_id: usize, cur_state: &SomeState, rules: Vec<RuleStore>) -> Self {
        SomeAction {
            id: act_id,
            dom_id,
            num_bits: cur_state.num_bits(),
            groups: GroupStore::new(vec![]),
            squares: SquareStore::new(HashMap::new(), cur_state.num_bits()),
            do_something: ActionInterface::new(rules),
            cleanup_trigger: 0,
            check_remainder: false,
            remainder_check_regions: RegionStore::new(vec![]),
        }
    }

    /// Add a new square from a sample.
    pub fn add_new_sample(&mut self, smpl: &SomeSample) -> &SomeSquare {
        //println!("Dom {} Act {} add_new_sample: {}", self.dom_id, self.id, smpl);
        debug_assert_eq!(smpl.num_bits(), self.num_bits);

        self.add_new_square(SomeSquare::new(smpl))
    }

    /// Add a new square.
    fn add_new_square(&mut self, sqrx: SomeSquare) -> &SomeSquare {
        debug_assert_eq!(sqrx.num_bits(), self.num_bits);

        self.cleanup_trigger += 1;

        let key = sqrx.state.clone();
        self.squares_insert(sqrx);
        self.squares.find(&key).expect("SNH")
    }

    /// Add a square, print info.
    pub fn squares_insert(&mut self, sqrx: SomeSquare) {
        println!(
            "\nDom {} Adding square {} -{}-> {}",
            self.dom_id,
            sqrx.state,
            self.id,
            sqrx.first_result()
        );
        self.squares.insert(sqrx);
    }

    /// Evaluate a changed square.
    /// The square may be new, that is its first sample.
    /// The square may have become pnc == true, or changed pnc from true to false.
    /// Return true if something changed.
    fn eval_changed_square(&mut self, key: &SomeState) {
        //println!("Dom {} Act {} eval_changed_square: {key}", self.dom_id, self.id);
        debug_assert_eq!(key.num_bits(), self.num_bits);

        let sqrx = self.squares.find_mut(key).expect("SNH");

        // Check if it invalidates any groups.
        let regs_invalid: RegionStore = self.groups.check_square(sqrx, self.dom_id, self.id);

        let pnc = sqrx.pnc;
        let pn = sqrx.pn;

        if regs_invalid.is_not_empty() {
            self.check_remainder = true;
            self.process_invalid_regions(&regs_invalid);
        }

        if pn == Pn::One || pnc {
        } else {
            return;
        }

        if !self.groups.any_superset_of(key) {
            self.create_groups_from_squares(&[key.clone()]);
            //if !self.groups.any_superset_of(key) {
            //    panic!("No groups superset of {}", key);
            //}
            return;
        }

        // Check if a group can be confirmed.
        for grpx in self.groups.iter_mut() {
            if grpx.pnc || !grpx.region.is_superset_of(key) {
                continue;
            }

            if grpx.region.len() == 1 {
                grpx.set_pnc(self.dom_id, self.id);
                continue;
            }

            // If the square is first, or last, in the region,
            // check if the group can be set to pnc.
            if key == grpx.region.first_state() {
                if let Some(sqr2) = self.squares.find(&grpx.region.far_state()) {
                    if sqr2.pnc {
                        grpx.set_pnc(self.dom_id, self.id);
                    }
                }
                continue;
            }

            if *key == grpx.region.far_state() {
                let sqr1 = self.squares.find(grpx.region.first_state()).expect("SHN");
                if sqr1.pnc {
                    grpx.set_pnc(self.dom_id, self.id);
                }
            }
        } // next grpx
    }

    /// Do basic functions for any new sample.
    /// Return true if a matching square exists.
    pub fn eval_sample(&mut self, smpl: &SomeSample) -> bool {
        //println!("Dom {} Act {} eval_sample: {}", self.dom_id, self.id, smpl);
        debug_assert_eq!(smpl.num_bits(), self.num_bits);

        // If a square already exists, update it.
        // If the square changes, evaluate it.
        if let Some(sqrx) = self.squares.find_mut(&smpl.initial) {
            if sqrx.add_sample(smpl) {
                self.eval_changed_square(&smpl.initial);
            }
            return true;
        }

        // If a square in memory already exists, update it.
        // If the square changes, remember and evaluate it.
        if let Some(sqrx) = self.squares.memory_find_mut(&smpl.initial) {
            if sqrx.add_sample(smpl) {
                self.squares.remember(&smpl.initial);
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

        // If state is not in a group, create a group from the square.
        if !self.groups.any_superset_of(&smpl.initial) {
            // Add new square.
            self.add_new_sample(smpl);
            // Create possible groups from square.
            self.create_groups_from_squares(&[smpl.initial.clone()]);

            return true;
        }

        self.squares.update_memory(smpl);

        false
    }

    /// Evaluate an arbitrary sample, creating a square if needed.
    pub fn eval_sample_arbitrary(&mut self, smpl: &SomeSample) {
        //println!("Dom {} Act {} eval_sample_arbitrary {}", self.dom_id, self.id, smpl);
        debug_assert_eq!(smpl.num_bits(), self.num_bits);

        if !self.eval_sample(smpl) {
            self.add_new_sample(smpl);
        }
    }

    /// Check invalid regions for orphaned squares, create new regions.
    fn process_invalid_regions(&mut self, invalid_regs: &RegionStore) {
        debug_assert!(invalid_regs.is_empty() || invalid_regs.num_bits().unwrap() == self.num_bits);

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
        //println!("Dom {} Act {} create_groups_from_squares: {}", self.dom_id, self.id, tools::vec_string(keys));

        assert!(!keys.is_empty());
        debug_assert_eq!(keys[0].num_bits(), self.num_bits);

        // Set flag to later check for regions not covered by groups.
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

        // Store possible groups, some may be duplicates.
        for grpx in groups {
            if !self.groups.any_superset_of(&grpx.region) {
                self.groups.push_nosubs(grpx, self.dom_id, self.id);
            }
        }
    }

    /// Check groups due to a new, or updated, square.
    fn create_groups_from_squares2(&self, key: &SomeState) -> Vec<SomeGroup> {
        //println!("Dom {} Act {} create_groups_from_squares2: {key}", self.dom_id, self.id);
        debug_assert_eq!(key.num_bits(), self.num_bits);
        debug_assert!(!self.groups.any_superset_of(key));

        // Lookup square.
        let sqrx = self
            .squares
            .find(key)
            .expect("key should refer to an existing square");

        //println!("create_groups_from_squares2: square {}", sqrx);

        // Check if square can be used to create groups.
        // Allowing a square to make a group with a single sample is needed
        // for group bootstrapping.
        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return vec![];
        }

        //println!("Checking Square {} for new groups", sqrx.state);

        // Get possible regions, sqrx.state will be <region>.first_state
        // Duplicate group regions are possible.
        self.possible_groups_from_square(sqrx)
    } // end create_groups_from_squares2

    /// Return needs for states that are not in a group.
    /// The Domain current state for which there are no samples.
    /// A pn > 1 state that needs more samples.
    pub fn state_not_in_group_needs(&self, cur_state: &SomeState) -> NeedStore {
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);

        let mut nds = NeedStore::new(vec![]);

        // Check if current state is in any groups
        if !self.groups.any_superset_of(cur_state) {
            if let Some(sqrx) = self.squares.find(cur_state) {
                if sqrx.pn == Pn::One || sqrx.pnc {
                    panic!(
                        "Problem: Dom {} Act {} square {} not in group?",
                        self.dom_id, self.id, sqrx
                    );
                    //return nds;
                } else {
                    let mut needx = SomeNeed::StateNotInGroup {
                        dom_id: self.dom_id,
                        act_id: self.id,
                        target_state: cur_state.clone(),
                        priority: 0,
                    };
                    needx.add_priority_base();
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
                needx.add_priority_base();
                nds.push(needx);
                return nds;
            }
        }

        // Look for a pn > 1, pnc == false, not in group squares.
        // Extra samples are needed to gain pnc, then the first group.
        let sqrs_pngt1 = self.squares.pn_gt1_no_pnc();

        for stax in sqrs_pngt1.iter() {
            if stax == cur_state || self.groups.any_superset_of(stax) {
                continue;
            }

            let mut needx = SomeNeed::StateNotInGroup {
                dom_id: self.dom_id,
                act_id: self.id,
                target_state: stax.clone(),
                priority: cur_state.distance(stax),
            };
            needx.add_priority_base();
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
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

        let mut ret = NeedStore::new(vec![]);

        let mut get_new_needs = true;
        while get_new_needs {
            // Get some needs.
            ret = self.get_needs2(cur_state, dom_id, max_reg);

            get_new_needs = false;

            // Check for first need satisfied by a square in memory.
            for ndx in ret.iter() {
                match ndx.target() {
                    ATarget::State { state } => {
                        if self.squares.memory_contains(state) {
                            self.squares.remember(state);
                            self.eval_changed_square(state);
                            get_new_needs = true;
                            break;
                        }
                    }
                    ATarget::Region { region } => {
                        if let Some(key) = self.squares.memory_key_in_reg(region) {
                            self.squares.remember(&key);
                            self.eval_changed_square(&key);
                            get_new_needs = true;
                            break;
                        }
                    }
                    _ => panic!("SNH"),
                };
            } // next ndx
        } // end while
        ret
    }

    /// Get needs, process any housekeeping needs.
    pub fn get_needs2(
        &mut self,
        cur_state: &SomeState,
        dom_id: usize,
        max_reg: &SomeRegion,
    ) -> NeedStore {
        //println!("Running Action {}::get_needs2 {}", self.num, cur_state);
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

        let mut nds = NeedStore::new(vec![]);

        // loop until no housekeeping need is returned.
        let mut try_again = true;
        while try_again {
            try_again = false;

            nds = NeedStore::new(vec![]);

            // Check for additional samples for group states needs
            nds.append(self.confirm_group_needs());

            // Check any two groups for overlapping groups that form a contradictory intersection.
            nds.append(self.group_pair_needs());

            // Check for group limiting needs
            if let Some(ndx) = self.limit_groups_needs(max_reg) {
                nds.append(ndx);
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

            // Process housekeeping needs.
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

                    self.groups.push_nosubs(
                        SomeGroup::new(group_region.clone(), rules.clone()),
                        dom_id,
                        self.id,
                    );
                    try_again = true;
                }
            } // next ndx
        } // end loop

        // Do cleanup, if needed.
        if self.cleanup_trigger >= CLEANUP {
            self.cleanup(&nds, dom_id);
            self.cleanup_trigger = 0;
        }

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
                needx.add_priority_base();
                nds.push(needx);
            }
        }

        nds
    } // end get_needs

    /// Check for needs in a region not covered by current groups.
    fn remainder_check_region(&self, max_reg: &SomeRegion) -> RegionStore {
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

        let mut remainder_regs = RegionStore::new(vec![max_reg.clone()]);

        for grpx in self.groups.iter() {
            remainder_regs = remainder_regs.subtract_item(&grpx.region);
        }

        remainder_regs
    }

    /// Cleanup unneeded squares.
    fn cleanup(&mut self, needs: &NeedStore, dom_id: usize) {
        // Identify bridge regions.
        let mut shared_regions = RegionStore::new(vec![]);
        for inx in 0..(self.groups.len() - 1) {
            let grpx = &self.groups[inx];
            if grpx.anchor.is_none() {
                continue;
            }
            for iny in (inx + 1)..self.groups.len() {
                let grpy = &self.groups[iny];

                if grpy.pn != grpx.pn {
                    continue;
                }
                if grpy.anchor.is_none() {
                    continue;
                }
                if grpy.region.is_adjacent(&grpx.region) {
                    if let Some(shared) = grpy.region.bridge(&grpx.region) {
                        if shared.is_superset_of(&grpy.region)
                            || shared.is_superset_of(&grpx.region)
                        {
                        } else {
                            //println!("grp1 {} adj grp2 {} bridge {}", grpy.region, grpx.region, shared);
                            shared_regions.push(shared);
                        }
                    }
                }
            } // next iny
        } // next inx
          // Delete shared symmetric groups.
        for regx in shared_regions.iter() {
            self.groups.remove_subsets_of(regx, dom_id, self.id);
        }

        // Store for keys of squares to delete.
        let mut to_del = StateStore::new(vec![]);

        'next_sqr: for keyx in self.squares.ahash.keys() {
            // Check needs
            for ndx in needs.iter() {
                if match ndx.target() {
                    ATarget::State { state } => state == keyx,
                    ATarget::Region { region } => region.is_superset_of(keyx),
                    _ => panic!("SNH"),
                } {
                    continue 'next_sqr;
                }
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
                    if grpx.region.states.len() > 2 && grpx.region.far_state() == *keyx {
                        continue 'next_sqr;
                    }

                    if let Some(stay) = &grpx.anchor {
                        if stay == keyx {
                            continue 'next_sqr;
                        }
                        if *keyx == grpx.region.state_far_from(stay) {
                            continue 'next_sqr;
                        }
                        if keyx.is_adjacent(stay) {
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
        if to_del.is_not_empty() {
            println!(
                "\nDom {} Act {} deleted unneeded squares: {}",
                self.dom_id, self.id, to_del
            );
            for keyx in to_del.iter() {
                if let Some(sqrx) = self.squares.remove(keyx) {
                    self.squares.add_square_to_memory(sqrx);
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
                .find(grpx.region.first_state())
                .expect("Group region states should refer to existing squares");

            if !sqrx.pnc {
                let mut needx = SomeNeed::ConfirmGroup {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    target_state: sqrx.state.clone(),
                    grp_reg: grpx.region.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.add_priority_base();
                ret_nds.push(needx);
                //println!("ConfirmGroup: (1) sqr {sqrx}");
            }

            // If this is a one-state group ..
            if grpx.one_state() {
                if sqrx.pnc {
                    grpx.set_pnc(self.dom_id, self.id);
                }
                continue;
            }

            if let Some(sqry) = self.squares.find(&grpx.region.far_state()) {
                if sqry.pnc {
                    if sqrx.pnc {
                        grpx.set_pnc(self.dom_id, self.id);
                    }
                    continue;
                }
                //println!("ConfirmGroup: (2) sqr {sqrx}");
            }

            let mut needx = SomeNeed::ConfirmGroup {
                dom_id: self.dom_id,
                act_id: self.id,
                target_state: grpx.region.far_state().clone(),
                grp_reg: grpx.region.clone(),
                priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
            };
            needx.add_priority_base();
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
    pub fn limit_groups_needs(&mut self, max_reg: &SomeRegion) -> Option<NeedStore> {
        //println!("limit_groups_needs chg {}", change_mask);
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

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
            if !grpx.pnc {
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

                if !self.groups[group_num].pnc {
                    continue;
                }

                if self.groups[group_num].limited {
                    continue;
                }

                let ndsx = self.limit_group_anchor_needs(&regx, group_num, max_reg);
                if ndsx.is_not_empty() {
                    ret_nds.append(ndsx);
                    continue;
                }

                if let Some(anchor) = self.groups[group_num].anchor.clone() {
                    if max_reg.is_superset_of(&anchor) {
                        if let Some(ndx) =
                            self.limit_group_adj_needs(&regx, &anchor, max_reg, group_num)
                        {
                            ret_nds.append(ndx);
                            continue;
                        } else {
                            let edges = self.groups[group_num]
                                .region
                                .edge_mask()
                                .bitwise_and(&max_reg.x_mask());
                            set_on.push((regx, edges));
                        }
                    }
                }
            }

            // Set limited on for selected groups.
            for (grp_reg, edges) in set_on {
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
    /// the given group, set their limit flag off, to recalculate the best
    /// anchor.
    fn set_group_limited(&mut self, grp_reg: &SomeRegion, edges: SomeMask) {
        debug_assert_eq!(grp_reg.num_bits(), self.num_bits);
        debug_assert_eq!(edges.num_bits(), self.num_bits);

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
    ///     The number of samples taken for the anchor state and adjacent states.
    /// To set an anchor, a square with at least one sample is required, but ..
    /// To rate a possible anchor state, no sample of the state is requried.
    /// When comparing tuples, Rust compares item pairs in order until there is a difference.
    pub fn group_anchor_rate(&self, regx: &SomeRegion, stax: &SomeState) -> (usize, usize, usize) {
        debug_assert_eq!(regx.num_bits(), self.num_bits);
        debug_assert_eq!(stax.num_bits(), self.num_bits);

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

            let stats = self.groups.in_one_anchor(&sta_adj);

            if stats == Some(true) {
                anchors += 1;
            } else if stats == Some(false) {
                in_1_group += 1;
            }

            if let Some(sqrx) = self.squares.find(&sta_adj) {
                sqr_samples += sqrx.rate();
            }
        } // next edge_bit

        // Get anchor samples.
        if let Some(sqrx) = self.squares.find(stax) {
            sqr_samples += sqrx.rate();
        }

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
        debug_assert_eq!(regx.num_bits(), self.num_bits);
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

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
            let stay = ancx.bitwise_xor(&regx.diff_edge_mask(ancx));

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

        // Calculate bridge regions.
        let mut shared_regions = RegionStore::new(vec![]);
        for (inx, grpy) in self.groups.iter().enumerate() {
            if inx == group_num {
                continue;
            }
            if grpy.pn != grpx_pn {
                continue;
            }
            if !regx.is_adjacent(&grpy.region) {
                continue;
            }
            if grpy.region.x_mask() == regx.x_mask() {
                continue;
            }
            if let Some(shared) = grpy.region.bridge(regx) {
                if shared.is_superset_of(regx) {
                    continue;
                }
                if grpx_pn == Pn::Unpredictable {
                    shared_regions.push(shared);
                } else if let Some(ruls) = &grpx_rules {
                    let rulsx = ruls.restrict_initial_region(&shared);
                    if let Some(ruls) = &grpy.rules {
                        let rulsy = ruls.restrict_initial_region(&shared);

                        if let Some(rule_shr) = rulsx.parsed_union(&rulsy) {
                            //println!("shared region : {} {} {shared}", rulsx.initial_region(), rulsy.initial_region());
                            shared_regions.push(rule_shr.initial_region());
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

            let sta_rate = self.group_anchor_rate(regx, stax);

            //println!("group {} possible anchor {} rating {} {} {}", regx, stax, sta_rate.0, sta_rate.1, sta_rate.2);

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
                needx.add_priority_base();
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
            needx.add_priority_base();
            ret_nds.push(needx);
        }
        ret_nds
    } // end limit_group_anchor_needs

    /// Return the limiting needs for a group with an anchor chosen, but not yet set to limited.
    /// If a group does not make a predictable change, also make needs to check adjacent squares
    /// to the anchor, internal to the anchor, as use of the group rules will not happen.
    pub fn limit_group_adj_needs(
        &mut self,
        regx: &SomeRegion,
        anchor_sta: &SomeState,
        max_reg: &SomeRegion,
        group_num: usize,
    ) -> Option<NeedStore> {
        debug_assert_eq!(regx.num_bits(), self.num_bits);
        debug_assert_eq!(anchor_sta.num_bits(), self.num_bits);
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

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
                    if anchor_sqr.compatible(adj_sqr) == Compatibility::Compatible {
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
                    needx.add_priority_base();
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
                needx.add_priority_base();
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

        // Group is non-X, so no far state
        if regx.len() == 1 {
            return None;
        }

        let sta_far = regx.state_far_from(anchor_sta);

        if let Some(sqrf) = self.squares.find(&sta_far) {
            if sqrf.pnc {
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
                needx.add_priority_base();
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
            needx.add_priority_base();
            ret_nds.push(needx);
        }

        // If a group that causes a single pedictable change, it will be confirmed by use.
        if self.groups[group_num].pn == Pn::One
            && self.groups[group_num].causes_predictable_change()
        {
            if ret_nds.is_empty() {
                return None;
            } else {
                return Some(ret_nds);
            }
        }

        // Confirm a group by checking for internal adjacent squares.
        let x_msks: Vec<SomeMask> = regx.x_mask().split();

        for mskx in x_msks {
            let adj_sta = anchor_sta.bitwise_xor(&mskx);

            if let Some(adj_sqr) = self.squares.find(&adj_sta) {
                if adj_sqr.pnc {
                } else {
                    // Get another sample of adjacent square.
                    let mut needx = SomeNeed::ConfirmGroupAdj {
                        dom_id: self.dom_id,
                        act_id: self.id,
                        anchor: anchor_sta.clone(),
                        target_state: adj_sta,
                        for_group: regx.clone(),
                        priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                    };
                    needx.add_priority_base();
                    ret_nds.push(needx);
                }
            } else {
                // Get first sample of adjacent square.
                let mut needx = SomeNeed::ConfirmGroupAdj {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    anchor: anchor_sta.clone(),
                    target_state: adj_sta,
                    for_group: regx.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.add_priority_base();
                ret_nds.push(needx);
            }
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
                } else if grpx.is_adjacent(grpy) {
                    nds.append(self.group_combine_needs(grpx, grpy));
                }
            } // next iny
        } // next inx

        nds
    } // end group_pair_needs

    /// Check if squares in a region can be used to make a single group.
    /// If so, return a region that meets the requirements for a group region, and rules.
    /// The returned region may be smaller than the given region.
    fn check_region_for_group(
        &self,
        regx: &SomeRegion,
        max_pn: Pn,
    ) -> Option<(SomeRegion, Option<RuleStore>)> {
        let squares_in = self.squares.squares_in_reg(regx);

        if squares_in.is_empty() {
            return None;
        }

        // Find squares in region that are pnc, and pn eq.
        let mut sqrs_pnc = Vec::<&SomeSquare>::new();
        let mut sqrs_pn_eq = Vec::<&SomeSquare>::new();

        for sqrx in squares_in.iter() {
            if sqrx.pn == max_pn {
                sqrs_pn_eq.push(sqrx);
                if sqrx.pnc {
                    sqrs_pnc.push(sqrx);
                }
            } else if sqrx.pn < max_pn && sqrx.pnc || sqrx.pn > max_pn {
                return None;
            }
        }
        debug_assert!(max_pn == Pn::One || !sqrs_pn_eq.is_empty());

        // Calc region that can be formed of all pn eq squares.
        let mut reg_pn_eq = SomeRegion::new(vec![sqrs_pn_eq[0].state.clone()]);
        for sqrx in squares_in.iter() {
            if !reg_pn_eq.is_superset_of(&sqrx.state) {
                reg_pn_eq = reg_pn_eq.union(&sqrx.state);
            }
        }
        // Calc rules.
        let mut rules: Option<RuleStore> = None;

        if max_pn < Pn::Unpredictable {
            let mut rulesx = if let Some(rulesy) = &sqrs_pn_eq[0].rules {
                rulesy.clone()
            } else {
                return None;
            };
            for sqrx in sqrs_pn_eq.iter().skip(1) {
                if let Some(rulesy) = &sqrx.rules {
                    if let Some(rulesz) = rulesx.union(rulesy) {
                        rulesx = rulesz;
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            }
            rules = Some(rulesx);
        }

        if sqrs_pnc.len() > 1 {
            // Check if any pair of pnc squares can form the region.
            for inx in 0..(sqrs_pnc.len() - 1) {
                for iny in (inx + 1)..sqrs_pnc.len() {
                    let ret_reg = SomeRegion::new(vec![
                        sqrs_pnc[inx].state.clone(),
                        sqrs_pnc[iny].state.clone(),
                    ]);
                    if ret_reg == reg_pn_eq {
                        return Some((ret_reg, rules));
                    }
                }
            }

            // Check if a pnc square and a pn eq square can form the region.
            for sqrx in sqrs_pnc.iter() {
                for sqry in sqrs_pn_eq.iter() {
                    if sqry.pnc {
                        continue;
                    }
                    let ret_reg = SomeRegion::new(vec![sqrx.state.clone(), sqry.state.clone()]);
                    if ret_reg == reg_pn_eq {
                        return Some((ret_reg, rules));
                    }
                }
            }

            // Choose a pnc square.
            let chosen_sqr = sqrs_pnc[rand::thread_rng().gen_range(0..sqrs_pnc.len())];
            let mut states = Vec::<SomeState>::new();
            states.push(chosen_sqr.state.clone());
            for sqry in sqrs_pn_eq.iter() {
                if sqry.state == chosen_sqr.state {
                    continue;
                }
                states.push(sqry.state.clone());
            }
            return Some((SomeRegion::new(states), rules));
        }

        // No pnc square, choose any pn eq square.
        let chosen_sqr = sqrs_pn_eq[rand::thread_rng().gen_range(0..sqrs_pn_eq.len())];
        let mut states = Vec::<SomeState>::new();
        states.push(chosen_sqr.state.clone());
        for sqry in sqrs_pn_eq.iter() {
            if sqry.state == chosen_sqr.state {
                continue;
            }
            states.push(sqry.state.clone());
        }
        Some((SomeRegion::new(states), rules))
    }

    /// Check two groups that may be combined.
    fn group_combine_needs(&self, grpx: &SomeGroup, grpy: &SomeGroup) -> NeedStore {
        //println!("Dom {} Act {} group_combine_needs: of group {} and {}", self.dom_id, self.id, grpx.region, grpy.region);
        let mut nds = NeedStore::new(vec![]);

        if !grpx.pnc || !grpy.pnc {
            return nds;
        }

        if grpx.pn != grpy.pn {
            return nds;
        }

        if grpx.limited || grpy.limited || grpx.anchor.is_some() || grpy.anchor.is_some() {
            return nds;
        }

        let reg_combined = grpx.region.union(&grpy.region);

        if let Some((regx, rules)) = self.check_region_for_group(&reg_combined, grpx.pn) {
            if regx == reg_combined {
                //println!("group_combine_needs: returning (2) {regx} for combination of {} and {}", grpx.region, grpy.region);
                nds.push(SomeNeed::AddGroup {
                    group_region: regx,
                    rules,
                });
            }
        }

        nds
    }

    /// Check two intersecting groups for
    /// a contradictatory intersection.
    fn group_pair_intersection_needs(
        &self,
        grpx: &SomeGroup,
        grpy: &SomeGroup,
        group_num: usize,
    ) -> NeedStore {
        //println!(
        //  "groups_pair_intersection_needs {} {} and {} {}",
        //  &grpx.region, grpx.pn, grpy.region, grpy.pn
        //);
        debug_assert_eq!(grpx.num_bits(), self.num_bits);
        debug_assert_eq!(grpy.num_bits(), self.num_bits);

        assert!(grpx.region.intersects(&grpy.region));

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

            if let Some(needx) =
                self.cont_int_region_need(&reg_int, grpx, grpy, group_num, rulsx, rulsy)
            {
                return NeedStore::new(vec![needx]);
            }
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

        // If the rules are the same, check if they should be combined.
        if rulsx == rulsy {
            nds.append(self.group_combine_needs(grpx, grpy));
            return nds;
        }

        // If contradictory, return needs to resolve

        // Check if a valid sub-region of the intersection exists
        if let Some(rulsxy) = rulsx.intersection(&rulsy) {
            // A valid sub-union exists, seek a sample in intersection that is not in rulsxy.initial_region
            let ok_reg = rulsxy.initial_region();

            // To test all bits that may be a problem.
            let far_reg = reg_int.far_reg(&ok_reg);

            //println!(
            //    "cont int {} and {}, intersection is {} ok rules {} ok reg {} far reg is {}",
            //    grpx.region, grpy.region, reg_int, rulsxy, ok_reg, far_reg
            //);

            // Calc rules for far region.
            let rulsx = rulsx.restrict_initial_region(&far_reg);
            let rulsy = rulsy.restrict_initial_region(&far_reg);

            if let Some(needx) =
                self.cont_int_region_need(&far_reg, grpx, grpy, group_num, Some(rulsx), Some(rulsy))
            {
                nds.push(needx);
            }
        } else {
            //println!("pn2 whole intersection is bad");
            if let Some(needx) =
                self.cont_int_region_need(&reg_int, grpx, grpy, group_num, Some(rulsx), Some(rulsy))
            {
                nds.push(needx);
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
        group_num: usize,
        rulsx: Option<RuleStore>,
        rulsy: Option<RuleStore>,
    ) -> Option<SomeNeed> {
        //println!("cont_int_region_needs {} for grp {} {} and grp {} {}", regx, grpx.region, grpx.rules, grpy.region, grpy.rules);
        debug_assert_eq!(regx.num_bits(), self.num_bits);
        debug_assert_eq!(grpx.num_bits(), self.num_bits);
        debug_assert_eq!(grpy.num_bits(), self.num_bits);
        debug_assert!(if let Some(ruls) = &rulsx {
            ruls.num_bits().unwrap() == self.num_bits
        } else {
            true
        });
        debug_assert!(if let Some(ruls) = &rulsy {
            ruls.num_bits().unwrap() == self.num_bits
        } else {
            true
        });

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
                needx.add_priority_base();
                Some(needx)
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
                needx.add_priority_base();
                Some(needx)
            }
            Err(PickError::PncSquare) => {
                panic!(
                    "Problem: Dom {} Act {} Pnc square found in {regx} ?",
                    self.dom_id, self.id
                );
                //None
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
        debug_assert_eq!(achange.num_bits(), self.num_bits);
        debug_assert!(if let Some(reg) = &within {
            reg.num_bits() == self.num_bits
        } else {
            true
        });

        debug_assert!(achange.b01.bitwise_and(&achange.b10).is_low()); // No X->x change wanted.

        let mut stps = StepStore::new(vec![]);

        for (inx, grpx) in self.groups.iter().enumerate() {
            if !grpx.causes_predictable_change() {
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
                let rulsx =
                    grpx.rules.as_ref().expect("SNH")[0].restrict_for_changes(achange, within);
                for rulx in rulsx.into_iter() {
                    stps.push(SomeStep::new(self.id, rulx, AltRuleHint::NoAlt {}, inx));
                }
                continue;
            }

            if grpx.pn == Pn::Two {
                // Get restricted regions for needed changes.

                let stps2 = self.get_steps2(0, 1, grpx, inx, achange, within);
                stps.append(stps2);

                let stps2 = self.get_steps2(1, 0, grpx, inx, achange, within);
                stps.append(stps2);
            } // end Pn::Two
        } // next grpx

        // println!("Steps: {}", stps);
        stps
    } // end get_steps

    /// Processing for Pn::Two rules.
    /// Check if an existing square may be expected to produce the wanted change on the first try.
    /// Check if an unwanted response will be no change, so trying again is easy.
    fn get_steps2(
        &self,
        inx: usize,
        iny: usize,
        grpx: &SomeGroup,
        grpx_inx: usize,
        achange: &SomeChange,
        within: Option<&SomeRegion>,
    ) -> StepStore {
        debug_assert_eq!(grpx.num_bits(), self.num_bits);
        debug_assert_eq!(achange.num_bits(), self.num_bits);
        debug_assert!(if let Some(reg) = &within {
            reg.num_bits() == self.num_bits
        } else {
            true
        });

        let mut stps = StepStore::new(vec![]);

        // Get rules restricted for changes.  There will be a rule for each single-bit change needed.
        let rulsx = grpx.rules.as_ref().expect("SNH")[inx].restrict_for_changes(achange, within);

        for rulx in rulsx.into_iter() {
            // Check if other rule has no changes, so a state could be sampled once, or twice, to get the desired change.
            let ruly = &grpx.rules.as_ref().expect("SNH")[iny];

            let ruly2 = ruly.restrict_initial_region(&rulx.initial_region());

            if ruly2.b01.is_low() && ruly2.b10.is_low() {
                stps.push(SomeStep::new(
                    self.id,
                    rulx,
                    AltRuleHint::AltNoChange {},
                    grpx_inx,
                ));
                continue;
            }

            // See if an existing square is ready to produce the desired result
            let i_reg = rulx.initial_region();
            let sqrs = self.squares.squares_in_reg(&i_reg);

            for sqrx in &sqrs {
                // Will include at least one bit change desired, but maybe others.
                let expected_result = rulx.result_from_initial_state(&sqrx.state);

                // If a Pn::Two squares last result is not equal to what is wanted,
                // the next result should be.
                if *sqrx.most_recent_result() != expected_result {
                    let stpx = SomeStep::new(
                        self.id,
                        rulx.restrict_initial_region(&SomeRegion::new(vec![sqrx.state.clone()])),
                        AltRuleHint::NoAlt {},
                        grpx_inx,
                    );
                    stps.push(stpx);
                } // end if
            } // next sqrx

            stps.push(SomeStep::new(
                self.id,
                rulx,
                AltRuleHint::AltRule { rule: ruly2 },
                grpx_inx,
            ));
        } // next rulx

        stps
    }

    /// Find groups that can be formed by a given square, and other similar squares.
    /// Squares that are incompatible limit the possible groups.
    /// Compatible squares, can be mutually incompatible. 0->1 and 0->0 are incompatible with each other,
    /// but compatible with 1->1.
    fn possible_groups_from_square(&self, sqrx: &SomeSquare) -> Vec<SomeGroup> {
        //println!("Dom {} Act {} possible_groups_from_square: {}", self.dom_id, self.id, sqrx.state);
        debug_assert_eq!(sqrx.num_bits(), self.num_bits);

        let mut ret_grps = Vec::<SomeGroup>::new();

        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return ret_grps;
        }

        // Calc the maximum possible region.
        let max_poss_reg = SomeRegion::new(vec![sqrx.state.new_high(), sqrx.state.new_low()]);

        // Init list for holding possible regions.
        let mut poss_regs = RegionStore::new(vec![max_poss_reg.clone()]);

        // Subtract states of incompatible squares.

        // Get squares that are in the region.
        let mut sqrs_in = Vec::<&SomeSquare>::new();
        for regx in poss_regs.iter() {
            let tmp_sqrs_in = self.squares.squares_in_reg(regx);
            for sqry in tmp_sqrs_in.iter() {
                if !sqrs_in.contains(sqry) {
                    sqrs_in.push(sqry);
                }
            }
        }

        // Check memory for additional squares.
        for regx in poss_regs.iter() {
            let tmp_sqrs_in = self.squares.memory_squares_in_reg(regx);
            for sqry in tmp_sqrs_in.iter() {
                if !sqrs_in.contains(sqry) {
                    sqrs_in.push(sqry);
                }
            }
        }

        // Subtract dissimilar squares.
        for sqry in sqrs_in.iter() {
            if sqry.state == sqrx.state {
                continue;
            }

            // Previous subtractions may put some squares out of reach.
            if poss_regs.any_superset_of_state(&sqry.state)
                && (sqrx.pn < sqry.pn || sqrx.compatible(sqry) == Compatibility::NotCompatible)
            {
                //println!("square {} not compatible with {}", sqrx, sqry);
                poss_regs = poss_regs.subtract_state_to_supersets_of(&sqry.state, &sqrx.state);
            }
        }

        // Check each possible region for subregions.
        for regx in poss_regs.iter() {
            let other_sqrs_in = self.squares.squares_in_reg(regx);

            // Process an Unpredictable square.
            if sqrx.pn == Pn::Unpredictable {
                if let Some(grpx) = self.validate_possible_group(sqrx, regx) {
                    ret_grps.push(grpx);
                }
                continue;
            }

            let mut poss_regs2 = RegionStore::new(vec![regx.clone()]);

            // Calc excluded regions formed by pairs of similar squares
            // that cannot be combined.
            // If the target square has a 1->0 bit,
            // it may combine with a square having a corresponding 0->1, forming X->x,
            // and one with 0->0, forming X->0,
            // but 0->0 and 0->1 cannot combine.
            if other_sqrs_in.len() > 1 {
                let mut excluded_regs = RegionStore::new(vec![]);

                for iny in 0..(other_sqrs_in.len() - 1) {
                    for inz in (iny + 1)..other_sqrs_in.len() {
                        if sqrx.pn == Pn::One {
                            // other two pn must == Pn::One
                            let rslt = other_sqrs_in[iny].compatible(other_sqrs_in[inz]);

                            // println!("checking {} {} rslt {rslt}", other_sqrs_in[iny], other_sqrs_in[inz]);
                            if rslt == Compatibility::NotCompatible {
                                excluded_regs.push_nosups(SomeRegion::new(vec![
                                    other_sqrs_in[iny].state.clone(),
                                    other_sqrs_in[inz].state.clone(),
                                ]));
                            }
                            continue;
                        }

                        // sqrx.pn == Pn::Two

                        // Check compatibility if the pair is compatible with the same
                        // part ([0] or [1]) of the Pn::Two RuleStore.
                        if other_sqrs_in[iny].pn == Pn::One && other_sqrs_in[inz].pn == Pn::One {
                            if sqrx.rules.as_ref().expect("SNH").subcompatible_index(
                                other_sqrs_in[iny].rules.as_ref().expect("SNH"),
                            ) != sqrx.rules.as_ref().expect("SNH").subcompatible_index(
                                other_sqrs_in[inz].rules.as_ref().expect("SNH"),
                            ) {
                                continue;
                            }
                            if !other_sqrs_in[iny]
                                .rules
                                .as_ref()
                                .unwrap()
                                .compatible(other_sqrs_in[inz].rules.as_ref().unwrap())
                            {
                                excluded_regs.push_nosups(SomeRegion::new(vec![
                                    other_sqrs_in[iny].state.clone(),
                                    other_sqrs_in[inz].state.clone(),
                                ]));
                            }
                            continue;
                        }

                        if other_sqrs_in[iny].pn == Pn::Two && other_sqrs_in[inz].pn == Pn::Two {
                            if let Some(rulsyz) = other_sqrs_in[iny]
                                .rules
                                .as_ref()
                                .unwrap()
                                .union(other_sqrs_in[inz].rules.as_ref().unwrap())
                            {
                                if rulsyz.union(sqrx.rules.as_ref().unwrap()).is_some() {
                                    continue;
                                }
                            }
                            excluded_regs.push_nosups(SomeRegion::new(vec![
                                other_sqrs_in[iny].state.clone(),
                                other_sqrs_in[inz].state.clone(),
                            ]));
                            continue;
                        }

                        if other_sqrs_in[inz].pn == Pn::Two {
                            // other_sqrs_in[iny].pn == Pn::One
                            if let Some(rulsxz) = sqrx
                                .rules
                                .as_ref()
                                .unwrap()
                                .union(other_sqrs_in[inz].rules.as_ref().unwrap())
                            {
                                if rulsxz
                                    .subcompatible_index(other_sqrs_in[iny].rules.as_ref().unwrap())
                                    .is_some()
                                {
                                    continue;
                                }
                            }
                            excluded_regs.push_nosups(SomeRegion::new(vec![
                                other_sqrs_in[iny].state.clone(),
                                other_sqrs_in[inz].state.clone(),
                            ]));
                            continue;
                        }

                        // other_sqrs_in[iny].pn == Pn::Two, other_sqrs_in[inz].pn == Pn::One
                        if let Some(rulsxy) = sqrx
                            .rules
                            .as_ref()
                            .unwrap()
                            .union(other_sqrs_in[iny].rules.as_ref().unwrap())
                        {
                            if rulsxy
                                .subcompatible_index(other_sqrs_in[inz].rules.as_ref().unwrap())
                                .is_some()
                            {
                                continue;
                            }
                        }
                        excluded_regs.push_nosups(SomeRegion::new(vec![
                            other_sqrs_in[iny].state.clone(),
                            other_sqrs_in[inz].state.clone(),
                        ]));
                    } // next inz
                } // next iny

                //println!("excluded regions {excluded_regs}");

                // Subtract excluded region state pairs.
                // So regions can contain either state, but not both.
                //
                // There is a tendency to produce contradictory intersections.
                // e.g. Given 4->4, 1->0 and D->D, that results in 0X0X and X10X, intersecting at 010X.
                // Within 010X, 5 is contradictory.
                //
                if !excluded_regs.is_empty() {
                    //println!("excluded regs {excluded_regs}");
                    //println!("poss_regs2 before: {poss_regs2}");
                    poss_regs2 = poss_regs2
                        .intersection(&excluded_regs.possible_regions_by_negative_inference());

                    poss_regs2 = poss_regs2.supersets_of(&sqrx.state);
                    //println!("poss_regs2 after: {poss_regs2}");
                }
            } // end if other_sqrs_in.len() > 1

            for regz in poss_regs2.iter() {
                if let Some(grpx) = self.validate_possible_group(sqrx, regz) {
                    ret_grps.push(grpx);
                }
            }
        } // next regx

        // If no groups, create a one-state group.
        if ret_grps.is_empty() {
            ret_grps.push(SomeGroup::new(
                SomeRegion::new(vec![sqrx.state.clone()]),
                sqrx.rules.clone(),
            ));
        }
        ret_grps
    } // end possible_regions_from_square

    /// Validate a region that may be made from a given square, in combination with similar squares.
    fn validate_possible_group(&self, sqrx: &SomeSquare, regx: &SomeRegion) -> Option<SomeGroup> {
        // println!("validate_possible_group: state {} num sim {} reg {}", sqrx.state, not_dissim_sqrs.len(), regx);
        debug_assert_eq!(sqrx.num_bits(), self.num_bits);
        debug_assert_eq!(regx.num_bits(), self.num_bits);
        debug_assert!(regx.is_superset_of(sqrx));

        if let Some((regy, rules)) = self.check_region_for_group(regx, sqrx.pn) {
            if regy.is_superset_of(&sqrx.state) {
                Some(SomeGroup::new(regy, rules))
            } else {
                None
            }
        } else {
            None
        }
    } // end validate_combination

    /// Take an action for a need.
    pub fn take_action_need(&mut self, cur_state: &SomeState, ndx: &SomeNeed) -> SomeSample {
        //println!("Dom {} Act {} take_action_arbitrary cur_state {cur_state}", self.dom_id, self.id);
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);
        debug_assert_eq!(ndx.act_id(), self.id);

        let asample = self.take_action_arbitrary(cur_state);

        // Additional processing for selected kinds of need
        match ndx {
            SomeNeed::ConfirmGroup { grp_reg, .. } => {
                // The sample could have invalidated the group.
                if let Some(grpx) = self.groups.find_mut(grp_reg) {
                    if !grpx.pnc {
                        let sqr1 = self.squares.find(grp_reg.first_state()).expect("SNH");
                        if grp_reg.len() == 1 {
                            if sqr1.pnc {
                                grpx.set_pnc(self.dom_id, self.id);
                            }
                        } else if let Some(sqr2) = self.squares.find(&grp_reg.far_state()) {
                            if sqr1.pnc && sqr2.pnc {
                                grpx.set_pnc(self.dom_id, self.id);
                            }
                        }
                    }
                }
            }
            SomeNeed::StateInRemainder { dom_id, .. } => {
                if !self.groups.any_superset_of(cur_state) {
                    let sqr1 = self.squares.find(cur_state).expect("SNH");
                    if sqr1.pnc {
                        self.check_remainder = true;
                        self.groups.push_nosubs(
                            SomeGroup::new(
                                SomeRegion::new(vec![cur_state.clone()]),
                                sqr1.rules.clone(),
                            ),
                            *dom_id,
                            self.id,
                        );
                    }
                }
            }
            _ => {}
        }

        asample
    }

    /// Take an action with the current state, add the sample teo squarestore.
    /// Return a sample.
    pub fn take_action_arbitrary(&mut self, cur_state: &SomeState) -> SomeSample {
        //println!("Dom {} Act {} take_action_arbitrary cur_state {cur_state}", self.dom_id, self.id);
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);

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
        //println!("Dom {} Act {} take_action_step cur_state {cur_state}", self.dom_id, self.id);
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);

        let astate = self
            .do_something
            .take_action(cur_state, self.dom_id, self.id);

        let asample = SomeSample::new(cur_state.clone(), astate);

        self.eval_sample(&asample);

        asample
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
        debug_assert_eq!(aregion.num_bits(), self.num_bits);

        if let Some(grpx) = self.groups.find(aregion) {
            self.display_group_anchor_info2(grpx)
        } else {
            Err("Group with region {aregion} not found".to_string())
        }
    }

    /// Display a groups' achor info.
    pub fn display_group_anchor_info2(&self, grpx: &SomeGroup) -> Result<(), String> {
        debug_assert_eq!(grpx.num_bits(), self.num_bits);

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
                    let sqrx = self.squares.find(stax).expect("SNH");
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
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

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
} // end impl SomeAction

// Some action tests are made from the domain level.
#[cfg(test)]
mod tests {
    use super::*;

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
    fn two_result_group() -> Result<(), String> {
        // Init action
        let sx = SomeState::new_from_string("s0b0000")?;
        let mut act0 = SomeAction::new(0, 0, &sx, vec![]);

        // Put in two incompatible one-result squares, but both subset of the
        // later two-result squares.
        // 0->1 and 0->1, in the fourth bit.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0001->0b1001")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0101->0b0101")?);

        // Set up first two_result square.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0000->0b0000")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0000->0b1000")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0000->0b0000")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0000->0b1000")?);

        // Set up second two_result square.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0111->0b1111")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0111->0b0111")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0111->0b1111")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0111->0b0111")?);

        println!("Groups {}", act0.groups);
        assert!(act0.groups.len() == 1);

        Ok(())
    }

    // Test making groups from a few single-sample states.
    #[test]
    fn groups_formed_1() -> Result<(), String> {
        // Init action
        let sx = SomeState::new_from_string("s0b0000")?;
        let mut act0 = SomeAction::new(0, 0, &sx, vec![]);

        // Eval sample that other samples will be incompatible with.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0111->0b0111")?);

        // Process three similar samples.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b1011->0b1010")?);

        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b1101->0b1100")?);

        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0001->0b0000")?);

        println!("Groups {}", act0.groups);
        assert!(act0.groups.len() == 4);

        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("r0111")?)
            .is_some());
        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("r1xx1")?)
            .is_some());
        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("rxx01")?)
            .is_some());
        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("rx0x1")?)
            .is_some());
        Ok(())
    }

    // Test making a group from two Pn::Two squares.
    #[test]
    fn possible_region() -> Result<(), String> {
        // Init Action.
        let sx = SomeState::new_from_string("s0b0000")?;
        let mut act0 = SomeAction::new(0, 0, &sx, vec![]);

        let max_reg = SomeRegion::new_from_string("rXXXX")?;

        // Set up 2-result square sf.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b1111->0b1111")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b1111->0b1110")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b1111->0b1111")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up 2-result square s1.
        let s1 = SomeState::new_from_string("s0b0001")?;
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0001->0b0001")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0001->0b0000")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0001->0b0001")?);
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0001->0b0000")?);

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
        let sx = SomeState::new_from_string("s0b0000")?;
        let mut act0 = SomeAction::new(0, 0, &sx, vec![]);

        // Set up square 0.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0000->0b0000")?);

        // Set up square 3.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0011->0b0011")?);

        // Set up square 5.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0101->0b0101")?);

        println!("Act: {}", act0);

        assert!(act0.groups.len() == 1);
        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("r0xxx")?)
            .is_some());

        Ok(())
    }

    // Test making a region from three samples, with one dissimilar sample.
    #[test]
    fn three_sample_region2() -> Result<(), String> {
        // Init action.
        let sx = SomeState::new_from_string("s0b0000")?;
        let mut act0 = SomeAction::new(0, 0, &sx, vec![]);

        // Set up square 0.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0000->0b0000")?);

        // Set up square 3.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0011->0b0011")?);

        // Set up square 4, dissimilar to s5 by third bit being 1->0.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0100->0b0000")?);

        // Set up square 5.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0101->0b0101")?);

        println!("Act: {}", act0);

        assert!(act0.groups.len() == 3);
        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("r0x00")?)
            .is_some());
        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("r00xx")?)
            .is_some());
        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("r0xx1")?)
            .is_some());

        Ok(())
    }

    // Test making a region from three samples, with two similar samples that cannot be combined,
    // due to the second bits being 1->0 and 1->1.
    #[test]
    fn three_sample_region3() -> Result<(), String> {
        // Init action.
        let sx = SomeState::new_from_string("s0b0000")?;
        let mut act0 = SomeAction::new(0, 0, &sx, vec![]);

        // Set up square 2.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0010->0b0000")?);

        // Set up square b.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b1011->0b1011")?);

        // Set up square 5.
        act0.eval_sample_arbitrary(&SomeSample::new_from_string("0b0101->0b0101")?);

        println!("Act: {}", act0);

        assert!(act0.groups.len() == 2);
        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("r0xxx")?)
            .is_some());
        assert!(act0
            .groups
            .find(&SomeRegion::new_from_string("rxxx1")?)
            .is_some());

        Ok(())
    }
}
