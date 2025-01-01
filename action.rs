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
use crate::square::{Compatibility, SomeSquare};
use crate::squarestore::{PickError, SquareStore};
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::step::{AltRuleHint, SomeStep};
use crate::stepstore::StepStore;
use crate::target::ATarget;
use crate::tools::{self, AccessStates};

use rand::Rng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;
use unicode_segmentation::UnicodeSegmentation;

const CLEANUP_TRIGGER: usize = 5;

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_state())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Clone)]
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
    /// Number of new squares since last cleanup.
    cleanup_number_new_squares: usize,
    /// When the actions groups change check for any missed regions.
    check_remainder: bool,
    /// Regions currently not covered by groups, when tere are no more needs to sample squares.
    remainder_check_regions: RegionStore,
    /// Changes possible for all groups.
    aggregate_changes: Option<SomeChange>,
    /// An indicator that the changes possible were recently updated.
    pub agg_chgs_updated: bool,
    /// Number of new squares added before a cleanup check is run.
    cleanup_trigger: usize,
}

/// Implement the PartialEq trait, since two SomeAction instances.
/// A quick comparison of definitions.
impl PartialEq for SomeAction {
    fn eq(&self, other: &Self) -> bool {
        if self.num_bits() != other.num_bits() {
            return false;
        }
        true
    }
}
impl Eq for SomeAction {}

impl SomeAction {
    /// Return a new SomeAction struct.
    pub fn new(rules: Vec<RuleStore>) -> Self {
        assert!(!rules.is_empty());
        let num_bits = rules[0].num_bits().expect("SNH");
        SomeAction {
            id: 0,
            dom_id: 0,
            num_bits: rules[0].num_bits().expect("SNH"),
            groups: GroupStore::new(vec![]),
            squares: SquareStore::new(HashMap::new(), num_bits),
            do_something: ActionInterface::new(rules),
            cleanup_number_new_squares: 0,
            check_remainder: false,
            remainder_check_regions: RegionStore::new(vec![]),
            aggregate_changes: None,
            agg_chgs_updated: false,
            cleanup_trigger: CLEANUP_TRIGGER,
        }
    }

    /// Return the number of bits used in an action.
    pub fn num_bits(&self) -> usize {
        self.num_bits
    }

    /// Set the id field.
    pub fn set_id(&mut self, id: usize) {
        self.id = id;
    }

    /// Set the domain id field.
    pub fn set_dom_id(&mut self, dom_id: usize) {
        self.dom_id = dom_id;
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

        self.cleanup_number_new_squares += 1;

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

        // Check if it invalidates any groups.
        let regs_invalid: RegionStore = self.groups_check_square(key);

        let sqrx = self.squares.find_mut(key).expect("SNH");

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

        for grpx in self.groups.iter_mut() {
            if grpx.pnc || !grpx.region.is_superset_of(key) {
                continue;
            }

            if grpx.region.len() == 1 {
                if grpx.set_pnc() {
                    println!(
                        "Dom {} Act {} Group {} confirmed",
                        self.dom_id, self.id, grpx.region
                    );
                }
                continue;
            }

            // If the square is first, or last, in the region,
            // check if the group can be set to pnc.
            if key == grpx.region.first_state() {
                if let Some(sqr2) = self.squares.find(&grpx.region.far_state()) {
                    if sqr2.pnc && grpx.set_pnc() {
                        println!(
                            "Dom {} Act {} Group {} confirmed",
                            self.dom_id, self.id, grpx.region
                        );
                    }
                }
                continue;
            }

            if *key == grpx.region.far_state() {
                let sqr1 = self.squares.find(grpx.region.first_state()).expect("SHN");
                if sqr1.pnc && grpx.set_pnc() {
                    println!(
                        "Dom {} Act {} Group {} confirmed",
                        self.dom_id, self.id, grpx.region
                    );
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

        // Remove the groups.
        for regx in invalid_regs.iter() {
            self.remove_group(regx);
        }

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
        let groups: Vec<GroupStore> = keys
            .par_iter() // par_iter for parallel processing, iter for sequential diagnostic messages.
            .map(|keyx| self.create_groups_from_squares2(keyx))
            .collect::<Vec<GroupStore>>();

        // Store possible groups, some may be duplicates.
        for grpstrx in groups {
            for grpx in grpstrx.into_iter() {
                if !self.groups.any_superset_of(&grpx.region) {
                    self.groups_push_nosubs(grpx);
                }
            }
        }
    }

    /// Check groups due to a new, or updated, square.
    fn create_groups_from_squares2(&self, key: &SomeState) -> GroupStore {
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
            return GroupStore::new(vec![]);
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
                        target: ATarget::State {
                            state: cur_state.clone(),
                        },
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
                    target: ATarget::State {
                        state: cur_state.clone(),
                    },
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
                target: ATarget::State {
                    state: stax.clone(),
                },
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
    ///
    /// You may think that the needs can be stored, and used again, if the action is
    /// not changed by a new sample.  However, changes to the current state, via
    /// a different action can elecit a new need in this action, if the new state is
    /// not in a group. A change in another action could expand, or restrict, the possible bit changes,
    /// affecting all actions needs.
    pub fn get_needs(&mut self, cur_state: &SomeState, max_reg: &SomeRegion) -> NeedStore {
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

        let mut ret = NeedStore::new(vec![]);

        let mut get_new_needs = true;
        while get_new_needs {
            // Get some needs.
            ret = self.get_needs2(cur_state, max_reg);

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

        // Check for cleanup.
        // For testing, set cleanup_trigger high to suppress this.
        if self.cleanup_number_new_squares >= self.cleanup_trigger {
            self.cleanup(&ret);
        }
        ret
    }

    /// Get needs, process any housekeeping needs.
    pub fn get_needs2(&mut self, cur_state: &SomeState, max_reg: &SomeRegion) -> NeedStore {
        //println!("action::get_needs2: Dom {} Act {} cur_state {cur_state} max_reg {max_reg}", self.dom_id, self.id);
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

        let mut nds = NeedStore::new(vec![]);

        // loop until no housekeeping need is returned.
        let mut try_again = true;
        let mut count = 0;
        while try_again {
            try_again = false;
            count += 1;
            if count > 20 {
                return nds;
            }

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
            for ndx in nds.iter() {
                match ndx {
                    SomeNeed::AddGroup { group_region, .. } => {
                        new_grp_regs.push_nosubs(group_region.clone())
                    }
                    _ => continue,
                };
            } // next ndx

            let mut remvec = vec![];
            for (inx, ndx) in nds.iter().enumerate() {
                match ndx {
                    SomeNeed::AddGroup { group_region, .. } => {
                        if self.groups.any_superset_of(group_region)
                            || !new_grp_regs.contains(group_region)
                        {
                            remvec.push(inx);
                        }
                    }
                    _ => continue,
                };
            } // next ndx

            // Remove unneeded AddGroups, highest index first.
            remvec.reverse();
            for inx in remvec {
                nds.remove(inx);
            }

            // Process housekeeping needs.
            for ndx in nds.iter_mut() {
                if let SomeNeed::AddGroup {
                    group_region,
                    rules,
                    pnc,
                } = ndx
                {
                    self.groups_push_nosubs(SomeGroup::new(
                        group_region.clone(),
                        rules.clone(),
                        *pnc,
                    ));
                    try_again = true;
                }
            } // next ndx
        } // end loop

        // Look for needs for states not in groups
        nds.append(self.state_not_in_group_needs(cur_state));

        if nds.kind_is_in("StateNotInGroup") {
        } else {
            if self.check_remainder {
                self.remainder_check_regions = self.remainder_check_region(max_reg);
                self.check_remainder = false;
            }
            if self.remainder_check_regions.len() > 1 {
                let rem_frags = self.remainder_check_regions.split_by_intersections();

                // Find number of intersections for priority setting.
                let mut frags_num_ints = Vec::<usize>::with_capacity(rem_frags.len());
                for regx in rem_frags.iter() {
                    frags_num_ints.push(self.remainder_check_regions.number_supersets_of(regx));
                }

                for (inx, regx) in rem_frags.into_iter().enumerate() {
                    let mut needx = SomeNeed::StateInRemainder {
                        dom_id: self.dom_id,
                        act_id: self.id,
                        target: ATarget::Region { region: regx },
                        priority: self.remainder_check_regions.len() - frags_num_ints[inx],
                    };
                    needx.add_priority_base();
                    nds.push(needx);
                }
            }
            for regx in self.remainder_check_regions.iter() {
                let mut needx = SomeNeed::StateInRemainder {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    target: ATarget::Region {
                        region: regx.clone(),
                    },
                    priority: self.remainder_check_regions.len(),
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
            remainder_regs = remainder_regs.subtract_region(&grpx.region);
        }

        remainder_regs
    }

    /// Cleanup unneeded squares.
    pub fn cleanup(&mut self, needs: &NeedStore) {
        //println!("action {} cleanup", self.id);
        // Check for groups that can be deleted.
        if needs.is_empty() {
            let mut del = Vec::<SomeRegion>::new();
            for grpx in self.groups.iter() {
                if grpx.anchor.is_none() {
                    del.push(grpx.region.clone());
                }
            }
            for regx in del.iter() {
                self.remove_group(regx);
            }
        }
        // Store for keys of squares to delete.
        let mut to_del = StateStore::new(vec![]);

        'next_sqr: for keyx in self.squares.ahash.keys() {
            // Check needs
            for ndx in needs.iter() {
                if match ndx.target() {
                    ATarget::State { state } => *state == *keyx,
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
                        if *keyx == grpx.region.far_from(stay) {
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

        self.cleanup_number_new_squares = 0;
    } // end cleanup

    /// Get additional sample needs for the states that form a group.
    /// Should only affect groups with Pn::One.
    /// Groups closer to the beginning of the group will have priority due to lower group number.
    pub fn confirm_group_needs(&mut self) -> NeedStore {
        //println!("action::confirm_group_needs: Dom {} Act {}", self.dom_id, self.id);
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
                    target: ATarget::State {
                        state: sqrx.state.clone(),
                    },
                    grp_reg: grpx.region.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.add_priority_base();
                ret_nds.push(needx);
                //println!("ConfirmGroup: (1) sqr {sqrx}");
            }

            // If this is a one-state group ..
            if grpx.one_state() {
                if sqrx.pnc && grpx.set_pnc() {
                    println!(
                        "Dom {} Act {} Group {} confirmed",
                        self.dom_id, self.id, grpx.region
                    );
                }

                continue;
            }

            if let Some(sqry) = self.squares.find(&grpx.region.far_state()) {
                if sqry.pnc {
                    if sqrx.pnc && grpx.set_pnc() {
                        println!(
                            "Dom {} Act {} Group {} confirmed",
                            self.dom_id, self.id, grpx.region
                        );
                    }
                    continue;
                }
                //println!("ConfirmGroup: (2) sqr {sqrx}");
            }

            let mut needx = SomeNeed::ConfirmGroup {
                dom_id: self.dom_id,
                act_id: self.id,
                target: ATarget::State {
                    state: grpx.region.far_state(),
                },
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
        //println!("action::limit_groups_needs: Dom {} Act {} max_reg {max_reg}", self.dom_id, self.id);
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

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
                        if *anchor_mask != max_mask && grpx.set_limited_off() {
                            println!(
                                "Dom {} Act {} Group {} set limited off",
                                self.dom_id, self.id, grpx.region
                            );
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
                if !self.groups[group_num].pnc {
                    continue;
                }

                if self.groups[group_num].limited {
                    continue;
                }

                let regx = self.groups[group_num].region.clone();

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

        println!(
            "Dom {} Act {} Group {} set limited on, adj mask {}",
            self.dom_id, self.id, grp_reg, edges
        );

        let grpx = self.groups.find_mut(grp_reg).expect("SNH");

        grpx.set_limited(edges);

        for grpy in self.groups.iter_mut() {
            if grpy.limited {
            } else {
                continue;
            }
            if grpy.region == *grp_reg {
                continue;
            }
            if let Some(edgesy) = &grpy.anchor_mask {
                if edgesy.num_one_bits() < num_edges && grpy.set_limited_off() {
                    println!(
                        "Dom {} Act {} Group {} set limited off",
                        self.dom_id, self.id, grpy.region
                    );
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
        for edge_bit in edge_msks.iter() {
            let sta_adj = stax.bitwise_xor(edge_bit).as_state();

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
        //println!(
        //    "action::limit_group_anchor_needs: Dom {} Act {} group {regx} max_reg {max_reg}",
        //    self.dom_id, self.id
        //);
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
            let stay = ancx.bitwise_xor(&regx.diff_edge_mask(ancx)).as_state();

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

        //println!("action::limit_group_anchor_needs: stas to check {stas_in}");

        //println!("shared regions {shared_regions}");

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
            if !self.groups.in_1_group(stax) || !max_reg.is_superset_of(stax) {
                //println!("stax for anchor {stax} in group {} skipped", regx);
                continue;
            }

            let sta_rate = self.group_anchor_rate(regx, stax);

            //println!(
            //    "group {} possible anchor {} rating {} {} {}",
            //    regx, stax, sta_rate.0, sta_rate.1, sta_rate.2
            //);

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
            //println!("group {} cfmv_max empty", regx);
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
                    //println!("group {} anchor {} pnc", regx, anchor);
                    return NeedStore::new(vec![]);
                }

                // Get additional samples of the anchor
                let mut needx = SomeNeed::LimitGroup {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    anchor: anchor.clone(),
                    target: ATarget::State {
                        state: anchor.clone(),
                    },
                    for_group: regx.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.add_priority_base();
                ret_nds.push(needx);

                //println!("Returning {ret_nds}");

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
                target: ATarget::State {
                    state: cfm_max.clone(),
                },
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
        // println!(
        //     "action::limit_group_adj_needs: Dom {} Act {} group {regx} anchor {anchor_sta}",
        //     self.dom_id, self.id
        // );

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
            //    println!("action::limit_group_adj_needs: returning None (1)");
            return None;
        }

        let change_bits = regx.edge_mask().bitwise_and(&max_reg.x_mask());

        let edge_msks = change_bits.split();

        for mskx in edge_msks.iter() {
            let adj_sta = anchor_sta.bitwise_xor(mskx).as_state();

            //println!("*** for group {} checking adj sqr {}", regx, adj_sta);

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
                            pnc: anchor_sqr.pnc,
                        });
                    }
                } else {
                    // Get another sample of adjacent square.
                    let mut needx = SomeNeed::LimitGroupAdj {
                        dom_id: self.dom_id,
                        act_id: self.id,
                        anchor: anchor_sta.clone(),
                        target: ATarget::State { state: adj_sta },
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
                    target: ATarget::State { state: adj_sta },
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
            //println!("action::limit_group_adj_needs: returning None (2)");
            return None;
        }

        let sta_far = regx.far_from(anchor_sta);

        if let Some(sqrf) = self.squares.find(&sta_far) {
            if sqrf.pnc {
            } else {
                // Get additional samples of the far state.
                let mut needx = SomeNeed::LimitGroup {
                    dom_id: self.dom_id,
                    act_id: self.id,
                    anchor: anchor_sta.clone(),
                    target: ATarget::State { state: sta_far },
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
                target: ATarget::State { state: sta_far },
                for_group: regx.clone(),
                priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
            };
            needx.add_priority_base();
            ret_nds.push(needx);
        }

        // If a group causes a pedictable change, it will be confirmed by use.
        if self.groups[group_num].causes_predictable_change()
            && self.groups[group_num].edge_mask().is_not_low()
        {
            if ret_nds.is_empty() {
                //println!("action::limit_group_adj_needs: returning None (3)");
                return None;
            } else {
                //println!("action::limit_group_adj_needs: returning {ret_nds}");
                return Some(ret_nds);
            }
        }

        // Confirm a group by checking for internal adjacent squares.
        let x_msks = regx.x_mask().split();

        for mskx in x_msks.iter() {
            let adj_sta = anchor_sta.bitwise_xor(mskx).as_state();

            if let Some(adj_sqr) = self.squares.find(&adj_sta) {
                if adj_sqr.pnc {
                } else {
                    // Get another sample of adjacent square.
                    let mut needx = SomeNeed::ConfirmGroupAdj {
                        dom_id: self.dom_id,
                        act_id: self.id,
                        anchor: anchor_sta.clone(),
                        target: ATarget::State { state: adj_sta },
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
                    target: ATarget::State { state: adj_sta },
                    for_group: regx.clone(),
                    priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                };
                needx.add_priority_base();
                ret_nds.push(needx);
            }
        }

        //println!("limit_group_needs: returning {}", &ret_nds);
        if ret_nds.is_empty() {
            //println!("action::limit_group_adj_needs: returning None (4)");
            None
        } else {
            //println!("action::limit_group_adj_needs: returning (2) {ret_nds}");
            Some(ret_nds)
        }
    } // end limit_group_adj_needs

    /// Check group pairs for an intersection.
    pub fn group_pair_needs(&self) -> NeedStore {
        //println!("action::group_pair_needs: Dom {} Act {}", self.dom_id, self.id);
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
        let squares_in: Vec<&SomeSquare> = self.squares.squares_in_reg(regx);

        if squares_in.is_empty() {
            return None;
        }

        // Find squares in region that are pnc, and pn eq.
        let mut sqrs_pn_eq = Vec::<&SomeSquare>::new();
        let mut sqrs_pnc = Vec::<&SomeSquare>::new(); // A subset of sqrs_pn_eq.

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
        if max_pn > Pn::One {
            if sqrs_pnc.is_empty() {
                return None;
            }
            sqrs_pn_eq = sqrs_pnc;
        }

        // Calc region that can be formed of all pn eq squares.
        let mut reg_pn_eq = SomeRegion::new(vec![sqrs_pn_eq[0].state.clone()]);
        for sqrx in sqrs_pn_eq.iter() {
            if !reg_pn_eq.is_superset_of(&sqrx.state) {
                reg_pn_eq = reg_pn_eq.union(&sqrx.state);
            }
        }

        // Calc rules.
        let mut rules: Option<RuleStore> = None;

        if max_pn < Pn::Unpredictable {
            let mut rulesx = sqrs_pn_eq[0].rules.as_ref().unwrap().clone();

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

        // Load square states.
        let mut stas_in = Vec::<SomeState>::with_capacity(sqrs_pn_eq.len());
        // Load pnc square states first.
        for sqrx in sqrs_pn_eq.iter() {
            if sqrx.pnc {
                stas_in.push(sqrx.state.clone());
            }
        }
        // Load non-pnc square states second.
        for sqrx in sqrs_pn_eq.iter() {
            if !sqrx.pnc {
                stas_in.push(sqrx.state.clone());
            }
        }

        Some((SomeRegion::new(stas_in), rules))
    } // end check_region_for_group

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
                    pnc: false,
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
        //  "action::groups_pair_intersection_needs: Dom {} Act {} {} {} and {} {}",
        //  self.dom_id, self.id, &grpx.region, grpx.pn, grpy.region, grpy.pn
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
            let far_reg = reg_int.far_from_reg(&ok_reg);

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
        //println!("action::cont_int_region_needs: Dom {} Act {} {} for grp {} and grp {}", self.dom_id, self.id, regx, grpx, grpy);
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
                    target: ATarget::Region {
                        region: SomeRegion::new(vec![sqrx.state.clone()]),
                    },
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
                    target: ATarget::Region {
                        region: regx.clone(),
                    },
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
    ///
    /// The within argument restricts where a rule should start, and restricts unwanted changes that may be included with wanted changes.
    pub fn get_steps(&self, wanted_changes: &SomeChange, within: &SomeRegion) -> StepStore {
        //println!("action::get_steps: Dom {} Act {} for wanted_changes {wanted_changes} within {within}", self.dom_id, self.id);
        debug_assert_eq!(wanted_changes.num_bits(), self.num_bits);
        debug_assert!(within.num_bits() == self.num_bits);

        debug_assert!(wanted_changes.m01.bitwise_and(&wanted_changes.m10).is_low()); // No X->x change wanted.

        let mut stps = StepStore::new(vec![]);

        for (inx, grpx) in self.groups.iter().enumerate() {
            // Skip no-change groups.
            if !grpx.causes_predictable_change() {
                continue;
            }

            if let Some(rules) = &grpx.rules {
                // Check if group rules cause at least one change that is needed.
                let mut skip = true;
                for rulx in rules.iter() {
                    if wanted_changes.intersection(rulx).is_not_low() {
                        skip = false;
                        break;
                    }
                }
                if skip {
                    //println!("action {} group {} no rules match wanted change", self.id, grpx.region);
                    continue;
                }

                // Process possible rule(s)
                let mut stpsx = self.get_steps_from_rulestore(rules, wanted_changes, within);
                if stpsx.is_empty() {
                    continue;
                }

                // Set group num for later need priority, lower number is higher priority.
                for stpx in stpsx.iter_mut() {
                    stpx.set_group_inx(inx);
                }
                stps.append(stpsx);
            }
        }
        //println!("action: steps within");
        stps
    } // end get_steps

    /// Get steps for a desired change, from a RuleStore that contains one or two rules.
    fn get_steps_from_rulestore(
        &self,
        rules: &RuleStore,
        wanted_changes: &SomeChange,
        within: &SomeRegion,
    ) -> StepStore {
        //println!("action::get_steps_from_rulestore: Dom {} Act {} rules {rules} rule_to_goal {rule_to_goal} within {within}", self.dom_id, self.id);
        debug_assert!(rules.is_not_empty());
        debug_assert!(wanted_changes.is_not_low());

        let mut stps = StepStore::new(vec![]);

        if rules[0].initial_region().intersects(within) {
        } else {
            return stps;
        }

        // Create a temporary rule vector.
        // Rule order is preserved.
        let rules2 = rules.within(within);

        // Process one-rule RuleStore.
        if rules2.len() == 1 {
            if let Some(rulx) = &rules2[0] {
                if wanted_changes.intersection(rulx).is_not_low() {
                    if let Some(rulx) = rulx.restrict_for_changes(wanted_changes) {
                        stps.push(SomeStep::new(self.id, rulx, AltRuleHint::NoAlt {}));
                    }
                }
            }

            //println!("action::get_steps_from_rulestore: returning {stps}");
            return stps;
        }

        // Must be a two-rule store, unfortunately.

        // Return if no rule within.
        if rules2[0].is_none() && rules2[1].is_none() {
            return stps;
        }

        // Split up group region between predictable result, that is, existing squares, and
        // unpredictable regions.
        //
        // A square result is predictable because you know the previous result.
        let rule_initial_reg = rules[0].initial_region().intersection(within).expect("SNH");

        let mut regs = RegionStore::new(vec![rule_initial_reg.clone()]);

        let mut sqrs_in: Vec<&SomeSquare> = self.squares.squares_in_reg(&rule_initial_reg);
        let mut memory_sqrs_in = self.squares.memory_squares_in_reg(&rule_initial_reg);
        sqrs_in.append(&mut memory_sqrs_in);

        //println!("{} squares in {rule_initial_reg}", sqrs_in.len());

        for sqrx in sqrs_in.iter() {
            // Subtract result-predictable square from rule initial region.
            regs = regs.subtract_region(&sqrx.state);

            // Add single square step.

            // The square may have only a single-sample, compatible with one of the group's rules.

            let last_result = sqrx.most_recent_result();
            //println!("sqr {} most recent result {last_result}", sqrx.state);

            if let Some(rulx) = &rules2[0] {
                if rulx.result_region().is_superset_of(last_result) { // next result should follow the alternate rule.
                } else {
                    let rul_sqr = rulx.restrict_initial_region(&sqrx.state);
                    if wanted_changes.intersection(&rul_sqr).is_not_low() {
                        stps.push(SomeStep::new(self.id, rul_sqr, AltRuleHint::NoAlt {}));
                    }
                }
            }

            if let Some(rulx) = &rules2[1] {
                if rulx.result_region().is_superset_of(last_result) { // next result should follow the alternate rule.
                } else {
                    let rul_sqr = rulx.restrict_initial_region(&sqrx.state);
                    if wanted_changes.intersection(&rul_sqr).is_not_low() {
                        stps.push(SomeStep::new(self.id, rul_sqr, AltRuleHint::NoAlt {}));
                    }
                }
            }
        } // next sqrx

        // println!("regs left {regs}");
        // Process regions not predicted from squares.
        for regx in regs.iter() {
            if let Some(rulx) = &rules2[0] {
                let ruly = rulx.restrict_initial_region(regx);
                if wanted_changes.intersection(&ruly).is_not_low() {
                    if let Some(rulz) = ruly.restrict_for_changes(wanted_changes) {
                        stps.push(SomeStep::new(
                            self.id,
                            rulz,
                            AltRuleHint::AltRule {
                                rule: rules[1].clone(),
                            },
                        ));
                    }
                }
            }

            if let Some(rulx) = &rules2[1] {
                let ruly = rulx.restrict_initial_region(regx);
                if wanted_changes.intersection(&ruly).is_not_low() {
                    if let Some(rulz) = ruly.restrict_for_changes(wanted_changes) {
                        stps.push(SomeStep::new(
                            self.id,
                            rulz,
                            AltRuleHint::AltRule {
                                rule: rules[0].clone(),
                            },
                        ));
                    }
                }
            }
        } // next regx

        //println!("action::get_steps_from_rulestore: returning {stps}");
        stps
    } // end get_steps_from_rulestore

    /// Find groups that can be formed by a given square, and other similar squares.
    /// Squares that are incompatible limit the possible groups.
    /// Compatible squares, can be mutually incompatible. 0->1 and 0->0 are incompatible with each other,
    /// but compatible with 1->1.
    fn possible_groups_from_square(&self, sqrx: &SomeSquare) -> GroupStore {
        //println!("action::possible_groups_from_square: Dom {} Act {} possible_groups_from_square: {}", self.dom_id, self.id, sqrx.state);
        debug_assert_eq!(sqrx.num_bits(), self.num_bits);

        let mut ret_grps = Vec::<SomeGroup>::new();

        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return GroupStore::new(ret_grps);
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
                sqrx.pnc,
            ));
        }
        GroupStore::new(ret_grps)
    } // end possible_regions_from_square

    /// Validate a region that may be made from a given square, in combination with similar squares.
    fn validate_possible_group(&self, sqrx: &SomeSquare, regx: &SomeRegion) -> Option<SomeGroup> {
        //println!("action::validate_possible_group: Dom {} Act {} state {} reg {}", self.dom_id, self.id, sqrx.state, regx);
        debug_assert_eq!(sqrx.num_bits(), self.num_bits);
        debug_assert_eq!(regx.num_bits(), self.num_bits);
        debug_assert!(regx.is_superset_of(sqrx));

        if let Some((regy, rules)) = self.check_region_for_group(regx, sqrx.pn) {
            if regy.is_superset_of(&sqrx.state) {
                Some(SomeGroup::new(regy, rules, false))
            } else {
                None
            }
        } else {
            None
        }
    } // end validate_combination

    /// Take an action for a need.
    pub fn take_action_need(&mut self, cur_state: &SomeState, ndx: &SomeNeed) -> SomeSample {
        //println!("action::take_action_need: Dom {} Act {} cur_state {cur_state} need {ndx}", self.dom_id, self.id);
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
                            if sqr1.pnc && grpx.set_pnc() {
                                println!(
                                    "Dom {} Act {} Group {} confirmed",
                                    self.dom_id, self.id, grpx.region
                                );
                            }
                        } else if let Some(sqr2) = self.squares.find(&grp_reg.far_state()) {
                            if sqr1.pnc && sqr2.pnc && grpx.set_pnc() {
                                println!(
                                    "Dom {} Act {} Group {} confirmed",
                                    self.dom_id, self.id, grpx.region
                                );
                            }
                        }
                    }
                }
            }
            SomeNeed::StateInRemainder { .. } => {
                if !self.groups.any_superset_of(cur_state) {
                    let sqr1 = self.squares.find(cur_state).expect("SNH");
                    if sqr1.pnc {
                        self.check_remainder = true;
                        self.groups_push_nosubs(SomeGroup::new(
                            SomeRegion::new(vec![cur_state.clone()]),
                            sqr1.rules.clone(),
                            sqr1.pnc,
                        ));
                    }
                }
            }
            _ => {}
        }

        asample
    }

    /// Take an action with the current state, add the sample to squarestore.
    /// Return a sample.
    pub fn take_action_arbitrary(&mut self, cur_state: &SomeState) -> SomeSample {
        //println!("action::take_action_arbitrary: Dom {} Act {} cur_state {cur_state}", self.dom_id, self.id);
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
        //println!("action::take_action_step: Dom {} Act {} cur_state {cur_state}", self.dom_id, self.id);
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);

        let astate = self
            .do_something
            .take_action(cur_state, self.dom_id, self.id);

        let asample = SomeSample::new(cur_state.clone(), astate);

        self.eval_sample(&asample);

        asample
    }

    /// Return a change with all changes that can be made for the action.
    pub fn aggregate_changes(&mut self) -> Option<&SomeChange> {
        //println!("SomeAction::aggregate_changes");
        if self.agg_chgs_updated {
        } else {
            self.aggregate_changes = self.groups.calc_aggregate_changes();
            self.agg_chgs_updated = true;
        }
        if let Some(changes) = &self.aggregate_changes {
            Some(changes)
        } else {
            None
        }
    }

    /// Reset to GroupStore agg_chgs_updated flag, after the incorporation
    /// of its aggregate changes into the parent ActionStore struct.
    pub fn reset_agg_chgs_updated(&mut self) {
        self.agg_chgs_updated = false;
    }

    /// Display anchor rates, like (number adjacent anchors, number other adjacent squares only in one region, samples)
    pub fn display_anchor_info(&self) -> Result<(), String> {
        //println!("action::display_anchor_info: Dom {} Act {} group anchor rates", self.dom_id, self.id);
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

    /// Return the number of groups defined in the action.
    pub fn number_groups_defined(&self) -> usize {
        self.do_something.len()
    }

    /// Return a String representation of a SomeAction state.
    fn formatted_state(&self) -> String {
        let mut rc_str = String::from("ACT(ID: ");

        rc_str += &self.id.to_string();

        rc_str += ", number squares: ";
        rc_str += &self.squares.len().to_string();

        //rc_str.push_str(&format!(", agg_chgs_updated {}", self.agg_chgs_updated));

        if self.remainder_check_regions.is_not_empty() {
            rc_str.push_str(&format!(", remainder: {}", self.remainder_check_regions));
        }

        let mut fil = "\n       Grps: ";
        for grpx in self.groups.iter() {
            let stas_in = self.squares.stas_in_reg(&grpx.region);

            let cnt: usize = stas_in
                .iter()
                .map(|stax| usize::from(self.groups.num_groups_in(stax) == 1))
                .sum();

            rc_str.push_str(&format!(
                "{}{} num Sqrs: {} Sqrs in: {} in1: {})",
                fil,
                grpx,
                grpx.region.len(),
                stas_in.len(),
                cnt,
            ));

            fil = ",\n             ";
        }

        rc_str.push(')');
        rc_str
    }

    /// Return a from_str compatible string for a SomeAction instance.
    #[allow(dead_code)]
    pub fn formatted_def(&self) -> String {
        let mut rc_str = String::from("ACT[");
        let mut first = true;
        for rulstrx in self.do_something.rules.iter() {
            if first {
                first = false;
            } else {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{rulstrx}"));
        }
        rc_str.push(']');

        rc_str
    }

    /// Check groups with a recently changed sqaure.
    /// Return the references to groups that are inactivated by a square.
    pub fn groups_check_square(&mut self, key: &SomeState) -> RegionStore {
        //println!("action::check_square: Dom {} Act {} key {key}", self.dom_id, self.id);

        let mut regs_invalid = RegionStore::new(vec![]);

        let sqrx = self.squares.find(key).expect("SNH");

        for grpx in self.groups.iter_mut() {
            if !grpx.check_square(sqrx) {
                if sqrx.pn > grpx.pn {
                    println!(
                        "\nDom {} Act {} square {} pn: {} invalidates\n             group {} pn: {}",
                        self.dom_id, self.id, sqrx.state, sqrx.pn , grpx.region, grpx.pn
                    );
                } else if sqrx.pn < grpx.pn && sqrx.pnc {
                    println!(
                        "\nDom {} Act {} square {} pn: {} pnc: true invalidates\n             group {} pn: {}",
                        self.dom_id, self.id, sqrx.state, sqrx.pn , grpx.region, grpx.pn
                    );
                } else {
                    println!(
                        "\nDom {} Act {} square {} {} invalidates\n             group {} {}",
                        self.dom_id,
                        self.id,
                        sqrx.state,
                        if let Some(rules) = &sqrx.rules {
                            rules.to_string()
                        } else {
                            String::from("None")
                        },
                        grpx.region,
                        if let Some(rules) = &grpx.rules {
                            rules.to_string()
                        } else {
                            String::from("None")
                        },
                    );
                }

                regs_invalid.push(grpx.region.clone());
            }
        } // next grpx

        // Check limited status of groups.
        for grpx in self.groups.iter_mut() {
            if !grpx.limited || !grpx.region.is_adjacent(&sqrx.state) {
                continue;
            }
            if let Some(anchor) = &grpx.anchor {
                if anchor.is_adjacent(&sqrx.state) {
                    if !sqrx.pnc || (grpx.pn == Pn::Unpredictable && sqrx.pn == Pn::Unpredictable) {
                        if grpx.set_limited_off() {
                            println!(
                                "Dom {} Act {} Group {} set limited off",
                                self.dom_id, self.id, grpx.region
                            );
                        }
                    } else if grpx.pn == sqrx.pn {
                        if let Some(grpx_ruls) = &grpx.rules {
                            if let Some(sqr_ruls) = &sqrx.rules {
                                if grpx_ruls.union(sqr_ruls).is_some() && grpx.set_limited_off() {
                                    println!(
                                        "Dom {} Act {} Group {} set limited off",
                                        self.dom_id, self.id, grpx.region
                                    );
                                }
                            }
                        }
                    }
                }
            }
        } // next grpx

        //println!("GroupStore::check_square: {} groups removed", regs_invalid.len());
        regs_invalid
    }

    /// Add a group to the end of the list.
    /// So older, longer surviving, groups are first in the list.
    pub fn groups_push_nosubs(&mut self, grp: SomeGroup) -> bool {
        // Check for supersets, which probably is an error
        if self.groups.any_superset_of(&grp.region) {
            let regs = self.groups.supersets_of(&grp.region);
            println!(
                "Dom {} Act {} skipped adding group {}, a superset exists in {regs}",
                self.dom_id, self.id, grp.region,
            );
            return false;
        }

        // Remove subset groups
        self.groups_remove_subsets_of(&grp.region);

        // Add the new group
        self.add_group(grp);

        true
    }

    /// Find and remove any groups that are a subset of a given region.
    pub fn groups_remove_subsets_of(&mut self, reg: &SomeRegion) -> bool {
        // Accumulate indices of groups that are subsets
        let mut rmvec = Vec::<SomeRegion>::new();

        for grpx in &mut self.groups.iter() {
            if grpx.is_subset_of(reg) {
                rmvec.push(grpx.region.clone());
            }
        }

        // Remove the groups
        for regx in rmvec.iter() {
            println!(
                "\nDom {} Act {} Group {} is a subset of {reg}",
                self.dom_id, self.id, regx
            );
            self.remove_group(regx);
        }

        !rmvec.is_empty()
    }

    // Add a group.
    // This is always used, instead of self.groups.push, to insure
    // that self.agg_chgs_updated is changed.
    pub fn add_group(&mut self, grpx: SomeGroup) {
        if grpx.region.states.len() > 1 {
            println!(
                "\nDom {} Act {} Adding Group {} from {}",
                self.dom_id, self.id, grpx, grpx.region.states,
            );
        } else {
            println!(
                "\nDom {} Act {} Adding Group {}",
                self.dom_id, self.id, grpx
            );
        }
        self.groups.push(grpx);
        self.agg_chgs_updated = false;
    }

    // Remove a group.
    // This is always used, instead of self.groups.remove_group, to insure
    // that self.agg_chgs_updated is changed.
    pub fn remove_group(&mut self, grp_reg: &SomeRegion) -> bool {
        if self.groups.remove_group(grp_reg) {
            self.agg_chgs_updated = false;
            println!(
                "\nDom {} Act {} Group {} deleted",
                self.dom_id, self.id, grp_reg
            );
            true
        } else {
            false
        }
    }

    // Set the cleanup trigger value.
    pub fn set_cleanup(&mut self, trigger: usize) {
        assert!(trigger > 0);
        self.cleanup_trigger = trigger;
    }
} // end impl SomeAction

impl FromStr for SomeAction {
    type Err = String;
    /// Return a SomeAction instance, given a string representation.
    ///
    /// Like ACT[[rulestores, affecting different regions], [RuleStores], state-to-sample[/number samples, default = 1], state-to-sample, ...]
    ///
    ///      ----- Rules for 0XXX -----  -- For 10XX -  -- For 11XX -  ----- Take samples of states ----
    /// ACT[ [00/XX/XX/XX, 01/XX/XX/XX], [11/01/XX/XX], [10/11/Xx/XX], s1000, s0000/3, s0101 ]
    ///
    /// All the rules must use the same number of bits.
    /// There must be at least one, non-empty, rulestore.
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("SomeAction::from_str: {str_in}");
        let src_str = str_in.trim();

        if src_str.is_empty() {
            return Err("SomeAction::from_str: Empty string?".to_string());
        }

        // Unwrap "ACT[...]". Check that the brackets are balanced.
        let mut src_str2 = String::new();
        let mut left = 0;
        let mut right = 0;

        for (inx, chr) in src_str.graphemes(true).enumerate() {
            if chr == "\n" {
                continue;
            }
            if inx == 0 {
                if chr != "A" {
                    return Err(
                        "SomeAction::from_str: Invalid string, should start with ACT[".to_string(),
                    );
                }
                continue;
            }
            if inx == 1 {
                if chr != "C" {
                    return Err(
                        "SomeAction::from_str: Invalid string, should start with ACT[".to_string(),
                    );
                }
                continue;
            }
            if inx == 2 {
                if chr != "T" {
                    return Err(
                        "SomeAction::from_str: Invalid string, should start with ACT[".to_string(),
                    );
                }
                continue;
            }
            if inx == 3 {
                if chr != "[" {
                    return Err(
                        "SomeAction::from_str: Invalid string, should start with ACT[".to_string(),
                    );
                }
                left += 1;
                continue;
            }
            if chr == "[" {
                left += 1;
            }
            if chr == "]" {
                right += 1;
                if right > left {
                    return Err("SomeAction::from_str: Brackets not balanced.".to_string());
                }
            }

            src_str2.push_str(chr);
        }
        if left != right {
            return Err("SomeAction::from_str: Brackets not balanced.".to_string());
        }

        // Remove last right-bracket, balancing first left bracket.
        src_str2.remove(src_str2.len() - 1);

        // Split substring into tokens.
        let mut token = String::new();
        let mut token_list = Vec::<String>::new();
        left = 0;
        right = 0;

        for chr in src_str2.graphemes(true) {
            if left == right && (chr == "," || chr == " ") {
                if left == right && !token.is_empty() {
                    token_list.push(token);
                    token = String::new();
                }
                continue;
            }

            token.push_str(chr);

            if chr == "[" {
                left += 1;
            }
            if chr == "]" {
                right += 1;
                if right > left {
                    return Err("SomeAction::from_str: Brackets not balanced.".to_string());
                }
            }
            if left == right && left > 0 {
                token_list.push(token);
                token = String::new();
                left = 0;
                right = 0;
            }
        }
        if !token.is_empty() {
            token_list.push(token);
        }
        //println!("token_list {:?}", token_list);

        let mut rs_vec = Vec::<RuleStore>::new();

        // Generate vector of RuleStores for action.
        for tokenx in token_list.iter() {
            //println!("rulestores for an action: {tokenx}");
            if tokenx[0..1] == *"[" {
                match RuleStore::from_str(tokenx) {
                    Ok(rulstrx) => {
                        if rulstrx.is_empty() {
                            return Err("SomeAction::from_str: Empty RuleStore.".to_string());
                        }
                        rs_vec.push(rulstrx);
                    }
                    Err(errstr) => return Err(errstr),
                }
            } else if tokenx[0..1] == *"s" {
                continue;
            } else {
                return Err(format!(
                    "SomeAction::from_str: Unrecognized token, {tokenx}"
                ));
            }
        }

        if rs_vec.is_empty() {
            return Err("SomeAction::from_str: No RuleStore.".to_string());
        }
        // Init the action.
        let mut actx = SomeAction::new(rs_vec);

        // Generate samples for each.
        for tokenx in token_list.iter() {
            //println!("rulestores for an action: {tokenx}");
            if tokenx[0..1] == *"[" {
                continue;
            } else if tokenx[0..1] == *"s" {
                let mut num_str = "1".to_string();
                let mut tokeny = tokenx.clone();
                // Split state token by "/" separator, if any.
                if let Some(inx) = tokenx.find('/') {
                    num_str = tokenx[(inx + 1)..].to_string();
                    tokeny = tokenx[0..inx].to_string();
                }
                //println!("tokeny {tokeny} num_str {num_str}");

                match SomeState::from_str(&tokeny) {
                    Ok(stax) => match num_str.parse::<usize>() {
                        Ok(num) => {
                            //println!("num times = {num}");
                            if num > 0 {
                                for _ in 0..num {
                                    actx.take_action_arbitrary(&stax);
                                }
                            } else {
                                return Err(format!("Did not understand count in token {tokenx}"));
                            }
                        }
                        Err(errstr) => return Err(errstr.to_string()),
                    },
                    Err(errstr) => return Err(errstr),
                }
            } else {
                return Err(format!(
                    "SomeAction::from_str: Unrecognized token, {tokenx}"
                ));
            }
        }

        Ok(actx)
    }
}

// Some action tests are made from the domain level.
#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

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
        // Put in two one-result squares, s0001 and s0101, both subset of the later two-result squares.
        let act0 = SomeAction::from_str(
            "ACT[[00/XX/XX/XX, 01/XX/XX/XX], [11/XX/XX/xx], s0001, s0101, s0000/4, s0111/4]",
        )?;

        println!("{act0}");

        assert!(act0.groups.len() == 1);
        if let Some(_) = act0.groups.find(&SomeRegion::from_str("r0XXX")?) {
        } else {
            return Err("Group 0XXX not found?".to_string());
        }
        //assert!(1 == 2);
        Ok(())
    }

    // Test making groups from a few single-sample states.
    #[test]
    fn groups_formed_1() -> Result<(), String> {
        // Init action
        let act0 = SomeAction::from_str(
            "ACT[[XX/00/XX/X0], [11/11/XX/Xx], [00/11/XX/XX], s0111, s1011, s1101, s0001]",
        )?;

        println!("{act0}");

        assert!(act0.groups.len() == 4);
        assert!(act0.groups.find(&SomeRegion::from_str("r0111")?).is_some());
        assert!(act0.groups.find(&SomeRegion::from_str("r1xx1")?).is_some());
        assert!(act0.groups.find(&SomeRegion::from_str("rxx01")?).is_some());
        assert!(act0.groups.find(&SomeRegion::from_str("rx0x1")?).is_some());

        Ok(())
    }

    // Test making a group from two Pn::Two squares.
    #[test]
    fn possible_region() -> Result<(), String> {
        // Init Action.
        let mut act0 = SomeAction::from_str("ACT[[XX/XX/XX/XX, XX/XX/XX/Xx], s1111/4, s0001/4]")?;

        let s1 = SomeState::from_str("s0001")?;
        let max_reg = SomeRegion::from_str("rXXXX")?;

        let nds = act0.get_needs(&s1, &max_reg);
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
        let act0 = SomeAction::from_str("ACT[[XX/XX/XX/XX], s0000, s0011, s0101]")?;

        println!("Act: {}", act0);

        assert!(act0.groups.len() == 1);
        assert!(act0.groups.find(&SomeRegion::from_str("r0xxx")?).is_some());

        Ok(())
    }

    // Test making a region from three samples, with one dissimilar sample.
    #[test]
    fn three_sample_region2() -> Result<(), String> {
        // Init action.
        let act0 = SomeAction::from_str(
            "ACT[[XX/00/XX/XX], [XX/XX/XX/11], [XX/10/XX/00], s0000, s0011, s0100, s0101]",
        )?;

        println!("Act: {}", act0);

        assert!(act0.groups.len() == 3);
        assert!(act0.groups.find(&SomeRegion::from_str("r0x00")?).is_some());
        assert!(act0.groups.find(&SomeRegion::from_str("r00xx")?).is_some());
        assert!(act0.groups.find(&SomeRegion::from_str("r0xx1")?).is_some());

        Ok(())
    }

    // Test making a region from three samples, with two similar samples that cannot be combined,
    // due to the second bits being 1->0 and 1->1.
    #[test]
    fn three_sample_region3() -> Result<(), String> {
        // Init action.
        let act0 = SomeAction::from_str("ACT[[00/XX/X0/XX], [XX/XX/XX/11], s0010, s1011, s0101]")?;

        println!("Act: {}", act0);

        assert!(act0.groups.len() == 2);
        assert!(act0.groups.find(&SomeRegion::from_str("r0xxx")?).is_some());
        assert!(act0.groups.find(&SomeRegion::from_str("rxxx1")?).is_some());

        Ok(())
    }

    #[test]
    fn aggregate_changes() -> Result<(), String> {
        // Init action.
        let mut act0 = SomeAction::from_str("ACT[[00/00/10/00], [01/11/00/X0]]")?;

        // Set up square 2.
        act0.take_action_arbitrary(&SomeState::from_str("s0010")?);

        assert!(act0.groups.len() == 1);
        // Group is not yet pnc, so no aggregate changes.
        assert!(act0.aggregate_changes.is_none());

        // Confirm group 2.
        act0.take_action_arbitrary(&SomeState::from_str("s0010")?);

        if let Some(cngs) = act0.aggregate_changes() {
            println!("agg cncs (1) {cngs}");
        } else {
            println!("agg cncs (1) None");
        }
        assert!(act0.aggregate_changes() == Some(&SomeChange::from_str("../../10/..")?));

        act0.take_action_arbitrary(&SomeState::from_str("s0010")?);

        if let Some(change2) = act0.aggregate_changes() {
            println!("change2 {change2}");
            assert!(*change2 == SomeChange::from_str("../../10/..")?);
        } else {
            return Err("Test 2 failed".to_string());
        }

        // Make group 45, pnc.
        act0.take_action_arbitrary(&SomeState::from_str("s0100")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0100")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0100")?);

        act0.take_action_arbitrary(&SomeState::from_str("s0101")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0101")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0101")?);

        assert!(act0.groups.len() == 2);

        if let Some(change3) = act0.aggregate_changes() {
            println!("change3 {change3}");
            assert!(*change3 == SomeChange::from_str("01/../10/10")?);
        } else {
            return Err("Test 3 failed".to_string());
        }

        // Make square 5 unpredictable.
        act0.eval_sample_arbitrary(&SomeSample::from_str("s0101->s1111")?);

        assert!(act0.groups.len() == 3); // groups 0010, 0100, 0101 (unpredictable).

        if let Some(change4) = act0.aggregate_changes() {
            println!("change4 {change4}");
            assert!(*change4 == SomeChange::from_str("01/../10/..")?);
        } else {
            return Err("Test 4 failed".to_string());
        }

        Ok(())
    }

    #[test]
    /// Test action definition from string to instance, then instance to string(2), then string(2) to instance.
    fn from_str() -> Result<(), String> {
        let actx_str = "ACT[[XX_10/XX/XX/XX], [Xx_00/XX/XX/XX]]";
        println!("actx_str {actx_str}");

        let actx = SomeAction::from_str(&actx_str)?; // String to instance.

        let actx_str2 = actx.formatted_def(); // Instance to string(2).
        println!("actxstr2 {actx_str2}",);

        match SomeAction::from_str(&actx_str2) {
            // String(2) to instance.
            Ok(acty) => {
                assert!(acty == actx);
                Ok(())
            }
            Err(errstr) => Err(errstr),
        }
    }
}
