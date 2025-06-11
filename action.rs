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

use crate::bits::{Bitint, SomeBits};
use crate::change::SomeChange;
use crate::group::SomeGroup;
use crate::groupstore::GroupStore;
use crate::mask::SomeMask;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::square::{Compatibility, SomeSquare};
use crate::squarestore::{PickError, SquareStore};
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::step::SomeStep;
use crate::stepstore::StepStore;
use crate::target::ATarget;
use crate::tools::{self, AccessStates, StrLen};

use rand::prelude::*;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

const CLEANUP_TRIGGER: usize = 5;

impl fmt::Display for SomeAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_state())
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
    /// Number of new squares since last cleanup.
    cleanup_number_new_squares: usize,
    /// Group regions recently marked as limited.
    limited: RegionStore,
    /// Initial rules given to generate samples.
    base_rules: Vec<RuleStore>,
    /// Store for calculated structure regions.
    structure_regions: RegionStore,
    /// Store for defining regions.
    defining_regions: RegionStore,
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

        // Check no empty rulestores.
        for rulsx in rules.iter() {
            assert!(rulsx.is_not_empty());
        }
        let num_bits = rules[0].num_bits().expect("SNH");

        // Check num_bits of rules.
        for rulsx in rules.iter().skip(1) {
            assert!(rulsx.num_bits().unwrap() == num_bits);
        }

        // Check that rules within each rulestore have the same initial region.
        for rulsx in rules.iter().skip(1) {
            for rulx in rulsx.iter() {
                assert!(rulx.initial_region() == rulsx[0].initial_region());
            }
        }
        // Check intersections, if any, for validity.
        for inx in 0..(rules.len() - 1) {
            for iny in (inx + 1)..rules.len() {
                if rules[inx]
                    .initial_region()
                    .intersects(&rules[iny].initial_region())
                {
                    if rules[inx].len() != rules[iny].len() {
                        panic!("Rulestores of different lengths intersect");
                    }
                    if rules[inx].len() < 3 {
                        assert!(rules[inx].intersection(&rules[iny]).is_some());
                    }
                }
            }
        }

        let sta0 = SomeState::new(SomeBits::new(num_bits as Bitint));

        SomeAction {
            id: 0,     // Caller changes, if needed.
            dom_id: 0, // Caller changes, if needed.
            num_bits,
            groups: GroupStore::new(vec![]),
            squares: SquareStore::new(HashMap::new(), num_bits),
            cleanup_number_new_squares: 0,
            limited: RegionStore::new(vec![]),
            base_rules: rules,
            structure_regions: RegionStore::new(vec![SomeRegion::new(vec![
                sta0.new_high(),
                sta0.clone(),
            ])]),
            defining_regions: RegionStore::new(vec![]),
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
            //println!(
            //    "\nDom {} Act {} Group {} is invalidated, removed.",
            //    self.dom_id, self.id, regx
            //);
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

        // Collect possible groups.
        let groups: Vec<GroupStore> = keys
            .par_iter() // par_iter for parallel processing, iter for sequential diagnostic messages.
            .map(|keyx| self.create_groups_from_squares2(keyx))
            .collect::<Vec<GroupStore>>();

        // Store possible groups, some may be duplicates.
        for grpstrx in groups {
            for grpx in grpstrx {
                //if !self.groups.any_superset_of(&grpx.region) {
                if self.groups.any_superset_of(&grpx.region) {
                    //println!(
                    //    "Dom {} Act {} skipped adding group (3) {}",
                    //    self.dom_id, self.id, grpx.region
                    //);
                } else {
                    self.add_group(grpx);
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
    pub fn state_not_in_group_needs(&mut self, cur_state: &SomeState) -> NeedStore {
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);

        let mut nds = NeedStore::new(vec![]);

        // Check if current state is in any groups
        if !self.groups.any_superset_of(cur_state) {
            if let Some(sqrx) = self.squares.find(cur_state) {
                if sqrx.pn == Pn::One || sqrx.pnc {
                    println!(
                        "Problem: Dom {} Act {} square {} not in group?",
                        self.dom_id, self.id, sqrx
                    );
                    self.create_groups_from_squares(&[sqrx.state.clone()]);
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

        // Check if any groups became limited.
        // If so, check intersecting groups with no anchor, for deletion.
        if self.limited.is_not_empty() {
            //println!("Recently limited groups: {}", self.limited);

            // Find non-equal intersecting groups, with no anchor.
            let mut grps_to_check = RegionStore::new(vec![]);
            for grpx in self.groups.iter() {
                if grpx.anchor.is_some() {
                    continue;
                }
                for grp_l in self.limited.iter() {
                    if grpx.region.intersects(grp_l) {
                        if grps_to_check.contains(&grpx.region) {
                        } else {
                            grps_to_check.push(grpx.region.clone());
                        }
                    }
                }
            }
            //println!("grps_to_check: {grps_to_check}");
            // Subtract limited groups from non-anchor groups, delete if nothing left.
            let mut grps_to_remove = RegionStore::new(vec![]);
            for grpx_reg in grps_to_check.iter() {
                let mut left = RegionStore::new(vec![grpx_reg.clone()]);
                for grp_a in self.groups.iter() {
                    if grp_a.region != *grpx_reg
                        && grp_a.limited
                        && left.any_intersection_of(&grp_a.region)
                    {
                        left = left.subtract_region(&grp_a.region);
                    }
                }
                //println!("group to check: {grpx}, whats left: {left}");
                if left.is_empty() {
                    grps_to_remove.push_nosubs(grpx_reg.clone());
                }
            }
            for grpx in grps_to_remove.iter() {
                println!(
                    "\nDom {} Act {} Group {} is completely overlapped by limited groups, removed.",
                    self.dom_id, self.id, grpx
                );
                self.remove_group(grpx);
            }
            self.limited = RegionStore::new(vec![]);
        }

        let mut get_new_needs = true;
        while get_new_needs {
            get_new_needs = false;

            // Get some needs.
            ret = self.get_needs2(cur_state, max_reg);

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
        if self.cleanup_number_new_squares >= CLEANUP_TRIGGER {
            self.cleanup(&ret);
        }

        ret
    }

    /// Get needs, process any housekeeping needs.
    pub fn get_needs2(&mut self, cur_state: &SomeState, max_reg: &SomeRegion) -> NeedStore {
        //println!("action::get_needs2: Dom {} Act {} cur_state {cur_state} max_reg {max_reg}", self.dom_id, self.id);
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);
        debug_assert_eq!(max_reg.num_bits(), self.num_bits);

        let pair_nds = self.incompatible_pair_needs(max_reg);
        if pair_nds.is_empty() {
            // Delete regions that do not fit.
            let mut del_regs = RegionStore::new(vec![]);
            for grpx in self.groups.iter() {
                if !self.structure_regions.any_superset_of(&grpx.region) {
                    del_regs.push(grpx.region.clone());
                }
            }
            if del_regs.is_not_empty() {
                println!(
                    "Dom {} Act {} processing invalid regs {del_regs} due to defining regions {}",
                    self.dom_id, self.id, self.structure_regions
                );
                self.process_invalid_regions(&del_regs);
            }
        }

        let mut nds = NeedStore::new(vec![]);

        // Look for needs for states not in groups, may add a one-state group.
        nds.append(self.state_not_in_group_needs(cur_state));

        // Check for additional samples for group states needs
        nds.append(self.confirm_group_needs());

        // Check any two groups for overlapping groups that form a contradictory intersection.
        nds.append(self.group_pair_needs());

        // Check for group limiting needs
        if let Some(ndx) = self.limit_groups_needs(max_reg) {
            nds.append(ndx);
        }

        nds.append(pair_nds);

        nds
    } // end get_needs

    /// Return needs for incompatible square pairs.
    /// Inchoate perceptions, translated into code, the goal of the project.
    fn incompatible_pair_needs(&mut self, max_reg: &SomeRegion) -> NeedStore {
        // Init NeedStore to return.
        let mut nds = NeedStore::new(vec![]);

        let structure_regions = RegionStore::new(vec![max_reg.clone()]);

        // Check each possible square pair.
        let sqrs: Vec<&SomeSquare> = self.squares.all_squares();
        if sqrs.len() < 2 {
            self.structure_regions = structure_regions;
            return nds;
        }

        // Init storage for incompatible pairs, stored as two states in a region.
        let mut incompat_regions = RegionStore::new(vec![]);

        // Check each pair for incompatibility, save pairs, no supersets.
        for inx in 0..(sqrs.len() - 1) {
            if !sqrs[inx].pnc {
                continue;
            }
            for iny in (inx + 1)..sqrs.len() {
                if !sqrs[iny].pnc {
                    continue;
                }
                if sqrs[inx].compatible(sqrs[iny]) == Compatibility::NotCompatible {
                    incompat_regions.push_nosups(SomeRegion::new(vec![
                        sqrs[inx].state.clone(),
                        sqrs[iny].state.clone(),
                    ]));
                }
            }
        }
        // Check for none found.
        if incompat_regions.is_empty() {
            self.structure_regions = structure_regions;
            return nds;
        }

        // Split regions based on adjacency.
        let mut adjacent_pairs = RegionStore::new(vec![]);
        let mut non_adjacent_pairs = RegionStore::new(vec![]);

        for regx in incompat_regions {
            if regx.first_state().is_adjacent(regx.last_state()) {
                adjacent_pairs.push(regx);
            } else {
                non_adjacent_pairs.push(regx);
            }
        }
        //println!("adjacent_pairs:     {adjacent_pairs}");
        //println!("non_adjacent_pairs: {non_adjacent_pairs}");

        // Calc possible regions.
        let mut poss_regions = RegionStore::new(vec![max_reg.clone()]);
        for regx in adjacent_pairs.iter() {
            //println!("Applying {regx} to {poss_regions}");
            let regs1 = max_reg.subtract(regx.first_state());
            let regs2 = max_reg.subtract(regx.last_state());
            poss_regions = poss_regions.intersection(&regs1.union(&regs2));
        }
        //println!("poss_regions: {poss_regions}");

        // Filter non-adjacent pairs, that are only in one region of poss_regions.
        let mut temp = RegionStore::new(vec![]);
        for regx in non_adjacent_pairs.iter() {
            if poss_regions.in_one_region(regx) {
                temp.push(regx.clone());
            }
        }
        non_adjacent_pairs = temp;

        // Apply non-adjacent pairs to poss_regions.
        for regx in non_adjacent_pairs.iter() {
            //println!("Applying {regx} to {poss_regions}");
            let regs1 = max_reg.subtract(regx.first_state());
            let regs2 = max_reg.subtract(regx.last_state());
            poss_regions = poss_regions.intersection(&regs1.union(&regs2));
        }

        // Check for adjacent pair confirm needs.
         for regx in adjacent_pairs.iter() {
             let Some(sqrx) = self.squares.find(regx.first_state()) else { panic!("SNH"); };
             if !sqrx.pnc {
                 // Construct need for sample of state between.
                let mut needx = SomeNeed::ConfirmIP {
                            dom_id: self.dom_id,
                            act_id: self.id,
                            target: ATarget::State {
                                state: sqrx.state.clone(),
                            },
                            other_state: regx.last_state().clone(),
                            priority: 0,
                        };
                needx.add_priority_base();
                nds.push(needx);
             }

             let Some(sqrx) = self.squares.find(regx.last_state()) else { panic!("SNH"); };
             if !sqrx.pnc {
                 // Construct need for sample of state between.
                let mut needx = SomeNeed::ConfirmIP {
                            dom_id: self.dom_id,
                            act_id: self.id,
                            target: ATarget::State {
                                state: sqrx.state.clone(),
                            },
                            other_state: regx.first_state().clone(),
                            priority: 0,
                        };
                needx.add_priority_base();
                nds.push(needx);
             }
         }

        // If there are any adjacent pair confirm needs, return them.
        if nds.is_not_empty() {
            return nds;
        }

        // Save defining regions to action instance.
        self.defining_regions = poss_regions.defining_regions();

        // Save structure regions in action instance.
        self.structure_regions = poss_regions;

        // Check for non-adjacent pair confirm needs.
         for regx in non_adjacent_pairs.iter() {
             let Some(sqrx) = self.squares.find(regx.first_state()) else { panic!("SNH"); };
             if !sqrx.pnc {
                 // Construct need for sample of state between.
                let mut needx = SomeNeed::ConfirmIP {
                            dom_id: self.dom_id,
                            act_id: self.id,
                            target: ATarget::State {
                                state: sqrx.state.clone(),
                            },
                            other_state: regx.last_state().clone(),
                            priority: 0,
                        };
                needx.add_priority_base();
                nds.push(needx);
             }

             let Some(sqrx) = self.squares.find(regx.last_state()) else { panic!("SNH"); };
             if !sqrx.pnc {
                 // Construct need for sample of state between.
                let mut needx = SomeNeed::ConfirmIP {
                            dom_id: self.dom_id,
                            act_id: self.id,
                            target: ATarget::State {
                                state: sqrx.state.clone(),
                            },
                            other_state: regx.first_state().clone(),
                            priority: 0,
                        };
                needx.add_priority_base();
                nds.push(needx);
             }
         }

        // If there are any non-adjacent pair confirm needs, return them.
        if nds.is_not_empty() {
            return nds;
        }

        // Check for non-adjacent pair between needs.
        for regx in non_adjacent_pairs.iter() {
            //println!("Processing non-adjacent Incompatible pair: {regx}");

            // Get squares represented by the states.
            let Some(sqrx) = self.squares.find(regx.first_state()) else { panic!("SNH"); };

            let Some(sqry) = self.squares.find(regx.last_state()) else {
                panic!("SNH");
            };

            let pri = regx.x_mask().num_one_bits(); // Make smaller regions the priority.

            let sqrs_in = self.squares.squares_in_reg(regx);
            //println!("sqrs_in: {}", tools::vec_ref_string(&sqrs_in));

            // If no squares other than sqrx, sqry, are in the region, look for something between.
            if sqrs_in.len() == 2 {
                let dif_msk = sqrx.state.diff_edge_mask(&sqry.state);
                let dif_num = dif_msk.num_one_bits();
                if dif_num > 3 {
                    // Look for states equidistant from the two squares.

                    // Get a store of single-bit masks.
                    let msk_bits = dif_msk.split();

                    // Form a vector of single-bit mask refs.
                    let mut msk_refs = Vec::<&SomeMask>::with_capacity(msk_bits.len());
                    for mskx in msk_bits.iter() {
                        msk_refs.push(mskx);
                    }
                    let options = tools::anyxofn(dif_num >> 1, &msk_refs);
                    for optx in options.iter() {
                        // Make a single mask from multiple single-bit masks.
                        let mut msk_tmp = optx[0].clone();
                        for msky in optx.iter().skip(1) {
                            msk_tmp = msk_tmp.bitwise_or(*msky);
                        }

                        // Construct need for sample of state between.
                        let mut needx = SomeNeed::CloserIP {
                            dom_id: self.dom_id,
                            act_id: self.id,
                            target: ATarget::State {
                                state: sqrx.state.bitwise_xor(&msk_tmp).as_state(),
                            },
                            unknown_region: regx.clone(),
                            priority: pri,
                        };
                        needx.add_priority_base();
                        nds.push(needx);
                    }
                } else {
                    // Look for any square inbetween.
                    let regs = regx.subtract(&sqrx.state).subtract_region(&sqry.state);
                    //println!("regs left = {regs}");
                    for regz in regs {
                        let mut needx = SomeNeed::CloserIP {
                            dom_id: self.dom_id,
                            act_id: self.id,
                            target: ATarget::Region { region: regz },
                            unknown_region: regx.clone(),
                            priority: pri,
                        };
                        needx.add_priority_base();
                        nds.push(needx);
                    }
                }
            } else {
                // Look for squares inbetween that have already been partially sampled.
                for sqrz in sqrs_in.iter() {
                    if sqrz.state == sqrx.state || sqrz.state == sqry.state {
                        continue;
                    }
                    if sqrz.pnc {
                        panic!("SNH");
                    }
                    let mut needx = SomeNeed::CloserIP {
                        dom_id: self.dom_id,
                        act_id: self.id,
                        target: ATarget::State {
                            state: sqrz.state.clone(),
                        },
                        unknown_region: regx.clone(),
                        priority: pri,
                    };
                    needx.add_priority_base();
                    nds.push(needx);
                }
            }
            continue;
        }

        nds
    } // end incompatible_pair_needs

    /// Cleanup unneeded squares.
    pub fn cleanup(&mut self, needs: &NeedStore) {
        //println!("action {} cleanup", self.id);
        // Check for groups that can be deleted.
        if needs.is_empty() {
            let mut del = Vec::<SomeRegion>::new();
            for grpx in self.groups.iter() {
                if grpx.anchor.is_none() {
                    println!(
                        "\nDom {} Act {} Group {} is not needed, removed.",
                        self.dom_id, self.id, grpx.region
                    );
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

            // Don't delete squares that define groups.
            for grpx in self.groups.iter() {
                if grpx.is_superset_of(keyx) {
                    for stax in grpx.region.states.iter() {
                        if stax == keyx {
                            continue 'next_sqr;
                        }
                    }
                    if grpx.region.states.len() > 2 && grpx.region.last_state() == keyx {
                        continue 'next_sqr;
                    }

                    if let Some(anchor) = &grpx.anchor {
                        if keyx == &anchor.pinnacle {
                            continue 'next_sqr;
                        }
                    }
                } else if let Some(anchor) = &grpx.anchor {
                    if anchor.edges.contains(keyx) {
                        continue 'next_sqr;
                    }
                }
            }

            // Don't delete squares represented in defining regions.
            for regx in self.structure_regions.iter() {
                if keyx == regx.first_state() {
                    continue 'next_sqr;
                }
                if keyx == regx.last_state() {
                    continue 'next_sqr;
                }
            }

            // Don't delete squares that are not in a group.
            // That is, some squares with Pn: > One that need more samples.
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
    /// Groups closer to the beginning of the group store vector will have priority due to lower group number.
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
            let Some(anchor) = &grpx.anchor else {
                continue;
            };

            if !self.groups.in_1_group(&anchor.pinnacle) {
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

        // Reset limited indicator to recheck, if maximum region changes.
        for grpx in self.groups.iter_mut() {
            if !grpx.pnc {
                continue;
            }
            if !grpx.limited {
                continue;
            }
            if let Some(anchor) = &grpx.anchor {
                if max_reg.is_superset_of(&anchor.pinnacle) {
                   // if let Some(anchor_mask) = &grpx.anchor_mask {
                        let max_mask = max_reg.x_mask().bitwise_and(&grpx.region.edge_mask());
                        if anchor.edge_mask != max_mask {
                            println!(
                                "Dom {} Act {} Group {} set limited off mask {}",
                                self.dom_id, self.id, grpx.region, anchor.edge_mask
                            );
                            grpx.set_limited_off();
                        }
                    //}
                }
            }
        }

        // Get anchor needs.

        // Gather group anchor adjacent needs.
        // Possibly decide to turn the limited flag on.
        let mut set_on = Vec::<(SomeRegion, SomeMask)>::new();

        for group_num in 0..self.groups.len() {
            if !self.groups[group_num].pnc {
                continue;
            }

            let regx = self.groups[group_num].region.clone();

            let ndsx = self.limit_group_anchor_needs(&regx, group_num, max_reg);
            if ndsx.is_not_empty() {
                ret_nds.append(ndsx);
                continue;
            }

            // Quick limit logic.
            //if self.groups[group_num].anchor.is_some() && self.structure_regions.contains(&self.groups[group_num].region) {
            //    let msk = self.groups[group_num].edge_mask().bitwise_and(&max_reg.x_mask());
            //    self.groups[group_num].set_limited(msk);
            //    continue;
            //}

            if let Some(anchor) = &self.groups[group_num].anchor {
                if let Some(ndx) = self.limit_group_adj_needs(&regx, &anchor.pinnacle.clone(), max_reg, group_num) {
                    ret_nds.append(ndx);
                    continue;
                } else if self.groups[group_num].limited {
                } else {
                    let edges = self.groups[group_num]
                        .region
                        .edge_mask()
                        .bitwise_and(&max_reg.x_mask());
                    set_on.push((regx, edges));
                }
            }
        }

        // Set limited on for selected groups.
        for (grp_reg, edges) in set_on {
            self.set_group_limited(&grp_reg, edges);
        }

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

        println!(
            "Dom {} Act {} Group {} set limited on, adj mask {}",
            self.dom_id, self.id, grp_reg, edges
        );

        let grpx = self.groups.find_mut(grp_reg).expect("SNH");

        grpx.set_limited();
        self.limited.push(grpx.region.clone());
    }

    /// Return a rate for a possible anchor for a group.
    /// The number adjacent states that are in only one group,
    pub fn group_anchor_rate(&self, regx: &SomeRegion, stax: &SomeState) -> usize {
        debug_assert_eq!(regx.num_bits(), self.num_bits);
        debug_assert_eq!(stax.num_bits(), self.num_bits);

        if !self.groups.in_1_group(stax) {
            return 0;
        }

        let mut in_1_group = 0;

        // Get masks of edge bits to use to limit group.
        let edge_msks = regx.edge_mask().split();

        // Rate adjacent external states
        for edge_bit in edge_msks.iter() {
            let sta_adj = stax.bitwise_xor(edge_bit).as_state();

            if self.groups.in_1_group(&sta_adj) {
                in_1_group += 1;
            }
        } // next edge_bit

        in_1_group
    }

    /// Return the limiting anchor needs for a group.
    /// If no state in the group is in only one group, return None.
    /// If an existing anchor has the same, or better, rating than other possible states,
    /// retain it, else replace it.
    /// If the anchor is not pnc, return a need to get an additional sample.
    pub fn limit_group_anchor_needs(&mut self, regx: &SomeRegion, group_num: usize, max_reg: &SomeRegion) -> NeedStore {
        //println!(
        //    "action::limit_group_anchor_needs: Dom {} Act {} group {regx} max_reg {max_reg}",
        //    self.dom_id, self.id
        //);
        debug_assert_eq!(regx.num_bits(), self.num_bits);

        // Check group anchor, if any.
        let mut grp_anchor: Option<SomeState> = None;

        let grpx = self.groups.find(regx).expect("SNH");

        if let Some(anchor) = &grpx.anchor {
            grp_anchor = Some(anchor.pinnacle.clone());
        }

        // Init NeedStore to return.
        let mut ret_nds = NeedStore::new(vec![]);

        // Check structure_regions.

        // Find possible anchor states.
        let mut adj_states = StateStore::new(vec![]);
        for regx in self.structure_regions.iter() {
            if regx.is_superset_of(&grpx.region)
                && grpx.region.is_superset_of(regx.first_state())
                && self.structure_regions.in_one_region(regx.first_state())
                && self.squares.find(regx.first_state()).is_some()
            {
                adj_states.push(regx.first_state().clone());
            }
        }

        // Set anchor, if needed.
        if adj_states.is_not_empty() {
            if let Some(anchor) = &grpx.anchor {
                if adj_states.contains(&anchor.pinnacle) {
                } else {
                    //println!("defining region anchor {} for group {}, from {}", adj_states[0], grpx.region, anchor);
                    self.groups
                        .find_mut(regx)
                        .expect("SNH")
                        .set_anchor(&adj_states[0], max_reg);
                }
                //return ret_nds;
            } else {
                //println!("defining region anchor {} for group {}", adj_states[0], grpx.region);
                self.groups
                    .find_mut(regx)
                    .expect("SNH")
                    .set_anchor(&adj_states[0], max_reg);
                //return ret_nds;
            }
        }

        // Identify, and rate, all squares in the group region that are only in one region.

        // Get square states in the group region.
        let stas_in: StateStore = self.squares.stas_in_reg(regx);

        //println!("action::limit_group_anchor_needs: stas to check {stas_in}");

        // For each state in the group, not in any other group,
        // Calculate a rate, based on the number of adjacent states outside of the group,
        // that are also only in one group.
        let mut max_rate = 0;

        // Create a vector for states with the maximum rate.
        let mut cfmv_max = Vec::<&SomeState>::new();

        for stax in stas_in.iter() {
            // Potential new anchor must be in only one group.
            if !self.groups.in_1_group(stax) {
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

        // Check that at least one square only in one region was found.
        if cfmv_max.is_empty() {
            //println!("group {} cfmv_max empty", regx);
            return ret_nds;
        }

        // Check if current anchor, if any, is rated below other possibilities.
        // If so, remove it, else check if it needs more samples.
        if let Some(stax) = &grp_anchor {
            if cfmv_max.contains(&stax) {
                // Current anchor, still GE rate of any others.
                if let Some(sqrx) = self.squares.find(stax) {
                    if sqrx.pnc {
                    } else {
                        // Get additional samples.
                        let mut needx = SomeNeed::LimitGroup {
                            dom_id: self.dom_id,
                            act_id: self.id,
                            anchor: stax.clone(),
                            target: ATarget::State {
                                state: stax.clone(),
                            },
                            for_group: regx.clone(),
                            priority: group_num, // Adjust priority so groups in the beginning of the group list (longest survivor) are serviced first.
                        };
                        needx.add_priority_base();
                        ret_nds.push(needx);
                    }
                } else {
                    panic!("SNH");
                }
                return ret_nds;
            } else {
                println!(
                    "anchor {stax} in group {regx} removed, rate {} below {max_rate}",
                    self.group_anchor_rate(regx, stax)
                );
                if let Some(grpx) = self.groups.find_mut(regx) {
                    grpx.set_anchor_off();
                }
            }
        }

        // Select an anchor, get needs.
        let mut cfm_max = cfmv_max[0];

        if cfmv_max.len() > 1 {
            cfm_max = cfmv_max[rand::rng().random_range(0..cfmv_max.len())];
        }

        // Set new anchor.
        self.groups.find_mut(regx).expect("SNH").set_anchor(cfm_max, max_reg);

        // Check if more samples are needed.
        if let Some(sqrx) = self.squares.find(cfm_max) {
            if sqrx.pnc {
            } else {
                // Get additional samples.
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
        } else {
            panic!("SNH");
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

        // We want anchor square and external adjacent square to be pnc.
        if !anchor_sqr.pnc {
            return None;
        }

        // Check each adjacent external state
        let mut nds_grp = NeedStore::new(vec![]); // needs for more samples

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
                    if anchor_sqr.compatible(adj_sqr) == Compatibility::Compatible {
                        if let Some(grpx) = self.groups.find_mut(regx) {
                            grpx.set_anchor_off();
                        }
                        return None;
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
                    let mut needx = SomeNeed::LimitGroupAdj {
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
                let mut needx = SomeNeed::LimitGroupAdj {
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
                    //} else if grpx.is_adjacent(grpy) {
                    //    nds.append(self.group_combine_needs(grpx, grpy));
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

    /// Check two intersecting groups for
    /// a contradictatory intersection.
    fn group_pair_intersection_needs(
        &self,
        grpx: &SomeGroup,
        grpy: &SomeGroup,
        group_num: usize,
    ) -> NeedStore {
        //println!(
        //  "action::groups_pair_intersection_needs: Dom {} Act {} {} {} and {} {} at {}",
        //  self.dom_id, self.id, &grpx.region, grpx.pn, grpy.region, grpy.pn, grpx.region.intersection(&grpy.region).unwrap()
        //);
        debug_assert_eq!(grpx.num_bits(), self.num_bits);
        debug_assert_eq!(grpy.num_bits(), self.num_bits);

        assert!(grpx.region.intersects(&grpy.region));
        assert!(grpx.region != grpy.region);

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

        // If contradictory, return needs to resolve

        // Check if a valid sub-region of the intersection exists
        if let Some(rulsxy) = rulsx.intersection(&rulsy) {
            // If a valid sub-region exists, seek a sample in intersection that is not in rulsxy.initial_region.
            let ok_reg = rulsxy.initial_region();

            if ok_reg != reg_int {
                // To test all bits that may be a problem.
                let far_reg = reg_int.far_from_reg(&ok_reg);

                //println!(
                //    "cont int {} and {}, intersection is {} ok rules {} ok reg {} far reg is {}",
                //    grpx.region, grpy.region, reg_int, rulsxy, ok_reg, far_reg
                //);

                // Calc rules for far region.
                let rulsx = rulsx.restrict_initial_region(&far_reg);
                let rulsy = rulsy.restrict_initial_region(&far_reg);

                if let Some(needx) = self.cont_int_region_need(
                    &far_reg,
                    grpx,
                    grpy,
                    group_num,
                    Some(rulsx),
                    Some(rulsy),
                ) {
                    nds.push(needx);
                }
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

        for grpx in self.groups.iter() {
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
                let stpsx = self.get_steps_from_rulestore(rules, wanted_changes, within);
                if stpsx.is_empty() {
                    continue;
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
        //println!("action::get_steps_from_rulestore: Dom {} Act {} rules {rules} wanted {wanted_changes} within {within}", self.dom_id, self.id);
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
                        stps.push(SomeStep::new(self.id, rulx, None));
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
                        stps.push(SomeStep::new(self.id, rul_sqr, None));
                    }
                }
            }

            if let Some(rulx) = &rules2[1] {
                if rulx.result_region().is_superset_of(last_result) { // next result should follow the alternate rule.
                } else {
                    let rul_sqr = rulx.restrict_initial_region(&sqrx.state);
                    if wanted_changes.intersection(&rul_sqr).is_not_low() {
                        stps.push(SomeStep::new(self.id, rul_sqr, None));
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
                        stps.push(SomeStep::new(self.id, rulz, Some(&rules[1])));
                    }
                }
            }

            if let Some(rulx) = &rules2[1] {
                let ruly = rulx.restrict_initial_region(regx);
                if wanted_changes.intersection(&ruly).is_not_low() {
                    if let Some(rulz) = ruly.restrict_for_changes(wanted_changes) {
                        stps.push(SomeStep::new(self.id, rulz, Some(&rules[0])));
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

        let mut ret_grps = GroupStore::new(vec![]);

        if sqrx.pn == Pn::One || sqrx.pnc {
        } else {
            return ret_grps;
        }

        // Check regions the square is in.
        let mut try_all = true;
        for regx in self.structure_regions.iter() {
            if regx.is_superset_of(&sqrx.state) {
                for grpx in self.possible_groups_from_square2(sqrx, regx) {
                    ret_grps.push_nosubs(grpx);
                    try_all = false;
                }
            }
        }

        // Defining regions may not cover all, more defining regions may need to be discovered.
        if try_all {
            let max_region = SomeRegion::new(vec![sqrx.state.new_high(), sqrx.state.new_low()]);
            for grpx in self.possible_groups_from_square2(sqrx, &max_region) {
                ret_grps.push_nosubs(grpx);
            }
        }

        // If no groups, create a one-state group.
        if ret_grps.is_empty() {
            ret_grps.push_nosubs(SomeGroup::new(
                SomeRegion::new(vec![sqrx.state.clone()]),
                sqrx.rules.clone(),
                sqrx.pnc,
            ));
        }

        ret_grps
    }

    /// Find groups that can be formed by a given square, and other similar squares, in a given region.
    fn possible_groups_from_square2(&self, sqrx: &SomeSquare, regx: &SomeRegion) -> GroupStore {
        let mut ret_grps = GroupStore::new(vec![]);

        let mut sqrs_in = self.squares.squares_in_reg(regx);

        // Check memory for additional squares.
        let mem_sqrs_in = self.squares.memory_squares_in_reg(regx);
        for sqry in mem_sqrs_in {
            if !sqrs_in.contains(&sqry) {
                sqrs_in.push(sqry);
            }
        }

        // Subtract dissimilar squares.
        let mut poss_regs = RegionStore::new(vec![regx.clone()]);

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
            if regx.is_superset_of(&sqrx.state) {
            } else {
                continue;
            }
            let other_sqrs_in = self.squares.squares_in_reg(regx);

            // Process an Unpredictable square.
            if sqrx.pn == Pn::Unpredictable {
                if let Some(grpx) = self.validate_possible_group(sqrx, regx) {
                    ret_grps.push_nosubs(grpx);
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

            // Generate groups, if any.
            for regz in poss_regs2.iter() {
                if regz.is_superset_of(&sqrx.state) {
                    if let Some(grpx) = self.validate_possible_group(sqrx, regz) {
                        ret_grps.push_nosubs(grpx);
                    }
                }
            }
        } // next regx

        ret_grps
    } // end possible_regions_from_square

    /// Validate a region that may be made from a given square, in combination with similar squares.
    fn validate_possible_group(&self, sqrx: &SomeSquare, regx: &SomeRegion) -> Option<SomeGroup> {
        //println!("action::validate_possible_group: Dom {} Act {} state {} reg {}", self.dom_id, self.id, sqrx.state, regx);
        debug_assert_eq!(sqrx.num_bits(), self.num_bits);
        debug_assert_eq!(regx.num_bits(), self.num_bits);
        debug_assert!(regx.is_superset_of(sqrx));

        if let Some((regy, rules)) = self.check_region_for_group(regx, sqrx.pn) {
            if regy.is_superset_of(&sqrx.state) {
                if self.groups.any_superset_of(&regy) {
                    None
                } else {
                    Some(SomeGroup::new(regy, rules, false))
                }
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

        // Additional processing for selected kinds of needs.
        if let SomeNeed::ConfirmGroup { grp_reg, .. } = ndx {
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
        if let SomeNeed::LimitGroupAdj {
            target: ATarget::State { state },
            for_group,
            anchor,
            ..
        } = ndx
        {
            let sqr_targ = self.squares.find(state).expect("SNH");
            if sqr_targ.pnc {
                let sqr_anc = self.squares.find(anchor).expect("SNH");
                if sqr_anc.pnc && sqr_anc.compatible(sqr_targ) == Compatibility::Compatible {
                    if sqr_anc.pn == Pn::Unpredictable {
                        self.add_group(SomeGroup::new(
                            SomeRegion::new(vec![state.clone(), anchor.clone()]),
                            None,
                            true,
                        ));
                    } else if let Some(rules_a) = &sqr_anc.rules {
                        if let Some(rules_t) = &sqr_targ.rules {
                            if let Some(rules) = rules_a.union(rules_t) {
                                self.add_group(SomeGroup::new(
                                    SomeRegion::new(vec![state.clone(), anchor.clone()]),
                                    Some(rules),
                                    true,
                                ));
                                if let Some(grpx) = self.groups.find_mut(for_group) {
                                    grpx.set_anchor_off();
                                }
                            }
                        }
                    }
                }
            }
        }

        asample
    }

    /// Take an action with the current state, add the sample to squarestore.
    /// Return a sample.
    pub fn take_action_arbitrary(&mut self, cur_state: &SomeState) -> SomeSample {
        //println!("action::take_action_arbitrary: Dom {} Act {} cur_state {cur_state}", self.dom_id, self.id);
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);

        let asample = self.get_sample(cur_state);

        self.eval_sample_arbitrary(&asample);

        asample
    }

    /// Take an action with the current state, for a step.
    /// Assume the result is as expected.
    /// Return a sample.
    pub fn take_action_step(&mut self, cur_state: &SomeState) -> SomeSample {
        //println!("action::take_action_step: Dom {} Act {} cur_state {cur_state}", self.dom_id, self.id);
        debug_assert_eq!(cur_state.num_bits(), self.num_bits);

        let asample = self.get_sample(cur_state);

        self.eval_sample(&asample);

        asample
    }

    /// Return a change with all changes that can be made for the action.
    pub fn calc_aggregate_changes(&self) -> Option<SomeChange> {
        //println!("SomeAction::aggregate_changes: Act {}", self.id);

        self.groups.calc_aggregate_changes()
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
                .find(&anchor.pinnacle)
                .expect("Group anchor should refer to an existing square");
            let rate = self.group_anchor_rate(&grpx.region, &anchor.pinnacle);
            println!("anchor {sqrx} rate {:?}", rate);
            let stas_adj = self.squares.stas_adj_reg(&grpx.region);
            for stax in stas_adj.iter() {
                if stax.is_adjacent(&anchor.pinnacle) {
                    let sqrx = self.squares.find(stax).expect("SNH");
                    let grps = self.groups.groups_in(stax);
                    if grps.len() == 1 {
                        if let Some(grpy) = self.groups.find(grps[0]) {
                            if let Some(anchory) = &grpy.anchor {
                                if stax == &anchory.pinnacle {
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
        self.base_rules.len()
    }

    /// Return a String representation of a SomeAction instance.
    fn formatted_state(&self) -> String {
        let mut rc_str = String::from("ACT(ID: ");

        rc_str += &self.id.to_string();

        rc_str += ", number squares: ";
        rc_str += &self.squares.len().to_string();

//       if self.structure_regions.len() > 1 {
//           rc_str += &format!(", calculated structure regions: {}", self.structure_regions);
//           if self.structure_regions.len() > 1 {
//               rc_str += &format!(", defining regions: {}", self.structure_regions.defining_regions());
//           }
//       }

        let mut fil = "\n       Grps: ";

        // Get max group str length.
        let mut max_len = 0;
        for grpx in self.groups.iter() {
            if let Some(ruls) = &grpx.rules {
                if ruls.strlen() > max_len {
                    max_len = ruls.strlen();
                }
            }
        }

        // Get group string, max string len, of all groups.
        let mut max_len2 = 0;
        let mut grps = Vec::<String>::with_capacity(self.groups.len());
        for grpx in self.groups.iter() {
            let tmp_str = grpx.formatted_str_adjusted(max_len);
            let tmp_len = tmp_str.len();
            if tmp_len > max_len2 {
                max_len2 = tmp_len;
            }
            grps.push(tmp_str);
        }

        for (grpx, grpx_str) in self.groups.iter().zip(grps.iter()) {
            let stas_in = self.squares.stas_in_reg(&grpx.region);

            let cnt: usize = stas_in
                .iter()
                .map(|stax| usize::from(self.groups.num_groups_in(stax) == 1))
                .sum();

            rc_str.push_str(&format!("{}{}", fil, grpx_str));

            let grp_len = grpx_str.len();
            let fil2 = if grp_len < max_len2 {
                " ".repeat(max_len2 - grp_len)
            } else {
                String::new()
            };

            rc_str.push_str(&format!(
                "{} num Sqrs: {} Sqrs in: {} in1: {})",
                fil2,
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
    pub fn formatted_def(&self) -> String {
        let mut rc_str = String::from("ACT[");
        let mut first = true;
        for rulstrx in self.base_rules.iter() {
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
                        self.dom_id, self.id, sqrx.state, sqrx.pn, grpx.region, grpx.pn
                    );
                } else if sqrx.pn < grpx.pn && sqrx.pnc {
                    println!(
                        "\nDom {} Act {} square {} pn: {} pnc: true invalidates\n             group {} pn: {}",
                        self.dom_id, self.id, sqrx.state, sqrx.pn, grpx.region, grpx.pn
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
                if anchor.pinnacle.is_adjacent(&sqrx.state) {
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
                "\nDom {} Act {} Group {} is a subset of {reg}, removed.",
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
        // Check for supersets, which probably is an error
        if self.groups.any_superset_of(&grpx.region) {
            let regs = self.groups.supersets_of(&grpx.region);
            println!(
                "Dom {} Act {} skipped adding group {}, a superset exists in {regs}",
                self.dom_id, self.id, grpx.region,
            );
            return;
        }

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
        // Remove subset groups
        //self.groups_remove_subsets_of(&grpx.region);

        self.groups.push_nosubs(grpx);
    }

    // Remove a group.
    // This is always used, instead of self.groups.remove_group, to insure
    // that self.agg_chgs_updated is changed.
    // Caller should print a message to the user.
    pub fn remove_group(&mut self, grp_reg: &SomeRegion) -> bool {
        if self.groups.remove_group(grp_reg) {
            //println!(
            //    "\nDom {} Act {} Group {} deleted",
            //    self.dom_id, self.id, grp_reg
            //);
            true
        } else {
            false
        }
    }

    /// Return a sample for a taking an action on a given state.
    fn get_sample(&self, initial_state: &SomeState) -> SomeSample {
        let asample = self.get_sample2(initial_state);
        println!(
            "\nDom {} {} -{}-> {} [{}]",
            self.dom_id,
            asample.initial,
            self.id,
            asample.result,
            SomeRule::new(&SomeSample::new(
                asample.initial.clone(),
                asample.result.clone()
            ))
        );
        asample
    }
    fn get_sample2(&self, initial_state: &SomeState) -> SomeSample {
        for rulsx in self.base_rules.iter() {
            if rulsx.initial_region().is_superset_of(initial_state) {
                if rulsx.len() == 1 {
                    return SomeSample::new(
                        initial_state.clone(),
                        rulsx[0].result_from_initial_state(initial_state),
                    );
                }

                // Get ref to existing square, if any.
                if let Some(sqrx) = self.squares.find(initial_state) {
                    let recent_result = sqrx.most_recent_result();
                    // Get rule number to get the most recent result.
                    for (inx, rulx) in rulsx.iter().enumerate() {
                        if rulx.result_from_initial_state(initial_state) == *recent_result {
                            // Assume left-to-right-wrap-around use-order in multiple rules.
                            let iny = (inx + 1) % rulsx.len();
                            return SomeSample::new(
                                initial_state.clone(),
                                rulsx[iny].result_from_initial_state(initial_state),
                            );
                        }
                    }
                    panic!("No rule found?");
                } else if let Some(sqrx) = self.squares.memory_find(initial_state) {
                    let recent_result = sqrx.most_recent_result();
                    // Get rule number to get the most recent result.
                    for (inx, rulx) in rulsx.iter().enumerate() {
                        if rulx.result_from_initial_state(initial_state) == *recent_result {
                            // Assume left-to-right-wrap-around use-order in multiple rules.
                            let iny = (inx + 1) % rulsx.len();
                            return SomeSample::new(
                                initial_state.clone(),
                                rulsx[iny].result_from_initial_state(initial_state),
                            );
                        }
                    }
                    panic!("No rule found?");
                } else {
                    // Choose a rule randomly.  If the last result for a state was not saved in a square, or the square was deleted,
                    // its the same as if the state was never sampled.
                    return SomeSample::new(
                        initial_state.clone(),
                        rulsx[rand::rng().random_range(0..rulsx.len())]
                            .result_from_initial_state(initial_state),
                    );
                };
            }
        }
        // If no rule found, assume no change.
        SomeSample::new(initial_state.clone(), initial_state.clone())
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
        let str_in2 = str_in.trim();

        // Strip off id and surrounding brackets.
        if str_in2.len() < 5 {
            return Err(
                "action::from_str: string should be at least = ACT[<one RulesStore>]".to_string(),
            );
        }

        if str_in2[0..4].to_uppercase() != *"ACT[" {
            return Err("action::from_str: string should begin with ACT[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("action::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[4..(str_in2.len() - 1)];

        // Split string into RuleStore tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("action::from_str: {errstr}")),
        };
        //println!("tokens {:?}", tokens);

        let mut rs_vec = Vec::<RuleStore>::with_capacity(tokens.len());

        // Generate vector of RuleStores for action.
        for tokenx in tokens.iter() {
            //println!("rulestores for an action: {tokenx}");
            if tokenx[0..1] == *"[" {
                match RuleStore::from_str(tokenx) {
                    Ok(rulstrx) => {
                        if rulstrx.is_empty() {
                            return Err("action::from_str: Empty RuleStore.".to_string());
                        }
                        rs_vec.push(rulstrx);
                    }
                    Err(errstr) => return Err(errstr),
                }
            } else if tokenx[0..1].to_lowercase() == *"s" {
                continue;
            } else {
                return Err(format!("action::from_str: Unrecognized token, {tokenx}"));
            }
        }

        if rs_vec.is_empty() {
            return Err("action::from_str: No RuleStore.".to_string());
        }
        // Init the action.
        let mut actx = SomeAction::new(rs_vec);

        // Generate samples for each.
        for tokenx in tokens.iter() {
            //println!("rulestores for an action: {tokenx}");
            if tokenx[0..1] == *"[" {
                continue;
            } else if tokenx[0..1].to_lowercase() == *"s" {
                let mut num_str = "1".to_string();
                let mut tokeny = tokenx.clone();
                // Split state token by "/" separator, if any.
                if let Some(inx) = tokenx.find('/') {
                    num_str = tokenx[(inx + 1)..].to_string();
                    tokeny = tokenx[0..inx].to_string();
                }
                //println!("tokeny {tokeny} num_str {num_str}");

                match SomeState::from_str(&tokeny) {
                    Ok(stax) => {
                        match num_str.parse::<usize>() {
                            Ok(num) => {
                                //println!("num times = {num}");
                                if num > 0 {
                                    for _ in 0..num {
                                        actx.take_action_arbitrary(&stax);
                                    }
                                } else {
                                    return Err(format!(
                                        "action::from_str: Did not understand count in token {tokenx}"
                                    ));
                                }
                            }
                            Err(errstr) => return Err(errstr.to_string()),
                        }
                    }
                    Err(errstr) => return Err(errstr),
                }
            } else {
                return Err(format!("action::from_str: Unrecognized token, {tokenx}"));
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

        assert!(nds.contains_need_type("LimitGroupAdj"));
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
    fn calc_aggregate_changes() -> Result<(), String> {
        // Init action.
        let mut act0 = SomeAction::from_str("ACT[[00/00/10/00], [01/11/00/X0]]")?;

        // Set up square 2.
        act0.take_action_arbitrary(&SomeState::from_str("s0010")?);

        println!("act0: {act0}");
        assert!(act0.groups.len() == 1);

        assert!(act0.calc_aggregate_changes() == Some(SomeChange::from_str("../../10/..")?));

        // Make group 45.
        act0.take_action_arbitrary(&SomeState::from_str("s0100")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0101")?);

        println!("act0: {act0}");
        assert!(act0.groups.len() == 2);

        if let Some(change3) = act0.calc_aggregate_changes() {
            println!("change3 {change3}");
            assert!(change3 == SomeChange::from_str("01/../10/10")?);
        } else {
            return Err("Test 3 failed".to_string());
        }

        // Make square 5 unpredictable.
        act0.eval_sample_arbitrary(&SomeSample::from_str("s0101->s1111")?);
        act0.eval_sample_arbitrary(&SomeSample::from_str("s0101->s0111")?);

        println!("act0: {act0}");
        assert!(act0.groups.len() == 3); // groups 0010, 0100, 0101 (unpredictable).

        if let Some(change4) = act0.calc_aggregate_changes() {
            println!("change4 {change4}");
            assert!(change4 == SomeChange::from_str("01/../10/..")?);
        } else {
            return Err("Test 4 failed".to_string());
        }

        Ok(())
    }

    #[test]
    /// Test action definition from string to instance, then instance to string(2), then string(2) to instance.
    fn from_str() -> Result<(), String> {
        let actx_str = "ACT[[XX_10/XX/XX/XX], [Xx_00/XX/XX/XX]]";
        let actx = SomeAction::from_str(&actx_str)?; // String to instance.
        println!("actx {}", actx.formatted_def());
        assert!(actx_str == actx.formatted_def());
        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn non_adjacent_incompatibility_needs0() -> Result<(), String> {
        let mut act0 = SomeAction::from_str(
            "ACT[[01/XX/01/XX], [00/XX/10/XX], [10/XX/00/XX], [11/XX/11/XX]]",
        )?;

        println!("act0 {}", act0.formatted_def());

        let sta4 = SomeState::from_str("s0100")?;

        act0.take_action_arbitrary(&SomeState::from_str("s0100")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0111")?);

        println!("act0 {}", act0.formatted_state());

        let max_reg = SomeRegion::max_region(act0.num_bits);

        let nds = act0.get_needs(&sta4, &max_reg);
        println!("needs: {nds}");

        // Get more samples of incompatible pair.
        act0.take_action_arbitrary(&SomeState::from_str("s0100")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0100")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0111")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0111")?);

        let nds = act0.get_needs(&sta4, &max_reg);
        println!("needs: {nds}");

        // Needs to get closer incompatible pair should exist.
        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::Region {
                region: SomeRegion::from_str("r0101")?
            }
        ));
        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::Region {
                region: SomeRegion::from_str("r0110")?
            }
        ));

        // Add needed sample.
        act0.take_action_arbitrary(&SomeState::from_str("s0101")?);
        let nds = act0.get_needs(&sta4, &max_reg);
        println!("needs: {nds}");

        // Confirm 0101, so incompatible pair is 5, 7.
        act0.take_action_arbitrary(&SomeState::from_str("s0101")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0101")?);

        // The pair (4, 7) should not trigger any NAI needs.
        let nds = act0.get_needs(&sta4, &max_reg);
        println!("needs: {nds}");
        assert!(!nds.contains_need_type("CloserIP"));

        // Add square 6, the pairing of (4, 6) should now generate needs.
        act0.take_action_arbitrary(&SomeState::from_str("s0110")?);

        let nds = act0.get_needs(&sta4, &max_reg);
        println!("needs: {nds}");

        // Confirm 0110.
        act0.take_action_arbitrary(&SomeState::from_str("s0110")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0110")?);

        let nds = act0.get_needs(&sta4, &max_reg);
        println!("needs: {nds}");
        assert!(!nds.contains_need_type("CloserIP"));

        // Add square 1011.
        act0.take_action_arbitrary(&SomeState::from_str("s1011")?);
        let nds = act0.get_needs(&sta4, &max_reg);
        println!("needs: {nds}");

        // Confirm 1011.
        act0.take_action_arbitrary(&SomeState::from_str("s1011")?);
        act0.take_action_arbitrary(&SomeState::from_str("s1011")?);
        let nds = act0.get_needs(&sta4, &max_reg);
        println!("needs: {nds}");

        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::Region {
                region: SomeRegion::from_str("r1111")?
            }
        ));
        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::Region {
                region: SomeRegion::from_str("r0011")?
            }
        ));

        // Give it samples of 0011.
        act0.take_action_arbitrary(&SomeState::from_str("s0011")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0011")?);
        act0.take_action_arbitrary(&SomeState::from_str("s0011")?);
        let nds = act0.get_needs(&sta4, &max_reg);
        println!("needs: {nds}");

        assert!(!nds.contains_need_type("CloserIP"));

        //assert!(1 ==2);
        Ok(())
    }

    #[test]
    fn non_adjacent_incompatibility_needs2() -> Result<(), String> {
        let mut act0 = SomeAction::from_str(
            "ACT[[XX/XX/00/XX], [XX/XX/11/XX, XX/XX/10/xx, Xx/XX/11/XX], s0000/3, s1111/3]",
        )?;

        println!("act0 {}", act0.formatted_def());

        let nds = act0.get_needs(
            &SomeState::from_str("s1111")?,
            &SomeRegion::from_str("rXXXX")?,
        );
        println!("needs: {nds}");

        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::State {
                state: SomeState::from_str("s0011")?
            }
        ));

        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::State {
                state: SomeState::from_str("s0101")?
            }
        ));

        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::State {
                state: SomeState::from_str("s0110")?
            }
        ));

        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::State {
                state: SomeState::from_str("s1100")?
            }
        ));

        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::State {
                state: SomeState::from_str("s1001")?
            }
        ));

        assert!(nds.contains_similar_need(
            "CloserIP",
            &ATarget::State {
                state: SomeState::from_str("s1010")?
            }
        ));

        //assert!(1 == 2);
        Ok(())
    }
}
