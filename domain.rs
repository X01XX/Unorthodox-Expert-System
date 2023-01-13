//! The SomeDomain struct, representing a pseudo Karnaugh Map with a specific number of bits.
//!
//! Contains a vector of Action structs, the current state, and a few other fields.
//!
//! Generates needs to improve the understanding of rules.
//!
//! Generates needs to seek optimal regions.
//!
//! Generates plans to satisfy needs.
//!
//! Executes a plan to satisfy a need.
//!
//! When the current state is in at least one optimal region, sets and counts down a boredom value.
//! The boredom value is greater if the the current state is in multiple optimal regions (intersection).

use crate::action::SomeAction;
use crate::actionstore::ActionStore;
use crate::bits::SomeBits;
use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::randompick::RandomPick;
use crate::region::SomeRegion;
use crate::removeunordered::remove_unordered;
use crate::state::SomeState;
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use rand::Rng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing

impl fmt::Display for SomeDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("D(ID: ");

        rc_str.push_str(&self.num.to_string());

        let _ = write!(rc_str, ", Current State: {}", &self.cur_state);

        rc_str.push(')');

        write!(f, "{}", rc_str)
    }
}

const MAX_MEMORY: usize = 20; // Max number of recent current states to keep in a circular buffer.

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct SomeDomain {
    /// Domain number.  Index into a higher-level DomainStore.
    pub num: usize,
    /// Actions the Domain can take.
    pub actions: ActionStore,
    /// The Current, internal, State.
    pub cur_state: SomeState,
    /// A counter to indicate the number of steps the current state is in the same optimal region
    /// before getting bored.
    pub memory: VecDeque<SomeState>,
    /// Most recent aggregate changes
    agg_changes: SomeChange,
}

impl SomeDomain {
    /// Return a new domain instance, given the number of integers, the
    /// initial state, the optimal state(s), the index into the higher-level DomainStore.
    pub fn new(dom: usize, num_ints: usize) -> Self {
        // Set up a domain instance with the correct value for num_ints
        SomeDomain {
            num: dom,
            actions: ActionStore::new(),
            cur_state: initialize_state(num_ints),
            memory: VecDeque::<SomeState>::with_capacity(MAX_MEMORY),
            agg_changes: SomeChange::new(SomeMask::new_low(num_ints), SomeMask::new_low(num_ints)),
        }
    }

    /// Return a reference to the current, internal, state.
    pub fn get_current_state(&self) -> &SomeState {
        &self.cur_state
    }

    /// Add a SomeAction instance to the store.
    pub fn add_action(&mut self) {
        let actx = SomeAction::new(self.num, self.actions.len(), &self.cur_state);
        self.actions.push(actx);
    }

    /// Return needs gathered from all actions.
    pub fn get_needs(&mut self) -> NeedStore {
        // Get aggregate changes mask
        let cur_agg_cngs = self.aggregate_changes();

        if cur_agg_cngs != self.agg_changes {
            let new_chgs = self.agg_changes.c_not().c_and(&cur_agg_cngs);

            if new_chgs.is_low() {
                // fewer changes
            } else {
                // additional changes
                self.actions.check_limited(&new_chgs);
            }

            self.agg_changes = cur_agg_cngs;
        }

        // Get all needs.
        let mut nst =
            self.actions
                .get_needs(&self.cur_state, self.num, &self.memory, &self.agg_changes);

        for ndx in nst.iter_mut() {
            ndx.set_dom(self.num);
        }

        nst
    }

    /// Evaluate an arbitrary sample given by the user.
    /// This tends to break things for an action, unless all samples are arbitrary.
    /// Useful for testing a wholly different series of samples/results.
    pub fn eval_sample_arbitrary(
        &mut self,
        act_num: usize,
        i_state: &SomeState,
        r_state: &SomeState,
    ) {
        self.actions[act_num].eval_sample(i_state, r_state);
        self.set_state(r_state);
    }

    /// Take an action for a need, evaluate the resulting sample.
    /// It is assumed that a sample made for a need must be saved.
    pub fn take_action_need(&mut self, ndx: &SomeNeed) {
        let astate = self.actions[ndx.act_num()].take_action_need(self.num, &self.cur_state, ndx);

        self.set_state(&astate);
    }

    /// Take an action with the current state.
    /// Save sample if it is not in a group, or invalidates a group.
    pub fn take_action_arbitrary(&mut self, act_num: usize) {
        let astate = self.actions[act_num].take_action_arbitrary(&self.cur_state);

        self.set_state(&astate);
    }

    /// Set the current state field.
    pub fn set_state(&mut self, new_state: &SomeState) {
        // Update memory.
        if self.memory.len() >= MAX_MEMORY {
            self.memory.pop_back();
        }
        self.memory.push_front(self.cur_state.clone());

        // Set current state.
        self.cur_state = new_state.clone();
    }

    /// Run a plan, return true if it runs to completion.
    pub fn run_plan(&mut self, pln: &SomePlan) -> bool {
        assert!(pln.dom_num == self.num);

        if pln.is_empty() {
            return true;
        }
        self.run_plan2(pln, 3) // Init depth counter.
    }

    /// Run a plan, with a recursion depth check.
    /// An unexpected result has a few flavors.
    /// A step from a group whose rule is a combination of samples that covers too broad a region.
    ///   The rule is deleted and maybe others are formed.  A re-plan-to-goal is done.
    /// A rule that has two expected results, in order, and the state is not known by a most-recent
    /// sample to be the alternate result.
    ///   If the alternate result has no changes, the action can be immediately tried again.
    ///   If the alternate result changes something, a re-plan-to-goal is done.
    fn run_plan2(&mut self, pln: &SomePlan, depth: usize) -> bool {
        if depth == 0 {
            println!("\nDepth exceeded, at plan {}", &pln);
            return false;
        }

        if !pln.initial_region().is_superset_of_state(&self.cur_state) {
            println!(
                "\nCurrent state {} is not in the start region of plan {}",
                &self.cur_state, &pln
            );
            return false;
        }

        for stpx in pln.iter() {
            let astate = self.actions[stpx.act_num].take_action_step(&self.cur_state);

            let prev_state = self.cur_state.clone();

            self.set_state(&astate);

            if stpx.result.is_superset_of_state(&self.cur_state) {
                continue;
            }

            // Handle unexpected/unwanted result
            // May be an expected possibility from a two result state

            if prev_state == self.cur_state && stpx.alt_rule {
                println!("Try action a second time");

                let astate = self.actions[stpx.act_num].take_action_step(&self.cur_state);

                self.set_state(&astate);

                if stpx.result.is_superset_of_state(&self.cur_state) {
                    continue;
                }
            }

            println!(
                "\nChange [{} -{:02}> {}] unexpected, expected {}",
                &prev_state, &stpx.act_num, &self.cur_state, stpx,
            );

            // Try re-plan to goal
            if pln.result_region().is_superset_of_state(&self.cur_state) {
                println!("The unexpected result is in the goal region");
                return true;
            }

            if let Some(planx) = self.make_plan(pln.result_region()) {
                println!(
                    "The new plan from {} to {} is {}",
                    &self.cur_state,
                    &pln.result_region(),
                    &planx.str_terse()
                );
                return self.run_plan2(&planx, depth - 1);
            }
            println!(
                "A plan from {} to {} is not found",
                &self.cur_state,
                &pln.result_region(),
            );
            return false;
        } // next stpx

        pln.result_region().is_superset_of_state(&self.cur_state)
    } // end run_plan2

    /// Return the steps of a plan to go from a given state/region to a given region.
    pub fn random_depth_first_search(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        steps_str: &StepStore,
        steps_by_change_vov: &Vec<Vec<&SomeStep>>,
        depth: usize,
    ) -> Option<SomePlan> {
        if let Some(steps) = self.random_depth_first_search2(
            from_reg,
            goal_reg,
            steps_str,
            steps_by_change_vov,
            depth,
        ) {
            if let Some(steps2) = steps.shortcuts() {
                if steps2.is_valid() {
                    return Some(steps2);
                }
            }
            if steps.is_valid() {
                return Some(steps);
            }
        }
        None
    }

    /// Return the steps of a plan to go from a given state/region to a given region.
    ///
    /// This may be called from random_depth_first_search, or may be recursively called to make a sub-plan.
    ///
    /// If any needed bit changes are only possible through Asymmetric Chaining, randomly choose a step and recurse.
    ///
    /// Otherwise, randomly choose a step and do Forward Chaining, Backward Chaining, or Asymmetric Chaining, as needed.
    ///
    fn random_depth_first_search2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        steps_str: &StepStore,
        steps_by_change_vov: &Vec<Vec<&SomeStep>>,
        depth: usize,
    ) -> Option<SomePlan> {
        //println!("random_depth_first_search2: from {} to {}", from_reg, goal_reg);

        // Check if one step makes the required change, the end point of any search.
        let mut rand_inx = RandomPick::new(steps_str.len());
        while let Some(inx) = rand_inx.pick() {
            if steps_str[inx].initial.intersects(from_reg) {
                let stepy = steps_str[inx].restrict_initial_region(from_reg);

                if goal_reg.is_superset_of(&stepy.result) {
                    //println!("forward_depth_first_search2: suc 1 Found one step {} to go from {} to {}", &stepy, from_reg, goal_reg);
                    return Some(SomePlan::new_with_step(self.num, stepy));
                }
            }
        }

        // Check depth
        if depth == 0 {
            //println!("depth limit exceeded");
            return None;
        }

        // Check for single-bit vectors where no rules have an initial-region that
        // is a superset of the from-region, and the result-region does not intersect the
        // goal-region.
        let mut asym_inx = Vec::<usize>::new();

        'next_vecx: for (inx, vecx) in steps_by_change_vov.iter().enumerate() {
            for stepx in vecx.iter() {
                if stepx.initial.is_superset_of(from_reg) || stepx.result.intersects(goal_reg) {
                    continue 'next_vecx;
                }
            }
            asym_inx.push(inx);
        } // next vecx

        // Init selected steps
        let mut selected_steps = Vec::<&SomeStep>::new();

        // If any asymmetrical single-bit changes found
        if !asym_inx.is_empty() {
            // find min number of steps for the selected bit-changes
            let mut min_steps = 99;
            for inx in asym_inx.iter() {
                if steps_by_change_vov[*inx].len() < min_steps {
                    min_steps = steps_by_change_vov[*inx].len();
                }
            }
            // Assemble possible steps
            for inx in asym_inx.iter() {
                if steps_by_change_vov[*inx].len() == min_steps {
                    for stepx in steps_by_change_vov[*inx].iter() {
                        selected_steps.push(stepx);
                    }
                }
            }

            // Randomly choose a step.
            assert!(!selected_steps.is_empty());
            let stepx = selected_steps[rand::thread_rng().gen_range(0..selected_steps.len())];

            return self.asymmetric_chaining(from_reg, goal_reg, stepx, depth - 1);
        }

        // Randomly choose a step.
        let setx = rand::thread_rng().gen_range(0..steps_by_change_vov.len());
        let stinx = rand::thread_rng().gen_range(0..steps_by_change_vov[setx].len());

        let stepx = steps_by_change_vov[setx][stinx];

        // Process a forward chaining step
        if stepx.initial.is_superset_of(from_reg) {
            let stepy = stepx.restrict_initial_region(from_reg);

            let required_change = SomeChange::region_to_region(&stepy.result, goal_reg);

            // Get steps, check if steps include all changes needed.
            if let Some(steps_str) = self.get_steps(&required_change) {
                // Get vector of steps for each bit change.
                if let Some(steps_by_change_vov) =
                    self.get_steps_by_bit_change(&steps_str, &required_change)
                {
                    if let Some(next_steps) = self.random_depth_first_search2(
                        &stepy.result,
                        goal_reg,
                        &steps_str,
                        &steps_by_change_vov,
                        depth - 1,
                    ) {
                        if let Some(final_plan) =
                            SomePlan::new_with_step(self.num, stepy).link(&next_steps)
                        {
                            return Some(final_plan);
                        }
                        return None;
                    }
                }
            }
            return None;
        }

        // Process a backward chaining step
        if stepx.result.intersects(goal_reg) {
            let stepy = stepx.restrict_result_region(goal_reg);

            let required_change = SomeChange::region_to_region(from_reg, &stepy.initial);

            // Get steps, check if steps include all changes needed.
            if let Some(steps_str) = self.get_steps(&required_change) {
                // Get vector ofactions steps for each bit change.
                if let Some(steps_by_change_vov) =
                    self.get_steps_by_bit_change(&steps_str, &required_change)
                {
                    if let Some(prev_steps) = self.random_depth_first_search2(
                        from_reg,
                        &stepy.initial,
                        &steps_str,
                        &steps_by_change_vov,
                        depth - 1,
                    ) {
                        if let Some(final_plan) =
                            prev_steps.link(&SomePlan::new_with_step(self.num, stepy))
                        {
                            return Some(final_plan);
                        }
                        return None;
                    }
                }
            }
            return None;
        }

        // Must be an asymmetric step
        self.asymmetric_chaining(from_reg, goal_reg, stepx, depth - 1)
    } // end random_depth_first_search2

    /// Do asymmetric chaining.
    ///
    /// Two plans need to be made:
    ///   From the from_region to the initial region of a step.
    ///   From the result region of a step to the goal.
    ///
    /// One plan may involve more restrictions, less flexibility, than the other.
    ///
    /// So randomly choose which plan to make first.
    ///
    fn asymmetric_chaining(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        stepx: &SomeStep,
        depth: usize,
    ) -> Option<SomePlan> {
        if rand::random::<bool>() {
            let required_change = SomeChange::region_to_region(from_reg, &stepx.initial);

            let Some(steps_str) = self.get_steps(&required_change) else {
                return None;
            };

            let Some(steps_by_change_vov) = self.get_steps_by_bit_change(&steps_str, &required_change) else {
                return None;
            };

            let Some(to_step_plan) = self.random_depth_first_search2(
                    from_reg,
                    &stepx.initial,
                    &steps_str,
                    &steps_by_change_vov,
                    depth,
                ) else {
                    return None;
                };

            // Get steps, check if steps include all changes needed.
            let stepy = stepx.restrict_initial_region(to_step_plan.result_region());

            let required_change = SomeChange::region_to_region(&stepy.result, goal_reg);

            // Get steps, check if steps include all changes needed.
            let Some(steps_str) = self.get_steps(&required_change) else {
                return None;
            };

            let Some(steps_by_change_vov) = self.get_steps_by_bit_change(&steps_str, &required_change) else {
                return None;
            };

            let Some(from_step_plan) = self.random_depth_first_search2(
                &stepy.result,
                goal_reg,
                &steps_str,
                &steps_by_change_vov,
                depth,
            ) else {
                return None;
            };

            let Some(plan_part1) = to_step_plan.link(&SomePlan::new_with_step(self.num, stepy)) else { return None; };

            let Some(final_plan) = plan_part1.link(&from_step_plan) else { return None; };

            //println!(
            //    "Asym 1 OK from {} to {} to {} is {}",
            //    from_reg, stepx, goal_reg, plan_found
            //);

            return Some(final_plan);
        }

        // Try step result to goal first.
        let required_change = SomeChange::region_to_region(&stepx.result, goal_reg);

        let Some(steps_str) = self.get_steps(&required_change) else {
            return None;
        };

        let Some(steps_by_change_vov) = self.get_steps_by_bit_change(&steps_str, &required_change) else {
            return None;
        };

        let Some(from_step_plan) = self.random_depth_first_search2(
                &stepx.result,
                goal_reg,
                &steps_str,
                &steps_by_change_vov,
                depth,
            ) else {
                return None;
            };

        // Get steps, check if steps include all changes needed.
        let stepy = stepx.restrict_result_region(from_step_plan.initial_region());

        let required_change = SomeChange::region_to_region(from_reg, &stepy.initial);

        // Get steps, check if steps include all changes needed.
        let Some(steps_str) = self.get_steps(&required_change) else {
            return None;
        };

        let Some(steps_by_change_vov) = self.get_steps_by_bit_change(&steps_str, &required_change) else {
            return None;
        };

        let Some(to_step_plan) = self.random_depth_first_search2(
            from_reg,
            &stepy.initial,
            &steps_str,
            &steps_by_change_vov,
            depth,
        ) else {
            return None;
        };

        let Some(plan_part1) = to_step_plan.link(&SomePlan::new_with_step(self.num, stepy)) else { return None; };

        let Some(final_plan) = plan_part1.link(&from_step_plan) else { return None; };

        //println!(
        //    "Asym 2 OK from {} to {} to {} is {}",
        //    from_reg, stepx, goal_reg, plan_found
        //);
        Some(final_plan)
    }

    /// Make a plan to change the current state to another region.
    /// Since there are some random choices, it may be useful to try
    /// running make_plan more than once.
    pub fn make_plan(&self, goal_reg: &SomeRegion) -> Option<SomePlan> {
        //println!("make_plan start cur {} goal {}", self.cur_state, goal_reg);

        if goal_reg.is_superset_of_state(&self.cur_state) {
            //println!("no plan needed from {} to {} ?", &self.cur_state, goal_reg);
            return Some(SomePlan::new(self.num));
        }

        let cur_reg = SomeRegion::new(self.cur_state.clone(), self.cur_state.clone());

        // Figure the required change.
        let required_change = SomeChange::region_to_region(&cur_reg, goal_reg);

        // Get steps, check if steps include all changes needed.
        let Some(steps_str) = self.get_steps(&required_change) else { return None; };

        // Get vector of steps for each bit change.
        let Some(steps_by_change_vov) = self.get_steps_by_bit_change(&steps_str, &required_change) else { return None; };

        // Tune maximum depth to be a multiple of the number of bit changes required.
        let num_depth = 3 * required_change.number_changes();

        // steps_str, and steps_by_change_vov, are calculated ahead so that thay don't have to be
        // recalculated for each run, below, of random_depth_first_search.
        let mut plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| {
                self.random_depth_first_search(
                    &cur_reg,
                    goal_reg,
                    &steps_str,
                    &steps_by_change_vov,
                    num_depth,
                )
            })
            .collect::<Vec<SomePlan>>();

        // Check for failure.
        if plans.is_empty() {
            return None;
        }

        // Return one of the plans, avoiding the need to clone.
        if plans.len() == 1 {
            //println!("make_plan returned plan");
            return Some(plans.remove(0));
        }

        //println!("make_plan returned plan");
        let inx = self.choose_a_plan(&plans);
        Some(plans.remove(inx))
    } // end make plan

    /// Get steps that may allow a change to be made.
    ///
    /// Steps that make at least one of the needed changes are extracted from domain actions.
    ///
    /// A rule that changes X->x will be pruned to make a step that changes 0->1 or 1->0 depending
    /// on the change needed. X->0 will be pruned to 1->0.  X->1 will be pruned to 0->1.
    ///
    /// If all steps found, in aggregate, cannot change all bits needed, return None.
    ///
    fn get_steps(&self, required_change: &SomeChange) -> Option<StepStore> {
        // Check if changes are possible.

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str: StepStore = self.actions.get_steps(required_change);
        //println!("\nrandom_depth_first_search2: from {} to {} steps_str steps {}", from_reg, goal_reg, steps_str.formatted_string(" "));

        // Check that the steps roughly encompass all needed changes, else return None.
        if let Some(can_change) = steps_str.aggregate_changes() {
            if required_change.is_subset_of(&can_change) {
            } else {
                //println!("get_steps: step_vec wanted changes {} are not a subset of step_vec changes {}, returning None", &required_change, &can_change);
                return None;
            }
        } else {
            return None;
        }

        Some(steps_str)
    }

    /// Given steps are split up into a vector of step vectors, each step vector has steps that change a
    /// particular needed-change bit.  Often, there will only be one step in a step vector.
    ///
    /// Steps that make more than one needed bit change will be in multiple step vectors.
    ///
    /// All possible step vector pairs are checked for being mutually exclusive, that is the needed change
    /// needs to be backed off to run any step in the other step vector.  If any such pair is found, return None.
    ///
    /// Any pair of vectors that have the same step, due to multiple bit changes, will not be construed to be
    /// mutually exclusive.
    ///
    fn get_steps_by_bit_change<'a>(
        &'a self,
        steps_str: &'a StepStore,
        required_change: &SomeChange,
    ) -> Option<Vec<Vec<&SomeStep>>> {
        // Sort the steps by each needed bit change. (some actions may change more than one bit, so will appear more than once)
        let mut steps_by_change_vov: Vec<Vec<&SomeStep>> =
            steps_str.steps_by_change_bit(required_change);

        // Check if any pair of single-bit changes, all steps, are mutually exclusive.
        if any_mutually_exclusive_changes(&steps_by_change_vov, required_change) {
            //println!("forward_depth_first_search2: mutually exclusive change rules found");
            return None;
        }

        // Check for steps that should be done after all the others.
        // To void some backtracking.
        if steps_by_change_vov.len() > 1 {
            let inxs: Vec<usize> = do_later_changes(&steps_by_change_vov, required_change);
            for inx in inxs.iter() {
                remove_unordered(&mut steps_by_change_vov, *inx);
            }
        }

        Some(steps_by_change_vov)
    }

    /// Change the current state to be within a given region.
    /// Return True if the change succeeds.
    pub fn to_region(&mut self, goal_region: &SomeRegion) -> bool {
        if goal_region.is_superset_of_state(&self.cur_state) {
            return true;
        }

        if let Some(pln) = self.make_plan(goal_region) {
            // Do the plan
            self.run_plan(&pln);
            goal_region.is_superset_of_state(&self.cur_state)
        } else {
            false
        }
    } // end to_region

    /// Return a Region from a string.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn region_from_string(&self, str: &str) -> Result<SomeRegion, String> {
        SomeRegion::new_from_string(self.cur_state.num_ints(), str)
    } // end region_from_string

    /// Return a Region from a string.
    /// Left-most, consecutive, ommitted zeros are assumed tobe X.
    pub fn region_from_string_pad_x(&self, str: &str) -> Result<SomeRegion, String> {
        SomeRegion::new_from_string_pad_x(self.cur_state.num_ints(), str)
    } // end region_from_string

    /// Return a State from a string.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn state_from_string(&self, str: &str) -> Result<SomeState, String> {
        SomeState::new_from_string(self.cur_state.num_ints(), str)
    } // end state_from_string

    /// Return a Action number from a string with a format that the parse method can understand.
    /// Left-most, consecutive, zeros can be omitted.
    /// Returns an error if the string is bad or no action exists of that number.
    pub fn act_num_from_string(&self, str_num: &str) -> Result<usize, String> {
        match str_num.parse::<usize>() {
            Ok(act_num) => {
                if act_num >= self.actions.len() {
                    return Err(format!("Action number too large {}", act_num));
                }
                Ok(act_num)
            }
            Err(error) => Err(format!("\nDid not understand action number, {}", error)),
        }
    } // end act_num_from_string

    /// Return the index value of a chosen Plan
    /// TODO better criteria.
    fn choose_a_plan(&self, ret_plans: &Vec<SomePlan>) -> usize {
        assert!(!ret_plans.is_empty());

        if ret_plans.len() == 1 {
            return 0;
        }

        // Find plan maximum length
        let mut max_len = 0;
        for rets in ret_plans.iter() {
            if rets.len() > max_len {
                max_len = rets.len();
            }
        } // next rets

        //println!("ret_plans len = {} min {} max {}", ret_plans.len(), &min_len, &max_len);

        // Rate plans, highest rate is best.
        let rates_vec: Vec<(usize, usize)> = ret_plans
            .par_iter()
            .enumerate() // par_iter for parallel, .iter for easier reading of diagnostic messages
            .map(|(inx, planx)| (inx, self.rate_a_plan(planx)))
            .collect::<Vec<(usize, usize)>>();

        // Find max rate
        let mut max_rate = 0;
        for rtx in rates_vec.iter() {
            if rtx.1 > max_rate {
                max_rate = rtx.1;
            }
        }

        // Gather highest rated plan indexes
        let mut inx_ary = Vec::<usize>::new();

        for rtx in rates_vec.iter() {
            if rtx.1 == max_rate {
                inx_ary.push(rtx.0);
            }
        }

        // Choose a step
        inx_ary[rand::thread_rng().gen_range(0..inx_ary.len())]
    } // end choose_a_plan

    // Rate a given plan.
    // Length of plan is rated inversely.
    // Return an integer, higher is better.
    fn rate_a_plan(&self, planx: &SomePlan) -> usize {
        // Rate the length of the plan
        let mut rate = 1000 - planx.len();

        // Check for steps that may have two results
        for stpx in planx.iter() {
            if stpx.alt_rule {
                if let Some(sqrx) = self.actions[stpx.act_num]
                    .squares
                    .find(&stpx.initial.state1)
                {
                    // For an alternating square, you want the most recent result to be NEQ the desired next result.
                    if *sqrx.most_recent_result() == stpx.result.state1 {
                        if rate > 4 {
                            rate -= 5;
                        } else {
                            rate = 0;
                        }
                    }
                } else if rate > 1 {
                    rate -= 2;
                } else {
                    rate = 0;
                }
            }
        }
        rate
    }

    /// Return all changes that can be made,
    fn aggregate_changes(&self) -> SomeChange {
        self.actions.aggregate_changes(&self.cur_state)
    }
} // end impl SomeDomain

/// Return true if two references are identical, thanks to
/// github.com/rust-lang/rfcs/issues/1155, (eddyb, kimundi and RalfJung)
fn ptr_eq<T>(a: *const T, b: *const T) -> bool {
    a == b
}

/// Return true if any single-bit change vector pairs are all mutually exclusive
fn any_mutually_exclusive_changes(by_change: &Vec<Vec<&SomeStep>>, wanted: &SomeChange) -> bool {
    for inx in 0..(by_change.len() - 1) {
        for iny in (inx + 1)..by_change.len() {
            //println!("mex checking {:?} and {:?}", &by_change[inx], &by_change[iny]);
            if all_mutually_exclusive_changes(&by_change[inx], &by_change[iny], wanted) {
                return true;
            }
        } // next iny
    } // next inx

    false
}

/// Return true if all combinations of steps are mutually exclusive
fn all_mutually_exclusive_changes(
    vec_x: &[&SomeStep],
    vec_y: &[&SomeStep],
    wanted: &SomeChange,
) -> bool {
    for refx in vec_x.iter() {
        for refy in vec_y.iter() {
            if ptr_eq(*refx, *refy) {
                return false;
            }
            if refx.mutually_exclusive(refy, wanted) {
                //println!("step {} mutually exclusive to step {}", refx, refy);
            } else {
                return false;
            }
        } //next refy
    } // next refx
    true
}

/// Return a vector of reverse sorted indicies, of step vectors that should be done later
/// than all other steps.
fn do_later_changes(by_change: &Vec<Vec<&SomeStep>>, wanted: &SomeChange) -> Vec<usize> {
    let mut inxs = Vec::<usize>::new();

    'next_inx: for inx in 0..by_change.len() {
        for iny in 0..by_change.len() {
            if iny == inx {
                continue;
            }
            if !step_vecs_order_bad(&by_change[inx], &by_change[iny], wanted) {
                continue 'next_inx;
            }
        } // next iny

        inxs.push(inx);
    } //next inx

    if inxs.len() > 1 {
        inxs.sort_by(|a, b| b.cmp(a));
    }
    inxs
}

/// Return true if the order of step vectors, arg one to arg two, is bad.
fn step_vecs_order_bad(
    vec_x: &Vec<&SomeStep>,
    vec_y: &Vec<&SomeStep>,
    wanted: &SomeChange,
) -> bool {
    assert!(!vec_x.is_empty());
    assert!(!vec_y.is_empty());
    for refx in vec_x.iter() {
        for refy in vec_y.iter() {
            if ptr_eq(*refx, *refy) {
                return false;
            }
            if !refx.rule.order_bad(&refy.rule, wanted) {
                return false;
            }
        } //next refy
    } // next refx

    true
}

// Return the first state value.
pub fn initialize_state(num_ints: usize) -> SomeState {
    SomeState::new(SomeBits::new_random(num_ints))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::bits_xor;

    // Test a simple four-step plan to change the domain current state
    // from s0111 to s1000.
    #[test]
    fn make_plan_direct() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();

        let s0 = dm0.state_from_string("s0b0").unwrap();
        let sf = dm0.state_from_string("s0b1111").unwrap();

        let s1 = SomeBits::new_from_string(1, "0x01").unwrap();
        let s2 = SomeBits::new_from_string(1, "0x02").unwrap();
        let s4 = SomeBits::new_from_string(1, "0x04").unwrap();
        let s8 = SomeBits::new_from_string(1, "0x08").unwrap();

        // Create group for region XXXX, Act 0.
        dm0.eval_sample_arbitrary(0, &s0, &SomeState::new(bits_xor(&s0, &s1)));
        dm0.eval_sample_arbitrary(0, &sf, &SomeState::new(bits_xor(&sf, &s1)));

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(1, &s0, &SomeState::new(bits_xor(&s0, &s2)));
        dm0.eval_sample_arbitrary(1, &sf, &SomeState::new(bits_xor(&sf, &s2)));

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(2, &s0, &SomeState::new(bits_xor(&s0, &s4)));
        dm0.eval_sample_arbitrary(2, &sf, &SomeState::new(bits_xor(&sf, &s4)));

        // Create group for region XXXX, Act 3.
        dm0.eval_sample_arbitrary(3, &s0, &SomeState::new(bits_xor(&s0, &s8)));
        dm0.eval_sample_arbitrary(3, &sf, &SomeState::new(bits_xor(&sf, &s8))); // Last sample changes current state to s0111

        // Get plan for 7 to 8
        dm0.set_state(&dm0.state_from_string("s0b111").unwrap());
        let mut toreg = dm0.region_from_string("r1000").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 4);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err(String::from("no plan found to r1000?"));
        }

        // Get plan for 8 to 7
        dm0.set_state(&dm0.state_from_string("s0b1000").unwrap());
        toreg = dm0.region_from_string("r111").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 4);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err(String::from("no plan found to r111?"));
        }

        Ok(())
    }

    // Test asymmetric chaining.  The plan must step out of the direct
    // glide path X1XX, between 7 and C, into X0XX, to change the third bit,
    // then step back into the glide path to get to the goal.
    #[test]
    fn make_plan_asymmetric() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();

        let s0 = dm0.state_from_string("s0b0").unwrap();
        let sf = dm0.state_from_string("s0b1111").unwrap();
        let sb = dm0.state_from_string("s0b1011").unwrap();

        let s1 = SomeBits::new_from_string(1, "0x01").unwrap();
        let s2 = SomeBits::new_from_string(1, "0x02").unwrap();
        let s4 = SomeBits::new_from_string(1, "0x04").unwrap();
        let s8 = SomeBits::new_from_string(1, "0x08").unwrap();

        // Create group for region XXXX, Act 0.
        dm0.eval_sample_arbitrary(0, &s0, &SomeState::new(bits_xor(&s0, &s1)));
        dm0.eval_sample_arbitrary(0, &sf, &SomeState::new(bits_xor(&sf, &s1)));

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(1, &s0, &SomeState::new(bits_xor(&s0, &s2)));
        dm0.eval_sample_arbitrary(1, &sf, &SomeState::new(bits_xor(&sf, &s2)));

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(2, &s0, &SomeState::new(bits_xor(&s0, &s4)));
        dm0.eval_sample_arbitrary(2, &sf, &SomeState::new(bits_xor(&sf, &s4)));

        // Create group for region X0XX, Act 3.
        dm0.eval_sample_arbitrary(3, &s0, &SomeState::new(bits_xor(&s0, &s8)));
        dm0.eval_sample_arbitrary(3, &sb, &SomeState::new(bits_xor(&sb, &s8)));

        println!("\nActs: {}", &dm0.actions);

        // Get plan for 7 to C
        dm0.set_state(&dm0.state_from_string("s0b111").unwrap());
        let mut toreg = dm0.region_from_string("r1100").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 5);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err(String::from("No plan found s111 to r1100?"));
        }

        // Get plan for C to 7
        dm0.set_state(&dm0.state_from_string("s0b1100").unwrap());
        toreg = dm0.region_from_string("r111").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 5);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err(String::from("No plan found s1100 to r111?"));
        }

        Ok(())
    }

    // Test action:get_needs StateNotInGroup, two flavors.
    #[test]
    fn need_for_state_not_in_group() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0]
            .state_not_in_group_needs(&dm0.cur_state, &VecDeque::<SomeState>::new());

        assert!(nds1.len() == 1);
        assert!(
            nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap())
        );

        // Create group for one sample
        let s1 = dm0.state_from_string("s0b1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1").unwrap())
        {
        } else {
            return Err(String::from("Group r1 not found ??"));
        }

        println!("\nActs: {}", &dm0.actions[0]);

        // Invalidate group for sample 1 by giving it GT 1 different result.
        // Current state changes to zero.
        let s1 = dm0.state_from_string("s0b1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &SomeState::new(SomeBits::new_low(1)));

        println!("\nActs: {}", &dm0.actions[0]);

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1").unwrap())
        {
            return Err(String::from("Group r1  found ??"));
        }

        // Check needs for pn > 1 and not in group, and current state not in a group.
        let nds1 = dm0.get_needs();

        assert!(nds1.len() == 2);
        assert!(
            nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap())
        );
        assert!(
            nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r0").unwrap())
        );

        // Err(String::from("Done"));
        Ok(())
    }

    // Test confirm_group_needs.
    #[test]
    fn need_additional_group_state_samples() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0]
            .state_not_in_group_needs(&dm0.cur_state, &VecDeque::<SomeState>::new());

        assert!(nds1.len() == 1);
        assert!(
            nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap())
        );

        // Create group for one sample
        let s1 = dm0.state_from_string("s0b1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1").unwrap())
        {
        } else {
            return Err(String::from("Group r1 not found ??"));
        }

        // Expand group
        let s2 = dm0.state_from_string("s0b10").unwrap();
        dm0.eval_sample_arbitrary(0, &s2, &s2);

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("rXX").unwrap())
        {
        } else {
            return Err(String::from("Group rXX not found ??"));
        }

        let nds2 = dm0.actions[0].confirm_group_needs();
        println!("needs {}", nds2);

        assert!(nds2.len() == 2);
        assert!(nds2.contains_similar_need("ConfirmGroup", &dm0.region_from_string("r1").unwrap()));
        assert!(nds2.contains_similar_need("ConfirmGroup", &dm0.region_from_string("r10").unwrap()));

        // Satisfy one need.
        dm0.eval_sample_arbitrary(0, &s2, &s2);

        let nds3 = dm0.actions[0].confirm_group_needs();
        //println!("needs {}", nds3);

        assert!(nds3.len() == 1);
        assert!(nds3.contains_similar_need("ConfirmGroup", &dm0.region_from_string("r1").unwrap()));

        // Satisfy second need.
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        let nds4 = dm0.actions[0].confirm_group_needs();
        //println!("needs {}", nds4);

        // Check for no more needs.
        assert!(nds4.len() == 0);

        Ok(())
    }

    // Seek a part of a group intersection that is contradictory.
    // Group X1XX intersects group XX0X at X10X.
    // The intersection is not wholly contradictory, as would be expected due to
    // the two groups being defined by a square, D, in X10X.
    // The region X100 (4, C) in X10X is the contradictory part due to
    // different results expected from the least significant bit.
    #[test]
    fn need_for_sample_in_contradictory_intersection() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();

        let s00 = dm0.state_from_string("s0b0").unwrap();
        let s01 = dm0.state_from_string("s0b01").unwrap();
        let s06 = dm0.state_from_string("s0b110").unwrap();
        let s0d = dm0.state_from_string("s0b1101").unwrap();

        // Create group for region XX0X.
        dm0.eval_sample_arbitrary(0, &s00, &s01);
        dm0.eval_sample_arbitrary(0, &s00, &s01);

        dm0.eval_sample_arbitrary(0, &s0d, &s0d);
        dm0.eval_sample_arbitrary(0, &s0d, &s0d);

        // Create group X1XX
        dm0.eval_sample_arbitrary(0, &s06, &s06);
        dm0.eval_sample_arbitrary(0, &s06, &s06);

        // Get and check needs.
        let nds1 = dm0.actions.avec[0].group_pair_needs();
        assert!(nds1.len() == 1);
        assert!(nds1.contains_similar_need(
            "ContradictoryIntersection",
            &dm0.region_from_string("rX100").unwrap()
        ));

        Ok(())
    }

    #[test]
    fn limit_group_needs() -> Result<(), String> {
        // Init domain with one action.
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();

        // Set up group XXXX_XX0X->XXXX_XX0X
        let s04 = dm0.state_from_string("s0b00000100").unwrap();
        dm0.eval_sample_arbitrary(0, &s04, &s04);
        dm0.eval_sample_arbitrary(0, &s04, &s04);

        let sf9 = dm0.state_from_string("s0b11111001").unwrap();
        dm0.eval_sample_arbitrary(0, &sf9, &sf9);
        dm0.eval_sample_arbitrary(0, &sf9, &sf9);

        println!("dm0 {}", &dm0.actions[0]);

        // Set group pnc
        let grp_reg = dm0.region_from_string("rXXXX_XX0X").unwrap();
        dm0.actions[0].set_group_pnc(&grp_reg);
        println!("dm0 {}", &dm0.actions[0]);

        let nds1 = dm0.actions[0].limit_groups_needs(&SomeChange {
            b01: SomeMask::new_from_string(1, "m0b1111").unwrap(),
            b10: SomeMask::new_from_string(1, "m0b1111").unwrap(),
        });
        println!("needs1 are {}", nds1);

        let mut found = false;
        for needx in nds1.iter() {
            match needx {
                SomeNeed::SetGroupAnchor { group_region, .. } => {
                    if *group_region == grp_reg {
                        found = true;
                    }
                }
                _ => (),
            }
        }
        assert!(found);

        dm0.actions[0].set_group_anchor(&grp_reg, &s04);
        println!("dm0 {}", &dm0.actions[0]);

        let nds2 = dm0.actions[0].limit_groups_needs(&SomeChange {
            b01: SomeMask::new_from_string(1, "m0b1111").unwrap(),
            b10: SomeMask::new_from_string(1, "m0b1111").unwrap(),
        });
        println!("needs2 are {}", nds2);

        let s06 = dm0.state_from_string("s0b00000110").unwrap();
        assert!(
            nds2.contains_similar_need("LimitGroupAdj", &SomeRegion::new(s06.clone(), s06.clone()))
        );

        let s02 = dm0.state_from_string("s0b00000010").unwrap();
        dm0.eval_sample_arbitrary(0, &s06, &s02);
        dm0.eval_sample_arbitrary(0, &s06, &s02);

        println!("dm0 {}", &dm0.actions[0]);
        let nds3 = dm0.actions[0].limit_groups_needs(&SomeChange {
            b01: SomeMask::new_from_string(1, "m0b1111").unwrap(),
            b10: SomeMask::new_from_string(1, "m0b1111").unwrap(),
        });
        println!("needs3 are {}", nds3);

        let mut found = false;
        for needx in nds3.iter() {
            match needx {
                SomeNeed::SetGroupLimited { group_region, .. } => {
                    if *group_region == grp_reg {
                        found = true;
                    }
                }
                _ => (),
            }
        }
        assert!(found);

        Ok(())
    }

    /// Form a group, X1X1 from two squares that have alternating (pn=Two) results.
    ///
    /// Sample a square, 0111, in the group, once.  There should be no change.
    ///
    /// Sample the square a second time, with the same result, proving it cannot have an
    /// alternting result.
    ///
    /// Then group X1X1 should be invalidatprintln!("\nActs: {}", &dm0.actions);ed and removed.
    /// **********************************************************************************
    #[test]
    fn group_pn_2_union_then_invalidation() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();

        let s5 = dm0.state_from_string("s0b101").unwrap();

        let s4 = dm0.state_from_string("s0b100").unwrap();

        let sf = dm0.state_from_string("s0b1111").unwrap();

        let se = dm0.state_from_string("s0b1110").unwrap();

        let s7 = dm0.state_from_string("s0b111").unwrap();

        let rx1x1 = dm0.region_from_string("rx1x1").unwrap();

        dm0.eval_sample_arbitrary(0, &s5, &s5);
        dm0.eval_sample_arbitrary(0, &s5, &s4);
        dm0.eval_sample_arbitrary(0, &s5, &s5);
        dm0.eval_sample_arbitrary(0, &s5, &s4);

        dm0.eval_sample_arbitrary(0, &sf, &se);
        dm0.eval_sample_arbitrary(0, &sf, &sf);
        dm0.eval_sample_arbitrary(0, &sf, &se);
        dm0.eval_sample_arbitrary(0, &sf, &sf);

        println!("\nActs: {}", &dm0.actions[0]);

        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
            dm0.eval_sample_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
                dm0.eval_sample_arbitrary(0, &s7, &s7); // cause not-pn=2 condition

                if let Some(_) = dm0.actions[0].groups.find(&rx1x1) {
                    //println!("\nActs: {}", &dm0.actions[0]);
                    //println!(" {}", dm0.actions[0].squares);
                    return Err(String::from("failed, rx1x1 should have been deleted"));
                } else {
                    //println!("\nActs: {}", &dm0.actions[0]);
                    //println!("       Sqrs: ({})", dm0.actions[0].squares);
                    //return Ok(());
                }
            } else {
                //println!("\nActs: {}", &dm0.actions[0]);
                //println!("       Sqrs: ({})", dm0.actions[0].squares);
                //println!("Group deleted too soon!");
                return Err(String::from("failed, rx1x1 deleted too soon"));
            }
        } else {
            //println!("\nActs: {}", &dm0.actions[0]);
            return Err(String::from(
                "failed, group rx1x1 was not formed by two squares",
            ));
        }
        // Err(String::from("Done!"))
        Ok(())
    } // end group_pn_2_union_then_invalidation

    // Form a group, X1X1 from two squares that have unpredictable results.
    //
    // Sample a square, 0111, in the group, several times.  There should be no change until pnc (4 samples).
    //
    // Then group X1X1 should be invalidate and removed.
    // **********************************************************************************
    #[test]
    fn group_pn_u_union_then_invalidation() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();

        let s5 = dm0.state_from_string("s0b101").unwrap();

        let s4 = dm0.state_from_string("s0b100").unwrap();

        let sf = dm0.state_from_string("s0b1111").unwrap();

        let se = dm0.state_from_string("s0b1110").unwrap();

        let s7 = dm0.state_from_string("s0b111").unwrap();

        let rx1x1 = dm0.region_from_string("rx1x1").unwrap();

        //println!(
        //    "state 5 = {} s4 {} sF {} sE {} rxx1x1 {}",
        //    s5, s4, sf, se, rx1x1
        //);
        dm0.eval_sample_arbitrary(0, &s5, &s5);
        dm0.eval_sample_arbitrary(0, &s5, &s4);
        dm0.eval_sample_arbitrary(0, &s5, &se);

        dm0.eval_sample_arbitrary(0, &sf, &se);
        dm0.eval_sample_arbitrary(0, &sf, &sf);
        dm0.eval_sample_arbitrary(0, &sf, &s4);

        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
            println!("\nActs: {}", &dm0.actions[0]);
            dm0.eval_sample_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
                dm0.eval_sample_arbitrary(0, &s7, &s7); // cause pn-not-Two invalidation
                if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
                    //println!("\nActs: {}", &dm0.actions[0]);
                    //println!(" {}", dm0.actions[0].get_squares());
                    return Err(String::from(
                        "Four samples for s7 failed to invalidate group xx1x1",
                    ));
                } else {
                    return Ok(());
                }
            } else {
                //println!("\nActs: {}", &dm0.actions[0]);
                //println!("       Sqrs: ({})", dm0.actions[0].get_squares());
                return Err(String::from("Group deleted too soon"));
            }
        } else {
            //println!("\nActs: {}", &dm0.actions[0]);
            return Err(String::from("group rx1x1 was not formed by two squares!"));
        }
    } // end group_pn_u_union_then_invalidation

    // For showing something easily understandable, the groups in the program are shown
    // with four, or fewer, edges.
    // It is important to show that any arbitrary number of edges can form a group / rule.
    #[test]
    fn create_group_rule_with_ten_edges() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 2);
        dm0.cur_state = SomeState::new_from_string(2, "s0b1").unwrap();
        dm0.add_action();

        let bts16 = SomeBits::new_from_string(2, "0x10").unwrap();

        let s0 = dm0.state_from_string("s0b0001010010101000").unwrap();
        let s1 = dm0.state_from_string("s0b1111010110101011").unwrap();
        // Region                        XXX1010X101010XX.

        // Create group for region XXX1010X101010XX.
        dm0.eval_sample_arbitrary(0, &s0, &SomeState::new(bits_xor(&s0, &bts16)));
        dm0.eval_sample_arbitrary(0, &s1, &SomeState::new(bits_xor(&s1, &bts16)));

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("rXXX1010X101010XX").unwrap())
        {
        } else {
            return Err(String::from("Group rXXX1010X101010XX not found ??"));
        }

        Ok(())
    }

    #[test]
    fn compatible_group_intersection_needs() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();

        let s0 = dm0.state_from_string("s0b000").unwrap();
        let s3 = dm0.state_from_string("s0b011").unwrap();
        let s5 = dm0.state_from_string("s0b101").unwrap();

        dm0.eval_sample_arbitrary(0, &s0, &s0);
        dm0.eval_sample_arbitrary(0, &s3, &s3);
        dm0.eval_sample_arbitrary(0, &s5, &s5);

        println!("\nActs: {}", &dm0.actions[0]);

        if let Some(grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r00XX").unwrap())
        {
            if let Some(grpy) = dm0.actions[0]
                .groups
                .find(&dm0.region_from_string("r0XX1").unwrap())
            {
                let nds1 = dm0.actions.avec[0].group_pair_intersection_needs(&grpx, &grpy);
                println!("needs {}", &nds1);
                assert!(nds1.contains_similar_need(
                    "AStateMakeGroup",
                    &dm0.region_from_string("r100").unwrap()
                ));
            } else {
                return Err(String::from("Group r0XX1 not found ??"));
            }
        } else {
            return Err(String::from("Group r0X0X not found ??"));
        }
        // Err(String::from("Done!"))
        Ok(())
    }

    #[test]
    fn seek_edge_needs() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();

        // Establish group XXXX
        let s0 = dm0.state_from_string("s0b0000").unwrap();
        let sf = dm0.state_from_string("s0b1111").unwrap();

        dm0.eval_sample_arbitrary(0, &s0, &s0);
        dm0.eval_sample_arbitrary(0, &sf, &sf);
        // Establish pnc for group XXXX.
        dm0.eval_sample_arbitrary(0, &s0, &s0);
        dm0.eval_sample_arbitrary(0, &sf, &sf);

        println!("\nActs: {}", &dm0.actions);

        // Break group XXXX
        let s5 = dm0.state_from_string("s0b0101").unwrap();

        dm0.eval_sample_arbitrary(0, &s5, &s0);

        println!("\nActs: {}", &dm0.actions);
        assert!(dm0.actions[0]
            .seek_edge
            .contains(&dm0.region_from_string("rx1x1").unwrap()));

        let needs = dm0.actions[0].seek_edge_needs();
        println!("needs1: {}", &needs);

        assert!(needs.len() == 1);
        assert!(needs.contains_similar_need("SeekEdge", &dm0.region_from_string("r0101").unwrap()));

        dm0.eval_sample_arbitrary(0, &s5, &s0);

        let needs = dm0.actions[0].seek_edge_needs();
        println!("needs2: {}", &needs);
        assert!(needs.len() == 1);
        if needs.contains_similar_need("SeekEdge", &dm0.region_from_string("r1101").unwrap()) {
            // Seek even closer sample s1101
            let sd = dm0.state_from_string("s0b1101").unwrap();
            dm0.eval_sample_arbitrary(0, &sd, &s0);
            let needs = dm0.actions[0].seek_edge_needs();
            println!("needs3: {}", &needs);
            assert!(needs.len() == 1);
            assert!(
                needs.contains_similar_need("SeekEdge", &dm0.region_from_string("r1101").unwrap())
            );

            dm0.eval_sample_arbitrary(0, &sd, &s0);
            let needs = dm0.actions[0].seek_edge_needs();
            println!("needs4: {}", &needs);
            let mut found = false;
            let regexp = dm0.region_from_string("r11x1").unwrap();
            for needx in needs.iter() {
                match needx {
                    SomeNeed::AddSeekEdge { reg } => {
                        if *reg == regexp {
                            found = true;
                        }
                    }
                    _ => (),
                }
            }
            assert!(found);

            // At the next run of get_needs, r11x1 will replace the superset region rx1x1, then
            // r11x1 will be deleted because its defining squares are adjacent.
        } else if needs.contains_similar_need("SeekEdge", &dm0.region_from_string("r0111").unwrap())
        {
            // Seek even closer sample s0111
            let s7 = dm0.state_from_string("s0b0111").unwrap();
            dm0.eval_sample_arbitrary(0, &s7, &s0);
            let needs = dm0.actions[0].seek_edge_needs();
            println!("needs3: {}", &needs);
            assert!(needs.len() == 1);
            assert!(
                needs.contains_similar_need("SeekEdge", &dm0.region_from_string("r0111").unwrap())
            );

            dm0.eval_sample_arbitrary(0, &s7, &s0);
            let needs = dm0.actions[0].seek_edge_needs();
            println!("needs4: {}", &needs);
            let mut found = false;
            let regexp = dm0.region_from_string("rx111").unwrap();
            for needx in needs.iter() {
                match needx {
                    SomeNeed::AddSeekEdge { reg } => {
                        if *reg == regexp {
                            found = true;
                        }
                    }
                    _ => (),
                }
            } // Set up a limited group.

            assert!(found);

            // At the next run of get_needs, rx111 will replace the superset region rx1x1, then
            // rx111 will be deleted because its defining squares are adjacent.
        } else {
            panic!("unexpected need!");
        }

        let needs = dm0.get_needs();
        assert!(dm0.actions[0].seek_edge.len() == 0);

        println!("\nActs: {}", &dm0.actions);
        println!("needs9: {}", &needs);

        Ok(())
    }

    #[test]
    fn astate_make_group() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1").unwrap();
        dm0.add_action();

        // Sample 1
        let sdb = dm0.state_from_string("s0b1101_1011").unwrap();
        let sd9 = dm0.state_from_string("s0b1101_1001").unwrap();
        let ndx = SomeNeed::StateNotInGroup {
            dom_num: 0,
            act_num: 0,
            target_state: sdb.clone(),
        };
        dm0.actions[0].eval_need_sample(&sdb, &ndx, &sd9, 0);

        // Sample 2
        let se5 = dm0.state_from_string("s0b1110_0101").unwrap();
        let se7 = dm0.state_from_string("s0b1110_0111").unwrap();
        let ndx = SomeNeed::StateNotInGroup {
            dom_num: 0,
            act_num: 0,
            target_state: se5.clone(),
        };
        dm0.actions[0].eval_need_sample(&se5, &ndx, &se7, 0);

        // Sample 3
        let s25 = dm0.state_from_string("s0b0010_0101").unwrap();
        let s27 = dm0.state_from_string("s0b0010_0111").unwrap();
        let ndx = SomeNeed::StateNotInGroup {
            dom_num: 0,
            act_num: 0,
            target_state: s25.clone(),
        };
        dm0.actions[0].eval_need_sample(&s25, &ndx, &s27, 0);

        // Sample 4    // Set up a limited group.
        let s2c = dm0.state_from_string("s0b0010_1100").unwrap();
        let s2e = dm0.state_from_string("s0b0010_1110").unwrap();
        let ndx = SomeNeed::StateNotInGroup {
            dom_num: 0,
            act_num: 0,
            target_state: s2c.clone(),
        };
        dm0.actions[0].eval_need_sample(&s2c, &ndx, &s2e, 0);

        // Sample 5
        let sd3 = dm0.state_from_string("s0b1101_0011").unwrap();
        let sd1 = dm0.state_from_string("s0b1101_0001").unwrap();
        let ndx = SomeNeed::AStateMakeGroup {
            dom_num: 0,
            act_num: 0,
            target_state: sd3.clone(),
            for_reg: dm0.region_from_string("rxxxx_xxxx").unwrap(),
            far: dm0.state_from_string("s0b0010_1100").unwrap(),
            num_x: 8,
        };
        dm0.actions[0].eval_need_sample(&sd3, &ndx, &sd1, 0);

        println!("\nActs: {}", &dm0.actions);
        assert!(dm0.actions[0].groups.len() == 1);
        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("rxxxx_xxxx").unwrap())
        {
        } else {
            return Err(String::from("Group rxxxx_xxxx not found ??"));
        }

        //Err(String::from("Done!"))
        Ok(())
    }

    // Create a group with no way to change a non-X bit position, so its initially
    // set to limited.
    //
    // Introduce a new bit position that can change.
    // Group should go from limited = true to limited = false.
    //
    // So the next step would be to seek a sample of an adjacent square, if needed.
    // Assumes groups are added to the end of the group list.
    #[test]
    fn limited_flag_change() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1);
        dm0.cur_state = SomeState::new_from_string(1, "s0b1011").unwrap();
        dm0.add_action();
        dm0.add_action();

        // Create group XXX1 -> XXX1, no way to change any bit.
        let s0b = dm0.state_from_string("s0b1011").unwrap();
        let s05 = dm0.state_from_string("s0b0101").unwrap();

        // Start group.
        dm0.eval_sample_arbitrary(0, &s0b, &s0b);
        dm0.eval_sample_arbitrary(0, &s05, &s05);

        // Confirm group.
        dm0.eval_sample_arbitrary(0, &s0b, &s0b);
        dm0.eval_sample_arbitrary(0, &s05, &s05);

        // Print domain and needs, if needed for error resolution.
        // Also get_needs checks the limited flag for each group.
        println!("{}", dm0.actions[0]);
        let nds = dm0.get_needs();
        println!("needs {}", nds);

        // Limited flag should be true.
        if !dm0.actions[0].groups[0].limited {
            return Err("Limited flag is false?".to_string());
        }

        // Add a way to change bit position 1, 0->1.
        let s09 = dm0.state_from_string("s0b1001").unwrap();
        dm0.eval_sample_arbitrary(1, &s09, &s0b);

        // Print domain and needs, if needed for error resolution.
        // Also get_needs checks the limited flag for each group.
        println!("{}", dm0.actions[0]);
        let nds = dm0.get_needs();
        println!("needs {}", nds);

        // Changing bit position 1 should not affect the limited flag,
        // where the group bit position one is X.
        if !dm0.actions[0].groups[0].limited {
            return Err("Limited flag is false?".to_string());
        }

        // Add a way to change bit position 0, 0->1.
        let s08 = dm0.state_from_string("s0b1000").unwrap();
        dm0.eval_sample_arbitrary(1, &s08, &s09);

        // Print domain and needs, if needed for error resolution.
        // Also get_needs checks the limited flag for each group.
        println!("{}", dm0.actions[0]);
        let nds = dm0.get_needs();
        println!("needs {}", nds);

        // Changing bit position 1 should not affect the limited flag,
        // where the group bit position one is X.
        if !dm0.actions[0].groups[0].limited {
            return Err("Limited flag is false?".to_string());
        }

        // Add a way to change bit position 0, 1->0.
        let s0a = dm0.state_from_string("s0b1010").unwrap();
        dm0.eval_sample_arbitrary(1, &s0b, &s0a);

        // Print domain and needs, if needed for error resolution.
        // Also get_needs checks the limited flag for each group.
        println!("{}", dm0.actions[0]);
        let nds = dm0.get_needs();
        println!("needs {}", nds);

        // Allowing the bit position 0 to change 1->0 should affect the limited flag.
        if dm0.actions[0].groups[0].limited {
            return Err("Limited flag is true?".to_string());
        }

        // Check for limit need.
        for ndx in nds.iter() {
            if ndx.type_string() == "LimitGroupAdj" {
                match ndx {
                    SomeNeed::LimitGroupAdj { anchor, .. } => {
                        if anchor == &s0b || anchor == &s05 {
                            return Ok(());
                        }
                    }
                    _ => (),
                }
            }
        }

        Err("LimitGroupAdj need not found?".to_string())
    }
} // end tests
