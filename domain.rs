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
//! When the current state is in an optimal region, the bordom value is extended if the position
//! is in an intersection of multiple optimal regions.

use crate::action::SomeAction;
use crate::actionstore::ActionStore;
use crate::change::SomeChange;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::state::SomeState;
use crate::step::SomeStep;
use crate::stepstore::StepStore;
use rand::Rng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fmt;

impl fmt::Display for SomeDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("D(ID: ");

        rc_str.push_str(&self.num.to_string());

        rc_str.push_str(&format!(", Current State: {}", &self.cur_state));
        if self.optimal_and_ints.len() > 0 {
            rc_str.push_str(&format!(", Optimal Regions: {}", &self.optimal_and_ints));
        } else {
            rc_str.push_str(&format!(", Optimal Regions: None"));
        }

        rc_str.push_str(")");

        write!(f, "{}", rc_str)
    }
}

pub const MAX_MEMORY: usize = 20; // Max number of recent current states to keep in a circular buffer.

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct SomeDomain {
    /// Domain number.  Index into a higher-level DomainStore.
    pub num: usize,
    /// Number integers making up a bits struct.
    pub num_ints: usize,
    /// Actions the Domain can take.
    pub actions: ActionStore,
    /// The Current, internal, State.
    cur_state: SomeState,
    /// A counter to indicate the number of steps the current state is in the same optimal region
    /// before getting bored.
    pub boredom: usize,
    /// Zero, or more, optimal regions that are sought if there are no needs.
    /// This may be changed from the UI, see the help display for the commands "oa" and "od".
    /// If more than one region, boredom may cause the program to run rules to switch to a different region.
    optimal: RegionStore,
    /// RegionStore to add all possible intersections of the optimal states to discern.
    pub optimal_and_ints: RegionStore,
    /// Memory of recent current states.
    /// Using VecDeque allows memory to be read in most-recent to least-recent order,
    /// with self.memory.iter().
    pub memory: VecDeque<SomeState>,
}

impl SomeDomain {
    /// Return a new domain instance, given the number of integers, the
    /// initial state, the optimal state(s), the index into the higher-level DomainStore.
    pub fn new(dom: usize, num_ints: usize, start_state: SomeState, optimal: RegionStore) -> Self {
        // Check that optimal regions, if any, are the same number of ints.
        for regx in optimal.iter() {
            assert!(regx.num_ints() == num_ints);
        }

        // Set up a domain instance with the correct value for num_ints
        return SomeDomain {
            num: dom,
            num_ints,
            actions: ActionStore::new(),
            cur_state: start_state,
            boredom: 0,
            optimal_and_ints: optimal.and_intersections(),
            optimal: optimal,
            memory: VecDeque::<SomeState>::with_capacity(MAX_MEMORY),
        };
    }

    /// Return a copy of the current , internal, state.
    pub fn get_current_state(&self) -> SomeState {
        self.cur_state.clone()
    }

    /// Add a region, no subsets, to the optimal region store.
    /// Return true if added.
    pub fn add_optimal(&mut self, areg: SomeRegion) -> bool {
        if self.optimal.push_nosubs(areg) {
            self.optimal_and_ints = self.optimal.and_intersections();
            return true;
        }
        false
    }

    /// Delete a region from the optimal region store.
    /// Return true if deleted.
    pub fn delete_optimal(&mut self, areg: &SomeRegion) -> bool {
        if self.optimal.remove_region(areg) {
            self.optimal_and_ints = self.optimal.and_intersections();
            return true;
        }
        false
    }

    /// Add a SomeAction instance to the store.
    pub fn add_action(&mut self) {
        let actx = SomeAction::new(self.actions.len(), self.num_ints);

        self.actions.push(actx);
    }

    /// Return needs gathered from all actions.
    pub fn get_needs(&mut self) -> NeedStore {
        // Get all changes that can be made, to avoid needs that have no chance
        // of being satisfied.
        // Sample the current state, for any reason, does not require any changes.
        if let Some(agg_chgs) = self.actions.aggregate_changes() {
            let mut nst =
                self.actions
                    .get_needs(&self.cur_state, &agg_chgs, self.num, &self.memory);

            if let Some(ndx) = self.check_optimal() {
                nst.push(ndx);
            }

            for ndx in nst.iter_mut() {
                ndx.set_dom(self.num);
            }

            return nst;
        }
        NeedStore::new()
    }

    /// Do functions related to the wish to be in an optimum region.
    /// Increment the boredom duration, if needed.
    /// Return a need to move to another optimal region, if needed.
    pub fn check_optimal(&mut self) -> Option<SomeNeed> {
        // Check if there are no optimal regions.
        if self.optimal_and_ints.len() == 0 {
            return None;
        }

        // If the current state is not in at least one optimal region,
        // return a need to move to an optimal region.
        let sups = self.optimal_and_ints.supersets_of_state(&self.cur_state);
        if sups.len() == 0 {
            let notsups = self
                .optimal_and_ints
                .not_supersets_of_state(&self.cur_state);
            let inx = rand::thread_rng().gen_range(0..notsups.len());
            return Some(SomeNeed::ToRegion {
                dom_num: self.num,
                act_num: 0,
                goal_reg: notsups[inx].clone(),
            });
        }

        // Get the optimal regions the current state is not in.
        let mut notsups = self
            .optimal_and_ints
            .not_supersets_of_state(&self.cur_state);
        if notsups.len() == 0 {
            return None;
        }

        self.boredom += 1;

        if self.boredom > (3 * sups.len()) {
            // Boredom action trigger.

            // Get intersections, if any.
            notsups = notsups.and_intersections();

            println!("\nDom {}: I'm bored lets move to {}", self.num, &notsups);

            // Some optimal regions may not be accessable, a plan to get to the
            // selected region is not yet known.
            // Random selections may allow an accessable region to be chosen, eventually.
            // A previously unaccessable region may be come accessable with new rules.
            let inx = rand::thread_rng().gen_range(0..notsups.len());
            return Some(SomeNeed::ToRegion {
                dom_num: self.num,
                act_num: 0,
                goal_reg: notsups[inx].clone(),
            });
        }

        None
    }

    /// Return the total number of actions.
    pub fn num_actions(&self) -> usize {
        self.actions.len()
    }

    /// Evaluate an arbitrary sample given by the user.
    /// This tends to break things for an action, unless all samples are arbitrary,
    /// like for testing a wholly different series of samples/results.
    pub fn eval_sample_arbitrary(
        &mut self,
        act_num: usize,
        i_state: &SomeState,
        r_state: &SomeState,
    ) {
        // May break canned setup, so its best to use all, or no, arbitrary samples.
        self.actions[act_num].eval_arbitrary_sample(i_state, r_state, self.num);
        self.set_state(&r_state);
    }

    /// Take an action for a need, evaluate the resulting sample.
    /// It is assumed that a sample made for a need must be saved.
    pub fn take_action_need(&mut self, ndx: &SomeNeed) {
        let act_num = ndx.act_num();

        let astate = self.actions[ndx.act_num()].take_action(self.num, &self.cur_state);

        self.actions[act_num].eval_need_sample(&self.cur_state, ndx, &astate, self.num);

        self.set_state(&astate);
    }

    /// Take an action with the current state.
    /// A sample made for a rule-path should not be saved if the result is expected.
    pub fn take_action(&mut self, act_num: usize) {
        let astate = self.actions[act_num].take_action(self.num, &self.cur_state);

        self.actions[act_num].eval_sample(&self.cur_state, &astate, self.num);

        self.set_state(&astate);
    }

    /// Accessor, set the cur_state field.
    pub fn set_state(&mut self, new_state: &SomeState) {
        // Check optimal regions and boredom duration.
        if self.optimal_and_ints.len() > 1 {
            let opt_sups = self.optimal_and_ints.supersets_of_state(&self.cur_state);

            if opt_sups.len() == 0 {
                self.boredom = 0;
            } else {
                let opt_sups_new = self.optimal_and_ints.supersets_of_state(&new_state);
                if opt_sups != opt_sups_new {
                    self.boredom = 0;
                }
            }
        }

        // Update memory.
        if self.memory.len() >= MAX_MEMORY {
            self.memory.pop_back();
        }
        self.memory.push_front(self.cur_state.clone());

        // Set current state.
        self.cur_state = new_state.clone();
    }

    /// Run a plan.
    pub fn run_plan(&mut self, pln: &SomePlan) {
        self.run_plan2(pln, 0); // Init depth counter.
    }

    /// Run a plan, with a recursion depth check.
    /// An unexpected result has a few flavors.
    /// A step from a group whose rule is a combination of samples that covers too broad a region.
    ///   The rule is deleted and maybe others are formed.  A re-plan-to-goal is done.
    /// A rule that has two expected results, in order, and the state is not known by a most-recent
    /// sample to be the alternate result.
    ///   If the alternate result has no changes, the action can be immediately tried again.
    ///   If the alternate result changes something, a re-plan-to-goal is done.
    fn run_plan2(&mut self, pln: &SomePlan, depth: usize) {
        if depth > 3 {
            println!("\nDepth exceeded, at plan {}", &pln);
            return;
        }

        if pln.initial_region().is_superset_of_state(&self.cur_state) == false {
            println!(
                "\nCurrent state {} is not in the start region of plan {}",
                &self.cur_state, &pln
            );
            return;
        }

        for stpx in pln.iter() {
            let astate = self.actions[stpx.act_num].take_action(self.num, &self.cur_state);

            self.actions[stpx.act_num].eval_step_sample(&self.cur_state, &astate, self.num);

            let prev_state = self.cur_state.clone();

            self.set_state(&astate);

            if stpx.result.is_superset_of_state(&self.cur_state) {
                continue;
            }

            // Handle unexpected/unwanted result
            // May be an expected possibility from a two result state

            if prev_state == self.cur_state && stpx.alt_rule {
                println!("Try action a second time");

                let astate = self.actions[stpx.act_num].take_action(self.num, &self.cur_state);

                self.actions[stpx.act_num].eval_step_sample(&self.cur_state, &astate, self.num);

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
                return;
            }

            if let Some(planx) = self.make_plan(pln.result_region()) {
                println!(
                    "The new plan from {} to {} is {}",
                    &self.cur_state,
                    &pln.result_region(),
                    &planx.str_terse()
                );
                return self.run_plan2(&planx, depth + 1);
            }
            println!(
                "A plan from {} to {} is not found",
                &self.cur_state,
                &pln.result_region(),
            );
            return;
        } // next stpx
    } // end run_plan2

    /// Return the steps of a plan to go from a given state/region to a given region.
    ///
    /// The required change of bits are calculated from the two given regions.
    ///
    /// Steps that make at least one of the needed changes are extracted from domain actions.
    /// A rule that changes X->x will be pruned to make a step that changes 0->1 or 1->0 depending
    /// on the change needed. X->0 will be pruned to 1->0.  X->1 will be pruned to 0->1.
    ///
    /// If all steps found, in aggregate, cannot change all bits needed, return None.
    ///
    /// The steps are split up into a vector of step vectors, each step vector has steps that change a
    /// particular needed-change bit.  Often, there will only be one step in a step vector.
    ///
    /// Steps that make more than one needed bit change will be in multiple step vectors.
    ///
    /// All possible step vector pairs are checked for being mutually exclusive, that is the needed change
    /// needs to be backed off to run any step in the other step vector.  If any such pair, return None.
    ///
    /// All step vectors are checked for the circumstance that all steps have an initial-region that is not a
    /// superset of the from-region, and the result-region does not intersect the goal-region.
    ///
    /// If no step vetors are found in the above step, all step vectors are considered to be selected.
    ///
    /// Find the minimum number of steps for all the selected step vectors.
    ///
    /// Aggregate the steps from all the selected step vectors with the minimum number of steps.
    ///
    /// Randomly choose a step.
    ///
    /// For the selected step, do Forward Chaining, Backward Chaining, or Asymmetric Chaining, as needed.
    ///
    pub fn random_depth_first_search(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        depth: usize,
    ) -> Option<StepStore> {
        if let Some(steps) = self.random_depth_first_search2(from_reg, goal_reg, depth) {
            if let Some(steps2) = steps.shortcuts() {
                return Some(steps2);
            }
            return Some(steps);
        }
        None
    }

    pub fn random_depth_first_search2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        depth: usize,
    ) -> Option<StepStore> {
        //println!("random_depth_first_search2: from {} to {}", from_reg, goal_reg);

        // Check if from_reg is at the goal, no steps are needed.
        if goal_reg.is_superset_of(from_reg) {
            //println!("from {} subset goal {} ?", from_reg, goal_reg);
            return Some(StepStore::new());
        }

        // Check if change is possible

        // Calculate the minimum bit changes needed.
        let required_change = SomeChange::region_to_region(from_reg, goal_reg);
        // println!("forward_depth_first_search2: required_change {}", &required_change);

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str: StepStore = self.actions.get_steps(&required_change);
        //println!("\nforward_depth_first_search2: from {} to {} steps_str steps {}", from_reg, goal_reg, steps_str.formatted_string(" "));

        // Check that the steps roughly encompass all needed changes, else return None.
        if let Some(can_change) = steps_str.aggregate_changes() {
            if required_change.is_subset_of(&can_change) {
            } else {
                // println!("forward_depth_first_search2: step_vec wanted changes {} are not a subset of step_vec changes {}, returning None", &required_change, &can_change);
                return None;
            }
        } else {
            return None;
        }

        // Check if one step makes the required change, the end point of any search.
        for stepx in steps_str.iter() {
            if stepx.initial.intersects(from_reg) {
                let stepy = stepx.restrict_initial_region(from_reg);

                if goal_reg.is_superset_of(&stepy.result) {
                    //println!("forward_depth_first_search2: suc 1 Found one step {} to go from {} to {}", &stepy, from_reg, goal_reg);
                    return Some(StepStore::new_with_step(stepy));
                }
            }
        }

        // Check depth
        if depth == 0 {
            return None;
        }

        // Sort the steps by each needed bit change. (some actions may change more than one bit, so will appear more than once)
        let steps_by_change_vov: Vec<Vec<&SomeStep>> =
            steps_str.steps_by_change_bit(&required_change);

        // Check if any pair of single-bit changes, all steps, are mutually exclusive.
        if any_mutually_exclusive_changes(&steps_by_change_vov, &required_change) {
            //println!("forward_depth_first_search2: mutually exclusive change rules found");
            return None;
        }

        // Check for single-bit vectors where no rules have an initial-region that
        // is a superset of the from-region, and the result-region does not intersect the
        // goal-region.
        let mut asym_inx = Vec::<usize>::new();
        let mut inx = 0;
        for vecx in steps_by_change_vov.iter() {
            let mut no_asym = true;
            for stepx in vecx.iter() {
                if stepx.initial.is_superset_of(from_reg) || stepx.result.intersects(goal_reg) {
                    no_asym = false;
                    break;
                }
            }
            if no_asym {
                asym_inx.push(inx);
            }
            inx += 1;
        } // next vecx

        // Init selected steps
        let mut selected_steps = Vec::<&SomeStep>::new();

        // If any asymmetrical single-bit changes found
        if asym_inx.len() > 0 {
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
        } else {
            // find min number of steps for any bit-change
            let mut min_steps = 99;
            for vecx in steps_by_change_vov.iter() {
                if vecx.len() < min_steps {
                    min_steps = vecx.len();
                }
            }
            // Assemble possible steps
            for vecx in steps_by_change_vov.iter() {
                if vecx.len() == min_steps {
                    for stepx in vecx.iter() {
                        selected_steps.push(stepx);
                    }
                }
            }
        }

        // Randomly choose a step.
        let stepx = selected_steps[rand::thread_rng().gen_range(0..selected_steps.len())];

        // Forward chaining
        if stepx.initial.is_superset_of(from_reg) {
            let stepy = stepx.restrict_initial_region(from_reg);
            if let Some(next_steps) =
                self.random_depth_first_search2(&stepy.result, goal_reg, depth - 1)
            {
                return StepStore::new_with_step(stepy).link(&next_steps);
            }
            return None;
        }

        // Backward chaining
        if stepx.result.intersects(goal_reg) {
            let stepy = stepx.restrict_result_region(goal_reg);
            if let Some(prev_steps) =
                self.random_depth_first_search2(from_reg, &stepy.initial, depth - 1)
            {
                return prev_steps.link(&StepStore::new_with_step(stepy));
            }
            return None;
        }

        // Asymmetrical chaining
        if let Some(first_steps) =
            self.random_depth_first_search2(from_reg, &stepx.initial, depth - 1)
        {
            let stepy = stepx.restrict_initial_region(&first_steps.result());
            if let Some(next_steps) =
                self.random_depth_first_search(&stepy.result, goal_reg, depth - 1)
            {
                if let Some(steps1) = first_steps.link(&StepStore::new_with_step(stepy)) {
                    return steps1.link(&next_steps);
                }
            }
        }

        None
    } // end random_depth_first_search2

    /// Make a plan to change the current state to another region.
    /// Since there are some random choices, it may be useful to try
    /// running make_plan more than once.
    pub fn make_plan(&self, goal_reg: &SomeRegion) -> Option<SomePlan> {
        //println!("make_plan start");

        if goal_reg.is_superset_of_state(&self.cur_state) {
            //println!("no plan needed from {} to {} ?", &self.cur_state, goal_reg);
            return Some(SomePlan::new(StepStore::new()));
        }

        // Try to make a plan several times.
        //let mut plans = Vec::<SomePlan>::new();
        let cur_reg = SomeRegion::new(&self.cur_state, &self.cur_state);
        //println!("make_plan: from {} to {}", cur_reg, goal_reg);

        // Tune maximum depth to be a multiple of the number of bit changes required.
        let required_change = SomeChange::region_to_region(&cur_reg, goal_reg);
        let num_depth = 3 * required_change.number_changes();

        let mut plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(
                |_| match self.random_depth_first_search(&cur_reg, &goal_reg, num_depth) {
                    Some(x) => Some(SomePlan::new(x)),
                    None => None,
                },
            )
            .collect::<Vec<SomePlan>>();

        // Return one of the plans, avoiding the need to clone.
        if plans.len() == 1 {
            //println!("forward_depth_first_search returned plan");
            return Some(plans.remove(0));
        }
        if plans.len() > 1 {
            //println!("forward_depth_first_search returned plan");
            let inx = self.choose_a_plan(&plans);
            return Some(plans.remove(inx));
        }

        // No plan to return.
        //println!("random_depth_first_search did not return a plan");
        None
    } // end make plan

    /// Change the current state to be within a given region.
    /// Return True if the change succeeds.
    pub fn to_region(&mut self, goal_region: &SomeRegion) -> bool {
        if goal_region.is_superset_of_state(&self.cur_state) {
            return true;
        }

        if let Some(pln) = self.make_plan(&goal_region) {
            // Do the plan
            self.run_plan(&pln);
            if goal_region.is_superset_of_state(&self.cur_state) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    } // end to_region

    /// Return a Region from a string.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn region_from_string(&self, str: &str) -> Result<SomeRegion, String> {
        SomeRegion::new_from_string(self.num_ints, &str)
    } // end region_from_string

    /// Return a State from a string.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn state_from_string(&self, str: &str) -> Result<SomeState, String> {
        SomeState::new_from_string(self.num_ints, &str)
    } // end state_from_string

    /// Return a Action number from a string with a format that the parse method can understand.
    /// Left-most, consecutive, zeros can be omitted.
    /// Returns an error if the string is bad or no action exists of that number.
    pub fn act_num_from_string(&self, str_num: &str) -> Result<usize, String> {
        match str_num.parse::<usize>() {
            Ok(act_num) => {
                if act_num >= self.num_actions() {
                    return Err(format!("Action number too large {}", act_num));
                }
                return Ok(act_num);
            }
            Err(error) => {
                return Err(format!("\nDid not understand action number, {}", error));
            }
        }
    } // end act_num_from_string

    /// Return the index value of a chosen Plan
    fn choose_a_plan(&self, ret_plans: &Vec<SomePlan>) -> usize {
        assert!(ret_plans.len() > 0);

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

        // Rate plans, highest rate is best rate.
        // TODO better critera
        // Length of plan is rated inversely
        let mut rates_vec = Vec::<usize>::with_capacity(ret_plans.len());
        for rets in ret_plans.iter() {
            // Rate the length of the plan
            let mut rate = max_len - rets.len() + 10;

            // Check for steps that may have two results
            for stpx in rets.iter() {
                if stpx.alt_rule {
                    if let Some(sqrx) = self.actions[stpx.act_num]
                        .squares
                        .find(&stpx.initial.state1)
                    {
                        // For an alternating square, you want the most recent result to be NEQ the desired next result.
                        if *sqrx.most_recent_result() == stpx.result.state1 {
                            if rate > 1 {
                                rate = rate - 2;
                            } else {
                                rate = 0;
                            }
                        }
                    } else {
                        if rate > 1 {
                            rate = rate - 2;
                        } else {
                            rate = 0;
                        }
                    }
                }
            }

            rates_vec.push(rate);
        }

        // Find max rate
        let mut max_rate = 0;
        for rtx in rates_vec.iter() {
            if *rtx > max_rate {
                max_rate = *rtx;
            }
        }

        // Gather highest rated plan indexes
        let mut inx_ary = Vec::<usize>::new();
        let mut inx = 0;
        for rets in rates_vec.iter() {
            if *rets == max_rate {
                inx_ary.push(inx);
            }
            inx += 1;
        }

        // Choose a step
        if inx_ary.len() == 1 {
            return inx_ary[0];
        }
        return inx_ary[rand::thread_rng().gen_range(0..inx_ary.len())];
    } // end choose_a_plan
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
            for refx in by_change[inx].iter() {
                for refy in by_change[iny].iter() {
                    if ptr_eq(*refx, *refy) {
                        return false;
                    }
                    if refx.mutually_exclusive(refy, wanted) {
                        //println!("step {} mutually exclusive to step {}", refx, refy);
                    } else {
                        return false;
                    }
                } //next numy
            } // next refx
        } // next iny
    } // next inx

    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mask::SomeMask;

    // Test a simple four-step plan to change the domain current state
    // from s0111 to s1000.
    #[test]
    fn make_plan_direct() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();

        let s0 = dm0.state_from_string("s0").unwrap();
        let sf = dm0.state_from_string("s1111").unwrap();

        // Create group for region XXXX, Act 0.
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &sf, &sf.toggle_bits(vec![0]));

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(1, &s0, &s0.toggle_bits(vec![1]));
        dm0.eval_sample_arbitrary(1, &sf, &sf.toggle_bits(vec![1]));

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(2, &s0, &s0.toggle_bits(vec![2]));
        dm0.eval_sample_arbitrary(2, &sf, &sf.toggle_bits(vec![2]));

        // Create group for region XXXX, Act 3.
        dm0.eval_sample_arbitrary(3, &s0, &s0.toggle_bits(vec![3]));
        dm0.eval_sample_arbitrary(3, &sf, &sf.toggle_bits(vec![3])); // Last sample changes current state to s0111

        // Get plan for 7 to 8
        dm0.set_state(&dm0.state_from_string("s111").unwrap());
        let mut toreg = dm0.region_from_string("r1000").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 4);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err("no plan found to r1000?".to_string());
        }

        // Get plan for 8 to 7
        dm0.set_state(&dm0.state_from_string("s1000").unwrap());
        toreg = dm0.region_from_string("r111").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 4);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err("no plan found to r111?".to_string());
        }

        Ok(())
    }

    // Test asymmetric chaining.  The plan must step out of the direct
    // glide path X1XX, between 7 and C, into X0XX, to change the third bit,
    // then step back into the glide path to get to the goal.
    #[test]
    fn make_plan_asymmetric() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();

        let s0 = dm0.state_from_string("s0").unwrap();
        let sf = dm0.state_from_string("s1111").unwrap();
        let sb = dm0.state_from_string("s1011").unwrap();

        // Create group for region XXXX, Act 0.
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &sf, &sf.toggle_bits(vec![0]));

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(1, &s0, &s0.toggle_bits(vec![1]));
        dm0.eval_sample_arbitrary(1, &sf, &sf.toggle_bits(vec![1]));

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(2, &s0, &s0.toggle_bits(vec![2]));
        dm0.eval_sample_arbitrary(2, &sf, &sf.toggle_bits(vec![2]));

        // Create group for region X0XX, Act 3.
        dm0.eval_sample_arbitrary(3, &s0, &s0.toggle_bits(vec![3]));
        dm0.eval_sample_arbitrary(3, &sb, &sb.toggle_bits(vec![3]));

        println!("\nActs: {}", &dm0.actions);

        // Get plan for 7 to C
        dm0.set_state(&dm0.state_from_string("s111").unwrap());
        let mut toreg = dm0.region_from_string("r1100").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 5);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err("No plan found s111 to r1100?".to_string());
        }

        // Get plan for C to 7
        dm0.set_state(&dm0.state_from_string("s1100").unwrap());
        toreg = dm0.region_from_string("r111").unwrap();
        if let Some(aplan) = dm0.make_plan(&toreg) {
            assert!(aplan.len() == 5);
            assert!(*aplan.result_region() == toreg);
        } else {
            return Err("No plan found s1100 to r111?".to_string());
        }

        Ok(())
    }

    // Test action:get_needs StateNotInGroup, two flavors.
    #[test]
    fn need_for_state_not_in_group() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0]
            .state_not_in_group_needs(&dm0.cur_state, &VecDeque::<SomeState>::new());

        assert!(nds1.len() == 1);
        assert!(
            nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap())
        );

        // Create group for one sample
        let s1 = dm0.state_from_string("s1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1").unwrap())
        {
        } else {
            return Err("Group r1 not found ??".to_string());
        }

        println!("\nActs: {}", &dm0.actions[0]);

        // Invalidate group for sample 1 by giving it GT 1 different result.
        // Current state changes to zero.
        let s1 = dm0.state_from_string("s1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1.toggle_bits(vec![0]));

        println!("\nActs: {}", &dm0.actions[0]);

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1").unwrap())
        {
            return Err("Group r1  found ??".to_string());
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

        //return Err("Done".to_string());
        Ok(())
    }

    // Test additional_group_state_samples.
    #[test]
    fn need_additional_group_state_samples() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0]
            .state_not_in_group_needs(&dm0.cur_state, &VecDeque::<SomeState>::new());

        assert!(nds1.len() == 1);
        assert!(
            nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap())
        );

        // Create group for one sample
        let s1 = dm0.state_from_string("s1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1").unwrap())
        {
        } else {
            return Err("Group r1 not found ??".to_string());
        }

        // Expand group
        let s2 = dm0.state_from_string("s10").unwrap();
        dm0.eval_sample_arbitrary(0, &s2, &s2);

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("rXX").unwrap())
        {
        } else {
            return Err("Group rXX not found ??".to_string());
        }

        let nds2 = dm0.actions[0].additional_group_state_samples();
        println!("needs {}", nds2);

        assert!(nds2.len() == 2);
        assert!(nds2.contains_similar_need(
            "StateAdditionalSample",
            &dm0.region_from_string("r1").unwrap()
        ));
        assert!(nds2.contains_similar_need(
            "StateAdditionalSample",
            &dm0.region_from_string("r10").unwrap()
        ));

        // Satisfy one need.
        dm0.eval_sample_arbitrary(0, &s2, &s2);

        let nds3 = dm0.actions[0].additional_group_state_samples();
        //println!("needs {}", nds3);

        assert!(nds3.len() == 1);
        assert!(nds3.contains_similar_need(
            "StateAdditionalSample",
            &dm0.region_from_string("r1").unwrap()
        ));

        // Satisfy second need.
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        let nds4 = dm0.actions[0].additional_group_state_samples();
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
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();

        let s0 = dm0.state_from_string("s0").unwrap();
        let s6 = dm0.state_from_string("s110").unwrap();
        let sd = dm0.state_from_string("s1101").unwrap();

        // Create group for region XX0X.
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![0]));

        dm0.eval_sample_arbitrary(0, &sd, &sd);
        dm0.eval_sample_arbitrary(0, &sd, &sd);

        // Create group X1XX
        dm0.eval_sample_arbitrary(0, &s6, &s6);
        dm0.eval_sample_arbitrary(0, &s6, &s6);

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
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();

        // Set up two groups.
        // Changes for each sample set up two, and only two groups.
        // Changes for bit three insure the seeking external dissimilar squares
        // that differ by the third bit.  Like D and 5, 0 and 8.
        let s0 = dm0.state_from_string("s0").unwrap();
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![3]));
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![3]));

        let s7 = dm0.state_from_string("s111").unwrap();
        dm0.eval_sample_arbitrary(0, &s7, &s7.toggle_bits(vec![3]));
        dm0.eval_sample_arbitrary(0, &s7, &s7.toggle_bits(vec![3]));

        let sd = dm0.state_from_string("s1101").unwrap();
        dm0.eval_sample_arbitrary(0, &sd, &sd.toggle_bits(vec![0, 1, 3]));
        dm0.eval_sample_arbitrary(0, &sd, &sd.toggle_bits(vec![0, 1, 3]));

        let sa = dm0.state_from_string("s1010").unwrap();
        dm0.eval_sample_arbitrary(0, &sa, &sa.toggle_bits(vec![0, 1, 3]));
        dm0.eval_sample_arbitrary(0, &sa, &sa.toggle_bits(vec![0, 1, 3]));

        println!("dm0 {}", &dm0.actions[0]);

        // Directly run limit_groups_needs.
        let agg_chg = SomeChange::new(
            &SomeMask::new_from_string(1, "m1111").unwrap(),
            &SomeMask::new_from_string(1, "m1111").unwrap(),
        );
        let nds1 = dm0.actions[0].limit_groups_needs(&agg_chg);

        // Check for needs of adjacent, external, squares to 0 (8), 7 (F), A (2) , D (5).
        println!("needs are {}", nds1);
        assert!(nds1.len() == 2);
        assert!(
            nds1.contains_similar_need("LimitGroup", &dm0.region_from_string("r1111").unwrap())
                || nds1
                    .contains_similar_need("LimitGroup", &dm0.region_from_string("r1000").unwrap())
        );
        assert!(
            nds1.contains_similar_need("LimitGroup", &dm0.region_from_string("r101").unwrap())
                || nds1
                    .contains_similar_need("LimitGroup", &dm0.region_from_string("r10").unwrap())
        );

        // Start homing in with sample of 5, adjacent, external, to D in 1XXX.
        let s5 = dm0.state_from_string("s101").unwrap();
        dm0.eval_sample_arbitrary(0, &s5, &s5.toggle_bits(vec![3]));
        let nds2 = dm0.get_needs();

        // Check for second need for 5, to reach pnc for 5.
        assert!(nds2.len() == 2);
        assert!(nds2.contains_similar_need("LimitGroup", &dm0.region_from_string("r101").unwrap()));

        // Get second sample for 5.
        dm0.eval_sample_arbitrary(0, &s5, &s5.toggle_bits(vec![3]));
        let nds3 = dm0.get_needs();

        // Check for need of square 10, far from square 5, in 0XXX.
        assert!(nds3.len() == 1);
        assert!(nds3.contains_similar_need("LimitGroup", &dm0.region_from_string("r10").unwrap()));

        // Get sample of 2, far from 5 in 0XXX.
        // In 1XXX, A is already far from D, and is pnc, so no further needs for 1XXX.
        let s2 = dm0.state_from_string("s10").unwrap();
        dm0.eval_sample_arbitrary(0, &s2, &s2.toggle_bits(vec![3]));
        let nds4 = dm0.get_needs();

        // Check for need of second sample of square 10, to reach pnc.
        assert!(nds4.len() == 1);
        assert!(nds4.contains_similar_need("LimitGroup", &dm0.region_from_string("r10").unwrap()));

        // Take second sample of square 10.
        dm0.eval_sample_arbitrary(0, &s2, &s2.toggle_bits(vec![3]));
        let nds5 = dm0.get_needs();

        // The two groups, 0XXX and 1XXX, should be limited, and have no further needs.
        assert!(nds5.len() == 0);

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
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();

        let s5 = dm0.state_from_string("s101").unwrap();

        let s4 = dm0.state_from_string("s100").unwrap();

        let sf = dm0.state_from_string("s1111").unwrap();

        let se = dm0.state_from_string("s1110").unwrap();

        let s7 = dm0.state_from_string("s111").unwrap();

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
        //return Err(String::from("Done!",));
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
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();

        let s5 = dm0.state_from_string("s101").unwrap();

        let s4 = dm0.state_from_string("s100").unwrap();

        let sf = dm0.state_from_string("s1111").unwrap();

        let se = dm0.state_from_string("s1110").unwrap();

        let s7 = dm0.state_from_string("s111").unwrap();

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
        let mut dm0 = SomeDomain::new(
            0,
            2,
            SomeState::new_from_string(2, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();

        let s0 = dm0.state_from_string("s0001010010101000").unwrap();
        let s1 = dm0.state_from_string("s1111010110101011").unwrap();
        // Region                        XXX1010X101010XX.

        // Create group for region XXX1010X101010XX.
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![4]));
        dm0.eval_sample_arbitrary(0, &s1, &s1.toggle_bits(vec![4]));

        if let Some(_grpx) = dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("rXXX1010X101010XX").unwrap())
        {
        } else {
            return Err("Group rXXX1010X101010XX not found ??".to_string());
        }

        Ok(())
    }

    #[test]
    fn compatible_group_intersection_needs() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();

        let s0 = dm0.state_from_string("s000").unwrap();
        let s3 = dm0.state_from_string("s011").unwrap();
        let s5 = dm0.state_from_string("s101").unwrap();

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
                return Err("Group r0XX1 not found ??".to_string());
            }
        } else {
            return Err("Group r0X0X not found ??".to_string());
        }
        //return Err("Done!".to_string());
        Ok(())
    }

    #[test]
    fn seek_edge_needs() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(
            0,
            1,
            SomeState::new_from_string(1, "s1").unwrap(),
            RegionStore::new(),
        );
        dm0.add_action();

        // Establish group XXXX
        let s0 = dm0.state_from_string("s0000").unwrap();
        let sf = dm0.state_from_string("s1111").unwrap();

        dm0.eval_sample_arbitrary(0, &s0, &s0);
        dm0.eval_sample_arbitrary(0, &sf, &sf);
        // Establish pnc for group XXXX
        dm0.eval_sample_arbitrary(0, &s0, &s0);
        dm0.eval_sample_arbitrary(0, &sf, &sf);

        println!("\nActs: {}", &dm0.actions);

        // Break group XXXX
        let s5 = dm0.state_from_string("s0101").unwrap();
        //        let s3 = dm0.state_from_string("s0011").unwrap();

        dm0.eval_sample_arbitrary(0, &s5, &s0);

        println!("\nActs: {}", &dm0.actions);
        assert!(dm0.actions[0]
            .seek_edge
            .contains(&dm0.region_from_string("rx1x1").unwrap()));

        let needs = dm0.actions[0].seek_edge_needs2();
        println!("needs: {}", &needs);

        let sd = dm0.state_from_string("s1101").unwrap();
        dm0.eval_sample_arbitrary(0, &sd, &sd);

        let needs = dm0.actions[0].seek_edge_needs1();
        println!("needs2: {}", &needs);
        assert!(needs.len() == 1);

        dm0.eval_sample_arbitrary(0, &sd, &sd);
        let needs = dm0.actions[0].seek_edge_needs1();
        println!("needs3: {}", &needs);

        println!("\nActs: {}", &dm0.actions);

        //assert!(1 == 2);
        Ok(())
    }
} // end tests
