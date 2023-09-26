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

use crate::actionstore::ActionStore;
use crate::bits::SomeBits;
use crate::change::SomeChange;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::step::SomeStep;
use crate::stepstore::StepStore;
use crate::tools::{self, StrLen};

use rand::Rng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

impl fmt::Display for PathStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::new();
        rc_str.push('[');
        rc_str.push_str(&format!("from: {}", self.from));
        rc_str.push_str(&format!(", to: {}", self.to));
        rc_str.push_str(&format!(", within: {}", self.within));
        rc_str.push(']');
        write!(f, "{}", rc_str)
    }
}

/// Implement the trait StrLen for SomeRegion.
impl StrLen for PathStep {
    fn strlen(&self) -> usize {
        24 + (3 * self.from.strlen())
    }
}

pub struct PathStep {
    pub from: SomeRegion,
    pub to: SomeRegion,
    pub within: SomeRegion,
}

pub struct Path {
    pub steps: Vec<PathStep>,
}

impl fmt::Display for SomeDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
/// The SomeDomain struct, a state and actions that can be run.
pub struct SomeDomain {
    /// Domain number.  Index into a higher-level DomainStore.
    pub num: usize,
    /// Actions the Domain can take.
    pub actions: ActionStore,
    /// The Current, internal, State.
    pub cur_state: SomeState,
    /// Memory for past samples that were not stored in a square.
    pub max_region: SomeRegion,
    /// Save complements of negative SelectRegions, for more efficent calculations.
    /// Exiting a negative region, or avoiding a negative region, may be time sensitive.
    /// HashMap does not work well with SomeRegion:PartialEq, so use SelectRegions index instead.
    pub complements: HashMap<usize, RegionStore>,
}

impl SomeDomain {
    /// Return a new domain instance, given the number of integers, the
    /// initial state, the optimal state(s), the index into the higher-level DomainStore.
    pub fn new(num_ints: usize) -> Self {
        debug_assert_ne!(num_ints, 0);
        // Set up a domain instance with the correct value for num_ints
        let ur_state = SomeState::new(SomeBits::new(vec![0; num_ints]));
        let cur_state = ur_state.new_random();
        let max_region = SomeRegion::new(vec![ur_state.new_high(), ur_state.new_low()]);
        Self {
            num: 0, // May be changed later
            actions: ActionStore::new(
                vec![],
                SomeChange::new(ur_state.new_low().to_mask(), ur_state.new_low().to_mask()),
            ),
            cur_state,
            max_region,
            complements: HashMap::new(),
        }
    }

    /// Return the current state as a region.
    pub fn current_region(&self) -> SomeRegion {
        SomeRegion::new(vec![self.cur_state.clone()])
    }

    /// Set the domain number.
    pub fn set_domain_num(&mut self, dom_num: usize) {
        self.num = dom_num;
    }

    /// Return a reference to the current, internal, state.
    pub fn get_current_state(&self) -> &SomeState {
        &self.cur_state
    }

    /// Add a SomeAction instance to the store.
    pub fn add_action(&mut self) {
        self.actions.add_action(self.num, self.cur_state.to_mask());
    }

    /// Return needs gathered from all actions.
    /// Some housekeeping is done, so self is mutable.
    pub fn get_needs(&mut self) -> NeedStore {
        // Get all needs.
        self.actions.get_needs(&self.cur_state, self.num)
    }

    /// Add the complement of a region.
    pub fn add_complement(&mut self, sel_num: usize, regx: &SomeRegion) {
        if regx.all_x() {
            return;
        }

        if self.complements.get(&sel_num).is_none() {
            self.complements
                .insert(sel_num, RegionStore::new(regx.complement()));
        }
    }

    /// Return the complement of a region.
    pub fn get_complement(&self, sel_num: usize) -> Option<&RegionStore> {
        self.complements.get(&sel_num)
    }

    /// Evaluate an arbitrary sample given by the user.
    /// This tends to break things for an action, unless all samples are arbitrary.
    /// Useful for testing a wholly different series of samples/results.
    /// Using the command: ss  action-number  initial-state  result-state
    /// e.g. ss  0  s0b1010  s0b1111
    pub fn eval_sample_arbitrary(&mut self, act_num: usize, smpl: &SomeSample) {
        self.actions[act_num].eval_sample_arbitrary(smpl);
        self.set_cur_state(smpl.result.clone());
    }

    /// Take an action for a need, evaluate the resulting sample.
    /// It is assumed that a sample made for a need must be saved.
    pub fn take_action_need(&mut self, ndx: &SomeNeed) {
        let asample = self.actions[ndx.act_num()].take_action_need(&self.cur_state, ndx);
        self.set_cur_state(asample.result.clone());
    }

    /// Take an action with the current state.
    pub fn take_action_arbitrary(&mut self, act_num: usize) {
        let asample = self.actions[act_num].take_action_arbitrary(&self.cur_state);
        self.set_cur_state(asample.result.clone());
    }

    /// Set the current state field.
    pub fn set_cur_state(&mut self, stax: SomeState) {
        self.cur_state = stax;
    }

    /// Set current state field, without affecting memory.
    pub fn set_state(&mut self, new_state: &SomeState) {
        self.cur_state = new_state.clone();
    }

    /// Run a plan, return true if it runs to completion.
    pub fn run_plan(&mut self, pln: &SomePlan) -> bool {
        assert_eq!(pln.dom_num, self.num);

        if pln.is_empty() {
            return true;
        }

        if !pln.initial_region().is_superset_of_state(&self.cur_state) {
            panic!(
                "\nCurrent state {} is not in the start region of plan {}",
                &self.cur_state, &pln
            );
        }

        // Run each step of the plan.
        for stpx in pln.iter() {
            let prev_state = self.cur_state.clone();

            let asample = self.actions[stpx.act_num].take_action_step(&self.cur_state);
            self.set_cur_state(asample.result.clone());

            if stpx.result.is_superset_of_state(&self.cur_state) {
                continue;
            }

            // Handle unexpected/unwanted result
            // May be an expected possibility from a two result state.
            if prev_state == self.cur_state && stpx.alt_rule {
                println!("Try action a second time");

                let asample = self.actions[stpx.act_num].take_action_step(&self.cur_state);
                self.set_cur_state(asample.result.clone());

                if stpx.result.is_superset_of_state(&self.cur_state) {
                    continue;
                }
            }

            println!(
                "\nChange [{} -{:02}> {}] unexpected, expected {}",
                &prev_state, &stpx.act_num, &self.cur_state, stpx,
            );

            return false;
        } // next stpx

        pln.result_region().is_superset_of_state(&self.cur_state)
    } // end run_plan

    /// Return the steps of a plan to go from a given state/region to a given region.
    pub fn random_depth_first_search(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        steps_str: &StepStore,
        steps_by_change_vov: &Vec<Vec<&SomeStep>>,
        depth: usize,
    ) -> Option<SomePlan> {
        if let Some(plan1) = self.random_depth_first_search2(
            from_reg,
            goal_reg,
            steps_str,
            steps_by_change_vov,
            depth - 1,
        ) {
            if let Some(plan2) = plan1.shortcuts() {
                return Some(plan2);
            }
            return Some(plan1);
        }
        None
    }

    /// Return the steps of a plan to go from a given state/region to a given region.
    ///
    /// This may be called from random_depth_first_search, or may be recursively called to make a sub-plan.
    ///
    /// If any needed bit changes are only possible through Asymmetric Chaining, randomly choose a step and recurse.
    ///
    /// Otherwise, randomly choose a step and do Forward Chaining, Backward Chaining, or Asymmetric Chaining.
    ///
    fn random_depth_first_search2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        steps_str: &StepStore,
        steps_by_change_vov: &Vec<Vec<&SomeStep>>,
        depth: usize,
    ) -> Option<SomePlan> {
        // println!("random_depth_first_search2: from {} to {} depth {}", from_reg, goal_reg, depth);

        // Check if one step makes the required change, the end point of any search.
        // In case there is more than one such step, choose it randomly.
        let mut rand_inx = tools::RandomPick::new(steps_str.len());
        while let Some(inx) = rand_inx.pick() {
            if steps_str[inx].initial.intersects(from_reg)
                && steps_str[inx].result.intersects(goal_reg)
            {
                let stepy = steps_str[inx].restrict_initial_region(from_reg);

                if stepy.result.intersects(goal_reg) {
                    let stepz = stepy.restrict_result_region(goal_reg);

                    //println!("random_depth_first_search2: suc 1 Found one step {} to go from {} to {}", &stepy, from_reg, goal_reg);
                    return Some(SomePlan::new(self.num, vec![stepz]));
                }
            }
        }

        // Check depth
        if depth == 0 {
            //println!("depth limit exceeded");
            return None;
        }

        // Check for single-bit step-vectors where no steps have an initial-region that
        // is a superset of the from-region, and the result-region does not intersect the
        // goal-region.
        // Choose a step with the least options to avoid, first.
        // So run initial -(plan 1)> least-options-to-avoid-step -(plan 2)> goal.
        let mut asym_inx = Vec::<usize>::new();

        'next_vecx: for (inx, vecx) in steps_by_change_vov.iter().enumerate() {
            for stepx in vecx.iter() {
                if stepx.initial.is_superset_of(from_reg) || stepx.result.intersects(goal_reg) {
                    continue 'next_vecx;
                }
            }
            asym_inx.push(inx);
        } // next vecx

        // If any forced asymmetrical single-bit changes found
        if !asym_inx.is_empty() {
            // Init selected steps
            let mut selected_steps = Vec::<&SomeStep>::new();

            // find min number of steps for the selected bit-changes
            let mut min_steps = usize::MAX;
            for inx in &asym_inx {
                if steps_by_change_vov[*inx].len() < min_steps {
                    min_steps = steps_by_change_vov[*inx].len();
                }
            }
            // Assemble possible steps
            for inx in asym_inx {
                if steps_by_change_vov[inx].len() == min_steps {
                    selected_steps.extend(&steps_by_change_vov[inx]);
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

        // Process a forward chaining step.
        if stepx.initial.is_superset_of(from_reg) {
            //println!("forward step");
            let stepy = stepx.restrict_initial_region(from_reg);

            let plan_to_goal = self.plan_steps_between(&stepy.result, goal_reg, depth - 1)?;

            return SomePlan::new(self.num, vec![stepy]).link(&plan_to_goal);
        }

        // Process a backward chaining step.
        if stepx.result.intersects(goal_reg) {
            //println!("backward step");
            let stepy = stepx.restrict_result_region(goal_reg);

            let plan_to_step = self.plan_steps_between(from_reg, &stepy.initial, depth - 1)?;

            return plan_to_step.link(&SomePlan::new(self.num, vec![stepy]));
        }

        // Must be an asymmetric step.
        self.asymmetric_chaining(from_reg, goal_reg, stepx, depth - 1)
    } // end random_depth_first_search2

    /// Return possible plan to change state between two regions.
    fn plan_steps_between(
        &self,
        from_reg: &SomeRegion,
        to_reg: &SomeRegion,
        depth: usize,
    ) -> Option<SomePlan> {
        // println!("plan_steps_between: from {} to {} depth {}", from_reg, to_reg, depth);
        if depth == 0 {
            return None;
        }

        let required_change = SomeChange::region_to_region(from_reg, to_reg);

        let steps_str = self.get_steps(&required_change, None)?;

        let steps_by_change_vov = steps_str.get_steps_by_bit_change(&required_change)?;

        self.random_depth_first_search2(
            from_reg,
            to_reg,
            &steps_str,
            &steps_by_change_vov,
            depth - 1,
        )
    }

    /// Do asymmetric chaining for a given step.
    ///
    /// The step intial, and result, regions MAY be completely outside of the region
    /// formed by the union of the from-region and goal-region, hence the term asymmetric.
    ///
    /// This splits the problem into two smaller parts, two plans need to be made:
    ///   From the from_region to the initial region of a step.
    ///   From the result region of a step to the goal.
    ///
    /// One plan may involve more restrictions, less flexibility, than the other,
    /// so randomly choose which plan to make first.
    ///
    fn asymmetric_chaining(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        stepx: &SomeStep,
        depth: usize,
    ) -> Option<SomePlan> {
        // println!("asymmetric_chaining: from {} to {} depth {}", from_reg, goal_reg, depth);
        if depth == 0 {
            return None;
        }

        let (to_step_plan, stepy, from_step_plan) = if rand::random::<bool>() {
            let to_step_plan = self.plan_steps_between(from_reg, &stepx.initial, depth - 1)?;

            // Restrict the step initial region, in case it is different from the to_step_plan result region,
            // possibly changing the step result region.
            let stepy = stepx.restrict_initial_region(to_step_plan.result_region());

            let from_step_plan = self.plan_steps_between(&stepy.result, goal_reg, depth - 1)?;

            (to_step_plan, stepy, from_step_plan)
        } else {
            let from_step_plan = self.plan_steps_between(&stepx.result, goal_reg, depth - 1)?;

            // Restrict the step result region, in case it is different from the from_step_plan initial region,
            // possibly changing the step initial region.
            let stepy = stepx.restrict_result_region(from_step_plan.initial_region());

            let to_step_plan = self.plan_steps_between(from_reg, &stepy.initial, depth - 1)?;

            (to_step_plan, stepy, from_step_plan)
        };

        // Try linking two plans together with the step.
        to_step_plan
            .link(&SomePlan::new(self.num, vec![stepy]))?
            .link(&from_step_plan)
    }

    /// Make a plan to change the current state to another region.
    /// Since there are some random choices, it may be useful to try
    /// running make_plan more than once.
    pub fn make_plans(&self, goal_reg: &SomeRegion) -> Option<Vec<SomePlan>> {
        //println!("make_plan start cur {} goal {}", self.cur_state, goal_reg);

        // Return no-op plan if the goal is already met.
        if goal_reg.is_superset_of_state(&self.cur_state) {
            //println!("no plan needed from {} to {} ?", &self.cur_state, goal_reg);
            return Some(vec![SomePlan::new(self.num, vec![])]);
        }

        let cur_reg = SomeRegion::new(vec![self.cur_state.clone()]);

        self.make_plans2(&cur_reg, goal_reg, None)
    }

    /// Make a plan to change from a region to another region.
    pub fn make_plans2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        within: Option<&SomeRegion>,
    ) -> Option<Vec<SomePlan>> {
        //println!("dom {} make_plans: from {from_reg} goal {goal_reg}", self.num);
        // Figure the required change.
        let required_change = SomeChange::region_to_region(from_reg, goal_reg);

        // Tune maximum depth to be a multiple of the number of bit changes required.
        let num_depth = 4 * required_change.number_changes();

        // Get steps, check if steps include all changes needed.
        let steps_str = self.get_steps(&required_change, within)?;

        // Get vector of steps for each bit change.
        let steps_by_change_vov = steps_str.get_steps_by_bit_change(&required_change)?;

        // Calculated steps_str, and steps_by_change_vov, ahead so that thay don't have to be
        // recalculated for each run, below, of random_depth_first_search.
        let plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| {
                self.random_depth_first_search(
                    from_reg,
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

        //println!("make_plan returned {}", SomePlan::vec_string(&plans));
        Some(plans)
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
    fn get_steps(
        &self,
        required_change: &SomeChange,
        within: Option<&SomeRegion>,
    ) -> Option<StepStore> {
        // Check if changes are possible.

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str: StepStore = self.actions.get_steps(required_change, within);

        // Check that the steps roughly encompass all needed changes, else return None.
        let can_change = steps_str.aggregate_changes()?;

        if required_change.is_subset_of(&can_change) {
        } else {
            //println!("get_steps: step_vec wanted changes {} are not a subset of step_vec changes {}, returning None", &required_change, &can_change);
            return None;
        }

        Some(steps_str)
    }

    /// Return a Region from a string.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn region_from_string(&self, str: &str) -> Result<SomeRegion, String> {
        self.max_region.new_from_string(str)
    } // end region_from_string

    /// Return a SomeRegion instance from a string.
    /// Left-most, consecutive, ommitted zeros are assumed tobe X.
    pub fn region_from_string_pad_x(&self, str: &str) -> Result<SomeRegion, String> {
        self.max_region.new_from_string_pad_x(str)
    }

    /// Return a SomeState instance from a string.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn state_from_string(&self, str: &str) -> Result<SomeState, String> {
        self.cur_state.new_from_string(str)
    }

    /// Return a Action number from a string with a format that the parse method can understand.
    /// Left-most, consecutive, zeros can be omitted.
    /// Returns an error if the string is bad or no action exists of that number.
    pub fn act_num_from_string(&self, str_num: &str) -> Result<usize, String> {
        match usize::from_str(str_num) {
            Ok(act_num) => {
                if act_num >= self.actions.len() {
                    return Err(format!("Action number too large {act_num}"));
                }
                Ok(act_num)
            }
            Err(error) => Err(format!("\nDid not understand action number, {error}")),
        }
    } // end act_num_from_string

    /// Return the current maximum region that can be reached from the current state.
    pub fn reachable_region(&self) -> SomeRegion {
        SomeRegion::new(vec![
            self.cur_state.clone(),
            self.cur_state
                .bitwise_xor(&self.actions.aggregate_changes.bits_change_mask()),
        ])
    }

    pub fn regions_not_covered(&self, act_num: usize) -> RegionStore {
        let mut ncov = RegionStore::new(vec![self.reachable_region()]);

        for grpx in self.actions[act_num].groups.iter() {
            ncov = ncov.subtract_region(&grpx.region);
        }
        ncov
    }

    /// Display anchor rates, like (number adjacent anchors, number other adjacent squares only in one region, samples)
    pub fn display_action_anchor_info(&self, act_num: usize) -> Result<(), String> {
        let max_region = self.reachable_region();

        self.actions[act_num].display_anchor_info()?;

        let whats_left = self.regions_not_covered(act_num);
        println!(
            "\nMaximum Region: {}, Regions not covered by a group: {}",
            max_region, whats_left
        );
        Ok(())
    }

    /// Display a group anchor and adjacent squares.
    pub fn display_group_anchor_info(
        &self,
        act_num: usize,
        aregion: &SomeRegion,
    ) -> Result<(), String> {
        self.actions[act_num].display_group_anchor_info(aregion)
    }

    /// Get aggregate changes for a domain.
    pub fn aggregate_changes(&self) -> &SomeChange {
        &self.actions.aggregate_changes
    }

    /// Return the total number of groups in all the actions.
    pub fn number_groups(&self) -> usize {
        let mut tot = 0;
        for actx in self.actions.iter() {
            tot += actx.number_groups();
        }
        tot
    }

    /// Return a String representation of SomeDomain.
    fn formatted_string(&self) -> String {
        let mut rc_str = String::from("D(ID: ");

        rc_str.push_str(&self.num.to_string());

        rc_str.push_str(&format!(", Current State: {}", &self.cur_state));

        rc_str.push(')');

        rc_str
    }

    /// Plan a path, from an initial region, to a goal region,
    /// within a series of intersecting regions.
    pub fn plan_path_through_regions(
        &self,
        start_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        path_regions: &RegionStore,
    ) -> Option<PlanStore> {
        //println!(
        //    "plan_path_through_regions: starting: domain: {} cur state: {} start: {start_reg} goal: {goal_reg} path_regions: {path_regions}",
        //    self.num, self.cur_state
        //);

        if path_regions.is_empty() {
            return None;
        }

        let mut ret_plans = PlanStore::new(vec![]);

        // Generate a plan.

        // Check if no plans are needed.
        if start_reg.is_subset_of(goal_reg) {
            //println!("No plan needed");
            ret_plans.push(SomePlan::new(self.num, vec![]));
            return Some(ret_plans);
        }

        // Gather possible start regions.
        let mut start_in: Vec<&SomeRegion> = vec![];
        for pathx in path_regions.iter() {
            if pathx.is_superset_of(start_reg) {
                if pathx.is_superset_of(goal_reg) {
                    if let Some(step1_plans) = self.make_plans2(
                        &SomeRegion::new(vec![self.cur_state.clone()]),
                        goal_reg,
                        Some(pathx),
                    ) {
                        for planx in step1_plans.iter() {
                            if planx.path_region().expect("SNH").is_subset_of(pathx) {
                                ret_plans.push(planx.clone());
                                return Some(ret_plans);
                            }
                        }
                    } else {
                        //println!("No plans found");
                    }
                }
                start_in.push(pathx);
            }
        }
        //println!("start {start_reg} in {}", tools::vec_ref_string(&start_in));
        if start_in.is_empty() {
            //println!("Start_in is empty");
            return None;
        }

        // Gather possible goal regions.
        let mut goal_in: Vec<&SomeRegion> = vec![];
        for pathx in path_regions.iter() {
            if pathx.is_superset_of(goal_reg) {
                goal_in.push(pathx);
            }
        }
        //println!("goal {goal_reg} in {}", tools::vec_ref_string(&goal_in));
        if goal_in.is_empty() {
            //println!("Goal _in is empty");
            return None;
        }

        // Init vector for paths thas lead from a domain start region to the goal region.
        let mut paths: Vec<Path> = vec![];

        // Init start stack, levels of connections with non-negative regions.
        let mut start_stack = vec![];
        // Add first level to start stack.
        start_stack.push(start_in);

        // Init goal stack, levels of connections with non-negative regions.
        let mut goal_stack = vec![];
        // Add first level to goal stack.
        goal_stack.push(vec![goal_reg]);
        // Add next level.
        goal_stack.push(goal_in);

        let mut intersections = vec![];
        let mut more_found = true;

        // Add next levels, until no more can be found, or a connection(s) between the stacks is found.
        while more_found {
            more_found = false;

            //println!("Start Levels:");
            //for (inx, levelx) in start_stack.iter().enumerate() {
            //    println!("  {}  {}", inx, tools::vec_ref_string(levelx));
            //}
            //println!("Goal Levels:");
            //for (inx, levelx) in goal_stack.iter().enumerate() {
            //    println!("  {} {}", inx, tools::vec_ref_string(levelx));
            //}

            // Check if any top-level non-negative start region intersects an existing item is in the goal stack.
            if let Some(start_last_level) = start_stack.last() {
                let start_levx = start_stack.len() - 1;
                for (start_itemx, regx) in start_last_level.iter().enumerate() {
                    for (goal_levx, levelx) in goal_stack.iter().enumerate() {
                        for (goal_itemx, regy) in levelx.iter().enumerate() {
                            if let Some(_regsint) = regx.intersection(regy) {
                                //println!(
                                //    "  new start reg {} intersects goal reg {} at {}",
                                //    regx, regy, regsint
                                //);
                                let item = (start_levx, start_itemx, goal_levx, goal_itemx);
                                intersections.push(item);
                            }
                        } // next regsx
                    } // next levelx
                } // next regsx
            }

            // Check if any top-level non-negative goal region intersects an existing item is in the start stack.
            if let Some(goal_last_level) = goal_stack.last() {
                let goal_levx = goal_stack.len() - 1;
                for (goal_itemx, regx) in goal_last_level.iter().enumerate() {
                    for (start_levx, levelx) in start_stack.iter().enumerate() {
                        for (start_itemx, regy) in levelx.iter().enumerate() {
                            if let Some(_regsint) = regx.intersection(regy) {
                                //println!(
                                //    "  new goal regs {} intersects start regs {} at {}",
                                //    regx, regy, regsint
                                //);
                                let item = (start_levx, start_itemx, goal_levx, goal_itemx);
                                if !intersections.contains(&item) {
                                    intersections.push(item);
                                }
                            }
                        } // next regsx
                    } // next levelx
                } // next regsx
            }

            if !intersections.is_empty() {
                //print!("Intersections found");
                break;
            }

            // Get next level for the start stack.
            let mut next_start_level = vec![];
            'next_nnx: for selnnx in path_regions.iter() {
                // Check if selnnx is in the stack already.
                for levelx in start_stack.iter() {
                    for regx in levelx.iter() {
                        if regx == &selnnx {
                            continue 'next_nnx;
                        }
                    } // next regsx
                } // next levelx

                // Check for intersection with higest level.
                if let Some(levelx) = start_stack.last() {
                    for regx in levelx.iter() {
                        if selnnx.intersects(regx) {
                            next_start_level.push(selnnx);
                        }
                    } // next regsx
                }
            } // next selnnx

            // Get next level for the goal stack.
            let mut next_goal_level = vec![];
            'next_nnx: for nnx in path_regions.iter() {
                // Check if selnnx is in the stack already.
                for levelx in goal_stack.iter() {
                    for regx in levelx.iter() {
                        if regx == &nnx {
                            continue 'next_nnx;
                        }
                    } // next regsx
                } // next levelx
                  // Check for intersection with higest level.
                if let Some(levelx) = goal_stack.last() {
                    for regx in levelx.iter() {
                        if nnx.intersects(regx) {
                            next_goal_level.push(nnx);
                        }
                    } // next regsx
                }
            } // next selnnx

            if next_start_level.is_empty() && next_goal_level.is_empty() {
                //println!("No next level found");
                return None;
            }
            if !next_start_level.is_empty() {
                more_found = true;
                start_stack.push(next_start_level);
            }
            if !next_goal_level.is_empty() {
                more_found = true;
                goal_stack.push(next_goal_level);
            }
        } // end while

        if intersections.is_empty() {
            //println!("No intersections to process");
            return None;
        }

        //println!("intersections found");
        //for (start_levx, start_itemx, goal_levx, goal_itemx) in intersections.iter() {
        //    println!(
        //        "   {} and {} at {}",
        //        start_stack[*start_levx][*start_itemx],
        //        goal_stack[*goal_levx][*goal_itemx],
        //        start_stack[*start_levx][*start_itemx]
        //            .intersection(goal_stack[*goal_levx][*goal_itemx])
        //            .expect("SNH")
        //    );
        //}
        //println!("Search for paths");

        // Build paths from intersections.
        for (start_levx, start_itemx, goal_levx, goal_itemx) in intersections.iter() {
            //println!(
            //    "   {} and {} at {}",
            //    start_stack[*start_levx][*start_itemx],
            //    goal_stack[*goal_levx][*goal_itemx],
            //    start_stack[*start_levx][*start_itemx]
            //        .intersection(goal_stack[*goal_levx][*goal_itemx])
            //        .expect("SNH")
            //);

            // Find start side.
            let mut start_side = Vec::<&SomeRegion>::with_capacity(*start_levx + 1);
            start_side.push(start_stack[*start_levx][*start_itemx]);
            for levx in (0..*start_levx).rev() {
                let Some(start_match) = start_side.last() else {
                    panic!("SNH");
                };
                for start_itemx in start_stack[levx].iter() {
                    if start_itemx.intersects(start_match) {
                        start_side.push(start_itemx);
                        break;
                    }
                }
            }
            start_side.reverse();
            //println!("start side: {}", tools::vec_ref_string(&start_side));

            // Find goal side.
            let mut goal_side = Vec::<&SomeRegion>::with_capacity(*goal_levx + 1);
            goal_side.push(goal_stack[*goal_levx][*goal_itemx]);
            for levx in (0..*goal_levx).rev() {
                let Some(goal_match) = goal_side.last() else {
                    panic!("SNH");
                };
                for goal_itemx in goal_stack[levx].iter() {
                    if goal_itemx.intersects(goal_match) {
                        goal_side.push(goal_itemx);
                        break;
                    }
                }
            }
            //println!("goal side:  {}", tools::vec_ref_string(&goal_side));

            // Check if the start siede end is eual to the goal_side begin.
            if let Some(start_end) = start_side.last() {
                if let Some(goal_start) = goal_side.first() {
                    if std::ptr::eq(start_end, goal_start) {
                        start_side.pop();
                    }
                }
            }

            start_side.append(&mut goal_side);
            let mut full_path = start_side;
            full_path.append(&mut goal_side);
            //println!("Full path: {}", tools::vec_ref_string(&full_path));

            // Build path.
            let mut path = Path { steps: vec![] };
            let mut cur_result = start_reg.clone();

            for inx in 0..(full_path.len() - 1) {
                let intx = full_path[inx]
                    .intersection(full_path[inx + 1])
                    .expect("SNH");
                path.steps.push(PathStep {
                    from: cur_result.clone(),
                    to: intx.clone(),
                    within: full_path[inx].clone(),
                });
                cur_result = intx;
            }
            paths.push(path);
        } // End Build paths from intersections.

        //println!("Paths:");
        //for pathx in paths.iter() {
        //    println!("    {}", tools::vec_string(&pathx.steps));
        //}
        if paths.is_empty() {
            //println!("No path found");
            return None;
        }

        // Get some plans from paths.
        let mut plans = (0..1)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .map(|_| self.plan_path_through_regions2(start_reg, goal_reg, &paths))
            .flatten()
            .collect::<Vec<SomePlan>>();

        // Check for failure.
        if plans.is_empty() {
            //println!(
            //    "plan_path_through_regions: No plan found for domain {}",
            //    self.num
            //);
            return None;
        }

        let selected_plan = plans.swap_remove(rand::thread_rng().gen_range(0..plans.len()));

        //println!("Returning {}", selected_plan);
        ret_plans.push(selected_plan);

        // Return one of the plans, avoiding the need to clone.
        Some(ret_plans)
    } // end plan_path_through_regions

    /// Find esteps thur a series of intersecting regions.
    fn plan_path_through_regions2(
        &self,
        _start_reg: &SomeRegion,
        _goal_reg: &SomeRegion,
        paths: &[Path],
    ) -> Vec<SomePlan> {
        //println!(
        //    "plan_path_through_regions2: starting: start reg  {start_reg}, goal_reg {goal_reg}"
        //);
        //for pathx in paths.iter() {
        //println!("A path");
        //    for pairx in pathx.steps.iter() {
        //        println!(
        //            "    from {} to {} staying within {}",
        //            pairx.from, pairx.to, pairx.within
        //        );
        //    }
        //}

        // Select a path.
        let pathx = &paths[rand::thread_rng().gen_range(0..paths.len())];
        //println!("Chosen path");
        //for pairx in pathx.steps.iter() {
        //    println!(
        //        "    from {} to {} staying within {}",
        //        pairx.from, pairx.to, pairx.within
        //    );
        //}

        // Process each path step.
        let mut cur_rslt = pathx.steps[0].from.clone();
        let mut plans = Vec::<SomePlan>::new();

        for stepx in pathx.steps.iter() {
            // Process each step, domain by domain.

            if stepx.from.is_subset_of(&stepx.to) {
                //println!(
                //    "dom {} from {} is a subset of to {}",
                //    self.num, stepx.from, stepx.to
                //);
                continue;
            }

            assert!(cur_rslt.is_subset_of(&stepx.from));

            // Get plans from start region to intersection region.
            let Some(step1_plans) = self.make_plans2(&cur_rslt, &stepx.to, Some(&stepx.within))
            else {
                //println!(
                //    "dom {} no plans found from {} to {}",
                //    self.num, cur_rslt, stepx.to
                //);
                return vec![];
            };

            //println!(
            //    "dom {}  {} step1_plans found from {} is a subset of to {}",
            //    self.num,
            //    step1_plans.len(),
            //    stepx.from,
            //    stepx.to
            //);

            let mut inps = vec![];
            for (inp, plan1) in step1_plans.iter().enumerate() {
                // Check plan stays within path.
                let path_region = if plan1.is_empty() {
                    self.current_region()
                } else {
                    plan1.path_region().expect("SNH")
                };
                // Check result is OK.
                let rslt = plan1.result_region();
                if !rslt.is_subset_of(&stepx.to) {
                    //println!(
                    //    "    plan {} result {} is not within {}",
                    //    plan1, rslt, stepx.to
                    //);
                    continue;
                }
                // Check path is OK.
                if !path_region.is_subset_of(&stepx.within) {
                    //println!("    plan {} does NOT stay within {}", plan1, stepx.within);
                    continue;
                }
                //println!(
                //    "    plan {} stays within {}, result {} in {}",
                //    plan1, stepx.within, rslt, stepx.to
                //);

                inps.push(inp);
            } // next plan1
            if inps.is_empty() {
                //println!("No plan found");
                return vec![];
            }
            let inp = inps[rand::thread_rng().gen_range(0..inps.len())];
            //println!("plan {} chosen {}", inp, step1_plans[inp]);
            cur_rslt = step1_plans[inp].result_region().clone();

            if plans.is_empty() {
                plans = vec![step1_plans[inp].clone()];
            } else if let Some(extended_plan) = plans[0].link(&step1_plans[inp]) {
                plans = vec![extended_plan];
            } else {
                panic!("Plan link failure, {} to {}", plans[0], step1_plans[inp]);
            }
        } // next stepx
          //if plans.is_empty() {
          //    println!("Plan NOT found");
          //} else {
          //    println!("Plan found: {}", plans[0]);
          //}
        plans
    } // end plan_path_through_regions2
} // end impl SomeDomain

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domainstore::DomainStore;

    /// Return true if a need with a given type and target is in a NeedStore.
    fn contains_similar_need(nds: &NeedStore, name: &str, target: &SomeRegion) -> bool {
        for nedx in nds.iter() {
            if nedx.name() == name {
                for targx in nedx.target().iter() {
                    if targx.region == *target {
                        return true;
                    }
                }
            }
        }
        false
    }

    // Test a simple four-step plan to change the domain current state
    // from s0111 to s1000.
    #[test]
    fn make_plan_direct() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(1);
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        let mut dmxs = DomainStore::new(vec![dm0]);
        let dm0 = &mut dmxs[0];

        let s0 = dm0.state_from_string("s0b0")?;
        let sf = dm0.state_from_string("s0b1111")?;

        // Create group for region XXXX, Act 0.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s0.change_bit(0)));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), sf.change_bit(0)));

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s0.change_bit(1)));
        dm0.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sf.change_bit(1)));

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s0.change_bit(2)));
        dm0.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sf.change_bit(2)));

        // Create group for region XXXX, Act 3.
        dm0.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s0.change_bit(3)));
        dm0.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), sf.change_bit(3))); // Last sample changes current state to s0111

        // Get plan for 7 to 8
        let cur_state = dm0.state_from_string("s0b111")?;
        dm0.set_state(&cur_state);
        let toreg = dm0.region_from_string("r1000")?;

        if dmxs.get_plans(0, &toreg).is_some() {
        } else {
            return Err(String::from("No plan found to r1000?"));
        }

        Ok(())
    }

    // Test asymmetric chaining.  The plan may step out of the direct
    // glide path X1XX, between 7 and C, into X0XX, to change the third bit,
    // then step back into the glide path to get to the goal.
    #[test]
    fn make_plan_asymmetric() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(1);
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        dm0.add_action();
        let mut dmxs = DomainStore::new(vec![dm0]);
        let dm0 = &mut dmxs[0];

        let s0 = dm0.state_from_string("s0b0")?;
        let sf = dm0.state_from_string("s0b1111")?;
        let sb = dm0.state_from_string("s0b1011")?;

        // Create group for region XXXX->XXXx, Act 0.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s0.change_bit(0)));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), sf.change_bit(0)));

        // Create group for region XXXX->XXxX, Act 1.
        dm0.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s0.change_bit(1)));
        dm0.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sf.change_bit(1)));

        // Create group for region XXXX-XxXX, Act 2.
        dm0.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s0.change_bit(2)));
        dm0.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sf.change_bit(2)));

        // Create group for region X0XX->x0XX, Act 3.
        dm0.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s0.change_bit(3)));
        dm0.eval_sample_arbitrary(3, &SomeSample::new(sb.clone(), sb.change_bit(3)));

        println!("\nActs: {}", &dm0.actions);

        // Glide Path is 7 + C = X1XX

        // Get plan for 7 to C
        // One bit that has to change, bit 3, 0...->1..., needs to use Act 3, 00XX->10XX,
        // which is outside of the Glide Path.
        let s7 = dm0.state_from_string("s0x07")?;
        dm0.set_state(&s7);
        let toreg = dm0.region_from_string("r1100")?;

        if let Some(plans) = &mut dmxs.get_plans(0, &toreg) {
            println!("plan: {}", tools::vec_string(&plans));
        } else {
            return Err(String::from("No plan found s111 to r1100?"));
        }

        Ok(())
    }

    // Test action:get_needs StateNotInGroup, two flavors.
    #[test]
    fn need_for_state_not_in_group() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(1);
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0].state_not_in_group_needs(&dm0.cur_state);

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &dm0.region_from_string("r1")?
        ));

        // Create group for one sample
        let s1 = dm0.state_from_string("s0b1")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s1.clone(), s1.clone()));

        println!("\nActs: {}", &dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1")?)
            .is_some());

        // Invalidate group for sample 1 by giving it GT 1 different result.
        // Current state changes to zero.
        let s1 = dm0.state_from_string("s0b1")?;
        dm0.eval_sample_arbitrary(
            0,
            &SomeSample::new(s1.clone(), dm0.state_from_string("s0")?),
        );

        println!("\nActs: {}", &dm0.actions[0]);

        assert!(dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1")?)
            .is_none());

        // Check needs for pn > 1 and not in group, and current state not in a group.
        let nds1 = dm0.get_needs();
        println!("needs: {}", nds1);

        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &dm0.region_from_string("r0000")?
        ));

        Ok(())
    }

    // Test confirm_group_needs.
    #[test]
    fn need_additional_group_state_samples() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(1);
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0].state_not_in_group_needs(&dm0.cur_state);

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &dm0.region_from_string("r1")?
        ));

        // Create group for one sample
        let s1 = dm0.state_from_string("s0b1")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s1.clone(), s1.clone()));

        println!("\nActs: {}", &dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1")?)
            .is_some());

        // Expand group
        let s2 = dm0.state_from_string("s0b10")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s2.clone(), s2.clone()));

        println!("\nActs: {}", &dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("rXX")?)
            .is_some());

        let nds2 = dm0.actions[0].confirm_group_needs();
        println!("needs {}", nds2);

        assert_eq!(nds2.len(), 2);
        assert!(contains_similar_need(
            &nds2,
            "ConfirmGroup",
            &dm0.region_from_string("r1")?
        ));
        assert!(contains_similar_need(
            &nds2,
            "ConfirmGroup",
            &dm0.region_from_string("r10")?
        ));

        // Satisfy one need.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s2.clone(), s2.clone()));

        let nds3 = dm0.actions[0].confirm_group_needs();
        println!("needs {}", nds3);
        assert_eq!(nds3.len(), 1);
        assert!(contains_similar_need(
            &nds3,
            "ConfirmGroup",
            &dm0.region_from_string("r1")?
        ));

        // Satisfy second need.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s1.clone(), s1.clone()));

        let nds4 = dm0.actions[0].confirm_group_needs();
        println!("needs {}", nds4);

        // Check for no more needs.
        assert!(nds4.is_empty());

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
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(1);
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action();

        let s00 = dm0.state_from_string("s0b0")?;
        let s01 = dm0.state_from_string("s0b01")?;
        let s06 = dm0.state_from_string("s0b110")?;
        let s0d = dm0.state_from_string("s0b1101")?;

        // Create group for region XX0X.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s00.clone(), s01.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s00.clone(), s01.clone()));

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0d.clone(), s0d.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0d.clone(), s0d.clone()));

        // Create group X1XX
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s06.clone(), s06.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s06.clone(), s06.clone()));

        // Get and check needs.
        let nds1 = dm0.actions.avec[0].group_pair_needs();
        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "ContradictoryIntersection",
            &dm0.region_from_string("rX100")?
        ));

        Ok(())
    }

    #[test]
    fn limit_group_needs() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(1);
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action();

        // Set up group XXXX_XX0X->XXXX_XX0X
        let s04 = dm0.state_from_string("s0b00000100")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s04.clone(), s04.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s04.clone(), s04.clone()));

        let sf9 = dm0.state_from_string("s0b11111001")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf9.clone(), sf9.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf9.clone(), sf9.clone()));

        println!("dm0 {}", &dm0.actions[0]);

        // Set group pnc
        let grp_reg = dm0.region_from_string("rXXXX_XX0X")?;
        dm0.actions[0].set_group_pnc(&grp_reg);
        println!("dm0 {}", &dm0.actions[0]);

        let msk_f = dm0.cur_state.new_from_string("s0b1111")?.to_mask();

        let Some(nds1) = dm0.actions[0].limit_groups_needs(&msk_f) else {
            return Err("No needs?".to_string());
        };

        println!("Needs: {}", nds1);
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

        let Some(nds2) = dm0.actions[0].limit_groups_needs(&msk_f) else {
            return Err("limit_groups_needs returns None?".to_string());
        };

        println!("needs are {}", nds2);
        let s06 = dm0.state_from_string("s0b00000110")?;
        assert!(contains_similar_need(
            &nds2,
            "LimitGroupAdj",
            &SomeRegion::new(vec![s06.clone()])
        ));

        let s02 = dm0.state_from_string("s0b00000010")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s06.clone(), s02.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s06.clone(), s02.clone()));

        println!("dm0 {}", &dm0.actions[0]);
        let Some(nds3) = dm0.actions[0].limit_groups_needs(&msk_f) else {
            return Err("limit_groups_needs returns None?".to_string());
        };
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
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(1);
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action();

        let s5 = dm0.state_from_string("s0b101")?;

        let s4 = dm0.state_from_string("s0b100")?;

        let sf = dm0.state_from_string("s0b1111")?;

        let se = dm0.state_from_string("s0b1110")?;

        let s7 = dm0.state_from_string("s0b111")?;

        let rx1x1 = dm0.region_from_string("rx1x1")?;

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s5.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s4.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s5.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s4.clone()));

        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), sf.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), sf.clone()));

        println!("\nActs: {}", &dm0.actions[0]);

        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
            dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone()));
            println!("\nActs: {}", &dm0.actions[0]);

            if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
                dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone())); // pn=1, pnc condition
                assert!(!dm0.actions[0].groups.find(&rx1x1).is_some());
            } else {
                return Err(String::from("Group rx1x1 deleted too soon?"));
            }
        } else {
            return Err(String::from("Group rx1x1 was not formed by two squares?"));
        }
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
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(1);
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action();

        let s5 = dm0.state_from_string("s0b101")?;
        let s4 = dm0.state_from_string("s0b100")?;
        let sf = dm0.state_from_string("s0b1111")?;
        let se = dm0.state_from_string("s0b1110")?;
        let s7 = dm0.state_from_string("s0b111")?;

        let rx1x1 = dm0.region_from_string("rx1x1")?;

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s5.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s4.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), se.clone()));

        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), sf.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), s4.clone()));

        println!("\n1 Acts: {}", &dm0.actions[0]);
        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone()));
        println!("\n2 Acts: {}", &dm0.actions[0]);

        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone())); // cause pn-not-Two invalidation
        println!("\n3 Acts: {}", &dm0.actions[0]);

        assert!(dm0.actions[0].groups.find(&rx1x1).is_none());

        Ok(())
    } // end group_pn_u_union_then_invalidation

    // For showing something easily understandable, the groups in the program are shown
    // with four, or fewer, edges.
    // It is important to show that any arbitrary number of edges can form a group / rule.
    #[test]
    fn create_group_rule_with_ten_edges() -> Result<(), String> {
        // Create a domain that uses two integer for bits.
        let mut dm0 = SomeDomain::new(2);
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action();

        let s0 = dm0.state_from_string("s0b0001010010101000")?;
        let s1 = dm0.state_from_string("s0b1111010110101011")?;
        // Region                          XXX1010X101010XX.

        // Create group for region XXX1010X101010XX.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s0.change_bit(4)));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s1.clone(), s1.change_bit(4)));

        println!("\nActs: {}", &dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("rXXX1010X101010XX")?)
            .is_some());

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
        let act0: usize = 0;

        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(1);
        dm0.cur_state = dm0.state_from_string("s0b1011")?;
        dm0.add_action();

        // Create group XXX1 -> XXX1, no way to change any bit.
        let s0b = dm0.state_from_string("s0b1011")?;
        let s05 = dm0.state_from_string("s0b0101")?;

        // Start group.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0b.clone(), s0b.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s05.clone(), s05.clone()));

        // Confirm group.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0b.clone(), s0b.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s05.clone(), s05.clone()));

        // get_needs checks the limited flag for each group.
        let nds = dm0.get_needs();
        println!("\n{}", dm0.actions[act0]);
        println!("needs {}", nds);

        // Limited flag should be true.
        assert!(dm0.actions[act0].groups[0].limited);

        // Add a way to change bit position 1, 0->1.
        let s10 = dm0.state_from_string("s0x10")?;
        let s12 = dm0.state_from_string("s0x12")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s10.clone(), s12.clone()));

        let nds = dm0.get_needs();
        println!("\n{}", dm0.actions[act0]);
        println!("needs {}", nds);

        // Changing bit position 1 should not affect the limited flag,
        // where the group bit position one is X.
        assert!(dm0.actions[act0].groups[0].limited);

        // Add a way to change bit position 0, 0->1.
        let s21 = dm0.state_from_string("s0x21")?;
        let s20 = dm0.state_from_string("s0x20")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s21.clone(), s20.clone()));

        // Add a way to change bit position 0, 1->0.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s20.clone(), s21.clone()));

        let nds = dm0.get_needs();
        println!("\n{}", dm0.actions[act0]);
        println!("needs {}", nds);

        // Changing bit position 1 should not affect the limited flag,
        // where the group bit position one is X.
        assert!(!dm0.actions[act0].groups[0].limited);

        // Check for limit need.
        for ndx in nds.iter() {
            if ndx.name() == "LimitGroupAdj" {
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

        Err("5 LimitGroupAdj need not found?".to_string())
    }
} // end tests
