//! The SomeDomain struct, representing a pseudo Karnaugh Map with a specific number of bits.
//!
//! Contains a vector of Action structs, the current state, and a few other fields.

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
use crate::removeunordered::remove_unordered;

use std::fmt;
extern crate rand;
use rand::Rng;
use serde::{Deserialize, Serialize};

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
}

impl SomeDomain {
    /// Return a new domain instance, given the number of integers, and strings for the
    /// initial state, the optimal state(s), the index into the higher-level DomainStore.
    pub fn new(dom: usize, num_ints: usize, start_state: &str, optimal: RegionStore) -> Self {
        // Convert the state string into a state type instance.
        let cur = SomeState::new_from_string(num_ints, &start_state).unwrap();

        // Check that optimal regions, if any, are the same number of ints.
        for regx in optimal.iter() {
            assert!(regx.num_ints() == num_ints);
        }

        // Set up a domain instance with the correct value for num_ints
        return SomeDomain {
            num: dom,
            num_ints,
            actions: ActionStore::new(),
            cur_state: cur.clone(),
            boredom: 0,
            optimal_and_ints: optimal.and_intersections(),
            optimal: optimal,
        };
    }

    /// Return a copy of the current , internal, state.
    pub fn get_current_state(&self) -> SomeState {
        self.cur_state.clone()
    }

    /// Used to add a region, no subsets, to the optimal region store
    pub fn add_optimal(&mut self, areg: SomeRegion) -> bool {
        if self.optimal.push_nosubs(areg) {
            self.optimal_and_ints = self.optimal.and_intersections();
            return true;
        }
        false
    }

    /// Delete a region from the optimal region store
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

        let agg_chgs = self.actions.get_aggregate_changes(self.num_ints);
        //println!("aggregate changes are {}", &agg_chgs);

        let mut nst = self
            .actions
            .get_needs(&self.cur_state, &agg_chgs, self.num);

        if let Some(ndx) = self.check_optimal() {
            nst.push(ndx);
        }

        for ndx in nst.iter_mut() {
            ndx.set_dom(self.num);
        }

        nst
    }

    /// Do functions related to the wish to be in an optimum region.
    /// Increment the boredom level, if needed.
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
            self.boredom = 0;
            let notsups = self.optimal_and_ints.not_supersets_of_state(&self.cur_state);
            let inx = rand::thread_rng().gen_range(0, notsups.len());
            return Some(SomeNeed::ToRegion {
                    dom_num: self.num,
                    act_num: 0,
                    goal_reg: notsups[inx].clone(),
                });
        }

        // Get the optimal regions the current state is not in.
        let mut notsups = self.optimal_and_ints.not_supersets_of_state(&self.cur_state);
        if notsups.len() == 0 {
            return None;
        }

        self.boredom += 1;

        if self.boredom > (3 * sups.len()) { // Boredom action trigger.
                
            // Get intersections, if any.
            notsups = notsups.and_intersections();

            println!("\nDom {}: I'm bored lets move to {}", self.num, &notsups);

            // Some optimal regions may not be accessable, a plan to get to the
            // selected region is not yet known.
            // Random selections may allow an accessable region to be chosen, eventually.
            // A previously unaccessable region may be come accessable with new rules.
            let inx = rand::thread_rng().gen_range(0, notsups.len());
            return Some(SomeNeed::ToRegion { dom_num: self.num, act_num: 0, goal_reg: notsups[inx].clone() });
        }

        None
    }

    /// Return the total number of actions.
    pub fn num_actions(&self) -> usize {
        self.actions.len()
    }

    /// Evaluate an arbitrary sample given by the user.
    pub fn eval_sample_arbitrary(
        &mut self,
        act_num: usize,
        i_state: &SomeState,
        r_state: &SomeState,
    ) {
        // May break alternating sample, so do not mix with take_action_need
        self.actions[act_num].eval_arbitrary_sample(i_state, r_state, self.num);
        self.set_state(&r_state);
    }

    /// Take an action for a need, evaluate the resulting sample.
    pub fn take_action_need(&mut self, ndx: &SomeNeed) {

        let act_num = ndx.act_num();

        let astate = self.actions[ndx.act_num()].take_action(self.num, &self.cur_state);

        self.actions[act_num].eval_need_sample(&self.cur_state, ndx, &astate, self.num);

        self.set_state(&astate);
    }

    /// Take an action with the current state.
    pub fn take_action(&mut self, act_num: usize) {

        let astate = self.actions[act_num].take_action(self.num, &self.cur_state);

        self.actions[act_num].eval_sample(&self.cur_state, &astate, self.num);

        self.set_state(&astate);
    }

    /// Accessor, set the cur_state field.
    pub fn set_state(&mut self, new_state: &SomeState) {
        if self.optimal_and_ints.len() > 1 {
            let opt_sups = self.optimal_and_ints.supersets_of_state(&self.cur_state);

            if opt_sups.len() > 0 {
                let opt_sups_new = self.optimal_and_ints.supersets_of_state(&new_state);
                if opt_sups != opt_sups_new {
                    self.boredom = 0;
                }
            }
        }

        self.cur_state = new_state.clone();
    }

    /// Run a plan.
    pub fn run_plan(&mut self, pln: &SomePlan) {
        self.run_plan2(pln, 0);
    }

    /// Run a plan.  Try to replan and run if a plan encounters an unexpected result.
    fn run_plan2(&mut self, pln: &SomePlan, recur: usize) {
        if recur > 3 {
            println!("\nPlan {} failed", &pln);
            return;
        }

        for stpx in pln.iter() {
            if stpx.initial.is_superset_of_state(&self.cur_state) {

                let astate = self.actions[stpx.act_num].take_action(self.num, &self.cur_state);

                self.actions[stpx.act_num].eval_step_sample(&self.cur_state, &astate, self.num);

                let prev_state = self.cur_state.clone();

                self.set_state(&astate);

                if stpx.result.is_superset_of_state(&self.cur_state) {
                    continue;
                }

                // Handle unexpected/unwanted result
                // May be an expected possibility from a two result state

                if prev_state == self.cur_state {
                    println!("Try action a second time");

                    let astate = self.actions[stpx.act_num].take_action(self.num, &self.cur_state);

                    self.actions[stpx.act_num].eval_step_sample(&self.cur_state, &astate, self.num);

                    self.set_state(&astate);

                    if stpx.result.is_superset_of_state(&self.cur_state) {
                        continue;
                    }
                }

                // Try re-plan to goal
                if let Some(planx) = self.make_plan(pln.result_region()) {
                    println!(
                        "\nChange [{} -{:02}> {}] unexpected, expected {}, new plan from {} to {} is {}",
                        &prev_state,
                        &stpx.act_num,
                        &self.cur_state,
                        stpx,
                        &self.cur_state,
                        &pln.result_region(),
                        &planx.str_terse()
                        );
                    return self.run_plan2(&planx, recur + 1);
                    //panic!("done");
                }
                println!(
                    "\nChange [{} -{:02}> {}] unexpected, expected {}, no plan from {} to {}, failed.",
                    &prev_state,
                    &stpx.act_num,
                    &self.cur_state,
                    stpx,
                    &self.cur_state,
                    &pln.result_region()
                );
                return;

            } else {
                panic!(
                    "step initial {} rule {} is not superset of the result found {}, plan building problem",
                    &stpx.initial, &stpx.rule, &self.cur_state
                );
            }
        } // next stpx
    } // end run_plan2

    /// Get the steps of a plan, wrap the steps into a plan, return Some(SomePlan).
    pub fn make_plan2(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion) -> Option<SomePlan> {
        //println!("make_plan2 start");

        if let Some(steps) = self.make_plan3(from_reg, goal_reg, 0) {
            //println!("make_plan3 returns {}", &steps);
            if steps.initial().intersects(from_reg) {
                if steps.result().intersects(goal_reg) {
                    //println!("make_plan2 returns {}", &steps);
                    return Some(SomePlan::new(steps));
                } else {
                    //println!("problem5: result {} not in steps {}", goal_reg, &steps);
                }
            } else {
                //println!("problem6: initial {} not in steps {}", from_reg, &steps);
            }
        }
        //println!("make_plan3 returns None");
        None
    }

    /// It seems possible that a failed forward-chaining path could intersect a failed backward-chaining path,
    /// so maybe forward-chaining and backward-chaining can be combined.
    ///
    /// Return the steps of a plan to go from a given region (usually a non-x region, that is a state) to a given region,
    /// using rules that are in the direct path.
    pub fn random_depth_first_chaining(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, depth: usize) -> Option<StepStore> {
        //println!("random_depth_first_chaining start");

        // Init return vector
        let mut ret_steps = StepStore::new();

        // Check for premature success.
        if from_reg.is_subset_of(goal_reg) {
            return Some(ret_steps);
        }

        // Check depth
        if depth > 10 {
            return None;
        }

        // Calculate the minimum bit changes needed.
        let required_change = SomeChange::region_to_region(from_reg, goal_reg);

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str: StepStore = self.actions.get_steps(&required_change);

        // Get from steps
        // Check if one step makes the desired change.
        let mut from_steps = StepStore::new();
        for stepx in steps_str.iter() {

            // Check if the step can be applied to the from_reg.
            if stepx.initial.intersects(from_reg) == false {
                continue;
            }

            // Restrict step to match from_reg, if needed.
            let stepz = stepx.restrict_initial_region(from_reg);

            // Check if this rule does the whole desired change.
            if stepz.result.is_subset_of(goal_reg) {
                ret_steps.push(stepz);
                return Some(ret_steps);
            }

            // Check that the restricted rule still has at least one desired change.
            let chg2 = stepz.rule.change().c_and(&required_change);
            if chg2.is_low() {
                continue;
            }

            // Save the step for later.
            from_steps.push(stepz);

        } // Next stepx.

        // Get goal steps
        let mut goal_steps = StepStore::new();
        for stepx in steps_str.iter() {

            // Check if the step can be applied to the goal_reg.
            if stepx.result.intersects(goal_reg) == false {
                continue;
            }

            // Restrict step to match goal_reg, if needed.
            let stepz = stepx.restrict_result_region(goal_reg);

            // Check that the restricted rule still has at least one desired change.
            let chg2 = stepz.rule.change().c_and(&required_change);
            if chg2.is_low() {
                continue;
            }

            // Save the step for later.
            goal_steps.push(stepz);

        } // Next stepx.

        // Choose which steps to use next
        let mut try_from_steps = true;
        let mut try_goal_steps = true;

        if from_steps.len() == 0 {
            try_from_steps = false;
        }
        if goal_steps.len() == 0 {
            try_goal_steps = false;
        }
        if try_from_steps && try_goal_steps {
            let mut choice = rand::thread_rng().gen_range(0, 111);
            choice = choice % 2;
            if choice == 0 {
                try_from_steps = false;
            } else {
                try_goal_steps = false;
            }
        }

        if try_goal_steps {

            // Choose a goal step.
            let mut inx = 0;
            if goal_steps.len() > 1 {
                inx = rand::thread_rng().gen_range(0, goal_steps.len() * 11);
                inx = inx % goal_steps.len();
            }

            // Try the rest of the path.
            if let Some(before_steps) = self.random_depth_first_chaining(&from_reg, &goal_steps[inx].initial, depth + 1) {
                ret_steps.push(goal_steps[inx].clone());
                ret_steps = before_steps.link(&ret_steps).unwrap();
//                println!("random_depth_first_chaining: Found 2 {}", &ret_steps);
                return Some(ret_steps);
            }
            return None;
        }

        if try_from_steps {

            // Choose a from step.
            let mut inx = 0;
            if from_steps.len() > 1 {
                inx = rand::thread_rng().gen_range(0, from_steps.len() * 11);
                inx = inx % from_steps.len();
            }

            // Try the rest of the path.
            if let Some(after_steps) = self.random_depth_first_chaining(&from_steps[inx].result, &goal_reg, depth + 1) {
                ret_steps.push(from_steps[inx].clone());
                ret_steps = ret_steps.link(&after_steps).unwrap();
//                println!("random_depth_first_chaining: Found 3 {}", &ret_steps);
                return Some(ret_steps);
            }
            return None;
        }

        None
    } // end random_depth_first_chaining

    /// Return true if all steps in a list do not intersect the from_region and goal region.
    fn steps_between(&self, steps: &Vec::<&SomeStep>, from_reg: &SomeRegion, goal_reg: &SomeRegion) -> bool {
        for stepx in steps.iter() {
            if stepx.initial.intersects(from_reg) {
                return false;
            }
            if stepx.result.intersects(goal_reg) {
                return false;
            }
        }
        true
    }

    /// Return true if the order of two steps vectors is OK in at lease one pair.
    fn order_ok(&self, steps_from: &Vec::<&SomeStep>, steps_to: &Vec::<&SomeStep>, required_change: &SomeChange) -> bool {
        for stepx in steps_from.iter() {
            for stepy in steps_to.iter() {
                if ptr_eq(stepx, stepy) || stepx.rule.order_ok(&stepy.rule, required_change) {
                    return true;
                } 
            }
        }
        false
    }

    /// Return indexes of bit-change vectors that should be run after another.
    fn order_after(&self, step_vecs: &Vec<Vec<&SomeStep>>, required_change: &SomeChange) -> Vec<usize> {
        let mut avec = Vec::<usize>::new();

        // Check each combination of bit-change step vectors
        for inx in 0..(step_vecs.len() - 1) {
            for iny in (inx + 1)..step_vecs.len() {
                if self.order_ok(&step_vecs[inx], &step_vecs[iny], required_change) == false {
                    if avec.contains(&inx) == false {
                        avec.push(inx);
                    }
                }
                if self.order_ok(&step_vecs[iny], &step_vecs[iny], required_change) == false {
                    if avec.contains(&iny) == false {
                        avec.push(iny);
                    }
                }
            }
        }

        avec.sort_by(|a, b| a.cmp(&b));  // Ascending order.
        //if avec.len() > 0 {
        //    println!("order_after: returning {:?}", &avec);
        //}
        avec
    }

    /// Return the steps of a plan to go from a given state to a given region.
    ///
    /// The required change of bits is calculated from the two given regions.
    ///
    /// Steps that make at least one of the needed changes are extracted from domain actions.
    ///
    /// The steps are split up into a vector of step vectors, each step vector has steps that change a 
    /// particular bit.  Often, there will only be one step in a step vector.
    ///
    /// Steps that make more than one needed bit change will be in multiple step vectors.
    ///
    /// All possible step vector pairs are checked for being mutually exclusive, that is the needed change
    /// needs to be backed off to run any step in the other step vector.  If any such pair, return None. 
    ///
    /// All possible step vector pairs are checked for needing one to be run before the other.  
    /// If found, the step vector needing to be run after is removed.
    ///
    /// All step vectors are checked for the circumstance that all have an initial region that does not 
    /// intersect the from region, and have a result region that does not intersect the goal region.
    /// If found, one step will be randomly chosen to calculate:
    /// From region to step initial region, then step result region to goal region.
    /// This have the effect of splitting the recursion depth violation risk.
    ///
    /// Steps with an initial region the intersects the from region are found.
    /// Steps with a result region that intersects the goal region are found.
    /// One of the above steps is chosen for recursion, either:
    /// From region to step initial region, or
    /// Step result region to goal region.
    pub fn make_plan3(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, depth: usize) -> Option<StepStore> {
        //println!("make_plan3: from {} to {}", from_reg, goal_reg);
        // Check if from_reg is at the goal
        if goal_reg.intersects(from_reg) {
            return Some(StepStore::new());
        }

        // Check if change is possible

        // Calculate the minimum bit changes needed.
        let required_change = SomeChange::region_to_region(from_reg, goal_reg);
        // println!("make_plan3: required_change {}", &required_change);

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str: StepStore = self.actions.get_steps(&required_change);
        //println!("\nmake_plan3: from {} to {} steps_str steps {}", from_reg, goal_reg, steps_str.formatted_string(" "));

        // Check that the steps roughly encompass all needed changes, else return None.
        let can_change = steps_str.aggregate_changes(self.num_ints);

        if required_change.is_subset_of(&can_change) {
        } else {
            // println!("make_plan3: step_vec wanted changes {} are not a subset of step_vec changes {}, returning None", &required_change, &can_change);
            return None;
        }

        // Check if one step makes the required change.
        // The ultimate end-point of any path search.
        for stepx in steps_str.iter() {
            if stepx.initial.intersects(from_reg) {

                let rslt = stepx.result_from_initial(from_reg);

                if goal_reg.intersects(&rslt) {
                    
                    let bstep = stepx.restrict_initial_region(from_reg);

                    //println!("make_plan3: suc 1 Found one step {} to go from {} to {}", &bstep, from_reg, goal_reg);
                    return Some(StepStore::new_with_step(bstep));
                }
            }
        }

        // Check recursion depth
        if depth > 5 {
            //println!("recursion depth maximum exceeded");
            return None;
        }
        
        // Sort the steps by each needed bit change. (some actions may change more than one bit, so will appear more than once)
        let mut steps_by_change_vov: Vec<Vec<&SomeStep>> = steps_str.steps_by_change_bit(&required_change);

        // Check if any changes, all steps, are mutually exclusive.
        if any_mutually_exclusive_changes(&steps_by_change_vov, &required_change) {
            //println!("make_plan3: mutually exclusive change rules found");
            return None;
        }

        // Take out bit changes that must be run after others.
        let after_inxs: Vec::<usize> = self.order_after(&steps_by_change_vov, &required_change);
        if after_inxs.len() > 0 {
            //println!("order_after:  removing {} of {} step vectors", &after_inxs.len(), &steps_by_change_vov.len());

            for inx in after_inxs.iter().rev() {
                //for stepx in steps_by_change_vov[*inx].iter() {
                    //println!("removing inx {} step {}", inx, stepx);
                //}
                remove_unordered(&mut steps_by_change_vov, *inx);
            }
            //for inx in 0..steps_by_change_vov.len() {
                //for iny in 0..steps_by_change_vov[inx].len() {
                    //println!("remaining inx {} step {}", inx, &steps_by_change_vov[inx][iny]);
                //}
            //}
        }

        // Check for a bit change vector with all steps not reachable by the needed changes.
        // Set a flag, asym_steps.
        let mut between_bit_changes = Vec::<usize>::new();

        for inx in 0..steps_by_change_vov.len() {
    
            if self.steps_between(&steps_by_change_vov[inx], from_reg, goal_reg) {
                //for stepx in steps_by_change_vov[inx].iter() {
                    //println!("step_between inx {} {}", &inx, &stepx);
                //}
                between_bit_changes.push(inx);
            }
        } // next inx

        if between_bit_changes.len() > 0 {

            //println!("between_bit_changes {:?}", between_bit_changes);
            let mut inx = rand::thread_rng().gen_range(0, between_bit_changes.len());
            inx = between_bit_changes[inx]; // get index to steps_by_change_vov
            
            //for stepx in steps_by_change_vov[inx].iter() {
            //    println!("step_between based on random inx {} {}", &inx, &stepx);
            //}
            let iny = rand::thread_rng().gen_range(0, steps_by_change_vov[inx].len());
            //println!("step between based on inx, iny {} {} {}", &inx, &iny, &steps_by_change_vov[inx][iny]);

            // Try Asymmetric chaining
            if let Some(ret_steps) = self.asymmetric_chaining_step(from_reg, goal_reg, steps_by_change_vov[inx][iny], depth + 1) {
                //println!("make_plan3: asym returns steps {}", ret_steps.formatted_string(" "));
                return Some(ret_steps);
            }
            return None;
        }

        // Evaluate glide path.
        // Some steps may intersect both the from region and goal region without
        // being a single step solution.
        let mut steps_from = false;
        let mut steps_goal = false;

        for stpx in steps_str.iter() {
            if steps_from == false && stpx.initial.intersects(from_reg) {
                steps_from = true;
            }
            if steps_goal == false && stpx.result.intersects(goal_reg) {
                steps_goal = true;
            }
        }

        if steps_from == false && steps_goal == false {
            //println!("no steps in glide path from {} goal {}", &steps_from, &steps_goal);
            return None;
        }

        // Initalization for chaining
        let num_tries = 4;
        let mut step_options = Vec::<StepStore>::with_capacity(num_tries);

        for _ in 0..num_tries {
            if let Some(steps4) = self.random_depth_first_chaining(from_reg, goal_reg, 0) {
                if steps4.initial().intersects(from_reg) {
                    if steps4.result().intersects(goal_reg) {
                        //println!("random_depth_first_chaining: for {} to {}, returns {}", from_reg, goal_reg, &steps4);
                        step_options.push(steps4);
                    }
                }
            }
        }// next try

        if step_options.len() > 0 {
            let inx = choose_one(&step_options);
            //println!("chaining test worked! {}", &step_options[inx]);
            return Some(step_options[inx].clone());
        }

        None
    } // end make_plan3

    /// Asymmetric chaining for a given step
    fn asymmetric_chaining_step(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, stepx: &SomeStep, depth: usize) -> Option<StepStore> {
        //println!("asymmetric_chaining_step from {} to {} with step {}", from_reg, goal_reg, stepx);
        let mut ret_stepsx = StepStore::new();

        let mut stepy = stepx.clone();

        // Part one
        //println!("asymmetric_chaining_step part 1");
        if stepy.initial.is_superset_of(from_reg) {
            stepy = stepy.restrict_initial_region(from_reg);
            ret_stepsx.push(stepy.clone());
            //println!("asymmetric_chaining_step steps to 1 = {}", &ret_stepsx);
        } else {
            if let Some(mut gap_steps1) = self.make_plan3(from_reg, &stepy.initial, depth + 1) {
                ret_stepsx.append(&mut gap_steps1);
                if ret_stepsx.result().intersects(&stepy.initial) {
                    ret_stepsx.push(stepx.restrict_initial_region(&ret_stepsx.result()));
                    //println!("asymmetric_chaining_step steps to 2 = {}", &ret_stepsx);
                } else {
                   // println!("asymmetric_chaining_step returning 1 None");
                    return None;
                //println!("xx for from {} to {} step {}", from_reg, goal_reg, stepx);
                //println!("plan {} result does not intersect {}", &ret_stepsx, &stepx.initial);
                //assert!(1 == 2);
                }
            } else {
                //println!("asymmetric_chaining_step returning 2 None");
                return None;
            }
        }

        // Part two
        //println!("asymmetric_chaining_step part 2");
        let result1 = ret_stepsx.result();

        if result1.intersects(goal_reg) {
           // println!("asymmetric_chaining_step part 2 from 1 {}", &ret_stepsx);
            return Some(ret_stepsx);
        }

        if let Some(gap_steps2) = self.make_plan3(&result1, goal_reg, depth + 1) {
            if let Some(ret_steps3) = ret_stepsx.link(&gap_steps2) {
               // println!("asymmetric_chaining_step step part 2 from  2= {}", &ret_steps3);
                return Some(ret_steps3);
            }
        }
        //println!("asymmetric_chaining_step returning 3 None");
        None
    }

    /// Make a plan from a region to another region.
    /// Since there are some random choices, it may be useful to try
    /// running make_one_plan more than once.
    pub fn make_plan(&self, goal_reg: &SomeRegion) -> Option<SomePlan> {
        // Check if a need can be achieved, if so store index and Option<plan>.
        // Higher priority needs that can be reached will superceed lower priority needs.
        //println!("make_plan start");

        if goal_reg.is_superset_of_state(&self.cur_state) {
            return Some(SomePlan::new(StepStore::new()));
        }

        for _ in 0..2 {

            if let Some(planz) = self.make_plan2(&SomeRegion::new(&self.cur_state, &self.cur_state), &goal_reg) {
                // println!("make_plan2 worked!");
                return Some(planz);
            }

        }

        None
    } // end make plan

    /// Change the current state to be within a given region.
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

    // Return a StepStore using random depth-first backward chaining to a state from a goal.
//    fn random_depth_first_backward_chaining(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, steps_str: &StepStore, depth: usize) -> Option<StepStore> {

        //println!("random_depth_first_backward_chaining: to {} from {}", from_reg.formatted_string(), goal_reg.formatted_string());

//        if depth > 20 {
//            println!("random_depth_first_backward_chaining: depth exceeded");
//            return None;
//        }

        // Initialize vector for possible options.
        // Most StepStores will contain one step.  Some will contain two steps, due
        // to undoing an unwanted change.
//        let mut steps_rev2 = Vec::<StepStore>::with_capacity(steps_str.len());

        // Check each step result region for intersection with the goal_reg.
        // If so, try to develop an option for getting closer to the from_reg, from the goal_reg.
//        for stpy in steps_str.iter() {

//            if stpy.result.intersects(goal_reg) {
//            } else {
//                continue;
//            }

//            let stpstr = StepStore::new_with_step(stpy.restrict_result_region(goal_reg));
//            //println!("testing: adding1 stepstore {}", &stpstr);
//            steps_rev2.push(stpstr);

//        } // next stpy

//        if steps_rev2.len() == 0 {
//            return None;
//        }

        // Get stepstores that have a result region that intersects the goal region
//        let mut next_steps = Vec::<usize>::new();
//        let mut inx = 0;
//        for stpstrx in &steps_rev2 {
//            if stpstrx.initial().intersects(from_reg) {
//                next_steps.push(inx);
//            }
//            inx += 1;
//        }

        // Pick a solution stepstore that works, if there are any
//        for inx in &next_steps {
//            if steps_rev2[*inx].initial().intersects(from_reg) {
//                if let Some(steps2) = steps_rev2[*inx].restrict_initial_region(from_reg) {
//                    if steps2.result() .intersects(goal_reg) {
//                        return Some(steps2.restrict_result_region(goal_reg).unwrap());
//                    }
//                }
//            }
//        } // next stpstrx

        // Randomly pick a step index from the options.
//        let step_inx = rand::thread_rng().gen_range(0, steps_rev2.len());

        // Calculate the next region from the step
//        let prev_reg = steps_rev2[step_inx].initial();

        // Insure that the step gets closer to the start
//        let dist_cur = goal_reg.distance(from_reg);
//        let dist_next = prev_reg.distance(from_reg);
//        if dist_cur >= dist_next {
//            return None;
//        }

        // Use recursion to get next steps
//        if let Some(more_steps) = self.random_depth_first_backward_chaining(from_reg, &prev_reg, steps_str, depth + 1) {

//            if let Some(ret_steps2) = more_steps.link(&steps_rev2[step_inx]) {
//                return Some(ret_steps2);
//            }
//        }

//        None
//    } // end random_depth_first_backward_chaining

    // Return a StepStore from random depth-first forward chaining to goal.
    // The steps_str argument is a vector of all steps that change at least one bit that is required to reach the goal.
    // If a step contains an unwanted change, see if a second step will undo the unwanted change.
    // Without correcting an unwanted change, such a step will be missed since it does not get closer to the goal.
    // Recursion depth check is not needed due to the requirement that each call gets at least one bit
    // closer to the goal.
//    fn random_depth_first_forward_chaining(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, steps_str: &StepStore, depth: usize) -> Option<StepStore> {
        //println!("rdffc: from {} to {} steps {}", from_reg, goal_reg, steps_str);

//        if depth > 20 {
//            println!("random_depth_first_forward_chaining: depth exceeded");
//            return None;
//        }
        
        // Get the bit changes required to go from from_reg to goal_reg.
//        let required_change = SomeChange::region_to_region(from_reg, goal_reg);
        //println!("rdffc: required change: {}", &required_change);

        // Initialize vector for possible options.
        // Most StepStores will contain one step.  Some will contain two steps, due
        // to undoing an unwanted change. 
//        let mut steps_rev2 = Vec::<StepStore>::with_capacity(steps_str.len());

        // Bits in the from_reg that correspond to X-bits in the goal, can change in any way.
//        let care_chg_mask = goal_reg.x_mask().m_not();

        // Check each step initial region for intersection with the from_reg.
        // If so, try to develop an option for getting closer to the goal.
//        for stpy in steps_str.iter() {

//            if stpy.initial.intersects(from_reg) {
//            } else {
//                continue;
//            }

            // Restrict step to non-X bits in the from region
//            let stpx = stpy.restrict_initial_region(from_reg);

            // Get the changes in the step that are not required, that are unwanted, and
            // do not correspond with an X-bit in the goal.
//            let chg_not = &required_change.c_not();
//            let chg_dif = stpx.rule.change().c_and(&chg_not).c_and_mask(&care_chg_mask);

//            if chg_dif.is_low() {
                // No unwanted changes
//                let stpstr = StepStore::new_with_step(stpx);
                //println!("testing: adding1 stepstore {}", &stpstr);
//                steps_rev2.push(stpstr);
//            } else {
                // Unwanted changes found
                //println!("testing: required_change {} step chg: {} unneeded chg: {}", &required_change, stpx.rule.change(), &chg_dif);

                // Calc the changes that will reverse the unwanted changes
//                let chg_rev = chg_dif.change_reverse();

                // Get a vector of steps that may accomplish the reversal.
//                let steps_rev : StepStore = self.actions.get_steps_exact(&chg_rev);
                //println!("testing: rev steps found: {}", &steps_rev);

                // Check each possible reverse step for intersection with
                // the current steps result region and exact change needed.
//                for stp_revx in steps_rev.iter() {

//                    if stp_revx.initial.intersects(&stpx.result) {
//                    } else {
//                        continue;
//                    }

                    // Restrict the second step to synchronize with the first step.
//                    let stp2 = stp_revx.restrict_initial_region(&stpx.result);

                    // Check for exact match to the needed reverse change(s)
//                    if stp2.change() == chg_rev {

                        // Sync the intersection backward to the first step
//                        let stp1 = stpx.restrict_result_region(&stp2.initial);

                        // Check that all changes are not lost.
//                        if stp1.change().is_not_low() {

                            // Check that the required change is not lost
//                            if stp1.change().intersects(&required_change) {

                                // If the required change is in the step, it must be closer to the goal_reg
                                
                                // Check if the unwanted change has been lost
//                                if stp2.change() == chg_rev {
//                                    let mut stpstr = StepStore::new_with_step(stp1);
//                                    stpstr.push(stp2);
//                                    //println!("testing: adding2 stepstore {}", &stpstr);
//                                    steps_rev2.push(stpstr);
//                                } else {
//                                    continue;
//                                }
// dist                           } else {
//                                println!("glitch3 stp1 chg {} is not subset req chg {}", &stp1.change(), &required_change);
//                            }
//                        } else {
//                            println!("glitch2 stp1 change is {}", &stp1.change());
//                        }
//                    } else {
                        //println!("glitch1 stp2 chg {} not eq rev chg needed {}", &stp2.change(), &chg_rev);
//                        let stpstr = StepStore::new_with_step(stpx.clone());
                        //println!("testing: adding1 stepstore {}", &stpstr);
//                        steps_rev2.push(stpstr);
//                    }
//                } // next stp_revx
//            } //end if
//        } // next stpy

        //println!("rdffc: steps_rev2:");
        //for strtmp in &steps_rev2 {
        //    print!(" {}", strtmp);
        //}
        //println!(" ");
//        if steps_rev2.len() == 0 {
//            return None;
//        }

        // Get stepstores that have a result region that intersects the goal region
//        let mut next_steps = Vec::<usize>::new();
//        let mut inx = 0;
//        for stpstrx in &steps_rev2 {
//            if stpstrx.result().intersects(goal_reg) {
//                next_steps.push(inx);
//            }
//            inx += 1;
//        }

        // Randomly pick a solution stepstore, if there are any
//        if next_steps.len() > 0 {
//            let mut step_inx_inx = 0;
//            if next_steps.len() > 1 {
//                step_inx_inx = rand::thread_rng().gen_range(0, next_steps.len());
//            }
//            let step_inx = next_steps[step_inx_inx];
//            return Some(steps_rev2[step_inx].clone());
//        }

        // Randomly pick a step index from the options.
//        let step_inx = rand::thread_rng().gen_range(0, steps_rev2.len());

        // Calculate the next region from the step
//        let next_reg = steps_rev2[step_inx].result();

        // Insure the step gets closer to the goal
//        let dist_cur = goal_reg.distance(goal_reg);
//        let dist_next = next_reg.distance(goal_reg);
//        if dist_cur >= dist_next {
//            return None;
//        }

        // Use recursion to get next steps
//        if let Some(more_steps) = self.random_depth_first_forward_chaining(&next_reg, goal_reg, steps_str, depth + 1) {

//            if let Some(ret_steps2) = steps_rev2[step_inx].link(&more_steps) {
//                return Some(ret_steps2);
//            }
//        }

//        None
//    } // end random_depth_first_forward_chaining_test

    /// Return a Action number from a string.
    /// Left-most, consecutive, zeros can be omitted.
    ///
    /// if let Ok(sta) = SomeState::state_from_string(1, "0101")) {
    ///    println!("State {}", &sta);
    /// } else {
    ///    panic!("Invalid State");
    /// }
    pub fn act_num_from_string(&self, str_num: &str) -> Result<usize, String> {

        match str_num.parse() {
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

} // end impl SomeDomain

/// Return true if two references are identical, thanks to
/// github.com/rust-lang/rfcs/issues/1155, (eddyb, kimundi and RalfJung)
fn ptr_eq<T>(a: *const T, b: *const T) -> bool { a == b }

/// Return true if any step pairs are all mutually exclusive
fn any_mutually_exclusive_changes(by_change: &Vec<Vec<&SomeStep>>, wanted: &SomeChange) -> bool {

    for inx in 0..(by_change.len() - 1) {
        for iny in (inx+1)..by_change.len() {
            
            let mut exclusive = true;
            //println!("mex checking {:?} and {:?}", &by_change[inx], &by_change[iny]);
            for refx in by_change[inx].iter() {
                for refy in by_change[iny].iter() {
                    if ptr_eq(*refx, *refy) {
                        exclusive = false;
                        continue;
                    }
                    if refx.mutually_exclusive(refy, wanted) {
                        //println!("step {} mutually exclusive to step {}", refx, refy);
                    } else {
                        exclusive = false;
                        break;
                    }
                } //next numy
                if exclusive == false {
                    break;
                }

            } // next refx
            //println!(" ex: {}", exclusive);
            if exclusive {
                //println!("exclusive inx {} iny {}", inx, iny);
                return true;
            }
        } // next iny
    } // next inx

    false
}

/// Return the index value of a chosen StepStore
fn choose_one(ret_steps: &Vec::<StepStore>) -> usize {
    assert!(ret_steps.len() > 0);

    if ret_steps.len() == 1 {
        return 0;
    }

    let mut min_len = 9999;
    let mut max_len = 0;
    for rets in ret_steps.iter() {
        if rets.len() < min_len {
            min_len = rets.len();
        }
        if rets.len() > max_len {
            max_len = rets.len();
        }
    } // next rets

    //println!("ret_steps len = {} min {} max {}", ret_steps.len(), &min_len, &max_len);
    let mut chosen = 0;
    if ret_steps.len() > 1 {
        // Choose step, TODO better criteria, maybe total of least rule "cost", or negative/positive effects.
        let mut inx_ary = Vec::<usize>::new();
        let mut inx = 0;
        for rets in ret_steps.iter() {
            if rets.len() == min_len {
                inx_ary.push(inx);
            }
            inx += 1;
        }
        if inx_ary.len() == 1 {
            chosen = inx_ary[0];
        } else {
            chosen = inx_ary[rand::thread_rng().gen_range(0, inx_ary.len())];
        }
    }
    chosen
} // end choose_one

#[cfg(test)]
mod tests {
    use crate::mask::SomeMask;
    use crate::change::SomeChange;
    //use crate::need::SomeNeed;
    use crate::region::SomeRegion;
    use crate::domain::SomeDomain;
    use crate::regionstore::RegionStore;
    
    // Test a simple four-step plan to change the domain current state 
    // from s0111 to s1000.
    #[test]
    fn make_plan_direct() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
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
        dm0.eval_sample_arbitrary(3, &sf, &sf.toggle_bits(vec![3]));    // Last sample changes current state to s0111

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
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
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
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0].state_not_in_group_needs(&dm0.cur_state);

        assert!(nds1.len() == 1);
        assert!(nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap()));

        // Create group for one sample
        let s1 = dm0.state_from_string("s1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("r1").unwrap()) {
        } else {
            return Err("Group r1 not found ??".to_string());
        }

        // Invalidate group for sample 1 by giving it GT 1 different result.
        // Current state changes to zero.
        let s1 = dm0.state_from_string("s1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1.toggle_bits(vec![0]));
        
        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("r1").unwrap()) {
            return Err("Group r1  found ??".to_string());
        }

        // Check needs for pn > 1 and not in group, and current state not in a group.
        let nds1 = dm0.get_needs();

        assert!(nds1.len() == 2);
        assert!(nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap()));
        assert!(nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r0").unwrap()));

        Ok(())
    }

    // Test additional_group_state_samples.
    #[test]
    fn need_additional_group_state_samples()  -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0].state_not_in_group_needs(&dm0.cur_state);

        assert!(nds1.len() == 1);
        assert!(nds1.contains_similar_need("StateNotInGroup", &dm0.region_from_string("r1").unwrap()));

        // Create group for one sample
        let s1 = dm0.state_from_string("s1").unwrap();
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("r1").unwrap()) {
        } else {
            return Err("Group r1 not found ??".to_string());
        }

       // Expand group
        let s2 = dm0.state_from_string("s10").unwrap();
        dm0.eval_sample_arbitrary(0, &s2, &s2);

        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("rXX").unwrap()) {
        } else {
            return Err("Group rXX not found ??".to_string());
        }
        
        let nds2 = dm0.actions[0].additional_group_state_samples();
        //println!("needs {}", nds2);

        assert!(nds2.len() == 2);
        assert!(nds2.contains_similar_need("StateAdditionalSample", &dm0.region_from_string("r1").unwrap()));
        assert!(nds2.contains_similar_need("StateAdditionalSample", &dm0.region_from_string("r10").unwrap()));

        // Satisfy one need.
        dm0.eval_sample_arbitrary(0, &s2, &s2);
        
        let nds3 = dm0.actions[0].additional_group_state_samples();
        //println!("needs {}", nds3);

        assert!(nds3.len() == 1);
        assert!(nds3.contains_similar_need("StateAdditionalSample", &dm0.region_from_string("r1").unwrap()));

        // Satisfy second need.
        dm0.eval_sample_arbitrary(0, &s1, &s1);

        let nds4 = dm0.actions[0].additional_group_state_samples();
        //println!("needs {}", nds4);

        // Check for no more needs.
        assert!(nds4.len() == 0);

        Ok(())
    }

    // Test Seek adjacent overlapping region.
    // Create region X10X and adjacent region 1X1X.
    // The overlapping part is region 11X1, with no squares in it.
    // Should seek samples of the high (f) and low (d) squares of region 11X1.
    // After two samples of each, it should produce the AddGroup need for 11X1.
    #[test]
    fn need_for_samples_in_adjacent_overlapping_part() -> Result<(), String> {

        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let s5 = dm0.state_from_string("s101").unwrap();

        let s7 = dm0.state_from_string("s111").unwrap();

        let s9 = dm0.state_from_string("s1001").unwrap();

        let sb = dm0.state_from_string("s1011").unwrap();

        let sc = dm0.state_from_string("s1100").unwrap();

        let sd = dm0.state_from_string("s1101").unwrap();

        let se = dm0.state_from_string("s1110").unwrap();

        let sf = dm0.state_from_string("s1111").unwrap();

        let mskf = SomeMask::new_from_string(1, "m1111").unwrap();

        let reg11x1 = dm0.region_from_string("r11X1").unwrap();

        // Create group for region X10X.
        dm0.eval_sample_arbitrary(0, &s5, &s5.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &s5, &s5.toggle_bits(vec![0]));

        dm0.eval_sample_arbitrary(0, &sc, &sc.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &sc, &sc.toggle_bits(vec![0]));

        // Add square to prevent squares (sc, s5) combining with squares (se, sb).
        dm0.eval_sample_arbitrary(0, &s7, &s7.toggle_bits(vec![1]));
        dm0.eval_sample_arbitrary(0, &s7, &s7.toggle_bits(vec![1]));

        // Add square to prevent squares (sc, s5) combining with squares (se, sb).
        dm0.eval_sample_arbitrary(0, &s9, &s9.toggle_bits(vec![2,1,0]));
        dm0.eval_sample_arbitrary(0, &s9, &s9.toggle_bits(vec![2,1,0]));

        // Create group for region 1X1X.
        dm0.eval_sample_arbitrary(0, &se, &se);
        dm0.eval_sample_arbitrary(0, &se, &se);

        dm0.eval_sample_arbitrary(0, &sb, &sb.toggle_bits(vec![0]));
        dm0.eval_sample_arbitrary(0, &sb, &sb.toggle_bits(vec![0]));

        // Get first needs.
        let nds1 = dm0.actions.avec[0].group_pair_needs();

        // Check for two needs, targets f and d.
        println!("dm0 {}", &dm0.actions[0]);
        println!("nds1: {}", &nds1);
        assert!(nds1.len() == 2);
        assert!(nds1.contains_similar_need("AStateMakeGroup", &SomeRegion::new(&sf, &sf)));
        assert!(nds1.contains_similar_need("AStateMakeGroup", &SomeRegion::new(&sd, &sd)));

        dm0.eval_sample_arbitrary(0, &sf, &sf.toggle_bits(vec![0]));

        let nds2 = dm0.actions.avec[0].group_pair_needs();

        assert!(nds2.len() == 1);
        assert!(nds1.contains_similar_need("AStateMakeGroup", &SomeRegion::new(&sd, &sd)));

        dm0.eval_sample_arbitrary(0, &sd, &sd.toggle_bits(vec![0]));

        // Generate overlapping group.
        let chgf = SomeChange { b01: mskf.clone(), b10: mskf };
        let _nds3 = dm0.actions.avec[0].get_needs(&sd, &chgf, 0);

        if let Some(_grpx) = dm0.actions[0].groups.find(&reg11x1) {
        } else {
            return Err("Group r11X1 not found ??".to_string());
        }

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
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
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
        assert!(nds1.contains_similar_need("ContradictoryIntersection", &dm0.region_from_string("rX100").unwrap()));

        Ok(())
    }

    #[test]
    fn test_limit_group_needs() -> Result<(), String> {
        // Init domain with one action.
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
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
        dm0.eval_sample_arbitrary(0, &sd, &sd.toggle_bits(vec![0,1,3]));
        dm0.eval_sample_arbitrary(0, &sd, &sd.toggle_bits(vec![0,1,3]));

        let sa = dm0.state_from_string("s1010").unwrap();
        dm0.eval_sample_arbitrary(0, &sa, &sa.toggle_bits(vec![0,1,3]));
        dm0.eval_sample_arbitrary(0, &sa, &sa.toggle_bits(vec![0,1,3]));

        println!("dm0 {}", &dm0.actions[0]);

        // Directly run limit_groups_needs.
        let agg_chg = SomeChange { b01: SomeMask::new_from_string(1, "m1111").unwrap(), b10: SomeMask::new_from_string(1, "m1111").unwrap() };
        let nds1 = dm0.actions[0].limit_groups_needs(&agg_chg);

        // Check for needs of adjacent, external, squares to 0 (8), 7 (F), A (2) , D (5).
        println!("needs are {}", nds1);
        assert!(nds1.len() == 2);
        assert!(nds1.contains_similar_need("LimitGroup", &dm0.region_from_string("r1111").unwrap()) || nds1.contains_similar_need("LimitGroup", &dm0.region_from_string("r1000").unwrap()));
        assert!(nds1.contains_similar_need("LimitGroup", &dm0.region_from_string("r101").unwrap()) || nds1.contains_similar_need("LimitGroup", &dm0.region_from_string("r10").unwrap()));

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
    
        // Test the expansion of a group with dissimilar external square.
    #[test]
    fn possible_regions_for_group_with_dis_sqrs() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let reg_1x0x = dm0.region_from_string("r1x0x").unwrap();
        let reg_1xxx = dm0.region_from_string("r1xxx").unwrap();
        let reg_xx0x = dm0.region_from_string("rxx0x").unwrap();

        let sq2 = dm0.state_from_string("s10").unwrap();

        let sq5 = dm0.state_from_string("s101").unwrap();

        let sq9 = dm0.state_from_string("s1001").unwrap();

        let sqc = dm0.state_from_string("s1100").unwrap();

        let chg_maskf = SomeMask::new_from_string(1, "m1111").unwrap();

        // Form group r11xx
        dm0.eval_sample_arbitrary(0, &sqc, &sqc);
        dm0.eval_sample_arbitrary(0, &sq9, &sq9);

        // Add square 2.
        dm0.eval_sample_arbitrary(0, &sq2, &sq5);

        println!("action {}", &dm0.actions[0]);
        assert!(dm0.actions[0].groups.len() == 2);

        if let Some(grpx) = dm0.actions[0].groups.find(&reg_1x0x) {
            let mut regs_found = RegionStore::with_capacity(6);
            let mut cnt = 0;
            let limit = 20;
            while regs_found.len() < 2 {
                cnt += 1;
                if cnt > limit {
                    return Err(format!("failed to find 6 options in {} tries", limit));
                }
                let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_maskf);
                println!("regs {}", &regs_exp);
                assert!(regs_exp.len() == 1);
                assert!(regs_exp.contains(&reg_1xxx) || regs_exp.contains(&reg_xx0x));
                if regs_found.contains(&regs_exp[0]) {
                } else {
                    regs_found.push(regs_exp[0].clone());
                }
            }
        } else {
            return Err(format!("Group 1X0X not found?"));
        }

        Ok(())
    }

    // Test the expansion of a group with a dissimilar adjacent square.
    #[test]
    fn possible_regions_for_group_with_adj_dis_sqr() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let reg_1x0x = dm0.region_from_string("r1x0x").unwrap();
        let reg_1xxx = dm0.region_from_string("r1xxx").unwrap();

        let sq9 = dm0.state_from_string("s1001").unwrap();

        let sqc = dm0.state_from_string("s1100").unwrap();

        let sq5 = dm0.state_from_string("s101").unwrap();

        let sq7 = dm0.state_from_string("s111").unwrap();

        let chg_maskf = SomeMask::new_from_string(1, "m1111").unwrap();

        // Form group r11xx
        dm0.eval_sample_arbitrary(0, &sqc, &sqc);
        dm0.eval_sample_arbitrary(0, &sq9, &sq9);

        // Add square 2.
        dm0.eval_sample_arbitrary(0, &sq5, &sq7);

        println!("action {}", &dm0.actions[0]);
        assert!(dm0.actions[0].groups.len() == 2);

        if let Some(grpx) = dm0.actions[0].groups.find(&reg_1x0x) {
            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_maskf);
            println!("regs {}", &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp.contains(&reg_1xxx));
        } else {
            return Err(format!("Group 1X0X not found?"));
        }

        Ok(())
    }

    // Test the expansion of a group with similar external square.
    #[test]
    fn possible_regions_for_group_with_sim_sqr() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let reg_11xx = dm0.region_from_string("r11xx").unwrap();
        let reg_x1x1 = dm0.region_from_string("rx1x1").unwrap();
        let reg_x10x = dm0.region_from_string("rx10x").unwrap();
        let reg_1xxx = dm0.region_from_string("r1xxx").unwrap();
        let reg_xxxx = dm0.region_from_string("rxxxx").unwrap();
        
        let sq5 = dm0.state_from_string("s101").unwrap();

        let sqc = dm0.state_from_string("s1100").unwrap();

        let sqf = dm0.state_from_string("s1111").unwrap();

        let chg_mask7 = SomeMask::new_from_string(1, "m111").unwrap();
        
        let chg_maskf = SomeMask::new_from_string(1, "m1111").unwrap();

        // Form group r11xx
        dm0.eval_sample_arbitrary(0, &sqc, &sqc);
        dm0.eval_sample_arbitrary(0, &sqf, &sqf);

        // Add square 5, forming groups rx1x1, x10x.
        dm0.eval_sample_arbitrary(0, &sq5, &sq5);

        assert!(dm0.actions[0].groups.len() == 3);
        assert!(if let Some(_) = dm0.actions[0].groups.find(&reg_11xx) { true } else { false });
        assert!(if let Some(_) = dm0.actions[0].groups.find(&reg_x1x1) { true } else { false });
        assert!(if let Some(_) = dm0.actions[0].groups.find(&reg_x10x) { true } else { false });

        if let Some(grpx) = dm0.actions[0].groups.find(&reg_11xx) {
            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_maskf);
            println!("for {} seek regs {}", &reg_11xx, &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp.contains(&reg_xxxx));

            // Test non-sim-expansion under change mask, reverts to no-sim, no-dis, under change mask.
            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask7);
            println!("for {} seek regs {}", &reg_11xx, &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp.contains(&reg_1xxx));
        }

        Ok(())
    }

    // Test the expansion of a group with no external squares.
    #[test]
    fn possible_regions_for_group_no_dis_or_sim_sqrs() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm0.add_action();

        let reg_110x = dm0.region_from_string("r110x").unwrap();
        
        let sq0 = dm0.state_from_string("s0").unwrap();

        let sqc = dm0.state_from_string("s1100").unwrap();

        let sqd = dm0.state_from_string("s1101").unwrap();

        dm0.eval_sample_arbitrary(0, &sqd, &sq0);

        let reg_sqd = SomeRegion::new(&sqd, &sqd);

        let chg_mask5 = SomeMask::new_from_string(1, "m101").unwrap();
        let chg_mask6 = SomeMask::new_from_string(1, "m110").unwrap();
        let chg_mask7 = SomeMask::new_from_string(1, "m111").unwrap();
        let chg_maskf = SomeMask::new_from_string(1, "m1111").unwrap();

        // Test logic for a group, with no external similar, or disimilar, squares.
        if let Some(grpx) = dm0.actions[0].groups.find(&reg_sqd) {
            // Test with 3-bit change mask
            let mut regs_found = RegionStore::with_capacity(6);
 
            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_maskf);
            println!("for {} seek regs {}", &reg_sqd, &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp[0].x_mask().num_one_bits() == 4);
            assert!(regs_exp[0].x_mask().m_and(&chg_maskf.m_not()).is_low());
            assert!(regs_exp[0].is_superset_of(&reg_sqd));
            if regs_found.contains(&regs_exp[0]) {
            } else {
                regs_found.push(regs_exp[0].clone());
            }

            println!("regs found {}", &regs_found);

            // Test with 3-bit change mask.
            let mut regs_found = RegionStore::with_capacity(3);
 
            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask7);
            println!("for {} seek regs {}", &reg_sqd, &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp[0].x_mask().num_one_bits() == 3);
            assert!(regs_exp[0].x_mask().m_and(&chg_mask7.m_not()).is_low());
            assert!(regs_exp[0].is_superset_of(&reg_sqd));
            if regs_found.contains(&regs_exp[0]) {
            } else {
                regs_found.push(regs_exp[0].clone());
            }

            println!("regs found {}", &regs_found);

            // Test with 2-bit change mask.
            let mut regs_found = RegionStore::with_capacity(2);

            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask5);
            println!("for {} seek regs {}", &reg_sqd, &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp[0].x_mask().num_one_bits() == 2);
            assert!(regs_exp[0].x_mask().m_and(&chg_mask5.m_not()).is_low());
            assert!(regs_exp[0].is_superset_of(&reg_sqd));
            if regs_found.contains(&regs_exp[0]) {
            } else {
                regs_found.push(regs_exp[0].clone());
            }

            println!("regs found {}", &regs_found);
        } else {
            return Err(format!("Group r1101 not found!"));
        }

        dm0.eval_sample_arbitrary(0, &sqc, &sq0);  // Group r110x

        if let Some(grpx) = dm0.actions[0].groups.find(&reg_110x) {
            // Test region with 1 X, with 2-bit change mask.

            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask6);
            println!("for {} seek regs {}", &reg_110x, &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp[0].x_mask().num_one_bits() == 3);
            assert!(regs_exp[0].is_superset_of(&reg_110x));

            // Test region with 1 X, with 2-bit change mask that includes a region X-bit position.
            let regs_exp = dm0.actions[0].possible_regions_for_group(&grpx, &chg_mask5);
            println!("for {} seek regs {}", &reg_110x, &regs_exp);
            assert!(regs_exp.len() == 1);
            assert!(regs_exp[0].x_mask().num_one_bits() == 2);
            assert!(regs_exp[0].is_superset_of(&reg_110x));
        } else {
            return Err(format!("Group r110x not found!"));
        }

        Ok(())
    }

    /// Form a group, X1X1 from two squares that have alternating (pn=Two) results.
    ///
    /// Sample a square, 0111, in the group, once.  There should be no change.
    ///
    /// Sample the square a second time, with the same result, proving it cannot have an
    /// alternting result.
    ///
    /// Then group X1X1 should be invalidated and removed.
    /// **********************************************************************************
    #[test]
    fn group_pn_2_union_then_invalidation() -> Result<(), String> {
        let mut dm1 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm1.add_action();

        let s5 = dm1.state_from_string("s101").unwrap();

        let s4 = dm1.state_from_string("s100").unwrap();

        let sf = dm1.state_from_string("s1111").unwrap();

        let se = dm1.state_from_string("s1110").unwrap();

        let s7 = dm1.state_from_string("s111").unwrap();

        let rx1x1 = dm1.region_from_string("rx1x1").unwrap();

        dm1.eval_sample_arbitrary(0, &s5, &s5);
        dm1.eval_sample_arbitrary(0, &s5, &s4);
        dm1.eval_sample_arbitrary(0, &s5, &s5);
        dm1.eval_sample_arbitrary(0, &s5, &s4);

        dm1.eval_sample_arbitrary(0, &sf, &se);
        dm1.eval_sample_arbitrary(0, &sf, &sf);
        dm1.eval_sample_arbitrary(0, &sf, &se);
        dm1.eval_sample_arbitrary(0, &sf, &sf);

        if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
            dm1.eval_sample_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
                dm1.eval_sample_arbitrary(0, &s7, &s7); // cause not-pn=2 condition

                if let Some(_) = dm1.actions[0].groups.find(&rx1x1) {
                    //println!("\nActs: {}", &dm1.actions[0]);
                    //println!(" {}", dm1.actions[0].squares);
                    return Err(String::from("failed, rx1x1 should have been deleted"));
                } else {
                    //println!("\nActs: {}", &dm1.actions[0]);
                    //println!("       Sqrs: ({})", dm1.actions[0].squares);
                    return Ok(());
                }
            } else {
                //println!("\nActs: {}", &dm1.actions[0]);
                //println!("       Sqrs: ({})", dm1.actions[0].squares);
                //println!("Group deleted too soon!");
                return Err(String::from("failed, rx1x1 deleted too soon"));
            }
        } else {
            //println!("\nActs: {}", &dm1.actions[0]);
            return Err(String::from(
                "failed, group rx1x1 was not formed by two squares",
            ));
        }
    } // end group_pn_2_union_then_invalidation

    // Form a group, X1X1 from two squares that have unpredictable results.
    //
    // Sample a square, 0111, in the group, several times.  There should be no change until pnc (4 samples).
    //
    // Then group X1X1 should be invalidate and removed.
    // **********************************************************************************
    #[test]
    fn group_pn_u_union_then_invalidation() -> Result<(), String> {
        let mut dm1 = SomeDomain::new(0, 1, "s1", RegionStore::new());
        dm1.add_action();

        let s5 = dm1.state_from_string("s101").unwrap();

        let s4 = dm1.state_from_string("s100").unwrap();

        let sf = dm1.state_from_string("s1111").unwrap();

        let se = dm1.state_from_string("s1110").unwrap();

        let s7 = dm1.state_from_string("s111").unwrap();

        let rx1x1 = dm1.region_from_string("rx1x1").unwrap();

        //println!(
        //    "state 5 = {} s4 {} sF {} sE {} rxx1x1 {}",
        //    s5, s4, sf, se, rx1x1
        //);
        dm1.eval_sample_arbitrary(0, &s5, &s5);
        dm1.eval_sample_arbitrary(0, &s5, &s4);
        dm1.eval_sample_arbitrary(0, &s5, &se);

        dm1.eval_sample_arbitrary(0, &sf, &se);
        dm1.eval_sample_arbitrary(0, &sf, &sf);
        dm1.eval_sample_arbitrary(0, &sf, &s4);

        if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
            println!("\nActs: {}", &dm1.actions[0]);
            dm1.eval_sample_arbitrary(0, &s7, &s7);

            if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
                dm1.eval_sample_arbitrary(0, &s7, &s7); // cause pn-not-Two invalidation
                if let Some(_regx) = dm1.actions[0].groups.find(&rx1x1) {
                    //println!("\nActs: {}", &dm1.actions[0]);
                    //println!(" {}", dm1.actions[0].get_squares());
                    return Err(String::from(
                        "Four samples for s7 failed to invalidate group xx1x1",
                    ));
                } else {
                    return Ok(());
                }
            } else {
                 //println!("\nActs: {}", &dm1.actions[0]);
                 //println!("       Sqrs: ({})", dm1.actions[0].get_squares());
                return Err(String::from("Group deleted too soon"));
            }
        } else {
            //println!("\nActs: {}", &dm1.actions[0]);
            return Err(String::from("group rx1x1 was not formed by two squares!"));
        }
    } // end group_pn_u_union_then_invalidation

    // For showing something easily understandable, the groups in the program are shown
    // with four, or fewer, edges.
    // It is important to show that any arbitrary number of edges can form a group / rule.
    #[test]
    fn create_group_rule_with_ten_edges() -> Result<(), String> {
        let mut dm0 = SomeDomain::new(0, 2, "s1", RegionStore::new());
        dm0.add_action();
        
        let s0 = dm0.state_from_string("s0001010010101000").unwrap();
        let s1 = dm0.state_from_string("s1111010110101011").unwrap();
        // Region                        XXX1010X101010XX.

        // Create group for region XXX1010X101010XX.
        dm0.eval_sample_arbitrary(0, &s0, &s0.toggle_bits(vec![4]));
        dm0.eval_sample_arbitrary(0, &s1, &s1.toggle_bits(vec![4]));

        if let Some(_grpx) = dm0.actions[0].groups.find(&dm0.region_from_string("rXXX1010X101010XX").unwrap()) {
        } else {
            return Err("Group rXXX1010X101010XX not found ??".to_string());
        }

        Ok(())
    }

} // end tests
