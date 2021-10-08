//! The SomeDomain struct, representing a pseudo Karnaugh Map with a specific number of bits.
//!
//! Contains a vector of Action structs, the current state, and a few other fields.

use crate::action::SomeAction;
use crate::actions::take_action;
use crate::actionstore::ActionStore;
use crate::change::SomeChange;
//use crate::mask::SomeMask;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::region::SomeRegion;
use crate::state::SomeState;
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use std::collections::HashMap;

use std::fmt;
extern crate rand;
use rand::Rng;
use serde::{Deserialize, Serialize};

impl fmt::Display for SomeDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("D(ID: ");

        rc_str.push_str(&self.num.to_string());

        rc_str.push_str(&format!(", Current State: {}", &self.cur_state));
        if let Some(areg) = &self.optimal {
            rc_str.push_str(&format!(", Optimal Region: {}", &areg));
        } else {
            rc_str.push_str(&format!(", Optimal Region: None"));
        }

        rc_str.push_str(")");

        write!(f, "{}", rc_str)
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct SomeDomain {
    /// Domain number.  Index into a DomainStore.
    pub num: usize,
    /// Number integers making up a bits struct.    
    pub num_ints: usize,
    /// Actions the Domain can take.    
    pub actions: ActionStore,
    /// The Current State.    
    pub cur_state: SomeState,
    /// An optimal region that is sought if there are no needs.  This may be changed.    
    pub optimal: Option<SomeRegion>,
    /// A copy of the current state, to detect if it has changed between Domain activities.    
    pub prev_state: SomeState,
    /// Hashmaps, one per action, allowing for "hidden variable" per state, for testing.   
    vec_hash: Vec<HashMap<SomeState, usize>>,
    /// Hidden variable range, for testing.  0-max(exclusive).
    /// Chosen randomly at first sample, incremented after each subsequent sample.     
    vec_hvr: Vec<usize>,
}

impl SomeDomain {
    /// Return a new domain instance, given the number of integers, and strings for the
    /// initial state, the optimal state, and index into the DomainStore struct.
    pub fn new(num_ints: usize, start_state: &str, optimal: Option<SomeRegion>) -> Self {
        // Convert the state string into a state type instance.
        let cur = SomeState::from_string(num_ints, &start_state).unwrap();

        // Set up a domain instance with the correct value for num_ints
        return SomeDomain {
            num: 0, // will be set later
            num_ints,
            actions: ActionStore::new(),
            cur_state: cur.clone(),
            optimal: optimal,
            prev_state: cur.clone(),
            vec_hash: Vec::<HashMap<SomeState, usize>>::new(),
            vec_hvr: Vec::<usize>::new(),
        };
    }

    /// Accessor, set the value of the num field
    pub fn set_num(&mut self, anum: usize) {
        self.num = anum;
    }

    /// Accessor, set the optimal field
    pub fn set_optimal(&mut self, areg: Option<SomeRegion>) {
        self.optimal = areg;
    }

    /// Add a SomeAction struct to the store.
    pub fn push(&mut self, mut actx: SomeAction, hv: usize) {
        actx.set_num(self.actions.len());
        self.actions.push(actx);

        // For canned actions, to show 2 and 3 result states. Not needed for real life actions.
        self.vec_hash.push(HashMap::new());
        self.vec_hvr.push(hv);
    }

    /// Return needs gathers from all actions.
    pub fn get_needs(&mut self) -> NeedStore {
        
        let agg_chgs = self.actions.get_aggregate_changes(self.num_ints);
        //println!("aggregate changes are {}", &agg_chgs);

        let mut nst = self
            .actions
            .get_needs(&self.cur_state, &agg_chgs, self.num);

        for ndx in nst.iter_mut() {
            ndx.set_dom(self.num);
        }

        if let Some(areg) = &self.optimal {
            if areg.is_superset_of_state(&self.cur_state) {
            } else {
                nst.push(SomeNeed::ToRegion {
                    dom_num: self.num,
                    act_num: 0,
                    goal_reg: areg.clone(),
                });
            }
        }

        nst
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
        self.check_async();

        // may break hv info, so do not mix with take_action_need
        self.actions[act_num].eval_arbitrary_sample(i_state, r_state, self.num);
        self.set_cur_state(&r_state);
    }

    /// Take an action for a need, evaluate the resulting sample.
    pub fn take_action_need(&mut self, ndx: &SomeNeed) {
        self.check_async();

        let act_num = ndx.act_num();

        let hv = self.get_hv(act_num);
        let astate = take_action(self.num, ndx.act_num(), &self.cur_state, hv);
        self.actions[act_num].eval_need_sample(&self.cur_state, ndx, &astate, self.num);
        self.set_cur_state(&astate);
    }

    /// Take an action with the current state.
    pub fn take_action(&mut self, act_num: usize) {
        self.check_async();

        let hv = self.get_hv(act_num);
        let astate = take_action(self.num, act_num, &self.cur_state, hv);
        self.actions[act_num].eval_sample(&self.cur_state, &astate, self.num);
        self.set_cur_state(&astate);
    }
    
    /// Accessor, set the cur_state field.
    pub fn set_cur_state(&mut self, new_state: &SomeState) {
        self.prev_state = new_state.clone();

        self.cur_state = new_state.clone();
    }

    /// Check for a state change between sampling.
    pub fn check_async(&mut self) {
        if self.cur_state != self.prev_state {
            println!(
                "Asynchronous change from {} to {}",
                &self.prev_state, &self.cur_state
            );
            // TODO store change?
            self.prev_state = self.cur_state.clone();
        }
    }

    /// Run a plan.
    pub fn run_plan(&mut self, pln: &SomePlan) {
        self.check_async();

        self.run_plan2(pln, 0);
    }

    /// Get a hidden variable value, for testing purposes.
    /// Its per state sampled, in the range 0-N(exclusive), where N was given in the <domain>.add_action call.
    /// If N was given as 0, then no hidden variable.
    /// Is randomly generated on the first sample of a state.
    /// In subsequent samples it is incremented, with wrap around.
    fn get_hv(&mut self, act_num: usize) -> usize {
        let hvr = self.vec_hvr[act_num];
        //println!("get_hv: hvr {}", hvr);

        let mut hvx = 0;
        if hvr == 0 {
            return 0;
        }

        if let Some(hvx) = self.vec_hash[act_num].get_mut(&self.cur_state) {
            *hvx += 1;
            if *hvx == hvr {
                *hvx = 0;
            }
            return *hvx;
        }

        if hvr > 0 {
            hvx = rand::thread_rng().gen_range(0, hvr);
        }

        self.vec_hash[act_num].insert(self.cur_state.clone(), hvx);

        return hvx;
    }

    /// Run a plan.  Try to replan and run if a plan encounters an unexpected result.
    fn run_plan2(&mut self, pln: &SomePlan, recur: usize) {
        if recur > 3 {
            println!("\nPlan {} failed", &pln);
            return;
        }

        for stpx in pln.iter() {
            if stpx.initial.is_superset_of_state(&self.cur_state) {
                let hv = self.get_hv(stpx.act_num);

                let astate = take_action(self.num, stpx.act_num, &self.cur_state, hv);

                self.actions[stpx.act_num].eval_step_sample(&self.cur_state, &astate, self.num);

                let prev_state = self.cur_state.clone();

                self.set_cur_state(&astate);

                if stpx.result.is_superset_of_state(&self.cur_state) {
                    continue;
                }

                // Handle unexpected/unwanted result
                // May be an expected possibility from a two result state

                if prev_state == self.cur_state {
                    println!("Try action a second time");

                    let astate = take_action(self.num, stpx.act_num, &self.cur_state, hv);

                    self.actions[stpx.act_num].eval_step_sample(&self.cur_state, &astate, self.num);

                    self.set_cur_state(&astate);

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
    } // end run_plan

    /// Get the steps of a plan, wrap the steps into a plan, return Some(SomePlan).
    pub fn make_plan2(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion) -> Option<SomePlan> {
        if let Some(steps) = self.make_plan3(from_reg, goal_reg, 0) {
                if steps.initial().intersects(from_reg) {
                    if steps.result().intersects(goal_reg) {
                        return Some(SomePlan::new(steps));
                    } else {
                        println!("problem5: result {} not in steps {}", goal_reg, &steps);
                    }
                } else {
                    println!("problem6: initial {} not in steps {}", from_reg, &steps);
                }
        }
        None
    }

    /// Return the steps of a plan to go from a given state to a given region.
    pub fn make_plan3(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, depth: usize) -> Option<StepStore> {

        // Check if from_reg is at the goal
        if goal_reg.intersects(from_reg) {
            return Some(StepStore::new());
        }

        // Check if change is possible

        // Calculate the minimum bit changes needed.
        let required_change = SomeChange::region_to_region(from_reg, goal_reg);
        // println!("make_plan3: required_change {}", &required_change);

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str : StepStore = self.actions.get_steps(&required_change);
        // println!("\nmake_plan3: steps_str steps {}", steps_str.formatted_string(" "));

        // Check that the steps roughly encompass all needed changes, else return None.
        let mut can_change = SomeChange::new_low(self.num_ints);

        for stpx in steps_str.iter() {
            can_change = can_change.union(&stpx.rule.change());
        }

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

//                    println!("make_plan3: suc 1 Found one step {} to go from {} to {}", &bstep, from_reg, goal_reg);
                    return Some(StepStore::new_with_step(bstep));
                }
            }
        }

        // Check recursion depth
        if depth > 2 {
//            println!("recursion depth maximum exceeded");
            return None;
        }
        
        // Sort the steps by each needed bit change. (some actions may change more than one bit, so will appear more than once)
        let steps_by_change_vov: Vec<Vec<usize>> = steps_str.steps_by_change_bit(&required_change);

        // Check if any changes are mutually exclusive
        if any_mutually_exclusive_changes(&steps_str, &steps_by_change_vov, &required_change) {
//            println!("make_plan3: mutually exclusive change rules found");
            return None;
        }

        // Check for bit change vector with all steps outside of the glide path.
        // Accumulate a vector of all step indicies in such vectors.
        let agg_reg = goal_reg.union(from_reg);
        let mut asym_stps = Vec::<usize>::new();
        for inx in 0..steps_by_change_vov.len() {

            let mut all_external = true;
            
            for stp_inx in &steps_by_change_vov[inx] {
                if steps_str[*stp_inx].initial.intersects(&agg_reg) ||
                   steps_str[*stp_inx].result.intersects(&agg_reg) {
                       all_external = false;
                       break;
                   }
            } // next stp_inx

            if all_external {
                for stp_inx in &steps_by_change_vov[inx] {
                    //println!("Asym chaining required {} to {} agg {} step {}", from_reg, goal_reg, &agg_reg, &steps_str[*stp_inx]);
                    asym_stps.push(*stp_inx);
                }
            }
        } // next inx

        if asym_stps.len() == 0 {

        // Evaluate glide path
        // Some steps may intersect both the from region and goal region witout
        // being a single step solution
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

        if steps_from == false || steps_goal == false {
            return None;
        }

        // Initalization for chaining
        let num_tries = 3;
        let mut step_options = Vec::<StepStore>::with_capacity(num_tries);

        // Run a number of depth-first forward chaining
        for _ in 0..num_tries {
            if let Some(poss_steps) = self.random_depth_first_forward_chaining(from_reg, goal_reg, &steps_str) {
                if poss_steps.initial().intersects(from_reg) {
                    if poss_steps.result().intersects(goal_reg) {
                        step_options.push(poss_steps);
                    } else {
                        println!("problem1: result {} not in steps {}", goal_reg, &poss_steps);
                    }
                } else {
                    println!("problem2: initial {} not in steps {}", from_reg, &poss_steps);
                }
            }
        } // next try

        if step_options.len() > 0 {
            let inx = choose_one(&step_options);
            //println!("forward chaining test worked! {}", &step_options[inx]);
            return Some(step_options[inx].clone());
        }

        // Run a number of depth-first backward chaining 
        for _ in 0..num_tries {
            if let Some(poss_steps) = self.random_depth_first_backward_chaining(from_reg, goal_reg, &steps_str) {
    
                if poss_steps.initial().intersects(from_reg) {
                    if poss_steps.result().intersects(goal_reg) {
                        step_options.push(poss_steps);
                    } else {
                        println!("problem3: result {} not in steps {}", goal_reg, &poss_steps);
                    }
                } else {
                    println!("problem4: initial {} not in steps {}", from_reg, &poss_steps);
                }
            }
        } // next try

        if step_options.len() > 0 {
            let inx = choose_one(&step_options);
            //println!("backward chaining worked! {}", &step_options[inx].clone());
            return Some(step_options[inx].clone());
        }

    } // endif asym_stps.len() == 0
    
        // Try Asymmetric forward chaining
        if let Some(ret_steps) = self.asymmetric_forward_chaining(from_reg, goal_reg, &steps_str, depth) {

            // Return a plan found so far, if any
            if ret_steps.len() > 0 {
                let chosen = choose_one(&ret_steps);
                //println!("make_plan3: asym forward chosen returns steps {}", ret_steps[chosen].formatted_string(" "));
                return Some(ret_steps[chosen].clone());
            }
        }

        // Try Asymmetric backward chaining
        if let Some(ret_steps) = self.asymmetric_backward_chaining(from_reg, goal_reg, &steps_str, depth) {

            // Return a plan found so far, if any
            if ret_steps.len() > 0 {
                let chosen = choose_one(&ret_steps);
                //println!("make_plan3: asym backward worked chosen returns steps {}", ret_steps[chosen].formatted_string(" "));
                return Some(ret_steps[chosen].clone());
            }
        }

        None
    } // end make_plan3

    /// Asymmetric chaining for a given step
    fn asymmetric_chaining(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, stepx: &SomeStep, depth: usize) -> Option<StepStore> {
        let mut ret_stepsx = StepStore::new();

        assert!(from_reg.intersects(goal_reg) == false);
        assert!(from_reg.intersects(&stepx.initial) == false);

        // Part one
        if let Some(mut gap_steps1) = self.make_plan3(from_reg, &stepx.initial, depth + 1) {
            ret_stepsx.append(&mut gap_steps1);
            if ret_stepsx.result().intersects(&stepx.initial) {
                ret_stepsx.push(stepx.restrict_initial_region(&ret_stepsx.result()));
            } else {
                //println!("xx for from {} to {} step {}", from_reg, goal_reg, stepx);
                //println!("plan {} result does not intersect {}", &ret_stepsx, &stepx.initial);
                //assert!(1 == 2);
            }
        } else {
            return None;
        }

        // Part two
        let result1 = ret_stepsx.result();

        if result1.intersects(goal_reg) {
            return Some(ret_stepsx);
        }

        if let Some(gap_steps2) = self.make_plan3(&result1, goal_reg, depth + 1) {

            if let Some(ret_steps3) = ret_stepsx.link(&gap_steps2) {
                return Some(ret_steps3);
            }
        }
        None
    }
    
    /// Try Asymmetric forward chaining
    fn asymmetric_forward_chaining(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, steps_str: &StepStore, depth: usize) -> Option<Vec<StepStore>> {

        let mut ret_steps = Vec::<StepStore>::new();

        let agg_reg = goal_reg.union(from_reg);

        let mut min_dist_from = 9999;

        // Find min distance of step initial regions, that are external from agg_reg, to the from_reg
        for stepx in steps_str.iter() {

            if stepx.initial.intersects(&agg_reg) {
                continue;
            }

            let dist = stepx.initial.distance(from_reg);
            if dist < min_dist_from {
                min_dist_from = dist;
            }
        } // next stepx

        if min_dist_from == 9999 {
            return None;
        }

        // Accumulate indicies to steps found to be external to agg_reg and closest to from_reg
        let mut asym_steps = Vec::<usize>::new();
        let mut inx = 0;
        for stepx in steps_str.iter() {

            if stepx.initial.intersects(&agg_reg) == false {

                let dist = stepx.initial.distance(from_reg);
                if dist == min_dist_from {
                    asym_steps.push(inx);
                }
            }

            inx += 1;
        } // next step

        for inx in &asym_steps {

            if steps_str[*inx].result.intersects(goal_reg) {
                let ret_steps1 = StepStore::new_with_step(steps_str[*inx].restrict_result_region(goal_reg));

                if let Some(ret_steps2) = self.make_plan3(from_reg, &ret_steps1.initial(), depth + 1) {
                    if let Some(ret_steps3) = ret_steps2.link(&ret_steps1) {
                        ret_steps.push(ret_steps3);
                    }
                }
                continue;
            }
            
            if let Some(ret_stepsx) = self.asymmetric_chaining(from_reg, goal_reg, &steps_str[*inx], depth + 1) {
                ret_steps.push(ret_stepsx);
            }
        } // next inx

       if ret_steps.len() == 0 {
           return None;
       }

       //println!("asymmetric_forward_chaining: worked returning num {} stepstores", &ret_steps.len());
       Some(ret_steps)
   } // end asymmetric_forward_chaining

    /// Try Asymmetric backward chaining
    fn asymmetric_backward_chaining(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, steps_str: &StepStore, depth: usize) -> Option<Vec<StepStore>> {

        let mut ret_steps = Vec::<StepStore>::new();
        
        let agg_reg = goal_reg.union(from_reg);

        let mut min_dist_goal = 9999;

        // Find min distance of step result regions, that are external from agg_reg, to the goal_reg
        for stepx in steps_str.iter() {

            if stepx.result.intersects(&agg_reg) {
                continue;
            }

            let dist = stepx.result.distance(goal_reg);
            if dist < min_dist_goal {
                min_dist_goal = dist;
            }
        } // next stepx

        if min_dist_goal == 9999 {
            return None;
        }

        // Accumulate indicies to steps found to be external to agg_reg and closest to goal_reg
        let mut asym_steps = Vec::<usize>::new();
        let mut inx = 0;
        for stepx in steps_str.iter() {

            if stepx.result.intersects(&agg_reg) == false {

                let dist = stepx.result.distance(goal_reg);
                if dist == min_dist_goal {
                    asym_steps.push(inx);
                }

            }

            inx += 1;
        } // next step

        for inx in &asym_steps {

            if steps_str[*inx].initial.intersects(from_reg) {

                let ret_steps1 = StepStore::new_with_step(steps_str[*inx].restrict_initial_region(from_reg));

                if let Some(ret_steps2) = self.make_plan3(&ret_steps1.result(), goal_reg, depth + 1) {

                    if let Some(ret_steps3) = ret_steps1.link(&ret_steps2) {
                        ret_steps.push(ret_steps3);
                    }
                }
                continue;
            }
            
            if let Some(ret_steps3) = self.asymmetric_chaining(from_reg, goal_reg, &steps_str[*inx], depth + 1) {
                ret_steps.push(ret_steps3);
            }

        } // next inx

       if ret_steps.len() == 0 {
           return None;
       }

       //println!("asymmetric_backward_chaining: worked returning num {} stepstores", &ret_steps.len());
       Some(ret_steps)
   } // end asymmetric_backward_chaining

    /// Make a plan from a region to another region.
    /// Since there are some random choices, it may be useful to try
    /// running make_one_plan more than once.
    pub fn make_plan(&self, goal_reg: &SomeRegion) -> Option<SomePlan> {
        // Check if a need can be achieved, if so store index and Option<plan>.
        // Higher priority needs that can be reached will superceed lower priority needs.

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
        SomeRegion::from_string(self.num_ints, &str)
    } // end region_from_string

    /// Return a State from a string.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn state_from_string(&self, str: &str) -> Result<SomeState, String> {
        SomeState::from_string(self.num_ints, &str)
    } // end state_from_string

    /// Return a StepStore using random depth-first backward chaining to a state from a goal.
    fn random_depth_first_backward_chaining(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, steps_str: &StepStore) -> Option<StepStore> {
    
        //println!("random_depth_first_backward_chaining2: to {} from {}", from_reg.formatted_string(), goal_reg.formatted_string());

        // Initialize vector for possible options.
        // Most StepStores will contain one step.  Some will contain two steps, due
        // to undoing an unwanted change.
        let mut steps_rev2 = Vec::<StepStore>::with_capacity(steps_str.len());

        // Check each step result region for intersection with the goal_reg.
        // If so, try to develop an option for getting closer to the from_reg, from the goal_reg.
        for stpy in steps_str.iter() {

            if stpy.result.intersects(goal_reg) {
            } else {
                continue;
            }

            let stpstr = StepStore::new_with_step(stpy.restrict_result_region(goal_reg));
            //println!("testing: adding1 stepstore {}", &stpstr);
            steps_rev2.push(stpstr);

        } // next stpy

        if steps_rev2.len() == 0 {
            return None;
        }

        // Get stepstores that have a result region that intersects the goal region
        let mut next_steps = Vec::<usize>::new();
        let mut inx = 0;
        for stpstrx in &steps_rev2 {
            if stpstrx.initial().intersects(from_reg) {
                next_steps.push(inx);
            }
            inx += 1;
        }

        // Pick a solution stepstore that works, if there are any
        for inx in &next_steps {
            if steps_rev2[*inx].initial().intersects(from_reg) {
                if let Some(steps2) = steps_rev2[*inx].restrict_initial_region(from_reg) {
                    if steps2.result() .intersects(goal_reg) {
                        return Some(steps2.restrict_result_region(goal_reg).unwrap());
                    }
                }
            }
        } // next stpstrx

        // Randomly pick a step index from the options.
        let step_inx = rand::thread_rng().gen_range(0, steps_rev2.len());

        // Calculate the next region from the step
        let prev_reg = steps_rev2[step_inx].initial();

        // Use recursion to get next steps
        if let Some(more_steps) = self.random_depth_first_backward_chaining(from_reg, &prev_reg, steps_str) {

            if let Some(ret_steps2) = more_steps.link(&steps_rev2[step_inx]) {
                return Some(ret_steps2);
            }
        }

        None
    } // end random_depth_first_backward_chaining

    /// Return a StepStore from random depth-first forward chaining to goal.
    /// The steps_str argument is a vector of all steps that change at least one bit that is required to reach the goal.
    /// If a step contains an unwanted change, see if a second step will undo the unwanted change.
    /// Without correcting an unwanted change, such a step will be missed since it does not get closer to the goal.
    /// Recursion depth check is not needed due to the requirement that each call gets at least one bit
    /// closer to the goal.
    fn random_depth_first_forward_chaining(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion, steps_str: &StepStore) -> Option<StepStore> {

        // Get the bit changes required to go from from_reg to goal_reg.
        let required_change = SomeChange::region_to_region(from_reg, goal_reg);

        // Initialize vector for possible options.
        // Most StepStores will contain one step.  Some will contain two steps, due
        // to undoing an unwanted change. 
        let mut steps_rev2 = Vec::<StepStore>::with_capacity(steps_str.len());

        // Bits in the from_reg that correspond to X-bits in the goal, can change in any way.
        let care_chg_mask = goal_reg.x_mask().m_not();

        // Check each step initial region for intersection with the from_reg.
        // If so, try to develop an option for getting closer to the goal.
        for stpy in steps_str.iter() {

            if stpy.initial.intersects(from_reg) {
            } else {
                continue;
            }

            // Restrict step to non-X bits in the from region
            let stpx = stpy.restrict_initial_region(from_reg);

            // Get the changes in the step that are not required, that are unwanted, and
            // do not correspond with an X-bit in the goal.
            let chg_not = &required_change.change_not();
            let chg_dif = stpx.rule.change().bitwise_and(&chg_not).bitwise_and_mask(&care_chg_mask);

            if chg_dif.is_low() {
                // No unwanted changes
                let stpstr = StepStore::new_with_step(stpx);
                //println!("testing: adding1 stepstore {}", &stpstr);
                steps_rev2.push(stpstr);
            } else {
                // Unwanted changes found
                //println!("testing: required_change {} step chg: {} unneeded chg: {}", &required_change, stpx.rule.change(), &chg_dif);

                // Calc the changes that will reverse the unwanted changes
                let chg_rev = chg_dif.change_reverse();

                // Get a vector of steps that may accomplish the reversal.
                let steps_rev : StepStore = self.actions.get_steps_exact(&chg_rev);
                //println!("testing: rev steps found: {}", &steps_rev);

                // Check each possible reverse step for intersection with
                // the current steps result region and exact change needed.
                for stp_revx in steps_rev.iter() {

                    if stp_revx.initial.intersects(&stpx.result) {
                    } else {
                        continue;
                    }

                    // Restrict the second step to synchronize with the first step.
                    let stp2 = stp_revx.restrict_initial_region(&stpx.result);

                    // Check for exact match to the needed reverse change(s)
                    if stp2.change() == chg_rev {

                        // Sync the intersection backward to the first step
                        let stp1 = stpx.restrict_result_region(&stp2.initial);

                        // Check that all changes are not lost.
                        if stp1.change().is_not_low() {

                            // Check that the required change is not lost
                            if stp1.change().intersects(&required_change) {

                                // If the required change is in the step, it must be closer to the goal_reg
                                
                                // Check if the unwanted change has been lost
                                if stp2.change() == chg_rev {
                                    let mut stpstr = StepStore::new_with_step(stp1);
                                    stpstr.push(stp2);
                                    //println!("testing: adding2 stepstore {}", &stpstr);
                                    steps_rev2.push(stpstr);
                                } else {
                                    continue;
                                }
                            } else {
                                //println!("glitch3 stp1 chg {} is not subset req chg {}", &stp1.change(), &required_change);
                            }
                        } else {
                            //println!("glitch2");
                        }
                    } else {
                        //println!("glitch1 stp2 chg {} not eq rev chg needed {}", &stp2.change(), &chg_rev);
                        let stpstr = StepStore::new_with_step(stpx.clone());
                        //println!("testing: adding1 stepstore {}", &stpstr);
                        steps_rev2.push(stpstr);
                    }
                } // next stp_revx
            } //end if
        } // next stpy

        if steps_rev2.len() == 0 {
            return None;
        }

        // Get stepstores that have a result region that intersects the goal region
        let mut next_steps = Vec::<usize>::new();
        let mut inx = 0;
        for stpstrx in &steps_rev2 {
            if stpstrx.result().intersects(goal_reg) {
                next_steps.push(inx);
            }
            inx += 1;
        }

        // Randomly pick a solution stepstore, if there are any
        if next_steps.len() > 0 {
            let mut step_inx_inx = 0;
            if next_steps.len() > 1 {
                step_inx_inx = rand::thread_rng().gen_range(0, next_steps.len());
            }
            let step_inx = next_steps[step_inx_inx];
            return Some(steps_rev2[step_inx].clone());
        }

        // Randomly pick a step index from the options.
        let step_inx = rand::thread_rng().gen_range(0, steps_rev2.len());

        // Calculate the next region from the step
        let next_reg = steps_rev2[step_inx].result();

        // Use recursion to get next steps
        if let Some(more_steps) = self.random_depth_first_forward_chaining(&next_reg, goal_reg, steps_str) {

            if let Some(ret_steps2) = steps_rev2[step_inx].link(&more_steps) {
                return Some(ret_steps2);
            }
        }

        None
    } // end random_depth_first_forward_chaining_test

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

/// Return true if any step pairs are all mutually exclusive
fn any_mutually_exclusive_changes(store: &StepStore, by_change: &Vec<Vec<usize>>, wanted: &SomeChange) -> bool {
    
    for inx in 0..(by_change.len() - 1) {
        for iny in (inx+1)..by_change.len() {
            
            let mut exclusive = true;
            //println!("mex checking {:?} and {:?}", &by_change[inx], &by_change[iny]);
            for numx in &by_change[inx] {
                for numy in &by_change[iny] {
                    if *numx == *numy {
                        exclusive = false;
                        continue;
                    }
                    if store[*numx].mutually_exclusive(&store[*numy], wanted) {
                        //println!("step {} mutually exclusive to step {}", store[*numx], store[*numy]);
                    } else {
                        exclusive = false;
                        //break;
                    }
                } //next numy
                //if exclusive == false {
                //    break;
                //}

            } // next numx
            //println!(" ex: {}", exclusive);
            if exclusive {
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


