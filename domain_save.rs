//! The SomeDomain struct, representing a pseudo Karnaugh Map with a specific number of bits.
//!
//! Contains a vector of Action structs, the current state, and a few other fields.
//!
use crate::action::SomeAction;
use crate::actions::take_action;
use crate::actionstore::ActionStore;
//use crate::bits::SomeBits;
use crate::change::SomeChange;
use crate::mask::SomeMask;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::region::SomeRegion;
//use crate::regionstore::RegionStore;
use crate::state::SomeState;
use crate::step::SomeStep;
use crate::stepstore::StepStore;
use crate::randompick::RandomPick;
use std::collections::HashMap;

use std::fmt;
extern crate rand;
use rand::Rng;
// use std::process;
use serde::{Deserialize, Serialize};

impl fmt::Display for SomeDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("D(ID: ");

        rc_str.push_str(&self.num.to_string());

        rc_str.push_str(&format!(", Current State: {}", &self.cur_state));
        rc_str.push_str(&format!(", Maximum Region: {}", &self.max_region));
        rc_str.push_str(&format!(", Optimal Region: {}", &self.optimal));

        rc_str.push_str(")");

        write!(f, "{}", rc_str)
    }
}

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
    /// The region formed by the union of all Current States experienced.    
    pub max_region: SomeRegion,
    /// An optimal region that is sought if there are no needs.  This may be changed.    
    pub optimal: SomeRegion,
    /// A copy of the current state, to detect if it has changed between Domain activities.    
    pub prev_state: SomeState,
    /// A store of possible, predictable, bit changes.
    pub predictable_mask: SomeMask,
    /// Hashmaps, one per action, allowing for "hidden variable" per state, for testing.   
    vec_hash: Vec<HashMap<SomeState, usize>>,
    /// Hidden variable range, for testing.  0-max(exclusive).
    /// Chosen randomly at first sample, incremented after each subsequent sample.     
    vec_hvr: Vec<usize>,
}

impl SomeDomain {
    /// Return a new domain instance, given the number of integers, and strings for the
    /// initial state, the optimal state, and index into the DomainStore struct.
    pub fn new(num_ints: usize, start_state: &str, optimal: &str) -> Self {
        // Convert the state string into a state type instance.
        let cur = SomeState::from_string(num_ints, &start_state).unwrap();

        // Convert the optimal region from string to SomeRegion.
        let opt = SomeRegion::from_string(num_ints, &optimal).unwrap();

        // Set up a domain instance with the correct value for num_ints
        return SomeDomain {
            num: 0, // will be set later
            num_ints,
            actions: ActionStore::new(),
            cur_state: cur.clone(),
            max_region: SomeRegion::new(&cur, &cur),
            optimal: opt,
            prev_state: cur.clone(),
            predictable_mask: SomeMask::new_low(num_ints),
            vec_hash: Vec::<HashMap<SomeState, usize>>::new(),
            vec_hvr: Vec::<usize>::new(),
        };
    }

    /// Add a SomeAction struct to the store.
    pub fn push(&mut self, mut actx: SomeAction, hv: usize) {
        actx.num = self.actions.len();
        //actx.num_ints = self.num_ints;
        self.actions.push(actx);

        // For canned actions, to show 2 and 3 result states. Not needed for real life actions.
        self.vec_hash.push(HashMap::new());
        self.vec_hvr.push(hv);
    }

    /// Return needs gathers from all actions.
    pub fn get_needs(&mut self) -> NeedStore {
        let mut nst = self
            .actions
            .get_needs(&self.cur_state, &self.predictable_mask, self.num);

        for ndx in nst.iter_mut() {
            ndx.set_dom(self.num);
        }

        nst
    }

    /// Check for changes in the predictable change mask
    fn check_predictable_mask(&mut self) {
        let sav_mask = self.predictable_mask.clone();

        self.predictable_mask = self.actions.get_predictable_mask(self.num_ints);

        if self.predictable_mask != sav_mask {
            println!("\n  Old Predictable Change mask {}", &sav_mask);
            println!("  New Predictable Change mask {}", &self.predictable_mask);
        }
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
        self.set_cur_state(r_state.clone());

        self.check_predictable_mask();
    }

    /// Take an action for a need, evaluate the resulting sample.
    pub fn take_action_need(&mut self, ndx: &SomeNeed) {
        self.check_async();

        let act_num = ndx.act_num();

        let hv = self.get_hv(act_num);
        let astate = take_action(self.num, ndx.act_num(), &self.cur_state, hv);
        self.actions[act_num].eval_need_sample(&self.cur_state, ndx, &astate, self.num);
        self.set_cur_state(astate);

        self.check_predictable_mask();
    }

    /// Set the current state.
    fn set_cur_state(&mut self, new_state: SomeState) {
        self.prev_state = new_state.clone();

        self.cur_state = new_state;

        if self.max_region.is_superset_of_state(&self.cur_state) {
        } else {
            let new_max_region = self.max_region.union_state(&self.cur_state);
            println!("\n  Old max region  {}", &self.max_region);
            println!("  New max region  {}", &new_max_region,);
            self.max_region = new_max_region;
        }
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

        self.check_predictable_mask();
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

                self.set_cur_state(astate);

                if stpx.result.is_superset_of_state(&self.cur_state) {
                    continue;
                }

                // Handle unexpected/unwanted result
                // May be an expected possibility from a two result state

                if prev_state == self.cur_state {
                    println!("Try action a second time");

                    let astate = take_action(self.num, stpx.act_num, &self.cur_state, hv);

                    self.actions[stpx.act_num].eval_step_sample(&self.cur_state, &astate, self.num);

                    self.set_cur_state(astate);

                    if stpx.result.is_superset_of_state(&self.cur_state) {
                        continue;
                    }
                }

                // Try re-plan to goal
                if let Some(planx) = self.make_plan3(pln.result_region()) {
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
            //panic!("done");
            } else {
                panic!(
                    "step initial {} rule {} is not superset of the result found {}, plan building problem",
                    &stpx.initial, &stpx.rule, &self.cur_state
                );
            }
        } // next stpx
    } // end run_plan

    fn make_one_plan3(
        &self,
        from_state: &SomeState,
        goal_reg: &SomeRegion,
        ) -> Option<SomePlan> {
            let mut prev_states = Vec::<SomeState>::new();
            if let Some(aplan) = self.make_one_plan3b(from_state, goal_reg, &mut prev_states) {
                if let Some(bplan) = self.make_one_plan3b(from_state, goal_reg, &mut prev_states) {
                    if bplan.len() < aplan.len() {
                        println!("make_one_plan3: returns b {} lt a {} {}", bplan.len(), aplan.len(), &bplan);
                        return Some(bplan);
                    } else {
                        println!("make_one_plan3: returns {}", &aplan);
                        return Some(aplan);
                    }
                } else {
                    println!("make_one_plan3: returns a {}", &aplan);
                    return Some(aplan);
                }
            } else if let Some(bplan) = self.make_one_plan3b(from_state, goal_reg, &mut prev_states) {
                       println!("make_one_plan3: returns b {}", &bplan);
                       return Some(bplan);
            }
        None
        }
        
    fn make_one_plan3b(
        &self,
        from_state: &SomeState,
        goal_reg: &SomeRegion,
        prev_states: &mut Vec<SomeState>,
    ) -> Option<SomePlan> {

        println!("\nmake_one_plan2: from {} to {} depth {}", from_state, goal_reg, &prev_states.len());

        // Check for from state already being a subset of the goal region.
        if goal_reg.is_superset_of_state(from_state) {
            return Some(SomePlan::new(StepStore::new()));
        }
        
        // Check for path becoming too long
        if prev_states.len() >= (self.num_actions() * 2) {
            println!("make_one_plan2: recursion limit exceeded by {}", prev_states.len());
            return None;
        }

        // Check if at least some rules can be applied.
        if self.actions.any_rules_initial_superset_state(from_state) == false { return None; }

        if self.actions.any_rules_result_intersect_region(goal_reg) == false { return None; }

        // Create an aggregate change to represent the changes needed.
        let required_change = SomeChange::state_to_region(from_state, goal_reg);
        //println!("**** new required_change {}", &required_change);

        // Get steps that include at least one bit of the needed change masks.
        let steps_str = self.actions.get_steps(&required_change);
        //println!("\n**** steps_str number steps is {} for change {}\n", steps_str.len(), &required_change);

        if steps_str.len() == 0 {
            //if recur == 0 {
            //    println!("No steps found for {} to {} not found", from_reg, goal_reg);
            //}
            println!("No steps found");
            return None;
        }

        let mut can_change = SomeChange::new_low(required_change.b01.num_ints());
        for stpx in steps_str.iter() {
            can_change = can_change.union(&stpx.rule.change());
        }

        if required_change.is_subset_of(&can_change) {
        } else {
            println!("*** step_vec wanted changes {} are not a subset of step_vec changes {}", &required_change, &can_change);
            return None;
        }

        // Check if one step works
        for stepx in steps_str.iter() {
            if stepx.initial.is_superset_of_state(from_state) {

                let rslt = stepx.rule.result_from_initial_state(from_state);

                if goal_reg.is_superset_of_state(&rslt) {
                    let bstep = stepx.restrict_initial_region_to_state(from_state);

                    println!("suc 1 Found one step {} to go from {} to {}", &bstep, from_state, goal_reg);
                    let mut stpstr = StepStore::new_with_capacity(1);
                    stpstr.push(bstep);
                    return Some(SomePlan::new(stpstr));
                }
            }
        }

        // Sort steps by needed change bit.
        let steps_by_change_vov = steps_str.steps_by_change_bit(&required_change);

        // Check if any change-steps list (often only one step) is mutually exclusive to any other. 
        if steps_str.len() == 1 && steps_str.any_mutually_exclusive_changes(&steps_by_change_vov, &required_change) {
            println!("steps_by_change: Mutually exclusive changes, returning None");
            //reg_hist.pop();
            return None;
        }
        
        // Analyze steps by change.
        // Similar to the traveling salesperson problem.
        //
        // Current_state = from_state.
        // Previous_step = none.
        //
        // With the current_state, random_pick a change list, often holding only one step.
        //
        // Set the distance_log to 0.
        //
        // Random_pick a step within the list,
        //
        // If the previous_step is not none, check that the previous_step is not equal to the next_step.
        // As in the case when a step makes multiple needed bit changes.
        //
        // Check that the order of previous_step and next_step is OK.
        //
        // If the step initial region is a superset of the from_state, no traveling is required, the 
        // result state can be calculated, producing the next current_state.
        // If the new current_state was a previous state, return none.
        // Set the previous_step.
        //
        // If the from_state is not in the step initial region, travelling is required.
        // Calculate the next current_state by making the minimum changes to the current_state to
        // intersect the next_step initial_region.
        //
        // Calculate the next current_state from the step.
        // If the new current_state was a previous state, return none.
        //
        // Add the number of bit changes needed to the distance_log.
        // Set the previous_step.
        //
        // if the current_state is in the goal_region, return steps and distance_log.

        let mut rand_change_bit_inx = RandomPick::new(steps_by_change_vov.len());

        prev_states.push(from_state.clone());

        for _ in 0..steps_by_change_vov.len() {

            let chg_bit_inx = rand_change_bit_inx.pick().unwrap();

            let mut rand_chg_steps_inx = RandomPick::new(steps_by_change_vov[chg_bit_inx].len());

            for _ in 0..steps_by_change_vov[chg_bit_inx].len() {

                let chg_step_inx = rand_chg_steps_inx.pick().unwrap();

                let step_inx = steps_by_change_vov[chg_bit_inx][chg_step_inx];

                let stepx = &steps_str[step_inx];

                println!("cur: {} step: {}", from_state, stepx);

                if stepx.initial.is_superset_of_state(from_state) {

                    let rslt = stepx.rule.result_from_initial_state(from_state);

                    prev_states.push(from_state.clone());
                    if let Some(plany) = self.make_one_plan3b(&rslt, goal_reg, prev_states) {

                        let planx = SomePlan::new_step(stepx.restrict_initial_region_to_state(from_state));

                        if let Some(planz) = planx.link(&plany) {
                            return Some(planz);
                        } else {
                            println!("failx: link {} to {}", &planx, &plany);
                        }
                    } else {
                        prev_states.pop();
                        println!("failx: no plan from {} to {}", &rslt, goal_reg);
                    }
                    
                } else {

                    prev_states.push(from_state.clone());
                    if let Some(planx) = self.make_one_plan3b(from_state, &stepx.initial, prev_states) {

                        let rslt = planx.result_from_initial_state(from_state);

                        prev_states.push(from_state.clone());
                        if let Some(plany) = self.make_one_plan3b(&rslt, goal_reg, prev_states) {

                            if let Some(planz) = planx.link(&plany) {
                                return Some(planz);
                            } else {
                                prev_states.pop();
                                println!("failx: link {} to {}", &planx, &plany);
                            }
                        } else {
                            prev_states.pop();
                            println!("failx: no plan from {} to {}", &rslt, goal_reg);
                        }
                    } else {
                        prev_states.pop();
                        println!("failx: no plan from {} to {}", from_state, &stepx.initial);
                    }

               } // next step
           }  // next change bit step
       }  // next change bit step list
       None
   } // end make_one_plan3b

    /// Make a plan from a state to a region.
    ///
    /// Calculate the minimum bit changes needed.
    ///
    /// Get a vector of steps (from rules) that make part of the needed changes.
    ///
    /// Check that the steps roughly encompass all needed changes, else return None.
    ///
    /// Check if one change makes the required change. If so return a plan with that step.
    ///
    /// Sort the change by each bit change they make.
    ///
    /// Check if any change-steps are wholly mutually exclusive of another change.  If so, return None.
    ///
    /// Check if any changes should be done first, to avoid undoing a change to get another change.
    /// If any are found, they replace the original vector of steps needed.
    ///
    /// Sort steps into those that can be run due to having an initial region that the from_state is in (list1),
    /// and those that do not (list 2).
    ///
    /// Randomly choose a step from list1, use recusion to find a plan.  Try all options until a plan is found or all fail.
    ///
    ///     Calculate the effect of the step, and its result, r1.
    ///     Use recursion to find a plan from the r1 to the goal.
    ///
    /// Randomly choose a step from list2, use recusion to find a plan.  Try all options until a plan is found or all fail.
    ///
    ///     Use recursion to find a plan for from_state to step initial region.
    ///     Calculate the effect of the step, and the result state, r1. 
    ///     Use recursion to find a plan from the r1 to the goal.
    ///
    fn make_one_plan2(
        &self,
        from_state: &SomeState,
        goal_reg: &SomeRegion,
        reg_hist: &mut Vec<SomeState>,
    ) -> Option<SomePlan> {

        println!("\nmake_one_plan2: from {} to {} depth {}", from_state, goal_reg, &reg_hist.len());

        // Check for from state already being a subset of the goal region.
        if goal_reg.is_superset_of_state(from_state) {
            return Some(SomePlan::new(StepStore::new()));
        }
        
        // Check for path becoming too long
        if reg_hist.len() >= (self.num_actions() * 2) {
            println!("make_one_plan2: recursion limit exceeded by {}", reg_hist.len());
            //panic!("Done");
            return None;
        }

        // Check for loop in the path so far
        for statex in reg_hist.iter() {
            if *statex == *from_state {
                println!(
                    "from state {} already in reg_hist",
                    from_state,
                );
                return None;
            }
        }

        // Check if at least some rules can be applied.
        if self.actions.any_rules_initial_superset_state(from_state) == false { return None; }

        if self.actions.any_rules_result_intersect_region(goal_reg) == false { return None; }

        // Update path history
//        reg_hist.push((from_state.clone(), goal_reg.clone()));

       // println!("make_one_plan2: from {} to {}", from_state, goal_reg);

        // Create an aggregate change to represent the changes needed.
        let required_change = SomeChange::state_to_region(from_state, goal_reg);
        //println!("**** new required_change {}", &required_change);

        // println!(
        //    "find steps for from_reg {} to goal_reg {}, changes needed {}",
        //     from_reg, goal_reg, &required_change
        // );

        // Get steps that include at least one bit of the needed change masks.
        let steps_str = self.actions.get_steps(&required_change);
        //println!("\n**** steps_str number steps is {} for change {}\n", steps_str.len(), &required_change);

        if steps_str.len() == 0 {
            //if recur == 0 {
            //    println!("No steps found for {} to {} not found", from_reg, goal_reg);
            //}
            println!("No steps found");
            //reg_hist.pop();
            return None;
        }

        let mut can_change = SomeChange::new_low(required_change.b01.num_ints());
        for stpx in steps_str.iter() {
            can_change = can_change.union(&stpx.rule.change());
        }

        if required_change.is_subset_of(&can_change) {
        } else {
            println!("*** step_vec wanted changes {} are not a subset of step_vec changes {}", &required_change, &can_change);
            //reg_hist.pop();
            return None;
        }

        // Check if one step works
        for stepx in steps_str.iter() {
            if stepx.initial.is_superset_of_state(from_state) {

                let rslt = stepx.rule.result_from_initial_state(from_state);

                if goal_reg.is_superset_of_state(&rslt) {
                    let bstep = stepx.restrict_initial_region_to_state(from_state);

                    println!("suc 1 Found one step {} to go from {} to {}", &bstep, from_state, goal_reg);
                    let mut stpstr = StepStore::new_with_capacity(1);
                    stpstr.push(bstep);
                    return Some(SomePlan::new(stpstr));
                }
            }
        }

        // Sort steps by needed change bit.
        let steps_by_change_vov = steps_str.steps_by_change_bit(&required_change);

        // Check if any change-steps list (often only one step) is mutually exclusive to any other. 
        if steps_str.len() == 1 && steps_str.any_mutually_exclusive_changes(&steps_by_change_vov, &required_change) {
            println!("steps_by_change: Mutually exclusive changes, returning None");
            //reg_hist.pop();
            return None;
        }

        // Look for steps that intersect the from state.
        let mut non_intersecting = Vec::<usize>::new();
        for inx in 0..steps_str.len() {
            
            if steps_str[inx].initial.is_superset_of_state(from_state) {

                let rslt = steps_str[inx].rule.result_from_initial_state(from_state);

                reg_hist.push(from_state.clone());
                if let Some(plany) = self.make_one_plan2(&rslt, goal_reg, reg_hist) {

                    let planx = SomePlan::new_step(steps_str[inx].restrict_initial_region_to_state(from_state));

                    if let Some(planz) = planx.link(&plany) {
                        return Some(planz);
                    } else {
                        println!("failx: link {} to {}", &planx, &plany);
                    }
                } else {
                    reg_hist.pop();
                    println!("failx: no plan from {} to {}", &rslt, goal_reg);
                }
            } else {
                non_intersecting.push(inx);
            }
        } // next stepx

        // Look for steps between the from_state and the initial region of a non-intersecting step
        for inx in non_intersecting.iter() {

            reg_hist.push(from_state.clone());
            if let Some(planx) = self.make_one_plan2(from_state, &steps_str[*inx].initial, reg_hist) {

                let rslt = planx.result_from_initial_state(from_state);

                reg_hist.push(from_state.clone());
                if let Some(plany) = self.make_one_plan2(&rslt, goal_reg, reg_hist) {

                    if let Some(planz) = planx.link(&plany) {
                        return Some(planz);
                    } else {
                        reg_hist.pop();
                        println!("failx: link {} to {}", &planx, &plany);
                    }
                } else {
                    reg_hist.pop();
                    println!("failx: no plan from {} to {}", &rslt, goal_reg);
                }
            } else {
                reg_hist.pop();
                println!("failx: no plan from {} to {}", from_state, &steps_str[*inx].initial);
            }
        }
        None
    } // end make_one_plan2

    // Analyze two steps where the result region of the first step intersects the initial region of the second step.
    // The first step should have its initial region restricted to the from_state.
    pub fn _analyze_union(&self, step_one: &SomeStep, step_two: &SomeStep, required_change: &SomeChange, goal_reg: &SomeRegion) {

        let step_one = &step_one.clone();
        let step_two = &step_two.restrict_initial_region(&step_one.result);

        let chg_x = step_one.rule.change();
        let chg_x_needed = chg_x._intersection(&required_change);

        let chg_y = step_two.rule.change();
        let chg_y_needed = chg_y._intersection(&required_change);

        if let Some(chg_z) = chg_x_needed._seq_union(&chg_y_needed) {

            let mut ind = String::from(" ");
            if step_two.result.is_subset_of(goal_reg) {
                ind = String::from("*");
            } else if step_one.initial.distance(goal_reg) > step_two.result.distance(goal_reg) {
                    let dif = step_one.initial.distance(goal_reg) - step_two.result.distance(goal_reg);
                    ind = format!("c {}", &dif);
            }

            println!("chg_x union {} can be run before chg_y {}, giving {} one {} two {} ind {}", &chg_x, &chg_y, &chg_z, &step_one, &step_two, &ind);
        } else {
            println!("chg_x_needed {} should not run before chg_y_needed {}", &chg_x_needed, &chg_y_needed);
        }
    }

    // Analyze two steps where the result region of the first step does not intersect the initial region of the second step.
    // The first step should have its initial region restricted to the from_state.
    pub fn _analyze_disunion(&self, step_one: &SomeStep, stepy: &SomeStep, required_change: &SomeChange, goal_reg: &SomeRegion) {

        let mut step_two = stepy.clone();
        
        // Get "remote intersection" of step two
        if let Some(new_initial) = &step_two.initial._project_to(&step_one.result) {
            step_two = step_two.restrict_initial_region(&new_initial);
        }

        let chg_x = step_one.rule.change();
        let chg_x_needed = chg_x._intersection(&required_change);

        let chg_y = step_two.rule.change();
        let chg_y_needed = chg_y._intersection(&required_change);

        if let Some(_chg_z) = chg_x_needed._seq_union(&chg_y_needed) {

            let chg_between = SomeChange::region_to_region(&step_one.result, &step_two.initial);

            if let Some(chg_xb) = chg_x._seq_union(&chg_between) {

                if let Some(chg_final) = chg_xb._seq_union(&chg_y) {

                    let mut ind = String::from(" ");
                    if step_two.result.is_subset_of(goal_reg) {
                        ind = String::from("*");
                    } else if step_one.initial.distance(goal_reg) > step_two.result.distance(goal_reg) {
                        let dif = step_one.initial.distance(goal_reg) - step_two.result.distance(goal_reg);
                        ind = format!("c {}", &dif);
                    }

                    println!("chg_x disunion {} can be run before chg_y {}, giving {}, one {} two {} ind {}", &chg_x, &chg_y, &chg_final, &step_one, &step_two, &ind);

                } else {
                    println!("chg_xb {} seq_union chg_y {} failed", &chg_xb, &chg_y);
                }
            } else {
                println!("chg_x {} seq_union chg_between {} failed", &chg_x, &chg_between);
            }
        } else {
            println!("chg_x_needed {} should not run before chg_y_needed {}", &chg_x_needed, &chg_y_needed);
        }
    }

    pub fn make_plan3(&self, goal_reg: &SomeRegion) -> Option<SomePlan> {

        if goal_reg.is_superset_of_state(&self.cur_state) {
            return Some(SomePlan::new(StepStore::new()));
        }

        println!("\nplan0:      {}   -> {}", &self.cur_state, goal_reg);
        
        self.make_one_plan3(&self.cur_state, goal_reg);
        
        if 1 == 2 {
            if let Some(plan1) = self.make_plan(goal_reg) {
                println!("plan1: {}", &plan1);
                return Some(plan1);
            }
            println!("plan1: empty");
            return None;
        }
        
        if let Some(plan2) = self.make_plan2(&self.cur_state, goal_reg) {
            println!("plan2: {}", &plan2);
            if let Some(plan1) = self.make_plan(goal_reg) {
                println!("plan1: {}", &plan1);
                if plan1.len() < plan2.len() {
                    println!("plan1: {} shorter {} -improve", plan1.len(), plan2.len());
                    return Some(plan1);
                } else if plan2.len() < plan1.len() {
                    println!("plan2: {} shorter {} +improve", plan2.len(), plan1.len());
                } else {
                    println!("plan2: same len plan1, =improve");
                }
            } else {
                println!("plan2: plan1 empty +improve");
            }
            return Some(plan2);
        } else {
            println!("plan2: empty");
            
            // Check the reverse
//            let required_change = SomeChange::state_to_region(&self.cur_state, goal_reg);
//            let trans_state = required_change.apply_to_state(&self.cur_state);
//            let goal2 = SomeRegion::new(&self.cur_state, &self.cur_state);
//            if let Some(plan2) = self.make_plan2(&trans_state, &goal2) {
//                println!("from {} to {}, instead of from {} to {} plan {}", &trans_state, &goal2, &self.cur_state, goal_reg, &plan2);
//                println!("plan2: rev_improve!");
//            }
            
            if let Some(plan1) = self.make_plan(goal_reg) {
                println!("plan1: {}", &plan1);
                println!("plan1: plan2 empty -improve");
                return Some(plan1);
            } else {
                println!("plan1: empty plan2 empty =improve");
            }
        }

        None
    } // end make_plan3

    pub fn make_plan2(&self, from_state: &SomeState, goal_reg: &SomeRegion) -> Option<SomePlan> {

        // Check is goal is achieved.
        if goal_reg.is_superset_of_state(from_state) {
            return Some(SomePlan::new(StepStore::new()));
        }
        
        // Try to find a plan
        let mut plans = Vec::<SomePlan>::new();
        for _ in 0..2 {
            let mut reg_hist = Vec::<SomeState>::new();
            //reg_hist.push(self.cur_state.clone());
            if let Some(planz) = self.make_one_plan2(from_state, goal_reg, &mut reg_hist) {
                plans.push(planz.clone());
            }
        }

        if plans.len() == 0 {
            return None;
        }
        
        if plans.len() == 1 {
            return Some(plans[0].clone());
        }
        
        Some(plans[rand::thread_rng().gen_range(0, plans.len())].clone())
    }  // end make_plan2
    
    /// Make a plan from a region to another region.
    /// Since there are some random choices, it may be useful to try
    /// running make_one_plan more than once.
    pub fn make_plan(&self, goal_reg: &SomeRegion) -> Option<SomePlan> {
        // Check if a need can be achieved, if so store index and Option<plan>.
        // Higher priority needs that can be reached will superceed lower priority needs.

        if goal_reg.is_superset_of_state(&self.cur_state) {
            return Some(SomePlan::new(StepStore::new()));
        }

        // Try to find a plan
        let from_reg = SomeRegion::new(&self.cur_state, &self.cur_state);

        for _ in 0..2 {
            let mut reg_hist = Vec::<(SomeRegion, SomeRegion)>::new();
            if let Some(planz) = self.make_one_plan(&from_reg, goal_reg, &mut reg_hist) {
                if from_reg.is_subset_of(planz.initial_region()) {
                } else {
                    //println!(
                    //    "make_plan: from reg {} does not intersect {}",
                    //    &from_reg, &planz
                    //);
                    continue;
                }

                if let Some(planx) = planz.short_cuts() {
                    if from_reg.is_subset_of(planx.initial_region()) {
                    } else {
                        //println!(
                        //    "make_plan: short_cuts from reg {} does not intersect {}",
                        //    &from_reg, &planz
                        //);
                        continue;
                    }

                    if planx.result_region().is_subset_of(goal_reg) {
                        return Some(planx);
                    } else {
                        //println!("Short cut failed to match the goal region");
                        continue;
                    }
                }

                if planz.result_region().is_subset_of(goal_reg) {
                    //println!("plan from {} to {} found", &from_reg, goal_reg);
                    return Some(planz);
                } else {
                    //println!("Plan failed to match the goal region");
                    continue;
                }
            } else {
                //println!("plan from {} to {} not found", &from_reg, goal_reg);
            } // end if let planz
        } // end loop
        None
    } // end make plan

    /// Given a from-region and a goal-region, then calculate the required bit-changes.
    ///
    /// Get a list of from-action-to steps that will roughly produce all the desired changes.
    ///     else return None.
    ///
    /// Check if one step will do the change needed.
    ///
    /// Forward-chaining examples I have read assume that any rule is available under any context.
    /// That can happen here.
    /// But there will be some rules that require a particular context, so you must
    /// forward-chain to that context before using the rule.
    ///
    /// For example, upon entering a room to retrieve an object, you may find that you
    /// need to first turn on the light.
    ///
    /// The general rule is to use the step(s), with intial-regions
    /// furthest from the goal-region, then the next furthest step(s), until the goal-region
    /// is attained.
    ///
    /// Figure one step, then call recursively with history of from/to regions.
    fn make_one_plan(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        reg_hist: &mut Vec<(SomeRegion, SomeRegion)>,
    ) -> Option<SomePlan> {
//        println!(
//            "make_one_plan: from {} to {} recur {}",
//            from_reg, goal_reg, reg_hist.len()
//         );

        // Check for from region already being a subset of the goal region.
        if from_reg.is_subset_of(goal_reg) {
//            println!("zero len plan returned");
            return Some(SomePlan::new(StepStore::new()));
        }

        // Check for path becoming too long
        if reg_hist.len() >= (self.num_actions() * 2) {
            println!("make_one_plan: recursion limit exceeded by {}", reg_hist.len());
            //panic!("Done");
            return None;
        }

        // Check for loop in the path so far
        for pairx in reg_hist.iter() {
            if pairx.0 == *from_reg && pairx.1 == *goal_reg {
                println!(
                    "from reg {} to goal reg {} is already in reg_hist, ", from_reg, goal_reg);
                //panic!("Done");
                return None;
            }
        }
        
        // Check if at least some rules can be applied.
        if self.actions.any_rules_initial_intersect_region(from_reg) == false { return None; }

        if self.actions.any_rules_result_intersect_region(goal_reg) == false { return None; }

        // Update path history
        reg_hist.push((from_reg.clone(), goal_reg.clone()));

        // Create an aggregate change to represent the changes needed.
        let required_change = SomeChange::region_to_region(from_reg, goal_reg);

        // println!(
        //    "find steps for from_reg {} to goal_reg {}, changes needed {}",
        //     from_reg, goal_reg, &required_change
        // );

        // Get steps that include at least one bit of the needed change masks.
        let stpsx = self.actions.get_steps(&required_change);
        // println!("**** Standard, get steps returns {} steps", stpsx.len());
        // println!("**** Standard change: {}", &required_change);
        
        if stpsx.len() == 0 {
//            if reg_hist.len() == 1 {
//                println!("No steps found for {} to {} not found", from_reg, goal_reg);
//            }
            //println!("No steps found");
            reg_hist.pop();
            return None;
        }

        //println!("steps found: {}", stpsx);

        // Create an initial change with no bits set to use for unions
        let mut can_change = SomeChange::new_low(self.num_ints);

        // Get union of changes for each step
        for stpx in stpsx.iter() {
            can_change = can_change.union(&stpx.rule.change());
        }

        // Check if the changes found roughly satisfy the needed change
        if required_change.is_subset_of(&can_change) {
//            println!("changes found {} are equal to, or superset of, the desired changes {}", &can_change, &required_change);
        } else {
//            println!("changes found {} are NOT equal, or superset, of the desired changes {}", &can_change, &required_change);
            reg_hist.pop();
            return None;
        }

//        println!("{} steps found", stpsx.len());

        // Look for one step that makes the whole change, single-result step preferred.
        let mut stp_vec = Vec::<SomeStep>::new();
        let mut stp_vec_2 = Vec::<SomeStep>::new();

        for stpx in stpsx.iter() {
            if stpx.initial.is_superset_of(from_reg) {
                let stpy = stpx.restrict_initial_region(from_reg);

                if stpy.result.is_subset_of(goal_reg) {
                    if stpy.alt_rule {
                        stp_vec_2.push(stpy);
                    } else {
                        stp_vec.push(stpy);
                    }
                } // end if step.result
            } // end if stpx.initial
        } // next vecx

        // Return step found
        if stp_vec.len() > 0 {
            let mut inx = 0;
            if stp_vec.len() > 1 {
                inx = rand::thread_rng().gen_range(0, stp_vec.len());
            }
//            println!("stp_vec: found {}", &stp_vec[0]);
            return Some(SomePlan::new_step(stp_vec[inx].clone())); // done in one step, often the end stage of recursion
        } else if stp_vec_2.len() > 0 {
            let mut inx = 0;
            if stp_vec_2.len() > 1 {
                inx = rand::thread_rng().gen_range(0, stp_vec_2.len());
            }
//            println!("stp_vec_2: found {}", &stp_vec_2[0]);
            return Some(SomePlan::new_step(stp_vec_2[inx].clone())); // done in one step, often the end stage of recursion
        } else {
//            println!("stp_vec: did not find a step to make the whole change");
        }

        // Get steps inicies, sorted by step change.
       let stp_cngs: Vec<Vec<usize>> = self.sort_steps(&stpsx, &required_change);

//         println!("{} stp_cngs steps", stp_cngs.len());

        // Print step vector
//        println!("stp_cngs:");
//        for vecx in stp_cngs.iter() {
//           let mut strx = String::from("  [");
//          for stpx in vecx.iter() {
//                strx.push_str(&format!("{}, ", stpx));
//            }
//            println!("{}]", strx);
//         }

        // Select steps per-change with initial regions closest to goal.
        // Of those steps selected, select any step furthest from the goal.
        let mut max_diff = 0;
        let mut max_diff_goal = Vec::<&SomeStep>::with_capacity(5);

        for vecx in stp_cngs.iter() {
//            println!("One vecx:\n");
//            for stpx in vecx.iter() {
//               println!("    {}", &stpx);
//            }
            // Select a step
            let mut astep = &stpsx[vecx[0]];

            if vecx.len() > 1 {
                // Get a vector of steps that have an initial region closest to the goal-region
                let mut local_min = std::usize::MAX;
                let mut min_diff_goal = Vec::<&SomeStep>::with_capacity(5);

                // Set single-result state preference flag.
                let mut sr_found = false;

                // Scan for a single-result step
                for inx in vecx.iter() {
                    let stpx = &stpsx[*inx];
                    if stpx.alt_rule == false {
                        sr_found = true;
                    }
                }

                for inx in vecx.iter() {
                    let stpx = &stpsx[*inx];

                    if stpx.alt_rule {
                        if sr_found {
                            continue;
                        }
                    }

                    let tmp_diff = stpx.initial.distance(goal_reg);

                    if tmp_diff < local_min {
                        local_min = tmp_diff;
                        min_diff_goal = Vec::<&SomeStep>::with_capacity(5);
                    }
                    if tmp_diff == local_min {
                        min_diff_goal.push(&stpx);
                    }
                }

                // Get one of the closest to goal steps
                if min_diff_goal.len() == 1 {
                    astep = min_diff_goal[0];
                } else {
                    astep = min_diff_goal[rand::thread_rng().gen_range(0, min_diff_goal.len())];
                }
//                println!("    local closest to {} is: {}", &from_reg, astep);
            } // end vecx.len

            // A step has been selected
            let tmp_diff = astep.initial.distance(goal_reg);

            if tmp_diff > max_diff {
                max_diff = tmp_diff;
                max_diff_goal = Vec::<&SomeStep>::with_capacity(5);
            }

            if tmp_diff == max_diff {
                max_diff_goal.push(astep);
            }
        } // next vecx

        // Print steps selected
//        let mut strx = String::from("Steps selected for max diff from goal: ");
//        strx.push_str(&format!("{} [", goal_reg));
//          for stpx in max_diff_goal.iter() {
//             strx.push_str(&format!("{}, ", stpx));
//         }
//         println!("{}]", strx);

        // Pick a step
        let mut a_step = max_diff_goal[0];
        if max_diff_goal.len() > 1 {
            a_step = max_diff_goal[rand::thread_rng().gen_range(0, max_diff_goal.len())];
        }
//        println!("Step chosen {}", &a_step);

        // Plan and return the next steps
        if let Some(plnx) = self.plan_next_step(from_reg, goal_reg, a_step.clone(), reg_hist) {
            if reg_hist.len() == 0 {
//                println!("plan from {} to {} found", from_reg, goal_reg);
            }
            return Some(plnx);
        }
        if reg_hist.len() == 0 {
//            println!("plan from {} to {} not found", from_reg, goal_reg);
        }
        reg_hist.pop();
        None
    } // end make_one_plan

    /// Process a possible step for translation of the from-region to the goal-region.
    ///
    /// If the given step translates the from-region to the goal-region.
    /// return a one-step plan.
    ///
    /// If needed, get a plan from the from-region to the step initial-region,
    /// else return None.
    ///
    /// If needed, get a plan from the step result-region to the goal-region,
    /// else return None.
    ///
    /// Link sub plans together as needed.
    ///
    /// Return a plan.
    fn plan_next_step(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        astep: SomeStep,
        reg_hist: &mut Vec<(SomeRegion, SomeRegion)>,
    ) -> Option<SomePlan> {
        let mut bstep = astep.clone();

        // Check for step intial region is a superset of the from region.
        if astep.initial.is_superset_of(from_reg) {
            if astep.initial == *from_reg {
            } else {
                bstep = astep.restrict_initial_region(from_reg);
            }
        }

        //println!(
        //    "\nplan_next_steps: from {} to {} step {} recur {}",
        //     from_reg, goal_reg, &bstep, reg_hist.len()
        // );

        // Make a plan out of the step.  This is so plan (step list) linking logic can be used.
        let mut aplan = SomePlan::new_step(bstep.clone());

        // Check plan initial region within from_reg
        if aplan.initial_region().is_superset_of(from_reg) {
            // Check if the plan result region is within the goal region
            if aplan.result_region().is_subset_of(goal_reg) {
                return Some(aplan);
            }

            // Plan result region is not within the goal region, find a path
            if let Some(planx) = self.make_one_plan(&aplan.result_region(), goal_reg, reg_hist) {
                if let Some(plany) = aplan.link(&planx) {
                    return Some(plany);
                } else {
                    // println!("plan_next_steps: failed at 0");
                    return None;
                }
            } else {
                return None;
            }
        }

        // Get a plan for from-region to step initial-region
        if let Some(planx) = self.make_one_plan(from_reg, &aplan.initial_region(), reg_hist) {
            if let Some(plany) = planx.link(&aplan) {
                // println!(
                //    "make plan from {} to {} giving {}",
                //    &from_reg,
                //    &aplan.initial_region(),
                //    &plany
                // );
                aplan = plany;
            } else {
                // println!(
                //      "plan_next_steps, failed at 3, linking {} to {}",
                //      &aplan, &planx
                //  );
                return None;
            }
        } else {
            // println!(
            //     "plan_next_steps, failed at 4 {} to {}",
            //    &from_reg,
            //     &aplan.initial_region()
            // );
            return None;
        }

        // Check if plan result region is with the goal region
        if aplan.result_region().is_subset_of(goal_reg) {
            return Some(aplan);
        }

        // Get a plan for plan result region to goal region
        if let Some(planw) = self.make_one_plan(&aplan.result_region(), goal_reg, reg_hist) {
            if let Some(planz) = aplan.link(&planw) {
                return Some(planz); // done
            } else {
                //println!("plan_next_steps, failed at 6");
                return None;
            }
        }

        return None;
    } // end plan_next_step

    // Position to the optimal region
    //    pub fn to_optimal(&mut self) -> bool {
    //        self.to_region(&self.optimal.clone())
    //    }

    /// Change the current state to be within a given region.
    pub fn to_region(&mut self, goal_region: &SomeRegion) -> bool {
        if goal_region.is_superset_of_state(&self.cur_state) {
            return true;
        }

        if let Some(pln) = self.make_plan3(goal_region) {
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

    /// Sort a list of steps, by change(s), into a vector of step stores of
    /// steps that have the same change.
    /// Filter out steps that make unneeded changes, if their needed changes are
    /// satisfied by other steps.
    pub fn sort_steps(&self, stpsx: &StepStore, required_change: &SomeChange) -> Vec<Vec<usize>> {
        let mut stp_cngs = Vec::<Vec<usize>>::with_capacity(stpsx.len());

        // Check and store each step that has changes that are an exact subset of
        // the needed changes.
        let mut inx = 0;
        for stpx in stpsx.iter() {
            if stpx.rule.b01.is_subset_of(&required_change.b01)
                && stpx.rule.b10.is_subset_of(&required_change.b10)
            {
                // Check for an existing vector of one or more changes of the same kind
                // If found, push the step onto it.
                let mut add_new_vec = true;

                for stp_cng in stp_cngs.iter_mut() {
                    if stpx.rule.b01 == stpsx[stp_cng[0]].rule.b01
                        && stpx.rule.b10 == stpsx[stp_cng[0]].rule.b10
                    {
                        stp_cng.push(inx);
                        add_new_vec = false;
                        break;
                    }
                }

                // If no vector of similar step changes found, add one.
                if add_new_vec {
                    stp_cngs.push(vec![inx]);
                }
            }
            inx += 1;
        } // next stpx

        // Check each step that is not an exact subset of the needed changes,
        // Save index if it should be stored.
        let mut stp_inx = Vec::<usize>::new();
        let mut inx = 0;

        for stpx in stpsx.iter() {
            if stpx.rule.b01.is_subset_of(&required_change.b01)
                && stpx.rule.b10.is_subset_of(&required_change.b10)
            {
            } else {
                // Check for an existing vector of one or more changes of the same kind.
                // If not found, save inx number.

                // Get desired changes in step
                let mut b01 = stpx.rule.b01.m_and(&required_change.b01);
                let mut b10 = stpx.rule.b10.m_and(&required_change.b10);

                // Check steps stored in stp_cngs to see if they satisfy the same needed changes.
                for stp_cng in stp_cngs.iter_mut() {
                    // Check a b01 needed change
                    let stpx01 = stpsx[stp_cng[0]].rule.b01.m_and(&b01);
                    if stpx01.is_not_low() {
                        b01 = b01.m_and(&stpx01.m_not());
                    }

                    // Check a b10 needed change
                    let stpx10 = stpsx[stp_cng[0]].rule.b10.m_and(&b10);
                    if stpx10.is_not_low() {
                        b10 = b10.m_and(&stpx10.m_not());
                    }
                } // next stp_cng

                // If no steps, or combination of steps, satisfies the same
                // needed changes, save the index to the step.
                if b01.is_not_low() || b10.is_not_low() {
                    stp_inx.push(inx);
                }
            } // end else

            inx += 1;
        } // next stpx

        // Add steps that have an unrequired change with a still required change
        for inx in stp_inx.iter() {
            let stpx = &stpsx[*inx];

            // Check for an existing vector of one or more changes of the same kind
            // If found, push the step onto it.
            let mut add_new_vec = true;

            for stp_cng in stp_cngs.iter_mut() {
                if stpx.rule.b01 == stpsx[stp_cng[0]].rule.b01
                    && stpx.rule.b10 == stpsx[stp_cng[0]].rule.b10
                {
                    stp_cng.push(*inx);
                    add_new_vec = false;
                    break;
                }
            }

            // If no vector of similar step changes found, add one.
            if add_new_vec {
                stp_cngs.push(vec![*inx]);
            }
        } // next inx

        stp_cngs
    } // end sort_steps

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
} // end impl SomeDomain

#[derive(Debug)]
pub struct InxDif {
    /// Index to a StepStore.
    pub inx: usize,
    /// Diff (distance from-state to step-initial-region) + (Distance step-result-region to goal region).
    pub dif: usize,
}
