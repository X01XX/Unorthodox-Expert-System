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
            //panic!("done");
            } else {
                panic!(
                    "step initial {} rule {} is not superset of the result found {}, plan building problem",
                    &stpx.initial, &stpx.rule, &self.cur_state
                );
            }
        } // next stpx
    } // end run_plan

    /// Make a plan from a region to another region.
    /// Since there are some random choices, it may be useful to try
    /// running make_one_plan more than once.
    pub fn make_plan(&self, goal_reg: &SomeRegion) -> Option<SomePlan> {
        // Check if a need can be achieved, if so store index and Option<plan>.
        // Higher priority needs that can be reached will superceed lower prioriyt needs.

        if goal_reg.is_superset_of_state(&self.cur_state) {
            return Some(SomePlan::new(StepStore::new()));
        }

        // Try to find a plan
        let from_reg = SomeRegion::new(&self.cur_state, &self.cur_state);

        // Try make_one_plan up to two times, to allow for some random choices
        for _ in 0..2 {
            let mut reg_hist = Vec::<(SomeRegion, SomeRegion)>::new();
            if let Some(planz) = self.make_one_plan(&from_reg, &goal_reg, &mut reg_hist) {
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

                    if planx.result_region().is_subset_of(&goal_reg) {
                        return Some(planx);
                    } else {
                        //println!("Short cut failed to match the goal region");
                        continue;
                    }
                }

                if planz.result_region().is_subset_of(&goal_reg) {
                    //println!("plan from {} to {} found", &from_reg, &goal_reg);
                    return Some(planz);
                } else {
                    //println!("Plan failed to match the goal region");
                    continue;
                }
            } else {
                //println!("plan from {} to {} not found", &from_reg, &goal_reg);
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
        //println!(
        //    "make_one_plan: from {} to {} recur {}",
        //    &from_reg, &goal_reg, reg_hist.len()
        // );

        // Check for from region already being a subset of the goal region.
        if from_reg.is_subset_of(&goal_reg) {
            println!("zero len plan returned");
            return Some(SomePlan::new(StepStore::new()));
        }

        // Check for path becoming too long
        if reg_hist.len() >= (self.num_actions() * 2) {
            //println!("recursion limit exceeded by {}", reg_hist.len());
            //panic!("Done");
            reg_hist.pop();
            return None;
        }

        // Check for loop in the path so far
        for (fromx, tox) in reg_hist.iter() {
            if fromx == from_reg && tox == goal_reg {
                //println!(
                //    "from reg {} and goal reg {} already in reg_hist, recur {}",
                //    &from_reg,
                //    &goal_reg,
                //    reg_hist.len()
                //);
                //panic!("Done");
                reg_hist.pop();
                return None;
            }
        }

        // Update path history
        reg_hist.push((from_reg.clone(), goal_reg.clone()));

        // Create an aggregate change to represent the changes needed.
        let required_change = SomeChange::region_to_region(&from_reg, &goal_reg);

        // println!(
        //    "find steps for from_reg {} to goal_reg {}, changes needed {}",
        //     &from_reg, &goal_reg, &required_change
        // );

        // Get steps that include at least one bit of the needed change masks.
        let stpsx = self.actions.get_steps(&required_change);

        if stpsx.len() == 0 {
            //if recur == 0 {
            //    println!("No steps found for {} to {} not found", &from_reg, &goal_reg);
            //}
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
            //println!("changes found b01: {} b10: {} are equal to, or superset of, the desired changes b01: {} b10: {}", b01, b10, rule_agg.b01, rule_agg.b10);
        } else {
            //println!("changes found b01: {} b10: {} are NOT equal, or superset, of the desired changes b01: {} b10: {}", b01, b10, rule_agg.b01, rule_agg.b10);
            reg_hist.pop();
            return None;
        }

        // println!("{} steps found", stpsx.len());

        // Look for one step that makes the whole change, single-result step preferred.
        let mut stp_vec = Vec::<SomeStep>::new();
        let mut stp_vec_2 = Vec::<SomeStep>::new();

        for stpx in stpsx.iter() {
            if stpx.initial.is_superset_of(&from_reg) {
                let stpy = stpx.restrict_initial_region(&from_reg);

                if stpy.result.is_subset_of(&goal_reg) {
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
            //println!("stp_vec: found {}", &stp_vec[0]);
            return Some(SomePlan::new_step(stp_vec[inx].clone())); // done in one step, often the end stage of recursion
        } else if stp_vec_2.len() > 0 {
            let mut inx = 0;
            if stp_vec_2.len() > 1 {
                inx = rand::thread_rng().gen_range(0, stp_vec_2.len());
            }
            //println!("stp_vec_2: found {}", &stp_vec_2[0]);
            return Some(SomePlan::new_step(stp_vec_2[inx].clone())); // done in one step, often the end stage of recursion
        } else {
            //println!("stp_vec: did not find a step to make the whole change");
        }

        // Get steps inicies, sorted by step change.
        let stp_cngs: Vec<Vec<usize>> = self.sort_steps(&stpsx, &required_change);

        // println!("{} stp_cngs steps", stp_cngs.len());

        // Print step vector
        //println!("stp_cngs:");
        //for vecx in stp_cngs.iter() {
        //   let mut strx = String::from("  [");
        //  for stpx in vecx.iter() {
        //        strx.push_str(&format!("{}, ", stpx));
        //    }
        //    println!("{}]", strx);
        // }

        // Delete change-steps lists that wholly must be run after another list
        let later_chgs = stpsx.order_next_changes(&stp_cngs, &required_change);
        if later_chgs.len() > 0 {
            // println!("later_chgs: {:?}", &later_chgs);
            let stp_cngs2 = stp_cngs.clone();
            let mut stp_cngs = Vec::<Vec<usize>>::with_capacity(stp_cngs2.len() - later_chgs.len());
            for i in 0..stp_cngs2.len() {
                if later_chgs.contains(&i) == false {
                    stp_cngs.push(stp_cngs2[i].clone());
                }
            }
        }

        // Make list of steps with initial regions farthest from goal.
        // if there is more than one step with the same change,
        // use the one with the initial region closest to the goal region.
        let mut max_diff = 0;
        let mut max_diff_goal = Vec::<&SomeStep>::with_capacity(5);

        for vecx in stp_cngs.iter() {
            //println!("One vecx:\n");
            //for stpx in vecx.iter() {
            //   println!("    {}", &stpx);
            //}
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

                    let tmp_diff = stpx.initial.distance(&goal_reg);

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
                //println!("    local closest to {} is: {}", &from_reg, astep);
            } // end vecx.len

            // A step has been selected
            let tmp_diff = astep.initial.distance(&goal_reg);

            if tmp_diff > max_diff {
                max_diff = tmp_diff;
                max_diff_goal = Vec::<&SomeStep>::with_capacity(5);
            }

            if tmp_diff == max_diff {
                max_diff_goal.push(astep);
            }
        } // next vecx

        // Print steps selected
        //let mut strx = String::from("Steps selected for max diff from goal: ");
        //strx.push_str(&format!("{} [", &goal_reg));
        //  for stpx in max_diff_goal.iter() {
        //     strx.push_str(&format!("{}, ", stpx));
        // }
        //println!("{}]", strx);

        // Pick a step
        let mut a_step = max_diff_goal[0];
        if max_diff_goal.len() > 1 {
            a_step = max_diff_goal[rand::thread_rng().gen_range(0, max_diff_goal.len())];
        }
        //println!("Step chosen {}", &a_step);

        // Plan and return the next steps
        if let Some(plnx) = self.plan_next_step(&from_reg, &goal_reg, a_step.clone(), reg_hist) {
            //if reg_hist.len() == 0 {
            //    println!("plan from {} to {} found", &from_reg, &goal_reg);
            //}
            return Some(plnx);
        }
        //if reg_hist.len() == 0 {
        //    println!("plan from {} to {} not found", &from_reg, &goal_reg);
        // }
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
        if astep.initial.is_superset_of(&from_reg) {
            if &astep.initial == from_reg {
            } else {
                bstep = astep.restrict_initial_region(&from_reg);
            }
        }

        //println!(
        //    "\nplan_next_steps: from {} to {} step {} recur {}",
        //     &from_reg, &goal_reg, &bstep, reg_hist.len()
        // );

        // Make a plan out of the step.  This is so plan (step list) linking logic can be used.
        let mut aplan = SomePlan::new_step(bstep.clone());

        // Check plan initial region within from_reg
        if aplan.initial_region().is_superset_of(&from_reg) {
            // Check if the plan result region is within the goal region
            if aplan.result_region().is_subset_of(&goal_reg) {
                return Some(aplan);
            }

            // Plan result region is not within the goal region, find a path
            if let Some(planx) = self.make_one_plan(&aplan.result_region(), &goal_reg, reg_hist) {
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
        if let Some(planx) = self.make_one_plan(&from_reg, &aplan.initial_region(), reg_hist) {
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
        if aplan.result_region().is_subset_of(&goal_reg) {
            return Some(aplan);
        }

        // Get a plan for plan result region to goal region
        if let Some(planw) = self.make_one_plan(&aplan.result_region(), &goal_reg, reg_hist) {
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
                        b01 = b01.m_xor(&stpx01);
                    }

                    // Check a b10 needed change
                    let stpx10 = stpsx[stp_cng[0]].rule.b10.m_and(&b10);
                    if stpx10.is_not_low() {
                        b10 = b10.m_xor(&stpx10);
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
