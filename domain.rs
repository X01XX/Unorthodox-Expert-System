use crate::action::SomeAction;
use crate::actionstore::ActionStore;
//use crate::bits::SomeBits;
use crate::mask::SomeMask;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::region::{region_from_string, SomeRegion};
//use crate::regionstore::RegionStore;
use crate::rule::region_to_region;
use crate::state::{state_from_string, SomeState};
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use std::collections::HashMap;

use std::fmt;
extern crate rand;
use rand::Rng;

impl fmt::Display for SomeDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("D(ID: ");

        rc_str.push_str(&self.num.to_string());

        rc_str.push_str(&format!(", Murrent State: {}", &self.cur_state));
        rc_str.push_str(&format!(", Maximum Region: {}", &self.max_region));
        rc_str.push_str(&format!(", Optimal Region: {}", &self.optimal));

        rc_str.push_str(")");

        write!(f, "{}", rc_str)
    }
}

pub struct SomeDomain {
    pub num: usize,
    pub num_ints: usize,
    pub actions: ActionStore,
    pub cur_state: SomeState,
    pub max_region: SomeRegion,
    pub optimal: SomeRegion,
    vec_hash: Vec<HashMap<SomeState, usize>>, // Hashmaps, one per action, allowing for "hidden variable" per state, for testing
    vec_hvr: Vec<usize>, // hidden variable hidden variable range, for testing.  0-max(exclusive)
                         // chosen randomly at first sample, incremented after each subsequent sample.
}

impl SomeDomain {
    pub fn new(num_ints: usize, start_state: &str, optimal: &str) -> Self {
        // Convert the state string into a state type instance.
        if let Ok(cur) = state_from_string(num_ints, &start_state) {
            if let Ok(opt) = region_from_string(num_ints, &optimal) {
                // Set up a domain instance with the correct value for num_ints
                return SomeDomain {
                    num: 0, // may be changed when added to a DomainStore, to reflect the index into a vector
                    num_ints,
                    actions: ActionStore::new(),
                    cur_state: cur.clone(),
                    max_region: SomeRegion::new(&cur, &cur),
                    optimal: opt,
                    vec_hash: Vec::<HashMap<SomeState, usize>>::new(),
                    vec_hvr: Vec::<usize>::new(),
                };
            } else {
                panic!("Bad optimal region");
            }
        } else {
            panic!("Bad state string");
        }
    }

    pub fn add_action(&mut self, fx: fn(&SomeState, usize) -> SomeState, hv_range: usize) {
        self.actions.push(SomeAction::new(fx)); // Add an action
        self.vec_hash.push(HashMap::new());
        self.vec_hvr.push(hv_range);
    }

    pub fn get_needs(&mut self) -> NeedStore {
        let mut nst = self
            .actions
            .get_needs(&self.cur_state, &self.max_region.x_mask());

        for ndx in nst.iter_mut() {
            ndx.set_dom(self.num);
        }

        nst
    }

    pub fn num_actions(&self) -> usize {
        self.actions.len()
    }

    pub fn take_action_arbitrary(
        &mut self,
        act_num: usize,
        i_state: &SomeState,
        r_state: &SomeState,
    ) {
        // may break hv info
        self.actions[act_num].take_action_arbitrary(i_state, r_state, &self.max_region.x_mask());
        self.set_cur_state(r_state.clone());
    }

    pub fn take_action_need(&mut self, ndx: &SomeNeed) {
        let act_num = ndx.act_num();
        let hv = self.get_hv(act_num);
        let astate = self.actions[act_num].take_action_need(
            &self.cur_state,
            ndx,
            &self.max_region.x_mask(),
            hv,
        );
        self.set_cur_state(astate);
    }

    fn set_cur_state(&mut self, new_state: SomeState) {
        self.cur_state = new_state;

        if self.max_region.is_superset_of_state(&self.cur_state) {
        } else {
            let new_max_region = self.max_region.union_state(&self.cur_state);
            let new_x_bits = new_max_region.x_mask().m_xor(&self.max_region.x_mask());
            println!(
                "new max region {} new x bits {}",
                &new_max_region, &new_x_bits
            );
            self.max_region = new_max_region;
            self.actions.new_x_bits(&new_x_bits);
        }
    }

    // Run a plan, return a state, which may not be the desired result.

    // Run the plan, the resulting state is stored in the domain.
    pub fn run_plan(&mut self, pln: &SomePlan) {
        self.run_plan2(pln, 0) // return run_plan2, which uses a recursion count limit
    }

    // Get a hidden variable value, for testing purposes.
    // Its per state sampled, in the range 0-N(exclusive), where N was given in the <domain>.add_action call.
    // If N was given as 0, then no hidden variable.
    // Is randomly generated on the first sample of a state.
    // In subsequent samples it is incremented, with wrap around.
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

    fn run_plan2(&mut self, pln: &SomePlan, recur: usize) {
        if recur > 3 {
            println!("run_plan2 recursion limit exceeded, plan failed");
            return;
        }

        for stpx in pln.iter() {
            if stpx.initial.is_superset_of_state(&self.cur_state) {
                let hv = self.get_hv(stpx.act_num);

                let astate = self.actions[stpx.act_num].take_action_step(
                    &self.cur_state,
                    &self.max_region.x_mask(),
                    hv,
                );

                let prev_state = self.cur_state.clone();

                self.set_cur_state(astate);

                if stpx.result.is_superset_of_state(&self.cur_state) {
                    continue;
                }

                // Handle unexpected/unwanted result
                // May be an expected possibility from a two result state

                println!(
                    "step result {} is not superset of the result found {}",
                    stpx.result, &self.cur_state
                );

                // Check if the alt_rule explains the plan error
                if stpx.alt_rule {
                    // Check if the alternate rule predicted the result
                    if self.actions[stpx.act_num].group_exists_and_active(&stpx.group_reg) {
                        // alt_rule predicted the result
                        if stpx.rule.no_change() {
                            println!("alt rule, retry same state");
                            // If no-change, try again once, if OK continue with plan
                            let hv = self.get_hv(stpx.act_num);

                            let astate = self.actions[stpx.act_num].take_action_step(
                                &self.cur_state,
                                &self.max_region.x_mask(),
                                hv,
                            );

                            self.set_cur_state(astate);

                            if stpx.result.is_superset_of_state(&self.cur_state) {
                                continue;
                            }
                        } else {
                            // Possible change happened, try plan back to step initial-region, rerun step, if OK continue with plan
                            if let Some(planx) =
                                self.make_plan(&SomeRegion::new(&prev_state, &prev_state))
                            {
                                println!("alt rule, go back to previous state");
                                self.run_plan2(&planx, recur + 1);

                                if stpx.initial.is_superset_of_state(&self.cur_state) {
                                    // Try step again
                                    let hv = self.get_hv(stpx.act_num);

                                    let astate = self.actions[stpx.act_num].take_action_step(
                                        &self.cur_state,
                                        &self.max_region.x_mask(),
                                        hv,
                                    );

                                    self.set_cur_state(astate);

                                    if stpx.result.is_superset_of_state(&self.cur_state) {
                                        // continue with plan
                                        continue;
                                    }
                                }
                            } else {
                                // alt_2 make_plan failed
                                println!("plan failed");
                                return;
                            }
                        }
                    } else {
                        // println!(
                        //    "group {} invalidated by sample",
                        //     &stpx.group_reg
                        // );
                    }
                } else {
                    println!("result {} unexpected", &self.cur_state);
                }

                // Default, try re-plan to goal
                println!(
                    "Make new plan from {} to {}",
                    &self.cur_state,
                    &pln.result_region()
                );
                if let Some(planx) = self.make_plan(pln.result_region()) {
                    return self.run_plan2(&planx, recur + 1);
                }
                println!("plan failed");
                return;
            } else {
                println!(
                    "step initial {} rule {} is not superset of the result found {}, plan failed",
                    &stpx.initial, &stpx.rule, &self.cur_state
                );
                println!("plan failed");
                return;
            }
        } // next stpx
    } // end run_plan

    //    pub fn make_plan2(&self, goal_reg: &SomeRegion) -> SomePlan {
    //		if let Some(plx) = self.make_plan(goal_reg) {
    //			return plx;
    //		}
    //		return SomePlan::new(StepStore::new());
    //	}

    // Make a plan from a region to another region
    // Since there are some random choices, it may be useful to try
    // running make_one_plan more than once.
    pub fn make_plan(&self, goal_reg: &SomeRegion) -> Option<SomePlan> {
        // Check if a need can be achieved, if so store index and Option<plan>.
        // Higher priority needs that can be reached will superceed lower prioriyt needs.

        if goal_reg.is_superset_of_state(&self.cur_state) {
            return Some(SomePlan::new(StepStore::new()));
        }

        // Try to find a plan
        let from_reg = SomeRegion::new(&self.cur_state, &self.cur_state);

        let mut reg_hist = Vec::<(SomeRegion, SomeRegion)>::new();
        if let Some(planz) = self.make_one_plan(&from_reg, &goal_reg, &mut reg_hist) {
            if from_reg.is_subset_of(planz.initial_region()) {
            } else {
                println!(
                    "make_plan: from reg {} does not intersect {}",
                    &from_reg, &planz
                );
            }

            if let Some(planx) = planz.short_cuts() {
                if from_reg.is_subset_of(planx.initial_region()) {
                } else {
                    println!(
                        "make_plan: short_cuts from reg {} does not intersect {}",
                        &from_reg, &planz
                    );
                }

                if planx.result_region().is_subset_of(&goal_reg) {
                    return Some(planx);
                } else {
                    println!("failed at 44");
                    return None;
                }
            }

            if planz.result_region().is_subset_of(&goal_reg) {
                println!("plan from {} to {} found", &from_reg, &goal_reg);
                return Some(planz);
            } else {
                println!("failed at 55");
                return None;
            }
        } else {
            println!("plan from {} to {} not found", &from_reg, &goal_reg);
        } // end if let planz

        None
    } // end make plan

    // Given a from-region and a goal-region, the calculate the required bit-changes.
    //
    // Get a list of action-rule steps that will roughly produce all the desired changes.
    //     else return None.
    //
    // Find a plan using one step.  If the result does not intersect the goal-region,
    // use recursion for the following steps.
    //
    // The initial-regions of the steps, representing the paths to the needed changes, will be
    // at different distances (number bit differences) from the goal-region.
    //
    // The general rule is to use the step(s), with intial-regions
    // furthest from the goal-region, then the next furthest step(s), until the goal-region
    // is attained.
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

        for tpl in reg_hist.iter() {
            if tpl.0 == *from_reg && tpl.1 == *goal_reg {
                println!(
                    "from reg {} and goal reg {} already in reg_hist, recur {}",
                    &from_reg,
                    &goal_reg,
                    reg_hist.len()
                );
                //panic!("Done");
                return None;
            }
        }

        reg_hist.push((from_reg.clone(), goal_reg.clone()));
        if reg_hist.len() > (self.num_actions() * 2) {
            println!("recursion limit exceeded by {}", reg_hist.len());
            //panic!("Done");
            return None;
        }

        // Check for mistaken request
        if from_reg.is_subset_of(&goal_reg) {
            println!("zero len plan returned");
            return Some(SomePlan::new(StepStore::new()));
            //panic!("from_reg {} is at goal_reg {}?", &from_reg, &goal_reg);
            //return Some(SomePlan::new(StepStore::new()));
            //return None;
        }

        // Create an aggregate rule to represent the changes needed
        // 1->X and 0->X bits are a column of zero bits in the rule,
        // which would cause the rule to fail a valid_intersection test.
        let rule_agg = region_to_region(&from_reg, &goal_reg);

        // println!(
        //    "find steps for from_reg {} to goal_reg {}, rule {}",
        //     &from_reg, &goal_reg, &rule_agg
        // );

        // Get steps that include at least one bit of the needed change masks.
        let stpsx = self.actions.get_steps(&rule_agg);

        if stpsx.len() == 0 {
            //if recur == 0 {
            //    println!("No steps found for {} to {} not found", &from_reg, &goal_reg);
            //}
            //println!("No steps found");
            return None;
        }

        //println!("steps found: {}", stpsx);

        // Create an initial change with no bits set to use for unions
        let mut b01 = SomeMask::new(from_reg.state1.bts.new_low());
        let mut b10 = b01.clone();

        // Get union of changes for each step

        for stpx in stpsx.iter() {
            b01 = b01.m_or(&stpx.rule.b01);
            b10 = b10.m_or(&stpx.rule.b10);
        }

        // Check if the changes found roughly satisfy the needed change
        if rule_agg.b01.is_subset_of(&b01) && rule_agg.b10.is_subset_of(&b10) {
            //println!("changes found b01: {} b10: {} are equal to, or superset of, the desired changes b01: {} b10: {}", b01, b10, rule_agg.b01, rule_agg.b10);
        } else {
            //println!("changes found b01: {} b10: {} are NOT equal, or superset, of the desired changes b01: {} b10: {}", b01, b10, rule_agg.b01, rule_agg.b10);
            return None;
        }

        // println!("{} steps found", stpsx.len());

        // Create a vector of step vectors, steps with the same changes are grouped together
        let mut stp_cngs = Vec::<Vec<&SomeStep>>::with_capacity(stpsx.len());

        for stpx in stpsx.iter() {
            // Check for an existing vector of one or more changes of the same kind
            // If found, push the step onto it.
            let mut add_new_vec = true;

            for stp_cng in stp_cngs.iter_mut() {
                if stpx.rule.b01 == stp_cng[0].rule.b01 && stpx.rule.b10 == stp_cng[0].rule.b10 {
                    stp_cng.push(stpx);

                    add_new_vec = false;
                    break;
                }
            }

            // If no vector of similar step changes found, add one.
            if add_new_vec {
                stp_cngs.push(vec![stpx]);
            }
        } // next stpx

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

        // Look for one step that makes the whole change, single rule step preferred.
        let mut stp_vec = Vec::<SomeStep>::new();

        for vecx in stp_cngs.iter() {
            // Set flag if any single result step is in vecx
            let mut sr = false;
            for stpx in vecx.iter() {
                if stpx.alt_rule == false {
                    sr = true;
                }
            }

            for stpx in vecx.iter() {
                if stpx.alt_rule && sr {
                    continue;
                }

                if stpx.initial.is_superset_of(&from_reg) {
                    let stpy = stpx.restrict_initial_region(&from_reg);
                    // closer

                    if stpy.result.is_subset_of(&goal_reg) {
                        if stpy.alt_rule {
                            stp_vec.push(stpy);
                        } else {
                            sr = true;
                            if stp_vec.len() > 0 {
                                stp_vec[0] = stpy;
                            } else {
                                stp_vec.push(stpy);
                            }
                        }
                    } // end if step.result
                } // end if stpx.initial
            } // next stpx
        } // next vecx

        // Return step found
        if stp_vec.len() > 0 {
            //println!("stp_vec: found {}", &stp_vec[0]);
            return Some(SomePlan::new_step(stp_vec[0].clone())); // done in one step, often the end stage of recursion
        } else {
            //println!("stp_vec: did not find a step to make the whole change");
        }

        // Make list of steps with initial regions farthest from goal, or
        // if there is a group of steps, use the one closest to the from region.
        let mut max_diff = 0;
        let mut max_diff_goal = Vec::<&SomeStep>::with_capacity(5);
        for vecx in stp_cngs.iter() {
            //println!("One vecx:\n");
            //for stpx in vecx.iter() {
            //   println!("    {}", &stpx);
            //}
            // Select a step
            let mut astep = vecx[0];

            if vecx.len() > 1 {
                // Get a vector of steps that have an initial region closest to the from-region
                let mut local_min = std::usize::MAX;
                let mut min_diff_from = Vec::<&SomeStep>::with_capacity(5);

                // set single preference flag
                let mut sr_found = false;

                for stpx in vecx.iter() {
                    if stpx.alt_rule == false {
                        sr_found = true;
                    }
                }

                for stpx in vecx.iter() {
                    if stpx.alt_rule {
                        if sr_found {
                            continue;
                        }
                    }

                    let tmp_diff = stpx.initial.num_diff_bits(&from_reg);

                    if tmp_diff < local_min {
                        local_min = tmp_diff;
                        min_diff_from = Vec::<&SomeStep>::with_capacity(5);
                    }
                    if tmp_diff == local_min {
                        min_diff_from.push(stpx);
                    }
                }

                // Get one of the closest to goal steps
                if min_diff_from.len() == 1 {
                    astep = min_diff_from[0];
                } else {
                    astep = min_diff_from[rand::thread_rng().gen_range(0, min_diff_from.len())];
                }
                //println!("    local closest to {} is: {}", &from_reg, astep);
            }

            // A step has been selected
            let tmp_diff = astep.initial.num_diff_bits(&goal_reg);

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

        // Pick steps with initial-region closest to from_reg
        let mut min_diff = std::usize::MAX;
        let mut min_diff_from = Vec::<&SomeStep>::with_capacity(5);
        for stpx in max_diff_goal.iter() {
            let tmp_diff = stpx.initial.num_diff_bits(&from_reg);

            if tmp_diff < min_diff {
                min_diff = tmp_diff;
                min_diff_from = Vec::<&SomeStep>::with_capacity(5);
            }
            if tmp_diff == min_diff {
                min_diff_from.push(stpx);
            }
        } // next stpx

        // Randomly pick a step
        let a_step = min_diff_from[rand::thread_rng().gen_range(0, min_diff_from.len())];
        //println!("Step chosen {}", &a_step);

        // Plan and return the next steps
        if let Some(plnx) = self.plan_next_steps(&from_reg, &goal_reg, a_step.clone(), reg_hist) {
            //if reg_hist.len() == 0 {
            //    println!("plan from {} to {} found", &from_reg, &goal_reg);
            //}
            Some(plnx)
        } else {
            //if reg_hist.len() == 0 {
            //    println!("plan from {} to {} not found", &from_reg, &goal_reg);
            // }
            None
        }
    } // end make_one_plan

    // Process a possible step for translation of the from-region to the goal-region.
    //
    // If the given step translates the from-region to the goal-region.
    // return a one-step plan.
    //
    // If needed, get a plan from the from-region to the step initial-region,
    // else return None.
    //
    // If needed, get a plan from the step result-region to the goal-region,
    // else return None.
    //
    // Link plans together as needed.
    //
    // Return a plan.
    fn plan_next_steps(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        astep: SomeStep,
        reg_hist: &mut Vec<(SomeRegion, SomeRegion)>,
    ) -> Option<SomePlan> {
        let mut bstep = astep.clone();

        if astep.initial.is_superset_of(&from_reg) {
            bstep = astep.restrict_initial_region(&from_reg);
        }

        // println!(
        //      "\nplan_next_steps: from {} to {} step {} recur {}",
        //      &from_reg, &goal_reg, &bstep, recur
        //  );

        // Make a plan out of the step.  This is so plan (step list) linking logic can be used.
        let mut aplan = SomePlan::new_step(bstep.clone());

        // Check plan initial region within from_reg
        if aplan.initial_region().is_subset_of(&from_reg) {
            // Check plan result region is within the goal region
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
    } // end plan_next_steps
} // end impl SomeDomain
