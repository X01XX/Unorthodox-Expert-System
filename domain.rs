//! The SomeDomain struct, representing a pseudo Karnaugh Map with a specific number of bits.
//!
//! Contains a vector of Action structs, the current state, and a few other fields.
//!
//! Generates needs to improve the understanding of rules.
//!
//! Generates needs to seek optimal regions, avoid negative regions.
//!
//! Generates plans to satisfy needs.
//!
//! Executes a plan to satisfy a need.
//!
//! When the current state is in at least one optimal region, sets and counts down a boredom value.
//! The boredom value is greater if the the current state is in multiple optimal regions (intersection).

use crate::actionstore::ActionStore;
use crate::change::SomeChange;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::step::{AltRuleHint, SomeStep};
use crate::stepstore::StepStore;
use crate::tools::{self, StrLen};

use rand::Rng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt;

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

/// Implement the trait StrLen for PathStep.
impl StrLen for PathStep {
    fn strlen(&self) -> usize {
        24 + (3 * self.from.strlen())
    }
}

/// A restricted path from one region to another.
/// Used to make a plan that avoids negative regions.
pub struct PathStep {
    /// A starting region.
    pub from: SomeRegion,
    /// A region to seek, either the intersection of another region, or the end goal.
    pub to: SomeRegion,
    /// A, usually, non-negative valued region to stay within.
    pub within: SomeRegion,
}

/// A vector of PathSteps that a plan may be made from.
pub struct Path {
    /// Each PathStep.to region intersects the next PathStep.from region or, for the last step, it is the goal.
    pub steps: Vec<PathStep>,
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::new();
        rc_str.push('[');

        for (inx, stepx) in self.steps.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", stepx));
        }

        rc_str.push(']');
        write!(f, "{}", rc_str)
    }
}

/// Implement the trait StrLen for Path.
impl StrLen for Path {
    fn strlen(&self) -> usize {
        if self.steps.is_empty() {
            2
        } else {
            (2 * self.steps.len()) + (self.steps[0].strlen() * self.steps.len())
        }
    }
}

impl fmt::Display for SomeDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
/// The SomeDomain struct, a current state and actions that can be run.
pub struct SomeDomain {
    /// Domain number.  Index into a higher-level DomainStore.
    pub id: usize,
    /// Actions the Domain can take.
    pub actions: ActionStore,
    /// The Current, internal, State.
    pub cur_state: SomeState,
}

impl SomeDomain {
    /// Return a new domain instance, given the ID number and initial state.
    pub fn new(dom_id: usize, cur_state: SomeState) -> Self {
        Self {
            id: dom_id,
            actions: ActionStore::new(vec![]),
            cur_state,
        }
    }

    /// Return a reference to the current, internal, state.
    pub fn current_state(&self) -> &SomeState {
        &self.cur_state
    }

    /// Add a SomeAction instance to the store.
    /// The rules will be used to generate responses to running the action
    /// against various states.
    pub fn add_action(&mut self, rules: Vec<RuleStore>) {
        debug_assert!(rules.is_empty() || rules[0].num_bits().unwrap() == self.num_bits());

        self.actions.add_action(self.id, &self.cur_state, rules);
    }

    /// Return needs gathered from all actions.
    /// Some housekeeping is done, so self is mutable.
    pub fn get_needs(&mut self) -> NeedStore {
        self.actions.get_needs(&self.cur_state)
    }

    /// Evaluate an arbitrary sample given by the user.
    /// This tends to break things for an action, unless all samples are arbitrary.
    /// Useful for testing a wholly different series of samples/results.
    /// Using the command: ss  action-number  initial-state  result-state
    /// e.g. ss  0  s0b1010  s0b1111
    pub fn eval_sample_arbitrary(&mut self, act_id: usize, smpl: &SomeSample) {
        debug_assert_eq!(smpl.num_bits(), self.num_bits());
        debug_assert!(act_id < self.actions.len());

        self.actions.eval_sample_arbitrary(act_id, smpl);
        self.set_cur_state(smpl.result.clone());
    }

    /// Take an action for a need, evaluate the resulting sample.
    /// It is assumed that a sample made for a need must be saved.
    pub fn take_action_need(&mut self, ndx: &SomeNeed) {
        debug_assert_eq!(ndx.dom_id().unwrap(), self.id);

        let asample = self.actions.take_action_need(ndx, &self.cur_state);
        self.set_cur_state(asample.result.clone());
    }

    /// Take an action with the current state, store the sample.
    pub fn take_action_arbitrary(&mut self, act_id: usize) {
        debug_assert!(act_id < self.actions.len());

        let asample = self.actions.take_action_arbitrary(act_id, &self.cur_state);
        self.set_cur_state(asample.result.clone());
    }

    /// Set the current state field.
    pub fn set_cur_state(&mut self, stax: SomeState) {
        debug_assert_eq!(stax.num_bits(), self.num_bits());

        self.cur_state = stax;
    }

    /// Run a plan, return number steps if it runs to completion.
    pub fn run_plan(&mut self, pln: &SomePlan, depth: usize) -> Result<usize, String> {
        //debug_assert_eq!(pln.dom_id, self.id);
        debug_assert!(pln.is_empty() || pln.num_bits().unwrap() == self.num_bits());

        let mut num_steps = 0;

        if pln.is_empty() {
            return Ok(num_steps);
        }

        if !pln.initial_region().is_superset_of(&self.cur_state) {
            return Err(format!(
                "domain::run_plan: Current state {} is not in the start region of plan {}",
                &self.cur_state, &pln
            ));
        }

        // Run each step of the plan.
        for stpx in pln.iter() {
            let prev_state = self.cur_state.clone();

            if stpx.act_id.is_none() {
                continue;
            }

            let act_id = stpx.act_id.expect("SNH");
            let asample = self.actions.take_action_step(act_id, &self.cur_state);
            num_steps += 1;

            self.set_cur_state(asample.result.clone());

            if stpx.result.is_superset_of(&self.cur_state) {
                continue;
            }

            // Handle unexpected/unwanted result.
            println!(
                "\nChange [{} -{:02}> {}] unexpected, expected {}",
                prev_state, act_id, self.cur_state, stpx,
            );

            // Avoid an infinite loop of retries.
            if depth == 1 {
                return Err("domain::run_plan: Try return/retry depth limit exceeded".to_string());
            }

            // May be an expected possibility from a two result state.
            match &stpx.alt_rule {
                AltRuleHint::NoAlt {} => {
                    return Err("domain::run_plan: Unexpected result, step failed".to_string());
                }
                AltRuleHint::AltNoChange {} => {
                    println!("Try action a second time");

                    let asample = self.actions.take_action_step(act_id, &self.cur_state);
                    num_steps += 1;

                    self.set_cur_state(asample.result.clone());

                    if stpx.result.is_superset_of(&self.cur_state) {
                        continue;
                    }
                    return Err("domain::run_plan: Unexpected result, step failed".to_string());
                }
                AltRuleHint::AltRule { rule } => {
                    if rule.result_region().is_superset_of(&self.cur_state) {
                        //println!("Recalculate plan");
                        return Err("domain::run_plan: Recalculate plan".to_string());
                    } else {
                        return Err("domain::run_plan: Unexpected result, step failed".to_string());
                    }
                }
            }
        } // next stpx

        if pln.result_region().is_superset_of(&self.cur_state) {
            Ok(num_steps)
        } else {
            Err("domain::run_plan: Plan result region not reached".to_string())
        }
    } // end run_plan

    /// Return the steps of a plan to go from a given state/region to a given region.
    ///
    /// This may be called from random_depth_first_search, or may be recursively called to make a sub-plan.
    ///
    /// If a possible bit-change only has Asymmetric Chaining steps, randomly choose one to use.
    ///
    /// Otherwise, randomly choose a forward or backward chaining step.
    ///
    /// The within argument restricts where a rule should start, and restricts unwanted changes that may be included with wanted changes.
    fn depth_first_search(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        steps_str: &StepStore,
        steps_by_change_vov: &[Vec<&SomeStep>],
        depth: usize,
        within: &SomeRegion,
    ) -> Result<SomePlan, String> {
        //println!("\ndomain::depth_first_search2: from {from_reg} to {goal_reg} depth {depth}");
        debug_assert_eq!(from_reg.num_bits(), self.num_bits());
        debug_assert_eq!(goal_reg.num_bits(), self.num_bits());
        debug_assert!(steps_str.is_empty() || steps_str.num_bits().unwrap() == self.num_bits());
        debug_assert!(within.is_superset_of(from_reg));
        debug_assert!(within.is_superset_of(goal_reg));

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

                    return Ok(SomePlan::new(vec![stepz]));
                }
            }
        }

        // Check depth
        if depth == 0 {
            return Err("domain::depth_first_search: Depth limit exceeded".to_string());
        }

        // Calc wanted, and unwanted, changes.
        let rule_to_goal = SomeRule::new_region_to_region(from_reg, goal_reg);
        //println!("rule to goal {rule_to_goal}");

        let wanted_changes = rule_to_goal.wanted_changes();

        let unwanted_changes = rule_to_goal.unwanted_changes();

        // Check for single-bit changes, where all steps are between the from-region and goal-region,
        // not intersecting either.
        //
        // This indicates that a, possibly significant, deviation from a straight-forward
        // rule-path is required.
        //
        // Storage for indicies of asymmetric-only step vectors.
        let mut asym_only_changes = Vec::<usize>::new();

        for (inx, vecx) in steps_by_change_vov.iter().enumerate() {
            let mut all = true;
            for stepx in vecx.iter() {
                if stepx.initial.intersects(from_reg) || stepx.result.intersects(goal_reg) {
                    all = false;
                    break;
                }
            }
            if all {
                asym_only_changes.push(inx);
            }
        }

        // Check if any forced asymmetrical single-bit changes have been found.
        if asym_only_changes.is_empty() {
        } else {
            // Storage for steps, vector index, step index within vector, wanted - unwanted changes.
            let mut ratios: Vec<(usize, usize, isize)> = vec![];

            for inx in asym_only_changes {
                for (iny, stepx) in steps_by_change_vov[inx].iter().enumerate() {
                    let rule_to = SomeRule::new_region_to_region(from_reg, &stepx.initial);

                    let rulx = rule_to.combine_sequence(&stepx.rule);

                    let step_num_unwanted_changes = rulx
                        .to_change()
                        .intersection(&unwanted_changes)
                        .number_changes();

                    let step_num_wanted_changes = rulx
                        .to_change()
                        .intersection(&wanted_changes)
                        .number_changes();

                    //println!("rulx {rulx} wanted {step_num_wanted_changes} unwanted {step_num_unwanted_changes}");
                    // Save ratio info.
                    // Ratio is wanted - unwanted.
                    let mut ratio =
                        step_num_wanted_changes as isize - step_num_unwanted_changes as isize;
                    // Treat GE zero ratios as equal.
                    if ratio > 0 {
                        ratio = 1;
                    }
                    ratios.push((inx, iny, ratio));
                }
            }
            // Order by descending ratio.
            ratios
                .sort_by(|(_, _, ratio_a), (_, _, ratio_b)| ratio_b.partial_cmp(ratio_a).unwrap());
            // println!("ratios: {ratios:?}");
            let mut num_same_ratios = 1;
            for (_inx, _iny, ratio) in ratios.iter().skip(1) {
                if *ratio == ratios[0].2 {
                    num_same_ratios += 1;
                } else {
                    break;
                }
            }
            let inx = rand::thread_rng().gen_range(0..num_same_ratios);
            //println!("inx {inx} of 0..{num_same_ratios}");
            let stepx = steps_by_change_vov[ratios[inx].0][ratios[inx].1];

            //println!("\n    use asymmetric step {} ", stepx);

            return self.asymmetric_chaining(from_reg, goal_reg, stepx, depth - 1, within);
        }

        // Asymmetric chaining is not required.

        // Calc unwanted and unwanted changes for each possible step.
        let mut ratios: Vec<(usize, usize, isize)> = vec![];

        for (inx, step_vecx) in steps_by_change_vov.iter().enumerate() {
            for (iny, stepx) in step_vecx.iter().enumerate() {
                let (step_num_wanted_changes, step_num_unwanted_changes) =
                    if from_reg.intersects(&stepx.initial) {
                        let rulx = stepx.rule.restrict_initial_region(from_reg);
                        (
                            wanted_changes.intersection(&rulx).number_changes(),
                            rulx.to_change()
                                .intersection(&unwanted_changes)
                                .number_changes(),
                        )
                    } else if goal_reg.intersects(&stepx.result) {
                        let rulx = stepx.rule.restrict_result_region(goal_reg);
                        (
                            wanted_changes.intersection(&rulx).number_changes(),
                            rulx.to_change()
                                .intersection(&unwanted_changes)
                                .number_changes(),
                        )
                    } else {
                        let rule_to = SomeRule::new_region_to_region(from_reg, &stepx.initial);
                        let rulx = rule_to.combine_sequence(&stepx.rule);
                        (
                            wanted_changes.intersection(&rulx).number_changes(),
                            rulx.to_change()
                                .intersection(&unwanted_changes)
                                .number_changes(),
                        )
                    };

                // Save ratio info, step vector index, step within vecter index, num wanted changes - num unwanted changes.
                ratios.push((
                    inx,
                    iny,
                    step_num_wanted_changes as isize - step_num_unwanted_changes as isize,
                ));
            } // next iny, stepx
        } // next inx, step_vecx

        ratios.sort_by(|(_, _, ratio_a), (_, _, ratio_b)| ratio_b.partial_cmp(ratio_a).unwrap());

        let mut num_same_ratios = 1;
        for (_inx, _iny, ratio) in ratios.iter().skip(1) {
            if *ratio == ratios[0].2 {
                num_same_ratios += 1;
            } else {
                break;
            }
        }
        let inx = rand::thread_rng().gen_range(0..num_same_ratios);

        let stepx = steps_by_change_vov[ratios[inx].0][ratios[inx].1];

        // Process a forward chaining step.
        if stepx.initial.intersects(from_reg) {
            let stepy = stepx.restrict_initial_region(from_reg);

            //println!("\n    use forward chaining step {}", stepy);
            //println!(
            //    "    wanted changes {} unwanted_changes {}",
            //    wanted_changes.intersection(&stepy.rule),
            //    stepy.rule.to_change().intersection(&unwanted_changes)
            //);

            if within.is_superset_of(&stepy.result) {
                let plan_to_goal =
                    self.plan_steps_between(&stepy.result, goal_reg, depth - 1, within)?;

                return SomePlan::new(vec![stepy]).link(&plan_to_goal);
            } else {
                return Err(format!("domain::depth_first_search: Step {stepy} result region is not a subset of within {within}"));
            }
        }

        // Process backward chaining step.
        if stepx.result.intersects(goal_reg) {
            let stepy = stepx.restrict_result_region(goal_reg);

            //println!("\n    use backward chaining step {}", stepy);
            //println!(
            //    "    wanted changes {} unwanted_changes {}",
            //    wanted_changes.intersection(&stepy.rule),
            //    stepy.rule.to_change().intersection(&unwanted_changes)
            //);

            if within.is_superset_of(&stepy.initial) {
                let plan_to_step =
                    self.plan_steps_between(from_reg, &stepy.initial, depth - 1, within)?;

                return plan_to_step.link(&SomePlan::new(vec![stepy]));
            } else {
                return Err(format!("domain::depth_first_search: Step {stepy} initial region is not a subset of within {within}"));
            }
        }

        // Must be an asymmetric step.

        if within.is_superset_of(&stepx.initial) {
            self.asymmetric_chaining(from_reg, goal_reg, stepx, depth - 1, within)
        } else {
            Err(format!("domain::depth_first_search: Step {stepx} initial region is not a subset of within {within}"))
        }
    } // end depth_first_search

    /// Return possible plan to change state between two regions.
    fn plan_steps_between(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        depth: usize,
        within: &SomeRegion,
    ) -> Result<SomePlan, String> {
        //println!(
        //    "\ndomain::plan_steps_between: from {from_reg} to {goal_reg} within {within} whence {whence} depth {depth}");

        debug_assert_eq!(from_reg.num_bits(), self.num_bits());
        debug_assert_eq!(goal_reg.num_bits(), self.num_bits());
        debug_assert!(within.is_superset_of(from_reg));
        debug_assert!(within.is_superset_of(goal_reg));

        if depth == 0 {
            //println!("\n    depth limit exceeded.");
            return Err("domain::plan_steps_between: Depth limit exceeded".to_string());
        }

        let rule_to_goal = SomeRule::new_region_to_region(from_reg, goal_reg);
        let wanted_changes = rule_to_goal.wanted_changes();

        let steps_str = self.get_steps(&rule_to_goal, within);
        if steps_str.is_empty() {
            //println!("\n    rules covering all needed bit changes {wanted_changes} not found");
            return Err(format!("domain::plan_steps_between: No steps found for rule {rule_to_goal} within {within}"));
        }

        let steps_by_change_vov = steps_str.get_steps_by_bit_change(&wanted_changes)?;

        self.depth_first_search(
            from_reg,
            goal_reg,
            &steps_str,
            &steps_by_change_vov,
            depth - 1,
            within,
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
        within: &SomeRegion,
    ) -> Result<SomePlan, String> {
        //println!(
        //    "\ndomain::asymmetric_chaining: from: {} to step: {} to goal {} depth: {}",
        //    from_reg, stepx, goal_reg, depth
        //);

        if depth == 0 {
            //println!("\n    depth limit exceeded.");
            return Err("domain::asymmetric_chaining: Depth limit exceeded".to_string());
        }

        debug_assert_eq!(from_reg.num_bits(), self.num_bits());
        debug_assert_eq!(goal_reg.num_bits(), self.num_bits());
        debug_assert_eq!(stepx.num_bits(), self.num_bits());

        debug_assert!(!stepx.initial.intersects(from_reg));
        debug_assert!(!stepx.result.intersects(goal_reg));
        debug_assert!(within.is_superset_of(from_reg));
        debug_assert!(within.is_superset_of(goal_reg));

        let (to_step_plan, stepy, from_step_plan) = if rand::random::<bool>() {
            let to_step_plan =
                self.plan_steps_between(from_reg, &stepx.initial, depth - 1, within)?;

            // Restrict the step initial region, in case it is different from the to_step_plan result region,
            // possibly changing the step result region.
            let stepy = stepx.restrict_initial_region(to_step_plan.result_region());

            let from_step_plan =
                self.plan_steps_between(&stepy.result, goal_reg, depth - 1, within)?;

            (to_step_plan, stepy, from_step_plan)
        } else {
            let from_step_plan =
                self.plan_steps_between(&stepx.result, goal_reg, depth - 1, within)?;

            // Restrict the step result region, in case it is different from the from_step_plan initial region,
            // possibly changing the step initial region.
            let stepy = stepx.restrict_result_region(from_step_plan.initial_region());

            let to_step_plan =
                self.plan_steps_between(from_reg, &stepy.initial, depth - 1, within)?;

            (to_step_plan, stepy, from_step_plan)
        };

        // Try linking two plans together with the step.
        //println!("\n    linking plan {to_step_plan} step {stepy} plan {from_step_plan}");
        to_step_plan
            .link(&SomePlan::new(vec![stepy]))?
            .link(&from_step_plan)
    }

    // Make a plan to change the current state to another region.
    // Since there are some random choices, it may be useful to try
    // running make_plan more than once.
    //    pub fn _make_plans(&self, goal_reg: &SomeRegion) -> Option<PlanStore> {
    //        //println!("dom: {} make_plan start cur {} goal {}", self.num, self.cur_state, goal_reg);
    //        debug_assert_eq!(goal_reg.num_bits(), self.num_bits());
    //
    //        // Return no-op plan if the goal is already met.
    //        if goal_reg.is_superset_of(&self.cur_state) {
    //            //println!("no plan needed from {} to {} ?", self.cur_state, goal_reg);
    //            return Some(PlanStore::new(vec![SomePlan::new(self.id, vec![])]));
    //        }
    //
    //        let cur_reg = SomeRegion::new(vec![self.cur_state.clone()]);
    //
    //        self.make_plans2(&cur_reg, goal_reg, None)
    //    }

    /// Make a plan to change from a region to another region.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    pub fn make_plans(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        within: &SomeRegion,
    ) -> Result<PlanStore, Vec<String>> {
        //if let Some(in_reg) = within {
        //    println!("domain::make_plans: from {from_reg} goal {goal_reg} within {in_reg}");
        //} else {
        //    println!("domain::make_plans: from {from_reg} goal {goal_reg}");
        //}
        debug_assert_eq!(from_reg.num_bits(), self.num_bits());
        debug_assert_eq!(goal_reg.num_bits(), self.num_bits());
        debug_assert!(within.num_bits() == self.num_bits());
        debug_assert!(within.is_superset_of(from_reg));
        debug_assert!(within.is_superset_of(goal_reg));

        match self.make_plans2(from_reg, goal_reg, within) {
            Ok(mut plans) => {
                //println!("make_plans2 num found {} plans", plans.len());

                let mut addplans = PlanStore::new(vec![]);
                for planx in plans.iter() {
                    if let Some(shortcuts) = self.shortcuts(planx, within) {
                        addplans.append(shortcuts);
                    }
                }
                if addplans.is_not_empty() {
                    plans.append(addplans);
                }
                Ok(plans)
            }
            Err(errvec) => Err(errvec),
        }
    }

    /// Make a plan to change from a region to another region.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    pub fn make_plans2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        within: &SomeRegion,
    ) -> Result<PlanStore, Vec<String>> {
        //println!("\ndom {} make_plans2: from {from_reg} goal {goal_reg}", self.id);
        debug_assert_eq!(from_reg.num_bits(), self.num_bits());
        debug_assert_eq!(goal_reg.num_bits(), self.num_bits());
        debug_assert!(within.num_bits() == self.num_bits());
        debug_assert!(within.is_superset_of(from_reg));
        debug_assert!(within.is_superset_of(goal_reg));

        if goal_reg.is_superset_of(from_reg) {
            return Ok(PlanStore::new(vec![SomePlan::new(vec![])]));
        }
        // Figure the required change.
        let rule_to_goal = SomeRule::new_region_to_region(from_reg, goal_reg);
        let change_to_goal = rule_to_goal.wanted_changes();

        // Tune maximum depth to be a multiple of the number of bit changes required.
        let num_depth = 4 * change_to_goal.number_changes();

        // Get steps, check if steps include all changes needed.
        let steps_str = self.get_steps(&rule_to_goal, within);
        if steps_str.is_empty() {
            return Err(vec![format!(
                "domain::make_plans2: No steps found for rule {rule_to_goal} within {within}"
            )]);
        }

        // Get vector of steps for each bit change.
        let steps_by_change_vov = match steps_str.get_steps_by_bit_change(&change_to_goal) {
            Ok(stps) => stps,
            Err(errstr) => return Err(vec![errstr]),
        };

        // Calculated steps_str, and steps_by_change_vov, ahead so that thay don't have to be
        // recalculated for each run, below, of random_depth_first_search.
        let plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .map(|_| {
                self.depth_first_search(
                    from_reg,
                    goal_reg,
                    &steps_str,
                    &steps_by_change_vov,
                    num_depth,
                    within,
                )
            })
            .collect::<Vec<Result<SomePlan, String>>>();

        // Check for failure.
        if plans.is_empty() {
            return Err(vec![format!(
                "domain::make_plans2: No plans found for {from_reg} to {goal_reg} within {within}"
            )]);
        }

        // Check for plans.
        let mut plans2 = Vec::<SomePlan>::new();
        let mut problems = Vec::<String>::new();
        for rslt in plans {
            match rslt {
                Ok(planx) => {
                    if !plans2.contains(&planx) {
                        debug_assert!(planx.remains_within(within));
                        plans2.push(planx);
                    }
                }
                Err(errstr) => {
                    if !problems.contains(&errstr) {
                        problems.push(errstr);
                    }
                }
            }
        } // next rslt.

        if plans2.is_empty() {
            Err(problems)
        } else {
            Ok(PlanStore::new(plans2))
        }
        //println!("dom {} make_plans2 returns {}", self.id, tools::vec_string(&plans));
    } // end make_plans2

    /// Get steps that may allow a change to be made.
    ///
    /// Steps that make at least one of the needed bit changes are extracted from domain actions.
    ///
    /// A rule that changes X->x will be pruned to make a step that changes 0->1 or 1->0 depending
    /// on the change needed. X->0 will be pruned to 1->0.  X->1 will be pruned to 0->1.
    ///
    /// If all steps found, in aggregate, cannot change all bits needed, return None.
    ///
    pub fn get_steps(&self, rule_to_goal: &SomeRule, within: &SomeRegion) -> StepStore {
        debug_assert_eq!(rule_to_goal.num_bits(), self.num_bits());
        debug_assert!(within.num_bits() == self.num_bits());
        debug_assert!(within.is_superset_of(&rule_to_goal.initial_region()));
        //debug_assert!(within.is_superset_of(&rule_to_goal.result_region()));

        // Check if changes are possible.
        let wanted_changes = rule_to_goal.wanted_changes();

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str: StepStore = self.actions.get_steps(rule_to_goal, within);

        // Check that the steps roughly encompass all needed changes, else return None.
        let Some(can_change) = steps_str.aggregate_changes() else {
            return StepStore::new(vec![]);
        };

        if wanted_changes.is_subset_of(&can_change) {
            //println!("Rules for each needed change have been found");
        } else {
            //println!("get_steps: step_vec wanted changes {} are not a subset of step_vec changes {}, returning None", required_change, can_change);
            return StepStore::new(vec![]);
        }

        steps_str
    }

    /// Return the current maximum region that can be reached from the current state.
    pub fn reachable_region(&self) -> SomeRegion {
        self.actions.reachable_region(&self.cur_state)
    }

    /// Return regions not covered by existing groups.
    pub fn regions_not_covered(&self, act_id: usize) -> RegionStore {
        debug_assert!(act_id < self.actions.len());

        let mut ncov = RegionStore::new(vec![]);

        let reachable = self.reachable_region();
        ncov.push(reachable);

        for grpx in self.actions[act_id].groups.iter() {
            ncov = ncov.subtract_item(&grpx.region);
        }
        ncov
    }

    /// Display anchor rates, like (number adjacent anchors, number other adjacent squares only in one region, samples)
    pub fn display_action_anchor_info(&self, act_id: usize) -> Result<(), String> {
        debug_assert!(act_id < self.actions.len());

        let max_region = self.reachable_region();
        self.actions[act_id].display_anchor_info()?;

        let whats_left = self.regions_not_covered(act_id);
        println!(
            "\nMaximum Region: {}, Regions not covered by a group: {}",
            max_region, whats_left
        );
        Ok(())
    }

    /// Display a group anchor and adjacent squares.
    pub fn display_group_anchor_info(
        &self,
        act_id: usize,
        aregion: &SomeRegion,
    ) -> Result<(), String> {
        debug_assert!(act_id < self.actions.len());

        self.actions[act_id].display_group_anchor_info(aregion)
    }

    /// Get aggregate changes for a domain.
    pub fn aggregate_changes(&self) -> Option<&SomeChange> {
        if let Some(chgs) = &self.actions.aggregate_changes {
            Some(chgs)
        } else {
            None
        }
    }

    /// Return the total number of groups in all the actions.
    pub fn number_groups(&self) -> usize {
        let mut tot = 0;
        for actx in self.actions.iter() {
            tot += actx.number_groups();
        }
        tot
    }

    /// Return the total number of groups expected in all the actions.
    pub fn number_groups_expected(&self) -> usize {
        let mut tot = 0;
        for actx in self.actions.iter() {
            tot += actx.number_groups_expected();
        }
        tot
    }

    /// Return a String representation of SomeDomain.
    fn formatted_string(&self) -> String {
        let mut rc_str = String::from("D(ID: ");

        rc_str.push_str(&self.id.to_string());

        rc_str.push_str(&format!(", Current State: {}", &self.cur_state));

        rc_str.push(')');

        rc_str
    }

    /// Return the number of bits used in a domain.
    pub fn num_bits(&self) -> usize {
        self.cur_state.num_bits()
    }

    /// Return a plan shortcut, or None.
    fn shortcuts(&self, planx: &SomePlan, within: &SomeRegion) -> Option<PlanStore> {
        if planx.len() < 3 {
            return None;
        }
        //println!("shortcuts: plan {planx}");
        self.shortcuts2(planx, 0, within)
    }

    /// Try to get multiple plan shortcuts.
    /// Return a plan shortcut, or None.
    fn shortcuts2(&self, planx: &SomePlan, depth: usize, within: &SomeRegion) -> Option<PlanStore> {
        //println!("shortcuts2: plan {planx} depth {depth}");
        if depth > 5 {
            return None;
        }
        if let Some(plany) = self.shortcuts3(planx, within) {
            let mut inx = 0;
            if plany.len() > 1 {
                inx = rand::thread_rng().gen_range(0..plany.len());
            }
            if plany[inx].len() < planx.len() {
                return self.shortcuts2(&plany[inx], depth + 1, within);
            }
        }
        if depth == 0 {
            None
        } else {
            Some(PlanStore::new(vec![planx.clone()]))
        }
    }

    /// Return one plan shortcut.
    fn shortcuts3(&self, planx: &SomePlan, within: &SomeRegion) -> Option<PlanStore> {
        if planx.len() < 3 {
            return None;
        }

        let mut shortcuts = PlanStore::new(vec![]); // (Step index, nextep index, distance target step initial to goal)

        // Find targets closer to goal than step initial.
        let mut poss_steps = Vec::<(usize, usize, usize)>::new(); // (From step-initial index, To step-result index, steps that may be excluded/priority)

        // Check each step for distance-to-following-step-result cicle-back.
        // Each step result is the next step's initial, and incudes the goal,
        // using each step's initial would not include the goal.
        for inx in 0..(planx.len() - 2) {
            let initial_x = &planx[inx].initial;
            let mut difs = Vec::<usize>::with_capacity(planx.len() - inx - 1);
            // println!("\nCheck step {inx} initial {initial_x}");
            for iny in inx..planx.len() {
                let result_y = &planx[iny].result;
                let dist = initial_x.distance(result_y);
                //println!("\n    next step {iny} result {} dist {}", result_y, dist);
                difs.push(dist);
            }
            //println!("inx {inx} - len {} = {}", planx.len(), planx.len() - inx);
            //let lowest_inx = Self::lowest_inx(&difs);
            //println!("difs {:?}  lowest is {} at inx {}", difs, difs[lowest_inx], inx + lowest_inx + 1);
            let mut last_dif = 0;
            for (inz, difx) in difs.iter().enumerate() {
                if difs[inz] < last_dif {
                    poss_steps.push((inx, inx + inz, inz));
                }
                last_dif = *difx;
            }
        }
        if poss_steps.is_empty() {
            return None;
        }
        // Check eack shortcut, by decreasing priority.
        if poss_steps.len() > 1 {
            poss_steps.sort_by(|(_, _, pri_a), (_, _, pri_b)| pri_b.partial_cmp(pri_a).unwrap());
        }
        //for (from_inx, to_inx, pri) in poss_steps.iter() {
        //println!(
        //    "  from_inx {from_inx} {} to_inx {to_inx} {} pri {pri}",
        //     planx[*from_inx].initial, planx[*to_inx].result
        //);
        //}

        // Set var to detect a priority change.
        // If any shortcuts found before a priority change, return them.
        let mut last_pri = poss_steps[0].2;

        for (from_inx, to_inx, pri) in poss_steps.iter() {
            // println!(
            //     "\nprocess from_inx {from_inx} {} to_inx {to_inx} {} pri {pri}",
            //     planx[*from_inx].initial, planx[*to_inx].result
            //  );
            if *pri != last_pri {
                if shortcuts.is_empty() {
                    last_pri = *pri;
                } else {
                    //println!("    found shortcuts {shortcuts}");
                    return Some(shortcuts);
                }
            }
            if let Ok(plans2) =
                self.make_plans2(&planx[*from_inx].initial, &planx[*to_inx].result, within)
            {
                // println!(
                //     "    plans found from {} to {}",
                //     planx[*from_inx].initial, &planx[*to_inx].result
                //);
                //println!("    Plans found {plans2}");
                for plany in plans2 {
                    let mut new_plan = SomePlan::new(vec![]);
                    if *from_inx > 0 {
                        for inz in 0..*from_inx {
                            new_plan = match new_plan.link(&SomePlan::new(vec![planx[inz].clone()]))
                            {
                                Ok(planx) => planx,
                                Err(_errstr) => return None,
                            }
                        }
                    }
                    //println!("    sub plan1 {}", plany);
                    new_plan = match new_plan.link(&plany) {
                        Ok(planx) => planx,
                        Err(_errstr) => return None,
                    };

                    if *to_inx < planx.len() {
                        for inz in (to_inx + 1)..planx.len() {
                            new_plan = match new_plan.link(&SomePlan::new(vec![planx[inz].clone()]))
                            {
                                Ok(planx) => planx,
                                Err(_errstr) => return None,
                            }
                        }
                    }
                    //println!("    new_plan {new_plan}");
                    if new_plan.len() >= planx.len() {
                        //println!("    plan too big, continue");
                        continue;
                    }
                    shortcuts.push(new_plan);
                } // next plany
            } // endif
        } // next opts item
        if shortcuts.is_empty() {
            None
        } else {
            //println!("    found shortcuts {shortcuts}");
            Some(shortcuts)
        }
    }

    /// Return the maximum region for the domain.
    pub fn maximum_region(&self) -> SomeRegion {
        SomeRegion::new(vec![self.cur_state.new_high(), self.cur_state.new_low()])
    }
} // end impl SomeDomain

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::domainstore::DomainStore;
    use crate::target::ATarget;

    /// Return true if a need with a given type and target is in a NeedStore.
    fn contains_similar_need(nds: &NeedStore, name: &str, target: &ATarget) -> bool {
        for nedx in nds.iter() {
            if nedx.name() == name {
                if match (nedx.target(), target) {
                    (ATarget::State { state: state1 }, ATarget::State { state: state2 }) => {
                        &state1 == state2
                    }
                    (ATarget::Region { region: region1 }, ATarget::Region { region: region2 }) => {
                        &region1 == region2
                    }
                    _ => false,
                } {
                    return true;
                }
            }
        }
        false
    }

    // Test running a plan using an alt-rule (change) group.
    #[test]
    fn alt_rule1() -> Result<(), String> {
        // Create a domain that uses four bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));

        let ruls0: Vec<RuleStore> = vec![RuleStore::from("[00/11/01/11, 00/11/00/10]")?];
        dm0.add_action(ruls0);

        let sta_5 = SomeState::from("0b0101")?;

        // Load samples for action 0, state 5.  The first change is chosen randomly from the rule options.
        dm0.cur_state = sta_5.clone();
        dm0.take_action_arbitrary(0);

        dm0.cur_state = sta_5.clone();
        dm0.take_action_arbitrary(0);

        dm0.cur_state = sta_5.clone();
        dm0.take_action_arbitrary(0);

        dm0.cur_state = sta_5.clone();
        dm0.take_action_arbitrary(0);

        let rslt = if let Some(sqrx) = dm0.actions[0].squares.find(&sta_5) {
            sqrx.most_recent_result().clone()
        } else {
            return Err("Square 5 not found".to_string());
        };
        println!("rslt1 {rslt}");

        // Force current result to 0111, so next result will be 0100.
        if rslt == SomeState::from("0b0100")? {
            dm0.cur_state = sta_5.clone();
            dm0.take_action_arbitrary(0);
        }

        let rslt = if let Some(sqrx) = dm0.actions[0].squares.find(&sta_5) {
            sqrx.most_recent_result().clone()
        } else {
            return Err("Square 5 not found".to_string());
        };
        println!("rslt2 {rslt}");

        dm0.cur_state = sta_5.clone();

        println!("\ndm0: cur_state {}", dm0.cur_state);
        println!("Acts: {}\n", dm0.actions);

        // One of the following plans will succeed as is, one will need to return to square 5 and try again, then it will succeed.
        if let Ok(plans) = dm0.make_plans(
            &SomeRegion::from("r0101").expect("SNH"),
            &SomeRegion::from("r0100").expect("SNH"),
            &SomeRegion::from("rXXXX").expect("SNH"),
        ) {
            println!("1plans {plans}");
            match dm0.run_plan(&plans[0], 2) {
                Ok(num) => {
                    if num == 1 {
                        return Ok(());
                    } else {
                        return Err(format!("{num} steps?"));
                    }
                }
                Err(msg) => return Err(msg),
            }
        }
        Err("No plan found".to_string())
    }

    // Test running a plan using an alt-rule (no change) group.
    #[test]
    fn within1() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));

        let ruls0: Vec<RuleStore> = vec![RuleStore::from("[00/XX/01/XX]").expect("SNH")];
        dm0.add_action(ruls0);

        // Form first group.
        dm0.cur_state = SomeState::from("0b0101")?; // -> 0111
        dm0.take_action_arbitrary(0);

        dm0.cur_state = SomeState::from("0b0000")?; // -> 0010
        dm0.take_action_arbitrary(0);

        println!("\n(1) dm0: cur_state {}", dm0.cur_state);
        println!("Acts: {}\n", dm0.actions);

        // One of the following plans will succeed as is, one will need to return to square 5 and try again, then it will succeed.
        dm0.cur_state = SomeState::from("0b0001")?; // -> 0011

        if let Ok(plans) = dm0.make_plans(
            &SomeRegion::from("r0001").expect("SNH"),
            &SomeRegion::from("r0011").expect("SNH"),
            &SomeRegion::from("r00XX")?,
        ) {
            // println!("2plans {plans}");
            match dm0.run_plan(&plans[0], 2) {
                Ok(num) => {
                    if num == 1 {
                        return Ok(());
                    } else {
                        return Err(format!("{num} steps?"));
                    }
                }
                Err(msg) => return Err(msg),
            }
        }

        Err("No plan found".to_string())
    }

    // Test a simple four-step plan to change the domain current state
    // from s0111 to s1000.
    #[test]
    fn make_plan_direct() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let dm0 = &mut dmxs[0];

        dm0.cur_state = SomeState::from("0b0001")?;
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);

        // Create group for region XXXX, Act 0.
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dm0.eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dm0.eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Create group for region XXXX, Act 3.
        dm0.eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dm0.eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?); // Last sample changes current state to s0111

        // Get plan for 7 to 8
        //        let cur_state = SomeState::from("0b0111")?;
        //        dm0.set_cur_state(cur_state);
        //        let toreg = SomeRegion::from("r1000")?;
        //
        //        if dmxs[0].make_plans(&toreg).is_some() {
        //        } else {
        //            return Err(String::from("No plan found to r1000?"));
        //        }

        Ok(())
    }

    // Test asymmetric chaining.  The plan may step out of the direct
    // glide path X1XX, between 7 and C, into X0XX, to change the third bit,
    // then step back into the glide path to get to the goal.
    #[test]
    fn make_plans_asymmetric() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let dm0 = &mut dmxs[0];
        dm0.cur_state = SomeState::from("0b0001")?;
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);

        // Create group for region XXXX->XXXx, Act 0.
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Create group for region XXXX->XXxX, Act 1.
        dm0.eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dm0.eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Create group for region XXXX-XxXX, Act 2.
        dm0.eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dm0.eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Create group for region X0XX->x0XX, Act 3.
        dm0.eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dm0.eval_sample_arbitrary(3, &SomeSample::from("0b1011->0b0011")?);

        println!("\nActs: {}", dm0.actions);

        // Glide Path is 7 + C = X1XX

        // Get plan for 7 to C
        // One bit that has to change, bit 3, 0...->1..., needs to use Act 3, 00XX->10XX,
        // which is outside of the Glide Path.
        //        let s7 = SomeState::from("0x7")?;
        //        dm0.set_cur_state(s7);
        //        let toreg = SomeRegion::from("r1100")?;
        //
        //        if let Some(plans) = &mut dmxs[0].make_plans(&toreg) {
        //            println!("plan: {}", plans);
        //        } else {
        //            return Err(String::from("No plan found s111 to r1100?"));
        //        }

        //assert!(1 == 2);
        Ok(())
    }

    // Test action:get_needs StateNotInGroup, two flavors.
    #[test]
    fn need_for_state_not_in_group() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0001")?;
        dm0.add_action(vec![]);

        // Check need for the current state not in a group.
        let nds1 = dm0.actions[0].state_not_in_group_needs(&dm0.cur_state);

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &ATarget::State {
                state: &SomeState::from("0b0001")?
            }
        ));

        // Create group for one sampledomain::tests::make_plans_asymmetric
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0001->0b0001")?);

        println!("\nActs: {}", &dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::from("r0001")?)
            .is_some());

        // Invalidate group for sample 1 by giving it GT 1 different result.
        // Current state changes to zero.
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0001->0b0000")?);

        println!("\nActs: {}", dm0.actions[0]);

        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::from("r0001")?)
            .is_none());

        // Check needs for pn > 1 and not in group, and current state not in a group.
        let nds1 = dm0.get_needs();
        println!("needs: {}", nds1);

        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &ATarget::State {
                state: &SomeState::from("0b0000")?
            }
        ));

        Ok(())
    }

    // Test confirm_group_needs.
    #[test]
    fn need_additional_group_state_samples() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0001")?;
        dm0.add_action(vec![]);

        // Check need for the current state not in a group.
        let nds1 = dm0.actions[0].state_not_in_group_needs(&dm0.cur_state);

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &ATarget::State {
                state: &SomeState::from("0b0001")?
            }
        ));

        // Create group for one sample
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0001->0b0001")?);

        println!("\nActs: {}", dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::from("r0001")?)
            .is_some());

        // Expand group
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0010->0b0010")?);

        println!("\nActs: {}", dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::from("r00XX")?)
            .is_some());

        let nds2 = dm0.actions[0].confirm_group_needs();
        println!("needs {}", nds2);

        assert_eq!(nds2.len(), 2);
        assert!(contains_similar_need(
            &nds2,
            "ConfirmGroup",
            &ATarget::State {
                state: &SomeState::from("0b0001")?
            }
        ));
        assert!(contains_similar_need(
            &nds2,
            "ConfirmGroup",
            &ATarget::State {
                state: &SomeState::from("0b0010")?
            }
        ));

        // Satisfy one need.
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0010->0b0010")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0010->0b0010")?);

        let nds3 = dm0.actions[0].confirm_group_needs();
        println!("needs {}", nds3);
        assert_eq!(nds3.len(), 1);
        assert!(contains_similar_need(
            &nds3,
            "ConfirmGroup",
            &ATarget::State {
                state: &SomeState::from("0b0001")?
            }
        ));

        // Satisfy second need.
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0001->0b0001")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0001->0b0001")?);

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
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0001")?;
        dm0.add_action(vec![]);

        // Create group for region XX0X.
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1101->0b1101")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1101->0b1101")?);

        // Create group X1XX
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0110->0b0110")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0110->0b0110")?);

        // Get and check needs.
        let nds1 = dm0.actions[0].group_pair_needs();
        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "ContradictoryIntersection",
            &ATarget::Region {
                region: &SomeRegion::from("rX100")?
            }
        ));

        Ok(())
    }

    #[test]
    fn limit_group_needs() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0001")?;
        dm0.add_action(vec![]);

        let max_reg = SomeRegion::from("rXXXX")?;

        // Set up group XXXX_XX0X->XXXX_XX0X
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0100->0b0010")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0100->0b0010")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0100->0b0010")?);

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1001->0b0010")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1001->0b0010")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1001->0b0010")?);

        let Some(nds1) = dm0.actions[0].limit_groups_needs(&max_reg) else {
            return Err("No needs?".to_string());
        };
        println!("dm0 {}", dm0.actions[0]);
        println!("Needs: {}", nds1);

        let grp_reg = SomeRegion::from("rXX0X")?;
        let Some(anchor_sta) = &dm0.actions[0]
            .groups
            .find(&grp_reg)
            .as_ref()
            .expect("SNH")
            .anchor
        else {
            return Err("limit_groups_needs anchor not set".to_string());
        };

        println!("anchor is {}", anchor_sta);

        if *anchor_sta == SomeState::from("0b1001")? {
            // limiting square for anchor 9 is B.
            dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1011->0b1011")?);
            dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1011->0b1011")?);
            dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1011->0b1011")?);
        } else {
            // Limiting square for anchor 4 is 6.
            dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0110->0b0110")?);
            dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0110->0b0110")?);
            dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0110->0b0110")?);
        }

        println!("dm0 {}", dm0.actions[0]);

        let Some(nds2) = dm0.actions[0].limit_groups_needs(&max_reg) else {
            return Err("limit_groups_needs returns None?".to_string());
        };

        println!("dm0 {}", dm0.actions[0]);

        println!("needs are {}", nds2);

        let grpx = dm0.actions[0].groups.find(&grp_reg).expect("SNH");
        assert!(grpx.limited);

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
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0001")?;
        dm0.add_action(vec![]);

        let rx1x1 = SomeRegion::from("rx1x1")?;

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0101->0b0101")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0101->0b0100")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0101->0b0101")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0101->0b0100")?);

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1111")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1111")?);

        println!("\nActs: {}", dm0.actions[0]);

        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
            println!("\nActs: {}", dm0.actions[0]);
        } else {
            return Err(String::from("Group rx1x1 was not formed by two squares?"));
        }

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0111->0b0111")?);
        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
        } else {
            return Err(String::from("Group rx1x1 deleted too soon?"));
        }

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0111->0b0111")?);
        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
        } else {
            return Err(String::from("Group rx1x1 deleted too soon?"));
        }

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0111->0b0111")?);
        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
            return Err(String::from("Group rx1x1 not deleted?"));
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
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0001")?;
        dm0.add_action(vec![]);

        let rx1x1 = SomeRegion::from("rx1x1")?;

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0101->0b0101")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0101->0b0100")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0101->0b1110")?);

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1111")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b0100")?);

        println!("\n1 Acts: {}", dm0.actions[0]);
        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0111->0b0111")?);
        println!("\n2 Acts: {}", dm0.actions[0]);

        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0111->0b0111")?);
        println!("\n2 Acts: {}", dm0.actions[0]);

        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b0111->0b0111")?); // cause pn-not-Two invalidation
        println!("\n3 Acts: {}", dm0.actions[0]);

        assert!(dm0.actions[0].groups.find(&rx1x1).is_none());

        Ok(())
    } // end group_pn_u_union_then_invalidation

    // For showing something easily understandable, the groups in the program are shown
    // with four, or fewer, edges.
    // It is important to show that any arbitrary number of edges can form a group / rule.
    #[test]
    fn create_group_rule_with_ten_edges() -> Result<(), String> {
        // Create a domain that uses two integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(16)));
        dm0.cur_state = SomeState::from("0b0000000000000001")?;
        dm0.add_action(vec![]);

        // Create group for region XXX1010X101010XX.
        dm0.eval_sample_arbitrary(
            0,
            &SomeSample::from("0b0001010010101000->0b0001010010111000")?,
        );
        dm0.eval_sample_arbitrary(
            0,
            &SomeSample::from("0b1111010110101011->0b1111010110111011")?,
        );

        println!("\nActs: {}", dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::from("rXXX1010X101010XX")?)
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
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0011")?;
        dm0.add_action(vec![]); // Act 0

        // Start groups.
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1101->0b1101")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1111")?);

        // Confirm groups.
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1101->0b1101")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1111")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1101->0b1101")?);
        dm0.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1111")?);

        // get_needs checks the limited flag for each group.
        let nds = dm0.get_needs();
        println!("\n(1){}", dm0.actions[act0]);
        println!("needs {}", nds);

        let grpx = dm0.actions[act0]
            .groups
            .find(&SomeRegion::from("r11x1").expect("SNH"))
            .expect("SNH");
        assert!(grpx.limited);

        // Get needs for a given max_reg.
        let max_reg = SomeRegion::from("rX11X").expect("SNH");
        let nds = dm0.actions[act0].limit_groups_needs(&max_reg);
        println!("\n(1){}", dm0.actions[act0]);
        if let Some(needs) = nds {
            println!("needs {}", needs);
            assert!(needs.len() == 2);
            assert!(contains_similar_need(
                &needs,
                "LimitGroupAdj",
                &ATarget::State {
                    state: &SomeState::from("0b1110")?
                }
            ));
            assert!(contains_similar_need(
                &needs,
                "LimitGroupAdj",
                &ATarget::State {
                    state: &SomeState::from("0b0111")?
                }
            ));
        } else {
            println!("needs []");
            panic!("SNH");
        }

        // Get needs for a another max_reg.
        let max_reg = SomeRegion::from("rX10X").expect("SNH");
        let nds = dm0.actions[act0].limit_groups_needs(&max_reg);
        println!("\n(2){}", dm0.actions[act0]);
        if let Some(needs) = nds {
            println!("needs {}", needs);
            assert!(needs.len() == 2);
            assert!(contains_similar_need(
                &needs,
                "LimitGroupAdj",
                &ATarget::State {
                    state: &SomeState::from("0b1100").expect("SNH")
                }
            ));
            assert!(contains_similar_need(
                &needs,
                "LimitGroupAdj",
                &ATarget::State {
                    state: &SomeState::from("0b0101").expect("SNH")
                }
            ));
        } else {
            println!("needs []");
            panic!("SNH");
        }

        Ok(())
    }

    #[test]
    fn shortcuts3() -> Result<(), String> {
        // Create a domain that uses 4 bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0011")?;

        // Set up action 0, changing bit 0.
        let ruls0: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/XX/Xx]").expect("SNH")];
        dm0.add_action(ruls0);

        // Set up action 1, changing bit 1.
        let ruls1: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/Xx/XX]").expect("SNH")];
        dm0.add_action(ruls1);

        // Set up action 2, changing bit 2.
        let ruls2: Vec<RuleStore> = vec![RuleStore::from("[XX/Xx/XX/XX]").expect("SNH")];
        dm0.add_action(ruls2);

        // Set up action 3, changing bit 3.
        let ruls3: Vec<RuleStore> = vec![RuleStore::from("[Xx/XX/XX/XX]").expect("SNH")];
        dm0.add_action(ruls3);

        // Create states for setting up groups.
        let sta_0 = SomeState::from("0b0000")?;
        let sta_f = SomeState::from("0b1111")?;

        // Set up groups for action 0.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(0);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(0);

        // Set up groups for action 1.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(1);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(1);

        // Set up groups for action 2.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(2);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(2);

        // Set up groups for action 3.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(3);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(3);

        println!("Acts: {}\n", dm0.actions);

        let pln1 = SomePlan::from("P[r0000-2->r0100-3->r1100-2->r1000]")?;
        println!("pln1: {}", pln1);

        if let Some(shortcuts) = dm0.shortcuts(&pln1, &SomeRegion::from("rXXXX")?) {
            assert!(shortcuts.len() == 1);
            let shrt = &shortcuts[0];
            println!("shrt {shrt}");
            assert!(shrt.len() == 1);
            assert!(shrt[0].initial == SomeRegion::from("r0000")?);
            assert!(shrt[0].result == SomeRegion::from("r1000")?);
            //assert!(1 == 2);
            return Ok(());
        }
        Err(format!("Shortcuts not found?"))
    }

    #[test]
    fn shortcuts5() -> Result<(), String> {
        // Create a domain that uses 4 bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0011")?;

        // Set up action 0, changing bit 0.
        let ruls0: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/XX/Xx]").expect("SNH")];
        dm0.add_action(ruls0);

        // Set up action 1, changing bit 1.
        let ruls1: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/Xx/XX]").expect("SNH")];
        dm0.add_action(ruls1);

        // Set up action 2, changing bit 2.
        let ruls2: Vec<RuleStore> = vec![RuleStore::from("[XX/Xx/XX/XX]").expect("SNH")];
        dm0.add_action(ruls2);

        // Set up action 3, changing bit 3.
        let ruls3: Vec<RuleStore> = vec![RuleStore::from("[Xx/XX/XX/XX]").expect("SNH")];
        dm0.add_action(ruls3);

        // Create states for setting up groups.
        let sta_0 = SomeState::from("0b0000")?;
        let sta_f = SomeState::from("0b1111")?;

        // Set up groups for action 0.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(0);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(0);

        // Set up groups for action 1.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(1);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(1);

        // Set up groups for action 2.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(2);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(2);

        // Set up groups for action 3.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(3);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(3);

        println!("Acts: {}\n", dm0.actions);

        let pln1 = SomePlan::from(
            "P[r0100-0->r0101-1->r0111-0->r0110-2->r0010-0->r0011-1->r1011]",
        )?;
        println!("pln1: {}", pln1);

        if let Some(shortcuts) = dm0.shortcuts(&pln1, &SomeRegion::from("rXXXX")?) {
            // Check shortcuts.
            println!("Shortcuts: {shortcuts}");
            assert!(shortcuts.len() == 1);

            let shrt = &shortcuts[0];
            assert!(shrt.len() == 4);

            assert!(shrt.initial_region() == pln1.initial_region());

            assert!(shrt.result_region() == pln1.result_region());
        } else {
            return Err("No shortcuts found".to_string());
        }
        Ok(())
    }

    #[test]
    fn shortcuts6() -> Result<(), String> {
        // Create a domain that uses 4 bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0011")?;

        // Set up action 0, changing bit 0.
        let ruls0: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/XX/Xx]").expect("SNH")];
        dm0.add_action(ruls0);

        // Set up action 1, changing bit 1.
        let ruls1: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/Xx/XX]").expect("SNH")];
        dm0.add_action(ruls1);

        // Set up action 2, changing bit 2.
        let ruls2: Vec<RuleStore> = vec![RuleStore::from("[XX/Xx/XX/XX]").expect("SNH")];
        dm0.add_action(ruls2);

        // Set up action 3, changing bit 3.
        let ruls3: Vec<RuleStore> = vec![RuleStore::from("[Xx/XX/XX/XX]").expect("SNH")];
        dm0.add_action(ruls3);

        // Create states for setting up groups.
        let sta_0 = SomeState::from("0b0000")?;
        let sta_f = SomeState::from("0b1111")?;

        // Set up groups for action 0.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(0);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(0);

        // Set up groups for action 1.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(1);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(1);

        // Set up groups for action 2.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(2);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(2);

        // Set up groups for action 3.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(3);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(3);

        println!("Acts: {}\n", dm0.actions);

        let pln1 = SomePlan::from("P[r1011-0->r1111-1->r1110-0->r0110-2->r0111]")?;
        println!("pln1: {}", pln1);

        if let Some(shortcuts) = dm0.shortcuts(&pln1, &SomeRegion::from("rXXXX")?) {
            // Check shortcuts.
            println!("Shortcuts: {shortcuts}");
            assert!(shortcuts.len() == 1);

            let shrt = &shortcuts[0];
            assert!(shrt.len() == 2);
            assert!(shrt.initial_region() == pln1.initial_region());
            assert!(shrt.result_region() == pln1.result_region());
        } else {
            return Err("No shortcuts found".to_string());
        }

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn shortcuts7() -> Result<(), String> {
        // Create a domain that uses 4 bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = SomeState::from("0b0011")?;

        // Set up action 0, changing bit 0.
        let ruls0: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/XX/Xx]").expect("SNH")];
        dm0.add_action(ruls0);

        // Set up action 1, changing bit 1.
        let ruls1: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/Xx/XX]").expect("SNH")];
        dm0.add_action(ruls1);

        // Set up action 2, changing bit 2.
        let ruls2: Vec<RuleStore> = vec![RuleStore::from("[XX/Xx/XX/XX]").expect("SNH")];
        dm0.add_action(ruls2);

        // Set up action 3, changing bit 3.
        let ruls3: Vec<RuleStore> = vec![RuleStore::from("[Xx/XX/XX/XX]").expect("SNH")];
        dm0.add_action(ruls3);

        // Create states for setting up groups.
        let sta_0 = SomeState::from("0b0000")?;
        let sta_f = SomeState::from("0b1111")?;

        // Set up groups for action 0.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(0);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(0);

        // Set up groups for action 1.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(1);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(1);

        // Set up groups for action 2.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(2);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(2);

        // Set up groups for action 3.
        dm0.set_cur_state(sta_0.clone());
        dm0.take_action_arbitrary(3);
        dm0.set_cur_state(sta_f.clone());
        dm0.take_action_arbitrary(3);

        println!("Acts: {}\n", dm0.actions);

        let reg_0 = SomeRegion::from("r0000")?;
        let reg_f = SomeRegion::from("r1111")?;

        let pln1 = SomePlan::from(
            "P[r0000-0->r0001-1->r0011-0->r0010-2->r0110-2->r0111-2->r0101-2->r1101-2->r1111]",
        )?;
        println!("pln1: {}", pln1);

        if let Some(shortcuts) = dm0.shortcuts(&pln1, &SomeRegion::from("rXXXX")?) {
            // Check shortcuts.
            println!("Shortcuts: {shortcuts}");
            assert!(shortcuts.len() == 1);

            let plnx = &shortcuts[0];

            if plnx.len() < pln1.len() {
                if plnx.len() == 4 {
                    assert!(plnx.initial_region() == &reg_0);
                    assert!(plnx.result_region() == &reg_f);
                } else {
                    return Err("Four step shortcut not found".to_string());
                }
            } else {
                return Err(format!("shortcut {plnx} not shorter than {pln1}"));
            }
            if plnx.initial_region() != pln1.initial_region() {
                return Err(format!("shortcut {plnx} invalid initial region"));
            }
            if plnx.result_region() != pln1.result_region() {
                return Err(format!("shortcut {plnx} invalid initial region"));
            }
        } else {
            return Err("No shortcuts (1) found".to_string());
        }
        Ok(())
    }
} // end tests
