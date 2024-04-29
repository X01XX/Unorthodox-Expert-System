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
    /// Region with all bit positions set to X.
    pub max_poss_region: SomeRegion,
}

impl SomeDomain {
    /// Return a new domain instance, given the number of integers, the
    /// initial state, the optimal state(s), the index into the higher-level DomainStore.
    pub fn new(dom_id: usize, cur_state: SomeState) -> Self {
        let max_poss_region = SomeRegion::new(vec![cur_state.new_high(), cur_state.new_low()]);
        Self {
            id: dom_id,
            actions: ActionStore::new(vec![]),
            cur_state,
            max_poss_region,
        }
    }

    /// Return a reference to the current, internal, state.
    pub fn get_current_state(&self) -> &SomeState {
        &self.cur_state
    }

    /// Add a SomeAction instance to the store.
    pub fn add_action(&mut self, rules: Vec<RuleStore>) {
        self.actions.add_action(self.id, rules);
    }

    /// Return needs gathered from all actions.
    /// Some housekeeping is done, so self is mutable.
    pub fn get_needs(&mut self) -> NeedStore {
        self.actions.get_needs(&self.cur_state, self.id)
    }

    /// Evaluate an arbitrary sample given by the user.
    /// This tends to break things for an action, unless all samples are arbitrary.
    /// Useful for testing a wholly different series of samples/results.
    /// Using the command: ss  action-number  initial-state  result-state
    /// e.g. ss  0  s0b1010  s0b1111
    pub fn eval_sample_arbitrary(&mut self, act_id: usize, smpl: &SomeSample) {
        self.actions.eval_sample_arbitrary(act_id, smpl);
        self.set_cur_state(smpl.result.clone());
    }

    /// Take an action for a need, evaluate the resulting sample.
    /// It is assumed that a sample made for a need must be saved.
    pub fn take_action_need(&mut self, ndx: &SomeNeed) {
        let asample = self.actions.take_action_need(ndx, &self.cur_state);
        self.set_cur_state(asample.result.clone());
    }

    /// Take an action with the current state, store the sample.
    pub fn take_action_arbitrary(&mut self, act_id: usize) {
        let asample = self.actions.take_action_arbitrary(act_id, &self.cur_state);
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
    pub fn run_plan(&mut self, pln: &SomePlan, depth: usize) -> Result<usize, String> {
        assert_eq!(pln.dom_id, self.id);

        let mut num_steps = 0;

        if pln.is_empty() {
            return Ok(num_steps);
        }

        if !pln.initial_region().is_superset_of(&self.cur_state) {
            return Err(format!(
                "Current state {} is not in the start region of plan {}",
                &self.cur_state, &pln
            ));
        }

        // Run each step of the plan.
        for stpx in pln.iter() {
            let prev_state = self.cur_state.clone();

            let asample = self.actions.take_action_step(stpx.act_id, &self.cur_state);
            num_steps += 1;

            self.set_cur_state(asample.result.clone());

            if stpx.result.is_superset_of(&self.cur_state) {
                continue;
            }

            // Handle unexpected/unwanted result.
            println!(
                "\nChange [{} -{:02}> {}] unexpected, expected {}",
                prev_state, stpx.act_id, self.cur_state, stpx,
            );

            // Avoid an infinite loop of retries.
            if depth == 1 {
                return Err("Try return/retry depth limit exceeded".to_string());
            }

            // May be an expected possibility from a two result state.
            match &stpx.alt_rule {
                AltRuleHint::NoAlt {} => {
                    self.actions.eval_unexpected_result(stpx.act_id, &asample);
                    return Err("Unexpected result, step failed".to_string());
                }
                AltRuleHint::AltNoChange {} => {
                    println!("Try action a second time");

                    let asample = self.actions.take_action_step(stpx.act_id, &self.cur_state);
                    num_steps += 1;

                    self.set_cur_state(asample.result.clone());

                    if stpx.result.is_superset_of(&self.cur_state) {
                        continue;
                    }
                    self.actions.eval_unexpected_result(stpx.act_id, &asample);
                    return Err("Unexpected result, step failed".to_string());
                }
                AltRuleHint::AltRule { rule } => {
                    if rule.result_region().is_superset_of(&self.cur_state) {
                        if let Some(planx) =
                            self.make_plans(&SomeRegion::new(vec![prev_state.clone()]))
                        {
                            println!("Try action plan to return {}", planx[0]);
                            match self.run_plan(&planx[0], 1) {
                                Ok(num) => {
                                    println!("Try action return worked.");
                                    num_steps += num;

                                    let asample =
                                        self.actions.take_action_step(stpx.act_id, &self.cur_state);
                                    num_steps += 1;

                                    self.set_cur_state(asample.result.clone());

                                    if stpx.result.is_superset_of(&self.cur_state) {
                                        continue;
                                    }
                                    self.actions.eval_unexpected_result(stpx.act_id, &asample);
                                    return Err("Action return/retry failed.".to_string());
                                }
                                Err(msg) => {
                                    return Err(msg);
                                }
                            }
                        } else {
                            return Err(
                                "No plan to return, and try again, found. Step failed".to_string()
                            );
                        }
                    } else {
                        self.actions.eval_unexpected_result(stpx.act_id, &asample);
                        return Err("Unexpected result, step failed".to_string());
                    }
                }
            }
        } // next stpx

        if pln.result_region().is_superset_of(&self.cur_state) {
            Ok(num_steps)
        } else {
            Err("Plan result region not reached".to_string())
        }
    } // end run_plan

    /// Return the steps of a plan to go from a given state/region to a given region.
    pub fn depth_first_search(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        steps_str: &StepStore,
        steps_by_change_vov: &[Vec<&SomeStep>],
        depth: usize,
        verbose: bool,
    ) -> Option<SomePlan> {
        if let Some(plan1) = self.depth_first_search2(
            from_reg,
            goal_reg,
            steps_str,
            steps_by_change_vov,
            depth - 1,
            verbose,
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
    /// If a possible bit-change only has Asymmetric Chaining steps, randomly choose one to use.
    ///
    /// Otherwise, randomly choose a forward or backward chaining step.
    ///
    fn depth_first_search2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        steps_str: &StepStore,
        steps_by_change_vov: &[Vec<&SomeStep>],
        depth: usize,
        verbose: bool,
    ) -> Option<SomePlan> {
        if verbose {
            println!("\ndepth_first_search2: from {} to {}", from_reg, goal_reg);
        }

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

                    if verbose {
                        println!(
                            "    use one step {}",
                            stepy.restrict_initial_region(from_reg)
                        );
                    }
                    //println!("random_depth_first_search2: suc 1 Found one step {} to go from {} to {}", stepy, from_reg, goal_reg);
                    return Some(SomePlan::new(self.id, vec![stepz]));
                }
            }
        }

        // Check depth
        if depth == 0 {
            if verbose {
                println!("depth limit exceeded");
            }
            return None;
        }

        // Check for single-bit changes, where all steps are between the from-region and goal-region,
        // not intersecting either.
        //
        // This indicates that a, possibly significant, deviation from a straight-forward
        // rule-path is required.
        //
        // Storage for indicies of step vectors.
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

            let wanted_changes = SomeChange::new_region_to_region(from_reg, goal_reg);

            for inx in asym_only_changes.iter() {
                for (iny, stepx) in steps_by_change_vov[*inx].iter().enumerate() {
                    let rule_to = SomeRule::new_region_to_region(from_reg, &stepx.initial);

                    let aggregate_rule = rule_to.combine_sequence(&stepx.rule);

                    let step_changes = aggregate_rule.to_change();

                    let step_unwanted_changes = step_changes.bitwise_and_not(&wanted_changes);

                    let step_num_unwanted_changes = step_unwanted_changes.number_changes();

                    let step_num_wanted_changes =
                        step_changes.intersection(&wanted_changes).number_changes();

                    //println!("From {from_reg} To {goal_reg} step rule: {}->{} agg rule {}->{} num wanted changes {num_wanted_changes} step num wanted {step_num_wanted_changes} step num unwanted {step_num_unwanted_changes}",
                    //    stepx.rule.initial_region(), stepx.rule.result_region(),
                    //    aggregate_rule.initial_region(), aggregate_rule.result_region());

                    // Save ratio info.
                    // Ratio is wanted - unwanted.
                    ratios.push((
                        *inx,
                        iny,
                        step_num_wanted_changes as isize - step_num_unwanted_changes as isize,
                    ));
                }
            }
            //println!("ratios1: {ratios:?}");
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
            if verbose {
                println!("    use asymmetric step {}", stepx);
            }
            return self.asymmetric_chaining(from_reg, goal_reg, stepx, depth - 1, verbose);
        }

        // Asymmetric chaining not required.

        // Calc unwanted - unwanted changes for each possible step.
        let mut ratios: Vec<(usize, usize, isize)> = vec![];

        let wanted_changes = SomeChange::new_region_to_region(from_reg, goal_reg);

        for (inx, step_vecx) in steps_by_change_vov.iter().enumerate() {
            for (iny, stepx) in step_vecx.iter().enumerate() {
                let rule_to = SomeRule::new_region_to_region(from_reg, &stepx.initial);

                let step_changes = rule_to.combine_sequence(&stepx.rule).to_change();

                let step_unwanted_changes = step_changes.bitwise_and_not(&wanted_changes);

                let step_num_unwanted_changes = step_unwanted_changes.number_changes();

                let step_num_wanted_changes =
                    step_changes.intersection(&wanted_changes).number_changes();

                //println!("From {from_reg} To {goal_reg} step rule: {}->{} agg rule {}->{} num wanted changes {num_wanted_changes} step num wanted {step_num_wanted_changes} step num unwanted {step_num_unwanted_changes}",
                //    stepx.rule.initial_region(), stepx.rule.result_region(),
                //    aggregate_rule.initial_region(), aggregate_rule.result_region());

                // Save ratio info.
                // Ratio is wanted - unwanted.
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
            if verbose {
                println!(
                    "    use forward chaining step {}",
                    stepy.restrict_initial_region(from_reg)
                );
            }
            let plan_to_goal =
                self.plan_steps_between(&stepy.result, goal_reg, depth - 1, verbose)?;

            return SomePlan::new(self.id, vec![stepy]).link(&plan_to_goal);
        }

        // Process backward chaining step.
        if stepx.result.intersects(goal_reg) {
            let stepy = stepx.restrict_result_region(goal_reg);

            if verbose {
                println!(
                    "    use backward chaining step {}",
                    stepy.restrict_result_region(goal_reg)
                );
            }
            let plan_to_step =
                self.plan_steps_between(from_reg, &stepy.initial, depth - 1, verbose)?;

            return plan_to_step.link(&SomePlan::new(self.id, vec![stepy]));
        }

        // Must be an asymmetric step.
        if verbose {
            println!("    use asymmetric step {}", stepx);
        }
        self.asymmetric_chaining(from_reg, goal_reg, stepx, depth - 1, verbose)
    } // end random_depth_first_search2

    /// Return possible plan to change state between two regions.
    fn plan_steps_between(
        &self,
        from_reg: &SomeRegion,
        to_reg: &SomeRegion,
        depth: usize,
        verbose: bool,
    ) -> Option<SomePlan> {
        // println!("plan_steps_between: from {} to {} depth {}", from_reg, to_reg, depth);
        if depth == 0 {
            return None;
        }

        let required_change = SomeChange::new_region_to_region(from_reg, to_reg);

        let steps_str = self.get_steps(&required_change, None);
        if steps_str.is_empty() {
            return None;
        }

        let steps_by_change_vov = steps_str.get_steps_by_bit_change(&required_change)?;

        self.depth_first_search2(
            from_reg,
            to_reg,
            &steps_str,
            &steps_by_change_vov,
            depth - 1,
            verbose,
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
        verbose: bool,
    ) -> Option<SomePlan> {
        // println!("asymmetric_chaining: from: {} to: {} step: {} depth: {}", from_reg, goal_reg, stepx, depth);
        if depth == 0 {
            return None;
        }

        debug_assert!(!stepx.initial.intersects(from_reg));
        debug_assert!(!stepx.result.intersects(goal_reg));

        let (to_step_plan, stepy, from_step_plan) = if rand::random::<bool>() {
            let to_step_plan =
                self.plan_steps_between(from_reg, &stepx.initial, depth - 1, verbose)?;

            // Restrict the step initial region, in case it is different from the to_step_plan result region,
            // possibly changing the step result region.
            let stepy = stepx.restrict_initial_region(to_step_plan.result_region());

            let from_step_plan =
                self.plan_steps_between(&stepy.result, goal_reg, depth - 1, verbose)?;

            (to_step_plan, stepy, from_step_plan)
        } else {
            let from_step_plan =
                self.plan_steps_between(&stepx.result, goal_reg, depth - 1, verbose)?;

            // Restrict the step result region, in case it is different from the from_step_plan initial region,
            // possibly changing the step initial region.
            let stepy = stepx.restrict_result_region(from_step_plan.initial_region());

            let to_step_plan =
                self.plan_steps_between(from_reg, &stepy.initial, depth - 1, verbose)?;

            (to_step_plan, stepy, from_step_plan)
        };

        // Try linking two plans together with the step.
        to_step_plan
            .link(&SomePlan::new(self.id, vec![stepy]))?
            .link(&from_step_plan)
    }

    /// Make a plan to change the current state to another region.
    /// Since there are some random choices, it may be useful to try
    /// running make_plan more than once.
    pub fn make_plans(&self, goal_reg: &SomeRegion) -> Option<Vec<SomePlan>> {
        //println!("dom: {} make_plan start cur {} goal {}", self.num, self.cur_state, goal_reg);

        // Return no-op plan if the goal is already met.
        if goal_reg.is_superset_of(&self.cur_state) {
            //println!("no plan needed from {} to {} ?", self.cur_state, goal_reg);
            return Some(vec![SomePlan::new(self.id, vec![])]);
        }

        let cur_reg = SomeRegion::new(vec![self.cur_state.clone()]);

        self.make_plans2(&cur_reg, goal_reg, None)
    }

    /// Make a plan to change from a region to another region.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    pub fn make_plans2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        within: Option<&SomeRegion>,
    ) -> Option<Vec<SomePlan>> {
        //println!("\ndom {} make_plans2: from {from_reg} goal {goal_reg}", self.num);
        if goal_reg.is_superset_of(from_reg) {
            return Some(vec![SomePlan::new(self.id, vec![])]);
        }

        // Figure the required change.
        let required_change = SomeChange::new_region_to_region(from_reg, goal_reg);

        // Tune maximum depth to be a multiple of the number of bit changes required.
        let num_depth = 4 * required_change.number_changes();

        // Get steps, check if steps include all changes needed.
        let steps_str = self.get_steps(&required_change, within);
        if steps_str.is_empty() {
            return None;
        }

        // Get vector of steps for each bit change.
        let steps_by_change_vov = steps_str.get_steps_by_bit_change(&required_change)?;

        // Calculated steps_str, and steps_by_change_vov, ahead so that thay don't have to be
        // recalculated for each run, below, of random_depth_first_search.
        let plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| {
                self.depth_first_search(
                    from_reg,
                    goal_reg,
                    &steps_str,
                    &steps_by_change_vov,
                    num_depth,
                    false,
                )
            })
            .collect::<Vec<SomePlan>>();

        // Check for failure.
        if plans.is_empty() {
            return None;
        }

        //println!("dom {} make_plans2 returns {}", self.num, tools::vec_string(&plans));
        Some(plans)
    } // end make_plans2

    /// Make a plan to change the current state to another region.
    /// Since there are some random choices, it may be useful to try
    /// running make_plan more than once.
    pub fn make_one_plan(&self, from_reg: &SomeRegion, goal_reg: &SomeRegion) -> Option<SomePlan> {
        //println!("dom: {} make_one_plan start cur {} goal {}", self.num, self.cur_state, goal_reg);

        // Return no-op plan if the goal is already met.
        if goal_reg.is_superset_of(from_reg) {
            println!("no plan needed from {} to {} ?", self.cur_state, goal_reg);
            return Some(SomePlan::new(self.id, vec![]));
        }

        self.make_one_plan2(from_reg, goal_reg, None)
    }

    /// Make a plan to change from a region to another region.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    pub fn make_one_plan2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        within: Option<&SomeRegion>,
    ) -> Option<SomePlan> {
        //println!("\ndom {} make_plans2: from {from_reg} goal {goal_reg}", self.num);
        if goal_reg.is_superset_of(from_reg) {
            return Some(SomePlan::new(self.id, vec![]));
        }

        // Figure the required change.
        let required_change = SomeChange::new_region_to_region(from_reg, goal_reg);

        // Tune maximum depth to be a multiple of the number of bit changes required.
        let num_depth = 4 * required_change.number_changes();

        // Get steps, check if steps include all changes needed.
        let steps_str = self.get_steps(&required_change, within);
        if steps_str.is_empty() {
            println!("Rules covering all needed bit changes {required_change} not found");
            return None;
        }

        // Get vector of steps for each bit change.
        let steps_by_change_vov = steps_str.get_steps_by_bit_change(&required_change)?;

        // Calculated steps_str, and steps_by_change_vov, ahead so that thay don't have to be
        // recalculated for each run, below, of random_depth_first_search.
        self.depth_first_search(
            from_reg,
            goal_reg,
            &steps_str,
            &steps_by_change_vov,
            num_depth,
            true,
        )
    } // end make_one_plan2

    /// Get steps that may allow a change to be made.
    ///
    /// Steps that make at least one of the needed changes are extracted from domain actions.
    ///
    /// A rule that changes X->x will be pruned to make a step that changes 0->1 or 1->0 depending
    /// on the change needed. X->0 will be pruned to 1->0.  X->1 will be pruned to 0->1.
    ///
    /// If all steps found, in aggregate, cannot change all bits needed, return None.
    ///
    pub fn get_steps(
        &self,
        required_change: &SomeChange,
        within: Option<&SomeRegion>,
    ) -> StepStore {
        // Check if changes are possible.

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str: StepStore = self.actions.get_steps(required_change, within);

        // Check that the steps roughly encompass all needed changes, else return None.
        let Some(can_change) = steps_str.aggregate_changes() else {
            return StepStore::new(vec![]);
        };

        if required_change.is_subset_of(&can_change) {
        } else {
            //println!("get_steps: step_vec wanted changes {} are not a subset of step_vec changes {}, returning None", required_change, can_change);
            return StepStore::new(vec![]);
        }

        steps_str
    }

    /// Return a Region from a string.
    /// Left-most, consecutive, zeros can be omitted.
    //    pub fn region_from_string(&self, str: &str) -> Result<SomeRegion, String> {
    //        self.max_poss_region.new_from_string(str)
    //    } // end region_from_string

    /// Return a SomeRegion instance from a string.
    /// Left-most, consecutive, ommitted zeros are assumed tobe X.
    //    pub fn region_from_string_pad_x(&self, str: &str) -> Result<SomeRegion, String> {
    //        self.max_poss_region.new_from_string_pad_x(str)
    //    }

    /// Return a SomeState instance from a string.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn state_from_string(&self, str: &str) -> Result<SomeState, String> {
        self.cur_state._new_from_string(str)
    }

    /// Return a SomeRule instance from a string.
    /// Left-most, consecutive, 00s can be omitted.
    pub fn rule_from_string(&self, str: &str) -> Result<SomeRule, String> {
        SomeRule::new_from_string(str)
    }

    /// Return a Action number from a string with a format that the parse method can understand.
    /// Left-most, consecutive, zeros can be omitted.
    /// Returns an error if the string is bad or no action exists of that number.
    pub fn act_id_from_string(&self, str_num: &str) -> Result<usize, String> {
        match usize::from_str(str_num) {
            Ok(act_id) => {
                if act_id >= self.actions.len() {
                    return Err(format!("Action number too large {act_id}"));
                }
                Ok(act_id)
            }
            Err(error) => Err(format!("\nDid not understand action number, {error}")),
        }
    } // end act_id_from_string

    /// Return the current maximum region that can be reached from the current state.
    pub fn reachable_region(&self) -> SomeRegion {
        self.actions.reachable_region(&self.cur_state)
    }

    /// Return regions not covered by existing groups.
    pub fn regions_not_covered(&self, act_id: usize) -> RegionStore {
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

    /// Return a vector of rules for the "rx" command.
    pub fn all_rules(&self) -> Vec<(usize, &SomeRule)> {
        self.actions.all_rules()
    }
} // end impl SomeDomain

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
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

    // Test running a plan using an alt-rule (change) group.
    #[test]
    fn alt_rule1() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));

        let ruls0: Vec<RuleStore> = vec![RuleStore::new(vec![
            dm0.rule_from_string("00/11/01/11").expect("SNH"),
            dm0.rule_from_string("00/11/00/10").expect("SNH"),
        ])];
        dm0.add_action(ruls0);

        let ruls1: Vec<RuleStore> = vec![RuleStore::new(vec![dm0
            .rule_from_string("00/11/x0/x1")
            .expect("SNH")])];
        dm0.add_action(ruls1);

        dm0.cur_state = dm0.state_from_string("s0b0101")?;
        dm0.take_action_arbitrary(0);
        dm0.take_action_arbitrary(1);
        dm0.take_action_arbitrary(0);
        dm0.take_action_arbitrary(1);
        dm0.take_action_arbitrary(0);
        dm0.take_action_arbitrary(1);
        dm0.take_action_arbitrary(0);
        dm0.take_action_arbitrary(1);

        println!("\ndm0: cur_state {}", dm0.cur_state);
        println!("Acts: {}\n", dm0.actions);

        let mut num_steps1 = 0;

        // One of the following plans will succeed as is, one will need to return to square 5 and try again, then it will succeed.
        if let Some(plans) = dm0.make_plans2(
            &SomeRegion::new_from_string("r0101").expect("SNH"),
            &SomeRegion::new_from_string("r0100").expect("SNH"),
            None,
        ) {
            //println!("plans {}", tools::vec_string(&plans));
            match dm0.run_plan(&plans[0], 2) {
                Ok(num) => num_steps1 = num,
                Err(msg) => return Err(msg),
            }
        }

        // Reset current state to 5.
        dm0.take_action_arbitrary(1);

        let mut num_steps2 = 0;

        // Redo plans, as step alt value may change due to previous running of a plan.
        if let Some(plans) = dm0.make_plans2(
            &SomeRegion::new_from_string("r0101").expect("SNH"),
            &SomeRegion::new_from_string("r0100").expect("SNH"),
            None,
        ) {
            match dm0.run_plan(&plans[0], 2) {
                Ok(num) => num_steps2 = num,
                Err(msg) => return Err(msg),
            }
        }

        println!("num steps 1 {num_steps1} num steps 2 {num_steps2}");
        assert!(num_steps1 == 3 || num_steps2 == 3);

        Ok(())
    }

    // Test running a plan using an alt-rule (no change) group.
    #[test]
    fn alt_rule2() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));

        let ruls0: Vec<RuleStore> = vec![RuleStore::new(vec![
            dm0.rule_from_string("00/11/00/10").expect("SNH"),
            dm0.rule_from_string("00/11/00/11").expect("SNH"),
        ])];
        dm0.add_action(ruls0);

        let ruls1: Vec<RuleStore> = vec![RuleStore::new(vec![dm0
            .rule_from_string("00/11/x0/x1")
            .expect("SNH")])];
        dm0.add_action(ruls1);

        dm0.cur_state = dm0.state_from_string("s0b0101")?;
        dm0.take_action_arbitrary(0);
        dm0.take_action_arbitrary(1);
        dm0.take_action_arbitrary(0);
        dm0.take_action_arbitrary(1);
        dm0.take_action_arbitrary(0);
        dm0.take_action_arbitrary(1);
        dm0.take_action_arbitrary(0);
        dm0.take_action_arbitrary(1);

        println!("\n(1) dm0: cur_state {}", dm0.cur_state);
        println!("Acts: {}\n", dm0.actions);

        let mut num_steps1 = 0;

        // One of the following plans will succeed as is, one will need to return to square 5 and try again, then it will succeed.
        if let Some(plans) = dm0.make_plans2(
            &SomeRegion::new_from_string("r0101").expect("SNH"),
            &SomeRegion::new_from_string("r0100").expect("SNH"),
            None,
        ) {
            //println!("plans {}", tools::vec_string(&plans));
            match dm0.run_plan(&plans[0], 2) {
                Ok(num) => num_steps1 = num,
                Err(msg) => return Err(msg),
            }
        }

        println!("\n(2) dm0: cur_state {}", dm0.cur_state);
        println!("Acts: {}\n", dm0.actions);

        // Reset current state to 5.
        dm0.take_action_arbitrary(1);

        println!("\n(3) dm0: cur_state {}", dm0.cur_state);
        println!("Acts: {}\n", dm0.actions);

        let mut num_steps2 = 0;

        // Redo plans, as step alt value may change due to previous running of a plan.
        if let Some(plans) = dm0.make_plans2(
            &SomeRegion::new_from_string("r0101").expect("SNH"),
            &SomeRegion::new_from_string("r0100").expect("SNH"),
            None,
        ) {
            match dm0.run_plan(&plans[0], 2) {
                Ok(num) => num_steps2 = num,
                Err(msg) => return Err(msg),
            }
        }

        println!("num steps 1 {num_steps1} num steps 2 {num_steps2}");
        assert!(num_steps1 == 2 || num_steps2 == 2);

        Ok(())
    }

    // Test a simple four-step plan to change the domain current state
    // from s0111 to s1000.
    #[test]
    fn make_plan_direct() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let dm0 = &mut dmxs[0];

        dm0.cur_state = dm0.state_from_string("s0b0001")?;
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);

        let s0 = dm0.state_from_string("s0b0000")?;
        let s1 = dm0.state_from_string("s0b0001")?;
        let s2 = dm0.state_from_string("s0b0010")?;
        let s4 = dm0.state_from_string("s0b0100")?;
        let s7 = dm0.state_from_string("s0b0111")?;
        let s8 = dm0.state_from_string("s0b1000")?;
        let sb = dm0.state_from_string("s0b1011")?;
        let sd = dm0.state_from_string("s0b1101")?;
        let se = dm0.state_from_string("s0b1110")?;
        let sf = dm0.state_from_string("s0b1111")?;

        // Create group for region XXXX, Act 0.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se));

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2));
        dm0.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd));

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4));
        dm0.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb));

        // Create group for region XXXX, Act 3.
        dm0.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8));
        dm0.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7)); // Last sample changes current state to s0111

        // Get plan for 7 to 8
        let cur_state = dm0.state_from_string("s0b0111")?;
        dm0.set_state(&cur_state);
        let toreg = SomeRegion::new_from_string("r1000")?;

        if dmxs[0].make_plans(&toreg).is_some() {
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
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let dm0 = &mut dmxs[0];
        dm0.cur_state = dm0.state_from_string("s0b0001")?;
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);
        dm0.add_action(vec![]);

        let s0 = dm0.state_from_string("s0b0000")?;
        let s1 = dm0.state_from_string("s0b0001")?;
        let s2 = dm0.state_from_string("s0b0010")?;
        let s3 = dm0.state_from_string("s0b0011")?;
        let s4 = dm0.state_from_string("s0b0100")?;
        let s8 = dm0.state_from_string("s0b1000")?;
        let sb = dm0.state_from_string("s0b1011")?;
        let sd = dm0.state_from_string("s0b1101")?;
        let se = dm0.state_from_string("s0b1110")?;
        let sf = dm0.state_from_string("s0b1111")?;

        // Create group for region XXXX->XXXx, Act 0.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se));

        // Create group for region XXXX->XXxX, Act 1.
        dm0.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2));
        dm0.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd));

        // Create group for region XXXX-XxXX, Act 2.
        dm0.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4));
        dm0.eval_sample_arbitrary(2, &SomeSample::new(sf, sb.clone()));

        // Create group for region X0XX->x0XX, Act 3.
        dm0.eval_sample_arbitrary(3, &SomeSample::new(s0, s8));
        dm0.eval_sample_arbitrary(3, &SomeSample::new(sb, s3));

        println!("\nActs: {}", dm0.actions);

        // Glide Path is 7 + C = X1XX

        // Get plan for 7 to C
        // One bit that has to change, bit 3, 0...->1..., needs to use Act 3, 00XX->10XX,
        // which is outside of the Glide Path.
        let s7 = dm0.state_from_string("s0x07")?;
        dm0.set_state(&s7);
        let toreg = SomeRegion::new_from_string("r1100")?;

        if let Some(plans) = &mut dmxs[0].make_plans(&toreg) {
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
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action(vec![]);

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0].state_not_in_group_needs(&dm0.cur_state);

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &SomeRegion::new_from_string("r0001")?
        ));

        // Create group for one sample
        let s1 = dm0.state_from_string("s0b0001")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s1.clone(), s1.clone()));

        println!("\nActs: {}", &dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::new_from_string("r0001")?)
            .is_some());

        // Invalidate group for sample 1 by giving it GT 1 different result.
        // Current state changes to zero.
        let s1 = dm0.state_from_string("s0b0001")?;
        dm0.eval_sample_arbitrary(
            0,
            &SomeSample::new(s1.clone(), dm0.state_from_string("s0b0000")?),
        );

        println!("\nActs: {}", dm0.actions[0]);

        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::new_from_string("r0001")?)
            .is_none());

        // Check needs for pn > 1 and not in group, and current state not in a group.
        let nds1 = dm0.get_needs();
        println!("needs: {}", nds1);

        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &SomeRegion::new_from_string("r0000")?
        ));

        Ok(())
    }

    // Test confirm_group_needs.
    #[test]
    fn need_additional_group_state_samples() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = dm0.state_from_string("s0b0001")?;
        dm0.add_action(vec![]);

        // Check need for the current state not in a group.
        let nds1 = dm0.actions.avec[0].state_not_in_group_needs(&dm0.cur_state);

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &SomeRegion::new_from_string("r0001")?
        ));

        // Create group for one sample
        let s1 = dm0.state_from_string("s0b0001")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s1.clone(), s1.clone()));

        println!("\nActs: {}", dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::new_from_string("r0001")?)
            .is_some());

        // Expand group
        let s2 = dm0.state_from_string("s0b0010")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s2.clone(), s2.clone()));

        println!("\nActs: {}", dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::new_from_string("r00XX")?)
            .is_some());

        let nds2 = dm0.actions[0].confirm_group_needs();
        println!("needs {}", nds2);

        assert_eq!(nds2.len(), 2);
        assert!(contains_similar_need(
            &nds2,
            "ConfirmGroup",
            &SomeRegion::new_from_string("r0001")?
        ));
        assert!(contains_similar_need(
            &nds2,
            "ConfirmGroup",
            &SomeRegion::new_from_string("r0010")?
        ));

        // Satisfy one need.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s2.clone(), s2.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s2.clone(), s2.clone()));

        let nds3 = dm0.actions[0].confirm_group_needs();
        println!("needs {}", nds3);
        assert_eq!(nds3.len(), 1);
        assert!(contains_similar_need(
            &nds3,
            "ConfirmGroup",
            &SomeRegion::new_from_string("r0001")?
        ));

        // Satisfy second need.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s1.clone(), s1.clone()));
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
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = dm0.state_from_string("s0b0001")?;
        dm0.add_action(vec![]);

        let s00 = dm0.state_from_string("s0b0000")?;
        let s01 = dm0.state_from_string("s0b0001")?;
        let s06 = dm0.state_from_string("s0b0110")?;
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
            &SomeRegion::new_from_string("rX100")?
        ));

        Ok(())
    }

    #[test]
    fn limit_group_needs() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut dm0 = SomeDomain::new(0, SomeState::new(SomeBits::new(4)));
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action(vec![]);

        let max_reg = SomeRegion::new_from_string("rXXXX")?;

        // Set up group XXXX_XX0X->XXXX_XX0X
        let s04 = dm0.state_from_string("s0b0100")?;
        let s02 = dm0.state_from_string("s0b0010")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s04.clone(), s02.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s04.clone(), s02.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s04.clone(), s02.clone()));

        let sf9 = dm0.state_from_string("s0b1001")?;
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf9.clone(), s02.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf9.clone(), s02.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf9.clone(), s02.clone()));

        let Some(nds1) = dm0.actions[0].limit_groups_needs(&max_reg) else {
            return Err("No needs?".to_string());
        };
        println!("dm0 {}", dm0.actions[0]);
        println!("Needs: {}", nds1);

        let grp_reg = SomeRegion::new_from_string("rXX0X")?;
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

        if *anchor_sta == dm0.state_from_string("s0b1001")? {
            // limiting square for anchor 9 is B.
            let s0b = dm0.state_from_string("s0b1011")?;
            dm0.eval_sample_arbitrary(0, &SomeSample::new(s0b.clone(), s0b.clone()));
            dm0.eval_sample_arbitrary(0, &SomeSample::new(s0b.clone(), s0b.clone()));
            dm0.eval_sample_arbitrary(0, &SomeSample::new(s0b.clone(), s0b.clone()));
        } else {
            // Limiting square for anchor 4 is 6.
            let s06 = dm0.state_from_string("s0b0110")?;
            dm0.eval_sample_arbitrary(0, &SomeSample::new(s06.clone(), s06.clone()));
            dm0.eval_sample_arbitrary(0, &SomeSample::new(s06.clone(), s06.clone()));
            dm0.eval_sample_arbitrary(0, &SomeSample::new(s06.clone(), s06.clone()));
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
        dm0.cur_state = dm0.state_from_string("s0b0001")?;
        dm0.add_action(vec![]);

        let s5 = dm0.state_from_string("s0b0101")?;

        let s4 = dm0.state_from_string("s0b0100")?;

        let sf = dm0.state_from_string("s0b1111")?;

        let se = dm0.state_from_string("s0b1110")?;

        let s7 = dm0.state_from_string("s0b0111")?;

        let rx1x1 = SomeRegion::new_from_string("rx1x1")?;

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s5.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s4.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s5.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s4.clone()));

        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), sf.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), sf.clone()));

        println!("\nActs: {}", dm0.actions[0]);

        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
            println!("\nActs: {}", dm0.actions[0]);
        } else {
            return Err(String::from("Group rx1x1 was not formed by two squares?"));
        }

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone()));
        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
        } else {
            return Err(String::from("Group rx1x1 deleted too soon?"));
        }

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone()));
        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
        } else {
            return Err(String::from("Group rx1x1 deleted too soon?"));
        }

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone()));
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
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action(vec![]);

        let s5 = dm0.state_from_string("s0b0101")?;
        let s4 = dm0.state_from_string("s0b0100")?;
        let sf = dm0.state_from_string("s0b1111")?;
        let se = dm0.state_from_string("s0b1110")?;
        let s7 = dm0.state_from_string("s0b0111")?;

        let rx1x1 = SomeRegion::new_from_string("rx1x1")?;

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s5.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), s4.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s5.clone(), se.clone()));

        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), sf.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), s4.clone()));

        println!("\n1 Acts: {}", dm0.actions[0]);
        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone()));
        println!("\n2 Acts: {}", dm0.actions[0]);

        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone()));
        println!("\n2 Acts: {}", dm0.actions[0]);

        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(0, &SomeSample::new(s7.clone(), s7.clone())); // cause pn-not-Two invalidation
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
        dm0.cur_state = dm0.state_from_string("s0b1")?;
        dm0.add_action(vec![]);

        let s0 = dm0.state_from_string("s0b0001010010101000")?;
        let s1 = dm0.state_from_string("s0b0001010010111000")?;

        let s2 = dm0.state_from_string("s0b1111010110101011")?;
        let s3 = dm0.state_from_string("s0b1111010110111011")?;
        // Region                          XXX1010X101010XX.

        // Create group for region XXX1010X101010XX.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0, s1));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s2, s3));

        println!("\nActs: {}", dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&SomeRegion::new_from_string("rXXX1010X101010XX")?)
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
        dm0.cur_state = dm0.state_from_string("s0b0011")?;
        dm0.add_action(vec![]); // Act 0

        let s0d = dm0.state_from_string("s0b1101")?;
        let s0f = dm0.state_from_string("s0b1111")?;

        // Start groups.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0d.clone(), s0d.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0f.clone(), s0f.clone()));

        // Confirm groups.
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0d.clone(), s0d.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0f.clone(), s0f.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0d.clone(), s0d.clone()));
        dm0.eval_sample_arbitrary(0, &SomeSample::new(s0f.clone(), s0f.clone()));

        // get_needs checks the limited flag for each group.
        let nds = dm0.get_needs();
        println!("\n(1){}", dm0.actions[act0]);
        println!("needs {}", nds);

        let grpx = dm0.actions[act0]
            .groups
            .find(&SomeRegion::new_from_string("r11x1").expect("SNH"))
            .expect("SNH");
        assert!(grpx.limited);

        // Get needs for a given max_reg.
        let max_reg = SomeRegion::new_from_string("rX11X").expect("SNH");
        let nds = dm0.actions[act0].limit_groups_needs(&max_reg);
        println!("\n(1){}", dm0.actions[act0]);
        if let Some(needs) = nds {
            println!("needs {}", needs);
            assert!(needs.len() == 2);
            assert!(contains_similar_need(
                &needs,
                "LimitGroupAdj",
                &SomeRegion::new_from_string("r1110").expect("SNH")
            ));
            assert!(contains_similar_need(
                &needs,
                "LimitGroupAdj",
                &SomeRegion::new_from_string("r0111").expect("SNH")
            ));
        } else {
            println!("needs []");
            panic!("SNH");
        }

        // Get needs for a another max_reg.
        let max_reg = SomeRegion::new_from_string("rX10X").expect("SNH");
        let nds = dm0.actions[act0].limit_groups_needs(&max_reg);
        println!("\n(2){}", dm0.actions[act0]);
        if let Some(needs) = nds {
            println!("needs {}", needs);
            //assert!(needs.len() == 2);
            //assert!(contains_similar_need(&needs, "LimitGroupAdj", &SomeRegion::new_from_string("r1110").expect("SNH")));
            //assert!(contains_similar_need(&needs, "LimitGroupAdj", &SomeRegion::new_from_string("r0111").expect("SNH")));
        } else {
            println!("needs []");
            panic!("SNH");
        }

        Ok(())
    }
} // end tests
