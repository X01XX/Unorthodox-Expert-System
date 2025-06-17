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

use crate::action::SomeAction;
use crate::actionstore::ActionStore;
use crate::change::SomeChange;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::step::AltRuleHint;
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

        let mut first = true;
        for stepx in self.steps.iter() {
            if first {
                first = false;
            } else {
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
        let mut ret = 2; // for [...]
        if !self.steps.is_empty() {
            ret += (2 * self.steps.len()) + (self.steps[0].strlen() * self.steps.len())
        }
        ret
    }
}

impl fmt::Display for SomeDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_state())
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
    /// A aggregate of all samples seen.
    pub union_all_states: SomeRegion,
}

/// Implement the PartialEq trait, since two SomeDomain instances.
/// A quick comparison of definitions.
impl PartialEq for SomeDomain {
    fn eq(&self, other: &Self) -> bool {
        if self.actions.len() != other.actions.len() {
            return false;
        }
        for (actx, acty) in self.actions.iter().zip(other.actions.iter()) {
            if actx != acty {
                return false;
            }
        }
        true
    }
}
impl Eq for SomeDomain {}

impl SomeDomain {
    /// Return a new domain instance, given the ID number and initial state.
    pub fn new(dom_id: usize, cur_state: SomeState) -> Self {
        // Build a rule that does nothing.
        let low_state = cur_state.new_low();
        let high_state = cur_state.new_high();

        let rule0 = SomeRule::new(&SomeSample::new(low_state.clone(), low_state.clone())).union(
            &SomeRule::new(&SomeSample::new(high_state.clone(), high_state.clone())),
        );

        let mut domx = Self {
            id: dom_id,
            actions: ActionStore::new(vec![]),
            union_all_states: SomeRegion::new(vec![cur_state.clone()]),
            cur_state,
        };

        // Build, populate and add the first action, which does nothing.
        let mut act0 = SomeAction::new(vec![RuleStore::new(vec![rule0])]);
        act0.take_action_arbitrary(&low_state);
        act0.take_action_arbitrary(&low_state);
        act0.take_action_arbitrary(&low_state);
        act0.take_action_arbitrary(&high_state);
        act0.take_action_arbitrary(&high_state);
        act0.take_action_arbitrary(&high_state);

        domx.push(act0);

        domx
    }

    /// Set the domain id, update same in all actions.
    pub fn set_id(&mut self, id: usize) {
        self.id = id;
        for actx in self.actions.iter_mut() {
            actx.set_dom_id(id);
        }
    }

    /// Return a reference to the current, internal, state.
    pub fn current_state(&self) -> &SomeState {
        &self.cur_state
    }

    /// Add a new action.
    pub fn push(&mut self, mut actx: SomeAction) {
        actx.set_id(self.actions.len());
        actx.set_dom_id(self.id);
        self.actions.push(actx);
    }

    /// Return needs gathered from all actions.
    /// Some housekeeping is done, so self is mutable.
    pub fn get_needs(&mut self) -> NeedStore {
        let needs = self
            .actions
            .get_needs(&self.cur_state, &self.union_all_states);

        if needs.is_empty() {
            // Try again, as get_needs can change a few things.
            self.actions
                .get_needs(&self.cur_state, &self.union_all_states)
        } else {
            needs
        }
    }

    /// Check, and update, union_all_states.
    fn check_union_all_states(&mut self, smpl: &SomeSample) {
        let previous = self.union_all_states.clone();

        // Add  sample result state, if needed.
        if self.union_all_states.is_superset_of(&smpl.result) {
        } else {
            self.union_all_states = self.union_all_states.union(&smpl.result);
        }

        // If the union_all_states is updated, that means there is at least one new X position.
        // Limited groups, with a 0 or 1 under the new X bits, will need to
        // be re-limited.
        if self.union_all_states != previous {
            self.actions.check_limited(&self.union_all_states);
        }
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

        self.check_union_all_states(&asample);

        self.set_cur_state(asample.result.clone());
    }

    /// Take an action with the current state, store the sample.
    pub fn take_action(&mut self, act_id: usize) {
        debug_assert!(act_id < self.actions.len());

        let asample = self.actions.take_action_arbitrary(act_id, &self.cur_state);

        self.check_union_all_states(&asample);

        self.set_cur_state(asample.result.clone());
    }

    /// Take an action with a given state, store the sample.
    pub fn take_action_arbitrary(&mut self, act_id: usize, astate: &SomeState) {
        debug_assert!(act_id < self.actions.len());

        let asample = self.actions.take_action_arbitrary(act_id, astate);

        self.check_union_all_states(&asample);

        self.set_cur_state(asample.result.clone());
    }

    /// Take an action with a given state, store the sample, a number of times..
    pub fn take_action_arbitrary_repeat(
        &mut self,
        act_id: usize,
        astate: &SomeState,
        num_times: usize,
    ) {
        debug_assert!(act_id < self.actions.len());

        for _x in 0..num_times {
            self.take_action_arbitrary(act_id, astate);
        }
    }

    /// Set the current state field.
    pub fn set_cur_state(&mut self, stax: SomeState) {
        debug_assert_eq!(stax.num_bits(), self.num_bits());

        self.cur_state = stax;
    }

    /// Run a plan, return number steps if it runs to completion.
    pub fn run_plan(&mut self, pln: &SomePlan) -> Result<usize, String> {
        //debug_assert_eq!(pln.dom_id, self.id);
        debug_assert!(pln.dom_id == self.id);

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

            if stpx.act_id == 0 {
                continue;
            }

            let act_id = stpx.act_id;
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

    /// Make a plan to change from a region to another region,
    /// within a region that must encompass the intermediate steps of a returned plan.
    pub fn make_plans(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        within: &SomeRegion,
    ) -> Result<PlanStore, Vec<String>> {
        //println!("domain::make_plans: from {from_reg} goal {goal_reg} within {within}");

        debug_assert_eq!(from_reg.num_bits(), self.num_bits());
        debug_assert_eq!(goal_reg.num_bits(), self.num_bits());
        debug_assert!(within.num_bits() == self.num_bits());
        debug_assert!(within.is_superset_of(from_reg));
        debug_assert!(within.is_superset_of(goal_reg));

        // Figure the required changes.
        let wanted_changes = SomeRule::new_region_to_region_min(from_reg, goal_reg).as_change();

        // Tune maximum depth to be a multiple of the number of bit changes required.
        let num_depth = 3 * wanted_changes.number_changes();

        // Collect possible groups.
        let plans = (0..6)
            .into_par_iter() // par_iter for parallel processing, iter for sequential diagnostic messages.
            .filter_map(|_| {
                match self.make_plans2(from_reg, goal_reg, within, num_depth) {
                    Ok(planx) => {
                        //println!("  {} plans found 1", plans.len());
                        //println!("make_plans2 num found {} plans", plans.len());

                        if let Some(shortcuts) = self.shortcuts(&planx, within) {
                            Some(shortcuts)
                        } else {
                            //println!("  {} plans return 1", plans.len());
                            Some(planx)
                        }
                    }
                    Err(_) => None,
                }
            })
            .collect::<Vec<SomePlan>>();

        if plans.is_empty() {
            Err(vec!["No plans found".to_string()])
        } else {
            Ok(PlanStore::new(plans))
        }
    }

    /// Make a plan to change from a region to another region.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    pub fn make_plans2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        within: &SomeRegion,
        depth: usize,
    ) -> Result<SomePlan, Vec<String>> {
        //println!(
        //    "\ndom {} make_plans2: from {from_reg} goal {goal_reg}",
        //    self.id
        //);
        debug_assert_eq!(from_reg.num_bits(), self.num_bits());
        debug_assert_eq!(goal_reg.num_bits(), self.num_bits());
        debug_assert!(within.num_bits() == self.num_bits());
        debug_assert!(within.is_superset_of(from_reg));
        debug_assert!(within.is_superset_of(goal_reg));
        debug_assert!(!goal_reg.is_superset_of(from_reg));

        // Check depth.
        if depth == 0 {
            return Err(vec!["Depth exceeded".to_string()]);
        }

        // Figure the required changes.
        let wanted_changes = SomeRule::new_region_to_region_min(from_reg, goal_reg).as_change();

        // Get steps, check if steps include all changes needed.
        let steps_str = self.get_steps(&wanted_changes, within);
        if steps_str.is_empty() {
            return Err(vec![format!(
                "domain::make_plans2: No steps found from {from_reg} to  {goal_reg} within {within}"
            )]);
        }
        //println!("steps_str {steps_str}");

        // Check if all wanted changes are available.
        if let Some(agg_change) = steps_str.aggregate_changes() {
            if !wanted_changes.is_subset_of(&agg_change) {
                return Err(vec![format!(
                    "domain::make_plans2: Some needed changes are not available."
                )]);
            }
        }

        // Accumulate steps that can change the from_reg.
        let mut from_steps = StepStore::new(vec![]);
        for stpx in steps_str.iter() {
            if from_reg.intersects(&stpx.initial) {
                from_steps.push(stpx.restrict_initial_region(from_reg));
            }
        }
        if from_steps.is_empty() {
            return Err(vec![format!(
                "domain::make_plans2: No steps found from {from_reg}"
            )]);
        }

        // Accumulate steps that can change from the goal_reg.
        let mut goal_steps = StepStore::new(vec![]);
        for stpx in steps_str.iter() {
            if stpx.result.intersects(goal_reg) {
                goal_steps.push(stpx.restrict_result_region(goal_reg));
            }
        }
        if goal_steps.is_empty() {
            return Err(vec![format!(
                "domain::make_plans2: No steps found to {goal_reg}"
            )]);
        }

        // Check if one step can solve the problem.
        let mut plan_options = vec![];
        for stpx in from_steps.iter() {
            if stpx.result.intersects(goal_reg) {
                plan_options.push(SomePlan::new(
                    self.id,
                    vec![stpx.restrict_result_region(goal_reg)],
                ));
            }
        }
        for stpx in goal_steps.iter() {
            if stpx.initial.intersects(from_reg) {
                plan_options.push(SomePlan::new(
                    self.id,
                    vec![stpx.restrict_initial_region(from_reg)],
                ));
            }
        }
        if !plan_options.is_empty() {
            let planx = plan_options.remove(rand::rng().random_range(0..plan_options.len()));
            //println!("planxx: One step {}", planx);
            return Ok(planx);
        }

        // Check if two steps can solve the problem.
        for stp_f in from_steps.iter() {
            for stp_g in goal_steps.iter() {
                if stp_f.result.intersects(&stp_g.initial) {
                    match SomePlan::new(self.id, vec![stp_f.clone()])
                        .link(&SomePlan::new(self.id, vec![stp_g.clone()]))
                    {
                        Ok(planx) => plan_options.push(planx),
                        Err(errstr) => return Err(vec![errstr]),
                    }
                }
            }
        }
        if !plan_options.is_empty() {
            let planx = plan_options.remove(rand::rng().random_range(0..plan_options.len()));
            //println!("planxx: Two steps {}", planx);
            return Ok(planx);
        }

        // Accumulate asymmetric steps.
        let mut asym_steps = vec![];
        for stpx in steps_str.iter() {
            if !stpx.initial.intersects(from_reg) && !stpx.result.intersects(goal_reg) {
                asym_steps.push(stpx.clone());
            }
        }
        if !asym_steps.is_empty() {
            // Calc plan from_reg to step, step to goal_reg.
            let stepx = asym_steps.remove(rand::rng().random_range(0..asym_steps.len()));

            if let Ok(plan_t) = self.make_plans2(from_reg, &stepx.initial, within, depth - 1) {
                if let Ok(plan_t2) = plan_t.link(&SomePlan::new(self.id, vec![stepx])) {
                    if let Ok(plan_f) =
                        self.make_plans2(plan_t2.result_region(), &goal_reg, within, depth - 1)
                    {
                        if let Ok(plan_ret) = plan_t2.link(&plan_f) {
                            //println!("planxx: asymmetric step {}", plan_ret);
                            return Ok(plan_ret);
                        }
                    }
                }
            }
        }

        // Take random forward, or backward, step.
        if 0 == rand::rng().random_range(0..2) {
            let stepx = &from_steps[rand::rng().random_range(0..from_steps.len())];
            if let Ok(plan_t) = self.make_plans2(&stepx.result, goal_reg, within, depth - 1) {
                match SomePlan::new(self.id, vec![stepx.clone()]).link(&plan_t) {
                    Ok(plan_ret) => {
                        //println!("planxx: From step {}", plan_ret);
                        return Ok(plan_ret);
                    }
                    Err(errstr) => return Err(vec![errstr]),
                }
            }
        } else {
            let stepx = &goal_steps[rand::rng().random_range(0..goal_steps.len())];
            if let Ok(plan_f) = self.make_plans2(from_reg, &stepx.initial, within, depth - 1) {
                match plan_f.link(&SomePlan::new(self.id, vec![stepx.clone()])) {
                    Ok(plan_ret) => {
                        //println!("planxx: Goal step {}", plan_ret);
                        return Ok(plan_ret);
                    }
                    Err(errstr) => return Err(vec![errstr]),
                }
            }
        }

        Err(vec!["No plan found".to_string()])
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
    pub fn get_steps(&self, wanted_changes: &SomeChange, within: &SomeRegion) -> StepStore {
        //println!("domain::get_steps: change {} within {within}", wanted_changes);
        debug_assert_eq!(wanted_changes.num_bits(), self.num_bits());
        debug_assert!(within.num_bits() == self.num_bits());

        // Check if changes are possible.

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str: StepStore = self.actions.get_steps(wanted_changes, within);

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

    /// Display anchor rates, like (number adjacent anchors, number other adjacent squares only in one region, samples)
    pub fn display_action_anchor_info(&self, act_id: usize) -> Result<(), String> {
        debug_assert!(act_id < self.actions.len());

        //let max_region = self.reachable_region();
        self.actions[act_id].display_anchor_info()?;

        println!("\nMaximum Region: {}", self.union_all_states);
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

    /// Calc aggregate changes, for SessionData initialization.
    pub fn calc_aggregate_changes(&self) -> Option<SomeChange> {
        self.actions.calc_aggregate_changes()
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
    pub fn number_groups_defined(&self) -> usize {
        let mut tot = 0;
        for actx in self.actions.iter() {
            tot += actx.number_groups_defined();
        }
        tot
    }

    /// Return a String representation of a SomeDomain state.
    fn formatted_state(&self) -> String {
        let mut rc_str = String::from("D(ID: ");

        rc_str.push_str(&self.id.to_string());

        rc_str.push_str(&format!(", Current State: {}", &self.cur_state));

        rc_str.push(')');

        for actx in self.actions.iter() {
            rc_str.push_str(&format!("\n    Action {actx}"));
        }

        rc_str
    }

    /// Return a from_str compatible string for a SomeDomain instance.
    pub fn formatted_def(&self) -> String {
        let mut rc_str = String::from("DOMAIN[");

        // Ad Action defs.
        let mut first = true;
        for actx in self.actions.iter() {
            if first {
                first = false;
            } else {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&actx.formatted_def());
        }
        rc_str.push(']');

        rc_str
    }

    /// Return the number of bits used in a domain.
    pub fn num_bits(&self) -> usize {
        self.cur_state.num_bits()
    }

    /// Return a plan shortcut, or None.
    fn shortcuts(&self, planx: &SomePlan, within: &SomeRegion) -> Option<SomePlan> {
        if planx.len() < 3 {
            return None;
        }
        //println!("shortcuts: plan {planx}");
        let plans = self.shortcuts2(planx, 5, within);

        if let Some(mut plans2) = plans {
            Some(plans2.remove(rand::rng().random_range(0..plans2.len())))
        } else {
            None
        }
    }

    /// Try to get multiple plan shortcuts.
    /// Return a plan shortcut, or None.
    fn shortcuts2(&self, planx: &SomePlan, depth: usize, within: &SomeRegion) -> Option<PlanStore> {
        //println!("shortcuts2: plan {planx} depth {depth}");
        if depth == 0 {
            return None;
        }
        if let Some(plany) = self.shortcuts3(planx, within) {
            let mut inx = 0;
            if plany.len() > 1 {
                inx = rand::rng().random_range(0..plany.len());
            }
            if plany[inx].len() < planx.len() {
                return self.shortcuts2(&plany[inx], depth - 1, within);
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
            if let Ok(plany) = self.make_plans2(
                &planx[*from_inx].initial,
                &planx[*to_inx].result,
                within,
                10,
            ) {
                //println!(
                //       "    plans found from {} to {}",
                //    planx[*from_inx].initial, &planx[*to_inx].result
                //);
                //println!("    Plans found 2");
                //println!("    sub plan1 {}", plany);
                let mut new_plan = SomePlan::new(self.id, vec![]);
                if *from_inx > 0 {
                    for inz in 0..*from_inx {
                        new_plan =
                            match new_plan.link(&SomePlan::new(self.id, vec![planx[inz].clone()])) {
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
                        new_plan =
                            match new_plan.link(&SomePlan::new(self.id, vec![planx[inz].clone()])) {
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

    /// Run cleanup for a  action.
    pub fn cleanup(&mut self, act_id: usize, needs: &NeedStore) {
        assert!(act_id < self.actions.len());
        self.actions[act_id].cleanup(needs);
    }

    /// Find an action that matches a given ID, return a reference.
    pub fn find_action(&self, act_id: usize) -> Option<&SomeAction> {
        if act_id >= self.actions.len() {
            None
        } else {
            Some(&self.actions[act_id])
        }
    }

    /// Find an action that matches a given ID, return a reference.
    pub fn find_action_mut(&mut self, act_id: usize) -> Option<&mut SomeAction> {
        if act_id >= self.actions.len() {
            None
        } else {
            Some(&mut self.actions[act_id])
        }
    }

    /// Return the number of actions.
    pub fn number_actions(&self) -> usize {
        self.actions.len()
    }
} // end impl SomeDomain

impl FromStr for SomeDomain {
    type Err = String;
    /// Return a SomeDomain instance, given a string representation.
    ///
    /// "DOMAIN[ ACT[[XX/XX/XX/Xx], ACT[XX/XX/Xx/XX]], ACT[[XX/Xx/XX/XX]], ACT[[Xx/XX/XX/XX]]]"
    ///
    /// All the rules must use the same number of bits, initial state will be set to a random value.
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("SomeDomain::from_str: {str_in}");
        let str_in2 = str_in.trim();

        // Strip off surrounding id and brackets.
        if str_in2.len() < 8 {
            return Err(
                "domain::from_str: string should be at least = DOMAIN[<one action>]".to_string(),
            );
        }

        if str_in2[0..7].to_uppercase() != *"DOMAIN[" {
            return Err("domain::from_str: string should begin with DOMAIN[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("domain::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[7..(str_in2.len() - 1)];

        // Split substring into tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("domain::from_str: {errstr}")),
        };
        //println!("tokens {:?}", tokens);

        let mut act_vec = Vec::<SomeAction>::with_capacity(tokens.len());

        // Push each action.
        for tokenx in tokens.iter() {
            if tokenx.len() > 3 && tokenx[0..4].to_uppercase() == *"ACT[" {
                act_vec.push(SomeAction::from_str(tokenx)?);
            } else if tokenx[0..1] == *"[" { // skip for now.
            } else {
                return Err(format!(
                    "SomeDomain::from_str: Unrecognized token, {tokenx}"
                ));
            }
        }

        let num_bits = act_vec[0].num_bits();

        let mut domx = SomeDomain::new(0, SomeState::new_random(num_bits));

        for actx in act_vec {
            assert!(actx.num_bits() == num_bits);
            domx.push(actx);
        }

        // Check for samples to take.
        for tokenx in tokens.iter() {
            if tokenx.len() > 3 && tokenx[0..4].to_uppercase() == *"ACT[" { // skip
            } else if tokenx[0..1] == *"[" {
                //println!("process token: {}", tokenx);
                // Unwrap brackets.
                let tokeny = &tokenx[1..(tokenx.len() - 1)];
                //print!(" tokeny: {}", tokeny);
                // Split substring into tokens.
                let tokens2 = match tools::parse_input(tokeny) {
                    Ok(tokenvec) => tokenvec,
                    Err(errstr) => return Err(format!("domain::from_str: {errstr}")),
                };
                //print!(" action: {}", tokens2[0]);
                // Translate action number as string to action number as usize.
                let actx = match tokens2[0].parse::<usize>() {
                    Ok(num) => num,
                    Err(errstr) => return Err(errstr.to_string()),
                };
                //print!(" action: {}", actx);

                // Split state token by "/" separator, if any.
                let mut statex = tokens2[1].clone();
                let mut num_str = "1".to_string();
                if let Some(inx) = statex.find('/') {
                    num_str = statex[(inx + 1)..].to_string();
                    statex = statex[0..inx].to_string();
                }

                match SomeState::from_str(&statex) {
                    Ok(stax) => {
                        //print!(" state: {}", stax);
                        match num_str.parse::<usize>() {
                            Ok(num) => {
                                //print!(" num_times: {}", num);
                                domx.take_action_arbitrary_repeat(actx, &stax, num);
                            }
                            Err(errstr) => return Err(errstr.to_string()),
                        }
                    }
                    Err(errstr) => return Err(errstr),
                }
                //println!(" ");
            } else {
                return Err(format!(
                    "SomeDomain::from_str: Unrecognized token, {tokenx}"
                ));
            }
        }
        Ok(domx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    //use crate::domainstore::DomainStore;
    use crate::target::ATarget;
    //use crate::statescorr::StatesCorr;
    use std::str::FromStr;

    // Test running a plan using an alt-rule (change) group.
    #[test]
    fn alt_rule1() -> Result<(), String> {
        // Create a domain that uses four bits.
        let mut dm0 = SomeDomain::from_str(
            "DOMAIN[
            ACT[[00/11/01/11, 00/11/00/10]], [1, s0101/4]
        ]",
        )?;

        let sta_5 = SomeState::from_str("s0101")?;

        let rslt = if let Some(sqrx) = dm0.actions[1].squares.find(&sta_5) {
            sqrx.most_recent_result().clone()
        } else {
            return Err("Square 5 not found".to_string());
        };
        println!("rslt1 {rslt}");

        // Force current result to 0111, so next result will be 0100.
        if rslt == SomeState::from_str("s0100")? {
            dm0.cur_state = sta_5.clone();
            dm0.take_action(1);
        }

        let rslt = if let Some(sqrx) = dm0.actions[1].squares.find(&sta_5) {
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
            &SomeRegion::from_str("r0101")?,
            &SomeRegion::from_str("r0100")?,
            &SomeRegion::from_str("rXXXX")?,
        ) {
            println!("1plans {plans}");
            match dm0.run_plan(&plans[0]) {
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
        let mut dm0 = SomeDomain::from_str(
            "DOMAIN[
            ACT[[00/XX/01/XX]], [1, s0101], [1, s0000],
        ]",
        )?;

        println!("\n(1) dm0: cur_state {}", dm0.cur_state);
        println!("Acts: {}\n", dm0.actions);

        // One of the following plans will succeed as is, one will need to return to square 5 and try again, then it will succeed.
        dm0.cur_state = SomeState::from_str("s0001")?; // -> 0011

        if let Ok(plans) = dm0.make_plans(
            &SomeRegion::from_str("r0001")?,
            &SomeRegion::from_str("r0011")?,
            &SomeRegion::from_str("r00XX")?,
        ) {
            match dm0.run_plan(&plans[0]) {
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
        let domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]],
            [1, s0000], [1, s1111],
            [2, s0000], [2, s1111],
            [3, s0000], [3, s1111],
            [4, s0000], [4, s1111]
        ]",
        )?;

        println!("{domx}");

        // Get plan for 7 to 8
        let from_reg = SomeRegion::from_str("r0111")?;
        let to_reg = SomeRegion::from_str("r1000")?;
        let within = SomeRegion::from_str("rXXXX")?;

        if let Ok(plans) = domx.make_plans(&from_reg, &to_reg, &within) {
            println!("plans {plans}");
        } else {
            return Err(String::from("No plan found to r1000?"));
        }
        Ok(())
    }

    // Test action:get_needs StateNotInGroup, two flavors.
    #[test]
    fn need_for_state_not_in_group() -> Result<(), String> {
        // Create a domain that uses 4 bits.
        let mut domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/XX/XX/Xx, XX/XX/XX/XX]],
        ]",
        )?;
        domx.set_cur_state(SomeState::from_str("s0001")?);

        // Check need for the current state not in a group.
        let nds1 = domx.actions[1].state_not_in_group_needs(&domx.cur_state);

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(nds1.contains_similar_need(
            "StateNotInGroup",
            &ATarget::State {
                state: SomeState::from_str("s0001")?
            }
        ));

        // Create group for one sampledomain::tests::make_plans_asymmetric
        domx.take_action_arbitrary(1, &SomeState::from_str("s0001")?);

        println!("\nActs: {}", &domx.actions[1]);
        assert!(
            domx.actions[1]
                .groups
                .find(&SomeRegion::from_str("r0001")?)
                .is_some()
        );

        // Invalidate group for sample 1 by giving it GT 1 different result.
        domx.take_action_arbitrary(1, &SomeState::from_str("s0001")?);

        println!("\nActs: {}", domx.actions[1]);

        assert!(
            domx.actions[1]
                .groups
                .find(&SomeRegion::from_str("r0001")?)
                .is_none()
        );

        // Check needs for pn > 1 and not in group, and current state not in a group.
        domx.set_cur_state(SomeState::from_str("s0000")?); // Current state could be s0001 or s0000, so set it to be sure.
        let nds1 = domx.get_needs();
        println!("needs: {}", nds1);

        assert!(nds1.is_not_empty());
        assert!(nds1.contains_similar_need(
            "StateNotInGroup",
            &ATarget::State {
                state: SomeState::from_str("s0000")?
            }
        ));
        Ok(())
    }

    // Test confirm_group_needs.
    #[test]
    fn need_additional_group_state_samples() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/XX/XX/XX]],
        ]",
        )?;
        domx.set_cur_state(SomeState::from_str("s0001")?);

        // Check need for the current state not in a group.
        let nds1 = domx.actions[1].state_not_in_group_needs(&domx.cur_state);

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(nds1.contains_similar_need(
            "StateNotInGroup",
            &ATarget::State {
                state: SomeState::from_str("s0001")?
            }
        ));

        // Create group for one sample
        domx.take_action_arbitrary(1, &SomeState::from_str("s0001")?);

        println!("\nActs: {}", domx.actions[1]);
        assert!(
            domx.actions[1]
                .groups
                .find(&SomeRegion::from_str("r0001")?)
                .is_some()
        );

        // Expand group
        domx.take_action_arbitrary(1, &SomeState::from_str("s0010")?);

        println!("\nActs: {}", domx.actions[1]);
        assert!(
            domx.actions[1]
                .groups
                .find(&SomeRegion::from_str("r00XX")?)
                .is_some()
        );

        let nds2 = domx.actions[1].confirm_group_needs();
        println!("needs {}", nds2);

        assert_eq!(nds2.len(), 2);
        assert!(nds2.contains_similar_need(
            "ConfirmGroup",
            &ATarget::State {
                state: SomeState::from_str("s0001")?
            }
        ));
        assert!(nds2.contains_similar_need(
            "ConfirmGroup",
            &ATarget::State {
                state: SomeState::from_str("s0010")?
            }
        ));

        // Satisfy one need.
        domx.take_action_arbitrary(1, &SomeState::from_str("s0010")?);
        domx.take_action_arbitrary(1, &SomeState::from_str("s0010")?);

        let nds3 = domx.actions[1].confirm_group_needs();
        println!("needs {}", nds3);
        assert_eq!(nds3.len(), 1);
        assert!(nds3.contains_similar_need(
            "ConfirmGroup",
            &ATarget::State {
                state: SomeState::from_str("s0001")?
            }
        ));

        // Satisfy second need.
        domx.take_action_arbitrary(1, &SomeState::from_str("s0001")?);
        domx.take_action_arbitrary(1, &SomeState::from_str("s0001")?);

        let nds4 = domx.actions[1].confirm_group_needs();
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
        let domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/00/XX/Xx], [XX/11/XX/XX]], [1, s0000], [1, s1101], [1, s0110],
        ]",
        )?;

        // Get and check needs.
        let nds1 = domx.actions[1].group_pair_needs();
        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(nds1.contains_similar_need(
            "ContradictoryIntersection",
            &ATarget::Region {
                region: SomeRegion::from_str("rX100")?
            }
        ));

        Ok(())
    }

    #[test]
    fn limit_group_needs() -> Result<(), String> {
        // Create a domain that uses one integer for bits.
        let mut domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/XX/11/XX], [X0/X0/01/x0]], [1, s0100/3], [1, s1001/3],
        ]",
        )?;

        let max_reg = SomeRegion::from_str("rXXXX")?;

        let nds1 = domx.actions[1].groups_limit_needs(&max_reg);
        println!("domx {}", domx.actions[1]);
        println!("Needs: {}", nds1);

        let grp_reg = SomeRegion::from_str("rXX0X")?;
        let Some(anchor_sta) = &domx.actions[1]
            .groups
            .find(&grp_reg)
            .as_ref()
            .expect("SNH")
            .anchor
        else {
            return Err("limit_groups_needs anchor not set".to_string());
        };

        println!("anchor is {}", anchor_sta);

        if anchor_sta.pinnacle == SomeState::from_str("s1001")? {
            // limiting square for anchor 9 is B.
            domx.take_action_arbitrary(1, &SomeState::from_str("s1011")?);
            domx.take_action_arbitrary(1, &SomeState::from_str("s1011")?);
            domx.take_action_arbitrary(1, &SomeState::from_str("s1011")?);
        } else {
            // Limiting square for anchor 4 is 6.
            domx.take_action_arbitrary(1, &SomeState::from_str("s0110")?);
            domx.take_action_arbitrary(1, &SomeState::from_str("s0110")?);
            domx.take_action_arbitrary(1, &SomeState::from_str("s0110")?);
        }

        println!("domx {}", domx.actions[1]);

        let nds2 = domx.actions[1].groups_limit_needs(&max_reg);

        println!("domx {}", domx.actions[1]);

        println!("needs are {}", nds2);

        let grpx = domx.actions[1].groups.find(&grp_reg).expect("SNH");
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
        let mut domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/XX/00/11, XX/XX/00/10], [11/XX/XX/11, 11/XX/XX/10], [00/XX/11/XX]], [1, s0101/4], [1, s1111/4],
        ]",
        )?;

        let rx1x1 = SomeRegion::from_str("rx1x1")?;

        println!("\nActs: {}", domx.actions[1]);

        if let Some(_regx) = domx.actions[1].groups.find(&rx1x1) {
            println!("\nActs: {}", domx.actions[1]);
        } else {
            return Err(String::from("Group rx1x1 was not formed by two squares?"));
        }

        domx.take_action_arbitrary(1, &SomeState::from_str("s0111")?);
        if let Some(_regx) = domx.actions[1].groups.find(&rx1x1) {
        } else {
            return Err(String::from("Group rx1x1 deleted too soon?"));
        }

        domx.take_action_arbitrary(1, &SomeState::from_str("s0111")?);
        if let Some(_regx) = domx.actions[1].groups.find(&rx1x1) {
        } else {
            return Err(String::from("Group rx1x1 deleted too soon?"));
        }

        domx.take_action_arbitrary(1, &SomeState::from_str("s0111")?);
        if let Some(_regx) = domx.actions[1].groups.find(&rx1x1) {
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
        let mut domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[00/XX/11/XX],
                [XX/XX/00/11, XX/XX/01/10, XX/XX/01/11],
                [11/11/XX/XX, 11/10/XX/XX, 10/11/XX/XX]], [1, s0101/4], [1, s1111/4],
        ]",
        )?;

        let rx1x1 = SomeRegion::from_str("rx1x1")?;

        println!("\n1 Acts: {}", domx.actions[1]);
        assert!(domx.actions[1].groups.find(&rx1x1).is_some());

        domx.take_action_arbitrary(1, &SomeState::from_str("s0111")?);
        println!("\n2 Acts: {}", domx.actions[1]);

        assert!(domx.actions[1].groups.find(&rx1x1).is_some());

        domx.take_action_arbitrary(1, &SomeState::from_str("s0111")?);
        println!("\n2 Acts: {}", domx.actions[1]);

        assert!(domx.actions[1].groups.find(&rx1x1).is_some());

        domx.take_action_arbitrary(1, &SomeState::from_str("s0111")?); // cause pn-not-Two invalidation
        println!("\n3 Acts: {}", domx.actions[1]);

        assert!(domx.actions[1].groups.find(&rx1x1).is_none());

        Ok(())
    } // end group_pn_u_union_then_invalidation

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
        let act1: usize = 1;

        // Create a domain that uses one integer for bits.
        let mut domx = SomeDomain::new(0, SomeState::from_str("s1101")?);
        domx.push(SomeAction::from_str("ACT[[XX/XX/Xx/XX]]")?);

        println!("domx {domx}");
        println!("union_all_states {}", domx.union_all_states);
        //assert!(1 == 2);

        // Start groups.
        let sta_d = SomeState::from_str("s1101")?;
        let sta_f = SomeState::from_str("s1111")?;

        domx.take_action_arbitrary(act1, &sta_d);
        domx.take_action_arbitrary(act1, &sta_f);
        println!("union_all_states {}", domx.union_all_states);

        // Confirm groups.
        domx.take_action_arbitrary(act1, &sta_d);
        domx.take_action_arbitrary(act1, &sta_f);
        domx.take_action_arbitrary(act1, &sta_d);
        domx.take_action_arbitrary(act1, &sta_f);

        // get_needs checks the limited flag for each group.
        let nds = domx.get_needs();
        println!("\n(1){}", domx.actions[act1]);
        println!("union_all_states {}", domx.union_all_states);
        println!("needs {}", nds);

        let grpx = domx.actions[act1]
            .groups
            .find(&SomeRegion::from_str("r11x1")?)
            .expect("SNH");
        assert!(grpx.limited);

        // Get needs for an expanded max_region.

        let max_reg = SomeRegion::from_str("r11XX")?;
        let needs = domx.actions[act1].groups_limit_needs(&max_reg);

        println!("\n(1){}", domx.actions[act1]);

        println!("needs {}", needs);
        assert!(needs.len() == 1);
        assert!(
            needs.contains_similar_need(
                "LimitGroupAdj",
                &ATarget::State {
                    state: SomeState::from_str("s1110")? // anchor is s1111
                }
            ) || needs.contains_similar_need(
                "LimitGroupAdj",
                &ATarget::State {
                    state: SomeState::from_str("s1100")? // anchor is s1101.
                }
            )
        );

        Ok(())
    }

    #[test]
    fn shortcuts3() -> Result<(), String> {
        // Create a domain that uses 4 bits.
        let domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]],
            [1, s0000], [1, s1111],
            [2, s0000], [2, s1111],
            [3, s0000], [3, s1111],
            [4, s0000], [4, s1111]
        ]",
        )?;

        println!("Acts: {}\n", domx.actions);

        let pln1 = SomePlan::from_str("P[0, r0000-2->r0100-3->r1100-2->r1000]")?;
        println!("pln1: {}", pln1);

        if let Some(shrt) = domx.shortcuts(&pln1, &SomeRegion::from_str("rXXXX")?) {
            println!("shrt {shrt}");
            assert!(shrt.len() == 1);
            assert!(shrt[0].initial == SomeRegion::from_str("r0000")?);
            assert!(shrt[0].result == SomeRegion::from_str("r1000")?);

            return Ok(());
        }
        Err(format!("Shortcuts not found?"))
    }

    #[test]
    fn shortcuts5() -> Result<(), String> {
        // Create a domain that uses 4 bits.
        let domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]],
            [1, s0000], [1, s1111],
            [2, s0000], [2, s1111],
            [3, s0000], [3, s1111],
            [4, s0000], [4, s1111]
        ]",
        )?;

        println!("Acts: {}\n", domx.actions);

        let pln1 = SomePlan::from_str(
            "P[0, r0100-1->r0101-3->r0111-2->r0110-4->r0010-2->r0011-3->r1011]",
        )?;
        println!("pln1: {}", pln1);

        if let Some(shrt) = domx.shortcuts(&pln1, &SomeRegion::from_str("rXXXX")?) {
            // Check shortcuts.
            println!("shrt: {shrt}");

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
        let domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]],
            [1, s0000], [1, s1111],
            [2, s0000], [2, s1111],
            [3, s0000], [3, s1111],
            [4, s0000], [4, s1111]
        ]",
        )?;

        println!("Acts: {}\n", domx.actions);

        let pln1 = SomePlan::from_str("P[0, r1011-1->r1111-2->r1110-1->r0110-23->r0111]")?;
        println!("pln1: {}", pln1);

        if let Some(shrt) = domx.shortcuts(&pln1, &SomeRegion::from_str("rXXXX")?) {
            // Check shortcuts.
            println!("shrt: {shrt}");

            assert!(shrt.len() == 2);
            assert!(shrt.initial_region() == pln1.initial_region());
            assert!(shrt.result_region() == pln1.result_region());
        } else {
            return Err("No shortcuts found".to_string());
        }

        Ok(())
    }

    #[test]
    fn shortcuts7() -> Result<(), String> {
        // Create a domain that uses 4 bits.
        let domx = SomeDomain::from_str(
            "DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]],
            [1, s0000], [1, s1111],
            [2, s0000], [2, s1111],
            [3, s0000], [3, s1111],
            [4, s0000], [4, s1111]
        ]",
        )?;

        println!("Acts: {}\n", domx.actions);

        let reg_0 = SomeRegion::from_str("r0000")?;
        let reg_f = SomeRegion::from_str("r1111")?;

        let pln1 = SomePlan::from_str(
            "P[0, r0000-1->r0001-2->r0011-1->r0010-3->r0110-3->r0111-3->r0101-3->r1101-3->r1111]",
        )?;
        println!("pln1: {}", pln1);

        if let Some(plnx) = domx.shortcuts(&pln1, &SomeRegion::from_str("rXXXX")?) {
            // Check shortcut.
            println!("plnx: {plnx}");

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

    #[test]
    /// Test domain definition from string to instance, then instance to string(2), then string(2) to instance.
    fn from_str() -> Result<(), String> {
        let domx_str = "DOMAIN[ ACT[[XX/XX/XX/00, XX/XX/Xx/01], [Xx/XX/XX/11]],
                                ACT[[XX/Xx/XX/XX]],
                                ACT[[Xx/XX/XX/XX]]]";
        println!("domx_str {domx_str}");

        let domx = SomeDomain::from_str(&domx_str)?; // String to instance.

        if domx.actions.len() != 4 {
            return Err("s/b 4 actions".to_string());
        }
        Ok(())
    }
} // end tests
