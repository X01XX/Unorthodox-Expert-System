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
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::step::SomeStep;
use crate::stepstore::StepStore;
use crate::tools;

use rand::Rng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fmt;
use std::str::FromStr;

impl fmt::Display for SomeDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

const MAX_MEMORY: usize = 20; // Max number of recent current states to keep in a circular buffer.

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
    /// A counter to indicate the number of steps the current state is in the same optimal region
    /// before getting bored.
    pub memory: VecDeque<SomeSample>,
    /// Region used for propagation.
    tmp_reg: SomeRegion,
}

impl SomeDomain {
    /// Return a new domain instance, given the number of integers, the
    /// initial state, the optimal state(s), the index into the higher-level DomainStore.
    pub fn new(num_ints: usize) -> Self {
        debug_assert_ne!(num_ints, 0);
        // Set up a domain instance with the correct value for num_ints
        let tmp_sta = SomeState::new(SomeBits::new(vec![0; num_ints]));
        let cur_state = tmp_sta.new_random();
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);
        Self {
            num: 0, // May be changed later
            actions: ActionStore::new(
                vec![],
                SomeChange::new(tmp_sta.to_mask(), tmp_sta.to_mask()),
            ),
            cur_state,
            memory: VecDeque::<SomeSample>::with_capacity(MAX_MEMORY),
            tmp_reg,
        }
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
        self.actions
            .get_needs(&self.cur_state, self.num, &self.memory)
    }

    /// Evaluate an arbitrary sample given by the user.
    /// This tends to break things for an action, unless all samples are arbitrary.
    /// Useful for testing a wholly different series of samples/results.
    /// Using the command: ss  action-number  initial-state  result-state
    /// e.g. ss  0  s0b1010  s0b1111
    pub fn eval_sample_arbitrary(&mut self, smpl: &SomeSample) {
        self.actions[smpl.act_num].eval_sample_arbitrary(smpl);
        self.set_state_memory(smpl.clone());
    }

    /// Take an action for a need, evaluate the resulting sample.
    /// It is assumed that a sample made for a need must be saved.
    pub fn take_action_need(&mut self, ndx: &SomeNeed) {
        let asample = self.actions[ndx.act_num()].take_action_need(&self.cur_state, ndx);

        self.set_state_memory(asample);
    }

    /// Take an action with the current state.
    pub fn take_action_arbitrary(&mut self, act_num: usize) {
        let asample = self.actions[act_num].take_action_arbitrary(&self.cur_state);

        self.set_state_memory(asample);
    }

    /// Set the current state field.
    pub fn set_state_memory(&mut self, asample: SomeSample) {
        let new_state = asample.result.clone();

        // Save sample in memory.
        if self.memory.len() >= MAX_MEMORY {
            self.memory.pop_back();
        }
        self.memory.push_front(asample);

        // Set current state.
        self.cur_state = new_state;
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
            //return false;
        }

        for stpx in pln.iter() {
            let asample = self.actions[stpx.act_num].take_action_step(&self.cur_state);

            let prev_state = self.cur_state.clone();

            self.set_state_memory(asample);

            if stpx.result.is_superset_of_state(&self.cur_state) {
                continue;
            }

            // Handle unexpected/unwanted result
            // May be an expected possibility from a two result state.
            if prev_state == self.cur_state && stpx.alt_rule {
                println!("Try action a second time");

                let asample = self.actions[stpx.act_num].take_action_step(&self.cur_state);

                self.set_state_memory(asample);

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

        let steps_str = self.get_steps(&required_change)?;

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

        self.make_plans2(&cur_reg, goal_reg)
    }

    /// Make a plan to change from a region to another region.
    pub fn make_plans2(
        &self,
        from_reg: &SomeRegion,
        goal_reg: &SomeRegion,
    ) -> Option<Vec<SomePlan>> {
        //println!("dom {} make_plans: from {from_reg} goal {goal_reg}", self.num);
        // Figure the required change.
        let required_change = SomeChange::region_to_region(from_reg, goal_reg);

        // Tune maximum depth to be a multiple of the number of bit changes required.
        let num_depth = 4 * required_change.number_changes();

        // Get steps, check if steps include all changes needed.
        let steps_str = self.get_steps(&required_change)?;

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
    fn get_steps(&self, required_change: &SomeChange) -> Option<StepStore> {
        // Check if changes are possible.

        // Get a vector of steps (from rules) that make part of the needed changes.
        let steps_str: StepStore = self.actions.get_steps(required_change);

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
        self.tmp_reg.new_from_string(str)
    } // end region_from_string

    /// Return a SomeRegion instance from a string.
    /// Left-most, consecutive, ommitted zeros are assumed tobe X.
    pub fn region_from_string_pad_x(&self, str: &str) -> Result<SomeRegion, String> {
        self.tmp_reg.new_from_string_pad_x(str)
    }

    /// Return a SomeState instance from a string.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn state_from_string(&self, str: &str) -> Result<SomeState, String> {
        self.cur_state.new_from_string(str)
    }

    /// Return a maximum region for a domain.
    pub fn maximum_region(&self) -> SomeRegion {
        SomeRegion::new(vec![self.cur_state.new_high(), self.cur_state.new_low()])
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
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::from("D(ID: ");

        rc_str.push_str(&self.num.to_string());

        rc_str.push_str(&format!(", Current State: {}", &self.cur_state));

        rc_str.push(')');

        rc_str
    }
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
        dm0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s0.change_bit(0)));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, sf.change_bit(0)));

        // Create group for region XXXX, Act 1.
        dm0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s0.change_bit(1)));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sf.change_bit(1)));

        // Create group for region XXXX, Act 2.
        dm0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s0.change_bit(2)));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sf.change_bit(2)));

        // Create group for region XXXX, Act 3.
        dm0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s0.change_bit(3)));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 3, sf.change_bit(3))); // Last sample changes current state to s0111

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
        dm0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s0.change_bit(0)));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, sf.change_bit(0)));

        // Create group for region XXXX->XXxX, Act 1.
        dm0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s0.change_bit(1)));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sf.change_bit(1)));

        // Create group for region XXXX-XxXX, Act 2.
        dm0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s0.change_bit(2)));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sf.change_bit(2)));

        // Create group for region X0XX->x0XX, Act 3.
        dm0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s0.change_bit(3)));
        dm0.eval_sample_arbitrary(&SomeSample::new(sb.clone(), 3, sb.change_bit(3)));

        println!("\nActs: {}", &dm0.actions);

        // Glide Path is 7 + C = X1XX

        // Get plan for 7 to C
        // One bit that has to change, bit 3, 0...->1..., needs to use Act 3, 00XX->10XX,
        // which is outside of the Glide Path.
        let s7 = dm0.state_from_string("s0x07")?;
        dm0.set_state(&s7);
        let toreg = dm0.region_from_string("r1100")?;

        if let Some(plans) = &mut dmxs.get_plans(0, &toreg) {
            println!("plan: {}", SomePlan::vec_string(&plans));
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
        let nds1 = dm0.actions.avec[0]
            .state_not_in_group_needs(&dm0.cur_state, &VecDeque::<SomeSample>::new());

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &dm0.region_from_string("r1")?
        ));

        // Create group for one sample
        let s1 = dm0.state_from_string("s0b1")?;
        dm0.eval_sample_arbitrary(&SomeSample::new(s1.clone(), 0, s1.clone()));

        println!("\nActs: {}", &dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1")?)
            .is_some());

        // Invalidate group for sample 1 by giving it GT 1 different result.
        // Current state changes to zero.
        let s1 = dm0.state_from_string("s0b1")?;
        dm0.eval_sample_arbitrary(&SomeSample::new(
            s1.clone(),
            0,
            dm0.state_from_string("s0")?,
        ));

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
            &dm0.region_from_string("r0")?
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
        let nds1 = dm0.actions.avec[0]
            .state_not_in_group_needs(&dm0.cur_state, &VecDeque::<SomeSample>::new());

        println!("Needs: {nds1}");
        assert_eq!(nds1.len(), 1);
        assert!(contains_similar_need(
            &nds1,
            "StateNotInGroup",
            &dm0.region_from_string("r1")?
        ));

        // Create group for one sample
        let s1 = dm0.state_from_string("s0b1")?;
        dm0.eval_sample_arbitrary(&SomeSample::new(s1.clone(), 0, s1.clone()));

        println!("\nActs: {}", &dm0.actions[0]);
        assert!(dm0.actions[0]
            .groups
            .find(&dm0.region_from_string("r1")?)
            .is_some());

        // Expand group
        let s2 = dm0.state_from_string("s0b10")?;
        dm0.eval_sample_arbitrary(&SomeSample::new(s2.clone(), 0, s2.clone()));

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
        dm0.eval_sample_arbitrary(&SomeSample::new(s2.clone(), 0, s2.clone()));

        let nds3 = dm0.actions[0].confirm_group_needs();
        println!("needs {}", nds3);
        assert_eq!(nds3.len(), 1);
        assert!(contains_similar_need(
            &nds3,
            "ConfirmGroup",
            &dm0.region_from_string("r1")?
        ));

        // Satisfy second need.
        dm0.eval_sample_arbitrary(&SomeSample::new(s1.clone(), 0, s1.clone()));

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
        dm0.eval_sample_arbitrary(&SomeSample::new(s00.clone(), 0, s01.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s00.clone(), 0, s01.clone()));

        dm0.eval_sample_arbitrary(&SomeSample::new(s0d.clone(), 0, s0d.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s0d.clone(), 0, s0d.clone()));

        // Create group X1XX
        dm0.eval_sample_arbitrary(&SomeSample::new(s06.clone(), 0, s06.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s06.clone(), 0, s06.clone()));

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
        dm0.eval_sample_arbitrary(&SomeSample::new(s04.clone(), 0, s04.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s04.clone(), 0, s04.clone()));

        let sf9 = dm0.state_from_string("s0b11111001")?;
        dm0.eval_sample_arbitrary(&SomeSample::new(sf9.clone(), 0, sf9.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf9.clone(), 0, sf9.clone()));

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
        dm0.eval_sample_arbitrary(&SomeSample::new(s06.clone(), 0, s02.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s06.clone(), 0, s02.clone()));

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

        dm0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), 0, s5.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), 0, s4.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), 0, s5.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), 0, s4.clone()));

        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, sf.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, sf.clone()));

        println!("\nActs: {}", &dm0.actions[0]);

        if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
            dm0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), 0, s7.clone()));
            println!("\nActs: {}", &dm0.actions[0]);

            if let Some(_regx) = dm0.actions[0].groups.find(&rx1x1) {
                dm0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), 0, s7.clone())); // pn=1, pnc condition
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

        dm0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), 0, s5.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), 0, s4.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s5.clone(), 0, se.clone()));

        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, sf.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, s4.clone()));

        println!("\n1 Acts: {}", &dm0.actions[0]);
        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), 0, s7.clone()));
        println!("\n2 Acts: {}", &dm0.actions[0]);

        assert!(dm0.actions[0].groups.find(&rx1x1).is_some());

        dm0.eval_sample_arbitrary(&SomeSample::new(s7.clone(), 0, s7.clone())); // cause pn-not-Two invalidation
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
        dm0.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s0.change_bit(4)));
        dm0.eval_sample_arbitrary(&SomeSample::new(s1.clone(), 0, s1.change_bit(4)));

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
        dm0.eval_sample_arbitrary(&SomeSample::new(s0b.clone(), act0, s0b.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s05.clone(), act0, s05.clone()));

        // Confirm group.
        dm0.eval_sample_arbitrary(&SomeSample::new(s0b.clone(), act0, s0b.clone()));
        dm0.eval_sample_arbitrary(&SomeSample::new(s05.clone(), act0, s05.clone()));

        // get_needs checks the limited flag for each group.
        let nds = dm0.get_needs();
        println!("\n{}", dm0.actions[act0]);
        println!("needs {}", nds);

        // Limited flag should be true.
        assert!(dm0.actions[act0].groups[0].limited);

        // Add a way to change bit position 1, 0->1.
        let s10 = dm0.state_from_string("s0x10")?;
        let s12 = dm0.state_from_string("s0x12")?;
        dm0.eval_sample_arbitrary(&SomeSample::new(s10.clone(), act0, s12.clone()));

        let nds = dm0.get_needs();
        println!("\n{}", dm0.actions[act0]);
        println!("needs {}", nds);

        // Changing bit position 1 should not affect the limited flag,
        // where the group bit position one is X.
        assert!(dm0.actions[act0].groups[0].limited);

        // Add a way to change bit position 0, 0->1.
        let s21 = dm0.state_from_string("s0x21")?;
        let s20 = dm0.state_from_string("s0x20")?;
        dm0.eval_sample_arbitrary(&SomeSample::new(s21.clone(), act0, s20.clone()));

        // Add a way to change bit position 0, 1->0.
        dm0.eval_sample_arbitrary(&SomeSample::new(s20.clone(), act0, s21.clone()));

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
