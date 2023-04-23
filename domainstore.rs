//! The DomainStore struct, a vector of SomeDomain structs.

use crate::change::SomeChange;
use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::removeunordered;
use crate::selectregionsstore::{SelectRegions, SelectRegionsStore};
use crate::state::SomeState;
use crate::targetstore::TargetStore;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;
use std::ops::{Index, IndexMut};

use rayon::prelude::*;

impl fmt::Display for DomainStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("[");

        for (inx, mskx) in self.avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &mskx));
        }
        rc_str.push(']');

        write!(f, "{rc_str}")
    }
}

/// An InxPlan struct, containing an index to a SomeNeed vector, and a SomePlan struct.
///
/// An integer is used instead of &SomeNeed to avoid borrow checker problems.
#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct InxPlan {
    /// Index to a need in a NeedStore.
    pub inx: usize,
    /// Plan to satisfy need, may be empty if the current state satisfies the need, or None.
    pub plans: Option<PlanStore>,
    /// Rate based on the select regions a plan passes through.
    pub rate: isize,
    /// Order to run plans, is any.  Otherwise, run in parallel.
    pub order: Option<Vec<usize>>,
}

#[readonly::make]
#[derive(Serialize, Deserialize, Default)]
pub struct DomainStore {
    /// Vector of SomeDomain structs.
    pub avec: Vec<SomeDomain>,
    /// Domain displayed to user.
    pub current_domain: usize,
    /// A counter to indicate the number of steps the current state is in the same select region.
    pub boredom: usize,
    /// A limit for becomming bored, then moving to another select state.
    pub boredom_limit: usize,
    /// Zero, or more, select regions that are sought if there are no needs.
    /// This may be changed from the UI, see the help display for the commands "oa" and "od".
    /// If more than one region, boredom may cause the program to run rules to switch to a different region.
    pub select: SelectRegionsStore,
    /// RegionStore to add all possible intersections of the select states to discern.
    pub select_and_ints: SelectRegionsStore,
    /// Save the results of the last run of get_needs.
    pub needs: NeedStore,
    /// Vector of InxPlans for selected needs, where a plan was calculated.
    pub can_do: Vec<InxPlan>,
    /// Vector of indicies for selected needs, where a plan could not be calculated.
    pub cant_do: Vec<usize>,
    /// The current step number.
    pub step_num: usize,
}

impl DomainStore {
    /// Return a new, empty, DomainStore struct.
    pub fn new(mut avec: Vec<SomeDomain>) -> Self {
        for (inx, domx) in avec.iter_mut().enumerate() {
            domx.set_domain_num(inx);
        }
        Self {
            avec,
            current_domain: 0,
            boredom: 0,
            boredom_limit: 0,
            select_and_ints: SelectRegionsStore::new(vec![]),
            select: SelectRegionsStore::new(vec![]),
            needs: NeedStore::new(vec![]),
            can_do: Vec::<InxPlan>::new(),
            cant_do: Vec::<usize>::new(),
            step_num: 0,
        }
    }

    /// Add an select region.
    /// One region for each domain.
    /// The logical "and" of each domain region given.
    pub fn add_select(&mut self, regstr: RegionStore, value: isize) {
        debug_assert!(regstr.len() == self.avec.len());

        if self.select.any_supersets_of(&regstr) {
            println!("Superset select regions found");
            return;
        }

        for (inx, dmx) in self.avec.iter().enumerate() {
            if regstr[inx].state1.num_ints() != dmx.cur_state.num_ints() {
                panic!("reg {} bad number ints for domain {}", regstr[inx], inx);
            }
        }

        self.select.push(regstr, value);
    }

    /// Calculate parts of select regions, in case of any overlapps.
    pub fn calc_select(&mut self) {
        // Calc any subregions due to intersecitons.
        if self.select.len() == 1 {
            self.select_and_ints = self.select.clone();
        } else {
            self.select_and_ints = self.select.split_by_intersections();
        }
    }

    /// Add a Domain struct to the store.
    /// Add select regions after the last domain has been added.
    pub fn push(&mut self, mut domx: SomeDomain) -> usize {
        debug_assert!(self.select.is_empty());

        let dom_num = self.avec.len();

        domx.set_domain_num(dom_num);

        self.avec.push(domx);

        dom_num
    }

    /// Get needs for each Domain.
    /// Run in parallel per Domain.
    /// Each Domain uses parallel processing to get needs for each Action.
    ///  plans.
    /// Set DomainStore fields with need info.
    pub fn get_needs(&mut self) {
        // Inc step number.
        self.step_num += 1;

        // Get all needs.
        let mut vecx: Vec<SomeNeed> = self
            .avec
            .par_iter_mut() // .par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|domx| domx.get_needs().avec)
            .flatten()
            .collect::<Vec<SomeNeed>>();

        // Get select region needs.
        if let Some(mut needs) = self.check_select() {
            vecx.append(&mut needs.avec);
        }

        // Sort needs by ascending priority, and store.
        vecx.sort_by_key(|ndx| ndx.priority());
        self.needs = NeedStore::new(vecx);

        self.evaluate_needs();
    }

    /// Run a vector of plans.
    /// The vector may have one plan, or a plan for each domain, some of which may be empty.
    pub fn run_plans(&mut self, plans: &PlanStore, order: &Option<Vec<usize>>) -> bool {
        assert!(plans.is_not_empty());

        if plans.len() == 1 {
            return self.avec[plans[0].dom_num].run_plan(&plans[0]);
        }

        // Check if any plan never passes though a negative (domain restricted) select region.
        if order.is_none() {
            // Run plans in parallel.
            if self
                .avec
                .par_iter_mut() // .par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
                .map(|domx| domx.run_plan(&plans[domx.num]))
                .filter(|b| *b) // filter out any false returns.
                .collect::<Vec<bool>>()
                .len()
                == plans.len()
            // Does the number of true returns equal the number of plans run?
            {
                return true;
            }
            return false;
        }

        // Run plans in order.
        if let Some(order_vec) = order {
            for itemx in order_vec.iter() {
                if !self.run_plan(&plans[*itemx]) {
                    return false;
                }
            }
        } else {
            panic!("This should not happen");
        }
        true
    }

    /// Run a plan for a given Domain.
    /// Return true if the plan ran to completion.
    pub fn run_plan(&mut self, pln: &SomePlan) -> bool {
        self.avec[pln.dom_num].run_plan(pln)
    }

    /// Take an action to satisfy a need
    pub fn take_action_need(&mut self, nd_inx: usize) {
        self.avec[self.needs[nd_inx].dom_num()].take_action_need(&self.needs[nd_inx]);
    }

    /// Take an arbitrary action
    pub fn take_action_arbitrary(&mut self, dmxi: usize, actx: usize) {
        self.avec[dmxi].take_action_arbitrary(actx);
    }

    /// Return a reference to the current state of a given Domain index
    pub fn cur_state(&self, dmxi: usize) -> &SomeState {
        self.avec[dmxi].get_current_state()
    }
    /// Set can_do, and cant_do, struct fields for the DomainStore needs, which are sorted in ascending priority number order.
    /// Scan successive slices of needs, of the same priority, until one, or more, needs can be planned.
    pub fn evaluate_needs(&mut self) {
        //println!("evaluate_needs: {} needs", self.needs.len());

        // Init self need can/can't do vectors.
        self.can_do = Vec::<InxPlan>::new();
        self.cant_do = Vec::<usize>::new();

        if self.needs.is_empty() {
            println!("\nNumber needs: 0");
            return;
        }

        let mut cur_pri = self.needs[0].priority();
        let mut count = 0;
        print!("\nNumber needs: {}, priority(count): ", self.needs.len());
        for needx in self.needs.iter() {
            if needx.priority() > cur_pri {
                print!("{}({}) ", cur_pri, count);
                cur_pri = needx.priority();
                count = 0;
            }
            count += 1;
        }
        println!("{}({}) ", cur_pri, count);

        // Get select region info.
        let in_select = self
            .select
            .any_supersets_of_states(&self.all_current_states());

        let select_priority = if in_select && self.boredom < self.boredom_limit {
            SomeNeed::ToSelectRegion {
                target_regions: SelectRegions {
                    regions: RegionStore::new(vec![]),
                    value: 0,
                },
            }
            .priority()
        } else {
            usize::MAX
        };

        // Init current priority value and start index.
        let mut cur_pri = self.needs[0].priority();

        let mut cur_pri_start = 0;

        // Find current priority end index.
        let mut cur_pri_end = cur_pri_start;
        let needs_len = self.needs.len();

        // Scan successive slices of items with the same priority.
        loop {
            // Find end of current slice.
            while cur_pri_end < needs_len && self.needs[cur_pri_end].priority() == cur_pri {
                cur_pri_end += 1;
                continue;
            }
            // Process a priority slice.
            print!(
                "Priority {}, slice: {}..{}",
                cur_pri, cur_pri_start, cur_pri_end,
            );

            // Test all slice needs for plans.
            let mut ndsinx_plan = (cur_pri_start..cur_pri_end)
                .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
                .map(|nd_inx| (nd_inx, self.make_plans(&self.needs[nd_inx].target())))
                .map(|plnstr| InxPlan {
                    inx: plnstr.0,
                    plans: plnstr.1,
                    rate: 0,
                    order: None,
                })
                .collect::<Vec<InxPlan>>();

            let mut can_do = Vec::<InxPlan>::new();
            let mut cant_do = Vec::<usize>::new();

            // If at least one plan found, return vector of InxPlan structs.
            for ndsinx in ndsinx_plan.iter_mut() {
                if let Some(aplan) = &ndsinx.plans {
                    (ndsinx.rate, ndsinx.order) = self.rate_plans(aplan);
                    can_do.push(ndsinx.clone());
                } else {
                    cant_do.push(ndsinx.inx);
                }
            }

            if !can_do.is_empty() {
                if can_do.len() == 1 {
                    println!(", found 1 need that can be done.");
                } else {
                    println!(", found {} needs that can be done.", can_do.len());
                }
                self.can_do = can_do;
                self.cant_do = cant_do;
                return;
            }
            println!(" ");

            if cur_pri_end == needs_len || cur_pri > select_priority {
                return;
            }

            cur_pri_start = cur_pri_end;
            cur_pri = self.needs[cur_pri_start].priority();
            cur_pri_end = cur_pri_start;
        } // End loop
          // Unreachable, since there is no break command.
    } // end evaluate_needs

    /// Return an Option PlanStore, to go from the current state to the region of each target.
    /// Return None if any one of the targets cannot be satisfied.
    pub fn make_plans(&self, targets: &TargetStore) -> Option<PlanStore> {
        let mut plans = Vec::<SomePlan>::new();

        for targx in targets.iter() {
            if let Some(planx) = self.get_plan(targx.dom_num, &targx.region) {
                plans.push(planx);
            }
        }
        if plans.is_empty() || plans.len() < targets.len() {
            return None;
        }
        Some(PlanStore::new(plans))
    }

    /// Return a rate for a plan, based on the sum of values of select regions the plan passes through.
    fn rate_plans(&self, aplan: &PlanStore) -> (isize, Option<Vec<usize>>) {
        self.select.rate_plans(aplan, &self.all_current_states())
    }

    /// Return the sum of all negative select regions a plan passes though, restricted to the domain number
    /// part of select regions.
    #[allow(dead_code)]
    fn number_negative_regions(&self, aplan: &SomePlan) -> usize {
        self.select.number_negative_regions(aplan)
    }

    /// Get plans to move to a goal region, choose a plan.
    pub fn get_plan(&self, dom_num: usize, goal_region: &SomeRegion) -> Option<SomePlan> {
        if let Some(mut plans) = self.avec[dom_num].make_plans(goal_region) {
            if plans.len() == 1 {
                return Some(plans.remove(0));
            }
            return Some(plans.remove(self.choose_a_plan(&plans)));
        }
        None
    }

    /// Choose a plan from a vector of plans, for a need.
    /// Return index of plan chosen.
    pub fn choose_a_plan(&self, plans: &[SomePlan]) -> usize {
        assert!(!plans.is_empty());

        // No choice to be made.
        if plans.len() == 1 {
            return 0;
        }

        // Gather plan rate data.
        let mut rates = Vec::<isize>::with_capacity(plans.len());
        let current_states = self.all_current_states();

        for planx in plans.iter() {
            rates.push(self.select.rate_plan(planx, &current_states));
        }
        let max_rate = rates.iter().max().unwrap();

        // Find plans with the max rate.
        let mut max_rate_plans = Vec::<usize>::new();
        for (inx, rate) in rates.iter().enumerate() {
            if rate == max_rate {
                max_rate_plans.push(inx);
            }
        }

        if max_rate_plans.len() == 1 {
            return max_rate_plans[0];
        }

        // Gather length data.
        let mut lengths = Vec::<usize>::new();
        for inx in 0..max_rate_plans.len() {
            lengths.push(plans[max_rate_plans[inx]].len());
        }
        let min_len = lengths.iter().min().unwrap();

        // Find plans with the min length.
        let mut min_len_plans = Vec::<usize>::new();
        for (inx, lenx) in lengths.iter().enumerate() {
            if lenx == min_len {
                min_len_plans.push(max_rate_plans[inx]);
            }
        }

        if min_len_plans.len() == 1 {
            return min_len_plans[0];
        }

        min_len_plans[rand::thread_rng().gen_range(0..min_len_plans.len())]
    }

    /// Choose a need, given a vector of needs,
    /// a vector of InxPlans Vec::<{ inx: need vector index, pln: Some(plan}>
    /// at least one do
    ///
    /// Sort needs by priority.
    ///
    /// Scan needs, by priority, to see what need can be satisfied by a plan.
    ///
    /// Return an index to the can_do vector.
    pub fn choose_need(&self) -> usize {
        assert!(!self.can_do.is_empty());

        //println!("choose_need: number InxPlans {}", can_do.len());

        // Find least negative plan rate.
        let mut max_rate: isize = std::isize::MIN;
        for inx_planx in self.can_do.iter() {
            if inx_planx.rate > max_rate {
                max_rate = inx_planx.rate;
            }
        }

        // Make selection of max_rate plans.
        let mut max_rate_inxplans = Vec::<usize>::new();

        // Push index to max rate plans.
        for (inx, inx_planx) in self.can_do.iter().enumerate() {
            if inx_planx.rate == max_rate {
                max_rate_inxplans.push(inx);
            }
        }

        //println!("max rate {max_rate}");
        if max_rate_inxplans.len() < self.can_do.len() {
            //println!("skipped low rate plans");
        }

        // Make selection of min_len plans.
        let mut min_len_inxplans = Vec::<usize>::new();

        // Find the shortest plan length
        let mut min_plan_len = std::usize::MAX;
        for inx in &max_rate_inxplans {
            if let Some(plans) = &self.can_do[*inx].plans {
                if plans.number_steps() < min_plan_len {
                    min_plan_len = plans.number_steps();
                }
            }
        }
        //println!("min len = {}", min_plan_len);

        // Push index to shortest plan needs
        for inx in &max_rate_inxplans {
            if let Some(plans) = &self.can_do[*inx].plans {
                if plans.number_steps() == min_plan_len {
                    min_len_inxplans.push(*inx);
                }
            }
        }

        //println!("choose_need: min len {min_plan_len} number plans {}", min_len_inxplans.len());

        assert!(!min_len_inxplans.is_empty());

        // Take a random choice
        let cd2_inx = rand::thread_rng().gen_range(0..min_len_inxplans.len());
        //println!("inx2 = {}  can_do2 = {}", &inx2, &can_do2[inx2]);

        let itmx = &self.can_do[min_len_inxplans[cd2_inx]];
        //println!("itmx.inx = {}", &itmx.inx);

        let ndx = &self.needs[itmx.inx]; // get need using tuple index

        println!(
            "\nNeed chosen: {:2} {} {}",
            &min_len_inxplans[cd2_inx],
            &ndx,
            &itmx.plans.as_ref().unwrap().str_terse()
        );

        min_len_inxplans[cd2_inx]
    } // end choose_need

    /// Get a domain number from a string.
    pub fn domain_num_from_string(&self, num_str: &str) -> Result<usize, String> {
        match num_str.parse() {
            Ok(d_num) => {
                if d_num >= self.len() {
                    Err(format!("\nDomain number too large, {d_num}"))
                } else {
                    Ok(d_num)
                }
            }
            Err(error) => Err(format!("Did not understand domain number, {error}")),
        } // end match
    }

    /// Return the length, the number of domains.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return a vector of domain current state references, in domain number order.
    pub fn all_current_states(&self) -> Vec<&SomeState> {
        let mut all_states = Vec::<&SomeState>::with_capacity(self.len());

        for domx in self.avec.iter() {
            all_states.push(domx.get_current_state());
        }

        all_states
    }

    /// Set the boredom limit.
    pub fn set_boredom_limit(&mut self) {
        let mut boredom_limit = 0;
        self.boredom = 0;
        // Get the select regions the current state is in.
        for optregs in self.select.iter() {
            if optregs
                .regions
                .is_superset_corr_states(&self.all_current_states())
            {
                boredom_limit += optregs.value;
            }
        }
        if boredom_limit > 0 {
            self.boredom_limit = boredom_limit as usize;
        } else {
            self.boredom_limit = 0;
        }
        self.boredom = 0;
    }

    /// Do functions related to the wish to be in an optimum region.
    /// Increment the boredom duration, if needed.
    /// Return a need to move to another select region, if needed.
    pub fn check_select(&mut self) -> Option<NeedStore> {
        // Check if there are no select regions.
        if self.select.is_empty() {
            return None;
        }

        // Get all domain states vector.
        // Calling self.all_current_states runs into problems with the combiler.
        let mut all_states = Vec::<&SomeState>::with_capacity(self.len());
        for domx in self.avec.iter() {
            all_states.push(domx.get_current_state());
        }

        // Get value of select states.
        let val = self.select.value_supersets_of_states(&all_states);
        if val < 0 {
            self.boredom = 0;
            self.boredom_limit = 0;
            return self
                .select
                .choose_select_exit_needs(&all_states, &self.aggregate_changes());
        }

        // Check current status within an select region, or not.
        if self.select.any_supersets_of_states(&all_states) {
            self.boredom += 1;
            if self.boredom <= self.boredom_limit {
                return None;
            }
        } else {
            self.boredom = 0;
            self.boredom_limit = 0;
        }

        self.select_goal_needs(&all_states)
    }

    /// Return a vector of aggregate change references, per domain.
    pub fn aggregate_changes(&self) -> Vec<&SomeChange> {
        let mut change_vec = Vec::<&SomeChange>::with_capacity(self.len());
        for domx in self.avec.iter() {
            change_vec.push(domx.aggregate_changes());
        }
        change_vec
    }

    /// Return a need for moving to an select region.
    fn select_goal_needs(&self, all_states: &[&SomeState]) -> Option<NeedStore> {
        // Get regions the current state is not in.
        let mut notsups = self.select_and_ints.not_supersets_of_states(all_states);

        // Remove negative, and zero, value regions.
        let mut inxs = Vec::<usize>::with_capacity(notsups.len());
        for (inx, nsupx) in notsups.iter().enumerate() {
            if nsupx.value < 1 {
                inxs.push(inx);
            }
        }
        if inxs.len() > 1 {
            inxs.reverse();
        }
        if !inxs.is_empty() {
            for iny in inxs.iter() {
                removeunordered::remove_unordered(&mut notsups, *iny);
            }
        }

        // If the current state is not in at least one select region, return None.
        if notsups.is_empty() {
            return None;
        }

        // Load return vector.
        let mut ret_str = NeedStore::with_capacity(notsups.len());
        for nsupx in notsups.iter() {
            ret_str.push(SomeNeed::ToSelectRegion {
                target_regions: (*nsupx).clone().clone(),
            });
        }
        Some(ret_str)
    }

    /// Print current states and select information.
    /// Return true if the current states are in an select region.
    pub fn print_select(&self) -> bool {
        let mut ret = false;

        let all_states = self.all_current_states();
        let select_supersets = self.select.supersets_of_states(&all_states);
        if select_supersets.is_empty() {
            print!(
                "\nAll Current states: {} in select regions: None, not in {}",
                SomeState::vec_ref_string(&all_states),
                self.select
            );
        } else {
            ret = true;
            print!(
                "\nAll Current states: {} in select regions: {}",
                SomeState::vec_ref_string(&all_states),
                SelectRegions::vec_ref_string(&select_supersets)
            );

            print!(
                ", not in: {}",
                SelectRegions::vec_ref_string(&self.select.not_supersets_of_states(&all_states))
            );
        }

        println!(
            ", Boredom level = {} of limit {}",
            self.boredom, self.boredom_limit
        );

        println!(" ");

        ret
    }

    /// Print a domain.
    pub fn print_domain(&self) {
        let dom_num = self.current_domain;

        print!("\nCurrent Domain: {} of {}", dom_num, self.len(),);

        println!("\nActs: {}", &self.avec[dom_num].actions);

        let cur_state = &self.avec[dom_num].get_current_state();

        // Calc current status.
        let all_states = self.all_current_states();
        let select_supersets = self.select.supersets_of_states(&all_states);
        let mut in_pos = 0;
        let mut in_neg = 0;
        for optx in select_supersets.iter() {
            match optx.value.cmp(&0) {
                Ordering::Less => in_neg += optx.value,
                Ordering::Greater => in_pos += optx.value,
                _ => (),
            }
        }

        let status = if in_pos > 0 && in_neg < 0 {
            format!("Conflicted {}/{}", in_pos, in_neg)
        } else if in_pos > 0 {
            format!("Positive {}", in_pos)
        } else if in_neg < 0 {
            format!("Negative {}", in_neg)
        } else {
            "Neutral".to_string()
        };

        println!("\nDom: {dom_num} Current State: {cur_state} Status: {status}");
    }

    /// Print needs that can be done.
    pub fn print_can_do(&self) {
        if self.can_do.is_empty() {
            println!("\nNeeds that can be done: None");
            self.print_select();
        } else {
            println!("\nNeeds that can be done:");

            for (inx, ndplnx) in self.can_do.iter().enumerate() {
                if ndplnx.rate != 0 {
                    println!(
                        "{:2} {} {}/{}",
                        inx,
                        &self.needs[ndplnx.inx],
                        ndplnx.plans.as_ref().unwrap().str_terse(),
                        ndplnx.rate,
                    );
                } else {
                    println!(
                        "{:2} {} {}",
                        inx,
                        &self.needs[ndplnx.inx],
                        ndplnx.plans.as_ref().unwrap().str_terse(),
                    );
                }
            } // next ndplnx
        }
    }

    /// Change the current display domain.
    pub fn change_domain(&mut self, dom_num: usize) {
        assert!(dom_num < self.avec.len());

        self.current_domain = dom_num;
    }

    /// Return a SomeState instance from a string, for the current domain.
    /// Left-most, consecutive, zeros can be omitted.
    pub fn state_from_string(&self, str: &str) -> Result<SomeState, String> {
        let dmx = self.current_domain;
        self[dmx].state_from_string(str)
    }

    // Set the current state field, of the current domain.
    pub fn set_state(&mut self, new_state: &SomeState) {
        let dmx = self.current_domain;
        self[dmx].set_state(new_state)
    }

    /// Generate and display domain and needs.
    pub fn generate_and_display_needs(&mut self) {
        // Get the needs of all Domains / Actions
        self.get_needs();
        self.display_needs();
    }

    pub fn display_needs(&self) {
        println!(
            "\nStep {} All domain states: {}",
            self.step_num,
            SomeState::vec_ref_string(&self.all_current_states())
        );
        assert!(self.step_num < 1000); // Remove for continuous use

        self.print_domain();

        // Print needs that cannot be done.
        if self.cant_do.is_empty() {
            println!("\nNeeds that cannot be done: None");
        } else {
            println!("\nNeeds that cannot be done:");

            for ndplnx in self.cant_do.iter() {
                println!("   {}", self.needs[*ndplnx]);
            }
        }

        // Print needs that can be done.
        self.print_can_do();
    }

    /// Change the current state to be within a given region.
    /// Return True if the change succeeds.
    pub fn seek_state_in_region(&mut self, dom_num: usize, goal_region: &SomeRegion) -> bool {
        if goal_region.is_superset_of_state(&self.avec[dom_num].cur_state) {
            return true;
        }

        let Some(pln) = self.get_plan(dom_num, goal_region) else { return false; };

        // Do the plan
        self.run_plan(&pln);
        goal_region.is_superset_of_state(&self.avec[dom_num].cur_state)
    }

    /// Return a store of regions that have a positive value.
    #[allow(dead_code)]
    fn positive_regions(&self, dom_num: usize) -> RegionStore {
        let mut pos_regs = RegionStore::new(vec![self[dom_num].maximum_region()]);

        for selregs in self.select.iter() {
            if selregs.value < 0 && !selregs.regions[dom_num].x_mask().is_high() {
                pos_regs = pos_regs.subtract_region(&selregs.regions[dom_num]);
            }
        }
        pos_regs
    }
} // end impl DomainStore

impl Index<usize> for DomainStore {
    type Output = SomeDomain;
    fn index(&self, i: usize) -> &SomeDomain {
        &self.avec[i]
    }
}

impl IndexMut<usize> for DomainStore {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regionstore::RegionStore;
    use crate::sample::SomeSample;

    #[test]
    /// Test case where positive regions the start and goal are in, intersect.
    /// Avoidance 0 is randomly finding paths, and choosing the one with least intersections of negative regions.
    fn avoidance1() -> Result<(), String> {
        let sf = SomeState::new_from_string(1, "s0b1111")?;
        let s0 = SomeState::new_from_string(1, "s0b0")?;

        // Init a domain, using one integer.
        let mut domx = SomeDomain::new(1);

        // Set up action to change the first bit.
        domx.add_action();
        let s1 = SomeState::new_from_string(1, "s0b1")?;
        let se = SomeState::new_from_string(1, "s0b1110")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = SomeState::new_from_string(1, "s0b10")?;
        let sd = SomeState::new_from_string(1, "s0b1101")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = SomeState::new_from_string(1, "s0b100")?;
        let sb = SomeState::new_from_string(1, "s0b1011")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = SomeState::new_from_string(1, "s0b1000")?;
        let s7 = SomeState::new_from_string(1, "s0b0111")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s8.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 3, s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(
            dmxs[0]
                .region_from_string_pad_x("r01X1")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr1, -1);

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(
            dmxs[0]
                .region_from_string_pad_x("rX101")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr2, -2);

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let goal_region = SomeRegion::new(sf.clone(), sf.clone());

        dmxs[0].get_needs(); // set aggregate changes

        let max_reg = dmxs[0].maximum_region();
        let glide_path = SomeRegion::new(s1.clone(), sf.clone());
        println!("\nmax region {max_reg}, glide path {glide_path}");

        let pos_regs = dmxs.positive_regions(0);
        println!("\npos_regs: {}", &pos_regs);
        assert!(pos_regs.len() > 1);

        let regs_goal_in = pos_regs.supersets_of(&goal_region);
        println!(
            "\nGoal {} in pos_regs: {}",
            goal_region,
            SomeRegion::vec_ref_string(&regs_goal_in)
        );
        assert!(regs_goal_in.len() > 0);

        let regs_cur_in = pos_regs.supersets_of_state(&dmxs[0].cur_state);
        println!(
            "\ncur_state {} in pos_regs: {}",
            dmxs[0].cur_state,
            SomeRegion::vec_ref_string(&regs_cur_in)
        );
        assert!(regs_cur_in.len() > 0);

        // Try to find a simple intersection of goal and cur_state regions.
        let mut intersections = RegionStore::new(vec![]);
        for greg in regs_goal_in.iter() {
            for creg in regs_cur_in.iter() {
                if greg.intersects(creg) {
                    let int_reg = greg.intersection(creg).unwrap();
                    //println!("goal region {} intersects cru region {} at {}", greg, creg, int_reg);
                    intersections.push(int_reg);
                }
            }
        }
        println!("\nintersections {}", &intersections);
        assert!(intersections.len() > 0);

        let cur_reg = SomeRegion::new(dmxs[0].cur_state.clone(), dmxs[0].cur_state.clone());
        for int_reg in intersections.iter() {
            if let Some(plans) = dmxs[0].make_plans2(&cur_reg, int_reg) {
                // choose plan.
                let plan1 = &plans[0];
                println!("\nplan 1: {} of {}", plan1, plans.len());

                if let Some(plans) = dmxs[0].make_plans2(plan1.result_region(), &goal_region) {
                    // choose a plan
                    let plan2 = &plans[0];

                    println!("\nplan 2: {} of {}", plan2, plans.len());

                    if let Some(planx) = plan1.link(&plan2) {
                        let rate = dmxs.select.rate_plan(&planx, &dmxs.all_current_states());
                        println!("final plan {} rate {}", planx, rate);
                        assert!(rate == 0);
                    } else {
                        return Err(format!("plan1+2 cannot link?"));
                    }
                } else {
                    return Err(format!("plan2 not found"));
                }
            } else {
                return Err(format!("plan1 not found"));
            }
        }

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    /// Test case where positive regions the start and goal are in, do not intersect,
    /// but another region intersects both.
    fn avoidance2() -> Result<(), String> {
        let sf = SomeState::new_from_string(1, "s0b1111")?;
        let s0 = SomeState::new_from_string(1, "s0b0")?;

        // Init a domain, using one integer.
        let mut domx = SomeDomain::new(1);

        // Set up action to change the first bit.
        domx.add_action();
        let s1 = SomeState::new_from_string(1, "s0b1")?;
        let se = SomeState::new_from_string(1, "s0b1110")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = SomeState::new_from_string(1, "s0b10")?;
        let sd = SomeState::new_from_string(1, "s0b1101")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = SomeState::new_from_string(1, "s0b100")?;
        let sb = SomeState::new_from_string(1, "s0b1011")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = SomeState::new_from_string(1, "s0b1000")?;
        let s7 = SomeState::new_from_string(1, "s0b0111")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s8.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 3, s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(
            dmxs[0]
                .region_from_string_pad_x("r0101")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr1, -1);

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(
            dmxs[0]
                .region_from_string_pad_x("r1001")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr2, -1);

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let goal_region = SomeRegion::new(sd.clone(), sd.clone());

        dmxs[0].get_needs(); // set aggregate changes

        let max_reg = dmxs[0].maximum_region();
        let glide_path = SomeRegion::new(s1.clone(), sd.clone());
        println!("\nmax region {max_reg}, glide path {glide_path}");

        let pos_regs = dmxs.positive_regions(0);
        println!("\npos_regs: {}", &pos_regs);
        assert!(pos_regs.len() > 1);

        let regs_goal_in = pos_regs.supersets_of(&goal_region);
        println!(
            "\nGoal {} in pos_regs: {}",
            goal_region,
            SomeRegion::vec_ref_string(&regs_goal_in)
        );
        assert!(regs_goal_in.len() > 0);

        let regs_cur_in = pos_regs.supersets_of_state(&dmxs[0].cur_state);
        println!(
            "\ncur_state {} in pos_regs: {}",
            dmxs[0].cur_state,
            SomeRegion::vec_ref_string(&regs_cur_in)
        );
        assert!(regs_cur_in.len() > 0);

        // Try to find a simple intersection of goal and cur_state regions.
        let mut intersections = RegionStore::new(vec![]);
        for greg in regs_goal_in.iter() {
            for creg in regs_cur_in.iter() {
                if greg.intersects(creg) {
                    let int_reg = greg.intersection(creg).unwrap();
                    //println!("goal region {} intersects cru region {} at {}", greg, creg, int_reg);
                    intersections.push(int_reg);
                }
            }
        }
        println!("\nintersections {}", &intersections);
        assert!(intersections.len() == 0);

        let mut other_pos_regions = Vec::<&SomeRegion>::new();
        for regx in pos_regs.iter() {
            if regx.is_superset_of_state(&dmxs[0].cur_state) || regx.is_superset_of(&goal_region) {
                continue;
            }
            other_pos_regions.push(regx);
        }
        println!(
            "\nOther regions: {}",
            SomeRegion::vec_ref_string(&other_pos_regions)
        );

        let mut intersections = Vec::<(SomeRegion, SomeRegion)>::new();
        for regx in other_pos_regions.iter() {
            for cregx in regs_cur_in.iter() {
                for gregx in regs_goal_in.iter() {
                    if let Some(cint) = regx.intersection(cregx) {
                        if let Some(gint) = regx.intersection(gregx) {
                            intersections.push((cint, gint));
                        }
                    }
                }
            }
        }
        println!("\nIntersections:");
        for intx in intersections.iter() {
            println!("cint {}, gint {}", intx.0, intx.1);
        }

        let cur_reg = SomeRegion::new(dmxs[0].cur_state.clone(), dmxs[0].cur_state.clone());
        for intx in intersections.iter() {
            if let Some(plans1) = dmxs[0].make_plans2(&cur_reg, &intx.0) {
                // Choose plan 1
                let plan1 = &plans1[0];

                if let Some(plans2) = dmxs[0].make_plans2(&plan1.result_region(), &intx.1) {
                    // Choose plan 2
                    let plan2 = &plans2[0];

                    if let Some(plans3) = dmxs[0].make_plans2(&plan2.result_region(), &goal_region)
                    {
                        // Choose plan 3
                        let plan3 = &plans3[0];

                        if let Some(planx) = plan1.link(&plan2) {
                            if let Some(plany) = planx.link(&plan3) {
                                let rate =
                                    dmxs.select.rate_plan(&plany, &dmxs.all_current_states());
                                println!("final plan {} rate {}", plany, rate);
                                assert!(rate == 0);
                            } else {
                                return Err(format!("plan1+2 and plan3 cannot link?"));
                            }
                        } else {
                            return Err(format!("plan1+2 cannot link?"));
                        }
                    } else {
                        return Err(format!("no plans3?"));
                    }
                } else {
                    return Err(format!("no plans2?"));
                }
            } else {
                return Err(format!("no plans1?"));
            }
        }
        Ok(())
    }
    #[test]
    /// Test case where positive regions the start and goal are in, do not intersect,
    /// and another region does not intersect both.
    /// Generalized intersection checking is needed.
    fn avoidance3() -> Result<(), String> {
        let sf = SomeState::new_from_string(1, "s0b1111")?;
        let s0 = SomeState::new_from_string(1, "s0b0")?;

        // Init a domain, using one integer.
        let mut domx = SomeDomain::new(1);

        // Set up action to change the first bit.
        domx.add_action();
        let s1 = SomeState::new_from_string(1, "s0b1")?;
        let se = SomeState::new_from_string(1, "s0b1110")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = SomeState::new_from_string(1, "s0b10")?;
        let sd = SomeState::new_from_string(1, "s0b1101")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = SomeState::new_from_string(1, "s0b100")?;
        let sb = SomeState::new_from_string(1, "s0b1011")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = SomeState::new_from_string(1, "s0b1000")?;
        let s7 = SomeState::new_from_string(1, "s0b0111")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s8.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 3, s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(
            dmxs[0]
                .region_from_string_pad_x("r0x00")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr1, -1);

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(
            dmxs[0]
                .region_from_string_pad_x("rx100")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr2, -1);

        let mut regstr3 = RegionStore::with_capacity(1);
        regstr3.push(
            dmxs[0]
                .region_from_string_pad_x("r01x1")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr3, -1);

        let mut regstr4 = RegionStore::with_capacity(1);
        regstr4.push(
            dmxs[0]
                .region_from_string_pad_x("r10x1")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr4, -1);

        let mut regstr5 = RegionStore::with_capacity(1);
        regstr5.push(
            dmxs[0]
                .region_from_string_pad_x("r101x")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr5, -1);

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let goal_region = SomeRegion::new(sd.clone(), sd.clone());

        dmxs[0].get_needs(); // set aggregate changes

        let max_reg = dmxs[0].maximum_region();
        let glide_path = SomeRegion::new(s1.clone(), sd.clone());
        println!("\nmax region {max_reg}, glide path {glide_path}");

        let pos_regs = dmxs.positive_regions(0);
        println!("\npos_regs: {}", &pos_regs);
        assert!(pos_regs.len() > 1);

        // Get regions the goal is in.
        let regs_goal_in = pos_regs.supersets_of(&goal_region);
        println!(
            "\nGoal {} in pos_regs: {}",
            goal_region,
            SomeRegion::vec_ref_string(&regs_goal_in)
        );
        assert!(regs_goal_in.len() > 0);

        // Get regions the current state is in.
        let regs_cur_in = pos_regs.supersets_of_state(&dmxs[0].cur_state);
        println!(
            "\ncur_state {} in pos_regs: {}",
            dmxs[0].cur_state,
            SomeRegion::vec_ref_string(&regs_cur_in)
        );
        assert!(regs_cur_in.len() > 0);

        let mut other_pos_regions = Vec::<&SomeRegion>::new();
        for regx in pos_regs.iter() {
            if regx.is_superset_of_state(&dmxs[0].cur_state) || regx.is_superset_of(&goal_region) {
                continue;
            }
            other_pos_regions.push(regx);
        }
        println!(
            "\nOther regions: {}",
            SomeRegion::vec_ref_string(&other_pos_regions)
        );

        // Make vectors to build intersections on.
        let mut goal_options = Vec::<Vec<&SomeRegion>>::new();
        for regx in regs_goal_in.iter() {
            goal_options.push(vec![regx]);
        }

        let mut cur_options = Vec::<Vec<&SomeRegion>>::new();
        for regx in regs_cur_in.iter() {
            cur_options.push(vec![regx]);
        }

        let mut changed = true;
        while changed {
            changed = false;

            // Check for intersections.
            if let Some(intersections) =
                SomeRegion::vec_ref_intersections(&goal_options, &cur_options)
            {
                for an_int in intersections.iter() {
                    println!(
                        "\n{} int {} at {}",
                        SomeRegion::vec_ref_string(&cur_options[an_int.1]),
                        SomeRegion::vec_ref_string(&goal_options[an_int.0]),
                        an_int.2
                    );
                }
                println!(" ");
                let mut inx = 0;
                if intersections.len() > 1 {
                    inx = rand::thread_rng().gen_range(0..intersections.len());
                }

                println!(
                    "\nan_int chosen {} int {} at {}",
                    SomeRegion::vec_ref_string(&cur_options[intersections[inx].1]),
                    SomeRegion::vec_ref_string(&goal_options[intersections[inx].0]),
                    intersections[inx].2
                );

                let from_goal = &goal_options[intersections[inx].0];
                let from_cur = &cur_options[intersections[inx].1];

                // Fill from_cur with remaining from goal regions.
                let mut final_path = <Vec<&SomeRegion>>::new();
                for regx in from_cur.iter() {
                    final_path.push(regx);
                    if regx.is_superset_of(&intersections[inx].2) {
                        break;
                    }
                }

                let mut from_goal2 = Vec::<&SomeRegion>::new();
                for regx in from_goal.iter() {
                    if *regx == &intersections[inx].2 {
                        break;
                    }
                    from_goal2.push(regx);
                    if regx.intersects(&intersections[inx].2) {
                        break;
                    }
                }
                println!("\nfrom_goal2 {}", SomeRegion::vec_ref_string(&from_goal2));
                for regx in from_goal2.iter().rev() {
                    final_path.push(regx);
                }
                final_path.push(&goal_region);
                println!("\nfinal path {}", SomeRegion::vec_ref_string(&final_path));

                // Process final_regs.
                let mut final_plan: Option<SomePlan> = None;
                let mut tmp_cur =
                    SomeRegion::new(dmxs[0].cur_state.clone(), dmxs[0].cur_state.clone());

                for inx in 0..(final_path.len() - 1) {
                    let tmp_int = final_path[inx].intersection(final_path[inx + 1]).unwrap();
                    if tmp_int == tmp_cur {
                        println!(
                            "{} int {} = {} which = cur {}",
                            final_path[inx],
                            final_path[inx + 1],
                            tmp_int,
                            tmp_cur
                        );
                        continue;
                    }
                    println!(" figure plan for {} to {}", &tmp_cur, &tmp_int);
                    if let Some(plans) = dmxs[0].make_plans2(&tmp_cur, &tmp_int) {
                        // Try to insure that the plan is within the positive region.
                        let mut inz = 0;
                        for (iny, planx) in plans.iter().enumerate() {
                            // Check planx stays within the expected region, that is, it does not stray into a negative region.
                            if let Some(regx) = planx.path_region() {
                                if regx.is_subset_of(final_path[inx]) {
                                    inz = iny;
                                    break;
                                }
                            } else {
                                return Err(format!("Empty plan {} or plan_path error?", planx));
                            }
                        }
                        println!(
                            "\nplans {} to {} at {} = {}",
                            tmp_cur,
                            final_path[inx + 1],
                            tmp_int,
                            plans[inz]
                        );
                        tmp_cur = plans[inz].result_region().clone();
                        if let Some(planx) = final_plan {
                            final_plan = planx.link(&plans[inz]);
                        } else {
                            final_plan = Some(plans[inz].clone());
                        }
                    } else {
                        panic!("problem");
                    }
                }
                if let Some(planx) = final_plan {
                    println!("\nfinal plan is {}\n", planx);
                    assert!(planx.initial_region().state1 == dmxs[0].cur_state,);
                    assert!(*planx.result_region() == goal_region);
                    //assert!(1 == 2);
                    return Ok(());
                } else {
                    println!("final plan is None");
                    return Err(format!("no final plan?"));
                }
            }

            // Build new connections.
            for regx in other_pos_regions.iter() {
                let mut tmp = Vec::<Vec<&SomeRegion>>::new();
                for vecx in cur_options.iter() {
                    if vecx.contains(regx) {
                        continue;
                    }
                    if let Some(regy) = vecx.last() {
                        if regy.intersects(regx) {
                            let mut tmp_vec = vecx.clone();
                            tmp_vec.push(regx);
                            tmp.push(tmp_vec);
                            changed = true;
                        }
                    }
                } // next vecx
                cur_options.append(&mut tmp);

                let mut tmp = Vec::<Vec<&SomeRegion>>::new();
                for vecx in goal_options.iter() {
                    if vecx.contains(regx) {
                        continue;
                    }
                    if let Some(regy) = vecx.last() {
                        if regy.intersects(regx) {
                            let mut tmp_vec = vecx.clone();
                            tmp_vec.push(regx);
                            tmp.push(tmp_vec);
                            changed = true;
                        }
                    }
                } // next vecx
                goal_options.append(&mut tmp);
            } // next regx
        } // end while
        Ok(())
    }

    #[test]
    fn all_current_states() -> Result<(), String> {
        // Init a DomainStore.
        // Domain 0 uses 1 integer for bits.
        // Domain 1 uses 2 integers for bits.
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1), SomeDomain::new(2)]);

        // Set state for domain 0, using 1 integer for bits.
        let init_state1 = dmxs[0].state_from_string("s0x12")?;
        dmxs[0].set_state(&init_state1);

        // Set state for domain 1, using 2 integers for bits.
        let init_state2 = dmxs[1].state_from_string("s0xabcd")?;
        dmxs[1].set_state(&init_state2);

        let all_states = dmxs.all_current_states();
        println!("all states {}", SomeState::vec_ref_string(&all_states));

        if all_states.len() != 2 {
            return Err(format!("Invalid length {}", all_states.len()));
        }

        if *all_states[0] != init_state1 {
            return Err(format!("Invalid first state {}", all_states[0]));
        }

        if *all_states[1] != init_state2 {
            return Err(format!("Invalid second state {}", all_states[1]));
        }

        Ok(())
    }

    #[test]
    fn check_select() -> Result<(), String> {
        // Start a DomainStore
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1), SomeDomain::new(2)]);

        // Add action to domain 0.
        dmxs[0].add_action();

        // Add action to domain 1.
        dmxs[1].add_action();

        // Load select regions
        let mut regstr1 = RegionStore::with_capacity(2);
        regstr1.push(dmxs[0].region_from_string("r0x0x")?);
        regstr1.push(dmxs[1].region_from_string("rXXXXXX10_1XXX_XXXX")?);

        let mut regstr2 = RegionStore::with_capacity(2);
        regstr2.push(dmxs[0].region_from_string("r0xx1")?);
        regstr2.push(dmxs[1].region_from_string("rXXXXXX10_1XXX_XXXX")?);

        let mut regstr3 = RegionStore::with_capacity(2);
        regstr3.push(dmxs[0].region_from_string("rx1x1")?);
        regstr3.push(dmxs[1].region_from_string("rXXXXXX10_1XXX_XXXX")?);

        let mut regstr4 = RegionStore::with_capacity(2);
        regstr4.push(dmxs[0].region_from_string("r1110")?);
        regstr4.push(dmxs[1].region_from_string("rXXXXXX10_1XXX_XXXX")?);

        // Add select region stores.
        dmxs.add_select(regstr1, 1);
        dmxs.add_select(regstr2, 1);
        dmxs.add_select(regstr3, 1);
        dmxs.add_select(regstr4, 1);
        dmxs.calc_select();

        println!("Select and ints:");
        for regstrx in dmxs.select_and_ints.iter() {
            println!("regstrx: {}", regstrx);
        }

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x12")?;
        dmxs[0].set_state(&state1);

        // Set state for domain 1.
        let state2 = dmxs[1].state_from_string("s0xabcd")?;
        dmxs[1].set_state(&state2);

        dmxs.boredom = 0;
        dmxs.boredom_limit = 0;

        let num_sup = dmxs
            .select
            .number_supersets_of_states(&vec![&state1, &state2]);
        println!("\nNumber supersets: {num_sup}",);
        if num_sup > 0 {
            return Err(format!("num_sup {num_sup} GT 0?"));
        }

        if let Some(needx) = dmxs.check_select() {
            println!("\nCheck_select returns {}", needx);
        } else {
            return Err(format!("No need found?"));
        }

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x05")?;
        dmxs[0].set_state(&state1);

        // Set state for domain 1.
        let state2 = dmxs[1].state_from_string("s0xa28d")?;
        dmxs[1].set_state(&state2);

        dmxs.boredom = 0;
        dmxs.boredom_limit = 2;
        println!(
            "\nBoredom level {} Boredom_limit {}",
            dmxs.boredom, dmxs.boredom_limit
        );

        let num_sup = dmxs
            .select
            .number_supersets_of_states(&vec![&state1, &state2]);
        println!("\nNumber supersets: {num_sup}",);
        if num_sup != 3 {
            return Err(format!("num_sup {num_sup} NE 3?"));
        }

        if let Some(needx) = dmxs.check_select() {
            return Err(format!("\nCheck_select returns need? {}", needx));
        } else {
            println!("\nCheck_select returns None");
        }

        println!(
            "\nBoredom level {} Boredom_limit {}",
            dmxs.boredom, dmxs.boredom_limit
        );

        let val = dmxs
            .select
            .value_supersets_of_states(&vec![&state1, &state2]);
        println!("val = {val}");

        Ok(())
    }
}
