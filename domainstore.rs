//! The DomainStore struct, a vector of SomeDomain structs.

use crate::change::SomeChange;
use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
use crate::selectregions::SelectRegions;
use crate::selectregionsstore::SelectRegionsStore;
use crate::state::SomeState;
use crate::targetstore::TargetStore;
use crate::tools;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;
use std::ops::{Index, IndexMut};

use rayon::prelude::*;

impl fmt::Display for DomainStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
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
    /// To allow for intersecting Select RegionStores, store RegionStores that are a subset of
    /// one, or more, Select Regions, with a sum of the values of the superset RegionStores that is greater than zero.
    /// These become goals, after rule-building needs are satisfied.
    pub select_subsets: SelectRegionsStore,
    /// Non-negative Select Regions.
    pub select_non_negative: SelectRegionsStore,
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
            select_subsets: SelectRegionsStore::new(vec![]),
            select: SelectRegionsStore::new(vec![]),
            select_non_negative: SelectRegionsStore::new(vec![]),
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
            if regstr[inx].state1().num_bits() != dmx.cur_state.num_bits() {
                panic!("reg {} bad number ints for domain {}", regstr[inx], inx);
            }
        }

        self.select.push_nosubs(SelectRegions::new(regstr, value));
    }

    /// Calculate parts of select regions, in case of any overlapps.
    pub fn calc_select(&mut self) {
        println!("\nSelect Regions:");
        for selx in self.select.iter() {
            println!("  {}", selx);
        }
        self.select_subsets = self.select.split_to_subsets();
        //println!(
        //    "\nSelect Regions positive subsets: {}",
        //    RegionStore::vec_string(&self.select_subsets)
        //);
        let mut max = RegionStore::with_capacity(self.len());
        for domx in self.avec.iter() {
            max.push(domx.maximum_region());
        }
        let mut nnvec = SelectRegionsStore::new(vec![SelectRegions::new(max, 0)]);
        for selx in self.select.iter() {
            if selx.value < 0 {
                nnvec = nnvec.subtract(selx);
            }
        }
        self.select_non_negative = nnvec;
        println!("\nNon-negative Select Regions:");
        for regsx in self.select_non_negative.iter() {
            println!("  {}", regsx);
        }

        let mut adj_regs = SelectRegionsStore::new(vec![]);

        let mut num_int = 0;
        let mut num_adjacent = 0;
        for inx in 0..(self.select_non_negative.len()) {
            for iny in (inx + 1)..self.select_non_negative.len() {
                if self.select_non_negative[inx].intersects(&self.select_non_negative[iny]) {
                    num_int += 1;
                    //println!("{} intersects {}", self.select_non_negative[inx], self.select_non_negative[iny]);
                } else if self.select_non_negative[inx].is_adjacent(&self.select_non_negative[iny])
                {
                    let adjx =
                        self.select_non_negative[inx].adjacent_part(&self.select_non_negative[iny]);
                    println!(
                        "{} adjacent {} part {}",
                        self.select_non_negative[inx], self.select_non_negative[iny], adjx
                    );
                    adj_regs.push(adjx);
                    num_adjacent += 1;
                }
            }
        }
        self.select_non_negative.append(adj_regs);

        println!("  {num_int} intersections, {num_adjacent} adjacent");
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
        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // .par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|domx| domx.get_needs())
            .collect::<Vec<NeedStore>>();

        // Get select region needs.
        if let Some(needs) = self.check_select() {
            vecx.push(needs);
        }

        // Consolidate needs into one NeedStore.
        let num_items = vecx.iter().map(|ndsx| ndsx.len()).sum();
        let mut needs = NeedStore::new(Vec::<SomeNeed>::with_capacity(num_items));
        for needsx in vecx {
            needs.append(needsx);
        }

        // Sort needs by ascending priority, and store.
        needs.sort_by_priority();
        self.needs = needs;

        self.evaluate_needs();
    }

    /// Run a PlanStore.
    pub fn run_plan_store(&mut self, plns: &PlanStore) -> bool {
        for plnx in plns.iter() {
            if !self.run_plan(plnx) {
                return false;
            }
        }
        true
    }

    /// Run a plan for a given Domain.
    /// Return true if the plan ran to completion.
    pub fn run_plan(&mut self, pln: &SomePlan) -> bool {
        self.avec[pln.dom_num].run_plan(pln)
    }

    /// Take an action to satisfy a need,
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
            let mut needx = SomeNeed::ToSelectRegion {
                target_regions: RegionStore::new(vec![]),
                priority: 0,
            };
            needx.set_priority();
            needx.priority()
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
            print!("Priority {cur_pri}");

            // Test all slice needs for plans.
            let ndsinx_plan = (cur_pri_start..cur_pri_end)
                .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
                .map(|nd_inx| (nd_inx, self.make_plans(&self.needs[nd_inx].target())))
                .map(|plnstr| InxPlan {
                    inx: plnstr.0,
                    plans: plnstr.1,
                    rate: 0,
                })
                .collect::<Vec<InxPlan>>();

            // See if any plans have been found.
            let mut none_found = true;
            for ndsinx in ndsinx_plan.iter() {
                if ndsinx.plans.is_some() {
                    none_found = false;
                    break;
                }
            }
            if none_found {
                println!(", none.");
                if cur_pri_end == needs_len || cur_pri > select_priority {
                    self.cant_do = (0..self.needs.len()).collect();
                    return;
                }

                cur_pri_start = cur_pri_end;
                cur_pri = self.needs[cur_pri_start].priority();
                continue;
            }

            // Separate needs that can be done, or not.
            // Find maximum rate.
            let mut max_rate = isize::MIN;
            for mut ndsinx in ndsinx_plan {
                if let Some(aplan) = &ndsinx.plans {
                    ndsinx.rate = self.rate_plans(aplan);
                    if ndsinx.rate > max_rate {
                        max_rate = ndsinx.rate;
                    }
                    self.can_do.push(ndsinx);
                } else {
                    self.cant_do.push(ndsinx.inx);
                }
            }

            // Return if max rate is non-negative.
            if max_rate >= 0 {
                if self.can_do.len() == 1 {
                    println!(", found 1 need that can be done.");
                } else {
                    println!(", found {} needs that can be done.", self.can_do.len());
                }
                return;
            }

            // Init vector for need/plans thatn can avoid negative regions.
            let mut can_do = Vec::<InxPlan>::new();

            // Form start region corresponding.
            let all_states = self.all_current_states();
            let mut start_regs = RegionStore::with_capacity(self.len());
            for stax in all_states.iter() {
                start_regs.push(SomeRegion::new(vec![(*stax).clone()]));
            }

            // Check if negative regions can be avoided.
            for ndinx in self.can_do.iter() {
                // Form goal region corresponding
                let mut goal_regs = RegionStore::with_capacity(self.len());
                for (dom_inx, stax) in all_states.iter().enumerate() {
                    if let Some(regt) = self.needs[ndinx.inx].target().target_region(dom_inx) {
                        goal_regs.push(regt.clone());
                    } else {
                        goal_regs.push(SomeRegion::new(vec![(*stax).clone()]));
                    }
                }

                if let Some(planx) = self.avoid_negative_select_regions(&start_regs, &goal_regs) {
                    let rate = self.select.rate_plans(&planx, &all_states.to_vec());
                    if rate > max_rate {
                        //eprintln!("\nPlan {} rate {}", aplan.unwrap(), max_rate);
                        //eprintln!(
                        //"Better plan found: {} rate {}",
                        //planx, rate
                        //);
                        can_do.push(InxPlan {
                            inx: ndinx.inx,
                            plans: Some(planx),
                            rate,
                        });
                    }
                }
            }

            for inxplnx in can_do {
                self.can_do.push(inxplnx);
            }
            if self.can_do.len() == 1 {
                println!(", found 1 need that can be done.");
            } else {
                println!(", found {} needs that can be done.", self.can_do.len());
            }
            return;
        } // End loop
          // Unreachable, since there is no break command.
    } // end evaluate_needs

    /// Return an Option PlanStore, to go from the current state to the region of each target.
    /// Return None if any one of the targets cannot be satisfied.
    pub fn make_plans(&self, targets: &TargetStore) -> Option<PlanStore> {
        //println!("domainstore: make_plans: {}", targets);
        debug_assert!(targets.is_not_empty());

        let mut plans_per_target = PlanStore::new(Vec::<SomePlan>::with_capacity(targets.len()));

        // Find a plan for each target.
        for targx in targets.iter() {
            // Try making plans.
            let mut plans = self.get_plans(targx.dom_num, &targx.region)?; // return None if any target cannot be reached.
            let inx = self.choose_a_plan(&plans);
            plans_per_target.push(plans.swap_remove(inx));
        } // next optx

        Some(plans_per_target)
    }

    /// Return a rate for a plan, based on the sum of values of select regions the plan passes through.
    fn rate_plans(&self, aplan: &PlanStore) -> isize {
        self.select.rate_plans(aplan, &self.all_current_states())
    }

    /// Get plans to move to a goal region, choose a plan.
    pub fn get_plans(&self, dom_num: usize, goal_region: &SomeRegion) -> Option<Vec<SomePlan>> {
        //println!("domainstore: get_plans: dom {dom_num} goal {goal_region}");
        self.avec[dom_num].make_plans(goal_region)
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
        let mut lengths = Vec::<usize>::with_capacity(max_rate_plans.len());
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
        let max_rate: isize = self
            .can_do
            .iter()
            .map(|inxplanx| inxplanx.rate)
            .max()
            .unwrap();

        // Make selection of max_rate plans.
        let max_rate_inxplans: Vec<usize> = (0..self.can_do.len())
            .filter(|inx| self.can_do[*inx].rate == max_rate)
            .collect();

        // Find the shortest plan length, within maximum rate plans.
        let min_plan_len: usize = max_rate_inxplans
            .iter()
            .map(|inx| self.can_do[*inx].plans.as_ref().unwrap().number_steps())
            .min()
            .unwrap();

        // Make selection of shortest plans.
        let min_len_inxplans: Vec<usize> = max_rate_inxplans
            .into_iter()
            .filter(|inx| self.can_do[*inx].plans.as_ref().unwrap().number_steps() == min_plan_len)
            .collect();

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

    /// Update counters for times_visited.
    pub fn update_times_visited(&mut self) {
        // Build vector to avoid mutable/shared reference compiler problems.
        let mut all_states = Vec::<SomeState>::with_capacity(self.len());
        for domx in self.avec.iter() {
            all_states.push(domx.cur_state.clone());
        }
        let mut all_states2 = Vec::<&SomeState>::with_capacity(self.len());
        for stax in all_states.iter() {
            all_states2.push(stax);
        }

        // Get the select regions the current state is in.
        for optregs in self.select.iter_mut() {
            if optregs.regions.is_superset_states_corr(&all_states2) {
                optregs.inc_times_visited();
            }
        }
    }

    /// Set the boredom limit.
    pub fn set_boredom_limit(&mut self) -> bool {
        let mut boredom_limit = 0;
        self.boredom = 0;
        let mut ret = false;

        // Get the select regions the current state is in.
        for optregs in self.select.iter() {
            if optregs
                .regions
                .is_superset_states_corr(&self.all_current_states())
            {
                ret = true;
                boredom_limit += optregs.value;
            }
        }
        if boredom_limit > 0 {
            self.boredom_limit = boredom_limit as usize;
        } else {
            self.boredom_limit = 0;
        }
        self.boredom = 0;
        ret
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
        // Calling self.all_current_states runs into problems with the compiler.
        let mut all_states = Vec::<&SomeState>::with_capacity(self.len());
        for domx in self.avec.iter() {
            all_states.push(domx.get_current_state());
        }

        // Get value of select states.
        let val = self.select.value_supersets_of_states(&all_states);
        if val < 0 {
            self.boredom = 0;
            self.boredom_limit = 0;

            // Find closest non-negative regions.
            let mut min_dist = usize::MAX;
            for regsx in self.select_non_negative.iter() {
                let dist = regsx.distance_states(&all_states);
                if dist < min_dist {
                    min_dist = dist;
                }
            }

            // Process closest non-negative regions.
            let mut non_neg_inxs = Vec::<usize>::new();
            for (inx, regsx) in self.select_non_negative.iter().enumerate() {
                let dist = regsx.distance_states(&all_states);
                if dist == min_dist {
                    non_neg_inxs.push(inx);
                }
            }
            // Generate needs.
            let mut ndstr = NeedStore::with_capacity(non_neg_inxs.len());
            for inx in non_neg_inxs.iter() {
                let mut needx = SomeNeed::ExitSelectRegion {
                    target_regions: self.select_non_negative[*inx].regions.clone(),
                    priority: 0,
                };
                needx.set_priority();
                ndstr.push(needx);
            }
            return Some(ndstr);
        }

        // Check current status within a select region, or not.
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
        let mut notsups = self.select.not_supersets_of_states(all_states);

        // If the current state is not in at least one select region, return None.
        if notsups.is_empty() {
            return None;
        }

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
                tools::remove_unordered(&mut notsups, *iny);
            }
        }

        let mut notsups2 = SelectRegionsStore::new(vec![]);
        for subx in self.select_subsets.iter() {
            let mut found = false;
            for sely in notsups.iter() {
                if subx.regions.is_subset_of_corr(&sely.regions) {
                    found = true;
                    break;
                }
            }
            if found {
                notsups2.push(subx.clone());
            }
        }

        // Load return vector.
        let mut ret_str = NeedStore::with_capacity(notsups.len());

        for nsupx in notsups2.iter() {
            // Calc priority addon, to weight the priority by distance and value of the region.
            let (value, times_visited) = self.select.rate_regions(&nsupx.regions);
            if value < 0 {
                continue;
            }
            let val2: usize = value as usize;

            let mut adjust = nsupx.distance_states(all_states) + (times_visited / 2);
            if val2 < adjust {
                adjust -= val2;
            } else {
                adjust = 0;
            };

            let mut needx = SomeNeed::ToSelectRegion {
                target_regions: (nsupx.regions.clone()),
                priority: adjust,
            };
            needx.set_priority();
            ret_str.push(needx);
        }
        Some(ret_str)
    }

    /// Print select region information.
    pub fn print_select(&self) {
        if self
            .select
            .any_supersets_of_states(&self.all_current_states())
        {
            println!(
                "\nSelect Region boredom/satiation level = {} of limit {}",
                self.boredom, self.boredom_limit
            );
        } else {
            print!("\nNot in any select regions",);
        }
    }

    /// Print a domain.
    pub fn print_domain(&self) {
        // Calc current status.
        let mut in_str = String::new();
        let all_states = self.all_current_states();
        let select_supersets = self.select.supersets_of_states(&all_states);
        let mut in_pos = false;
        let mut in_neg = false;
        for optx in select_supersets.iter() {
            in_str += &format!("in {} ", optx);
            match optx.value.cmp(&0) {
                Ordering::Less => in_neg = true,
                Ordering::Greater => in_pos = true,
                _ => (),
            }
        }

        let status = if in_pos && in_neg {
            "Conflicted, ".to_string()
        } else if in_pos {
            "Positive, ".to_string()
        } else if in_neg {
            "Negative, ".to_string()
        } else {
            "Neutral".to_string()
        };

        println!(
            "\nStep {} All domain states: {} Status: {status}{in_str}",
            self.step_num,
            SomeState::vec_ref_string(&self.all_current_states())
        );

        let dom_num = self.current_domain;

        print!("\nCurrent Domain: {} of {}", dom_num, self.len(),);

        println!("\nActs: {}", &self.avec[dom_num].actions);

        let cur_state = &self.avec[dom_num].get_current_state();

        println!("\nDom: {dom_num} Current State: {cur_state}");
    }

    /// Print needs that can be done.
    pub fn print_can_do(&self) {
        if self.can_do.is_empty() {
            if !self.needs.is_empty() {
                println!("\nNeeds that can be done: None");
            }
            self.print_select();
        } else {
            println!("\nNeeds that can be done:");

            for (inx, ndplnx) in self.can_do.iter().enumerate() {
                if ndplnx.rate != 0 {
                    println!(
                        "{:2} {} {}/{:+}",
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
        self.print_domain();
        self.get_needs();
        self.display_needs();
    }

    pub fn display_needs(&self) {
        assert!(self.step_num < 1100); // Remove for continuous use

        // Print needs.
        if self.needs.is_empty() {
            println!("\nNumber needs: 0");
        } else {
            // Print needs that cannot be done.
            if self.cant_do.is_empty() {
                // println!("\nNeeds that cannot be done: None");
            } else {
                println!("\nNeeds that cannot be done:");
                for ndplnx in self.cant_do.iter() {
                    println!("   {}", self.needs[*ndplnx]);
                }
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

        let Some(plans) = self.get_plans(dom_num, goal_region) else {
            return false;
        };

        // Do a plan
        self.run_plan(&plans[rand::thread_rng().gen_range(0..plans.len())])
    }

    /// When the random depth-first plans all traverse a negative region, try to form a plan
    /// that avoids negative regions.  Use random depth-first logic.
    fn avoid_negative_select_regions(
        &self,
        start_regs: &RegionStore,
        goal_regs: &RegionStore,
    ) -> Option<PlanStore> {
        //println!("avoid_negative_select_region: starting: start {start_regs} goal: {goal_regs}");
        // Get (max region - negative regions), so potitive/non-negative regions.

        // Check if no plans are needed.
        if start_regs.is_subset_of_corr(goal_regs) {
            return Some(PlanStore::new(vec![]));
        }

        if self.select_non_negative.is_empty() {
            //println!("no nn regs");
            return None;
        }

        // Check if one nn region is superset start_reg and intersects goal_reg.
        for regsx in self.select_non_negative.iter() {
            if regsx.regions.is_superset_of_corr(start_regs)
                && regsx.regions.intersects_corr(goal_regs)
            {
                //println!("one nn region found");
                return None; // Standard path search should have worked.
            }
        }

        // Get non-negative regions the start region is in.
        let mut regs_start_in = Vec::<&RegionStore>::new();
        for regsx in self.select_non_negative.iter() {
            if regsx.regions.is_superset_of_corr(start_regs) {
                regs_start_in.push(&regsx.regions);
            }
        }

        // Start region may not be in a non-negative region.
        let mut additional_start_regs = Vec::<RegionStore>::new();
        if regs_start_in.is_empty() {
            // Find closest non-negative regions.
            let mut min_dist = usize::MAX;
            for regsx in self.select_non_negative.iter() {
                let dist = regsx.regions.distance_corr(start_regs);
                if dist < min_dist {
                    min_dist = dist;
                }
            }

            for regsx in self.select_non_negative.iter() {
                let dist = regsx.regions.distance_corr(start_regs);
                if dist == min_dist {
                    let dif_msk = regsx.regions.diff_masks_corr(start_regs);
                    additional_start_regs.push(start_regs.set_to_x_corr(&dif_msk));
                }
            }
            for regx in additional_start_regs.iter() {
                //println!("Adding start reg {regx}");
                regs_start_in.push(regx);
            }
        }
        //println!(
        //    "start nn regs: {}",
        //    RegionStore::vec_ref_string(&regs_start_in)
        //);

        // Get non-negative regions the goal intersects.
        let mut regs_goal_in = Vec::<&RegionStore>::new();
        for regsx in self.select_non_negative.iter() {
            if regsx.regions.intersects_corr(goal_regs) {
                regs_goal_in.push(&regsx.regions);
            }
        }

        // Home for additional regions, caused by the goal region not being in a positive region.
        let mut additional_goal_regs = Vec::<RegionStore>::new();

        // Goal region may not intersect a non-negative region.
        if regs_goal_in.is_empty() {
            // Find closest positive regions.
            let mut min_dist = usize::MAX;
            for regsx in self.select_non_negative.iter() {
                let dist = regsx.regions.distance_corr(goal_regs);
                if dist < min_dist {
                    min_dist = dist;
                }
            }

            for regsx in self.select_non_negative.iter() {
                let dist = regsx.regions.distance_corr(goal_regs);
                if dist == min_dist {
                    let dif_msk = regsx.regions.diff_masks_corr(goal_regs);
                    additional_goal_regs.push(goal_regs.set_to_x_corr(&dif_msk));
                }
            }
            for regx in additional_goal_regs.iter() {
                //println!("Adding goal reg {regx}");
                regs_goal_in.push(regx);
            }
        }
        //println!(
        //    "goal nn regs: {}",
        //    RegionStore::vec_ref_string(&regs_goal_in)
        //);

        // Get non-negative regions the start region and goal region are not in.
        let mut other_nn_regions = Vec::<&RegionStore>::new();
        for regsx in self.select_non_negative.iter() {
            if regsx.regions.is_superset_of_corr(start_regs)
                || regsx.regions.intersects_corr(goal_regs)
            {
                continue;
            }
            other_nn_regions.push(&regsx.regions);
        }
        //println!(
        //    "other nn regs: {}",
        //    RegionStore::vec_ref_string(&other_nn_regions)
        //);

        let mut plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| {
                self.avoid_negative_select_regions2(
                    start_regs,
                    goal_regs,
                    &regs_start_in,
                    &regs_goal_in,
                    &other_nn_regions,
                )
            })
            .collect::<Vec<PlanStore>>();

        // Check for failure.
        if plans.is_empty() {
            //println!("avoid_negative_select_regions: plans is empty");
            return None;
        }

        // Look for best rated plan.
        let mut max_rate = isize::MIN;
        let mut plan_inxs = Vec::<usize>::new();
        let all_states = self.all_current_states();
        for (inx, planx) in plans.iter().enumerate() {
            let rate = self.select.rate_plans(planx, &all_states);
            if rate > max_rate {
                max_rate = rate;
                plan_inxs = Vec::<usize>::new();
            }
            if rate == max_rate {
                plan_inxs.push(inx);
            }
        }

        // Return one of the plans, avoiding the need to clone.
        Some(plans.swap_remove(plan_inxs[rand::thread_rng().gen_range(0..plan_inxs.len())]))
    } // end avoid_negative_select_regions

    /// When the random depth-first plans all traverse a negative region, try to form a plan
    /// that avoids negative regions, given options calculated in avoid_negative_select_regions.
    /// The PlanStore, when run, will process like:
    /// First step, if any needed, for each domain.
    /// Second step, if any needed, for each domain, ...
    fn avoid_negative_select_regions2(
        &self,
        start_reg: &RegionStore,
        goal_reg: &RegionStore,
        start_in: &[&RegionStore],
        goal_in: &[&RegionStore],
        other_regions: &[&RegionStore],
    ) -> Option<PlanStore> {
        //println!("avoid_negative_select_regions2: starting: start reg    {start_reg}");
        //println!(
        //    "avoid_negative_select_regions2: starting: start regs: {}",
        //    RegionStore::vec_ref_string(start_in),
        //);
        //println!("avoid_negative_select_regions2: starting: goal reg     {goal_reg}");
        //println!(
        //    "avoid_negative_select_regions2: starting: goal regs:  {}",
        //    RegionStore::vec_ref_string(goal_in)
        //);
        //println!(
        //    "avoid_negative_select_regions2: starting: other regs: {}",
        //    RegionStore::vec_ref_string(other_regions)
        //);

        // Init vector for vectors of RegionStore(s) between the start and goal.
        let mut between_vecs = Vec::<Vec<&RegionStore>>::new();
        for regcrx in other_regions.iter() {
            between_vecs.push(vec![regcrx]);
        }

        let bridge: (&RegionStore, Vec<&RegionStore>, &RegionStore);

        'find_bridge: loop {
            // Check if between_vecs contains a vector that can connect between
            // a start RegionStore and a goal RegionStore.
            for btw_x in between_vecs.iter() {
                // Get item that may intersect, or be adjacent to, an item that the start_reg is in.
                // If btw_x only contains a single item, then first() and last() will be the same item.
                let left = btw_x.first().unwrap();

                for regcr_x in start_in.iter() {
                    if regcr_x.intersects_corr(left) {
                        // Get item that may intersect, or be adjacent to, an item that the goal_reg is in.
                        let right = btw_x.last().unwrap();
                        for regcr_y in goal_in.iter() {
                            if right.intersects_corr(regcr_y) {
                                bridge = (*regcr_x, btw_x.clone(), *regcr_y);
                                break 'find_bridge;
                            }
                        }
                    }
                } // next regcr_x
            } // next btw_x

            let mut next_between_vecs = Vec::<Vec<&RegionStore>>::new();

            // Generate next level of bridge vectors.
            for btw_x in between_vecs.iter() {
                for regcr_y in other_regions.iter() {
                    if btw_x.contains(regcr_y) {
                        continue;
                    }
                    let left = btw_x.first().unwrap();
                    if left.intersects_corr(regcr_y) {
                        // Generate new between path.
                        let mut tmp_btw = Vec::<&RegionStore>::with_capacity(btw_x.len() + 1);
                        tmp_btw.push(regcr_y);
                        for regcradj_x in btw_x.iter() {
                            tmp_btw.push(regcradj_x);
                        }
                        // Save new between path for next pass.
                        next_between_vecs.push(tmp_btw);
                    }

                    let right = btw_x.last().unwrap();
                    if right.intersects_corr(regcr_y) {
                        // Generate new between path.
                        let mut tmp_btw = Vec::<&RegionStore>::with_capacity(btw_x.len() + 1);
                        for regcr_x in btw_x.iter() {
                            tmp_btw.push(regcr_x);
                        }
                        tmp_btw.push(regcr_y);
                        // Save new between path for next pass.
                        next_between_vecs.push(tmp_btw);
                    }
                }
            }

            // Check if any new bridge vectors.
            if next_between_vecs.is_empty() {
                return None;
            }
            between_vecs = next_between_vecs;
        } // end loop

        // Get path parts.
        let (start_regcr, mut bridge_regcr_vec, goal_regcr) = bridge;
        //println!(
        //    "Bridge found start {} from {} to bridge {} to {} to goal {}, ",
        //    start_reg,
        //    start_regcr,
        //    RegionStore::vec_ref_string(&bridge_regcr_vec),
        //    goal_regcr,
        //   goal_reg
        //);

        // Build path to goal.
        let mut cur_regs = start_reg.clone();
        let mut path = Vec::<&RegionStore>::with_capacity(bridge_regcr_vec.len() + 2);
        path.push(start_regcr);
        path.append(&mut bridge_regcr_vec);
        path.push(goal_regcr);
        path.push(goal_reg);

        //println!("cur_regs: {}", cur_regs);
        //println!(
        //    "path to proccess: {}",
        //    RegionStore::vec_ref_string(&path)
        //);
        //println!("goal: {}", goal_reg);
        let mut all_plans = PlanStore::new(vec![]);

        for inx in 0..(path.len() - 1) {
            let iny = inx + 1;

            let intxy = path[inx].intersection_corr(path[iny]).unwrap();
            //println!("process {} and {} int at: {}", path[inx], path[iny], intxy);
            for (domx, regx) in intxy.iter().enumerate() {
                if cur_regs[domx].is_subset_of(regx) {
                    //println!(
                    //    "dom: {}, cur reg: {}, is subset of {}, no change.",
                    //    domx, cur_regs[domx], regx
                    //);
                } else {
                    //println!(
                    //    "dom: {}, seek path from cur reg: {}, to: {}, through {}.",
                    //    domx, cur_regs[domx], regx, path[inx][domx]
                    //);
                    let mut step_plans = self.avec[domx].make_plans2(&cur_regs[domx], regx)?;

                    //println!("plans: {}", SomePlan::vec_string(&step_plans));
                    // Find plans that stay in the non-negative region.
                    let mut plan_inzs = Vec::<usize>::new();
                    for (inz, planx) in step_plans.iter().enumerate() {
                        if path[inx][domx].is_superset_of(&planx.path_region().unwrap()) {
                            plan_inzs.push(inz);
                        }
                    }
                    if plan_inzs.is_empty() {
                        return None;
                    }
                    let inz = plan_inzs[rand::thread_rng().gen_range(0..plan_inzs.len())];
                    //println!("Plan is: {}", step_plans[inz]);
                    let aplan = step_plans.swap_remove(inz);
                    cur_regs[domx] = aplan.result_region().clone();
                    all_plans.push(aplan);
                }
            }
            //assert!(1 == 2);
        }
        //println!("all plans: {all_plans}");

        assert!(goal_reg.is_superset_of_corr(&all_plans.result_region(&start_reg.states_corr())));

        Some(all_plans)
    } // end avoid_negative_select_regions2

    /// Return the total number of groups in all the domains.
    pub fn number_groups(&self) -> usize {
        let mut tot = 0;
        for domx in self.avec.iter() {
            tot += domx.number_groups();
        }
        tot
    }

    /// Return a String representation of a DomainStore.
    pub fn formatted_string(&self) -> String {
        let mut rc_str = String::from("[");

        for (inx, mskx) in self.avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &mskx));
        }
        rc_str.push(']');

        rc_str
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
    use crate::sample::SomeSample;

    /// Return the number of supersets of a StateStore
    fn number_supersets_of_states(select: &SelectRegionsStore, stas: &[&SomeState]) -> usize {
        select
            .regionstores
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_states_corr(stas)))
            .sum()
    }

    #[test]
    /// Test case where positive regions the start and goal are in, intersect.
    /// Avoidance 0 is randomly finding paths, and choosing the one with least intersections of negative regions.
    fn avoidance1() -> Result<(), String> {
        // Init a domain, using one integer.
        let mut domx = SomeDomain::new(1);

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action();
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
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
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        dmxs[0].get_needs(); // set aggregate changes

        let start_region = RegionStore::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStore::new(vec![SomeRegion::new(vec![sf.clone()])]);

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            println!(
                "Plan found: {} start {start_region} goal {goal_region}",
                planx
            );
            assert!(planx.initial_region(&[&state1]) == start_region);
            assert!(planx
                .result_region(&[&state1])
                .is_subset_of_corr(&goal_region));
            assert!(dmxs.select.rate_plans(&planx, &[&state1]) == 0);
            return Ok(());
        }
        Err("No plan found?".to_string())
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// but another region intersects both.
    fn avoidance2() -> Result<(), String> {
        // Init a domain, using one integer.
        let mut domx = SomeDomain::new(1);

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action();
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
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
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionStore::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStore::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            let rate = dmxs.select.rate_plans(&planx, &[&state1]);
            println!("Plan found: {} rate: {}", planx, rate);
            assert!(planx.initial_region(&[&state1]) == start_region);
            assert!(planx.result_region(&[&state1]) == goal_region);
            assert!(dmxs.select.rate_plans(&planx, &[&state1]) == 0);
            //assert!(1 == 2);
            return Ok(());
        }
        Err("No plan found?".to_string())
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// and another region does not intersect both.
    fn avoidance3() -> Result<(), String> {
        // Init a domain, using one integer.
        let mut domx = SomeDomain::new(1);

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action();
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
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
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionStore::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStore::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            println!("Plan found: {}", planx);
            assert!(planx.initial_region(&[&state1]) == start_region);
            assert!(planx.result_region(&[&state1]) == goal_region);
            assert!(dmxs.select.rate_plans(&planx, &[&state1]) == 0);
            //assert!(1 == 2);
            return Ok(());
        }

        Err("No plan found?".to_string())
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// and there is no path that does not cross a negative select region.
    fn avoidance4() -> Result<(), String> {
        // Init a domain, using one integer.
        let mut domx = SomeDomain::new(1);

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action();
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s8.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 3, s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(
            dmxs[0]
                .region_from_string_pad_x("r01xx")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr1, -1);

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(
            dmxs[0]
                .region_from_string_pad_x("r10xx")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr2, -1);

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionStore::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStore::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            return Err(format!("Plan found {}?", planx));
        }
        Ok(())
    }

    #[test]
    /// Test case where start and goal regions are not in a non-negative region.
    fn avoidance5() -> Result<(), String> {
        // Init a domain, using one integer.
        let mut domx = SomeDomain::new(1);

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action();
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s8.clone()));
        domx.eval_sample_arbitrary(&SomeSample::new(sf.clone(), 3, s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(
            dmxs[0]
                .region_from_string_pad_x("rxx0x")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr1, -1);
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionStore::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStore::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            println!("Plan found: {}", planx);
            assert!(planx.initial_region(&[&state1]) == start_region);
            assert!(planx.result_region(&[&state1]) == goal_region);
            assert!(dmxs.select.rate_plans(&planx, &[&state1]) == 0);
            return Ok(());
        }
        Err(format!("Plan not found ?"))
    }

    #[test]
    /// Test case where there are multiple domains.
    fn avoidance6() -> Result<(), String> {
        // Init domainstore and two domains.
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1), SomeDomain::new(1)]);
        dmxs[0].add_action();
        dmxs[0].add_action();
        dmxs[0].add_action();
        dmxs[0].add_action();

        dmxs[1].add_action();
        dmxs[1].add_action();
        dmxs[1].add_action();
        dmxs[1].add_action();

        let sf = dmxs[0].state_from_string("s0b1111")?;
        let s0 = dmxs[0].state_from_string("s0b0")?;

        // Set up action to change the first bit.
        let s1 = dmxs[0].state_from_string("s0b1")?;
        let se = dmxs[0].state_from_string("s0b1110")?;
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        let s2 = dmxs[0].state_from_string("s0b10")?;
        let sd = dmxs[0].state_from_string("s0b1101")?;
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        let s4 = dmxs[0].state_from_string("s0b100")?;
        let sb = dmxs[0].state_from_string("s0b1011")?;
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the fourth bit.
        let s8 = dmxs[0].state_from_string("s0b1000")?;
        let s7 = dmxs[0].state_from_string("s0b0111")?;
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s8.clone()));
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 3, s7.clone()));

        // Set up action to change the fourth bit.
        let s8 = dmxs[1].state_from_string("s0b1000")?;
        let s7 = dmxs[1].state_from_string("s0b0111")?;
        dmxs[1].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s8.clone()));
        dmxs[1].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, s7.clone()));

        // Set up action to change the first bit.
        let s1 = dmxs[1].state_from_string("s0b0001")?;
        let se = dmxs[1].state_from_string("s0b1110")?;
        dmxs[1].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s1.clone()));
        dmxs[1].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, se.clone()));

        // Set up action to change the second bit.
        let s2 = dmxs[1].state_from_string("s0b0010")?;
        let sd = dmxs[1].state_from_string("s0b1101")?;
        dmxs[1].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s2.clone()));
        dmxs[1].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sd.clone()));

        // Set up action to change the third bit.
        let s4 = dmxs[1].state_from_string("s0b0100")?;
        let sb = dmxs[1].state_from_string("s0b1011")?;
        dmxs[1].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s4.clone()));
        dmxs[1].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 3, sb.clone()));

        // Init aggregate needs.
        dmxs.get_needs();

        // Set select regions.

        // Set up dom 0 00XX dependent on dom 1 01XX.
        let mut regstr0 = RegionStore::with_capacity(1);
        regstr0.push(
            dmxs[0]
                .region_from_string_pad_x("r11xx")
                .expect("String should be formatted correctly"),
        );
        regstr0.push(
            dmxs[1]
                .region_from_string_pad_x("r01xx")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr0, -1);

        // Set up dom 0 00XX dependent on dom 1 10XX.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(
            dmxs[0]
                .region_from_string_pad_x("r11xx")
                .expect("String should be formatted correctly"),
        );
        regstr1.push(
            dmxs[1]
                .region_from_string_pad_x("r1xx1")
                .expect("String should be formatted correctly"),
        );
        dmxs.add_select(regstr1, -1);
        dmxs.calc_select();

        // Set current state for domain 0.
        let cur0 = dmxs[0].state_from_string("s0x0")?;
        dmxs[0].set_state(&cur0);

        // Set current state for domain 1.
        let cur1 = dmxs[1].state_from_string("s0x1")?;
        dmxs[1].set_state(&cur1);

        println!("\nDom 0 Actions {}\n", dmxs[0].actions);
        println!("\nDom 1 Actions {}\n", dmxs[1].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionStore::new(vec![
            SomeRegion::new(vec![cur0.clone()]),
            SomeRegion::new(vec![cur1.clone()]),
        ]);
        let goal_region = RegionStore::new(vec![
            SomeRegion::new(vec![sf.clone()]),
            SomeRegion::new(vec![sf.clone()]),
        ]);

        let all_states = dmxs.all_current_states();

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            print!("Plans {}", plans);
            let rate = dmxs.select.rate_plans(&plans, &all_states);
            print!(", rate {}", rate);
            println!(" ");
        } else {
            return Err(format!("No plan found?"));
        }

        //assert!(1 == 2);
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

        assert!(all_states.len() == 2);
        assert!(*all_states[0] == init_state1);
        assert!(*all_states[1] == init_state2);

        Ok(())
    }

    #[test]
    /// Test case using adjacent non-negative regions.
    fn avoidance7() -> Result<(), String> {
        // Init domainstore and two domains.
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1)]);
        dmxs[0].add_action();
        dmxs[0].add_action();
        dmxs[0].add_action();
        dmxs[0].add_action();

        let sf = dmxs[0].state_from_string("s0b1111")?;
        let s0 = dmxs[0].state_from_string("s0b0")?;

        // Set up action to change the first bit.
        let s1 = dmxs[0].state_from_string("s0b0001")?;
        let se = dmxs[0].state_from_string("s0b1110")?;
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 0, s1.clone()));
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 0, se.clone()));

        // Set up action to change the second bit.
        let s2 = dmxs[0].state_from_string("s0b0010")?;
        let sd = dmxs[0].state_from_string("s0b1101")?;
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 1, s2.clone()));
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 1, sd.clone()));

        // Set up action to change the third bit.
        let s4 = dmxs[0].state_from_string("s0b0100")?;
        let sb = dmxs[0].state_from_string("s0b1011")?;
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 2, s4.clone()));
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 2, sb.clone()));

        // Set up action to change the fourth bit.
        let s8 = dmxs[0].state_from_string("s0b1000")?;
        let s7 = dmxs[0].state_from_string("s0b0111")?;
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(s0.clone(), 3, s8.clone()));
        dmxs[0].eval_sample_arbitrary(&SomeSample::new(sf.clone(), 3, s7.clone()));

        // Init aggregate needs.
        dmxs.get_needs();

        // Set select regions.

        // Set up dom 0 00XX dependent on dom 1 01XX.
        let mut regstr0 = RegionStore::with_capacity(1);
        regstr0.push(dmxs[0].region_from_string_pad_x("r1100").expect("SNH"));
        dmxs.add_select(regstr0, -1);

        // Set up dom 0 00XX dependent on dom 1 10XX.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r1011").expect("SNH"));
        dmxs.add_select(regstr1, -1);
        dmxs.calc_select();

        let s0 = dmxs[0].state_from_string("s0b0000").expect("SNH");
        dmxs[0].set_state(&s0);

        let sd = dmxs[0].state_from_string("s0b1101")?;

        let start_region = RegionStore::new(vec![SomeRegion::new(vec![s0.clone()])]);
        let goal_region = RegionStore::new(vec![SomeRegion::new(vec![sd.clone()])]);

        let all_states = dmxs.all_current_states();

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            print!("Plans {}", plans);
            let rate = dmxs.select.rate_plans(&plans, &all_states);
            print!(", rate {}", rate);
            println!(" ");
            assert!(rate == 0);
            assert!(plans.len() == 3);
            assert!(plans[0].result_region() == &dmxs[0].region_from_string("r0110").expect("SNH"));
            assert!(plans[1].result_region() == &dmxs[0].region_from_string("r0111").expect("SNH"));
        } else {
            return Err(format!("No plan found?"));
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

        println!("Select subsets:");
        for regstrx in dmxs.select_subsets.iter() {
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

        let num_sup = number_supersets_of_states(&dmxs.select, &vec![&state1, &state2]);
        println!("\nNumber supersets: {num_sup}",);
        assert!(num_sup == 0);

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

        let num_sup = number_supersets_of_states(&dmxs.select, &vec![&state1, &state2]);
        println!("\nNumber supersets: {num_sup}",);
        assert!(num_sup == 3);

        if let Some(needx) = dmxs.check_select() {
            return Err(format!("\nCheck_select returns need? {}", needx));
        }

        println!(
            "\nBoredom level {} Boredom_limit {}",
            dmxs.boredom, dmxs.boredom_limit
        );
        assert!(dmxs.boredom == 1);
        assert!(dmxs.boredom_limit == 2);

        let val = dmxs
            .select
            .value_supersets_of_states(&vec![&state1, &state2]);
        println!("val = {val}");
        assert!(val == 3);

        Ok(())
    }

    // Test exit_select_needs with two overlapping negative select regions.
    // from s0111 to s1000.
    #[test]
    fn test_exit_select_needs() -> Result<(), String> {
        // Init a DomainStore.
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1)]);

        let mut regstr1 = RegionStore::with_capacity(1);
        let neg_reg1 = dmxs[0]
            .region_from_string_pad_x("rX1XX")
            .expect("String should be formatted correctly");

        regstr1.push(neg_reg1.clone());

        // Add select regionstores.
        dmxs.add_select(regstr1, -1);

        let mut regstr1 = RegionStore::with_capacity(1);
        let neg_reg2 = dmxs[0]
            .region_from_string_pad_x("r1XX1")
            .expect("String should be formatted correctly");
        regstr1.push(neg_reg2.clone());

        // Add select regionstores.
        dmxs.add_select(regstr1, -1);

        // Set state for domain 0, using 1 integer for bits.
        let state1 = dmxs[0].state_from_string("s0xd")?;
        dmxs[0].set_state(&state1);

        // Finish select regions setup.
        dmxs.calc_select();
        println!("select regs: {}", dmxs.select.formatted_string());
        println!(
            "non-neg:     {}",
            &dmxs.select_non_negative.formatted_string()
        );

        // Get exit needs.
        if let Some(nds) = dmxs.check_select() {
            println!("needs len {}", nds.len());
            assert!(nds.len() == 2);
            println!("needs: {}", nds);
            for ndsx in nds.iter() {
                assert!(!neg_reg1.intersects(&ndsx.target()[0].region));
                assert!(!neg_reg2.intersects(&ndsx.target()[0].region));
            }
        } else {
            return Err(format!("Needs are None?"));
        }

        Ok(())
    }
}
