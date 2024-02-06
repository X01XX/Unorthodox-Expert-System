//! The DomainStore struct, a vector of SomeDomain structs.

use crate::change::SomeChange;
use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionstorecorr::RegionStoreCorr;
use crate::selectregions::SelectRegions;
use crate::selectregionsstore::SelectRegionsStore;
use crate::state::SomeState;
use crate::statestorecorr::StateStoreCorr;
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
    pub plans: PlanStore,
    /// Rate based on the select regions a plan passes through.
    pub rate: isize,
}

#[readonly::make]
#[derive(Serialize, Deserialize, Default)]
pub struct DomainStore {
    /// Vector of SomeDomain structs.
    pub domains: Vec<SomeDomain>,
    /// Domain displayed to user.
    pub current_domain: usize,
    /// A counter to indicate the number of steps the current state is in the same select region.
    pub boredom: usize,
    /// A limit for becomming bored, then moving to another select state.
    pub boredom_limit: usize,
    /// Zero, or more, select regions that may have poitive, or negative, value.
    /// They may overlap.
    pub select: SelectRegionsStore,
    /// Positive regions, not overlapped by negative >= value regions.
    /// These tend to be goals.
    pub select_positive: SelectRegionsStore,
    /// Negative regions, value regions.
    /// These tend to be places to avoid in planning a path to a goal.
    pub select_negative: SelectRegionsStore,
    /// Non-negative, may be positive, regions.
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
    pub fn new() -> Self {
        Self {
            domains: Vec::<SomeDomain>::new(),
            current_domain: 0,
            boredom: 0,
            boredom_limit: 0,
            select: SelectRegionsStore::new(vec![]),
            select_negative: SelectRegionsStore::new(vec![]),
            select_positive: SelectRegionsStore::new(vec![]),
            select_non_negative: SelectRegionsStore::new(vec![]),
            needs: NeedStore::new(vec![]),
            can_do: Vec::<InxPlan>::new(),
            cant_do: Vec::<usize>::new(),
            step_num: 0,
        }
    }

    /// Add SelectRegions, one region for each domain.
    /// [1x0x, x101] = Domain 0, 1x0x, AND domain 1, x101.
    /// [1x0x, xxxx],
    /// [xxxx, x101] (two additions) = Domain 0, 1x0x, OR domain 1, x101.
    pub fn add_select(&mut self, selx: SelectRegions) {
        debug_assert!(selx.len() == self.domains.len());

        // Require some aggregate value.
        if selx.pos == selx.neg {
            return;
        }

        // Do not allow dups.
        if self.select.any_equal_regions(&selx) {
            println!("Equal select regions found");
            return;
        }

        // Check that each select region matches the corresponding domain region size.
        // Check that at least one region has at least one non-x bit.
        let mut allx = true;
        for selx in selx.regions.iter() {
            if allx && !selx.all_x() {
                allx = false;
            }
        }

        if allx {
            return;
        }

        self.select.push_nosubs(selx);
    }

    /// Calculate parts of select regions, in case of any overlaps.
    pub fn calc_select(&mut self) {
        if self.select.is_empty() {
            return;
        }

        println!("\nSelect Regions:");
        for selx in self.select.iter() {
            println!("  {}", selx);
        }

        // Init negative and positive stores.
        for selx in self.select.iter() {
            match selx.pos.cmp(&selx.neg) {
                Ordering::Greater => self.select_positive.push_nosubs(selx.clone()),
                Ordering::Less => self.select_negative.push_nosubs(selx.clone()),
                _ => (),
            }
        }

        // Get subset intersection regions.
        let subsets = self.select.split_by_intersections();

        //println!("\nSubset Regions:");
        for sely in subsets.into_iter() {
            match sely.pos.cmp(&sely.neg) {
                Ordering::Greater => {
                    //println!("  {} pos {} neg {} = {:+}", sely, sely.pos, sely.neg, sely.pos - sely.neg);
                    self.select_positive.push_nosups(sely);
                }
                Ordering::Less => {
                    //println!("  {} pos {} neg {} = {}", sely, sely.pos, sely.neg, sely.pos - sely.neg);
                    self.select_negative.push_nosups(sely);
                }
                _ => (),
            }
        }

        println!("\nNegative Select Regions:");
        for selx in self.select_negative.iter() {
            println!("  {}", selx);
        }

        println!("\nPositive Select Regions:");
        for selx in self.select_positive.iter() {
            println!("  {}", selx);
        }

        // Calc non-negative regions.
        let max_select =
            SelectRegionsStore::new(vec![SelectRegions::new(self.maximum_regions(), 0, 0)]);
        self.select_non_negative = max_select.subtract(&self.select_negative);

        println!("\nNon negative select Regions:");
        for selx in self.select_non_negative.iter() {
            println!("  {}", selx);
        }
    }

    /// Add a Domain struct to the store.
    /// Add select regions after the last domain has been added.
    pub fn add_domain(&mut self, cur_state: SomeState) {
        debug_assert!(self.select.is_empty());

        self.domains
            .push(SomeDomain::new(self.domains.len(), cur_state));
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
            .domains
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

    /// Return the number of steps in a vector of PlanStores.
    pub fn number_steps(&self, plns: &Option<PlanStore>) -> usize {
        if let Some(planx) = plns {
            planx.number_steps()
        } else {
            0
        }
    }

    /// Run a PlanStore.
    pub fn run_plan_store(&mut self, plns: &PlanStore) -> bool {
        for plnx in plns.iter() {
            if plnx.is_not_empty() {
                match self.run_plan(plnx) {
                    Ok(_) => continue,
                    Err(msg) => {
                        println!("{}", msg);
                        return false;
                    }
                }
            }
        }
        true
    }

    /// Run a plan for a given Domain.
    /// Return true if the plan ran to completion.
    pub fn run_plan(&mut self, pln: &SomePlan) -> Result<usize, String> {
        self.domains[pln.dom_id].run_plan(pln, 0)
    }

    /// Take an action to satisfy a need,
    pub fn take_action_need(&mut self, nd_inx: usize) {
        self.domains[self.needs[nd_inx].dom_id()].take_action_need(&self.needs[nd_inx]);
    }

    /// Return a reference to the current state of a given Domain index
    pub fn cur_state(&self, dmxi: usize) -> &SomeState {
        self.domains[dmxi].get_current_state()
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
                target_regions: RegionStoreCorr::with_capacity(1),
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
        let mut cur_pri_end = 0;
        let needs_len = self.needs.len();

        // Scan successive slices of items with the same priority.
        loop {
            // Find end of current slice.
            while cur_pri_end < needs_len && self.needs[cur_pri_end].priority() == cur_pri {
                cur_pri_end += 1;
            }
            // Process a priority slice.
            print!("Priority {cur_pri}");

            // Test all slice needs for plans.
            let inx_plan = (cur_pri_start..cur_pri_end)
                .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
                .map(|nd_inx| (nd_inx, self.make_plans(&self.needs[nd_inx].target())))
                .collect::<Vec<(usize, Option<PlanStore>)>>();

            // See if any plans have been found.
            let mut cant = Vec::<usize>::new();
            let mut can = Vec::<InxPlan>::new();

            for (inx, planx) in inx_plan {
                if let Some(plany) = planx {
                    let ratex = self.rate_plans(&plany);
                    can.push(InxPlan {
                        inx,
                        plans: plany,
                        rate: ratex,
                    });
                } else {
                    cant.push(inx);
                }
            }
            if can.is_empty() {
                println!(", none.");
                if cur_pri_end == needs_len || cur_pri > select_priority {
                    self.cant_do = cant;
                    return;
                }

                cur_pri_start = cur_pri_end;
                cur_pri = self.needs[cur_pri_start].priority();
                continue;
            }

            if can.len() == 1 {
                println!(", found 1 need that can be done.");
            } else {
                println!(", found {} needs that can be done.", can.len());
            }

            // Form start regions corresponding to domains.
            let start_regs = self.all_current_regions();

            // Check if negative regions can be avoided.
            for ndinx in can {
                //let Some(planx) = &self.can_do[ndinx].plans else {
                //    continue;
                //};

                if ndinx.rate >= 0 {
                    self.can_do.push(ndinx);
                    continue;
                }

                // Form goal region corresponding
                let goal_regs =
                    self.targetstore_to_regionstorecorr(&self.needs[ndinx.inx].target());

                if let Some(planx) = self.avoid_negative_select_regions(&start_regs, &goal_regs) {
                    let rate = self.rate_plans(&planx);
                    //println!(" new rate {rate}");
                    if rate > ndinx.rate {
                        println!(
                            "\nFor plan {}/{}, {} to {}, better plan found\n         {}/{}",
                            ndinx.plans.str_terse(),
                            ndinx.rate,
                            start_regs,
                            goal_regs,
                            planx.str_terse(),
                            rate
                        );
                        println!("\nFirst plan:");
                        self.print_plan_detail(&ndinx.plans);
                        println!("\nA better plan:");
                        self.print_plan_detail(&planx);
                        println!(" ");

                        self.can_do.push(InxPlan {
                            inx: ndinx.inx,
                            plans: planx,
                            rate,
                        });
                    } else {
                        self.can_do.push(ndinx);
                    }
                } else {
                    self.can_do.push(ndinx);
                }
            }

            return;
        } // End loop
          // Unreachable, since there is no break command.
    } // end evaluate_needs

    /// Return an Option PlanStore, to go from the current states to the region of each target.
    /// Return None if any one of the targets cannot be satisfied.
    pub fn make_plans(&self, targets: &TargetStore) -> Option<PlanStore> {
        //println!("domainstore: make_plans: {}", targets);
        debug_assert!(targets.is_not_empty());

        let from = self.all_current_regions();
        let goal = self.targetstore_to_regionstorecorr(targets);

        self.make_plans2(&from, &goal, None)
    }

    /// Use parallel processing to get plans.
    pub fn make_plans2(
        &self,
        from: &RegionStoreCorr,
        goal: &RegionStoreCorr,
        within: Option<&RegionStoreCorr>,
    ) -> Option<PlanStore> {
        let mut plans = (0..2)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| self.make_plans3(from, goal, within))
            .collect::<Vec<PlanStore>>();

        if plans.is_empty() {
            return None;
        }

        Some(plans.swap_remove(self.choose_a_plan(&plans, from)))
    }

    /// Return an Option PlanStore, to go from a set of domain/regions to another set of domain/regions.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    pub fn make_plans3(
        &self,
        from: &RegionStoreCorr,
        goal: &RegionStoreCorr,
        within: Option<&RegionStoreCorr>,
    ) -> Option<PlanStore> {
        //println!("domainstore: make_plans3: from {from} goal {goal}");

        let mut plans_per_target = PlanStore::new(Vec::<SomePlan>::with_capacity(from.len()));

        if let Some(within_regs) = within {
            // Find a plan for each target.
            for (dom_id, (regx, regy)) in from.iter().zip(goal.iter()).enumerate() {
                if regy.is_superset_of(regx) {
                    plans_per_target.push(SomePlan::new(dom_id, vec![]));
                } else {
                    // Try making plans.
                    let mut plans =
                        self.get_plans(dom_id, regx, regy, Some(&within_regs[dom_id]))?; // return None if any target cannot be reached.
                    plans_per_target
                        .push(plans.swap_remove(rand::thread_rng().gen_range(0..plans.len())));
                }
            } // next optx
        } else {
            for (dom_id, (regx, regy)) in from.iter().zip(goal.iter()).enumerate() {
                if regy.is_superset_of(regx) {
                    plans_per_target.push(SomePlan::new(dom_id, vec![]));
                } else {
                    // Try making plans.
                    let mut plans = self.get_plans(dom_id, regx, regy, None)?; // return None if any target cannot be reached.
                    plans_per_target
                        .push(plans.swap_remove(rand::thread_rng().gen_range(0..plans.len())));
                }
            } // next optx
        }

        //println!("domainstore: make_plans3: from {from} goal {goal} returning {plans_per_target}");
        assert!(plans_per_target.validate(from, goal));
        Some(plans_per_target)
    }

    /// Return a rate for a plan, based on the sum of values of select regions the plan passes through.
    fn rate_plans(&self, plans: &PlanStore) -> isize {
        self.rate_plans2(plans, &self.all_current_regions())
    }

    /// Return a rate for a set of plans, based on the sum of values of negative select regions the plan passes through.
    /// This ignores the select regions a plan starts, or ends, in.
    /// A square in a region can exit the region to an adjacent square by changing one bit.
    /// A square adjacent to a region can enter the region by changing one bit.
    fn rate_plans2(&self, plans: &PlanStore, regions: &RegionStoreCorr) -> isize {
        // Store rate for each step.
        let mut rates = Vec::<isize>::with_capacity(plans.number_steps());

        let mut cur_regions = regions.clone();
        // Skip start value of negative SelectRegions.
        for planx in plans.iter() {
            if planx.is_empty() {
                continue;
            }
            for stepx in planx.iter() {
                cur_regions[planx.dom_id] = stepx.rule.result_from(&cur_regions[planx.dom_id]);
                rates.push(self.select.value_intersections_of(&cur_regions));
            }
        }
        rates.pop(); // lose end value of negative SelectRegions.
        rates.iter().sum()
    }

    /// Get plans to move to a goal region, choose a plan.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    pub fn get_plans(
        &self,
        dom_id: usize,
        from_region: &SomeRegion,
        goal_region: &SomeRegion,
        within: Option<&SomeRegion>,
    ) -> Option<Vec<SomePlan>> {
        //println!("domainstore: get_plans2: dom {dom_id} from {from_region} goal {goal_region}");
        self.domains[dom_id].make_plans2(from_region, goal_region, within)
    }

    /// Choose a plan from a vector of PlanStare references, for vector of PlanStores.
    /// Return index of plan chosen.
    pub fn choose_a_plan(&self, plans: &[PlanStore], start_regs: &RegionStoreCorr) -> usize {
        self.choose_a_plan2(&tools::ref_vec(plans), start_regs)
    }

    /// Choose a plan from a vector of PlanStare references, for vector of PlanStore references.
    /// Return index of plan chosen.
    pub fn choose_a_plan2(&self, plans: &[&PlanStore], start_regs: &RegionStoreCorr) -> usize {
        assert!(!plans.is_empty());

        // No choice to be made.
        if plans.len() == 1 {
            return 0;
        }

        // Gather plan rate data.
        let mut rates = Vec::<isize>::with_capacity(plans.len());

        for planx in plans.iter() {
            rates.push(self.rate_plans2(planx, start_regs));
        }

        // Get max rate.
        let max_rate = rates.iter().max().unwrap();

        // Find plans with the max rate.
        let mut max_rate_plans = Vec::<usize>::new();
        for (inx, rate) in rates.iter().enumerate() {
            if rate == max_rate {
                max_rate_plans.push(inx);
            }
        }

        // No further choice to be made.
        if max_rate_plans.len() == 1 {
            return max_rate_plans[0];
        }

        // Gather length data.
        let mut lengths = Vec::<usize>::with_capacity(max_rate_plans.len());
        for inx in max_rate_plans.iter() {
            lengths.push(plans[*inx].number_steps());
        }
        let min_len = lengths.iter().min().unwrap();

        // Find plans with the min length.
        let mut min_len_plans = Vec::<usize>::new();
        for (inx, lenx) in lengths.iter().enumerate() {
            if lenx == min_len {
                min_len_plans.push(max_rate_plans[inx]);
            }
        }

        // No further choice to be made.
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
    pub fn choose_a_need(&self) -> usize {
        //println!("choose_a_need: number InxPlans {}", can_do.len());

        let mut ref_vec = Vec::<&PlanStore>::with_capacity(self.can_do.len());
        for inxplanx in self.can_do.iter() {
            ref_vec.push(&inxplanx.plans);
        }

        let inx = self.choose_a_plan2(&ref_vec, &self.all_current_regions());

        println!(
            "\nNeed chosen: {:2} {} {}",
            inx,
            self.needs[self.can_do[inx].inx],
            self.can_do[inx].plans.str_terse()
        );

        inx
    } // end choose_need

    /// Get a domain number from a string.
    pub fn domain_id_from_string(&self, num_str: &str) -> Result<usize, String> {
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
        self.domains.len()
    }

    /// Return true if the store is empty
    pub fn is_empty(&self) -> bool {
        self.domains.is_empty()
    }

    /// Return a vector of domain current state references, in domain number order.
    pub fn all_current_states(&self) -> StateStoreCorr {
        let mut all_states = StateStoreCorr::with_capacity(self.len());

        for domx in self.domains.iter() {
            all_states.push(domx.get_current_state().clone());
        }

        all_states
    }

    /// Return a vector of domain current state references, as regions, in domain number order.
    pub fn all_current_regions(&self) -> RegionStoreCorr {
        let mut all_regions = RegionStoreCorr::with_capacity(self.len());

        for domx in self.domains.iter() {
            all_regions.push(SomeRegion::new(vec![domx.get_current_state().clone()]));
        }

        all_regions
    }

    /// Update counters for times_visited.
    pub fn update_times_visited(&mut self) {
        // Get all domain current states.
        let all_states = self.all_current_states();

        // Get the select regions the current state is in.
        for optregs in self.select.iter_mut() {
            if optregs.regions.is_superset_states(&all_states) {
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
                .is_superset_states(&self.all_current_states())
            {
                ret = true;
                boredom_limit += optregs.value();
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

        // Get all domain current states vector.
        let all_states = self.all_current_states();

        // Check if current state is in a negative select state.
        // Since SelectRegions have a Boolean AND relationship, only
        // changing in one non-negative region, in one domain, is needed.
        if self.select_negative.any_supersets_of_states(&all_states) {
            self.boredom = 0;
            self.boredom_limit = 0;

            let mut ndstr = NeedStore::new(vec![]);

            print!("all_states: [");
            for stax in all_states.iter() {
                print!(" {}", stax);
            }
            println!("], is subset of a negative region. ");

            for dom_id in 0..self.len() {
                //let non_negs = self._calc_non_negative_regions(dom_id, &all_states, None);
                //let non_negs = &self.select_non_negative;

                // Find closest non-negative region distance.
                let mut min_dist = usize::MAX;
                for regsx in self.select_non_negative.iter() {
                    let dist = regsx.distance_states(&all_states);
                    if dist < min_dist {
                        min_dist = dist;
                    }
                }

                // Process closest non-negative regions.
                for regsx in self.select_non_negative.iter() {
                    if regsx.distance_states(&all_states) == min_dist {
                        let mut needx = SomeNeed::ExitSelectRegion {
                            dom_id,
                            target_regions: regsx.regions.clone(),
                            priority: 0,
                        };
                        needx.set_priority();
                        ndstr.push(needx);
                    }
                }
            } // next dom_id
            return Some(ndstr);
        }

        // Check current status within a select region, or not.
        if self.select_positive.any_supersets_of_states(&all_states) {
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
        for domx in self.domains.iter() {
            if let Some(changes) = &domx.aggregate_changes() {
                change_vec.push(changes);
            }
        }
        change_vec
    }

    /// Return a need for moving to an select region.
    fn select_goal_needs(&self, all_states: &StateStoreCorr) -> Option<NeedStore> {
        //println!("domainstore: select_goal_needs");
        // Get regions the current state is not in.
        let mut notsups = self.select_positive.not_supersets_of_states(all_states);

        // If the current state is not in at least one select region, return None.
        if notsups.is_empty() {
            //println!("ret 1");
            return None;
        }

        // Remove negative, and zero, value regions.
        let mut inxs = Vec::<usize>::with_capacity(notsups.len());
        for (inx, nsupx) in notsups.iter().enumerate() {
            if nsupx.value() < 1 {
                inxs.push(inx);
            }
        }
        if inxs.len() > 1 {
            inxs.reverse();
        }
        if inxs.is_empty() {
        } else {
            for iny in inxs.iter() {
                tools::remove_unordered(&mut notsups, *iny);
            }
        }

        // Load return vector.
        let mut ret_str = NeedStore::with_capacity(notsups.len());

        for nsupx in notsups.iter() {
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
            match optx.value().cmp(&0) {
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
            self.all_current_states()
        );

        let dom_id = self.current_domain;

        print!("\nCurrent Domain: {} of {}", dom_id, self.len(),);

        println!("\nActs: {}", self.domains[dom_id].actions);

        let cur_state = &self.domains[dom_id].get_current_state();

        println!("\nDom: {dom_id} Current State: {cur_state}");
    }

    /// Print needs that can be done.
    pub fn print_can_do(&self) {
        if self.can_do.is_empty() {
            if self.needs.is_not_empty() {
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
                        self.needs[ndplnx.inx],
                        ndplnx.plans.str_terse(),
                        ndplnx.rate,
                    );
                } else {
                    println!(
                        "{:2} {} {}",
                        inx,
                        self.needs[ndplnx.inx],
                        ndplnx.plans.str_terse(),
                    );
                }
            } // next ndplnx
        }
    }

    /// Change the current display domain.
    pub fn change_domain(&mut self, dom_id: usize) {
        assert!(dom_id < self.domains.len());

        self.current_domain = dom_id;
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
    pub fn seek_state_in_region(&mut self, dom_id: usize, goal_region: &SomeRegion) -> bool {
        if goal_region.is_superset_of(&self.domains[dom_id].cur_state) {
            return true;
        }

        let from = self.all_current_regions();
        let mut goal = from.clone();
        goal[dom_id] = goal_region.clone();

        let Some(plans) = self.make_plans2(&from, &goal, None) else {
            return false;
        };

        // Do a plan
        match self.run_plan(&plans[rand::thread_rng().gen_range(0..plans.len())]) {
            Ok(_) => true,
            Err(msg) => {
                println!("{}", msg);
                false
            }
        }
    }
    /// Like Real Life, formulate a direct plan,
    /// notice there are some negative aspects,
    /// then try to form a plan that avoids the negative.
    fn avoid_negative_select_regions(
        &self,
        start_regs: &RegionStoreCorr,
        goal_regs: &RegionStoreCorr,
    ) -> Option<PlanStore> {
        //println!(
        //    "avoid_negative_select_regions_new: starting: start {start_regs} goal: {goal_regs}"
        //);
        assert!(start_regs.len() == self.len());
        assert!(goal_regs.len() == self.len());

        // Check if no plans are needed.
        if start_regs.intersects(goal_regs) {
            //println!("No plan needed (1)");
            return None;
        }

        // Check if this function is needed.
        let mut not_needed = true;
        for selx in self.select_negative.iter() {
            if selx.regions.is_between(start_regs, goal_regs) {
                not_needed = false;
                break;
            }
        }
        if not_needed {
            //println!("No plan needed (2)");
            return None;
        }

        let mut plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| self.avoid_negative_select_regions2(start_regs, goal_regs))
            .collect::<Vec<PlanStore>>();

        if plans.is_empty() {
            //println!("No plan needed (3)");
            return None;
        }

        // Find the highest rated plans.
        let mut max_rate = isize::MIN;
        let mut inxs = Vec::<usize>::new();
        for (inx, planx) in plans.iter().enumerate() {
            let rate = self.rate_plans2(planx, start_regs);
            if rate > max_rate {
                max_rate = rate;
                inxs = Vec::<usize>::new();
            }
            if rate == max_rate {
                inxs.push(inx);
            }
        }

        Some(plans.swap_remove(inxs[rand::thread_rng().gen_range(0..inxs.len())]))
    }

    /// Return plans for a change from start to goal
    fn avoid_negative_select_regions2(
        &self,
        start_regs: &RegionStoreCorr,
        goal_regs: &RegionStoreCorr,
    ) -> Option<PlanStore> {
        //println!(
        //    "avoid_negative_select_regions2_new: starting: start {start_regs} goal: {goal_regs}"
        //);
        //println!(
        //    "select_non_negative regions = {}",
        //    self.select_non_negative.len()
        //);

        // Check no plan needed.
        if goal_regs.is_superset_of(start_regs) {
            //println!("goal {goal_regs} is superset of start {start_regs}");
            return None;
        }

        // Check if start_regs and goal_regs are in the same non-negative region.
        for selx in self.select_non_negative.iter() {
            if selx.regions.is_superset_of(start_regs) && selx.regions.intersects(goal_regs) {
                //println!("{} and {} are in {}", start_regs, goal_regs, selx.regions);
                if let Some(plans) = self.make_plans2(
                    start_regs,
                    &selx.regions.intersection(goal_regs)?,
                    Some(&selx.regions),
                ) {
                    //println!("returning PlanStore {plans}");
                    assert!(plans.validate(start_regs, goal_regs));
                    return Some(plans);
                } else {
                    return None;
                }
            }
        }

        let mut ret_plan_store = PlanStore::new(vec![]);

        // Init current start.
        let mut cur_start = start_regs.clone();

        // Check if start regs are currently in a negative region.
        let mut in_negative = false;
        for selx in self.select_negative.iter() {
            if selx.regions.is_superset_of(start_regs) {
                in_negative = true;
                break;
            }
        }

        // Make plan to move start to a non-negative SelectRegion, if needed.
        // Only a one-bit change should be needed, unless there are overlapping negative SelectRegions.
        if in_negative {
            // Find closest non-negative SelectRegions, if any.
            let mut min_distance = usize::MAX;
            let mut targets = Vec::<&SelectRegions>::new();
            for regsx in self.select_non_negative.iter() {
                let dist = start_regs.distance(&regsx.regions);
                if dist < min_distance {
                    min_distance = dist;
                    targets = Vec::<&SelectRegions>::new();
                }
                if dist == min_distance {
                    targets.push(regsx);
                }
            } // next regsx
            if targets.is_empty() {
                return None;
            }
            // Choose a close non-negative SelectRegion to move to.
            let near_pos_regs = targets[rand::thread_rng().gen_range(0..targets.len())];

            // Try to plan move to the selected SelectRegion.
            if let Some(plans) = self.make_plans2(&cur_start, &near_pos_regs.regions, None) {
                // Adjust new start regions.
                cur_start = start_regs.clone();
                for planx in plans.iter() {
                    if planx.is_not_empty() {
                        cur_start[planx.dom_id] = planx.result_region().clone();
                    }
                }
                //println!("First plan {}", plans);
                ret_plan_store.append(plans);
                //println!("cur_start {cur_start}");
            } else {
                return None;
            }
        } // end in_negative.

        // Now, use local cur_start, instead of argument start_regs.

        // Init last PlanStore, may end up being empty.
        let mut last_plan_store = PlanStore::new(vec![]);

        // Init current goal.
        let mut cur_goal = goal_regs.clone();

        // Check if goal_regs are in a negative SelectRegion.
        let mut in_negative = false;
        for selx in self.select_negative.iter() {
            if selx.regions.is_superset_of(goal_regs) {
                in_negative = true;
                break;
            }
        }

        // Make plan to move goal from a non-negative region, if needed.
        if in_negative {
            // Find closest non-negative SelectRegions, if any.
            let mut min_distance = usize::MAX;
            let mut targets = Vec::<&SelectRegions>::new();
            for regsx in self.select_non_negative.iter() {
                let dist = goal_regs.distance(&regsx.regions);
                if dist < min_distance {
                    min_distance = dist;
                    targets = Vec::<&SelectRegions>::new();
                }
                if dist == min_distance {
                    targets.push(regsx);
                }
            } // next regsx
            if targets.is_empty() {
                return None;
            }
            // Choose a close non-negative SelectRegion to move to.
            let near_pos_regs = targets[rand::thread_rng().gen_range(0..targets.len())];

            // Calc closest regions in the chosen SelectRegion, congruent to the goal_regs.
            let goal_regs_pos = cur_goal.translate_to(&near_pos_regs.regions);

            // Try to plan move to from the selected SelectRegion, to the goal.
            if let Some(plans) = self.make_plans2(&goal_regs_pos, &cur_goal, None) {
                // Save plans to goal.
                last_plan_store = plans;

                // Adjust goal_regs.
                cur_goal = goal_regs.clone();
                for planx in last_plan_store.iter() {
                    if planx.is_not_empty() {
                        cur_goal[planx.dom_id] = planx.initial_region().clone();
                    }
                }
                //println!("Last plan {}", last_plan_store);
                //println!("cur_goal {cur_goal}");
            } else {
                return None;
            }
        } // end in_negative.

        // Check if no further plan needed.
        if cur_goal.is_superset_of(&cur_start) {
            if last_plan_store.is_not_empty() {
                ret_plan_store.append(last_plan_store);
            }
            //println!("returning PlanStore {}", tools::vec_string(&ret_plan_store));
            assert!(ret_plan_store.validate(start_regs, goal_regs));
            return Some(ret_plan_store);
        }

        // Check if cur_start and cur_goal are in the same non-negative region.
        for selx in self.select_non_negative.iter() {
            if selx.regions.is_superset_of(&cur_start) && selx.regions.is_superset_of(&cur_goal) {
                //println!("{} and {} are in {}", cur_start, cur_goal, selx.regions);
                let plans = self.make_plans2(&cur_start, &cur_goal, Some(&selx.regions))?;
                //println!("returning PlanStore {plans}");
                ret_plan_store.append(plans);
                if last_plan_store.is_not_empty() {
                    ret_plan_store.append(last_plan_store);
                }
                assert!(ret_plan_store.validate(start_regs, goal_regs));
                return Some(ret_plan_store);
            }
        }

        // Now, use local cur_goal, instead of argument goal_regs.

        // Try generating multiple paths.
        let mut mid_paths = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| self.avoid_negative_select_regions3(&cur_start, &cur_goal))
            .collect::<Vec<Vec<&RegionStoreCorr>>>();

        if mid_paths.is_empty() {
            return None;
        }

        // Randomly choose a possible path.
        let pathx = mid_paths.swap_remove(rand::thread_rng().gen_range(0..mid_paths.len()));

        let mut cur_regs = cur_start.clone(); // Borrow checker thinks the above map is still in force?

        let mut mid_plans = PlanStore::new(vec![]);
        for inx in 1..(pathx.len() - 1) {
            if let Some(intx) = pathx[inx].intersection(pathx[inx + 1]) {
                //println!("{} intersects {} at {}", pathx[inx], pathx[inx + 1], intx);
                if let Some(plans) = self.make_plans2(&cur_regs, &intx, Some(pathx[inx])) {
                    //println!("  mid plans {}", plans);
                    cur_regs = plans.result_regions(&cur_regs);
                    //println!("cur_regs {cur_regs}");
                    mid_plans.append(plans);
                } else {
                    return None;
                }
            } else {
                panic!("{} does not intersect {} ?", pathx[inx], pathx[inx + 1]);
            }
        }
        if mid_plans.is_empty() {
            return None;
        }

        // Add mid_plans to existing start_plan_store, if any.
        ret_plan_store.append(mid_plans);

        // Add last_plan_store. if needed.
        if last_plan_store.is_not_empty() {
            ret_plan_store.append(last_plan_store);
        }

        //println!("returning PlanStore {}", tools::vec_string(&ret_plan_store));
        assert!(ret_plan_store.validate(start_regs, goal_regs));
        Some(ret_plan_store)
    } // end avoid_negative_select_regions2

    /// Return a series of intersecting, non-negative SelectRegions, to
    /// guide a path between start and goal, without passing through any
    /// negative SelectRegions.
    fn avoid_negative_select_regions3<'a>(
        &'a self,
        cur_start: &'a RegionStoreCorr,
        cur_goal: &'a RegionStoreCorr,
    ) -> Option<Vec<&RegionStoreCorr>> {
        // Init start intersections vector.
        let mut start_ints = Vec::<&RegionStoreCorr>::new();
        start_ints.push(cur_start);

        // Randomly pick non-negative SelectRegions, to find one that is a superset of cur_start.
        let mut randpick = tools::RandomPick::new(self.select_non_negative.len());
        while let Some(inx) = randpick.pick() {
            let selx = &self.select_non_negative[inx];

            if selx.regions.is_superset_of(cur_start) {
                start_ints.push(&selx.regions);
                break;
            }
        }
        if start_ints.len() == 1 {
            return None;
        }

        // Init goal intersections vector.
        let mut goal_ints = Vec::<&RegionStoreCorr>::new();
        goal_ints.push(cur_goal);

        // Randomly pick non-negative SelectRegions, to find one that is a superset of cur_goal.
        let mut randpick = tools::RandomPick::new(self.select_non_negative.len());
        while let Some(inx) = randpick.pick() {
            let selx = &self.select_non_negative[inx];

            if selx.regions.is_superset_of(cur_goal) {
                goal_ints.push(&selx.regions);
                break;
            }
        }
        if goal_ints.len() == 1 {
            return None;
        }

        // Build up start and goal vectors, until the is an intersection between them, or
        // no more intersections.
        loop {
            // Check the last start_ints item against all goal_ints.
            let start_last = start_ints.last()?;

            // Keep track of goal items traversed, in case an intersection is found.
            let mut tmp_path = Vec::<&RegionStoreCorr>::new();

            for regs_gx in goal_ints.iter() {
                if regs_gx.intersects(start_last) {
                } else {
                    // If no intersection is found, the iter will exit, without returning a result.
                    tmp_path.push(regs_gx);
                    continue;
                }
                // Use current start_ints vector as the return vector.

                // Avoid two consecutive, equal, RegionStoreCorrs.
                if std::ptr::eq(*regs_gx, *start_last) {
                } else {
                    start_ints.push(regs_gx);
                }
                // Unwind successive goal path items.
                for regs_tx in tmp_path.iter().rev() {
                    start_ints.push(regs_tx);
                }
                //println!("path 1 {}", tools::vec_ref_string(&start_ints));
                return Some(start_ints);
            }

            // Check the last goal_ints item against all start_ints.
            let goal_last = goal_ints.last()?;

            // Save RegionStoreCorrs traversed, to use if/when an intersection is found.
            let mut tmp_path = Vec::<&RegionStoreCorr>::new();

            for regs_sx in start_ints.iter() {
                if regs_sx.intersects(goal_last) {
                } else {
                    // If no intersection is found, the iter will exit, without returning a result.
                    // tmp_path will be a waste.
                    tmp_path.push(regs_sx);
                    continue;
                }
                // Avoid two consecutive, equal, RegionStoreCorrs.
                if std::ptr::eq(regs_sx, goal_last) {
                } else {
                    tmp_path.push(regs_sx);
                }
                // Unwind successive goal path RegionStoreCorrs.
                for regs_gx in goal_ints.iter().rev() {
                    tmp_path.push(regs_gx);
                }
                //println!("path 2 {}", tools::vec_ref_string(&tmp_path));
                return Some(tmp_path);
            }

            // Get next layer of intersections for start_ints.
            let mut start_added = false;
            let start_last = &start_ints.last()?;

            // Randomly pick possible SelectRegions to test.
            let mut randpick = tools::RandomPick::new(self.select_non_negative.len());
            while let Some(inx) = randpick.pick() {
                let selx = &self.select_non_negative[inx];

                // A new RegionStoreCorr must intersect the last one in the vector.
                if start_last.intersects(&selx.regions) {
                } else {
                    continue;
                }
                // Avoid duplicating a vector item.
                if tools::vec_contains_ref(&start_ints, &selx.regions) {
                    continue;
                }
                // Avoid the case of multiple RegionStoreCorrs intersecting the start RegionStoreCorr,
                // one has already been chosen.
                if selx.regions.is_superset_of(cur_start) {
                    continue;
                }

                start_ints.push(&selx.regions);
                start_added = true;
                break;
            } // next pick.

            // Get next layer of intersections for goal_ints.
            let mut goal_added = false;
            let goal_last = &goal_ints.last()?;

            // Randomly pick possible SelectRegions to test.
            let mut randpick = tools::RandomPick::new(self.select_non_negative.len());
            while let Some(inx) = randpick.pick() {
                let selx = &self.select_non_negative[inx];

                // A new RegionStoreCorr must intersect the last one in the vector.
                if goal_last.intersects(&selx.regions) {
                } else {
                    continue;
                }
                // Avoid duplicating a vector item.
                if tools::vec_contains_ref(&goal_ints, &selx.regions) {
                    continue;
                }
                // Avoid the case of multiple RegionStoreCorrs intersecting the goal RegionStoreCorr,
                // one has already been chosen.
                if selx.regions.is_superset_of(cur_goal) {
                    continue;
                }

                goal_ints.push(&selx.regions);
                goal_added = true;
                break;
            } // next pick.

            // Check if done, with no result.
            if start_added || goal_added {
            } else {
                return None;
            }
        } // end loop
    } // end avoid_negative_select_regions3

    /// Return a String representation of a DomainStore.
    fn formatted_string(&self) -> String {
        let mut rc_str = String::from("[");

        for (inx, mskx) in self.domains.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&mskx.to_string());
        }
        rc_str.push(']');

        rc_str
    }

    /// Return the total number of groups in all the domains.
    pub fn number_groups(&self) -> usize {
        let mut tot = 0;
        for domx in self.domains.iter() {
            tot += domx.number_groups();
        }
        tot
    }

    /// Return the total number of groups expected in all the domains.
    pub fn number_groups_expected(&self) -> usize {
        let mut tot = 0;
        for domx in self.domains.iter() {
            tot += domx.number_groups_expected();
        }
        tot
    }

    /// Print a plan step-by-step, indicating changes.
    pub fn print_plan_detail(&self, plan_str: &PlanStore) {
        self.print_plan_detail2(plan_str, &self.all_current_states());
    }

    /// Print a plan step-by-step, indicating changes.
    pub fn print_plan_detail2(&self, plan_str: &PlanStore, states: &StateStoreCorr) {
        let mut cur_states = states.clone();

        let mut step_num = 0;
        for planx in plan_str.iter() {
            if planx.is_empty() {
                continue;
            }

            println!("\n  Domain: {}, Plan:", planx.dom_id);
            for stepx in planx.iter() {
                let df = stepx.initial.diff_mask(&stepx.result);
                print!(
                    "    {} Action {:02} Group {} ",
                    &stepx.initial, &stepx.act_id, &stepx.group_reg
                );
                if step_num > 0 {
                    for sel_regx in self.select.iter() {
                        if sel_regx.value() < 0 && sel_regx.regions.is_superset_states(&cur_states)
                        {
                            print!(" in {:+}", sel_regx);
                        }
                    }
                }

                println!("\n    {}", df.str2());
                cur_states[planx.dom_id] = stepx.rule.result_state(&cur_states[planx.dom_id]);
                step_num += 1;
            } // next stepsx
            println!("    {}", cur_states[planx.dom_id]);
        } // next planx
    }

    /// Return the maximum possible regions.
    pub fn maximum_regions(&self) -> RegionStoreCorr {
        let mut ret_regs = RegionStoreCorr::new(Vec::<SomeRegion>::with_capacity(self.len()));
        for domx in self.domains.iter() {
            ret_regs.push(domx.max_poss_region.clone());
        }
        ret_regs
    }

    /// Convert a TargetStore to a RegionsStoreCorr.
    pub fn targetstore_to_regionstorecorr(&self, targ: &TargetStore) -> RegionStoreCorr {
        let mut regs = RegionStoreCorr::new(vec![]);
        for dom_id in 0..self.len() {
            let mut dom_targ: Option<&SomeRegion> = None;
            for targx in targ.iter() {
                if targx.dom_id == dom_id {
                    dom_targ = Some(&targx.region);
                    break;
                }
            }
            if let Some(regx) = dom_targ {
                regs.push(regx.clone());
            } else {
                regs.push(SomeRegion::new(vec![self[dom_id].cur_state.clone()]));
            }
        }
        regs
    }
} // end impl DomainStore

impl Index<usize> for DomainStore {
    type Output = SomeDomain;
    fn index(&self, i: usize) -> &SomeDomain {
        &self.domains[i]
    }
}

impl IndexMut<usize> for DomainStore {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.domains[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::rule::SomeRule;
    use crate::sample::SomeSample;
    use crate::step::{AltRuleHint, SomeStep};
    use crate::target::SomeTarget;

    /// Return the number of supersets of a StateStore
    fn number_supersets_of_states(select: &SelectRegionsStore, stas: &StateStoreCorr) -> usize {
        select
            .regionstores
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_states(stas)))
            .sum()
    }

    #[test]
    fn choose_a_plan() -> Result<(), String> {
        let ur_bits = SomeBits::new(4);

        let sta2 = SomeState::new(ur_bits.new_from_string("0x2")?);
        let sta7 = SomeState::new(ur_bits.new_from_string("0x7")?);
        let sta9 = SomeState::new(ur_bits.new_from_string("0x9")?);
        let stab = SomeState::new(ur_bits.new_from_string("0xb")?);
        let staf = SomeState::new(ur_bits.new_from_string("0xf")?);

        let grp_reg = SomeRegion::new(vec![sta2.clone()]);

        let stp27 = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new(sta2.clone(), sta7.clone())),
            AltRuleHint::NoAlt {},
            grp_reg.clone(),
        );
        let stp7f = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new(sta7.clone(), staf.clone())),
            AltRuleHint::NoAlt {},
            grp_reg.clone(),
        );
        let stp29 = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new(sta2.clone(), sta9.clone())),
            AltRuleHint::NoAlt {},
            grp_reg.clone(),
        );
        let stp2b = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new(sta2.clone(), stab.clone())),
            AltRuleHint::NoAlt {},
            grp_reg.clone(),
        );
        let stp9b = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new(sta9.clone(), stab.clone())),
            AltRuleHint::NoAlt {},
            grp_reg.clone(),
        );
        let stpbf = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new(stab.clone(), staf.clone())),
            AltRuleHint::NoAlt {},
            grp_reg.clone(),
        );

        // Init DomainStore. Domain.
        let mut dmxs = DomainStore::new();
        // Set up first domain.
        dmxs.add_domain(sta2.clone());

        // Set select region.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string("r01XX")?);
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        // Do SelectRegion calculations.
        dmxs.calc_select();

        // Make plan 2->7->F (rate -1)
        let plnstr1 = PlanStore::new(vec![SomePlan::new(0, vec![stp27, stp7f])]);
        assert!(dmxs.rate_plans(&plnstr1) == -1);

        // Make plan 2->9->b->f (rate 0)
        let plnstr2 = PlanStore::new(vec![(SomePlan::new(0, vec![stp29, stp9b, stpbf.clone()]))]);
        assert!(dmxs.rate_plans(&plnstr2) == 0);

        // Make plan 2->B->F (rate 0)
        let plnstr3 = PlanStore::new(vec![(SomePlan::new(0, vec![stp2b, stpbf]))]);
        assert!(dmxs.rate_plans(&plnstr3) == 0);

        let start_regs = dmxs.all_current_regions();

        let inx = dmxs.choose_a_plan2(&vec![&plnstr1, &plnstr2, &plnstr3], &start_regs);
        println!("inx = {inx}");
        assert!(inx == 2);

        let inx = dmxs.choose_a_plan2(&vec![&plnstr3, &plnstr2.clone(), &plnstr1], &start_regs);
        println!("inx = {inx}");
        assert!(inx == 0);

        let inx = dmxs.choose_a_plan2(&vec![&plnstr1, &plnstr3, &plnstr2], &start_regs);
        println!("inx = {inx}");
        assert!(inx == 1);

        let inx = dmxs.choose_a_plan2(&vec![&plnstr3, &plnstr1, &plnstr2], &start_regs);
        println!("inx = {inx}");
        assert!(inx == 0);

        // Check for random choice of two possible plans.
        let mut zero = false;
        let mut five = false;
        let plan_vec = vec![&plnstr3, &plnstr1, &plnstr1, &plnstr2, &plnstr2, &plnstr3];
        for _ in 0..100 {
            let inx = dmxs.choose_a_plan2(&plan_vec, &start_regs);
            println!("inx = {inx}");
            if inx == 0 {
                zero = true;
                if five {
                    break;
                }
            } else if inx == 5 {
                five = true;
                if zero {
                    break;
                }
            }
        }
        assert!(zero && five);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn test_conversion() -> Result<(), String> {
        // Init DomainStore. Domain.
        let mut dmxs = DomainStore::new();
        // Set up first domain.
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let s5 = dmxs[0].state_from_string("s0b0101")?;
        dmxs[0].set_state(&s5);

        // Set up second domain.
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let sf = dmxs[0].state_from_string("s0b1111")?;
        dmxs[0].set_state(&sf);

        let mut targs = TargetStore::new(vec![]);
        let rsc = dmxs.targetstore_to_regionstorecorr(&targs);
        println!("rsc from empty Targetstore: {rsc}");
        assert!(rsc.len() == 2);
        assert!(format!("{rsc}") == "[r1111, r0000]");

        targs.push(SomeTarget::new(
            1,
            SomeRegion::new(vec![s5.clone(), sf.clone()]),
        ));
        let rsc = dmxs.targetstore_to_regionstorecorr(&targs);
        println!("rsc from Targetstore with region from domain 1: {rsc}");
        assert!(rsc.len() == 2);
        assert!(format!("{rsc}") == "[r1111, rx1x1]");

        targs.push(SomeTarget::new(
            0,
            SomeRegion::new(vec![s5.clone(), dmxs[0].state_from_string("s0b0100")?]),
        ));
        let rsc = dmxs.targetstore_to_regionstorecorr(&targs);
        println!("rsc from Targetstore with region from domain 1, 0: {rsc}");
        assert!(rsc.len() == 2);
        assert!(format!("{rsc}") == "[r010X, rx1x1]");

        Ok(())
    }

    #[test]
    /// Test case where positive regions the start and goal are in, intersect.
    fn avoidance1() -> Result<(), String> {
        // Init DomainStore. Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        let domx = &mut dmxs[0];

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action(vec![]);
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action(vec![]);
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r01X1")?);
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("rX101")?);
        dmxs.add_select(SelectRegions::new(regstr2, 0, 2));
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        dmxs[0].get_needs(); // set aggregate changes

        let start_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![sf.clone()])]);

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            println!(
                "Plan found: {} start {start_region} goal {goal_region}",
                planx
            );
            assert!(dmxs.rate_plans2(&planx, &start_region) == 0);
            //assert!(1 == 2);
            return Ok(());
        }
        Err("No plan found?".to_string())
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// but another region intersects both.
    fn avoidance2() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        let domx = &mut dmxs[0];

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action(vec![]);
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action(vec![]);
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r0101")?);
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("r1001")?);
        dmxs.add_select(SelectRegions::new(regstr2, 0, 1));
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            let rate = dmxs.rate_plans2(
                &planx,
                &RegionStoreCorr::new(vec![SomeRegion::new(vec![state1.clone()])]),
            );
            println!("Plan found: {} rate: {}", planx, rate);
            assert!(dmxs.rate_plans2(&planx, &start_region) == 0);
            //assert!(1 == 2);
            return Ok(());
        }
        Err("No plan found?".to_string())
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// and another region does not intersect both.
    fn avoidance3() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        let domx = &mut dmxs[0];

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action(vec![]);
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action(vec![]);
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r0x00")?);
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("rx100")?);
        dmxs.add_select(SelectRegions::new(regstr2, 0, 1));

        let mut regstr3 = RegionStoreCorr::with_capacity(1);
        regstr3.push(dmxs[0].region_from_string_pad_x("r01x1")?);
        dmxs.add_select(SelectRegions::new(regstr3, 0, 1));

        let mut regstr4 = RegionStoreCorr::with_capacity(1);
        regstr4.push(dmxs[0].region_from_string_pad_x("r10x1")?);
        dmxs.add_select(SelectRegions::new(regstr4, 0, 1));

        let mut regstr5 = RegionStoreCorr::with_capacity(1);
        regstr5.push(dmxs[0].region_from_string_pad_x("r101x")?);
        dmxs.add_select(SelectRegions::new(regstr5, 0, 1));
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            println!("Plan found: {}", planx);
            assert!(dmxs.rate_plans2(&planx, &start_region) == 0);
            //assert!(1 == 2);
            return Ok(());
        }

        Err("No plan found?".to_string())
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// and there is no path that does not cross a negative select region.
    fn avoidance4() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        let domx = &mut dmxs[0];

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action(vec![]);
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action(vec![]);
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r01xx")?);
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("r10xx")?);
        dmxs.add_select(SelectRegions::new(regstr2, 0, 1));
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            return Err(format!("Plan found {}?", planx));
        }
        Ok(())
    }

    #[test]
    /// Test case where start and goal regions are not in a non-negative region.
    fn avoidance5() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        let domx = &mut dmxs[0];

        let sf = domx.state_from_string("s0b1111")?;
        let s0 = domx.state_from_string("s0b0")?;

        // Set up action to change the first bit.
        domx.add_action(vec![]);
        let s1 = domx.state_from_string("s0b1")?;
        let se = domx.state_from_string("s0b1110")?;
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action(vec![]);
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("rxx0x")?);
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x1")?;
        dmxs[0].set_state(&state1);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![state1.clone()])]);
        let goal_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            println!("Plan found: {} rate {}", planx, dmxs.rate_plans(&planx));
            planx.str_terse();
        } else {
            return Err(format!("No plan ?"));
        }
        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn all_current_states() -> Result<(), String> {
        // Init DomainStore, Domains.
        // Domain 0 uses 1 integer for bits.
        // Domain 1 uses 2 integers for bits.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        dmxs.add_domain(SomeState::new(SomeBits::new(16)));

        // Set state for domain 0, using 1 integer for bits.
        let init_state1 = dmxs[0].state_from_string("s0x12")?;
        dmxs[0].set_state(&init_state1);

        // Set state for domain 1, using 2 integers for bits.
        let init_state2 = dmxs[1].state_from_string("s0xabcd")?;
        dmxs[1].set_state(&init_state2);

        let all_states = dmxs.all_current_states();
        println!("all states {}", all_states);

        assert!(all_states.len() == 2);
        assert!(all_states[0] == init_state1);
        assert!(all_states[1] == init_state2);

        Ok(())
    }

    #[test]
    /// Test case using adjacent non-negative regions.
    fn avoidance6() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));

        // Add actions.
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);

        let sf = dmxs[0].state_from_string("s0b1111")?;
        let s0 = dmxs[0].state_from_string("s0b0")?;

        // Set up action to change the first bit.
        let s1 = dmxs[0].state_from_string("s0b0001")?;
        let se = dmxs[0].state_from_string("s0b1110")?;
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        let s2 = dmxs[0].state_from_string("s0b0010")?;
        let sd = dmxs[0].state_from_string("s0b1101")?;
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        let s4 = dmxs[0].state_from_string("s0b0100")?;
        let sb = dmxs[0].state_from_string("s0b1011")?;
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the fourth bit.
        let s8 = dmxs[0].state_from_string("s0b1000")?;
        let s7 = dmxs[0].state_from_string("s0b0111")?;
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Init aggregate needs.
        dmxs.get_needs();

        // Set select regions.

        // Set up dom 0 00XX dependent on dom 1 01XX.
        let mut regstr0 = RegionStoreCorr::with_capacity(1);
        regstr0.push(dmxs[0].region_from_string_pad_x("r1100")?);
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Set up dom 0 00XX dependent on dom 1 10XX.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r1011")?);
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));
        dmxs.calc_select();

        let s0 = dmxs[0].state_from_string("s0b0000")?;
        dmxs[0].set_state(&s0);

        let sd = dmxs[0].state_from_string("s0b1101")?;

        let start_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![s0.clone()])]);
        let goal_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            print!("Plans {}", plans);
            let rate = dmxs.rate_plans2(&plans, &start_region);
            print!(", rate {}", rate);
            println!(" ");
            assert!(rate == 0);
        } else {
            return Err(format!("No plan found?"));
        }
        Ok(())
    }

    #[test]
    /// Test case using two domains.
    fn avoidance7() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));

        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);

        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);

        let sf = dmxs[0].state_from_string("s0b1111")?;
        let s0 = dmxs[0].state_from_string("s0b0")?;

        // Set up action to change the first bit.
        let s1 = dmxs[0].state_from_string("s0b0001")?;
        let se = dmxs[0].state_from_string("s0b1110")?;
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        let s2 = dmxs[0].state_from_string("s0b0010")?;
        let sd = dmxs[0].state_from_string("s0b1101")?;
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        let s4 = dmxs[0].state_from_string("s0b0100")?;
        let sb = dmxs[0].state_from_string("s0b1011")?;
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the fourth bit.
        let s8 = dmxs[0].state_from_string("s0b1000")?;
        let s7 = dmxs[0].state_from_string("s0b0111")?;
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select region.
        let max_region = dmxs[1].region_from_string_pad_x("rxxxx")?;

        // Set up dom 0 negative regions.
        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r01x1")?);
        regstr0.push(max_region.clone());
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("rx101")?);
        regstr0.push(max_region.clone());
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Set up dom 1 negative regions.
        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(max_region.clone());
        regstr0.push(dmxs[1].region_from_string_pad_x("r011x")?);
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(max_region.clone());
        regstr0.push(dmxs[1].region_from_string_pad_x("rx111")?);
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s0 = dmxs[0].state_from_string("s0x0")?;
        dmxs[0].set_state(&s0);

        let s1 = dmxs[1].state_from_string("s0x1")?;
        dmxs[1].set_state(&s1);

        let sf = dmxs[0].state_from_string("s0xf")?;
        let se = dmxs[1].state_from_string("s0xe")?;

        let start_region = RegionStoreCorr::new(vec![
            SomeRegion::new(vec![s0.clone()]),
            SomeRegion::new(vec![s1.clone()]),
        ]);
        let goal_region = RegionStoreCorr::new(vec![
            SomeRegion::new(vec![sf.clone()]),
            SomeRegion::new(vec![se.clone()]),
        ]);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            print!("Plans {}", plans);
            assert!(plans.is_not_empty());
            let rate = dmxs.rate_plans2(&plans, &start_region);
            print!(", rate {}", rate);
            println!(" ");
            assert!(rate == 0);
        } else {
            return Err(format!("No plan found?"));
        }
        Ok(())
    }

    #[test]
    /// Test case using two domains, where a non-target domain must change first,
    /// due to the boolean AND relationship, with two non-maximum regions, in a negative SelectRegions instance.
    fn avoidance8() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));

        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);

        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);

        let sf = dmxs[0].state_from_string("s0b1111")?;
        let s0 = dmxs[0].state_from_string("s0b0")?;

        // Set up action to change the first bit.
        let s1 = dmxs[0].state_from_string("s0b0001")?;
        let se = dmxs[0].state_from_string("s0b1110")?;
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        let s2 = dmxs[0].state_from_string("s0b0010")?;
        let sd = dmxs[0].state_from_string("s0b1101")?;
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        let s4 = dmxs[0].state_from_string("s0b0100")?;
        let sb = dmxs[0].state_from_string("s0b1011")?;
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the fourth bit.
        let s8 = dmxs[0].state_from_string("s0b1000")?;
        let s7 = dmxs[0].state_from_string("s0b0111")?;
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set up negative regions.
        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r00xx")?);
        regstr0.push(dmxs[0].region_from_string_pad_x("rxx11")?);
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r11xx")?);
        regstr0.push(dmxs[0].region_from_string_pad_x("r01xx")?);
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = dmxs[0].state_from_string("s0b0101")?;
        dmxs[0].set_state(&s5);

        let s7 = dmxs[1].state_from_string("s0b0111")?;
        dmxs[1].set_state(&s7);

        let start_regions = RegionStoreCorr::new(vec![
            SomeRegion::new(vec![s5.clone()]),
            SomeRegion::new(vec![s7.clone()]),
        ]);

        let s9 = dmxs[0].state_from_string("s0b1001")?;

        let goal_regions = RegionStoreCorr::new(vec![
            SomeRegion::new(vec![s9.clone()]),
            SomeRegion::new(vec![s7.clone()]),
        ]);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_regions, &goal_regions) {
            print!("Plans {}", plans);
            let rate = dmxs.rate_plans2(&plans, &start_regions);
            print!(", rate {}", rate);
            println!(" ");
            assert!(rate == 0);
            let mut cur_states = dmxs.all_current_states();
            for planx in plans.iter() {
                if planx.is_empty() {
                    continue;
                }
                cur_states[planx.dom_id] = planx.result_state(&cur_states[planx.dom_id]);
            }
            assert!(goal_regions.is_superset_states(&cur_states));
        } else {
            return Err(format!("No plan found?"));
        }
        Ok(())
    }

    #[test]
    /// Test case using two domains, like avoidance8, but a way around traps.
    fn avoidance9() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));

        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);

        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);

        let sf = dmxs[0].state_from_string("s0b1111")?;
        let s0 = dmxs[0].state_from_string("s0b0")?;

        // Set up action to change the first bit.
        let s1 = dmxs[0].state_from_string("s0b0001")?;
        let se = dmxs[0].state_from_string("s0b1110")?;
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        let s2 = dmxs[0].state_from_string("s0b0010")?;
        let sd = dmxs[0].state_from_string("s0b1101")?;
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        let s4 = dmxs[0].state_from_string("s0b0100")?;
        let sb = dmxs[0].state_from_string("s0b1011")?;
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the fourth bit.
        let s8 = dmxs[0].state_from_string("s0b1000")?;
        let s7 = dmxs[0].state_from_string("s0b0111")?;
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set up negative regions.
        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r000x")?);
        regstr0.push(dmxs[0].region_from_string_pad_x("rxx11")?);
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r11x1")?);
        regstr0.push(dmxs[0].region_from_string_pad_x("r01xx")?);
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = dmxs[0].state_from_string("s0b0101")?;
        dmxs[0].set_state(&s5);

        let s7 = dmxs[1].state_from_string("s0b0111")?;
        dmxs[1].set_state(&s7);

        let start_regions = RegionStoreCorr::new(vec![
            SomeRegion::new(vec![s5.clone()]),
            SomeRegion::new(vec![s7.clone()]),
        ]);

        let s9 = dmxs[0].state_from_string("s0b1001")?;

        let goal_regions = RegionStoreCorr::new(vec![
            SomeRegion::new(vec![s9.clone()]),
            SomeRegion::new(vec![s7.clone()]),
        ]);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_regions, &goal_regions) {
            print!("Plans {}", plans);
            let rate = dmxs.rate_plans2(&plans, &start_regions);
            print!(", rate {}", rate);
            println!(" ");
            //assert!(rate == 0);
            //assert!(plans.len() == 1);
            //assert!(plans.dom_result(0, &goal_regions[0]));
        } else {
            return Err(format!("No plan found?"));
        }
        Ok(())
    }

    #[test]
    fn check_select() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));
        dmxs.add_domain(SomeState::new(SomeBits::new(16)));

        // Add action to domain 0.
        dmxs[0].add_action(vec![]);

        // Add action to domain 1.
        dmxs[1].add_action(vec![]);

        // Load select regions
        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(dmxs[0].region_from_string("r0x0x")?);
        regstr1.push(dmxs[1].region_from_string("rXXXXXX10_1XXX_XXXX")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(2);
        regstr2.push(dmxs[0].region_from_string("r0xx1")?);
        regstr2.push(dmxs[1].region_from_string("rXXXXXX10_1XXX_XXXX")?);

        let mut regstr3 = RegionStoreCorr::with_capacity(2);
        regstr3.push(dmxs[0].region_from_string("rx1x1")?);
        regstr3.push(dmxs[1].region_from_string("rXXXXXX10_1XXX_XXXX")?);

        let mut regstr4 = RegionStoreCorr::with_capacity(2);
        regstr4.push(dmxs[0].region_from_string("r1110")?);
        regstr4.push(dmxs[1].region_from_string("rXXXXXX10_1XXX_XXXX")?);

        // Add select region stores.
        dmxs.add_select(SelectRegions::new(regstr1, 1, 0));
        dmxs.add_select(SelectRegions::new(regstr2, 1, 0));
        dmxs.add_select(SelectRegions::new(regstr3, 1, 0));
        dmxs.add_select(SelectRegions::new(regstr4, 1, 0));
        dmxs.calc_select();

        // Set state for domain 0.
        let state1 = dmxs[0].state_from_string("s0x12")?;
        dmxs[0].set_state(&state1);

        // Set state for domain 1.
        let state2 = dmxs[1].state_from_string("s0xabcd")?;
        dmxs[1].set_state(&state2);

        dmxs.boredom = 0;
        dmxs.boredom_limit = 0;

        let num_sup = number_supersets_of_states(
            &dmxs.select,
            &StateStoreCorr::new(vec![state1.clone(), state2.clone()]),
        );
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

        if let Some(needx) = dmxs.check_select() {
            return Err(format!("\nCheck_select returns need? {}", needx));
        }

        println!(
            "\nBoredom level {} Boredom_limit {}",
            dmxs.boredom, dmxs.boredom_limit
        );
        assert!(dmxs.boredom == 1);
        assert!(dmxs.boredom_limit == 2);

        Ok(())
    }

    // Test exit_select_needs with two overlapping negative select regions.
    // from s0111 to s1000.
    #[test]
    fn test_exit_select_needs() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        let neg_reg1 = dmxs[0].region_from_string_pad_x("rX1XX")?;

        regstr1.push(neg_reg1.clone());

        // Add select regionstores.
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        let neg_reg2 = dmxs[0].region_from_string_pad_x("r1XX1")?;
        regstr1.push(neg_reg2.clone());

        // Add select regionstores.
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        // Set state for domain 0, using 1 integer for bits.
        let state1 = dmxs[0].state_from_string("s0xd")?;
        dmxs[0].set_state(&state1);

        // Finish select regions setup.
        dmxs.calc_select();
        println!("select regs: {}", dmxs.select.to_string());

        // Get exit needs.
        if let Some(nds) = dmxs.check_select() {
            println!("needs len {} {}", nds.len(), nds);
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

    #[test]
    fn calc_select() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(8)));

        // Add action to domain 0.
        dmxs[0].add_action(vec![]);

        // Load select regions
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string("rxx0x")?);

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string("r00x1")?);

        let mut regstr3 = RegionStoreCorr::with_capacity(1);
        regstr3.push(dmxs[0].region_from_string("r11x1")?);

        let mut regstr4 = RegionStoreCorr::with_capacity(1);
        regstr4.push(dmxs[0].region_from_string("r10x0")?);

        // Add select region stores.
        dmxs.add_select(SelectRegions::new(regstr1, 4, 0));
        dmxs.add_select(SelectRegions::new(regstr2, 0, 2));
        dmxs.add_select(SelectRegions::new(regstr3, 0, 4));
        dmxs.add_select(SelectRegions::new(regstr4, 0, 5));
        dmxs.calc_select();

        println!("\nPositive select:");
        for selx in dmxs.select_positive.iter() {
            println!("  {selx}, {:+}", selx.value());
        }
        assert!(dmxs.select_positive.len() == 5);

        println!("\nNegative select:");
        for selx in dmxs.select_negative.iter() {
            println!("  {selx}, {}", selx.value());
        }
        assert!(dmxs.select_negative.len() == 4);

        Ok(())
    }
}
