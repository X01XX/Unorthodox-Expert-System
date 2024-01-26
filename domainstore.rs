//! The DomainStore struct, a vector of SomeDomain structs.

use crate::change::SomeChange;
use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionstore::RegionStore;
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
    pub plans: Option<PlanStore>,
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

    /// Calculate non-negative regions for a domain, given all current states.
    /// Regions in a negative SelectRegions have an AND relationship, so the states of other domains
    /// need to be considered.
    /// If the current state is in a negative SelectRegions,
    /// and the select region for the given domain is the maximum region, the result will be empty,
    /// implying the need to change the state of another domain.
    ///
    /// The assume argument indicates the need to assume a given domain number is in a SelectRegions region.
    fn calc_non_negative_regions(
        &self,
        dom_id: usize,
        cur_states: &StateStoreCorr,
        assume: Option<usize>,
    ) -> RegionStore {
        let mut non_neg_regs = RegionStore::new(vec![self[dom_id].max_poss_region.clone()]);

        'next_selx: for (sel_num, selx) in self.select_negative.iter().enumerate() {
            // Check if any other domain is out of the SelectRegions.
            for (inx, regx) in selx.regions.iter().enumerate() {
                if inx == dom_id {
                    continue;
                }
                if let Some(iny) = assume {
                    if inx == iny {
                        continue;
                    }
                }
                if !regx.is_superset_of(&cur_states[inx]) {
                    continue 'next_selx;
                }
            } // next inx, regx

            // If the region for the given domain is the maximum region, done.
            // Changing the current state of another domain might solve this, in the above code.
            if selx.regions[dom_id].all_x() {
                return RegionStore::new(vec![]);
            }
            // Calc complement, intersection.
            if let Some(comps) = self[dom_id].get_complement(sel_num) {
                non_neg_regs = non_neg_regs.intersection(comps);
            } else {
                panic!("SNH");
            }
        } // next selx

        non_neg_regs
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
        //for inx in 0..(self.select.len() - 1) {
        //    for iny in (inx + 1)..self.select.len() {
        //        if self.select[inx].pos > 0 && self.select[iny].neg > 0 ||
        //           self.select[inx].neg > 0 && self.select[iny].pos > 0 {
        //               if self.select[inx].intersects(&self.select[iny]) {
        //                   println!("{} intersects {}", self.select[inx], self.select[iny]);
        //               }
        //        }
        //    }
        //}

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
            //println!(
            //    "  {} pos {} neg {} = {:+}",
            //    sely,
            //    sely.pos,
            //    sely.neg,
            //    sely.pos as isize - sely.neg as isize
            //);
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
        //for sely in self.select_negative.iter() {
        //    for selx in self.select.iter() {
        //        if selx.pos > 0 {
        //            if selx.intersects(sely) {
        //                println!("{sely} intersects primary {selx}");
        //            }
        //        }
        //    }
        //}

        println!("\nPositive Select Regions:");
        for selx in self.select_positive.iter() {
            println!("  {}", selx);
        }
        //for sely in self.select_positive.iter() {
        //    for selx in self.select.iter() {
        //        if selx.neg > 0 {
        //            if selx.intersects(sely) {
        //                println!("{sely} intersects {selx}");
        //            }
        //        }
        //    }
        //}

        // Add complement calculation to each domain.
        // There is a potential problem with HashMap used with SomeRegion:PartialEq, so
        // use SelectRegions number which should not be changed later.
        let mut comps = Vec::<(usize, usize, SomeRegion)>::new();
        for (sel_num, selx) in self.select_negative.iter().enumerate() {
            for (dom_id, regx) in selx.regions.iter().enumerate() {
                comps.push((sel_num, dom_id, regx.clone()));
            }
        }
        for (sel_num, dom_id, regx) in comps.iter() {
            self[*dom_id].add_complement(*sel_num, regx);
        }

        // Calc non-negative regions.
        let max_select =
            SelectRegionsStore::new(vec![SelectRegions::new(self.maximum_regions(), 0, 0)]);
        self.select_non_negative = max_select.subtract(&self.select_negative);

        println!("\nNon negative select Regions:");
        for selx in self.select_non_negative.iter() {
            println!("  {}", selx);
            //for sely in self.select.iter() {
            //    if selx.intersects(sely) {
            //        println!("{selx} intersects primary {sely}");
            //    }
            //}
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

            // println!(" max_rate = {max_rate}");

            // Form start region corresponding.
            //let all_states = self.all_current_states();
            let mut start_regs = RegionStoreCorr::with_capacity(self.len());
            for domx in self.domains.iter() {
                start_regs.push(SomeRegion::new(vec![domx.cur_state.clone()]));
            }

            // Check if negative regions can be avoided.
            //for ndinx in self.can_do.iter_mut() {
            for ndinx in 0..self.can_do.len() {
                // Form goal region corresponding
                let mut goal_regs = RegionStoreCorr::with_capacity(start_regs.len());
                for (dom_inx, domx) in self.domains.iter().enumerate() {
                    if let Some(regt) = self.needs[self.can_do[ndinx].inx]
                        .target()
                        .target_region(dom_inx)
                    {
                        goal_regs.push(regt.clone());
                    } else {
                        goal_regs.push(SomeRegion::new(vec![domx.cur_state.clone()]));
                    }
                }

                if let Some(planx) = self.avoid_negative_select_regions(&start_regs, &goal_regs) {
                    let rate = self
                        .select_negative
                        .rate_plans(&planx, &self.all_current_states());
                    //println!(" new rate {rate}");
                    if rate > max_rate {
                        println!(
                            "\nFor plan {}/{}, {} to {}, better plan found {}/{}",
                            self.can_do[ndinx].plans.as_ref().expect("SNH").str_terse(),
                            max_rate,
                            start_regs,
                            goal_regs,
                            planx.str_terse(),
                            rate
                        );
                        println!("First plan:");
                        self.print_plan_detail(self.can_do[ndinx].plans.as_ref().expect("SNH"));
                        println!("A better plan");
                        self.print_plan_detail(&planx);

                        self.can_do[ndinx].plans = Some(planx);
                        self.can_do[ndinx].rate = rate;
                    } else {
                        println!(
                            "\nFor plan {}/{}, better plan NOT found",
                            self.can_do[ndinx].plans.as_ref().expect("SNH").str_terse(),
                            max_rate,
                        );
                    }
                }
            }

            //for inxplnx in can_do {
            //    self.can_do.push(inxplnx);
            //}
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
            let mut plans = self.get_plans(targx.dom_id, &targx.region)?; // return None if any target cannot be reached.
            let inx = self.choose_a_plan(&plans);
            plans_per_target.push(plans.swap_remove(inx));
        } // next optx

        Some(plans_per_target)
    }

    /// Return a rate for a plan, based on the sum of values of select regions the plan passes through.
    fn rate_plans(&self, aplan: &PlanStore) -> isize {
        self.select_negative
            .rate_plans(aplan, &self.all_current_states())
    }

    /// Get plans to move to a goal region, choose a plan.
    pub fn get_plans(&self, dom_id: usize, goal_region: &SomeRegion) -> Option<Vec<SomePlan>> {
        //println!("domainstore: get_plans: dom {dom_id} goal {goal_region}");
        self.domains[dom_id].make_plans(goal_region)
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
            rates.push(self.select_negative.rate_plan(planx, &current_states));
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
        //println!("inx2 = {}  can_do2 = {}", inx2, can_do2[inx2]);

        let itmx = &self.can_do[min_len_inxplans[cd2_inx]];
        //println!("itmx.inx = {}", itmx.inx);

        let ndx = &self.needs[itmx.inx]; // get need using tuple index

        println!(
            "\nNeed chosen: {:2} {} {}",
            min_len_inxplans[cd2_inx],
            ndx,
            itmx.plans.as_ref().unwrap().str_terse()
        );

        min_len_inxplans[cd2_inx]
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

    /// Update counters for times_visited.
    pub fn update_times_visited(&mut self) {
        // Get all domain current states.
        let all_states = self.all_current_states();

        // Get the select regions the current state is in.
        for optregs in self.select.iter_mut() {
            if optregs.regions.is_superset_states_corr(&all_states) {
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
                        ndplnx.plans.as_ref().unwrap().str_terse(),
                        ndplnx.rate,
                    );
                } else {
                    println!(
                        "{:2} {} {}",
                        inx,
                        self.needs[ndplnx.inx],
                        ndplnx.plans.as_ref().unwrap().str_terse(),
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

        let Some(plans) = self.get_plans(dom_id, goal_region) else {
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

    // Find traps to further progess from start to goal.
    // Return traps, vec![(dom_id, vec![SelectRegions index, ...])]
    fn find_a_trap(
        &self,
        start_regs: &RegionStoreCorr,
        goal_regs: &RegionStoreCorr,
    ) -> Option<(usize, Vec<usize>)> {
        if self.len() == 1 {
            return None;
        }

        // Find negative SelectRegions affecting the change, and how they affect the change.
        // Assuming all actions change one bit at a time.
        // F surrounded by 7, D, E and B, cannot be reached using 1-bit change actions.

        // Union of the start and goal region, per domain.
        let mut unions = Vec::<SomeRegion>::with_capacity(self.len());

        // SelectRegions that have only one region that is not a superset of a start region.
        // (domain index, SelectRegion index)
        let mut traps = Vec::<(usize, usize)>::new();

        // Init memory, per domain.
        for dom_id in 0..self.len() {
            unions.push(start_regs[dom_id].union(&goal_regs[dom_id]));
        }

        // Load info, per SelectRegions.
        for (inx, selx) in self.select_negative.iter().enumerate() {
            for dom_id in 0..self.len() {
                if start_regs[dom_id] == goal_regs[dom_id] {
                    continue;
                }

                if !selx.regions[dom_id].intersects(&unions[dom_id]) {
                    continue;
                }

                if selx.regions[dom_id].is_superset_of(&start_regs[dom_id]) {
                    continue;
                }

                // Check if all other domains have current states in the SelectRegions.
                let mut all_other_in = true;
                for (iny, regx) in selx.regions.iter().enumerate() {
                    if iny == dom_id {
                        continue;
                    }
                    if regx.is_superset_of(&start_regs[iny]) {
                    } else {
                        all_other_in = false;
                        break;
                    }
                }
                if all_other_in {
                    traps.push((dom_id, inx));
                }
            } // next dom_id
        } // next inx, selx

        // Check for no traps.
        if traps.is_empty() {
            return None;
        }

        // Get all domain numbers in a trap.
        let mut dom_ids = Vec::<usize>::new();
        for trapx in traps.iter() {
            if dom_ids.contains(&trapx.0) {
            } else {
                dom_ids.push(trapx.0);
            }
        }

        // Aggregate SelectRegions for each domain.
        let mut traps2 = Vec::<(usize, Vec<usize>)>::with_capacity(dom_ids.len());
        for dom_id in dom_ids.iter() {
            let mut sel_inxs = Vec::<usize>::new();
            for (dom, sel_inx) in traps.iter() {
                if dom == dom_id {
                    sel_inxs.push(*sel_inx);
                }
            }
            traps2.push((*dom_id, sel_inxs));
        }

        // Try each trap.
        let mut rp1 = tools::RandomPick::new(traps2.len());
        while let Some(trap_inx) = rp1.pick() {
            let (dom_id, sel_inxs) = &traps2[trap_inx];

            let mut comp = RegionStore::new(vec![self[*dom_id].max_poss_region.clone()]);
            for sely in sel_inxs.iter() {
                comp =
                    comp.intersection(&self.select_negative[*sely].regions[*dom_id].complement());
            }

            let paths = self[*dom_id].find_paths_through_regions(
                &start_regs[*dom_id],
                &goal_regs[*dom_id],
                &comp,
            );

            if paths.is_empty() {
                //println!(
                //    "found trap, domain {} start {} goal {}",
                //    dom_id, start_regs[*dom_id], goal_regs[*dom_id]
                //);
                //for selx in sel_inxs.iter() {
                //    println!("   {}", self.select_negative[*selx]);
                //}
                // Cannot get around trap, return trap details.
                return Some((*dom_id, sel_inxs.to_vec()));
            }
        }
        None
    }

    /// Like Real Life, formulate a direct plan,
    /// notice there are some negative aspects,
    /// then try to form a plan that avoids the negative.
    fn avoid_negative_select_regions(
        &self,
        start_regs: &RegionStoreCorr,
        goal_regs: &RegionStoreCorr,
    ) -> Option<PlanStore> {
        //println!("avoid_negative_select_regions: starting: start {start_regs} goal: {goal_regs}");
        assert!(start_regs.len() == self.len());
        assert!(goal_regs.len() == self.len());

        // Check if currently in, or going to, a negative region.
        for selx in self.select_negative.iter() {
            if selx.regions.is_superset_of_corr(start_regs)
                || selx.regions.is_superset_of_corr(goal_regs)
            {
                return None;
            }
        }

        let mut first_plans = PlanStore::new(vec![]);

        let mut cur_start = start_regs.clone();

        for _ in 0..4 {
            if let Some((dom_id, sel_inxs)) = self.find_a_trap(&cur_start, goal_regs) {
                println!("\nfound trap dom {} sel_inxs {:?}", dom_id, sel_inxs);

                // Find fix, else fail.

                // See if other domains can change to disarm the trap.
                'next_dom: for domx in 0..self.len() {
                    if domx == dom_id {
                        continue;
                    }
                    let mut comp = RegionStore::new(vec![self[domx].max_poss_region.clone()]);
                    for sely in sel_inxs.iter() {
                        comp = comp
                            .intersection(&self.select_negative[*sely].regions[domx].complement());
                    }
                    // Find closest complement regions.
                    let mut min_dist = usize::MAX;
                    for compx in comp.iter() {
                        if compx.distance(&cur_start[domx]) < min_dist {
                            min_dist = compx.distance(&cur_start[domx]);
                        }
                    }
                    // Add closest complement regions.
                    let mut closest = vec![];
                    for compx in comp.iter() {
                        if compx.distance(&cur_start[domx]) == min_dist {
                            closest.push(compx.clone());
                        }
                    }

                    for compx in closest.iter() {
                        if let Some(mut plans) =
                            self[domx].make_plans2(&cur_start[domx], compx, None)
                        {
                            // Alter cur_start, add to PlanStore.
                            let rslt = plans[0].result_region().clone();
                            cur_start[domx] = rslt;
                            first_plans.push(plans.swap_remove(0));

                            break 'next_dom;
                        }
                    }
                } // next domx
            } else {
                // No more traps
                break;
            }
        } // end while
        if first_plans.is_not_empty() {
            if let Some(next_plans) = self.avoid_negative_select_regions2(&cur_start, goal_regs) {
                first_plans.append(next_plans);
                return Some(first_plans);
            } else {
                return None;
            }
        }
        self.avoid_negative_select_regions2(start_regs, goal_regs)
    }

    /// Return plans for a change from start to goal,
    /// without checking for traps.
    fn avoid_negative_select_regions2(
        &self,
        start_regs: &RegionStoreCorr,
        goal_regs: &RegionStoreCorr,
    ) -> Option<PlanStore> {
        //println!("avoid_negative_select_regions2: starting: start {start_regs} goal: {goal_regs}");

        // Outcome may depend on the order of the domains checked, so try a number of times.

        'try_again: for _ in 0..(self.len() * 2) {
            let mut start_states = StateStoreCorr::with_capacity(self.len());
            for regx in start_regs.iter() {
                start_states.push(regx.state1().clone());
            }

            let mut ret_plans = PlanStore::new(vec![]);

            // Split domains based on the goal being in a SelectRegions part, or not.
            let mut sr_out = vec![];
            let mut sr_in = vec![];
            for (dom_id, regx) in goal_regs.iter().enumerate() {
                if self.select_negative.any_intersection_dom(dom_id, regx) {
                    sr_in.push(dom_id);
                } else {
                    sr_out.push(dom_id);
                }
            }

            if sr_out.is_empty() {
            } else {
                let mut options = tools::RandomPick::new(sr_out.len());

                while let Some(sr_out_inx) = options.pick() {
                    let dom_id = sr_out[sr_out_inx];

                    if goal_regs[dom_id].is_superset_of(&start_regs[dom_id]) {
                        continue;
                    }

                    if let Some(aplan) = self.avoid_negative_select_regions2_dom(
                        dom_id,
                        &start_regs[dom_id],
                        &goal_regs[dom_id],
                        &start_states,
                    ) {
                        if aplan.is_not_empty() {
                            start_states[dom_id] = aplan.result_region().state1().clone();
                            ret_plans.push(aplan);
                        }
                    } else {
                        continue 'try_again;
                    }
                }
            }

            if sr_in.is_empty() {
            } else {
                let mut options = tools::RandomPick::new(sr_in.len());

                while let Some(sr_in_inx) = options.pick() {
                    let dom_id = sr_in[sr_in_inx];

                    if goal_regs[dom_id].is_superset_of(&start_regs[dom_id]) {
                        continue;
                    }

                    // Generate new all_states.
                    let mut start_states_ref = Vec::<&SomeState>::with_capacity(self.len());
                    for stax in start_states.iter() {
                        start_states_ref.push(stax);
                    }

                    if let Some(aplan) = self.avoid_negative_select_regions2_dom(
                        dom_id,
                        &start_regs[dom_id],
                        &goal_regs[dom_id],
                        &start_states,
                    ) {
                        if aplan.is_not_empty() {
                            start_states_ref[dom_id] = aplan.result_region().state1();
                            ret_plans.push(aplan);
                        }
                    } else {
                        continue 'try_again;
                    }
                }
            }

            if ret_plans.is_empty() {
                continue 'try_again;
            }
            return Some(ret_plans);
        }
        None
    } // end avoid_negative_select_regions2

    fn avoid_negative_select_regions2_dom(
        &self,
        dom_id: usize,
        start_reg: &SomeRegion,
        goal_reg: &SomeRegion,
        start_states: &StateStoreCorr,
    ) -> Option<SomePlan> {
        //println!("avoid_negative_select_regions2_dom: dom: {} start {} goal {}", dom_id, start_reg, goal_reg);

        let non_neg = self.calc_non_negative_regions(dom_id, start_states, Some(dom_id));
        //let non_neg = &self.select_non_negative;

        if non_neg.is_empty() {
            //println!("    No non neg found");
            return None;
        }

        // Find plans that avoid negative regions.
        let mut dom_plans = self[dom_id].plan_paths_through_regions(start_reg, goal_reg, &non_neg);
        if dom_plans.is_empty() {
            return None;
        }

        let inx = self.choose_a_plan(&dom_plans);

        assert!(goal_reg.is_superset_of(dom_plans[inx].result_region()));

        Some(dom_plans.swap_remove(inx))
    } // end avoid_negative_select_regions2_dom

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
        let mut cur_states = self.all_current_states();

        for planx in plan_str.iter() {
            if planx.is_empty() {
                continue;
            }

            println!("\nDomain: {}, Plan:", planx.dom_id);

            for (inx, stepx) in planx.iter().enumerate() {
                let df = stepx.initial.diff_mask(&stepx.result);
                print!(
                    "{} Action {:02} Group {} ",
                    &stepx.initial, &stepx.act_id, &stepx.group_reg
                );
                if inx > 0 {
                    for sel_regx in self.select_negative.iter() {
                        if sel_regx.regions.is_superset_states_corr(&cur_states) {
                            print!(" in {:+}", sel_regx);
                        }
                    }
                }
                println!("\n{}", df.str2());

                cur_states[planx.dom_id] = stepx.result.state1().clone();
            } // next steps
            println!("{}", planx.result_region());
        } // next planx
    }

    /// Testing...
    /// For plans for more than one domain,
    /// Where running each in order results it a negative rating,
    /// try splitting up the steps, like
    /// Domain 1, step 1.  Domain 3, step 1. Domain 1, step 2. Domain 2, step 1.  ....
    /// Do in a way similar to a random depth-first search.
    pub fn try_plans_step_by_step(&self, plans: &PlanStore) -> Option<PlanStore> {
        let mut work_vec: Vec<Vec<SomePlan>> = vec![];

        // Check for to few, non-empty plans.
        if plans.num_non_empty() < 2 {
            println!("Returning None");
            return None;
        }

        let base_rate = self.rate_plans(plans);
        println!("Base rate {base_rate}");
        if base_rate >= 0 {
            println!("Returning None");
            return None;
        }

        let mut num_choices = 0;
        for planx in plans.iter() {
            let mut vecy = Vec::<SomePlan>::with_capacity(planx.len());
            for stepx in planx.iter() {
                vecy.push(SomePlan::new(planx.dom_id, vec![stepx.clone()]));
                num_choices += 1;
            }
            if vecy.is_empty() {
            } else {
                work_vec.push(vecy);
            }
        }

        //println!("work_vec {:?} num_choices {num_choices}", work_vec);

        let mut plan_tuples = (0..5)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| self.try_plans_step_by_step2(&work_vec, base_rate, num_choices))
            .collect::<Vec<(isize, PlanStore)>>();
        println!("num returned {}", plan_tuples.len());

        if plan_tuples.is_empty() {
            println!("Returning None");
            return None;
        }

        // Look for max rate plans.
        let mut max_rate = base_rate;
        let mut inx = 0;

        for (num, (rate, _plans)) in plan_tuples.iter().enumerate() {
            if *rate > max_rate {
                max_rate = *rate;
                inx = num;
            }
        }

        println!("Returning {} rate {}", plan_tuples[inx].1, max_rate);

        Some(plan_tuples.remove(inx).1)
    }
    /// Try a random depth-first series of single-step plans, looking for a better rate.
    fn try_plans_step_by_step2(
        &self,
        work_vec: &Vec<Vec<SomePlan>>,
        base_rate: isize,
        num_choices: usize,
    ) -> Option<(isize, PlanStore)> {
        let mut work_vec2 = Vec::<Vec<&SomePlan>>::with_capacity(work_vec.len());
        for vecx in work_vec.iter() {
            let mut vecy = Vec::<&SomePlan>::with_capacity(vecx.len());
            for planx in vecx.iter() {
                vecy.push(planx);
            }
            work_vec2.push(vecy);
        }
        //println!("work_vec2 {:?}", work_vec2);

        let mut seq = PlanStore::new(Vec::<SomePlan>::with_capacity(num_choices));

        while !work_vec2.is_empty() {
            let opt_next = if work_vec.len() > 1 {
                rand::thread_rng().gen_range(0..work_vec2.len())
            } else {
                0
            };

            let choice = work_vec2[opt_next].remove(0);

            if work_vec2[opt_next].is_empty() {
                work_vec2.remove(opt_next);
            }
            //println!("opt_next {opt_next} work_vec2 {:?} choice {choice}", work_vec2);

            seq.push(choice.clone());
            if self.rate_plans(&seq) <= base_rate {
                return None;
            }
        }
        let rate = self.rate_plans(&seq);
        //println!("plans {} rate {}", seq, rate);
        Some((rate, seq))
    }

    /// Return the maximum possible regions.
    pub fn maximum_regions(&self) -> RegionStoreCorr {
        let mut ret_regs = RegionStoreCorr::new(Vec::<SomeRegion>::with_capacity(self.len()));
        for domx in self.domains.iter() {
            ret_regs.push(domx.max_poss_region.clone());
        }
        ret_regs
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
    use crate::rulestore::RuleStore;
    use crate::sample::SomeSample;
    use crate::target::SomeTarget;

    /// Return the number of supersets of a StateStore
    fn number_supersets_of_states(select: &SelectRegionsStore, stas: &StateStoreCorr) -> usize {
        select
            .regionstores
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_states_corr(stas)))
            .sum()
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

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r01X1").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("rX101").expect("SNH"));
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
            assert!(
                dmxs.select
                    .rate_plans(&planx, &StateStoreCorr::new(vec![state1]))
                    == 0
            );

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

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r0101").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("r1001").expect("SNH"));
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
            let rate = dmxs
                .select
                .rate_plans(&planx, &StateStoreCorr::new(vec![state1.clone()]));
            println!("Plan found: {} rate: {}", planx, rate);
            assert!(
                dmxs.select
                    .rate_plans(&planx, &StateStoreCorr::new(vec![state1]))
                    == 0
            );
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

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r0x00").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("rx100").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr2, 0, 1));

        let mut regstr3 = RegionStoreCorr::with_capacity(1);
        regstr3.push(dmxs[0].region_from_string_pad_x("r01x1").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr3, 0, 1));

        let mut regstr4 = RegionStoreCorr::with_capacity(1);
        regstr4.push(dmxs[0].region_from_string_pad_x("r10x1").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr4, 0, 1));

        let mut regstr5 = RegionStoreCorr::with_capacity(1);
        regstr5.push(dmxs[0].region_from_string_pad_x("r101x").expect("SNH"));
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
            assert!(
                dmxs.select
                    .rate_plans(&planx, &StateStoreCorr::new(vec![state1]))
                    == 0
            );
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

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r01xx").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStoreCorr::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("r10xx").expect("SNH"));
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

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Set select regions.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("rxx0x").expect("SNH"));
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
            println!("Plan found: {}", planx);
            return Err(format!("Plan found ?"));
        }
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
        regstr0.push(dmxs[0].region_from_string_pad_x("r1100").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Set up dom 0 00XX dependent on dom 1 10XX.
        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r1011").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));
        dmxs.calc_select();

        let s0 = dmxs[0].state_from_string("s0b0000").expect("SNH");
        dmxs[0].set_state(&s0);

        let sd = dmxs[0].state_from_string("s0b1101")?;

        let start_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![s0.clone()])]);
        let goal_region = RegionStoreCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        let all_states = dmxs.all_current_states();

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            print!("Plans {}", plans);
            let rate = dmxs.select.rate_plans(&plans, &all_states);
            print!(", rate {}", rate);
            println!(" ");
            assert!(rate == 0);
            assert!(plans.len() == 1);
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
        let max_region = dmxs[1].region_from_string_pad_x("rxxxx").expect("SNH");

        // Set up dom 0 negative regions.
        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r01x1").expect("SNH"));
        regstr0.push(max_region.clone());
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("rx101").expect("SNH"));
        regstr0.push(max_region.clone());
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Set up dom 1 negative regions.
        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(max_region.clone());
        regstr0.push(dmxs[1].region_from_string_pad_x("r011x").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(max_region.clone());
        regstr0.push(dmxs[1].region_from_string_pad_x("rx111").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s0 = dmxs[0].state_from_string("s0x0").expect("SNH");
        dmxs[0].set_state(&s0);

        let s1 = dmxs[1].state_from_string("s0x1").expect("SNH");
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

        let all_states = dmxs.all_current_states();

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_region, &goal_region) {
            print!("Plans {}", plans);
            let rate = dmxs.select.rate_plans(&plans, &all_states);
            print!(", rate {}", rate);
            println!(" ");
            assert!(rate == 0);
            assert!(plans.len() == 2);
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
        regstr0.push(dmxs[0].region_from_string_pad_x("r00xx").expect("SNH"));
        regstr0.push(dmxs[0].region_from_string_pad_x("rxx11").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r11xx").expect("SNH"));
        regstr0.push(dmxs[0].region_from_string_pad_x("r01xx").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = dmxs[0].state_from_string("s0b0101").expect("SNH");
        dmxs[0].set_state(&s5);

        let s7 = dmxs[1].state_from_string("s0b0111").expect("SNH");
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

        let all_states = dmxs.all_current_states();

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_regions, &goal_regions) {
            print!("Plans {}", plans);
            let rate = dmxs.select.rate_plans(&plans, &all_states);
            print!(", rate {}", rate);
            println!(" ");
            assert!(rate == 0);
            assert!(plans.len() == 3);
            // Domain 1, move 7 out of negative select regions.
            assert!(plans[0].dom_id == 1);
            // Domain 0, move 5 to 9.
            assert!(plans[1].dom_id == 0);
            // Domain 1, move state from the first step, back to 7.
            assert!(plans[2].dom_id == 1);
            assert!(plans.dom_result(0, &goal_regions[0]));
            assert!(plans.dom_result(1, &goal_regions[1]));
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
        regstr0.push(dmxs[0].region_from_string_pad_x("r000x").expect("SNH"));
        regstr0.push(dmxs[0].region_from_string_pad_x("rxx11").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r11x1").expect("SNH"));
        regstr0.push(dmxs[0].region_from_string_pad_x("r01xx").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = dmxs[0].state_from_string("s0b0101").expect("SNH");
        dmxs[0].set_state(&s5);

        let s7 = dmxs[1].state_from_string("s0b0111").expect("SNH");
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

        let all_states = dmxs.all_current_states();

        // Try making plans.
        if let Some(plans) = dmxs.avoid_negative_select_regions(&start_regions, &goal_regions) {
            print!("Plans {}", plans);
            let rate = dmxs.select.rate_plans(&plans, &all_states);
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
    /// Test case using two domains, where running plans traverses a negative region.
    /// Domain 0, 1 -> D.
    /// Domain 1, F -> 3.
    fn avoidance10() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));

        // Set up domain 0, action 0, action response and groups.
        let act0 = 0;

        let cng0: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX/XX/XX/Xx")
            .expect("SNH")])];

        dmxs[0].add_action(cng0);
        let s0 = dmxs[0].state_from_string("s0b0000")?;
        dmxs[0].set_cur_state(s0.clone());
        dmxs[0].take_action_arbitrary(act0);
        let sf = dmxs[0].state_from_string("s0b1111")?;
        dmxs[0].set_cur_state(sf.clone());
        dmxs[0].take_action_arbitrary(act0);

        // Set up domain 0, action 1, action response and groups.
        let act1 = 1;

        let cng1: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX/XX/Xx/XX")
            .expect("SNH")])];

        dmxs[0].add_action(cng1);
        dmxs[0].set_cur_state(s0.clone());
        dmxs[0].take_action_arbitrary(act1);
        dmxs[0].set_cur_state(sf.clone());
        dmxs[0].take_action_arbitrary(act1);

        // Set up domain 0, action 2, action response and groups.
        let act2 = 2;

        let cng2: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[0]
            .rule_from_string("XX/Xx/XX/XX")
            .expect("SNH")])];

        dmxs[0].add_action(cng2);
        dmxs[0].set_cur_state(s0.clone());
        dmxs[0].take_action_arbitrary(act2);
        dmxs[0].set_cur_state(sf.clone());
        dmxs[0].take_action_arbitrary(act2);

        // Set up domain 0, action 3, action response and groups.
        let act3 = 3;

        let cng3: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[0]
            .rule_from_string("Xx/XX/XX/XX")
            .expect("SNH")])];

        dmxs[0].add_action(cng3);
        dmxs[0].set_cur_state(s0.clone());
        dmxs[0].take_action_arbitrary(act3);
        dmxs[0].set_cur_state(sf.clone());
        dmxs[0].take_action_arbitrary(act3);

        // Set up domain 1 actions.

        // Set up domain 1, action 0, action response and groups.
        let act0 = 0;

        let cng0: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
            .rule_from_string("XX/XX/XX/Xx")
            .expect("SNH")])];

        dmxs[1].add_action(cng0);
        let s0 = dmxs[1].state_from_string("s0b0000")?;
        dmxs[1].set_cur_state(s0.clone());
        dmxs[1].take_action_arbitrary(act0);
        let sf = dmxs[1].state_from_string("s0b1111")?;
        dmxs[1].set_cur_state(sf.clone());
        dmxs[1].take_action_arbitrary(act0);

        // Set up domain 1, action 1, action response and groups.
        let act1 = 1;

        let cng1: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
            .rule_from_string("XX/XX/Xx/XX")
            .expect("SNH")])];

        dmxs[1].add_action(cng1);
        dmxs[1].set_cur_state(s0.clone());
        dmxs[1].take_action_arbitrary(act1);
        dmxs[1].set_cur_state(sf.clone());
        dmxs[1].take_action_arbitrary(act1);

        // Set up domain 1, action 2, action response and groups.
        let act2 = 2;

        let cng2: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
            .rule_from_string("XX/Xx/XX/XX")
            .expect("SNH")])];

        dmxs[1].add_action(cng2);
        dmxs[1].set_cur_state(s0.clone());
        dmxs[1].take_action_arbitrary(act2);
        dmxs[1].set_cur_state(sf.clone());
        dmxs[1].take_action_arbitrary(act2);

        // Set up domain 1, action 3, action response and groups.
        let act3 = 3;

        let cng3: Vec<RuleStore> = vec![RuleStore::new(vec![dmxs[1]
            .rule_from_string("Xx/XX/XX/XX")
            .expect("SNH")])];

        dmxs[1].add_action(cng3);
        dmxs[1].set_cur_state(s0.clone());
        dmxs[1].take_action_arbitrary(act3);
        dmxs[1].set_cur_state(sf.clone());
        dmxs[1].take_action_arbitrary(act3);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Set up negative regions.
        let mut regstr0 = RegionStoreCorr::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r00xx").expect("SNH"));
        regstr0.push(dmxs[0].region_from_string_pad_x("r00xx").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr1 = RegionStoreCorr::with_capacity(2);
        regstr1.push(dmxs[0].region_from_string_pad_x("r11xx").expect("SNH"));
        regstr1.push(dmxs[0].region_from_string_pad_x("r11xx").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        // Set domain 0 current state.
        let s1 = dmxs[0].state_from_string("s0b0001")?;
        let sd = dmxs[0].state_from_string("s0b1101")?;
        dmxs[0].set_state(&s1);

        // Set domain 1 current state.
        let s3 = dmxs[1].state_from_string("s0b0011")?;
        let sf = dmxs[1].state_from_string("s0b1111")?;
        dmxs[1].set_state(&sf);

        let targets = TargetStore::new(vec![
            SomeTarget::new(0, SomeRegion::new(vec![sd.clone()])),
            SomeTarget::new(1, SomeRegion::new(vec![s3.clone()])),
        ]);

        if let Some(plans) = dmxs.make_plans(&targets) {
            println!("Number plans {}", plans.len());
            for planx in plans.iter() {
                println!("plan {}", planx);
            }
            let rate = dmxs.rate_plans(&plans);
            if rate < 0 {
                println!("rate {rate}");
                if let Some(plans) = dmxs.try_plans_step_by_step(&plans) {
                    dmxs.run_plan_store(&plans);
                    assert!(dmxs[0].cur_state == sd);
                    assert!(dmxs[1].cur_state == s3);
                } else {
                    return Err("Secondary plans not found".to_string());
                }
            }
        } else {
            return Err("Initial plans not found".to_string());
        }

        //assert!(1 == 2);
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
        let neg_reg1 = dmxs[0].region_from_string_pad_x("rX1XX").expect("SNH");

        regstr1.push(neg_reg1.clone());

        // Add select regionstores.
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr1 = RegionStoreCorr::with_capacity(1);
        let neg_reg2 = dmxs[0].region_from_string_pad_x("r1XX1").expect("SNH");
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
