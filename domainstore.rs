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
    /// Zero, or more, select regions that may have poitive, or negative, value.
    /// They may overlap.
    pub select: SelectRegionsStore,
    /// Positive regions, not overlapped by negative >= value regions.
    /// These tend to be goals.
    pub select_positive: SelectRegionsStore,
    /// Negative regions, value regions.
    /// These tend to be places to avoid in planning a path to a goal.
    pub select_negative: SelectRegionsStore,
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
            select: SelectRegionsStore::new(vec![]),
            select_negative: SelectRegionsStore::new(vec![]),
            select_positive: SelectRegionsStore::new(vec![]),
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
        debug_assert!(selx.len() == self.avec.len());

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
        for (inx, dmx) in self.avec.iter().enumerate() {
            if selx[inx].state1().num_bits() != dmx.cur_state.num_bits() {
                panic!("reg {} bad number ints for domain {}", selx[inx], inx);
            }
            if allx && !selx[inx].all_x() {
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
    fn calc_non_negative_regions(&self, dom_num: usize, cur_states: &[&SomeState]) -> RegionStore {
        let mut non_neg_regs = RegionStore::new(vec![self[dom_num].max_region.clone()]);

        'next_selx: for (sel_num, selx) in self.select_negative.iter().enumerate() {
            // Check if any other domain is out of the SelectRegions.
            for (inx, regx) in selx.regions.iter().enumerate() {
                if inx == dom_num {
                    continue;
                }
                if !regx.is_superset_of_state(cur_states[inx]) {
                    continue 'next_selx;
                }
            } // next inx, regx

            // If the region for the given domain is the maximum region, done.
            // Changing the current state of another domain might solve this, in the above code.
            if selx.regions[dom_num].all_x() {
                return RegionStore::new(vec![]);
            }
            // Calc complement, intersection.
            if let Some(comps) = self[dom_num].get_complement(sel_num) {
                non_neg_regs = non_neg_regs.intersection(comps).expect("SNH");
            } else {
                panic!("SNH");
            }
        } // next selx

        non_neg_regs
    }

    /// Calculate parts of select regions, in case of any overlaps.
    pub fn calc_select(&mut self) {
        let mut select_neg = Vec::<SelectRegions>::new();
        println!("\nSelect Regions:");
        for selx in self.select.iter() {
            println!("  {}", selx);
            if selx.neg > selx.pos {
                select_neg.push(selx.clone());
            }
        }
        // Add complement calculation to each domain.
        // There is a potential problem with HashMap used with SomeRegion:PartialEq, so
        // use SelectRegions number which should not be changed later.
        for (sel_num, selx) in select_neg.iter().enumerate() {
            for (dom_num, regx) in selx.regions.iter().enumerate() {
                self[dom_num].add_complement(sel_num, regx);
            }
        }

        let subs: Vec<RegionStore> = self.select.split_to_subsets();
        //println!("subs {} {}", tools::vec_string(&subs), subs.len());

        // Check each subset is subset of at least one SelectRegions.
        // And not a partial subset of any SelectRegion.
        let mut subselect_pos = Vec::<SelectRegions>::new();

        //println!("\nSelectRegions superset of each subset");
        for subx in subs.iter() {
            //println!("sub {}", subx);
            let mut count = 0;
            let mut pos: usize = 0;
            let mut neg: usize = 0;
            for selx in self.select.iter() {
                if subx.is_subset_of_corr(&selx.regions) {
                    //println!("   sup is {}", selx);
                    count += 1;
                    if selx.value() < 1 {
                        neg += selx.neg;
                    } else {
                        pos += selx.pos;
                    }
                } else if subx.intersects_corr(&selx.regions) {
                    panic!("   intersects ?? {}", selx);
                }
            }
            if count == 0 {
                panic!("    Not subset anything?");
            }
            // Store non-zero value subsets.
            if pos > neg {
                subselect_pos.push(SelectRegions::new(subx.clone(), pos, neg));
            }
            //println!("   value: pos {pos} neg {neg}");
        }
        self.select_positive = SelectRegionsStore::new(subselect_pos);
        self.select_negative = SelectRegionsStore::new(select_neg);

        // Check the subsets of each SelectRegion.
        //println!("\nSubsets of each SelectRegion");
        for selx in self.select.iter() {
            //println!("selx {}", selx);
            let mut subs_of = Vec::<RegionStore>::new();
            for subx in subs.iter() {
                if subx.is_subset_of_corr(&selx.regions) {
                    //println!("   sub is {}", subx);
                    subs_of.push(subx.clone());
                }
            }
            // Check subs fully account for the SelectRegions.
            if subs_of.is_empty() {
                panic!("No subs?");
            } else if subs_of.len() == 1 {
                if !subs_of[0].eq_corr(&selx.regions) {
                    panic!("NE?");
                }
            } else {
                // Check that each region of the superset SelectRegions is fully
                // used by the subsets.
                for (inx, regx) in selx.regions.iter().enumerate() {
                    let mut regx_left = RegionStore::new(vec![regx.clone()]);
                    for subx in subs_of.iter() {
                        regx_left = regx_left.subtract_region(&subx[inx]);
                    }
                    if !regx_left.is_empty() {
                        panic!("inx {} regs_left {}", inx, regx_left);
                    }
                }
            }
        } // next selx

        println!("\nPositive Select Sub-Regions, taking into account intersections:");
        for selx in self.select_positive.iter() {
            println!("  {}", selx);
        }
        //        println!("\nNegative Select Sub-Regions, taking into account intersections:");
        //        for selx in self.select_negative.iter() {
        //            println!("  {}", selx);
        //        }

        let mut pos = 0;
        let mut neg = 0;
        for selx in self.select.iter() {
            if selx.pos < selx.neg {
                neg += 1;
            }
            if selx.pos > selx.neg {
                pos += 1;
            }
        }
        println!(
            "\nSelect Regions: {}, Positive regions: {}, Negative Regions: {}",
            self.select.len(),
            pos,
            neg
        );
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

            // println!(" max_rate = {max_rate}");

            // Form start region corresponding.
            //let all_states = self.all_current_states();
            let mut start_regs = RegionStore::with_capacity(self.len());
            for domx in self.avec.iter() {
                start_regs.push(SomeRegion::new(vec![domx.cur_state.clone()]));
            }

            // Check if negative regions can be avoided.
            //for ndinx in self.can_do.iter_mut() {
            for ndinx in 0..self.can_do.len() {
                // Form goal region corresponding
                let mut goal_regs = RegionStore::with_capacity(start_regs.len());
                for (dom_inx, domx) in self.avec.iter().enumerate() {
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
                    let rate = self.select.rate_plans(&planx, &self.all_current_states());
                    //println!(" new rate {rate}");
                    if rate > max_rate {
                        println!(
                            "\nFor plan {}/{}, better plan found {}/{}",
                            self.can_do[ndinx].plans.as_ref().expect("SNH").str_terse(),
                            max_rate,
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

        // Get all domain states vector.
        // Calling self.all_current_states runs into problems with the compiler.
        let mut all_states = Vec::<&SomeState>::with_capacity(self.len());
        for domx in self.avec.iter() {
            all_states.push(domx.get_current_state());
        }

        // Check if current state is in a negative select state.
        if self.select_negative.any_supersets_of_states(&all_states) {
            // Since SelectRegions have a Boolean AND relationship, only
            // changing in one non-negative region, in one domain, is needed.
            let mut ndstr = NeedStore::new(vec![]);

            print!("all_states: [");
            for stax in all_states.iter() {
                print!(" {}", stax);
            }
            println!("], is subset of a negative region. ");
            let mut non_negs = Vec::<RegionStore>::with_capacity(self.len());
            for dom_num in 0..self.len() {
                non_negs.push(self.calc_non_negative_regions(dom_num, &all_states));
            }

            self.boredom = 0;
            self.boredom_limit = 0;

            for (inx, regstr) in non_negs.iter().enumerate() {
                // Find closest non-negative region distance.
                let mut min_dist = usize::MAX;
                for regx in regstr.iter() {
                    let dist = regx.distance_state(all_states[inx]);
                    if dist < min_dist {
                        min_dist = dist;
                    }
                }

                // Process closest non-negative regions.
                for regx in regstr.iter() {
                    if regx.distance_state(all_states[inx]) == min_dist {
                        let mut needx = SomeNeed::ExitSelectRegion {
                            dom_num: inx,
                            target_region: regx.clone(),
                            priority: 0,
                        };
                        needx.set_priority();
                        ndstr.push(needx);
                    }
                }
            }
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
        for domx in self.avec.iter() {
            change_vec.push(domx.aggregate_changes());
        }
        change_vec
    }

    /// Return a need for moving to an select region.
    fn select_goal_needs(&self, all_states: &[&SomeState]) -> Option<NeedStore> {
        //println!("domainstore: select_goal_needs");
        // Get regions the current state is not in.
        let mut notsups = self.select_positive.not_supersets_of_states(all_states);

        // If the current state is not in at least one select region, return None.
        if notsups.is_empty() {
            println!("ret 1");
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
        if !inxs.is_empty() {
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
            tools::vec_ref_string(&self.all_current_states())
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

    /// Like Real Life, formulate a direct plan,
    /// notice there are some negative aspects,
    /// then try to form a plan that avoids the negative.
    fn avoid_negative_select_regions(
        &self,
        start_regs: &RegionStore,
        goal_regs: &RegionStore,
    ) -> Option<PlanStore> {
        // println!("avoid_negative_select_region: starting: start {start_regs} goal: {goal_regs}");
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

        // Outcome may depend on the order of the domains checked, so try a number of times.
        for _ in 0..self.len() {
            let mut ret_plans = PlanStore::new(vec![]);

            let mut all_states_mem = Vec::<SomeState>::with_capacity(self.len());
            for domx in self.avec.iter() {
                all_states_mem.push(domx.cur_state.clone());
            }

            // Find a plan for a domain.
            let mut found_one = true;
            let mut doms = Vec::<usize>::with_capacity(self.len());

            while found_one {
                found_one = false;

                // Generate new all_states.
                let mut all_states = Vec::<&SomeState>::with_capacity(self.len());
                for stax in all_states_mem.iter() {
                    all_states.push(stax);
                }

                let mut options = tools::RandomPick::new(self.len());

                while let Some(dom_num) = options.pick() {
                    let start_reg = &start_regs[dom_num];
                    let goal_reg = &goal_regs[dom_num];

                    // Check if a plan has been found for this domain.
                    if doms.contains(&dom_num) {
                        continue;
                    }

                    let non_neg = self.calc_non_negative_regions(dom_num, &all_states);
                    if non_neg.is_empty() {
                        continue;
                    }

                    // Find plans that avoid negative regions.
                    if let Some(mut dom_plans) =
                        self[dom_num].plan_path_through_regions(start_reg, goal_reg, &non_neg)
                    {
                        found_one = true;
                        doms.push(dom_num);
                        //let planx = dom_plans[rand::thread_rng().gen_range(0..dom_plans.len())].clone();
                        let inx = rand::thread_rng().gen_range(0..dom_plans.len());
                        if !dom_plans[inx].is_empty() {
                            all_states_mem[dom_num] =
                                dom_plans[inx].result_region().state1().clone();
                            ret_plans.push(dom_plans.swap_remove(inx));
                        }

                        break;
                    }
                } // next inx, start_reg, goal_reg
            }
            // Return the plans.
            if doms.len() == self.len() {
                return Some(ret_plans);
            }
        }
        None
    } // end avoid_negative_select_regions

    /// Return a String representation of a DomainStore.
    fn formatted_string(&self) -> String {
        let mut rc_str = String::from("[");

        for (inx, mskx) in self.avec.iter().enumerate() {
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
        for domx in self.avec.iter() {
            tot += domx.number_groups();
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

            println!("\nDomain: {}, Plan:", planx.dom_num);

            for (inx, stepx) in planx.iter().enumerate() {
                let df = stepx.initial.diff_mask(&stepx.result);
                print!(
                    "{} Action {:02} Group {} ",
                    &stepx.initial, &stepx.act_num, &stepx.group_reg
                );
                if inx > 0 {
                    for sel_regx in self.select.iter() {
                        if sel_regx.regions.is_superset_states_corr(&cur_states)
                            && sel_regx.value() < 0
                        {
                            print!(" in {:+}", sel_regx);
                        }
                    }
                }
                println!("\n{}", df.str2());

                cur_states[planx.dom_num] = stepx.result.state1();
            } // next steps
            println!("{}", planx.result_region());
        } // next planx
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
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r01X1").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("rX101").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr2, 0, 2));
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
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r0101").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("r1001").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr2, 0, 1));
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
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r0x00").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("rx100").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr2, 0, 1));

        let mut regstr3 = RegionStore::with_capacity(1);
        regstr3.push(dmxs[0].region_from_string_pad_x("r01x1").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr3, 0, 1));

        let mut regstr4 = RegionStore::with_capacity(1);
        regstr4.push(dmxs[0].region_from_string_pad_x("r10x1").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr4, 0, 1));

        let mut regstr5 = RegionStore::with_capacity(1);
        regstr5.push(dmxs[0].region_from_string_pad_x("r101x").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr5, 0, 1));
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
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r01xx").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string_pad_x("r10xx").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr2, 0, 1));
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
        domx.eval_sample_arbitrary(0, &SomeSample::new(s0.clone(), s1.clone()));
        domx.eval_sample_arbitrary(0, &SomeSample::new(sf.clone(), se.clone()));

        // Set up action to change the second bit.
        domx.add_action();
        let s2 = domx.state_from_string("s0b10")?;
        let sd = domx.state_from_string("s0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new(s0.clone(), s2.clone()));
        domx.eval_sample_arbitrary(1, &SomeSample::new(sf.clone(), sd.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s4 = domx.state_from_string("s0b100")?;
        let sb = domx.state_from_string("s0b1011")?;
        domx.eval_sample_arbitrary(2, &SomeSample::new(s0.clone(), s4.clone()));
        domx.eval_sample_arbitrary(2, &SomeSample::new(sf.clone(), sb.clone()));

        // Set up action to change the third bit.
        domx.add_action();
        let s8 = domx.state_from_string("s0b1000")?;
        let s7 = domx.state_from_string("s0b0111")?;
        domx.eval_sample_arbitrary(3, &SomeSample::new(s0.clone(), s8.clone()));
        domx.eval_sample_arbitrary(3, &SomeSample::new(sf.clone(), s7.clone()));

        // Init DomainStore.
        let mut dmxs = DomainStore::new(vec![domx]);

        // Set select regions.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("rxx0x").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));
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
            return Err(format!("Plan found ?"));
        }
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
        println!("all states {}", tools::vec_ref_string(&all_states));

        assert!(all_states.len() == 2);
        assert!(*all_states[0] == init_state1);
        assert!(*all_states[1] == init_state2);

        Ok(())
    }

    #[test]
    /// Test case using adjacent non-negative regions.
    fn avoidance6() -> Result<(), String> {
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
        let mut regstr0 = RegionStore::with_capacity(1);
        regstr0.push(dmxs[0].region_from_string_pad_x("r1100").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Set up dom 0 00XX dependent on dom 1 10XX.
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string_pad_x("r1011").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));
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
            assert!(plans.len() == 1);
        } else {
            return Err(format!("No plan found?"));
        }
        Ok(())
    }

    #[test]
    /// Test case using two domains.
    fn avoidance7() -> Result<(), String> {
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
        let mut regstr0 = RegionStore::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("r01x1").expect("SNH"));
        regstr0.push(max_region.clone());
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStore::with_capacity(2);
        regstr0.push(dmxs[0].region_from_string_pad_x("rx101").expect("SNH"));
        regstr0.push(max_region.clone());
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        // Set up dom 1 negative regions.
        let mut regstr0 = RegionStore::with_capacity(2);
        regstr0.push(max_region.clone());
        regstr0.push(dmxs[1].region_from_string_pad_x("r011x").expect("SNH"));
        dmxs.add_select(SelectRegions::new(regstr0, 0, 1));

        let mut regstr0 = RegionStore::with_capacity(2);
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

        let start_region = RegionStore::new(vec![
            SomeRegion::new(vec![s0.clone()]),
            SomeRegion::new(vec![s1.clone()]),
        ]);
        let goal_region = RegionStore::new(vec![
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
        // Init a DomainStore.
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1)]);

        let mut regstr1 = RegionStore::with_capacity(1);
        let neg_reg1 = dmxs[0].region_from_string_pad_x("rX1XX").expect("SNH");

        regstr1.push(neg_reg1.clone());

        // Add select regionstores.
        dmxs.add_select(SelectRegions::new(regstr1, 0, 1));

        let mut regstr1 = RegionStore::with_capacity(1);
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
        // Start a DomainStore
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1)]);

        // Add action to domain 0.
        dmxs[0].add_action();

        // Load select regions
        let mut regstr1 = RegionStore::with_capacity(1);
        regstr1.push(dmxs[0].region_from_string("rxx0x")?);

        let mut regstr2 = RegionStore::with_capacity(1);
        regstr2.push(dmxs[0].region_from_string("r00x1")?);

        let mut regstr3 = RegionStore::with_capacity(1);
        regstr3.push(dmxs[0].region_from_string("r11x1")?);

        let mut regstr4 = RegionStore::with_capacity(1);
        regstr4.push(dmxs[0].region_from_string("r10x0")?);

        // Add select region stores.
        dmxs.add_select(SelectRegions::new(regstr1, 4, 0));
        dmxs.add_select(SelectRegions::new(regstr2, 0, 2));
        dmxs.add_select(SelectRegions::new(regstr3, 0, 4));
        dmxs.add_select(SelectRegions::new(regstr4, 0, 5));
        dmxs.calc_select();

        assert!(dmxs.select_positive.len() == 5);
        assert!(dmxs.select_negative.len() == 3);

        // assert!(1 == 2);
        Ok(())
    }
}
