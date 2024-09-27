//! The DomainStore struct, a vector of SomeDomain structs,
//! and values/methods that manage the domains.

use crate::change::SomeChange;
use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planscorr::PlansCorr;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionscorr::RegionsCorr;
use crate::selectregions::SelectRegions;
use crate::selectregionsstore::SelectRegionsStore;
use crate::state::SomeState;
use crate::statescorr::StatesCorr;
use crate::step::AltRuleHint::AltRule;
use crate::target::ATarget;
use crate::tools;

use rand::Rng;
use serde::{Deserialize, Serialize};
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
    /// Number bits different between start and end.
    pub desired_num_bits_changed: usize,
    /// Number bits changed, step by step.
    pub process_num_bits_changed: usize,
}

#[readonly::make]
#[derive(Serialize, Deserialize, Default)]
/// A vector of SomeDomain structs, and SomeDomain-specifc functions.
pub struct DomainStore {
    /// Vector of SomeDomain structs.
    pub items: Vec<SomeDomain>,

    /// Domain displayed to user.
    pub current_domain: usize,
    /// The current step number of a user session.
    pub step_num: usize,

    /// A counter to indicate the number of steps the current state is in the same select region.
    /// When no more rule-testing needs can be done.
    pub boredom: usize,
    /// A limit for becomming bored, then moving to another select state.
    /// When no more rule-testing needs can be done.
    pub boredom_limit: usize,

    /// Zero, or more, select regions that may have a positive, or negative, value.
    /// They may overlap.
    pub select: SelectRegionsStore,
    /// Positive region fragments, not overlapped by negative value regions.
    /// These tend to be goals.
    pub select_positive: SelectRegionsStore,
    /// Negative select fragments.
    pub select_negative: SelectRegionsStore,
    /// Times visited positive select regions.
    pub times_visited: Vec<usize>,
    /// Non-negative, may be, or overlap, positive, regions.
    pub select_non_negative: SelectRegionsStore,

    /// Use to save the results of the last run of get_needs.
    pub needs: NeedStore,
    /// A vector of InxPlans for selected needs, where a plan could be calculated.
    pub can_do: Vec<InxPlan>,
    /// Vector of indicies for selected needs, where a plan could not be calculated.
    pub cant_do: Vec<usize>,

    /// Save the maximum fragment value.
    /// Used to calculate the ToSelectRegion need priority.
    pub max_pos_value: isize,
}

impl DomainStore {
    /// Return a new, empty, DomainStore struct.
    pub fn new() -> Self {
        Self {
            items: Vec::<SomeDomain>::new(),
            current_domain: 0,
            boredom: 0,
            boredom_limit: 0,
            select: SelectRegionsStore::new(vec![]),
            select_positive: SelectRegionsStore::new(vec![]),
            select_non_negative: SelectRegionsStore::new(vec![]),
            select_negative: SelectRegionsStore::new(vec![]),
            needs: NeedStore::new(vec![]),
            can_do: Vec::<InxPlan>::new(),
            cant_do: Vec::<usize>::new(),
            step_num: 0,
            max_pos_value: 0,
            times_visited: vec![],
        }
    }

    /// Add SelectRegions, one region for each domain.
    /// [1x0x, x101] = Domain 0, 1x0x, AND domain 1, x101.
    /// [1x0x, xxxx],
    /// [xxxx, x101] (two additions) = Domain 0, 1x0x, OR domain 1, x101.
    /// The SR value cannot be zero.
    /// Duplicates are not allowed.
    pub fn add_select(&mut self, selx: SelectRegions) {
        // Require some aggregate value.
        if selx.net_value == 0 {
            println!("add_select: {} SR value zero, skipped.", selx);
            return;
        }

        // Check length.
        debug_assert!(selx.len() == self.items.len());

        // Do not allow dups.
        if self.select.contains(&selx) {
            println!("add_select: {} Equal select region found, skipped.", selx);
            return;
        }

        // Check that each select region matches the corresponding domain number bits.
        for (sely, domx) in selx.regions.iter().zip(self.items.iter()) {
            if sely.num_bits() == domx.cur_state.num_bits() {
            } else {
                return;
            }
        }

        self.select.push_nosubs(selx);
    }

    /// Calculate parts of select regions, in case of any overlaps.
    pub fn calc_select(&mut self) {
        if self.select.is_empty() {
            return;
        }

        println!("\nSelect Regions ({}):", self.select.len());
        for selx in self.select.iter() {
            println!("  {}, value {}", selx.regions, selx.net_value);
        }
        println!("  Avoid negative SRs in rule-paths.");
        println!("  Exit negative SR if the current state is within one.");

        // Get fragments due to different-value intersections.
        let fragments = self.select.split_by_intersections();

        //        println!("fragments");
        for sely in fragments.into_iter() {
            //            println!("  {sely}");
            if sely.pos_value > 0 && sely.neg_value == 0 {
                if sely.pos_value > self.max_pos_value {
                    self.max_pos_value = sely.pos_value;
                }
                self.select_positive.push(sely);
            }
        }

        if self.select_positive.is_not_empty() {
            println!(
                "\nPositive SR fragments, Positive SRs split by intersections ({}):",
                self.select_positive.len()
            );
            for selx in self.select_positive.iter() {
                println!("  {}, value {}", selx.regions, selx.pos_value);
            }
            println!("  To seek, when no other needs can be done.");
            self.times_visited = vec![0; self.select_positive.len()];
        }

        // Calc non-negative regions.
        let mut non_neg_select =
            SelectRegionsStore::new(vec![SelectRegions::new(self.maximum_regions(), 0)]);

        for selx in self.select.iter() {
            if selx.neg_value < 0 {
                non_neg_select = non_neg_select.subtract_selectregions(selx);
            }
        }
        if non_neg_select.is_not_empty() {
            self.select_non_negative = non_neg_select;

            println!(
                "\nNon-negative SRs, maximum regions minus negative SRs ({}):",
                self.select_non_negative.len()
            );
            for selx in self.select_non_negative.iter() {
                println!("  {}", selx.regions);
            }
            println!("  Seek when exiting a negative SR the current state is in.");
            println!("  Used to find a rule-path that crosses fewer negative SRs.");
            // Double check.
            for selx in self.select_non_negative.iter() {
                for selz in self.select.iter() {
                    if selz.neg_value < 0 {
                        assert!(!selz.intersects(selx));
                    }
                }
            }
        }

        // Calc negative fragments.
        let mut neg_srs = SelectRegionsStore::new(vec![]);
        for selx in self.select.iter() {
            if selx.neg_value < 0 {
                neg_srs.push(selx.clone());
            }
        }
        if neg_srs.is_not_empty() {
            self.select_negative = neg_srs.split_by_intersections();
            println!("\nNegative SR fragments ({}):", self.select_negative.len());
            for selx in self.select_negative.iter() {
                println!("  {}, {}", selx.regions, selx.net_value);
            }
            println!("  If more than one value, least negative values may be traversed to find a plan-path.");
        }
    }

    /// Add a Domain struct to the store.
    /// Add select regions after the last domain has been added.
    pub fn add_domain(&mut self, cur_state: SomeState) {
        debug_assert!(self.select.is_empty());

        self.items
            .push(SomeDomain::new(self.items.len(), cur_state));
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
            .items
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

    /// Run a PlansCorr.
    pub fn run_planscorr(&mut self, plns: &PlansCorr) -> bool {
        let vecb = plns
            .plans
            .items
            .par_iter()
            .zip(self.items.par_iter_mut())
            .map(|(plnx, domx)| domx.run_plan(plnx, 0))
            .collect::<Vec<Result<usize, String>>>();

        for rsltx in vecb.iter() {
            match rsltx {
                Ok(_) => continue,
                Err(_) => return false,
            }
        }
        true
    }

    /// Run a plan for a given Domain.
    /// Return true if the plan ran to completion.
    pub fn run_plan(&mut self, pln: &SomePlan) -> Result<usize, String> {
        self.items[pln.dom_id].run_plan(pln, 0)
    }

    /// Take an action to satisfy a need,
    pub fn take_action_need(&mut self, nd_inx: usize) {
        self.items[self.needs[nd_inx].dom_id().unwrap()].take_action_need(&self.needs[nd_inx]);
    }

    /// Return a reference to the current state of a given Domain index
    pub fn cur_state(&self, dmxi: usize) -> &SomeState {
        self.items[dmxi].current_state()
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
            // Make fake need, to get priority.
            let mut needx = SomeNeed::ToSelectRegion {
                target_regions: RegionsCorr::with_capacity(1),
                priority: 0,
                times_visited: 0,
                value: 0,
            };
            needx.add_priority_base();
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
                .map(|nd_inx| {
                    (
                        nd_inx,
                        self.make_plans(self.needs[nd_inx].dom_id(), &self.needs[nd_inx].target()),
                    )
                })
                .collect::<Vec<(usize, Option<PlanStore>)>>();

            // See if any plans have been found.
            let mut cant = Vec::<usize>::new();
            let mut can = Vec::<InxPlan>::new();

            for (inx, planx) in inx_plan {
                if let Some(plany) = planx {
                    let ratex = self.rate_plans(&plany);
                    let cur_regs = self.all_current_regions();
                    let desired_num_bits_changed =
                        cur_regs.distance(&plany.result_regions(&cur_regs));
                    let process_num_bits_changed = plany.num_bits_changed();
                    can.push(InxPlan {
                        inx,
                        plans: plany,
                        rate: ratex,
                        desired_num_bits_changed,
                        process_num_bits_changed,
                    });
                } else {
                    cant.push(inx);
                }
            }
            if can.is_empty() {
                println!(", none.");
                if cur_pri_end == needs_len || cur_pri > select_priority {
                    self.cant_do = (0..self.needs.len()).collect();
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
                let goal_regs = match self.needs[ndinx.inx].target() {
                    ATarget::State { state } => self.current_regions_except(
                        self.needs[ndinx.inx].dom_id().unwrap(),
                        &SomeRegion::new(vec![state.clone().clone()]),
                    ),
                    ATarget::Region { region } => {
                        self.current_regions_except(self.needs[ndinx.inx].dom_id().unwrap(), region)
                    }
                    ATarget::DomainRegions { regions } => regions.clone(),
                };

                if let Some(planx) =
                    self.plan_using_least_negative_select_regions(&start_regs, &goal_regs)
                {
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

                        let cur_regs = self.all_current_regions();
                        let desired_num_bits_changed =
                            cur_regs.distance(&planx.result_regions(&cur_regs));
                        let process_num_bits_changed = planx.num_bits_changed();
                        self.can_do.push(InxPlan {
                            inx: ndinx.inx,
                            plans: planx,
                            rate,
                            desired_num_bits_changed,
                            process_num_bits_changed,
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
    pub fn make_plans(&self, dom_id: Option<usize>, target: &ATarget) -> Option<PlanStore> {
        //println!("\ndomainstore: make_plans: {}", target);

        let from = self.all_current_regions();

        // Calculate the goal regions.  These are needed for ATarget::State and ATarget::Region as
        // other domain regions may need to be changed to atain the target and avoid negative conditions,
        // which are defined as superset of all domain current states.
        let goal = match target {
            ATarget::State { state } => &self
                .current_regions_except(dom_id.unwrap(), &SomeRegion::new(vec![(*state).clone()])),
            ATarget::Region { region } => &self.current_regions_except(dom_id.unwrap(), region),
            ATarget::DomainRegions { regions } => regions,
        };

        self.make_plans2(&from, goal, None)
    }

    /// Create a RegionsStoreCorr of current regions, except a given region.
    pub fn current_regions_except(&self, dom_id: usize, targ: &SomeRegion) -> RegionsCorr {
        let mut regs = RegionsCorr::new(vec![]);

        for dom_idx in 0..self.len() {
            if dom_idx == dom_id {
                regs.push(targ.clone());
            } else {
                regs.push(SomeRegion::new(vec![self.items[dom_idx].cur_state.clone()]));
            }
        }
        regs
    }

    /// Use parallel processing to get plans.
    pub fn make_plans2(
        &self,
        from: &RegionsCorr,
        goal: &RegionsCorr,
        within: Option<&RegionsCorr>,
    ) -> Option<PlanStore> {
        debug_assert!(from.len() == goal.len());
        debug_assert!(from.corresponding_num_bits(goal));
        if let Some(regs) = within {
            debug_assert!(regs.len() == from.len() && from.corresponding_num_bits(regs));
            debug_assert!(regs.is_superset_of(from));
            debug_assert!(regs.is_superset_of(goal));
        }
        //if let Some(path) = within {
        //    println!("make_plans2: from {from} to {goal} within {path}");
        //} else {
        //    println!("make_plans2: from {from} to {goal}");
        //}

        if from.is_subset_of(goal) {
            return Some(PlanStore::new(vec![]));
        }
        let mut plans = (0..6)
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
        from: &RegionsCorr,
        goal: &RegionsCorr,
        within: Option<&RegionsCorr>,
    ) -> Option<PlanStore> {
        //println!("domainstore: make_plans3: from {from} goal {goal}");
        debug_assert!(from.len() == goal.len());
        debug_assert!(from.corresponding_num_bits(goal));
        if let Some(regs) = within {
            debug_assert!(regs.len() == from.len() && from.corresponding_num_bits(regs));
            debug_assert!(regs.is_superset_of(from));
            debug_assert!(regs.is_superset_of(goal));
        }

        let mut plans_per_target = PlanStore::new(Vec::<SomePlan>::with_capacity(from.len()));

        if let Some(within_regs) = within {
            // Find a plan for each target.
            for (dom_id, (regx, regy)) in from.iter().zip(goal.iter()).enumerate() {
                if regy.is_superset_of(regx) {
                    //plans_per_target.push_link(SomePlan::new(dom_id, vec![]));
                } else {
                    // Try making plans.
                    if let Some(mut plans) =
                        self.get_plans(dom_id, regx, regy, Some(&within_regs[dom_id]))
                    {
                        if !plans_per_target
                            .push_link(plans.remove(rand::thread_rng().gen_range(0..plans.len())))
                        {
                            return None;
                        }
                    } else {
                        // return None if any target cannot be reached.
                        return None;
                    }
                }
                //                if !plans_per_target.remains_within(&within_regs) {
                //                    println!("plans_per-target {plans_per_target} is not within {within_regs}");
                //                    panic!("Done");
                //                }
            } // next dom_id
        } else {
            for (dom_id, (regx, regy)) in from.iter().zip(goal.iter()).enumerate() {
                if regy.is_superset_of(regx) {
                    //plans_per_target.push_link(SomePlan::new(dom_id, vec![]));
                } else {
                    // Try making plans.
                    if let Some(mut plans) = self.get_plans(dom_id, regx, regy, None) {
                        // return None if any target cannot be reached.
                        if !plans_per_target
                            .push_link(plans.remove(rand::thread_rng().gen_range(0..plans.len())))
                        {
                            return None;
                        }
                    } else {
                        // return None if any target cannot be reached.
                        return None;
                    }
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
    /// This ignores the select regions a plan starts in.
    /// A square in a region can exit the region to an adjacent square by changing one bit.
    /// A square adjacent to a region can enter the region by changing one bit.
    fn rate_plans2(&self, plans: &PlanStore, regions: &RegionsCorr) -> isize {
        // Store rate for each step.
        let mut rate = 0;

        let mut cur_regions = regions.clone();
        // Skip start value of negative SelectRegions.
        for planx in plans.iter() {
            if planx.is_empty() {
                continue;
            }
            for stepx in planx.iter() {
                cur_regions[planx.dom_id] = stepx
                    .rule
                    .result_from_initial_region(&cur_regions[planx.dom_id]);
                rate += self.select.rate_by_negative_regions(&cur_regions);
                if let AltRule { .. } = &stepx.alt_rule {
                    rate -= 1;
                }
            }
        }
        rate
    }

    /// Get plans to move to a goal region, choose a plan.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    pub fn get_plans(
        &self,
        dom_id: usize,
        from_region: &SomeRegion,
        goal_region: &SomeRegion,
        within: Option<&SomeRegion>,
    ) -> Option<PlanStore> {
        debug_assert!(from_region.num_bits() == goal_region.num_bits());
        if let Some(reg) = within {
            debug_assert!(reg.num_bits() == from_region.num_bits());
            debug_assert!(reg.is_superset_of(from_region));
            debug_assert!(reg.is_superset_of(goal_region));
        }
        //println!("domainstore: get_plans2: dom {dom_id} from {from_region} goal {goal_region}");

        self.items[dom_id].make_plans(from_region, goal_region, within)
    }

    /// Choose a plan from a vector of PlanStare references, for vector of PlanStores.
    /// Return index of plan chosen.
    pub fn choose_a_plan(&self, plans: &[PlanStore], start_regs: &RegionsCorr) -> usize {
        self.choose_a_plan2(&(plans.iter().collect::<Vec<&PlanStore>>()), start_regs)
    }

    /// Choose a plan from a vector of PlanStore references, for vector of PlanStore references.
    /// Return index of plan chosen.
    pub fn choose_a_plan2(&self, plans: &[&PlanStore], start_regs: &RegionsCorr) -> usize {
        debug_assert!(self.len() == start_regs.len());
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
        let max_rate_plans = rates
            .iter()
            .enumerate()
            .filter_map(|(inx, rate)| if rate == max_rate { Some(inx) } else { None })
            .collect::<Vec<usize>>();

        // No further choice to be made.
        if max_rate_plans.len() == 1 {
            return max_rate_plans[0];
        }

        // Gather length data.
        let lengths = max_rate_plans
            .iter()
            .map(|inx| plans[*inx].number_steps())
            .collect::<Vec<usize>>();

        let min_len = lengths.iter().min().unwrap();

        // Find plans with the min length.
        let min_len_plans = lengths
            .iter()
            .enumerate()
            .filter_map(|(inx, lenx)| {
                if lenx == min_len {
                    Some(max_rate_plans[inx])
                } else {
                    None
                }
            })
            .collect::<Vec<usize>>();

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
        assert!(!self.can_do.is_empty());

        let ref_vec = self
            .can_do
            .iter()
            .map(|inxplanx| &inxplanx.plans)
            .collect::<Vec<&PlanStore>>();

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
        self.items.len()
    }

    /// Return a vector of domain current state references, in domain number order.
    pub fn all_current_states(&self) -> StatesCorr {
        let mut all_states = StatesCorr::with_capacity(self.len());

        for domx in self.items.iter() {
            all_states.push(domx.current_state().clone());
        }

        all_states
    }

    /// Return a vector of domain current state references, as regions, in domain number order.
    pub fn all_current_regions(&self) -> RegionsCorr {
        let mut all_regions = RegionsCorr::with_capacity(self.len());

        for domx in self.items.iter() {
            all_regions.push(SomeRegion::new(vec![domx.current_state().clone()]));
        }

        all_regions
    }

    /// Update counters for times_visited.
    pub fn update_times_visited(&mut self) {
        // Get all domain current states.
        let all_states = self.all_current_states();

        // Get the select regions the current state is in.
        for (inx, optregs) in self.select_positive.iter_mut().enumerate() {
            if optregs.regions.is_superset_states(&all_states) {
                self.times_visited[inx] += 1;
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
                boredom_limit += optregs.net_value;
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
        let all_regs = self.all_current_regions();

        let rate = self.select.rate_regions(&all_regs);

        // Check if current state rate is negative.
        if rate < 0 {
            self.boredom = 0;
            self.boredom_limit = 0;

            let mut ndstr = NeedStore::new(vec![]);

            //print!("all_states: [");
            //for regx in all_regs.iter() {
            //    print!(" {}", regx.first_state());
            //}
            //println!("], is subset of a negative region. ");

            if let Some(near_nn_regs) =
                self.closest_select_regions(&all_regs, &self.select_non_negative)
            {
                // Process closest non-negative regions.
                let mut needx = SomeNeed::ExitSelectRegion {
                    target_regions: near_nn_regs.clone(),
                    priority: 0,
                };
                needx.add_priority_base();
                ndstr.push(needx);
                return Some(ndstr);
            }
        }

        // Check current status within a select region, or not.
        let rate = self.select.rate_regions(&all_regs);
        if rate > 0 {
            self.boredom += 1;
            if self.boredom <= self.boredom_limit {
                return None;
            }
        } else {
            self.boredom = 0;
            self.boredom_limit = 0;
        }

        self.select_goal_needs(&all_regs)
    }

    /// Return a vector of aggregate change references, per domain.
    pub fn aggregate_changes(&self) -> Vec<&SomeChange> {
        let mut change_vec = Vec::<&SomeChange>::with_capacity(self.len());
        for domx in self.items.iter() {
            if let Some(changes) = &domx.aggregate_changes() {
                change_vec.push(changes);
            }
        }
        change_vec
    }

    /// Return a need for moving to an select region.
    fn select_goal_needs(&self, goal_regs: &RegionsCorr) -> Option<NeedStore> {
        //println!("domainstore: select_goal_needs");
        debug_assert!(self.len() == goal_regs.len());

        // Load return vector.
        let mut ret_str = NeedStore::new(vec![]);

        // Get current states.
        let all_states = self.all_current_states();

        for (inx, psupx) in self.select_positive.iter().enumerate() {
            if psupx.is_superset_of_states(&all_states) {
                continue;
            }
            let adjust = (self.max_pos_value - psupx.net_value) + self.times_visited[inx] as isize;
            let mut needx = SomeNeed::ToSelectRegion {
                target_regions: (psupx.regions.clone()),
                priority: adjust as usize,
                times_visited: self.times_visited[inx],
                value: psupx.net_value as usize,
            };
            needx.add_priority_base();
            ret_str.push(needx);
        }

        // If no needs, return None.
        if ret_str.is_empty() {
            //println!("ret 1");
            return None;
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
        let mut in_pos = false;
        let mut in_neg = false;

        for selx in self.select.iter() {
            if selx.net_value != 0 && selx.is_superset_of_states(&all_states) {
                in_str += &format!("in {}, {} ", selx.regions, selx.net_value);
                if selx.net_value > 0 {
                    in_pos = true;
                } else {
                    in_neg = true;
                }
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

        println!("\nActs: {}", self.items[dom_id].actions);

        let cur_state = &self.items[dom_id].current_state();

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
                if ndplnx.desired_num_bits_changed != 0 {
                    println!(
                        "{:2} {} {}/{}/{}/{:+}",
                        inx,
                        self.needs[ndplnx.inx],
                        ndplnx.plans.str_terse(),
                        ndplnx.desired_num_bits_changed,
                        ndplnx.process_num_bits_changed,
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
        assert!(dom_id < self.items.len());

        self.current_domain = dom_id;
    }

    // Set the current state field, of the current domain.
    pub fn set_cur_state(&mut self, new_state: SomeState) {
        debug_assert!(new_state.num_bits() == self[self.current_domain].num_bits());

        let dmx = self.current_domain;
        self[dmx].set_cur_state(new_state)
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

    /// Return a vector of bridges between non-negative SRs and negative SRs of a limited net_value.
    /// Really, to get away from immutible/mutible reference problems.
    fn calc_bridges(&self, neg_limit_val: isize) -> Vec<RegionsCorr> {
        //println!("bridges : neg_limit_val {neg_limit_val}");
        let mut ret_regs = vec![];

        for selx in self.select_negative.iter() {
            if selx.net_value < neg_limit_val {
                continue;
            }
            for sely in self.select_non_negative.iter() {
                if sely.regions.is_adjacent(&selx.regions) {
                    ret_regs.push(sely.regions.bridge(&selx.regions));
                }
            }
        }
        ret_regs
    }

    /// Like Real Life, formulate a direct plan,
    /// notice there are some negative aspects,
    /// then try to form a plan that avoids the negative.
    fn plan_using_least_negative_select_regions(
        &self,
        start_regs: &RegionsCorr,
        goal_regs: &RegionsCorr,
    ) -> Option<PlanStore> {
        //println!(
        //    "plan_using_least_negative_select_regions_new: starting: start {start_regs} goal: {goal_regs}"
        //);
        assert!(start_regs.len() == self.len());
        assert!(goal_regs.len() == self.len());

        // Check if no plans are needed.
        if start_regs.intersects(goal_regs) {
            //println!("No plan needed (1)");
            return None;
        }

        // Build initial SelectRegions vector.
        let mut slrgs = Vec::<RegionsCorr>::with_capacity(self.select_non_negative.len());
        for selx in self.select_non_negative.iter() {
            slrgs.push(selx.regions.clone());
        }

        // Create list of negative SR values, sorted in lower to greater negative value.
        let mut neg_srs_vals = vec![];
        for selx in self.select_negative.iter() {
            if selx.neg_value < 0 && !neg_srs_vals.contains(&selx.neg_value) {
                neg_srs_vals.push(selx.neg_value);
            }
        }

        if !neg_srs_vals.is_empty() {
            if neg_srs_vals.len() > 1 {
                neg_srs_vals.sort_by(|a, b| b.cmp(a));
            }
            //println!("Neg srs values {:?}", neg_srs_vals);
            neg_srs_vals.pop(); // Lose the highest value negative.
        }

        let mut neg_srs_inx2: isize = -1;

        loop {
            // Build initial SelectRegions vector.
            let mut slrgs2 = Vec::<&RegionsCorr>::with_capacity(self.select_non_negative.len());
            for selx in self.select_non_negative.iter() {
                slrgs2.push(&selx.regions);
            }
            let brgs;

            if neg_srs_inx2 == neg_srs_vals.len() as isize {
                return None;
            }

            if neg_srs_inx2 > -1 {
                let limit = neg_srs_vals[neg_srs_inx2 as usize];

                for selx in self.select_negative.iter() {
                    if selx.neg_value <= limit {
                        slrgs2.push(&selx.regions);
                    }
                }
                brgs = self.calc_bridges(limit);
                for brgx in brgs.iter() {
                    slrgs2.push(brgx);
                }
            }
            neg_srs_inx2 += 1;

            let mut plans = (0..6)
                .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
                .filter_map(|_| {
                    self.plan_using_least_negative_select_regions2(start_regs, goal_regs, &slrgs2)
                })
                .collect::<Vec<PlanStore>>();

            if plans.is_empty() {
                continue;
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

            let ret_path = plans.swap_remove(inxs[rand::thread_rng().gen_range(0..inxs.len())]);
            //println!("plan_using_least_negative_select_regions: returning {ret_path}");
            return Some(ret_path);
        }
    }

    /// Return the nearest non-negative regions.
    pub fn closest_select_regions<'a>(
        &'a self,
        from_regs: &RegionsCorr,
        select_regions: &'a SelectRegionsStore,
    ) -> Option<&RegionsCorr> {
        // Find closest non-negative SelectRegions, if any.
        debug_assert!(from_regs.len() == self.len());

        let mut min_distance = usize::MAX;
        let mut targets = Vec::<&SelectRegions>::new();
        for regsx in select_regions.iter() {
            let dist = from_regs.distance(&regsx.regions);
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
        Some(&targets[rand::thread_rng().gen_range(0..targets.len())].regions)
    }

    /// Return plans for a change from start to goal.
    fn plan_using_least_negative_select_regions2(
        &self,
        start_regs: &RegionsCorr,
        goal_regs: &RegionsCorr,
        select_regions: &[&RegionsCorr],
    ) -> Option<PlanStore> {
        debug_assert!(start_regs.len() == goal_regs.len());
        debug_assert!(start_regs.corresponding_num_bits(goal_regs));
        //println!(
        //    "plan_using_least_negative_select_regions2: starting: start {start_regs} goal: {goal_regs} select {}", tools::vec_ref_string(select_regions)
        //);

        // Check no plan needed.
        if goal_regs.is_superset_of(start_regs) {
            return Some(PlanStore::new(vec![]));
        }

        // Check if start_regs and goal_regs are in the same non-negative region.
        for selx in select_regions.iter() {
            if selx.is_superset_of(start_regs) && selx.intersects(goal_regs) {
                if let Some(plans) =
                    self.make_plans2(start_regs, &selx.intersection(goal_regs)?, Some(selx))
                {
                    assert!(plans.validate(start_regs, goal_regs));
                    //println!("plan_using_least_negative_select_regions2: returning (1) {plans}");
                    return Some(plans);
                } else {
                    continue;
                }
            }
        }

        let mut ret_plan_store = PlanStore::new(vec![]);

        // Init current start.
        let mut cur_start = start_regs.clone();

        // Make plan to move start to a non-negative SelectRegion, if needed.
        // Only a one-bit change should be needed, unless there are overlapping negative SelectRegions.
        if self.select.rate_regions(&cur_start) < 0 {
            // Find closest non-negative SelectRegions, if any.
            let near_nn_regs =
                self.closest_select_regions(&cur_start, &self.select_non_negative)?;
            // Try to plan move to the selected SelectRegion.
            let plans = self.make_plans2(&cur_start, near_nn_regs, None)?;

            // Adjust new start regions.
            cur_start = plans.result_regions(start_regs);

            if plans.result_regions(start_regs).is_subset_of(goal_regs) {
                assert!(plans.validate(start_regs, goal_regs));
                //println!("plan_using_least_negative_select_regions2: returning (2) {plans}");
                return Some(plans);
            }

            ret_plan_store = plans;
        } // end in_negative.

        // Now, use local cur_start, instead of argument start_regs.

        // Init last PlanStore, may end up being empty.
        let mut last_plan_store = PlanStore::new(vec![]);

        // Init current goal.
        let mut cur_goal = goal_regs.clone();

        // Make plan to move goal from a non-negative region, if needed.
        if self.select.rate_regions(goal_regs) < 0 {
            // Find closest non-negative SelectRegions, if any.
            let near_nn_regs = self.closest_select_regions(goal_regs, &self.select_non_negative)?;

            // Calc closest regions in the chosen SelectRegion, congruent to the goal_regs.
            let goal_regs_nn = cur_goal.translate_to(near_nn_regs);

            // Try to plan move to from the selected SelectRegion, to the goal.
            let plans = self.make_plans2(&goal_regs_nn, &cur_goal, None)?;

            // Check if done.
            if cur_start.intersects(goal_regs) {
                if !ret_plan_store.append_link(plans) {
                    return None;
                }
                assert!(ret_plan_store.validate(start_regs, goal_regs));
                //println!("plan_using_least_negative_select_regions2: returning (3) {ret_plan_store}");
                return Some(ret_plan_store);
            }

            // Save plans to goal.
            last_plan_store = plans;

            // Adjust goal_regs.
            cur_goal = goal_regs_nn;
        } // end in_negative.

        // Check if no further plan needed.
        if cur_start.is_subset_of(&cur_goal) {
            if last_plan_store.is_not_empty() {
                if let Some(ret_plan_store2) =
                    self.link_two_planstores(&ret_plan_store, &last_plan_store)
                {
                    assert!(ret_plan_store2.validate(start_regs, goal_regs));
                    //println!("plan_using_least_negative_select_regions2: returning (4) {ret_plan_store2}");
                    return Some(ret_plan_store2);
                } else {
                    return None;
                }
            }
            assert!(ret_plan_store.validate(start_regs, goal_regs));
            //println!("plan_using_least_negative_select_regions2: returning (5) {ret_plan_store}");
            return Some(ret_plan_store);
        }

        // Check if cur_start and cur_goal are in the same non-negative region.
        for selx in select_regions.iter() {
            if selx.is_superset_of(&cur_start) && selx.is_superset_of(&cur_goal) {
                let plans = self.make_plans2(&cur_start, &cur_goal, Some(selx))?;

                if !ret_plan_store.append_link(plans) {
                    return None;
                }

                let ret_plan_store_result_regions = ret_plan_store.result_regions(start_regs);

                if goal_regs.is_superset_of(&ret_plan_store_result_regions) {
                    assert!(ret_plan_store.validate(start_regs, goal_regs));
                    //println!("plan_using_least_negative_select_regions2: returning (6) {ret_plan_store}");
                    return Some(ret_plan_store);
                }

                if last_plan_store.is_empty() {
                    return None;
                }

                if let Some(ret_plan_store2) =
                    self.link_two_planstores(&ret_plan_store, &last_plan_store)
                {
                    assert!(ret_plan_store2.validate(start_regs, goal_regs));
                    //println!("plan_using_least_negative_select_regions2: returning (7) {ret_plan_store2}");
                    return Some(ret_plan_store2);
                }
                //println!("plan_using_least_negative_select_regions2: returning (16): None");
                return None;
            }
        }

        // Now, use local cur_goal, instead of argument goal_regs.

        // Try generating multiple paths.
        let mut mid_paths = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| {
                self.plan_using_least_negative_select_regions3(
                    &cur_start,
                    &cur_goal,
                    select_regions,
                )
            })
            .collect::<Vec<Vec<&RegionsCorr>>>();

        if mid_paths.is_empty() {
            return None;
        }

        // Randomly choose a possible path.
        let pathx = mid_paths.swap_remove(rand::thread_rng().gen_range(0..mid_paths.len()));

        let mut cur_regs = cur_start.clone(); // Borrow checker thinks the above map is still in force?

        let mut mid_plans = PlanStore::new(vec![]);
        for inx in 0..(pathx.len() - 1) {
            if let Some(intx) = pathx[inx].intersection(pathx[inx + 1]) {
                if let Some(plans) = self.make_plans2(&cur_regs, &intx, Some(pathx[inx])) {
                    cur_regs = plans.result_regions(&cur_regs);
                    if !mid_plans.append_link(plans) {
                        return None;
                    }
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        if mid_plans.is_empty() {
            return None;
        }

        // Add mid_plans to existing start_plan_store, if any.
        if !ret_plan_store.append_link(mid_plans) {
            return None;
        }

        // Add last_plan_store, if needed.
        // Last_plan_store plans may begin with a region containing 0, 1, or more, X bit positions.
        if last_plan_store.is_not_empty() {
            if let Some(ret_plan_store2) =
                self.link_two_planstores(&ret_plan_store, &last_plan_store)
            {
                assert!(ret_plan_store2.validate(start_regs, goal_regs));
                //println!("plan_using_least_negative_select_regions2: returning (8) {ret_plan_store2}");
                return Some(ret_plan_store2);
            };
            return None;
        }

        assert!(ret_plan_store.validate(start_regs, goal_regs));
        //println!("plan_using_least_negative_select_regions2: returning (9) {ret_plan_store}");
        Some(ret_plan_store)
    } // end plan_using_least_negative_select_regions2

    /// Link two PlanStores together.
    /// Link two PlanStores together.
    /// It may be necessary to generate plans between corresponding
    /// domains.
    fn link_two_planstores(&self, first: &PlanStore, second: &PlanStore) -> Option<PlanStore> {
        let max_regs = self.maximum_regions();

        let first_result_regions = first.result_regions(&max_regs);

        let second_initial_regions = second.initial_regions(&max_regs);

        if first_result_regions.intersects(&second_initial_regions) {
            if let Some(ret_store) = first.link(second) {
                //println!("Link_two_planstores: Returning: {ret_store}");
                return Some(ret_store);
            } else {
                //println!("Link_two_planstores: Returning: None");
                return None;
            }
        }

        if let Some(plans) = self.make_plans2(&first_result_regions, &second_initial_regions, None)
        {
            //println!("linking 3 plans: {},  {},  {}", first, plans, second);

            if let Some(ret_store1) = first.link(&plans) {
                if let Some(ret_store2) = ret_store1.link(second) {
                    //println!("Link_two_planstores: Returning: {ret_store2}");
                    return Some(ret_store2);
                }
            }
        }
        //println!("Link_two_planstores: Returning: None");
        None
    }

    /// Return a series of intersecting, non-negative RegionsCorrs, to
    /// guide a path between start and goal, without passing through only the
    /// given RegionsCorrs.
    fn plan_using_least_negative_select_regions3<'a>(
        &'a self,
        start_regs: &'a RegionsCorr,
        goal_regs: &'a RegionsCorr,
        select_regions: &'a [&RegionsCorr],
    ) -> Option<Vec<&RegionsCorr>> {
        //println!(
        //    "plan_using_least_negative_select_regions3: starting: start {start_regs} goal: {goal_regs} select {}", tools::vec_ref_string(select_regions)
        //);
        // Start a list with the start regions, and a list with the goal regions.
        // Successively add intersections, of the last item of each list, to extend each list,
        // until the the two lists intersect, or no more new intersections can extend the lists.
        //
        // Kind of like two random depth-first searches.

        // Init start intersections vector.
        let mut start_ints = Vec::<&RegionsCorr>::new();
        start_ints.push(start_regs);

        // Init goal intersections vector.
        let mut goal_ints = Vec::<&RegionsCorr>::new();
        goal_ints.push(goal_regs);

        // Build up start and goal vectors, until the is an intersection between them, or
        // no more intersections.
        loop {
            // Get next layer of intersections for start_ints.
            let mut start_added = false;
            let start_last = &start_ints.last()?;

            // Randomly pick possible SelectRegions to test.
            let mut randpick = tools::RandomPick::new(select_regions.len());
            while let Some(inx) = randpick.pick() {
                let selx = &select_regions[inx];

                // A new RegionsCorr must intersect the last one in the vector.
                if start_last.intersects(selx) {
                } else {
                    continue;
                }
                // Avoid duplicating a vector item.
                if tools::vec_contains_ref(&start_ints, selx) {
                    continue;
                }
                // Avoid the case of multiple RegionsCorrs intersecting the start RegionsCorr,
                // one has already been chosen.
                if start_ints.len() > 1 && selx.is_superset_of(start_regs) {
                    continue;
                }

                start_ints.push(selx);
                start_added = true;
                break;
            } // next pick.

            // Get next layer of intersections for goal_ints.
            let mut goal_added = false;
            let goal_last = &goal_ints.last()?;

            // Randomly pick possible SelectRegions to test.
            let mut randpick = tools::RandomPick::new(select_regions.len());
            while let Some(inx) = randpick.pick() {
                let selx = &select_regions[inx];

                // A new RegionsCorr must intersect the last one in the vector.
                if goal_last.intersects(selx) {
                } else {
                    continue;
                }
                // Avoid duplicating a vector item.
                if tools::vec_contains_ref(&goal_ints, selx) {
                    continue;
                }
                // Avoid the case of multiple RegionsCorrs intersecting the goal RegionsCorr,
                // one has already been chosen.
                if goal_ints.len() > 1 && selx.is_superset_of(goal_regs) {
                    continue;
                }

                goal_ints.push(selx);
                goal_added = true;
                break;
            } // next pick.

            // Check if done, with no result.
            if start_added || goal_added {
            } else {
                //println!("plan_using_least_negative_select_regions3: returning (1) no new intersections found");
                return None;
            }

            // Check the last start_ints item against all goal_ints.
            let start_last = start_ints.last()?;

            // Keep track of goal items traversed, in case an intersection is found.
            let mut tmp_path = Vec::<&RegionsCorr>::new();

            for regs_gx in goal_ints.iter() {
                if regs_gx.intersects(start_last) {
                } else {
                    // If no intersection is found, the iter will exit, without returning a result.
                    tmp_path.push(regs_gx);
                    continue;
                }
                // Use current start_ints vector as the return vector.

                // Avoid two consecutive, equal, RegionsCorrs.
                if std::ptr::eq(*regs_gx, *start_last) {
                } else {
                    start_ints.push(regs_gx);
                }
                // Unwind successive goal path items.
                for regs_tx in tmp_path.iter().rev() {
                    start_ints.push(regs_tx);
                }
                //println!("plan_using_least_negative_select_regions3: reurning (1) {}", tools::vec_ref_string(&start_ints));
                return Some(start_ints);
            }

            // Check the last goal_ints item against all start_ints.
            let goal_last = goal_ints.last()?;

            // Save RegionsCorrs traversed, to use if/when an intersection is found.
            let mut tmp_path = Vec::<&RegionsCorr>::new();

            for regs_sx in start_ints.iter() {
                if regs_sx.intersects(goal_last) {
                } else {
                    // If no intersection is found, the iter will exit, without returning a result.
                    // tmp_path will be a waste.
                    tmp_path.push(regs_sx);
                    continue;
                }
                // Avoid two consecutive, equal, RegionsCorrs.
                if std::ptr::eq(regs_sx, goal_last) {
                } else {
                    tmp_path.push(regs_sx);
                }
                // Unwind successive goal path RegionsCorrs.
                for regs_gx in goal_ints.iter().rev() {
                    tmp_path.push(regs_gx);
                }
                //println!("plan_using_least_negative_select_regions3: returning (2) {}", tools::vec_ref_string(&tmp_path));
                return Some(tmp_path);
            }
        } // end loop
    } // end plan_using_least_negative_select_regions3

    /// Return a String representation of a DomainStore.
    fn formatted_string(&self) -> String {
        let mut rc_str = String::from("[");

        for (inx, domx) in self.items.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&domx.to_string());
        }
        rc_str.push(']');

        rc_str
    }

    /// Return the total number of groups in all the domains.
    pub fn number_groups(&self) -> usize {
        let mut tot = 0;
        for domx in self.items.iter() {
            tot += domx.number_groups();
        }
        tot
    }

    /// Return the total number of groups expected in all the domains.
    pub fn number_groups_expected(&self) -> usize {
        let mut tot = 0;
        for domx in self.items.iter() {
            tot += domx.number_groups_expected();
        }
        tot
    }

    /// Print a plan step-by-step, indicating changes.
    pub fn print_plan_detail(&self, plan_str: &PlanStore) {
        self.print_plan_detail2(plan_str, &self.all_current_states());
    }

    /// Print a plan step-by-step, indicating changes.
    pub fn print_plan_detail2(&self, plan_str: &PlanStore, states: &StatesCorr) {
        let mut cur_states = states.clone();

        for planx in plan_str.iter() {
            if planx.is_empty() {
                continue;
            }

            println!("\n  Domain: {}, Plan:", planx.dom_id);
            for stepx in planx.iter() {
                let df = stepx.initial.diff_edge_mask(&stepx.result);
                if let Some(act_id) = stepx.act_id {
                    print!(
                        "    {} Action {:02} -> {}",
                        stepx.initial, act_id, stepx.result
                    );
                } else {
                    print!("    {} Action no -> {}", stepx.initial, stepx.result);
                }
                cur_states[planx.dom_id] = stepx
                    .rule
                    .result_from_initial_state(&cur_states[planx.dom_id]);

                if let AltRule { .. } = &stepx.alt_rule {
                    print!(" Alt_rule: -1");
                }

                for sel_regx in self.select.iter() {
                    if sel_regx.net_value < 0 && sel_regx.regions.is_superset_states(&cur_states) {
                        print!(" in {:+}, {}", sel_regx.regions, sel_regx.net_value);
                    }
                }

                println!("\n     {}", df.mark_ones());
            } // next stepsx
            println!("    {}", cur_states[planx.dom_id]);
        } // next planx
    }

    /// Return the maximum possible regions.
    pub fn maximum_regions(&self) -> RegionsCorr {
        let mut ret_regs = RegionsCorr::new(Vec::<SomeRegion>::with_capacity(self.len()));
        for domx in self.items.iter() {
            ret_regs.push(SomeRegion::new(vec![
                domx.cur_state.new_high(),
                domx.cur_state.new_low(),
            ]));
        }
        ret_regs
    }
} // end impl DomainStore

impl Index<usize> for DomainStore {
    type Output = SomeDomain;
    fn index(&self, i: usize) -> &SomeDomain {
        &self.items[i]
    }
}

impl IndexMut<usize> for DomainStore {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bits::SomeBits;
    use crate::rule::SomeRule;
    use crate::rulestore::RuleStore;
    use crate::sample::SomeSample;
    use crate::step::{AltRuleHint, SomeStep};

    /// Return the number of supersets of a StateStore
    fn number_supersets_of_states(select: &SelectRegionsStore, stas: &StatesCorr) -> usize {
        select
            .items
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_states(stas)))
            .sum()
    }

    #[test]
    fn choose_a_plan() -> Result<(), String> {
        let sta2 = SomeState::new(SomeBits::new_from_string("0x2")?);

        let stp27 = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new_from_string("0b0010->0b0111")?),
            AltRuleHint::NoAlt {},
        );
        let stp7f = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new_from_string("0b0111->0b1111")?),
            AltRuleHint::NoAlt {},
        );
        let stp29 = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new_from_string("0b0010->0b1001")?),
            AltRuleHint::NoAlt {},
        );
        let stp2b = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new_from_string("0b0010->0b1011")?),
            AltRuleHint::NoAlt {},
        );
        let stp9b = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new_from_string("0b1001->0b1011")?),
            AltRuleHint::NoAlt {},
        );
        let stpbf = SomeStep::new(
            0,
            SomeRule::new(&SomeSample::new_from_string("0b1011->0b1111")?),
            AltRuleHint::NoAlt {},
        );

        // Init DomainStore. Domain.
        let mut dmxs = DomainStore::new();
        // Set up first domain.
        dmxs.add_domain(sta2.clone());

        // Set select region.
        let mut regstr1 = RegionsCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("r01XX")?);
        dmxs.add_select(SelectRegions::new(regstr1, -1));

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

        Ok(())
    }

    #[test]
    /// Test case where positive regions the start and goal are in, intersect.
    fn avoidance1x() -> Result<(), String> {
        // Init DomainStore. Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let domx = &mut dmxs[0];

        let sf = SomeState::new_from_string("0b1111")?;

        // Set up action to change the first bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Set select regions.
        let mut regstr1 = RegionsCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("r01X1")?);
        dmxs.add_select(SelectRegions::new(regstr1, -1));

        let mut regstr2 = RegionsCorr::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string("rX101")?);
        dmxs.add_select(SelectRegions::new(regstr2, -2));
        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::new_from_string("0x1")?;
        dmxs[0].set_cur_state(first_state.clone());

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        dmxs[0].get_needs(); // set aggregate changes

        let start_region = RegionsCorr::new(vec![SomeRegion::new(vec![first_state.clone()])]);
        let goal_region = RegionsCorr::new(vec![SomeRegion::new(vec![sf.clone()])]);

        if let Some(planx) =
            dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region)
        {
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
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![]);
        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![]);

        let sd = SomeState::new_from_string("0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Set select regions.
        let mut regstr1 = RegionsCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("r0101")?);
        dmxs.add_select(SelectRegions::new(regstr1, -1));

        let mut regstr2 = RegionsCorr::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string("r1001")?);
        dmxs.add_select(SelectRegions::new(regstr2, -1));
        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::new_from_string("0x1")?;
        dmxs[0].set_cur_state(first_state.clone());

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::new(vec![SomeRegion::new(vec![first_state.clone()])]);
        let goal_region = RegionsCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) =
            dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region)
        {
            let rate = dmxs.rate_plans2(
                &planx,
                &RegionsCorr::new(vec![SomeRegion::new(vec![first_state.clone()])]),
            );
            println!("Plan found: {} rate: {}", planx, rate);
            assert!(dmxs.rate_plans2(&planx, &start_region) == 0);

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
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![]);
        let sd = SomeState::new_from_string("0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![]);
        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);
        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Set select regions.
        let mut regstr1 = RegionsCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("r0x00")?);
        dmxs.add_select(SelectRegions::new(regstr1, -1));

        let mut regstr2 = RegionsCorr::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string("rx100")?);
        dmxs.add_select(SelectRegions::new(regstr2, -1));

        let mut regstr3 = RegionsCorr::with_capacity(1);
        regstr3.push(SomeRegion::new_from_string("r01x1")?);
        dmxs.add_select(SelectRegions::new(regstr3, -1));

        let mut regstr4 = RegionsCorr::with_capacity(1);
        regstr4.push(SomeRegion::new_from_string("r10x1")?);
        dmxs.add_select(SelectRegions::new(regstr4, -1));

        let mut regstr5 = RegionsCorr::with_capacity(1);
        regstr5.push(SomeRegion::new_from_string("r101x")?);
        dmxs.add_select(SelectRegions::new(regstr5, -1));
        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::new_from_string("0x1")?;
        dmxs[0].set_cur_state(first_state.clone());

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::new(vec![SomeRegion::new(vec![first_state.clone()])]);
        let goal_region = RegionsCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) =
            dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region)
        {
            println!("Plan found: {}", planx);
            assert!(dmxs.rate_plans2(&planx, &start_region) == 0);
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
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![]);
        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![]);

        let sd = SomeState::new_from_string("0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Set select regions.
        let mut regstr1 = RegionsCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("r01xx")?);
        dmxs.add_select(SelectRegions::new(regstr1, -1));

        let mut regstr2 = RegionsCorr::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string("r10xx")?);
        dmxs.add_select(SelectRegions::new(regstr2, -1));
        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::new_from_string("0x1")?;
        dmxs[0].set_cur_state(first_state.clone());

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::new(vec![SomeRegion::new(vec![first_state.clone()])]);
        let goal_region = RegionsCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) =
            dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region)
        {
            return Err(format!("Plan found {}?", planx));
        }
        Ok(())
    }

    #[test]
    /// Test case where start and goal regions are not in a non-negative region.
    fn avoidance5() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![]);

        let sd = SomeState::new_from_string("0b1101")?;
        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![]);

        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![]);
        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Set select regions.
        let mut regstr1 = RegionsCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("rxx0x")?);
        dmxs.add_select(SelectRegions::new(regstr1, -1));
        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::new_from_string("0x1")?;
        dmxs[0].set_cur_state(first_state.clone());

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::new(vec![SomeRegion::new(vec![first_state.clone()])]);
        let goal_region = RegionsCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        dmxs[0].get_needs(); // set aggregate changes

        if let Some(planx) =
            dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region)
        {
            println!("Plan found: {} rate {}", planx, dmxs.rate_plans(&planx));
            planx.str_terse();
        } else {
            return Err(format!("No plan ?"));
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
        let init_first_state = SomeState::new_from_string("0x12")?;
        dmxs[0].set_cur_state(init_first_state.clone());

        // Set state for domain 1, using 2 integers for bits.
        let init_state2 = SomeState::new_from_string("0xabcd")?;
        dmxs[1].set_cur_state(init_state2.clone());

        let all_states = dmxs.all_current_states();
        println!("all states {}", all_states);

        assert!(all_states.len() == 2);
        assert!(all_states[0] == init_first_state);
        assert!(all_states[1] == init_state2);

        Ok(())
    }

    #[test]
    /// Test case using adjacent non-negative regions.
    fn avoidance6() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));

        // Add actions.
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Init aggregate needs.
        dmxs.get_needs();

        // Set select regions.

        // Set up dom 0 00XX dependent on dom 1 01XX.
        let mut regstr0 = RegionsCorr::with_capacity(1);
        regstr0.push(SomeRegion::new_from_string("r1100")?);
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Set up dom 0 00XX dependent on dom 1 10XX.
        let mut regstr1 = RegionsCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("r1011")?);
        dmxs.add_select(SelectRegions::new(regstr1, -1));
        dmxs.calc_select();

        let s0 = SomeState::new_from_string("0b0000")?;
        dmxs[0].set_cur_state(s0.clone());

        let sd = SomeState::new_from_string("0b1101")?;

        let start_region = RegionsCorr::new(vec![SomeRegion::new(vec![s0.clone()])]);
        let goal_region = RegionsCorr::new(vec![SomeRegion::new(vec![sd.clone()])]);

        // Try making plans.
        if let Some(plans) =
            dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region)
        {
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
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));

        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);

        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Set select region.
        let max_region = SomeRegion::new_from_string("rxxxx")?;

        // Set up dom 0 negative regions.
        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(SomeRegion::new_from_string("r01x1")?);
        regstr0.push(max_region.clone());
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(SomeRegion::new_from_string("rx101")?);
        regstr0.push(max_region.clone());
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Set up dom 1 negative regions.
        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(max_region.clone());
        regstr0.push(SomeRegion::new_from_string("r011x")?);
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(max_region.clone());
        regstr0.push(SomeRegion::new_from_string("rx111")?);
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s0 = SomeState::new_from_string("0x0")?;
        dmxs[0].set_cur_state(s0.clone());

        let s1 = SomeState::new_from_string("0x1")?;
        dmxs[1].set_cur_state(s1.clone());

        let sf = SomeState::new_from_string("0xf")?;
        let se = SomeState::new_from_string("0xe")?;

        let start_region = RegionsCorr::new(vec![
            SomeRegion::new(vec![s0.clone()]),
            SomeRegion::new(vec![s1.clone()]),
        ]);
        let goal_region = RegionsCorr::new(vec![
            SomeRegion::new(vec![sf.clone()]),
            SomeRegion::new(vec![se.clone()]),
        ]);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        if let Some(plans) =
            dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region)
        {
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
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));

        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);

        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Set up negative regions.
        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(SomeRegion::new_from_string("r00xx")?);
        regstr0.push(SomeRegion::new_from_string("rxx11")?);
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(SomeRegion::new_from_string("r11xx")?);
        regstr0.push(SomeRegion::new_from_string("r01xx")?);
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = SomeState::new_from_string("0b0101")?;
        dmxs[0].set_cur_state(s5.clone());

        let s7 = SomeState::new_from_string("0b0111")?;
        dmxs[1].set_cur_state(s7.clone());

        let start_regions = RegionsCorr::new(vec![
            SomeRegion::new(vec![s5.clone()]),
            SomeRegion::new(vec![s7.clone()]),
        ]);

        let s9 = SomeState::new_from_string("0b1001")?;

        let goal_regions = RegionsCorr::new(vec![
            SomeRegion::new(vec![s9.clone()]),
            SomeRegion::new(vec![s7.clone()]),
        ]);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        if let Some(plans) =
            dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions)
        {
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
                cur_states[planx.dom_id] =
                    planx.result_from_initial_state(&cur_states[planx.dom_id]);
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
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));

        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);

        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);
        dmxs[1].add_action(vec![]);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Set up negative regions.
        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(SomeRegion::new_from_string("r000x")?);
        regstr0.push(SomeRegion::new_from_string("rxx11")?);
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(SomeRegion::new_from_string("r11x1")?);
        regstr0.push(SomeRegion::new_from_string("r01xx")?);
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = SomeState::new_from_string("0b0101")?;
        dmxs[0].set_cur_state(s5.clone());

        let s7 = SomeState::new_from_string("0b0111")?;
        dmxs[1].set_cur_state(s7.clone());

        let start_regions = RegionsCorr::new(vec![
            SomeRegion::new(vec![s5.clone()]),
            SomeRegion::new(vec![s7.clone()]),
        ]);

        let s9 = SomeState::new_from_string("0b1001")?;

        let goal_regions = RegionsCorr::new(vec![
            SomeRegion::new(vec![s9.clone()]),
            SomeRegion::new(vec![s7.clone()]),
        ]);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        if let Some(plans) =
            dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions)
        {
            print!("Plans {}", plans);
            let rate = dmxs.rate_plans2(&plans, &start_regions);
            print!(", rate {}", rate);
            println!(" ");
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
        let mut regstr1 = RegionsCorr::with_capacity(2);
        regstr1.push(SomeRegion::new_from_string("r00000x0x")?);
        regstr1.push(SomeRegion::new_from_string("rXXXXXX10_1XXX_XXXX")?);

        let mut regstr2 = RegionsCorr::with_capacity(2);
        regstr2.push(SomeRegion::new_from_string("r00000xx1")?);
        regstr2.push(SomeRegion::new_from_string("rXXXXXX10_1XXX_XXXX")?);

        let mut regstr3 = RegionsCorr::with_capacity(2);
        regstr3.push(SomeRegion::new_from_string("r0000x1x1")?);
        regstr3.push(SomeRegion::new_from_string("rXXXXXX10_1XXX_XXXX")?);

        let mut regstr4 = RegionsCorr::with_capacity(2);
        regstr4.push(SomeRegion::new_from_string("r00001110")?);
        regstr4.push(SomeRegion::new_from_string("rXXXXXX10_1XXX_XXXX")?);

        // Add select region stores.
        dmxs.add_select(SelectRegions::new(regstr1, 1));
        dmxs.add_select(SelectRegions::new(regstr2, 1));
        dmxs.add_select(SelectRegions::new(regstr3, 1));
        dmxs.add_select(SelectRegions::new(regstr4, 1));
        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::new_from_string("0x12")?;
        dmxs[0].set_cur_state(first_state.clone());

        // Set state for domain 1.
        let state2 = SomeState::new_from_string("0xabcd")?;
        dmxs[1].set_cur_state(state2.clone());

        dmxs.boredom = 0;
        dmxs.boredom_limit = 0;

        let num_sup = number_supersets_of_states(
            &dmxs.select,
            &StatesCorr::new(vec![first_state.clone(), state2.clone()]),
        );
        println!("\nNumber supersets: {num_sup}",);
        assert!(num_sup == 0);

        if let Some(needx) = dmxs.check_select() {
            println!("\nCheck_select returns {}", needx);
        } else {
            return Err(format!("No need found?"));
        }

        // Set state for domain 0.
        let first_state = SomeState::new_from_string("0x05")?;
        dmxs[0].set_cur_state(first_state.clone());

        // Set state for domain 1.
        let state2 = SomeState::new_from_string("0xa28d")?;
        dmxs[1].set_cur_state(state2.clone());

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
    fn exit_select_needs() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));

        let mut regstr1 = RegionsCorr::with_capacity(1);
        let neg_reg1 = SomeRegion::new_from_string("rX1XX")?;

        regstr1.push(neg_reg1.clone());

        // Add selectregions.
        dmxs.add_select(SelectRegions::new(regstr1.clone(), -1));

        let mut regstr2 = RegionsCorr::with_capacity(1);
        let neg_reg2 = SomeRegion::new_from_string("r1XX1")?;
        regstr2.push(neg_reg2.clone());

        // Add select regionstores.
        dmxs.add_select(SelectRegions::new(regstr2.clone(), -1));

        // Set state for domain 0, using 1 integer for bits.
        let first_state = SomeState::new_from_string("0xd")?;
        dmxs[0].set_cur_state(first_state.clone());

        // Finish select regions setup.
        dmxs.calc_select();
        println!("select regs: {}", dmxs.select.to_string());

        // Get exit needs.
        if let Some(nds) = dmxs.check_select() {
            println!("needs len {} {}", nds.len(), nds);
            assert!(nds.len() == 1);
            println!("needs: {}", nds);
            for ndsx in nds.iter() {
                match ndsx.target() {
                    ATarget::DomainRegions { regions } => {
                        assert!(!regstr1.intersects(regions));
                        assert!(!regstr2.intersects(regions));
                    }
                    _ => {
                        panic!("SNH");
                    }
                }
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
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));

        // Add action to domain 0.
        dmxs[0].add_action(vec![]);

        // Load select regions
        let mut regstr1 = RegionsCorr::with_capacity(1);
        regstr1.push(SomeRegion::new_from_string("rxx0x")?);

        let mut regstr2 = RegionsCorr::with_capacity(1);
        regstr2.push(SomeRegion::new_from_string("r00x1")?);

        let mut regstr3 = RegionsCorr::with_capacity(1);
        regstr3.push(SomeRegion::new_from_string("r11x1")?);

        let mut regstr4 = RegionsCorr::with_capacity(1);
        regstr4.push(SomeRegion::new_from_string("r10x0")?);

        // Add select region stores.
        dmxs.add_select(SelectRegions::new(regstr1, 4));
        dmxs.add_select(SelectRegions::new(regstr2, -2));
        dmxs.add_select(SelectRegions::new(regstr3, -4));
        dmxs.add_select(SelectRegions::new(regstr4, -5));
        dmxs.calc_select();

        assert!(dmxs.select_positive.len() == 4);
        assert!(dmxs.select_positive.contains(&SelectRegions::new(
            RegionsCorr::new(vec![SomeRegion::new_from_string("rx100")?]),
            4
        )));
        assert!(dmxs.select_positive.contains(&SelectRegions::new(
            RegionsCorr::new(vec![SomeRegion::new_from_string("r0x00")?]),
            4
        )));
        assert!(dmxs.select_positive.contains(&SelectRegions::new(
            RegionsCorr::new(vec![SomeRegion::new_from_string("r010x")?]),
            4
        )));
        assert!(dmxs.select_positive.contains(&SelectRegions::new(
            RegionsCorr::new(vec![SomeRegion::new_from_string("r1001")?]),
            4
        )));

        assert!(dmxs.select_non_negative.len() == 4);
        assert!(dmxs.select_non_negative.contains(&SelectRegions::new(
            RegionsCorr::new(vec![SomeRegion::new_from_string("rx1x0")?]),
            0
        )));
        assert!(dmxs.select_non_negative.contains(&SelectRegions::new(
            RegionsCorr::new(vec![SomeRegion::new_from_string("r0xx0")?]),
            0
        )));
        assert!(dmxs.select_non_negative.contains(&SelectRegions::new(
            RegionsCorr::new(vec![SomeRegion::new_from_string("r01xx")?]),
            0
        )));
        assert!(dmxs.select_non_negative.contains(&SelectRegions::new(
            RegionsCorr::new(vec![SomeRegion::new_from_string("r10x1")?]),
            0
        )));

        Ok(())
    }

    #[test]
    /// Test case using two domains, like avoidance8, but a way around traps.
    fn avoidance10() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));

        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);
        dmxs[0].add_action(vec![]);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::new_from_string("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::new_from_string("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::new_from_string("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::new_from_string("0b1111->0b0111")?);

        // Set up negative regions.
        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(SomeRegion::new_from_string("r01xx")?);
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let mut regstr0 = RegionsCorr::with_capacity(2);
        regstr0.push(SomeRegion::new_from_string("r10xx")?);
        dmxs.add_select(SelectRegions::new(regstr0, -2));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s3 = SomeState::new_from_string("0b0011")?;
        dmxs[0].set_cur_state(s3.clone());

        let start_regions = RegionsCorr::new(vec![SomeRegion::new(vec![s3.clone()])]);

        let sf = SomeState::new_from_string("0b1111")?;

        let goal_regions = RegionsCorr::new(vec![SomeRegion::new(vec![sf.clone()])]);

        println!("\nActions {}\n", dmxs[0].actions);

        // Try making plans.
        if let Some(plans) =
            dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions)
        {
            print!("Plans {}", plans);
            let rate = dmxs.rate_plans2(&plans, &start_regions);
            print!(", rate {}", rate);
            println!(" ");
            assert!(rate == -1);
        } else {
            return Err(format!("No plan found?"));
        }
        Ok(())
    }

    #[test]
    /// Test running plans in parallel, as in going from SelectRegions fragment to
    /// intersection of another SelectRegions fragment.
    fn run_plans_in_parallel() -> Result<(), String> {
        // Init DomainStore.
        let mut dmxs = DomainStore::new();

        // Add domains.
        dmxs.add_domain(SomeState::new(SomeBits::new(4)));
        dmxs.add_domain(SomeState::new(SomeBits::new(3)));
        dmxs.add_domain(SomeState::new(SomeBits::new(2)));

        // Add an action for each domain.
        dmxs[0].add_action(vec![RuleStore::new(vec![SomeRule::new_from_string(
            "XX/XX/XX/Xx",
        )?])]);
        dmxs[1].add_action(vec![RuleStore::new(vec![SomeRule::new_from_string(
            "XX/XX/Xx",
        )?])]);
        dmxs[2].add_action(vec![RuleStore::new(vec![SomeRule::new_from_string(
            "XX/Xx",
        )?])]);

        // Set domain starting states.
        dmxs[0].set_cur_state(SomeState::new_from_string("0b0000")?);
        dmxs[1].set_cur_state(SomeState::new_from_string("0b001")?);
        dmxs[2].set_cur_state(SomeState::new_from_string("0b10")?);

        // Set up plan for domain 0, action 0.
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/00/00/01")?,
            AltRuleHint::NoAlt {},
        );
        let plan0 = SomePlan::new(0, vec![stp1]);

        // Set up plan for domain 1, action 0, no-op.
        let stp1 = SomeStep::new_no_op(SomeRule::new_from_string("00/00/11")?);
        let plan1 = SomePlan::new(1, vec![stp1]);

        // Set up plan for domain 2, action 0.
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("11/01")?,
            AltRuleHint::NoAlt {},
        );
        let plan2 = SomePlan::new(2, vec![stp1]);

        // Set up PlansCorr.
        let plnsc1 = PlansCorr::new(vec![plan0, plan1, plan2]);
        println!("{plnsc1}");

        let before = dmxs.all_current_states();
        dmxs.run_planscorr(&plnsc1);
        let after = dmxs.all_current_states();
        println!("Before: {before}");
        println!("After:  {after}");
        assert!(after[0] == SomeState::new_from_string("0b0001")?);
        assert!(after[1] == SomeState::new_from_string("0b001")?);
        assert!(after[2] == SomeState::new_from_string("0b11")?);

        //assert!(1 == 2);
        Ok(())
    }
}
