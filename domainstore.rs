//! The DomainStore struct, a vector of SomeDomain structs,
//! and values/methods that manage the domains.

use crate::domain::SomeDomain;
use crate::need::{SomeNeed, TO_SELECT_REGION_PRIORITY};
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planscorr::PlansCorr;
use crate::planscorrstore::PlansCorrStore;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionscorr::RegionsCorr;
use crate::regionscorrstore::RegionsCorrStore;
use crate::selectregions::SelectRegions;
use crate::selectregionsstore::SelectRegionsStore;
use crate::state::SomeState;
use crate::statescorr::StatesCorr;
use crate::step::{AltRuleHint::AltRule, SomeStep};
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

/// The three possible results of evaluating a need.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum NeedPlan {
    AtTarget {},
    PlanFound { plan: PlansCorrStore },
}

/// An InxPlan struct, containing an index to a SomeNeed vector, and a SomePlan struct.
///
/// An integer is used instead of &SomeNeed to avoid borrow checker problems.
#[readonly::make]
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct InxPlan {
    /// Index to a need in a NeedStore.
    pub inx: usize,
    /// Plan to satisfy need, may be empty if the current state satisfies the need, or None if no plan found.
    pub plans: NeedPlan,
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
    /// Times visited, order corresponding to, positive select regions.
    pub times_visited: Vec<usize>,
    /// Non-negative, may be, or overlap, positive, regions.
    pub rc_non_negative: RegionsCorrStore,

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
            rc_non_negative: RegionsCorrStore::new(vec![]),
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
            println!("  {selx}");
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
                println!("  {selx}");
            }
            println!("  To seek, when no other needs can be done.");
            self.times_visited = vec![0; self.select_positive.len()];
        }

        // Calc non-negative regions.
        let mut non_neg_select = RegionsCorrStore::new(vec![self.maximum_regions()]);

        for selx in self.select.iter() {
            if selx.neg_value < 0 {
                non_neg_select = non_neg_select.subtract_regionscorr(&selx.regions);
            }
        }
        if non_neg_select.is_not_empty() {
            self.rc_non_negative = non_neg_select;

            println!(
                "\nNon-negative RCs, maximum RC regions minus negative SR regions ({}):",
                self.rc_non_negative.len()
            );
            for rcx in self.rc_non_negative.iter() {
                println!("  {rcx}");
            }
            println!("  Seek when exiting a negative SR the current state is in.");

            // Double check.
            for rcx in self.rc_non_negative.iter() {
                for selz in self.select.iter() {
                    if selz.neg_value < 0 {
                        assert!(!selz.regions.intersects(rcx));
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
                println!("  {selx}");
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
        //println!("domainstore::get_needs");
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

    /// Run a PlansCorrStore.
    /// A series of steps from one SelectRegions fragment to another, to another.
    pub fn run_planscorrstore(&mut self, plnsstr: &PlansCorrStore) -> Result<usize, String> {
        //println!("domainstore::run_planscorrstore: {plnsstr}");
        let mut not_first = false;
        let mut steps_run = 0;

        for plnscrx in plnsstr.iter() {
            if plnscrx
                .initial_regions()
                .is_superset_of(&self.all_current_regions())
            {
                if not_first {
                    println!("----");
                }
                match self.run_planscorr(plnscrx) {
                    Ok(num) => {
                        not_first = true;
                        steps_run += num;
                    }
                    Err(errstr) => return Err(errstr),
                }
            } else {
                return Err(format!(
                    "planscorr initial regions {} not within current regions {}",
                    plnscrx.initial_regions(),
                    self.all_current_regions()
                ));
            }
        }
        Ok(steps_run)
    }

    /// Run a PlansCorr, plans will be run in parallel.
    pub fn run_planscorr(&mut self, plns: &PlansCorr) -> Result<usize, String> {
        let mut steps_run = 0;

        let vecb = plns
            .plans
            .items
            .par_iter()
            .zip(self.items.par_iter_mut())
            .map(|(plnx, domx)| domx.run_plan(plnx, 0))
            .collect::<Vec<Result<usize, String>>>();

        for rsltx in vecb {
            match rsltx {
                Ok(num) => steps_run += num,
                Err(errstr) => return Err(errstr),
            }
        }
        Ok(steps_run)
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

        let mut can = Vec::<InxPlan>::new();

        for (inx, ndx) in self.needs.iter().enumerate() {
            if let Some(dom_id) = ndx.dom_id() {
                if ndx.satisfied_by(&self.items[dom_id].cur_state) {
                    //println!("need {ndx} At Target");
                    can.push(InxPlan {
                        inx,
                        plans: NeedPlan::AtTarget {},
                        rate: 0,
                        desired_num_bits_changed: 0,
                        process_num_bits_changed: 0,
                    });
                }
            }
        }

        if !can.is_empty() {
            self.can_do = can;
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
            TO_SELECT_REGION_PRIORITY
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
            let inx_ndpln = (cur_pri_start..cur_pri_end)
                .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
                .map(|nd_inx| {
                    (
                        nd_inx,
                        self.plan_using_least_negative_select_regions_for_target(
                            self.needs[nd_inx].dom_id(),
                            &self.needs[nd_inx].target(),
                        ),
                    )
                })
                .collect::<Vec<(usize, Result<NeedPlan, Vec<String>>)>>();

            // See if any plans have been found.
            let mut cant = Vec::<usize>::new();

            for (inx, ndplnx) in inx_ndpln {
                if let Ok(ndplnx) = ndplnx {
                    match ndplnx {
                        NeedPlan::AtTarget {} => {
                            can.push(InxPlan {
                                inx,
                                plans: ndplnx,
                                rate: 0,
                                desired_num_bits_changed: 0,
                                process_num_bits_changed: 0,
                            });
                        }
                        NeedPlan::PlanFound { plan: ref plany } => {
                            let ratex = plany.value;
                            let cur_regs = self.all_current_regions();
                            let desired_num_bits_changed =
                                cur_regs.distance(&plany.result_regions());
                            let process_num_bits_changed = plany.num_bits_changed();
                            can.push(InxPlan {
                                inx,
                                plans: ndplnx,
                                rate: ratex,
                                desired_num_bits_changed,
                                process_num_bits_changed,
                            });
                        }
                    }
                } else {
                    cant.push(inx);
                }
            } // next inx, ndplnx

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

            // Save can do results.
            self.can_do = can;

            return;
        } // End loop
          // Unreachable, since there is no break command.
    } // end evaluate_needs

    /// Create a RegionsStoreCorr of maximum regions, except a for given domain.
    pub fn maximum_regions_except(&self, dom_id: usize, targ: &SomeRegion) -> RegionsCorr {
        let mut regs = self.maximum_regions();

        regs[dom_id] = targ.clone();

        regs
    }

    /// Return a rate for a plan, based on the sum of values of select regions the plan passes through.
    fn rate_planscorrstore(&self, plans: &PlansCorrStore) -> isize {
        let mut cnt = 0;
        for plnscrx in plans.iter() {
            cnt += self.rate_planscorr(plnscrx);
        }
        cnt
    }

    /// Return a rate for a set of plans, based on the sum of values of negative select regions the plan passes through.
    /// This ignores the select regions a plan starts in.
    /// A square in a region can exit the region to an adjacent square by changing one bit.
    /// A square adjacent to a region can enter the region by changing one bit.
    fn rate_planscorr(&self, planscs: &PlansCorr) -> isize {
        let mut cur_regions = self.all_current_regions();

        // Collect path regions for each plan.
        for (dom_id, planx) in planscs.iter().enumerate() {
            cur_regions[dom_id] = planx.path_for(&cur_regions[dom_id]);
        }
        self.select.rate_by_negative_regions(&cur_regions)
    }

    /// Get plans to move to a goal region, choose a plan.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    /// The within argument restricts where a rule should start, and restricts unwanted changes that may be included with wanted changes.
    pub fn get_plans(
        &self,
        dom_id: usize,
        from_region: &SomeRegion,
        goal_region: &SomeRegion,
        within: &SomeRegion,
    ) -> Result<PlanStore, Vec<String>> {
        debug_assert!(from_region.num_bits() == goal_region.num_bits());
        debug_assert!(within.num_bits() == from_region.num_bits());
        debug_assert!(within.is_superset_of(from_region));
        debug_assert!(within.is_superset_of(goal_region));
        //println!("domainstore: get_plans: dom {dom_id} from {from_region} goal {goal_region}");

        self.items[dom_id].make_plans(from_region, goal_region, within)
    }

    /// Choose a plan from a vector of PlanStores, for vector of PlanStore references.
    /// Return index of plan chosen.
    pub fn choose_a_plan(&self, plans: &[PlansCorrStore]) -> usize {
        //println!("choose_a_plan: num plans = {}", plans.len());
        assert!(!plans.is_empty());

        // No choice to be made.
        if plans.len() == 1 {
            return 0;
        }

        // Gather plan rate data.
        let mut rates = Vec::<isize>::with_capacity(plans.len());

        for planx in plans.iter() {
            rates.push(planx.value - planx.num_altrules());
        }

        // Get max rate.
        let max_rate = rates.iter().max().unwrap();
        //println!("max rate = {max_rate}");

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
            .map(|inx| plans[*inx].number_steps_to_run())
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

        // Gather plan rates.
        let mut rts = vec![];
        for inxpln in self.can_do.iter() {
            match &inxpln.plans {
                NeedPlan::AtTarget {} => {
                    rts.push(2000 - self.needs[inxpln.inx].priority() as isize)
                } // change lower priority to higher rate.
                NeedPlan::PlanFound { plan: plnx } => rts.push(plnx.value),
            };
        }
        // Find maximum rate.
        let max = rts.iter().max().unwrap();

        // Gather can_do indexes that match the maximum rate.
        let mut inxs = vec![];
        for (inx, rtx) in rts.iter().enumerate() {
            if rtx == max {
                inxs.push(inx);
            }
        }
        // Select a can_do item by index.
        let inx = inxs[rand::thread_rng().gen_range(0..inxs.len())];

        println!(
            "\nNeed chosen: {:2} {} {}",
            inx,
            self.needs[self.can_do[inx].inx],
            match &self.can_do[inx].plans {
                NeedPlan::AtTarget {} => "At Target".to_string(),
                NeedPlan::PlanFound { plan: plnx } => plnx.str_terse(),
            }
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

            if let Some(near_nn_regs) = self.closest_rc_regions(&all_regs) {
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
                target_select: psupx.clone(),
                priority: adjust as usize,
                times_visited: self.times_visited[inx],
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

        println!(
            "\nDom: {dom_id} Current State: {cur_state} Available changes: {}",
            if let Some(agg_cngs) = self.items[dom_id].aggregate_changes() {
                format!("{agg_cngs}")
            } else {
                "None".to_string()
            }
        );
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
                        match &ndplnx.plans {
                            NeedPlan::AtTarget {} => "At Target".to_string(),
                            NeedPlan::PlanFound { plan: plnx } => plnx.str_terse(),
                        },
                        ndplnx.desired_num_bits_changed,
                        ndplnx.process_num_bits_changed,
                        ndplnx.rate,
                    );
                } else {
                    println!(
                        "{:2} {} {}",
                        inx,
                        self.needs[ndplnx.inx],
                        match &ndplnx.plans {
                            NeedPlan::AtTarget {} => "At Target".to_string(),
                            NeedPlan::PlanFound { plan: plnx } => plnx.str_terse(),
                        }
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

    /// Return a plan for a given start RegionsCorr, goal RegionsCorr, within a RegionsCorrStore.
    fn plan_using_least_negative_select_regions_get_plan(
        &self,
        start_regs: &RegionsCorr,
        goal_regs: &RegionsCorr,
        within: &RegionsCorrStore,
    ) -> Result<PlansCorrStore, Vec<String>> {
        //println!(
        //    "plan_using_least_negative_select_regions_get_plan: starting: start {start_regs} goal: {goal_regs} within {within}"
        //);
        let plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .map(|_| self.plan_using_least_negative_select_regions2(start_regs, goal_regs, within))
            .collect::<Vec<Result<PlansCorrStore, Vec<String>>>>();

        let mut plans2 = Vec::<PlansCorrStore>::new();
        let mut problems = Vec::<String>::new();
        for rsltx in plans {
            match rsltx {
                Ok(plnscs) => {
                    //plan2.set_value(self.rate_planscorrstore(&plnscs));
                    plans2.push(plnscs);
                }
                Err(errvec) => {
                    for errstr in errvec {
                        if !problems.contains(&errstr) {
                            problems.push(errstr);
                        }
                    }
                }
            };
        }

        if plans2.is_empty() {
            return Err(problems);
        }

        let inx = self.choose_a_plan(&plans2);
        let ret_path = plans2.swap_remove(inx);
        //println!("plan_using_least_negative_select_regions_get_plan: returning {ret_path}");
        Ok(ret_path)
    }

    /// Find a plan for a change to a specific domain, possibly changing other domain states
    /// to avoid negative SelectRegions.
    pub fn plan_using_least_negative_select_regions_for_target(
        &self,
        dom_id: Option<usize>,
        target: &ATarget,
    ) -> Result<NeedPlan, Vec<String>> {
        let from = self.all_current_regions();

        // Calculate the goal regions.  These are needed for ATarget::State and ATarget::Region as
        // other domain regions may need to be changed to atain the target and avoid negative conditions,
        // which are defined as superset of all domain current states.
        let goal = match target {
            ATarget::State { state } => &self
                .maximum_regions_except(dom_id.unwrap(), &SomeRegion::new(vec![(*state).clone()])),
            ATarget::Region { region } => &self.maximum_regions_except(dom_id.unwrap(), region),
            ATarget::DomainRegions { regions } => regions,
        };
        self.plan_using_least_negative_select_regions(&from, goal)
    }

    /// Like Real Life, formulate a direct plan,
    /// notice there are some negative aspects,
    /// then try to form a plan that avoids the negative.
    fn plan_using_least_negative_select_regions(
        &self,
        start_regs: &RegionsCorr,
        goal_regs: &RegionsCorr,
    ) -> Result<NeedPlan, Vec<String>> {
        //println!(
        //    "plan_using_least_negative_select_regions: starting: start {start_regs} goal: {goal_regs}"
        //);
        assert!(start_regs.len() == self.len());
        assert!(goal_regs.len() == self.len());

        // Check if no plans are needed.
        if start_regs.is_subset_of(goal_regs) {
            //println!("domainstore::plan_using_least_negative_select_regions: return 1");
            return Ok(NeedPlan::AtTarget {});
        }

        // Define last_results.
        let mut last_results: Result<NeedPlan, Vec<String>>;

        // Test direct path.
        match self.plan_using_least_negative_select_regions_get_plan(
            start_regs,
            goal_regs,
            &RegionsCorrStore::new(vec![start_regs.union(goal_regs)]),
        ) {
            Ok(planx) => {
                if self.rate_planscorrstore(&planx) == 0 {
                    //println!("domainstore::plan_using_least_negative_select_regions: return 3");
                    return Ok(NeedPlan::PlanFound { plan: planx });
                } else {
                    // Save result, maybe something better will be found, below.
                    last_results = Ok(NeedPlan::PlanFound { plan: planx });
                }
            }
            Err(errstr) => last_results = Err(errstr),
        };

        // Successively subtract out most negative regions first, until no path can be found.
        // Find first min.
        let mut next_most_min = 0;
        for selx in self.select.iter() {
            if selx.neg_value < next_most_min {
                next_most_min = selx.neg_value;
            }
        }
        //println!("domainstore::plan_using_least_negative_select_regions: initial next most min is {next_most_min}");

        let mut sel_regs = RegionsCorrStore::new(vec![self.maximum_regions()]);

        // Find successive mins, lt 0.
        while next_most_min < 1 {
            //println!("domainstore::plan_using_least_negative_select_regions: next most min is {next_most_min}");
            // Find next most min num.
            let mut next_most_min2 = 1;
            for selx in self.select.iter() {
                if selx.neg_value > next_most_min && selx.neg_value < next_most_min2 {
                    next_most_min2 = selx.neg_value;
                }
            }

            // Process all items with next most min num.
            for selx in self.select.iter() {
                if selx.neg_value == next_most_min {
                    // Subtract select regions.
                    // It is possible that the intersection of the remainder select regions
                    // with the start_regs, and goal_regs, decreases, or even disappears.
                    // The function get_path_through_select_regions will adapt, or return None.
                    sel_regs = sel_regs.subtract_regionscorr(&selx.regions);
                }
            }
            // Test remainder regions.
            match self
                .plan_using_least_negative_select_regions_get_plan(start_regs, goal_regs, &sel_regs)
            {
                Ok(mut planx) => {
                    planx.set_value(if next_most_min2 > 0 {
                        0
                    } else {
                        next_most_min2
                    });
                    last_results = Ok(NeedPlan::PlanFound { plan: planx });
                }
                Err(_errstr) => {
                    return last_results;
                }
            }
            next_most_min = next_most_min2;
        }
        // Return previous results.
        last_results
    }

    /// Return the nearest non-negative regions.
    pub fn closest_rc_regions(&self, from_regs: &RegionsCorr) -> Option<&RegionsCorr> {
        // Find closest non-negative SelectRegions, if any.
        debug_assert!(from_regs.len() == self.len());

        let mut min_distance = usize::MAX;
        let mut targets = Vec::<&RegionsCorr>::new();
        for regsx in self.rc_non_negative.iter() {
            let dist = from_regs.distance(regsx);
            if dist < min_distance {
                min_distance = dist;
                targets = Vec::<&RegionsCorr>::new();
            }
            if dist == min_distance {
                targets.push(regsx);
            }
        } // next regsx
        if targets.is_empty() {
            return None;
        }
        // Choose a close non-negative SelectRegion to move to.
        Some(targets[rand::thread_rng().gen_range(0..targets.len())])
    }

    /// Return plans for a change from start to goal.
    fn plan_using_least_negative_select_regions2(
        &self,
        start_regs: &RegionsCorr,
        goal_regs: &RegionsCorr,
        select_regions: &RegionsCorrStore,
    ) -> Result<PlansCorrStore, Vec<String>> {
        debug_assert!(start_regs.is_congruent(goal_regs));
        //println!(
        //    "plan_using_least_negative_select_regions2: starting: start {start_regs} goal: {goal_regs} select {}", select_regions
        //);

        // Check no plan needed.
        if goal_regs.is_superset_of(start_regs) {
            return Err(vec![format!("domainstore::plan_using_least_negative_select_regions2: goal_regs {goal_regs} superset of start_regs {start_regs}")]);
        }

        // Check if start_regs and goal_regs are in the same RegionsCorr.
        for selx in select_regions.iter() {
            if selx.is_superset_of(start_regs) && selx.intersects(goal_regs) {
                match self.make_plans(
                    start_regs,
                    &selx.intersection(goal_regs).expect("SNH"),
                    selx,
                ) {
                    Ok(plans) => {
                        //assert!(plans.is_valid());
                        let mut plncs = PlansCorrStore::new(vec![plans]);
                        plncs.set_value(self.rate_planscorrstore(&plncs));
                        //println!("plan_using_least_negative_select_regions2: returning (1) {plncs}");
                        return Ok(plncs);
                    }
                    Err(errvec) => return Err(errvec),
                };
            }
        }

        let mut ret_planscorrstore = PlansCorrStore::new(vec![]);

        // Try generating multiple paths.
        let mut mid_paths = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .filter_map(|_| {
                self.get_path_through_select_regions(start_regs, goal_regs, select_regions)
            })
            .collect::<Vec<RegionsCorrStore>>();

        if mid_paths.is_empty() {
            return
            Err(vec![format!("domainstore::plan_using_least_negative_select_regions2: mid_paths from {start_regs} to {goal_regs} within {select_regions} failed")]);
        }

        // Randomly choose a possible path.
        let pathx = mid_paths.swap_remove(rand::thread_rng().gen_range(0..mid_paths.len()));

        let mut cur_regs = start_regs.clone(); // Borrow checker thinks the above map is still in force?

        let mut mid_plans = PlansCorrStore::new(vec![]);
        for inx in 1..(pathx.len() - 1) {
            if let Some(intx) = pathx[inx].intersection(&pathx[inx + 1]) {
                match self.make_plans(&cur_regs, &intx, &pathx[inx]) {
                    Ok(plans) => {
                        //println!("domainstore::plan_using_least_negative_select_regions2: for {cur_regs} to {intx} plans found {plans}");
                        match mid_plans.link(&PlansCorrStore::new(vec![plans])) {
                            Ok(mid_plans2) => {
                                //println!("mid plans {mid_plans}");
                                cur_regs = mid_plans2.result_regions();
                                mid_plans = mid_plans2;
                            }
                            Err(errstr) => return Err(vec![errstr]),
                        };
                    }
                    Err(errstr) => return Err(errstr),
                }
            } else {
                return Err(vec![format!("domainstore::plan_using_least_negative_select_regions2: {} does not intersect {}", pathx[inx], pathx[inx + 1])]);
            }
        }
        if mid_plans.is_empty() {
            return Err(vec![format!("domainstore::plan_using_least_negative_select_regions2: plans for {pathx} mid_plans not found")]);
        }

        // Add mid_plans to existing start_plan_store, if any.
        ret_planscorrstore = ret_planscorrstore.link(&mid_plans).unwrap();

        assert!(ret_planscorrstore
            .result_from_initial_regions(start_regs)
            .expect("SNH")
            .intersects(goal_regs));
        //println!("plan_using_least_negative_select_regions2: returning (2) {ret_planscorrstore}");
        Ok(ret_planscorrstore)
    } // end plan_using_least_negative_select_regions2

    /// Massage a path for return.
    fn get_path_through_select_regions2(&self, path: &[&RegionsCorr]) -> RegionsCorrStore {
        let num_items = path.len();
        let mut ret_path = RegionsCorrStore::with_capacity(num_items);

        ret_path.push(path[0].intersection(path[1]).expect("SNH"));

        for itemx in path.iter().take(num_items - 1).skip(1) {
            ret_path.push((*itemx).clone());
        }

        ret_path.push(
            path[num_items - 1]
                .intersection(path[num_items - 2])
                .expect("SNH"),
        );

        ret_path
    }

    /// Return a series of intersecting RegionsCorrs, to
    /// guide a path between start and goal.
    fn get_path_through_select_regions<'a>(
        &'a self,
        start_regs: &'a RegionsCorr,
        goal_regs: &'a RegionsCorr,
        select_regions: &'a RegionsCorrStore,
    ) -> Option<RegionsCorrStore> {
        //println!(
        //    "get_path_through_select_regions: starting: start {start_regs} goal: {goal_regs} select {}", select_regions
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
                if start_ints.len() > 1 && selx.intersects(start_regs) {
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
                if goal_ints.len() > 1 && selx.intersects(goal_regs) {
                    continue;
                }

                goal_ints.push(selx);
                goal_added = true;
                break;
            } // next pick.

            // Check if done, with no result.
            if start_added || goal_added {
            } else {
                //println!("get_path_through_select_regions: returning (1) no new intersections found");
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
                //println!("get_path_through_select_regions: returning (1) {}", tools::vec_ref_string(&start_ints));
                return Some(self.get_path_through_select_regions2(&start_ints));
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
                //println!("get_path_through_select_regions: returning (2) {}", tools::vec_ref_string(&tmp_path));
                return Some(self.get_path_through_select_regions2(&tmp_path));
            }
        } // end loop
    } // end get_path_through_select_regions

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
    pub fn print_planscorrstore_detail(&self, plan_str: &PlansCorrStore) {
        self.print_planscorrstore_detail2(plan_str, &self.all_current_states());
    }

    /// Print a plan step-by-step, indicating changes.
    pub fn print_planscorrstore_detail2(&self, planscsx: &PlansCorrStore, states: &StatesCorr) {
        let mut cur_states = states.clone();

        for planscx in planscsx.iter() {
            for (dom_id, planx) in planscx.iter().enumerate() {
                if planx.causes_change() {
                    println!("\n  Domain: {}, Plan:", dom_id);
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
                        cur_states[dom_id] =
                            stepx.rule.result_from_initial_state(&cur_states[dom_id]);

                        if let AltRule { .. } = &stepx.alt_rule {
                            print!(" Alt_rule: -1");
                        }

                        for sel_regx in self.select.iter() {
                            if sel_regx.net_value < 0
                                && sel_regx.regions.is_superset_states(&cur_states)
                            {
                                print!(" in {:+}, {}", sel_regx.regions, sel_regx.net_value);
                            }
                        }

                        println!("\n     {}", df.mark_ones());
                    } // next stepsx
                    println!("    {}", cur_states[dom_id]);
                } else {
                    println!("\n  Domain: {}, Plan: At Target", dom_id);
                }
            } // next planx
        } // next planscx
    }

    /// Return the maximum possible regions.
    pub fn maximum_regions(&self) -> RegionsCorr {
        let mut ret_regs = RegionsCorr::with_capacity(self.len());
        for domx in self.items.iter() {
            ret_regs.push(domx.maximum_region());
        }
        ret_regs
    }

    /// Get plans for moving from one set of domain states to another, within a given set of domain regions.
    pub fn make_plans(
        &self,
        from: &RegionsCorr,
        goal: &RegionsCorr,
        within: &RegionsCorr,
    ) -> Result<PlansCorr, Vec<String>> {
        debug_assert!(from.is_congruent(goal));
        debug_assert!(from.is_congruent(within));
        debug_assert!(within.is_superset_of(from));
        debug_assert!(within.is_superset_of(goal));

        //println!("make_plans: from {from} to {goal} within {within}");

        if from.is_subset_of(goal) {
            return Err(vec![format!(
                "domainstore::make_plans: from {from} is a subset of the goal {goal}"
            )]);
        }
        let plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .map(|_| self.make_plans2(from, goal, within))
            .collect::<Vec<Result<PlansCorr, Vec<String>>>>();

        let mut plans2 = Vec::<PlansCorr>::new();
        let mut errs = Vec::<String>::new();
        for rslts in plans {
            match rslts {
                Ok(plns) => {
                    plans2.push(plns);
                }
                Err(errvec) => {
                    for errstr in errvec {
                        if !errs.contains(&errstr) {
                            errs.push(errstr);
                        }
                    }
                }
            };
        }

        if plans2.is_empty() {
            Err(errs)
        } else {
            Ok(plans2.swap_remove(rand::thread_rng().gen_range(0..plans2.len())))
        }
    }

    /// Return an Option PlanStore, to go from a set of domain/regions to another set of domain/regions.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    pub fn make_plans2(
        &self,
        from: &RegionsCorr,
        goal: &RegionsCorr,
        within: &RegionsCorr,
    ) -> Result<PlansCorr, Vec<String>> {
        //println!("domainstore: make_plans2: from {from} goal {goal}");
        debug_assert!(from.is_congruent(goal));
        debug_assert!(from.is_congruent(within));
        debug_assert!(within.is_superset_of(from));
        debug_assert!(within.is_superset_of(goal));

        let mut plans_per_target = PlansCorr::with_capacity(from.len());

        // Find a plan for each target.
        for (dom_id, (regx, regy)) in from.iter().zip(goal.iter()).enumerate() {
            if regy.is_superset_of(regx) {
                plans_per_target.push(SomePlan::new(vec![SomeStep::new_no_op(regx)]));
            } else {
                // Try making plans.
                match self.get_plans(dom_id, regx, regy, &within[dom_id]) {
                    Ok(mut plans) => plans_per_target
                        .push(plans.remove(rand::thread_rng().gen_range(0..plans.len()))),
                    Err(errvec) => return Err(errvec),
                };
            }
        } // next domain

        Ok(plans_per_target)
    }

    /// Try to satisfy a need.
    /// Return true if success.
    pub fn do_a_need(&mut self, inx_pln: InxPlan) -> bool {
        let dom_id = self.current_domain;
        let nd_inx = inx_pln.inx;

        // Display Domain info, if needed.
        match self.needs[nd_inx] {
            SomeNeed::ToSelectRegion { .. } => {
                //println!("\nNeed chosen: {} {}", ndx, plans.str_terse())
            }
            SomeNeed::ExitSelectRegion { .. } => {
                //println!("\nNeed chosen: {} {}", nd_inx, inx_pln.plans.str_terse())
            }
            _ => {
                let nd_dom = self.needs[nd_inx].dom_id().unwrap();
                if dom_id != nd_dom {
                    // Show "before" state before running need.
                    println!("\nAll domain states: {}", self.all_current_states());
                    self.change_domain(nd_dom);
                    self.print_domain();
                    //println!("\nNeed chosen: {} {}", ndx, plans.str_terse());
                }
            }
        }

        // Run the plan, allow for one failure.
        if let NeedPlan::PlanFound { plan: plans } = &inx_pln.plans {
            match self.run_planscorrstore(plans) {
                Ok(num) => {
                    if num == 1 {
                        println!("{num} step run.")
                    } else {
                        println!("{num} steps run.")
                    }
                }
                Err(errstr) => {
                    println!("Run plan failed, {errstr}.");
                    if let Ok(ndpln2) = self.plan_using_least_negative_select_regions_for_target(
                        self.needs[inx_pln.inx].dom_id(),
                        &self.needs[inx_pln.inx].target(),
                    ) {
                        match ndpln2 {
                            NeedPlan::PlanFound { plan: plans2 } => {
                                println!("Try again with {}", plans2);
                                match self.run_planscorrstore(&plans2) {
                                    Ok(num) => {
                                        if num == 1 {
                                            println!("{num} step run.")
                                        } else {
                                            println!("{num} steps run.")
                                        }
                                    }
                                    Err(errstr) => {
                                        println!("Second failure, giving up, {errstr}");
                                        return false;
                                    }
                                }
                            }
                            _ => return false,
                        }
                    } else {
                        println!("Unexpected result, new path to goal not found.");
                    }
                }
            }
        }

        // Take action after the desired state is reached.
        match &self.needs[nd_inx] {
            SomeNeed::ToSelectRegion { target_select, .. } => {
                if target_select
                    .regions
                    .is_superset_states(&self.all_current_states())
                {
                    if self.set_boredom_limit() {
                        self.update_times_visited();
                    }
                    return true;
                }
            }
            SomeNeed::ExitSelectRegion { target_regions, .. } => {
                if target_regions.is_superset_states(&self.all_current_states()) {
                    return true;
                }
            }
            _ => {
                if self.needs[nd_inx]
                    .satisfied_by(self.cur_state(self.needs[nd_inx].dom_id().unwrap()))
                {
                    self.take_action_need(nd_inx);
                    return true;
                }
            }
        }
        false
    }
    /// Run cleanup for a domain and action.
    pub fn cleanup(&mut self, dom_id: usize, act_id: usize) {
        assert!(dom_id < self.len());
        self.items[dom_id].cleanup(act_id, &self.needs);
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
    use crate::rulestore::RuleStore;
    use crate::sample::SomeSample;

    /// Return the number of supersets of a StateStore
    fn number_supersets_of_states(select: &SelectRegionsStore, stas: &StatesCorr) -> usize {
        select
            .items
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_states(stas)))
            .sum()
    }

    #[test]
    /// Test case where positive regions the start and goal are in, intersect.
    fn avoidance1x() -> Result<(), String> {
        // Init DomainStore. Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("0x0")?);
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);

        // Set select regions.
        dmxs.add_select(SelectRegions::from("SR[RC[r01X1], -1]")?);

        dmxs.add_select(SelectRegions::from("SR[RC[rX101], -2]")?);
        dmxs.calc_select();

        // Set state for domain 0.
        dmxs[0].set_cur_state(SomeState::from("0x1")?);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        dmxs[0].get_needs(); // set aggregate changes

        let start_region = RegionsCorr::from("RC[0x1]")?;
        let goal_region = RegionsCorr::from("RC[0xf]")?;

        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(ndpln) => {
                match ndpln {
                    NeedPlan::PlanFound { plan: planx } => {
                        println!(
                            "Plan found: {} start {start_region} goal {goal_region}",
                            planx
                        );
                        assert!(planx.value == 0);
                        //assert!(1 == 2);
                        Ok(())
                    }
                    NeedPlan::AtTarget {} => Err("AtTarget not expected".to_string()),
                }
            }
            Err(errvec) => Err(format!("{:?}", errvec)),
        }
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// but another region intersects both.
    fn avoidance2() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("0x0")?);
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![], 5);
        domx.eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);

        // Set select regions.
        dmxs.add_select(SelectRegions::from("SR[RC[r0101], -1]")?);

        dmxs.add_select(SelectRegions::from("SR[RC[r1001], -1]")?);

        dmxs.calc_select();

        // Set state for domain 0.
        dmxs[0].set_cur_state(SomeState::from("0x1")?);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::from("RC[0x1]")?;
        let goal_region = RegionsCorr::from("RC[0xd]")?;

        dmxs[0].get_needs(); // set aggregate changes

        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: planx }) => {
                println!("Plan found: {planx}");
                assert!(planx.value == 0);
                Ok(())
            }
            _ => Err("No plan found?".to_string()),
        }
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// and another region does not intersect both.
    fn avoidance3() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("0x0")?);
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![], 5);
        domx.eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![], 5);
        domx.eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);

        // Set select regions.
        dmxs.add_select(SelectRegions::from("SR[RC[r0x00], -1]")?);

        dmxs.add_select(SelectRegions::from("SR[RC[rx100], -1]")?);

        dmxs.add_select(SelectRegions::from("SR[RC[r01x1], -1]")?);

        dmxs.add_select(SelectRegions::from("SR[RC[r10x1], -1]")?);

        dmxs.add_select(SelectRegions::from("SR[RC[r101x], -1]")?);

        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::from("0x1")?;
        dmxs[0].set_cur_state(first_state.clone());

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::from("RC[0x1]")?;
        let goal_region = RegionsCorr::from("RC[0xd]")?;

        dmxs[0].get_needs(); // set aggregate changes

        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: planx }) => {
                println!("Plan found: {}", planx);
                assert!(planx.value == 0);
                Ok(())
            }
            _ => Err("No plan found?".to_string()),
        }
    }

    #[test]
    /// Test case where start and goal regions are not in a non-negative region.
    fn avoidance5() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("0x0")?);
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![], 5);
        domx.eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);

        // Set select regions.
        let regstr1 = RegionsCorr::from("RC[rxx0x]")?;
        dmxs.add_select(SelectRegions::new(regstr1, -1));
        dmxs.calc_select();

        // Set state for domain 0.
        dmxs[0].set_cur_state(SomeState::from("0x1")?);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::from("RC[0x1]")?;
        let goal_region = RegionsCorr::from("RC[0xd]")?;

        dmxs[0].get_needs(); // set aggregate changes

        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: planx }) => {
                println!("Plan found: {planx}");
                planx.str_terse();
                Ok(())
            }
            _ => Err(format!("No plan ?")),
        }
    }

    #[test]
    fn all_current_states() -> Result<(), String> {
        // Init DomainStore, Domains.
        // Domain 0 uses 1 integer for bits.
        // Domain 1 uses 2 integers for bits.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("0x00")?);
        dmxs.add_domain(SomeState::from("0x0000")?);

        // Set state for domain 0, using 1 integer for bits.
        let init_first_state = SomeState::from("0x12")?;
        dmxs[0].set_cur_state(init_first_state.clone());

        // Set state for domain 1, using 2 integers for bits.
        let init_state2 = SomeState::from("0xabcd")?;
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
        dmxs.add_domain(SomeState::from("0x0")?);

        // Add actions.
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);

        // Init aggregate needs.
        dmxs.get_needs();

        // Set select regions.

        // Set up dom 0 00XX dependent on dom 1 01XX.
        let regstr0 = RegionsCorr::from("RC[r1100]")?;
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Set up dom 0 00XX dependent on dom 1 10XX.
        let regstr1 = RegionsCorr::from("RC[r1011]")?;
        dmxs.add_select(SelectRegions::new(regstr1, -1));
        dmxs.calc_select();

        let s0 = SomeState::from("0b0000")?;
        dmxs[0].set_cur_state(s0.clone());

        let start_region = RegionsCorr::from("RC[0x0]")?;
        let goal_region = RegionsCorr::from("RC[0xd]")?;

        // Try making plans.
        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.value == 0);
                Ok(())
            }
            _ => Err(format!("No plan found?")),
        }
    }

    #[test]
    /// Test case using two domains.
    fn avoidance7() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("s0x0")?);
        dmxs.add_domain(SomeState::from("s0x0")?);

        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);

        // Set up dom 0 negative regions.
        let regstr0 = RegionsCorr::from("RC[r01x1, rxxxx]")?;
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let regstr0 = RegionsCorr::from("RC[rx101, rxxxx]")?;
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Set up dom 1 negative regions.
        let regstr0 = RegionsCorr::from("RC[rxxxx, r011x]")?;
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let regstr0 = RegionsCorr::from("RC[rxxxx, rx111]")?;
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s0 = SomeState::from("0x0")?;
        dmxs[0].set_cur_state(s0.clone());

        let s1 = SomeState::from("0x1")?;
        dmxs[1].set_cur_state(s1.clone());

        let start_region = RegionsCorr::from("RC[r0000, r0001]")?;
        let goal_region = RegionsCorr::from("RC[r1111, r1110]")?;

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.value == 0);
                Ok(())
            }
            _ => Err(format!("No plan found?")),
        }
    }

    #[test]
    /// Test case using two domains, where a non-target domain must change first,
    /// due to the boolean AND relationship, with two non-maximum regions, in a negative SelectRegions instance.
    fn avoidance8() -> Result<(), String> {
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("0x0")?);
        dmxs.add_domain(SomeState::from("0x0")?);

        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);

        // Set up negative regions.
        let regstr0 = RegionsCorr::from("RC[r00xx, rxx11]")?;

        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let regstr0 = RegionsCorr::from("RC[r11xx, r01xx]")?;

        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = SomeState::from("0b0101")?;
        dmxs[0].set_cur_state(s5.clone());

        let s7 = SomeState::from("0b0111")?;
        dmxs[1].set_cur_state(s7.clone());

        let start_regions = RegionsCorr::from("RC[r0101, r0111]")?;

        let goal_regions = RegionsCorr::from("RC[r1001, r0111]")?;

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        match dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.value == 0);
                let mut cur_states = dmxs.all_current_states();
                for planx in plans.iter() {
                    if let Some(next_states) = planx.result_from_initial_states(&cur_states) {
                        cur_states = next_states;
                    } else {
                        return Err(format!("Next states not found?"));
                    }
                }
                assert!(goal_regions.is_superset_states(&cur_states));
                Ok(())
            }
            _ => Err(format!("No plan found?")),
        }
    }

    #[test]
    /// Test case using two domains, like avoidance8, but a way around traps.
    fn avoidance9() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("s0x0")?);
        dmxs.add_domain(SomeState::from("s0x0")?);

        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);

        // Set up negative regions.
        let regstr0 = RegionsCorr::from("RC[r000x, rxx11]")?;
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let regstr0 = RegionsCorr::from("RC[r11x1, r01xx]")?;
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = SomeState::from("s0b0101")?;
        dmxs[0].set_cur_state(s5.clone());

        let s7 = SomeState::from("s0b0111")?;
        dmxs[1].set_cur_state(s7.clone());

        let start_regions = RegionsCorr::from("RC[r0101, r0111]")?;

        let goal_regions = RegionsCorr::from("RC[r1001, r0111]")?;

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        match dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(ndpln) => match ndpln {
                NeedPlan::PlanFound { plan: plans } => {
                    println!("Plans {}", plans);
                    return Ok(());
                }
                _ => return Err("No plan found".to_string()),
            },
            Err(errvec) => return Err(format!("{:?}", errvec)),
        }
    }

    #[test]
    fn check_select() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("s0x00")?);
        dmxs.add_domain(SomeState::from("s0x0000")?);

        // Add action to domain 0.
        dmxs[0].add_action(vec![], 5);

        // Add action to domain 1.
        dmxs[1].add_action(vec![], 5);

        // Load select regions
        let regstr1 = RegionsCorr::from("RC[r00000x0x, rXXXXXX10_1XXX_XXXX]")?;

        let regstr2 = RegionsCorr::from("RC[r00000xx1, rXXXXXX10_1XXX_XXXX]")?;

        let regstr3 = RegionsCorr::from("RC[r0000x1x1, rXXXXXX10_1XXX_XXXX]")?;

        let regstr4 = RegionsCorr::from("RC[r00001110, rXXXXXX10_1XXX_XXXX]")?;

        // Add select region stores.
        dmxs.add_select(SelectRegions::new(regstr1, 1));
        dmxs.add_select(SelectRegions::new(regstr2, 1));
        dmxs.add_select(SelectRegions::new(regstr3, 1));
        dmxs.add_select(SelectRegions::new(regstr4, 1));
        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::from("s0x12")?;
        dmxs[0].set_cur_state(first_state.clone());

        // Set state for domain 1.
        let state2 = SomeState::from("s0xabcd")?;
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
        let first_state = SomeState::from("s0x05")?;
        dmxs[0].set_cur_state(first_state.clone());

        // Set state for domain 1.
        let state2 = SomeState::from("s0xa28d")?;
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
        dmxs.add_domain(SomeState::from("s0x0")?);

        let regstr1 = RegionsCorr::from("RC[rX1XX]")?;

        // Add selectregions.
        dmxs.add_select(SelectRegions::new(regstr1.clone(), -1));

        let regstr2 = RegionsCorr::from("RC[r1XX1]")?;

        // Add select regionstores.
        dmxs.add_select(SelectRegions::new(regstr2.clone(), -1));

        // Set state for domain 0, using 1 integer for bits.
        let first_state = SomeState::from("s0xd")?;
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
                    _ => panic!("SNH"),
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
        dmxs.add_domain(SomeState::from("s0x0")?);

        // Add action to domain 0.
        dmxs[0].add_action(vec![], 5);

        // Load select regions
        let regstr1 = RegionsCorr::from("RC[rxx0x]")?;

        let regstr2 = RegionsCorr::from("RC[r00x1]")?;

        let regstr3 = RegionsCorr::from("RC[r11x1]")?;

        let regstr4 = RegionsCorr::from("RC[r10x0]")?;

        // Add select region stores.
        dmxs.add_select(SelectRegions::new(regstr1, 4));
        dmxs.add_select(SelectRegions::new(regstr2, -2));
        dmxs.add_select(SelectRegions::new(regstr3, -4));
        dmxs.add_select(SelectRegions::new(regstr4, -5));
        dmxs.calc_select();

        assert!(dmxs.select_positive.len() == 4);
        assert!(dmxs
            .select_positive
            .contains(&SelectRegions::from("SR[RC[rx100], 4]")?));
        assert!(dmxs
            .select_positive
            .contains(&SelectRegions::from("SR[RC[r0x00], 4]")?));
        assert!(dmxs
            .select_positive
            .contains(&SelectRegions::from("SR[RC[r010x], 4]")?));
        assert!(dmxs
            .select_positive
            .contains(&SelectRegions::from("SR[RC[r1001], 4]")?));

        assert!(dmxs.rc_non_negative.len() == 4);
        assert!(dmxs
            .rc_non_negative
            .contains(&RegionsCorr::from("RC[rx1x0]")?));
        assert!(dmxs
            .rc_non_negative
            .contains(&RegionsCorr::from("RC[r0xx0]")?));
        assert!(dmxs
            .rc_non_negative
            .contains(&RegionsCorr::from("RC[r01xx]")?));
        assert!(dmxs
            .rc_non_negative
            .contains(&RegionsCorr::from("RC[r10x1]")?));

        Ok(())
    }

    #[test]
    /// Test case using two domains, like avoidance8, but a way around traps.
    fn avoidance10() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from("s0x0")?);

        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b0000->0b0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from("0b1111->0b1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b0000->0b0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from("0b1111->0b1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b0000->0b0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from("0b1111->0b1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b0000->0b1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from("0b1111->0b0111")?);

        // Set up negative regions.
        let regstr0 = RegionsCorr::from("RC[r01xx]")?;
        dmxs.add_select(SelectRegions::new(regstr0, -1));

        let regstr0 = RegionsCorr::from("RC[r10xx]")?;
        dmxs.add_select(SelectRegions::new(regstr0, -2));

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s3 = SomeState::from("0b0011")?;
        dmxs[0].set_cur_state(s3.clone());

        let start_regions = RegionsCorr::from("RC[0x3]")?;

        let goal_regions = RegionsCorr::from("RC[0xf]")?;

        println!("\nActions {}\n", dmxs[0].actions);

        // Try making plans.
        match dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.value == -1);
                Ok(())
            }
            _ => Err(format!("No plan found?")),
        }
    }

    #[test]
    /// Test running plans in parallel, as in going from SelectRegions fragment to
    /// intersection of another SelectRegions fragment.
    fn run_planscorr() -> Result<(), String> {
        // Init DomainStore.
        let mut dmxs = DomainStore::new();

        // Add domains.
        dmxs.add_domain(SomeState::from("s0x0")?);
        dmxs.add_domain(SomeState::from("s0b000")?);
        dmxs.add_domain(SomeState::from("s0b00")?);

        // Add an action for each domain.
        dmxs[0].add_action(vec![RuleStore::from("[XX/XX/XX/Xx]")?], 5);
        dmxs[1].add_action(vec![RuleStore::from("[XX/XX/Xx]")?], 5);
        dmxs[2].add_action(vec![RuleStore::from("[XX/Xx]")?], 5);

        // Set domain starting states.
        dmxs[0].set_cur_state(SomeState::from("s0b0000")?);
        dmxs[1].set_cur_state(SomeState::from("s0b001")?);
        dmxs[2].set_cur_state(SomeState::from("s0b10")?);

        // Set up PlansCorr.
        let plnsc1 = PlansCorr::from("PC[P[r0000-0->r0001], P[r001-no->r001], P[r10-0->11]]")?;
        println!("{plnsc1}");

        let before = dmxs.all_current_states();
        println!("Before: {before}");

        match dmxs.run_planscorr(&plnsc1) {
            Err(errstr) => return Err(errstr),
            _ => (),
        }

        let after = dmxs.all_current_states();
        println!("After:  {after}");

        assert!(after[0] == SomeState::from("s0b0001")?);
        assert!(after[1] == SomeState::from("s0b001")?);
        assert!(after[2] == SomeState::from("s0b11")?);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    /// Test running a PlansCorrStore.
    fn run_planscorrstore() -> Result<(), String> {
        // Init DomainStore.
        let mut dmxs = DomainStore::new();

        // Add domain 0.
        dmxs.add_domain(SomeState::from("0x0")?);

        // Add domain 0 actions.
        dmxs[0].add_action(vec![RuleStore::from("[XX/XX/XX/Xx]")?], 5);
        dmxs[0].add_action(vec![RuleStore::from("[XX/XX/Xx/XX]")?], 5);
        dmxs[0].add_action(vec![RuleStore::from("[XX/Xx/XX/XX]")?], 5);
        dmxs[0].add_action(vec![RuleStore::from("[Xx/XX/XX/XX]")?], 5);

        // Set domain 0 starting state.
        dmxs[0].set_cur_state(SomeState::from("s0b0000")?);

        // Add domain 1.
        dmxs.add_domain(SomeState::from("s0x0")?);

        // Add domain 1 actions.
        dmxs[1].add_action(vec![RuleStore::from("[XX/XX/XX/Xx]")?], 5);
        dmxs[1].add_action(vec![RuleStore::from("[XX/XX/Xx/XX]")?], 5);
        dmxs[1].add_action(vec![RuleStore::from("[XX/Xx/XX/XX]")?], 5);
        dmxs[1].add_action(vec![RuleStore::from("[Xx/XX/XX/XX]")?], 5);

        // Set domain 1 starting state.
        dmxs[1].set_cur_state(SomeState::from("s0b1111")?);

        // Set up PlansCorrStore.
        let plnscrstr = PlansCorrStore::from("PCS[PC[P[r0000-2->r0100], P[r1111-0->r1110]], PC[P[r0100-0->0101], P[r1110-1->r1100]]]")?;
        println!("plnscrstr {plnscrstr}");

        let before = dmxs.all_current_states();
        println!("Before: {before}");

        // Run it.
        match dmxs.run_planscorrstore(&plnscrstr) {
            Err(errstr) => return Err(errstr),
            _ => (),
        }

        // Check results.
        let after = dmxs.all_current_states();
        println!("After:  {after}");

        assert!(after[0] == SomeState::from("0b0101")?);
        assert!(after[1] == SomeState::from("0b1100")?);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    /// Test run_planscorrstore, where start and goal are not subsets of any select regions.
    fn get_path_through_select_regions() -> Result<(), String> {
        // Init DomainStore.
        let dmxs = DomainStore::new();

        let start_regs = RegionsCorr::from("RC[r1X11]")?;
        let goal_regs = RegionsCorr::from("RC[rX000]")?;

        let within = RegionsCorrStore::from("RCS[RC[r0XXX], RC[rX1XX]]")?;

        let path = dmxs.get_path_through_select_regions(&start_regs, &goal_regs, &within);
        if let Some(pathx) = path {
            print!("path: ");
            for rcx in pathx.iter() {
                print!("{rcx}");
            }
            println!(" ");
            assert!(pathx[0] == RegionsCorr::from("RC[r1111]")?);
            assert!(pathx[pathx.len() - 1] == RegionsCorr::from("RC[r0000]")?);
        } else {
            return Err("No path found?".to_string());
        }
        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn cleanup() -> Result<(), String> {
        // Create DomainStore.
        let mut dmxs = DomainStore::new();

        // Create a domain that uses 4 bits.
        dmxs.add_domain(SomeState::new(SomeBits::new_random(4)));

        // Set up action 0, changing bit 0.
        let ruls0: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/XX/Xx]").expect("SNH")];
        dmxs[0].add_action(ruls0, 5);

        // Set up action 1, changing bit 1.
        let ruls1: Vec<RuleStore> = vec![RuleStore::from("[XX/XX/Xx/XX]").expect("SNH")];
        dmxs[0].add_action(ruls1, 5);

        // Set up action 2, changing bit 2.
        let ruls2: Vec<RuleStore> = vec![RuleStore::from("[XX/Xx/XX/XX]").expect("SNH")];
        dmxs[0].add_action(ruls2, 5);

        // Set up action 3, changing bit 3.
        let ruls3: Vec<RuleStore> = vec![RuleStore::from("[Xx/XX/XX/XX]").expect("SNH")];
        dmxs[0].add_action(ruls3, 5);

        // Set up action 3, changing bit 3.
        let ruls4: Vec<RuleStore> = vec![
            RuleStore::from("[XX/11/01/XX]").expect("SNH"),
            RuleStore::from("[11/XX/10/XX]").expect("SNH"),
            RuleStore::from("[Xx/00/00/XX]").expect("SNH"),
            RuleStore::from("[01/XX/11/XX]").expect("SNH"),
        ];
        dmxs[0].add_action(ruls4, 500); // Effectively, turn off clean_up.

        loop {
            // Generate needs, get can_do and cant_do need vectors.
            dmxs.generate_and_display_needs();

            // Check for end.
            if dmxs.can_do.is_empty() {
                break;
            }

            let np_inx = dmxs.choose_a_need();

            if dmxs.do_a_need(dmxs.can_do[np_inx].clone()) {
                println!("Need satisfied");
            }
        } // end loop

        dmxs.print_domain();
        assert!(dmxs[0].actions[0].groups.len() == 1);
        assert!(dmxs[0].actions[1].groups.len() == 1);
        assert!(dmxs[0].actions[2].groups.len() == 1);
        assert!(dmxs[0].actions[3].groups.len() == 1);
        assert!(dmxs[0].actions[4].groups.len() == 6);

        // Check action 4 primary groups.
        if let Some(grpx) = dmxs[0].actions[4].groups.find(&SomeRegion::from("rX10X")?) {
            assert!(grpx.limited);
        } else {
            return Err("Group rX10X not found?".to_string());
        }
        if let Some(grpx) = dmxs[0].actions[4].groups.find(&SomeRegion::from("r1X1X")?) {
            assert!(grpx.limited);
        } else {
            return Err("Group r1X1X not found?".to_string());
        }
        if let Some(grpx) = dmxs[0].actions[4].groups.find(&SomeRegion::from("rX00X")?) {
            assert!(grpx.limited);
        } else {
            return Err("Group rX00X not found?".to_string());
        }
        if let Some(grpx) = dmxs[0].actions[4].groups.find(&SomeRegion::from("r0X1X")?) {
            assert!(grpx.limited);
        } else {
            return Err("Group r0X1X not found?".to_string());
        }

        // Check unneeded groups.
        let subs = dmxs[0].actions[4]
            .groups
            .subsets_of(&SomeRegion::from("r00XX")?);
        println!("subsets of r00XX {subs}");
        assert!(subs.len() == 1);
        let grpx = dmxs[0].actions[4].groups.find(&subs[0]).expect("SNH");
        assert!(!grpx.limited);

        let subs = dmxs[0].actions[4]
            .groups
            .subsets_of(&SomeRegion::from("r11XX")?);
        println!("subsets of r11XX {subs}");
        assert!(subs.len() == 1);
        let grpx = dmxs[0].actions[4].groups.find(&subs[0]).expect("SNH");
        assert!(!grpx.limited);

        // Do cleanup to delete unneeded groups.
        dmxs.cleanup(0, 4);

        dmxs.print_domain();
        assert!(dmxs[0].actions[4].groups.len() == 4);
        let subs = dmxs[0].actions[4]
            .groups
            .subsets_of(&SomeRegion::from("r11XX")?);
        assert!(subs.is_empty());

        let subs = dmxs[0].actions[4]
            .groups
            .subsets_of(&SomeRegion::from("r00XX")?);
        assert!(subs.is_empty());

        //assert!(1 == 2);
        Ok(())
    }
}
