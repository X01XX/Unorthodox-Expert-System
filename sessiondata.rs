//! The DomainStore struct, a vector of SomeDomain structs,
//! and values/methods that manage the domains.

use crate::change::SomeChange;
use crate::domain::SomeDomain;
use crate::domainstore::DomainStore;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planscorr::PlansCorr;
use crate::planscorrstore::PlansCorrStore;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionscorr::RegionsCorr;
use crate::regionscorrstore::RegionsCorrStore;
use crate::sample::SomeSample;
use crate::selectregions::SelectRegions;
use crate::selectregionsstore::SelectRegionsStore;
use crate::state::SomeState;
use crate::statescorr::StatesCorr;
use crate::step::{AltRuleHint, SomeStep};
use crate::stepstore::StepStore;
use crate::target::ATarget;
use crate::tools::{self, CorrespondingItems};

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

use rayon::prelude::*;

impl fmt::Display for SessionData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
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
#[derive(Serialize, Deserialize)]
/// A vector of SomeDomain structs, and session state.
pub struct SessionData {
    /// Domain structures.
    pub domains: DomainStore,

    /// UI data.

    /// Domain displayed to user.
    pub current_domain: usize,
    /// The current step number of a user session.
    pub step_num: usize,

    /// Select Region data.

    /// Zero, or more, select regions that may have a positive, or negative, value.
    /// They may overlap.
    pub select: SelectRegionsStore,
    /// Positive region fragments, possibly overlapped by negative value regions.
    /// These tend to be goals.
    pub select_net_positive: SelectRegionsStore,
    /// Negative select fragments.
    pub select_negative: SelectRegionsStore,
    /// Non-negative, may be, or overlap, positive, regions.
    pub rc_non_negative: RegionsCorrStore,
    /// Save the maximum fragment value.
    /// Used to calculate the ToSelectRegion need priority.
    pub max_pos_value: isize,

    /// Cyclical goal, positve Select Region, data.

    /// A counter to indicate the number of steps the current state is in the same select region.
    /// When no more rule-testing needs can be done.
    pub boredom: usize,
    /// A limit for becomming bored, then moving to another select state.
    /// When no more rule-testing needs can be done.
    pub boredom_limit: usize,
    /// Times visited, order corresponding to, positive select regions.
    pub times_visited: Vec<usize>,

    /// Need data.

    /// List of current needs.
    pub needs: NeedStore,
    /// List of need indicies and plans.
    pub can_do: Vec<InxPlan>,
    /// List of indices of needs that cannot be done.
    pub cant_do: Vec<usize>,
}

/// Implement the PartialEq trait, for a
/// A quick comparison of definitions.
impl PartialEq for SessionData {
    fn eq(&self, other: &Self) -> bool {
        if self.domains != other.domains {
            return false;
        }
        if self.select != other.select {
            return false;
        }
        self.max_pos_value == other.max_pos_value
    }
}
impl Eq for SessionData {}

impl SessionData {
    /// Return a new, empty, SessionData struct.
    pub fn new(domains: DomainStore) -> Self {
        Self {
            domains,
            current_domain: 0,
            step_num: 0,
            select: SelectRegionsStore::new(vec![]),
            select_net_positive: SelectRegionsStore::new(vec![]),
            rc_non_negative: RegionsCorrStore::new(vec![]),
            select_negative: SelectRegionsStore::new(vec![]),
            max_pos_value: 0,
            boredom: 0,
            boredom_limit: 0,
            times_visited: vec![],
            needs: NeedStore::new(vec![]),
            can_do: vec![],
            cant_do: vec![],
        }
    }

    /// Add SelectRegions, with non-zero values.
    ///
    /// The SelectRegionsStore SelectRegions RegionsCorr can be interpreted as:
    ///   [1x0x, x101], // Domain 0, 1x0x, AND domain 1, x101, OR
    ///   [1x0x, xxxx], // Domain 0, 1x0x, OR
    ///   [xxxx, x101]  // Domain 1, x101.
    /// For SelectRegions, any value, with duplicate RegionsCorrs, the first is accepted, then following skipped.
    pub fn add_select(&mut self, selx: SelectRegions) {
        if selx.value != 0 {
            assert!(self.is_congruent(&selx));
            self.select.push(selx);
        }
    }

    /// Calculate parts of select regions, in case of any overlaps.
    pub fn calc_select(&mut self) {
        // Get fragments due to different-value intersections.
        let fragments = self.select.split_by_intersections();
        // println!("fragments");

        // Separate fragments by value.
        for selx in fragments {
            if selx.value < 0 {
                self.select_negative.push(selx.clone());
            } else {
                if selx.value > self.max_pos_value {
                    self.max_pos_value = selx.value;
                }
                self.select_net_positive.push(selx);
            }
        }

        // Calc non-negative regions.
        let mut non_neg_select = RegionsCorrStore::new(vec![self.maximum_regions()]);

        for selx in self.select.iter() {
            if selx.value < 0 {
                non_neg_select = non_neg_select.subtract_regionscorr(&selx.regions);
            }
        }
        self.rc_non_negative = non_neg_select;

        // Init times visitied vector.
        self.times_visited = vec![0; self.select_net_positive.len()];
    }

    /// Print info about select stores.
    pub fn print_select_stores_info(&self) {
        if self.select.is_empty() {
            println!("\nGiven Select Regions: None\n");
        } else {
            println!("\nGiven Select Regions ({}):\n", self.select.len());
            for selx in self.select.iter() {
                println!("  {selx}");
            }
        }

        println!(" ");
        if self.select_net_positive.is_empty() {
            println!("Net-Positive SR fragments: None");
        } else {
            println!(
                "Net-Positive SR fragments, each a subset of one or more SRs, no partial intersections. ({}):\n",
                self.select_net_positive.len()
            );
            for selx in self.select_net_positive.iter() {
                println!("  {selx}");
            }
            println!(" ");
            println!("  To seek, when no other needs can be done.");
        }

        println!(" ");
        if self.rc_non_negative.is_empty() {
            println!("Non-Negative RCs: None");
        } else {
            println!(
                "Non-Negative RCs, maximum RC regions minus negative SR regions ({}):\n",
                self.rc_non_negative.len()
            );
            for rcx in self.rc_non_negative.iter() {
                println!("  {rcx}");
            }
            println!(" ");
            println!("  Seek one of these when exiting a negative SR the current state is in.");
            println!(" ");
            println!("  Only one bit needs to change to exit a region, with at least one edge,");
            println!("  but there may be overlapping and adjacent negative SRs.");
        }

        println!(" ");
        if self.select_negative.is_empty() {
            println!("Net-Negative SR fragments: None");
        } else {
            println!("Net-Negative SR fragments, each a subset of one or more negative SRs, no partial intersections. ({}):\n", self.select_negative.len());
            for selx in self.select_negative.iter() {
                println!("  {selx}");
            }
            println!(" ");
            println!("  If more than one different value, more and more negative value SRs may be considered to find a plan-path.");
        }
        println!(" ");
    }

    /// Get needs for each Domain.
    /// Run in parallel per Domain.
    /// Each Domain uses parallel processing to get needs for each Action.
    ///  plans.
    /// Set SessionData fields with need info.
    pub fn get_needs(&mut self) {
        self.step_num += 1;
        let mut needs = self.domains.get_needs();

        // Get select region needs.
        if let Some(sel_needs) = self.check_select() {
            needs.append(sel_needs);
        }

        // Sort needs by ascending priority, and store.
        needs.sort_by_priority();

        self.needs = needs;

        self.evaluate_needs();
    }

    /// Run a PlansCorrStore.
    /// A series of steps from one SelectRegions fragment to another, to another.
    pub fn run_planscorrstore(&mut self, plnsstr: &PlansCorrStore) -> Result<usize, String> {
        //println!("sessiondata::run_planscorrstore: {plnsstr}");
        let mut not_first = false;
        let mut steps_run = 0;

        for plnscrx in plnsstr.iter() {
            if plnscrx
                .initial_regions()
                .is_superset_of(&self.all_current_regions())
            {
                if not_first {
                    print!("---- ");
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
        self.domains.run_planscorr(plns)
    }

    /// Take an action to satisfy a need,
    pub fn take_action_need(&mut self, inx: usize) {
        self.domains
            .take_action_need(self.needs[inx].dom_id().unwrap(), &self.needs[inx]);
    }

    /// Return a reference to the current state of a given Domain index
    pub fn cur_state(&self, dom_id: usize) -> &SomeState {
        self.domains.cur_state(dom_id)
    }

    /// Set can_do, and cant_do, struct fields for the SessionData needs, which are sorted in ascending priority number order.
    /// Scan successive slices of needs, of the same priority, until one, or more, needs can be planned.
    pub fn evaluate_needs(&mut self) {
        //println!("evaluate_needs: {} needs", needs.len());
        self.can_do = vec![];
        self.cant_do = vec![];

        if self.needs.is_empty() {
            return;
        }

        let mut can_do = Vec::<InxPlan>::new();

        let cur_states = self.all_current_states();

        // Check for needs that can be satisfied with the current state.
        for (inx, ndx) in self.needs.iter().enumerate() {
            if ndx.satisfied_by(&cur_states) {
                //println!("need {ndx} At Target");
                can_do.push(InxPlan {
                    inx,
                    plans: NeedPlan::AtTarget {},
                    rate: 0,
                    desired_num_bits_changed: 0,
                    process_num_bits_changed: 0,
                });
            }
        }

        if !can_do.is_empty() {
            self.can_do = can_do;
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

        // Init current priority value and start index.
        let mut cur_pri = self.needs[0].priority();

        let mut cur_pri_start = 0;

        // Find current priority end index.
        let mut cur_pri_end = 0;
        let needs_len = self.needs.len();

        let states = self.all_current_states();

        // Scan successive slices of items with the same priority.
        loop {
            // Find end of current slice.
            while cur_pri_end < needs_len && self.needs[cur_pri_end].priority() == cur_pri {
                cur_pri_end += 1;
            }
            // Process a priority slice.
            print!("Priority {cur_pri}");

            // Test distance check.
            // Find needs distance to the current state.
            let mut need_inx_vec = Vec::<(usize, usize)>::with_capacity(self.needs.len()); // (need index, distance)

            for inx in cur_pri_start..cur_pri_end {
                let dist = self.needs[inx].distance(&states);
                need_inx_vec.push((inx, dist));
            }

            // Sort needs, of same priority, by distance.
            need_inx_vec.sort_by(|(_, dist_a), (_, dist_b)| dist_a.partial_cmp(dist_b).unwrap());

            // Calc slice of needs, of same priority, by distance.
            let mut cur_dist_start = 0;
            let mut cur_dist_end = 0;

            while cur_dist_end < need_inx_vec.len() {
                // calc next cur_dist_end.
                if cur_dist_end < need_inx_vec.len() {
                    for inx in cur_dist_start..need_inx_vec.len() {
                        if need_inx_vec[inx].1 == need_inx_vec[cur_dist_start].1 {
                            cur_dist_end += 1;
                        } else {
                            break;
                        }
                    }
                }

                // Test all slice needs for plans.
                let inx_ndpln = (cur_dist_start..cur_dist_end)
                    .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
                    .map(|dist_inx| {
                        (
                            need_inx_vec[dist_inx].0,
                            self.plan_using_least_negative_select_regions_for_target(
                                self.needs[need_inx_vec[dist_inx].0].dom_id(),
                                self.needs[need_inx_vec[dist_inx].0].target(),
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
                                can_do.push(InxPlan {
                                    inx,
                                    plans: ndplnx,
                                    rate: 0,
                                    desired_num_bits_changed: 0,
                                    process_num_bits_changed: 0,
                                });
                            }
                            NeedPlan::PlanFound { plan: ref plany } => {
                                let ratex = plany.rate();
                                let cur_regs = self.all_current_regions();
                                let desired_num_bits_changed =
                                    cur_regs.distance(&plany.result_regions());
                                let process_num_bits_changed = plany.num_bits_changed();
                                can_do.push(InxPlan {
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

                if can_do.is_empty() {
                    cur_dist_start = cur_dist_end;
                } else {
                    self.can_do = can_do;
                    return;
                }
            }

            if can_do.is_empty() {
                println!(", none.");
                if cur_pri_end == needs_len {
                    self.cant_do = (0..self.needs.len()).collect();
                    return;
                }

                cur_pri_start = cur_pri_end;
                cur_pri = self.needs[cur_pri_start].priority();
                continue;
            }

            if can_do.len() == 1 {
                println!(", found 1 need that can be done.");
            } else {
                println!(", found {} needs that can be done.", can_do.len());
            }

            self.can_do = can_do;
            return;
        } // End loop
    } // end evaluate_needs

    /// Create a RegionsStoreCorr of maximum regions, except a for given domain.
    pub fn maximum_regions_except(&self, dom_id: usize, targ: &SomeRegion) -> RegionsCorr {
        let mut regs = self.maximum_regions();

        regs[dom_id] = targ.clone();

        regs
    }

    /// Get plans to move to a goal region, choose a plan.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    /// The within argument restricts where a rule should start, and restricts unwanted changes that may be included with wanted changes.
    pub fn make_plans_domain(
        &self,
        dom_id: usize,
        from_region: &SomeRegion,
        goal_region: &SomeRegion,
        within: &SomeRegion,
    ) -> Result<PlanStore, Vec<String>> {
        //println!("sessiondata: make_plans_domain: dom {dom_id} from {from_region} goal {goal_region}");

        self.domains
            .make_plans_domain(dom_id, from_region, goal_region, within)
    }

    /// Run a plan, in its given domain.
    pub fn run_plan_domain(&mut self, dom_id: usize, planx: &SomePlan) -> Result<usize, String> {
        self.domains[dom_id].run_plan(planx)
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
            rates.push(planx.rate() - planx.num_altrules());
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
                NeedPlan::PlanFound { plan: plnx } => rts.push(plnx.rate()),
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

    /// Return the length, the number of domains.
    pub fn len(&self) -> usize {
        self.domains.len()
    }

    /// Return a vector of domain current states, in domain number order.
    pub fn all_current_states(&self) -> StatesCorr {
        self.domains.all_current_states()
    }

    /// Return a vector of domain current states, as regions, in domain number order.
    pub fn all_current_regions(&self) -> RegionsCorr {
        self.domains.all_current_regions()
    }

    /// Update counters for times_visited.
    pub fn update_times_visited(&mut self) {
        // Get all domain current states.
        let all_states = self.all_current_states();

        // Get the select regions the current state is in.
        for (inx, optregs) in self.select_net_positive.iter_mut().enumerate() {
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
        //println!("sessiondata::check_select:");
        // Check if there are no select regions.
        if self.select.is_empty() {
            return None;
        }

        // Get all domain current states vector.
        let all_regs = self.all_current_regions();

        // Check if current states are in a negative region.
        if self.select.in_negative_regions(&all_regs) {
            self.boredom = 0;
            self.boredom_limit = 0;

            let mut ndstr = NeedStore::new(vec![]);

            if let Some(near_nn_regs) = self.closest_rc_regions(&all_regs) {
                // Process closest non-negative regions.
                let mut needx = SomeNeed::ExitSelectRegions {
                    target: ATarget::DomainRegions {
                        regions: near_nn_regs.clone(),
                    },
                    priority: 0,
                };
                needx.add_priority_base();
                ndstr.push(needx);
                return Some(ndstr);
            }
        }

        // Check current status within a select region, or not.
        let rate = self.select.supersets_sum(&all_regs);
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

    /// Return a need for moving to a select region.
    fn select_goal_needs(&self, goal_regs: &RegionsCorr) -> Option<NeedStore> {
        //println!("sessiondata::select_goal_needs:");
        debug_assert!(self.is_congruent(goal_regs));

        // Load return vector.
        let mut ret_str = NeedStore::new(vec![]);

        // Get current states.
        let all_states = self.all_current_states();

        for (inx, psupx) in self.select_net_positive.iter().enumerate() {
            if psupx.is_superset_of_states(&all_states) {
                continue;
            }
            let adjust = (self.max_pos_value - psupx.value) + self.times_visited[inx] as isize;
            let mut needx = SomeNeed::ToSelectRegions {
                target: ATarget::SelectRegions {
                    select: psupx.clone(),
                },
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
    pub fn print(&self) {
        // Calc current status.
        let mut in_str = String::new();
        let all_states = self.all_current_states();
        let mut in_pos = false;
        let mut in_neg = false;

        let supersets = self.select.supersets_of_states(&all_states);
        for selx in supersets.iter() {
            if selx.value != 0 {
                in_str += &format!("in {}, {} ", selx.regions, selx.value);
                if selx.value > 0 {
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
            "\nStep: {} All domain states: {} Status: {status}{in_str}",
            self.step_num,
            self.all_current_states()
        );

        let dom_id = self.current_domain;

        print!("\nCurrent Domain: {} of {}", dom_id, self.len(),);

        println!("\nActs: {}", self.domains[dom_id].actions);

        let cur_state = &self.domains[dom_id].current_state();

        println!(
            "\nDom: {dom_id} Current State: {cur_state} Available changes: {}",
            if let Some(agg_cngs) = self.domains[dom_id].aggregate_changes() {
                format!("{agg_cngs}")
            } else {
                "None".to_string()
            }
        );
    }

    /// Change the current display domain.
    pub fn change_domain(&mut self, dom_id: usize) {
        debug_assert!(dom_id < self.domains.len());

        self.current_domain = dom_id;
    }

    /// Set the current state field, of the current domain.
    pub fn set_cur_state(&mut self, new_state: SomeState) {
        debug_assert!(new_state.num_bits() == self.domains[self.current_domain].num_bits());

        self.set_domain_state(self.current_domain, new_state);
    }

    /// Set the state of a domain.
    pub fn set_domain_state(&mut self, dom_id: usize, new_state: SomeState) {
        debug_assert!(dom_id < self.domains.len());
        debug_assert!(new_state.num_bits() == self.domains[dom_id].num_bits());

        self.domains[dom_id].set_cur_state(new_state);
    }

    /// Set the current state fields, of each domain.
    pub fn set_cur_states(&mut self, new_states: &StatesCorr) {
        self.domains.set_cur_states(new_states);
    }

    /// Return a plan for a given start RegionsCorr, goal RegionsCorr, within a RegionsCorrStore.
    pub fn plan_using_least_negative_select_regions_get_plan(
        &self,
        start_regs: &RegionsCorr,
        goal_regs: &RegionsCorr,
        within: &[&RegionsCorr],
        cur_rate: isize,
    ) -> Result<PlansCorrStore, Vec<String>> {
        //println!(
        //    "plan_using_least_negative_select_regions_get_plan: starting: start {start_regs} goal: {goal_regs} within {}", tools::vec_ref_string(within)
        //);
        debug_assert!(!within.is_empty());

        let plans = (0..6)
            .into_par_iter() // into_par_iter for parallel, .into_iter for easier reading of diagnostic messages
            .map(|_| {
                self.plan_using_least_negative_select_regions_get_plan2(
                    start_regs, goal_regs, within, cur_rate,
                )
            })
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
            ATarget::SelectRegions { select } => &select.regions,
        };
        self.plan_using_least_negative_select_regions(&from, goal)
    }

    /// Return the most negative fragment value that is a superset
    /// of a given RegionsCorr.
    pub fn most_negative_rate(&self, rcx: &RegionsCorr) -> isize {
        let mut min_val = 0;
        for selx in self.select_negative.iter() {
            if selx.value < min_val && selx.regions.is_superset_of(rcx) {
                min_val = selx.value;
            }
        }
        //println!("min_val of {rcx} is {min_val}");
        min_val
    }

    /// Try to formulate a plan using non-negative paths first,
    /// then add increasingly negative paths if needed.
    /// Each set of plans to traverse an SR can be run in parallel.
    pub fn plan_using_least_negative_select_regions(
        &self,
        start_regs: &RegionsCorr,
        goal_regs: &RegionsCorr,
    ) -> Result<NeedPlan, Vec<String>> {
        //println!(
        //    "plan_using_least_negative_select_regions: starting: start {start_regs} goal: {goal_regs}"
        //);
        debug_assert!(self.is_congruent(start_regs));
        debug_assert!(self.is_congruent(goal_regs));

        // Check if no plans are needed.
        if start_regs.is_subset_of(goal_regs) {
            //println!("sessiondata::plan_using_least_negative_select_regions: return 1");
            return Ok(NeedPlan::AtTarget {});
        }

        // Make a list of negative values in self.select_negative.
        let mut neg_values = Vec::<isize>::new();
        for selx in self.select_negative.iter() {
            if !neg_values.contains(&selx.value) {
                neg_values.push(selx.value);
            }
        }

        neg_values.sort_by(|a, b| b.cmp(a)); // sort to least negative first.
                                             //println!("neg values {neg_values:?}");

        // Init available path fragments with non-negative fragments.
        let mut fragments = Vec::<&RegionsCorr>::new();

        for rcx in self.rc_non_negative.iter() {
            fragments.push(rcx);
        }

        // Check for edge case, no SelectRegions.
        let all_regs = self.maximum_regions();
        if fragments.is_empty() && self.select_negative.is_empty() {
            fragments.push(&all_regs);
        }
        //println!("fragments {}", tools::vec_ref_string(&fragments));

        // Check need for negative fragments, at the start.
        let mut neg_inx = 0;
        let min_start = self.most_negative_rate(start_regs);
        let min_goal = self.most_negative_rate(goal_regs);
        let mut current_rate = if min_start < min_goal {
            min_start
        } else {
            min_goal
        };

        for nvx in neg_values.iter() {
            if *nvx < current_rate {
                break;
            }
            neg_inx += 1;

            for selx in self.select_negative.iter() {
                if selx.value == *nvx {
                    //println!("  adding neg value {nvx}");
                    fragments.push(&selx.regions);
                }
            }
        }

        let maximum_regions = self.domains.maximum_regions();

        loop {
            //println!("current_rate is {current_rate}");
            // fragments could be empty if there is a single, maximunm-region, negative select region.
            if !fragments.is_empty() {
                // until plan found or all negative SRs added to fragments.
                // Use fragments to find path.
                if let Ok(planx) = self.plan_using_least_negative_select_regions_get_plan(
                    start_regs,
                    goal_regs,
                    &fragments,
                    current_rate,
                ) {
                    //println!("sessiondata::plan_using_least_negative_select_regions: returns {planx} current_rate {current_rate}");
                    return Ok(NeedPlan::PlanFound { plan: planx });
                }
            }

            // Add more negative regioncorrs.
            //println!("   adding {} valued SRs", neg_values[neg_inx]);
            if neg_inx == neg_values.len() {
                break;
            }
            if neg_inx == neg_values.len() - 1 {
                fragments = vec![&maximum_regions];
            } else {
                current_rate = neg_values[neg_inx];
                for selx in self.select_negative.iter() {
                    if selx.value == current_rate {
                        //println!("  found {selx}");
                        fragments.push(&selx.regions);
                    }
                }
            }

            neg_inx += 1;
        }

        // Return failure.
        Err(vec!["No plan found".to_string()])
    }

    /// Return the nearest non-negative regions.
    pub fn closest_rc_regions(&self, from_regs: &RegionsCorr) -> Option<&RegionsCorr> {
        // Find closest non-negative SelectRegions, if any.
        debug_assert!(self.is_congruent(from_regs));

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
    fn plan_using_least_negative_select_regions_get_plan2(
        &self,
        start_regs: &RegionsCorr,
        goal_regs: &RegionsCorr,
        select_regions: &[&RegionsCorr],
        cur_rate: isize,
    ) -> Result<PlansCorrStore, Vec<String>> {
        debug_assert!(!select_regions.is_empty());
        //println!(
        //    "plan_using_least_negative_select_regions2: starting: start {start_regs} goal: {goal_regs} select {}", tools::vec_ref_string(select_regions)
        //);

        // Check no plan needed.
        if goal_regs.is_superset_of(start_regs) {
            return Err(vec![format!("sessiondata::plan_using_least_negative_select_regions2: goal_regs {goal_regs} superset of start_regs {start_regs}")]);
        }

        // Check if start_regs and goal_regs are in the same RegionsCorr.
        for selx in select_regions.iter() {
            if selx.is_superset_of(start_regs) && selx.intersects(goal_regs) {
                match self.make_plans(
                    start_regs,
                    &selx.intersection(goal_regs).expect("SNH"),
                    selx,
                ) {
                    Ok(mut plans) => {
                        plans.set_rate(cur_rate);
                        //assert!(plans.is_valid());
                        let plncs = PlansCorrStore::new(vec![plans]);
                        //println!("sessiondata::plan_using_least_negative_select_regions2: returning (1) {plncs}");
                        return Ok(plncs);
                    }
                    Err(errvec) => {
                        return Err(errvec);
                    }
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
            Err(vec![format!("plan_using_least_negative_select_regions2: mid_paths from {start_regs} to {goal_regs} within {} failed",
             tools::vec_ref_string(select_regions))]);
        }

        // Find lowest length paths, if needed.
        let mut low = usize::MAX;
        let mut high = 0;
        for planx in mid_paths.iter() {
            let len = planx.len();
            if len > high {
                high = len;
            }
            if len < low {
                low = len;
            }
        }
        if low != high {
            let mut lows = Vec::<RegionsCorrStore>::new();
            for planx in mid_paths {
                if planx.len() == low {
                    lows.push(planx);
                }
            }
            mid_paths = lows;
        }

        // Randomly choose a possible path.
        let pathx = mid_paths.swap_remove(rand::thread_rng().gen_range(0..mid_paths.len()));

        let mut cur_regs = start_regs.clone(); // Borrow checker thinks the above map is still in force?

        let mut mid_plans = PlansCorrStore::new(vec![]);
        for inx in 1..(pathx.len() - 1) {
            if let Some(intx) = pathx[inx].intersection(&pathx[inx + 1]) {
                if intx != cur_regs && !intx.is_superset_of(&cur_regs) {
                    match self.make_plans(&cur_regs, &intx, &pathx[inx]) {
                        Ok(mut plans) => {
                            plans.set_rate(cur_rate);
                            //println!("sessiondata::plan_using_least_negative_select_regions2: for {cur_regs} to {intx} plans found {plans}");
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
                }
            } else {
                return Err(vec![format!("sessiondata::plan_using_least_negative_select_regions2: {} does not intersect {}", pathx[inx], pathx[inx + 1])]);
            }
        }
        if mid_plans.is_empty() {
            return Err(vec![format!("sessiondata::plan_using_least_negative_select_regions2: plans for {pathx} mid_plans not found")]);
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

    /// Return a series of intersecting RegionsCorrs, to
    /// guide a path between start and goal.
    fn get_path_through_select_regions<'a>(
        &'a self,
        start_regs: &'a RegionsCorr,
        goal_regs: &'a RegionsCorr,
        select_regions: &'a [&RegionsCorr],
    ) -> Option<RegionsCorrStore> {
        //println!(
        //    "get_path_through_select_regions: starting: start {start_regs} goal: {goal_regs} select {}", tools::vec_ref_string(select_regions)
        //);
        // Start a list with the start regions, and a list with the goal regions.
        // Successively add intersections, of the last item of each list, to extend each list,
        // until the the two lists intersect, or no more new intersections can extend the lists.
        //
        // Kind of like two random depth-first searches.

        // Init start intersections RCS.
        let mut start_ints = RegionsCorrStore::new(vec![]);
        start_ints.push(start_regs.clone());

        // Init goal intersections RCS.
        let mut goal_ints = RegionsCorrStore::new(vec![]);
        goal_ints.push(goal_regs.clone());

        // Init work vector.
        let mut select_regions2: Vec<_> = select_regions.to_vec();

        // Build out start intersection and goal intersection RCSs, until there is an intersection/adjacency between them, or
        // no more options.
        while !select_regions2.is_empty() {
            // Get next layer of intersections for start_ints.
            let mut int_added = false;

            let start_last = start_ints.last()?;
            let goal_last = goal_ints.last()?;

            // Randomly pick possible SelectRegions to test.
            let mut randpick = tools::RandomPick::new(select_regions2.len());

            while let Some(inx) = randpick.pick() {
                let selx = select_regions2[inx];

                // Add a new RegionsCorr intersection, to start_int or goal_ints.

                // A new RegionsCorr may intersect the last RC in the start_ints RCS.
                if start_last.intersects(selx) {
                    start_ints.push((*selx).clone());
                    tools::remove_unordered(&mut select_regions2, inx);
                    int_added = true;
                    break;
                } else if start_last.is_adjacent(selx) {
                    let bridge = start_last.symmetrical_overlapping_regions(selx);
                    start_ints.push(bridge);
                    start_ints.push((*selx).clone());
                    tools::remove_unordered(&mut select_regions2, inx);
                    int_added = true;
                    break;
                } else {
                    // A new RegionsCorr may intersect the last RC in the goal_ints RCS.
                    if goal_last.intersects(selx) {
                        goal_ints.push((*selx).clone());
                        tools::remove_unordered(&mut select_regions2, inx);
                        int_added = true;
                        break;
                    } else if goal_last.is_adjacent(selx) {
                        let bridge = goal_last.symmetrical_overlapping_regions(selx);
                        goal_ints.push(bridge);
                        goal_ints.push((*selx).clone());
                        tools::remove_unordered(&mut select_regions2, inx);
                        int_added = true;
                        break;
                    }
                }
            } // next pick.

            // Check if start/goal connection found.
            if !int_added {
                return None;
            }

            // A select regions, or a bridge and a select regions, has been added to either start_ints, or goal_ints.

            // Check for two intersecting items between the start and goal intersection RCSs.
            if let Some((s, g)) = start_ints.intersecting_pair(&goal_ints) {
                start_ints.truncate(s + 1);
                goal_ints.truncate(g + 1);

                goal_ints.reverse();
                start_ints.append(goal_ints);
                return Some(start_ints);

                // Check for two adjacent items between the start and goal intersection RCSs.
            }
            if let Some((s, g)) = start_ints.adjacent_pair(&goal_ints) {
                start_ints.truncate(s + 1);
                goal_ints.truncate(g + 1);

                let bridge = start_ints[s].symmetrical_overlapping_regions(&goal_ints[g]);
                start_ints.push(bridge);

                goal_ints.reverse();
                start_ints.append(goal_ints);
                return Some(start_ints);
            }
        } // end loop
        None
    } // end get_path_through_select_regions

    /// Return a String representation of a SessionData.
    fn formatted_str(&self) -> String {
        let mut rc_str = String::from("[");

        rc_str.push_str(&self.domains.to_string());

        rc_str.push(']');

        rc_str
    }

    /// Return a from_str compatible string for a SessionData instance.
    pub fn formatted_def(&self) -> String {
        let mut rc_str = String::from("SD[");

        rc_str.push_str(&self.domains.formatted_def());
        rc_str.push_str(", ");

        for selx in self.select.iter() {
            rc_str.push_str(&format!(", {selx}"));
        }
        rc_str.push_str(&format!(", {}", self.all_current_states()));

        rc_str.push(']');

        rc_str
    }

    /// Return the total number of groups in all the domains.
    pub fn number_groups(&self) -> usize {
        self.domains.number_groups()
    }

    /// Return the total number of groups defined in all the domains.
    pub fn number_groups_defined(&self) -> usize {
        self.domains.number_groups_defined()
    }

    /// Print a plan step-by-step, indicating changes.
    pub fn print_planscorrstore_detail(&self, plan_str: &PlansCorrStore) {
        self.print_planscorrstore_detail2(plan_str, &self.all_current_states());
    }

    /// Print a plan step-by-step, indicating changes.
    pub fn print_planscorrstore_detail2(&self, planscsx: &PlansCorrStore, states: &StatesCorr) {
        let mut cur_states = states.clone();

        for (pc_id, planscx) in planscsx.iter().enumerate() {
            println!("\nPC: {pc_id}");
            for (dom_id, planx) in planscx.iter().enumerate() {
                if planx.causes_change() {
                    println!("\n  Domain: {}, Plan:", dom_id);
                    for stepx in planx.iter() {
                        let df = stepx.initial.diff_edge_mask(&stepx.result);
                        print!(
                            "    {} Action {:02} -> {}",
                            stepx.initial, stepx.act_id, stepx.result
                        );

                        cur_states[dom_id] =
                            stepx.rule.result_from_initial_state(&cur_states[dom_id]);

                        if let AltRuleHint::AltRule { .. } = &stepx.alt_rule {
                            print!(" Alt_rule: -1");
                        }

                        for sel_regx in self.select.iter() {
                            if sel_regx.value < 0
                                && sel_regx.regions.is_superset_states(&cur_states)
                            {
                                print!(" in {:+}, {}", sel_regx.regions, sel_regx.value);
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
        self.domains.maximum_regions()
    }

    /// Get plans for moving from one set of domain states to another, within a given set of domain regions.
    pub fn make_plans(
        &self,
        from: &RegionsCorr,
        goal: &RegionsCorr,
        within: &RegionsCorr,
    ) -> Result<PlansCorr, Vec<String>> {
        debug_assert!(within.is_superset_of(from));
        debug_assert!(within.is_superset_of(goal));

        //println!("make_plans: from {from} to {goal} within {within}");

        if from.is_subset_of(goal) {
            return Err(vec![format!(
                "sessiondata::make_plans: from {from} is a subset of the goal {goal}"
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
            //println!("sessiondata::make_plans: returning:");
            //for plnx in plans2.iter() {
            //    println!("    {plnx}");
            //}
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
        //println!("sessiondata:: make_plans2: from {from} goal {goal}");
        debug_assert!(within.is_superset_of(from));
        debug_assert!(within.is_superset_of(goal));

        let mut plans_per_target = PlansCorr::with_capacity(from.len());

        // Find a plan for each target.
        for (dom_id, (regx, regy)) in from.iter().zip(goal.iter()).enumerate() {
            //println!("dom_id {dom_id}");
            if regy.is_superset_of(regx) {
                plans_per_target.push(SomePlan::new(dom_id, vec![SomeStep::new_no_op(regx)]));
            } else {
                // Try making plans.
                match self.make_plans_domain(dom_id, regx, regy, &within[dom_id]) {
                    Ok(mut plans) => {
                        //println!(" {} plans from make_plans", plans.len());
                        plans_per_target
                            .push(plans.remove(rand::thread_rng().gen_range(0..plans.len())));
                    }
                    Err(errvec) => {
                        return Err(errvec);
                    }
                };
            }
        } // next domain

        //println!("sessiondata::make_plans2: returning {plans_per_target}");
        Ok(plans_per_target)
    }

    /// Try to satisfy a need.
    pub fn do_a_need(&mut self, np_inx: usize) -> Result<(), String> {
        let dom_id = self.current_domain;

        let inx_pln = self.can_do[np_inx].clone();
        let inx = self.can_do[np_inx].inx;

        //let needx = &self.needs[inx];

        // Display Domain info, if needed.
        match self.needs[inx] {
            SomeNeed::ToSelectRegions { .. } => {
                //println!("\nNeed chosen: {} {}", ndx, plans.str_terse())
            }
            SomeNeed::ExitSelectRegions { .. } => {
                //println!("\nNeed chosen: {} {}", nd_inx, inx_pln.plans.str_terse())
            }
            _ => {
                let nd_dom = self.needs[inx].dom_id().unwrap();
                if dom_id != nd_dom {
                    // Show "before" state before running need.
                    println!("\nAll domain states: {}", self.all_current_states());
                    self.change_domain(nd_dom);
                    //self.print();
                    //println!("\nNeed chosen: {} {}", ndx, plans.str_terse());
                }
            }
        }

        // Run the plan, allow for one failure.
        match &inx_pln.plans {
            NeedPlan::PlanFound { plan: plans } => match self.run_planscorrstore(plans) {
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
                        self.needs[inx].dom_id(),
                        self.needs[inx].target(),
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
                                        return Err(format!("Second failure, giving up, {errstr}"));
                                    }
                                }
                            }
                            NeedPlan::AtTarget {} => (),
                        }
                    } else {
                        return Err(format!(
                            "New path from {} to goal {} not found.",
                            self.all_current_states(),
                            self.needs[inx].target()
                        ));
                    }
                }
            },
            NeedPlan::AtTarget {} => (),
        }

        // Take action after the desired state is reached.

        if self.needs[inx].satisfied_by(&self.all_current_states()) {
            match self.needs[inx] {
                SomeNeed::ToSelectRegions { .. } => {
                    if self.set_boredom_limit() {
                        self.update_times_visited();
                    }
                }
                SomeNeed::ExitSelectRegions { .. } => (),
                _ => {
                    self.take_action_need(inx);
                }
            }
            Ok(())
        } else {
            Err("Need not satisfied".to_string())
        }
    } // end do_a_need

    /// Run cleanup for a domain and action.
    pub fn cleanup(&mut self, dom_id: usize, act_id: usize, needs: &NeedStore) {
        self.domains.cleanup(dom_id, act_id, needs);
    }

    /// Return true if corresponding items have the same number of bits.
    pub fn is_congruent(&self, other: &impl CorrespondingItems) -> bool {
        self.num_bits_vec() == other.num_bits_vec()
    }

    /// Return a vector of corresponding num_bits.
    pub fn num_bits_vec(&self) -> Vec<usize> {
        self.domains.num_bits_vec()
    }

    /// Find a domain that matches a given ID, return a reference.
    pub fn find_domain(&self, dom_id: usize) -> Option<&SomeDomain> {
        self.domains.find_domain(dom_id)
    }

    /// Take an action in a domain.
    pub fn take_action(&mut self, dom_id: usize, act_id: usize) {
        self.domains.take_action(dom_id, act_id);
    }

    /// Evaluate an arbitrary sample.
    pub fn eval_sample_arbitrary(&mut self, dom_id: usize, act_id: usize, smpl: &SomeSample) {
        self.domains.eval_sample_arbitrary(dom_id, act_id, smpl);
    }

    /// Take an arbitrary action.
    pub fn take_action_arbitrary(&mut self, dom_id: usize, act_id: usize, astate: &SomeState) {
        self.domains.take_action_arbitrary(dom_id, act_id, astate);
    }

    /// Set the cleanup limit for a domain-action.
    pub fn set_domain_cleanup(&mut self, dom_id: usize, act_id: usize, trigger: usize) {
        self.domains.set_domain_cleanup(dom_id, act_id, trigger);
    }

    /// Calculate aggregate changes, for SessionData initialization.
    pub fn calc_aggregate_changes(&mut self) {
        self.domains.calc_aggregate_changes();
    }

    /// Collect steps that contain at least one wanted change.
    pub fn get_steps_domain(
        &self,
        dom_id: usize,
        wanted_changes: &SomeChange,
        within: &SomeRegion,
    ) -> StepStore {
        self.domains
            .get_steps_domain(dom_id, wanted_changes, within)
    }

    /// Return a SelectRegions rating for a given RegionsCorr.
    pub fn rate_results(&self, regscr: &RegionsCorr) -> (isize, isize) {
        let mut pos = 0;
        let mut neg = 0;
        for selx in self.select.iter() {
            if selx.regions.is_superset_of(regscr) {
                if selx.value < 0 {
                    neg += selx.value;
                } else {
                    pos += selx.value;
                }
            }
        }
        (pos, neg)
    }

    /// Validate, massage, RegionsCorr input by user.
    pub fn validate_rc(&self, rcx: RegionsCorr) -> Result<RegionsCorr, String> {
        self.domains.validate_rc(rcx)
    }
}

impl FromStr for SessionData {
    type Err = String;
    /// Return a SessionData instance, given a string representation.
    /// The string contains:
    ///   One, or more, domains,.
    ///   Zero, or more, SelectRegions, after the actions.
    ///   Zero, or one, StatesCorr, for each domain initial state. The default domain initial state is random.
    ///
    /// SD[DS[DOMAIN[
    ///     ACT[[XX/XX/XX/Xx]],
    ///     ACT[[XX/XX/Xx/XX]],
    ///     ACT[[XX/Xx/XX/XX]],
    ///     ACT[[Xx/XX/XX/XX]]]],
    ///     SR[RC[r0X0X], 3],
    ///     SR[RC[r0XX1], -1],
    ///     SR[RC[rX111], -2],
    ///     SC[s1011]]
    ///
    /// All the rules must use the same number of bits.
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("SessionData::from_str: {str_in}");
        let str_in2 = str_in.trim();

        // Strip off surrounding id and brackets.
        if str_in2.len() < 4 {
            return Err("sessiondata::from_str: should be at least SD[]?".to_string());
        }

        if str_in2[0..3].to_uppercase() != *"SD[" {
            return Err("sessiondata::from_str: string should begin with SD[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("sessiondata::from_str: string should end with ]".to_string());
        }

        // Strip off id and surrounding brackets.
        let token_str = &str_in2[3..(str_in2.len() - 1)];

        // Split string into DS, SR or SC tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokvec) => tokvec,
            Err(errstr) => return Err(format!("sessiondata::from_str: {errstr}")),
        };

        // Process tokens.

        // There must be a DomainStore..
        let mut sdx_opt: Option<SessionData> = None;
        for tokenx in tokens.iter() {
            if tokenx[0..3].to_uppercase() == *"DS[" {
                match DomainStore::from_str(tokenx) {
                    Ok(dsx) => sdx_opt = Some(SessionData::new(dsx)),
                    Err(errstr) => return Err(format!("sessiondata::from_str: {errstr}")),
                }
                break;
            }
        }

        let mut sdx = sdx_opt.expect("No DomainStore token found");

        for tokenx in tokens.iter() {
            if tokenx[0..3].to_uppercase() == *"DS[" {
            } else if tokenx[0..3].to_uppercase() == *"SR[" {
                //println!("found SelectRegions {tokenx}");
                match SelectRegions::from_str(tokenx) {
                    Ok(selx) => sdx.add_select(selx),
                    Err(errstr) => return Err(format!("sessiondata::from_str: {errstr}")),
                }
            } else if tokenx[0..3].to_uppercase() == *"SC[" {
                //println!("found StatesCorr {tokenx}");
                match StatesCorr::from_str(tokenx) {
                    Ok(stacx) => sdx.set_cur_states(&stacx),
                    Err(errstr) => return Err(format!("sessiondata::from_str: {errstr}")),
                }
            } else {
                return Err(format!("sessiondata::from_str: Invalid token, {tokenx}"));
            }
        }
        // Finish up.
        sdx.calc_select();
        sdx.calc_aggregate_changes();

        Ok(sdx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    /// Return the number of supersets of a StateStore
    fn number_supersets_of_states(select: &SelectRegionsStore, stas: &StatesCorr) -> usize {
        select
            .items
            .iter()
            .map(|regsx| usize::from(regsx.regions.is_superset_states(stas)))
            .sum()
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let sdx_str = "
        SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx]],
            ACT[[XX/XX/Xx/XX]],
            ACT[[XX/Xx/XX/XX]],
            ACT[[Xx/XX/XX/XX]]]],
            SR[RC[r0X0X], 3],
            SR[RC[r0XX1], -1],
            SR[RC[rX111], -2],
            SC[s1011]]";

        let sdx = match SessionData::from_str(&sdx_str) {
            Ok(sdx) => sdx,
            Err(errstr) => panic!("{errstr}"),
        };

        println!("sdx {sdx_str}");
        println!("sdx {sdx}");
        if let Some(domx) = sdx.find_domain(0) {
            if let Some(_actx) = domx.find_action(4) {
                return Ok(());
            } else {
                return Err("No action 4?".to_string());
            }
        } else {
            return Err("No domain?".to_string());
        }

        //      let sdx_str2 = sdx.formatted_def(); // Instance to String(2).
        //      println!("sdx_str2 {sdx_str2}");

        //        match SessionData::from_str(&sdx_str2) {
        //            // String(2) to instance.
        //            Ok(sdy) => {
        //                assert!(sdy == sdx);
        //                Ok(())
        //            }
        //            Err(errstr) => Err(errstr),
        //        }
    }

    #[test]
    /// Test case where positive regions the start and goal are in, intersect.
    fn avoidance1x() -> Result<(), String> {
        // Init SessionData. Domain.
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx], s0000, s1111],
            ACT[[XX/XX/Xx/XX], s0000, s1111],
            ACT[[XX/Xx/XX/XX], s0000, s1111],
            ACT[[Xx/XX/XX/XX], s0000, s1111]]],
            SR[RC[r01X1], -1],
            SR[RC[rX101], -2],
            SC[s0000]
        ]",
        )?;

        // Set state for domain 0.
        sdx.set_domain_state(0, SomeState::from_str("s0001")?);

        println!("\nActions {}\n", sdx.find_domain(0).expect("SNH").actions);
        println!("Select Regions: {}\n", sdx.select);

        let start_region = RegionsCorr::from_str("RC[r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1111]")?;

        match sdx.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(ndpln) => match ndpln {
                NeedPlan::PlanFound { plan: planx } => {
                    println!(
                        "Plan found: {} start {start_region} goal {goal_region}",
                        planx
                    );
                    assert!(planx.rate() == 0);
                    Ok(())
                }
                NeedPlan::AtTarget {} => Err("AtTarget not expected".to_string()),
            },
            Err(errvec) => Err(format!("{:?}", errvec)),
        }
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// but another region intersects both.
    fn avoidance2() -> Result<(), String> {
        // Init SessionData, Domain.
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx], s0000, s1111],
            ACT[[XX/XX/Xx/XX], s0000, s1111],
            ACT[[XX/Xx/XX/XX], s0000, s1111],
            ACT[[Xx/XX/XX/XX], s0000, s1111]]],
            SR[RC[r0101], -1],
            SR[RC[r1001], -1],
            SC[s0000]
        ]",
        )?;

        // Set state for domain 0.
        sdx.set_domain_state(0, SomeState::from_str("s0001")?);

        println!("\nActions {}\n", sdx.find_domain(0).expect("SNH").actions);
        println!("Select Regions: {}\n", sdx.select);

        let start_region = RegionsCorr::from_str("RC[r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1101]")?;

        match sdx.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: planx }) => {
                println!("Plan found: {planx}");
                assert!(planx.rate() == 0);
                Ok(())
            }
            _ => Err("No plan found?".to_string()),
        }
    }

    #[test]
    /// Test case where non-negative regions the start and goal are in, do not intersect,
    /// and another region does not intersect both.
    fn avoidance3() -> Result<(), String> {
        // Init SessionData, Domain.
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx], s0000, s1111],
            ACT[[XX/XX/Xx/XX], s0000, s1111],
            ACT[[XX/Xx/XX/XX], s0000, s1111],
            ACT[[Xx/XX/XX/XX], s0000, s1111]]],
            SR[RC[r0x00], -1],
            SR[RC[rx100], -1],
            SR[RC[r01x1], -1],
            SR[RC[r10x1], -1],
            SR[RC[r101x], -1],
            SC[s0000]
        ]",
        )?;

        // Set state for domain 0.
        let first_state = SomeState::from_str("s0001")?;
        sdx.set_domain_state(0, first_state.clone());

        println!("\nActions {}\n", sdx.find_domain(0).expect("SNH").actions);
        println!("Select Regions: {}\n", sdx.select);

        let start_region = RegionsCorr::from_str("RC[r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1101]")?;

        match sdx.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: planx }) => {
                println!("Plan found: {}", planx);
                assert!(planx.rate() == 0);
                Ok(())
            }
            _ => Err("No plan found?".to_string()),
        }
    }

    #[test]
    /// Test case where start and goal regions are not in a non-negative region.
    fn avoidance5() -> Result<(), String> {
        // Init SessionData, Domain.
        let mut sdx = SessionData::from_str(
            "SD[DS[DOMAIN[
            ACT[[XX/XX/XX/Xx], s0000, s1111],
            ACT[[XX/XX/Xx/XX], s0000, s1111],
            ACT[[XX/Xx/XX/XX], s0000, s1111],
            ACT[[Xx/XX/XX/XX], s0000, s1111]]],
            SR[RC[rxx0x], -1],
            SC[s0000]
        ]",
        )?;

        // Set state for domain 0.
        sdx.set_domain_state(0, SomeState::from_str("s0001")?);

        println!("\nActions {}\n", sdx.find_domain(0).expect("SNH").actions);
        println!("Select Regions: {}\n", sdx.select);

        let start_region = RegionsCorr::from_str("RC[r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1101]")?;

        match sdx.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: planx }) => {
                println!("Plan found: {planx}");
                planx.str_terse();
                Ok(())
            }
            _ => Err(format!("No plan {start_region} to {goal_region}?")),
        }
    }

    #[test]
    fn all_current_states() -> Result<(), String> {
        // Init SessionData.
        let sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[ACT[[XX/XX/XX/XX_XX/XX/XX/XX]]],
            DOMAIN[ACT[[XX/XX/XX/XX_XX/XX/XX/XX_XX/XX/XX/XX_XX/XX/XX/XX]]]],
            SC[s0001_0010, s1010_1011_1100_1101]
        ]",
        )?;

        let all_states = sdx.all_current_states();
        println!("all states {}", all_states);

        assert!(all_states.len() == 2);
        assert!(all_states[0] == sdx.find_domain(0).expect("SNH").cur_state);
        assert!(all_states[1] == sdx.find_domain(1).expect("SNH").cur_state);

        Ok(())
    }

    #[test]
    /// Test case using adjacent non-negative regions.
    fn avoidance6() -> Result<(), String> {
        // Init SessionData.
        let sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[
            ACT[[XX/XX/XX/Xx], s0000, s1111],
            ACT[[XX/XX/Xx/XX], s0000, s1111],
            ACT[[XX/Xx/XX/XX], s0000, s1111],
            ACT[[Xx/XX/XX/XX], s0000, s1111]]],
            SR[RC[r1100], -1],
            SR[RC[r1011], -1],
            SC[s0000]
        ]",
        )?;

        let start_region = RegionsCorr::from_str("RC[r0000]")?;
        let goal_region = RegionsCorr::from_str("RC[r1101]")?;

        // Try making plans.
        match sdx.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.rate() == 0);
                Ok(())
            }
            _ => Err(format!("No plan found?")),
        }
    }

    #[test]
    /// Test case using two domains.
    fn avoidance7() -> Result<(), String> {
        // Init SessionData, Domains.
        let sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[
                ACT[[XX/XX/XX/Xx], s0000, s1111],
                ACT[[XX/XX/Xx/XX], s0000, s1111],
                ACT[[XX/Xx/XX/XX], s0000, s1111],
                ACT[[Xx/XX/XX/XX], s0000, s1111]],
            DOMAIN[
                ACT[[XX/XX/XX/Xx], s0000, s1111],
                ACT[[XX/XX/Xx/XX], s0000, s1111],
                ACT[[XX/Xx/XX/XX], s0000, s1111],
                ACT[[Xx/XX/XX/XX], s0000, s1111]]],
            SR[RC[r01x1, rxxxx], -1],
            SR[RC[rx101, rxxxx], -1],
            SR[RC[rxxxx, r011x], -1],
            SR[RC[rxxxx, rx111], -1],
            SC[s0000, s0001]
        ]",
        )?;

        let start_region = RegionsCorr::from_str("RC[r0000, r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1111, r1110]")?;

        println!("\nActions {}\n", sdx.find_domain(0).expect("SNH").actions);
        println!("\nActions {}\n", sdx.find_domain(1).expect("SNH").actions);

        // Try making plans.
        match sdx.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.rate() == 0);
                Ok(())
            }
            _ => Err(format!("No plan found?")),
        }
    }

    #[test]
    /// Test case using two domains, where a non-target domain must change first,
    /// due to the boolean AND relationship, with two non-maximum regions, in a negative SelectRegions instance.
    fn avoidance8() -> Result<(), String> {
        // Init SessionData, Domain.
        let sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[
                ACT[[XX/XX/XX/Xx], s0000, s1111],
                ACT[[XX/XX/Xx/XX], s0000, s1111],
                ACT[[XX/Xx/XX/XX], s0000, s1111],
                ACT[[Xx/XX/XX/XX], s0000, s1111]],
            DOMAIN[
                ACT[[XX/XX/XX/Xx], s0000, s1111],
                ACT[[XX/XX/Xx/XX], s0000, s1111],
                ACT[[XX/Xx/XX/XX], s0000, s1111],
                ACT[[Xx/XX/XX/XX], s0000, s1111]]],
            SR[RC[r00xx, rxx11], -1],
            SR[RC[r11xx, r01xx], -1],
            SC[s0101, s0111]
        ]",
        )?;

        let start_regions = RegionsCorr::from_str("RC[r0101, r0111]")?;

        let goal_regions = RegionsCorr::from_str("RC[r1001, r0111]")?;

        println!("\nActions {}\n", sdx.find_domain(0).expect("SNH").actions);
        println!("\nActions {}\n", sdx.find_domain(1).expect("SNH").actions);

        // Try making plans.
        match sdx.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.rate() == 0);
                let mut cur_states = sdx.all_current_states();
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
        // Init SessionData, Domains.
        let sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[
                ACT[[XX/XX/XX/Xx], s0000, s1111],
                ACT[[XX/XX/Xx/XX], s0000, s1111],
                ACT[[XX/Xx/XX/XX], s0000, s1111],
                ACT[[Xx/XX/XX/XX], s0000, s1111]],
            DOMAIN[
                ACT[[XX/XX/XX/Xx], s0000, s1111],
                ACT[[XX/XX/Xx/XX], s0000, s1111],
                ACT[[XX/Xx/XX/XX], s0000, s1111],
                ACT[[Xx/XX/XX/XX], s0000, s1111]]],
            SR[RC[r000x, rxx11], -1],
            SR[RC[r11x1, r01xx], -1],
            SC[s0101, s0111]
        ]",
        )?;

        let start_regions = RegionsCorr::from_str("RC[r0101, r0111]")?;

        let goal_regions = RegionsCorr::from_str("RC[r1001, r0111]")?;

        println!("\nActions {}\n", sdx.find_domain(0).expect("SNH").actions);
        println!("\nActions {}\n", sdx.find_domain(1).expect("SNH").actions);

        // Try making plans.
        match sdx.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
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
        // Init SessionData, Domains.
        let mut sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[ACT[[XX/XX/XX/XX_XX/XX/XX/XX]]],
            DOMAIN[ACT[[XX/XX/XX/XX_XX/XX/XX/XX_XX/XX/XX/XX_XX/XX/XX/XX]]]],
            SR[RC[r00000x0x, rXXXXXX10_1XXX_XXXX], 1],
            SR[RC[r00000xx1, rXXXXXX10_1XXX_XXXX], 1],
            SR[RC[r0000x1x1, rXXXXXX10_1XXX_XXXX], 1],
            SR[RC[r00001110, rXXXXXX10_1XXX_XXXX], 1]
        ]",
        )?;

        // Set state for domain 0.
        let first_state = SomeState::from_str("s0001_0010")?;
        sdx.set_domain_state(0, first_state.clone());

        // Set state for domain 1.
        let state2 = SomeState::from_str("s1010_1011_1100_1101")?;
        sdx.set_domain_state(1, state2.clone());

        sdx.boredom = 0;
        sdx.boredom_limit = 0;

        let mut stasc = StatesCorr::with_capacity(2);
        stasc.push(first_state.clone());
        stasc.push(state2.clone());

        let num_sup = number_supersets_of_states(&sdx.select, &stasc);
        println!("\nNumber supersets: {num_sup}",);
        assert!(num_sup == 0);

        if let Some(needx) = sdx.check_select() {
            println!("\nCheck_select returns {}", needx);
        } else {
            return Err(format!("No need found?"));
        }

        // Set state for domain 0.
        let first_state = SomeState::from_str("s0000_0101")?;
        sdx.set_domain_state(0, first_state.clone());

        // Set state for domain 1.
        let state2 = SomeState::from_str("s1010_0010_1000_1101")?;
        sdx.set_domain_state(1, state2.clone());

        sdx.boredom = 0;
        sdx.boredom_limit = 2;
        println!(
            "\nBoredom level {} Boredom_limit {}",
            sdx.boredom, sdx.boredom_limit
        );

        if let Some(needx) = sdx.check_select() {
            return Err(format!("\nCheck_select returns need? {}", needx));
        }

        println!(
            "\nBoredom level {} Boredom_limit {}",
            sdx.boredom, sdx.boredom_limit
        );
        assert!(sdx.boredom == 1);
        assert!(sdx.boredom_limit == 2);

        Ok(())
    }

    // Test exit_select_needs with two overlapping negative select regions.
    // from s0111 to s1000.
    #[test]
    fn exit_select_needs() -> Result<(), String> {
        // Init SessionData, Domain.
        let regstr1 = RegionsCorr::from_str("RC[rX1XX]")?;
        let regstr2 = RegionsCorr::from_str("RC[r1XX1]")?;

        let mut sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[ACT[[XX/XX/XX/XX]]]],
            SR[RC[rX1XX], -1],
            SR[RC[r1XX1], -1],
            SC[s1101]
        ]",
        )?;

        println!("select regs: {}", sdx.select.to_string());

        // Get exit needs.
        if let Some(nds) = sdx.check_select() {
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
        // Init SessionData, Domain.
        let sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[ACT[[XX/XX/XX/XX]]]],
            SR[RC[rxx0x], 4],
            SR[RC[r00x1], -2],
            SR[RC[r11x1], -4],
            SR[RC[r10x0], -5],
            SR[RC[rX111], -1]
            SC[s0000]
        ]",
        )?;

        println!("select positive: {}", sdx.select_net_positive);
        assert!(sdx.select_net_positive.len() == 5);

        assert!(sdx
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[rx100], 4]")?));
        assert!(sdx
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[r0x00], 4]")?));
        assert!(sdx
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[r010x], 4]")?));
        assert!(sdx
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[r1001], 4]")?));
        assert!(sdx
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[r0001], 2]")?));

        println!("non negative: {}", sdx.rc_non_negative);
        assert!(sdx.rc_non_negative.len() == 4);

        assert!(sdx
            .rc_non_negative
            .contains(&RegionsCorr::from_str("RC[rx1x0]")?));
        assert!(sdx
            .rc_non_negative
            .contains(&RegionsCorr::from_str("RC[r0xx0]")?));
        assert!(sdx
            .rc_non_negative
            .contains(&RegionsCorr::from_str("RC[r010x]")?));
        assert!(sdx
            .rc_non_negative
            .contains(&RegionsCorr::from_str("RC[r10x1]")?));

        println!("select negative: {}", sdx.select_negative);
        assert!(sdx.select_negative.len() == 5);

        assert!(sdx
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r0011], -2]")?));
        assert!(sdx
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r1000], -1]")?));
        assert!(sdx
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r1010], -5]")?));
        assert!(sdx
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r0111], -1]")?));
        assert!(sdx
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r1111], -5]")?));

        Ok(())
    }

    #[test]
    /// Test case using two domains, like avoidance8, but a way around traps.
    fn avoidance10() -> Result<(), String> {
        // Init SessionData, Domains.
        let mut sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[
                ACT[[XX/XX/XX/Xx], s0000, s1111],
                ACT[[XX/XX/Xx/XX], s0000, s1111],
                ACT[[XX/Xx/XX/XX], s0000, s1111],
                ACT[[Xx/XX/XX/XX], s0000, s1111]]],
            SR[RC[01XX], -1],
            SR[RC[10XX], -2],
            SC[s0000]
        ]",
        )?;

        let s3 = SomeState::from_str("s0011")?;
        sdx.set_domain_state(0, s3.clone());

        let start_regions = RegionsCorr::from_str("RC[r0011]")?;

        let goal_regions = RegionsCorr::from_str("RC[r1111]")?;

        println!("\nActions {}\n", sdx.find_domain(0).expect("SNH").actions);

        // Try making plans.
        // Bridge overlap, using 01XX, between 3 and F, at 7, strangely
        // makes a step within 01XX unneeded.
        match sdx.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.rate() == -1);
                assert!(plans.len() == 2);
            }
            _ => return Err(format!("No plan found?")),
        }

        let start_regions = RegionsCorr::from_str("RC[r0011]")?;

        let goal_regions = RegionsCorr::from_str("RC[r1101]")?;

        println!("\nActions {}\n", sdx.find_domain(0).expect("SNH").actions);

        // Try making plans.
        // No bridge overlap here, using 01XX, between 3 and D, requiring a step from
        // 7 to 5, within 01XX.
        match sdx.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.rate() == -1);
                assert!(plans.len() == 2 || plans.len() == 3);
                Ok(())
            }
            _ => Err(format!("No plan found?")),
        }
    }

    #[test]
    /// Test running plans in parallel, as in going from SelectRegions fragment to
    /// intersection of another SelectRegions fragment.
    fn run_planscorr() -> Result<(), String> {
        // Init SessionData.
        let mut sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[ACT[[XX/XX/XX/Xx]]],
            DOMAIN[ACT[[XX/XX/Xx]]],
            DOMAIN[ACT[[XX/Xx]]]],
            SC[s0000, s001, s10]
        ]",
        )?;

        // Set up PlansCorr.
        let plnsc1 = PlansCorr::from_str(
            "PC[[P[0, r0000-1->r0001], P[1, r001-0->r001], P[2, r10-1->r11]], 0]",
        )?;
        println!("{plnsc1}");

        let before = sdx.all_current_states();
        println!("Before: {before}");

        match sdx.run_planscorr(&plnsc1) {
            Err(errstr) => return Err(errstr),
            _ => (),
        }

        let after = sdx.all_current_states();
        println!("After:  {after}");

        assert!(after[0] == SomeState::from_str("s0001")?);
        assert!(after[1] == SomeState::from_str("s001")?);
        assert!(after[2] == SomeState::from_str("s11")?);

        Ok(())
    }

    #[test]
    /// Test running a PlansCorrStore.
    fn run_planscorrstore() -> Result<(), String> {
        // Init SessionData.
        let mut sdx = SessionData::from_str(
            "SD[DS[
            DOMAIN[ACT[[XX/XX/XX/Xx]], ACT[[XX/XX/Xx/XX]], ACT[[XX/Xx/XX/XX]], ACT[[Xx/XX/XX/XX]]],
            DOMAIN[ACT[[XX/XX/XX/Xx]], ACT[[XX/XX/Xx/XX]], ACT[[XX/Xx/XX/XX]], ACT[[Xx/XX/XX/XX]]]],
            SC[s0000, s1111]
        ]",
        )?;

        // Set up PlansCorrStore.
        let plnscrstr = PlansCorrStore::from_str("PCS[PC[[P[0, r0000-3->r0100], P[1, r1111-1->r1110]], 0], PC[[P[0, r0100-1->r0101], P[1, r1110-2->r1100]], 0]]")?;
        println!("plnscrstr {plnscrstr}");

        let before = sdx.all_current_states();
        println!("Before: {before}");

        // Run it.
        match sdx.run_planscorrstore(&plnscrstr) {
            Err(errstr) => return Err(errstr),
            _ => (),
        }

        // Check results.
        let after = sdx.all_current_states();
        println!("After:  {after}");

        assert!(after[0] == SomeState::from_str("s0101")?);
        assert!(after[1] == SomeState::from_str("s1100")?);

        Ok(())
    }

    #[test]
    /// Test run_planscorrstore, where start and goal are not subsets of any select regions.
    fn get_path_through_select_regions() -> Result<(), String> {
        // Init SessionData.
        let sdx = SessionData::from_str("SD[DS[DOMAIN[ACT[[XX/XX/XX/Xx]], ACT[[XX/XX/Xx/XX]], ACT[[XX/Xx/XX/XX]], ACT[[Xx/XX/XX/XX]]]]]")?;

        let start_regs = RegionsCorr::from_str("RC[r1X11]")?;
        let goal_regs = RegionsCorr::from_str("RC[rX000]")?;

        let within1 = RegionsCorr::from_str("RC[r0XXX]")?;
        let within2 = RegionsCorr::from_str("RC[rX1XX]")?;

        let path =
            sdx.get_path_through_select_regions(&start_regs, &goal_regs, &vec![&within1, &within2]);
        if let Some(pathx) = path {
            print!("path: ");
            for rcx in pathx.iter() {
                print!("{rcx}");
            }
            println!(" ");
            assert!(
                pathx[0] == RegionsCorr::from_str("RC[r1111]")?
                    || pathx[0] == RegionsCorr::from_str("RC[r1X11]")?
            );
            assert!(
                pathx[pathx.len() - 1] == RegionsCorr::from_str("RC[rX000]")?
                    || pathx[pathx.len() - 1] == RegionsCorr::from_str("RC[r0000]")?
            );
        } else {
            return Err("No path found?".to_string());
        }
        Ok(())
    }
}
