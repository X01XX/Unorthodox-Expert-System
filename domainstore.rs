//! The DomainStore struct, a vector of SomeDomain structs,
//! and values/methods that manage the domains.

use crate::domain::SomeDomain;
use crate::need::SomeNeed;
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
use crate::tools::{self, CorrespondingItems};

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::Iter;

use rayon::prelude::*;

impl fmt::Display for DomainStore {
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
    /// Positive region fragments, possibly overlapped by negative value regions.
    /// These tend to be goals.
    pub select_net_positive: SelectRegionsStore,
    /// Negative select fragments.
    pub select_negative: SelectRegionsStore,
    /// Times visited, order corresponding to, positive select regions.
    pub times_visited: Vec<usize>,
    /// Non-negative, may be, or overlap, positive, regions.
    pub rc_non_negative: RegionsCorrStore,
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
            select_net_positive: SelectRegionsStore::new(vec![]),
            rc_non_negative: RegionsCorrStore::new(vec![]),
            select_negative: SelectRegionsStore::new(vec![]),
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

        assert!(self.is_congruent(&selx));

        self.select.push(selx);
    }

    /// Calculate parts of select regions, in case of any overlaps.
    pub fn calc_select(&mut self) {
        if self.select.is_empty() {
            println!("\nGiven Select Regions: None\n");
        } else {
            println!("\nGiven Select Regions ({}):\n", self.select.len());
            for selx in self.select.iter() {
                println!("  {selx}");
            }
        }
        //println!("\n  Avoid negative SRs in rule-paths.");
        //println!("\n  Exit a negative SR if the current state is within one.");

        // Get fragments due to different-value intersections.
        let fragments = self.select.split_by_intersections();
        // println!("fragments");

        // Separate fragments by value.
        for selx in fragments {
            if selx.net_value > 0 {
                if selx.net_value > self.max_pos_value {
                    self.max_pos_value = selx.net_value;
                }
                self.select_net_positive.push(selx);
            }
        }

        println!(" ");
        if self.select_net_positive.is_empty() {
            println!("Net-positive SR fragments: None");
        } else {
            println!(
                "Net-positive SR fragments, each a subset of one or more SRs, no partial intersections. ({}):\n",
                self.select_net_positive.len()
            );
            for selx in self.select_net_positive.iter() {
                println!("  {selx}");
            }
            println!(" ");
            println!("  To seek, when no other needs can be done.");
            self.times_visited = vec![0; self.select_net_positive.len()];
        }

        // Calc non-negative regions.
        let mut non_neg_select = RegionsCorrStore::new(vec![self.maximum_regions()]);

        for selx in self.select.iter() {
            if selx.neg_value < 0 {
                non_neg_select = non_neg_select.subtract_regionscorr(&selx.regions);
            }
        }
        self.rc_non_negative = non_neg_select;

        println!(" ");
        if self.rc_non_negative.is_empty() {
            println!("Non-negative RCs: None");
        } else {
            println!(
                "Non-negative RCs, maximum RC regions minus negative SR regions ({}):\n",
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

        // Calc negative fragments.
        let mut select_negative = SelectRegionsStore::new(vec![]);
        for selx in self.select.iter() {
            if selx.pos_value == 0 && selx.neg_value < 0 {
                select_negative.push(selx.clone());
            }
        }
        self.select_negative = select_negative.split_by_intersections();

        println!(" ");
        if self.select_negative.is_empty() {
            println!("Negative SR fragments: None");
        } else {
            println!("Negative SR fragments, each a subset of one or more negative SRs, no partial intersections. ({}):\n", self.select_negative.len());
            for selx in self.select_negative.iter() {
                println!("  {selx}");
            }
            println!(" ");
            println!("  If more than one different value, more and more negative value SRs may be considered to find a plan-path.");
        }
    }

    /// Add a Domain struct to the store.
    /// Enforce domain id = index into domainstore items vector.
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
    pub fn get_needs(&mut self) -> (NeedStore, Vec<InxPlan>, Vec<usize>) {
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

        let (can_do, cant_do) = self.evaluate_needs(&needs);

        (needs, can_do, cant_do)
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
        debug_assert!(self.is_congruent(plns));

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
                Ok(num) => {
                    steps_run += num;
                }
                Err(errstr) => return Err(errstr),
            }
        }
        Ok(steps_run)
    }

    /// Take an action to satisfy a need,
    pub fn take_action_need(&mut self, needx: &SomeNeed) {
        self.items[needx.dom_id().unwrap()].take_action_need(needx);
    }

    /// Return a reference to the current state of a given Domain index
    pub fn cur_state(&self, dmxi: usize) -> &SomeState {
        self.items[dmxi].current_state()
    }

    /// Set can_do, and cant_do, struct fields for the DomainStore needs, which are sorted in ascending priority number order.
    /// Scan successive slices of needs, of the same priority, until one, or more, needs can be planned.
    pub fn evaluate_needs(&mut self, needs: &NeedStore) -> (Vec<InxPlan>, Vec<usize>) {
        //println!("evaluate_needs: {} needs", needs.len());

        if needs.is_empty() {
            return (vec![], vec![]);
        }

        let mut can_do = Vec::<InxPlan>::new();

        let cur_states = self.all_current_states();

        // Check for needs that can be satisfied with the current state.
        for (inx, ndx) in needs.iter().enumerate() {
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
            return (can_do, vec![]);
        }

        let mut cur_pri = needs[0].priority();
        let mut count = 0;
        print!("\nNumber needs: {}, priority(count): ", needs.len());
        for needx in needs.iter() {
            if needx.priority() > cur_pri {
                print!("{}({}) ", cur_pri, count);
                cur_pri = needx.priority();
                count = 0;
            }
            count += 1;
        }
        println!("{}({}) ", cur_pri, count);

        // Init current priority value and start index.
        let mut cur_pri = needs[0].priority();

        let mut cur_pri_start = 0;

        // Find current priority end index.
        let mut cur_pri_end = 0;
        let needs_len = needs.len();

        let states = self.all_current_states();

        // Scan successive slices of items with the same priority.
        loop {
            // Find end of current slice.
            while cur_pri_end < needs_len && needs[cur_pri_end].priority() == cur_pri {
                cur_pri_end += 1;
            }
            // Process a priority slice.
            print!("Priority {cur_pri}");

            // Test distance check.
            // Find needs distance to the current state.
            let mut need_inx_vec = Vec::<(usize, usize)>::with_capacity(needs.len()); // (need index, distance)

            for inx in cur_pri_start..cur_pri_end {
                let dist = needs[inx].distance(&states);
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
                                needs[need_inx_vec[dist_inx].0].dom_id(),
                                needs[need_inx_vec[dist_inx].0].target(),
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
                    return (can_do, vec![]);
                }
            }

            if can_do.is_empty() {
                println!(", none.");
                if cur_pri_end == needs_len {
                    return (vec![], (0..needs.len()).collect());
                }

                cur_pri_start = cur_pri_end;
                cur_pri = needs[cur_pri_start].priority();
                continue;
            }

            if can_do.len() == 1 {
                println!(", found 1 need that can be done.");
            } else {
                println!(", found {} needs that can be done.", can_do.len());
            }

            return (can_do, vec![]);
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
    pub fn choose_a_need(&self, can_do: &[InxPlan], needs: &NeedStore) -> usize {
        //println!("choose_a_need: number InxPlans {}", can_do.len());
        assert!(!can_do.is_empty());

        // Gather plan rates.
        let mut rts = vec![];
        for inxpln in can_do.iter() {
            match &inxpln.plans {
                NeedPlan::AtTarget {} => rts.push(2000 - needs[inxpln.inx].priority() as isize), // change lower priority to higher rate.
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
            needs[can_do[inx].inx],
            match &can_do[inx].plans {
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
        //println!("domainstore::check_select:");
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

    /// Return a need for moving to a select region.
    fn select_goal_needs(&self, goal_regs: &RegionsCorr) -> Option<NeedStore> {
        //println!("domainstore::select_goal_needs:");
        debug_assert!(self.is_congruent(goal_regs));

        // Load return vector.
        let mut ret_str = NeedStore::new(vec![]);

        // Get current states.
        let all_states = self.all_current_states();

        for (inx, psupx) in self.select_net_positive.iter().enumerate() {
            if psupx.is_superset_of_states(&all_states) {
                continue;
            }
            let adjust = (self.max_pos_value - psupx.net_value) + self.times_visited[inx] as isize;
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

    /// Return a plan for a given start RegionsCorr, goal RegionsCorr, within a RegionsCorrStore.
    fn plan_using_least_negative_select_regions_get_plan(
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
                self.plan_using_least_negative_select_regions2(
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
            if selx.neg_value < min_val && selx.regions.is_superset_of(rcx) {
                min_val = selx.neg_value;
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
            //println!("domainstore::plan_using_least_negative_select_regions: return 1");
            return Ok(NeedPlan::AtTarget {});
        }

        // Make a list of negative values in self.select_negative.
        let mut neg_values = Vec::<isize>::new();
        for selx in self.select_negative.iter() {
            if !neg_values.contains(&selx.neg_value) {
                neg_values.push(selx.neg_value);
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
                if selx.neg_value == *nvx {
                    //println!("  adding neg value {nvx}");
                    fragments.push(&selx.regions);
                }
            }
        }

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
                    //println!("domainstore::plan_using_least_negative_select_regions: returns {planx} current_rate {current_rate}");
                    return Ok(NeedPlan::PlanFound { plan: planx });
                }
            }

            // Add more negative regioncorrs.
            //println!("   adding {} valued SRs", neg_values[neg_inx]);
            if neg_inx == neg_values.len() {
                break;
            }
            current_rate = neg_values[neg_inx];
            for selx in self.select_negative.iter() {
                if selx.neg_value == current_rate {
                    //println!("  found {selx}");
                    fragments.push(&selx.regions);
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
    fn plan_using_least_negative_select_regions2(
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
                    Ok(mut plans) => {
                        plans.set_rate(cur_rate);
                        //assert!(plans.is_valid());
                        let plncs = PlansCorrStore::new(vec![plans]);
                        //println!("1plan_using_least_negative_select_regions2: returning (1) {plncs}");
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
            Err(vec![format!("domainstore::plan_using_least_negative_select_regions2: mid_paths from {start_regs} to {goal_regs} within {} failed",
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
                            //println!("2domainstore::plan_using_least_negative_select_regions2: for {cur_regs} to {intx} plans found {plans}");
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
        let mut select_regions2: Vec<_> = select_regions.to_vec(); // clippy requires the argument to be a slice.

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

    /// Return a String representation of a DomainStore.
    fn formatted_str(&self) -> String {
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
            //println!("domainstore::make_plans: returning:");
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
        //println!("domainstore: make_plans2: from {from} goal {goal}");
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
                    Ok(mut plans) => {
                        //println!(" {} plans from get_plans", plans.len());
                        plans_per_target
                            .push(plans.remove(rand::thread_rng().gen_range(0..plans.len())));
                    }
                    Err(errvec) => return Err(errvec),
                };
            }
        } // next domain

        //println!("domainstore::make_plans2: returning {plans_per_target}");
        Ok(plans_per_target)
    }

    /// Try to satisfy a need.
    pub fn do_a_need(&mut self, needx: &SomeNeed, inx_pln: &InxPlan) -> Result<(), String> {
        let dom_id = self.current_domain;

        // Display Domain info, if needed.
        match needx {
            SomeNeed::ToSelectRegions { .. } => {
                //println!("\nNeed chosen: {} {}", ndx, plans.str_terse())
            }
            SomeNeed::ExitSelectRegions { .. } => {
                //println!("\nNeed chosen: {} {}", nd_inx, inx_pln.plans.str_terse())
            }
            _ => {
                let nd_dom = needx.dom_id().unwrap();
                if dom_id != nd_dom {
                    // Show "before" state before running need.
                    println!("\nAll domain states: {}", self.all_current_states());
                    self.change_domain(nd_dom);
                    self.print();
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
                        needx.dom_id(),
                        needx.target(),
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
                            needx.target()
                        ));
                    }
                }
            },
            NeedPlan::AtTarget {} => (),
        }

        // Take action after the desired state is reached.

        if needx.satisfied_by(&self.all_current_states()) {
            match needx {
                SomeNeed::ToSelectRegions { .. } => {
                    if self.set_boredom_limit() {
                        self.update_times_visited();
                    }
                }
                SomeNeed::ExitSelectRegions { .. } => (),
                _ => {
                    self.take_action_need(needx);
                }
            }
            Ok(())
        } else {
            Err("Need not satisfied".to_string())
        }
    } // end do_a_need

    /// Run cleanup for a domain and action.
    pub fn cleanup(&mut self, dom_id: usize, act_id: usize, needs: &NeedStore) {
        assert!(dom_id < self.len());
        self.items[dom_id].cleanup(act_id, needs);
    }

    /// Return true if corresponding items have the same number of bits.
    pub fn is_congruent(&self, other: &impl CorrespondingItems) -> bool {
        self.num_bits_vec() == other.num_bits_vec()
    }

    /// Return a vector of corresponding num_bits.
    pub fn num_bits_vec(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::with_capacity(self.len());
        for domx in self.iter() {
            ret_vec.push(domx.num_bits());
        }
        ret_vec
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeDomain> {
        self.items.iter()
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
    use crate::rulestore::RuleStore;
    use crate::sample::SomeSample;
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
    /// Test case where positive regions the start and goal are in, intersect.
    fn avoidance1x() -> Result<(), String> {
        // Init DomainStore. Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from_str("0000")?);
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);

        // Set select regions.
        dmxs.add_select(SelectRegions::from_str("SR[RC[r01X1], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[rX101], -2]")?);
        dmxs.calc_select();

        // Set state for domain 0.
        dmxs[0].set_cur_state(SomeState::from_str("s0001")?);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        dmxs[0].get_needs(); // set aggregate changes

        let start_region = RegionsCorr::from_str("RC[r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1111]")?;

        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
            Ok(ndpln) => {
                match ndpln {
                    NeedPlan::PlanFound { plan: planx } => {
                        println!(
                            "Plan found: {} start {start_region} goal {goal_region}",
                            planx
                        );
                        assert!(planx.rate() == 0);
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
        dmxs.add_domain(SomeState::from_str("0000")?);
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![], 5);
        domx.eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);

        // Set select regions.
        dmxs.add_select(SelectRegions::from_str("SR[RC[r0101], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[r1001], -1]")?);

        dmxs.calc_select();

        // Set state for domain 0.
        dmxs[0].set_cur_state(SomeState::from_str("s0001")?);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::from_str("RC[r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1101]")?;

        dmxs[0].get_needs(); // set aggregate changes

        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
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
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from_str("0000")?);
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![], 5);
        domx.eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![], 5);
        domx.eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);

        // Set select regions.
        dmxs.add_select(SelectRegions::from_str("SR[RC[r0x00], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[rx100], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[r01x1], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[r10x1], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[r101x], -1]")?);

        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::from_str("s0001")?;
        dmxs[0].set_cur_state(first_state.clone());

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::from_str("RC[r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1101]")?;

        dmxs[0].get_needs(); // set aggregate changes

        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
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
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from_str("s0000")?);
        let domx = &mut dmxs[0];

        // Set up action to change the first bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        domx.eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);

        // Set up action to change the second bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        domx.eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);

        // Set up action to change the third bit.
        domx.add_action(vec![], 5);

        domx.eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        domx.eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);

        // Set up action to change the fourth bit.
        domx.add_action(vec![], 5);
        domx.eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        domx.eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);

        // Set select regions.
        dmxs.add_select(SelectRegions::from_str("SR[RC[rxx0x], -1]")?);
        dmxs.calc_select();

        // Set state for domain 0.
        dmxs[0].set_cur_state(SomeState::from_str("s0001")?);

        println!("\nActions {}\n", dmxs[0].actions);
        println!("Select Regions: {}\n", dmxs.select);

        let start_region = RegionsCorr::from_str("RC[r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1101]")?;

        dmxs[0].get_needs(); // set aggregate changes

        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
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
        // Init DomainStore, Domains.
        // Domain 0 uses 1 integer for bits.
        // Domain 1 uses 2 integers for bits.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from_str("s0000_0000")?);
        dmxs.add_domain(SomeState::from_str("s0000_0000_0000_0000")?);

        // Set state for domain 0, using 1 integer for bits.
        let init_first_state = SomeState::from_str("s0001_0010")?;
        dmxs[0].set_cur_state(init_first_state.clone());

        // Set state for domain 1, using 2 integers for bits.
        let init_state2 = SomeState::from_str("s1010_1011_1100_1101")?;
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
        dmxs.add_domain(SomeState::from_str("s0000")?);

        // Add actions.
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);

        // Set select regions.

        // Set up dom 0 00XX dependent on dom 1 01XX.
        dmxs.add_select(SelectRegions::from_str("SR[RC[r1100], -1]")?);

        // Set up dom 0 00XX dependent on dom 1 10XX.
        dmxs.add_select(SelectRegions::from_str("SR[RC[r1011], -1]")?);
        dmxs.calc_select();

        // Init aggregate needs.
        dmxs.get_needs();

        let s0 = SomeState::from_str("s0000")?;
        dmxs[0].set_cur_state(s0.clone());

        let start_region = RegionsCorr::from_str("RC[r0000]")?;
        let goal_region = RegionsCorr::from_str("RC[r1101]")?;

        // Try making plans.
        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
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
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from_str("s0000")?);
        dmxs.add_domain(SomeState::from_str("s0000")?);

        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);

        // Set up dom 0 negative regions.
        dmxs.add_select(SelectRegions::from_str("SR[RC[r01x1, rxxxx], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[rx101, rxxxx], -1]")?);

        // Set up dom 1 negative regions.
        dmxs.add_select(SelectRegions::from_str("SR[RC[rxxxx, r011x], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[rxxxx, rx111], -1]")?);

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s0 = SomeState::from_str("s0000")?;
        dmxs[0].set_cur_state(s0.clone());

        let s1 = SomeState::from_str("s0001")?;
        dmxs[1].set_cur_state(s1.clone());

        let start_region = RegionsCorr::from_str("RC[r0000, r0001]")?;
        let goal_region = RegionsCorr::from_str("RC[r1111, r1110]")?;

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        match dmxs.plan_using_least_negative_select_regions(&start_region, &goal_region) {
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
        // Init DomainStore, Domain.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from_str("s0000")?);
        dmxs.add_domain(SomeState::from_str("s0000")?);

        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);

        // Set up negative regions.
        dmxs.add_select(SelectRegions::from_str("SR[RC[r00xx, rxx11], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[r11xx, r01xx], -1]")?);

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = SomeState::from_str("s0101")?;
        dmxs[0].set_cur_state(s5.clone());

        let s7 = SomeState::from_str("s0111")?;
        dmxs[1].set_cur_state(s7.clone());

        let start_regions = RegionsCorr::from_str("RC[r0101, r0111]")?;

        let goal_regions = RegionsCorr::from_str("RC[r1001, r0111]")?;

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        match dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.rate() == 0);
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
        dmxs.add_domain(SomeState::from_str("s0000")?);
        dmxs.add_domain(SomeState::from_str("s0000")?);

        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);
        dmxs[1].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        dmxs[1].eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        dmxs[1].eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        dmxs[1].eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        dmxs[1].eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);

        // Set up negative regions.
        dmxs.add_select(SelectRegions::from_str("SR[RC[r000x, rxx11], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[r11x1, r01xx], -1]")?);

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s5 = SomeState::from_str("s0101")?;
        dmxs[0].set_cur_state(s5.clone());

        let s7 = SomeState::from_str("s0111")?;
        dmxs[1].set_cur_state(s7.clone());

        let start_regions = RegionsCorr::from_str("RC[r0101, r0111]")?;

        let goal_regions = RegionsCorr::from_str("RC[r1001, r0111]")?;

        println!("\nActions {}\n", dmxs[0].actions);
        println!("\nActions {}\n", dmxs[1].actions);

        // Try making plans.
        match dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(ndpln) => match ndpln {
                NeedPlan::PlanFound { plan: plans } => {
                    println!("Plans {}", plans);
                    //assert!(1 == 2);
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
        dmxs.add_domain(SomeState::from_str("s0000_0000")?);
        dmxs.add_domain(SomeState::from_str("s0000_0000_0000_0000")?);

        // Add action to domain 0.
        dmxs[0].add_action(vec![], 5);

        // Add action to domain 1.
        dmxs[1].add_action(vec![], 5);

        // Load select regions.
        dmxs.add_select(SelectRegions::from_str(
            "SR[RC[r00000x0x, rXXXXXX10_1XXX_XXXX], 1]",
        )?);
        dmxs.add_select(SelectRegions::from_str(
            "SR[RC[r00000xx1, rXXXXXX10_1XXX_XXXX], 1]",
        )?);
        dmxs.add_select(SelectRegions::from_str(
            "SR[RC[r0000x1x1, rXXXXXX10_1XXX_XXXX], 1]",
        )?);
        dmxs.add_select(SelectRegions::from_str(
            "SR[RC[r00001110, rXXXXXX10_1XXX_XXXX], 1]",
        )?);
        dmxs.calc_select();

        // Set state for domain 0.
        let first_state = SomeState::from_str("s0001_0010")?;
        dmxs[0].set_cur_state(first_state.clone());

        // Set state for domain 1.
        let state2 = SomeState::from_str("s1010_1011_1100_1101")?;
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
        let first_state = SomeState::from_str("s0000_0101")?;
        dmxs[0].set_cur_state(first_state.clone());

        // Set state for domain 1.
        let state2 = SomeState::from_str("s1010_0010_1000_1101")?;
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
        dmxs.add_domain(SomeState::from_str("s0000")?);

        let regstr1 = RegionsCorr::from_str("RC[rX1XX]")?;

        // Add selectregions.
        dmxs.add_select(SelectRegions::new(regstr1.clone(), -1));

        let regstr2 = RegionsCorr::from_str("RC[r1XX1]")?;

        // Add select regionstores.
        dmxs.add_select(SelectRegions::new(regstr2.clone(), -1));

        // Set state for domain 0, using 1 integer for bits.
        let first_state = SomeState::from_str("s1101")?;
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
        dmxs.add_domain(SomeState::from_str("s0000")?);

        // Add action to domain 0.
        dmxs[0].add_action(vec![], 5);

        // Load select regions
        dmxs.add_select(SelectRegions::from_str("SR[RC[rxx0x], 4]")?);
        dmxs.add_select(SelectRegions::from_str("SR[RC[r00x1], -2]")?);
        dmxs.add_select(SelectRegions::from_str("SR[RC[r11x1], -4]")?);
        dmxs.add_select(SelectRegions::from_str("SR[RC[r10x0], -5]")?);
        dmxs.add_select(SelectRegions::from_str("SR[RC[rX111], -1]")?);
        dmxs.calc_select();

        assert!(dmxs.select_net_positive.len() == 5);
        assert!(dmxs
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[rx100], 4]")?));
        assert!(dmxs
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[r0x00], 4]")?));
        assert!(dmxs
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[r010x], 4]")?));
        assert!(dmxs
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[r1001], 4]")?));
        assert!(dmxs
            .select_net_positive
            .contains(&SelectRegions::from_str("SR[RC[r0001], 2]")?));

        assert!(dmxs.rc_non_negative.len() == 4);
        assert!(dmxs
            .rc_non_negative
            .contains(&RegionsCorr::from_str("RC[rx1x0]")?));
        assert!(dmxs
            .rc_non_negative
            .contains(&RegionsCorr::from_str("RC[r0xx0]")?));
        assert!(dmxs
            .rc_non_negative
            .contains(&RegionsCorr::from_str("RC[r010x]")?));
        assert!(dmxs
            .rc_non_negative
            .contains(&RegionsCorr::from_str("RC[r10x1]")?));

        assert!(dmxs.select_negative.len() == 5);
        assert!(dmxs
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r00X1], -2]")?));
        assert!(dmxs
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r1101], -4]")?));
        assert!(dmxs
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r10X0], -5]")?));
        assert!(dmxs
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r0111], -1]")?));
        assert!(dmxs
            .select_negative
            .contains(&SelectRegions::from_str("SR[RC[r1111], -5]")?));

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    /// Test case using two domains, like avoidance8, but a way around traps.
    fn avoidance10() -> Result<(), String> {
        // Init DomainStore, Domains.
        let mut dmxs = DomainStore::new();
        dmxs.add_domain(SomeState::from_str("s0000")?);

        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);
        dmxs[0].add_action(vec![], 5);

        // Set up action to change the first bit.
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s0000->s0001")?);
        dmxs[0].eval_sample_arbitrary(0, &SomeSample::from_str("s1111->s1110")?);

        // Set up action to change the second bit.
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s0000->s0010")?);
        dmxs[0].eval_sample_arbitrary(1, &SomeSample::from_str("s1111->s1101")?);

        // Set up action to change the third bit.
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s0000->s0100")?);
        dmxs[0].eval_sample_arbitrary(2, &SomeSample::from_str("s1111->s1011")?);

        // Set up action to change the fourth bit.
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s0000->s1000")?);
        dmxs[0].eval_sample_arbitrary(3, &SomeSample::from_str("s1111->s0111")?);

        // Set up negative select regions.
        dmxs.add_select(SelectRegions::from_str("SR[RC[r01xx], -1]")?);

        dmxs.add_select(SelectRegions::from_str("SR[RC[r10xx], -2]")?);

        // Calc non-negative RegionSores.
        dmxs.calc_select();

        let s3 = SomeState::from_str("s0011")?;
        dmxs[0].set_cur_state(s3.clone());

        let start_regions = RegionsCorr::from_str("RC[r0011]")?;

        let goal_regions = RegionsCorr::from_str("RC[r1111]")?;

        println!("\nActions {}\n", dmxs[0].actions);

        // Try making plans.
        // Bridge overlap, using 01XX, between 3 and F, at 7, strangely
        // makes a step within 01XX unneeded.
        match dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.rate() == -1);
                assert!(plans.len() == 2);
                //assert!(1 == 2);
                //Ok(())
            }
            _ => return Err(format!("No plan found?")),
        }

        let start_regions = RegionsCorr::from_str("RC[r0011]")?;

        let goal_regions = RegionsCorr::from_str("RC[r1101]")?;

        println!("\nActions {}\n", dmxs[0].actions);

        // Try making plans.
        // No bridge overlap here, using 01XX, between 3 and D, requiring a step from
        // 7 to 5, within 01XX.
        match dmxs.plan_using_least_negative_select_regions(&start_regions, &goal_regions) {
            Ok(NeedPlan::PlanFound { plan: plans }) => {
                println!("Plans {}", plans);
                assert!(plans.rate() == -1);
                assert!(plans.len() == 2 || plans.len() == 3);
                //assert!(1 == 2);
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
        dmxs.add_domain(SomeState::from_str("s0000")?);
        dmxs.add_domain(SomeState::from_str("s000")?);
        dmxs.add_domain(SomeState::from_str("s00")?);

        // Add an action for each domain.
        dmxs[0].add_action(vec![RuleStore::from_str("[XX/XX/XX/Xx]")?], 5);
        dmxs[1].add_action(vec![RuleStore::from_str("[XX/XX/Xx]")?], 5);
        dmxs[2].add_action(vec![RuleStore::from_str("[XX/Xx]")?], 5);

        // Set domain starting states.
        dmxs[0].set_cur_state(SomeState::from_str("s0000")?);
        dmxs[1].set_cur_state(SomeState::from_str("s001")?);
        dmxs[2].set_cur_state(SomeState::from_str("s10")?);

        // Set up PlansCorr.
        let plnsc1 =
            PlansCorr::from_str("PC[[P[r0000-0->r0001], P[r001-no->r001], P[r10-0->r11]], 0]")?;
        println!("{plnsc1}");

        let before = dmxs.all_current_states();
        println!("Before: {before}");

        match dmxs.run_planscorr(&plnsc1) {
            Err(errstr) => return Err(errstr),
            _ => (),
        }

        let after = dmxs.all_current_states();
        println!("After:  {after}");

        assert!(after[0] == SomeState::from_str("s0001")?);
        assert!(after[1] == SomeState::from_str("s001")?);
        assert!(after[2] == SomeState::from_str("s11")?);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    /// Test running a PlansCorrStore.
    fn run_planscorrstore() -> Result<(), String> {
        // Init DomainStore.
        let mut dmxs = DomainStore::new();

        // Add domain 0.
        dmxs.add_domain(SomeState::from_str("0000")?);

        // Add domain 0 actions.
        dmxs[0].add_action(vec![RuleStore::from_str("[XX/XX/XX/Xx]")?], 5);
        dmxs[0].add_action(vec![RuleStore::from_str("[XX/XX/Xx/XX]")?], 5);
        dmxs[0].add_action(vec![RuleStore::from_str("[XX/Xx/XX/XX]")?], 5);
        dmxs[0].add_action(vec![RuleStore::from_str("[Xx/XX/XX/XX]")?], 5);

        // Set domain 0 starting state.
        dmxs[0].set_cur_state(SomeState::from_str("s0000")?);

        // Add domain 1.
        dmxs.add_domain(SomeState::from_str("s0000")?);

        // Add domain 1 actions.
        dmxs[1].add_action(vec![RuleStore::from_str("[XX/XX/XX/Xx]")?], 5);
        dmxs[1].add_action(vec![RuleStore::from_str("[XX/XX/Xx/XX]")?], 5);
        dmxs[1].add_action(vec![RuleStore::from_str("[XX/Xx/XX/XX]")?], 5);
        dmxs[1].add_action(vec![RuleStore::from_str("[Xx/XX/XX/XX]")?], 5);

        // Set domain 1 starting state.
        dmxs[1].set_cur_state(SomeState::from_str("s1111")?);

        // Set up PlansCorrStore.
        let plnscrstr = PlansCorrStore::from_str("PCS[PC[[P[r0000-2->r0100], P[r1111-0->r1110]], 0], PC[[P[r0100-0->r0101], P[r1110-1->r1100]], 0]]")?;
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

        assert!(after[0] == SomeState::from_str("s0101")?);
        assert!(after[1] == SomeState::from_str("s1100")?);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    /// Test run_planscorrstore, where start and goal are not subsets of any select regions.
    fn get_path_through_select_regions() -> Result<(), String> {
        // Init DomainStore.
        let dmxs = DomainStore::new();

        let start_regs = RegionsCorr::from_str("RC[r1X11]")?;
        let goal_regs = RegionsCorr::from_str("RC[rX000]")?;

        let within1 = RegionsCorr::from_str("RC[r0XXX]")?;
        let within2 = RegionsCorr::from_str("RC[rX1XX]")?;

        let path = dmxs.get_path_through_select_regions(
            &start_regs,
            &goal_regs,
            &vec![&within1, &within2],
        );
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
        //assert!(1 == 2);
        Ok(())
    }
}
