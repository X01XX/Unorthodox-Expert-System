//! The DomainStore struct, a vector of SomeDomain structs.

/// The highest number of needs to seek a plan for, in parallel.
const SPAN: usize = 6;

use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::optimalregionsstore::OptimalRegionsStore;
use crate::plan::SomePlan;
use crate::planstore::PlanStore;
use crate::regionstore::RegionStore;
use crate::state::{self, SomeState};
use crate::targetstore::TargetStore;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing
use std::mem;
use std::ops::{Index, IndexMut};

use crate::randompick;

use rayon::prelude::*;

impl fmt::Display for DomainStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::from("[");

        for mskx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            let _ = write!(rc_str, "{}", &mskx);
            flg = 1;
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
}

#[readonly::make]
#[derive(Serialize, Deserialize, Default)]
pub struct DomainStore {
    /// Vector of SomeDomain structs.
    pub avec: Vec<SomeDomain>,
    /// Domain displayed to user.
    pub current_domain: usize,
    /// A counter to indicate the number of steps the current state is in the same optimal region.
    pub boredom: usize,
    /// A limit for becomming bored, then moving to another optimal state.
    pub boredom_limit: usize,
    /// Zero, or more, optimal regions that are sought if there are no needs.
    /// This may be changed from the UI, see the help display for the commands "oa" and "od".
    /// If more than one region, boredom may cause the program to run rules to switch to a different region.
    pub optimal: OptimalRegionsStore,
    /// RegionStore to add all possible intersections of the optimal states to discern.
    pub optimal_and_ints: OptimalRegionsStore,
    /// Save the results of the last run of get_needs.
    pub needs: NeedStore,
    /// Vector of InxPlans for selected needs, where a plan was calculated.
    pub can_do: Vec<InxPlan>,
    /// Vector of InxPlans for selected needs, where a plan could not be calculated.
    pub cant_do: Vec<InxPlan>,
    /// The current step number.
    pub step_num: usize,
}

impl DomainStore {
    /// Return a new, empty, DomainStore struct.
    pub fn new(mut avec: Vec<SomeDomain>) -> Self {
        for (inx, domx) in avec.iter_mut().enumerate() {
            domx.num = inx;
        }
        Self {
            avec,
            current_domain: 0,
            boredom: 0,
            boredom_limit: 0,
            optimal_and_ints: OptimalRegionsStore::new(vec![]),
            optimal: OptimalRegionsStore::new(vec![]),
            needs: NeedStore::new(vec![]),
            can_do: Vec::<InxPlan>::new(),
            cant_do: Vec::<InxPlan>::new(),
            step_num: 0,
        }
    }

    /// Add an optimal region.
    /// One region for each domain.
    /// The logical "and" of each domain region given.
    pub fn add_optimal(&mut self, regstr: RegionStore) {
        debug_assert!(regstr.len() == self.avec.len());

        if self.optimal.any_supersets_of(&regstr) {
            println!("Superset optimal regions found");
            return;
        }

        for (inx, dmx) in self.avec.iter().enumerate() {
            if regstr[inx].state1.num_ints() != dmx.cur_state.num_ints() {
                panic!("reg {} bad number ints for domain {}", regstr[inx], inx);
            }
        }

        self.optimal.push(regstr);

        if self.optimal.len() == 1 {
            self.optimal_and_ints = self.optimal.clone();
        } else {
            self.optimal_and_ints = self.optimal.and_intersections();
        }
    }

    /// Add a Domain struct to the store.
    /// Add optimal regions after the last domain has been added.
    pub fn push(&mut self, mut domx: SomeDomain) -> usize {
        debug_assert!(self.optimal.is_empty());

        let dom_num = self.avec.len();

        domx.num = dom_num;

        self.avec.push(domx);

        dom_num
    }

    /// Get needs for each Domain.
    /// Run in parallel per Domain.
    /// Each Domain uses parallel processing to get needs for each Action.
    /// Return selected plans.
    pub fn get_needs(&mut self) {
        // Inc step number.
        self.step_num += 1;

        // Get all needs.
        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // .par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|domx| domx.get_needs())
            .filter(|ndsx| ndsx.is_not_empty())
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::with_capacity(vecx.iter().map(|ndsx| ndsx.len()).sum());

        for nst in vecx.iter_mut() {
            nds_agg.append(nst);
        }

        // Get optimal region needs.
        if let Some(needx) = self.check_optimal() {
            nds_agg.push(needx);
        }

        //println!("number needs = {}", nds_agg.len());
        self.needs = nds_agg;

        self.evaluate_needs();
    }

    /// Run a vector of plans.
    pub fn run_plans(&mut self, plans: &PlanStore) -> bool {
        assert!(plans.is_not_empty());

        // Run a non-empty plan for one domain.
        if plans.len() == 1 {
            if plans[0].is_not_empty() && !self.run_plan(&plans[0]) {
                return false;
            }
            return true;
        }

        if plans.len() == self.len() {
            // Run plans in parallel for achieving a state in an optimal region, when the number of domains is GT 1.
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
        }

        false
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

    /// Set needs, can_do and cant_do for the DomainStore.
    /// Select up to a given number (SPAN) of the lowest value priority needs,
    /// return a span of needs when one, or more, needs can be planned.
    /// Each InxPlan will contain an index to the NeedStore, and a PlanStore.
    pub fn evaluate_needs(&mut self) {
        //println!("evaluate_needs: {} needs", self.needs.len());
        let mut last_priority = 0;

        // Get optimal region info.
        let in_optimal = self
            .optimal
            .any_supersets_of_states(&self.all_current_states());

        let optimal_priority = SomeNeed::ToOptimalRegion {
            target_regions: RegionStore::new(vec![]),
        }
        .priority();

        //println!("evaluate_needs: in_optimal {in_optimal}, optimal_priority {optimal_priority}");

        loop {
            // find next lowest priority number (highest priority)needs
            let mut avec = Vec::<usize>::new();
            let mut least_priority = usize::MAX;

            for ndsx in self.needs.iter() {
                let pri = ndsx.priority();
                if pri > last_priority && pri < least_priority {
                    least_priority = pri;
                }
            }

            //println!("least priority = {}", least_priority);

            // If in optimal region, and priority not LE optimal priority, stop evaluating needs.
            if in_optimal && least_priority > optimal_priority {
                self.can_do = Vec::<InxPlan>::new();
                self.cant_do = Vec::<InxPlan>::new();
                return;
            }

            // No plans found for any need, or no needs.
            if least_priority == usize::MAX {
                self.can_do = Vec::<InxPlan>::new();
                self.cant_do = Vec::<InxPlan>::new();
                return;
            }

            // Load avec with indices to needs of the current priority.
            for (inx, ndsx) in self.needs.iter().enumerate() {
                if ndsx.priority() == least_priority {
                    avec.push(inx);
                }
            }
            //println!("evaluate_needs: number {least_priority} found is {}", avec.len());

            // Scan needs to see what can be achieved with a plan.
            // Run make_plans in parallel for selected needs.
            //
            // To avoid the cycles required to make plans for many needs,
            //   Process needs by groups of the same, decreasing priority (increasing priority number),
            //   until len() == 0at least one has a plan.
            //

            // Randomly pick up to SPAN needs at a time, from the current priority.
            // The length of rp1 goes down as numbers are chosen.
            let mut rp1 = randompick::RandomPick::new(avec.len()); // put numbers 0..avec.len() into a vector.

            while !rp1.is_empty() {
                let mut end = SPAN;

                if end > rp1.len() {
                    end = rp1.len();
                }

                let mut avec2 = Vec::<usize>::with_capacity(end);
                for _inx in 0..end {
                    avec2.push(avec[rp1.pick().unwrap()]);
                }

                let mut ndsinx_plan = avec2
                    .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
                    .map(|nd_inx| (nd_inx, self.make_plans(&self.needs[*nd_inx].target())))
                    .map(|plnstr| InxPlan {
                        inx: *plnstr.0,
                        plans: plnstr.1,
                    })
                    .collect::<Vec<InxPlan>>();

                let mut can_do = Vec::<InxPlan>::new();
                let mut cant_do = Vec::<InxPlan>::new();

                // If at least one plan found, return vector of InxPlan structs.
                for ndsinx in ndsinx_plan.iter_mut() {
                    if ndsinx.plans.is_some() {
                        can_do.push(mem::take(ndsinx));
                    } else {
                        cant_do.push(mem::take(ndsinx));
                    }
                }

                //println!("evaluate_needs: can_do {} cant_do {}", can_do.len(), cant_do.len());
                if !can_do.is_empty() {
                    //println!("evaluate_needs: RETURNING can_do {} cant_do {}", can_do.len(), cant_do.len());
                    self.can_do = can_do;
                    self.cant_do = cant_do;
                    return;
                }
            } // end while

            // Increase the lower bound of the next, least, priority number.
            last_priority = least_priority;
        } // end loop
          // Unreachable, since there is no break command.
    } // end evaluate_needs

    /// Return an Option PlanStore, to go from the current state to the region or each target.
    /// Return None if any one of the targets cannot be satisfied.
    pub fn make_plans(&self, targets: &TargetStore) -> Option<PlanStore> {
        let mut plans = Vec::<SomePlan>::new();

        for targx in targets.iter() {
            if let Some(planx) = self.avec[targx.dom_num].make_plan(&targx.region) {
                plans.push(planx);
            }
        }
        if plans.is_empty() || plans.len() < targets.len() {
            return None;
        }
        Some(PlanStore::new(plans))
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

        // Make selection of min_len plans.
        let mut min_len_inxplans = Vec::<usize>::new();

        // Find the shortest plan length
        let mut min_plan_len = std::usize::MAX;
        for inx_planx in self.can_do.iter() {
            if let Some(plans) = &inx_planx.plans {
                if plans.len() < min_plan_len {
                    min_plan_len = plans.len();
                }
            }
        }

        // Push index to shortest plan needs
        for (inx, inx_planx) in self.can_do.iter().enumerate() {
            if let Some(plans) = &inx_planx.plans {
                if plans.len() == min_plan_len {
                    min_len_inxplans.push(inx);
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
        // Get the optimal regions the current state is in.
        let all_states = self.all_current_states();
        let num_sups = self.optimal.number_supersets_of_states(&all_states);
        self.boredom_limit = 3 * num_sups;
        self.boredom = 0;
    }

    /// Do functions related to the wish to be in an optimum region.
    /// Increment the boredom duration, if needed.
    /// Return a need to move to another optimal region, if needed.
    pub fn check_optimal(&mut self) -> Option<SomeNeed> {
        // Check if there are no optimal regions.
        if self.optimal.is_empty() {
            return None;
        }

        // Get all domain states vector.
        // Calling self.all_current_states runs into problems with the combiler.
        let mut all_states = Vec::<&SomeState>::with_capacity(self.len());
        for domx in self.avec.iter() {
            all_states.push(domx.get_current_state());
        }

        // Check current status within an optimal region, or not.
        if self.optimal.any_supersets_of_states(&all_states) {
            self.boredom += 1;
            if self.boredom <= self.boredom_limit {
                return None;
            }
        } else {
            self.boredom = 0;
            self.boredom_limit = 0;
        }

        // Get regions the current state is not in.
        let notsups = self.optimal_and_ints.not_supersets_of_states(&all_states);

        // If the current state is not in at least one optimal region,
        // return a need to move to an optimal region.
        if notsups.is_not_empty() {
            let inx = rand::thread_rng().gen_range(0..notsups.len());
            return Some(SomeNeed::ToOptimalRegion {
                target_regions: notsups[inx].clone(),
            });
        }

        None
    }

    /// Print current states and optimal information.
    /// Return true if the current states are in an optimal region.
    pub fn print_optimal(&self) -> bool {
        let mut ret = false;

        let all_states = self.all_current_states();
        let optimal_supersets = self.optimal.supersets_of_states(&all_states);
        if optimal_supersets.is_empty() {
            print!(
                "\nAll Current states: {} in optimal regions: None, not in ",
                state::somestate_ref_vec_string(&all_states)
            );
            for optx in self.optimal.iter() {
                print!(" {optx}");
            }
        } else {
            ret = true;
            print!(
                "\nAll Current states: {} in optimal regions: ",
                state::somestate_ref_vec_string(&all_states)
            );
            for optx in optimal_supersets.iter() {
                print!(" {optx}");
            }
            print!(", not in: ");
            let optimal_not_supersets = self.optimal.not_supersets_of_states(&all_states);
            for optx in optimal_not_supersets.iter() {
                print!(" {optx}");
            }
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

        println!("\nDom: {dom_num} Current State: {cur_state}");
    }

    /// Print needs that can be done.
    pub fn print_can_do(&self) {
        if self.can_do.is_empty() {
            println!("\nNeeds that can be done: None");
            self.print_optimal();
        } else {
            println!("\nNeeds that can be done:");

            for (inx, ndplnx) in self.can_do.iter().enumerate() {
                println!(
                    "{:2} {} {}",
                    inx,
                    &self.needs[ndplnx.inx],
                    ndplnx.plans.as_ref().unwrap().str_terse()
                );
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

    #[test]
    fn all_current_states() -> Result<(), String> {
        // Init a DomainStore.
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1), SomeDomain::new(2)]);

        // Ste state for domain 0, using 1 integer for bits.
        let init_state1 = dmxs[0].state_from_string("s0x12")?;
        dmxs[0].set_state(&init_state1);

        // Set state for domain 1, using 2 integers for bits.
        let init_state2 = dmxs[1].state_from_string("s0xabcd")?;
        dmxs[1].set_state(&init_state2);

        let all_states = dmxs.all_current_states();
        println!(
            "all states {}",
            state::somestate_ref_vec_string(&all_states)
        );

        if all_states.len() != 2 {
            return Err(format!("Invalid length {}", all_states.len()));
        }

        if *all_states[0] != init_state1 {
            return Err(format!("Invalid first state {}", all_states[0]));
        }

        if *all_states[1] != init_state2 {
            return Err(format!("Invalid second state {}", all_states[1]));
        }

        //assert!(1 == 2);

        Ok(())
    }

    #[test]
    fn check_optimal() -> Result<(), String> {
        // Start a DomainStore
        let mut dmxs = DomainStore::new(vec![SomeDomain::new(1), SomeDomain::new(2)]);

        // Add action to domain 0.
        dmxs[0].add_action();

        // Add action to domain 1.
        dmxs[1].add_action();

        // Load optimal regions
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

        // Add optimal region stores.
        dmxs.add_optimal(regstr1);
        dmxs.add_optimal(regstr2);
        dmxs.add_optimal(regstr3);
        dmxs.add_optimal(regstr4);

        println!("Optimal and ints:");
        for regstrx in dmxs.optimal_and_ints.iter() {
            println!("regstrx: {}", regstrx);
        }

        let all_states = dmxs.all_current_states();
        println!(
            "\nCurr st: {}",
            state::somestate_ref_vec_string(&all_states)
        );

        println!(
            "\nNumber supersets: {}",
            dmxs.optimal.number_supersets_of_states(&all_states)
        );

        if let Some(needx) = dmxs.check_optimal() {
            println!("\nCheck_optimal returns {}", needx);
        } else {
            println!("\nCheck_otimal returns None");
        }

        println!(
            "\nBoredom level {} Boredom_limit {}",
            dmxs.boredom, dmxs.boredom_limit
        );

        println!(" ");

        //return Err("done".to_string());
        Ok(())
    }
}
