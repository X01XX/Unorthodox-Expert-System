/*
 * domainstore.rs
 *
 * Copyright 2021 Bitflogger <earl.dukerschein@wisc.edu>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 *
 */

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
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::targetstore::TargetStore;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing
use std::ops::{Index, IndexMut};

use crate::randompick::RandomPick;

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

        write!(f, "{}", rc_str)
    }
}

/// An InxPlan struct, containing an index to a SomeNeed vector, and a SomePlan struct.
///
/// A Vec<T> is needed as a collector for parallel processing of needs to find plans,
/// but a tuple, or array, does not qualify as a "T".
///
/// An integer is used instead of &SomeNeed to avoid borrow checker problems.
#[readonly::make]
#[derive(Debug)]
pub struct InxPlan {
    /// Index to a need in a NeedStore.
    pub inx: usize,
    /// Plan to satisfy need (may be empty if the current state satisfies the need), or None.
    pub plans: PlanStore,
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct DomainStore {
    /// Vector of SomeDomain structs.
    pub avec: Vec<SomeDomain>,
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
}

impl Default for DomainStore {
    fn default() -> Self {
        Self::new()
    }
}

impl DomainStore {
    /// Return a new, empty, DomainStore struct.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeDomain>::new(),
            boredom: 0,
            boredom_limit: 0,
            optimal_and_ints: OptimalRegionsStore::new(),
            optimal: OptimalRegionsStore::new(),
        }
    }

    /// Add an optimal region.
    /// One region for each domain.
    /// The logical "and" of each domain region given.
    pub fn add_optimal(&mut self, regstr: RegionStore) {
        assert!(regstr.len() == self.avec.len());

        if self.optimal.any_supersets_of(&regstr) {
            println!("Superset optimal regions found");
            return;
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
    pub fn push(&mut self, domx: SomeDomain) {
        assert!(self.optimal.is_empty());

        self.avec.push(domx);
    }

    /// Get needs for each Domain.
    /// Run in parallel per Domain.
    /// Each Domain uses parallel processing to get needs for each Action.
    pub fn get_needs(&mut self) -> NeedStore {

        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // .par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|domx| domx.get_needs())
            .filter(|ndsx| !ndsx.is_empty())
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::new_with_capacity(vecx.iter().map(|ndsx| ndsx.len()).sum());

        for nst in vecx.iter_mut() {
            nds_agg.append(nst);
        }

        // Get optimal region needs.
        if let Some(needx) = self.check_optimal() {
            nds_agg.push(needx);
        }

        nds_agg
    }

    /// Run a vector of plans.
    pub fn run_plans(&mut self, plans: &PlanStore) -> bool {
        assert!(!plans.is_empty());

        // Run a non-empty plan for one domain.
        if plans.len() == 1 {
            if !plans[0].is_empty() && !self.run_plan(&plans[0]) {
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
                == plans.len()  // Does the number of true returns equal the number of plans run?
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
    pub fn take_action_need(&mut self, dmxi: usize, ndx: &SomeNeed) {
        self.avec[dmxi].take_action_need(ndx);
    }

    /// Take an action for a step
    pub fn take_action_step(&mut self, dmxi: usize, actx: usize) {
        self.avec[dmxi].take_action_step(actx);
    }

    /// Take an arbitrary action
    pub fn take_action_arbitrary(&mut self, dmxi: usize, actx: usize) {
        self.avec[dmxi].take_action_arbitrary(actx);
    }

    /// Return the current state of a given Domain index
    pub fn cur_state(&self, dmxi: usize) -> SomeState {
        self.avec[dmxi].get_current_state()
    }

    /// Return a vector of InxPlan structs, given a NeedStore.
    /// Select up to a given number (SPAN) of the lowest value priority needs,
    /// return a span of needs when one, or more, needs can be planned.
    /// Each InxPlan will contain an index to the NeedStore, and a PlanStore.
    pub fn evaluate_needs(&self, nds: &NeedStore) -> Vec<InxPlan> {
        //println!("domainstore::evaluate_needs");
        let mut last_priority = 0;

        // Get optimal region info.
        let in_optimal = self
            .optimal
            .any_supersets_of_states(&self.all_current_states());
        let optimal_priority = SomeNeed::ToOptimalRegion {
            target_regions: RegionStore::new(),
        }
        .priority();

        loop {
            // find next lowest priority number (highest priority)needs
            let mut avec = Vec::<usize>::new();
            let mut least_priority = usize::MAX;

            for ndsx in nds.iter() {
                let pri = ndsx.priority();
                if pri > last_priority && pri < least_priority {
                    least_priority = pri;
                }
            }

            //println!("least priority = {}", least_priority);

            // If in optimal region, and priority not LE optimal priority, stop evaluating needs.
            if in_optimal && least_priority > optimal_priority {
                return Vec::<InxPlan>::new();
            }

            // No plans found for any need, or no needs.
            if least_priority == usize::MAX {
                return Vec::<InxPlan>::new();
            }

            // Load avec with indicies to needs of the current priority.
            for (inx, ndsx) in nds.iter().enumerate() {
                if ndsx.priority() == least_priority {
                    avec.push(inx);
                }
            }

            // Scan needs to see what can be achieved with a plan.
            // Run make_plans in parallel for selected needs.
            //
            // To avoid the cycles required to make plans for many needs,
            //   Process needs by groups of the same, decreasing priority (increasing priority number),
            //   until at least one has a plan.
            //

            // Randomly pick up to SPAM needs at a time, from the current priority.
            // The length of rp1 goes down as numbers are chosen.
            let mut rp1 = RandomPick::new(avec.len()); // put numbers 0..avec.len() into a vector.

            while !rp1.is_empty() {
                let mut end = SPAN;

                if end > rp1.len() {
                    end = rp1.len();
                }

                let mut avec2 = Vec::<usize>::with_capacity(end);
                for _inx in 0..end {
                    avec2.push(avec[rp1.pick().unwrap()]);
                }

                let ndsinx_plan = avec2
                    .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
                    .map(|nd_inx| (nd_inx, self.make_plans(&nds[*nd_inx].target())))
                    .filter(|plnstr| plnstr.1.is_some())
                    .map(|plnstr| InxPlan {
                        inx: *plnstr.0,
                        plans: plnstr.1.unwrap(),
                    })
                    .collect::<Vec<InxPlan>>();

                // If at least one plan found, return vector of InxPlan structs.
                if !ndsinx_plan.is_empty() {
                    return ndsinx_plan;
                }
            } // end while

            // Increase the lower bound of the next, least, priority number.
            last_priority = least_priority;
        } // end loop
          // Unreachable, since there is no break command.
    } // end evaluate_needs

    // Return an Option PlanStore, to go from the current state to the region or each target.
    // Return None if any one of the targets cannot be satisfied.
    pub fn make_plans(&self, targets: &TargetStore) -> Option<PlanStore> {
        let mut plans = PlanStore::new();

        for targx in targets.iter() {
            if let Some(planx) = self.avec[targx.dom_num].make_plan(&targx.region) {
                plans.push(planx);
            }
        }
        if plans.is_empty() || plans.len() < targets.len() {
            return None;
        }
        Some(plans)
    }

    /// Choose a need, given a vector of needs,
    /// a vector of InxPlans Vec::<{ inx: need vector index, pln: Some(plan}>
    /// at least one do
    ///
    /// Sort needs by priority.
    ///
    /// Scan needs, by priority, to see what need can be satisfied by a plan.
    ///
    /// Return an index to the ndsinx_plan_all vector.
    pub fn choose_need(
        &self,
        nds: &NeedStore,
        ndsinx_plan_all: &[InxPlan],
        need_can: &Vec<usize>, // indicies to ndsinx_plan_all
    ) -> usize {
        assert!(!need_can.is_empty());

        // Find highest priority needs
        let mut min_pri = std::usize::MAX;

        let mut cnt = 0; // Count the number of high-priority needs
        for npa_inx in need_can {
            let nds_inx = ndsinx_plan_all[*npa_inx].inx;

            let pri = nds[nds_inx].priority();

            if pri < min_pri {
                min_pri = pri;
                cnt = 0;
            }
            if pri == min_pri {
                cnt += 1;
            }
        }

        // Gather highest priority needs

        // Make vector of tuples (need_can index, ndsinx_plan_all index)
        let mut can_nds_pln = Vec::<(usize, usize)>::with_capacity(cnt);

        for (can_inx, npa_inx) in need_can.iter().enumerate() {
            let nds_inx = ndsinx_plan_all[*npa_inx].inx;

            if nds[nds_inx].priority() == min_pri {
                can_nds_pln.push((can_inx, *npa_inx));
            }
        }

        // Make second selection, index to can_nds_pln
        let mut can_do2 = Vec::<usize>::new();

        assert!(!can_nds_pln.is_empty());

        // Find the shortest plan length
        let mut min_plan_len = std::usize::MAX;
        for cnp_tpl in &can_nds_pln {
            let itmx = &ndsinx_plan_all[cnp_tpl.1];

            if itmx.plans.len() < min_plan_len {
                min_plan_len = itmx.plans.len();
            }
        }

        // Push index to shortest plan needs
        for (inx, cnp_tpl) in can_nds_pln.iter().enumerate() {
            let itmx = &ndsinx_plan_all[cnp_tpl.1];

            if itmx.plans.len() == min_plan_len {
                can_do2.push(inx);
            }
        }

        assert!(!can_do2.is_empty());

        // Take a random choice
        let cd2_inx = rand::thread_rng().gen_range(0..can_do2.len());
        //println!("inx2 = {}  can_do2 = {}", &inx2, &can_do2[inx2]);

        let itmx = &ndsinx_plan_all[can_nds_pln[can_do2[cd2_inx]].1];
        //println!("itmx.inx = {}", &itmx.inx);

        let ndx = &nds[itmx.inx]; // get need using tuple index

        println!(
            "\nNeed chosen: {} {} {}",
            &can_nds_pln[can_do2[cd2_inx]].0,
            &ndx,
            &itmx.plans.str_terse()
        );

        can_nds_pln[can_do2[cd2_inx]].1
    } // end choose_need

    /// Get a domain number from a string.
    pub fn domain_num_from_string(&self, num_str: &str) -> Result<usize, String> {
        match num_str.parse() {
            Ok(d_num) => {
                if d_num >= self.len() {
                    Err(format!("\nDomain number too large, {}", d_num))
                } else {
                    Ok(d_num)
                }
            }
            Err(error) => Err(format!("Did not understand domain number, {}", error)),
        } // end match
    }

    /// Return the length, the number of domains.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty
    pub fn is_empty(&self) -> bool {
        self.avec.len() == 0
    }

    /// Return a StateStore of all domain current states.
    pub fn all_current_states(&self) -> StateStore {
        let mut all_states = StateStore::with_capacity(self.len());

        for domx in self.avec.iter() {
            all_states.push(domx.cur_state.clone());
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

        // Get all domain states
        let all_states = self.all_current_states();

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
        if !notsups.is_empty() {
            let inx = rand::thread_rng().gen_range(0..notsups.len());
            return Some(SomeNeed::ToOptimalRegion {
                target_regions: notsups[inx].clone(),
            });
        }

        None
    }

    // Print current states and optimal information.
    /// Return true if the current states are in an optimal region.
    pub fn print_optimal(&self) -> bool {
        let mut ret = false;

        let all_states = self.all_current_states();
        let optimal_supersets = self.optimal.supersets_of_states(&all_states);
        if optimal_supersets.is_empty() {
            print!(
                "\nAll Current states: {} in optimal regions: None, not in ",
                all_states
            );
            for optx in self.optimal.iter() {
                print!(" {}", optx);
            }
        } else {
            ret = true;
            print!("\nAll Current states: {} in optimal regions: ", all_states);
            for optx in optimal_supersets.iter() {
                print!(" {}", optx);
            }
            print!(", not in: ");
            let optimal_not_supersets = self.optimal.not_supersets_of_states(&all_states);
            for optx in optimal_not_supersets.iter() {
                print!(" {}", optx);
            }
        }

        println!(
            ", Boredom level = {} of limit {}",
            self.boredom, self.boredom_limit
        );
        println!(" ");
        ret
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
    use crate::region::SomeRegion;
    use crate::regionstore::RegionStore;

    #[test]
    fn all_current_states() -> Result<(), String> {
        // Init a DomainStore.
        let mut dmxs = DomainStore::new();

        let init_state1 = SomeState::new_from_string(1, "s0x12").unwrap();

        // Create domain 0.
        let dom0 = SomeDomain::new(dmxs.len(), init_state1.clone());
        dmxs.push(dom0);

        // Create domain 1.
        let init_state2 = SomeState::new_from_string(2, "s0xabcd").unwrap();
        let dom1 = SomeDomain::new(dmxs.len(), init_state2.clone());
        dmxs.push(dom1);

        let all_states = dmxs.all_current_states();
        println!("all states {}", all_states);

        if all_states.len() != 2 {
            return Err(format!("Invalid length {}", all_states.len()));
        }

        if all_states[0] != init_state1 {
            return Err(format!("Invalid first state {}", all_states[0]));
        }

        if all_states[1] != init_state2 {
            return Err(format!("Invalid second state {}", all_states[1]));
        }

        //assert!(1 == 2);

        Ok(())
    }

    #[test]
    fn check_optimal() -> Result<(), String> {
        // Start a DomainStore
        let mut dmxs = DomainStore::new();

        // Initialize a domain, with number of integers = 1, initial state, optimal region.

        let num_ints = 1;
        let init_state = SomeState::new_random(num_ints);

        // Create domain 0.
        let mut dom0 = SomeDomain::new(dmxs.len(), init_state);

        // Add actions 0 through 8;
        dom0.add_action();

        // Add the domain to the DomainStore.
        dmxs.push(dom0);

        // Initialize a domain, with number of integers = 2, initial state, optimal region.

        let num_ints = 2;
        let init_state = SomeState::new_random(num_ints);

        // Create domain 1.
        let mut dom1 = SomeDomain::new(dmxs.len(), init_state);

        // Add actions 0 through 4.
        dom1.add_action();

        // Add the domain to the DomainStore.
        dmxs.push(dom1);

        // Load optimal regions
        let mut regstr = RegionStore::with_capacity(2);
        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
        dmxs.add_optimal(regstr);

        let mut regstr = RegionStore::with_capacity(2);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
        dmxs.add_optimal(regstr);

        let mut regstr = RegionStore::with_capacity(2);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
        dmxs.add_optimal(regstr);

        let mut regstr = RegionStore::with_capacity(2);
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
        dmxs.add_optimal(regstr);

        println!("Optimal and ints:");
        for regstrx in dmxs.optimal_and_ints.iter() {
            println!("regstrx: {}", regstrx);
        }

        let all_states = dmxs.all_current_states();
        println!("\nCurr st: {}", all_states);

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

        //assert!(1 == 2);
        Ok(())
    }
}
