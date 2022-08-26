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

use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::state::SomeState;
use crate::statestore::StateStore;
use crate::optimalregionsstore::OptimalRegionsStore;

use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
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
            rc_str.push_str(&format!("{}", &mskx));
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

// An InxPlan struct, containing an index to a SomeNeed vector, and a SomePlan struct.
//
// A Vec<T> is needed as a collector for parallel processing of needs to find plans,
// but a tuple, or array, does not qualify as a "T".
#[readonly::make]
#[derive(Debug)]
pub struct InxPlan {
    /// Index to a need in a NeedStore.
    pub inx: usize,
    /// Plan to satisfy need (may be empty if the current state satisfies the need), or None.
    pub pln: Option<SomePlan>,
}

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct DomainStore {
    /// Vector of SomeDomain structs.
    pub avec: Vec<SomeDomain>,
    /// Current step number of the user interface.
    pub step: usize, // The current step number in the UI.
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

impl DomainStore {
    /// Return a new, empty, DomainStore struct.
    pub fn new(optimal: OptimalRegionsStore) -> Self {

        let mut optimal_and_ints = optimal.clone();
        if optimal.len() > 1 {
            optimal_and_ints = optimal.and_intersections();
        }

        Self {
            avec: Vec::<SomeDomain>::with_capacity(5),
            step: 0,
            boredom: 0,
            boredom_limit: 0,
            optimal_and_ints,
            optimal: optimal,
        }
    }

    /// Accessor, set the setp field.
    pub fn set_step(&mut self, anum: usize) {
        self.step = anum;
    }

    /// Add a Domain struct to the store.
    pub fn push(&mut self, domx: SomeDomain) {
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
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::new();

        for mut nst in vecx.iter_mut() {
            nds_agg.append(&mut nst);
        }

//        if let Some(ndx) = self.check_optimal() {
//            nds_agg.push(ndx);
//        }
        // Check optimal regions and boredom duration.
        if self.optimal.len() > 1 {
            let all_states = self.all_current_states();
            let opt_sups = self.optimal.supersets_of_states(&all_states);

            if opt_sups.len() == 0 {
                self.boredom = 0;
            } else {
                self.boredom += 1;
            }
        }

        nds_agg
    }

    /// Run a plan for a given Domain.
    /// Return true if the plan ran to completion.
    pub fn run_plan(&mut self, dmxi: usize, pln: &SomePlan) -> bool {
        self.avec[dmxi].run_plan(pln)
    }

    /// Take an action to satisfy a need
    pub fn take_action_need(&mut self, dmxi: usize, ndx: &SomeNeed) {
        self.avec[dmxi].take_action_need(ndx);
    }

    /// Take an action for a step
    pub fn take_action_step(&mut self, dmxi: usize, actx: usize) {
        self.avec[dmxi].take_action_step(actx);
    }

    /// Return the current state of a given Domain index
    pub fn cur_state(&self, dmxi: usize) -> SomeState {
        self.avec[dmxi].get_current_state()
    }

    /// Return the number of domains
    pub fn num_domains(&self) -> usize {
        self.avec.len()
    }

    /// Return a vector of InxPlan structs, given a NeedStore.
    /// Each InxPlan will contain an index to the NeedStore, and an Option<SomePlan>
    pub fn evaluate_needs(&self, nds: &NeedStore) -> Vec<InxPlan> {
        //println!("domainstore::evaluate_needs");
        let mut last_priority = 0;

        loop {
            // find next lowest priority number (highest priority)needs
            let mut avec = Vec::<usize>::new();
            let mut least_priority = 9999;

            for ndsx in nds.iter() {
                let pri = ndsx.priority();
                if pri > last_priority {
                    if pri < least_priority {
                        least_priority = pri;
                    }
                }
            }

            //println!("least priority = {}", least_priority);

            // No plans found for any need, or no needs.
            if least_priority == 9999 {
                // Push InxPlan struct for each need, indicating no plan found.
                let mut inxvec = Vec::<InxPlan>::with_capacity(nds.len());
                for inx in 0..nds.len() {
                    inxvec.push(InxPlan {
                        inx: inx,
                        pln: None,
                    });
                }
                return inxvec;
            }

            // Load avec with indicies to needs of the current priority.
            for (inx, ndsx) in nds.iter().enumerate() {
                if ndsx.priority() == least_priority {
                    avec.push(inx);
                }
            }

            // Scan needs to see what can be achieved with a plan
            // Parallel make_plans for needs.
            // It likes to collect a vector of structures, in this case InxPlan,
            // instead of a tuple or array.
            //
            // To avoid the cycles required to make plans for many needs,
            //   Process needs by groups of the same, decreasing priority (increasing priority number),
            //   until at least one has a plan.
            //
            // Split groups into vectors of length 6 at the most.
            let span = 6;

            // Randomly pick up to 6 needs at a time, from the current priority.
            // The length of rp1 goes down as numbers are chosen.
            let mut rp1 = RandomPick::new(avec.len()); // put numbers 0..avec.len() into a vector.

            while rp1.len() > 0 {
                let mut end = span;

                if end > rp1.len() {
                    end = rp1.len();
                }

                let mut avec2 = Vec::<usize>::with_capacity(end);
                for _inx in 0..end {
                    avec2.push(avec[rp1.pick().unwrap()]);
                }

                let ndsinx_plan = avec2
                    .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
                    .map(|nd_inx| InxPlan {
                        inx: *nd_inx,
                        pln: self.avec[nds[*nd_inx].dom_num()]
                            .make_plan(&nds[*nd_inx].target().clone()),
                    })
                    .collect::<Vec<InxPlan>>();

                // If at least one plan found, return vector of InxPlan structs.
                for inxplnx in ndsinx_plan.iter() {
                    if let Some(_) = &inxplnx.pln {
                        //println!("inxplnx_plan need {} plan {}", &nds[inxplnx.inx], &apln);
                        //println!("domainstore::evaluate_needs returning vec pri {} num items {}", least_priority, ndsinx_plan.len());
                        return ndsinx_plan;
                    }
                }
            } // end while

            // Increase the lower bound of the next, least, priority number.
            last_priority = least_priority;
        } // end loop
        // Unreachable, since there is no break command.
    } // end evaluate_needs

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
        ndsinx_plan_all: &Vec<InxPlan>,
        need_can: &Vec<usize>, // indicies to ndsinx_plan_all
    ) -> usize {
        assert!(need_can.len() > 0);

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

        assert!(can_nds_pln.len() > 0);

        // Get the first need, in a group of needs with the same priority/type
        let cnp_inx = can_nds_pln[0].1;

        let nd0 = &nds[ndsinx_plan_all[cnp_inx].inx];
        //println!("nd0 {}", nd0);

        // Make further selections of needs that can be met,
        //
        // If the needs are AStateMakeGroup,
        //   Find the largest number-X group that will be created,
        //   Select all needs that create a group that large.
        //
        // Otherwise
        //   Find the shortest plan length.
        //   Select needs with the shortest plans.
        match nd0 {
            // Get the largest number-X group created
            SomeNeed::AStateMakeGroup { .. } => {
                // Get max x group num
                let mut a_state_make_group_max_x = 0;
                for cnp_tpl in &can_nds_pln {
                    //let cd = cnp_tup.2;

                    //let itmx = &ndsinx_plan_all[*cd];

                    let ndx = &nds[ndsinx_plan_all[cnp_tpl.1].inx];

                    match ndx {
                        SomeNeed::AStateMakeGroup { num_x: nx, .. } => {
                            if *nx > a_state_make_group_max_x {
                                a_state_make_group_max_x = *nx;
                            }
                        }
                        _ => (),
                    } // end match ndx
                } // next cnp_tup

                // Save indicies for the max-X group created needs
                for (cnp_inx, cnp_tpl) in can_nds_pln.iter().enumerate() {
                    //let itmx = &ndsinx_plan_all[*cd];

                    let ndx = &nds[ndsinx_plan_all[cnp_tpl.1].inx];

                    match ndx {
                        SomeNeed::AStateMakeGroup { num_x: nx, .. } => {
                            if *nx == a_state_make_group_max_x {
                                can_do2.push(cnp_inx);
                            }
                        }
                        _ => (),
                    } // end match ndx
                }
            } // end match AStateMakeGroup
            _ => {
                // Find the shortest plan length
                let mut min_plan_len = std::usize::MAX;
                for cnp_tpl in &can_nds_pln {
                    let itmx = &ndsinx_plan_all[cnp_tpl.1];

                    match &itmx.pln {
                        Some(plnx) => {
                            if plnx.len() < min_plan_len {
                                min_plan_len = plnx.len();
                            }
                        }
                        None => (),
                    }
                }

                // Push index to shortest plan needs
                for (inx, cnp_tpl) in can_nds_pln.iter().enumerate() {
                    let itmx = &ndsinx_plan_all[cnp_tpl.1];

                    match &itmx.pln {
                        Some(plnx) => {
                            if plnx.len() == min_plan_len {
                                can_do2.push(inx);
                            }
                        }
                        None => (),
                    }
                }
            } // End match all other needs
        } // End match nd0

        assert!(can_do2.len() > 0);

        // Take a random choice
        let cd2_inx = rand::thread_rng().gen_range(0..can_do2.len());
        //println!("inx2 = {}  can_do2 = {}", &inx2, &can_do2[inx2]);

        let itmx = &ndsinx_plan_all[can_nds_pln[can_do2[cd2_inx]].1];
        //println!("itmx.inx = {}", &itmx.inx);

        let ndx = &nds[itmx.inx]; // get need using tuple index

        if let Some(pln) = &itmx.pln {
            println!(
                "\nNeed chosen: {} {} {}",
                &can_nds_pln[can_do2[cd2_inx]].0,
                &ndx,
                &pln.str_terse()
            );
        }

        can_nds_pln[can_do2[cd2_inx]].1
    } // end choose_need

    /// Get a domain number from a string.
    pub fn domain_num_from_string(&self, num_str: &str) -> Result<usize, String> {
        match num_str.parse() {
            Ok(d_num) => {
                if d_num >= self.num_domains() {
                    return Err(format!("\nDomain number too large, {}", d_num));
                } else {
                    return Ok(d_num);
                }
            }
            Err(error) => {
                return Err(format!("Did not understand domain number, {}", error));
            }
        } // end match
    }

    /// Return the length of a DomainStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Combine all domain current states, producing a larger state.
    /// Possible future use.
    pub fn combine_states(&self) -> SomeState {
        let mut ret_state = self[0].cur_state.clone();

        for inx in 1..self.len() {
            ret_state = ret_state.combine(&self[inx].cur_state);
        }

        ret_state
    }

    /// Split a state into states that match the size of each domain current state.
    /// Possible future use.
    pub fn split_state(&self, astate: &SomeState) -> Vec<SomeState> {
        let mut ret_vec = Vec::<SomeState>::new();

        let mut place = 0;
        for domx in self.avec.iter() {
            let num_ints = domx.num_ints;

            ret_vec.push(astate.clone().slice(place, place + num_ints));

            place += num_ints;
        }

        ret_vec
    }

    /// Return a StateStore of all domain current states.
    pub fn all_current_states(&self) -> StateStore {

        let mut all_states = StateStore::with_capacity(self.len());

        for domx in self.avec.iter() {
            all_states.push(domx.cur_state.clone());
        }

        all_states
    }

    /// Return the boredom limit, given the current domain states, if any.
    pub fn boredom_limit(&self, stas: &StateStore) -> usize {
        // Get the optimal regions the current state is in.
        let num_sups = self.optimal.number_supersets_of_states(stas);

        3 * num_sups
    }

    /// Do functions related to the wish to be in an optimum region.
    /// Increment the boredom duration, if needed.
    /// Return a need to move to another optimal region, if needed.
    pub fn check_optimal(&mut self) -> Option<SomeNeed> {
        // Check if there are no optimal regions.
        if self.optimal.len() == 0 {
            return None;
        }

        let all_states = self.all_current_states();

        let limit = self.boredom_limit(&all_states);

        if self.optimal.any_supersets_of_states(&all_states) {
            self.boredom += 1;
            if self.boredom <= limit {
                return None;
            }
        } else {
            self.boredom = 0;
        }

        // Get regions the current state is not in.
        let notsups = self.optimal.not_supersets_of_states(&all_states);

        // If the current state is not in at least one optimal region,
        // return a need to move to an optimal region.
        if notsups.len() > 0 {
            let inx = rand::thread_rng().gen_range(0..notsups.len());
            return Some(SomeNeed::ToRegion2 { goal_regs: notsups[inx].clone() });
        }

        None
    }

    // Print current states and optimal information.
    /// Return true if the current states are in an optimal region.
    pub fn print_optimal(&self) -> bool {
        let mut ret = false;

        let all_states = self.all_current_states();
        let optimal_supersets = self.optimal.supersets_of_states(&all_states);
        if optimal_supersets.len() == 0 {
            print!("\nAll Current states: {} in optimal regions: None, not in ", all_states);
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

        println!(", Boredom level = {} of limit {}", self.boredom, self.boredom_limit);
        println!(" ");
        ret
    }

    /// Change the current state to be within an optimal region that it is not currently in.
    pub fn change_optimal_state(&mut self) {
        //println!("change_optimal_state - start");
        let all_states = self.all_current_states();

        let optimal_not_supersets = self.optimal.not_supersets_of_states(&all_states);

        if optimal_not_supersets.len() > 0 {
            if self.optimal.any_supersets_of_states(&all_states) {
                println!("\nGo to another optimal state!");
            } else {
                println!("\nGo to an optimal state!");
            }

            // Pick a random optional region set
            let mut rp1 = RandomPick::new(optimal_not_supersets.len());

            // Try each optimal region set until one is satisfied or not.
            'next_inx: while let Some(inx) = rp1.pick() {
                println!("Try to go from {} to {}", all_states, optimal_not_supersets[inx]);
                let mut planvec = Vec::<SomePlan>::with_capacity(all_states.len());
                for (domx, regx) in optimal_not_supersets[inx].iter().enumerate() {
                    if regx.is_superset_of_state(&all_states[domx]) {
                        planvec.push(SomePlan::new());
                    } else {
                        if let Some(planx) = self.avec[domx].make_plan(regx) {
                            planvec.push(planx);
                        }
                    }
                }
                if planvec.len() == all_states.len() {

                    for (domx, planx) in planvec.iter().enumerate() {
                        if planx.len() > 0 {
                            if self.avec[domx].run_plan(planx) {
                            } else {
                                continue 'next_inx;
                            }
                        }
                    }
                    let all_states = self.all_current_states();
                    self.boredom = 0;
                    self.boredom_limit = self.boredom_limit(&all_states);
                    //self.print_optimal();
                    return;
                }
            } // end of random pick
            self.boredom = 0;
        }
    }

} // end impl DomainStore

impl Index<usize> for DomainStore {
    type Output = SomeDomain;
    fn index<'a>(&'a self, i: usize) -> &'a SomeDomain {
        &self.avec[i]
    }
}

impl IndexMut<usize> for DomainStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::region::SomeRegion;
    use crate::regionstore::RegionStore;

    // Test combine
    #[test]
    fn combine_states() -> Result<(), String> {
        // Init a DomainStore.
        let mut dmxs = DomainStore::new(OptimalRegionsStore::new());

        let init_state = SomeState::new_from_string(1, "s0x12").unwrap();

        // Create domain 0.
        let dom0 = SomeDomain::new(dmxs.len(), init_state);
        dmxs.push(dom0);

        // Create domain 1.
        let init_state = SomeState::new_from_string(2, "s0xabcd").unwrap();
        let dom1 = SomeDomain::new(dmxs.len(), init_state);
        dmxs.push(dom1);

        let st3 = dmxs.combine_states();
        println!("st3: {}", st3);

        let st4 = SomeState::new_from_string(3, "s0x12abcd").unwrap();

        if st3 != st4 {
            return Err(format!("{} ne {} ?", st3, st4));
        }

        Ok(())
    }

    #[test]
    fn split_state() -> Result<(), String> {
        // Init a DomainStore.
        let mut dmxs = DomainStore::new(OptimalRegionsStore::new());

        let init_state = SomeState::new_from_string(1, "s0x12").unwrap();

        // Create domain 0.
        let dom0 = SomeDomain::new(dmxs.len(), init_state);
        dmxs.push(dom0);

        // Create domain 1.
        let init_state = SomeState::new_from_string(2, "s0xabcd").unwrap();
        let dom1 = SomeDomain::new(dmxs.len(), init_state);
        dmxs.push(dom1);

        let st4 = SomeState::new_from_string(3, "s0x12abcd").unwrap();

        let stvec = dmxs.split_state(&st4);
        println!("dom 0: {} dom 1: {}", stvec[0], stvec[1]);

        Ok(())
    }

    #[test]
    fn all_current_states() -> Result<(), String> {
        // Init a DomainStore.
        let mut dmxs = DomainStore::new(OptimalRegionsStore::new());

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

        // Load optimal regions
        let mut optimal = OptimalRegionsStore::new();

        let mut regstr = RegionStore::with_capacity(2);
        regstr.push(SomeRegion::new_from_string(1, "r0x0x").unwrap());
        regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
        optimal.push(regstr);

        let mut regstr = RegionStore::with_capacity(2);
        regstr.push(SomeRegion::new_from_string(1, "r0xx1").unwrap());
        regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
        optimal.push(regstr);

        let mut regstr = RegionStore::with_capacity(2);
        regstr.push(SomeRegion::new_from_string(1, "rx1x1").unwrap());
        regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
        optimal.push(regstr);

        let mut regstr = RegionStore::with_capacity(2);
        regstr.push(SomeRegion::new_from_string(1, "r1110").unwrap());
        regstr.push(SomeRegion::new_from_string(2, "rXXXXXX10_1XXX_XXXX").unwrap());
        optimal.push(regstr);

        // Start a DomainStore
        let mut dmxs = DomainStore::new(optimal);

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

        println!("Optimal and ints:");
        for regstrx in dmxs.optimal_and_ints.iter() {
            println!("regstrx: {}", regstrx);
        }

        let all_states = dmxs.all_current_states();
        println!("\nCurr st: {}", all_states);

        println!("\nNumber supersets: {}", dmxs.optimal.number_supersets_of_states(&all_states));

        if let Some(needx) = dmxs.check_optimal() {
            println!("\nCheck_optimal returns {}", needx);
        } else {
            println!("\nCheck_otimal returns None");
        }

        println!("\nBoredom level {} Boredom_limit {}", dmxs.boredom, dmxs.boredom_limit(&all_states));

        println!(" ");

        //assert!(1 == 2);
        Ok(())
    }
}

