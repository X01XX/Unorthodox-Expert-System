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
use crate::inxplan::InxPlan;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::state::SomeState;

use std::fmt;
use std::ops::{Index, IndexMut};
extern crate rand;
use rand::Rng;
use serde::{Deserialize, Serialize};

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

#[readonly::make]
#[derive(Serialize, Deserialize)]
pub struct DomainStore {
    /// Vector of SomeDomain structs.
    pub avec: Vec<SomeDomain>,
    /// Current step number of the user interface.
    pub step: usize, // The current step number in the UI.
}

impl DomainStore {
    /// Return a new, empty, DomainStore struct.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeDomain>::with_capacity(5),
            step: 0,
        }
    }

    /// Accessor, return the value of the step field.
//    pub fn get_step(&self) -> usize {
//        self.step
//    }
    
    /// Accessor, set the setp field.
    pub fn set_step(&mut self, anum: usize) {
        self.step = anum;
    }
    
    /// Add a Domain struct to the store.
    pub fn push(&mut self, mut domx: SomeDomain) {
        domx.set_num(self.avec.len());
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

        nds_agg
    }

    /// Run a plan for a given Domain
    pub fn run_plan(&mut self, dmxi: usize, pln: &SomePlan) {
        self.avec[dmxi].run_plan(pln)
    }

    /// Take an action to satisfy a need
    pub fn take_action_need(&mut self, dmxi: usize, ndx: &SomeNeed) {
        self.avec[dmxi].take_action_need(ndx);
    }

    /// Take an action to satisfy a need
    pub fn take_action(&mut self, dmxi: usize, actx: usize) {
        self.avec[dmxi].take_action(actx);
    }
    
    /// Return the current state of a given Domain index
    pub fn cur_state(&self, dmxi: usize) -> &SomeState {
        &self.avec[dmxi].cur_state
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

            // find next lowest priority needs
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

            if least_priority == 9999 {
                //println!("domainstore::evaluate_needs returning empty vec");
                return Vec::<InxPlan>::new();
            }

            // Load avec with indicies
            let mut inx = 0;
            for ndsx in nds.iter() {

                if ndsx.priority() == least_priority {
                    avec.push(inx);
                }
                inx += 1;
            }
            //println!("num items in avec {}", avec.len());

            // Scan needs to see what can be achieved with a plan
            // Parallel make_plans for needs
            // It likes to collect a structure, in this case InxPlan,
            // instead of a tuple or array
            //
            // To avoid the cycles required to make plans for many needs,
            //   Process needs by groups of the same, decreasing priority, until at least one has a plan
            //   Split groups into vectors of 4 at the most. 
            let span = 6;

            let mut rp1 = RandomPick::new(avec.len());    // put numbers 0..avec.len() into a vector.

            while rp1.len() > 0 { 
                //println!("rp1 len = {}", rp1.len());

                let mut end = span; 

                if end > rp1.len() { 
                    end = rp1.len();
                }

                let mut avec2 = Vec::<usize>::with_capacity(span);
                //println!("sub while at 0");
                for _inx in 0..end {
                    avec2.push(avec[rp1.pick().unwrap()]);
                }

                //println!("sub while at 2");
                let ndsinx_plan = avec2
                    .iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
                    .map(|nd_inx| InxPlan {
                        inx: *nd_inx,
                        pln: self.avec[nds[*nd_inx].dom_num()].make_plan(&nds[*nd_inx].target().clone()),
                    })
                    .collect::<Vec<InxPlan>>();

                //println!("sub while at 3");
                for inxplnx in ndsinx_plan.iter() {
                    if let Some(_) = &inxplnx.pln {
                        //println!("inxplnx_plan need {} plan {}", &nds[inxplnx.inx], &apln);
                        //println!("domainstore::evaluate_needs returning vec pri {} num items {}", least_priority, ndsinx_plan.len());
                        return ndsinx_plan;
                    }
                }
                //println!("sub while at 5");
            } // end while

            last_priority = least_priority;

        } // end loop
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

        let mut can_inx = 0;
        for npa_inx in need_can {
            let nds_inx = ndsinx_plan_all[*npa_inx].inx;

            if nds[nds_inx].priority() == min_pri {
                can_nds_pln.push((can_inx, *npa_inx));
            }
            can_inx += 1;
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
            SomeNeed::AStateMakeGroup {
                dom_num: _,
                act_num: _,
                targ_state: _,
                for_reg: _,
                far: _,
                num_x: _,
            } => {
                // Get max x group num
                let mut a_state_make_group_max_x = 0;
                for cnp_tpl in &can_nds_pln {
                    //let cd = cnp_tup.2;

                    //let itmx = &ndsinx_plan_all[*cd];

                    let ndx = &nds[ndsinx_plan_all[cnp_tpl.1].inx];

                    match ndx {
                        SomeNeed::AStateMakeGroup {
                            dom_num: _,
                            act_num: _,
                            targ_state: _,
                            for_reg: _,
                            far: _,
                            num_x: nx,
                        } => {
                            if *nx > a_state_make_group_max_x {
                                a_state_make_group_max_x = *nx;
                            }
                        }
                        _ => {}
                    } // end match ndx
                } // next cnp_tup

                // Save indicies for the max-X group created needs
                let mut cnp_inx: usize = 0;
                for cnp_tpl in &can_nds_pln {
                    //let itmx = &ndsinx_plan_all[*cd];

                    let ndx = &nds[ndsinx_plan_all[cnp_tpl.1].inx];

                    match ndx {
                        SomeNeed::AStateMakeGroup {
                            dom_num: _,
                            act_num: _,
                            targ_state: _,
                            for_reg: _,
                            far: _,
                            num_x: nx,
                        } => {
                            if *nx == a_state_make_group_max_x {
                                can_do2.push(cnp_inx);
                            }
                        }
                        _ => {}
                    } // end match ndx

                    cnp_inx += 1;
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
                        None => {}
                    }
                }

                // Push index to shortest plan needs
                let mut cnp_inx = 0;
                for cnp_tpl in &can_nds_pln {
                    let itmx = &ndsinx_plan_all[cnp_tpl.1];

                    match &itmx.pln {
                        Some(plnx) => {
                            if plnx.len() == min_plan_len {
                                can_do2.push(cnp_inx);
                            }
                        }
                        None => {}
                    }

                    cnp_inx += 1;
                }
            } // End match all other needs
        } // End match nd0

        assert!(can_do2.len() > 0);

        // Take a random choice
        let cd2_inx = rand::thread_rng().gen_range(0, can_do2.len());
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
