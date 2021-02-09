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

//! The DomainStore struct, for an Unorthodox Expert System.
//!
//! Keeps a vector of Domain structs.
//!
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

#[derive(Serialize, Deserialize)]
pub struct DomainStore {
    pub avec: Vec<SomeDomain>,
    pub step: usize,            // The current step number in the UI.
}

impl DomainStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeDomain>::with_capacity(5),
            step: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    pub fn add_domain(&mut self, num_ints: usize, start_state: &str, optimal_region: &str) {
        self.avec.push(SomeDomain::new(
            num_ints,
            start_state,
            optimal_region,
            self.avec.len(),
        ));
    }

    // Add an action to the last added domain.
    pub fn add_action(&mut self, ran_num: usize) {
        let last = self.len() - 1;
        self.avec[last].add_action(ran_num);
    }

    //    pub fn push(&mut self, mut val: SomeDomain) {
    //        val.num = self.avec.len();
    //        self.avec.push(val);
    //    }

    //    pub fn iter(&self) -> Iter<SomeDomain> {
    //        self.avec.iter()
    //    }

    //    pub fn iter_mut(&mut self) -> IterMut<SomeDomain> {
    //        self.avec.iter_mut()
    //    }

    // Use threads to get needs for each Domain.
    // Each Domain uses threads to get needs for each Action.
    pub fn get_needs(&mut self) -> NeedStore {
        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // .par_iter for prallel, .iter_mut for easier reading of diagnostic messages
            .map(|domx| domx.get_needs())
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::new();

        for mut nst in vecx.iter_mut() {
            nds_agg.append(&mut nst);
        }

        //  For testing, to make the terminal ouput sequential, comment out the above code.
        //        let mut nds_agg = NeedStore::new();
        //        for domx in self.avec.iter_mut() {
        //		    let mut ndx = domx.get_needs();
        //		    if ndx.len() > 0 {
        //				nds_agg.append(&mut ndx);
        //			}
        //		}

        nds_agg
    }

    // Run a plan for a given Domain
    pub fn run_plan(&mut self, dmxi: usize, pln: &SomePlan) {
        self.avec[dmxi].run_plan(pln)
    }

    pub fn take_action_need(&mut self, dmxi: usize, ndx: &SomeNeed) {
        self.avec[dmxi].take_action_need(ndx);
    }

    // Return the current state of a given Domain index
    pub fn cur_state(&self, dmxi: usize) -> &SomeState {
        &self.avec[dmxi].cur_state
    }

    // Return the number of domains
    pub fn num_domains(&self) -> usize {
        self.avec.len()
    }

    //struct InxPlan {
    //    inx: usize,            // Index to a need in a NeedStore.
    //    pln: Option<SomePlan>, // Plan to satisfy need (may be empty if the current state satisfies the need), or None.
    //}

    // Return a vector of InxPlan structs, in no order, given a NeedStore.
    // Each InxPlan will contain an index to the NeedStore, and an Option<SomePlan>
    pub fn evaluate_needs(&self, nds: &NeedStore) -> Vec<InxPlan> {
        // Make a vector of need position indicies.
        let avec: Vec<usize> = (0..nds.len()).collect();

        // Scan needs to see what can be achieved with a plan
        // Parallel make_plans for needs
        // It likes to collect a structure, in this case InxPlan,
        // instead of a tuple or array
        let ndsinx_plan = avec
            .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
            .map(|nd_inx| InxPlan {
                inx: *nd_inx,
                pln: self.avec[nds[*nd_inx].dom_num()].make_plan(&nds[*nd_inx].target().clone()),
            })
            .collect::<Vec<InxPlan>>();

        // For testing, to make the terminal output sequential, comment out the above code.
        //        let mut ndsinx_plan = Vec::<InxPlan>::new();
        //        let mut inx = 0;
        //        for ndx in nds.iter() {
        //		    ndsinx_plan.push(InxPlan { inx, pln: self.avec[ndx.dom_num()].make_plan(&ndx.target().clone()) });
        //		    inx += 1;
        //		}

        ndsinx_plan
    }

    // Choose a need, given a vector of needs and a vectore of InxPlans (with
    // at least one doable need).
    //
    // Sort needs by priority.
    //
    // Scan needs, by priority, to see what need can be satisfied by a plan,
    // limit to three of the same priority.
    //
    // Return a tuple of the (index to the need that can be satisfied, a plan to do so).
    pub fn choose_need(
        &self,
        nds: &NeedStore,
        ndsinx_plan_all: &Vec<InxPlan>,
    ) -> Option<(usize, SomePlan)> {
        // Store tuples of NeedStore-index and plan, for needs that can be acheived
        // by matching the current state (empty plan) or by having a plan calculated.

        // A vector of vectors, for needs to be processed in order of priority,
        // lowest number first/highest.
        let mut pri_vec = Vec::<Vec<usize>>::with_capacity(8);

        // Scan for needs, put need indicies into a vector.
        // Sort by priority.
        let mut inx = 0;
        for ndx in nds.iter() {
            let pri = ndx.priority();
            while pri_vec.len() <= pri {
                pri_vec.push(Vec::<usize>::new());
            }

            pri_vec[pri].push(inx);

            inx += 1;
        } // end scan of needs to assign priority

        // A vector of need-index and plan, for needs that can be met with a plan.
        let mut ndsinx_plan = Vec::<usize>::new();

        // Scan needs to see what can be achieved with a plan
        for avec in pri_vec.iter() {
            if avec.len() == 0 {
                continue;
            }

            //println!(" ***** number needs to plan for is {}", avec.len());
            if avec.len() == 1 {
                let nd_inx = avec[0];

                // Find need in ndsinx_plan_all, and store index if there is a plan.
                let mut inx = 0;
                for ndsin in ndsinx_plan_all.iter() {
                    if ndsin.inx == nd_inx {
                        if let Some(_) = ndsin.pln {
                            ndsinx_plan.push(inx);
                            break;
                        }
                    }
                    inx += 1;
                }
            } else {
                for nd_inx in avec.iter() {
                    // Find need in ndsinx_plan_all, and store index if there is a plan.
                    let mut inx = 0;
                    for ndsin in ndsinx_plan_all.iter() {
                        if ndsin.inx == *nd_inx {
                            if let Some(_) = ndsin.pln {
                                ndsinx_plan.push(inx);
                                break;
                            }
                        }
                        inx += 1;
                    }
                } // next nd_inx
            }

            // If at least one need of the current priority has been
            // found to be doable, do not check later priority needs
            if ndsinx_plan.len() > 0 {
                break;
            }
        } // next avec in pri_vec

        // Selection for needs that can be planned
        // A vector of indicies to a (need-index and plan) vector, for needs that can be met with a plan.
        let mut can_do2 = Vec::<usize>::new();

        assert!(ndsinx_plan.len() > 0);

        // Get the first need, in a group of needs with the same priority/type
        let nd0 = &nds[ndsinx_plan_all[ndsinx_plan[0]].inx];
        //println!("nd0 {}", nd0);

        // Make further selections of needs that can be met,
        // after previously selecting for the highest priority.
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
                for cd in &ndsinx_plan {
                    let itmx = &ndsinx_plan_all[*cd];

                    let ndx = &nds[itmx.inx];

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
                } // next itmx

                // Save indicies for the max-X group created needs
                let mut inx: usize = 0;
                for cd in &ndsinx_plan {
                    let itmx = &ndsinx_plan_all[*cd];

                    let ndx = &nds[itmx.inx];

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
                                can_do2.push(inx);
                            }
                        }
                        _ => {}
                    } // end match ndx

                    inx += 1;
                }
            } // end match AStateMakeGroup
            _ => {
                // Find the shortest plan length
                let mut min_plan_len = std::usize::MAX;
                for cd in &ndsinx_plan {
                    let itmx = &ndsinx_plan_all[*cd];
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
                let mut inx = 0;
                for cd in &ndsinx_plan {
                    let itmx = &ndsinx_plan_all[*cd];
                    match &itmx.pln {
                        Some(plnx) => {
                            if plnx.len() == min_plan_len {
                                can_do2.push(inx);
                            }
                        }
                        None => {}
                    }

                    inx += 1;
                }
            } // End match all other needs
        } // End match nd0

        // Return if no plans.  Includes plans of zero length for the current state.
        if can_do2.len() == 0 {
            panic!("at least one doable need/plan should have been selected!");
        }

        //println!("can_do2: {:?}", can_do2);

        // Take a random choice
        let inx2 = rand::thread_rng().gen_range(0, can_do2.len());
        //println!("inx2 = {}  can_do2 = {}", &inx2, &can_do2[inx2]);

        let itmx = &ndsinx_plan_all[ndsinx_plan[inx2]];
        //println!("itmx.inx = {}", &itmx.inx);

        let ndx = &nds[itmx.inx]; // get need using tuple index

        let pln = itmx.pln.as_ref().unwrap().clone();

        println!("\nNeed chosen: {} {}", &ndx, &pln.str_terse());
        return Some((itmx.inx, pln));
    } // end choose_need
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
