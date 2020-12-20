// Implement a store for Domains

use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::state::SomeState;
use crate::stepstore::StepStore;

use std::fmt;
use std::ops::{Index, IndexMut};
extern crate rand;
use rand::Rng;

//use std::slice::{Iter}; //, IterMut};

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

pub struct DomainStore {
    pub avec: Vec<SomeDomain>,
}

impl DomainStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeDomain>::with_capacity(5),
        }
    }

    pub fn _len(&self) -> usize {
        self.avec.len()
    }

    pub fn push(&mut self, mut val: SomeDomain) {
        val.num = self.avec.len();
        self.avec.push(val);
    }

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
            .par_iter_mut()
            .map(|domx| domx.get_needs())
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::new();

        for mut nst in vecx.iter_mut() {
            nds_agg.append(&mut nst);
        }

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

    // Check need list to see if a need can be satisfied.
    //
    // Scan needs to see what need can be satisfied by the current state, sort by priority, limit to three.
    // In the beginning, this avoids numerous dead-ends in trying to find a plan.
    //
    // Else, scan needs to see what need can be satisfied by a plan, sort by priority, limit to three.
    //
    // Return a tuple of the (index to the need that should be satisfied, a plan to do so).
    pub fn choose_need(&self, nds: &NeedStore) -> Option<(usize, SomePlan)> {
        // Store tuples of NeedStore-index and plan, for needs that can be acheived
        // by matching the current state (empty plan) or by having a plan calculated.

        // A vector of vectors, for needs to be processed in order of priority,
        // lowest number first/highest.
        let mut pri_vec = Vec::<Vec<usize>>::with_capacity(8);

        // Scan for needs that are satisfied by the current state, put need indicies into a vector.
        // Sort by priority.
        let mut found = false;
        let mut inx = 0;
        for ndx in nds.iter() {
            let dmx = &self.avec[ndx.dom_num()];
            if ndx.satisfied_by(&dmx.cur_state) {
                found = true;

                if let Some(pri) = ndx.priority() {
                    while pri_vec.len() <= pri {
                        pri_vec.push(Vec::<usize>::new());
                    }

                    pri_vec[pri].push(inx);
                }
            }
            inx += 1;
        }

        // If one or more needs found that the current state satisfies, run one
        if found {
            // Print needs that can be achieved.
            println!(
                "{}",
                &String::from("\nSelected Action needs that can be done: ")
            );

            // print each need and plan
            for avec in pri_vec.iter() {
                if avec.len() > 0 {
                    for itmx in avec.iter() {
                        println!("{} satisfied by current state", &nds[*itmx]);
                    }
                    println!("-----");
                    break;
                }
            }

            for avec in pri_vec.iter() {
                if avec.len() > 0 {
                    let mut itmx = 0;

                    if avec.len() > 1 {
                        itmx = rand::thread_rng().gen_range(0, avec.len());
                    }

                    let ndx = &nds[avec[itmx]];
                    println!("Need chosen: {}  satisfied by the current state\n", &ndx);
                    //dmx.take_action_need(ndx);
                    //return true;
                    return Some((avec[itmx], SomePlan::new(StepStore::new())));
                }
            } // next avec
        }

        // Scan for needs, put need indicies into a vector.
        // Sort by priority.
        let mut inx = 0;
        for ndx in nds.iter() {
            if let Some(pri) = ndx.priority() {
                while pri_vec.len() <= pri {
                    pri_vec.push(Vec::<usize>::new());
                }

                pri_vec[pri].push(inx);
            } // else the need is a adinistrative need that has already been delt with, so skip it.
            inx += 1;
        } // end scan of needs to assign priority

        // A vector of need-index and plan, for needs that can be met with a plan.
        let mut inx_plan = Vec::<(usize, SomePlan)>::new();

        // Scan needs to see what can be achieved with a plan
        for avec in pri_vec.iter() {
            if avec.len() == 0 {
                continue;
            }

            for nd_inx in avec.iter() {
                let ndx = &nds[*nd_inx];

                let dmx = &self.avec[ndx.dom_num()];
                if let Some(plx) = dmx.make_plan(&ndx.target()) {
                    inx_plan.push((*nd_inx, plx));
                }
            }

            // If at least one need of the current priority has been
            // found to be doable, do not check later priority needs
            if inx_plan.len() > 0 {
                break;
            }
        } // next avec in pri_vec

        // Print needs that can be achieved.
        println!(
            "{}",
            &String::from("\nSelected Action needs that can be done: ")
        );

        // Print each need and plan
        for itmx in inx_plan.iter() {
            println!("{} {}", &nds[itmx.0], &itmx.1);
        }
        println!("-----");

        // Selection for needs that can be planned
        // A vector of indicies to a (need-index and plan) vector, for needs that can be met with a plan.
        let mut inx_plan2 = Vec::<usize>::new();

        let nd0 = &nds[inx_plan[0].0];

        match nd0 {
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
                for itmx in &inx_plan {
                    let ndx = &nds[itmx.0];

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

                let mut inx: usize = 0;
                for itmx in &inx_plan {
                    let ndx = &nds[itmx.0];

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
                                inx_plan2.push(inx);
                            }
                        }
                        _ => {}
                    } // end match ndx

                    inx += 1;
                }
            } // end match AStateMakeGroup
            _ => {
                // Get needs with shortest plan
                let mut min_plan_len = 99999999;
                for itmx in &inx_plan {
                    let plnx = &itmx.1;
                    if plnx.len() < min_plan_len {
                        min_plan_len = plnx.len();
                    }
                }

                // Push index to shortest plan needs
                let mut inx: usize = 0;
                for itmx in &inx_plan {
                    let plnx = &itmx.1;
                    if plnx.len() == min_plan_len {
                        inx_plan2.push(inx);
                    }

                    inx += 1;
                }
            } // End match all other needs
        } // End match nd0

        // Return if no plans.  Includes plans of zero length for the current state.
        if inx_plan2.len() == 0 {
            return None;
        }

        // Take a random choice
        let inx2 = rand::thread_rng().gen_range(0, inx_plan2.len());

        let itmx = &inx_plan[inx_plan2[inx2]];
        //let itmx = &inx_plan2[rand::thread_rng().gen_range(0, inx_plan.len())];
        //    pause_for_enter("6");

        let ndx = &nds[itmx.0]; // get need using tuple index

        let pln = &itmx.1;

        println!("Need chosen: {} {}\n", ndx, &pln);

        return Some((itmx.0, itmx.1.clone()));
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
