// Implement a store for Domains

use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::state::SomeState;
//use crate::stepstore::StepStore;

//use std::thread;
use std::fmt;
use std::ops::{Index, IndexMut};
extern crate rand;
use rand::Rng;
use serde::{Deserialize, Serialize};

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

#[derive(Serialize, Deserialize)]
pub struct DomainStore {
    pub avec: Vec<SomeDomain>,
    pub step: usize,
}

impl DomainStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeDomain>::with_capacity(5),
            step: 0,
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
            .par_iter_mut() // .par_iter for prallel, .iter_mut for easier reading of diagnostic messages
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
        let mut ndsinx_plan = Vec::<InxPlan>::new();

        // Scan needs to see what can be achieved with a plan
        for avec in pri_vec.iter() {
            if avec.len() == 0 {
                continue;
            }

            //println!(" ***** number needs to plan for is {}", avec.len());
            if avec.len() == 1 {
                let nd_inx = &avec[0];
                let ndx = &nds[*nd_inx];

                let dmx = &self.avec[ndx.dom_num()];

                ndsinx_plan.push(InxPlan {
                    inx: *nd_inx,
                    pln: dmx.make_plan(&ndx.target()),
                });
            } else {
                // Parallel make_plans for needs
                // It likes to collect a structure, in this case InxPlan,
                // instead of a tuple or array
                ndsinx_plan = avec
                    .par_iter() // par_iter for parallel, .iter for easier reading of diagnostic messages
                    .map(|nd_inx| InxPlan {
                        inx: *nd_inx,
                        pln: self.avec[nds[*nd_inx].dom_num()]
                            .make_plan(&nds[*nd_inx].target().clone()),
                    })
                    .collect::<Vec<InxPlan>>();

                // Print index and Option(SomePlan)
                //                for nst in vecx.iter() {
                //                    match &nst.pln {
                //                        Some(plnx) => {
                //                            println!("par Need: {} pln: {}", &nds[nst.inx], plnx);
                //                        }
                //                        None => {
                //                            println!("par Need: {} pln: None", &nds[nst.inx]);
                //                        }
                //                    }
                //                }

                // Non parallel make plan
                //                for nd_inx in avec.iter() {
                //                    let ndx = &nds[*nd_inx];

                //                    let dmx = &self.avec[ndx.dom_num()];
                //                    ndsinx_plan.push(InxPlan {
                //                        inx: *nd_inx,
                //                        pln: dmx.make_plan(&ndx.target()),
                //                    });
                //                }
            } // endif avec.len()

            // If at least one need of the current priority has been
            // found to be doable, do not check later priority needs
            let mut is_done = false;
            for itemx in ndsinx_plan.iter() {
                match itemx.pln {
                    Some(_) => {
                        is_done = true;
                    }
                    None => {}
                }
            }
            if is_done {
                break;
            }
        } // next avec in pri_vec

        // Print needs that can be achieved.
        println!(
            "{}",
            &String::from("\nSelected Action needs that can be done: ")
        );

        // Print each need and plan
        let mut can_do = Vec::<usize>::new(); // store indicies to inx_plan vector
        let mut inx = 0;
        for itmx in ndsinx_plan.iter() {
            match &itmx.pln {
                Some(plnx) => {
                    println!("need {} pln {}", &nds[itmx.inx], &plnx);
                    can_do.push(inx);
                }
                None => {}
            }
            inx += 1;
        } // next itmx
        println!("-----");
        //println!("can_do: {:?}", &can_do);

        // Selection for needs that can be planned
        // A vector of indicies to a (need-index and plan) vector, for needs that can be met with a plan.
        let mut can_do2 = Vec::<usize>::new();

        if can_do.len() == 0 {
            return None; // No needs can be done
        }

        // Get the first need, in a group of needs with the same priority/type
        let nd0 = &nds[ndsinx_plan[can_do[0]].inx];
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
                for cd in &can_do {
                    let itmx = &ndsinx_plan[*cd];

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
                for cd in &can_do {
                    let itmx = &ndsinx_plan[*cd];

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
                for cd in &can_do {
                    let itmx = &ndsinx_plan[*cd];
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
                for cd in &can_do {
                    let itmx = &ndsinx_plan[*cd];
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

        let itmx = &ndsinx_plan[can_do[inx2]];
        //println!("itmx.inx = {}", &itmx.inx);

        let ndx = &nds[itmx.inx]; // get need using tuple index
                                  //println!("need {}", &ndx);

        match &itmx.pln {
            Some(pln) => {
                println!("Need chosen: {} {}\n", ndx, &pln);
                return Some((itmx.inx, pln.clone()));
            }
            None => {
                panic!("should be a plan in Option");
            }
        }
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

// This struct is needed for parallel processing to make plans,
// in the choose_need method.  A need is chosen that a plan
// can be calculated for.
// A Vec<T> is needed but a tuple, or array, does not qualify as a "T".
struct InxPlan {
    inx: usize,            // Index to a need in a NeedStore.
    pln: Option<SomePlan>, // Plan to satisfy need (may be empty if the current state satisfies the need), or None.
}
