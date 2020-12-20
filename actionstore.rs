// Implement a store for actions

use crate::action::SomeAction;
use crate::mask::SomeMask;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
//use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::state::SomeState;
use crate::stepstore::StepStore;

use std::fmt;
use std::ops::{Index, IndexMut};

//use std::thread;
//use std::sync::mpsc;
use rayon::prelude::*;

impl fmt::Display for ActionStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::new();

        for actx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n");
            }
            rc_str.push_str(&format!("{}", &actx));
            flg = 1;
        }

        write!(f, "{}", rc_str)
    }
}

pub struct ActionStore {
    pub avec: Vec<SomeAction>,
}

impl ActionStore {
    pub fn new() -> Self {
        ActionStore {
            avec: Vec::<SomeAction>::with_capacity(5),
        }
    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    pub fn push(&mut self, mut val: SomeAction) {
        val.num = self.avec.len();
        self.avec.push(val);
    }

    // Get needs for all actions
    pub fn get_needs(&mut self, cur: &SomeState, max_x: &SomeMask) -> NeedStore {
        // Run a get_needs thread for each action
        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut()
            .map(|actx| actx.get_needs(cur, max_x))
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::new();

        for mut nst in vecx.iter_mut() {
            nds_agg.append(&mut nst);
        }

        nds_agg
    }

    // Return needs from all actions
    pub fn _get_needs2(&mut self, cur_sta: &SomeState, max_x: &SomeMask) -> NeedStore {
        //println!("actionstore get_needs");

        let mut nds = NeedStore::new();

        //println!("num actions {}", self.avec.len());
        for actx in &mut self.avec {
            nds.append(&mut actx.get_needs(cur_sta, &max_x));
        }

        // If no other needs, check that all states ever reached are
        // in each group of every action.
        if nds.len() == 0 {
            for actx in &self.avec {
                // Get RegionStore of regions in groups
                let regs = actx.groups.regions();

                for acty in &self.avec {
                    if acty.num == actx.num {
                        continue;
                    }

                    // Get StateStore of States not in regions
                    let stas = acty.squares.not_in_regions(&regs);

                    for stax in stas.iter() {
                        nds.push(SomeNeed::StateNotInGroup {
                            dom_num: 0, // set this in domain get_needs
                            act_num: actx.num,
                            targ_state: stax.clone(),
                        });
                    }
                } // next acty
            } // next actx
        } // endif len

        //println!("action store get_needs returns {}", &nds);
        nds
    }

    pub fn get_steps(&self, arule: &SomeRule) -> StepStore {
        let mut stps = StepStore::new();

        for actx in &self.avec {
            stps.append(actx.get_steps(arule));
        }

        //println!("possible steps: {}", stps.str());
        stps
    }

    //    pub fn set_inx(&mut self, inx: usize, astate: SomeAction) {
    //        self.avec[inx] = astate;
    //    }

    // Update actions with new X-bits mask
    pub fn new_x_bits(&mut self, bitsx: &SomeMask) {
        for actx in &mut self.avec {
            actx.new_x_bits(&bitsx);
        }
    }
}

impl Index<usize> for ActionStore {
    type Output = SomeAction;
    fn index<'a>(&'a self, i: usize) -> &'a SomeAction {
        &self.avec[i]
    }
}

impl IndexMut<usize> for ActionStore {
    fn index_mut<'a>(&mut self, i: usize) -> &mut Self::Output {
        &mut self.avec[i]
    }
}
