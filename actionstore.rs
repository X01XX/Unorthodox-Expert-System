// Implement a store for actions

use crate::action::SomeAction;
use crate::mask::SomeMask;
//use crate::need::SomeNeed;
use crate::needstore::NeedStore;
//use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::state::SomeState;
use crate::stepstore::StepStore;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};

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

#[derive(Serialize, Deserialize)]
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
    pub fn get_needs(&mut self, cur: &SomeState) -> NeedStore {
        // Run a get_needs thread for each action
        let mut vecx: Vec<NeedStore> = self
            .avec
            .par_iter_mut() // .iter for easier reading of diagnostic messages
            .map(|actx| actx.get_needs(cur))
            .collect::<Vec<NeedStore>>();

        // Aggregate the results into one NeedStore
        let mut nds_agg = NeedStore::new();

        for mut nst in vecx.iter_mut() {
            nds_agg.append(&mut nst);
        }

        nds_agg
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
