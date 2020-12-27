// Implement a store for steps

//use crate::bits::SomeBits;
//use crate::mask::SomeMask;
//use crate::region::SomeRegion;
//use crate::regionstore::RegionStore;
//use crate::rule::SomeRule;
use crate::step::SomeStep;

use std::fmt;
use std::ops::Index; // IndexMut
use std::slice::Iter;

impl fmt::Display for StepStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::from("[");

        for stpx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &stpx));
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
pub struct StepStore {
    avec: Vec<SomeStep>,
}

impl StepStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeStep>::with_capacity(5),
        }
    }

    pub fn new_with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeStep>::with_capacity(num),
        }
    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    pub fn push(&mut self, val: SomeStep) {
        self.avec.push(val);
    }

    pub fn append(&mut self, mut val: StepStore) {
        self.avec.append(&mut val.avec); // empties val.avec
    }

    //    pub fn str_terse(&self) -> String {
    //        let mut start = true;
    //        let mut rc_str = String::new();
    //
    //        for stpx in &self.avec {
    //            if start {
    //                rc_str.push_str(&format!("{}", &stpx.initial));
    //                rc_str.push_str(&format!(" -{}-> ", stpx.act_num));
    //                rc_str.push_str(&format!("{}", &stpx.result));
    //                start = false;
    //            } else {
    //                rc_str.push_str(&format!(" -{}-> ", stpx.act_num));
    //                rc_str.push_str(&format!("{}", stpx.result));
    //            }
    //        }
    //
    //        rc_str
    //    }

    pub fn iter(&self) -> Iter<SomeStep> {
        self.avec.iter()
    }

    pub fn clone(&self) -> Self {
        let mut rcstp = Self::new_with_capacity(self.len());
        for stpx in self.avec.iter() {
            rcstp.push(stpx.clone());
        }
        rcstp
    }

    //    pub fn reverse(&self) -> Self {
    //		let mut rc_steps = StepStore { avec:  Vec::<SomeStep>::with_capacity(self.len()) };
    //
    //        for inx in (0..self.len()).rev() {
    //			rc_steps.add(self.avec[inx]);
    //		}
    //
    //		rc_steps
    //	}

    pub fn reverse(&mut self) {
        self.avec.reverse();
    }

    // Return a vector with indices of two steps with the same initial region
    // else return an empty vector.
    pub fn same_intitial(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::new();

        let mut x = 0;
        for stpx in self.avec.iter() {
            for y in (x + 1)..self.len() {
                if stpx.initial == self.avec[y].initial {
                    ret_vec.push(x);
                    ret_vec.push(y);
                    return ret_vec;
                }
            }

            x = x + 1;
        }

        ret_vec
    }
} // end impl StepStore

impl Index<usize> for StepStore {
    type Output = SomeStep;
    fn index<'a>(&'a self, i: usize) -> &'a SomeStep {
        &self.avec[i]
    }
}
