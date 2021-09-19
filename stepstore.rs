//! The StepStore struct.  A vector of SomeStep structs.

use crate::mask::SomeMask;
//use crate::state::SomeState;
use crate::region::SomeRegion;
use crate::step::SomeStep;
use crate::change::SomeChange;
use crate::rule::SomeRule;

use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for StepStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string(""))
    }
}

#[derive(Debug)]
pub struct StepStore {
    /// A vector for steps.
    pub avec: Vec<SomeStep>,
}

impl StepStore {
    /// Return a new StepStore, empty.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeStep>::with_capacity(5),
        }
    }

    /// Return a new StepStore, empty, with an expected capacity.
    pub fn new_with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeStep>::with_capacity(num),
        }
    }

    /// Return the number of steps in a StepStore.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a step to a StepStore.
    pub fn push(&mut self, val: SomeStep) {
        self.avec.push(val);
    }

    /// Append a StepStore to a StepStore.
    pub fn append(&mut self, val: &mut StepStore) {
        self.avec.append(&mut val.avec); // empties val.avec
    }

    /// Link two stepstores together, return Some(StepStore).
    pub fn link(&self, other: &Self) -> Option<Self> {
        //println!("stepstore:link: {} and {}", self, other);

        let end_inx = self.len() - 1;

        if self.avec[end_inx].result == other.avec[0].initial {
            let mut rc_steps = StepStore::new_with_capacity(self.len() + other.len());

            for stp1 in self.iter() {
                rc_steps.push(stp1.clone());
            }

            for stp2 in other.iter() {
                rc_steps.push(stp2.clone());
            }

            return Some(rc_steps);
        }

        if self.avec[end_inx]
            .result
            .intersects(&other.avec[0].initial)
        {
            let regx = self.avec[end_inx]
                .result
                .intersection(&other.avec[0].initial);

            if let Some(steps1) = self.restrict_result_region(&regx) {
                if let Some(steps2) = other.restrict_initial_region(&regx) {

                    let mut rc_steps = StepStore::new_with_capacity(self.len() + other.len());

                    for stp1 in steps1.iter() {
                        rc_steps.push(stp1.clone());
                    }

                    for stp2 in steps2.iter() {
                        rc_steps.push(stp2.clone());
                    }

                    return Some(rc_steps);
                }
            }
        }
        None
    } // end link

    /// Return an immutable iterator for a StepStore.
    pub fn iter(&self) -> Iter<SomeStep> {
        self.avec.iter()
    }

    /// Reverse the order of steps in a StepStore.
    pub fn reverse(&mut self) {
        self.avec.reverse();
    }

    /// Return the expected length of a string representing a StepStore.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        if self.avec.len() > 0 {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string representing a StepStore.
    pub fn formatted_string(&self, prefix: &str) -> String {
        let mut flg = 0;
        let mut rc_str = String::with_capacity(prefix.len() + self.formatted_string_length());
        rc_str.push_str(prefix);
        rc_str.push('[');

        for stpx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &stpx));
            flg = 1;
        }
        rc_str.push(']');

        rc_str
    }

    /// Given a number of steps, and a required change, return a vector of vectors
    /// where the sub-vectors indicate a single bit change that is required.
    /// Note that a step that changes more than one bit may end up in more than one sub-vector.
    pub fn steps_by_change_bit(&self, required_change: &SomeChange) -> Vec<Vec<usize>> {

        let mut b01 = Vec::<SomeMask>::new();

        if required_change.b01.is_not_low() {
            b01 = required_change.b01.split();
        }

        let mut b10 = Vec::<SomeMask>::new();

        if required_change.b10.is_not_low() {
            b10 = required_change.b10.split();
        }

        let b01_len = b01.len();
        let b10_len = b10.len();
        let tot_len = b01_len + b10_len;

        let mut ret_vec = Vec::<Vec<usize>>::with_capacity(tot_len);

        // Populate ret-vec with empty vectors
        // Some will end up with only one number, indicating a rule that must be used.
        // Some will end up with more than one number, indicating a range from witch one must be picked.
        // Note that a rule might change more than one bit, so it may appear in more than one ret-vec sub-vectors.
        for _ in 0..tot_len {
            ret_vec.push(Vec::<usize>::new());
        }

        // Add step index numbers to the return vector.
        let mut step_inx = 0;
        for stepx in self.avec.iter() {

            // Check for matching b01 changes
            let mut b01_inx = 0;
            for b01x in b01.iter() {

                if stepx.rule.b01.m_and(b01x).is_not_low() {
                    ret_vec[b01_inx].push(step_inx);
                }

                b01_inx += 1;
            } // next b01x

            // Check for matching b10 changes
            let mut b10_inx = b01_len;
            for b10x in b10.iter() {

                if stepx.rule.b10.m_and(b10x).is_not_low() {
                    ret_vec[b10_inx].push(step_inx);
                }

                b10_inx += 1;
            } // next b01x

            step_inx += 1;
        } // next stepx

        ret_vec
    } // end steps_bt_change_bit

    /// Return the result region after running steps from a given initial region
    pub fn _result_from_initial(&self, init_reg: &SomeRegion) -> Option<SomeRegion> {

        let mut cur_reg = init_reg.clone();

        for stpx in self.iter() {
            if cur_reg.intersects(&stpx.initial) {
                let stpy = stpx.restrict_initial_region(&cur_reg);
                cur_reg = stpy.result.clone();
            } else {
                return None;
            }
        } //next stpx

        Some(cur_reg)
    }

    /// Return the initial region by running steps from a given result region
    pub fn _initial_from_result(&self, rslt_reg: &SomeRegion) -> Option<SomeRegion> {

        let mut cur_reg = rslt_reg.clone();

        let mut inx = self.len();
        while inx > 0 {
            inx -= 1;
            let stpx = &self.avec[inx];
            if cur_reg.intersects(&stpx.result) {
                let stpy = stpx.restrict_result_region(&cur_reg);
                cur_reg = stpy.initial.clone();
            } else {
                return None;
            }
        } //next inx
         
        Some(cur_reg)
    }

    /// Return a new Some(StepStore) after restricting the initial region.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Option<Self> {

        let mut rc_steps = StepStore::new_with_capacity(self.len());

        let mut regy = regx.clone();

        for stpx in self.iter() {
            if regy.intersects(&stpx.initial) {
                let stpy = stpx.restrict_initial_region(&regy);
                regy = stpy.result.clone();

                rc_steps.push(stpy);
            } else {
                return None;
            }
        } //next stepx

        Some(rc_steps)
    }

    /// Return a new Some(StepStore) after restricting the result region.
    pub fn restrict_result_region(&self, regx: &SomeRegion) -> Option<Self> {

        let mut rc_steps = StepStore::new_with_capacity(self.len());

        let mut regy = regx.clone();

        for inx in (0..self.len()).rev() {
            let stpx = &self.avec[inx];

            if regy.intersects(&stpx.result) {
                let stpy = stpx.restrict_result_region(&regy);

                regy = stpy.initial.clone();
                //println!("stepstore pushing {}  regy {}", stpy, regy);
                rc_steps.push(stpy);
            //println!("push worked");
            } else {
                //println!("stepstore restrict result {} does not intersect {}", regy, stpx.result);
                return None;
            }
        } //next stepx

        if rc_steps.len() > 1 {
            rc_steps.reverse();
        }

        Some(rc_steps)
    }
    
    pub fn initial(&self) -> SomeRegion {
        assert!(self.len() > 0);
        self[0].initial.clone()
    }

    pub fn result(&self) -> SomeRegion {
        assert!(self.len() > 0);
        self[self.len() - 1].result.clone()
    }

    /// Return true if two stepstores are mutually exclusive.  That is the change of either
    /// must be reversed to use (intersect the initial region) of the other.
    pub fn _mutually_exclusive(&self, other: &StepStore, wanted: &SomeChange) -> bool {

        assert!(self.len() > 0);
        assert!(other.len() > 0);
        
        let self_rule  = self._agg_rule();
        let other_rule = other._agg_rule();
        
        if self_rule.order_bad(&other_rule, wanted) {
            if other_rule.order_bad(&self_rule, wanted) {
                return true;
            }
        }
        
        false
    }

    /// Return true if the steps in a StepStore are all correctly linked,
    /// result-region to initial-region
    pub fn _valid_sequence(&self) -> bool {
        assert!(self.len() > 0);
        
        let mut prev_inx = 0;
        
        for inx in 1..self.len() {
            if self[prev_inx].result != self[inx].initial {
                return false;
            }
            prev_inx += 1;
        }
        true
    }
    
    /// Return a aggregate rule representing the changes in a stepstore
    pub fn _agg_rule(&self) -> SomeRule {

        assert!(self.len() > 0);
        assert!(self._valid_sequence());

        let initial = self.initial();
        let i_0 = initial._zeros_mask();
        let i_1 = initial._ones_mask();
        let i_x = initial.x_mask();
        
        let result  = self.result();
        let r_0 = result._zeros_mask();
        let r_1 = result._ones_mask();
        let r_x = result.x_mask();

        let b00 = i_0.m_and(&r_0);
        let b01 = i_0.m_and(&r_1);
        let b11 = i_1.m_and(&r_1);
        let b10 = i_1.m_and(&r_0);
        let bx0 = i_x.m_and(&r_0);
        let bx1 = i_x.m_and(&r_1);
        let bxx = i_x.m_and(&r_x);
        
        SomeRule {
            b00: b00.m_or(&bx0).m_or(&bxx),

            b01: b01.m_or(&bx1),

            b11: b11.m_or(&bx1).m_or(&bxx),

            b10: b10.m_or(&bx0),
        }
    }

} // end impl StepStore

impl Index<usize> for StepStore {
    type Output = SomeStep;
    fn index<'a>(&'a self, i: usize) -> &'a SomeStep {
        &self.avec[i]
    }
}

impl Clone for StepStore {
    fn clone(&self) -> Self {
        let mut rcstp = Self::new_with_capacity(self.len());
        for stpx in self.avec.iter() {
            rcstp.push(stpx.clone());
        }
        rcstp
    }
}
