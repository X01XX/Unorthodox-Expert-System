// Implement a Plan structure, for an Unorthodox Expert System
//
// A StepStore is zero of more steps that may not be related.
//
// A Plan uses a StepStore but enforces relatedness,
// where the result of one step is equal to the initial region
// of the next step.
//
// A finished plan can be considered to be a "forward chaining" plan from
// a given region (often a state, or a region with no X-bit positions) to
// an end-region.
//
// This is often a "pre-positioning", to change the current state to a state where a sample
// is needed.  The final sample taken is not part of the plan, at least so far.

use crate::region::SomeRegion;
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use std::slice::Iter;

use std::fmt;

impl fmt::Display for SomePlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.steps.formatted_string("P"))
    }
}

#[derive(Debug)]
pub struct SomePlan {
    pub steps: StepStore, // Do some steps
}

impl SomePlan {
    pub fn new(stpt: StepStore) -> Self {
        if stpt.len() > 1 {
            let mut last_step = &stpt[0];

            for inx in 1..stpt.len() {
                let stpx = &stpt[inx];

                // Use of the link function changes intersecting conections into eq conections
                if last_step.result != stpx.initial {
                    panic!(
                        "for steps {}, result {} does not equal {}",
                        stpt, last_step.result, stpx.initial
                    );
                }

                last_step = &stpx;
            }
        }

        Self { steps: stpt }
    }

    pub fn str_terse(&self) -> String {
        let mut rs = String::from("P[");

        let mut flg = 0;
        for stpx in self.steps.iter() {
            if flg == 0 {
                flg = 1;
            } else {
                rs.push_str(",");
            }
            rs.push_str(&format!("{}", stpx.act_num));
        }
        rs.push_str("]");
        rs
    }

    pub fn new_step(stpx: SomeStep) -> Self {
        let mut stps = StepStore::new_with_capacity(1);
        stps.push(stpx);
        Self { steps: stps }
    }

    pub fn len(&self) -> usize {
        self.steps.len()
    }

    // Add a step, verify the steps are compatible
    //    fn add(&mut self, val: SomeStep) {
    //        if self.steps.len() > 0 {
    //            if self.steps[self.steps.len() - 1].result == val.initial {
    //                panic!("initial state of step ne previous result")
    //            }
    //        }
    //
    //        self.steps.add(val);
    //    }

    //    pub fn str_terse(&self) -> String {
    //        let mut rc_str = String::from("[");
    //
    //        rc_str.push_str(&self.steps.str_terse());
    //
    //        rc_str.push(']');
    //
    //        rc_str
    //    }

    pub fn iter(&self) -> Iter<SomeStep> {
        self.steps.iter()
    }

    // Link two Plans together, return Some(SomePlan)
    // Return None if the link fails
    pub fn link(&self, other: &Self) -> Option<Self> {
        let end_inx = self.len() - 1;

        if self.steps[end_inx].result == other.steps[0].initial {
            let mut rc_steps = StepStore::new_with_capacity(self.len() + other.len());

            for stp1 in self.steps.iter() {
                rc_steps.push(stp1.clone());
            }

            for stp2 in other.steps.iter() {
                rc_steps.push(stp2.clone());
            }

            return Some(SomePlan::new(rc_steps));
        }

        if self.steps[end_inx]
            .result
            .intersects(&other.steps[0].initial)
        {
            let regx = self.steps[end_inx]
                .result
                .intersection(&other.steps[0].initial);

            if let Some(plan1) = self.restrict_result_region(&regx) {
                if let Some(plan2) = other.restrict_initial_region(&regx) {
                    let mut rc_steps = StepStore::new_with_capacity(self.len() + other.len());

                    for stp1 in plan1.steps.iter() {
                        rc_steps.push(stp1.clone());
                    }

                    for stp2 in plan2.steps.iter() {
                        rc_steps.push(stp2.clone());
                    }

                    return Some(SomePlan::new(rc_steps));
                }
            }
        }
        None
    }

    // Return a new Some(SomePlan) after restricting the initial region.
    // Return None if the restriction fails.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Option<Self> {
        let mut rc_steps = StepStore::new_with_capacity(self.len());

        let mut regy = regx.clone();

        for stpx in self.steps.iter() {
            if regy.intersects(&stpx.initial) {
                let stpy = stpx.restrict_initial_region(&regy);
                regy = stpy.result.clone();

                rc_steps.push(stpy);
            } else {
                return None;
            }
        } //next stepx

        Some(Self::new(rc_steps))
    }

    // Return a new Some(SomePlan) after restricting the result region.
    // Return None if the restriction fails.
    pub fn restrict_result_region(&self, regx: &SomeRegion) -> Option<Self> {
        let mut rc_steps = StepStore::new_with_capacity(self.len());

        let mut regy = regx.clone();

        for inx in (0..self.len()).rev() {
            let stpx = &self.steps[inx];

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

        // Check for X->0 or X->1 bits,
        // that result in 0->X, 1->X step bits
        let mut last_step = &rc_steps[0];
        let mut found = false;
        for inx in 1..rc_steps.len() {
            let stpx = &rc_steps[inx];

            if last_step.result != stpx.initial {
                if last_step.result.intersects(&stpx.initial) {
                    found = true;
                    println!("found - true! result {} and initial {}", &last_step, &stpx);
                } else {
                    panic!(
                        "for steps {}, result {} does not equal {}",
                        rc_steps, last_step.result, stpx.initial
                    );
                }
            }

            last_step = &stpx;
        }

        if found {
            // Correct X->0 or X->1 bits,
            // that result in 0->X, 1->X step bits, with restrict_initial
            let regx = rc_steps[0].initial.clone();
            return SomePlan { steps: rc_steps }.restrict_initial_region(&regx);
        }

        Some(Self::new(rc_steps))
    }

    pub fn initial_region(&self) -> &SomeRegion {
        return &self.steps[0].initial;
    }

    pub fn result_region(&self) -> &SomeRegion {
        return &self.steps[self.steps.len() - 1].result;
    }

    // Return a new plan if short-cuts found
    // A short cut is found by finding the same initial region for
    // two steps.
    pub fn short_cuts(&self) -> Option<SomePlan> {
        // Most plans will be checked and None will be returned
        let inx_vec = self.steps.same_intitial();

        if inx_vec.len() == 0 {
            return None;
        }

        // Create first shortcut from self
        let mut rc_steps = StepStore::new_with_capacity(self.steps.len() + inx_vec[0] - inx_vec[1]);

        let mut x = 0;
        for _stpx in self.steps.iter() {
            if x < inx_vec[0] || x >= inx_vec[1] {
                rc_steps.push(self.steps[x].clone());
            }
            x = x + 1;
        }

        // Create more shortcuts from rc_steps if needed
        loop {
            let inx_vec = rc_steps.same_intitial();

            // If no shortcut, return a plan with the current rc_steps
            if inx_vec.len() == 0 {
                return Some(SomePlan::new(rc_steps));
            }

            // Create the next shortcut
            let mut rcx_steps =
                StepStore::new_with_capacity(rc_steps.len() + inx_vec[0] - inx_vec[1]);

            let mut x = 0;
            for _stpx in rc_steps.iter() {
                if x < inx_vec[0] || x >= inx_vec[1] {
                    rcx_steps.push(rc_steps[x].clone());
                }
                x = x + 1;
            }

            // Prepare to run loop again
            rc_steps = rcx_steps;
        }
    } // end fn short_cuts
} // end impl SomePlan

impl Clone for SomePlan {
    fn clone(&self) -> Self {
        Self {
            steps: self.steps.clone(),
        }
    }
}
