// Implement a Plan structure for an Unorthodox Expert System
//
// A StepStore is zero of more steps that may not be related
//
// A Plan uses a StepStore where the steps are related,
// where the result of one step is equal to the initial region
// of the next step.

use crate::region::SomeRegion;
use crate::step::SomeStep;
use crate::stepstore::StepStore;

use std::slice::Iter;

use std::fmt;

impl fmt::Display for SomePlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rc_str = String::from("P");

        rc_str.push_str(&format!("{}", &self.steps));

        write!(f, "{}", rc_str)
    }
}

pub struct SomePlan {
    pub steps: StepStore, // Do some steps
}

impl SomePlan {
    pub fn new(stpt: StepStore) -> Self {
        if stpt.len() > 1 {
            let mut last_step = &stpt[0];

            for inx in 1..stpt.len() {
                let stpx = &stpt[inx];

                // Use interesects instead of equals, due to X->0 and X->1 bits,
                // where restricting the result region does not change the X.
                // if stpy.result.intersects(&stpx.initial) == false
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

    //    pub fn clone(&self) -> Self {
    //        Self {
    //            steps: self.steps.clone(),
    //        }
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
    pub fn short_cuts(&self) -> Option<SomePlan> {
        let mut not_changed = false;

        let mut rc_steps = self.steps.clone();

        loop {
            if not_changed {
                if rc_steps.len() < self.len() {
                    println!("make_plan reg final steps are: {}", &rc_steps);
                    return Some(SomePlan { steps: rc_steps });
                } else {
                    return None;
                }
            }

            not_changed = true;

            let mut inx_reg = Vec::<(usize, SomeRegion)>::new();

            // If a state appears twice, the intervening steps can be skipped.
            //
            // The result region of each step intersects the initial region of
            // the next step, except for the initial region of the first
            // step and the result region of the last step.
            //

            let mut inx = 0;

            let mut new_steps = StepStore::new();

            for stpx in rc_steps.iter() {
                for tupx in inx_reg.iter() {
                    if tupx.1 == stpx.initial {
                        println!(
                            "make_plan initial region {} at {} found twice at {} in {}",
                            &stpx.initial, &inx, &tupx.0, &rc_steps
                        );

                        if tupx.0 > 0 {
                            //println!("first slice is: {:?}", &rc_steps[0..tupx.0]);
                            for tmpx in 0..tupx.0 {
                                new_steps.push(rc_steps[tmpx].clone());
                            }

                            //println!("second slice is: {:?}", &rc_steps[inx..rc_steps.len()]);
                            for tmpx in inx..rc_steps.len() {
                                new_steps.push(rc_steps[tmpx].clone());
                            }
                            println!("new steps is {}", new_steps);
                            not_changed = false;
                            break;
                        } else {
                            //println!("second slice is: {:?}", &rc_steps[inx..rc_steps.len()]);
                            for tmpx in inx..rc_steps.len() {
                                new_steps.push(rc_steps[tmpx].clone());
                            }
                            println!("new steps is {}", new_steps);
                            not_changed = false;

                            break;
                        }
                    }
                }

                if not_changed == false {
                    break;
                }

                inx_reg.push((inx, stpx.initial.clone()));

                inx += 1;
            } // next stpx

            if new_steps.len() > 0 {
                rc_steps = new_steps;
                //not_changed = false;
            }
        } // end loop
    } // end fn short_cuts
}
