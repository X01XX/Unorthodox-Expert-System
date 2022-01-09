//! The Plan struct.  A StepStore struct where each step leads to the next.
//!
//! A StepStore is zero or more steps that may not be related.
//!
//! A Plan uses a StepStore but enforces relatedness,
//! where the result of one step is equal to the initial region
//! of the next step.
//!
//! A finished plan can be considered to be a "forward chaining" plan from
//! a given region (often a state, or a region with no X-bit positions) to
//! an end-region.
//!
//! This is often a "pre-positioning", to change the current state to a state where a sample
//! is needed.  The final sample taken is not part of the plan, at least so far.
//!
//! An empty plan means that the current state satisfies the need the plan is for, just
//! sample the current state.

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
    /// A StepStore instance.
    steps: StepStore, // Do some steps
}

impl SomePlan {
    /// Return a new plan, using a given StepStore.
    /// Check the steps to insure one leads to the next.
    /// The StepStore may be empty.
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

    /// Return a string of action numbers to represent a plan.
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

    /// Return the number of steps in a plan.
    pub fn len(&self) -> usize {
        self.steps.len()
    }

    /// Return a step iterator.
    pub fn iter(&self) -> Iter<SomeStep> {
        self.steps.iter()
    }

    /// Return the result region of a plan that contains at least one step.
    pub fn result_region(&self) -> &SomeRegion {
        return &self.steps[self.steps.len() - 1].result;
    }

    pub fn str2(&self) -> String {
        if self.steps.len() == 0 {
            return String::from("Empty plan");
        }

        //let max_dif = self.initial_region.diff_mask(&self.result_region);

        let mut rc_str = String::new();
        let inx_end = self.steps.len() - 1;
        for stpx in self.steps.iter() {
            let df = stpx.initial.diff_mask(&stpx.result);

            let dif_bits = stpx.initial.diff_mask(&self.steps[inx_end].result);
            let dif_num = dif_bits.num_one_bits();

            let dif_bits2 = stpx.result.diff_mask(&self.steps[inx_end].result);
            let dif_num2 = dif_bits2.num_one_bits();
    
            rc_str.push_str(&format!(
                "{} Action {:02} Group {} Rule {} #dif: {} {} to #dif: {} {}\n{}\n",
                &stpx.initial,
                &stpx.act_num,
                &stpx.group_reg,
                &stpx.rule,
                &dif_num,
                &dif_bits,
                &dif_num2,
                &dif_bits2,
                &df.str2()
            ));
        }

        let x = self.steps.len() - 1;
        rc_str.push_str(&format!("{}", &self.steps[x].result));

        rc_str
    }
} // end impl SomePlan

impl Clone for SomePlan {
    fn clone(&self) -> Self {
        Self {
            steps: self.steps.clone(),
        }
    }
}
