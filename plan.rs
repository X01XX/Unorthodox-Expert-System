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

use std::ops::Index;
use std::slice::Iter;

use std::fmt;

impl fmt::Display for SomePlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.steps.formatted_string("P"))
    }
}

#[readonly::make]
#[derive(Debug, Clone)]
pub struct SomePlan {
    /// A StepStore instance.
    pub steps: StepStore, // Do some steps
}

impl SomePlan {
    /// Return a new plan, using a given StepStore.
    /// Check the steps to insure one leads to the next.
    /// The StepStore may be empty.
    pub fn new() -> Self {
        Self { steps: StepStore::new() }
    }

    pub fn new_with_step(stepx: SomeStep) -> Self {
        Self { steps: StepStore::new_with_step(stepx) }
    }

    pub fn restrict_result_region(&self, regx: &SomeRegion) -> Option<Self> {
        let mut steps = StepStore::with_capacity(self.len());

        let mut regy = regx.clone();

        for inx in (0..self.len()).rev() {
            let stpx = &self.steps[inx];

            if regy.intersects(&stpx.result) {
                let stpy = stpx.restrict_result_region(&regy);

                regy = stpy.initial.clone();
                steps.push(stpy);
            } else {
                return None;
            }
        } //next inx

        if steps.len() > 1 {
            steps.reverse();
        }

        Some(Self { steps })
    }

    /// Check if a plan is a valid sequence of steps.
    /// There are greater than zero steps.
    /// Each step changes something.
    /// Sequential step pairs result and initial regions are equal.
    /// Steps do not form a loop.
    pub fn is_valid_plan(&self) -> bool {
        if self.len() == 0 {
            return false;
        }

        if self.len() > 1 {
            for inx in 1..self.len() {
                // Step changes something.
                if self.steps[inx].initial == self.steps[inx].result {
                    return false;
                }
                // Step is linked with previous step.
                if self.steps[inx - 1].result == self.steps[inx].initial {
                } else {
                    return false;
                }
            }
        }
        // Steps as a whole cause a change.
        if self.initial_region() == self.result_region() {
            return false;
        }
        true
    }

    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Option<Self> {
        let mut steps = StepStore::with_capacity(self.len());

        let mut regy = regx.clone();

        for stepx in self.iter() {
            if regy.intersects(&stepx.initial) {
                let stepy = stepx.restrict_initial_region(&regy);
                regy = stepy.result.clone();

                steps.push(stepy);
            } else {
                return None;
            }
        } //next stepx

        Some(Self { steps })
    }

    /// Append a StepStore to a StepStore.
    pub fn append(&mut self, val: &mut SomePlan) {
        self.steps.append(&mut val.steps); // empties val.avec
    }

    /// Add a step to a StepStore.
    pub fn push(&mut self, stepx: SomeStep) {
        if self.len() > 0 {
            assert!(self.result_region() == &stepx.initial);
        }
        self.steps.push(stepx);
    }

    /// Link two SomePlans together, return Some(SomePlan).
    /// Restrict StepStores that have an intersection of the result and
    /// initial regions.
    /// Restricting the steps, forward and backward, from that intersection may
    /// cause a break in the path, which is why None may be returned.
    pub fn link(&self, other: &Self) -> Option<Self> {
        // Sanity checks
        assert!(self.len() > 0);
        assert!(other.len() > 0);
        assert!(self.result_region().intersects(&other.initial_region()));

        // Restrict the StepStores, forward and backward.
        if self.result_region().intersects(&other.initial_region()) {
            let regx = self.result_region().intersection(&other.initial_region());

            if let Some(mut steps1) = self.restrict_result_region(&regx) {
                if let Some(mut steps2) = other.restrict_initial_region(&regx) {
                    steps1.append(&mut steps2);

                    //println!("stepstore:link: 2 {} and {} giving {}", self, other, rc_steps);
                    if steps1.is_valid_plan() {
                        return Some(steps1);
                    } else {
                        return None;
                    }
                }
            }
        }
        None
    } // end link

    /// Return a plan after checking for shortcuts.
    /// Return None if no shortcuts found.
    pub fn shortcuts(&self) -> Option<Self> {
        if self.len() == 1 {
            return None;
        }

        // CHeck for repeating initial region
        let mut reg_inx = Vec::<(SomeRegion, Vec<usize>)>::new();
        for inx in 0..self.len() {
            let initx = self.steps[inx].initial.clone();
            let mut found = false;
            for reg_inx_tup in reg_inx.iter_mut() {
                if reg_inx_tup.0 == initx {
                    reg_inx_tup.1.push(inx);
                    found = true;
                }
            }
            if found == false {
                reg_inx.push((initx, vec![inx]));
            }
        } // next inx

        // Process one shortcut
        if reg_inx.len() < self.len() {
            let mut steps2: SomePlan;
            //println!("shortcut initial found for {}", self);
            for tupx in reg_inx.iter() {
                if tupx.1.len() > 1 {
                    //println!("{} at {:?}", tupx.0, tupx.1);
                    steps2 = SomePlan::new();
                    let mut inx = 0;
                    for stepx in self.iter() {
                        if inx < tupx.1[0] || inx >= tupx.1[1] {
                            steps2.push(stepx.clone());
                        }
                        inx += 1;
                    }
                    // Remove shortcuts recursively, one by one.
                    //println!("shortcut step2 {}", steps2);
                    if let Some(steps3) = steps2.shortcuts() {
                        return Some(steps3);
                    }

                    return Some(steps2);
                }
            }
        }

        // Check for repeating result
        let mut reg_inx = Vec::<(SomeRegion, Vec<usize>)>::new();
        for inx in 0..self.len() {
            let rsltx = self[inx].result.clone();
            let mut found = false;
            for reg_inx_tup in reg_inx.iter_mut() {
                if reg_inx_tup.0 == rsltx {
                    reg_inx_tup.1.push(inx);
                    found = true;
                }
            }
            if found == false {
                reg_inx.push((rsltx, vec![inx]));
            }
        } // next inx

        // Process one shortcut
        if reg_inx.len() < self.len() {
            let mut steps2: SomePlan;
            //println!("shortcut result found for {}", self);
            for tupx in reg_inx.iter() {
                if tupx.1.len() > 1 {
                    //println!("{} at {:?}", tupx.0, tupx.1);
                    steps2 = SomePlan::new();
                    let mut inx = 0;
                    for stepx in self.iter() {
                        if inx <= tupx.1[0] || inx > tupx.1[1] {
                            steps2.push(stepx.clone());
                        }
                        inx += 1;
                    }
                    // Remove shortcuts recursively, one by one.
                    //println!("shortcut step2 {}", steps2);
                    if let Some(steps3) = steps2.shortcuts() {
                        return Some(steps3);
                    }
                    return Some(steps2);
                }
            }
        }

        None
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

    /// Return the initial region of a plan that contains at least one step.
    pub fn initial_region(&self) -> &SomeRegion {
        assert!(self.len() > 0);
        return &self.steps.initial_region();
    }

    /// Return the result region of a plan that contains at least one step.
    pub fn result_region(&self) -> &SomeRegion {
        assert!(self.len() > 0);
        return &self.steps.result_region();
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

impl Index<usize> for SomePlan {
    type Output = SomeStep;
    fn index<'a>(&'a self, i: usize) -> &'a SomeStep {
        &self.steps[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rule::SomeRule;

    // Test the link function. This also tests the len, push, result, initial, restrict_initial_region and restrict_result_region functions.
    #[test]
    fn link() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string(1, "r0x0x").unwrap();
        let reg2 = SomeRegion::new_from_string(1, "r0x1x").unwrap();
        let reg3 = SomeRegion::new_from_string(1, "r1x1x").unwrap();
        let reg4 = SomeRegion::new_from_string(1, "r111x").unwrap();
        let reg5 = SomeRegion::new_from_string(1, "r101x").unwrap();
        let reg6 = SomeRegion::new_from_string(1, "r000x").unwrap();

        let step1 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg1, &reg2),
            false,
            reg1.clone(),
        );
        let step2 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg2, &reg3),
            false,
            reg2.clone(),
        );
        let mut stp_str1 = SomePlan::new();
        stp_str1.push(step1);
        stp_str1.push(step2);

        let step4 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg4, &reg5),
            false,
            reg4.clone(),
        );
        let step5 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg5, &reg6),
            false,
            reg5.clone(),
        );
        let mut stp_str2 = SomePlan::new();
        stp_str2.push(step4);
        stp_str2.push(step5);

        println!("stp1 {}", &stp_str1);
        println!("stp2 {}", &stp_str2);

        let stp_str3 = stp_str1.link(&stp_str2).unwrap();
        println!("stp3 {}", &stp_str3);
        assert!(stp_str3.len() == 4);
        assert!(stp_str3.initial_region() == &SomeRegion::new_from_string(1, "r010x").unwrap());
        assert!(stp_str3.result_region() == &reg6);

        let stp_str4 = stp_str2.link(&stp_str1).unwrap();
        println!("stp4 {}", &stp_str4);
        assert!(stp_str4.len() == 4);
        assert!(stp_str4.initial_region() == &reg4);
        assert!(stp_str4.result_region() == &SomeRegion::new_from_string(1, "r101x").unwrap());

        Ok(())
    }
}
