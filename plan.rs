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
        Self {
            steps: StepStore::new(),
        }
    }

    /// Return a new plan with one given step.
    pub fn new_with_step(stepx: SomeStep) -> Self {
        Self {
            steps: StepStore::new_with_step(stepx),
        }
    }

    /// Return a plan where the result region of a known, valid, plan is restricted by a region
    /// known to intersect the plans result region.
    pub fn restrict_result_region(&self, regx: &SomeRegion) -> Self {
        assert!(self.len() > 0);
        assert!(regx.intersects(self.result_region()));

        let mut steps = StepStore::with_capacity(self.len());

        let mut cur_reg = regx.clone();

        for inx in (0..self.len()).rev() {
            let stpx = &self.steps[inx];

            let stpy = stpx.restrict_result_region(&cur_reg);

            cur_reg = stpy.initial.clone();

            steps.push(stpy);
        } //next inx

        if steps.len() > 1 {
            steps.reverse();
        }

        Self { steps }
    }

    /// Check if a plan is a valid sequence of steps.
    /// Each step changes something.
    /// Sequential step pairs result and initial regions are equal.
    /// The result appears only once in the chain of steps.
    /// The initial and result regions of the whole plan are not the same.
    pub fn is_valid(&self) -> bool {
        if self.len() == 0 {
            return true;
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

        let rslt = self.result_region();

        // Steps as a whole cause a change.
        if self.initial_region() == rslt {
            return false;
        }

        // Result does not repeat.
        if self.len() > 1 {
            for inx in 0..(self.len() - 1) {
                if self.steps[inx].result == *rslt {
                    return false;
                }
            }
        }

        true
    }

    /// Return a plan where the initial region of a known, valid, plan is restricted by a region
    /// known to intersect the plans initial region.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Self {
        assert!(self.len() > 0);
        assert!(regx.intersects(self.initial_region()));

        let mut steps = StepStore::with_capacity(self.len());

        let mut cur_reg = regx.clone();

        for stepx in self.iter() {
            let stepy = stepx.restrict_initial_region(&cur_reg);

            cur_reg = stepy.result.clone();

            steps.push(stepy);
        } //next stepx

        Self { steps }
    }

    /// Append a StepStore to a StepStore.
    pub fn append(&mut self, val: &mut SomePlan) {
        self.steps.append(&mut val.steps); // empties val.avec
    }

    /// Add a step to a SomePlan.
    pub fn push(&mut self, stepx: SomeStep) {
        if self.len() > 0 {
            assert!(self.result_region() == &stepx.initial);
        }
        self.steps.push(stepx);
    }

    /// Return the result of linking two plans together, that are known to have a result/initial intersection.
    pub fn link(&self, other: &Self) -> Self {
        // Sanity checks
        assert!(self.len() > 0);
        assert!(other.len() > 0);
        assert!(self.result_region().intersects(&other.initial_region()));

        // Restrict the StepStores, forward and backward.
        let regx = self.result_region().intersection(&other.initial_region());

        let mut steps1 = self.restrict_result_region(&regx);
        let mut steps2 = other.restrict_initial_region(&regx);
        steps1.append(&mut steps2);

        //println!("stepstore:link: 2 {} and {} giving {}", self, other, rc_steps);
        return steps1;
    } // end link

    /// Return a plan after checking for shortcuts.
    /// Return None if no shortcuts found.
    pub fn shortcuts(&self) -> Option<Self> {
        if let Some(mut planx) = self.shortcuts2() {
            // First shortcut found, any others?
            loop {
                if let Some(plany) = planx.shortcuts2() {
                    planx = plany;
                } else {
                    return Some(planx);
                }
            }
        }
        None
    }

    /// Return a plan after checking for one shortcut.
    /// Return None if no shortcut found.
    pub fn shortcuts2(&self) -> Option<Self> {
        if self.len() == 1 {
            return None;
        }

        // Check for repeating initial region
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
        &self.steps[0].initial
    }

    /// Return the result region of a plan that contains at least one step.
    pub fn result_region(&self) -> &SomeRegion {
        assert!(self.len() > 0);
        &self[self.len() - 1].result
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

    // Test the link function. This also tests the len, push, result_region, initial_region,
    // restrict_initial_region and restrict_result_region functions.
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

        let stp_str3 = stp_str1.link(&stp_str2);
        println!("stp3 {}", &stp_str3);
        if stp_str3.len() != 4 {
            return Err(format!("Len NE 4? {}", stp_str3).to_string());
        }
        let reg45 = SomeRegion::new_from_string(1, "r010x").unwrap();
        if stp_str3.initial_region() != &reg45 {
            return Err(format!(
                "initial {} NE {} ?",
                stp_str3.initial_region(),
                reg45
            ));
        }
        if stp_str3.result_region() != &reg6 {
            return Err(format!(
                "initial {} NE {} ?",
                stp_str3.result_region(),
                reg6
            ));
        }

        // Result 101X appears twice when linked in this order, so its invalid.
        let stp_str4 = stp_str2.link(&stp_str1);
        if stp_str4.is_valid() {
            return Err(format!("stp4 {} valid?", &stp_str4).to_string());
        } else {
            println!("Link not valid, as expected");
        }

        Ok(())
    }

    #[test]
    fn shortcuts() -> Result<(), String> {
        let mut pln1 = SomePlan::new();

        let reg0 = SomeRegion::new_from_string(1, "r0000").unwrap();
        let reg1 = SomeRegion::new_from_string(1, "r0001").unwrap();
        let reg3 = SomeRegion::new_from_string(1, "r0011").unwrap();
        let reg5 = SomeRegion::new_from_string(1, "r0101").unwrap();
        let reg7 = SomeRegion::new_from_string(1, "r0111").unwrap();

        let step1 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg1, &reg3),
            false,
            reg1.clone(),
        );
        pln1.push(step1);

        let step2 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg3, &reg7),
            false,
            reg3.clone(),
        );
        pln1.push(step2);

        let step3 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg7, &reg5),
            false,
            reg7.clone(),
        );
        pln1.push(step3);

        let step4 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg5, &reg1),
            false,
            reg5.clone(),
        );
        pln1.push(step4);

        let step5 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg1, &reg0),
            false,
            reg5.clone(),
        );
        pln1.push(step5);

        println!("pln1: {}", pln1);

        if let Some(pln1s) = pln1.shortcuts() {
            println!("shortcut found: {}", pln1s);
            assert!(pln1s.len() == 1);
            if pln1s.len() != 1 {
                return Err(format!("Len NE 1? {}", pln1s).to_string());
            }
            if pln1s.initial_region() != &reg1 {
                return Err(format!(
                    "Initial region {} NE {} ?",
                    pln1s.initial_region(),
                    reg1
                ));
            }
            if pln1s.result_region() != &reg0 {
                return Err(format!(
                    "result region {} NE {} ?",
                    pln1s.result_region(),
                    reg0
                ));
            }
        } else {
            return Err(format!("No shortcut found for {}?", pln1).to_string());
        }

        let mut pln2 = SomePlan::new();

        let step1 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg1, &reg3),
            false,
            reg1.clone(),
        );
        pln2.push(step1);

        let step2 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg3, &reg7),
            false,
            reg3.clone(),
        );
        pln2.push(step2);

        if let Some(pln2s) = pln2.shortcuts() {
            return Err(format!("shortcut found for {} is {} ?", pln2, pln2s).to_string());
        }
        println!("no shortcut found for pln2");

        Ok(())
    }
}
