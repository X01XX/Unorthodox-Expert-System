//! The Plan struct.
//!
//! A StepStore is zero, or more, steps that may not be related.
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
use serde::{Deserialize, Serialize};
use std::ops::Index;
use std::slice::Iter;

use std::fmt;

impl fmt::Display for SomePlan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            &self.steps.formatted_string(&format!("P:{}", &self.dom_num))
        )
    }
}

#[readonly::make]
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SomePlan {
    /// Domain indicator
    pub dom_num: usize,
    /// A StepStore instance.
    pub steps: StepStore, // Do some steps
}

impl SomePlan {
    /// Return a new, empty, plan.
    pub fn new(dom_num: usize, stepvec: Vec<SomeStep>) -> Self {
        Self {
            dom_num,
            steps: StepStore::new(stepvec),
        }
    }

    /// Return a plan after restricting the resilt region.
    pub fn restrict_result_region(&self, regx: &SomeRegion) -> Option<Self> {
        if self.is_empty() || !regx.intersects(self.result_region()) {
            return None;
        }

        let mut steps = StepStore::with_capacity(self.len());

        let mut cur_reg = regx.clone();

        for inx in (0..self.len()).rev() {
            let stpx = &self.steps[inx];

            if !cur_reg.intersects(&stpx.result) {
                return None;
            }

            let stpy = stpx.restrict_result_region(&cur_reg);

            cur_reg = stpy.initial.clone();

            steps.push(stpy);
        } //next inx

        if steps.len() > 1 {
            steps.reverse();
        }

        Some(Self {
            dom_num: self.dom_num,
            steps,
        })
    }

    /// Return a plan after restricting its initial region.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Option<Self> {
        if self.is_empty() || !regx.intersects(self.initial_region()) {
            return None;
        }

        let mut steps = StepStore::with_capacity(self.len());

        let mut cur_reg = regx.clone();

        for stepx in self.iter() {
            if !cur_reg.intersects(&stepx.initial) {
                return None;
            }

            let stepy = stepx.restrict_initial_region(&cur_reg);

            cur_reg = stepy.result.clone();

            steps.push(stepy);
        } //next stepx

        Some(Self {
            dom_num: self.dom_num,
            steps,
        })
    }

    /// Append a plan to another plan.
    pub fn append(&mut self, val: &mut SomePlan) {
        self.steps.append(&mut val.steps); // empties val.avec
    }

    /// Add a step to a SomePlan.
    pub fn push(&mut self, stepx: SomeStep) {
        if self.is_not_empty() {
            assert!(self.result_region() == &stepx.initial);
        }
        self.steps.push(stepx);
    }

    /// Return the result of linking two plans together, that are known to have a result/initial intersection.
    pub fn link(&self, other: &Self) -> Option<Self> {
        // Sanity checks
        if self.is_empty() || other.is_empty() || self.dom_num != other.dom_num {
            return None;
        }

        // Restrict the StepStores, forward and backward.
        let Some(regx) = self.result_region().intersection(other.initial_region()) else { return None; };

        let Some(mut steps1) = self.restrict_result_region(&regx) else { return None; };

        let Some(mut steps2) = other.restrict_initial_region(&regx) else { return None; };

        steps1.append(&mut steps2);

        //println!("stepstore:link: 2 {} and {} giving {}", self, other, rc_steps);
        Some(steps1)
    } // end link

    /// Return a plan after checking for shortcuts.
    /// Return None if no shortcuts found.
    pub fn shortcuts(&self) -> Option<Self> {
        if let Some(mut planx) = self.shortcuts2() {
            // Check for one shortcut after another
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
    fn shortcuts2(&self) -> Option<Self> {
        if self.len() == 1 {
            return None;
        }

        // Check for repeating Action needs: Noneinitial region
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
            if !found {
                reg_inx.push((initx, vec![inx]));
            }
        } // next inx

        // Process one shortcut
        if reg_inx.len() < self.len() {
            //println!("shortcut initial found for {}", self);
            for tupx in reg_inx.iter() {
                if tupx.1.len() > 1 {
                    //println!("{} at {:?}", tupx.0, tupx.1);
                    let mut steps2 = SomePlan::new(self.dom_num, vec![]);

                    for (inx, stepx) in self.iter().enumerate() {
                        if inx < tupx.1[0] || inx >= tupx.1[1] {
                            steps2.push(stepx.clone());
                        }
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
            if !found {
                reg_inx.push((rsltx, vec![inx]));
            }
        } // next inx

        // Process one shortcut
        if reg_inx.len() < self.len() {
            //println!("shortcut result found for {}", self);
            for tupx in reg_inx.iter() {
                if tupx.1.len() > 1 {
                    //println!("{} at {:?}", tupx.0, tupx.1);
                    let mut steps2 = SomePlan::new(self.dom_num, vec![]);

                    for (inx, stepx) in self.iter().enumerate() {
                        if inx <= tupx.1[0] || inx > tupx.1[1] {
                            steps2.push(stepx.clone());
                        }
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
        let mut rs = format!("P:{}[", self.dom_num);

        let mut flg = 0;
        for stpx in self.steps.iter() {
            if flg == 0 {
                flg = 1;
            } else {
                rs.push(',');
            }
            rs.push_str(&format!("{}", stpx.act_num));
        }
        rs.push(']');
        rs
    }

    /// Return the number of steps in a plan.
    pub fn len(&self) -> usize {
        self.steps.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.steps.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.steps.is_empty()
    }

    /// Return a step iterator.
    pub fn iter(&self) -> Iter<SomeStep> {
        self.steps.iter()
    }

    /// Return the initial region of a plan that contains at least one step.
    pub fn initial_region(&self) -> &SomeRegion {
        assert!(self.is_not_empty());
        &self.steps[0].initial
    }

    /// Return the result region of a plan that contains at least one step.
    pub fn result_region(&self) -> &SomeRegion {
        assert!(self.is_not_empty());
        &self[self.len() - 1].result
    }

    /// Return true if two plans are equal.
    pub fn eq(&self, other: &SomePlan) -> bool {
        if self.len() != other.len() {
            return false;
        }
        if self.dom_num != other.dom_num {
            return false;
        }
        if self.is_empty() {
            return true;
        }
        if self[0].initial != other[0].initial {
            return false;
        }
        for (stpx, stpy) in self.iter().zip(other.iter()) {
            if stpx.result != stpy.result {
                return false;
            }
        }
        true
    }

    /// Return the region encompassed by the path of a plan.
    pub fn path_region(&self) -> Option<SomeRegion> {
        if self.is_empty() {
            return None;
        }

        let mut ret_reg = self.initial_region().clone();
        for stpx in self.iter() {
            ret_reg = ret_reg.union(&stpx.result);
        }
        Some(ret_reg)
    }

    /// Return a string representing a vector of SomePlan.
    pub fn vec_string(avec: &[SomePlan]) -> String {
        let mut rc_str = String::new();
        rc_str.push('[');

        for (inx, planx) in avec.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", planx));
        }

        rc_str.push(']');

        rc_str
    }
} // end impl SomePlan

impl Index<usize> for SomePlan {
    type Output = SomeStep;
    fn index(&self, i: usize) -> &SomeStep {
        &self.steps[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rule::SomeRule;

    #[test]
    fn path_region() -> Result<(), String> {
        let planx = SomePlan::new(0, vec![]);
        if let Some(regx) = planx.path_region() {
            return Err(format!("Empty plan returns a region {}?", regx));
        }

        let step0 = SomeStep::new(
            0,
            SomeRule::new_from_string(1, "00/01/01/11").unwrap(),
            false,
            SomeRegion::new_from_string(1, "rXXXX").unwrap(),
        ); // 1 -> 7
        let step1 = SomeStep::new(
            0,
            SomeRule::new_from_string(1, "01/11/10/11").unwrap(),
            false,
            SomeRegion::new_from_string(1, "rXXXX").unwrap(),
        ); // 7 -> D
        let planx = SomePlan::new(0, vec![step0, step1]);
        println!("plan: {planx}");
        let Some(regx) = planx.path_region() else { panic!("Plan region is None?"); };
        println!("Plan region: {regx}");
        assert!(regx == SomeRegion::new_from_string(1, "rXXX1")?);

        Ok(())
    }

    // Test the link function. This also tests the len, push, result_region, initial_region,
    // restrict_initial_region and restrict_result_region functions.
    #[test]
    fn link() -> Result<(), String> {
        let reg1 = SomeRegion::new_from_string(1, "r0x0x")?;
        let reg2 = SomeRegion::new_from_string(1, "r0x1x")?;
        let reg3 = SomeRegion::new_from_string(1, "r1x1x")?;
        let reg4 = SomeRegion::new_from_string(1, "r111x")?;
        let reg5 = SomeRegion::new_from_string(1, "r101x")?;
        let reg6 = SomeRegion::new_from_string(1, "r000x")?;

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
        let stp_str1 = SomePlan::new(0, vec![step1, step2]);

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
        let stp_str2 = SomePlan::new(0, vec![step4, step5]);

        println!("stp1 {}", &stp_str1);
        println!("stp2 {}", &stp_str2);

        let Some(stp_str3) = stp_str1.link(&stp_str2) else { return Err("Link error?".to_string()); };
        println!("stp3 {}", &stp_str3);
        assert!(stp_str3.len() == 4);

        assert!(*stp_str3.initial_region() == SomeRegion::new_from_string(1, "r010x")?);

        assert!(*stp_str3.result_region() == reg6);

        Ok(())
    }

    #[test]
    fn shortcuts() -> Result<(), String> {
        let reg0 = SomeRegion::new_from_string(1, "r0000")?;
        let reg1 = SomeRegion::new_from_string(1, "r0001")?;
        let reg3 = SomeRegion::new_from_string(1, "r0011")?;
        let reg5 = SomeRegion::new_from_string(1, "r0101")?;
        let reg7 = SomeRegion::new_from_string(1, "r0111")?;

        let step1 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg1, &reg3),
            false,
            reg1.clone(),
        );

        let step2 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg3, &reg7),
            false,
            reg3.clone(),
        );

        let step3 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg7, &reg5),
            false,
            reg7.clone(),
        );

        let step4 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg5, &reg1),
            false,
            reg5.clone(),
        );

        let step5 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg1, &reg0),
            false,
            reg5.clone(),
        );

        let pln1 = SomePlan::new(0, vec![step1, step2, step3, step4, step5]);
        println!("pln1: {}", pln1);

        if let Some(pln1s) = pln1.shortcuts() {
            println!("shortcut found: {}", pln1s);
            assert_eq!(pln1s.len(), 1);
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
            assert!(pln1s.len() == 1);
        } else {
            return Err(format!("No shortcut found for {}?", pln1).to_string());
        }

        let step1 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg1, &reg3),
            false,
            reg1.clone(),
        );

        let step2 = SomeStep::new(
            0,
            SomeRule::region_to_region(&reg3, &reg7),
            false,
            reg3.clone(),
        );

        let pln2 = SomePlan::new(0, vec![step1, step2]);
        println!("pln2: {pln2}");
        assert!(pln2.shortcuts().is_none());

        Ok(())
    }
}
