#![allow(dead_code, unused_imports)]
//! The PlansCorrStore struct.
//!
//! A vector of PlansCorr with interlocking result plans and initial plans.
//!
use crate::planscorr::PlansCorr;
use crate::regionscorr::RegionsCorr;
use crate::statescorr::StatesCorr;
use crate::step::{AltRuleHint, SomeStep};
use crate::tools::{self, StrLen};

use serde::{Deserialize, Serialize};
use std::fmt;
use std::slice::Iter;

impl fmt::Display for PlansCorrStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} value {}", tools::vec_string(&self.items), self.value)
    }
}

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct PlansCorrStore {
    /// A vector of PlansCorr.
    pub items: Vec<PlansCorr>,
    pub value: isize,
}

impl PlansCorrStore {
    /// Return a new, PlansCorrStore.
    pub fn new(items: Vec<PlansCorr>) -> Self {
        let ret_plnsc = Self { items, value: 0 };
        assert!(ret_plnsc.is_valid());
        ret_plnsc
    }

    /// Return the number of plans.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Set the PlansCorrStore value.
    pub fn set_value(&mut self, value: isize) {
        self.value = value;
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Return true if a PlansCorrStore is valid sequence.
    pub fn is_valid(&self) -> bool {
        let mut last_plnscx: Option<&PlansCorr> = None;

        for plnscx in self.iter() {
            if let Some(plnsc_before) = last_plnscx {
                if plnsc_before.result_regions() != plnscx.initial_regions() {
                    return false;
                }
            }
            last_plnscx = Some(plnscx);
        }
        true
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<PlansCorr> {
        self.items.iter()
    }

    /// Return the initial regions of a non-empty PlansCorr.
    pub fn initial_regions(&self) -> RegionsCorr {
        debug_assert!(self.is_not_empty());

        self.items.first().expect("SNH").initial_regions()
    }

    /// Return the result regions of a non-empty PlansCorr.
    pub fn result_regions(&self) -> RegionsCorr {
        debug_assert!(self.is_not_empty());

        self.items.last().expect("SNH").result_regions()
    }

    /// Return a PlansCorrStore restricted by initial regions by a given RegionsCorr.
    pub fn restrict_initial_regions(&self, restrict: &RegionsCorr) -> Option<PlansCorrStore> {
        let mut plnsc_vec = Vec::<PlansCorr>::with_capacity(self.len());

        let mut last_initial = restrict.clone();

        for plnscx in self.items.iter() {
            if plnscx.initial_regions().intersects(&last_initial) {
                if let Some(plnscy) = plnscx.restrict_initial_regions(&last_initial) {
                    last_initial = plnscy.result_regions();
                    plnsc_vec.push(plnscy);
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        Some(Self::new(plnsc_vec))
    }

    /// Return a PlansCorrStore restricted by result regions by a given RegionsCorr.
    pub fn restrict_result_regions(&self, restrict: &RegionsCorr) -> Option<PlansCorrStore> {
        let mut plnsc_vec = Vec::<PlansCorr>::with_capacity(self.len());

        let mut last_result = restrict.clone();

        for plnscx in self.items.iter().rev() {
            if plnscx.result_regions().intersects(&last_result) {
                if let Some(plnscy) = plnscx.restrict_result_regions(&last_result) {
                    last_result = plnscy.initial_regions();
                    plnsc_vec.push(plnscy);
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        plnsc_vec.reverse();
        Some(Self::new(plnsc_vec))
    }

    /// Return a PlansCorrStorr linked to another.
    pub fn link(&self, other: &Self) -> Option<Self> {
        //println!("planscorrstore::link {self} to {other}");
        if self.is_empty() {
            return Some(other.clone());
        }
        let result_regs = self.result_regions();
        let initial_regs = other.initial_regions();

        if result_regs.intersects(&initial_regs) {
            if let Some(mut plnscx) = self.restrict_result_regions(&initial_regs) {
                if let Some(mut plnscy) = other.restrict_initial_regions(&result_regs) {
                    plnscx.items.append(&mut plnscy.items);
                    return Some(plnscx);
                }
            }
        }
        None
    }

    /// Return a more restricted display version of a PlansCorrStore.
    pub fn str_terse(&self) -> String {
        let mut rc_str = String::new();

        rc_str.push('[');
        for (inx, planscx) in self.items.iter().enumerate() {
            if inx > 0 {
                rc_str.push_str(", (");
            } else {
                rc_str.push('(');
            }

            let mut cnt = 0;
            for planx in planscx.iter() {
                if planx.causes_change() {
                    if cnt > 0 {
                        rc_str.push_str(", ");
                    }
                    rc_str.push_str(&planx.str_terse());
                    cnt += 1;
                }
            }
            rc_str.push(')');
        }
        rc_str.push(']');

        rc_str
    }

    /// Return the expected result, given ititial regions.
    pub fn result_from_initial_regions(&self, regions: &RegionsCorr) -> Option<RegionsCorr> {
        let mut cur_regs = regions.clone();
        for planscx in self.items.iter() {
            if let Some(next_regs) = planscx.result_from_initial_regions(&cur_regs) {
                cur_regs = next_regs;
            }
        }
        Some(cur_regs)
    }

    /// Return number bits changed running plans in store.
    pub fn num_bits_changed(&self) -> usize {
        let mut numc = 0;
        for planscx in self.iter() {
            numc += planscx.num_bits_changed();
        }
        numc
    }

    /// Return the number of steps to run for a PlansCorrStore.
    pub fn number_steps_to_run(&self) -> usize {
        let mut numr = 0;
        for planscx in self.iter() {
            numr += planscx.number_steps_to_run();
        }
        numr
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::SomePlan;
    use crate::region::SomeRegion;
    use crate::regionscorr::RegionsCorr;
    use crate::rule::SomeRule;
    use crate::step::{AltRuleHint, SomeStep};

    #[test]
    fn new() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("00/X0/10")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        let plnscstr = PlansCorrStore::new(vec![plnsc1]);
        println!("plnscstr {plnscstr}");

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn initial_regions() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("00/X0/10")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        let plnscstr = PlansCorrStore::new(vec![plnsc1]);
        println!("plnscstr {plnscstr}");

        let initial_regs = plnscstr.initial_regions();
        println!("initial_regs {initial_regs}");

        assert!(
            initial_regs
                == RegionsCorr::new(vec![
                    SomeRegion::new_from_string("r0X")?,
                    SomeRegion::new_from_string("r0X1")?
                ])
        );

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn result_regions() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("00/X0/10")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        let plnscstr = PlansCorrStore::new(vec![plnsc1]);
        println!("plnscstr {plnscstr}");

        let result_regs = plnscstr.result_regions();
        println!("result_regs {result_regs}");

        assert!(
            result_regs
                == RegionsCorr::new(vec![
                    SomeRegion::new_from_string("r00")?,
                    SomeRegion::new_from_string("r000")?
                ])
        );

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn restrict_initial_regions() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("Xx/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("X0/Xx/X0")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        let plnscstr = PlansCorrStore::new(vec![plnsc1]);
        println!("plnscstr {plnscstr}");

        let restrict_regs = RegionsCorr::new(vec![
            SomeRegion::new_from_string("r00")?,
            SomeRegion::new_from_string("r000")?,
        ]);

        if let Some(restricted) = plnscstr.restrict_initial_regions(&restrict_regs) {
            println!("resricted {restricted}");
            let result_regs = restricted.result_regions();
            assert!(
                result_regs
                    == RegionsCorr::new(vec![
                        SomeRegion::new_from_string("r10")?,
                        SomeRegion::new_from_string("r010")?
                    ])
            );
            assert!(
                restricted.initial_regions()
                    == RegionsCorr::new(vec![
                        SomeRegion::new_from_string("r00")?,
                        SomeRegion::new_from_string("r000")?
                    ])
            )
        } else {
            return Err("restrict_initial_regs failed".to_string());
        }

        // assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn restrict_result_regions() -> Result<(), String> {
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("Xx/X0")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            1,
            SomeRule::new_from_string("X0/Xx/X0")?,
            AltRuleHint::NoAlt {},
        );

        let plnsc1 = PlansCorr::new(vec![
            SomePlan::new(0, vec![stp1]),
            SomePlan::new(1, vec![stp2]),
        ]);
        println!("{plnsc1}");

        let plnscstr = PlansCorrStore::new(vec![plnsc1]);
        println!("plnscstr {plnscstr}");

        let restrict_regs = RegionsCorr::new(vec![
            SomeRegion::new_from_string("r0X")?,
            SomeRegion::new_from_string("r00X")?,
        ]);

        if let Some(restricted) = plnscstr.restrict_result_regions(&restrict_regs) {
            println!("resricted {restricted}");
            let initial_regs = restricted.initial_regions();
            println!("initial regs {initial_regs}");
            assert!(
                initial_regs
                    == RegionsCorr::new(vec![
                        SomeRegion::new_from_string("r1X")?,
                        SomeRegion::new_from_string("rX1X")?
                    ])
            );
            assert!(
                restricted.result_regions()
                    == RegionsCorr::new(vec![
                        SomeRegion::new_from_string("r00")?,
                        SomeRegion::new_from_string("r000")?
                    ])
            )
        } else {
            return Err("restrict_result_regs failed".to_string());
        }

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn link() -> Result<(), String> {
        // Set up first PlansCorrStore.
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/00/01/XX")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            0,
            SomeRule::new_from_string("00/01/11/XX")?,
            AltRuleHint::NoAlt {},
        );
        let plnsc1 = PlansCorr::new(vec![SomePlan::new(0, vec![stp1, stp2])]);
        let plnscstr1 = PlansCorrStore::new(vec![plnsc1]);

        // Set up second PlansCorrStore.
        let stp1 = SomeStep::new(
            0,
            SomeRule::new_from_string("XX/11/10/11")?,
            AltRuleHint::NoAlt {},
        );
        let stp2 = SomeStep::new(
            0,
            SomeRule::new_from_string("XX/11/00/10")?,
            AltRuleHint::NoAlt {},
        );
        let plnsc1 = PlansCorr::new(vec![SomePlan::new(0, vec![stp1, stp2])]);
        let plnscstr2 = PlansCorrStore::new(vec![plnsc1]);

        println!(
            "plnscstr1 results {} plnscstr2 initial {}",
            plnscstr1.result_regions(),
            plnscstr2.initial_regions()
        );

        // Calc link.
        if let Some(plnscstr3) = plnscstr1.link(&plnscstr2) {
            println!("plnscstr3 {plnscstr3}");
            assert!(
                plnscstr3.initial_regions()
                    == RegionsCorr::new(vec![SomeRegion::new_from_string("r0001")?])
            );
            assert!(
                plnscstr3.result_regions()
                    == RegionsCorr::new(vec![SomeRegion::new_from_string("r0100")?])
            );
        } else {
            return Err("link failed".to_string());
        }

        //assert!(1 == 2);
        Ok(())
    }
}
