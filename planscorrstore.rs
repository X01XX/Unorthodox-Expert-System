//! The PlansCorrStore struct.
//!
//! A vector of linked PlansCorr instances.
//!
use crate::planscorr::PlansCorr;
use crate::regionscorr::RegionsCorr;
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;
use unicode_segmentation::UnicodeSegmentation;

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
        for inx in (0..self.len()).skip(1) {
            if !self[inx - 1].is_linked(&self[inx]) {
                return false;
            }
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
    pub fn link(&self, other: &Self) -> Result<Self, String> {
        //println!("planscorrstore::link {self} to {other}");
        if self.is_empty() {
            return Ok(other.clone());
        }
        let result_regs = self.result_regions();
        let initial_regs = other.initial_regions();

        if result_regs.intersects(&initial_regs) {
            if let Some(mut plnscx) = self.restrict_result_regions(&initial_regs) {
                if let Some(mut plnscy) = other.restrict_initial_regions(&result_regs) {
                    plnscx.items.append(&mut plnscy.items);
                    return Ok(plnscx);
                }
            }
        }
        Err(format!("planscorrstore::link: {self} to {other} failed"))
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
            for (iny, planx) in planscx.iter().enumerate() {
                if planx.causes_change() {
                    if cnt > 0 {
                        rc_str.push_str(", ");
                    }
                    rc_str.push_str(&format!("P[{iny}]:{}]", &planx.str_terse()));
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

    /// Return the number of steps with AltRuleHint::AltRule set.
    pub fn num_altrules(&self) -> isize {
        let mut num_alt = 0;
        for planscx in self.iter() {
            num_alt += planscx.num_altrules();
        }
        num_alt
    }

    /// Return the last item in a non-empty PCS.
    pub fn last(&self) -> &PlansCorr {
        assert!(self.is_not_empty());

        self.items.last().unwrap()
    }

    /// Push a PlansCorr into a PlansCorrStore.
    pub fn push(&mut self, pcx: PlansCorr) {
        debug_assert!(self.is_empty() || pcx.len() == self[0].len());

        assert!(self.is_empty() || self.last().result_regions() == pcx.initial_regions());

        self.items.push(pcx);
    }

    /// Return a PlansCorrStore instance, given a string representation.
    /// Like PCS[], PCS[PC[P[r001-0>r101]]] or PCS[PC[P[r001-0>r101]], PC[P[r101-0>r101]]].
    pub fn from(pcs_str: &str) -> Result<Self, String> {
        //println!("regionscorrstore::from: {pcs_str}");

        // Unwrap PCS[], check that brackets are balanced overall.
        let mut pcs_str2 = String::new();
        let mut num_left = 0;
        let mut num_right = 0;
        let mut last_chr = String::new();

        for (inx, chr) in pcs_str.graphemes(true).enumerate() {
            if chr == " " {
                continue;
            }
            if inx == 0 {
                if chr == "P" {
                    continue;
                } else {
                    return Err(format!(
                        "PlansCorrStore::from: Invalid string, {pcs_str} should start with PCS["
                    ));
                }
            }
            if inx == 1 {
                if chr == "C" {
                    continue;
                } else {
                    return Err(format!(
                        "PlansCorrStore::from: Invalid string, {pcs_str} should start with PCS["
                    ));
                }
            }
            if inx == 2 {
                if chr == "S" {
                    continue;
                } else {
                    return Err(format!(
                        "PlansCorrStore::from: Invalid string, {pcs_str} should start with PCS["
                    ));
                }
            }
            if inx == 3 {
                if chr == "[" {
                    num_left += 1;
                    continue;
                } else {
                    return Err(format!(
                        "PlansCorrStore::from: Invalid string, {pcs_str} should start with PCS["
                    ));
                }
            }
            if chr == "[" {
                num_left += 1;
            }
            if chr == "]" {
                num_right += 1;
            }
            if num_right > num_left {
                return Err(format!(
                    "PlansCorrStore::from: Invalid string, {pcs_str}, brackets are not balanced."
                ));
            }
            last_chr = chr.to_string();
            pcs_str2.push_str(chr);
        }
        if num_right != num_left {
            return Err(format!(
                "PlansCorrStore::from: Invalid string, {pcs_str}, brackets are not balanced."
            ));
        }
        if last_chr != "]" {
            return Err(format!(
                "PlansCorrStore::from: Invalid string, {pcs_str} should end with ]"
            ));
        }
        // Remove last right-bracket, balancing PCS[.
        pcs_str2.remove(pcs_str2.len() - 1);
        //println!("pcs_str2 {pcs_str2}");

        // Process contents of PCS[], if any.
        let mut pcs = PlansCorrStore::new(vec![]);

        let mut pc_str = String::new();
        let mut num_left = 0;
        let mut num_right = 0;

        for chr in pcs_str2.graphemes(true) {
            if chr == "[" {
                num_left += 1;
            }

            if chr == "]" {
                num_right += 1;
            }

            if chr == "," && num_left == num_right {
                //println!("pc_str {pc_str}");
                match PlansCorr::from(&pc_str) {
                    Ok(pcx) => pcs.push(pcx),
                    Err(errstr) => return Err(format!("PlansCorrStore::from: {errstr}")),
                }
                pc_str = String::new();
                continue;
            }

            pc_str.push_str(chr);
        }

        if pc_str.is_empty() {
        } else {
            match PlansCorr::from(&pc_str) {
                Ok(pcx) => pcs.push(pcx),
                Err(errstr) => return Err(format!("PlansCorrStore::from: {errstr}")),
            }
        }

        Ok(pcs)
    }
}

impl Index<usize> for PlansCorrStore {
    type Output = PlansCorr;
    fn index(&self, i: usize) -> &PlansCorr {
        &self.items[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::region::SomeRegion;
    use crate::regionscorr::RegionsCorr;

    #[test]
    fn new() -> Result<(), String> {
        let plnscstr = PlansCorrStore::new(vec![]);
        println!("plnscstr {plnscstr}");

        Ok(())
    }

    #[test]
    fn from() -> Result<(), String> {
        let pcs1 = PlansCorrStore::from("PCS[]")?;
        println!("pcs1 {pcs1}");

        let pcs2 = PlansCorrStore::from("PCS[PC[P[r0X-0->r00], P[r0X1-1->r000]]]")?;
        println!("pcs2 {pcs2}");
        assert!(pcs2.len() == 1);

        let pcs3 = PlansCorrStore::from(
            "PCS[PC[P[r0X-0->r00], P[r0X1-1->r000]], PC[P[r00-0->r01], P[r000-1->r100]]]",
        )?;
        println!("pcs3 {pcs3}");
        assert!(pcs3.len() == 2);

        //assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn initial_regions() -> Result<(), String> {
        let plnscstr = PlansCorrStore::from("PCS[PC[P[r0X-0->r00], P[r0X1-1->r000]]]")?;
        println!("plnscstr {plnscstr}");

        let initial_regs = plnscstr.initial_regions();
        println!("initial_regs {initial_regs}");

        assert!(initial_regs == RegionsCorr::from("RC[r0X, r0X1]")?);
        Ok(())
    }

    #[test]
    fn result_regions() -> Result<(), String> {
        let plnscstr = PlansCorrStore::from("PCS[PC[P[r0X-0->r00], P[r0X1-1->r000]]]")?;
        println!("plnscstr {plnscstr}");

        let result_regs = plnscstr.result_regions();
        println!("result_regs {result_regs}");

        assert!(result_regs == RegionsCorr::from("RC[r00, r000]")?);
        Ok(())
    }

    #[test]
    fn restrict_initial_regions() -> Result<(), String> {
        let plnscstr = PlansCorrStore::from("PCS[PC[P[rXX-0->rx0], P[rXXX-1->r0x0]]]")?;
        println!("plnscstr {plnscstr}");

        let restrict_regs = RegionsCorr::from("RC[r00, r000]")?;

        if let Some(restricted) = plnscstr.restrict_initial_regions(&restrict_regs) {
            println!("resricted {restricted}");
            let result_regs = restricted.result_regions();
            assert!(result_regs == RegionsCorr::from("RC[r10, r010]")?);
            assert!(restricted.initial_regions() == RegionsCorr::from("RC[r00, r000]")?)
        } else {
            return Err("restrict_initial_regs failed".to_string());
        }
        Ok(())
    }

    #[test]
    fn restrict_result_regions() -> Result<(), String> {
        let plnscstr = PlansCorrStore::from("PCS[PC[P[rXX-0->rx0], P[rXXX-1->r0x0]]]")?;
        println!("plnscstr {plnscstr}");

        let restrict_regs = RegionsCorr::from("RC[r0X, r00X]")?;

        if let Some(restricted) = plnscstr.restrict_result_regions(&restrict_regs) {
            println!("resricted {restricted}");
            let initial_regs = restricted.initial_regions();
            println!("initial regs {initial_regs}");
            assert!(initial_regs == RegionsCorr::from("RC[r1X, rX1X]")?);
            assert!(restricted.result_regions() == RegionsCorr::from("RC[r00, r000]")?)
        } else {
            return Err("restrict_result_regs failed".to_string());
        }
        Ok(())
    }

    #[test]
    fn link() -> Result<(), String> {
        // Set up first PlansCorrStore.
        let plnscstr1 = PlansCorrStore::from("PCS[PC[P[r000X-0->r001X-0->r011X]]]")?;

        // Set up second PlansCorrStore.
        let plnscstr2 = PlansCorrStore::from("PCS[PC[P[rX111-0->X101-0->rX100]]]")?;

        println!(
            "plnscstr1 results {} plnscstr2 initial {}",
            plnscstr1.result_regions(),
            plnscstr2.initial_regions()
        );

        // Calc link.
        if let Ok(plnscstr3) = plnscstr1.link(&plnscstr2) {
            println!("plnscstr3 {plnscstr3}");
            assert!(plnscstr3.initial_regions() == RegionsCorr::from("RC[r0001]")?);
            assert!(plnscstr3.result_regions() == RegionsCorr::from("RC[r0100]")?);
        } else {
            return Err("link failed".to_string());
        }
        Ok(())
    }

    #[test]
    fn result_from_initial_regions() -> Result<(), String> {
        let plcstr = PlansCorrStore::from("PCS[PC[P[rXX-0->r0X-0->r11], P[rXXX-1->r0x1-1->1x0]]]")?;
        println!("plcstr {plcstr}");

        if let Some(rslts) =
            plcstr.result_from_initial_regions(&RegionsCorr::from("RC[r00, r001]")?)
        {
            println!("rslts {rslts}");
            assert!(rslts[0] == SomeRegion::from("r11")?);
            assert!(rslts[1] == SomeRegion::from("r110")?);
        } else {
            return Err("result_from_initial_regions failed".to_string());
        }
        Ok(())
    }
}
