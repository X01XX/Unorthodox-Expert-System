//! The PlanStore struct, a vector of SomePlan struct instances.
//!
//! The plans in the PlanStore may be different plans to go from the same region to the same goal region.
//!
//! Or the plans are linked, in that the result region of a plan for a given Domain ID is the same
//! as the initial region for a following plan with the same Domain ID.

use crate::plan::SomePlan;
use crate::regionscorr::RegionsCorr;
use crate::tools::StrLen;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::{Iter, IterMut};
use unicode_segmentation::UnicodeSegmentation;

impl fmt::Display for PlanStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

#[readonly::make]
#[derive(Debug, Default, Clone, Deserialize, Serialize)]
/// A vector of SomePlan struct instances.
/// When the plans in a planstore are run, each plan will be for a domain,
/// and run in the order they appear in the vector.
/// For some situations, the creator of the PlanStore may split a domain plan and intermingle
/// it with other domain plans.  This is due to the Boolen AND relationship of
/// domain positions in the SelectRegions struct, and some combinations of domain positions
/// have positive, or negative, values. This is like moving different Chess pieces, at different times,
/// to reach a desired position, without a major mess-up inbetween.
pub struct PlanStore {
    /// A vector of SomePlan instances.
    pub items: Vec<SomePlan>,
}

impl PlanStore {
    /// Return a new PlanStore instance.
    /// If more than one plan, plans will be run in order.
    pub fn new(items: Vec<SomePlan>) -> Self {
        Self { items }
    }

    /// Return a new PlanStore instance, with a given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            items: Vec::<SomePlan>::with_capacity(cap),
        }
    }

    /// Return the length of the SomePlan vector.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return true if the store does not contain at least one non-empty plan.
    pub fn is_empty(&self) -> bool {
        if self.items.is_empty() {
            return true;
        }
        for planx in self.items.iter() {
            if planx.is_not_empty() {
                return false;
            }
        }
        true
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.items.is_empty()
    }

    /// Add a plan to the PlanStore.
    pub fn push(&mut self, planx: SomePlan) {
        self.items.push(planx);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomePlan> {
        self.items.iter()
    }

    /// Return a reference to the las plan.
    pub fn last(&self) -> Option<&SomePlan> {
        self.items.last()
    }

    /// Return the number of steps in the plans of the PlanStore.
    pub fn number_steps(&self) -> usize {
        self.items.iter().map(|planx| planx.len()).sum()
    }

    /// Return a String representation of a PlanStore.
    fn formatted_str(&self) -> String {
        let mut flg = 0;

        let mut rc_str = String::with_capacity(self.strlen());

        rc_str.push('[');
        for planx in &self.items {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&planx.to_string());
            flg = 1;
        }
        rc_str.push(']');

        rc_str
    }

    /// Extend a StepStore by pushing another StepStore.
    pub fn append(&mut self, other: Self) {
        for planx in other.items {
            self.push(planx);
        }
    }

    /// Return number bits changed running plans in store.
    pub fn num_bits_changed(&self) -> usize {
        self.items
            .iter()
            .map(|planx| planx.num_bits_changed())
            .sum()
    }

    /// Return the initial_regions of a Planstore.
    /// The current regions should intersect the plan initial regions,
    /// and will be a default region in case a PlanStore does not have a plan for
    /// a domain.
    pub fn initial_regions(&self, default: &RegionsCorr) -> RegionsCorr {
        let mut ret_regs = default.clone();
        for planx in self.items.iter() {
            //if planx.is_empty() {
            //    continue;
            //}
            ret_regs.push(planx.initial_region().clone());
        }
        ret_regs
    }

    /// Return a PlanStore with duplicates deleted.
    pub fn _delete_duplicates(&self) -> Self {
        let mut ret_store = PlanStore::new(vec![]);
        for planx in self.iter() {
            ret_store.push(planx.clone()); // only adds non-duplicates.
        }
        ret_store
    }

    /// Return true if a PlanStore contains a plan.
    pub fn contains(&self, planx: &SomePlan) -> bool {
        for itemx in self.items.iter() {
            if itemx.num_bits() == planx.num_bits() && itemx == planx {
                return true;
            }
        }
        false
    }

    /// Return a mutable iterator
    pub fn iter_mut(&mut self) -> IterMut<SomePlan> {
        self.items.iter_mut()
    }

    /// Remove a plan from a PlanStore
    pub fn remove(&mut self, inx: usize) -> SomePlan {
        assert!(inx < self.items.len(), "Index out of bounds");

        let last_inx = self.items.len() - 1;

        if inx == last_inx {
            return self.items.pop().unwrap();
        }

        self.items.swap(inx, last_inx);
        self.items.pop().unwrap()
    }

    /// Return a planstore, given a string representation.
    /// Like [], [P[r001-0>r101]] or [P[r001-0>r101], P[r101-0>r101]].
    pub fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("planstore::from_str: {ps_str}");
        let ps_str = str_in.trim();

        if ps_str.is_empty() {
            return Err("PlansStore::from_str: Empty string?".to_string());
        }

        let mut ps_str2 = String::new();
        let mut last_chr = String::new();

        for (inx, chr) in ps_str.graphemes(true).enumerate() {
            if inx == 0 {
                if chr == "[" {
                    continue;
                } else {
                    return Err(format!(
                        "PlanStore::from_str: Invalid string, {ps_str} should start with ["
                    ));
                }
            }

            last_chr = chr.to_string();
            ps_str2.push_str(chr);
        }
        if last_chr != "]" {
            return Err(format!(
                "PlanStore::from_str: Invalid string, {ps_str} should end with ]"
            ));
        }

        if ps_str2.is_empty() {
            return Ok(PlanStore::new(vec![]));
        }

        // Remove last ] character.
        ps_str2.remove(ps_str2.len() - 1);

        // Split string into <plan> tokens.
        let mut token = String::new();
        let mut token_list = Vec::<String>::new();

        for chr in ps_str2.graphemes(true) {
            if chr == "," || chr == " " {
                if token.is_empty() {
                } else {
                    token_list.push(token);
                    token = String::new();
                }
            } else {
                token.push_str(chr);
            }
        }
        if token.is_empty() {
        } else {
            token_list.push(token);
        }
        //println!("token_list {:?}", token_list);

        // println!("token_list2 {:?}", token_list2);

        // Tally up tokens.
        let mut plans = Vec::<SomePlan>::new();

        for tokenx in token_list.into_iter() {
            match SomePlan::from_str(&tokenx) {
                Ok(regx) => plans.push(regx),
                Err(errstr) => return Err(format!("PlanStore::from_str: {errstr}")),
            }
        }
        let ret_planstore = PlanStore::new(plans);

        Ok(ret_planstore)
    }
} // end impl PlanStore

impl Index<usize> for PlanStore {
    type Output = SomePlan;
    fn index(&self, i: usize) -> &SomePlan {
        &self.items[i]
    }
}

/// Implement the trait StrLen for SomePlan.
impl StrLen for PlanStore {
    fn strlen(&self) -> usize {
        let mut cnt = 2; // brackets.
        for (inx, planx) in self.items.iter().enumerate() {
            if inx > 0 {
                cnt += 2; // comma space
            }
            cnt += planx.strlen();
        }
        cnt
    }
}

impl IntoIterator for PlanStore {
    type Item = SomePlan;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strlen() -> Result<(), String> {
        let mut plnstr = PlanStore::from_str("[]")?;
        let fstr = plnstr.to_string();
        let sb = plnstr.strlen();
        println!("{}", plnstr);
        if fstr.len() != sb {
            return Err(format!("str {} NE calced {}", fstr.len(), sb));
        }

        plnstr.push(SomePlan::from_str("P[r0000_0000-0->r0000_0000]")?);
        let fstr = plnstr.to_string();
        let sb = plnstr.strlen();
        println!("{}", plnstr);
        if fstr.len() != sb {
            return Err(format!("str {} NE calced {}", fstr.len(), sb));
        }
        Ok(())
    }

    #[test]
    fn from_str() -> Result<(), String> {
        let plnst1 = PlanStore::from_str("[]")?;
        println!("plnst1 {plnst1}");
        assert!(format!("{plnst1}") == "[]");

        let plnst2 = PlanStore::from_str("[P[r0000-0->r1111]]")?;
        println!("plnst2 {plnst2}");
        assert!(format!("{plnst2}") == "[P[r0000-0->r1111]]");

        let plnst3 = PlanStore::from_str("[P[r0000-0->r1111], P[r0000-0->r1100]]")?;
        println!("plnst3 {plnst3}");
        assert!(format!("{plnst3}") == "[P[r0000-0->r1111], P[r0000-0->r1100]]");

        Ok(())
    }
}
