use crate::change::SomeChange;
use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::plan::SomePlan;
use crate::planscorr::PlansCorr;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionscorr::RegionsCorr;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::statescorr::StatesCorr;
use crate::stepstore::StepStore;
use crate::tools::{self, CorrespondingItems};

use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};
use std::str::FromStr;

impl fmt::Display for DomainStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }
}

/// Implement the PartialEq trait, for a
/// A quick comparison of definitions.
impl PartialEq for DomainStore {
    fn eq(&self, other: &Self) -> bool {
        if self.items.len() != other.items.len() {
            return false;
        }
        for (domx, domy) in self.items.iter().zip(other.items.iter()) {
            if domx != domy {
                return false;
            }
        }
        true
    }
}
impl Eq for DomainStore {}

#[readonly::make]
#[derive(Serialize, Deserialize, Default)]
/// A vector of SomeDomain structs, and session state.
pub struct DomainStore {
    /// Vector of SomeDomain structs.
    pub items: Vec<SomeDomain>,
}

impl DomainStore {
    /// Return a new, empty, DomainStore struct.
    pub fn new() -> Self {
        Self { items: vec![] }
    }

    /// Return the length, the number of domains.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeDomain> {
        self.items.iter()
    }

    /// Return a vector mut iterator.
    pub fn iter_mut(&mut self) -> IterMut<SomeDomain> {
        self.items.iter_mut()
    }

    pub fn push(&mut self, mut domx: SomeDomain) {
        domx.set_id(self.items.len());

        self.items.push(domx);
    }

    /// Run a PlansCorr, plans will be run in parallel.
    pub fn run_planscorr(&mut self, plns: &PlansCorr) -> Result<usize, String> {
        debug_assert!(self.is_congruent(plns));

        let mut steps_run = 0;

        let vecb = plns
            .plans
            .items
            .par_iter()
            .zip(self.items.par_iter_mut())
            .map(|(plnx, domx)| domx.run_plan(plnx))
            .collect::<Vec<Result<usize, String>>>();

        for rsltx in vecb {
            match rsltx {
                Ok(num) => {
                    steps_run += num;
                }
                Err(errstr) => return Err(errstr),
            }
        }
        Ok(steps_run)
    }

    /// Return true if corresponding items have the same number of bits.
    pub fn is_congruent(&self, other: &impl CorrespondingItems) -> bool {
        self.num_bits_vec() == other.num_bits_vec()
    }

    /// Return a vector of corresponding num_bits.
    pub fn num_bits_vec(&self) -> Vec<usize> {
        let mut ret_vec = Vec::<usize>::with_capacity(self.len());
        for domx in self.iter() {
            ret_vec.push(domx.num_bits());
        }
        ret_vec
    }

    /// Get needs for each Domain.
    /// Run in parallel per Domain.
    /// Each Domain uses parallel processing to get needs for each Action.
    ///  plans.
    /// Set DomainStore fields with need info.
    pub fn get_needs(&mut self) -> NeedStore {
        //println!("domainstore::get_needs");
        // Inc step number.

        // Get all needs.
        let vecx: Vec<NeedStore> = self
            .items
            .par_iter_mut() // .par_iter_mut for parallel, .iter_mut for easier reading of diagnostic messages
            .map(|domx| domx.get_needs())
            .collect::<Vec<NeedStore>>();

        let mut ret = NeedStore::new(vec![]);
        for needsx in vecx {
            ret.append(needsx);
        }
        ret
    }

    /// Return a String representation of a DomainStore.
    fn formatted_str(&self) -> String {
        let mut rc_str = String::from("[");

        let mut first = true;
        for domx in self.items.iter() {
            if first {
                first = false;
            } else {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&domx.to_string());
        }
        rc_str.push(']');

        rc_str
    }

    /// Return a from_str compatible string for a DomainStore instance.
    pub fn formatted_def(&self) -> String {
        let mut rc_str = String::from("DS[");
        let mut first = true;
        for domx in self.items.iter() {
            if first {
                first = false;
            } else {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&domx.formatted_def());
        }
        rc_str.push(']');

        rc_str
    }

    /// Return a vector of domain current state references, in domain number order.
    pub fn all_current_states(&self) -> StatesCorr {
        let mut all_states = StatesCorr::with_capacity(self.len());

        for domx in self.items.iter() {
            all_states.push(domx.current_state().clone());
        }

        all_states
    }

    /// Return a vector of domain current states, as regions, in domain number order.
    pub fn all_current_regions(&self) -> RegionsCorr {
        let mut all_regions = RegionsCorr::with_capacity(self.len());

        for domx in self.items.iter() {
            all_regions.push(SomeRegion::new(vec![domx.current_state().clone()]));
        }

        all_regions
    }

    /// Set the current state fields, of each domain.
    pub fn set_cur_states(&mut self, new_states: &StatesCorr) {
        debug_assert!(self.is_congruent(new_states));

        for (domx, stax) in self.items.iter_mut().zip(new_states.iter()) {
            domx.set_cur_state(stax.clone());
        }
    }

    /// Return the total number of groups in all the domains.
    pub fn number_groups(&self) -> usize {
        let mut tot = 0;
        for domx in self.items.iter() {
            tot += domx.number_groups();
        }
        tot
    }

    /// Return the total number of groups defined in all the domains.
    pub fn number_groups_defined(&self) -> usize {
        let mut tot = 0;
        for domx in self.items.iter() {
            tot += domx.number_groups_defined();
        }
        tot
    }

    /// Return the maximum possible regions.
    pub fn maximum_regions(&self) -> RegionsCorr {
        let mut ret_regs = RegionsCorr::with_capacity(self.len());
        for domx in self.items.iter() {
            ret_regs.push(domx.maximum_region());
        }
        ret_regs
    }

    /// Take an action to satisfy a need,
    pub fn take_action_need(&mut self, dom_id: usize, needx: &SomeNeed) {
        self.items[dom_id].take_action_need(needx);
    }

    /// Return a reference to the current state of a given Domain.
    pub fn cur_state(&self, dom_id: usize) -> &SomeState {
        self.items[dom_id].current_state()
    }

    /// Get plans to move to a goal region, choose a plan.
    /// Accept an optional region that must encompass the intermediate steps of a returned plan.
    /// The within argument restricts where a rule should start, and restricts unwanted changes that may be included with wanted changes.
    pub fn make_plans_domain(
        &self,
        dom_id: usize,
        from_region: &SomeRegion,
        goal_region: &SomeRegion,
        within: &SomeRegion,
    ) -> Result<PlanStore, Vec<String>> {
        debug_assert!(from_region.num_bits() == goal_region.num_bits());
        debug_assert!(within.num_bits() == from_region.num_bits());
        debug_assert!(within.is_superset_of(from_region));
        debug_assert!(within.is_superset_of(goal_region));
        //println!("domainstore: make_plans_domain: dom {dom_id} from {from_region} goal {goal_region}");

        self.items[dom_id].make_plans(from_region, goal_region, within)
    }

    /// Run a plan for a domain.
    pub fn run_plan_domain(&mut self, dom_id: usize, planx: &SomePlan) -> Result<usize, String> {
        self.items[dom_id].run_plan(planx)
    }

    /// Find a domain that matches a given ID, return a reference.
    pub fn find(&self, dom_id: usize) -> Option<&SomeDomain> {
        if dom_id >= self.items.len() {
            return None;
        }
        Some(&self.items[dom_id])
    }

    /// Run cleanup for a domain and action.
    pub fn cleanup(&mut self, dom_id: usize, act_id: usize, needs: &NeedStore) {
        assert!(dom_id < self.len());
        self.items[dom_id].cleanup(act_id, needs);
    }

    /// Take an action in a domain.
    pub fn take_action(&mut self, dom_id: usize, act_id: usize) {
        debug_assert!(dom_id < self.len());

        self.items[dom_id].take_action(act_id);
    }

    /// Evaluate an arbitrary sample.
    pub fn eval_sample_arbitrary(&mut self, dom_id: usize, act_id: usize, smpl: &SomeSample) {
        debug_assert!(dom_id < self.len());

        self.items[dom_id].eval_sample_arbitrary(act_id, smpl);
    }

    /// Take an arbitrary action.
    pub fn take_action_arbitrary(&mut self, dom_id: usize, act_id: usize, astate: &SomeState) {
        debug_assert!(dom_id < self.len());

        self.items[dom_id].take_action_arbitrary(act_id, astate);
    }

    /// Set the cleanup limit for a domain-action.
    pub fn set_domain_cleanup(&mut self, dom_id: usize, act_id: usize, trigger: usize) {
        debug_assert!(dom_id < self.len());

        self.items[dom_id].set_cleanup(act_id, trigger);
    }

    /// Calc aggregate changes, for SessionData initialization.
    pub fn calc_aggregate_changes(&mut self) {
        for itemx in self.items.iter_mut() {
            itemx.calc_aggregate_changes();
        }
    }

    /// Collect steps that contain at least one wanted change.
    pub fn get_steps_domain(
        &self,
        dom_id: usize,
        wanted_changes: &SomeChange,
        within: &SomeRegion,
    ) -> StepStore {
        self.items[dom_id].get_steps(wanted_changes, within)
    }

    /// Change a RegionsCorr to match a DomainStore, if needed,
    /// to make user input of RegionsCorr definition easier.
    ///
    /// Each domain will match one, or zero, regions. Matching left to right, first match wins.
    /// At least one RC region must be given.
    /// All given regions must match a domain, based on number of bits used.
    /// Out of order regions, using the same number bits, will remain out of order.
    /// Domains matching zero regions will default to the domains' maximum region.
    pub fn validate_rc(&self, rcx: RegionsCorr) -> Result<RegionsCorr, String> {
        if rcx.is_empty() {
            return Err("domainstore::validate_rc: rc empty".to_string());
        }
        if rcx.len() > self.len() {
            return Err("domainstore::validate_rc: rc has too many regions".to_string());
        }
        // Check for correct, complete, input.
        if rcx.len() == self.len() && self.is_congruent(&rcx) {
            return Ok(rcx);
        }
        // Check for no domain match. Correct order error, if any.
        let mut regs: Vec<Option<SomeRegion>> = vec![None; self.len()];
        for regx in rcx {
            let mut not_found = true;
            for (inx, domx) in self.iter().enumerate() {
                if domx.num_bits() == regx.num_bits() && regs[inx].is_none() {
                    // Domain match
                    regs[inx] = Some(regx.clone());
                    not_found = false;
                    break;
                }
            }
            if not_found {
                return Err(format!(
                    "domainstore::validate_rc: Region {regx} with no domain match"
                ));
            }
        }

        // Replace missing regions with domain maximum region.
        let mut regs2 = RegionsCorr::with_capacity(self.len());
        for (optx, domx) in regs.into_iter().zip(self.iter()) {
            if let Some(regx) = optx {
                regs2.push(regx);
            } else {
                regs2.push(domx.maximum_region());
            }
        }

        Ok(regs2)
    }
}

impl Index<usize> for DomainStore {
    type Output = SomeDomain;
    fn index(&self, i: usize) -> &SomeDomain {
        &self.items[i]
    }
}

impl IndexMut<usize> for DomainStore {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.items[i]
    }
}

impl FromStr for DomainStore {
    type Err = String;
    /// Return a DomainStore instance, given a string representation.
    /// The string contains:
    ///   One, or more, actions,.
    ///
    /// DS[DOMAIN[ACT[[XX/XX/XX/Xx]], ACT[[XX/XX/Xx/XX]]]]
    ///
    /// All the rules must use the same number of bits.
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("DomainStore::from_str: {str_in}");
        let str_in2 = str_in.trim();

        // Strip off id and surrounding brackets.
        if str_in2.len() < 5 {
            return Err(
                "domainstore::from_str: string should be at least = DS[<one domain>]".to_string(),
            );
        }

        if str_in2[0..3].to_uppercase() != *"DS[" {
            return Err("domainstore::from_str: string should begin with DS[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("domainstore::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[3..(str_in2.len() - 1)];

        // Split tokens
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("domainstore::from_str: {errstr}")),
        };
        //println!("tokens {:?}", tokens);

        // Init DomainStore to return.
        let mut dmxs = DomainStore::new();

        // Process tokens.
        for tokenx in tokens.iter() {
            if tokenx[0..7].to_uppercase() == *"DOMAIN[" {
                //println!("found SomeDomain {tokenx}");
                match SomeDomain::from_str(tokenx) {
                    Ok(domx) => dmxs.push(domx),
                    Err(errstr) => return Err(format!("DomainStore::from_str: {errstr}")),
                }
            } else {
                return Err(format!("DomainStore::from_str: Invalid token, {tokenx}"));
            }
        }
        Ok(dmxs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_str() -> Result<(), String> {
        let dom_str = "DS[DOMAIN[ACT[[XX/XX/XX/Xx]], ACT[[XX/XX/Xx/XX]]]]";
        let dom = DomainStore::from_str(&dom_str)?;
        println!("dom {}", dom.formatted_def());
        assert!(dom.formatted_def() == dom_str);

        // assert!(1 == 2);
        Ok(())
    }

    #[test]
    fn validate_rc() -> Result<(), String> {
        let dom_str = "DS[
                        DOMAIN[ACT[[XX_XX/XX/XX/Xx], s0_0000, s1_1111]],
                        DOMAIN[ACT[[XX/XX/XX/Xx], s0000, s1111]],
                        DOMAIN[ACT[[XX/Xx/00], s000, s110]],
                        DOMAIN[ACT[[Xx/Xx], s00, s11]]
                    ]";

        let dsx = DomainStore::from_str(&dom_str)?;
        println!("{dsx}");

        // Test an RC set up as expected.
        let rc_str = "RC[r0_0000, r0000, r000, r00]";
        let rc = RegionsCorr::from_str(&rc_str)?;
        println!("rc {rc}");

        let rc2 = dsx.validate_rc(rc)?;
        println!("rc2 {rc2}");
        assert!(format!("{rc2}") == rc_str);

        // Test an RC with one out of order region and one missing region.
        let rc_str = "RC[r000, r0_0000, r0000]";
        let rc3 = RegionsCorr::from_str(&rc_str)?;
        println!("rc3 {rc3}");

        let rc4 = dsx.validate_rc(rc3)?;
        println!("rc4 {rc4}");
        assert!(format!("{rc4}") == "RC[r0_0000, r0000, r000, rXX]");

        // Test an RC that is empty.
        let rc5 = RegionsCorr::from_str("RC[]")?;
        println!("rc5 {rc5}");

        match dsx.validate_rc(rc5) {
            Ok(rcx) => return Err(format!("validate of rc5 worked {rcx} ?")),
            Err(errstr) => println!("rc5 {errstr}"),
        }

        // Test an RC with extra region.
        let rc6 = RegionsCorr::from_str("RC[r0_0000, r0000, r000, r00, r0]")?;
        println!("rc6 {rc6}");

        match dsx.validate_rc(rc6) {
            Ok(rcx) => return Err(format!("validate of rc6 worked {rcx} ?")),
            Err(errstr) => println!("rc6 {errstr}"),
        }

        // Test an RC with two regions of the same number bits, but only one matching domain.
        let rc7 = RegionsCorr::from_str("RC[r0_0000, r0000, r000, r000]")?;
        println!("rc7 {rc7}");

        match dsx.validate_rc(rc7) {
            Ok(rcx) => return Err(format!("validate of rc7 worked {rcx} ?")),
            Err(errstr) => println!("rc7 {errstr}"),
        }

        //assert!(1 == 2);
        Ok(())
    }
}
