use crate::domain::SomeDomain;
use crate::need::SomeNeed;
use crate::needstore::NeedStore;
use crate::planscorr::PlansCorr;
use crate::planstore::PlanStore;
use crate::region::SomeRegion;
use crate::regionscorr::RegionsCorr;
use crate::sample::SomeSample;
use crate::state::SomeState;
use crate::statescorr::StatesCorr;
use crate::tools::CorrespondingItems;

use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};
use std::str::FromStr;
use unicode_segmentation::UnicodeSegmentation;

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

    /// If exactly one domain uses a given number of bits, return its ID.
    pub fn find_num_bits(&self, num_bits: usize) -> Option<usize> {
        let mut dom_ids = vec![];
        for domx in self.items.iter() {
            if domx.num_bits() == num_bits {
                dom_ids.push(domx.id);
            }
        }
        if dom_ids.len() == 1 {
            Some(dom_ids[0])
        } else {
            None
        }
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
    /// "DS[ DOMAIN[ACT[[XX/XX/XX/Xx], ACT[XX/XX/Xx/XX]]],
    ///      DOMAIN[ACT[[XX/XX/XX/Xx/XX], ACT[XX/XX/Xx/XX/XX]]]"
    ///
    /// All the rules must use the same number of bits.
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("DomainStore::from_str: {str_in}");
        let ds_str = str_in.trim();

        // Unwrap "DS[ ... ]". Check that the brackets are balanced.
        let mut ds_str2 = String::new();
        let mut left = 0;
        let mut right = 0;

        for (inx, chr) in ds_str.graphemes(true).enumerate() {
            if chr == "\n" {
                continue;
            }
            if inx == 0 {
                if chr != "D" {
                    return Err(
                        "DomainStore::from_str: Invalid string, should start with DS[".to_string(),
                    );
                }
                continue;
            }
            if inx == 1 {
                if chr != "S" {
                    return Err(
                        "DomainStore::from_str: Invalid string, should start with DS[".to_string(),
                    );
                }
                continue;
            }
            if inx == 2 {
                if chr != "[" {
                    return Err(
                        "DomainStore::from_str: Invalid string, should start with DS[".to_string(),
                    );
                }
                left += 1;
                continue;
            }
            if chr == "[" {
                left += 1;
            }
            if chr == "]" {
                right += 1;
                if right > left {
                    return Err("DomainStore::from_str: Brackets not balanced.".to_string());
                }
            }

            ds_str2.push_str(chr);
        }
        if left != right {
            return Err("DomainStore::from_str: Brackets not balanced.".to_string());
        }
        ds_str2 = ds_str2.trim().to_string();

        // Find tokens
        let mut token_vec = vec![];
        let mut token = String::new();
        let mut left = 0;
        let mut right = 0;

        for chr in ds_str2.graphemes(true) {
            if chr == "\n" {
                continue;
            }
            if left == right && (chr == "," || chr == " ") {
                continue;
            }
            token.push_str(chr);

            if chr == "[" {
                left += 1;
            }
            if chr == "]" {
                right += 1;
                if left == right && left > 0 && !token.is_empty() {
                    token = token.trim().to_string();
                    token_vec.push(token);
                    token = String::new();
                }
            }
        }
        if token.is_empty() {
            token = token.trim().to_string();
            token_vec.push(token);
        }
        //println!("token_vec {:?}", token_vec);

        // Init DomainStore to return.
        let mut dmxs = DomainStore::new();

        // Process tokens.
        for tokenx in token_vec.iter() {
            if tokenx[0..7] == *"DOMAIN[" {
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
