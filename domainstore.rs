use crate::domain::SomeDomain;
use crate::planscorr::PlansCorr;
use crate::tools::CorrespondingItems;
use crate::needstore::NeedStore;
use crate::statescorr::StatesCorr;

use serde::{Deserialize, Serialize};
use std::slice::{Iter, IterMut};
use std::ops::{Index, IndexMut};
use rayon::prelude::*;
use std::str::FromStr;
use std::fmt;
use unicode_segmentation::UnicodeSegmentation;

impl fmt::Display for DomainStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_str())
    }    
}

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
        Self {
            items: vec![]
        }
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
            .map(|(plnx, domx)| domx.run_plan(plnx, 0))
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

        // Strip off "DS[ ... ]". Check that the brackets are balanced.
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
