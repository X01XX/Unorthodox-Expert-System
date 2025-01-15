//! The RulesCorr struct, a store of SomeRules, correspanding in order, to domains in a DomainStore instance.
//!
//! Each rule will have a number of bits equal to the bits used by the corresponding
//! domain, not necessarily the same as other rules in the vector.

use crate::rule::SomeRule;
use crate::rulestore::RuleStore;
use crate::regionscorr::RegionsCorr;
use crate::tools;
use crate::changescorr::ChangesCorr;

use std::slice::Iter;
use std::fmt;
use std::str::FromStr;

impl fmt::Display for RulesCorr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "RUC[]")
        } else {
            write!(f, "RUC{}", self.rules)
        }
    }   
}

#[readonly::make]
#[derive(Debug, Clone)]
/// A vector of rules, corresponding to domains in a vector.
pub struct RulesCorr {
    pub rules: RuleStore,
}

impl RulesCorr {
    /// Return a new RulesCorr instance, empty, with a specified capacity.
    pub fn with_capacity(cap: usize) -> Self {
        debug_assert!(cap > 0);
        Self {
            rules: RuleStore::with_capacity(cap),
        }
    }

    /// Return the number of rules.
    pub fn len(&self) -> usize {
        self.rules.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }

    /// Return true if the store is not empty.
    #[allow(dead_code)]
    pub fn is_not_empty(&self) -> bool {
        !self.rules.is_empty()
    }

    /// Add a rule to the rule store.
    pub fn push(&mut self, rulx: SomeRule) {
        self.rules.push(rulx);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRule> {
        self.rules.iter()
    }

    /// Return rules needed to change from one RegionsCorr to another.
    pub fn new_rc_to_rc(from: &RegionsCorr, to: &RegionsCorr) -> Self {
        let mut ret = Self::with_capacity(from.len());

        for (regx, regy) in from.iter().zip(to.iter()) {
            ret.push(SomeRule::new_region_to_region_min(regx, regy));
        }
        ret
    }

    /// Return a ChangesCorr for all changes.
    pub fn as_changes(&self) -> ChangesCorr {
        let mut ret = ChangesCorr::with_capacity(self.len());

        for rulx in self.iter() {
            ret.push(rulx.as_change());
        }
        ret
    }

    /// Return a ChangesCorr for all changes not wanted.
    pub fn unwanted_changes(&self) -> ChangesCorr {
        let mut ret = ChangesCorr::with_capacity(self.len());

        for rulx in self.iter() {
            ret.push(rulx.unwanted_changes());
        }
        ret
    }
}

impl FromStr for RulesCorr {
    type Err = String;
    /// Return a rulestore, given a string representation.
    /// Like RUC[], RUC[X0/11/11/10/00] or RUC[01/00/XX, 00/00/XX].
    fn from_str(str_in: &str) -> Result<Self, String> {
        //println!("rulescorr::from_str: {str_in}");
        let str_in2 = str_in.trim();

        if str_in2.len() < 5 {
            return Err("rulescorr::from_str: string should be at least = RCS[]".to_string());
        }

        if str_in2 == "RUC[]" {
            return Ok(Self::with_capacity(1));
        }

        if str_in2[0..4] != *"RUC[" {
            return Err("rulescorr::from_str: string should begin with RUC[".to_string());
        }
        if str_in2[(str_in2.len() - 1)..str_in2.len()] != *"]" {
            return Err("rulescorr::from_str: string should end with ]".to_string());
        }

        // Strip off surrounding brackets.
        let token_str = &str_in2[4..(str_in2.len() - 1)];

        // Split string into SomeRule tokens.
        let tokens = match tools::parse_input(token_str) {
            Ok(tokenvec) => tokenvec,
            Err(errstr) => return Err(format!("rulescorr::from_str: {errstr}")),
        };
        println!("tokens {:?}", tokens);

        // Tally up tokens.
        let mut ret_corr = RulesCorr::with_capacity(tokens.len());

        for tokenx in tokens.into_iter() {
            match SomeRule::from_str(&tokenx) {
                Ok(rulx) => ret_corr.push(rulx),
                Err(errstr) => return Err(format!("rulescorr::from_str: {errstr}")),
            }
        }

        Ok(ret_corr)
    }
}                                

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_str() -> Result<(), String> {                                                                                                             
        let rulcr1_str = "RUC[]";
        let rulcr1 = RulesCorr::from_str(&rulcr1_str)?;
        println!("rulcr1 {rulcr1}");
        assert!(format!("{rulcr1}") == rulcr1_str);
    
        let rulcr2_str = "RUC[X0_11/11/10/00]";
        let rulcr2 = RulesCorr::from_str(&rulcr2_str)?;
        println!("rulcr2 {rulcr2}");
        assert!(format!("{rulcr2}") == rulcr2_str);
        
        let rulcr3_str = "RUC[01/00/XX, 00/00]";
        let rulcr3 = RulesCorr::from_str(&rulcr3_str)?;
        println!("rulcr3 {rulcr3}");
        assert!(format!("{rulcr3}") == rulcr3_str);

        //assert!(1 == 2);
        Ok(())
    }   

    #[test]
    fn new_rc_to_rc() -> Result<(), String> {
        let rc1 = RegionsCorr::from_str("RC[r1010, r101X0]")?;
        let rc2 = RegionsCorr::from_str("RC[r1X11, rX00X0]")?;

        let ruc = RulesCorr::new_rc_to_rc(&rc1, &rc2);
        println!("{ruc}");
        assert!(format!("{ruc}") == "RUC[11/00/11/01, 11_00/10/XX/00]");

        //assert!(1 == 2);
        Ok(()) 
    }

    #[test]
    fn as_changes() -> Result<(), String> {
        let rc1 = RegionsCorr::from_str("RC[r1010, r101X0]")?;
        let rc2 = RegionsCorr::from_str("RC[r1X11, rX00X0]")?;

        let ruc = RulesCorr::new_rc_to_rc(&rc1, &rc2);
        println!("ruc {ruc}");

        let cngs = ruc.as_changes();
        println!("cngs {cngs}");
        assert!(format!("{cngs}") == "CC[../../../01, .._../10/../..]");

        let cngs = ruc.unwanted_changes();
        println!("unwn {cngs}");
        assert!(format!("{cngs}") == "CC[10/01/10/.., 10_01/../../01]");

        //assert!(1 == 2);
        Ok(()) 
    }
}
