use crate::pn::Pn;
use crate::rulestore::RuleStore;

pub trait Compare {
    fn get_pn_ref(&self) -> &Pn;
    fn get_pnc(&self) -> bool;
    fn get_rules_ref(&self) -> &RuleStore;
}

