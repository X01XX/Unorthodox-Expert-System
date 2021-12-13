use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::rulestore::RuleStore;

pub trait Compare {
    fn get_pn_ref(&self) -> &Pn;
    fn get_pnc(&self) -> bool;
    fn get_rules_ref(&self) -> &RuleStore;
    fn get_region(&self) -> SomeRegion; // try not to use frequently
}

