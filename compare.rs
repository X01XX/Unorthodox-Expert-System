///  A trait to allow SomeGroup and SomeSquare instances to be compared in any combination.
use crate::pn::Pn;
use crate::region::SomeRegion;
use crate::rulestore::RuleStore;

pub trait Compare {
    fn get_pn_ref(&self) -> &Pn;
    fn get_pnc(&self) -> bool;
    fn get_rules_ref(&self) -> &RuleStore;
    fn get_region(&self) -> SomeRegion;
}
