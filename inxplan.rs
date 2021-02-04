// InxPlan struct, for an Unorthodox Expert System.
//
// A Vec<T> is needed as a collector for parallel processing of needs to find plans,
// but a tuple, or array, does not qualify as a "T".
//

use crate::plan::SomePlan;

#[derive(Debug)]
pub struct InxPlan {
    pub inx: usize,            // Index to a need in a NeedStore.
    pub pln: Option<SomePlan>, // Plan to satisfy need (may be empty if the current state satisfies the need), or None.
}
