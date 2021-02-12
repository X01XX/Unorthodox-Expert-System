//! An InxPlan struct, containing an index to a SomeNeed vector, and a SomePlan struct.
//!
//! A Vec<T> is needed as a collector for parallel processing of needs to find plans,
//! but a tuple, or array, does not qualify as a "T".
//!

use crate::plan::SomePlan;

#[derive(Debug)]
pub struct InxPlan {
    /// Index to a need in a NeedStore.
    pub inx: usize,
    /// Plan to satisfy need (may be empty if the current state satisfies the need), or None.
    pub pln: Option<SomePlan>,
}
