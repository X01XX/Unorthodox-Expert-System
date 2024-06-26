//! A Sample is a state, an applied action, and a result state.

use crate::rule::SomeRule;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SomeSample {
    // Initial state of the sample.
    pub initial: SomeState,
    // State after running an action.
    pub result: SomeState,
}

impl fmt::Display for SomeSample {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeSample {
    pub fn new(initial: SomeState, result: SomeState) -> Self {
        debug_assert_eq!(initial.num_bits(), result.num_bits());

        Self { initial, result }
    }

    /// Create a rule from a sample.
    pub fn rule(&self) -> SomeRule {
        SomeRule::new(self)
    }

    /// Return a string to represent a SomeSample instance.
    fn formatted_string(&self) -> String {
        format!("{} -> {}", self.initial, self.result)
    }

    /// Return the number bits used in sample elements.
    pub fn num_bits(&self) -> usize {
        self.initial.num_bits()
    }
}
