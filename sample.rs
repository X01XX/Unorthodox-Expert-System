//! A Sample is a state, an applied action, and a result state.

use crate::rule::SomeRule;
use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SomeSample {
    pub initial: SomeState,
    pub result: SomeState,
}

impl fmt::Display for SomeSample {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeSample {
    pub fn new(initial: SomeState, result: SomeState) -> Self {
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
}
