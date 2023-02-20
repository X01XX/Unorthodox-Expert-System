//! A Sample is a state, a applied action, and a result state.

use crate::state::SomeState;

use serde::{Deserialize, Serialize};
use std::fmt;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SomeSample {
    pub initial: SomeState,
    pub action: usize,
    pub result: SomeState,
}

impl fmt::Display for SomeSample {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl SomeSample {
    pub fn new(initial: SomeState, action: usize, result: SomeState) -> Self {
        Self {
            initial,
            action,
            result,
        }
    }

    /// Return a string to represent a SomeSample instance.
    pub fn formatted_string(&self) -> String {
        format!(
            "{} - {} -> {}",
            self.initial.formatted_string(),
            self.action,
            self.result.formatted_string()
        )
    }
}
