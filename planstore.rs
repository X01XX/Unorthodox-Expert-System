//! The PlanStore struct, a vector of SomePlan structs.

use crate::plan::SomePlan;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index; // IndexMut
use std::slice::Iter;

impl fmt::Display for PlanStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n(");

        for planx in &self.avec {
            if flg == 1 {
                rc_str.push_str(",\n ");
            }
            rc_str.push_str(&format!("{}", &planx));
            flg = 1;
        }
        rc_str.push(')');

        write!(f, "{rc_str}")
    }
}

#[readonly::make]
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct PlanStore {
    /// A vector of SomePlan instances.
    pub avec: Vec<SomePlan>,
}

impl PlanStore {
    /// Return a new PlanStore instance.
    pub fn new(avec: Vec<SomePlan>) -> Self {
        Self { avec }
    }

    /// Return the length of the SomePlan vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Add a plan to the vector.
    pub fn push(&mut self, val: SomePlan) {
        self.avec.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomePlan> {
        self.avec.iter()
    }

    /// Return a second display version of a PlanStore.
    pub fn str2(&self) -> String {
        let mut rc_str = String::new();

        for planx in &self.avec {
            if planx.is_not_empty() {
                rc_str.push_str(&format!("\nPlan, Domain {}:\n", planx.dom_num));
                rc_str.push_str(&planx.str2());
                rc_str.push('\n');
            }
        }
        rc_str
    }

    /// Return a more restricted display version of a PlanStore.
    pub fn str_terse(&self) -> String {
        let mut rc_str = String::new();

        if self.avec.len() > 1 {
            rc_str.push('(');
        }
        let mut not_first = false;
        for planx in &self.avec {
            if not_first {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&planx.str_terse());
            not_first = true;
        }
        if self.avec.len() > 1 {
            rc_str.push(')');
        }
        rc_str
    }
} // end impl PlanStore

impl Index<usize> for PlanStore {
    type Output = SomePlan;
    fn index(&self, i: usize) -> &SomePlan {
        &self.avec[i]
    }
}
