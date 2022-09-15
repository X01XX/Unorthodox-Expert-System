//! The PlanStore struct, a vector of SomePlan structs.

use crate::plan::SomePlan;

use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing
use std::ops::Index; // IndexMut
use std::slice::Iter;

impl fmt::Display for PlanStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n[");

        for planx in &self.avec {
            if flg == 1 {
                rc_str.push_str(&String::from(",\n "));
            }
            let _ = write!(rc_str, "{}", &planx);
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
#[readonly::make]
pub struct PlanStore {
    /// A vector of SomePlan instances.
    pub avec: Vec<SomePlan>,
}

impl Default for PlanStore {
    fn default() -> Self {
        Self::new()
    }
}

impl PlanStore {
    /// Return a new PlanStore instance.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomePlan>::new(),
        }
    }

    /// Return the length of the SomePlan vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.len() == 0
    }

    /// Add a plan to the vector.
    pub fn push(&mut self, val: SomePlan) {
        self.avec.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomePlan> {
        self.avec.iter()
    }

    pub fn str2(&self) -> String {
        let mut rc_str = String::new();

        for planx in self.avec.iter() {
            if !planx.is_empty() {
                let _ = write!(rc_str, "\nPlan, Domain {}:\n", planx.dom_num);
                rc_str.push_str(&planx.str2());
                rc_str.push('\n');
            }
        }
        rc_str
    }

    pub fn str_terse(&self) -> String {
        let mut rc_str = String::new();

        let mut not_first = false;
        for planx in self.avec.iter() {
            if !planx.is_empty() {
                if not_first {
                    rc_str.push_str(", ");
                }
                rc_str.push_str(&planx.str_terse());
                not_first = true;
            }
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
