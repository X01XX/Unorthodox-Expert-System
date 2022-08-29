//! The TargetStore struct, a vector of SomeTarget structs.

use crate::target::SomeTarget;

use std::fmt;
use std::ops::Index; // IndexMut
use std::slice::Iter;

impl fmt::Display for TargetStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n[");

        for targx in &self.avec {
            if flg == 1 {
                rc_str.push_str(&String::from(",\n "));
            }
            rc_str.push_str(&format!("{}", &targx));
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

#[derive(Debug)]
#[readonly::make]
pub struct TargetStore {
    /// A vector of SomeTarget instances.
    avec: Vec<SomeTarget>,
}

impl TargetStore {
    /// Return a new TargetStore instance.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeTarget>::new(),
        }
    }

    /// Return a new TargetStore instance.
    pub fn new_with_target(target: SomeTarget) -> Self {
        Self { avec: vec![target] }
    }

    /// Return the length of the SomeTarget vector.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Add a target to the vector.
    pub fn push(&mut self, val: SomeTarget) {
        self.avec.push(val);
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeTarget> {
        self.avec.iter()
    }
} // end impl TargetStore

impl Index<usize> for TargetStore {
    type Output = SomeTarget;
    fn index<'a>(&'a self, i: usize) -> &'a SomeTarget {
        &self.avec[i]
    }
}
