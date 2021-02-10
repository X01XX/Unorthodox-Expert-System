// Implement a store for needs, for an Unorthodox Expert System.

use crate::need::SomeNeed;
use std::fmt;
use std::ops::Index; // IndexMut
use std::slice::IterMut;

impl fmt::Display for NeedStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;

        let mut rc_str = String::new();

        rc_str.push_str("\n[");

        for stax in &self.avec {
            if flg == 1 {
                rc_str.push_str(&String::from(",\n "));
            }
            rc_str.push_str(&format!("{}", &stax));
            flg = 1;
        }
        rc_str.push(']');

        write!(f, "{}", rc_str)
    }
}

pub struct NeedStore {
    avec: Vec<SomeNeed>,
}

impl NeedStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeNeed>::with_capacity(5),
        }
    }

    //    pub fn new_with_capacity(num: usize) -> Self {
    //        Self {
    //            avec: Vec::<SomeNeed>::with_capacity(num),
    //        }
    //    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    // Check if a need is already in a needstore
    pub fn contains(&self, new_need: &SomeNeed) -> bool {
        for nedx in &self.avec {
            if *nedx == *new_need {
                return true;
            }
        }

        false
    }

    pub fn push(&mut self, val: SomeNeed) {
        if self.contains(&val) == false {
            self.avec.push(val);
        }
    }

    pub fn append(&mut self, other: &mut NeedStore) {
        self.avec.append(&mut other.avec);
    }

    //    pub fn iter(&self) -> Iter<SomeNeed> {
    //        self.avec.iter()
    //    }

    pub fn iter_mut(&mut self) -> IterMut<SomeNeed> {
        self.avec.iter_mut()
    }
}

impl Index<usize> for NeedStore {
    type Output = SomeNeed;
    fn index<'a>(&'a self, i: usize) -> &'a SomeNeed {
        &self.avec[i]
    }
}
