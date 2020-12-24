// Implement a store for states

use crate::state::SomeState;
use std::ops::Index; // IndexMut
use std::slice::Iter;

use std::fmt;

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut flg = 0;
        let mut rc_str = String::from("[");

        for stax in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &stax));
            flg = 1;
        }
        write!(f, "{}]", rc_str)
    }
}

#[derive(Debug)]
pub struct StateStore {
    avec: Vec<SomeState>,
}

impl StateStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeState>::with_capacity(4),
        }
    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    pub fn push(&mut self, val: SomeState) {
        self.avec.push(val);
    }

    pub fn iter(&self) -> Iter<SomeState> {
        self.avec.iter()
    }

    pub fn contains(&self, stax: &SomeState) -> bool {
        for stay in &self.avec {
            if stay == stax {
                return true;
            }
        }
        false
    }
}

impl Index<usize> for StateStore {
    type Output = SomeState;
    fn index<'a>(&'a self, i: usize) -> &'a SomeState {
        &self.avec[i]
    }
}
