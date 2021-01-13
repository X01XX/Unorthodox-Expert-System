// Implement a store for states

use crate::state::SomeState;
use std::ops::Index; // IndexMut
use std::slice::Iter;

use std::fmt;

impl fmt::Display for StateStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

#[derive(Debug)]
pub struct StateStore {
    avec: Vec<SomeState>,
}

impl StateStore {
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeState>::new(),
        }
    }

    pub fn new_with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeState>::with_capacity(num),
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

    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 2;

        if self.avec.len() > 0 {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    pub fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::with_capacity(self.formatted_string_length());
        rc_str.push('[');

        for stax in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            rc_str.push_str(&format!("{}", &stax));
            flg = 1;
        }

        rc_str.push(']');

        rc_str
    }
} // end impl StateStore

impl Index<usize> for StateStore {
    type Output = SomeState;
    fn index<'a>(&'a self, i: usize) -> &'a SomeState {
        &self.avec[i]
    }
}
