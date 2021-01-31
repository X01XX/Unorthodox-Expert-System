// Implement a store for masks

use crate::mask::SomeMask;
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

impl fmt::Display for MaskStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

pub struct MaskStore {
    pub avec: Vec<SomeMask>,
}

impl MaskStore {
    pub fn _new() -> Self {
        Self {
            avec: Vec::<SomeMask>::new(),
        }
    }

    pub fn new_with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeMask>::with_capacity(num),
        }
    }

    pub fn len(&self) -> usize {
        self.avec.len()
    }

    pub fn contains(&self, amask: &SomeMask) -> bool {
        for mskx in self.avec.iter() {
            if mskx == amask {
                return true;
            }
        }
        false
    }

    //    pub fn contains_superset(&self, amask: &SomeMask) -> bool {
    //        for mskx in self.avec.iter() {
    //            if mskx.is_superset_of(&amask) {
    //                return true;
    //            }
    //        }
    //        false
    //    }

    pub fn push(&mut self, val: SomeMask) {
        self.avec.push(val);
    }

    pub fn iter(&self) -> Iter<SomeMask> {
        self.avec.iter()
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
}

impl Index<usize> for MaskStore {
    type Output = SomeMask;
    fn index<'a>(&'a self, i: usize) -> &'a SomeMask {
        &self.avec[i]
    }
}
