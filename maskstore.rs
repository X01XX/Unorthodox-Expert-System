//! The MaskStore struct, a vector of SomeMask structs.

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
    /// A vector for storing SomeMask stucts instances.
    pub avec: Vec<SomeMask>,
}

impl MaskStore {
    /// Return a new, empty, MaskStore struct instance.
    pub fn _new() -> Self {
        Self {
            avec: Vec::<SomeMask>::new(),
        }
    }

    /// Return a new, empty, MaskStore struct instance, with a given capacity.
    pub fn new_with_capacity(num: usize) -> Self {
        Self {
            avec: Vec::<SomeMask>::with_capacity(num),
        }
    }

    /// Return the length of the store.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store contains a given mask.
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

    /// Push a SomeMask instance onto the vector.
    pub fn push(&mut self, val: SomeMask) {
        self.avec.push(val);
    }

    /// Return a vector interator.
    pub fn iter(&self) -> Iter<SomeMask> {
        self.avec.iter()
    }

    /// Return the expected length of a string representing the MaskStore.
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

    /// Return a string representing the MaskStore.
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
