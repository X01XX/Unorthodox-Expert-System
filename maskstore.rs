//! The MaskStore struct, a vector of SomeMask structs.

use crate::mask::SomeMask;
use crate::removeunordered::remove_unordered;

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
    avec: Vec<SomeMask>,
}

impl MaskStore {
    /// Return a new, empty, MaskStore struct instance.
    pub fn new(mvec: Vec::<SomeMask>) -> Self {
        Self {
            avec: mvec,
        }
    }

    /// Return the length of the store.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return a vector interator.
    pub fn iter(&self) -> Iter<SomeMask> {
        self.avec.iter()
    }

    /// Return true if any masks in the store are subset of a given mask.
    pub fn any_subset(&self, mskx: &SomeMask) -> bool {
        for stax in &self.avec {
            if stax.is_subset_of(mskx) {
                return true;
            }
        }
        false
    }

    /// Return true if all masks in the store are one bit.
//    pub fn all_one_bit(&self) -> bool {
//        for stax in &self.avec {
//            if stax.just_one_bit() {
//            } else {
//                return false;
//            }
//        }
//        true
//    }

    /// Return true if any masks in the store are a superset of a given mask.
    pub fn any_superset(&self, mskx: &SomeMask) -> bool {
        for stax in &self.avec {
            if mskx.is_subset_of(stax) {
                return true;
            }
        }
        false
    }

    /// Push a given mask, deleting supersets.
    pub fn push_nosups(&mut self, mskx: SomeMask) {

        if self.any_subset(&mskx) {
            return;
        }

        // Get vector of indexes of supersets to remove.
        let mut inxs = Vec::<usize>::new();
        let mut inx = 0;
        for stax in &self.avec {
            if stax.is_superset_of(&mskx) {
                inxs.push(inx);
            }
            inx += 1;
        }

        // Remove supersets.
        for inx in inxs.iter().rev() {
            remove_unordered(&mut self.avec, *inx);
        }

        // Add new mask
        self.avec.push(mskx);
    }

    /// Push a given mask, deleting subsets.
    pub fn push_nosubs(&mut self, mskx: SomeMask) {

        if self.any_superset(&mskx) {
            return;
        }

        // Get vector of indexes of subsets to remove.
        let mut inxs = Vec::<usize>::new();
        let mut inx = 0;
        for stax in &self.avec {
            if stax.is_subset_of(&mskx) {
                inxs.push(inx);
            }
            inx += 1;
        }

        // Remove subsets.
        for inx in inxs.iter().rev() {
            remove_unordered(&mut self.avec, *inx);
        }

        // Add new mask
        self.avec.push(mskx);
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
