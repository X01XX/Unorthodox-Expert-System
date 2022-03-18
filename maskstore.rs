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

#[readonly::make]
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

    /// Return true if a MaskStore contains a given mask.
    pub fn contains(&self, mskx: &SomeMask) -> bool {
        self.avec.contains(mskx)
    }
} // end impl MaskStore

impl Index<usize> for MaskStore {
    type Output = SomeMask;
    fn index<'a>(&'a self, i: usize) -> &'a SomeMask {
        &self.avec[i]
    }
}

#[cfg(test)]
mod tests {
    use crate::mask::SomeMask;
    use crate::maskstore::MaskStore;

    #[test]
    fn test_any_subset() -> Result<(), String> {
        let mut msk_str = MaskStore::new(vec![SomeMask::new_from_string(1, "m11").unwrap()]);
        msk_str.push_nosups(SomeMask::new_from_string(1, "m1100").unwrap());

        if msk_str.any_subset(&SomeMask::new_from_string(1, "m1100").unwrap()) == false {
            return Err(format!("MaskStore::test_any_subset 1 False?"));
        }

        if msk_str.any_subset(&SomeMask::new_from_string(1, "m11").unwrap()) == false {
            return Err(format!("MaskStore::test_any_subset 2 False?"));
        }

        if msk_str.any_subset(&SomeMask::new_from_string(1, "m111").unwrap()) == false {
            return Err(format!("MaskStore::test_any_subset 3 False?"));
        }

        if msk_str.any_subset(&SomeMask::new_from_string(1, "m1110").unwrap()) == false {
            return Err(format!("MaskStore::test_any_subset 4 False?"));
        }

        if msk_str.any_subset(&SomeMask::new_from_string(1, "m1").unwrap()) {
            return Err(format!("MaskStore::test_any_subset 5 True?"));
        }

        Ok(())
    }

    #[test]
    fn test_any_superset() -> Result<(), String> {
        let mut msk_str = MaskStore::new(vec![SomeMask::new_from_string(1, "m11").unwrap()]);
        msk_str.push_nosups(SomeMask::new_from_string(1, "m1100").unwrap());

        if msk_str.any_superset(&SomeMask::new_from_string(1, "m1100").unwrap()) == false {
            return Err(format!("MaskStore::test_any_superset 1 False?"));
        }

        if msk_str.any_superset(&SomeMask::new_from_string(1, "m11").unwrap()) == false {
            return Err(format!("MaskStore::test_any_superset 2 False?"));
        }

        if msk_str.any_superset(&SomeMask::new_from_string(1, "m1").unwrap()) == false {
            return Err(format!("MaskStore::test_any_superset 3 False?"));
        }

        if msk_str.any_superset(&SomeMask::new_from_string(1, "m1000").unwrap()) == false {
            return Err(format!("MaskStore::test_any_superset 4 False?"));
        }

        if msk_str.any_superset(&SomeMask::new_from_string(1, "m10000").unwrap()) {
            return Err(format!("MaskStore::test_any_superset 5 True?"));
        }

        Ok(())
    }

    // Test MaskStore push_nosups.
    #[test]
    fn test_maskstore_push_nosups() -> Result<(), String> {
        let mut msk_str = MaskStore::new(vec![SomeMask::new_from_string(1, "m111").unwrap()]);
        msk_str.push_nosups(SomeMask::new_from_string(1, "m1110").unwrap());
        assert!(msk_str.len() == 2);
        msk_str.push_nosups(SomeMask::new_from_string(1, "m0110").unwrap());
        assert!(msk_str.len() == 1);
        assert!(msk_str.contains(&SomeMask::new_from_string(1, "m0110").unwrap()));
        Ok(())
    }

    // Test MaskStore push_nosubs.
    #[test]
    fn test_maskstore_push_nosubs() -> Result<(), String> {
        let mut msk_str = MaskStore::new(vec![SomeMask::new_from_string(1, "m1").unwrap()]);
        msk_str.push_nosups(SomeMask::new_from_string(1, "m10").unwrap());
        assert!(msk_str.len() == 2);
        msk_str.push_nosubs(SomeMask::new_from_string(1, "m011").unwrap());
        assert!(msk_str.len() == 1);
        assert!(msk_str.contains(&SomeMask::new_from_string(1, "m011").unwrap()));
        Ok(())
    }
}
