//! The RuleStore struct, a vector of SomeRule structs.
//!
//! The vector is sometimes empty, or has one or two rules.
//!
//! If two rules, they cannot be combined due to a 0->X or 1->X difference.
//!
//! If a square has two alternating results, since the square has a bit pattern with
//! no X positions, the different results must different by one, or more,
//! 1->X or 0->X bit positions.

use crate::region::SomeRegion;
use crate::rule::SomeRule;
use crate::truth::Truth;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Write as _; // import without risk of name clashing
use std::ops::Index;
use std::slice::Iter;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RuleStore {
    avec: Vec<SomeRule>,
}

impl fmt::Display for RuleStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.formatted_string())
    }
}

impl PartialEq for RuleStore {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for rulx in self.iter() {
            if !other.contains(rulx) {
                return false;
            }
        }

        true
    }
}

impl Eq for RuleStore {}

impl Default for RuleStore {
    fn default() -> Self {
        Self::new()
    }
}

impl RuleStore {
    /// Return a new RuleStore.
    pub fn new() -> Self {
        Self {
            avec: Vec::<SomeRule>::with_capacity(2),
        }
    }

    /// Return if a square result rulestore is valid
    pub fn is_valid(&self) -> bool {
        if self.is_empty() {
            return true;
        }

        if self.len() == 1 {
            return self.avec[0].is_valid();
        } // single rule is valid, or not

        if self.len() > 2 {
            return false;
        }

        // Length must be 2.  The two rules are different and the initial regions are the same.
        self.avec[0].is_valid()
            && self.avec[1].is_valid()
            && self.avec[0] != self.avec[1]
            && self.avec[0].initial_region() == self.avec[1].initial_region()
    }

    /// Return true if a RuleStore contains a rule.
    pub fn contains(&self, rul: &SomeRule) -> bool {
        self.avec.contains(rul)
    }

    /// Return the length of a RuleStore.
    /// Should be 0, 1 or 2.
    pub fn len(&self) -> usize {
        self.avec.len()
    }

    /// Return true if the store is empty.
    pub fn is_empty(&self) -> bool {
        self.avec.len() == 0
    }

    /// Add a rule to a RuleStore.
    /// If there are two rules, they will have at least one incompatibility,
    /// 0->0/0->1 or 1->1/1->0, and have equal initial regions.
    pub fn push(&mut self, val: SomeRule) {
        assert!(self.avec.len() < 2);

        self.avec.push(val);
        assert!(self.is_valid());
    }

    /// Return true if one RuleStore is a subset of another.
    /// This checks if a pn=1 rulestore is a subset of a pn=2 rulestore, the caller
    /// should check that the number of samples for the pn=1 rulestore is only 1.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        if self.len() > other.len() {
            return false;
        }

        if !self.initial_region().is_subset_of(&other.initial_region()) {
            return false;
        }

        if self.len() == 1 {
            if other.len() == 1 {
                return self[0].is_subset_of(&other[0]);
            } else if other.len() == 2 {
                return self[0].is_subset_of(&other[0]) || self[0].is_subset_of(&other[1]);
            }
        }

        if self.len() == 2 {
            if self[0].is_subset_of(&other[0]) && self[1].is_subset_of(&other[1]) {
                return true;
            }

            if self[0].is_subset_of(&other[1]) && self[1].is_subset_of(&other[0]) {
                return true;
            }

            return false;
        }

        panic!("unexpected rulestore length!");
    }

    /// Return true if a RuleStore is a superset of another.
    pub fn is_superset_of(&self, other: &RuleStore) -> bool {
        other.is_subset_of(self)
    }

    /// Return true if a RuleStore is a superset of a rule.
    pub fn is_superset_of_rule(&self, other: &SomeRule) -> bool {
        if self.len() == 1 {
            return self[0].is_superset_of(other);
        }

        if self.len() == 2 {
            return self[0].is_superset_of(other) || self[1].is_superset_of(other);
        }

        panic!("unexpected rulestore length!");
    }

    /// Return the union of two RuleStores.
    /// May return None, if the union is invalid.
    ///
    /// If the rules will are from two squares, the possible combinations
    /// for each bit position are:
    ///
    /// 0->0, 0->0 = 0->0
    /// 0->0, 0->1 = 0->X, disallowed
    /// 0->0, 1->1 = X->X
    /// 0->0, 1->0 = X->0
    ///
    /// 0->1, 0->1 = 0->1
    /// 0->1, 1->1 = X->1
    /// 0->1, 1->0 = X->x (X to x-not)
    ///
    /// 1->1, 1->1 = 1->1
    /// 1->1, 1->0 = 1->X, disallowed
    ///
    /// 1->0, 1->0 = 1->0
    ///
    /// For two result rules,
    /// They may not be compatible.
    /// They may be compatible in one way,
    /// They may appear to be compatible in two ways, which indicates a problem, so they will be
    /// considered to be incompatible.  Being compatible in two ways will produce either a
    /// (X->1 or X->0) in a bit position of one union, an a (X->x or X->X) in the corresponding position
    /// of another union.
    /// For (X->1, X->x), 1->?
    ///     (X->0, X->x), 0->?
    ///     (X->1, X->X), 0->?
    ///     (X->0, X->X), 1->?
    pub fn union(&self, other: &Self) -> Option<Self> {
        //println!("\nrulestore union {} and {}", &self, &other);
        if self.len() != other.len() {
            return None;
        }

        if self.is_empty() {
            //return Some(Self::new());
            panic!("Unpredictable union not allowed");
        }

        if self.len() == 1 {
            if let Some(rulx) = self.avec[0].union(&other.avec[0]) {
                let mut ret_store = Self::new();
                ret_store.push(rulx);
                return Some(ret_store);
            }

            return None;
        }

        if self.len() == 2 {
            let mut ordera = true;

            let rul0 = self.avec[0].union(&other.avec[0]);
            let rul1 = self.avec[1].union(&other.avec[1]);

            if rul0.is_none() || rul1.is_none() {
                ordera = false;
            }

            let mut orderb = true;

            let rul2 = self.avec[0].union(&other.avec[1]);
            let rul3 = self.avec[1].union(&other.avec[0]);

            if rul2.is_none() || rul3.is_none() {
                orderb = false;
            }

            // For any Pn::Two RuleStore, there must be at least one single-bit position of 0->1 and 0->0 alternating result,
            // or a 1->0 and 1->1 alternating result.
            //
            // To join two Pn::Two RuleStores in one of two possible sequences, there must be at least one matching initial single-bit position
            // with an alternating result.
            if ordera == orderb {
                // Both true or both false.
                return None;
            }

            let mut ret_store = Self::new();

            if ordera {
                ret_store.push(rul0.unwrap());
                ret_store.push(rul1.unwrap());
                return Some(ret_store);
            }

            // Must be orderb = true.
            ret_store.push(rul2.unwrap());
            ret_store.push(rul3.unwrap());
            return Some(ret_store);
        } // end if self.len() == 2

        panic!("unexpected RuleStore length");
    }

    /// Return Truth value of a possible union.
    pub fn can_form_union(&self, other: &Self) -> Truth {
        assert!(!self.is_empty());
        assert!(!other.is_empty());

        // Handle Pn1 vs. Pn2.
        // The Pn1 type should not have enough samples to be pnc, caller to insure.
        if self.len() < other.len() {
            for rulx in other.iter() {
                if rulx.union(&self[0]).is_some() {
                    return Truth::M;
                }
            }
            return Truth::F;
        }

        if other.len() < self.len() {
            for rulx in self.iter() {
                if rulx.union(&other[0]).is_some() {
                    return Truth::M;
                }
            }
            return Truth::F;
        }

        if let Some(_ruls) = self.union(other) {
            //println!("can_form_union: 1 returning T {}", &ruls);
            return Truth::T;
        }
        //println!("can_form_union: 2 returning F");
        Truth::F
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRule> {
        self.avec.iter()
    }

    /// Return intersection of two RuleStores.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        if self.len() != other.len() {
            panic!("rulestore lengths not eq!");
        }

        if self.len() == 1 {
            let Some(int1) = self[0].intersection(&other[0]) else { return None; };

            let mut ars = Self::new();
            ars.push(int1);
            return Some(ars);
        }

        if self.len() == 2 {
            // Intersect by order1
            if let Some(int00) = self[0].intersection(&other[0]) {
                if let Some(int11) = self[1].intersection(&other[1]) {
                    if int00.initial_region() == int11.initial_region() {
                        let mut ord1 = Self::new();
                        ord1.push(int00);
                        ord1.push(int11);
                        //println!("pn3 intersection of {} and {} is1 {}", self, other, ord1);
                        return Some(ord1);
                    }
                }
            }

            // Intersect by order2
            if let Some(int01) = self[0].intersection(&other[1]) {
                if let Some(int10) = self[1].intersection(&other[0]) {
                    if int01.initial_region() == int10.initial_region() {
                        let mut ord2 = Self::new();
                        ord2.push(int01);
                        ord2.push(int10);
                        //println!("pn3 intersection of {} and {} is2 {}", self, other, ord2);
                        return Some(ord2);
                    }
                }
            }

            //println!("pn3 intersection of {} and {} failed", self, other);
            return None;
        }
        panic!("not ready for pn {}!", self.len());
    }

    /// Return the result of restricting the initial region of rules.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Self {
        assert!(!self.is_empty());
        assert!(regx.intersects(&self.initial_region()));
        let mut rcrs = Self::new();

        for rulx in self.avec.iter() {
            rcrs.push(rulx.restrict_initial_region(regx));
        }
        rcrs
    }

    /// Return the initial region of the first rule in the store.
    pub fn initial_region(&self) -> SomeRegion {
        self.avec[0].initial_region()
    }

    /// Return the expected length of a string representing the store.
    pub fn formatted_string_length(&self) -> usize {
        let mut rc_len = 3;

        if !self.avec.is_empty() {
            rc_len += self.avec.len() * self.avec[0].formatted_string_length();
            if self.avec.len() > 1 {
                rc_len += (self.avec.len() - 1) * 2;
            }
        }

        rc_len
    }

    /// Return a string representing the store.
    pub fn formatted_string(&self) -> String {
        let mut flg = 0;
        let mut rc_str = String::with_capacity(self.formatted_string_length());

        rc_str.push_str("R[");
        for strx in &self.avec {
            if flg == 1 {
                rc_str.push_str(", ");
            }
            let _ = write!(rc_str, "{}", &strx);
            flg = 1;
        }
        rc_str.push(']');

        rc_str
    }
} // end impl RuleStore

impl Index<usize> for RuleStore {
    type Output = SomeRule;
    fn index(&self, i: usize) -> &SomeRule {
        &self.avec[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test restrict_initial_region and initial_region
    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        // Produce R[00/00/00/00/00/XX/XX/11, 00/00/00/00/00/XX/XX/10].
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/11/11").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "00/11/10").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "11/00/10").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "11/00/11").unwrap());

        if let Some(rul_str3) = rul_str1.union(&rul_str2) {
            let regx = SomeRegion::new_from_string(1, "r101").unwrap();
            let rul_str4 = rul_str3.restrict_initial_region(&regx);
            println!("Restrict_initial_region worked, is {}", &rul_str4);

            if rul_str4.initial_region() != regx {
                return Err(String::from("Test 1 failed"));
            }
            if rul_str4[1].initial_region() != regx {
                return Err(String::from("Test 2 failed"));
            }
        } else {
            return Err(String::from("Test 3 failed"));
        }
        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        // Intersect two single-rule RuleStores, it should work.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "X0/11/11/10/00").unwrap());

        if let Some(rul_str3) = rul_str1.intersection(&rul_str2) {
            println!("test_intersection 1 passed = {}", &rul_str3);
            let mut rul_str4 = RuleStore::new();
            rul_str4.push(SomeRule::new_from_string(1, "00/11/11/10/00").unwrap());
            if rul_str3 != rul_str4 {
                return Err(String::from("Test 1 failed"));
            }
        } else {
            return Err(String::from("Test 2 failed"));
        }

        // Produce failure due to bit 3, Xx intersect 11 = null.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/Xx/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "X0/11/11/10/00").unwrap());

        if let Some(_rul_str3) = rul_str1.intersection(&rul_str2) {
            return Err(String::from("Test 2 failed"));
        }

        // Intersect two two-rule RuleStores, it should work.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "01/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "X1/11/11/10/00").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "X0/11/11/10/00").unwrap());

        if let Some(rul_str3) = rul_str1.intersection(&rul_str2) {
            println!("Test 3 passed = {}", &rul_str3);
            let rulx = SomeRule::new_from_string(1, "00/11/11/10/00").unwrap();
            if rul_str3[0] != rulx && rul_str3[1] != rulx {
                return Err(String::from("Test 3 failed, 00/11/11/10/00 not in result"));
            }
            let rulx = SomeRule::new_from_string(1, "01/11/11/10/00").unwrap();
            if rul_str3[0] != rulx && rul_str3[1] != rulx {
                return Err(String::from("Test 3 failed, 01/11/11/10/00 not in result"));
            }
        } else {
            return Err(String::from("Test 3 failed"));
        }

        // Intersect two two-rule RuleStores, it should not work, due to the left-most bit.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "01/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "X1/11/11/10/00").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "X1/11/11/11/00").unwrap());

        if let Some(_rul_str3) = rul_str1.intersection(&rul_str2) {
            return Err(String::from("Test 4 failed"));
        }

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        // Compare one-rule RuleStores.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "00/11/11/10/00").unwrap());

        if rul_str2.is_subset_of(&rul_str1) {
        } else {
            return Err(String::from("Test 1 failed"));
        }

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "00/10/11/10/00").unwrap());

        if rul_str2.is_subset_of(&rul_str1) {
            return Err(String::from("Test 2 failed"));
        }

        // Compare two two-rule RuleStores.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "01/X1/XX/Xx/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "00/11/11/10/00").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "01/11/11/10/00").unwrap());

        if rul_str2.is_subset_of(&rul_str1) {
        } else {
            return Err(String::from("Test 3 failed"));
        }

        if rul_str1.is_subset_of(&rul_str2) {
            return Err(String::from("Test 4 failed"));
        }
        Ok(())
    }

    #[test]
    fn is_superset_of_rule() -> Result<(), String> {
        // Compare a rule to a one-rule RuleStore.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());

        let rul2 = SomeRule::new_from_string(1, "00/11/11/10/00").unwrap();

        if rul_str1.is_superset_of_rule(&rul2) {
        } else {
            return Err(String::from("Test 1 failed"));
        }

        let rul2 = SomeRule::new_from_string(1, "00/10/11/10/00").unwrap();

        if rul_str1.is_superset_of_rule(&rul2) {
            return Err(String::from("Test 2 failed"));
        }

        // Compare rule to a two-rule RuleStore.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/X1/XX/Xx/xx").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "01/X1/XX/Xx/xx").unwrap());

        let rul2 = SomeRule::new_from_string(1, "00/11/11/10/00").unwrap();

        if rul_str1.is_superset_of_rule(&rul2) {
        } else {
            return Err(String::from("Test 3 failed"));
        }

        let rul2 = SomeRule::new_from_string(1, "00/00/11/10/00").unwrap();
        if rul_str1.is_superset_of_rule(&rul2) {
            return Err(String::from("Test 4 failed"));
        }
        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        // Produce /X0/X1/XX/Xx/XX.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/01/00/01/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "10/11/11/10/xx").unwrap());

        if let Some(_rul_str3) = rul_str1.union(&rul_str2) {
        } else {
            return Err(String::from("Test 1 failed"));
        }

        // Fail due to bit 1 being 0X.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/01/00/01/xx").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "10/11/11/00/xx").unwrap());

        if let Some(_rul_str3) = rul_str1.union(&rul_str2) {
            return Err(String::from("Test 2 failed"));
        }

        // Produce R[00/00/00/00/00/XX/XX/11, 00/00/00/00/00/XX/XX/10].
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/11/11").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "00/11/10").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "11/00/10").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "11/00/11").unwrap());

        if let Some(_rul_str3) = rul_str1.union(&rul_str2) {
        } else {
            return Err(String::from("Test 3 failed"));
        }

        // Fail due to bit 1 forming 0X.
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/01/11").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "00/00/10").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "11/00/10").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "11/00/11").unwrap());

        if let Some(rul_str3) = rul_str1.union(&rul_str2) {
            println!("rule? {}", rul_str3);
            return Err(String::from("Test 4 failed"));
        }

        // Fail due to X1 and X0 forming (XX, Xx) and (X0, X1)
        let mut rul_str1 = RuleStore::new();
        rul_str1.push(SomeRule::new_from_string(1, "00/01/X1").unwrap());
        rul_str1.push(SomeRule::new_from_string(1, "00/00/X0").unwrap());

        let mut rul_str2 = RuleStore::new();
        rul_str2.push(SomeRule::new_from_string(1, "11/00/X0").unwrap());
        rul_str2.push(SomeRule::new_from_string(1, "11/00/X1").unwrap());

        if let Some(_rul_str3) = rul_str1.union(&rul_str2) {
            return Err(String::from("Test 5 failed"));
        }

        Ok(())
    }
}
