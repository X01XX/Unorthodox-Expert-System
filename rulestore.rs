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
use crate::tools;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Index;
use std::slice::Iter;

#[readonly::make]
#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct RuleStore {
    avec: Vec<SomeRule>,
}

impl fmt::Display for RuleStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", tools::vec_string(&self.avec))
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

impl RuleStore {
    /// Return a new, empty, RuleStore.
    pub fn new(avec: Vec<SomeRule>) -> Self {
        let ret = Self { avec };
        assert!(ret.is_valid());
        ret
    }

    /// Return if a square result rulestore is valid
    pub fn is_valid(&self) -> bool {
        if self.len() < 2 {
            return true;
        } // single rule is valid, or not

        if self.len() > 2 {
            return false;
        }

        // Length must be 2.  The two rules are different and the initial regions are the same.
        self.avec[0] != self.avec[1]
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
        self.avec.is_empty()
    }

    /// Return true if the store is not empty.
    pub fn is_not_empty(&self) -> bool {
        !self.avec.is_empty()
    }

    /// Add a rule to a RuleStore.
    /// If there are two rules, they will have at least one incompatibility,
    /// 0->0/0->1 or 1->1/1->0, and have equal initial regions.
    pub fn push(&mut self, val: SomeRule) {
        assert!(self.avec.len() < 2);

        self.avec.push(val);
        assert!(self.is_valid());
    }

    /// Return true if one non-empty RuleStore is a subset of another non-empty.
    /// This checks if a pn=1 rulestore is a subset of a pn=2 rulestore, the caller
    /// should check that the number of samples for the pn=1 rulestore is only 1.
    pub fn is_subset_of(&self, other: &Self) -> bool {
        assert!(!self.is_empty() && !other.is_empty());

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
    pub fn is_superset_of(&self, other: &Self) -> bool {
        other.is_subset_of(self)
    }

    /// Return true if a RuleStore is a superset of a rule.
    pub fn is_superset_of_rule(&self, other: &SomeRule) -> bool {
        assert!(!self.is_empty());

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
        assert!(!self.is_empty() && !other.is_empty());

        if self.len() != other.len() {
            //println!("\nrulestore union: returns None");
            return None;
        }

        if self.len() == 1 {
            let rulx = self.avec[0].union(&other.avec[0])?;
            return Some(Self::new(vec![rulx]));
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

            if ordera {
                return Some(Self::new(vec![rul0.unwrap(), rul1.unwrap()]));
            }

            // Must be orderb = true.
            return Some(Self::new(vec![rul2.unwrap(), rul3.unwrap()]));
        } // end if self.len() == 2

        panic!("unexpected RuleStore length");
    }

    /// Return Truth value of a union of two RuleStores.
    pub fn can_combine(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        if self.is_empty() {
            return true;
        }
        if self.union(other).is_some() {
            return true;
        }
        false
    }

    /// Return Truth value of a possible union.
    pub fn can_form_union(&self, other: &Self) -> Option<bool> {
        if self.is_empty() && other.is_empty() {
            return Some(true);
        }
        if self.is_empty() {
            return Some(false);
        }
        if other.is_empty() {
            return Some(false);
        }

        // Handle Pn1 vs. Pn2.
        // The Pn1 type should not have enough samples to be pnc, caller to insure.
        if self.len() < other.len() {
            for rulx in other.iter() {
                if rulx.union(&self[0]).is_some() {
                    return None;
                }
            }
            return Some(false);
        }

        if other.len() < self.len() {
            for rulx in self.iter() {
                if rulx.union(&other[0]).is_some() {
                    return None;
                }
            }
            return Some(false);
        }

        if let Some(_ruls) = self.union(other) {
            //println!("can_form_union: 1 returning T {}", &ruls);
            return Some(true);
        }
        //println!("can_form_union: 2 returning F");
        Some(false)
    }

    /// Return a vector iterator.
    pub fn iter(&self) -> Iter<SomeRule> {
        self.avec.iter()
    }

    /// Return the intersection of two RuleStores.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        if self.len() != other.len() {
            panic!("rulestore lengths not eq!");
        }

        if self.len() == 1 {
            let int1 = self[0].intersection(&other[0])?;

            return Some(Self::new(vec![int1]));
        }

        if self.len() == 2 {
            // Intersect by order1
            if let Some(int00) = self[0].intersection(&other[0]) {
                if let Some(int11) = self[1].intersection(&other[1]) {
                    if int00.initial_region() == int11.initial_region() {
                        //println!("pn3 intersection of {} and {}", self, other);
                        return Some(Self::new(vec![int00, int11]));
                    }
                }
            }

            // Intersect by order2
            if let Some(int01) = self[0].intersection(&other[1]) {
                if let Some(int10) = self[1].intersection(&other[0]) {
                    if int01.initial_region() == int10.initial_region() {
                        //println!("pn3 intersection of {} and {}", self, other);
                        return Some(Self::new(vec![int01, int10]));
                    }
                }
            }

            //println!("pn3 intersection of {} and {} failed", self, other);
            return None;
        }
        panic!("not ready for pn {}!", self.len());
    }

    /// Return the result of restricting the initial region of rules in a RuleStore.
    pub fn restrict_initial_region(&self, regx: &SomeRegion) -> Self {
        assert!(self.is_not_empty());
        assert!(
            regx.intersects(&self.initial_region()),
            "{}",
            format!("{} does not intersect {}", regx, self.initial_region())
        );
        let mut rcrs = Vec::<SomeRule>::with_capacity(self.avec.len());

        for rulx in self.avec.iter() {
            rcrs.push(rulx.restrict_initial_region(regx));
        }

        Self::new(rcrs)
    }

    /// Return the initial region of the first rule in the store.
    pub fn initial_region(&self) -> SomeRegion {
        self.avec[0].initial_region()
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
    use crate::bits::SomeBits;
    use crate::state::SomeState;

    // Test restrict_initial_region and initial_region
    #[test]
    fn restrict_initial_region() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_reg = SomeRegion::new(vec![tmp_sta.clone()]);
        let tmp_rul = SomeRule::new(&tmp_sta, &tmp_sta);

        // Produce R[00/00/00/00/00/XX/XX/11, 00/00/00/00/00/XX/XX/10].
        let rul_str1 = RuleStore::new(vec![
            tmp_rul.new_from_string("00/11/11")?,
            tmp_rul.new_from_string("00/11/10")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            tmp_rul.new_from_string("11/00/10")?,
            tmp_rul.new_from_string("11/00/11")?,
        ]);

        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        let Some(rul_str3) = rul_str1.union(&rul_str2) else {
            panic!("This should work!");
        };
        println!("rul_str3: {rul_str3}");

        let regx = tmp_reg.new_from_string("r101")?;
        let rul_str4 = rul_str3.restrict_initial_region(&regx);
        println!("rul_str4: {rul_str4}");

        assert!(rul_str4.initial_region() == regx);
        assert!(rul_str4[1].initial_region() == regx);

        Ok(())
    }

    #[test]
    fn intersection() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_rul = SomeRule::new(&tmp_sta, &tmp_sta);

        // Intersect two single-rule RuleStores, it should work.
        let rul_str1 = RuleStore::new(vec![tmp_rul.new_from_string("00/X1/XX/Xx/xx")?]);
        let rul_str2 = RuleStore::new(vec![tmp_rul.new_from_string("X0/11/11/10/00")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        let Some(rul_str3) = rul_str1.intersection(&rul_str2) else {
            panic!("This should work!");
        };
        println!("rul_str3: {rul_str3}");

        let rul_str4 = RuleStore::new(vec![tmp_rul.new_from_string("00/11/11/10/00")?]);
        println!("rul_str4: {rul_str4}");

        assert!(rul_str3 == rul_str4);

        // Produce failure due to bit 3, Xx intersect 11 = null.
        let rul_str1 = RuleStore::new(vec![tmp_rul.new_from_string("00/Xx/XX/Xx/xx")?]);
        let rul_str2 = RuleStore::new(vec![tmp_rul.new_from_string("X0/11/11/10/00")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.intersection(&rul_str2).is_none());

        // Intersect two two-rule RuleStores, it should work.
        let rul_str1 = RuleStore::new(vec![
            tmp_rul.new_from_string("00/X1/XX/Xx/xx")?,
            tmp_rul.new_from_string("01/X1/XX/Xx/xx")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            tmp_rul.new_from_string("X1/11/11/10/00")?,
            tmp_rul.new_from_string("X0/11/11/10/00")?,
        ]);

        let Some(rul_str3) = rul_str1.intersection(&rul_str2) else {
            panic!("This should work!");
        };
        println!("rul_str3: {rul_str3}");

        let rulx = tmp_rul.new_from_string("00/11/11/10/00")?;
        println!("rulx: {rulx}");

        assert!(rul_str3[0] == rulx || rul_str3[1] == rulx);

        let rulx = tmp_rul.new_from_string("01/11/11/10/00")?;
        println!("rulx: {rulx}");

        assert!(rul_str3[0] == rulx || rul_str3[1] == rulx);

        // Intersect two two-rule RuleStores, it should not work, due to the left-most bit.
        let rul_str1 = RuleStore::new(vec![
            tmp_rul.new_from_string("00/X1/XX/Xx/xx")?,
            tmp_rul.new_from_string("01/X1/XX/Xx/xx")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            tmp_rul.new_from_string("X1/11/11/10/00")?,
            tmp_rul.new_from_string("X1/11/11/11/00")?,
        ]);

        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.intersection(&rul_str2).is_none());

        Ok(())
    }

    #[test]
    fn is_subset_of() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_rul = SomeRule::new(&tmp_sta, &tmp_sta);

        // Compare one-rule RuleStores.
        let rul_str1 = RuleStore::new(vec![tmp_rul.new_from_string("00/X1/XX/Xx/xx")?]);
        let rul_str2 = RuleStore::new(vec![tmp_rul.new_from_string("00/11/11/10/00")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str2.is_subset_of(&rul_str1));

        let rul_str2 = RuleStore::new(vec![tmp_rul.new_from_string("00/10/11/10/00")?]);
        println!("rul_str2: {rul_str2}");

        assert!(!rul_str2.is_subset_of(&rul_str1));

        // Compare two two-rule RuleStores.
        let rul_str1 = RuleStore::new(vec![
            tmp_rul.new_from_string("00/X1/XX/Xx/xx")?,
            tmp_rul.new_from_string("01/X1/XX/Xx/xx")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            tmp_rul.new_from_string("00/11/11/10/00")?,
            tmp_rul.new_from_string("01/11/11/10/00")?,
        ]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str2.is_subset_of(&rul_str1));
        assert!(!rul_str1.is_subset_of(&rul_str2));

        Ok(())
    }

    #[test]
    fn is_superset_of_rule() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_rul = SomeRule::new(&tmp_sta, &tmp_sta);

        // Compare a rule to a one-rule RuleStore.
        let rul_str1 = RuleStore::new(vec![tmp_rul.new_from_string("00/X1/XX/Xx/xx")?]);
        let rul2 = tmp_rul.new_from_string("00/11/11/10/00")?;
        println!("rul_str1: {rul_str1}");
        println!("rul2: {rul2}");

        assert!(rul_str1.is_superset_of_rule(&rul2));

        let rul2 = tmp_rul.new_from_string("00/10/11/10/00")?;
        println!("rul2: {rul2}");

        assert!(!rul_str1.is_superset_of_rule(&rul2));

        // Compare rule to a two-rule RuleStore.
        let rul_str1 = RuleStore::new(vec![
            tmp_rul.new_from_string("00/X1/XX/Xx/xx")?,
            tmp_rul.new_from_string("01/X1/XX/Xx/xx")?,
        ]);

        let rul2 = tmp_rul.new_from_string("00/11/11/10/00")?;
        println!("rul_str1: {rul_str1}");
        println!("rul2: {rul2}");

        assert!(rul_str1.is_superset_of_rule(&rul2));

        let rul2 = tmp_rul.new_from_string("00/00/11/10/00")?;
        println!("rul2: {rul2}");

        assert!(!rul_str1.is_superset_of_rule(&rul2));

        Ok(())
    }

    #[test]
    fn union() -> Result<(), String> {
        let tmp_bts = SomeBits::new(vec![0]);
        let tmp_sta = SomeState::new(tmp_bts.clone());
        let tmp_rul = SomeRule::new(&tmp_sta, &tmp_sta);

        // Produce /X0/X1/XX/Xx/XX.
        let rul_str1 = RuleStore::new(vec![tmp_rul.new_from_string("00/01/00/01/xx")?]);
        let rul_str2 = RuleStore::new(vec![tmp_rul.new_from_string("10/11/11/10/xx")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_some());

        // Fail due to bit 1 being 0X.
        let rul_str1 = RuleStore::new(vec![tmp_rul.new_from_string("00/01/00/01/xx")?]);
        let rul_str2 = RuleStore::new(vec![tmp_rul.new_from_string("10/11/11/00/xx")?]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_none());

        // Produce R[00/00/00/00/00/XX/XX/11, 00/00/00/00/00/XX/XX/10].
        let rul_str1 = RuleStore::new(vec![
            tmp_rul.new_from_string("00/11/11")?,
            tmp_rul.new_from_string("00/11/10")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            tmp_rul.new_from_string("11/00/10")?,
            tmp_rul.new_from_string("11/00/11")?,
        ]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_some());

        // Fail due to bit 1 forming 0X.
        let rul_str1 = RuleStore::new(vec![
            tmp_rul.new_from_string("00/01/11")?,
            tmp_rul.new_from_string("00/00/10")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            tmp_rul.new_from_string("11/00/10")?,
            tmp_rul.new_from_string("11/00/11")?,
        ]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_none());

        // Fail due to X1 and X0 forming (XX, Xx) and (X0, X1)
        let rul_str1 = RuleStore::new(vec![
            tmp_rul.new_from_string("00/01/X1")?,
            tmp_rul.new_from_string("00/00/X0")?,
        ]);

        let rul_str2 = RuleStore::new(vec![
            tmp_rul.new_from_string("11/00/X0")?,
            tmp_rul.new_from_string("11/00/X1")?,
        ]);
        println!("rul_str1: {rul_str1}");
        println!("rul_str2: {rul_str2}");

        assert!(rul_str1.union(&rul_str2).is_none());

        Ok(())
    }
}
